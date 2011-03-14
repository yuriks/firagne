[map symbols]

LOAD_BASE equ 0x10000

%define DATA16(x) x + LOAD_BASE
%define DATA32(x) x - LOAB_BASE

; GDT_ENTRY %1=base %2=limit %3=flags
%macro	GDT_ENTRY 3
	dw %2 & 0xFFFF				; Limit 15:0
	dw %1 & 0xFFFF				; Base 15:0
	db (%1 >> 16) & 0xFF			; Base 23:16
	dw (%3 & 0xF0FF) | ((%2 >> 8) & 0x0F00)	; Flags + Limit 19:16
	db (%1 >> 24) & 0xFF			; Base 31:24
%endmacro

%macro	GDT_NULL_ENTRY 0
	dd 0
	dd 0
%endmacro

%macro	DEF_DATA 2+
	[SECTION .data]
	%1 %2
	__SECT__
%endmacro

GDT_GRANULARITY_BYTE equ (0 << 15)
GDT_GRANULARITY_4KB equ (1 << 15)
GDT_DEFAULT_16BIT equ (0 << 14)
GDT_DEFAULT_32BIT equ (1 << 14)
GDT_LONG_MODE equ (1 << 13)
GDT_OS_DEFINED equ (1 << 12)
GDT_PRESENT equ (1 << 7)
%define GDT_RING(x) ((x & 0x3) << 5)
GDT_DESCRIPTOR_SYSTEM equ (0 << 4)
GDT_DESCRIPTOR_CODE_DATA equ (1 << 4)
GDT_ACESSED equ (1 << 0)

GDT_TYPE_DATA equ (0 << 3)
GDT_TYPE_DATA_EXPAND_DOWN equ (1 << 2)
GDT_TYPE_DATA_WRITABLE equ (1 << 1)
GDT_TYPE_CODE equ (1 << 3)
GDT_TYPE_CODE_CONFORMING equ (1 << 2)
GDT_TYPE_CODE_READABLE equ (1 << 1)


MMAP_MAX_ENTRIES equ 32

[CPU X64]
[BITS 16]
section .data progbits follows=.text32 vfollows=.text32
section .text16 progbits start=0x0000
setup_unreal_mode:
	cli

	; Setup Unreal Mode
	lgdt [cs:unreal_gdt_desc]	; Load GDT with minimal entries

	mov eax, cr0		; Enable protected mode
	or eax, 1
	mov cr0, eax

	mov bx, 0x08		; Load base/limit/flags to seg. regs
	mov ds, bx
	mov es, bx

	and eax, ~1		; Exit protected mode
	mov cr0, eax

	xor ax, ax		; Set segments base to 0x0000
	mov ds, ax		; Only overwrites base, limit/flags unchanged
	mov es, ax
	mov ss, ax
	mov sp, stack_begin	; Setup stack
	sti

detect_video:
	and byte [boot_flags1], ~BF1_HAS_VGA	; Clear 'Has VGA' bit
	mov ax, 0x1A00		; int 10h,ax=1A00: GET DISPLAY COMBINATION CODE
	int 0x10
	cmp al, 0x1A
	jne .no_video

	or byte [boot_flags1], BF1_HAS_VGA	; We have a VGA

DEF_DATA .video_init_msg, db `Initialized video.\n`,0
	mov esi, .video_init_msg
	call print_str
.no_video:

enable_a20:
	; Check for A20 line
	xor ax, ax
	mov ds, ax
	mov ax, 0xFFFF
	mov es, ax

	mov ax, [0x7DFE]	; stage1 (with 0xAA55 signature) should still be loaded
	mov bx, [es:0x7E0E]
	cmp ax, bx
	jne .a20_enabled

	not ax			; Change and overwrite the signature
	mov [0x7DFE], ax
	mov bx, [es:0x7E0E]	; Check if wrapped location also changed
	cmp ax, bx
	jne .a20_enabled

	mov ax, 0x2401		; int 15h/ax=2401h - ENABLE A20 GATE
	int 0x15
	jnc .a20_enabled

	cmp ah, 0x86		; Function not supported?
	jne .fail_a20		; Failed for some other reason, bail.

	; Add more methods here as needed
	jmp .fail_a20		; All our attempts failed, bail.
.a20_enabled:
	jmp read_mmap

DEF_DATA .fail_a20_string, db `Failed to enable A20 gate!\n`,0
.fail_a20:
	mov esi, .fail_a20_string
	jmp fail16bstr

read_mmap:
	mov word [memmap_count], MMAP_MAX_ENTRIES	; MMAP_MAX_ENTRIES positions free

	mov ebx, 0		; Beginning of map
	mov di, memmap		; Destination buffer

SMAP_MAGIC equ 0x534D4150
.rmap_loop:
	mov eax, 0xE820		; int 15h,eax=E820: GET SYSTEM MEMORY MAP
	mov edx, SMAP_MAGIC	; 'SMAP'
	mov ecx, mmap_entry_size	; Buffer size
	mov dword [di+mmap_entry.flags], ~0	; Set defaults
	int 15h			; Call int
	jc .fail_int		; Carry flag on indicates failure

	cmp eax, SMAP_MAGIC	; eax != magic also means failure
	jne .fail_int

	add di, mmap_entry_size	; Advance di by number of bytes read
	dec word [memmap_count]	; Decrement number of free positions

	or ebx, ebx		; If ebx == 0 then copying is finished
	jz .rmap_done

	cmp word [memmap_count], 0	; We ran out of entries!
	je .fail_out_of_space

	jmp .rmap_loop

; Failure due to carry flag
DEF_DATA .fail_int_string, db `Failed to read BIOS memory map!\n`,0
.fail_int:
	mov esi, .fail_int_string
	jmp fail16bstr

; Failure due to running out of mmap entries
DEF_DATA .fail_out_of_space_string, db `Ran out of space for storing memory map entries!\n`,0
.fail_out_of_space:
	mov esi, .fail_out_of_space_string
	jmp fail16bstr

.rmap_done:
	mov ax, MMAP_MAX_ENTRIES	; entries read = max entries - free entries
	sub ax, word [memmap_count]
	mov word [memmap_count], ax

	jmp enable_pmode


; Print string pointed to by esi (null terminated).
print_str:
	test byte [boot_flags1], BF1_HAS_VGA
	jz .end_loop

	mov ah, 0x0E
	mov bx, 0x0F00

.loop:
	mov al, byte [esi]
	or al, al		; Check for 0
	jz .end_loop

	int 0x10

	inc esi

	cmp al, `\n`
	jne .loop
	mov al, `\r`
	int 0x10

	jmp .loop
.end_loop:

	ret

; Print string pointed to by esi then halt.
fail16bstr:
	call print_str
	cli
	hlt

enable_pmode:
DEF_DATA .pmode_pre_str, db "Entering protected mode...",0
	mov esi, .pmode_pre_str
	call print_str

	cli

	lgdt [cs:gdt_desc16]	; Load GDT

	mov eax, cr0		; Enable protection
	or eax, 1
	mov cr0, eax

	jmp 08h:dword jump_to_32b	; Jump to our code segment


unreal_gdt: ; Unreal mode GDT
; 0x00 - null descriptor
GDT_NULL_ENTRY

; 0x08 - data descriptor
GDT_ENTRY 0x00000000, 0xFFFFF, \
	GDT_GRANULARITY_4KB | GDT_DEFAULT_16BIT | GDT_PRESENT | GDT_RING(0) | \
	GDT_DESCRIPTOR_CODE_DATA | GDT_TYPE_DATA | GDT_TYPE_DATA_WRITABLE
unreal_gdt_end:

unreal_gdt_desc:
	dw unreal_gdt_end - unreal_gdt	; Limit
	dd LOAD_BASE + unreal_gdt	; Address

gdt_desc16:
	dw gdt_end - gdt	; Limit
	dd gdt			; Address


[BITS 32]
section .text32 progbits start=($-$$) vstart=(LOAD_BASE + ($-$$))
jump_to_32b:
	mov ax, 10h		; Set data and stack segments to our data descriptor
	mov ds, ax
	mov es, ax
	mov ss, ax

	mov esp, stack_begin	; Setup stack

	test byte [boot_flags1], BF1_HAS_VGA
	jz halt_loop

	mov byte [0xB8000], 'F'
	mov byte [0xB8001], 4Eh
	mov byte [0xB8002], 'I'
	mov byte [0xB8003], 4Eh
	mov byte [0xB8004], 'L'
	mov byte [0xB8005], 4Eh
	mov byte [0xB8006], 'O'
	mov byte [0xB8007], 4Eh

halt_loop:
	hlt
	jmp halt_loop

gdt:
; 0x00 - null descriptor
GDT_NULL_ENTRY

; 0x08 - code descriptor
GDT_ENTRY 0x00000000, 0xFFFFF, \
	GDT_GRANULARITY_4KB | GDT_DEFAULT_32BIT | GDT_PRESENT | GDT_RING(0) | \
	GDT_DESCRIPTOR_CODE_DATA | GDT_TYPE_CODE | GDT_TYPE_CODE_READABLE

; 0x10 - data descriptor
GDT_ENTRY 0x00000000, 0xFFFFF, \
	GDT_GRANULARITY_4KB | GDT_DEFAULT_32BIT | GDT_PRESENT | GDT_RING(0) | \
	GDT_DESCRIPTOR_CODE_DATA | GDT_TYPE_DATA | GDT_TYPE_DATA_WRITABLE
gdt_end:

gdt_desc:
	dw gdt_end - gdt	; Limit
	dd gdt			; Address

section .stack nobits start=0x0500
stack_end:
	resb 0x2000 - 0x0500
stack_begin:

section .bss start=0x2000

struc mmap_entry
	.base	resq 1
	.len	resq 1
	.type	resd 1 ; MMT_* enum
	.flags	resd 1
endstruc

MMT_RAM		equ 0x0001
MMT_RESERVED	equ 0x0002
MMT_ACPI_REC	equ 0x0003
MMT_ACPI_SAVE	equ 0x0004


boot_flags1: resb 1
BF1_HAS_VGA	equ 1 << 0


alignb 4
memmap_count: resd 1
alignb 8
memmap:	resb mmap_entry_size * MMAP_MAX_ENTRIES

; System map
; 0500 - 1FFF : Stack
; 2000 - 7DFF : Variables
; 10000 - 1FFFF : Stage 2 boot code
