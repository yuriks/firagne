MMAP_MAX_ENTRIES equ 32

[BITS 16]
section .text start=0x7C00
	cli
	jmp 0x0000:boot_loader
boot_loader:
	xor ax, ax		; Set segments to 0x0000
	mov ds, ax
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

	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'M'
	int 10h
.no_video:

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
.fail_int:
	mov dl, 'I';nterrupt
	jmp fail16b

; Failure due to running out of mmap entries
.fail_out_of_space:
	mov dl, 'B';uffer
	jmp fail16b

.rmap_done:
	mov ax, MMAP_MAX_ENTRIES	; entries read = max entries - free entries
	sub ax, word [memmap_count]
	mov word [memmap_count], ax

	jmp enable_pmode


; Print '!' and char in dl and then halt
fail16b:
	test byte [boot_flags1], BF1_HAS_VGA
	jz .inf_loop

	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, '!'
	int 10h
	mov al, dl
	int 10h

.inf_loop:
	jmp $


enable_pmode:
	test byte [boot_flags1], BF1_HAS_VGA
	jz .no_video

	; Print 'P'
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'P'
	int 10h
.no_video:

	cli

	lgdt [gdt_desc]		; Load GDT from 0000(DS):gdt_desc

	mov eax, cr0		; Enable protection
	or eax, 1
	mov cr0, eax

	jmp 08h:jump_to_32b	; Jump to our code segment

[BITS 32]
jump_to_32b:
	mov ax, 10h		; Set data and stack segments to our data descriptor
	mov ds, ax
	mov es, ax
	mov ss, ax

	mov esp, stack_begin	; Setup stack
	sti

	test byte [boot_flags1], BF1_HAS_VGA
	jz halt_loop

	mov byte [0B80A0h], 'F'
	mov byte [0B80A1h], 4Eh
	mov byte [0B80A2h], 'I'
	mov byte [0B80A3h], 4Eh
	mov byte [0B80A4h], 'L'
	mov byte [0B80A5h], 4Eh
	mov byte [0B80A6h], 'O'
	mov byte [0B80A7h], 4Eh

halt_loop:
	hlt
	jmp halt_loop

gdt:
gdt_null: ; 00h
	dd 0
	dd 0

gdt_code: ; 08h
	dw 0FFFFh		; Limit 0:15
	dw 00000h		; Base 0:15
	db 000h			; Base 16:23
	db 10011010b		; Seg present + ring 0 + !system seg + code read/exec
	db 11001111b		; 4kb granularity + 32-bit + 0xF limit
	db 000h			; Base 24:31

gdt_data: ; 16h
	dw 0FFFFh		; Limit 0:15
	dw 00000h		; Base 0:15
	db 000h			; Base 16:23
	db 10010010b		; Seg present + ring 0 + !system seg + data read/write
	db 11001111b		; 4kb granularity + 32-bit + 0xF limit
	db 000h			; Base 24:31
gdt_end:

gdt_desc:
	dw gdt_end - gdt	; Limit
	dd gdt			; Address

times 510-($-$$) db 0
	dw 0xAA55

[map symbols]
section .stack nobits vstart=0x0500
stack_end:
	resb 0x7C00 - 0x0500
stack_begin:
section .bss vstart=0x7E00

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


boot_flags1:
	resb 1
BF1_HAS_VGA	equ 1 << 0


memmap_count:
	resd 1
memmap:
	resb mmap_entry_size * MMAP_MAX_ENTRIES

; System map
; 0500 - 7BFF : Stack
; 7C00 - 7DFF : Boot code
; 7E00 -
