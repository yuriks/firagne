[BITS 16]
[ORG 0x7C00]
boot_loader:
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'M'
	int 10h

read_mmap:
	xor ax, ax		; Set DS and ES to 0000h
	mov ds, ax
	mov es, ax

	mov word [memmap_size], 1000h

SMAP_magic equ 0534D4150h

	mov eax, 0E820h		; int 15h; GET SYSTEM MEMORY MAP
	mov edx, SMAP_magic	; 'SMAP'
	mov ebx, 0			; Beginning of map
	movzx ecx, word [memmap_size]	; Buffer size
	mov di, memmap		; Destination buffer

.rmap_loop:
	int 15h
	jc .fail_int

	cmp eax, SMAP_magic
	jne .fail_magic

	mov eax, 0E820h
	mov edx, SMAP_magic

	add di, cx
	sub [memmap_size], ecx

	or ebx, ebx
	jz .rmap_done

	jmp .rmap_loop

.fail_int:
	mov dl, 'I'
	jmp fail16b

.fail_magic:
	mov dl, 'M'
	jmp fail16b

.rmap_done:
	mov eax, 1000h
	sub eax, [memmap_size]
	mov [memmap_size], eax

	jmp enable_pmode


fail16b:
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, '!'
	int 10h
	mov al, dl
	int 10h

	jmp $


enable_pmode:
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'P'
	int 10h

	cli

	xor ax, ax		; Set DS to 0000h
	mov ds, ax

	lgdt [gdt_desc] ; Load GDT from 0000(DS):gdt_desc

	mov eax, cr0	; Enable protection
	or eax, 1
	mov cr0, eax

	jmp 08h:jump_to_32b	; Jump to our code segment

[BITS 32]
jump_to_32b:
	mov ax, 10h		; Set up DS and SS to our data segment
	mov ds, ax
	mov ss, ax

	mov esp, 090000h	; Reserve some FFFFh bytes for the stack

	mov byte [0B8000h], 'F'
	mov byte [0B8001h], 4Eh
	mov byte [0B8002h], 'I'
	mov byte [0B8003h], 4Eh
	mov byte [0B8004h], 'L'
	mov byte [0B8005h], 4Eh
	mov byte [0B8006h], 'O'
	mov byte [0B8007h], 4Eh

	jmp $

gdt:
gdt_null: ; 00h
	dd 0
	dd 0

gdt_code: ; 08h
	dw 0FFFFh		; Limit 0:15
	dw 00000h		; Base 0:15
	db 000h			; Base 16:23
	db 10011010b	; Seg present + ring 0 + !system seg + code read/exec
	db 11001111b	; 4kb granularity + 32-bit + 0xF limit
	db 000h			; Base 24:31

gdt_data: ; 16h
	dw 0FFFFh		; Limit 0:15
	dw 00000h		; Base 0:15
	db 000h			; Base 16:23
	db 10010010b	; Seg present + ring 0 + !system seg + data read/write
	db 11001111b	; 4kb granularity + 32-bit + 0xF limit
	db 000h			; Base 24:31
gdt_end:

gdt_desc:
	dw gdt_end - gdt	; Limit
	dd gdt				; Address

times 510-($-$$) db 0
	dw 0AA55h

[map symbols]
section .bss vstart=07e00h

struc mmap_entry
	.base	resq 1
	.len	resq 1
	.type	resd 1
endstruc

memmap_size:
	resw 1
memmap:
	resb 01000h

; System map
; 7C00 - 7DFF : Boot code
; 7E00 - 
