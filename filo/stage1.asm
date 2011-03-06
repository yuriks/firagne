[map symbols]

FLOPPY_NCYLINDER equ 80
FLOPPY_NHEADS equ 2
FLOPPY_NSECTORS equ 18

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

	mov byte [boot_device], dl	; Store boot device for later use

detect_video:
	and byte [boot_flags1], ~BF1_HAS_VGA	; Clear 'Has VGA' bit
	mov ax, 0x1A00		; int 10h,ax=1A00: GET DISPLAY COMBINATION CODE
	int 0x10
	cmp al, 0x1A
	jne .no_video

	or byte [boot_flags1], BF1_HAS_VGA	; We have a VGA

	mov ax, 0x0002		; int 10h,ah=00: SET VIDEO MODE (mode 02h 80x25)
	int 0x10

	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'V'
	int 10h
.no_video:

read_drive_info:
	test byte [boot_flags1], BF1_HAS_VGA
	jz .no_video

	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, 'S'
	int 10h
.no_video:
	
	mov dl, byte [boot_device]
	test dl, 0x80 ; 0x80 bit indicates hard disk
	jnz .fail_not_floppy

	mov cx, 0x0003		; Cylinder 0, Sector 3
	xor dh, dh		; Head 0
	mov bx, read_buffer
	call read_sector

	cmp word [read_buffer+56], 0xEF53	; Verify ext2 magic number
	jne .fail_ext2

	jmp print_message

; Not booting from a floppy
.fail_not_floppy:
	mov dl, 'F'
	jmp fail16b

; ext2 format error
.fail_ext2:
	mov dl, '2'
	jmp fail16b

print_message:
	test byte [boot_flags1], BF1_HAS_VGA
	jz halt_loop

	mov ax, 0xB000
	mov es, ax
	mov byte [es:0x80A0], 'F'
	mov byte [es:0x80A1], 4Eh
	mov byte [es:0x80A2], 'I'
	mov byte [es:0x80A3], 4Eh
	mov byte [es:0x80A4], 'L'
	mov byte [es:0x80A5], 4Eh
	mov byte [es:0x80A6], 'O'
	mov byte [es:0x80A7], 4Eh

halt_loop:
	hlt
	jmp halt_loop


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


; Reads one sector from a disk
; Limited to cylinder < 256
; ch = cylinder/track number (0-255)
; cl = sector number (1-63)
; dh = head number (0-1)
; dl = disk id
; es:bx = destination buffer
; ax, si trashed
read_sector:
	mov si, 4		; 4 tries
	jmp .first_read

.retry_read:
	dec si
	jz .fail_io

	xor ax, ax		; int 13h,ah=00h: DISK - RESET DISK SYSTEM
	int 0x13
.first_read:
	mov ax, 0x0201		; int 13h,ah=02h: DISK - READ SECTOR(S) INTO MEMORY
	mov cx, 0x0003
	xor dh, dh
	mov bx, read_buffer
	int 0x13
	jc .retry_read		; Read error

	cmp al, 1
	jne .fail_io		; Incorrect number of sectors read

	ret

; I/O failure
.fail_io:
	mov dl, 'I'
	jmp fail16b


times 510-($-$$) db 0
	dw 0xAA55

section .stack nobits start=0x0500
stack_end:
	resb 0x7C00 - 0x0500
stack_begin:

section .bss start=0x7E00

read_buffer: resb 512

boot_device: resb 1
boot_flags1: resb 1
BF1_HAS_VGA	equ 1 << 0


; System map
; 0500 - 7BFF : Stack
; 7C00 - 7DFF : Boot code
; 7E00 -
