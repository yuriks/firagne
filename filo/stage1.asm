[map symbols]

[CPU X64]
[BITS 16]
section .text start=0x7C00
	cli
	jmp 0x0000:boot_loader
boot_loader:
	xor ax, ax		; Set segments to 0x0000
	mov ds, ax
	mov es, ax
	mov ax, 0x0050
	mov ss, ax
	mov sp, stack_begin	; Setup stack
	sti

	mov byte [boot_device], dl	; Store boot device for later use

	test dl, 0x80 ; 0x80 bit indicates hard disk
	jnz .fail_not_floppy

	push es

	mov ah, 0x08		; int 13h,ah=08h: DISK - GET DRIVE PARAMETERS
	mov di, read_buffer
	int 0x13
	jc .fail_disk_settings
	and cl, 00111111b	; Mask out cylinder number bits
	mov byte [disk_sectors_per_track], cl
	inc dh
	mov byte [disk_num_heads], dh

	pop es			; Restore register clobbered by previous call

	; Read ext2 superblock
	mov al, 1
	mov cx, 0x0003		; Cylinder 0, Sector 3
	xor dh, dh		; Head 0
	mov bx, read_buffer
	call read_sectors

	cmp word [read_buffer+56], 0xEF53	; Verify ext2 magic number
	jne .fail_ext2

	mov ax, [read_buffer+40]
	mov word [ext2_inodes_per_group], ax

	mov cl, byte [read_buffer+24]
	mov ax, 2
	shl ax, cl
	inc cl
	mov byte [ext2_block_size_shift], cl
	mov byte [ext2_block_size], al
	cmp ax, 8
	jg .fail_ext2_parameter
	shl ax, 9
	mov word [ext2_block_size_bytes], ax

	mov cx, 128
	cmp word [read_buffer+76], 1	; Verify major version
	jl .pass_check		; Ver. < 1.0 doesn't have extended superclock
	mov cx, word [read_buffer+88]	; read inode size
	test dword [read_buffer+96], ~0x2	; Required features, only "Directories type-field" supported
	jz .pass_check

	jmp .fail_ext2_parameter

.pass_check:
	mov word [ext2_inode_size], cx

	; We want inode 2, the root: /
	mov ax, 2
	call read_inode
	push ds
	push ds
	mov bp, 0x1000
	mov es, bp
	call read_inode_data
	pop es

	mov ds, bp
	; Parse directory and look for our kernel
	mov si, file_read_buf + 8
	jmp .first_dir

.next_dir:
	pop si
	mov cx, word [si + (4-8)]
	add si, cx			; Go to next directory entry
	sub dx, cx
	jz .fail_kernel_not_found
.first_dir:
	push si
	movzx cx, byte [si + (6-8)]	; Get filename len
	cmp cx, kernel_name_size
	jne .next_dir

	mov di, kernel_name
	repe cmpsb
	jne .next_dir

	pop si
	mov ax, word [si + (0-8)]	; Get inode

.final_file_read:
	mov di, ds
	pop ds
	call read_inode
	mov es, di
	call read_inode_data

	jmp 0x1000:0x0000

; Not booting from a floppy
.fail_not_floppy:
	mov dl, 'F'
	jmp fail16b

; Failed to get disk geometry from BIOS
.fail_disk_settings:
	mov dl, 'D'
	jmp fail16b

; ext2 format error
.fail_ext2:
	mov dl, '2'
	jmp fail16b

; ext2 unsupported parameters
.fail_ext2_parameter:
	mov dl, 'P'
	jmp fail16b

.fail_kernel_not_found:
	mov dl, 'K'
	jmp fail16b

; Print '!' and char in dl and then halt
fail16b:
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0
	mov al, '!'
	int 10h
	mov al, dl
	int 10h
.inf_loop:
	hlt
	jmp .inf_loop

; Read inode entry from disk
; ax = inode number
; out si = start of inode struct
; ax, bx, dx, si, trashed
read_inode:
	dec ax
	xor dx, dx
	; block_group (ax, si) = (inode-1) / inodes_per_group
	; index (dx) = (inode-1) % inodes_per_group
	div word [ext2_inodes_per_group]
	mov si, ax

	mov ax, 1			; Block Group Descriptor is at block 1
	cmp byte [ext2_block_size_shift], 1  ; Except if block size is 1024
	jne .dont_assign
	mov ax, 2			; Then it's at block 2
.dont_assign:

	; Read block group descriptor table
	mov bx, read_buffer
	call read_block

	shl si, 5		; ax << log2(32)
	mov si, word [read_buffer + si + 8]	; Starting block adress of inode table

	mov ax, dx
	; block (ax) = (index * inode_size) / block_size_bytes
	mul word [ext2_inode_size]
	div word [ext2_block_size_bytes]
	add ax, si
	call read_block

	mov si, dx
	add si, read_buffer
	ret


; Read file contents of inode into file_read_buf
; si = input inode
; out dx = file size in bytes
; ax, bx, cx, si, di, trashed
read_inode_data:
	mov di, file_read_buf
	; Read file size
	mov edx, dword [si + 4]
	cmp edx, 0x10000	; 64kB max file size
	jg .fail_file_too_large
	push dx

	; Move si to the first direct block pointer
	add si, 40
	mov cx, 12

.direct_loop:
	mov ax, word [si]	; Read block adress
	add si, 4
	test ax, ax
	jz .reading_end
	mov bx, di
	call read_block
	add di, word [ext2_block_size_bytes]
	loop .direct_loop

	mov ax, word [si]
	push es
	xor bx, bx
	mov es, bx
	mov bx, read_buffer
	call read_block
	pop es

	mov si, read_buffer
	mov cx, 256
	jmp .direct_loop

.reading_end:
	; Pop file size
	pop dx
	ret

.fail_file_too_large:
	mov dl, 'S'
	jmp fail16b


; Reads a ext2 block to the buffer
; ax = block number
; bx = buffer to read into
read_block:
	pusha
	; Convert ext2 blocks to disk sectors
	mov cl, byte [ext2_block_size_shift]
	shl ax, cl
	call lba_to_chs
	mov al, byte [ext2_block_size]
	call read_sectors
	popa
	ret


; Reads sectors from a disk
; Limited to cylinder < 256
; al = number of sectors to read
; ch = cylinder/track number (0-255)
; cl = sector number (1-63)
; dh = head number (0-1)
; bx = buffer to read into
; ax, dl, trashed
read_sectors:
	mov dl, byte [boot_device]
	push si
	mov ah, 0x02
	push ax
	mov si, 4		; 4 tries
	jmp .first_read

.retry_read:
	dec si
	jz .fail_io

	xor ax, ax		; int 13h,ah=00h: DISK - RESET DISK SYSTEM
	int 0x13

.first_read:
	;mov ah, 0x02		; int 13h,ah=02h: DISK - READ SECTOR(S) INTO MEMORY
	pop ax
	push ax
	int 0x13
	jc .retry_read		; Read error

	add sp, 2
	pop si
	ret

; I/O failure
.fail_io:
	mov dl, 'I'
	jmp fail16b


; Converts a LBA address to a CHS adress tuple
; ax = LBA address
; out ch = cylinder
; out cl = sector
; out dh = head
lba_to_chs:
	; temp = LBA / sectors_per_track
	; sector (cl) = (LBA % sectors_per_track) + 1
	div byte [disk_sectors_per_track]
	inc ah
	mov cl, ah
	; head = temp % num_heads
	; cylinder = temp / num_heads
	movzx ax, al
	div byte [disk_num_heads]
	mov dh, ah
	mov ch, al
	ret

kernel_name_size equ 15
kernel_name: db 'stage2-boot.bin'

end_of_code:
times 510-($-$$) db 0
	dw 0xAA55

section .stack nobits start=0x0000
stack_end:
	resb 0x1000
stack_begin:

section .bss nobits start=0x1500

read_buffer: resb 4096

ext2_inodes_per_group: resw 1
ext2_inode_size: resw 1 ; in bytes
ext2_block_size_bytes: resw 1 ; in bytes, obviously
disk_sectors_per_track: resb 1
disk_num_heads: resb 1
ext2_block_size_shift: resb 1 ; sector = (block << ext2_block_size)
ext2_block_size: resb 1 ; in sectors

boot_device: resb 1

section .file_read_buf nobits start=0x0 ;010000
file_read_buf:

; System map
; 0500 - 14FF : Stack
; 1500 - 7BFF : Variables and buffers
; 7C00 - 7DFF : Boot code
; 10000 -      : File reading buffer
