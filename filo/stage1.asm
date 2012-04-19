[map symbols]

[CPU X64]
[BITS 16]
section .bpb nobits start=0x7C00
bpb_jmp: resb 3
bpb_oem_id: resb 8
bpb_bytes_per_sector: resw 1
bpb_sectors_per_cluster: resb 1
bpb_reserved_sectors: resw 1
bpb_num_fats: resb 1
bpb_rootdir_size: resw 1
bpb_num_sectors: resw 1
bpb_media_descriptor: resb 1
bpb_sectors_per_fat: resw 1
bpb_sectors_per_track: resw 1
bpb_num_heads: resw 1
bpb_hidden_sectors: resd 1
bpb_num_sectors32: resd 1

ebpb_start:
ebpb_drive_number: resb 1
ebpb_reserved: resb 1
ebpb_signature: resb 1 ; Must be 0x28 or 0x29
ebpb_serial_num: resd 1
ebpb_label: resb 11
ebpb_system_id: resb 8

section .text start=0x7C00+62
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

	mov byte [ebpb_drive_number], dl	; Store boot device for later use

	test dl, 0x80 ; 0x80 bit indicates hard disk
	jnz fail_not_floppy

	push es

	; Read disk geometry info
	mov ah, 0x08		; int 13h,ah=08h: DISK - GET DRIVE PARAMETERS
	mov di, read_buffer
	int 0x13
	jc fail_disk_settings
	and cx, 00111111b	; Mask out cylinder number bits
	mov word [bpb_sectors_per_track], cx
	mov dl, dh
	xor dh, dh
	inc dx
	mov word [bpb_num_heads], dx

	pop es			; Restore register clobbered by previous call

	; Calculate size in sectors of root directory
	mov ax, word [bpb_rootdir_size]
	shl ax, 5 ; ax *= 32, directory entry size
	; Add dividend - 1. Makes division round up
	mov si, word [bpb_bytes_per_sector]
	add ax, si
	dec ax
	xor dx, dx
	div si
	mov cx, ax

	; Calculate root directory sector number
	; root dir sector = reserved sectors + (num fats * sectors/fat)
	mov al, byte [bpb_num_fats]
	mul word [bpb_sectors_per_fat] ; ax = al * word
	add ax, word [bpb_reserved_sectors]

	push ax

	; Calculate start of data area and store for later use
	mov bx, ax
	add bx, cx
	mov word [data_area_start], bx

	xor dx, dx
	mov ax, word [bpb_num_sectors]
	test ax, ax
	jnz .has_num_sectors
	mov ax, word [bpb_num_sectors32]
	mov dx, word [bpb_num_sectors32+2]
.has_num_sectors:

	div word [bpb_sectors_per_cluster]
	
	cmp ax, 4085
	setnb byte [fat_type]
	cmp ax, 65525
	jnb fail_fat ; Is FAT32

	pop ax
.dir_sector_read_loop:
	push cx

	; Read sector from root directory
	push ax
	call lba_to_chs
	mov bx, read_buffer
	mov di, bx
	mov al, 1
	call read_sectors
	pop ax
	inc ax ; Advance to next sector

	; Scan directory for kernel file
	xor bp, bp
.check_entry_loop:
	lea di, [read_buffer + bp]

	test byte [di + 11], 0x08 ; Check for VOLUME_ID flag
	jnz .skip_entry
	cmp byte [di], 0xE5 ; Check for 'blank entry' code
	je .skip_entry
	cmp byte [di], 0x00 ; Check for 'end of list' code
	je .end_entry_loop

	; Compare filename
	mov cx, 11
	mov si, kernel_name
	rep cmpsb
	je .kernel_found

.skip_entry:
	add bp, 32
	cmp bp, word [bpb_bytes_per_sector]
	jb .check_entry_loop

	pop cx
	loop .dir_sector_read_loop

.end_entry_loop:
	; File wasn't found
	jmp fail_kernel_not_found

.kernel_found:
	mov bp, word [di - 11 + 26] ; Get starting cluster

	test bp, bp
	jz fail_kernel_not_found ; Zero-size kernel?

	; Read kernel data
	xor di, di ; di = file_read_buf
.cluster_read_loop:
	mov si, 0x1000
	mov es, si

	; Calculate sector
	mov ax, bp
	sub ax, 2
	movzx bx, byte [bpb_sectors_per_cluster]
	mul bx
	add ax, word [data_area_start]

	call lba_to_chs
	mov al, bl
	push bx
	mov bx, di
	call read_sectors

	; Advance dest pointer by cluster size in bytes
	pop ax
	mul word [bpb_bytes_per_sector]
	add di, ax

	; Calculate sector/offset into FAT for read cluster
	mov ax, bp
	test byte [fat_type], 1 ; If fat_type == FAT_TYPE_16
	jnz .calc_cluster_fat16
	; fat_type == FAT_TYPE_12
	shr ax, 1
	add ax, bp
	jmp .calc_cluster_end
.calc_cluster_fat16:
	; fat_type == FAT_TYPE_16
	shl ax, 1
.calc_cluster_end:
	xor dx, dx
	div word [bpb_bytes_per_sector]
	add ax, word [bpb_reserved_sectors]

	; ax = sector number
	; dx = byte offset

	push dx

	call lba_to_chs
	xor ax, ax ; Reset es back to .bss
	mov es, ax
	mov al, 2 ; 2 instead of 1 handles the FAT12 edge case
	          ; where the entry crosses sectors
	mov bx, read_buffer
	call read_sectors

	pop bx
	mov bx, word [read_buffer + bx]

	mov ax, 0xFFF8 ; FAT16 EOC value
	test byte [fat_type], 1
	jnz .interp_cluster_fat16
	; fat_type == FAT_TYPE_12
	mov ax, 0xFF8 ; FAT12 EOC value
	test bp, 1
	jnz .odd_cluster
	and bx, 0xFFF
	jmp .interp_cluster_fat16
.odd_cluster:
	shr bx, 4
.interp_cluster_fat16:
	; Don't need anything for FAT16

	mov bp, bx

	cmp bp, ax ; Is next cluster an EOC value?
	jb .cluster_read_loop

	; Jump to loaded kernel
	push si ; 0x1000 from above
	push 0
	retf


; Reads sectors from a disk
; Limited to cylinder < 256
; al = number of sectors to read
; ch = cylinder/track number (0-255)
; cl = sector number (1-63)
; dh = head number (0-1)
; es:bx = buffer to read into
; ax, dl, trashed
read_sectors:
	mov dl, byte [ebpb_drive_number]
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

	pop si
	pop si
	ret

; I/O failure
.fail_io:
	mov al, 'I'
	jmp fail16b


; Converts a LBA address to a CHS adress tuple
; ax = LBA address
; out ch = cylinder
; out cl = sector
; out dh = head
; clobbers ax, dx
lba_to_chs:
	; temp = LBA / sectors_per_track
	; sector (cl) = (LBA % sectors_per_track) + 1
	xor dx, dx
	div word [bpb_sectors_per_track]
	mov cl, dl
	inc cl
	; head = temp % num_heads
	; cylinder = temp / num_heads
	xor dx, dx
	div word [bpb_num_heads]
	mov dh, dl
	mov ch, al
	ret


;;;;;;;;;;;;;;;;;;
; Error Handlers ;
;;;;;;;;;;;;;;;;;;

; Not booting from a floppy
fail_not_floppy:
	mov al, 'F'
	jmp fail16b

; Failed to get disk geometry from BIOS
fail_disk_settings:
	mov al, 'D'
	jmp fail16b

; fat format error
fail_fat:
	mov al, 'T'
	jmp fail16b

fail_kernel_not_found:
	mov al, 'K'
	;jmp fail16b ; Let fall through

; Print chars al,'!' and then halt
fail16b:
	mov ah, 0Eh
	mov bh, 0Fh
	mov bl, 0

	int 10h
	mov al, '!'
	int 10h
.inf_loop:
	hlt
	jmp .inf_loop

kernel_name: db 'FILO-STAGE2' ; FILO_STA.GE2

end_of_code:
times 448-($-$$) db 0
	dw 0xAA55

section .stack nobits start=0x0000
stack_end:
	resb 0x100
stack_begin:

section .bss nobits start=0x1100

read_buffer: resb 4096

data_area_start: resw 1

FAT_TYPE_12 equ 0
FAT_TYPE_16 equ 1
FAT_TYPE_32 equ 2
fat_type: resb 1

section .file_read_buf nobits start=0x0 ;010000
file_read_buf:

; System map
; 0500 - 14FF : Stack
; 1500 - 7BFF : Variables and buffers
; 7C00 - 7DFF : Boot code
; 10000 -      : File reading buffer
