;
; Feros stage A boot loader
; Julian Squires <tek@wiw.org> / 2001
;

	org 0x7c00
	bits 16
	
; Location of the initial stack (the 8KB mark)
stackloc   equ 0x2000
; seg:offs into which we load stage-b (the 32KB mark)
stagebpos  equ 0x8000
; how many 512 blocks is stageb
stageblen equ 3

entry:  ; fix segments
	jmp 0000:.entseg
.entseg:
	mov ax, cs
	mov ds, ax
	mov es, ax

	; set up a temporary stack
	mov ax, stackloc
	mov sp, ax

	; print a message to show we're alive
	mov si, initmsg
	call printstr

	; read stage-b into memory
	; FIXME: add retries, error status dumping
	mov ax, 0
	mov es, ax
	mov ah, 2
	mov al, stageblen
	mov cx, 2
	mov bx, stagebpos
	int 0x13

	jc .error

	; jump to stage-b
	; (ensuring that cx and dx contain the relevant drive info)
	add cl, stageblen
	jmp 0x0000:stagebpos

.error:	
	mov [initmsg], ah
	mov al, ah
	and al, 0xf
	add al, '0'
	mov ah, 0xe
	mov bl, al
	int 0x10

	mov al, [initmsg]
	shr al, 4
	add al, '0'
	mov ah, 0xe
	mov bl, al
	int 0x10

	mov si, errmsg
	call printstr


.end:	hlt
	jmp .end


; print a string located in ds:si
printstr:
	lodsb
	cmp al, 0
	je .l1
	mov ah, 0xe
	mov bl, al
	mov cx, 1
	int 0x10
	jmp printstr
.l1:	ret

initmsg: db 'feros > dedicated to future pioneers',0xd,0xa,0,0,0
errmsg:  db 'Well, you ran into something and the game is over.',0xd,0xa,0,0,0

	times 510-($-$$) db 0
signature: dw 0xAA55

;; EOF stage-a.s