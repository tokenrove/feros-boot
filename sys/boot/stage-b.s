;
; feros / stage-b.s
; Julian Squires <tek@wiw.org> / 2001
;
; Functions in this file: entry, printstr, putc, getline, tokencmp, printhelp,
;                         loadkernel, enablea20
;

	org 0x8000
	bits 16
	section .text

	; Entry point from stage A loader
entry:
	mov ax, cs
	mov ds, ax
	mov es, ax
	; save drive number and other such data
	mov [drivenum], dl
	mov [drivecurcx], cx
	mov [drivecurhead], dh

	;
	; provide boot prompt
	;
prompt:
	mov si, string_prompt
	call printstr

	; get a line of input
	mov di, linebuf
	mov cx, linebuflen
	call getline

	; ``help''
	mov di, string_help
	mov si, linebuf		; resets pointer back to beginning of linebuf
	call tokencmp
	jnz .boot
	mov si, helptext
	call printstr		; print some help and then back to the prompt
	jmp prompt

.boot:	; ``boot x''
	mov di, string_boot
	mov si, linebuf
	call tokencmp
	jnz .mem
	call printstr		; dump the remainder of the line to screen
	call loadkernel
	;; if loadkernel failed, something went wrong
	mov si, string_loadkernfailed
	call printstr

.mem:	; ``mem''
	mov di, string_mem
	mov si, linebuf
	call tokencmp
	jnz prompt
	call checkmem

	jmp prompt
	; end prompt/entry


	
	; 
	; loadkernel - loads the kernel into 0x100000
	; If the function returns at all, it has failed.
	; Expects the drive number in drivenum and the
	; starting CHS in drivecurcx/drivecurhead.
	;
loadkernel:
	; get drive parameters
	call getdriveparams

	; setup the copy GDT
	mov si, gdt
	mov di, si

	; skip the first sixteen bytes
	mov ax, 0
	mov cx, 8
	rep stosw

	; source
	mov ax, workspacelen
	dec ax
	stosw
	mov ax, workspace
	stosw
	mov al, 0
	stosb
	mov al, 0x93		; present = 1, privlevel = 0, type = 3
	stosb
	mov ax, 0
	stosw

	; destination
	mov ax, workspacelen
	dec ax
	stosw
	mov ax, 0
	stosw
	mov al, 0x10		; 0x100000 is where we start
	stosb
	mov al, 0x93		; present, privlevel 0, dataseg, writable
	stosb
	mov ax, 0
	stosw

	mov ax, 0
	mov cx, 8
	rep stosw

	; setup the block buffer
	mov di, workspace

	; read the first block
	call loadblock
	;; check ELF header
	;; determine how many blocks we need to copy
	;; copy remainder of first block
	call copyhigh

	;; loop
;	mov cx, 1
;.copyloop:
;	call loadblock
;	call copyhigh
;	loop .copyloop

	; disable interrupts
	cli

	mov ax, 0xb800
	mov gs, ax
	mov [gs:0], byte '*'

	; build the GDT
	mov di, gdt
	; null descriptor
	mov ax, 15
	stosw
	mov ax, gdt
	stosw
	mov ax, 0
	stosw
	stosw
	; linear map (flat real mode)
	mov ax, 0xffff
	stosw
	mov ax, 0
	stosw
	mov ax, 0x9200
	stosw
	mov ax, 0x00cf
	stosw

	o32 lgdt [gdt]

	mov [gs:2], byte '|'

	; move to pmode
	mov eax, cr0
	or eax, 1
	jmp .next		; clear the prefetch queue with a jump
.next:
	mov cx, 8
	mov ds, cx
	mov es, cx

	mov [gs:4], byte '?'

	; enable A20
	call enablea20

	; reroute irqs

	mov [gs:6], byte '+'

	jmp 0x100000		; jump into the kernel
	; end loadkernel


	
	;
	; loadblock - loads a 4k block into workspace from the drive
	;
loadblock:
	push ax
	push bx
	push cx
	push dx
	push es

	mov ax, ds
	mov es, ax
	mov bx, workspace
	mov ax, workspacelen
	shr ax, 9
	mov ah, 2

	mov cx, [drivecurcx]
	mov dh, [drivecurhead]
	mov dl, [drivenum]
	int 0x13

	jc .fail

	add cl, al
	adc ch, 0
	adc dh, 0

	mov al, '.'
	call putc

	jmp .end
	
.fail:	mov si, .str_fail
	call printstr

.end:	pop es
	pop dx
	pop cx
	pop bx
	pop ax	
	ret
.str_fail: db 'loadblock failed',0xd,0xa,0
	; end loadblock
	

	;
	; getdriveparams - fills the drivemaxcx and drivemaxhead
	;                  values
	; Expects the drive to use to be in drivenum
	;
getdriveparams:
	mov ax, 0
	mov es, ax		; guard against bios bugs apparently?
	mov di, ax
	mov ah, 8
	mov dl, [drivenum]
	int 0x13

	jc .fail
	mov [drivemaxcx], cx
	mov [drivemaxheads], dh
	jmp .end

.fail:	mov si, .str_fail
	call printstr
.end:	ret
.str_fail: db 'The BIOS bites! You die... --More--',0xd,0xa,0
	; end getdriveparams



	;
	; copyhigh - loads the workspace block into high memory using
	;            int 15h
	;
copyhigh:
	push ax
	push cx
	push si
	push es

	mov ax, 0
	mov es, ax
	mov si, gdt
	mov ah, 0x87
	mov cx, workspacelen
	shr cx, 1
	int 0x15

	call printword
	xor ax, ax
	int 0x16

	jc .fail

	mov di, gdt+0x1a
	mov si, di
	lodsw
	mov cl, [gdt+0x1c]
	add ax, workspacelen
	adc cl, 0
	stosw
	mov al, cl
	stosb
	jmp .end

.fail:	mov si, .str_elbereth
	call printstr

.end:
	pop es
	pop si
	pop cx
	pop ax
	ret
.str_elbereth: db 'You can',0x27,'t seem to think straight...',0xd,0xa,0
	; end copyhigh


	;
	; checkmem - determines how much extended RAM is in this machine
	;
checkmem:
	push ax
	push si

	mov ah, 0x88
	int 0x15
	jc .fail
	push ax
	mov si, .str_pre
	call printstr
	pop ax
	call printword
	mov si, .str_post
	call printstr
	jmp .end
.fail:	mov si, .str_fail
	call printstr

.end:	pop si
	pop ax
	ret
.str_pre:	db 'Extended memory: ',0
.str_post:	db 0xd,0xa,0
.str_fail:	db 0xd,0xa,'Who was that Maud person anyway?',0xd,0xa,0
	; end checkmem


	
	;
	; printstr - print a string to the console
	; Takes: a pointer to the string in DS:SI
	;
printstr:
	push ax
	push si
.l0:	lodsb
	cmp al, 0
	je .l1
	call putc
	jmp .l0
.l1:	pop si
	pop ax
	ret
	; end printstr


	;
	; putc - print a single character
	; Takes: the character in AL
	;
putc:
	push ax
	push bx
	push cx

	mov ah, 0xe
	mov bl, al
	mov cx, 1
	int 0x10

	pop cx
	pop bx
	pop ax
	ret
	; end putc
	

	;
	; printword - print a word in hex
	; Takes: the word to print in AX
	; Tries to be at least a bit civil and avoids destroying
	; anything.
	;
printword:
	push cx
	push bx
	push ax

	mov bx, hextoascii
	shr ax, 12
	xlatb
	call putc

	pop ax
	push ax
	mov bx, hextoascii
	shr ax, 8
	and ax, 0xf
	xlatb
	call putc

	pop ax
	push ax
	mov bx, hextoascii
	shr ax, 4
	and ax, 0xf
	xlatb
	call putc

	pop ax
	push ax
	mov bx, hextoascii
	and ax, 0xf
	xlatb
	call putc

	pop ax
	pop bx
	pop cx	
	ret
	; end printword


	
	;
	; tokencmp - Compare a word in ES:DI with the first word in DS:SI
	;
	; Returns with Z set if the strings match, and SI is left pointing
	; at the end of the token or the point at which they did not match.
	;
	; Destroys AL
	;
tokencmp:
.skipwspace:
	lodsb
	cmp al, ' '		; skip spaces
	jz .skipwspace

	; now we should be on the token. so we compare until we hit
	; whitespace or NUL
.compare:
	cmp al, ' '		; space marks the end of token
	jz .hitend
	cmp al, 0		; as does NUL
	jz .hitend
	; otherwise, compare al and ES:DI. if they don't match, return
	; non-zero.
	scasb
	jnz .end
	lodsb			; load the next character
	jmp .compare

.hitend:
	mov al, 0		; if this is the end of the string in ES:DI,
	scasb			; set the zero flag
.end:	ret
	; end tokencmp


	
	; 
	; enablea20 - set A20 gate on
	; Destroys: AX
	;
enablea20:
	; send ``read output port'' to the kbd control register
	mov al, 0xd0
	out 0x64, al

	; iodelay
	out 0xed, ax

	; read in whatever's in the output port
	in al, 0x60
	mov ah, al

	; send ``write output port'' to the kbd control register
	mov al, 0xd1
	out 0x64, al

	; iodelay
	out 0xed, ax

	; gate A20
	mov al, 0xdf
	out 0x60, al

	ret
	; end enablea20


	
	;
	; getline - get a line into [ES:DI]
	; Takes the line buffer in ES:DI, the max string length in CX
	;
getline:
	push ax
	push di
.l0:	xor ax, ax
	int 0x16
	call putc
	cmp al, 0xd
	jz .end
	;dec cx
	;jz .end
	stosb
	jmp .l0
.end:	mov al, 0
	stosb

	pop di
	pop ax
	ret
	; end getline

	

	;
	; Additional data
	;
string_prompt:	db 0xd,0xa,'> ',0
string_help:	db 'help',0
string_boot:	db 'boot',0
string_mem:	db 'mem',0
string_loadkernfailed: db 0xd,0xa,'You and your $42 have been eaten.',0xd,0xa,0
hextoascii:	db '0123456789abcdef'
helptext:	db 0xd,0xa,'stage-b, Julian Squires <tek@wiw.org> / 2001'
	        db 0xd,0xa,'Commands: boot mem help',0xd,0xa,0

	times 1024-($-$$) db 0

	section .bss

drivenum	resb 1
drivecurcx	resw 1
drivemaxcx	resw 1
drivecurhead	resb 1
drivemaxheads	resb 1

gdt		resb 0x30

linebuflen	equ 80
workspacelen	equ 0x1000
linebuf:
workspace:	resb workspacelen

; EOF stage-b.s