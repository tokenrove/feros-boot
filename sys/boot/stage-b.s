;
; feros / stage-b.s
; Julian Squires <tek@wiw.org> / 2001
;
; Functions in this file: entry, printstr, putc, getline, tokencmp, printhelp,
;                         loadkernel, enablea20
;

	org 0x8000
	section .text
	bits 16

	; Entry point from stage A loader
entry:
	mov ax, cs
	mov ds, ax
	mov es, ax
	; save drive number and other such data
	mov [drivenum], dl
	mov [drivecurcx], cx
	mov [drivecurhead], dh
	; enable A20 (heck, why not do it now?)
	cli
	call enablea20
	sti

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

	; where we'll be copying things
	mov eax, 0x100000
	mov [kernloadpos], eax

	mov ax, 0xb800
	mov gs, ax

	; disable interrupts
	cli

	; save our segment registers
	mov ax, 0
	mov ds, ax
	mov es, ax

	push ds
	push es

	; build the GDT
	mov ax, 0
	mov es, ax
	mov di, gdt
	; null descriptor
	mov ax, 23		; 8*(number of entries)-1
	stosw
	mov eax, gdt
	stosd
	mov ax, 0
	stosw
	; linear map (flat real mode)
	mov ax, 0xffff
	stosw
	mov ax, 0
	stosw
	mov al, 0
	stosb
	mov al, 0x92
	stosb
	mov al, 0xcf
	stosb
	mov al, 0x00
	stosb
	; code
	mov ax, 0xffff
	stosw
	mov ax, 0
	stosw
	mov al, 0
	stosb
	mov al, 0x9a
	stosb
	mov al, 0xcf
	stosb
	mov al, 0x00
	stosb

	lgdt [gdt]

	mov eax, cr0
	or eax, 1		; enable PE
	mov cr0, eax
	; set our selectors to the linear map
	mov cx, 8
	mov ds, cx
	mov es, cx

	; move to ``unreal'' mode
	dec eax
	mov cr0, eax

	pop es
	pop ds

	; setup the block buffer
	mov di, workspace

	sti

	; load the kernel into 0x100000
	mov cx, 1
.copyloop:
	call loadblock
	call copyhigh
	loop .copyloop

	; now, let's go back to pmode
	cli
	lgdt [gdt]
	mov eax, cr0
	or eax, 1		; enable PE
	mov cr0, eax
	jmp 0x10:lkern32	; flush prefetch cache

	bits 32
lkern32:

;	mov [gs:2], byte 'b'
	;; update our selectors
	mov ax, 8
	mov ds, ax
	mov ss, ax
	mov es, ax

	mov [dword es:0xb8000], byte 'c'

	mov [gs:0x6C], byte 'j'
	mov [gs:0x6E], byte 'u'
	mov [gs:0x70], byte 'm'
	mov [gs:0x72], byte 'p'
	mov [gs:0x74], byte '?'

	mov al, [dword ds:0x100000]
	mov [gs:0x76], al
	mov al, [dword ds:0x100001]
	mov [gs:0x78], al
	mov al, [dword ds:0x100002]
	mov [gs:0x7A], al
	mov al, [dword ds:0x100003]
	mov [gs:0x7C], al
	mov al, [dword ds:0x100004]
	mov [gs:0x7E], al

	mov [gs:0x74], byte '!'
	jmp dword 0x10:0x100000	; jump into the kernel
	; end loadkernel
	bits 16


	
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
	; copyhigh - loads the workspace block into high memory
	; Note:	expects to be in unreal mode
	;
copyhigh:
	mov [gs:0x19A], byte 'p'

	mov cx, workspacelen
	shr cx, 1
	mov esi, workspace
	mov edi, 0x100000
	rep a32 movsw

	mov [gs:0x230], al
	mov [gs:0x232], ah
	mov ax, [dword ds:0x100000]
	mov [gs:0x234], al
	mov [gs:0x236], ah

	mov [gs:0x19C], byte 'q'
	ret
	; end copyhigh	



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
	; tries several methods, starting with BIOS int 15h/2401h
	;
enablea20:
	call testa20		; maybe it's already on?
	jnz .success

	call enablea20_bios
	call testa20
	jnz .success

	call enablea20_kbc
	call testa20
	jnz .success

	; XXXX should try port 0x92 here

.fail:	push si
	mov si, .str_fail
	call printstr
	pop si

.success:
	push si
	mov si, .str_success
	call printstr
	pop si
	ret
.str_fail: db 'Being confused you have difficulties controlling your '
	   db 'actions.',0xd,0xa,0
.str_success: db 'You succeed in forcing the lock.',0xd,0xa,0
	; end enablea20



	;
	; enablea20_kbc - attempt to enable the a20 gate via the
	; keyboard controller. (the standard way)
	;
enablea20_kbc:	
	push ax
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

	pop ax
	ret
	; end enablea20_kbc


	;
	; enablea20_bios - attempt to use the bios to enable gate a20
	;
enablea20_bios:
	push ax

	mov ax, 0x2401
	int 0x15
	; we don't actually care about the return value, since we don't
	; trust that lying bastard bios anyway.

	pop ax
	ret
	; end enablea20_bios


	;
	; testa20 - determine whether the a20 gate is on.
	; destroys the vector for int 0
	;
testa20:
	push ax
	push ds
	push es

	mov ax, 0
	mov ds, ax
	mov ax, 0xffff
	mov es, ax

	mov al, [ds:0]
	mov ah, al
	mov al, [es:0x10]

	mov al, ah
	add al, 0x42
	mov [es:0x10], al

	mov al, [es:0x10]
	cmp al, [ds:0]

	pop es
	pop ds
	pop ax
	ret
	; end testa20


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

	times 1536-($-$$) db 0

	section .bss

drivenum	resb 1
drivecurcx	resw 1
drivemaxcx	resw 1
drivecurhead	resb 1
drivemaxheads	resb 1

kernloadpos	resd 1

gdt		resb 0x30

linebuflen	equ 80
workspacelen	equ 0x1000
linebuf:
workspace:	resb workspacelen

; EOF stage-b.s