;
; feros / entry.s
; Julian Squires <tek@wiw.org> / 2001
;

	use32
	org 0x100000

	global _start

	; Kernel entry point, from stage B loader.
_start:
	; XXX print something then halt
	mov eax, 0xb8000
	mov [eax], byte 0x66
	mov [eax+81], byte 0x6f
	mov [eax+162], byte 0x6f
	
.hlt:	hlt
	jmp .hlt

; EOF entry.s
