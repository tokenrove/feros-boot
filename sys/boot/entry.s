;
; feros / entry.s
; Julian Squires <tek@wiw.org> / 2001
;

	section .text
	bits 32

	global _start

	; Kernel entry point, from stage B loader.
_start:
	cli
	; reroute irqs
	; initialize kernel video routines
	; detect ram and cpu
.hlt:	hlt
	jmp .hlt

; EOF entry.s
