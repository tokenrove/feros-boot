
NASM=nasm
AS=$(NASM)
ASFLAGS=-felf

.phony: default

default: boot kernel

boot: stage-a.bin stage-b.bin
	cat stage-a.bin stage-b.bin > boot

stage-a.bin: stage-a.s
	$(NASM) -fbin stage-a.s -o stage-a.bin

stage-b.bin: stage-b.s
	$(NASM) -fbin stage-b.s -o stage-b.bin

kernel: entry.s
	$(NASM) -fbin entry.s -o kernel

#kernel: entry.o
#	ld $^ -o $@ -Ttext 0x100000

clean:
	$(RM) *.o stage-a.bin stage-b.bin

distclean: clean
	$(RM) kernel boot