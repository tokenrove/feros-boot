
NASM=nasm
AS=$(NASM)
ASFLAGS=-felf
LD=i386-elf-ld
LDFLAGS=-Ttext 0x100000 --oformat binary

.phony: default

default: boot kernel

boot: stage-a.bin stage-b.bin
	cat stage-a.bin stage-b.bin > boot

stage-a.bin: stage-a.s
	$(NASM) -fbin stage-a.s -o stage-a.bin

stage-b.bin: stage-b.s
	$(NASM) -fbin stage-b.s -o stage-b.bin

kernel: entry.o
	$(LD) $(LDFLAGS) $^ -o $@

clean:
	$(RM) *.o stage-a.bin stage-b.bin

distclean: clean
	$(RM) kernel boot