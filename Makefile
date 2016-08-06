all:
	ca65 asm.s -o asm.o
	ld65 -o asm-new.prg asm.o -C asm.cfg

clean:
	rm -f asm.o asm-new.prg
