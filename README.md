# Ultimate Tron II for C64

*Ultimate Tron II* is a game for C64 developed by Oliver Stiller in 1989 and published in 64'er Sonderheft 54 in 1990.

This is a reverse-engineered version that can be compiled into the same binary, using the cc65 suite.

## Credits

	MASTERS' DESIGN GROUP'S ULTIMATE TRON II
	        WRITTEN BY OLIVER STILLER
	DEVELOPMENT HISTORY:
	ULTIMATE TRON PUBLISHED BY 64'ER IN 1988
	ATARI ST ESCAPE-TRON WRITTEN AT COLOGNE
	UNIVERSITY IN AUGUST '88
	HEX-TRON WRITTEN IN MAY '89, PLAYED BY  
	MDG AND CYBERSTYLE
	IMPROVED VERSION: 'ULTIMATE TRON II'    
	PROGRAMMED IN NOVEMBER 1989

Reverse-engineering by Michael Steil <mist64@mac.com>, [http://www.pagetable.com/](http://www.pagetable.com/)

## Files
* `basic.prg`, `asm.prg`: The original BASIC and assembly binaries.
* `basic.bas`: The ASCII/PETSCII representation of the BASIC part.* `asm.s`: The reverse-engineered assembly part.* `Makefile`, `asm.cfg`: Needed for building the assembly part.## Some implementation details
* The game consists of two parts: A BASIC program that evaluates and keeps track of the scores, and an assembly program that is the actual game. BASIC calls into assembly for every round.
* The main program sits in a busy waiting loop, while the game is run based on a raster interrupt triggered by line 230.
* The code explicitly checks for keys to be pressed for at least 20 ms.
* The game state is the hi-res bitmap itself, collisions are detected by reading back pixels of the bitmap.
* Escaping is detected by comparing whether the player's most recently drawn pixel was at the screen edge - which can only be the case if another player's explosion has already removed the border pixel.
* Killer detection is done using a ring buffer for every player that holds the most recent 32 positions. Since the game only saves the lower 8 bits of the x coordinate, and the screen has a horizontal resolution of 320, it could in theory happen that the wrong killer is detected.

