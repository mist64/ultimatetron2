; Ultimage Tron II by Oliver Stiller 1989
;
; disassembled and commented by Michael Steil <mist64@mac.com> 2015
;

        .setcpu "6502"

; contains temporary data for explosion
pagea := $C800
pageb := $C900

; contains a ring buffer for each player containing
; the last 32 pixels drawn for kill detection
history_x := $CA00
history_y := $CB00

        jmp     entry

; "1" if the player is participating
player_participating:
        .byte   1,1,1,1,1,1

; extra points per player, based on index of the kill
points: .byte   0,0,0,0,0,0

; index of the winner
winner: .byte   0

; index of the player that escaped
escaped_player:
        .byte   0

; which player uses which key set
keyset_index:
        .byte   0,2,4,6,8,10

; keeps track of victims and their killers
victim: .byte   $80,$80,$80,$80,$80,$80
killer: .byte   $80,$80,$80,$80,$80,$80

; six player names, max. 8 chars
player_names:
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0

player_mapping:
        .byte   0,1,2,3,4,5

; counts down frames until player names are erased from the screen
player_name_countdown:
        .byte   0

; counts number of frames a key was down
keys_temp:
        .byte   0,0,0,0,0,0,0,0,0,0,0,0
; "1" if a key was detected as pressed (for long enough)
keys_pressed:
        .byte   0,0,0,0,0,0,0,0,0,0,0,0

handle_keyboard:
        php
        sei
        ldx     #11
LC076:  lda     keys_temp,x
        beq     :+
        dec     keys_temp,x
:       lda     kbdtab1,x
        sta     $DC00
        lda     $DC01
        and     kbdtab2,x
        beq     LC091 ; key pressed
LC08C:  dex
        bpl     LC076
        plp
        rts
LC091:  ldy     keys_temp,x
        lda     #2
        sta     keys_temp,x ; initialize timer for key with 2 frames
        tya
        bne     LC08C
        inc     keys_pressed,x ; key was stable for 2 frames -> hand to engine
        bne     LC08C

; $DC00/$DC01 codes for the 12 possible control keys
kbdtab1:
        .byte   $7F,$7F,$FB,$FD,$EF,$F7,$BF,$DF,$BF,$BF,$FE,$FE
kbdtab2:
        .byte   $20,$80,$80,$10,$80,$10,$80,$10,$08,$02,$10,$20

init_screen:
        ldy     #$00
:       pha
        sta     $0400,y ; set screen colors
        sta     $0500,y
        sta     $0600,y
        sta     $0700,y
        lda     #$00
        sta     history_x,y ; clear two pages
        sta     history_y,y
        pla
        iny
        bne     :-
        lda     #$C8
        sta     $D016; default X scroll, 40 chars wide, hi-res
        lda     #$18
        sta     $D018; screen RAM at $0400, hi-res at $2000
        lda     #0
        sta     pagea + 180
        sta     pageb + 180
        sta     kill_index
        tay
        ldx     #$20
        stx     $FF
        sty     $FE
:       sta     ($FE),y ; clear hi-res
        iny
        bne     :-
        inc     $FF
        dex
        bne     :-
        rts

; table for converting coordinates into hi-res bitmap offsets
hirestab:
        .byte   $20,$21,$22,$23,$25,$26,$27,$28
        .byte   $2A,$2B,$2C,$2D,$2F,$30,$31,$32
        .byte   $34,$35,$36,$37,$39,$3A,$3B,$3C
        .byte   $3E,$3F

; tables for setting and clearing bits
and_tab:.byte   $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
or_tab: .byte   $80,$40,$20,$10,$08,$04,$02,$01

:       pla ; return two levels up
        pla
        sec ; error
        lda     #$00
        rts

; converts a coordinate in ($FC/$FD)/Y into
; * a byte offset into the bitmap in $FE/$FF
; * the lowest 3 bits of the coordinate in X and Y
offset_from_coord:
        cpy     #200
        bcs     :-    ; Y exceeds coord space
        ldx     $FD   ; X MSB
        beq     :+    ; zero is fine
        dex
        bne     :-    ; != 1 -> error
        ldx     $FC
        cpx     #320 - 256
        bcs     :-    ; > 320 -> error
:       pha
        lda     $FC
        and     #$F8
        sta     $FB   ; X & ~3
        tya
        pha ; save Y coord
        lsr     a
        lsr     a
        lsr     a     ; Y >> 3
        tay
        ror     a
        ror     a
        ror     a
        and     #$C0
        clc
        adc     $FB   ; + X & ~3
        sta     $FE
        lda     hirestab,y
        adc     $FD   ; + X MSB
        sta     $FF
        lda     $FC
        and     #$07  ; X & 7
        tax
        pla           ; Y
        and     #$07  ; Y & 7
        tay
        pla
        rts

; sets (Z=1) or clears (Z=0) the pixel at coordinate ($FC/$FD)/Y
set_or_clear_pixel:
        jsr     offset_from_coord
        php
        lda     ($FE),y   ; read bitmap byte
        and     and_tab,x ; clear bit
        plp
        beq     LC174
        ora     or_tab,x  ; set bit
LC174:  sta     ($FE),y
        clc               ; no error
        rts

; reads the pixel at coordinate ($FC/$FD)/Y
read_pixel:
        jsr     offset_from_coord
        lda     ($FE),y
        and     or_tab,x
        clc               ; no error
        rts

draw_border:  
; line on the left and right edges of the screen
        lda     #198
        sta     $FA
:       ldy     $FA ; Y coord
        lda     #>1
        sta     $FD
        lda     #<1 ; left
        sta     $FC
        jsr     set_or_clear_pixel ; set
        ldy     $FA
        lda     #>318
        sta     $FD
        lda     #<318 ; right
        sta     $FC
        jsr     set_or_clear_pixel ; set
        dec     $FA
        bne     :-

; line at the top and bottom edges of the screen
:       ldy     #198 ; bottom
        lda     #$01
        jsr     set_or_clear_pixel ; set
        ldy     #1 ; top
        tya
        jsr     set_or_clear_pixel ; set
        lda     $FC
        sec
        sbc     #1
        sta     $FC
        lda     $FD
        sbc     #0
        sta     $FD
        bne     :-
        lda     $FC
        cmp     #$01
        bne     :-
        rts

init_player_state:
        ldx     #5
:       lda     player_mapping,x
        tay
        lda     start_xpositions_lo,y
        sta     xpositions_lo,x
        lda     start_xpositions_hi,y
        sta     xpositions_hi,x
        lda     start_ypositions,y
        sta     ypositions,x
        lda     start_directions,y
        sta     directions,x
        dex
        bpl     :-

        ldx     #5
:       lda     player_participating,x
        sta     player_alive,x
        lda     #$80
        sta     killer,x
        sta     victim,x
        dex
        bpl     :-
        rts

; initial state
start_xpositions_lo:
        .byte   <50,<270,<50,<270,<50,<270
start_xpositions_hi:
        .byte   >50,>270,>50,>270,>50,>270
start_ypositions:
        .byte   50,50,100,100,150,150
start_directions:
        .byte   1,3,1,3,1,3

xpositions_lo:
        .byte   0,0,0,0,0,0
xpositions_hi:
        .byte   0,0,0,0,0,0
ypositions:
        .byte   0,0,0,0,0,0
directions:
        .byte   0,0,0,0,0,0

addition_tab_x_lo:
        .byte   0,1,0,-1
addition_tab_x_hi:
        .byte   0,0,0,-1
addition_tab_y:
        .byte   -1,0,1,0

; "1" if the player is participating and alive
player_alive:
        .byte   0,0,0,0,0,0

; holds the current player while iterating over players in game_step
current_player:
        .byte   $00

; number of kills in this game
kill_count:
        .byte   $00

; pointer in the history ring buffer
history_index:
        .byte   $00

; number of kills in this game (XXX redundant)
kill_index:
        .byte   0

; advance players by one pixel, detect deaths, kills and escapes
game_step:
        ldx     #5
        stx     current_player
        inc     history_index
        lda     history_index
        and     #$1F
        sta     history_index ; history is a 32 entry ring buffer
player_loop:
        lda     player_alive,x
        bne     :+
        jmp     game_step_skip
; update position and draw a pixel
:       ldy     directions,x
        lda     xpositions_lo,x
        clc
        adc     addition_tab_x_lo,y
        sta     xpositions_lo,x
        sta     $FC
        lda     xpositions_hi,x
        adc     addition_tab_x_hi,y
        sta     xpositions_hi,x
        sta     $FD
        lda     ypositions,x
        clc
        adc     addition_tab_y,y
        sta     ypositions,x
        sta     $FA
        tay
        jsr     read_pixel
        beq     player_survived
        ldx     current_player
        lda     #0
        sta     player_alive,x ; player died!
        lda     kill_count
        sta     points,x
        inc     kill_count
; find killer
        ldy     #32 * 6 - 1 ; 32 entries for 6 players
find_killer_loop:
        lda     history_x,y
        cmp     $FC   ; compare X (lo)
        bne     :+
        lda     history_y,y
        cmp     $FA   ; compare Y
        beq     LC2B2 ; found a match
:       dey
        cpy     #$FF
        bne     find_killer_loop
LC2AC:  jsr     explosion_init
        jmp     game_step_skip
LC2B2:  tya
        lsr     a ; index >> 5
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        ldx     kill_index
        cmp     current_player
        beq     LC2AC ; player killed himself -> not a kill
        sta     killer,x
        lda     current_player
        sta     victim,x
        inc     kill_index
        bpl     LC2AC ; always

player_survived:
        ldy     $FA
        lda     #1
        jsr     set_or_clear_pixel
; save history for kill detection
        lda     current_player
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a ; player << 5
        ora     history_index
        tay
        lda     $FC
        sta     history_x,y
        lda     $FA
        sta     history_y,y
; test for escape
        lda     $FA ; Y coord
        beq     player_escaped ; Y == 0 -> escaped
        cmp     #199
        beq     player_escaped ; Y == 199 -> escaped
        lda     $FD
        ora     $FC
        beq     player_escaped ; X == 0 -> escaped
        lda     $FD
        beq     game_step_skip
        lda     $FC
        cmp     #320 - 256 - 1
        beq     player_escaped ; X == 319 -> escaped
game_step_skip:
        dec     current_player
        ldx     current_player
        bmi     :+
        jmp     player_loop
:       rts
player_escaped:
        lda     current_player
        sta     escaped_player
        rts

; update player directions based on keyboard input
handle_input:
        jsr     handle_keyboard
        ldy     #5
        sty     current_player
:       lda     player_participating,y
        beq     input_loop ; skip player
        lda     keyset_index,y
        tay
        ldx     current_player
        lda     keys_pressed,y
        bne     turn_left_pressed
        iny
        lda     keys_pressed,y
        bne     turn_right_pressed
input_loop:
        dec     current_player
        ldy     current_player
        bpl     :-
        rts
turn_right_pressed:
        dec     directions,x
        jmp     :+
turn_left_pressed: 
        inc     directions,x
:       lda     directions,x
        and     #3
        sta     directions,x
        lda     #0
        sta     keys_pressed,y ; reset pressed key
        jmp     input_loop

set_pagea:
        lda     #>pagea
        .byte $2c
set_pageb:
        lda     #>pageb
        sta     LC3E1
        sta     LC3E6
        sta     LC3EB
        sta     LC3F0
        sta     LC3F8
        sta     LC3FF
        sta     LC407
        sta     LC40E
        sta     LC42D
        sta     LC437
        sta     LC43E
        sta     LC442
        sta     LC445
        sta     LC448
        sta     LC44B
        sta     LC44E
        sta     LC451
        sta     LC455
        sta     LC458
        sta     LC45B
        sta     LC45E
        sta     LC461
        sta     LC464
        sta     LC467
        sta     LC46A
        sta     LC478
        sta     LC47D
        sta     LC482
        sta     LC3C1
        rts

explosion_init:
        jsr     set_pagea
        lda     pagea + 180
        beq     LC3BF
        jsr     set_pageb
LC3BF:
LC3C1           := * + 2
        lda     pagea + 180
        beq     LC3DB
        lda     $FA
        pha
        lda     $FC
        pha
        lda     $FD
        pha
        lda     #$00
        jsr     LC470
        pla
        sta     $FD
        pla
        sta     $FC
        pla
        sta     $FA
LC3DB:  ldy     #19
        lda     #50
LC3E1           := * + 2
        sta     pagea + 180
LC3E2:  lda     $FA
LC3E6           := * + 2
        sta     pagea + 60,y
        lda     $FC
LC3EB           := * + 2
        sta     pagea + 0,y
        lda     $FD
LC3F0           := * + 2
        sta     pagea + 20,y
        jsr     get_random
        sbc     #$80
LC3F8           := * + 2
        sta     pagea + 100,y
        lda     #$00
        sbc     #$00
LC3FF           := * + 2
        sta     pagea + 120,y
        jsr     get_random
        sbc     #$80
LC407           := * + 2
        sta     pagea + 140,y
        lda     #$00
        sbc     #$00
LC40E           := * + 2
        sta     pagea + 160,y
        dey
        bpl     LC3E2
        rts

get_random:
        lda     rnd_seed
        adc     $D012 ; current raster line - has some entropy
        tax
        eor     $E74F,x ; a page of code in the KERNAL
        sta     rnd_seed
rts0:   rts

rnd_seed:
        .byte 0

explosion_step:
        jsr     set_pagea ; run the sequence above twice, with pagea and pageb
        jsr     LC42B
        jsr     set_pageb
LC42B:
LC42D           := * + 2
        lda     pagea + 180
        beq     rts0
        lda     #0 ; flag to clear pixel
        jsr     LC470
LC437           := * + 2
        dec     pagea + 180
        beq     rts0
        ldy     #19
LC43C:
LC43E           := * + 2
        lda     pagea + 80,y
        clc
LC442           := * + 2
        adc     pagea + 140,y
LC445           := * + 2
        sta     pagea + 80,y
LC448           := * + 2
        lda     pagea + 60,y
LC44B           := * + 2
        adc     pagea + 160,y
LC44E           := * + 2
        sta     pagea + 60,y
LC451           := * + 2
        lda     pagea + 40,y
        clc
LC455           := * + 2
        adc     pagea + 100,y
LC458           := * + 2
        sta     pagea + 40,y
LC45B           := * + 2
        lda     pagea + 0,y
LC45E           := * + 2
        adc     pagea + 120,y
LC461           := * + 2
        sta     pagea + 0,y
LC464           := * + 2
        lda     pagea + 20,y
LC467           := * + 2
        adc     pagea + 120,y
LC46A           := * + 2
        sta     pagea + 20,y
        dey
        bpl     LC43C
        lda     #1 ; flag to set pixel
LC470:  pha
        ldx     #19
        stx     explosion_index
LC476:
LC478           := * + 2
        lda     pagea + 0,x
        sta     $FC ; X lo
LC47D           := * + 2
        lda     pagea + 20,x
        sta     $FD ; X hi
LC482           := * + 2
        lda     pagea + 60,x
        tay         ; Y
        pla
        pha
        jsr     set_or_clear_pixel
        dec     explosion_index
        ldx     explosion_index
        bpl     LC476
        pla
        rts

explosion_index:
        .byte 0

; the main program
; * draws the playing field
; * draws the player names
; * enables a raster interrupt for every frame that handles the game play
; * busy waits until the game has ended
entry:  jsr     init_screen
        jsr     init_player_state
        jsr     draw_border
        jsr     show_or_hide_player_names ; show player names
        lda     #0
        sta     kill_count
        sta     $DC0E ; stop CIA1 Timer A
        lda     #20
        sta     player_name_countdown ; show player names for another 20 frames
        lda     #$80
        sta     escaped_player
        lda     #$3B
        sta     $D011 ; enable hi-res graphics mode
        lda     #$00
        tax
        tay
:       dex
        bne     :-
        dey
        bne     :-
        clc
        adc     #1
        cmp     #5
        bcc     :-     ; delay 1.6 sec
        ldx     #<irq
        ldy     #>irq
        sei
        stx     $0314
        sty     $0315
        lda     #230
        sta     $D012 ; line for raster IRQ
        lda     #$81
        sta     $D01A ; enable raster IRQ
        cli
main_loop:
        lda     escaped_player
        bpl     end_game
        lda     #$80
        sta     winner ; no winner
        ldy     #0
        ldx     #5
l1:     lda     player_alive,x
        beq     :+
        stx     winner
        iny
:       dex
        bpl     l1
        cpy     #2
        bcs     main_loop ; more than one player alive
        lda     pagea + 180
        ora     pageb + 180
        bne     main_loop
end_game:
        ldx     #<$EA34 ; restore IRQ handler
        ldy     #>$EA34 ; (minus soft RTC driver)
        sei
        stx     $0314
        sty     $0315
        lda     #$00
        sta     $D01A ; disable raster IRQ
        sta     $C6
        lda     #$01
        sta     $DC0E ; re-enable CIA1 Timer A
        cli
        rts

; the core of the game runs in sync with the raster beam
irq:    lda     $D019 ; ACK raster IRQ
        sta     $D019
        lda     player_name_countdown
        bmi     :+
        dec     player_name_countdown
        bne     :+
        jsr     show_or_hide_player_names ; erase player names
:       jsr     game_step
        jsr     handle_input
        jsr     explosion_step
        jmp     $EA81 ; KERNAL RTI code

; memory offsets that point to the location in hires where names are shown
hiresoff_lo:
        .byte   <$23D0,<$24B0,<$2B50,<$2C30,<$32D0,<$33B0
hiresoff_hi:
        .byte   >$23D0,>$24B0,>$2B50,>$2C30,>$32D0,>$33B0

show_or_hide_player_names:
        ldx     #0
        sei
        lda     #$33 ; enable char ROM at $D000
        sta     $01
LC54F:  txa
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     player_mapping,y
        tay
        lda     hiresoff_lo,y
        sta     $FE
        lda     hiresoff_hi,y
        sta     $FF
        lda     #$08
        sta     $FB
LC566:  lda     player_names,x
        beq     show_or_hide_player_names_skip
        sec
        sbc     #$20
        asl     a
        asl     a
        asl     a
        sta     $F8
        lda     #$D0
        sta     $F9
        ldy     #$07
:       lda     ($F8),y ; XOR them onto the screen,
        eor     ($FE),y ; so if this is called twice,
        sta     ($FE),y ; they will be removed
        dey
        bpl     :-
show_or_hide_player_names_skip:
        lda     $FE
        clc
        adc     #$08
        sta     $FE
        lda     $FF
        adc     #$00
        sta     $FF
        inx
        dec     $FB
        bne     LC566
        cpx     #$30
        bcc     LC54F
        lda     #$37
        sta     $01 ; restore memory mapping
        cli
        rts
