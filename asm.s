; Ultimage Tron II by Oliver Stiller

        .setcpu "6502"

pagea := $C800
pageb := $C900
page1 := $CA00
page2 := $CB00

        .byte   $00,$C0

        jmp     entry

player_participating:
        .byte   $01,$01,$01,$01,$01,$01

LC009:  .byte   $00,$00,$00,$00,$00,$00
winner: .byte   $00

escaped_player:
        .byte   $00

LC011:  .byte   $00,$02,$04,$06,$08,$0A

tab6:   .byte   $80,$80,$80,$80,$80,$80
tab5:   .byte   $80,$80,$80,$80,$80,$80

player_names: ; six player names, max. 8 chars
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00

LC053:  .byte   $00,$01,$02,$03,$04,$05
LC059:  .byte   $00
LC05A:  .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00
LC066:  .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00

handle_keyboard:
        php
        sei
        ldx     #11
LC076:  lda     LC05A,x
        beq     :+
        dec     LC05A,x
:       lda     kbdtab1,x
        sta     $DC00
        lda     $DC01
        and     kbdtab2,x
        beq     LC091 ; key pressed
LC08C:  dex
        bpl     LC076
        plp
        rts
LC091:  ldy     LC05A,x
        lda     #$02
        sta     LC05A,x
        tya
        bne     LC08C
        inc     LC066,x
        bne     LC08C

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
        sta     page1,y ; clear two pages
        sta     page2,y
        pla
        iny
        bne     :-
        lda     #$C8
        sta     $D016; default X scroll, 40 chars wide, hi-res
        lda     #$18
        sta     $D018; screen RAM at $0400, hi-res at $2000
        lda     #$00
        sta     pagea + $B4
        sta     pageb + $B4
        sta     LC241
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

LC0FB:  .byte   $20,$21,$22,$23,$25,$26,$27,$28
        .byte   $2A,$2B,$2C,$2D,$2F,$30,$31,$32
        .byte   $34,$35,$36,$37,$39,$3A,$3B,$3C
        .byte   $3E,$3F

and_tab:.byte   $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
or_tab: .byte   $80,$40,$20,$10,$08,$04,$02,$01

LC125:  pla ; return two levels up
        pla
        sec ; error
        lda     #$00
        rts

; converts a coordinate in ($FC/$FD)/Y into
; * a byte offset into the framebuffer in $FE/$FF
; * the lowest 3 bits of the coordinate in X and Y
offset_from_coord:
        cpy     #200
        bcs     LC125 ; Y exceeds coord space
        ldx     $FD   ; X MSB
        beq     :+    ; zero is fine
        dex
        bne     LC125 ; != 1 -> error
        ldx     $FC
        cpx     #320 - 256
        bcs     LC125 ; > 320 -> error
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
        lda     LC0FB,y
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
        lda     ($FE),y   ; read framebuffer byte
        and     and_tab,x ; clear bit
        plp
        beq     LC174
        ora     or_tab,x  ; set bit
LC174:  sta     ($FE),y
        clc               ; no error
        rts

; sets the pixel at coordinate ($FC/$FD)/Y
set_pixel:
        jsr     offset_from_coord
        lda     ($FE),y
        and     or_tab,x
        clc               ; no error
        rts

draw_border:  
; line on the right edge of the screen
        lda     #198
        sta     $FA
:       ldy     $FA ; Y coord
        lda     #$00
        sta     $FD
        lda     #$01 ; X coord = 1
        sta     $FC
        jsr     set_or_clear_pixel ; set
        ldy     $FA
        lda     #>318
        sta     $FD
        lda     #<318
        sta     $FC
        jsr     set_or_clear_pixel ; set
        dec     $FA
        bne     :-

; line at the bottom edge of the screen
:       ldy     #198
        lda     #$01
        jsr     set_or_clear_pixel ; set
        ldy     #$01
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
        ldx     #$05 ; copy tab1 into tab2
:       lda     LC053,x
        tay
        lda     tab1a,y
        sta     tab2a,x
        lda     tab1b,y
        sta     tab2b,x
        lda     tab1c,y
        sta     tab2c,x
        lda     tab1d,y
        sta     tab2d,x
        dex
        bpl     :-

        ldx     #$05
:       lda     player_participating,x
        sta     player_alive,x
        lda     #$80
        sta     tab5,x
        sta     tab6,x
        dex
        bpl     :-
        rts

tab1a:  .byte   $32,$0E,$32,$0E,$32,$0E
tab1b:  .byte   $00,$01,$00,$01,$00,$01
tab1c:  .byte   $32,$32,$64,$64,$96,$96
tab1d:  .byte   $01,$03,$01,$03,$01,$03

tab2a:  .byte   $00,$00,$00,$00,$00,$00
tab2b:  .byte   $00,$00,$00,$00,$00,$00
tab2c:  .byte   $00,$00,$00,$00,$00,$00
tab2d:  .byte   $00,$00,$00,$00,$00,$00

LC22C:  .byte   $00,$01,$00,$FF
LC230:  .byte   $00,$00,$00,$FF
LC234:  .byte   $FF,$00,$01,$00

player_alive:
        .byte   $00,$00,$00,$00,$00,$00

current_player:
        .byte   $00

LC23F:  .byte   $00
LC240:  .byte   $00
LC241:  .byte   $00

game_step:
        ldx     #$05
        stx     current_player
        inc     LC240
        lda     LC240
        and     #$1F
        sta     LC240 ; Z(32)
player_loop:
        lda     player_alive,x
        bne     :+
        jmp     LC303
:       ldy     tab2d,x
        lda     tab2a,x
        clc
        adc     LC22C,y
        sta     tab2a,x
        sta     $FC
        lda     tab2b,x
        adc     LC230,y
        sta     tab2b,x
        sta     $FD
        lda     tab2c,x
        clc
        adc     LC234,y
        sta     tab2c,x
        sta     $FA
        tay
        jsr     set_pixel
        beq     LC2CE
        ldx     current_player
        lda     #0
        sta     player_alive,x ; player died!
        lda     LC23F
        sta     LC009,x
        inc     LC23F
        ldy     #$BF
LC299:  lda     page1,y
        cmp     $FC
        bne     LC2A7
        lda     page2,y
        cmp     $FA
        beq     LC2B2
LC2A7:  dey
        cpy     #$FF
        bne     LC299
LC2AC:  jsr     LC3B4
        jmp     LC303
LC2B2:  tya
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        ldx     LC241
        cmp     current_player
        beq     LC2AC
        sta     tab5,x
        lda     current_player
        sta     tab6,x
        inc     LC241
        bpl     LC2AC
LC2CE:  ldy     $FA
        lda     #$01
        jsr     set_or_clear_pixel
        lda     current_player
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        ora     LC240
        tay
        lda     $FC
        sta     page1,y
        lda     $FA
        sta     page2,y
; test for escape
        lda     $FA ; Y coord
        beq     player_escaped ; Y == 0 -> escaped
        cmp     #199
        beq     player_escaped ; Y == 199 -> escaped
        lda     $FD
        ora     $FC
        beq     player_escaped ; X == 0 -> escaped
        lda     $FD
        beq     LC303
        lda     $FC
        cmp     #320 - 256 - 1
        beq     player_escaped ; X == 319 -> escaped
LC303:  dec     current_player
        ldx     current_player
        bmi     :+
        jmp     player_loop
:       rts
player_escaped:
        lda     current_player
        sta     escaped_player
        rts

LC316:  jsr     handle_keyboard
        ldy     #$05
        sty     current_player
LC31E:  lda     player_participating,y
        beq     LC335
        lda     LC011,y
        tay
        ldx     current_player
        lda     LC066,y
        bne     LC344
        iny
        lda     LC066,y
        bne     LC33E
LC335:  dec     current_player
        ldy     current_player
        bpl     LC31E
        rts
LC33E:  dec     tab2d,x
        jmp     LC347
LC344:  inc     tab2d,x
LC347:  lda     tab2d,x
        and     #$03
        sta     tab2d,x
        lda     #$00
        sta     LC066,y
        jmp     LC335
LC357:  lda     #>pagea
        .byte $2c
LC35A:  lda     #>pageb
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

LC3B4:  jsr     LC357
        lda     pagea + $B4
        beq     LC3BF
        jsr     LC35A
LC3BF:
LC3C1           := * + 2
        lda     pagea + $B4
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
LC3DB:  ldy     #$13
        lda     #$32
LC3E1           := * + 2
        sta     pagea + $B4
LC3E2:  lda     $FA
LC3E6           := * + 2
        sta     pagea + $3C,y
        lda     $FC
LC3EB           := * + 2
        sta     pagea + $00,y
        lda     $FD
LC3F0           := * + 2
        sta     pagea + $14,y
        jsr     LC413
        sbc     #$80
LC3F8           := * + 2
        sta     pagea + $64,y
        lda     #$00
        sbc     #$00
LC3FF           := * + 2
        sta     pagea + $78,y
        jsr     LC413
        sbc     #$80
LC407           := * + 2
        sta     pagea + $8C,y
        lda     #$00
        sbc     #$00
LC40E           := * + 2
        sta     pagea + $A0,y
        dey
        bpl     LC3E2
        rts
LC413:  lda     LC421
        adc     $D012
        tax
        eor     $E74F,x
        sta     LC421
LC420:  rts
LC421:  brk
LC422:  jsr     LC357
        jsr     LC42B
        jsr     LC35A
LC42B:
LC42D           := * + 2
        lda     pagea + $B4
        beq     LC420
        lda     #$00
        jsr     LC470
LC437           := * + 2
        dec     pagea + $B4
        beq     LC420
        ldy     #$13
LC43C:
LC43E           := * + 2
        lda     pagea + $50,y
        clc
LC442           := * + 2
        adc     pagea + $8C,y
LC445           := * + 2
        sta     pagea + $50,y
LC448           := * + 2
        lda     pagea + $3C,y
LC44B           := * + 2
        adc     pagea + $A0,y
LC44E           := * + 2
        sta     pagea + $3C,y
LC451           := * + 2
        lda     pagea + $28,y
        clc
LC455           := * + 2
        adc     pagea + $64,y
LC458           := * + 2
        sta     pagea + $28,y
LC45B           := * + 2
        lda     pagea + $00,y
LC45E           := * + 2
        adc     pagea + $78,y
LC461           := * + 2
        sta     pagea + $00,y
LC464           := * + 2
        lda     pagea + $14,y
LC467           := * + 2
        adc     pagea + $78,y
LC46A           := * + 2
        sta     pagea + $14,y
        dey
        bpl     LC43C
        lda     #$01
LC470:  pha
        ldx     #$13
        stx     LC493
LC476:
LC478           := * + 2
        lda     pagea + $00,x
        sta     $FC
LC47D           := * + 2
        lda     pagea + $14,x
        sta     $FD
LC482           := * + 2
        lda     pagea + $3C,x
        tay
        pla
        pha
        jsr     set_or_clear_pixel
        dec     LC493
        ldx     LC493
        bpl     LC476
        pla
        rts

LC493:  .byte 0

; the main program
; * draws the playing field
; * draws the player names
; * enables a raster interrupt for every frame that handles the game play
; * busy waits until the game has ended
entry:  jsr     init_screen
        jsr     init_player_state
        jsr     draw_border
        jsr     show_player_names
        lda     #$00
        sta     LC23F
        sta     $DC0E ; stop CIA1 Timer A
        lda     #$14
        sta     LC059
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
        lda     pagea + $B4
        ora     pageb + $B4
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

irq:    lda     $D019 ; ACK raster IRQ
        sta     $D019
        lda     LC059
        bmi     LC530
        dec     LC059
        bne     LC530
        jsr     show_player_names
LC530:  jsr     game_step
        jsr     LC316
        jsr     LC422
        jmp     $EA81 ; KERNAL RTI code

LC53C:  .byte   $D0,$B0,$50,$30,$D0,$B0
LC542:  .byte   $23,$24,$2B,$2C,$32,$33

show_player_names:
        ldx     #$00
        sei
        lda     #$33 ; enable char ROM at $D000
        sta     $01
LC54F:  txa
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     LC053,y
        tay
        lda     LC53C,y
        sta     $FE
        lda     LC542,y
        sta     $FF
        lda     #$08
        sta     $FB
LC566:  lda     player_names,x
        beq     skip_player
        sec
        sbc     #$20
        asl     a
        asl     a
        asl     a
        sta     $F8
        lda     #$D0
        sta     $F9
        ldy     #$07
:       lda     ($F8),y
        eor     ($FE),y
        sta     ($FE),y
        dey
        bpl     :-
skip_player:
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
        sta     $01
        cli
        rts

        cli
        rts
