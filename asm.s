; da65 V2.13.2 - (C) Copyright 2000-2009,  Ullrich von Bassewitz
; Created:    2015-09-15 08:21:52
; Input file: asm.prg
; Page:       1


        .setcpu "6502"

LEA81           := $EA81
        .byte   $00,$C0
        jmp     LC494
LC003:  .byte   $01,$01,$01,$01,$01,$01
LC009:  .byte   $00,$00,$00,$00,$00,$00
LC00F:  .byte   $00
LC010:  .byte   $00
LC011:  .byte   $00,$02,$04,$06,$08,$0A
LC017:  .byte   $80,$80,$80,$80,$80,$80
LC01D:  .byte   $80,$80,$80,$80,$80,$80
LC023:  .byte   $00,$00,$00,$00,$00,$00,$00,$00
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
LC072:  php
        sei
        ldx     #$0B
LC076:  lda     LC05A,x
        beq     LC07E
        dec     LC05A,x
LC07E:  lda     LC0A1,x
        sta     $DC00
        lda     $DC01
        and     LC0AD,x
        beq     LC091
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
LC0A1:  .byte   $7F,$7F,$FB,$FD,$EF,$F7,$BF,$DF
        .byte   $BF,$BF,$FE,$FE
LC0AD:  .byte   $20,$80,$80,$10,$80,$10,$80,$10
        .byte   $08,$02,$10,$20
LC0B9:  ldy     #$00
LC0BB:  pha
        sta     $0400,y
        sta     $0500,y
        sta     $0600,y
        sta     $0700,y
        lda     #$00
        sta     $CA00,y
        sta     $CB00,y
        pla
        iny
        bne     LC0BB
        lda     #$C8
        sta     $D016
        lda     #$18
        sta     $D018
        lda     #$00
        sta     $C8B4
        sta     $C9B4
        sta     LC241
        tay
        ldx     #$20
        stx     $FF
        sty     $FE
LC0F0:  sta     ($FE),y
        iny
        bne     LC0F0
        inc     $FF
        dex
        bne     LC0F0
        rts
LC0FB:  .byte   $20,$21,$22,$23,$25,$26,$27,$28
        .byte   $2A,$2B,$2C,$2D,$2F,$30,$31,$32
        .byte   $34,$35,$36,$37,$39,$3A,$3B,$3C
        .byte   $3E,$3F
LC115:  .byte   $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
LC11D:  .byte   $80,$40,$20,$10,$08,$04,$02,$01
LC125:  pla
        pla
        sec
        lda     #$00
        rts
LC12B:  cpy     #$C8
        bcs     LC125
        ldx     $FD
        beq     LC13C
        dex
        bne     LC125
        ldx     $FC
        cpx     #$40
        bcs     LC125
LC13C:  pha
        lda     $FC
        and     #$F8
        sta     $FB
        tya
        pha
        lsr     a
        lsr     a
        lsr     a
        tay
        ror     a
        ror     a
        ror     a
        and     #$C0
        clc
        adc     $FB
        sta     $FE
        lda     LC0FB,y
        adc     $FD
        sta     $FF
        lda     $FC
        and     #$07
        tax
        pla
        and     #$07
        tay
        pla
        rts
LC165:  jsr     LC12B
        php
        lda     ($FE),y
        and     LC115,x
        plp
        beq     LC174
        ora     LC11D,x
LC174:  sta     ($FE),y
        clc
        rts
LC178:  jsr     LC12B
        lda     ($FE),y
        and     LC11D,x
        clc
        rts
LC182:  lda     #$C6
        sta     $FA
LC186:  ldy     $FA
        lda     #$00
        sta     $FD
        lda     #$01
        sta     $FC
        jsr     LC165
        ldy     $FA
        lda     #$01
        sta     $FD
        lda     #$3E
        sta     $FC
        jsr     LC165
        dec     $FA
        bne     LC186
LC1A4:  ldy     #$C6
        lda     #$01
        jsr     LC165
        ldy     #$01
        tya
        jsr     LC165
        lda     $FC
        sec
        sbc     #$01
        sta     $FC
        lda     $FD
        sbc     #$00
        sta     $FD
        bne     LC1A4
        lda     $FC
        cmp     #$01
        bne     LC1A4
        rts
LC1C7:  ldx     #$05
LC1C9:  lda     LC053,x
        tay
        lda     LC1FC,y
        sta     LC214,x
        lda     LC202,y
        sta     LC21A,x
        lda     LC208,y
        sta     LC220,x
        lda     LC20E,y
        sta     LC226,x
        dex
        bpl     LC1C9
        ldx     #$05
LC1EA:  lda     LC003,x
        sta     LC238,x
        lda     #$80
        sta     LC01D,x
        sta     LC017,x
        dex
        bpl     LC1EA
        rts
LC1FC:  .byte   $32,$0E,$32,$0E,$32,$0E
LC202:  .byte   $00,$01,$00,$01,$00,$01
LC208:  .byte   $32,$32,$64,$64,$96,$96
LC20E:  .byte   $01,$03,$01,$03,$01,$03
LC214:  .byte   $00,$00,$00,$00,$00,$00
LC21A:  .byte   $00,$00,$00,$00,$00,$00
LC220:  .byte   $00,$00,$00,$00,$00,$00
LC226:  .byte   $00,$00,$00,$00,$00,$00
LC22C:  .byte   $00,$01,$00,$FF
LC230:  .byte   $00,$00,$00,$FF
LC234:  .byte   $FF,$00,$01,$00
LC238:  .byte   $00,$00,$00,$00,$00,$00
LC23E:  .byte   $00
LC23F:  .byte   $00
LC240:  .byte   $00
LC241:  .byte   $00
LC242:  ldx     #$05
        stx     LC23E
        inc     LC240
        lda     LC240
        and     #$1F
        sta     LC240
LC252:  lda     LC238,x
        bne     LC25A
        jmp     LC303
LC25A:  ldy     LC226,x
        lda     LC214,x
        clc
        adc     LC22C,y
        sta     LC214,x
        sta     $FC
        lda     LC21A,x
        adc     LC230,y
        sta     LC21A,x
        sta     $FD
        lda     LC220,x
        clc
        adc     LC234,y
        sta     LC220,x
        sta     $FA
        tay
        jsr     LC178
        beq     LC2CE
        ldx     LC23E
        lda     #$00
        sta     LC238,x
        lda     LC23F
        sta     LC009,x
        inc     LC23F
        ldy     #$BF
LC299:  lda     $CA00,y
        cmp     $FC
        bne     LC2A7
        lda     $CB00,y
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
        cmp     LC23E
        beq     LC2AC
        sta     LC01D,x
        lda     LC23E
        sta     LC017,x
        inc     LC241
        bpl     LC2AC
LC2CE:  ldy     $FA
        lda     #$01
        jsr     LC165
        lda     LC23E
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        ora     LC240
        tay
        lda     $FC
        sta     $CA00,y
        lda     $FA
        sta     $CB00,y
        lda     $FA
        beq     LC30F
        cmp     #$C7
        beq     LC30F
        lda     $FD
        ora     $FC
        beq     LC30F
        lda     $FD
        beq     LC303
        lda     $FC
        cmp     #$3F
        beq     LC30F
LC303:  dec     LC23E
        ldx     LC23E
        bmi     LC30E
        jmp     LC252
LC30E:  rts
LC30F:  lda     LC23E
        sta     LC010
        rts
LC316:  jsr     LC072
        ldy     #$05
        sty     LC23E
LC31E:  lda     LC003,y
        beq     LC335
        lda     LC011,y
        tay
        ldx     LC23E
        lda     LC066,y
        bne     LC344
        iny
        lda     LC066,y
        bne     LC33E
LC335:  dec     LC23E
        ldy     LC23E
        bpl     LC31E
        rts
LC33E:  dec     LC226,x
        jmp     LC347
LC344:  inc     LC226,x
LC347:  lda     LC226,x
        and     #$03
        sta     LC226,x
        lda     #$00
        sta     LC066,y
        jmp     LC335
LC357:  lda     #$C8
LC35A           := * + 1
        bit     $C9A9
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
        lda     $C8B4
        beq     LC3BF
        jsr     LC35A
LC3BF:
LC3C1           := * + 2
        lda     $C8B4
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
        sta     $C8B4
LC3E2:  lda     $FA
LC3E6           := * + 2
        sta     $C83C,y
        lda     $FC
LC3EB           := * + 2
        sta     $C800,y
        lda     $FD
LC3F0           := * + 2
        sta     $C814,y
        jsr     LC413
        sbc     #$80
LC3F8           := * + 2
        sta     $C864,y
        lda     #$00
        sbc     #$00
LC3FF           := * + 2
        sta     $C878,y
        jsr     LC413
        sbc     #$80
LC407           := * + 2
        sta     $C88C,y
        lda     #$00
        sbc     #$00
LC40E           := * + 2
        sta     $C8A0,y
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
        lda     $C8B4
        beq     LC420
        lda     #$00
        jsr     LC470
LC437           := * + 2
        dec     $C8B4
        beq     LC420
        ldy     #$13
LC43C:
LC43E           := * + 2
        lda     $C850,y
        clc
LC442           := * + 2
        adc     $C88C,y
LC445           := * + 2
        sta     $C850,y
LC448           := * + 2
        lda     $C83C,y
LC44B           := * + 2
        adc     $C8A0,y
LC44E           := * + 2
        sta     $C83C,y
LC451           := * + 2
        lda     $C828,y
        clc
LC455           := * + 2
        adc     $C864,y
LC458           := * + 2
        sta     $C828,y
LC45B           := * + 2
        lda     $C800,y
LC45E           := * + 2
        adc     $C878,y
LC461           := * + 2
        sta     $C800,y
LC464           := * + 2
        lda     $C814,y
LC467           := * + 2
        adc     $C878,y
LC46A           := * + 2
        sta     $C814,y
        dey
        bpl     LC43C
        lda     #$01
LC470:  pha
        ldx     #$13
        stx     LC493
LC476:
LC478           := * + 2
        lda     $C800,x
        sta     $FC
LC47D           := * + 2
        lda     $C814,x
        sta     $FD
LC482           := * + 2
        lda     $C83C,x
        tay
        pla
        pha
        jsr     LC165
        dec     LC493
        ldx     LC493
        bpl     LC476
        pla
        rts
LC493:  brk
LC494:  jsr     LC0B9
        jsr     LC1C7
        jsr     LC182
        jsr     LC548
        lda     #$00
        sta     LC23F
        sta     $DC0E
        lda     #$14
        sta     LC059
        lda     #$80
        sta     LC010
        lda     #$3B
        sta     $D011
        lda     #$00
        tax
        tay
LC4BB:  dex
        bne     LC4BB
        dey
        bne     LC4BB
        clc
        adc     #$01
        cmp     #$05
        bcc     LC4BB
        ldx     #$1D
        ldy     #$C5
        sei
        stx     $0314
        sty     $0315
        lda     #$E6
        sta     $D012
        lda     #$81
        sta     $D01A
        cli
LC4DE:  lda     LC010
        bpl     LC504
        lda     #$80
        sta     LC00F
        ldy     #$00
        ldx     #$05
LC4EC:
LC4EE           := * + 2
        lda     LC238,x
        beq     LC4F5
LC4F2           := * + 1
        stx     LC00F
        iny
LC4F5:  dex
        bpl     LC4EC
        cpy     #$02
        bcs     LC4DE
        lda     $C8B4
        ora     $C9B4
        bne     LC4DE
LC504:  ldx     #$34
        ldy     #$EA
        sei
        stx     $0314
        sty     $0315
        lda     #$00
        sta     $D01A
        sta     $C6
        lda     #$01
        sta     $DC0E
        cli
        rts
        lda     $D019
        sta     $D019
        lda     LC059
        bmi     LC530
        dec     LC059
        bne     LC530
        jsr     LC548
LC530:  jsr     LC242
        jsr     LC316
        jsr     LC422
        jmp     LEA81
LC53C:  bne     LC4EE
        bvc     LC570
        bne     LC4F2
LC542:  .byte   $23,$24,$2B,$2C,$32,$33
LC548:  ldx     #$00
        sei
        lda     #$33
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
LC566:  lda     LC023,x
        beq     LC582
        sec
        sbc     #$20
        asl     a
        asl     a
LC570:  asl     a
        sta     $F8
        lda     #$D0
        sta     $F9
        ldy     #$07
LC579:  lda     ($F8),y
        eor     ($FE),y
        sta     ($FE),y
        dey
        bpl     LC579
LC582:  lda     $FE
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
        .byte   $CB
