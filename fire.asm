         *=$0801
         .byte $0c,$08,$0a,$00,$9e,$20
         .byte $34,$30,$39,$36,$00,$00
         .byte $00

         *= $1000

scrmem   = $0400 ;pointer to screen mem
clrmem   = $d800 ;pointer to color mem

scrbuf   = $4000

tmp      = $22
xor      = $23
yscroll  = $24
offset   = $40
wcount   = $fa ;wait counter
zp       = $fb
zp2      = $fd

getin    = $ffe4

init
         cld
         clc
         lda #$00
         sta offset
         sta wcount
         sta yscroll

         jsr clrbuf ;clear buffer
         ;sei
nextpage

         ;Store 24 to row counter
         ldx #$18

         ;load pointer to screen memory
         ;into zero page variable.
         lda #<scrbuf
         sta zp
         lda #>scrbuf
         sta zp+1

         ;load pointer to screen memory
         ;for the second screen line
         ;into zero page variable.
         lda #<scrbuf+40
         sta zp2
         lda #>scrbuf+40
         sta zp2+1

;---------------------------------------
;scroll loop
;this loop moves the current screen
;contents one line up. with each line we
;read the corresponding character from
;the next line and replace the current
;charater.
;blur routine is called for each char
;
;---------------------------------------
sloop2
         ldy #$00

         clc
sloop1
         lda (zp2),y  ;load from zp2
         sta (zp),y   ;write to zp
         jsr blur
         clc
         iny
         cpy #$28  ;check if end of row
         bne sloop1 ;loop if not

;Add 40 to zero page pointer to move
;to the next line
         clc
         lda zp
         adc #$28
         sta zp
         lda zp+1
         adc #$00
         sta zp+1
;Same with zp2
         clc
         lda zp2
         adc #$28
         sta zp2
         lda zp2+1
         adc #$00
         sta zp2+1
         clc

         ;Decrement row counter
         ;quit if zero
         dex
         cpx #$00
         bne sloop2

;---------------------------------------
;draw new line
;---------------------------------------
         ldy #$00
         lda #$00   ;lead the space char
lloop1
         sta (zp),y ;write line of spc
         iny
         cpy #$28   ;check if end of row
         bne lloop1 ;loop if not

;write character to the last line
;at position determined by offset
         ldy offset
         lda #$1f
         sta (zp),y
         iny
         sta (zp),y

;randomize offset for the next char.
;the randomization is done by reading
;timer registers and combining them with
;bitwise operations, additions and
;bitshifts.
         clc
         lda $dc04
         sta xor
         adc #$05
         lsr
         eor $dc06
         eor xor
         and #$3f
         cmp #$28   ;left shift if coord
         bcc llskip ;does not fit
         lsr        ;on screen
llskip   sta offset

;wait loop
;this loop compares the current raster
;line to the provided line ($f8).
;wcount variable is used to wait for
;a number of frames.
wloop1
         lda #$f8
wloop2
         cmp $d012   ;check raster line
         bne wloop2

         inc wcount  ;inc frame counter
         lda wcount  ;check if counter
         cmp #$06    ;reached 10
         bne wloop1  ;if not, loop

         lda #$00    ;reset counter
         sta wcount

         jsr printscr
         jmp nextpage

;end of the program
mainquit
         rts

;---------------------------------------
;subroutines
;---------------------------------------

;---------------------------------------
;blur
;reads the adjacent horiz pixel values,
;adds them together and divides them.
;value is weighed so that the original
;value is multiplied 2x, the adjacent
;values are added and the sum is divided
;by four.
;---------------------------------------
blur
         tya
         pha

         lda (zp),y ;read orig value
         asl        ;multiply 2x
         sta tmp    ;store to tmp

         iny        ;pointer +1
         lda (zp),y ;read value
         clc
         adc tmp    ;add and
         sta tmp    ;store to tmp
         dey        ;pointer -2
         dey
         lda (zp),y
         clc
         adc tmp    ;add with tmp
         lsr        ;divide
         lsr        ;by four
         and #$1f   ;limit to 31
         iny
         sta (zp),y

         pla
         tay
         rts


;---------------------------------------
;clear buffer
;zeroes out the working buffer
;---------------------------------------
clrbuf
         ldx #$00
         lda #$00
clrloop
         sta scrbuf,x
         sta scrbuf+$100,x
         sta scrbuf+$200,x
         sta scrbuf+$300,x
         sta scrbuf+$400,x
         inx
         bne clrloop
         rts

;---------------------------------------
;print screen
;copies the screen from working buffer
;to the actual screen memory
;---------------------------------------
printscr
         ldx #$00
printlp
         lda scrbuf,x
         jsr transp
         sta scrmem,x
         lda scrbuf+$100,x
         jsr transp
         sta scrmem+$100,x
         lda scrbuf+$200,x
         jsr transp
         sta scrmem+$200,x
         lda scrbuf+$300,x
         jsr transp
         sta scrmem+$300,x
         inx
         bne printlp
         rts

;---------------------------------------
;transpose character
;transposes the char in accumulator
;to the value in 'chars' table
;---------------------------------------
transp
         tay
         lda chars, y
         rts

;---------------------------------------
;kbwait
;wait for keyboard input
;and determine what action to take.
;---------------------------------------
kbwait
         jsr getin
         cmp #$00
         beq kbwait
         rts

;list of characters for transposing
chars    .byte $20, $31, $31, $32, $32
         .byte $33, $33, $34, $34, $35
         .byte $35, $36, $36, $37, $37
         .byte $38, $38, $39, $39, $39
         .byte $39, $39, $39, $39, $39
         .byte $39, $39, $39, $39, $39
         .byte $39, $39, $39, $39, $39
         .byte $39, $39, $39, $39, $39
