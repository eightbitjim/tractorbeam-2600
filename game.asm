	processor 6502
	
	; update the path to your dasm includes below:
    include "../dasm/machines/atari2600/vcs.h"
    include "../dasm/machines/atari2600/macro.h"

    ; SET NTSC or PAL mode
NTSC    equ     0
PAL     equ     1
SECAM   equ     2

    IFNCONST PRINTED_WELCOME_MESSAGE

PRINTED_WELCOME_MESSAGE equ 1

    IFNCONST    VIDEO_MODE
        ECHO    "You need to define the video mode by defining constant VIDEO_MODE to either 0 (NTSC), 1 (PAL) or 2 (SECAM)"
        ECHO    "Defaulting to NTSC as not specified"    
VIDEO_MODE  set NTSC
    ENDIF
    
    IF VIDEO_MODE=0
        ECHO "NTSC mode"
    ENDIF
    
    IF VIDEO_MODE=1
        ECHO "PAL mode"
    ENDIF
    
    IF VIDEO_MODE=2
        ECHO "SECAM mode"
    ENDIF
    ENDIF
        
    ; storage location (1st byte in RAM)
nextScanlineChange          = $80             
tempStash                   = $81
playfieldDataPosition       = $82
shipMajorX                       = $83
shipMinorX                  = $84
shipMajorY                       = $85
shipMinorY                  = $86
shipMinorDX                      = $87
shipMinorDY                      = $88
isDead                      = $89
boxMajorX                        = $90
boxMajorY                        = $91
boxMinorX                   = $92
boxMinorY                   = $93
boxMinorDX                       = $94
boxMinorDY                       = $95
boxDrawStartLine            = $96
boxDrawEndLine              = $97

; square root function inputs and outputs
NUMH                        = $98
NUML                        = $99
ROOT                        = $9a
REM                         = $9b
beamIsOn                    = $9c
slackLength                 = $9d
boxMajorDX                  = $9e
boxMajorDY                  = $9f
shipMajorDX                 = $a0
shipMajorDY                 = $a1
forceX                      = $a2
forceY                      = $a3
beamY0                      = $a4
jetX                        = $a5
beamY02                     = $a6
jetY                        = $a7
jetPosition                 = $a8 ; 0:off, 1: top, 2: bottom, 3: left, 4: right
randomSeed                  = $a9
frameCounter                = $aa
explosionCounter            = $ab
beamExtension               = $ac
screenStartY                = $ad
screenEndY                  = $ae
backgroundPointer0          = $af
backgroundPointer1          = $b0
level                       = $b1
beamElasticity              = $b2
boxMass                     = $b3
sceneryAnimationLine1       = $b4
sceneryAnimationLine2       = $b5
sceneryAnimationLength      = $b6
sceneryAnimationOffset      = $b7
sceneryAnimationPosition    = $b8
sceneryAnimationFramesUntilUpdate = $b9
sceneryLaserPosition        = $ba

; storage for playfield data. Allow 16 lines and 4 values for each = 64 bytes. Half our RAM!
sceneryStart0               = $c0
sceneryStart1               = $d0
sceneryStart2               = $e0
sceneryNextLine             = $f0

bottomOfScreen  = 248
topOfScreen     = 8
leftOfScreen    = 6
rightOfScreen   = 159

	seg mySeg
    org $f000
    
; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write

fineAdjustBegin
            dc.b %01110000; 
            dc.b %01100000; 
            dc.b %01010000; 
            dc.b %01000000; 
            dc.b %00110000; 
            dc.b %00100000; 
            dc.b %00010000;
            dc.b %00000000; 
            dc.b %11110000; 
            dc.b %11100000;
            dc.b %11010000; 
            dc.b %11000000; 
            dc.b %10110000; 
            dc.b %10100000;
            dc.b %10010000; 

fineAdjustTable EQU fineAdjustBegin - %11110001;

reset
    ldx #0
    txs
    pha          
    txa
    
clear 
    pha
    dex
    bne clear
    
    lda #0
    sta level
      
    lda #191
    sta screenEndY
    jsr titleScreen
      
startLevel
    ; once only initialisation per level
    jsr copyPlayfieldData
              
    jsr resetBox
    jsr resetPlayer
    
    ; set up sound
    lda #8  ; white noise
    sta AUDC0
    
    lda #6  ; tone
    sta AUDC1
    
    ; set the joystick for input
    lda #0
    sta SWACNT
    sta screenStartY    
    sta COLUP0
    
    lda #$1f
    sta COLUP1
                        
    ; playfield reflection
    lda #1 + 4 + 48 ; playfield reflection and ball really wide and sprites behind playfield
    sta CTRLPF
    sta CXCLR   ; clear collision registers
    
    ; make missles bigger
    lda #16
    sta NUSIZ0
    
startOfFrame    ; Start of vertical blank processing
    lda #0
    sta tempStash
    sta GRP0    ; clear sprite image data
    
    ; start vertical sync
    lda #2
    sta VSYNC

    ; 3 scanlines of VSYNCH signal...
    sta WSYNC
    sta WSYNC
    sta WSYNC

    lda  #43    ; start timer to end of vblank	
	sta  TIM64T
    lda #0
    sta VSYNC           
    
    ; need total of 37 lines of vertical blank.
    
    ; work out jet position
    ldx jetPosition
    ldy randomSeed
    lda jetPosX,x
    clc
    adc shipMajorX
    adc random0,y
    sta jetX
    lda jetPosY,x
    adc shipMajorY
    adc random1,y
    sta jetY
    
    ; update the random seed
    iny
    cpy #10
    bne .notResetSeed
    ldy #0
    
.notResetSeed
    sty randomSeed
    
    ; position the laser gate thingy in its x position
    ldx #1 ; sprite 1
    lda sceneryLaserPosition
    beq .drawLaser ; if set to zero, make the laser invisible
    jsr posObject    
    
    ; set the image and colour
    lda frameCounter
    and #%10000000
    bne .visible
    lda #0 ; invisible
    jmp .drawLaser
    
.visible
    lda frameCounter
    sta COLUP1
    lda #%10101010
    
.drawLaser
    sta GRP1
    
    ; position the ship in its x position
    ldx #0 ; sprite 0
    lda shipMajorX
    jsr posObject    
    
    ; position the box
    ldx #4 ; ball sprite
    lda boxMajorX
    jsr posObject
    
    ; position the jet effect missile
    lda jetX
    ldx #3  ; missile 1
    jsr posObject
    
    ; position the tractor beam missile, if tractor beam is active
    lda beamIsOn
    beq .beamNotOn
    
    clc
    lda shipMajorX
    adc boxMajorX
    ror         ; divides by 2 using carry flag. Neat.
    clc
    adc #4
    ldx #2  ; missile 0
    jsr posObject
    
    ; work out Y position so the missle can be enabled at the right time
    clc
    lda shipMajorY
    adc boxMajorY
    ror
    clc
    adc #4
    sta beamY0
    adc #1
    sta beamY02 
    jmp .beamDone
    
.beamNotOn
    lda #0
    sta beamY0
    sta beamY02
    
.beamDone
    ; get joystick position (includes scanline waits)
    inc frameCounter
    jsr control
        
    ; add DX and DY to x and y
    jsr physics
    jsr physicsNoGravity
    
    ; check for collisions
    jsr collisionCheck
      
    ; update background animation
    lda sceneryAnimationLength
    cmp #0
    beq .doneAnimation
    
    dec sceneryAnimationFramesUntilUpdate
    bne .doneAnimation

    lda #4
    sta sceneryAnimationFramesUntilUpdate
    
    dec sceneryAnimationPosition
    bne .updateAnimation
    
    ; reached end of animation. Reset.
    lda sceneryAnimationLength
    sta sceneryAnimationPosition
    
    ; reverse direction
    lda sceneryAnimationOffset
    eor #$ff
    clc
    adc #1
    sta sceneryAnimationOffset
    
.updateAnimation
    ldx sceneryAnimationLine1
    lda sceneryNextLine,x
    clc
    adc sceneryAnimationOffset
    sta sceneryNextLine,x
        
    ldx sceneryAnimationLine2
    lda sceneryNextLine,x
    clc
    adc sceneryAnimationOffset
    sta sceneryNextLine,x
    
    ; finished animation

.doneAnimation
    ; work out start and end lines to draw box
    lda boxMajorY
    sta boxDrawStartLine
    clc
    adc #8
    sta boxDrawEndLine
    
    jsr applyForce
	      
    ldy screenStartY ; scanline counter
    
    ; prepare so that it counts over shipMajorY zero bytes before getting to the player graphic
    lda screenStartY
    sec
    sbc shipMajorY ; ship draw counter
    tax
          
waitForVblankEnd
	lda INTIM	
	bne waitForVblankEnd	

    ; vertical blank done
    lda #0
    sta WSYNC
    sta HMOVE   
	sta VBLANK 
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp playfieldLoop
    
goToEndScreen
    jmp .endScreen

playfieldLoop    
    sta WSYNC   ; get to the start of the next scanline
    
playfieldLoopNoSync
    ;; FIRST PASS
    ; drawing ship takes (22, 21, 20 -> 10. Saved up to 12 cycles == 36 sprite pixels, or 9 playfield pixels)
    lda shipStart,x ;   (4+)
    sta GRP0        ;   (4) modify sprite shapes
    
    lda (backgroundPointer0),y
    sta COLUBK      
    inx             ;   (2)
    
    ; TODO save lots of cycles by having a list of points to start and stop drawing objects
    
    ; draw box?
    cpy boxDrawStartLine
    bne .notDrawBox1
    lda #255
    sta ENABL
    bne .doneBox ; always branch
    
.notDrawBox1
    cpy boxDrawEndLine
    bne .doneBox
    lda #0
    sta ENABL

.doneBox
    ; draw tractor beam?
    cpy beamY0
    bne .notBeam0
    lda #255
    sta ENAM0
    bne .doneBeam0 ; always branch
    
.notBeam0     
    lda #0
    sta ENAM0
    
.doneBeam0
    ; draw jet effect : (15) and (13) 
    cpy jetY            ;   (4)
    bne .notJet0        ;   (2,3) - check page boundary
    lda #255            ;   (2)
    sta ENAM1           ;   (4)
    bne .doneJet        ;   (3) ; always branch
    
.notJet0
    lda #0              ;   (2)
    sta ENAM1           ;   (4)
        
.doneJet
.checkPlayfield
    ; now prepare for next scanline
    iny
    cpy screenEndY
    beq goToEndScreen
        
    ; time to change to next playfield data on next scanline?  
    cpy nextScanlineChange
    bcc playfieldLoop ; no, just move to next scanline

    ; yes change the playfield and do one extra scanline to catch up again
    lda (backgroundPointer0),y

    sty tempStash   ; do this now as it uses up time and then we are at the start of the next scanline
    ldy playfieldDataPosition

    ; we are already at the start of a new scanline, so no need to write to WSYNC
    sta COLUBK      ; change the background colour

    ; now change the playfield, interleaving the ship drawing    
    lda sceneryStart0,y
    sta PF0
    
    ; drawing ship takes (22, 21, 20 -> 10. Saved up to 12 cycles == 36 sprite pixels, or 9 playfield pixels)
    lda shipStart,x ;   (4+)
    sta GRP0        ;   (4) modify sprite shapes
    inx             ;   (2)
    
    lda sceneryStart1,y
    sta PF1
    
    lda sceneryStart2,y
    sta PF2
    
    lda sceneryNextLine,y
    sta nextScanlineChange
    iny
    
    sty playfieldDataPosition
    ldy tempStash
    
    ; TODO save lots of cycles by having a list of points to start and stop drawing objects
    
    ; draw box?
    cpy boxDrawStartLine
    bne .notDrawBox1SP
    lda #255
    sta ENABL
    jmp .doneBoxSP
    
.notDrawBox1SP
    cpy boxDrawEndLine
    bne .doneBoxSP
    lda #0
    sta ENABL

.doneBoxSP
    ; don't draw tractor beam to save a few cycles
    
.doneBeam0SP
    ; draw jet effect : (15) and (13) 
    cpy jetY            ;   (4)
    bne .notJet0SP        ;   (2,3) - check page boundary
    lda #255            ;   (2)
    sta ENAM1           ;   (4)
    jmp .doneJetSP        ;   (3)
    
.notJet0SP
    lda #0              ;   (2)
    sta ENAM1           ;   (4)
        
.doneJetSP
    ; now prepare for next scanline. Don't wait for end of scanline
    iny
    cpy screenEndY
    beq .endScreen

    ; third pass, draw next line and assume no playfield changes
    nop ; delay until almost start of horizontal blanking (1 cycle too short...)
        
    ; drawing ship takes (22, 21, 20 -> 10. Saved up to 12 cycles == 36 sprite pixels, or 9 playfield pixels)
    lda shipStart,x ;   (4+) 
    sta GRP0        ;   (4) modify sprite shapes

    ; TODO save lots of cycles by having a list of points to start and stop drawing objects
    lda (backgroundPointer0),y
    sta COLUBK  

    inx             ;   (2)

    ; draw box?
    cpy boxDrawStartLine
    bne .notDrawBox1TP
    lda #255
    sta ENABL
    jmp .doneBoxTP
    
.notDrawBox1TP
    cpy boxDrawEndLine
    bne .doneBoxTP
    lda #0
    sta ENABL

.doneBoxTP
    ; draw tractor beam?
    cpy beamY0
    bne .notBeam0TP
    lda #255
    sta ENAM0
    jmp .doneBeam0TP
    
.notBeam0TP
    lda #0
    sta ENAM0
    
.doneBeam0TP
    ; draw jet effect : (15) and (13) 
    cpy jetY            ;   (4)
    bne .notJet0TP        ;   (2,3) - check page boundary
    lda #255            ;   (2)
    sta ENAM1           ;   (4)
    jmp .doneJetTP        ;   (3)
    
.notJet0TP
    lda #0              ;   (2)
    sta ENAM1           ;   (4)
        
.doneJetTP
    ; now prepare for next scanline. Don't wait for end of scanline
    iny
    cpy screenEndY
    beq .endScreen
    jmp playfieldLoopNoSync
     
.endScreen   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; now the bottom part of the screen
    ; wait for end of the current line before doing anything
    sta WSYNC
    
    lda #2
    sta VBLANK                     ; end of screen - enter blanking

    ; stop drawing the box, ship and everything else
    lda #0
    sta ENABL
    sta GRP0
    sta ENAM0
    sta ENAM1
    
    ; 30 scanlines of overscan...
    lda  #35   ; start timer to end of vblank	
	sta  TIM64T

    ; do some work while overscan is in progress
    ; check if ship and box are both in the left hand pool. If so, move to the
    ; next level
    lda shipMajorY
    cmp #bottomOfScreen - 2
    bne .notWon
    
    lda boxMajorY
    cmp #(bottomOfScreen - 2)
    bne .notWon
    
    lda shipMajorX
    cmp #(rightOfScreen / 2)
    bpl .notWon
    
    lda boxMajorX
    cmp #(rightOfScreen / 2)
    bpl .notWon

    ; won
    inc level
    jmp startLevel
        
.notWon
    ; work out screen Y position for next frame
    lda shipMajorY
    sec
    sbc #90
    bcs .notTooLow
    lda #0

.notTooLow
    cmp #255-191
    bcc .notTooHigh
    lda #255-191
    
.notTooHigh
    sta screenStartY
    clc
    adc #191
    sta screenEndY
     
    ; work out the top playfield bytes
    ldx #255
    lda screenStartY
    
.findPlayfieldDataLoop
    inx
    cmp sceneryNextLine,x
    bcs .findPlayfieldDataLoop
 
    lda sceneryStart0,x
    sta PF0
    lda sceneryStart1,x
    sta PF1
    lda sceneryStart2,x
    sta PF2
    lda sceneryNextLine,x
    sta nextScanlineChange
    
    inx
    stx playfieldDataPosition
    
    ; set up pointers so the background colours get drawn correctly
    ldx level
    lda #sceneryColourIndexLow,x
    sec
    sbc screenStartY
    sta backgroundPointer0
    lda #sceneryColourIndexHigh,x
    sbc #0
    sta backgroundPointer1
    
    ; make explosion sound if necessary
    lda explosionCounter
    beq .notExplosion
    
    lda #32
    sec
    sbc explosionCounter    
    sta AUDF0

    lda explosionCounter
    clc
    lsr
    sta COLUPF
    
    lda #15
    sta AUDV0
    dec explosionCounter
    jmp .notJetSound
    
.notExplosion
    ; make jet sound
    ldx jetPosition
    lda jetFrequency,x
    sta AUDF0
    lda jetVolume,x
    sta AUDV0
        
.notJetSound
    ; make tractor beam sound
    lda #0
    
    ldx beamIsOn
    beq .beamVolumeDecided
        
    lda #255
    sbc beamExtension
    sta AUDF1
    
    lda #15  ; volume 5. Not too loud.

.beamVolumeDecided
    sta AUDV1
    
.waitForOverscanToComplete
    lda INTIM	
	bne .waitForOverscanToComplete	
    jmp startOfFrame

control subroutine
    ldy #0 ; default to jet off
    lda SWCHA ; joystick input
    and #192 
    cmp #128 ; left
    bne .notLeft
    lda frameCounter
    ror ; sets carry with LSB. If set, can show horizontal jets, if clear can show vertical
    bcc .notRightJet
    ldy #4  ; jet right
.notRightJet
    ldx shipMinorDX
    cpx #$81
    beq .notRight
    cpx #$80
    beq .notRight
    dex
    stx shipMinorDX
    cpx #$81
    beq .notRight
    dex
    stx shipMinorDX
    cpx #$81
    beq .notRight
    dex
    stx shipMinorDX
    jmp .notRight
.notLeft
    cmp #64 ; right
    bne .notRight
    lda frameCounter
    ror ; sets carry with LSB. If set, can show horizontal jets, if clear can show vertical
    bcc .notLeftJet
    ldy #3 ; jet left
.notLeftJet
    ldx shipMinorDX
    cpx #$7f
    beq .notRight
    inx
    stx shipMinorDX
    cpx #$7f
    beq .notRight
    inx
    stx shipMinorDX
    cpx #$7f
    beq .notRight
    inx
    stx shipMinorDX
.notRight
    ; vertical movement
    lda SWCHA ; joystick input
    and #48
    cmp #32
    bne .notUp
    lda frameCounter
    ror ; sets carry with LSB. If set, can show horizontal jets, if clear can show vertical
    bcs .notDownJet
    ldy #2 ; jet down
.notDownJet
    ldx shipMinorDY
    cpx #$81
    beq .notUp
    dex
    stx shipMinorDY
    cpx #$81
    beq .notUp
    dex
    stx shipMinorDY
    cpx #$81
    beq .notUp
    dex
    stx shipMinorDY
    jmp .notDown
.notUp
    cmp #16 ; down
    bne .notDown
    lda frameCounter
    ror ; sets carry with LSB. If set, can show horizontal jets, if clear can show vertical
    bcs .notUpJet
    ldy #1 ; jet up
.notUpJet
    ldx shipMinorDY
    cpx #$7f
    beq .notDown
    inx
    stx shipMinorDY
    cpx #$7f
    beq .notDown
    inx
    stx shipMinorDY
    cpx #$7f
    beq .notDown
    inx
    stx shipMinorDY
.notDown
    
.doneShipX
    sty jetPosition
    rts
    
doFriction
    clc
    lsr
    lsr
    lsr
    lsr
    tax
    lda frictionTable,x
    clc
    rts
    
allPhysicsDone2
    rts
    
physicsNoGravity subroutine
    lda isDead
    bne allPhysicsDone2
    jmp doneGravityAndFriction

noPhysics
    rts
    
physics 
    lda isDead
    bne noPhysics
    
    ; add friction based on lookup table on top 4 bits
    lda shipMinorDY
    jsr doFriction
    adc shipMinorDY
    sta shipMinorDY   
    
    lda shipMinorDX
    jsr doFriction
    adc shipMinorDX
    sta shipMinorDX

    lda boxMinorDY
    jsr doFriction
    adc boxMinorDY
    sta boxMinorDY  

    lda boxMinorDX
    jsr doFriction
    adc boxMinorDX
    sta boxMinorDX  
    ; done friction
    
    ; add gravity
    ldx shipMinorDY
    cpx #$7f
    beq doneGravityAndFriction
    inx
    stx shipMinorDY
        
doneGravityAndFriction
    ; add box gravity
    ldx boxMinorDY
    cpx #$7f
    beq doneBoxGravity
    inx
    stx boxMinorDY

doneBoxGravity    
    ; add velocity to position
    ; use signed addition of 8 bit value to 16 bit value
    ; first X
    ldx #00  
    lda shipMinorDX
    bpl .notAddDX ; positive or negative value?
    dex      ; high byte becomes $ff to reflect negative delta
.notAddDX
    clc
    adc shipMinorX
    sta shipMinorX
    txa
    adc shipMajorX
    sta shipMajorX

    ; bounce?
    cmp #leftOfScreen
    bne .notBounceLeft
    inc shipMajorX
    jmp .reverseDX
    
.notBounceLeft    
    cmp #rightOfScreen
    bne .doneBounceX
    dec shipMajorX
    
.reverseDX
    lda shipMinorDX
    eor #255
    clc
    adc #1
    sta shipMinorDX
    
.doneBounceX
    ; now Y
    ldx #00  
    lda shipMinorDY
    bpl .notAddDY
    dex      ; high byte becomes $ff to reflect negative delta
.notAddDY
    clc
    adc shipMinorY
    sta shipMinorY
    txa
    adc shipMajorY
    sta shipMajorY
    
    ; bounce?
    cmp #bottomOfScreen
    beq .bounce
    jmp .physicsDone
    
.bounce    
    dec shipMajorY
    lda shipMinorDY
    lsr
    eor #255
    clc
    adc #1
    sta shipMinorDY
    
    ; reduce horizontal speed if bouncing
    lda #0
    sta shipMajorDX
    sta shipMinorDX

.physicsDone
    ; add box velocity to position
    ; first X
    ldx #00  
    lda boxMinorDX
    bpl .notAddBoxDX
    dex      ; high byte becomes $ff to reflect negative delta
.notAddBoxDX
    clc
    adc boxMinorX
    sta boxMinorX
    txa
    adc boxMajorX
    sta boxMajorX

    ;bounce?
    cmp #leftOfScreen
    bne .notBoxBounceLeft
    
    ; bounce the box off the left of the screen
    inc boxMajorX
    jmp .reverseBoxDX
    
.notBoxBounceLeft
    cmp #rightOfScreen
    bne .doneBoxBounceX
    
    ; bounce the box off the right of the screen
    dec boxMajorX
    
.reverseBoxDX
    lda boxMinorDX
    eor #255
    clc
    adc #1
    sta boxMinorDX

.doneBoxBounceX
    ; now Y
    ldx #00  
    lda boxMinorDY
    bpl .notAddBoxDY
    dex      ; high byte becomes $ff to reflect negative delta
.notAddBoxDY
    clc
    adc boxMinorY
    sta boxMinorY
    txa
    adc boxMajorY
    sta boxMajorY
    
    ; bounce?
    cmp #bottomOfScreen
    bne .allPhysicsDone
    
    dec boxMajorY
    lda boxMinorDY
    lsr
    eor #255
    clc
    adc #1
    sta boxMinorDY
    
    ; reduce horizontal speed if bouncing
    lda #0
    sta boxMajorDX
    sta boxMinorDX
.allPhysicsDone
    rts
            
collisionCheck subroutine
    lda CXP0FB  ; check player 0 hitting playfield
    and #128
    beq .doneCheck1
    jsr resetPlayer

.doneCheck1
    lda CXPPMM  ; check player 0 hitting player 1 (laser)
    and #128
    beq .doneCheck2
    jsr resetPlayer

.doneCheck2    
    lda CXBLPF  ; check ball hitting playfield (bit 7)
    and #128
    beq .doneCheck3
    jsr resetBox    

.doneCheck3
    lda CXP1FB  ; check ball hitting player 1 (laser)
    and #64
    beq .doneCheck4
    jsr resetBox    

.doneCheck4
    lda CXP0FB  ; check ball hitting player
    and #64
    beq .doneCheck5  
    jsr resetBox
    jsr resetPlayer
    
.doneCheck5
    ; reset collision registers
    sta CXCLR
    rts
    
resetBox subroutine    
    lda #134
    sta boxMajorX
    
    lda #bottomOfScreen - 1
    sta boxMajorY
    
    lda #0
    sta boxMinorDX
    sta boxMinorDY   
    sta beamIsOn 
    
    lda #32
    sta explosionCounter
    rts
    
resetPlayer subroutine
    lda #30
    sta shipMajorX
    
    lda #bottomOfScreen - 1
    sta shipMajorY
    
    lda #0
    sta shipMinorX
    sta shipMinorY
    sta shipMinorDX
    sta shipMinorDY
    sta isDead
    sta beamIsOn 
    
    lda #32
    sta explosionCounter
    rts
    
;-----------------------------

applyForce subroutine
    ; first work out how far away the box is
    jsr calculateDistance
    
    lda ROOT
    sec
    sbc slackLength ; are we close enough to engage the tractor beam?
    bcs .beyondSlackLength
    
    ; turn tractor beam on
    ldx #128
    stx beamIsOn
    jmp .done
    
.beyondSlackLength
    ; is the tractor beam on?
    ldx beamIsOn
    beq .done
    
    sta beamExtension
    
    ; force is proportional to tractor beam extension beyond the slack length    
    ; scale the force and then apply it
    ; fx = (x - bx) * distance , then apply to minor dx value
    lda shipMajorX
    sec
    sbc boxMajorX
    
    ; reduce force to make beam springy
    ldy beamElasticity
    jsr reduceAccumulator
    sta forceX
    
    clc
    adc boxMinorDX
    bvc .notOverflowDX
    
    ; sign bit is incorrect as overflow or underflow.
    bmi .underflowDX ; sign bit is set, but number should be positive
    lda #$81 ; max negative value
    jmp .notOverflowDX
.underflowDX
    lda #$7f ; max positive value
.notOverflowDX
    sta boxMinorDX

    ; now apply inverse of force to the ship, reduced by the box mass
    lda forceX
    ldy boxMass
    jsr reduceAccumulator
    sta forceX
    
    lda shipMinorDX
    sec
    sbc forceX
    bvc .notOverflowShipDX
    ; sign bit is incorrect as overflow or underflow.
    bmi .underflowShipDX ; sign bit is set, but number should be positive
    lda #$81 ; max negative value
    jmp .notOverflowShipDX
.underflowShipDX
    lda #$7f ; max positive value
.notOverflowShipDX
    sta shipMinorDX    
    
    lda shipMajorY
    sec
    sbc boxMajorY
    
    ; reduce force to make beam springy
    ldy beamElasticity
    jsr reduceAccumulator
    sta forceY
    
    clc
    adc boxMinorDY
    bvc .notOverflowDY
    ; sign bit is incorrect as overflow or underflow.
    bmi .underflowDY ; sign bit is set, but number should be positive
    lda #$81 ; max negative value
    jmp .notOverflowDY
.underflowDY
    lda #$7f ; max positive value
.notOverflowDY   
    sta boxMinorDY
    
    ; reduce force on ship by box mass amount
    lda forceY
    ldy boxMass
    jsr reduceAccumulator
    sta forceY
    
    lda shipMinorDY
    sec
    sbc forceY
    bvc .notOverflowShipDY
    ; sign bit is incorrect as overflow or underflow.
    bmi .underflowShipDY ; sign bit is set, but number should be positive
    lda #$81 ; max negative value
    jmp .notOverflowShipDY
.underflowShipDY
    lda #$7f ; max positive value
.notOverflowShipDY
    sta shipMinorDY    
    
.done
    rts
   
    ; shift the accumulator right y spaces preserving the sign
reduceAccumulator subroutine
    cpy #0
    beq .done
    
    clc
    cmp #0
    bpl .positive
    
    ; negative number. Make positive, shift, then make negative
    clc
    eor #$ff
    adc #1
    clc
.shiftLoop1
    lsr
    dey
    bne .shiftLoop1
    
    ; make negative
    clc
    eor #$ff
    adc #1
    rts
.positive
.shiftLoop2
    lsr
    dey
    bne .shiftLoop2
.done
    rts
         
;; calculate distance between two points, specifically box and spaceship. Ignore the
;; minor values, just use the major 8 bits.
   
calculateDistance subroutine
    lda #0
    sta NUMH
    sta NUML
    
    sec
    lda boxMajorX
    sbc shipMajorX
    bcs .sub1done ; positive result
    eor #255 ; invert number
    adc #1    
.sub1done
    lsr ; divide by 2
    cmp #30
    bcs .returnOutOfRange ; out of range
    tax
    lda squares,x
    sta NUML
    lda squaresHigh,x
    sta NUMH
.square1done

    sec
    lda boxMajorY
    sbc shipMajorY
    bcs .sub2done 
    eor #255
    adc #1
.sub2done
    lsr ; divide by 2
    cmp #30
    bcs .returnOutOfRange ; out of range
    tax
    lda squares,x
    clc
    adc NUML
    sta NUML
    lda squaresHigh,x
    adc NUMH
    sta NUMH
    
;; Square root. Thanks to 6502.org for this sample code on their wiki
squareRoot
    lda #0
    sta ROOT
    sta REM
    ldx #8
.L1 sec
    lda NUMH
    sbc #$40
    tay
    lda REM
    sbc ROOT
    bcc .L2
    sty NUMH
    sta REM
.L2 rol ROOT
    asl NUML
    rol NUMH
    rol REM
    asl NUML
    rol NUMH
    rol REM
    dex
    bne .L1
.return
    rts
.returnOutOfRange
    lda #255
    sta ROOT
    rts

copyPlayfieldData subroutine
    ; copy data for this level from ROM to the 64 bytes of RAM set aside
    ldx level
    ldy #0
    
.dataLoop
    lda sceneryDataIndexLow0,x
    sta backgroundPointer0
    lda sceneryDataIndexHigh0,x
    sta backgroundPointer1
    lda (backgroundPointer0),y
    sta sceneryStart0,y

    lda sceneryDataIndexLow1,x
    sta backgroundPointer0
    lda sceneryDataIndexHigh1,x
    sta backgroundPointer1
    lda (backgroundPointer0),y
    sta sceneryStart1,y

    lda sceneryDataIndexLow2,x
    sta backgroundPointer0
    lda sceneryDataIndexHigh2,x
    sta backgroundPointer1
    lda (backgroundPointer0),y
    sta sceneryStart2,y

    lda sceneryDataIndexLowNextLine,x
    sta backgroundPointer0
    lda sceneryDataIndexHighNextLine,x
    sta backgroundPointer1
    lda (backgroundPointer0),y
    sta sceneryNextLine,y

    iny
    
    cmp #255
    bne .dataLoop

    ; read the statistics from end of sceneryDataNextLine array for this level
    ; scenery stats: beam slack length, elasticity, boxMass
    
    ; read beam length
    lda (backgroundPointer0),y
    sta slackLength
    
    ; elasticity
    iny
    lda (backgroundPointer0),y
    sta beamElasticity
    
    ; box mass
    iny
    lda (backgroundPointer0),y
    sta boxMass
    
    iny
    lda (backgroundPointer0),y
    sta sceneryAnimationLine1

    iny
    lda (backgroundPointer0),y
    sta sceneryAnimationLine2
    
    iny
    lda (backgroundPointer0),y
    sta sceneryAnimationLength
    sta sceneryAnimationPosition
    
    iny
    lda (backgroundPointer0),y
    sta sceneryAnimationOffset
    
    iny
    lda (backgroundPointer0),y
    sta sceneryLaserPosition
    
    lda #4
    sta sceneryAnimationFramesUntilUpdate

    rts
    
;;;; Title screen
titleScreen
    ; set the joystick for input
    lda #0
    sta SWACNT
        
    ; set up colours
    lda #56
    sta shipMajorX
    lda #64
    sta shipMinorX
    
    ; playfield reflection
    lda #1 + 4 + 48 ; playfield reflection and ball really wide and sprites behind playfield
    sta CTRLPF
        
    lda #$ff
    sta COLUPF
    
    ldx #0 ; first colour set  
    lda #sceneryColourIndexLow,x
    sta backgroundPointer0
    lda #sceneryColourIndexHigh,x
    sta backgroundPointer1
        
textFrameStart    ; Start of vertical blank processing
    lda #0
    sta GRP0    ; clear sprite image data
    sta nextScanlineChange
    
    ; start vertical sync
    lda #2
    sta VSYNC

    ; 3 scanlines of VSYNCH signal...
    sta WSYNC
    sta WSYNC
    sta WSYNC

    lda  #43    ; start timer to end of vblank	
	sta  TIM64T
    lda #0
    sta VSYNC           
    
    ; need total of 37 lines of vertical blank.
    ldy #0 ; scanline counter
    
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta COLUBK
    
    ; set up playfield pointers
    ldx #255 ; offset into playfield data
    
    lda shipMajorX
    sta COLUPF
    dec shipMinorX
    bne .notIncreaseColour
    
    lda shipMajorX
    clc
    adc #28
    sta shipMajorX
    lda #64
    sta shipMinorX
    
.notIncreaseColour
    ; check for user hitting fire
    lda INPT4
	bmi textWaitForVblankEnd
    
    ; yes, finished
    rts
    
textWaitForVblankEnd
	lda INTIM	
	bne textWaitForVblankEnd	

    ; vertical blank done
    sta WSYNC
    sta HMOVE   
	sta VBLANK 
    
textPlayfieldLoop    
    sta WSYNC   ; get to the start of the next scanline
    lda (backgroundPointer0),y
    sta COLUBK 
    

    ; left hand of screen
textPlayfieldLoopNoSync
    lda text0Start0,x
    sta PF0
    lda text0Start1,x
    sta PF1
    lda text0Start2,x
    sta PF2

    ; right hand of screen
    nop
    nop
    nop
    
    lda text0Start5,x
    sta PF0  
              
    lda text0Start3,x
    sta PF2

    lda text0Start4,x
    sta PF1
    
    ; now prepare for next scanline
    iny
    cpy screenEndY
    beq textEndScreen
        
    ; time to change to next playfield data on next scanline?  
    cpy nextScanlineChange
    bcc textPlayfieldLoop ; no, just move to next scanline

    ; yes change the playfield
    inx
    lda text0NextLine,x
    sta nextScanlineChange
    nop
    jmp textPlayfieldLoopNoSync   
    
textEndScreen
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; now the bottom part of the screen
    ; wait for end of the current line before doing anything
    sta WSYNC
    
    lda #2
    sta VBLANK                     ; end of screen - enter blanking

    ; stop drawing the box, ship and everything else
    lda #0
    sta ENABL
    sta GRP0
    sta ENAM0
    sta ENAM1
    
    ; 30 scanlines of overscan...
    lda  #35   ; start timer to end of vblank	
	sta  TIM64T
	
.textWaitForOverscanToComplete
    lda INTIM	
	bne .textWaitForOverscanToComplete	
    jmp textFrameStart
    
; data for text block 0

text0Start0
    dc.b %00000000, %11110000, %11110000, %01000000, %01000000, %01000000, %01000000, %01000000, %00000000, %11110000, %11110000, %10110000, %10110000, %11110000, %10110000, %11110000, %00000000
text0Start1
    dc.b %00000000, %10111100, %10111110, %00110110, %00111100, %00110110, %00110110, %00110110, %00000000, %00011111, %00011111, %10011000, %10011000, %00011100, %10011000, %00011111, %00000000
text0Start2
    dc.b %00000000, %10001110, %11011111, %01011011, %01011111, %01011011, %11011011, %10011011, %00000000, %10011100, %10111110, %10110110, %10110110, %10111110, %10110110, %10110110, %00000000
text0Start3
    dc.b %00000000, %11011111, %11011111, %00000100, %00000100, %00000100, %11000100, %11000100, %00000000, %11001110, %11111110, %10110110, %10000110, %10000110, %10000110, %10000110, %00000000
text0Start4
    dc.b %00000000, %10011100, %10111110, %10110110, %10110110, %10110110, %10111110, %10011100, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
text0Start5
    dc.b %00000000, %11100000, %11110000, %10110000, %11100000, %10110000, %10110000, %10110000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
text0NextLine
    dc.b 16, 24, 32, 48, 64, 72, 80, 88, 104, 112, 120, 128, 136, 152, 168, 184, 255
        
;; mark this as the last byte, as it is the last byte before we start aligning data and
;; functions with page boundaries for performance reasons.
lastByte
    dc.b    0
   
    org $fa00
    
endOfFreeSpace

;; Thanks to AtariAge.com for sharing this piece of code and associated lookup table.
;; I have modified it slightly as it was not possible to display a sprite in the left
;; most 16 pixels (or at least, it was positioned incorrectly).
posObject   SUBROUTINE
            cmp #16 ; is it in the left most 16 pixels?
            bcc .notDelay
            sta WSYNC             
            sec                    
.divideby15 sbc #15               
            bcs .divideby15        
            tay
            lda fineAdjustTable,y   
            sta HMP0,x
            sta RESP0,x                  
            rts 
.notDelay
            ; left-most 16 pixels. Need slightly different timing.
            sec
            sbc #18
            tay
            sta WSYNC
            sta RESP0,x   
            lda fineAdjustTable,y   
            sta HMP0,x
            rts 
            
; data for level 3
scenery3Start0
    dc.b %11110000, 0, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
scenery3Start1
    dc.b %11111111, 0, %00000000, %00000000, %00000000, %00000000, %00000010, %00000010
scenery3Start2
    dc.b %11111111, 0, %00010100, %00000000, %00010100, %00010101, %00010101, %11110101
scenery3NextLine
    dc.b 8, 16, 40, 184, 208, 232, 248, 255
scenery3Stats ; needs to follow on after NextLine array
    dc.b 10, 4, 1,    1, 2, 32, 4,    84
    
scenery1Start0
    dc.b %11110000, %00000000, %00000000, %00000000, %00000000, %00000000
scenery1Start1
    dc.b %11111111, %00000001, %00000111, %00000000, %00000111, %00000000
scenery1Start2
    dc.b %11111111, %00000001, %00000111, %00000000, %11111111, %00000101
scenery1NextLine
    dc.b 8, 56, 72, 144, 160, 255
scenery1Stats ; needs to follow on after NextLine array
    dc.b 10, 3, 1, 1, 2, 70, 1, 0 

    org $fb00
    ;; table of 32 squares. To get a square of a number x < 32, use lda squares,x
squares
    dc.b    0 , 1 , 4 , 9 , 16 , 25 , 36 , 49 , 64 , 81 , 100 , 121 , 144 , 169 , 196 , 225
    dc.b    0 , 33 , 68 , 105 , 144 , 185 , 228 , 17 , 64 , 113 , 164 , 217 , 16 , 73 , 132 , 193

squaresHigh
    dc.b    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    dc.b    1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3

;; jet position relative to ship x and y coordinates
;; jet off, up, down, left right
jetPosX
    dc.b    4, 4, 4, 254, 9
    
jetPosY    
    dc.b    4, 252, 8, 3, 3

; 10 random bits    
random0
    dc.b    0,1,0,0,1,0,1,1,0,1
random1
    dc.b    1,1,0,1,0,1,0,0,1,0
    
    ; scenery data level 0. PF0, PF1, PF2, nextScanlineToChange. Remember PF0 and PF2 are reversed. PF0 only top 4 bits are used.
    ; scenery stats: beam slack length, elasticity, box mass, senery animation line1, line2, length, offset, laser x position
    
scenery0Start0
    dc.b %11110000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00010000
scenery0Start1
    dc.b %11111111, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
scenery0Start2
    dc.b %11111111, %11111000, %11100000, %00000000, %11110000, %11111000, %11111100, %11111110, %11111111, %11111111
scenery0NextLine
    dc.b 8, 16, 24, 128, 136, 144, 168, 240, 248, 255
scenery0Stats ; needs to follow on after NextLine array
    dc.b 10, 3, 1, 1, 2, 100, 1, 84
    
; data for level 2

scenery2Start0
    dc.b %11110000, %00110000, %00010000, %00010000, %00110000, %01110000, %11110000, %11110000
scenery2Start1
    dc.b %11111111, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10000000
scenery2Start2
    dc.b %11111111, %00000000, %00000000, %11111110, %11111100, %11111000, %11110000, %11100000
scenery2NextLine
    dc.b 8, 24, 80, 104, 128, 160, 208, 255
    dc.b 15, 3, 0, 0, 0, 0, 0, 84
    
numberOfLevels  equ 4

sceneryDataIndexLow0
    dc.b    <scenery0Start0, <scenery1Start0, <scenery2Start0, <scenery3Start0
sceneryDataIndexLow1
    dc.b    <scenery0Start1, <scenery1Start1, <scenery2Start1, <scenery3Start1
sceneryDataIndexLow2
    dc.b    <scenery0Start2, <scenery1Start2, <scenery2Start2, <scenery3Start2
sceneryDataIndexLowNextLine
    dc.b    <scenery0NextLine, <scenery1NextLine, <scenery2NextLine, <scenery3NextLine

sceneryDataIndexHigh0
    dc.b    >scenery0Start0, >scenery1Start0, >scenery2Start0, >scenery3Start0
sceneryDataIndexHigh1
    dc.b    >scenery0Start0, >scenery1Start1, >scenery2Start1, >scenery3Start1
sceneryDataIndexHigh2
    dc.b    >scenery0Start0, >scenery1Start2, >scenery2Start2, >scenery3Start2
sceneryDataIndexHighNextLine
    dc.b    >scenery0NextLine, >scenery1NextLine, >scenery2NextLine, >scenery3NextLine
    
sceneryColourIndexLow
    dc.b    <sceneryColoursLevel0, <sceneryColoursLevel1, <sceneryColoursLevel0, <sceneryColoursLevel1
    
sceneryColourIndexHigh
    dc.b    >sceneryColoursLevel0, >sceneryColoursLevel1, >sceneryColoursLevel0, >sceneryColoursLevel1
    
    org $fc00
sceneryColoursLevel0
    IF VIDEO_MODE=NTSC
    REPEAT  18
        dc.b    $50
    REPEND
    REPEAT  32
        dc.b    $50 
    REPEND
    REPEAT  28
        dc.b    $52 
    REPEND
    REPEAT  24
        dc.b    $54
    REPEND
    REPEAT  20
        dc.b    $56 
    REPEND
    REPEAT  16
        dc.b    $58 
    REPEND
    REPEAT  10
        dc.b    $5a 
    REPEND
    REPEAT  6
        dc.b    $5c 
    REPEND
    REPEAT  2
        dc.b    $5e 
    REPEND
    REPEAT 2
        dc.b    $7e
    REPEND
    REPEAT 2
        dc.b    $7c
    REPEND
    REPEAT 2
        dc.b    $7a
    REPEND
    REPEAT 2
        dc.b    $78
    REPEND
    REPEAT 4
        dc.b    $76
    REPEND
    REPEAT 4
        dc.b    $74
    REPEND
    REPEAT 6
        dc.b    $72
    REPEND
    REPEAT 8
        dc.b    $70
    REPEND
    REPEAT  6
        dc.b    $90
    REPEND
    ENDIF
    
    IF VIDEO_MODE=PAL
    REPEAT  17
        dc.b    $80
    REPEND
    REPEAT  32
        dc.b    $80 
    REPEND
    REPEAT  28
        dc.b    $82 
    REPEND
    REPEAT  24
        dc.b    $84
    REPEND
    REPEAT  20
        dc.b    $86 
    REPEND
    REPEAT  16
        dc.b    $88 
    REPEND
    REPEAT  12
        dc.b    $8a 
    REPEND
    REPEAT  8
        dc.b    $8c 
    REPEND
    REPEAT  4
        dc.b    $8e 
    REPEND
    REPEAT 1
        dc.b    $ce
    REPEND
    REPEAT 1
        dc.b    $cc
    REPEND
    REPEAT 2
        dc.b    $ca
    REPEND
    REPEAT 3
        dc.b    $c8
    REPEND
    REPEAT 4
        dc.b    $c6
    REPEND
    REPEAT 4
        dc.b    $c4
    REPEND
    REPEAT 6
        dc.b    $c2
    REPEND
    REPEAT 8
        dc.b    $c0
    REPEND
    REPEAT  3
        dc.b    $90
    REPEND    
    
    ENDIF
    
    org $fd00
sceneryColoursLevel1
    IF VIDEO_MODE=PAL
    REPEAT  18
        dc.b    $50
    REPEND
    REPEAT  32
        dc.b    $50 
    REPEND
    REPEAT  28
        dc.b    $52 
    REPEND
    REPEAT  24
        dc.b    $54
    REPEND
    REPEAT  20
        dc.b    $56 
    REPEND
    REPEAT  16
        dc.b    $58 
    REPEND
    REPEAT  10
        dc.b    $5a 
    REPEND
    REPEAT  6
        dc.b    $5c 
    REPEND
    REPEAT  2
        dc.b    $5e 
    REPEND
    REPEAT 2
        dc.b    $7e
    REPEND
    REPEAT 2
        dc.b    $7c
    REPEND
    REPEAT 2
        dc.b    $7a
    REPEND
    REPEAT 2
        dc.b    $78
    REPEND
    REPEAT 4
        dc.b    $76
    REPEND
    REPEAT 4
        dc.b    $74
    REPEND
    REPEAT 6
        dc.b    $72
    REPEND
    REPEAT 8
        dc.b    $70
    REPEND
    REPEAT  6
        dc.b    $90
    REPEND
    ENDIF
    
    IF VIDEO_MODE=NTSC
    REPEAT  17
        dc.b    $80
    REPEND
    REPEAT  32
        dc.b    $80 
    REPEND
    REPEAT  28
        dc.b    $82 
    REPEND
    REPEAT  24
        dc.b    $84
    REPEND
    REPEAT  20
        dc.b    $86 
    REPEND
    REPEAT  16
        dc.b    $88 
    REPEND
    REPEAT  12
        dc.b    $8a 
    REPEND
    REPEAT  8
        dc.b    $8c 
    REPEND
    REPEAT  4
        dc.b    $8e 
    REPEND
    REPEAT 1
        dc.b    $ce
    REPEND
    REPEAT 1
        dc.b    $cc
    REPEND
    REPEAT 2
        dc.b    $ca
    REPEND
    REPEAT 3
        dc.b    $c8
    REPEND
    REPEAT 4
        dc.b    $c6
    REPEND
    REPEAT 4
        dc.b    $c4
    REPEND
    REPEAT 6
        dc.b    $c2
    REPEND
    REPEAT 8
        dc.b    $c0
    REPEND
    REPEAT  3
        dc.b    $90
    REPEND    
    
    ENDIF
    
    org $fe00
shipStart
    ; display ship and then 248 blanks
    dc.b    %00111100
    dc.b    %00011000
    dc.b    %10011001
    dc.b    %11111111
    dc.b    %11111111
    dc.b    %10011001
    dc.b    %00011000
    dc.b    %00111100
    REPEAT  248
        dc.b   0
    REPEND
    
    org $ff00
    ; sound effect values
jetFrequency
    dc.b    0, 10, 20, 30, 40
    
jetVolume
    dc.b    0, 10, 10, 10, 10
    
frictionTable
    ; positive offsets
    dc.b    0, 0, -1, -1, -2, -4, -7, -10
    dc.b    10, 7, 4, 2, 1, 1, 0, 0
    ; negative offsets
    
    
;;;;;;;;;;;;;;;;;;;;

    IFNCONST PRINTED_SPACE_LEFT

PRINTED_SPACE_LEFT  equ 1
        echo "Bytes left in 4K cartidge: ", (endOfFreeSpace - lastByte)
    ENDIF
;;;;;;;;;;;;;;;;;;;;

    org $fffa
    .word reset          ; NMI
    .word reset          ; RESET
    .word reset          ; IRQ
    END
    	