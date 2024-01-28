;**************************************
;* Chip 8 & Super Chip 8 interpreter  *
;*        for Atari 8-bit             *
;*        version:                    *
.macro ver
    .byte '2.0'
.endm     
;* author: Pawel "pirx" Kalinowski    *
;* 2005-12-27 --> 2024-01-27          *
;**************************************
;2024-01-27
; Code translated to mads,
; command line interface for SpartaDos, BW-DOS, etc
; usage: CHIP8 path_to_game path_to_config
; [ESC] - quit to DOS
; [Return] - game restart
;2006-01-17
; It should work on the real thing (DLI was on during IO)
; Joystick should not jam when used with keyboard
;2006-01-14
; Added sound emulation
; Configured all games for use with joystick
; Adjusted emulation speed for (almost) all games
; Removed faulty game X_MIRROR (this file was corrupt)
;2006-01-13
; Added configuration file
; Added joystick mapping to Chip8 keys
;2006-01-11:
; Fixed directory listing under SpartaDos (X)
; Fixed inversed shooting - it shoots correctly when key is pressed.
; Reduced flickering a bit in Chip8 mode

    icl 'lib/ATARISYS.ASM'
    icl 'lib/MACRO.ASM'
;----------------------
    .macro inc_PC
        clc
        lda PC+1
        adc #2
        sta PC+1
        scc:inc PC
    .endm
;----------------------
;==================================
;virtual machine registers
    .zpvar V0            .byte = $80
    .zpvar V1            .byte
    .zpvar V2            .byte
    .zpvar V3            .byte
    .zpvar V4            .byte
    .zpvar V5            .byte
    .zpvar V6            .byte
    .zpvar V7            .byte
    .zpvar V8            .byte
    .zpvar V9            .byte
    .zpvar VA            .byte
    .zpvar VB            .byte
    .zpvar VC            .byte
    .zpvar VD            .byte
    .zpvar VE            .byte
    .zpvar VF            .byte ;carry and collision register
    .zpvar I             .word ;Chip 8 type - big endian
    .zpvar PC            .word ;Chip 8 type - big endian
    .zpvar soundTimer    .byte
    .zpvar delayTimer    .byte
    .zpvar superChipMode .byte
;==================================
;emulator registers - 6502 type - little endian
    .zpvar fetchAddress  .word
    .zpvar stackPointer  .word
    .zpvar currentInstruction .word
    .zpvar jumpPad       .word
    .zpvar screenWidth   .byte  ;Chip 8 = 8 bytes, Super Chip 8 = 16 bytes
;==================================
;other zero page variables
    .zpvar temp_in       .word ;for use only in most nested subroutines
    .zpvar temp_out      .word  ;for use only in most nested subroutines
    .zpvar collisionByte .byte
    
;===========================*=======
    org  $2000
dl_SuperChip8  
    .byte $70,$70,$70
    .byte $4b
    .word screen
    :63 dta $0b  ; 64 lines

    .byte $70,$70
    .byte $42
    .word helpScreen
    :4 .byte 0,2
    
    .byte $41
    .word dl_SuperChip8
;==================================
dl_Chip8  
    .byte $70,$70,$70+$80
    .byte $49
    .word screen
    :31 .byte $09  ; 32 lines
    
    .byte $70,$70
    .byte $42
    .word helpScreen
    :4 .byte 0,2

    .byte $41
    .word dl_Chip8
;==================================
textScreen
;    dta d"                                " ;32 bytes
helpScreen
    dta d"   Chip8 keys       Atari keys  " ;32 bytes
    dta d"  [1][2][3][C]     [1][2][3][4] " ;32 bytes
    dta d"  [4][5][6][D] --> [Q][W][E][R] " ;32 bytes
    dta d"  [7][8][9][E]     [A][S][D][F] " ;32 bytes
    dta d"  [A][0][B][F]     [Z][X][C][V] " ;32 bytes

crunch jmp $FFFF ; will be changed by INIT routine

err_msg_open  .byte 'OPEN Error', EOL
err_msg_bget  .byte 'BGET Error', EOL
err_msg_close .byte 'CLOSE Error', EOL
err_no_cmd    .byte 'Wrong DOS, no command line.', EOL
welcome_msg   .byte 'Chip8 interpreter v'
              ver
              .byte ' by pirx 2024', EOL
usage_msg     .byte 'Use: CHIP8 pathToGame [pathToConfig]', EOL
err_code      .byte 0
fname_len     .byte 0
bare_fname    :(8+1+3) .byte 0
config_fname  .byte 'D:CHIP8.CFG', EOL
save_COLOR2   .byte 0
save_DLPTRS   .word 0
save_VDSLST   .word 0
save_DMACTLS  .byte 0

; I/O equtes
zcrname = 3
comfnam = 33
write = $09
max_buf_len = 63


start
    ;save current graphics settings
    mva COLOR2 save_COLOR2
    mwa DLPTRS save_DLPTRS
    mwa VDSLST save_VDSLST
    mva DMACTLS save_DMACTLS

    jsr welcome

    lda BOOTQ       ; is DOS loaded at all?
    lsr
    jcc err_no_command_line

    lda DOSVEC+1    ; if so, does it point to RAM and not ROM?
    cmp #$c0
    jcs err_no_command_line

    ldy #$03
    lda (DOSVEC),y
    cmp #$4c
    jne err_no_command_line
;prepare command line read
    lda DOSVEC
    clc
    adc #$03
    sta crunch+1
    lda DOSVEC+1
    adc #$00
    sta crunch+2
command_line
    jsr crunch       ; get next command line entry.
    jeq usage        ; quit if there are no more.
; Set up for CIO print of data at COMFNAM
    ldx #0*$10       ; IOCB #0 (E:)
    lda #write       ; 'print string' command
    sta ICCOM,x
    lda #max_buf_len ; set buffer length for max
    sta ICBLL,x
    lda #0
    sta ICBLH,x
    lda DOSVEC       ; store DOSVEC+33 at icba
    clc
    adc #comfnam
    sta ICBAL,x
    lda DOSVEC+1
    adc #0
    sta ICBAH,x
    jsr CIOV         ; print it.

open_file
    jsr close_1      ; it seems that #1 is sometimes open on startup :O
    ldx #$10         ; IOCB #1
    lda DOSVEC       ; store DOSVEC+33 at icba
    clc
    adc #comfnam
    sta ICBAL,x
    lda DOSVEC+1
    adc #0
    sta ICBAH,x
    jsr open_1

buffer = chip8Code
buflen = $1000
read_binary
    ldx #$10       ; IOCB #1
    lda #GETCHR      ; GET BYTES / BINARY READ
    sta ICCOM,x
    lda #<buffer     ; memory address where data is supposed to go
    sta ICBAL,x
    lda #>buffer
    sta ICBAL+1,x
    lda #<buflen     ; read data block size in bytes
    sta ICBLL,x
    lda #>buflen
    sta ICBLL+1,x
    jsr CIOV
    cpy #EOFERR      ; 136 End of file, it is OK.
    seq:jmi err_bget
    
    jsr get_bare_file_name
    jsr crunch       ; get next command line entry.
    jeq open_default_config
    ldx #0           ; IOCB #0 (E:)
    lda #write       ; 'print string' command
    sta ICCOM,x
    lda #max_buf_len ; set buffer length for max
    sta ICBLL,x
    lda #0
    sta ICBLH,x
    lda DOSVEC       ; store DOSVEC+33 at icba
    clc
    adc #comfnam
    sta ICBAL,x
    lda DOSVEC+1
    adc #0
    sta ICBAH,x
    jsr CIOV         ; print it.

    jsr close_1
    ;open config
    ldx #$10
    lda DOSVEC       ; store DOSVEC+33 at icba
    clc
    adc #comfnam
    sta ICBAL,x
    lda DOSVEC+1
    adc #0
    sta ICBAH,x
    jmp @+
open_default_config
    jsr close_1
    ldx #$10
    lda #<config_fname
    sta ICBAL,x
    lda #>config_fname
    sta ICBAH,x
@
    jsr open_1
    jsr get_config
    
    
    
    jmp emulate

;---------------------------------
close_1
; IOCB #1 only
    ldx #$10         ; IOCB #1
    lda #_CLOSE      ; CLOSE
    sta ICCOM,x
    jsr CIOV
    jmi err_close
    rts
;---------------------------------
open_1
; IOCB #1 only
; file to open must be already in ICBAL and ICBAH
    ldx #$10         ; IOCB #1
    lda #_OPEN       ; command: OPEN
    sta ICCOM,x
    ;lda #<fname     ; filename address
    ;sta ICBAL,x
    ;lda #>fname
    ;sta ICBAL+1,x
    lda #OPNIN       ; $04 read, $08 save, $09 append, $0c R/W
    sta ICAX1,x
    lda #$00         ; additional parameter, $00 is always OK
    sta ICAX2,x 
    jsr CIOV
    jmi err_open
    rts

get_1
; IOCB #1 only
; get byte from channel #1
    ldx #$10
    LDA #0
    STA ICBLL,X
    STA ICBLH,X
    LDA #7
    STA ICCOM,X
    JMP CIOV

skip_spaces_1
; IOCB #1 only
; returns first non-space byte from open file in channel #1
    jsr get_1
    cmp #' '
    beq skip_spaces_1
    rts

.proc get_bare_file_name
; gets file name from command line buffer
    ; find end of the name
    adw DOSVEC #comfnam temp_in
    ldy #0
@
      lda (temp_in),y
      cmp #EOL
      beq eol_found
      iny
      cpy #max_buf_len
    bne @-
eol_found  ; with caviat that if it is longer than max_buf_len it will screw it up
    ;in Y we have the EOL after file name
    dey
    ldx #0
@
      lda (temp_in),y
      cmp #':'
      beq name_start_found
      cmp #'>'
      beq name_start_found
      cmp #'/'
      beq name_start_found
      cmp #'\'
      beq name_start_found
      inx
      dey
    bpl @-
name_start_found
    iny  ; move forward 1 char
    tya
    clc
    adc temp_in
    sta temp_in
    scc:inc temp_in+1
    ; in X - length of the filename
    stx fname_len
    ; in temp_in - beginning of the filename
    ldy #0
@
      lda (temp_in),y
      sta bare_fname,y
      iny
      cpy fname_len
    bne @-
    rts

.endp    
;---------------------------------
;(error) messages
welcome
    ldx #0 ;IOCB #0 (E:)
    lda #<welcome_msg
    sta ICBAL,x
    lda #>welcome_msg
    sta ICBAH,x
    jmp err_prnt
usage
    ldx #0 ;IOCB #0 (E:)
    lda #<usage_msg
    sta ICBAL,x
    lda #>usage_msg
    sta ICBAH,x
    jmp err_prnt
err_no_command_line
    ldx #0 ;IOCB #0 (E:)
    lda #<err_no_cmd
    sta ICBAL,x
    lda #>err_no_cmd
    sta ICBAH,x
    jmp err_prnt
err_open
    ldx #0 ;IOCB #0 (E:)
    lda #<err_msg_open
    sta ICBAL,x
    lda #>err_msg_open
    sta ICBAH,x
    jmp err_prnt
err_bget
    ldx #0 ;IOCB #0 (E:)
    lda #<err_msg_bget
    sta ICBAL,x
    lda #>err_msg_bget
    sta ICBAH,x
    jmp err_prnt
err_close
    ldx #0 ;IOCB #0 (E:)
    lda #<err_msg_close
    sta ICBAL,x
    lda #>err_msg_close
    sta ICBAH,x
    ;jmp err_prnt

err_prnt
    sty err_code
    lda #max_buf_len ; set buffer length for max
    sta ICBLL,x
    lda #0
    sta ICBLH,x
    lda #write ; 'print string' command
    sta ICCOM,x
    jsr CIOV ; print it.
    rts
    
;---------------------------------
get_config
;reads configuration file CHIP8.CFG
;from current directory
;and tries to assign joystick to Chip8 keyboard
;game name address in temp_in
;length of game name in fname_len

   ; open 1,4,0,"D:CHIP8.CFG"
get_configLoop
    mwa #bare_fname temp_in
    mva #0 temp_out   ;char counter 

get_configLoopInner
    jsr get_1
    cmp #'/'         ;  / = comment
    beq finishLine
    ldy #0
    cmp (temp_in),y
    bne finishLine
    ;1st char found!
    inw temp_in
    inc:lda temp_out
    cmp fname_len
    bne get_configLoopInner
    ; name found!!!!
    jsr skip_spaces_1
    jsr asciiFind 
    stx joystickConversion
    jsr skip_spaces_1
    jsr asciiFind 
    stx joystickConversion+1
    jsr skip_spaces_1
    jsr asciiFind 
    stx joystickConversion+2
    jsr skip_spaces_1
    jsr asciiFind 
    stx joystickConversion+3
    jsr skip_spaces_1
    jsr asciiFind 
    stx joystickConversion+4
    jsr skip_spaces_1
    jsr asciiFind
    lda toUpperNibble,X  ;A=X*16
    sta delay
    jsr close_1
    rts

finishLine
    jsr get_1
    bmi get_config_error
    cmp #EOL
    bne finishLine
    jmp get_configLoop
    
get_config_error
    jsr close_1
    rts
;----------------
asciiFind    
    ldx #15
asciiFindLoop
    cmp asciiNumber,X
    beq asciiFound
    dex
    bpl asciiFindLoop
    ;in X I have binary number to be put into 
asciiFound
    rts
asciiNumber
    .byte '0123456789ABCDEF'
;----------------
    
;==================================
exit_to_dos
;==================================
    VMAIN XITVBV,7
    mwa save_DLPTRS DLPTRS
    mva save_DMACTLS DMACTLS
    mwa save_VDSLST VDSLST
    mva save_COLOR2 COLOR2
    ;noSound
    mvx #0 AUDF1
    stx AUDC1
    dex
    sta CH1  ; clear keyboard
    rts
    
;==================================
emulate
;==================================
    mva #0 COLOR2 ;nice black background
    ;hold till keys released
    ;I do not think it is necessary
holdLoop
    LDA SKSTAT
    AND #$04
    BEQ holdLoop
    jsr emulationInit
    vmain vint,7
    vdli dli
    inc_PC  ;to help with reinitialisation
            ;PC starts not at $200, but at $200-2 !!!!!!!!!
            ;so it needs to be increased before emulation starts
emulationLoop
    ;translate Chip8 PC to 6502 address
    clc
    lda PC+1
    adc #<(Chip8Code-$200)
    sta fetchAddress
    lda PC
    adc #>(Chip8Code-$200)
    sta fetchAddress+1
    ;
    ;get the current instruction for execution
    ldy #1
    lda (fetchAddress),y
    sta currentInstruction+1
    dey
    lda (fetchAddress),y
    sta currentInstruction
    ;jsr debugPrint
    ;key
    jsr executeInstruction
    inc_PC
    jsr delayChip8  ;atari is too fast for Chip8
    lda escapeFlag
    seq:jmp exit_to_dos
    lda restartFlag
    seq:jmp emulate
    jmp emulationLoop
executeInstruction
    ;idea is that each type of instruction (NXXX)
    ;has got its own checking and executing block
    ;to avoid many CMP BEQ sequences number
    ;of the instruction is multiplied by 2
    ;(as it is in the upper nibble it is divided by 8
    ; to get the same result)
    ;jumpTable is 2 bytes long for each instruction block
    ;this is not the cleanest solution possible
    lda currentInstruction
    and #$f0
    :3 lsr
    tax
    lda jumpTable,x
    sta jumpPad
    lda jumpTable+1,x
    sta jumpPad+1
    jmp (jumpPad)
;==================================
;execute blocks for the Chip 8 instructions
;==================================
;------------------
Chip8_0XXX
    lda currentInstruction+1
    cmp #$EE
    beq returnFromSubroutine
    cmp #$E0
    beq clearScreen
    cmp #$FF
    beq setSuperMode
    cmp #$FC
    jeq scrollLeft
    cmp #$FB
    jeq scrollRight
    cmp #$FE
    jeq setChip8Mode
    cmp #$FD
    jeq emulationInit ;originally it is "quit from Emulator"
                       ;but we will just reset the game
                       ;it may not always work though...
    ;---the last one!!!
    and #$F0
    cmp #$C0
    beq scrollDown
    halt
    rts
;-------------------
setSuperMode
    mva #1 superChipMode
    mva #16 screenWidth
    vdl dl_SuperChip8
    rts
;-------------------
setChip8Mode
    mva #0 superChipMode
    mva #8 screenWidth
    vdl dl_Chip8
    rts
;-------------------
returnFromSubroutine
    ;get PC from stack
    ldy #0
    lda (stackPointer),y
    sta PC
    iny
    lda (stackPointer),y
    sta PC+1
    sbw stackPointer #2
    rts
;-------------------
clearScreen
    ;at the moment it is clearing the SuperChip 8 screen
    ;for Chip 8 screen a shorter loop would suffice screen+16*32
    ;it will need a change if speed is the issue
    ldy #0
    mwa #screen temp_out
@
      lda #0
      ;lda RANDOM
      sta (temp_out),y
      inw temp_out
      cpw temp_out #screen+32*64
    bne @-
    rts
;-------------------
scrollDown
    ;SuperChip8 ONLY instruction
    ;I will not do a Chip8 version
    ;if run in Chip8 mode it will possibly fail
    ;screen is 16 x 64 bytes
    lda currentInstruction+1
    and #$0F ;number of lines to be scrolled down is here
    sta currentInstruction+1 ;lame BEWARE
                             ;I will use currentInstruction+1
                             ;as a counter!!!
                             ;so after this instruction it will not have
                             ;normal value
repeatScroll
    mwa #screen+16*62 temp_in
    mwa #screen+16*63 temp_out
    ldx #64-1
scrollScreenLoop
    ldy #15
scrollLineLoop
    lda (temp_in),y
    sta (temp_out),y
    dey
    bpl scrollLineLoop
    sbw temp_in #16
    sbw temp_out #16
    dex
    bne scrollScreenLoop
    ;clear the first line
    mwa #screen temp_out
    lda #0
    ldy #15
clearFirstLineLoop
    sta (temp_out),y
    dey
    bpl clearFirstLineLoop
    dec currentInstruction+1
    bne repeatScroll
    rts
;------------------
scrollLeft
    ;scroll screen 4 pixels left
    ;what a shame it is not 8 pixels...
    ;SuperChip8 ONLY!!!
    mwa #screen temp_out
    mva #64 temp_in ;lines counter
scrollLeftNextLine
    ldy #0
scrollLeftLineLoop
    lda (temp_out),y
    and #$0F
    tax
    lda toUpperNibble,X
    sta (temp_out),y
    iny
    lda (temp_out),y
    :4 lsr
    dey
    ora (temp_out),y
    sta (temp_out),y
    iny
    cpy #16
    bne scrollLeftLineLoop
    ;clear the last nibble (the new one)
    lda (temp_out),y
    and #$F0
    sta (temp_out),y
    adw temp_out #16
    dec temp_in
    bne scrollLeftNextLine
    rts
;----------------
scrollRight
    ;scroll screen 4 pixels left
    ;what a shame it is not 8 pixels...
    ;SuperChip8 ONLY!!!
    mwa #screen temp_out
    mva #64 temp_in ;lines counter
scrollRightNextLine
    ldy #15
scrollRightLineLoop
    lda (temp_out),y
    :4 lsr
    sta (temp_out),y
    dey
    lda (temp_out),y
    and #$0F
    tax
    lda toUpperNibble,X
    iny
    ora (temp_out),y
    sta (temp_out),y
    dey
    bne scrollRightLineLoop
    ;clear the last nibble (the new one)
    lda (temp_out),y
    and #$0F
    sta (temp_out),y
    adw temp_out #16
    dec temp_in
    bne scrollRightNextLine
    rts
;------------------
;------------------
Chip8_1XXX
    ;1xxx       jmp xxx       jump to address xxx
    sec
    lda currentInstruction+1
    sbc #2      ;here I need to substract 2 from PC because
                ;just after RTS below the PC is increased by 2
    sta PC+1
    lda currentInstruction
    and #$0f    ; upper nibble is instruction code
    sbc #0
    sta PC
    rts
;------------------
Chip8_2XXX
    ;2xxx       jsr xxx       jump to subroutine at address xxx
    ;save current PC on stack
    adw stackPointer #2    ;stack pointer up
    lda PC
    ldy #0
    sta (stackPointer),y
    lda PC+1
    iny
    sta (stackPointer),y
    ;jump to the new address
    sec
    lda currentInstruction+1
    sbc #2      ;here I need to substract 2 from PC because
                ;just after RTS below the PC is increased by 2
    sta PC+1
    lda currentInstruction
    and #$0f    ; upper nibble is 2
    sbc #0
    sta PC
    rts
;------------------
Chip8_3XXX
    ;skeq vr,xx       skip if register r = constant
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    cmp currentInstruction+1
    bne @+
      inc_PC
@
    rts
;------------------
Chip8_4XXX
    ;4rxx       skne vr,xx       skip if register r <> constant
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    cmp currentInstruction+1
    beq @+
      inc_PC
@
    rts
;------------------
Chip8_5XXX
    ;5ry0       skeq vr,vy       skip if register r = register y
    ;load VR
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    tay
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    tya
    cmp V0,X
    bne @+
      inc_PC      ;skippng here
@
    rts
;------------------
Chip8_6XXX
    ;6rxx       mov vr,xx       move constant to register r
    lda currentInstruction
    and #$0F
    tax
    lda currentInstruction+1
    sta V0,X
    rts
;------------------
Chip8_7XXX
    ;7rxx       add vr,vx       add constant to register r
    ;No carry generated
    lda currentInstruction
    and #$0f
    tax
    lda V0,x
    clc
    adc currentInstruction+1
    sta V0,x
    rts
;------------------
Chip8_8XXX
    ; 8ryS - multiple register manipulation instruction
    ; lowest nibble (S) sets the operation
    ; Important - not all sub-instructions are defined
    ; if an undefined is spotted, the interpreter will HALT
    lda currentInstruction+1
    and #$0F
    asl     ;*2 because JumpTable is 2 bytes wide
    tax
    lda C8_8XXX_JumpTable,x
    sta JumpPad
    lda C8_8XXX_JumpTable+1,x
    sta JumpPad+1
    jmp (JumpPad)
;------
C8_8XX0
    ;8ry0       mov vr,vy       move register vy into vr
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    lda currentInstruction
    and #$0F
    tax
    tya
    sta V0,X
    rts
C8_8XX1
    ;8ry1       or rx,ry       or register vy into register vx
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    tya
    ora:sta V0,X
    rts
C8_8XX2
    ;8ry2       and rx,ry       and register vy into register vx
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    tya
    and:sta V0,X
    rts
C8_8XX3
    ;8ry3       xor rx,ry       exclusive or register ry into register rx
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    tya
    EOR:sta V0,X
    rts
C8_8XX4
    ;8ry4       add vr,vy       add register vy to vr,carry in vf
    mva #0 VF
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    sta temp_in
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    clc
    lda V0,X
    adc temp_in
    sta V0,X
    scc:mva #1 VF
    rts
C8_8XX5
    ; 8ry5       sub vr,vy       subtract register vy from vr,borrow in vf
    ; vf set to 1 if borrows
    ; VX = VX - VY
    mva #0 VF
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    sta temp_in
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    sec
    lda V0,X
    sbc temp_in
    sta V0,X
    scc:mva #1 VF
    rts
C8_8XX6
    ;8r06       shr vr       shift register vr right,
    ;bit 0 goes into register vf
    mva #0 VF
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    lsr
    sta V0,X
    scc:mva #1 VF
    rts
C8_8XX7
    ;8ry7       rsb vr,vy
    ;subtract register vr from register vy, result in vr
    ;vf set to 1 if borrows
    ;VX=VY-VX
    mva #0 VF
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    tya          ;VY
    sec
    sbc:sta V0,X     ;-VX ;VX=
    scc:mva #1 VF
    rts
C8_8XX8
C8_8XX9
C8_8XXa
C8_8XXb
C8_8XXc
C8_8XXd
C8_8XXf
    ;there are no such instructions, so HALT...
    HALT
C8_8XYe
    ;8r0e       shl vr       shift register vr left,bit 7 goes into register vf
    ;let's first ignore Y (although in ANT game it is set to 5)
    mva #0 VF
    lda currentInstruction
    and #$0F
    tax
    rol V0,X
    scc:mva #1 VF
    rts
;------
C8_8XXX_JumpTable
    .word C8_8XX0
    .word C8_8XX1
    .word C8_8XX2
    .word C8_8XX3
    .word C8_8XX4
    .word C8_8XX5
    .word C8_8XX6
    .word C8_8XX7
    .word C8_8XX8
    .word C8_8XX9
    .word C8_8XXa
    .word C8_8XXb
    .word C8_8XXc
    .word C8_8XXd
    .word C8_8XYe
    .word C8_8XXf
;------------------
Chip8_9XXX
    ; 9ry0       skne rx,ry       skip if register rx <> register ry
    ;load VY
    lda currentInstruction+1
    and #$F0
    :4 lsr
    tax
    lda V0,X
    tay
    ;load VX
    lda currentInstruction
    and #$0F
    tax
    tya
    cmp V0,X
    beq @+
    inc_PC       ;skip 1 instruction if not equal
@
    rts
;------------------
Chip8_AXXX
    ;axxx       mvi xxx       Load index register with constant xxx
    lda currentInstruction+1
    sta I+1
    lda currentInstruction
    and #$0F
    sta I
    rts
;------------------
Chip8_BXXX
        HALT
    rts
;------------------
Chip8_CXXX
    ;crxx       rand vr,xxx          vr = random number less than or equal to xxx
    ;from doc by DOOM:
    ;VX = Random number AND KK
    ;(Use a random number and use binary AND on it.
    ;Then store it in register X.
    ;Actually, I am unsure what method you should use, as many does this differently.
    ;However, I use binary AND to achieve results and it works fine for me.)
    lda currentInstruction
    and #$0F
    tax
randomizeAgain
    lda RANDOM
    and currentInstruction+1
    ;cmp currentInstruction+1
    ;bcc randomizeAgain
    sta V0,x
    ;mwa currentInstruction valueHex+1
    ;mwa #textScreen+15 temp_out
    ;jsr printHex
    rts
;------------------
Chip8_DXXX
    ;dxys       sprite rx,ry,s       Draw sprite at screen location rx,ry height s
    ;Draws an 8xN (width x height) sprite
    ;at the coordinates given in register X and Y.
    ;Sprites stored in memory at location in index register (I),
    ;maximum 8 bits wide.
    ;Wraps around the screen.
    ;If when drawn, clears a pixel, vf is set to 1 otherwise it is zero.
    ;All drawing is xor drawing (e.g. it toggles the screen pixels)
    ;If height specified is 0, draws a 16x16 sprite.
    mva #0 VF ;clear collision flag
    ;copy I to temp_in and add shift
    clc
    lda I+1
    adc #<(Chip8Code-$200)
    sta temp_in
    lda I
    adc #>(Chip8Code-$200)
    sta temp_in+1
    ;now (temp_in) points to the sprite source address in 6502 memory
    lda currentInstruction+1
    and #$0F
    tax ;x keeps height of the sprite
    jeq SuperChip8Sprite ;if height=0 then it is SuperChip sprite (16x16)
    ;--------------------
    ;regular Chip8 sprite
    ;clear sprite buffer
    ;it is OK to clear only odd bytes as even ones will be
    ;filled with the sprite
    ;LAME!!! it could be done in a loop where exact number of lines is cleared.
    ;If speed is the issue replace it with the proper code
    ;Most docs say sprites might be max 8 lines high, but I doubt it.
    ;I suppose they could be 16 lines high.
    lda #0
      :16 sta spriteBuffer+#*2+1
    ;copy sprite address to the buffer
    mwa #spriteBuffer temp_out
    ldy #0
@
      lda (temp_in),y
      sta (temp_out),y
      inw temp_in
      adw temp_out #2 ;sprite buffer is 2 bytes wide (sprite + space for shift)
      dex
    bne @-
    ;shift sprite in buffer to the right position
    ;if (X % 8 == 0) then shift not
    ;get XPOS
    lda currentInstruction
    and #$0f
    tax
    lda V0,X
    ;now in A is XPOS of the sprite
    sta temp_in  ;temporary store
    and #$07
    ;now in A there is number of shifts
    tax
doRorC8
    jeq doNotRorC8
    .rept 16, #
      lsr spriteBuffer+2*(#+1)-2
      ror spriteBuffer+2*(#+1)-1
    .endr
    dex
    jmp doRorC8
doNotRorC8
    ;copy buffer to the screen
    ;get YPOS
    lda currentInstruction+1
    and #$f0
    :4 lsr
    tax
    lda V0,x
    sta temp_out
    lda #0
    sta temp_out+1
    ; multiply by screenWidth (8 or 16)
    ldy SuperChipMode
    beq multiplyBy8C8
    asl temp_out
    rol temp_out+1
multiplyBy8C8
    .rept 3
      asl temp_out
      rol temp_out+1
    .endr
    ;now in temp_out there is an Y offset
    ;get XPOS
    lda temp_in ;there was XPOS value stored here
    :3 lsr
    clc
    adc temp_out
    sta temp_out
    scc:inc temp_out+1
    clc
    lda #<screen
    adc temp_out
    sta temp_out
    lda #>screen
    adc temp_out+1
    sta temp_out+1
    ;now in temp_out there is address
    ;of the first destination byte on screen for the sprite
    ;move sprite from buffer to screen (2 bytes wide)
    mwa #spriteBuffer temp_in
    lda currentInstruction+1 ;get sprite height
    and #$0F
    tax
SpriteLoopFinal
    ldy #0
    lda (temp_in),y
    ora (temp_out),y
    sta collisionByte ;for checking the collision
                      ;collision was when the screen byte EOR'ed and OR'er
                      ;with the sprite byte give the same result
    lda (temp_in),y
    eor:sta (temp_out),y
    cmp collisionByte
    beq noCollision1
    mva #1 VF     ;there was a collision
noCollision1
    iny
    lda (temp_in),y
    ora (temp_out),y
    sta collisionByte ;for checking the collision
    lda (temp_in),y
    eor:sta (temp_out),y
    cmp collisionByte
    beq noCollision2
    mva #1 VF     ;there was a collision
noCollision2
    lda SuperChipMode
    beq narrowScreenC8
    adw temp_out #8 ; screen is 16 bytes wide
narrowScreenC8
    adw temp_out #8 ; or only 8 bytes wide (LAME!!!)
    adw temp_in #2  ; sprite in buffer is 3 bytes wide
    dex
    bne SpriteLoopFinal
    rts
SuperChip8Sprite
    ;16x16 sprite
    ;clear sprite buffer
    ;it is OK to clear the third byte only as two other bytes
    ;will be copied from Chip8 memory anyway
    lda #0
    :16 sta spriteBuffer+3*(#+1)-1
    ;copy sprite to buffer
    mwa #spriteBuffer temp_out
    ldx #0
superSpriteLoop
    ldy #0
    lda (temp_in),y
    sta (temp_out),y
    iny
    lda (temp_in),y
    sta (temp_out),y
    adw temp_out #3  ; sprite buffer is 3 bytes wide
    adw temp_in #2   ; sprite itself is 2 bytes wide
    inx
    cpx #16          ; sprite is always 16 lines high
    bne superSpriteLoop
    ;shift sprite in buffer the right number of pixels
    ;if (X % 8 == 0) then shift not
    ;get XPOS
    lda currentInstruction
    and #$0f
    tax
    lda V0,X
    ;now in A is XPOS of the sprite
    sta temp_in
    and #$07
    ;now in A there is number of shifts
    tax
doRor
    jeq doNotRor
    .rept 16, #
      lsr spriteBuffer+3*(#+1)-3
      ror spriteBuffer+3*(#+1)-2
      ror spriteBuffer+3*(#+1)-1
    .endr
    dex
    jmp doRor
doNotRor
    ;copy buffer to the screen
    ;get YPOS
    lda currentInstruction+1
    and #$f0
    :4 lsr
    tax
    lda V0,x
    sta temp_out
    lda #0
    sta temp_out+1
    ; multiply by screenWidth (8 or 16)
    ldy SuperChipMode
    beq multiplyBy8
    asl temp_out
    rol temp_out+1
multiplyBy8
    .rept 3
      asl temp_out
      rol temp_out+1
    .endr
    ;now in temp_out there is an Y offset
    ;get XPOS
    lda temp_in ;there was XPOS value stored here
    :3 lsr
    clc
    adc:sta temp_out
    scc:inc temp_out+1
    clc
    lda #<screen
    adc:sta temp_out
    lda #>screen
    adc:sta temp_out+1
    ;now in temp_out there is address
    ;of the first destination byte on screen for the sprite
    ;move sprite from buffer to screen (3 bytes wide)
    mwa #spriteBuffer temp_in
    ldx #0
superSpriteLoopFinal
    ldy #0
    lda (temp_in),y
    ora (temp_out),y
    sta collisionByte
    lda (temp_in),y
    eor:sta (temp_out),y
    cmp collisionByte
    beq noCollision3
    mva #1 VF
noCollision3
    iny
    lda (temp_in),y
    ora (temp_out),y
    sta collisionByte
    lda (temp_in),y
    eor:sta (temp_out),y
    cmp collisionByte
    beq noCollision4
    mva #1 VF
noCollision4
    iny
    lda (temp_in),y
    ora (temp_out),y
    sta collisionByte
    lda (temp_in),y
    eor:sta (temp_out),y
    cmp collisionByte
    beq noCollision5
    mva #1 VF
noCollision5
    lda SuperChipMode ;theoretically it is always SuperChipMode
    beq narrowScreen  ;but some docs say SuperChip instructions can be used in Chip 8 mode, too
    adw temp_out #8 ; screen is 16 bytes wide
narrowScreen
    adw temp_out #8 ; or only 8 bytes wide (LAME!!!)
    adw temp_in #3  ; sprite in buffer is 3 bytes wide
    inx
    cpx #16          ; sprite is always 16 lines high
    bne superSpriteLoopFinal
    rts
;-----------------------------------------------------
;-----------------------------------------------------
Chip8_EXXX
    lda currentInstruction+1
    cmp #$a1
    beq skup.k
    cmp #$9e
    beq skpr.k
    ;apparently an error so halt...
    HALT $37
    ;rts
skup.k
    ;eka1       skup k       skip if key (register rk) not pressed
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    tax
    lda keyboardGrid,X
    bne skup.Pressed
skup.notPressed
    inc_PC ;skip the next instruction
skup.Pressed
    rts
    ;------------
skpr.k
 ;ek9e       skpr k       skip if key (register rk) pressed
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    tax
    lda keyboardGrid,X
    beq skpr.notPressed
skpr.Pressed
    inc_PC ;skip the next instruction
skpr.notPressed
    ;do not skip the next instruction
    rts
;-----------------------------------------------------
;-----------------------------------------------------
Chip8_FXXX
    lda currentInstruction+1
    cmp #$07
    jeq fr07
    cmp #$0a
    jeq fr0a
    cmp #$15
    jeq fr15
    cmp #$18
    jeq fr18
    cmp #$1E
    jeq fr1E
    cmp #$29
    jeq fr29
    cmp #$30
    jeq fr30
    cmp #$33
    jeq fr33
    cmp #$55
    jeq fr55
    cmp #$65
    jeq fr65
    cmp #$75
    jeq fr75
    cmp #$85
    jeq fr85
    HALT
    ;rts
fr1E
    ;fr1e       adi vr       add register vr to the index register
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    clc
    adc:sta I+1
    scc:inc I
skip07
    rts
;------------------
fr65
    ;fx65       ldr v0-vr       load registers v0-vr from location I onwards
    ;I is incremented to point to the next location on. e.g. I = I + r + 1
    lda currentInstruction
    and #$0F
    tay
    tax ;stored for later use (I=I+r+1), X=r
    clc
    lda I+1
    adc #<(Chip8Code-$200)
    sta temp_in
    lda I
    adc #>(Chip8Code-$200)
    sta temp_in+1
@
      lda (temp_in),y
      sta V0,y
      dey
    bpl @-
    ;increment I
    ;WARNING! Some docs say it is not incrementing!!!
    ;remove if something does not work
    ;indeed, I IS NOT INCREMENTED!!!!!
;
;
;    inx  ; I=I+r+1  <-- this is 1
;    txa
;    clc
;    adc I+1
;    sta I+1
;    bcc skip08
;    inc I
;skip08
    rts
fr55
    ;fr55       str v0-vr       store registers v0-vr at location I onwards
    ;I is incremented to point to the next location on. e.g. I = I + r + 1
    ;other docs say I is NOT incremented and I will stick to this option
    ;move I to temp_out
    clc
    lda I+1
    adc #<(Chip8Code-$200)
    sta temp_out
    lda I
    adc #>(Chip8Code-$200)
    sta temp_out+1
    lda currentInstruction
    and #$0F
    tay
@
      lda V0,y
      sta (temp_out),y
      dey
    bpl @-
    rts
fr15
    ;fr15       sdelay vr       set the delay timer to vr
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    sta delayTimer
    rts
fr07
    ;fr07       gdelay vr       get delay timer into vr
    lda currentInstruction
    and #$0F
    tax
    lda delayTimer
    sta V0,X
    rts
fr18
    ;fr18       ssound vr       set the sound timer to vr
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    sta soundTimer
    rts
fr29
    ; fr29       font vr
    ;point I to the sprite for hexadecimal character in vr
    ;Sprite is 5 bytes high
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    ;multiply A by 5
    ;mul by 1
    sta temp_in
    ;mul by 4
    :2 asl
    ;add multipl. by 2
    clc
    adc temp_in
    ; now in A there is char number multiplied by 5
    ;add charset start address
    clc
    adc #<(Chip8Font-Chip8Code+$200)
    sta I+1
    lda #0
    adc #>(Chip8Font-Chip8Code+$200)
    sta I
    rts
fr30
    ; fr30       xfont vr
    ;point I to the sprite for hexadecimal character in vr
    ;Sprite is 10 bytes high,Super only
    lda currentInstruction
    and #$0F
    tax
    lda V0,X
    ;multiply A by 10
    ;mul by 2
    asl
    sta temp_in
    ;mul by 8
    :2 asl
    ;add multipl. by 2
    clc
    adc temp_in
    ; now in A there is char number multiplied by 10
    ;add charset start address
    clc
    adc #<(SChip8Font-Chip8Code+$200)
    sta I+1
    lda #0
    adc #>(SChip8Font-Chip8Code+$200)
    sta I
    rts
fr33
    ;fr33       bcd vr
    ;store the bcd representation of register vr
    ;at location I,I+1,I+2
    ;Doesn't change I
    ;IT IS NOT 6502 style BCD !!!
    mva #0 decimal+1 ; older byte always equals 0
    lda currentInstruction
    and #$0F
    tax
    lda V0,x
    sta decimal
    jsr displayDecimal
    ;copy decimal result to the memory starting at I
    ;move I to temp_out
    clc
    lda I+1
    adc #<(Chip8Code-$200)
    sta temp_out
    lda I
    adc #>(Chip8Code-$200)
    sta temp_out+1
    ldy #2  ;3 digits
@
    lda decimalResult+1,y
    sta (temp_out),y
    dey
    bpl @-
    rts
fr0a
    ;fr0a       key vr       wait for for keypress,put key in register vr
    lda escapeFlag
    ora restartFlag
    bne doNotWaitForKey
    lda #0
    ldy #15
checkAllKeysLoop
    lda keyboardGrid,Y
    bne fr0a.KeyPressed
    dey
    bpl checkAllKeysLoop
    bmi fr0a ;loop until something pressed
fr0a.KeyPressed
    ;in Y there is key pressed
    lda currentInstruction
    and #$0F
    tax
    tya
    sta V0,X
doNotWaitForKey
    rts
fr75
    ;FX75 Save V0...VX (X<8) in the HP48 flags (***)
    mwa #HP48Flags temp_out
    lda currentInstruction
    and #$0F
    tay
@
      lda V0,y
      sta (temp_out),y
      dey
    bpl @-
    rts
fr85
    ;FX85 Load V0...VX (X<8) from the HP48 flags (***)
    mwa #HP48Flags temp_in
    lda currentInstruction
    and #$0F
    tay
@
      lda (temp_out),y
      sta V0,y
      dey
      bpl @-
    rts
;------------------
jumpTable
    .word Chip8_0XXX
    .word Chip8_1XXX
    .word Chip8_2XXX
    .word Chip8_3XXX
    .word Chip8_4XXX
    .word Chip8_5XXX
    .word Chip8_6XXX
    .word Chip8_7XXX
    .word Chip8_8XXX
    .word Chip8_9XXX
    .word Chip8_AXXX
    .word Chip8_BXXX
    .word Chip8_CXXX
    .word Chip8_DXXX
    .word Chip8_EXXX
    .word Chip8_FXXX
;==================================
emulationInit
    jsr clearScreen
    mva #0 superChipMode
    ldx #15+2 ;V0-VF and I
zeroRegisters
    sta V0,X
    sta keyboardGrid,X
    dex
    bpl zeroRegisters
    sta escapeFlag
    sta restartFlag
    mva #8 screenWidth
    ;mwa #$0002 PC ;all Chip 8 programs (I have) start at $0200
    lda #<($200-2)  ;-2 because it might be started from inside emulation loop
    sta PC+1        ; and always after instruction execution
    lda #>($200-2)  ; PC gets incremented by 2
    sta PC
    ;mwa #Chip8Code fetchAddress
    mwa #stackTop stackPointer
    vdl dl_Chip8,$01 ;return to Chip8 screen (just in case)
    POKEY_INIT
    rts
;==================================
;vertical interrupt
;==================================
vint
    lda soundTimer
    beq noSound
    dec soundTimer
    mva #197 AUDF1
    mva #$EF AUDC1
    jmp @+
noSound
    mva #0 AUDF1
    mva #0 AUDC1
@
    lda delayTimer
    seq:dec delayTimer
    ;----------keyboard;----------
    mva #0 escapeFlag
    ldx #15
    ;lda #0
clearGrid
    sta keyboardGrid,x
    dex
    bpl clearGrid
    lda SKSTAT
    cmp #$ff
    beq keyFinished     ;no keyboard key pressed so clear keyboardGrid
somethingPressed
    lda KBCODE
    cmp #@kbcode._esc ;ESCAPE
    sne:mva #1 escapeFlag
    cmp #@kbcode._ret
    sne:mva #1 restartFlag
notEscape
    ldx #15
checkKeyTableLoop
    cmp keyTable,x
    beq keyFound
    dex
    bpl checkKeyTableLoop
;   no Chip8 key pressed
    jmp keyFinished
keyFound
    ;X contains offset in the Chip8KeyTable
    lda Chip8KeyTable,X
    tax
    lda #1
    sta keyboardGrid,X
keyFinished
    ;------------JOY-------------
    ;happy happy joy joy
    ;check for joystick now
    ldy PORTA
    tya
    and #$01 ;up
    bne jNotUp
    ldx joystickConversion ;up
    lda #1
    sta keyboardGrid,x
jNotUp
    tya
    and #$02 ;up
    bne jNotDown
    ldx joystickConversion+1 ;up
    lda #1
    sta keyboardGrid,x
jNotDown
    tya
    and #$04 ;up
    bne jNotLeft
    ldx joystickConversion+2 ;up
    lda #1
    sta keyboardGrid,x
jNotLeft
    tya
    and #$08 ;up
    bne jNotRight
    ldx joystickConversion+3 ;up
    lda #1
    sta keyboardGrid,x
jNotRight
    ;fire
    lda TRIG0
    bne JNotFire
    ldx joystickConversion+4 ;up
    lda #1
    sta keyboardGrid,x
JNotFire
    jmp XITVBV
;==================================
dli ; Display List Interrupt
;==================================
;purpose of this DLI is to slow down machine in Chip8 mode
;and to make flickering less visible
    pha
    phx
    ldx #64 ;screen is 128 scanlines high
dliLoop
    sta WSYNC
    lda #$AA
    sta COLPF0
    sta WSYNC
    lda #$A9
    sta COLPF0
    dex
    bne dliLoop
    plx
    pla
    rti
;==================================
printHex
    ;prints 4 hexadecimal digits from valueHex
    ;to (temp_out)
    ldy #0
    lda valueHex+1
    :4 lsr
    tax
    lda tableHex,x
    sta (temp_out),y
    iny
    lda valueHex+1
    and #$0f
    tax
    lda tableHex,x
    sta (temp_out),y
    iny
    lda valueHex
    :4 lsr
    tax
    lda tableHex,x
    sta (temp_out),y
    iny
    lda valueHex
    and #$0f
    tax
    lda tableHex,x
    sta (temp_out),y
    rts
valueHex
    .word 0
tableHex
    dta d"0123456789abcdef"
;==================================
;--------------------------------------------------
displayDecimal ;decimal (word)
;--------------------------------------------------
; displays decimal number as in parameters (in text mode)
; leading zeores are removed
; the range is (0000..9999 - two bytes)
; result in decimalresult
;---------------
;this version of the routine puts BCD like results
;and not Atari chars.
    ldy #3              ; there will be 4 digits
NextDigit
      ldx #16           ; 16-bit dividee so Rotate 16 times
      lda #$00
Rotate000
      ASLW decimal
      rol               ; scroll dividee
                        ; (as highest byte - additional - byte is A)
      cmp #10           ; divider
      bcc TooLittle000  ; if A is smaller than divider
                        ; there is nothing to substract
      sbc #10           ; divider
      inc decimal       ; lowest bit set to 1
                        ; because it is 0 and this is the fastest way
TooLittle000
      dex
      bne Rotate000     ; and Rotate 16 times, Result will be in decimal
      tax               ; and the rest in A
                        ; (and it goes to X because it is our decimal digit)
      lda digits,x
      sta decimalresult,y
      dey
    bpl NextDigit    ; Result again /10 and we have next digit
rightnumber
    rts
;-------decimal constans
digits      .byte 0,1,2,3,4,5,6,7,8,9
;variables
decimal   .word 0
decimalresult
          .byte 0,0,0,0
;---------------------------------------------------
debugPrint
    ;print fetchAddress
    mwa fetchAddress valueHex
    mwa #textScreen temp_out
    jsr printHex
    ;print currentInstruction
    lda currentInstruction   ;big endian
    sta valueHex+1           ;low endian
    lda currentInstruction+1
    sta valueHex
    mwa #textScreen+5 temp_out
    jsr printHex
    ;print PC
    lda PC
    sta valueHex+1
    lda PC+1
    sta valueHex
    mwa #textScreen+10 temp_out
    jsr printHex
    rts
;------------------------------
delayChip8
    ;run slower when it is Chip 8 mode
    ;(for SUper Chip 8 emulation speed is just right)
    ; but some games run too fast (like BLINKY_S) and this is why
    ; now delay works for both modes
    ;lda superChipMode
    ;bne skipDelay
    lda CONSOL
    cmp #$6
    beq skipDelay
    ldx delay
    beq skipDelay
delayLoop
      nop
      dex
    bne delayLoop
skipDelay
    rts
;------------------------------
toUpperNibble
    .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
;------------------------------
keyTable
;    KBCODEs of
;    the following
;    Atari keys
;    1 2 3 4        1 2 3 C
;    q w e r  ___\  4 5 6 D
;    a s d f     /  7 8 9 E
;    z x c v        A 0 B F
;
; it will be possible to remap keys
; by changing KBCODE values in this table
    .byte $1f,$1e,$1a,$18
    .byte $2f,$2e,$2a,$28
    .byte $3f,$3e,$3a,$38
    .byte $17,$16,$12,$10
Chip8KeyTable
    .byte $01,$02,$03,$0C
    .byte $04,$05,$06,$0D
    .byte $07,$08,$09,$0E
    .byte $0A,$00,$0B,$0F
;==================================
keyboardGrid
    ;keys 0,1,2,3,4,...,F
    ;0 = key not pressed
    ;1 = key pressed
    :16 .byte 0
    .byte 0,0 ;to allow easier initialisation
joystickConversion
    .byte 2,8,4,6,5 ;default
    ;.byte 1,2,3,$C,$A
    ; up down left right trigger
    ; these bytes are set by readConfig
    ; the above are default settings that work for a few games, e.g. BREAKOUT
delay
    ;the higher the value the emulation is slower (in CHIP8 mode)
    .byte $80
stackTop
    ;16 levels of stack
    :16 .word 0
    ;aah, just to be safe
    :8 .word 0
;==================================
spriteBuffer
    ;3x16 bytes - largest sprite size possible (2x16) + shift buffer
    :48 .byte 0
;==================================
HP48Flags
    :8 .byte 0
    :8 .byte 0 ;8 bytes ;one doc says it is only 8, but who knows???
;==================================
escapeFlag ;1=quit to Title
    .byte 0
restartFlag ; 1=restart game
    .byte 0
;==================================
    ; the address space of Chip8 machine
chip8Code
    ; ins 'GAMES/BREAKOUT.CH8'
    ; ins 'GAMES/ALIEN.CH8'
    .ds $1000
    ;four kilobytes of space for Chip8 programs
;==================================
Chip8Font ;Chip 8 font set:
    ;font must be visible in the address space of Chip8 virtual machine
; ---- 0 ----
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %11110000
; ---- 1 ----
  .byte %00100000
  .byte %01100000
  .byte %00100000
  .byte %00100000
  .byte %01110000
; ---- 2 ----
  .byte %11110000
  .byte %00010000
  .byte %11110000
  .byte %10000000
  .byte %11110000
; ---- 3 ----
  .byte %11110000
  .byte %00010000
  .byte %11110000
  .byte %00010000
  .byte %11110000
; ---- 4 ----
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %00010000
  .byte %00010000
; ---- 5 ----
  .byte %11110000
  .byte %10000000
  .byte %11110000
  .byte %00010000
  .byte %11110000
; ---- 6 ----
  .byte %11110000
  .byte %10000000
  .byte %11110000
  .byte %10010000
  .byte %11110000
; ---- 7 ----
  .byte %11110000
  .byte %00010000
  .byte %00100000
  .byte %01000000
  .byte %01000000
; ---- 8 ----
  .byte %11110000
  .byte %10010000
  .byte %11110000
  .byte %10010000
  .byte %11110000
; ---- 9 ----
  .byte %11110000
  .byte %10010000
  .byte %11110000
  .byte %00010000
  .byte %11110000
; ---- A ----
  .byte %11110000
  .byte %10010000
  .byte %11110000
  .byte %10010000
  .byte %10010000
; ---- B ----
  .byte %11100000
  .byte %10010000
  .byte %11100000
  .byte %10010000
  .byte %11100000
; ---- C ----
  .byte %11110000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %11110000
; ---- D ----
  .byte %11100000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %11100000
; ---- E ----
  .byte %11110000
  .byte %10000000
  .byte %11110000
  .byte %10000000
  .byte %11110000
; ---- F ----
  .byte %11110000
  .byte %10000000
  .byte %11110000
  .byte %10000000
  .byte %10000000
SChip8Font ;Super Chip 8 font set:
    ;this font is rather ugly and could be much better !!!
    ;I supose this is not original Super Chip 8 font,
    ;but just regular Chip 8 font with doubled lines
    ;deleted some ones to make it a bit nicer
; ---- 0 ----
  .byte %01100000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %01100000
; ---- 1 ----
  .byte %00100000
  .byte %00100000
  .byte %01100000
  .byte %10100000
  .byte %00100000
  .byte %00100000
  .byte %00100000
  .byte %00100000
  .byte %01110000
  .byte %01110000
; ---- 2 ----
  .byte %11100000
  .byte %11110000
  .byte %00010000
  .byte %00010000
  .byte %01110000
  .byte %11100000
  .byte %10000000
  .byte %10000000
  .byte %11110000
  .byte %11110000
; ---- 3 ----
  .byte %11100000
  .byte %11110000
  .byte %00010000
  .byte %00010000
  .byte %11100000
  .byte %11100000
  .byte %00010000
  .byte %00010000
  .byte %11110000
  .byte %11100000
; ---- 4 ----
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %01110000
  .byte %00010000
  .byte %00010000
  .byte %00010000
  .byte %00010000
; ---- 5 ----
  .byte %11110000
  .byte %11110000
  .byte %10000000
  .byte %10000000
  .byte %11100000
  .byte %11110000
  .byte %00010000
  .byte %00010000
  .byte %11110000
  .byte %11100000
; ---- 6 ----
  .byte %0110000
  .byte %11110000
  .byte %10000000
  .byte %10000000
  .byte %11100000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %01100000
; ---- 7 ----
  .byte %11110000
  .byte %11110000
  .byte %00010000
  .byte %00010000
  .byte %00100000
  .byte %00100000
  .byte %01000000
  .byte %01000000
  .byte %01000000
  .byte %01000000
; ---- 8 ----
  .byte %01100000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %01100000
  .byte %01100000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %01100000
; ---- 9 ----
  .byte %01100000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %01110000
  .byte %00010000
  .byte %00010000
  .byte %11110000
  .byte %11100000
; ---- A ----
  .byte %01100000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %11110000
  .byte %11110000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
; ---- B ----
  .byte %11000000
  .byte %11100000
  .byte %10010000
  .byte %10010000
  .byte %11100000
  .byte %11100000
  .byte %10010000
  .byte %10010000
  .byte %11100000
  .byte %11000000
; ---- C ----
  .byte %01110000
  .byte %11100000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %11100000
  .byte %01110000
; ---- D ----
  .byte %11000000
  .byte %11100000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %10010000
  .byte %11100000
  .byte %11000000
; ---- E ----
  .byte %11110000
  .byte %11110000
  .byte %10000000
  .byte %10000000
  .byte %11110000
  .byte %11100000
  .byte %10000000
  .byte %10000000
  .byte %11110000
  .byte %11110000
; ---- F ----
  .byte %11110000
  .byte %11110000
  .byte %10000000
  .byte %10000000
  .byte %11110000
  .byte %11100000
  .byte %10000000
  .byte %10000000
  .byte %10000000
  .byte %10000000
;==================================
    
   .align $1000
screen
    run start
