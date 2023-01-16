   .IF *>0

getDir
   close 1
   OPEN 1,6,0,"D:*.CH8"

   mwa #nameBuffer,temp_out
   mva #0,fileCounter

   ;get line
getLine
    ;in Happy DOS there are 2 chars in front of the file name
    get 1
    bmi lineError ; I do not expect it here, but just in case...
    get 1
    bmi lineError ; I do not expect it here, but just in case...
    cmp #32  ;for normal file name here comes space, for the last one it is not space
             ;"0517 Free Sectors" begin - the last line of DIR
    bne dirEnd
    mva #8,temp_in
getFileNameLoop
    get 1
    bmi lineError ; I do not expect it here, but just in case...
    ldy #0
    sta (temp_out),y
    inw temp_out
    dec temp_in
    bne getFileNameLoop
    ;get the remaining bytes here
    
getRemains
    get 1
    bmi lineError ; I do not expect it here, but just in case...
    cmp #155 ;[endline]
    bne getRemains
lineError
    ;here I have one name in my name buffer.
    inc:lda fileCounter
    cmp #64 ; too many files... sorry, no bonus
    bne getLine
dirEnd
    close 1
    rts

;-----------------------------------------------
fileSelector
;-----------------------------------------------
   ;---display downloaded directory of .CH8 files
   mwa #nameBuffer,temp_in
   
   ldx #0

displayFileNamesLoop
   lda fileOffsetL,x
   sta temp_out
   lda fileOffsetH,x
   sta temp_out+1
   
   ldy #0
displayFileLoop
   lda (temp_in),y
   ;.sbyte "A" == $21
   ;.byte  "A" == $41
   sec 
   sbc #$20 ;.byte to .sbyte conversion
   iny      ;shift one char to right
   sta (temp_out),y
   cpy #8
   bne displayFileLoop
   
   adw temp_in #8
   
   inx
   cpx fileCounter
   bne displayFileNamesLoop

   ;---------------------------------
   ;select the file

   close 1         ;[lame]
   open 1,4,0,"K:" ;what the heck??? after 16 years of coding on Atari
                   ;am I going to use system to read a key for the very first time???
                   ;Am I getting old???
readKeyLoop
    jsr inverseCursor               

    get 1
    pha
    jsr inverseCursor
    pla
    cmp #45
    beq keyUp
    cmp #28
    beq keyUp
    cmp #61
    beq keyDown
    cmp #29
    beq keyDown
    cmp #43
    beq keyLeft
    cmp #30
    beq keyLeft                   
    cmp #42
    beq keyRight
    cmp #31
    beq keyRight                  
    cmp #155
    beq keyReturn
    ;cmp #27
    ;beq keyEsc
    jmp readKeyLoop

keyUp    
   lda selectedFile
   cmp #0
   beq doNotUp
   dec selectedFile
doNotUp
   jmp readKeyLoop

keyDown  
   ldx selectedFile
   inx
   cpx fileCounter
   bne doNotDown
   dex selectedFile
doNotDown
   stx selectedFile
   jmp readKeyLoop
keyLeft
   lda selectedFile
   sec
   sbc #22
   bmi doNotLeft
   sta selectedFile
doNotLeft
   jmp readKeyLoop   
      
keyRight    
   lda selectedFile
   clc
   adc #22 ;number of lines on the fileselector screen
   cmp fileCounter
   bpl doNotRight
   sta selectedFile
doNotRight
   jmp readKeyLoop
keyEsc   
   
keyReturn
   close 1
   
   ;----copy selected name
   mwa #selectedNameBuffer,temp_out
   jsr selFileAddr
  
   ldy #7
nameCopyLoop   
   lda (temp_in),y
   cmp #$20 ;space to be replaced with question mark
   bne notSpace
   lda #'? ;'
notSpace   
   sta (temp_out),y
   dey
   bpl nameCopyLoop
   ;now in selectedName I have full name like D:MAZE    .CH8
   ;hmm... I am curious if it would work...
   ;it looks like it doesn't, so I replace spaces with question marks
   ;like D:MAZE????.CH8
   ;it MIGHT make some problems if similar files are present in the 
   ;directory (and are in non-alphabetical order)
   ;but it is unlikely anyway... way too few games are available for Chip8
   
   rts
;--------
inverseCursor
    ;----display inversed cursor----
    ldx selectedFile
    lda fileOffsetL,x
    sta temp_out
    lda fileOffsetH,x
    sta temp_out+1
    
    ldy #9
inverseLoop
    lda (temp_out),y
    EOR #$80
    sta (temp_out),y
    dey
    bpl inverseLoop
    rts
;----------------------------------------
selFileAddr
   ;points temp_in to begin of the selected filename
   lda selectedFile
   sta temp_in
   lda #0
   sta temp_in+1
   ;mul by 8 (length of file names)
   asl temp_in
   rol temp_in+1
   asl temp_in
   rol temp_in+1
   asl temp_in
   rol temp_in+1
   ;add nameBuffer address
   clc
   lda temp_in
   adc #<nameBuffer
   sta temp_in
   lda temp_in+1
   adc #>nameBuffer
   sta temp_in+1
   rts
;----------------------------------------
loadGame
    ;game name + D: is in selectedName
    ;-----
    ;clear Chip8Code memory
    mwa #Chip8Code,temp_out
    
    ldy #0
memClearLoop
    lda #0
    sta (temp_out),y
    inw temp_out
    cpw temp_out #Chip8Code+$1000
    bne memClearLoop
    

    mwa #Chip8Code,temp_out
    
    close 1 ;[lame]
    opena 1,4,0,selectedName
readFileLoop
    get 1
    bmi endOfFile
    ldy #0
    sta (temp_out),y
    inw temp_out
    jmp readFileLoop
endOfFile
    close 1
    
    rts
;---------------------------------
getConfig
;reads configuration file CHIP8.CFG
;from current directory
;and tries to assign joystick to Chip8 keyboard
;selectedName should be already done, 
;so run it after FileSelector and before emulation starts

   close 1 ;[lame] because I should know if #1 is closed or not
           ; but it is a no-brainer
   open 1,4,0,"D:CHIP8.CFG"
   
getConfigLoop
    jsr selFileAddr
    mva #0,temp_out   ;char counter 

getConfigLoopInner
    get 1
    jmi getConfigError
    cmp #'/' ;  / = comment
    beq finishLine
    ldy #0
    cmp (temp_in),y
    bne finishLine
    ;1st char found!
    inw temp_in
    inc:lda temp_out
    cmp #8
    bne getConfigLoopInner
    ; name found!!!!
    get 1 ; one space after file name
    get 1
    jsr asciiFind 
    stx joystickConversion
    get 1 ; one space 
    get 1
    jsr asciiFind 
    stx joystickConversion+1
    get 1 ; one space 
    get 1
    jsr asciiFind 
    stx joystickConversion+2
    get 1 ; one space 
    get 1
    jsr asciiFind 
    stx joystickConversion+3
    get 1 ; one space 
    get 1
    jsr asciiFind 
    stx joystickConversion+4
    get 1 ; one space 
    get 1
    jsr asciiFind
    lda toUpperNibble,X  ;A=X*16
    sta delay
    close 1
    ;close 2
    rts

finishLine
    get 1
    bmi getConfigError
    cmp #155 ;[endline]
    bne finishLine
    jmp getConfigLoop
    
getConfigError
    close 1
    ;close 2
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
    .byte "0123456789ABCDEF"
;----------------
dl_fileSelector
    .byte $70,$70
    .byte $42
    .word fileSelectorHeader
    .byte 0,2,0,2
    .byte $40
    .byte $42
    .word fileSelectorScreen
    :21 .byte 2
    .byte $41
    .word dl_fileSelector

fileSelectorHeader
   ;.sbyte "                                " ;32 bytes
    .sbyte " Chip 8 & SuperChip 8 emulator  "
    .sbyte "by pirx@5oft.com 2006-01-17 v1.1"
    .sbyte "   select game with cursors     "
fileSelectorScreen
    :22 .sbyte "                                " ;32 bytes
fileSelectorScreenEnd

selectedFile
    .byte 0    
fileCounter
    .byte 0    
selectedName
    .byte "D:"

selectedNameBuffer
    .byte "        "
    .byte ".CH8"
    .byte $20+$80  ;end of name 
nameBuffer
    ;max 64 names on one directory... 
    ;quite a cheat... will not work good with Sparta....
    ;I am so sorry...
   ;.byte "12345678"
    :64 .byte "        "
        
fileOffsetL
    ;[lame]
    .rept 22, #
      .byte <[fileSelectorScreen+32*#]
    .endr
    ;----22--- 
    .rept 22, #
      .byte <[fileSelectorScreen+32*#+11]
    .endr
    ;----44--- 
    .rept 22, #
      .byte <[fileSelectorScreen+32*#+11+11]
    .endr
    ;----66--- 
fileOffsetH    
    .rept 22, #
      .byte >[fileSelectorScreen+32*#]
    .endr
    ;----22--- 
    .rept 22, #
      .byte >[fileSelectorScreen+32*#+11]
    .endr
    ;----44--- 
    .rept 22, #
      .byte >[fileSelectorScreen+32*#+11+11]
    .endr
    
    
  .ENDIF
    