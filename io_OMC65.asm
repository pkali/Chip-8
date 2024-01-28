;SAVE#D1:>macros>iomac.m65
;
;
;LOAD#D9:xxxxxx.xxx
;
;	.LOCAL 
;
;This file contains ONLY
;I/O macros.
;
;No math or graphics, etc.
;
;
;Last update 03.12.91	
;            12.11.94	
;

VCLOSE	JMP MCLOSE
VOPEN	JMP MOPEN
VGET	JMP MGET
VPUT	JMP MPUT
ENDVECS
	;.OPT NO LIST
;   .OPT LIST
;BUFF .= *
;   *=  *+1024
	;.OPT NO LIST
MCLOSE
	LDA #12
	STA ICCOM,X
	JMP CIOV
MOPEN
	STA ICAX1,X
	TYA 
	STA ICAX2,X
	LDA #3
	STA ICCOM,X
	JMP CIOV
MPUT
	TAY 
	LDA #0
	STA ICBLL,X
	STA ICBLH,X
	LDA #11
	STA ICCOM,X
	TYA 
	JMP CIOV
MGET
	LDA #0
	STA ICBLL,X
	STA ICBLH,X
	LDA #7
	STA ICCOM,X
	JMP CIOV
;
;
;Now come the actual macros.	
;
	.MACRO CLOSE 
	LDX #[%%1*16]
	JSR VCLOSE
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO OPEN 
	LDX #[%%1*16]
	.IF %%4<256 .AND %%4>0
	JMP @O
@O1	
	.BYTE %%$4,$9B
@O
	LDA # <@O1
	STA ICBAL,X
	LDA # >@O1
	STA ICBAH,X
	.ELSE 
	LDA # <%%4
	STA ICBAL,X
	LDA # >%%4
	STA ICBAH,X
	.ENDIF 
	LDA #%%2
	LDY #%%3
	JSR VOPEN
;   BPL *+5
;   JSR ERROR
;@OE1
;   TYA
	.ENDM 

;open but with address and not name

	.MACRO OPENA
	LDX #[%%1*16]
	LDA # <%%4
	STA ICBAL,X
	LDA # >%%4
	STA ICBAH,X
	LDA #%%2
	LDY #%%3
	JSR VOPEN
;   BPL *+5
;   JSR ERROR
;@OE1
;   TYA
	.ENDM 






;
;
	.MACRO PUT 
;PUT 0 ;PUTS A OUT CHANNEL	
	LDX #[%%1*16]
	JSR VPUT
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO GET 
;RETURNS ONE BYTE IN A	
	LDX #[%%1*16]
	JSR VGET
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
;
;
;
	.MACRO BGET 
	LDX #[%%1*16]
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #7
	STA ICCOM,X
	JSR CIOV
;   TYA
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO BPUT 
	LDX #[%%1*16]
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #11
	STA ICCOM,X
	JSR CIOV
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO INPUT 
;INPUT CH,BUFF,LEN	
	LDX #[%%1*16]
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #5
	STA ICCOM,X
	JSR CIOV
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO PRINT 
;PRINT 0,"HELLO", OR	
;PRINT 0,BUFFER, OR	
;PRINT 0,BUFFER,LEN	
	.IF %%0>1
	LDX #[%%1*16]
	.ELSE 
	LDX #$00
	.ENDIF 
	.IF [%%2<256] .AND [%%2>0]
	JMP @2PR
@1
	.BYTE %%2,$9B
@2PR
	.IF %%0=3
	  BPUT %%1,@1,%%3
	.ELSE 
	  BPUT %%1,@1,[@2PR-@1]
	.ENDIF 
	.ELSE 
	.IF %%0=3
	  BPUT %%1,%%2,%%3
	.ELSE 
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA #128
	STA ICBLL,X
	LDA #0
	STA ICBLH,X
	LDA #9
	STA ICCOM,X
	JSR CIOV
	.ENDIF 
	.ENDIF 
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO XIO 
;XIO 38,2,0,0,"R:"	
	LDX #%%2*16
	.IF [%%1>0] .AND [%%1<256]
	LDA #%%1
	.ELSE 
	LDA %%1
	.ENDIF 
	STA ICCOM,X
	.IF [%%3>0] .AND [%%3<256]
	LDA #%%3
	.ELSE 
	LDA %%3
	.ENDIF 
	STA ICAX1,X
	.IF [%%4>0] .AND [%%4<256]
	LDA #%%4
	.ELSE 
	LDA %%4
	.ENDIF 
	STA ICAX2,X
	.IF [%%5<256] .AND [%%5>0]
	JMP @2BP
@1	.= *
	.BYTE %%$5,$9B
@2BP	.= *
	LDA # <@1
	STA ICBAL,X
	LDA # >@1
	STA ICBAH,X
	.ELSE 
	LDA # <%%5
	STA ICBAL,X
	LDA # >%%5
	STA ICBAH,X
	.ENDIF 
	JSR CIOV
	TYA 
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
	.MACRO DSTATUS 
;DSTATUS 1	
	LDX #%%1*16
	LDA #$0D
	STA ICCOM,X
	JSR CIOV
	.ENDM 
;
;
;
	.MACRO RND 
;RND 0-255	
L1	
	LDA $D20A
	.IF %%1<256
	CMP #%%1
	.ELSE 
	CMP %%1
	.ENDIF 
	BCS L1
	.ENDM 
;
	.MACRO BPUTV 
;USE WHEN LEN IS A .WORD VALUE	
	LDX #[%%1*16]
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA %%3
	STA ICBLL,X
	LDA %%3+1
	STA ICBLH,X
	LDA #11
	STA ICCOM,X
	JSR CIOV
	.ENDM 
;
	.MACRO PAK 
	  PRINT 0,"Press a key.",12
L1	
	LDA 764
	CMP #$FF
	BEQ L1
	LDA #$FF
	STA 764
	.ENDM 
;
	.MACRO BGET_V 
;FOR USE WHEN BUFADR=WORD	
	LDX #[%%1*16]
	LDA %%2
	STA ICBAL,X
	LDA %%2+1
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #7
	STA ICCOM,X
	JSR CIOV
	TYA 
	.ENDM 
;
	.MACRO BGETV 
;FOR USE WHEN BUFLEN=WORD	
	LDX #[%%1*16]
	LDA # <%%2
	STA ICBAL,X
	LDA # >%%2
	STA ICBAH,X
	LDA %%3
	STA ICBLL,X
	LDA %%3+1
	STA ICBLH,X
	LDA #7
	STA ICCOM,X
	JSR CIOV
	TYA 
	.ENDM 
;
	.MACRO BPUT_V 
;USE WHEN BUF IS A .WORD VALUE	
	LDX #[%%1*16]
	LDA %%2
	STA ICBAL,X
	LDA %%2+1
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #11
	STA ICCOM,X
	JSR CIOV
	.ENDM 
;
	.MACRO INPUT_V 
;INPUT CH,POINTER,LEN	
	LDX #[%%1*16]
	LDA %%2
	STA ICBAL,X
	LDA %%2+1
	STA ICBAH,X
	LDA # <%%3
	STA ICBLL,X
	LDA # >%%3
	STA ICBLH,X
	LDA #5
	STA ICCOM,X
	JSR CIOV
;   BPL *+5
;   JSR ERROR
	.ENDM 
;
