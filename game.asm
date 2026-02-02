IDEAL
MODEL small
 
 macro SHOW_MOUSE
	mov ax, 1
	int 33h
endm


macro HIDE_MOUSE
	mov ax, 2
	int 33h
endm

;bx is an input
macro GET_MODOLO;לקבל שארית
	push ax
	push bx
	push dx
	
	mov ax,[maincnt]
	mov dx,1
	div bx
	mov [modulo],dx;in dx there is the modulo(שארית)
	pop dx
	pop bx
	pop ax
endm
STACK 0f500h

SCREEN_WIDTH = 320  

BLOCK_SIZE = 32

TankSpwanX = 200
TankSpwanY = 155
TankHightUD = 16
TankwidthUD = 10
TankHightLR = 12
TankwidthLR = 15

ETankSpwanX= 40
ETankSpwanY=50

ETankHightUD = 16
ETankwidthUD = 10
ETankHightLR = 12
ETankwidthLR = 15

ETankSpwanX2 = 90
ETankSpwanY2 = 50

LivesSpwanX = 40
LivesSpwanY = 180
LivesHight = 14
LivesWidth = 37
LivesSpace = 5
DATASEG

	maincnt dw 1;counter of main loop
	modulo dw ?;שארית
	
	StartStr db "super tank"
	lenStartStr = $ - StartStr
	StrScore db "Score: "
	lenStrScore = $-StrScore
	FileStart db "start.bmp",0
	ClickHere db "press to start!"
	lenClickHere= $ - ClickHere
	pressed db 0
	YourScoreStr db "your score: "
	lenYourScore = $ - YourScoreStr
	BestScorestr db "best Score: "
	lenBestScore = $ - BestScorestr
	
	ScrLine db SCREEN_WIDTH dup (0)  ; One Color line read buffer
	
	IntStr db 7 dup (' '),"$"
	ReadBuff db BLOCK_SIZE dup(?),'$'
	Filescore db "scoregame.txt",0
	
	;BMP File data
	FileMap db "map1.bmp",0
	FileTank db "tank.bmp",0
	FileTankDown db "tankd.bmp",0
	FileTankLeft db "tankl.bmp",0
	FileTankRight db "tankr.bmp",0
	FileBlackTank db "btank.bmp",0
	FileBlackTankLR db "btankLR.bmp",0
	FileETank db "Etank.bmp",0
	FileETankDown db "EtankD.bmp",0
	FileETankLeft db "EtankL.bmp",0
	FileETankRight db "EtankR.bmp",0
	FileLives db "Lives.bmp",0
	FileLivesBlack db "Livesb.bmp",0
	FileGameOver db "over.bmp",0
	FileNextLevel db "nextlev.bmp",0
	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)
	ErrorFile   db 0
	
	FileFound db ?
	FileCreated  db ?
	FatalError db ?
	
	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',0dh, 0ah,'$'
	
    BmpLeft dw ?
	BmpTop dw ?
	BmpWidth dw ?
	BmpHeight dw ?
	
    recthight dw ?
	rectwidth dw ?
	
	Xrect dw ?
	Yrect dw ?
	
	lives db 2
	delLivesCnt db 0
	delLivesArr db 0,14,28
	Xtank dw ?
	Ytank dw ?
	
	ud dw 1;if the movment is up or down
	lr dw 0;if the movment is left or right 
	up dw 1;if the movment is up 
	left dw 0;if the movment is left
	
	Score dw 0
	BestScore dw 0
	kexit db 0;k is the exit key
	
	HightbulletRL=5
	LentbulletRL=8
	bulletRL db 0,0,14h,14h,14h,14h,0,0; yellow color for right or left bullet
         db 0,14h,14h,14h,14h,14h,14h,0
         db 14h,14h,14h,14h,14h,14h,14h,14h
         db 0,14h,14h,14h,14h,14h,14h,0
         db 0,0,14h,14h,14h,14h,0,0
			
		   
	xbulletRL dw ?
	ybulletRL dw ?
	
	HightbulletUD = 8
	lentbulletUD = 5
	bulletUD db 0,0,14h,0,0;bulet up ot down 
			 db 0,14h,14h,14h,0
			 db 14h,14h,14h,14h,14h
			 db 14h,14h,14h,14h,14h
			 db 14h,14h,14h,14h,14h
			 db 14h,14h,14h,14h,14h
			 db 0,14h,14h,14h,0
			 db 0,0,14h,0,0
	xbulletUD dw ?
	ybulletUD dw ?	

	BulletColided db 0
	
	bulletFinish dw 1;1=not fired,0=fierd(נורה)
	bulletdirectoin dw 0;0=null,1=up,2=down,3=right,4=left/directoin of bullet
	
	XETank dw ?
	YETank dw ?
	
	Eud dw 1;if the movment is up or down
	Elr dw 0;if the movment is left or right 
	Eup dw 1;if the movment is up 
	Eleft dw 0;if the movment is left
	
	ETankShadow db 16 * 16 dup(1)
	
	EHightbulletRL=5
	ELentbulletRL=8
	 EbulletRL db 0,0,14,14,14,14,0,0  ; red color for right or left bullet
             db 0,14,14,14,14,14,14,0
             db 14,14,14,14,14,14,14,14
             db 0,14,14,14,14,14,14,0
             db 0,0,14,14,14,14,0,0
			
    ExbulletRL dw ?
    EybulletRL dw ?
	
    EHightbulletUD = 8
    ElentbulletUD = 5
    
    EbulletUD db 0,0,14,0,0  ; bullet up or down
             db 0,14,14,14,0
             db 14,14,14,14,14
             db 14,14,14,14,14
             db 14,14,14,14,14
             db 14,14,14,14,14
             db 0,14,14,14,0
             db 0,0,14,0,0
	ExbulletUD dw ?
	EybulletUD dw ?	

	EBulletColided db 0

	EbulletFinish dw 1;1=not fired,0=fierd(נורה)
	Ebulletdirectoin dw 0;0=null,1=up,2=down,3=right,4=left/directoin of bullet

	XETank2 dw ? ;of secand enemy
	YETank2 dw ? ;of secand enemy
	
	Eud2 dw 1;if the movment is up or down ;of secand enemy
	Elr2 dw 0;if the movment is left or right ;of secand enemy
	Eup2 dw 1;if the movment is up ;of secand enemy
	Eleft2 dw 0;if the movment is left ;of secand enemy
	
	ETankShadow2 db 16 * 16 dup(1)
	
	ExbulletRL2 dw ?
    EybulletRL2 dw ?
	
	ExbulletUD2 dw ?
	EybulletUD2 dw ?	

	EbulletFinish2 dw 1;1=not fired,0=fierd(נורה)
	Ebulletdirectoin2 dw 0;0=null,1=up,2=down,3=right,4=left/directoin of bullet
	EBulletColided2 db 0
	
	Collid db 0;if its colided it is 1 if not it is 0
	
	redmode db 0;0 is false 1 is true
	RedModePhoto db 0;0 is normel 1 is red photo
	redmodecnt dw 0
	
	endgame db 0;0 is false 1 is true
	
	HowManyTimes dw 500
	
	level db 0
	scoreLevel dw 0
CODESEG
 

 
start:
	mov ax, @data
	mov ds, ax
	call SetGrapic;set garpic to the right mode

	call StartScreen;print the start screen and the rect with the writing to press to play and wait util the it pressed 
	
	call drawmap;print map
	mov [Xtank],TankSpwanX;cord(מיקום) of the tank in the start
	mov [Ytank],TankSpwanY;cord(מיקום) of the tank in the start changed it abit from 154
	call drawtankup;print the tank looking up
	call printScore;print the str score: 
	call addScore;print the number of score
	call printLives
		
	mov [XETank],ETankSpwanX;cord(מיקום) of the enemy tank in the start
	mov [YETank],ETankSpwanY;cord(מיקום) of the enemy tank in the start
	
	mov ax,1;tell PutETankOnScreen its the first enemy, parmter of PutETankOnScreen
	push ax
	push offset ETankShadow ;parmter of PutETankOnScreen
	push [XEtank] ;parmter of PutETankOnScreen
	push [YETank] ;parmter of PutETankOnScreen
	call PutETankOnScreen
	call WhereToMoveEtank ;make the first enamy tank move towars the player
	
	mov [XETank2],ETankSpwanX2;cord(מיקום) of the enemy 2 tank in the start
	mov [YETank2],ETankSpwanY2;cord(מיקום) of the enemy 2 tank in the start
	
	mov ax,2 ;tell PutETankOnScreen its the secand enemy, parmter of PutETankOnScreen
	push ax
	push offset ETankShadow2;parmter of PutETankOnScreen
	push [XEtank2];parmter of PutETankOnScreen
	push [YETank2];parmter of PutETankOnScreen
	call PutETankOnScreen
	call WhereToMoveEtank2;make the first move of enemy 2 towards the player

@@mainloop:	

	mov bx,8
	GET_MODOLO
	cmp [modulo],0
	jnz @@MoveBullet
	call CheckAndReadKey;check if a key to move got pressed
	cmp [kexit],1;to exit
	jne @@MoveBullet;jmp to end
	jmp exit
	
@@MoveBullet:

	cmp [bulletFinish],0;if there is a bullet
	jnz @@moveETank
	mov bx,200
	GET_MODOLO
	cmp [modulo],0
	jnz @@moveETank
	call MoveBulletcount;countine the bullet movment if there is one
	
 @@moveETank:
	mov bx,10000
	GET_MODOLO
	cmp [modulo],0
	jnz @@MoveEBullet
	call WhereToMoveEtank;make the moves towards to plyer by enemy tank algritam
	call WhereToMoveEtank2;make the moves towards to plyer by enemy tank 2 algritam

@@MoveEBullet:
	cmp [EbulletFinish],0
	jnz @@MoveEBullet2
	mov bx,200
	GET_MODOLO
	cmp [modulo],0
	jnz @@MoveEBullet2
	mov ax,1;first enemy tank
	push ax
	call MoveEBulletcount;countine the bullet movment of the enemy bullet if there is one
	
@@MoveEBullet2:
	cmp [EbulletFinish2],0
	jnz @@redmode
	mov bx,400
	GET_MODOLO
	cmp [modulo],0
	jnz @@redmode
	mov ax,2;secand enemy tank,tells MoveEBulletcount its the secand enemy tank bullet that it need to countine
	push ax
	call MoveEBulletcount;countine the bullet movment of the enemy bullet if there is one
	
@@redmode:
	cmp [redmode],1
	jne @@checkNextLevel
	mov bx,40
	GET_MODOLO
	cmp [modulo],0
	jnz @@checkNextLevel
	
	inc [redmodecnt]
	cmp [redmodecnt],3000h
	je @@stop
	jmp @@checkNextLevel
@@stop:
	mov [redmode],0
	mov [redmodecnt],0
	jmp @@checkNextLevel

@@checkNextLevel:
	mov bx,40
	GET_MODOLO
	cmp [modulo],0
	jnz @@checkEndGame
	call NextLevel
	jmp @@checkEndGame
	
@@checkEndGame:
	cmp [endgame],1
	je @@endgame
	jmp @@countloop
@@endgame:	
	mov dx,offset FileGameOver
	mov [BmpTop],0
	mov [BmpLeft],0
	mov[BmpHeight],200
	mov[BmpWidth],320
	call OpenShowBmp
	call PrintbestScore
	jmp exit
@@countloop:
	inc [maincnt];add 1 to main loop counter
	jmp @@mainloop;loop of the game
exit:
	
	mov ah,0
	int 16h
	
	mov ax,2
	int 10h

	
	mov ax, 4c00h
	int 21h
	
;================================================
; Description - Write on screen the value of ax (decimal)
;               the practice :  
;				Divide AX by 10 and put the Mod on stack 
;               Repeat Until AX smaller than 10 then print AX (MSB) 
;           	then pop from the stack all what we kept there and show it. 
; INPUT: AX
; OUTPUT: Screen 
; Register Usage:   
;================================================
proc ShowAxDecimal
	push ax
	push bx
	push cx
	push dx

	 
	; check if negative
	test ax,08000h
	jz PositiveAx
		
	;  put '-' on the screen
	push ax
	mov dl,'-'
	mov ah,2
	int 21h
	pop ax

	neg ax ; make it positive
PositiveAx:
	mov cx,0   ; will count how many time we did push 
	mov bx,10  ; the divider

put_mode_to_stack:
	xor dx,dx
	div bx
	add dl,30h
	; dl is the current LSB digit 
	; we cant push only dl so we push all dx
	push dx    
	inc cx
	cmp ax,9   ; check if it is the last time to div
	jg put_mode_to_stack

	cmp ax,0
	jz pop_next  ; jump if ax was totally 0
	add al,30h  
	mov dl, al    
	mov ah, 2h
	int 21h        ; show first digit MSB
	   
pop_next: 
	pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	mov dl, al
	mov ah, 2h
	int 21h        ; show all rest digits
	loop pop_next

	mov dl, ' '
	mov ah, 2h
	int 21h

	pop dx
	pop cx
	pop bx
	pop ax

	ret
endp ShowAxDecimal


proc SetGrapic
	mov ax,13h   ; 320 X 200 
	int 10h
	ret
endp SetGrapic	

proc OpenShowBmp 
	mov [ErrorFile],0
	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call ShowBMP
	
	 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp
 
 proc OpenBmpFile							 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile
 
 proc ReadBmpHeader						
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader

proc ReadBmpPalette  ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette

; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette							
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette

proc ShowBMP
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpHeight lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
 
	mov ax,[BmpWidth] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	mov bp, 0
	and ax, 3
	jz @@row_ok
	mov bp,4
	sub bp,ax

@@row_ok:	
	mov cx,[BmpHeight]
    dec cx
	add cx,[BmpTop] ; add the Y on entire screen
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	add di,[BmpLeft]
	cld ; Clear direction flag, for movsb forward
	
	mov cx, [BmpHeight]
@@NextLine:
	push cx
 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpWidth]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory es:di
	mov cx,[BmpWidth]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	sub di,[BmpWidth]            ; return to left bmp
	sub di,SCREEN_WIDTH  ; jump one screen line up
	
	pop cx
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP

 proc CloseBmpFile 
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile

 proc StartScreen;print the startscrren and rect and the strings (it is befor the game)
	mov dx, offset FileStart;show the start picture
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpWidth],320
	mov [BmpHeight],200
	
	call OpenShowBmp
 
	mov ax,seg StartStr;print super tank
	mov es,ax
	mov ah,13h
	mov bp,offset StartStr
	mov dh,01
	mov dl,14d
	mov cx,lenStartStr
	mov al,1
	mov bh,0
	mov bl,1 
	int 10h

	mov si,10;make the rect for the press
	mov cx,60
	mov dx,90
	mov [recthight],40
	mov [rectwidth],180
	mov[Xrect],cx
	mov[Yrect],dx
	call rect
	
	mov ax,seg ClickHere;print press
	mov es,ax
	mov ah,13h
	mov bp,offset ClickHere
	mov dh,13
	mov dl,10
	mov cx,lenClickHere
	mov al,1
	mov bh,0
	mov bl,1 
	int 10h
	SHOW_MOUSE
	call RegisterAsyncMouse
@@next:
	;main do nothing but check exit condition 
	cmp [pressed], 1
	je @@exit
	jmp @@next
@@exit:	
	HIDE_MOUSE
	ret
 endp StartScreen
 
 proc rect;make a rect using DrawHorizontalLine and DrawVerticalLine
	push cx
	push si
	push dx
	
	mov si,[rectwidth];the length 
	call DrawHorizontalLine
	mov si,[recthight]
	call DrawVerticalLine
	add cx,[rectwidth];to go to right side
	call DrawVerticalLine
	sub cx,[rectwidth];to return to the left
	mov si,[rectwidth]
	add dx,[recthight];to go down
	call DrawHorizontalLine
	
	pop dx
	pop si
	pop cx
	ret
 endp rect
 
 proc DrawHorizontalLine;draw a Horizontal line	
	push si
	push cx
DrawLine:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	 
	
	inc cx
	dec si
	jmp DrawLine
	
	
ExitDrawLine:
	pop cx
    pop si
	ret
endp DrawHorizontalLine

proc DrawVerticalLine;draw a Vertical line	
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz @@ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	
	 
	
	inc dx
	dec si
	jmp DrawVertical
	
	
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine

proc checkpress far;check if the mouse prees is in the rect
	shr cx,1
	
	cmp cx, [Xrect]          ; Check if point is to the left of the rectangle
	jl @@ret

	mov ax, [Xrect]
	add ax, [rectwidth]           ; Calculate right edge of the rectangle
	cmp cx, ax             ; Check if point is to the right of the rectangle
	jg @@ret

	cmp dx, [Yrect]          ; Check if point is above the rectangle
	jl @@ret

	mov ax, [Yrect]
	add ax, [recthight]          ; Calculate bottom edge of the rectangle
	cmp dx, ax             ; Check if point is below the rectangle
	jg @@ret
	
@@pressed:
	mov[pressed],1
@@ret:	
	retf
endp checkpress 
  
proc  RegisterAsyncMouse;async function that call checkpress when the mouse is left clicked	
;----- Define interrupt subroutine parameters
        mov ax, seg checkpress
        mov es, ax
        mov dx, offset checkpress  
        mov cx,0000000000000010b 
		mov ax,0Ch
        int 33h                 
       	ret
endp RegisterAsyncMouse

proc printLives;print 3 tanks as lives
	push dx
	push ax
	
	mov dx,offset FileLives
	mov [BmpTop],LivesSpwanY
	mov [BmpLeft],LivesSpwanX
	mov[BmpHeight],LivesHight
	mov[BmpWidth],LivesWidth
	call OpenShowBmp
		
	pop ax	
	pop dx
	ret
endp printLives

proc delLives;delet the lives and uses an arr for where to delet
	push ax
	push bx
	
	mov bl,[delLivesCnt]
	mov ax,LivesSpwanX
	add al, [byte delLivesArr+bx]
	mov dx,offset FileLivesBlack
	xor ah,ah
	mov [BmpTop],LivesSpwanY
	mov [BmpLeft],ax
	mov[BmpHeight],14
	mov[BmpWidth],14
	call OpenShowBmp
	inc [delLivesCnt]
	
	pop bx
	pop ax
	ret
endp delLives

proc drawtankup;draw the tank up and when red mode is active draw if half of the time red
	push dx;becuse it can destroy them and mabey i will need them
	push ax
	
	cmp [redmode],1
	je @@printred
	jmp @@normal
@@printred:
	cmp [RedModePhoto],1
	je @@printredp
	jne @@normal
@@printredp:
	mov [RedModePhoto],0
	mov dx,offset FileETank
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightUD
	mov[BmpWidth],TankwidthUD
	call OpenShowBmp
	jmp @@ret
@@normal:
	mov [RedModePhoto],1
	mov dx,offset FileTank	
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightUD
	mov[BmpWidth],TankwidthUD
	call OpenShowBmp
@@ret:		
	pop ax	
	pop dx
	ret
endp drawtankup

proc drawtankdown;draw the tank down and when red mode is active draw if half of the time red
	push dx;becuse it can destroy them and mabey i will need them
	push ax
	
	cmp [redmode],1
	je @@printred
	jmp @@normal
@@printred:
	cmp [RedModePhoto],1
	je @@printredp
	jne @@normal
@@printredp:
	mov [RedModePhoto],0
	mov dx,offset FileETankDown
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightUD
	mov[BmpWidth],TankwidthUD
	call OpenShowBmp
	jmp @@ret
@@normal:
	mov [RedModePhoto],1
	mov dx,offset FileTankDown
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightUD
	mov[BmpWidth],TankwidthUD
	call OpenShowBmp
@@ret:	
	pop ax
	pop dx
	ret
endp drawtankdown
 
proc drawtankleft;draw the tank left and when red mode is active draw if half of the time red
	push dx;becuse it can destroy them and mabey i will need them
	push ax
	
	cmp [redmode],1
	je @@printred
	jmp @@normal
@@printred:
	cmp [RedModePhoto],1
	je @@printredp
	jne @@normal
@@printredp:
	mov [RedModePhoto],0
	mov dx,offset FileETankLeft
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightLR
	mov[BmpWidth],TankwidthLR
	call OpenShowBmp
	jmp @@ret
@@normal:
	mov [RedModePhoto],1
	mov dx,offset FileTankLeft
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightLR
	mov[BmpWidth],TankwidthLR
	call OpenShowBmp
@@ret:	
	pop ax
	pop dx
	ret
endp drawtankleft
 
proc drawtankright;draw the tank right and when red mode is active draw if half of the time red
	push dx;becuse it can destroy them and mabey i will need them
	push ax
	
	cmp [redmode],1
	je @@printred
	jmp @@normal
@@printred:
	cmp [RedModePhoto],1
	je @@printredp
	jne @@normal
@@printredp:
	mov [RedModePhoto],0
	mov dx,offset FileETankRight
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightLR
	mov[BmpWidth],TankwidthLR
	call OpenShowBmp
	jmp @@ret
@@normal:
	mov [RedModePhoto],1
	mov dx,offset FileTankRight	
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightLR
	mov[BmpWidth],TankwidthLR
	call OpenShowBmp
@@ret:
	pop ax
	pop dx
	ret
endp drawtankright
 
proc drawmap;draw the game map
	mov dx, offset FileMap
	mov [BmpLeft],20
	mov [BmpTop],20
	mov [BmpWidth], 277
	mov [BmpHeight] ,154
	call OpenShowBmp
	
	ret
endp drawmap

proc CheckAndReadKey;pooling to vheck if the keybored id pressed and if it is call checkkey
	  mov ah ,1
	  int 16h
	  pushf
	  jz @@return 
	  mov ah ,0
	  int 16h
	  call checkkey
@@return:	
	  popf
	  ret
endp CheckAndReadKey

proc checkkey;check what key have i pressed called by CheckAndReadKey
	cmp ah,48h
	jne @@checkdown
	call moveup
	
@@checkdown:	
	cmp ah,50h
	jne @@checkleft
	call movedown
	jmp @@ret
@@checkleft:	
	cmp ah,4bh
	jne @@checkRight
	call moveleft
	jmp @@ret
@@checkRight:	
	cmp ah,4dh
	jne @@checkfire
	call moveright
	jmp @@ret
@@checkfire:
	cmp ah,39h
	jne @@exit
	call fire
	jmp @@ret
@@exit:	
	cmp al,'k'
	jne @@ret
	mov [kexit],1
@@ret:	
	ret 
endp checkkey

proc deltank;used only when i switch direction it is deliting the tank
	cmp [ud],1
	jne @@RightOrLeft
	mov dx,offset FileBlackTank
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightUD
	mov[BmpWidth],TankwidthUD
	call OpenShowBmp
	jmp @@ret
@@RightOrLeft:
	mov dx,offset FileBlackTankLR
	mov ax,[Ytank]
	mov [BmpTop],ax
	mov ax,[Xtank]
	mov [BmpLeft],ax
	mov[BmpHeight],TankHightLR
	mov[BmpWidth],TankwidthLR
	call OpenShowBmp
@@ret:	
	ret
endp deltank

proc moveup;moves the tank up and checks border and if the tank took someting and colisoin(התנגשות)
	push ax;to not destroy it if i need it
@@start:	
	mov ax,[ytank];check the border
	dec ax
	cmp ax,23
	jle @@ret
	
	call checkup;check for points and red mode
	call checkup
	
	sub [Ytank],1;to see if in the next step there will be colisoin(התנגשות)
	
	push TankwidthUD
	push TankHightUD
	call playerToETankCollid;checks colisoin(התנגשות)
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [Ytank],1;return the change i did in the start

	cmp [lr],1
	jne @@cunt
	push 1;what direction is it 1 is up or down(UD)
	call checkCol
	call deltank;to delet the all tank only when i switch direction
	
@@cunt:	
	mov [ud],1;to know that im moving up or down later used in deltank
    mov [lr],0
	mov [up],1
	
	sub [Ytank],1
	
	call drawtankup

@@ret:	
	pop ax
	ret 
endp moveup

proc movedown;moves the tank down and checks border and if the tank took someting and colisoin(התנגשות)
	push ax
@@start:	
	mov ax,[ytank];check the border
	inc ax
	cmp ax,157
	jae @@ret
	
	add [Ytank],1

	call checkdown
	call checkdown
		
	push TankwidthUD
	push TankHightUD
	call playerToETankCollid;checks colisoin(התנגשות)
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [Ytank],1
	
	cmp [lr],1
	jne @@cunt
	push 1
	call checkCol
	call deltank;to delet the all tank only when i switch direction

@@cunt:		
	mov [ud],1;to know that im moving up or down later used in deltank
    mov [lr],0
	mov [up],0
		
	add [Ytank],1
	
	call drawtankdown

@@ret:	
	pop ax
	ret 
endp movedown

proc moveleft;moves the tank left and checks border and if the tank took someting and colisoin(התנגשות)
	push ax
@@start:	
	mov ax,[Xtank];check the border
	dec ax
	cmp ax,23
	jle @@ret
	
	call checkleft
	call checkleft;there can be 2 to take
	
	sub [Xtank],1
	
	push TankwidthLR
	push TankHightLR
	call playerToETankCollid;checks colisoin(התנגשות)
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [Xtank],1

	cmp [ud],1
	jne @@cunt
	push 2
	call checkCol
	call deltank;to delet the all tank only when i switch direction

@@cunt:	
	mov [lr],1
	mov [ud],0
	mov [left],1
	
	sub [Xtank],1
	call drawtankleft

@@ret:	
	pop ax
	ret
endp moveleft

proc moveright;moves the tank right and checks border and if the tank took someting and colisoin(התנגשות)
	push ax
@@start:	
	mov ax,[Xtank];check the border
	inc ax
	cmp ax,279
	jae @@ret
	
	call checkright
	call checkright
	
	add [Xtank],1
	
	push TankwidthLR
	push TankHightLR
	call playerToETankCollid;checks colisoin(התנגשות)
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [Xtank],1
	
	cmp [ud],1
	jne @@cunt
	push 2
	call checkCol
	call deltank;to delet the all tank only when i switch direction
	

@@cunt:	
	mov [lr],1
	mov [ud],0
	mov [left],0
	
	add [Xtank],1
	
	call drawtankright

@@ret:	
	pop ax
	ret
endp moveright

direction equ [bp+4];1 is up or down 2 is left or right 
proc checkcol;checks colisoin when the tank changes dirctoin
	push bp
	mov bp,sp
	
	mov ax,direction
	cmp ax,1
	jne @@LeftOrRight
	
	add [Ytank],6
	mov cx,6
@@checkcolup:;checks the collision 6 times	
	push cx
	call checkup
	dec [Ytank]
	pop cx
	loop @@checkcolup
	
	sub [Ytank],6
	mov cx,6
@@checkcoldown:;checks the collision 6 times	
	push cx
	call checkdown
	inc [Ytank]
	pop cx
	loop @@checkcoldown
	
	jmp @@ret
	
@@LeftOrRight:	
	add [Xtank],6
	mov cx,6
@@checkcolleft:;checks the collision 6 times	
	push cx
	call checkleft
	dec [Xtank]
	pop cx
	loop @@checkcolleft

	sub [Xtank],6
	mov cx,6
@@checkcolright:;checks the collision 6 times	
	push cx
	call checkRight
	inc [Xtank]
	pop cx
	loop @@checkcolright
	
@@ret:
	pop bp
	ret 2
endp checkCol

proc checkup;check if there is somthing above the tank called by move right
	mov di,10;width to serch all of it
	mov si,0;to add cx times to cx
@@check:	
	
	mov ah,0dh
	mov bx,0
	mov dx,[ytank]
	dec dx
	mov cx,[xtank]
	add cx,si
	int 10h;in al return color
	cmp al,113d;the green color
	je @@retCx;no need to serch more
	cmp al,79d
	je @@redmode
	inc si;to add to cx
	
	dec di
	cmp di,0
	jne @@check
	jmp @@retloop;if didnt found
@@redmode:
	mov [redmode],1	
	jmp @@delit	
@@retCx:	
	add [Score],50d
	add [scoreLevel],50d
	call addScore
@@delit:	
	sub cx,2;move it left
	call delgreenUD;del 
	dec dx ;mov it up
	call delgreenUD
	
@@retloop:	

	ret
endp checkup

proc checkdown;check if there is somthing above the tank called by move right
	mov di,TankwidthUD;width
	mov si,0
@@check:	
	mov ah,0dh
	mov bx,0
	mov dx,[ytank]
	add dx,TankHightUD;length + 1
	mov cx,[xtank]
	add cx,si
	int 10h
	cmp al,113d;the green color
	je @@retCx
	cmp al,79d
	je @@redmode
	inc si
	
	dec di
	cmp di,0
	jne @@check
	jmp @@retloop
@@redmode:
	mov [redmode],1	
	jmp @@delit		
@@retCx:	
	add [Score],50d
	add [scoreLevel],50d
	call addScore
@@delit:	
	sub cx,2
	call delgreenUD;del 
	inc dx 
	call delgreenUD
@@retloop:	
	ret
endp checkdown

proc checkright;check if there is somthing right the tank called by move right
	mov di,12;hight
	mov si,0
@@check:	
	mov ah,0dh
	mov bx,0
	mov dx,[ytank]
	add dx,si
	mov cx,[xtank]
	add cx,15;width of tank +1
	int 10h
	cmp al,113d;the green color
	je @@retCx
	cmp al,79d
	je @@redmode
	inc si
	
	dec di
	cmp di,0
	jne @@check
	jmp @@retloop
@@redmode:
	mov [redmode],1	
	jmp @@delit		
@@retCx:	
	add [Score],50d
	add [scoreLevel],50d
	call addScore
@@delit:	
	sub dx,2;mov it up
	call delgreenLR
	add cx,2;mov it right
	call delgreenLR
@@retloop:	
	ret
endp checkright

proc checkleft;check if there is somthing left the tank called by move left
	mov di,12;hight
	mov si,0
	
@@check:	
	mov ah,0dh
	mov bx,0
	mov dx,[ytank]
	add dx,si
	mov cx,[xtank]
	dec cx
	int 10h
	cmp al,113d;the green color
	je @@retCx
	cmp al,79d
	je @@redmode
	inc si
	
	dec di
	cmp di,0
	jne @@check
	jmp @@retloop
@@redmode:
	mov [redmode],1	
	jmp @@delit		
@@retCx:	
	add [Score],50d
	add [scoreLevel],50d
	call addScore
@@delit:	
	sub dx,2;mov it up
	call delgreenLR
	dec cx;mov it right
	call delgreenLR
	
@@retloop:	
	ret
endp checkleft

proc printScore;print the string score 
	mov ax,seg Score;print super tank score
	mov es,ax
	mov ah,13h
	mov bp,offset StrScore
	mov dh,1
	mov dl,1d
	mov cx,lenStrScore
	mov al,1
	mov bh,0
	mov bl,3 
	int 10h
	ret
endp printScore

proc addScore;print the score it self by sting the curser postion and calling ShowAxDecimal
	push cx;to not ruin delgreenUD
	push dx
	
	mov ah,2
	mov dl,lenStrScore
	mov dh,1
	int 10h
	mov ax,[Score]
	call ShowAxDecimal
	
	pop dx 
	pop cx
	ret
endp addScore

;cx=column number
;dx=row number
proc delgreenUD;delet the green(score) and red(redmode) dots the tank takes on the map in the up and down
	push cx
	push si
	push dx
	push cx
	
	mov di,2
@@agin:
	dec di
	mov si,8		
@@draw: 
	mov ah,0ch
	mov al,0
	mov bh,0
	int 10h
	dec si
	inc cx
	cmp si,0
	jne @@draw
	
	cmp di,1
	jne @@skippop
	pop cx
@@skippop:	
	inc dx
	cmp di,0
	jne @@agin
	
	pop dx
	pop si
	pop cx
	ret
endp delgreenUD

proc delgreenLR;delet the green(score) and red(redmode) dots the tank takes on the map in the left and right
	push cx
	push si
	push dx
	push dx
	
	mov di,2
@@agin:
	dec di
	mov si,8
@@draw: 
	mov ah,0ch
	mov al,0
	mov bh,3
	int 10h
	dec si
	inc dx
	cmp si,0
	jne @@draw
	
	dec cx
	cmp di,1
	jne @@skippop
	pop dx
@@skippop:	
	cmp di,0
	jne @@agin
	
	pop dx
	pop si
	pop cx
	ret
endp delgreenLR

proc putBulletRL;put the bullet on the screen pixel pixel (matrix) left or right
	
	
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[YBulletRL]
nextR:
	
		mov di,0
		mov cx,[XBulletRL]
	nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte bulletRL + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,lentbulletRL
		jb nextC
		inc dx
		inc si
	
	cmp si, HightbulletRL
	jb nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putBulletRL

proc  MoveBulletRLLeft;moves the bullet left also checks colisoin and border	
	mov ax,[XbulletRL];check the border
	dec ax
	cmp ax,23
	mov[bulletFinish],1;to see if its in the end
	jle @@retborder
		
	push LentbulletRL
	push HightbulletRL
	call checkPlayerBulletsHitsLR	
	
	cmp [BulletColided],0
	jne @@retcolided
	
	mov [bulletFinish],0;if its no in the end 
	call putBulletRL ; delete ball at curret locatein
	dec [XBulletRL]
	call putBulletRL ; put it  at new location
	jmp @@ret
@@retborder:
	call putBulletRL;if border delet it
	mov [bulletdirectoin],0;no direction when there is no bullet	
	jmp @@ret 
@@retcolided:
	mov [BulletColided],0
	mov [bulletdirectoin],0	
@@ret:	
	ret
endp MoveBulletRLLeft

proc  MoveBulletRLRight;moves the bullet right also checks colisoin and border	
	mov ax,[XbulletRL];check the border
	inc ax
	cmp ax,279
	mov[bulletFinish],1;to see if its in the end
	jae @@retborder
	
	push LentbulletRL
	push HightbulletRL
	call checkPlayerBulletsHitsLR	
	
	cmp [BulletColided],0
	jne @@retcolided
	
	mov [bulletFinish],0;if its no in the end 
	call putBulletRL ; delete ball at curret locatein
	inc [XBulletRL]
	call putBulletRL ; put it  at new location
	jmp @@ret
@@retborder:
	call putBulletRL;if border delet it	
	mov [bulletdirectoin],0	
	jmp @@ret 
@@retcolided:
	mov [BulletColided],0
	mov [bulletdirectoin],0	
	
@@ret:	
	ret
endp MoveBulletRLRight

proc putBulletUD;put the bullet on the screen pixel pixel (matrix) up or down
	
	
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[YBulletUD]
@@nextR:
	
		mov di,0
		mov cx,[XBulletUD]
	@@nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte BulletUD + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,lentbulletUD
		jb @@nextC
		inc dx
		inc si
	
	cmp si, HightbulletUD
	jb @@nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putBulletUD

proc  MoveBulletUDUp;moves the bullet up also checks colisoin and border	
	mov ax,[ybulletUD];check the border
	dec ax
	cmp ax,23
	mov[bulletFinish],1;to see if its in the end
	jle @@retborder
	
	push lentbulletUD
	push HightbulletUD
	call checkPlayerBulletsHitsUD	
	
	cmp [BulletColided],0
	jne @@retcolided 
	
	mov [bulletFinish],0;if its no in the end 
	call putBulletUD ; delete ball at curret locatein
	dec [ybulletUD]
	call putBulletUD ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putBulletUD;if its in the border delet it
	mov [bulletdirectoin],0
	jmp @@ret 
@@retcolided:
	mov [BulletColided],0
	mov [bulletdirectoin],0	
@@ret:	
	ret
endp MoveBulletUDUp

proc  MoveBulletUDDown;moves the bullet down also checks colisoin and border	
	mov ax,[ybulletUD];check the border
	inc ax
	cmp ax,157
	mov[bulletFinish],1;to see if its in the end
	jae @@retborder
	
	push lentbulletUD
	push HightbulletUD
	call checkPlayerBulletsHitsUD	
	
	cmp [BulletColided],0
	jne @@retcolided
	
	mov [bulletFinish],0;if its no in the end 
	call putBulletUD ; delete ball at curret locatein
	inc [ybulletUD]
	call putBulletUD ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putBulletUD;if its in the border delet it
	mov [bulletdirectoin],0
	jmp @@ret 
@@retcolided:
	mov [BulletColided],0
	mov [bulletdirectoin],0	
@@ret:	
	ret
endp MoveBulletUDDown

proc fire;fire the bullet up down left or right by ussing bools of the tank movment
	cmp [bulletFinish],0;if there is one dont do onther
	jne @@checkedDirectoin
	jmp @@ret
@@checkedDirectoin:
	cmp [ud],1
	jne @@checkRL;if its not up or down
	cmp [up],1
	je @@fireUp;if its up
	jmp @@fireDown;if its down
@@checkRL:	

	cmp [left],1;i know its left or right becaus we checked up or down and it wasnt them
	je @@fireLeft;if its left
	jmp @@fireRight;if its right	
@@fireUp:

	mov [bulletdirectoin],1
	mov ax,[Xtank]
	mov [xbulletUD],ax
	mov ax,[ytank]
	sub ax,3
	mov [YBulletUD],ax
	call putBulletUD
	call MoveBulletUDUp
	call Shoot
	jmp @@ret	
@@fireDown:
	
	mov [bulletdirectoin],2
	mov ax,[Xtank]
	add ax,3;after checking looks the best
	mov [xbulletUD],ax
	mov ax,[ytank]
	add ax,16;half length
	mov [ybulletUD],ax
	call putBulletUD
	call MoveBulletUDDown
	call Shoot
	jmp @@ret

@@fireRight:

	cmp [bulletFinish],1;if its in move only move it
	mov [bulletdirectoin],3	
	mov ax,[Xtank]
	add ax,15;length
	mov [xbulletRL],ax
	mov ax,[ytank]
	mov [YBulletRL],ax
	call putBulletRL
	call MoveBulletRLRight
	call Shoot
	jmp @@ret
@@fireLeft:

	cmp [bulletFinish],1;if its in move only move it
	mov [bulletdirectoin],4	
	mov ax,[Xtank]
	add ax,6;half of tank length
	mov [xbulletRL],ax
	mov ax,[ytank]
	add ax,5;hight of tank
	mov [YBulletRL],ax
	call putBulletRL;put it
	call MoveBulletRLLeft;move it
	call Shoot
	jmp @@ret	
@@ret:	
	ret
endp fire

proc MoveBulletcount;countine bullet movment by bools
	cmp [bulletdirectoin],0
	je @@ret
	cmp [bulletdirectoin],1
	je @@up
	cmp [bulletdirectoin],2
	je @@down
	cmp [bulletdirectoin],3
	je @@right
	cmp [bulletdirectoin],4
	je @@left
	
@@up:
call MoveBulletUDUp
jmp @@ret

@@down:
call MoveBulletUDDown
jmp @@ret

@@right:
call MoveBulletRLRight
jmp @@ret

@@left:
call MoveBulletRLLeft
jmp @@ret

@@ret:
	ret
endp MoveBulletcount

EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc drawEtankup;draw enemy tank up
	push bp
	mov bp, sp
	push dx
	push ax
	
	mov dx,offset FileETank
	mov ax,EtankY
	mov [BmpTop],ax
	mov ax,EtankX
	mov [BmpLeft],ax
	mov[BmpHeight],ETankHightUD
	mov[BmpWidth],ETankwidthUD
	call OpenShowBmp
		
	pop ax	
	pop dx
	pop bp
	ret 4
endp drawEtankup

EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc drawEtankdown;draw enemy tank down
	push bp
	mov bp, sp
	push dx
	
	mov dx,offset FileETankDown
	mov ax,EtankY
	mov [BmpTop],ax
	mov ax,EtankX
	mov [BmpLeft],ax
	mov[BmpHeight],ETankHightUD
	mov[BmpWidth],ETankwidthUD
	call OpenShowBmp
	
	pop dx
	pop bp
	ret 4
endp drawEtankdown
 
EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc drawEtankleft;draw enemy tank left
	push bp
	mov bp, sp
	push dx
	
	mov dx,offset FileETankLeft
	mov ax,EtankY
	mov [BmpTop],ax
	mov ax,EtankX
	mov [BmpLeft],ax
	mov[BmpHeight],ETankHightLR
	mov[BmpWidth],ETankwidthLR
	call OpenShowBmp
	
	pop dx
	pop bp
	ret 4
endp drawEtankleft

EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc drawEtankright;draw enemy tank right
	push bp
	mov bp, sp
	push dx
	
	mov dx,offset FileETankRight
	mov ax,EtankY
	mov [BmpTop],ax
	mov ax,EtankX
	mov [BmpLeft],ax
	mov[BmpHeight],ETankHightLR
	mov[BmpWidth],ETankwidthLR
	call OpenShowBmp
	
	pop dx
	pop bp
	ret 4
endp drawEtankright

witchETank equ [bp+10]
shadowoffset equ [bp+8]
EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc PutETankOnScreen;put with shadow and print the shadow
	push bp
	mov bp, sp
	push cx
	push dx
	push es
	
	mov ax,0a000h
	mov es,ax
	mov ax,1
	push ax   ; from shadow to screen
	mov cx,EtankX
	mov dx,EtankY
	 
	mov ax, 16; need to cahnge for etank size
	push ax
	mov ax, 16
	push ax
	
	call getXYonScreen  ; return ax
	push ax ; from screen 
	
	mov ax, shadowoffset
	push ax
	
	call FromToShadow  
	 
	mov ax,witchETank
	cmp ax,1
	jne @@ETank2
	call BoolToMove
	jmp @@ret
@@ETank2:
	call BoolToMove2
@@ret:	
	pop es
	pop dx
	pop cx
	pop bp
	ret 8
endp PutETankOnScreen

proc RemoveETankFromScreen;remove the e tank and get the shdow
	push bp
	mov bp, sp
	push cx
	push dx
	push es
	mov ax,0a000h
	mov es,ax
	
	mov ax,0
	push ax   ; from screen to shadow 
	
	mov ax, 16
	push ax

	mov ax, 16
	push ax
	
	mov cx,EtankX
	mov dx,EtankY
	call getXYonScreen  ; return ax
	;call ShowAxDecimal
	push ax
	mov ax,	shadowoffset
	push ax
 
	call FromToShadow 
	
	
	pop es
	pop dx
	pop cx
	pop bp
	ret 6
endp RemoveETankFromScreen
	
proc FromToShadow;print or take the shadow
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	push si
	push di
	 
	cld
	
	mov si, [bp +4] ;  shodowoffset
	mov di, [bp +6] ; screen location
	mov cx, [bp +8] ; hight
	mov dx, [bp +10]   ; len

start1:	
	cmp [word bp +12],0 ;direction for clear use 0, for print use 1
	jz @@toScreen

@@r:
	push cx
	mov cx, dx
@@c:
	mov al, [es:di] ; mov al, current pixel
	cmp al,113d
	jne @@countcheck
	mov [si],al ;mov to shadowarray the pixel 
	jmp @@count
@@countcheck:
	cmp al,79d
	jne @@black
	mov [si],al
	jmp @@count
@@black:
	mov al,0
	mov [si],al
@@count:	
	inc si ; move to next pixel on array
	inc di ;mov to next pixel on screen
	loop @@c ; loop to get array of collors
 	
	add di, 320 ;line down
	sub di, dx ;line down
	
	pop cx ; pop for loop
	loop @@r ;loop next line
	
	jmp @@ret ;finished getting array
	 
@@toScreen:
	 
@@r2:
	push cx
	mov cx, dx
	
	rep movsb ;print one Line cx times pointer for screen si showing to di
 	
	add di, 320 ;goto next Line
	sub di, dx ; -------------
	
	pop cx ; pop for loop
	loop @@r2 ;
	
@@ret:	
	 
	pop di
	pop si
	pop dx
	pop cx
	pop ax
	pop bp
	ret 10
endp FromToShadow
 
proc getXYonScreen;get from x,y to one dimantoin(חד ממדי)
	mov ax,dx
	shl dx,8
	shl ax, 6
	add ax,dx
	add ax,cx
	ret 
endp getXYonScreen

proc moveETankup;moves the enemy tank up and checks colisoin and borders
	push ax
@@start:	
	mov ax,[YETank];check the border
	dec ax
	cmp ax,23
	jae @@count
	jmp @@ret
@@count:	
	sub [YEtank],1
	
	mov ax,1
	push ax;witch etank is this
	push ETankwidthUD
	push ETankHightUD
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [YEtank],1

	cmp [Elr],1
	jne @@cunt	
@@cunt:	
	push offset ETankShadow
	push [XEtank]
	push [YETank]
    call RemoveETankFromScreen
	mov [Eud],1;to know that im moving up or down later used in deltank
    mov [Elr],0
	mov [Eup],1
	
	sub [YETank],1
	
	mov ax,1
	push ax
	push offset ETankShadow
	push [XEtank]
	push [YETank]
	call PutETankOnScreen
		
@@ret:	
	pop ax
	ret 
endp moveETankup

proc moveETankdown;moves the enemy tank down and checks colisoin and borders
	push ax
@@start:	
	mov ax,[YETank];check the border
	inc ax
	cmp ax,157
	jle @@count
	jmp @@ret
@@count:	
	add [YEtank],1
	
	mov ax,1
	push ax;witch etank is this
	push ETankwidthUD
	push ETankHightUD
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [YEtank],1	
		
	cmp [Elr],1
	jne @@cunt
@@cunt:	
	push offset ETankShadow
	push [XEtank]
	push [YETank]
	
	call RemoveETankFromScreen
	mov [Eud],1;to know that im moving up or down later used in deltank
    mov [Elr],0
	mov [Eup],0
		
	add [YETank],1
	
	mov ax,1
	push ax
	push offset ETankShadow
	push [xEtank]
	push [yETank]
	call PutETankOnScreen	

@@ret:	
	pop ax
	ret 
endp moveETankdown

proc moveETankleft;moves the enemy tank left and checks colisoin and borders
	push ax
@@start:	
	mov ax,[XETank];check the border
	dec ax
	cmp ax,23
	jae @@count
	jmp @@ret
@@count:	
	
	sub [XEtank],1
	
	mov ax,1
	push ax;witch etank is this
	push ETankwidthLR
	push ETankHightLR
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [XEtank],1
	
	cmp [Eud],1
	jne @@cunt
@@cunt:	
	push offset ETankShadow
	push [xEtank]
	push [yETank]
	call RemoveETankFromScreen
	mov [Elr],1
	mov [Eud],0
	mov [Eleft],1
	
	sub [XETank],1
	
	mov ax,1
	push ax
	push offset ETankShadow
	push [xEtank]
	push [yETank]
	call PutETankOnScreen
		
@@ret:	
	pop ax
	ret 
endp moveETankleft

proc moveETankright;moves the enemy tank right and checks colisoin and borders
	push ax
@@start:	
	mov ax,[XETank];check the border
	inc ax
	cmp ax,279
	jle @@count
	jmp @@ret
@@count:	

	add [XEtank],1
	
	mov ax,1
	push ax;witch etank is this
	push ETankwidthLR
	push ETankHightLR
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [XEtank],1
	
	cmp [Eud],1
	jne @@cunt

@@cunt:	
	push offset ETankShadow
	push [xEtank]
	push [yETank]
	call RemoveETankFromScreen
	
	mov [Elr],1
	mov [Eud],0
	mov [Eleft],0
	
	add [XETank],1
	
	mov ax,1
	push ax
	push offset ETankShadow
	push [xEtank]
	push [yETank]
	call PutETankOnScreen

@@ret:	
	pop ax
	ret 
endp moveETankright
 
proc WhereToMoveEtank;moves the enemy tank by the alg(going to your y and then yor x in the x it is shotting you) 
	push ax
	push dx
	mov ax,[XETank]
	mov dx,[YETank]
	cmp dx,[Ytank]
	je @@checkX
	jg @@Etankup
	jmp @@Etankdown
@@Etankup:	
	call moveETankup
	jmp @@ret
	
@@Etankdown:	
	call moveETankdown
	jmp @@ret
	
@@checkX:
	cmp ax,[xtank]
	je @@ret
	jg @@Etankleft
	jmp @@Etankright

@@Etankleft:
	call moveETankleft
	mov ax,1
	push ax
	call Efire
	jmp @@ret

@@Etankright:
	call moveETankright	
	mov ax,1
	push ax
	call Efire
	jmp @@ret

@@ret:	
	pop dx
	pop ax
	ret
endp WhereToMoveEtank

proc BoolToMove;print the right e tank bmp called by PutETankOnScreen
	cmp [Eud],1
	jne @@checkRL;if its not up or down
	cmp [Eup],1
	je @@drawUp;if its up
	jmp @@drawDown;if its down
@@checkRL:	

	cmp [Eleft],1;i know its left or right becaus we checked up or down and it wasnt them
	je @@drawLeft;if its left
	jmp @@drawRight;if its right
	
@@drawUp:

	mov ax,[XETank]
	push ax
	mov ax,[YETank]
	push ax
	call drawEtankup
	jmp @@ret	
@@drawDown:	

	mov ax,[XETank]
	push ax
	mov ax,[YETank]
	push ax
	call drawEtankdown
	jmp @@ret	
@@drawRight:

	mov ax,[XETank]
	push ax
	mov ax,[YETank]
	push ax
	call drawEtankright
	jmp @@ret	
@@drawLeft:

	mov ax,[XETank]
	push ax
	mov ax,[YETank]
	push ax
	call drawEtankleft
	jmp @@ret	
@@ret:	

	ret
endp BoolToMove

proc putEBulletRL;put the enamy bullet on the screen pixel pixel (matrix) left or right
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[EYBulletRL]
@@nextR:
	
		mov di,0
		mov cx,[EXBulletRL]
	@@nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte EbulletRL + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,ElentbulletRL
		jb @@nextC
		inc dx
		inc si
	
	cmp si, EHightbulletRL
	jb @@nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putEBulletRL

proc  MoveEBulletRLLeft;moves the enemy bullet left also checks colisoin and border	
	
	mov ax,[EXbulletRL];check the border
	dec ax
	cmp ax,23
	mov[EbulletFinish],1;to see if its in the end
	jle @@retborder
	
	mov ax,1
	push ax
	push LentbulletRL
	push HightbulletRL
	call checkETankBulletsHitsLR	
	cmp [EBulletColided],0
	jne @@retcolided

	mov [EbulletFinish],0;if its no in the end 
	call putEBulletRL ; delete ball at curret locatein
	dec [EXBulletRL]
	call putEBulletRL ; put it  at new location
	jmp @@ret
@@retborder:
	call putEBulletRL;if border delet it
	mov [Ebulletdirectoin],0;no direction when there is no bullet	
	jmp @@ret 
@@retcolided:
	mov [EBulletColided],0
	mov [Ebulletdirectoin],0
@@ret:	
	ret
endp MoveEBulletRLLeft

proc  MoveEBulletRLRight;moves the enemy bullet right also checks colisoin and border	
	
	mov ax,[EXbulletRL];check the border
	inc ax
	cmp ax,279
	mov[EbulletFinish],1;to see if its in the end
	jae @@retborder
	
	mov ax,1
	push ax
	push LentbulletRL
	push HightbulletRL
	call checkETankBulletsHitsLR
	cmp [EBulletColided],0
	jne @@retcolided	

	mov [EbulletFinish],0;if its no in the end 
	call putEBulletRL ; delete ball at curret locatein
	inc [EXBulletRL]
	call putEBulletRL ; put it  at new location
	jmp @@ret
@@retborder:
	call putEBulletRL;if border delet it	
	mov [Ebulletdirectoin],0	
	jmp @@ret 
@@retcolided:
	mov [EBulletColided],0
	mov [Ebulletdirectoin],0
	
@@ret:	
	ret
endp MoveEBulletRLRight

proc putEBulletUD;put the enemy bullet on the screen pixel pixel (matrix) up or down
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[EYBulletUD]
@@nextR:
	
		mov di,0
		mov cx,[EXBulletUD]
	@@nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte EBulletUD + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,ElentbulletUD
		jb @@nextC
		inc dx
		inc si
	
	cmp si, EHightbulletUD
	jb @@nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putEBulletUD
 
proc  MoveEBulletUDUp;moves the enemy bullet up also checks colisoin and border	
	mov ax,[EybulletUD];check the border
	dec ax
	cmp ax,23
	mov[EbulletFinish],1;to see if its in the end
	jle @@retborder
	
	mov ax,1
	push ax
	push LentbulletUD
	push HightbulletUD
	call checkETankBulletsHitsUD
	cmp [EBulletColided],0
	jne @@retcolided
		
	mov [EbulletFinish],0;if its no in the end 
	call putEBulletUD ; delete ball at curret locatein
	dec [EybulletUD]
	call putEBulletUD ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putEBulletUD;if its in the border delet it
	mov [Ebulletdirectoin],0
	jmp @@ret
@@retcolided:
	mov [EBulletColided],0
	mov [Ebulletdirectoin],0

@@ret:	
	ret
endp MoveEBulletUDUp

proc  MoveEBulletUDDown;moves the enemy bullet down also checks colisoin and border
	mov ax,[EybulletUD];check the border
	inc ax
	cmp ax,157
	mov[EbulletFinish],1;to see if its in the end
	jae @@retborder
	
	mov ax,1
	push ax
	push LentbulletUD
	push HightbulletUD
	call checkETankBulletsHitsUD
	cmp [EBulletColided],0
	jne @@retcolided	
	
	mov [EbulletFinish],0;if its no in the end 
	call putEBulletUD ; delete ball at curret locatein
	inc [EybulletUD]
	call putEBulletUD ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putEBulletUD;if its in the border delet it
	mov [Ebulletdirectoin],0
	jmp @@ret
@@retcolided:
	mov [EBulletColided],0
	mov [Ebulletdirectoin],0
@@ret:	
	ret
endp MoveEBulletUDDown

witchETank equ [bp+4];1 is first enemy tank 2 is the secand one
proc Efire;fire the bullet up down left or right by ussing bools of the enamy tank movment
	push bp
	mov bp,sp
	
	mov ax,witchETank
	cmp ax,1
	je @@checkEtank
	jmp @@checkETank2
@@checkEtank:;check first enemy	
	cmp [EbulletFinish],0;if there is one dont do onther
	jne @@checkedDirectoin
	jmp @@ret
@@checkedDirectoin:
	cmp [Eud],1
	jne @@checkRL;if its not up or down
	cmp [Eup],1
	je @@fireUp;if its up
	jmp @@fireDown;if its down
@@checkRL:	

	cmp [Eleft],1;i know its left or right becaus we checked up or down and it wasnt them
	je @@fireLeft;if its left
	jmp @@fireRight;if its right
	
@@fireUp:
	mov [Ebulletdirectoin],1
	mov ax,[XEtank]
	mov [ExbulletUD],ax
	mov ax,[yEtank]
	mov [EYBulletUD],ax
	call putEBulletUD
	call MoveEBulletUDUp
	jmp @@ret
	
@@fireDown:
	
	mov [Ebulletdirectoin],2
	mov ax,[XEtank]
	add ax,3;after checking looks the best
	mov [ExbulletUD],ax
	mov ax,[yEtank]
	add ax,5;half length
	mov [EybulletUD],ax
	call putEBulletUD
	call MoveEBulletUDDown
	jmp @@ret

@@fireRight:
	cmp [EbulletFinish],1;if its in move only move it
	mov [Ebulletdirectoin],3	
	mov ax,[XEtank]
	add ax,15;length
	mov [ExbulletRL],ax
	mov ax,[yEtank]
	mov [EYBulletRL],ax
	call putEBulletRL
	call MoveEBulletRLRight
	jmp @@ret

@@fireLeft:
	cmp [EbulletFinish],1;if its in move only move it
	mov [Ebulletdirectoin],4	
	mov ax,[XEtank]
	add ax,6;half of tank length
	mov [ExbulletRL],ax
	mov ax,[yEtank]
	add ax,5;hight of tank
	mov [EYBulletRL],ax
	call putEBulletRL;put it
	call MoveEBulletRLLeft;move it
	jmp @@ret
	
@@checkETank2:;check secand enemy	
	
	cmp [EbulletFinish2],0;if there is one dont do onther
	jne @@checkedDirectoin2
	jmp @@ret
@@checkedDirectoin2:
	cmp [Eud2],1
	jne @@checkRL2;if its not up or down
	cmp [Eup2],1
	je @@fireUp2;if its up
	jmp @@fireDown2;if its down
@@checkRL2:	

	cmp [Eleft2],1;i know its left or right becaus we checked up or down and it wasnt them
	je @@fireLeft2;if its left
	jmp @@fireRight2;if its right
	
@@fireUp2:
	mov [Ebulletdirectoin2],1
	mov ax,[XEtank2]
	mov [ExbulletUD2],ax
	mov ax,[yEtank2]
	mov [EYBulletUD2],ax
	call putEBulletUD2
	call MoveEBulletUDUp2
	jmp @@ret
	
@@fireDown2:
	
	mov [Ebulletdirectoin2],2
	mov ax,[XEtank2]
	add ax,3;after checking looks the best
	mov [ExbulletUD2],ax
	mov ax,[yEtank2]
	add ax,15;half length
	mov [EybulletUD2],ax
	call putEBulletUD2
	call MoveEBulletUDDown2
	jmp @@ret

@@fireRight2:
	cmp [EbulletFinish2],1;if its in move only move it
	mov [Ebulletdirectoin2],3	
	mov ax,[XEtank2]
	add ax,15;length
	mov [ExbulletRL2],ax
	mov ax,[yEtank2]
	mov [EYBulletRL2],ax
	call putEBulletRL2
	call MoveEBulletRLRight2
	jmp @@ret

@@fireLeft2:
	cmp [EbulletFinish2],1;if its in move only move it
	mov [Ebulletdirectoin2],4	
	mov ax,[XEtank2]
	add ax,6;half of tank length
	mov [ExbulletRL2],ax
	mov ax,[yEtank2]
	add ax,5;hight of tank
	mov [EYBulletRL2],ax
	call putEBulletRL2;put it
	call MoveEBulletRLLeft2;move it
	jmp @@ret
	
@@ret:	

	pop bp
	ret 2
endp Efire

witchETank equ [bp+4];1 is first enemy tank 2 is the secand one
proc MoveEBulletcount;countine enemy bullet movment by bools
	push bp
	mov bp,sp
	
	mov ax,witchETank
	cmp ax,1
	jne @@countETank2Bullet;if its the secand enemy tank
	
	cmp [Ebulletdirectoin],0
	je @@ret
	cmp [Ebulletdirectoin],1
	je @@up
	cmp [Ebulletdirectoin],2
	je @@down
	cmp [Ebulletdirectoin],3
	je @@right
	cmp [Ebulletdirectoin],4
	je @@left
	
@@up:
	call MoveEBulletUDUp
	jmp @@ret

@@down:
	call MoveEBulletUDDown
	jmp @@ret

@@right:
	call MoveEBulletRLRight
	jmp @@ret

@@left:
	call MoveEBulletRLLeft
	jmp @@ret
	
@@countETank2Bullet:
	cmp [Ebulletdirectoin2],0
	je @@ret
	cmp [Ebulletdirectoin2],1
	je @@up2
	cmp [Ebulletdirectoin2],2
	je @@down2
	cmp [Ebulletdirectoin2],3
	je @@right2
	cmp [Ebulletdirectoin2],4
	je @@left2
	
@@up2:
	call MoveEBulletUDUp2
	jmp @@ret

@@down2:
	call MoveEBulletUDDown2
	jmp @@ret

@@right2:
	call MoveEBulletRLRight2
	jmp @@ret

@@left2:
	call MoveEBulletRLLeft2
	jmp @@ret
@@ret:
	pop bp
	ret 2
endp MoveEBulletcount

width1 equ [word bp + 6]
height1 equ [word bp + 4]
proc checkPlayerBulletsHitsUD;check if the bullet of the tank if its up or down hits the enemy tank
	push bp
    mov bp, sp
	mov dl,0
	cmp [Eud],1
	jne @@CollidLR
	
	push width1
	push height1
	push [ybulletUD]
	push [xbulletUD]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	jne @@collidETank2
	call putBulletUD
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	mov [BulletColided],1
	jmp @@collidETank2
	
@@CollidLR:	

	push width1
	push height1
	push [ybulletUD]
	push [xbulletUD]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	jne @@collidETank2
	call putBulletUD
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	mov [BulletColided],1
	
@@collidETank2:
	cmp [Eud2],1
	jne @@CollidLRETank2
	
	push width1
	push height1
	push [ybulletUD]
	push [xbulletUD]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putBulletUD
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	mov [BulletColided],1
	jmp @@ret
	
@@CollidLRETank2:	

	push width1
	push height1
	push [ybulletUD]
	push [xbulletUD]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putBulletUD
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	mov [BulletColided],1
	
@@ret:
	pop bp
	ret 4 
endp checkPlayerBulletsHitsUD

width1 equ [word bp + 6]
height1 equ [word bp + 4]
proc checkPlayerBulletsHitsLR;check if the bullet of the tank if its left or right hits the enemy tank
	push bp
    mov bp, sp
	
	mov dl,0
	cmp [Eud],1
	jne @@CollidLR
	
	push width1
	push height1
	push [ybulletRL]
	push [xbulletRL]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	jne @@collidETank2
	call putBulletRL
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	mov [BulletColided],1
	jmp @@collidETank2
	
@@CollidLR:	

	push width1
	push height1
	push [ybulletRL]
	push [xbulletRL]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	jne @@collidETank2
	call putBulletRL
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	mov [BulletColided],1
	
@@collidETank2:
	cmp [Eud2],1
	jne @@CollidLRETank2
	
	push width1
	push height1
	push [ybulletRL]
	push [xbulletRL]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putBulletRL
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	mov [BulletColided],1
	jmp @@ret
	
@@CollidLRETank2:	

	push width1
	push height1
	push [ybulletRL]
	push [xbulletRL]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putBulletRL
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	mov [BulletColided],1
	
@@ret:
	pop bp
	ret 4 
endp checkPlayerBulletsHitsLR

wichETank equ[bp+8]
width1 equ [word bp + 6]
height1 equ [word bp + 4]
proc checkETankBulletsHitsUD;check if the enemy bullet of the tank if its up or down hits the tank
	push bp
	mov bp,sp
	
	mov ax,wichETank
	cmp ax,1
	je @@collidETank
	jmp @@collidETank2
@@collidETank:	
	cmp [ud],1
	jne @@CollidLR
	
	push width1
	push height1
	push [EybulletUD]
	push [ExbulletUD]
	push TankwidthUD
	push TankHightUD
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colidedUD
	jmp @@ret
@@colidedUD:
	call putEBulletUD
	call gameover
	mov [EBulletColided],1
	jmp @@ret
	
@@CollidLR:	

	push width1
	push height1
	push [EybulletUD]
	push [ExbulletUD]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colidedRL
	jmp @@ret
@@colidedRL:
	call putEBulletUD
	call gameover
	mov [EBulletColided],1
	
@@collidETank2:
	cmp [ud],1
	jne @@CollidLRETank2
	
	push width1
	push height1
	push [EybulletUD2]
	push [ExbulletUD2]
	push TankwidthUD
	push TankHightUD
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putEBulletUD2
	call gameover
	mov [EBulletColided2],1
	jmp @@ret
	
@@CollidLRETank2:	

	push width1
	push height1
	push [EybulletUD2]
	push [ExbulletUD2]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putEBulletUD2
	call gameover
	mov [EBulletColided2],1
	
@@ret:
	
	pop bp
	ret 6
endp checkETankBulletsHitsUD

proc checkETankBulletsHitsLR;check if the enemy bullet of the tank if its left or right hits the tank
	push bp
	mov bp,sp
	
	mov ax,wichETank
	cmp ax,1
	je @@collidETank
	jmp @@collidETank2
@@collidETank:	
	cmp [ud],1
	jne @@CollidLR
	cmp [lr],1
	je @@CollidLR
	
	push width1
	push height1
	push [EybulletRL]
	push [ExbulletRL]
	push TankwidthUD
	push TankHightUD
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colidedUD
	jmp @@ret
@@colidedUD:	
	call putEBulletRL
	call gameover
	mov [EBulletColided],1
	jmp @@ret
	
@@CollidLR:	

	push width1
	push height1
	push [EybulletRL]
	push [ExbulletRL]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colidedRL
	jmp @@ret
@@colidedRL:	
	call putEBulletRL
	call gameover
	mov [EBulletColided],1
	
@@collidETank2:
	cmp [ud],1
	jne @@CollidLRETank2
	
	push width1
	push height1
	push [EybulletRL2]
	push [ExbulletRL2]
	push TankwidthUD
	push TankHightUD
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putEBulletRL2	
	call gameover
	mov [EBulletColided2],1
	jmp @@ret
	
@@CollidLRETank2:	

	push width1
	push height1
	push [EybulletRL2]
	push [ExbulletRL2]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	jne @@ret
	call putEBulletRL2
	call gameover
	mov [EBulletColided2],1
	
@@ret:
	
	pop bp
	ret 6
endp checkETankBulletsHitsLR

proc WhereToMoveEtank2;moves the secand enemy tank by the alg(going to your x and then your y in the y it is shotting you) 
	push ax
	push dx
	mov ax,[YETank2]
	mov dx,[XETank2]
	cmp dx,[xtank]
	je @@checky
	jg @@Etankleft
	jmp @@Etankright
	
@@Etankleft:
	call moveETankleft2
	jmp @@ret

@@Etankright:
	call moveETankright2
	jmp @@ret

@@checky:
	cmp ax,[ytank]
	je @@ret
	jg @@Etankup
	jmp @@Etankdown
	

@@Etankup:	
	call moveETankup2
	mov ax,2
	push ax 
	call Efire
	jmp @@ret
	
@@Etankdown:	
	call moveETankdown2
	mov ax,2
	push ax 
	call Efire
	jmp @@ret
	
@@ret:	
	pop dx
	pop ax
	ret
endp WhereToMoveEtank2

proc BoolToMove2;print the right secand e tank bmp called by PutETankOnScreen
	cmp [Eud2],1
	jne @@checkRL;if its not up or down
	cmp [Eup2],1
	je @@drawUp;if its up
	jmp @@drawDown;if its down
@@checkRL:	

	cmp [Eleft2],1;i know its left or right becaus we checked up or down and it wasnt them
	je @@drawLeft;if its left
	jmp @@drawRight;if its right
	
@@drawUp:

	mov ax,[XETank2]
	push ax
	mov ax,[YETank2]
	push ax
	call drawEtankup
	jmp @@ret	
@@drawDown:	

	mov ax,[XETank2]
	push ax
	mov ax,[YETank2]
	push ax
	call drawEtankdown
	jmp @@ret	
@@drawRight:

	mov ax,[XETank2]
	push ax
	mov ax,[YETank2]
	push ax
	call drawEtankright
	jmp @@ret	
@@drawLeft:

	mov ax,[XETank2]
	push ax
	mov ax,[YETank2]
	push ax
	call drawEtankleft
	jmp @@ret	
@@ret:	

	ret
endp BoolToMove2

proc moveETankup2;moves the secand enemy tank up and checks colisoin and borders
	push ax
@@start:	
	mov ax,[YETank2];check the border
	dec ax
	cmp ax,23
	jae @@count
	jmp @@ret
@@count:	
	
	sub [YEtank2],1
	
	mov ax,2
	push ax;witch etank is this
	push ETankwidthUD
	push ETankHightUD
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [YEtank2],1
	
	
	cmp [Elr2],1
	jne @@cunt
	;call deltank;to delet the all tank only when i switch direction
	
@@cunt:	
	push offset ETankShadow2
	push [XEtank2]
	push [YETank2]
    call RemoveETankFromScreen
	
	mov [Eud2],1;to know that im moving up or down later used in deltank
    mov [Elr2],0
	mov [Eup2],1
	
	sub [YETank2],1
	
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XEtank2]
	push [YETank2]
	call PutETankOnScreen
	
	
	
@@ret:	
	pop ax
	ret 
endp moveETankup2

proc moveETankdown2;moves the secand enemy tank down and checks colisoin and borders
	push ax
@@start:	
	mov ax,[YETank2];check the border
	inc ax
	cmp ax,157
	jle @@count
	jmp @@ret
@@count:	
		
	add [YEtank2],1
	
	mov ax,2
	push ax;witch etank is this
	push ETankwidthUD
	push ETankHightUD
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [YEtank2],1	
		
	cmp [Elr],1
	jne @@cunt
	;call deltank;to delet the all tank only when i switch direction

@@cunt:	
	push offset ETankShadow2
	push [XEtank2]
	push [YETank2]
	
	call RemoveETankFromScreen
	mov [Eud2],1;to know that im moving up or down later used in deltank
    mov [Elr2],0
	mov [Eup2],0
		
	add [YETank2],1
	
	mov ax,2
	push ax
	push offset ETankShadow2
	push [xEtank2]
	push [yETank2]
	call PutETankOnScreen	

	
@@ret:	
	pop ax
	ret 
endp moveETankdown2

proc moveETankleft2;moves the secand enemy tank left and checks colisoin and borders
	push ax
@@start:	
	mov ax,[XETank2];check the border
	dec ax
	cmp ax,23
	jae @@count
	jmp @@ret
@@count:	
	
	sub [XEtank2],1
	
	mov ax,2
	push ax;witch etank is this
	push ETankwidthLR
	push ETankHightLR
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	add [XEtank2],1
	
	cmp [Eud2],1
	jne @@cunt
	;call deltank;to delet the all tank only when i switch direction

@@cunt:	
	push offset ETankShadow2
	push [xEtank2]
	push [yETank2]
	call RemoveETankFromScreen
	mov [Elr2],1
	mov [Eud2],0
	mov [Eleft2],1
	
	sub [XETank2],1
	
	mov ax,2
	push ax
	push offset ETankShadow2
	push [xEtank2]
	push [yETank2]
	call PutETankOnScreen
	
	
@@ret:	
	pop ax
	ret 
endp moveETankleft2

proc moveETankright2;moves the secand enemy tank right and checks colisoin and borders
	push ax
@@start:	
	mov ax,[XETank2];check the border
	inc ax
	cmp ax,279
	jle @@count
	jmp @@ret
@@count:	

	add [XEtank2],1
	
	mov ax,2
	push ax;witch etank is this
	push ETankwidthLR
	push ETankHightLR
	call ETankToPlayerCollid
	cmp [Collid],1
	jne @@noKillTank
	call gameover
	mov [Collid],0
@@noKillTank:	
	sub [XEtank2],1
	
	cmp [Eud2],1
	jne @@cunt
	;call deltank;to delet the all tank only when i switch direction
	

@@cunt:	
	push offset ETankShadow2
	push [xEtank2]
	push [yETank2]
	call RemoveETankFromScreen
	
	mov [Elr2],1
	mov [Eud2],0
	mov [Eleft2],0
	
	add [XETank2],1
	
	mov ax,2
	push ax
	push offset ETankShadow2
	push [xEtank2]
	push [yETank2]
	call PutETankOnScreen

@@ret:	
	pop ax
	ret 
endp moveETankright2
 
proc putEBulletRL2;put the secand enamy bullet on the screen pixel pixel (matrix) left or right	
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[EYBulletRL2]
@@nextR:
	
		mov di,0
		mov cx,[EXBulletRL2]
	@@nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte EbulletRL + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,ElentbulletRL
		jb @@nextC
		inc dx
		inc si
	
	cmp si, EHightbulletRL
	jb @@nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putEBulletRL2

proc  MoveEBulletRLLeft2;moves the secand enemy bullet left also checks colisoin and border
	mov ax,[EXbulletRL2];check the border
	dec ax
	cmp ax,23
	mov[EbulletFinish2],1;to see if its in the end
	jle @@retborder
	
	mov ax,2
	push ax
	push LentbulletRL
	push HightbulletRL
	call checkETankBulletsHitsLR	
	
	cmp [EBulletColided2],0
	jne @@retcolided
	
	mov [EbulletFinish2],0;if its no in the end 
	call putEBulletRL2 ; delete ball at curret locatein
	dec [EXBulletRL2]
	call putEBulletRL2 ; put it  at new location
	jmp @@ret
@@retborder:
	call putEBulletRL2;if border delet it
	mov [Ebulletdirectoin2],0;no direction when there is no bullet	
	jmp @@ret 
@@retcolided:
	mov [EBulletColided2],0
	mov [Ebulletdirectoin2],0
@@ret:	
	ret
endp MoveEBulletRLLeft2

proc  MoveEBulletRLRight2;moves the secand enemy bullet right also checks colisoin and border	
	mov ax,[EXbulletRL2];check the border
	inc ax
	cmp ax,279
	mov[EbulletFinish2],1;to see if its in the end
	jae @@retborder
	
	mov ax,2
	push ax
	push LentbulletRL
	push HightbulletRL
	call checkETankBulletsHitsLR	
	
	cmp [EBulletColided2],0
	jne @@retcolided
	
	mov [EbulletFinish2],0;if its no in the end 
	call putEBulletRL2 ; delete ball at curret locatein
	inc [EXBulletRL2]
	call putEBulletRL2 ; put it  at new location
	jmp @@ret
@@retborder:
	call putEBulletRL2;if border delet it	
	mov [Ebulletdirectoin2],0	
	jmp @@ret 
@@retcolided:
	mov [EBulletColided2],0
	mov [Ebulletdirectoin2],0
@@ret:	
	ret
endp MoveEBulletRLRight2

proc putEBulletUD2;put the secand enemy bullet on the screen pixel pixel (matrix) up or down	
	push si
	push di
	push ax
	push bx
	push cx
	push dx
	
    
	xor si,si
	xor bx,bx
	mov dx,[EYBulletUD2]
@@nextR:
	
		mov di,0
		mov cx,[EXBulletUD2]
	@@nextC:
		 
			mov ah,0dh
			int 10h ;; al got one pixell from screen
			
			xor al,[byte EBulletUD + bx ]
			
			mov ah,0ch	
			int 10h    ; put pixel	
			
		
			inc cx
			inc di
			inc bx

		cmp di,ElentbulletUD
		jb @@nextC
		inc dx
		inc si
	
	cmp si, EHightbulletUD
	jb @@nextR
 
	 
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	


	ret
endp putEBulletUD2
 
proc  MoveEBulletUDUp2;moves the secand enemy bullet up also checks colisoin and border	
	mov ax,[EybulletUD2];check the border
	dec ax
	cmp ax,23
	mov[EbulletFinish2],1;to see if its in the end
	jle @@retborder
	
	mov ax,2
	push ax
	push LentbulletUD
	push HightbulletUD
	call checkETankBulletsHitsUD
	
	cmp [EBulletColided2],0
	jne @@retcolided
	
	mov [EbulletFinish2],0;if its no in the end 
	call putEBulletUD2 ; delete ball at curret locatein
	dec [EybulletUD2]
	call putEBulletUD2 ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putEBulletUD2;if its in the border delet it
	mov [Ebulletdirectoin2],0
	jmp @@ret
@@retcolided:
	mov [EBulletColided2],0
	mov [Ebulletdirectoin2],0
@@ret:	
	ret
endp MoveEBulletUDUp2

proc  MoveEBulletUDDown2;moves the secand enemy bullet down also checks colisoin and border	
	mov ax,[EybulletUD2];check the border
	inc ax
	cmp ax,157
	mov[EbulletFinish2],1;to see if its in the end
	jae @@retborder
	
	mov ax,2
	push ax
	push LentbulletUD
	push HightbulletUD
	call checkETankBulletsHitsUD
	
	cmp [EBulletColided2],0
	jne @@retcolided
	
	mov [EbulletFinish2],0;if its no in the end 
	call putEBulletUD2 ; delete ball at curret locatein
	inc [EybulletUD2]
	call putEBulletUD2 ; put it  at new location
	jmp @@ret
	
@@retborder:
	call putEBulletUD2;if its in the border delet it
	mov [Ebulletdirectoin2],0
	jmp @@ret
@@retcolided:
	mov [EBulletColided2],0
	mov [Ebulletdirectoin2],0
@@ret:	
	ret
endp MoveEBulletUDDown2

witchETank equ [bp+10];1 is first enemy tank 2 is the secand one
shadowoffset equ [bp+8]
EtankX equ [bp + 6]
EtankY equ [bp + 4]
proc killETank;kills the enemy tank by RemoveETankFromScreen and make the shdow black agian
	push bp
	mov bp,sp
	
	push shadowoffset
	push EtankX
	push EtankY
	call RemoveETankFromScreen
	
	mov ax,witchETank
	cmp ax,1
	jne @@killETank2
	mov [XETank],ETankSpwanX
	mov [YETank],ETankSpwanY
	mov cx,256;16*16
	mov si,0
@@shadow:
	mov [byte ETankShadow + si],0
	inc si
	loop @@shadow
	jmp @@ret
@@killETank2:
	mov [XETank2],ETankSpwanX2
	mov [YETank2],ETankSpwanY2	
	mov cx,256;16*16
	mov si,0
@@shadow2:
	mov [byte ETankShadow2 + si],0
	inc si
	loop @@shadow2
@@ret:
	pop bp
	ret 8
endp killETank
 
width1 equ [bp + 18]
height1 equ [bp + 16]
y1 equ [bp + 14]
x1 equ [bp + 12]
width2 equ [bp + 10]
height2 equ [bp + 8]
y2 equ [bp + 6]
x2 equ [bp + 4]
proc HasCollided;returns in dl 1 if etank and tank touch, if not dl has 0

    push bp
    mov bp, sp

    push ax
    push bx
    push cx

    ; check for collision on X axis
    mov ax, x2
    add ax, width2
    cmp x1, ax ; is x1 to the right of 2's right edge?
    ja @@DontCollide  ; if yes, then no collision

    mov ax, x1
    add ax, width1
    cmp x2, ax ; is x2 to the right of 1's right edge?
    ja @@DontCollide  ; if yes, then no collision

    ; check for collision on Y axis
    mov ax, y2
    add ax, height2
    cmp y1, ax ; is y1 below 2's bottom edge?
    ja @@DontCollide  ; if yes, then no collision

    mov ax, y1
    add ax, height1
    cmp y2, ax ; is y2 above 1's top edge?
    ja @@DontCollide  ; if yes, then no collision

;Touched
    mov dl, 1
    jmp @@ret

@@DontCollide:
    mov dl, 0

@@ret:
    pop cx
    pop bx
    pop ax
    pop bp
    ret 16

endp HasCollided

width1 equ [word bp + 6]
height1 equ [word bp + 4]
proc playerToETankCollid;check if player and e tanks cillided using HasCollided and bools
	push bp
    mov bp, sp

	mov dl,0
	cmp [Eud],1
	jne @@CollidLR
	
	push width1
	push height1
	push [Ytank]
	push [Xtank]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	je @@count
	jmp @@collidETank2
@@count:	
	cmp[redmode],1
	jne @@notredmode
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	jmp @@collidETank2
@@notredmode:	
	mov [Collid],1
	jmp @@collidETank2
	
@@CollidLR:	

	push width1
	push height1
	push [Ytank]
	push [Xtank]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank]
	push [XEtank]
	call HasCollided
	cmp dl,1
	jne @@collidETank2
	cmp[redmode],1
	jne @@notredmode2
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	jmp @@collidETank2
@@notredmode2:
	mov [Collid],1
	
@@collidETank2:
	cmp [Eud2],1
	jne @@CollidLRETank2
	
	push width1
	push height1
	push [Ytank]
	push [Xtank]
	push ETankwidthUD
	push ETankHightUD
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	je @@count1
	jmp @@ret
@@count1:	
	cmp[redmode],1
	jne @@notredmode3
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	jmp @@ret
@@notredmode3:
	mov [Collid],1
	jmp @@ret
	
@@CollidLRETank2:	

	push width1
	push height1
	push [Ytank]
	push [Xtank]
	push ETankwidthLR
	push ETankHightLR
	push [YEtank2]
	push [XEtank2]
	call HasCollided
	cmp dl,1
	jne @@ret
	cmp[redmode],1
	jne @@notredmode4
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	jmp @@ret
@@notredmode4:
	mov [Collid],1
	
@@ret:
	pop bp
	ret 4
endp playerToETankCollid

wichETank equ [word bp+8]
width1 equ [word bp + 6]
height1 equ [word bp + 4]
proc ETankToPlayerCollid;check if e tanks and player cillided using HasCollided and bools
	push bp
    mov bp, sp
	
	mov ax,wichETank
	cmp ax,1
	je @@count
	jmp @@collidETank2
@@count:	
	cmp [ud],1
	jne @@CollidLR
	
	push width1
	push height1
	push [YEtank]
	push [XEtank]
    push TankwidthUD
    push TankHightUD
    push [Ytank]
    push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colided
	jmp @@ret
@@colided:	
	cmp [redmode],1
	jne @@notredmode
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	jmp @@ret
@@notredmode:	
	mov [Collid],1
	
	
@@CollidLR:	

	
	push width1
	push height1
	push [YEtank]
	push [XEtank]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@colided1
	jmp @@ret
@@colided1:	
	cmp[redmode],1
	jne @@notredmode2
	mov ax,1
	push ax
	push offset ETankShadow
	push [XETank]
	push [YETank]
	call killETank
	jmp @@ret
@@notredmode2:	
	mov [Collid],1
	
@@collidETank2:
	cmp [Eud2],1
	jne @@CollidLRETank2
	
	
	push width1
	push height1
	push [YEtank2]
	push [XEtank2]
	push TankwidthUD
	push TankHightUD
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	je @@count1
	jmp @@ret
@@count1:
	cmp[redmode],1
	jne @@notredmode3
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	jmp @@ret
@@notredmode3:	
	mov [Collid],1
	jmp @@ret
	
@@CollidLRETank2:	

	
	push width1
	push height1
	push [YEtank2]
	push [XEtank2]
	push TankwidthLR
	push TankHightLR
	push [Ytank]
	push [Xtank]
	call HasCollided
	cmp dl,1
	jne @@ret
	cmp[redmode],1
	jne @@notredmode4
	mov ax,2
	push ax
	push offset ETankShadow2
	push [XETank2]
	push [YETank2]
	call killETank
	jmp @@ret
@@notredmode4:	
	mov [Collid],1	
@@ret:
	pop bp
	ret 6
endp ETankToPlayerCollid

proc gameover;decres the lives and put the tank back in the spwan and if zero lives the game is ending
	cmp [lives],0
	je @@endgame
	call deltank
	mov [Xtank],TankSpwanX;cord(מיקום) of the tank in the start
	mov [Ytank],TankSpwanY;cord(מיקום) of the tank in the start changed it abit from 154
	mov [ud],1
    mov [lr],0
	mov [up],1
	call drawtankup
	dec [lives]
	call delLives
	jmp @@ret
	
@@endgame:	
	mov [endgame],1
@@ret:	
	ret
endp gameover

proc str_to_int 
	push bx
	push cx
	push dx
	push si
	push di
	
	mov si, 0  ; num of digits
	mov di,10
	xor ax, ax
	
@@NextDigit:
    mov bl, [IntStr + si]   ; read next ascii byte
	cmp bl,0dh  ; stop condition LF
	je @@ret
	cmp bl,13  ; or 13  CR
	je @@ret
	
	mul di
	
	mov bh ,0
	
	sub bl, '0'
	add ax , bx

	inc si
	cmp si, 5     ;one more stop condition
	jb @@NextDigit
	 
@@ret:
	; ax contains the number 	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	
	ret
endp str_to_int 

;================================================
; Description - Write to IntStr the num inside ax and put 13 10 after 
;			 
; INPUT: AX
; OUTPUT: IntStr 
; Register Usage: AX  
;================================================
proc int_to_str;makes the number a string
	   push ax
	   push bx
	   push cx
	   push dx
	   
	   xor si,si
	   mov cx,0   ; will count how many time we did push 
	   mov bx,10  ; the divider
   
put_mode_to_stackA:
	   xor dx,dx
	   div bx
	   add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
	   push dx    
	   inc cx
	   cmp ax,9   ; check if it is the last time to div
	   jg put_mode_to_stackA

	   cmp ax,0
	   jz pop_nextA  ; jump if ax was totally 0
	   add al,30h  
	   xor si, si
	   mov [IntStr], al
	   inc si
	   
		   
pop_nextA: 
	   pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   
	   mov [IntStr + si], al
	   inc si
	   loop pop_nextA
		
	   mov [IntStr + si], 13
	   mov [IntStr + si +1 ], 10
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp int_to_str

proc file_open_no_create;open file
	mov ah,3dh
	mov al,2	
	mov dx, offset filescore
	int 21h
	jc  openError   
	mov [FileHandle],ax
	mov	[FileFound],1
	jmp endopen
openError:
	mov [FatalError],1
	jmp endopen
endopen:
	ret
endp file_open_no_create

proc file_create;crate a file and open it
	cmp [FatalError],0
	jz endcreate
	mov ah,3ch
	mov al,0
	mov dx, offset filescore	
	mov cx,0
	int 21h
	jc  createError   
	mov [FileHandle],ax
	mov	[FileCreated],1
	mov [FatalError],0
	jmp endcreate
createError:
	mov [FatalError],1
	jmp endcreate
endcreate:
	call file_close
	ret
endp file_create

proc file_open_to_write;open file to write to it
	mov ah,3dh
	mov al,2
	mov dx, offset filescore	
	int 21h
	jc  openwError   
	mov [FileHandle],ax
	mov	[FileFound],1
	jmp endopen
openwError:
	mov [FatalError],1
	jmp endwopen
endwopen:
	ret
endp file_open_to_write

proc write_file_score;write the score to the file
	mov ah,40h
	mov al,0	
	mov cx,5
	mov bx,[FileHandle]
	mov dx,offset IntStr
	int 21h
	ret
endp write_file_score

proc file_close;colse the File

	mov ah,3eh
	mov bx,[FileHandle]
	int 21h
	ret 
endp file_close 

proc read_file;reads the File
	mov bx,[FileHandle]
	mov dx,offset ReadBuff
	mov cx,BLOCK_SIZE  
	mov ah,3Fh
	mov al,0
	int 21h 
	ret
endp read_file

proc getscore;gets the score

	call file_open_no_create
	cmp [FatalError],0
		jnz create1
		jmp nocreate1
create1:
		call file_create
		call file_open_no_create
		jmp Nothing1
nocreate1:
    call read_file
    call file_close	
	xor si,si
continueread:
	mov al,[ReadBuff + si]
	mov [IntStr+si],al
	cmp [ReadBuff + si+1],0Dh
	je EndRead
	cmp [ReadBuff + si+1],0Ah
	je EndRead
	inc si
	jmp continueread

EndRead:
	mov [IntStr+si+1],0Dh
	mov [IntStr+si+2],0Ah

	ret

Nothing1:
	mov [IntStr],0
	jmp EndRead	
endp getscore

proc PrintbestScore;prints the best score and your score

    call getscore;return score in intstr to str_to_int
	
	call str_to_int ;return score in ax
	mov [BestScore],ax
	call SaveScore
	jmp @@ret
 @@ErrorFile:
    ; Handle error in file operations
    mov dx, offset BmpFileErrorMsg
    mov ah, 09h
    int 21h
@@ret:	 
	ret
endp PrintbestScore 

proc SaveScore;saves the score to the file if needed

    cmp [Score], ax
    jle @@KeepHighScore

    mov ax,[score]
	mov [BestScore], ax
    call int_to_str
    call file_open_to_write
    jc @@ErrorFile

    call write_file_score
    call file_close

    jmp @@DisplayHighScore

@@KeepHighScore:
    ; Otherwise, keep the existing high score
    mov [BestScore], ax
	jmp @@DisplayHighScore

@@DisplayHighScore:
	
    ; Position cursor
    mov ah, 02h
    xor bh, bh
    mov dh, 17
    mov dl, 12
	add dl,lenYourScore
    int 10h
	
	call yourscore
	mov ax,[score]
	call ShowAxDecimal
	
	mov ah, 02h
    xor bh, bh
    mov dh, 20
    mov dl, 12
	add dl,lenBestScore
    int 10h
	call highscoreStr
	mov ax,[BestScore]
	call ShowAxDecimal
	
    jmp @@Done

@@ErrorFile:
    ; Handle error in file operations
    mov dx, offset BmpFileErrorMsg
    mov ah, 09h
    int 21h

@@Done:
	ret

endp SaveScore

proc yourscore;print the string your score
	mov ax,seg YourScoreStr;print super tank score
	mov es,ax
	mov ah,13h
	mov bp,offset YourScoreStr
	mov dh,17
	mov dl,12
	mov cx,lenYourScore
	mov al,1
	xor bh,bh
	mov bl,1 
	int 10h
	ret
endp yourscore

proc highscoreStr;print the string highscore
	mov ax,seg BestScorestr;print super tank score
	mov es,ax
	mov ah,13h
	mov bp,offset BestScorestr
	mov dh,20
	mov dl,12
	mov cx,lenBestScore
	mov al,1
	xor bh,bh
	mov bl,1 
	int 10h
	ret 
endp highscoreStr

proc Shoot;make the shoot sound

    mov cx,2
again1:
	mov dx,[HowManyTimes]
	push cx

	mov     bx,1             ; frequency value.
	 
	mov     al, 10110110b    ; 10110110b the magic number (use this binary number only)
	out     43h, al          ; init port 43h timer 2.
	 
next_frequency:               ; Dx times.
	 
	mov     ax, bx           
	 
	out     42h, al          ; lsb to port 42h.
	mov     al, ah            
	out     42h, al          ; send msb to port 42h.
	 
	in      al, 61h          ; get the value of port 61h.
	or      al, 00000011b    ; or al to this value, forcing first two bits high.
	out     61h, al          ; to turn on the speaker.
	 
	mov     cx, 100          ; just delay (bad delay)
delay_loop1:        
	loop    delay_loop1       

	inc     bx               ; inc requency 
	dec     dx          
	cmp     dx, 0          
	jnz     next_frequency  
	 
	in      al,61h           
	and     al,11111100b       ; turn speaker off
	out     61h,al           
	
	pop cx
	loop again1
	
	ret
endp Shoot

proc NextLevel;make all the changes back to the start 
	cmp [scoreLevel],3500
	je @@count
	jmp @@ret
@@count:	
	mov [scoreLevel],0
	
	mov dx,offset FileNextLevel
	mov [BmpTop],0
	mov [BmpLeft],0
	mov[BmpHeight],200
	mov[BmpWidth],320
	call OpenShowBmp
	
	mov cx,0ffffh
	mov dx,10h
@@bigbigloop:	
@@loop:	
	loop @@loop
	dec dx
	cmp dx,0
	jne @@bigbigloop
	
	mov dx, offset FileStart;show the start picture
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpWidth],320
	mov [BmpHeight],200
	call OpenShowBmp
	
	call drawmap;print map
	mov [Xtank],TankSpwanX;cord(מיקום) of the tank in the start
	mov [Ytank],TankSpwanY;cord(מיקום) of the tank in the start changed it abit from 154
	call moveup;print the tank looking up
	call printScore;print the str score: 
	call addScore;print the number of score
	call printLives
		
	mov [XETank],ETankSpwanX;cord(מיקום) of the enemy tank in the start
	mov [YETank],ETankSpwanY;cord(מיקום) of the enemy tank in the start
	
	mov ax,1;tell PutETankOnScreen its the first enemy, parmter of PutETankOnScreen
	push ax
	push offset ETankShadow ;parmter of PutETankOnScreen
	push [XEtank] ;parmter of PutETankOnScreen
	push [YETank] ;parmter of PutETankOnScreen
	call PutETankOnScreen
	call WhereToMoveEtank ;make the first enamy tank move towars the player
	
	
	mov [XETank2],ETankSpwanX2;cord(מיקום) of the enemy 2 tank in the start
	mov [YETank2],ETankSpwanY2;cord(מיקום) of the enemy 2 tank in the start
	
	mov ax,2 ;tell PutETankOnScreen its the secand enemy, parmter of PutETankOnScreen
	push ax
	push offset ETankShadow2;parmter of PutETankOnScreen
	push [XEtank2];parmter of PutETankOnScreen
	push [YETank2];parmter of PutETankOnScreen
	call PutETankOnScreen
	call WhereToMoveEtank2;make the first move of enemy 2 towards the player
	call deltank
@@ret:	
	ret
endp NextLevel

END start


