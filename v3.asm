; multi-segment executable file template.
PUTC    MACRO   char
        PUSH    AX
        MOV     AL, char
        MOV     AH, 0Eh
        INT     10h
        POP     AX
ENDM
data segment
    chaine_de_debut db "Calculatrice developpe par ***NASSIM TOUAT ET ANES MEZDOUD***  $"
    pkey db "Appuie sur 0 pour quitter...$"
    choix_baseB db "tapez b pour le binaire $"
    choix_baseH db "tapez h pour le hexadecimale $"
    choix_baseD db "tapez d pour le decimale $"
    msg_op1 db "inserez l'operande 1$"
    msg_op2 db "inserez l'operande 2$"
    choix_op db "tapez +: pour l'addition",0Dh,0Ah,"tapez -: pour la soustraction",0Dh,0Ah,"tapez *: pour la multiplication",0Dh,0Ah,"tapez /: pour la division$"
    sautligne  db 0Dh,0Ah,'$'
    opBinaire1 db 19 dup(?)
    opBinaire2 db 19 dup(?)
    BinaireResu db 19 dup(?)
    BinaireReste db 19 dup(?)
    op1Decimale db 8 dup(?)
    op2Decimale db 8 dup(?)
    op1HEXA db 7 db dup (?) 
    op2HEXA db 7 db dup (?)
    HEXAResu db 7 db dup (?)
    HEXARest db 7 db dup (?)  
    msgop1 db 'veuillez inserez la valeur de op1 $'
    msgop2 db 'veuillez inserez la valeur de op2 $'
    operation db 0
    base db 0
    op1 dw 0
    op2 dw 0
    opresu dw 0
    oprest dw 0
    messageErreur db 'veuillez inserez une valeur correcte$'
    MSG_RST DB 'LE RESTE EST:$'
    
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
; debut du sah
   
   ; affichage du la chaine de debut+ saut de ligne
   lea dx, chaine_de_debut
   mov ah,09h
   int 21h
   
  
   lea dx,sautligne
   mov ah,09h
   int 21h
    remettrebase:
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   ; affichage chaine de caractere choix de base
   lea dx,choix_baseB
   mov ah,09h
   int 21h
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   lea dx,choix_baseH
   mov ah,09h
   int 21h
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   lea dx,choix_baseD
   mov ah,09h
   int 21h
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   ; lecture du caractere
    mov ah, 1
    int 21h
   ; copie du caractere
   mov base,al
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   call clear_screen
   ; determinons le caractere saisie
   cmp base,"b"
   jne teste2
   
    ;partie binaire
 `   ;inisialisations des chaines
    mov opBinaire1,17
    mov opBinaire2,17
    mov BinaireResu,17
    mov BinaireReste,17
   ;lecture op
   
JMP FREMETTREOP1B
     
    REMETTREOP1B:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP1B:
   
    
    lea dx,msgop1
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset opBinaire1 
    call lireBinaire
    add sp,2
    
    lea dx,opBinaire1
    push dx
    call EST_BINAIRE
    POP AX
    
    CMP AX,0
    Je REMETTREOP1B
    
    LEA BX,opBinaire1
    MOV SI,0
    MOV AX,0
    MOV AL,[BX+1]
    ADD SI,AX
    ADD SI,2
    MOV [BX+SI],'$'
     
    
    PUSH OFFSET opBinaire1
    CALL STRING_BINAIRE
    POP OP1
    
    JMP FREMETTREOP2B
     
    REMETTREOP2B:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP2B:
   
    
    lea dx,msgop2
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset opBinaire2 
    call lireBinaire
    add sp,2
    
    lea dx,opBinaire2
    push dx
    call EST_BINAIRE
    POP AX
    
    CMP AX,0
    Je REMETTREOP2B
    
    LEA BX,opBinaire2
    MOV SI,0
    MOV AX,0
    MOV AL,[BX+1]
    ADD SI,AX
    ADD SI,2
    MOV [BX+SI],'$'
    
    
    
    
    PUSH OFFSET opBinaire2
    CALL STRING_BINAIRE
    POP OP2
    
   
    
    JMP FERREUROPB
 ERREUROPB:
 CALL CLEAR_SCREEN 
 
 LEA DX,messageErreur
 MOV AH,09H
 INT 21H
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 FERREUROPB:
 
 LEA DX,choix_op 
 MOV AH,09H
 INT 21H
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 MOV AH,1
 INT 21h
 
 
 
 MOV OPERATION,AL 
 
 CALL CLEAR_SCREEN
 
 CMP OPERATION,'+'
 JNE SOUSTRACTIONB 
 
 PUSH OP1
 PUSH OP2
 CALL ADDITION
 POP OPRESU
 POP OPRESU
 
 LEA DX,BinaireResu
 PUSH DX
 PUSH OPRESU
 CALL binaire_string
 ADD SP,4
 
 LEA DX,opBinaire1
 PUSH DX
 
 MOV DX,0
 MOV DL,OPERATION
 PUSH DX
 
 LEA DX,opBinaire2
 PUSH DX
 
 LEA DX,BinaireResu
 PUSH DX
 
 CALL AFFICHAGEB
 ADD SP,8
 
 
 JMP FP
 SOUSTRACTIONB:
 CMP OPERATION,'-'
 JNE MULTIPLICATIONB
 
 PUSH OP2
 PUSH OP1
 CALL SOUSTRACTION
 POP OPRESU
 POP OPRESU
 
 LEA DX,BinaireResu
 PUSH DX
 PUSH OPRESU
 CALL binaire_string
 ADD SP,4
 
 LEA DX,opBinaire1
 PUSH DX
 
 MOV DX,0
 MOV DL,OPERATION
 PUSH DX
 
 LEA DX,opBinaire2
 PUSH DX
 
 LEA DX,BinaireResu
 PUSH DX
 
 CALL AFFICHAGEB
 ADD SP,8
 
 
 
 JMP FP
 MULTIPLICATIONB:
 CMP OPERATION,'*'
 JNE DIVISIONB
 
     
PUSH OP1
PUSH OP2

CALL MUXD

POP OPREST
POP OPRESU 

LEA DX,BINAIRERESU
PUSH DX
PUSH OPREST
CALL BINAIRE_STRING

ADD SP,4

LEA DX,BINAIRERESTE
PUSH DX
PUSH OPRESU
CALL BINAIRE_STRING

ADD SP,4 

LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H

LEA DX,OPBINAIRE1
ADD DX,2
MOV AH,09H
INT 21H

 
     MOV AL,'*'
     MOV AH,0EH
     INT 10H
     
LEA DX,OPBINAIRE2
ADD DX,2
MOV AH,09H
INT 21H

 
        MOV AL,'='
        MOV AH,0EH
        INT 10H
        
LEA DX,BINAIRERESU
ADD DX,2
MOV AH,09H
INT 21H

LEA DX,BINAIRERESTE
ADD DX,2
MOV AH,09H
INT 21H
     
LEA DX,SAUTLIGNE
MOV AH,09H
INT 21H   
     
     
 
 JMP FP
 DIVISIONB:
 CMP OPERATION,'/'
 JNE ERREUROPB
 
 CMP OP2,0
 JE REMETTREOP2B
              
   
PUSH OP1
PUSH OP2

CALL DIVISION

POP OPRESU
POP OPREST 

LEA DX,BINAIRERESU
PUSH DX
PUSH OPRESU
CALL BINAIRE_STRING

ADD SP,4

LEA DX,BINAIRERESTE
PUSH DX
PUSH OPREST
CALL BINAIRE_STRING

ADD SP,4 

LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H

LEA DX,OPBINAIRE1
ADD DX,2
MOV AH,09H
INT 21H

 
     MOV AL,'/'
     MOV AH,0EH
     INT 10H
     
LEA DX,OPBINAIRE2
ADD DX,2
MOV AH,09H
INT 21H

 
        MOV AL,'='
        MOV AH,0EH
        INT 10H
        
LEA DX,BINAIRERESU
ADD DX,2
MOV AH,09H
INT 21H

LEA DX,SAUTLIGNE
MOV AH,09H
INT 21H 

LEA DX,MSG_RST
MOV AH,09H
INT 21H

LEA DX,BINAIRERESTE
ADD DX,2
MOV AH,09H
INT 21H
     
LEA DX,SAUTLIGNE
MOV AH,09H
INT 21H 
      
   
    
           
  JMP FP ;fin partie
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;hexa;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
   teste2:
   cmp base,"h"
   jne teste3 
    ;partie hexa 
    mov op1HEXA,5
    mov op2HEXA,5
    mov HEXAResu,5
    mov HEXARest,5
   ;lecture op
   
JMP FREMETTREOP1H
     
    REMETTREOP1H:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP1H:
   
    
    lea dx,msgop1
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset op1HEXA 
    call lirehexa
    add sp,2
    
    lea dx,op1HEXA
    push dx
    call EST_HEXADECIMALE
    POP AX
    
    CMP AX,0
    Je REMETTREOP1H
    
    LEA BX,op1HEXA
    MOV SI,0
    MOV AX,0
    MOV AL,[BX+1]
    ADD SI,AX
    ADD SI,2
    MOV [BX+SI],'$'
     
    
    PUSH OFFSET op1HEXA 
    CALL String_Hex
    POP OP1
    
    JMP FREMETTREOP2H
     
    REMETTREOP2H:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP2H:
   
    
    lea dx,msgop2
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset op2HEXA 
    call lirehexa
    add sp,2
    
    lea dx,op2HEXA
    push dx  
    call EST_HEXADECIMALE
    POP AX
    
    CMP AX,0
    Je REMETTREOP2H
    
    LEA BX,op2HEXA
    MOV SI,0
    MOV AX,0
    MOV AL,[BX+1]
    ADD SI,AX
    ADD SI,2
    MOV [BX+SI],'$'
    
    
    
    
    PUSH OFFSET op2HEXA
    CALL String_Hex
    POP OP2                 
    
   
    
    JMP FERREUROPH
 ERREUROPH:
 CALL CLEAR_SCREEN 
 
 LEA DX,messageErreur
 MOV AH,09H
 INT 21H
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 FERREUROPH:
 
 LEA DX,choix_op 
 MOV AH,09H
 INT 21H
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 MOV AH,1
 INT 21h
 
 
 
 MOV OPERATION,AL 
 
 CALL CLEAR_SCREEN
 
 CMP OPERATION,'+'
 JNE SOUSTRACTIONH 
 
 PUSH OP1
 PUSH OP2
 CALL ADDITION
 POP OPRESU
 POP OPRESU
 
 PUSH OP1
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'+'
 MOV AH,0EH
 INT 10H
 
 PUSH OP2
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'='
 MOV AH,0EH
 INT 10H
 
  PUSH OPRESU
 CALL Print_Hexa
 ADD SP,2
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H 
 
 ;;;;METTRE L'AFFICHAGE
 
 
 
 JMP FP
 SOUSTRACTIONH:
 CMP OPERATION,'-'
 JNE MULTIPLICATIONH
 
  PUSH OP2
 PUSH OP1
 CALL SOUSTRACTION
 POP OPRESU
 POP OPRESU
 
  
 PUSH OP1
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'-'
 MOV AH,0EH
 INT 10H
 
 PUSH OP2
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'='
 MOV AH,0EH
 INT 10H
 
  PUSH OPRESU
 CALL Print_Hexa
 ADD SP,2
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 ;;;METTRE L'AFFICHAGE
 
 
 
 JMP FP
 MULTIPLICATIONH:
 CMP OPERATION,'*'
 JNE DIVISIONH
 
 PUSH OP1
PUSH OP2

CALL MUXD

POP OPREST
POP OPRESU 

 
 PUSH OP1
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'*'
 MOV AH,0EH
 INT 10H
 
 PUSH OP2
 CALL Print_Hexa
 ADD SP,2
 
          
 MOV AL,'='
 MOV AH,0EH
 INT 10H
 
  PUSH OPREST
 CALL Print_Hexa
 ADD SP,2
 
  PUSH OPRESU
 CALL Print_Hexa
 ADD SP,2
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H


;;AFFICHAGE    

     
     
 
 JMP FP
 DIVISIONH:
 CMP OPERATION,'/'
 JNE ERREUROPH
 
 
  CMP OP2,0
 JE REMETTREOP2H
              
    
PUSH OP1
PUSH OP2

CALL DIVISION

POP OPRESU
POP OPREST 

;;AFFICHAGE
           




PUSH OP1        
CALL PRINT_HEXA
ADD SP,2


MOV AL,'/'
MOV AH,0EH
INT 10H


PUSH OP2        
CALL PRINT_HEXA
ADD SP,2





MOV AL,'='
MOV AH,0EH
INT 10H


PUSH OPRESU        
CALL PRINT_HEXA
ADD SP,2


LEA DX,SAUTLIGNE
MOV AH,09H
INT 21H

LEA DX,MSG_RST
MOV AH,09H
INT 21H


PUSH OPREST        
CALL PRINT_HEXA
ADD SP,2


LEA DX,SAUTLIGNE
MOV AH,09H
INT 21H     
   
    
           
  JMP FP ;fin partie
    
                 
                 
          
          
                 
      
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;decimale;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   teste3:
   cmp base,"d"
   jne erreur
   ;partie decimale 
   
    
    mov op1Decimale,6
    mov op2Decimale,6
    
    JMP FREMETTREOP1D
     
    REMETTREOP1D:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP1D:
   
    
    lea dx,msgop1
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset op1Decimale 
    call lireDecimale
    add sp,2
    
    lea dx,op1Decimale
    push dx
    call EST_DECIMALE
    POP AX
    
    CMP AX,0
    Je REMETTREOP1D
    
    
    PUSH OFFSET op1Decimale
    CALL STRING_DECIMALE
    POP OP1
    
    JMP FREMETTREOP2D
     
    REMETTREOP2D:
   CALL CLEAR_SCREEN
    
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   
   FREMETTREOP2D:
   
    
    lea dx,msgop2
    mov ah,09h
    int 21h
   
    lea dx,sautligne
    mov ah,09h
    int 21h
    
    push offset op2Decimale 
    call lireDecimale
    add sp,2
    
    lea dx,op2Decimale
    push dx
    call EST_DECIMALE
    POP AX
    
    CMP AX,0
    Je REMETTREOP2D
    
    
    PUSH OFFSET op2Decimale
    CALL STRING_DECIMALE
    POP OP2
    
    JMP FERREUROPD
 ERREUROPD:
 CALL CLEAR_SCREEN 
 
 LEA DX,messageErreur
 MOV AH,09H
 INT 21H
  LEA DX,sautligne
 MOV AH,09H
 INT 21H
 
 
 
 FERREUROPD:
 
 LEA DX,choix_op 
 MOV AH,09H
 INT 21H
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 
 MOV AH,1
 INT 21h
 
 MOV OPERATION,AL 
 
 CALL CLEAR_SCREEN
 
 CMP OPERATION,'+'
 JNE SOUSTRACTIOND
 
 PUSH OP1
 PUSH OP2
 CALL ADDITION
 POP OPRESU
 POP OPRESU
 
 PUSH OP1
 MOV AX,0
 MOV AL,OPERATION
 PUSH AX
 PUSH OP2
 PUSH OPRESU
 CALL AFFICHAGED
 ADD SP,8
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H

 
 JMP FP
 SOUSTRACTIOND:
 CMP OPERATION,'-'
 JNE MULTIPLICATIOND
 
 PUSH OP2
 PUSH OP1
 CALL SOUSTRACTION
 POP OPRESU
 POP OPRESU
 
 PUSH OP1
 MOV AX,0
 MOV AL,OPERATION
 PUSH AX
 PUSH OP2
 PUSH OPRESU
 CALL AFFICHAGED
 ADD SP,8
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
               
 JMP FP              
 MULTIPLICATIOND:
 CMP OPERATION,'*'
 JNE DIVISIOND
 
 PUSH OP1
 PUSH OP2
 CALL MUXD
 POP OPREST
 POP OPRESU
 
 PUSH OP1
 PUSH OP2
 PUSH OPRESU
 PUSH OPREST
 CALL AFFICHAGEDM
 ADD SP,8
 
  
 JMP FP 
 DIVISIOND:
 CMP OPERATION,'/'
 JNE ERREUROPD
 
  CMP OP2,0
 JE REMETTREOP2D
 
 PUSH OP1
 PUSH OP2
 CALL DIVISION
 POP OPRESU
 POP OPREST
 
 MOV AX,OP1
 CALL PRINT_NUM_UNS
 
 MOV AL,'/'
 MOV AH,0EH
 INT 10H
 
 MOV AX,OP2
 CALL PRINT_NUM_UNS
 
 MOV AL,'='
 MOV AH,0EH
 INT 10H 
 
 MOV AX,OPRESU
 CALL PRINT_NUM_UNS
 
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H
 
 LEA DX,MSG_RST
 MOV AH,09H
 INT 21H
 
 MOV AX,OPREST
 CALL PRINT_NUM_UNS   
    
 LEA DX,SAUTLIGNE
 MOV AH,09H
 INT 21H      
   
   
   jmp fp
   
   erreur:
   call clear_screen
   
   lea dx,messageErreur
   mov ah,09h
   int 21h   
   
   lea dx,sautligne
   mov ah,09h
   int 21h
   jmp remettrebase
   
   
   fp:    
; fin du programe         
    lea dx, pkey
    mov ah, 09h
    int 21h       
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    CMP AL,'0'
    JE FINP
    
    
    CALL CLEAR_SCREEN
    
    JMP start 
    
    FINP:
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    


;debut espace fonction
            Lirebinaire proc
        push ax
        push dx
        push bp
        mov bp,sp
        mov dx,[bp+8] 
        mov ah,0Ah
        int 21h
        lea dx,sautligne
        mov ah,09h
        int 21h
        pop bp
        pop dx
        pop ax
        ret
            endp
             liredecimale proc
                   
        push ax
        push dx
        push bp
        mov bp,sp
        mov dx,[bp+8]
        mov ah,0Ah
        int 21h
        lea dx,sautligne
        mov ah,09h
        int 21h
        pop bp
        pop dx
        pop ax
        ret
             endp
           lirehexa proc
        push ax
        push dx
        push bp
        mov bp,sp
        mov dx,[bp+8]
        mov ah,0Ah
        int 21h 
        lea dx,sautligne
        mov ah,09h
        int 21h
        pop bp
        pop dx
        pop ax
        ret
           endp 
           
           
    clear_screen proc
    
    push cx
    push ax
    push dx
    push bx
    
  mov ah, 02h   ; Set AH to 02h to call the set cursor position function
  mov bh, 0     ; Set BH to 0 to select the page number
  mov dh, 0     ; Set DH to 0 to set the row number to 0 (the first row)
  mov dl, 0     ; Set DL to 0 to set the column number to 0 (the first column)
  int 10h       ; Call interrupt 10h to set the cursor position 
  
  mov ah, 06h   ; Set AH to 06h to call the clear screen function
  mov al, 00h   ; Set AL to 0 to clear the entire screen
  mov bh, 07h   ; Set BH to 07h to set the background color to white
  mov cx, 0     ; Set CX to 0 to clear the entire screen
  mov dl, 99h    ; Set DL to 80 to set the width of the screen to 80 characters
  mov dh, 24    ; Set DH to 24 to set the height of the screen to 24 lines
  int 10h       ; Call interrupt 10h to clear the screen
                                                               
                                                               
    pop bx
    pop dx
    pop ax
    pop cx
    
    ret
    endp
    
    
 STRING_DECIMALE PROC       ;ON PUSH L'OFFSENT ON RETORUNE DANS LA MEME CASE LE CHIFFRE
    PUSH BX
    PUSH SI
    PUSH CX
    PUSH DI
    PUSH DX 
    PUSH AX
    PUSH BP
    
    MOV BP,SP
    MOV BX,[BP+16]
    MOV CX,0
    MOV Cl,[BX+1]
    MOV SI,2
    MOV AX,0
    MOV DX,0
    
    TRANSFORMATION:
    
    MOV DL,[BX+SI]
    SUB DL,30H
    INC SI
    
    MOV DI,AX
    
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    ADD AX,DI
    
    ADD AX,DX
    
    LOOP TRANSFORMATION 
    
    MOV [BP+16],AX
    
    POP BP
    POP AX
    POP DX
    POP DI
    POP CX
    POP SI 
    POP BX
    
    
    RET
 ENDP
 
 
 EST_DECIMALE PROC  ;ON PUSH L'OFFSET DE LA CHAINE EN RETOUNE 1 SI DECI 0 SI FAUX
    
 PUSH BX
 PUSH AX
 PUSH CX
 PUSH SI
 PUSH BP
 
 MOV BP,SP
 
 MOV BX,[BP+12]
 MOV CH,0
 MOV Cl,[BX+1]
 MOV SI,2
 MOV AL,1
 
 VERIFIE_D:
 
 MOV AH,[BX+SI]   ; ON RECUPERE LES CAR
 
 
 CMP AH,'0'    ; ONVERIFIE SI ILS SONT DANS L'INTERVALLE
 JB PAS_D
 
 CMP AH,'9'
 JA PAS_D
 
 INC SI
 
 LOOP VERIFIE_D
 
 JMP FPAS_D
 PAS_D:
 MOV AL,0
 FPAS_D:
 MOV AH,0
 
 MOV [BP+12],AX
 
 POP BP
 POP SI
 POP CX
 POP AX
 POP BX
  RET
   ENDP


 EST_HEXADECIMALE PROC  ;ON PUSH L'OFFSET DE LA CHAINE EN RETOUNE 1 SI HEXA 0 SI FAUX
    
 PUSH BX
 PUSH AX
 PUSH CX
 PUSH SI
 PUSH BP
 
 MOV BP,SP
 
 MOV BX,[BP+12]
 MOV CH,0
 MOV Cl,[BX+1]
 MOV SI,2
 MOV AL,1
 
 VERIFIE_H:
 
 MOV AH,[BX+SI]   ; ON RECUPERE LES CAR
 
 
 CMP AH,'0'    ; ONVERIFIE SI ILS SONT DANS L'INTERVALLE
 JB PAS_H
 
 CMP AH,'9'
 JA VERCH 
 
 JMP FVERCH
 VERCH:
 CMP AH,'A'
 JB PAS_H
 
 CMP AH,'F'
 JA PAS_H
 
 FVERCH:
 INC SI
 
 LOOP VERIFIE_H
 
 JMP FPAS_H
 PAS_H:
 MOV AL,0
 FPAS_H:
 MOV AH,0
 
 MOV [BP+12],AX
 
 POP BP
 POP SI
 POP CX
 POP AX
 POP BX
  RET
   ENDP 

; this procedure prints number in AX,
; used with PRINT_NUM_UNS to print signed numbers:
PRINT_NUM       PROC    NEAR
        PUSH    DX
        PUSH    AX

        CMP     AX, 0
        JNZ     not_zero

        PUTC    '0'
        JMP     printed

not_zero:
        ; the check SIGN of AX,
        ; make absolute if it's negative:
        CMP     AX, 0
        JNS     positive
        NEG     AX

        PUTC    '-'

positive:
        CALL    PRINT_NUM_UNS
printed:
        POP     AX
        POP     DX
        RET
PRINT_NUM       ENDP



; this procedure prints out an unsigned
; number in AX (not just a single digit)
; allowed values are from 0 to 65535 (FFFF)
PRINT_NUM_UNS   PROC    NEAR
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX

        ; flag to prevent printing zeros before number:
        MOV     CX, 1

        ; (result of "/ 10000" is always less or equal to 9).
        MOV     BX, 10000       ; 2710h - divider.

        ; AX is zero?
        CMP     AX, 0
        JZ      print_zero

begin_print:

        ; check divider (if zero go to end_print):
        CMP     BX,0
        JZ      end_print

        ; avoid printing zeros before number:
        CMP     CX, 0
        JE      calc
        ; if AX<BX then result of DIV will be zero:
        CMP     AX, BX
        JB      skip
calc:
        MOV     CX, 0   ; set flag.

        MOV     DX, 0
        DIV     BX      ; AX = DX:AX / BX   (DX=remainder).

        ; print last digit
        ; AH is always ZERO, so it's ignored
        ADD     AL, 30h    ; convert to ASCII code.
        PUTC    AL


        MOV     AX, DX  ; get remainder from last div.

skip:
        ; calculate BX=BX/10
        PUSH    AX
        MOV     DX, 0
        MOV     AX, BX
        DIV     CS:ten  ; AX = DX:AX / 10   (DX=remainder).
        MOV     BX, AX
        POP     AX

        JMP     begin_print
        
print_zero:
        PUTC    '0'
        
end_print:

        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
PRINT_NUM_UNS   ENDP
ten             DW      10      ; used as multiplier.  

  STRING_BINAIRE PROC       ;ON PUSH L'OFFSENT ON RETORUNE DANS LA MEME CASE LE CHIFFRE
    PUSH BX
    PUSH SI
    PUSH CX
    PUSH DX 
    PUSH AX
    PUSH BP
    
    MOV BP,SP
    MOV BX,[BP+14]
    MOV CX,0
    MOV Cl,[BX+1]
    MOV SI,2
    MOV AX,0
    MOV DX,0
    
    TRANSFORMATIONB:
    
    MOV DL,[BX+SI]
    SUB DL,30H
    INC SI
  
    ADD AX,AX
    
    
    ADD AX,DX
    
    LOOP TRANSFORMATIONB 
    
    MOV [BP+14],AX
    
    POP BP
    POP AX
    POP DX
    POP CX
    POP SI 
    POP BX
    
    
    RET
  ENDP
  
  
  EST_BINAIRE PROC  ;ON PUSH L'OFFSET DE LA CHAINE EN RETOUNE 1 SI DECI 0 SI FAUX
    
 PUSH BX
 PUSH AX
 PUSH CX
 PUSH SI
 PUSH BP
 
 MOV BP,SP
 
 MOV BX,[BP+12]
 MOV CH,0
 MOV Cl,[BX+1]
 MOV SI,2
 MOV AL,1
 
 VERIFIE_B:
 
 MOV AH,[BX+SI]   ; ON RECUPERE LES CAR
 
 
 CMP AH,'0'    ; ONVERIFIE SI ILS SONT DANS L'INTERVALLE
 JB PAS_B
 
 CMP AH,'1'
 JA PAS_B
 
 INC SI
 
 LOOP VERIFIE_B
 
 JMP FPAS_B
 PAS_B:
 MOV AL,0
 FPAS_B:
 MOV AH,0
 
 MOV [BP+12],AX
 
 POP BP
 POP SI
 POP CX
 POP AX
 POP BX
  RET
   ENDP
  
 AFFICHAGED PROC
        
        PUSH AX
        PUSH BX
        PUSH BP
        
        MOV BP,SP
        
        CALL CLEAR_SCREEN
        
        MOV AX,[BP+14]
        CALL PRINT_NUM
        
        MOV AX,[BP+12]
        MOV AH,0EH
        INT 10H
        
        MOV AX,[BP+10]
        CALL PRINT_NUM
        
        MOV AL,'='
        MOV AH,0EH
        INT 10H
        
        MOV AX,[BP+8]
        CALL PRINT_NUM
         
        
        POP BP
        POP BX
        POP AX
         RET
          ENDP
         
  
 binaire_string proc    ;on push la chaine puis le chiffre le resultat est directement dans la chaine
    
    push bx
    push si
    push cx
    push di
    push ax
    push dx
    push bp
    
    MOV BP,SP
    
    MOV BX,[BP+18]
    MOV AX,[BP+16]
    MOV CX,0
    MOV Cl,[BX]
    MOV DI,1
    MOV SI,17
    
    RETRANSFORMATION:
    MOV DX,DI
    AND DX,AX
    
    CMP DX,0
    JNE NOTZ
    MOV [BX+SI],'0'
    JMP FNOTZ
    NOTZ:
    MOV [BX+SI],'1'
    FNOTZ:
    DEC SI
    ADD DI,DI
    LOOP RETRANSFORMATION
    
    MOV [BX+18],'$' 
    POP BP
    POP DX
    POP AX
    POP DI
    POP CX
    POP SI
    POP BX
     RET
      ENDP 

 ADDITION PROC  ;PUSH OP1/OP2 LE RETOUR DANS OP1


    push ax
            push bx
            push bp

            mov bp,sp



            mov ax, [bp+8]
            mov bx, [bp+10]

            add ax,bx

            mov [bp+10],ax

            pop bp
            pop bx
            pop ax




            ret

 ENDP
 
 AFFICHAGEB PROC      ;ON PUSH STOP1/OP/STOP2/STRESU
        
        PUSH AX
        PUSH DX
        PUSH BX
        PUSH BP
        
        MOV BP,SP
        
        CALL CLEAR_SCREEN 
        
        MOV DX,[BP+16]
        ADD DX,2
        MOV AH,09h
        INT 21H
        
        MOV DX,[BP+14]
        MOV AL,DL
        MOV AH,0EH
        INT 10H
        
        MOV DX,[BP+12]
        ADD DX,2
        MOV AH,09H
        INT 21H
        
        MOV AL,'='
        MOV AH,0EH
        INT 10H
        
        MOV DX,[BP+10]
        ADD DX,2
        MOV AH,09h
        INT 21h
        
        LEA DX,sautligne
        MOV AH,09h
        INT 21H

        
        POP BP
        POP BX
        POP DX
        POP AX
         RET
 ENDP
    
    
  SOUSTRACTION PROC  ;PUSH OP1/OP2 LE RETOUR DANS OP1


    push ax
            push bx
            push bp

            mov bp,sp



            mov ax, [bp+8]
            mov bx, [bp+10]

            sub ax,bx

            mov [bp+10],ax

            pop bp
            pop bx
            pop ax




            ret

 ENDP   
 AFFICHAGEDM PROC       ;PUSH OP1/OP2/OPRESt/OPRESu
        
        PUSH AX
        PUSH BX
        PUSH BP
        
        MOV BP,SP
        
        CALL CLEAR_SCREEN
        
        MOV AX,[BP+14]
        CALL PRINT_NUM_UNS
        
        MOV AL,'*'
        MOV AH,0EH
        INT 10H
        
        MOV AX,[BP+12]
        CALL PRINT_NUM_UNS
        
        MOV AL,'='
        MOV AH,0EH
        INT 10H
        
        MOV AX,[BP+8]
        CALL PRINT_NUM_UNS
        
        MOV AX,[BP+10]
        CALL PRINT_NUM_UNS
         
        
        POP BP
        POP BX
        POP AX
         RET
ENDP


MUXD PROC   ;ON PUSH OP1/OP2 LE RETOUR DX DANS OP1/OP2
    
    PUSH BX
    PUSH AX
    PUSH DX
    PUSH BP
    
    MOV BP,SP
    XOR DX,DX
    MOV AX,[BP+10]
    MOV BX,[BP+12]
    MUL BX
    MOV [BP+10],DX
    MOV [BP+12],AX 
    
    POP BX
    POP AX
    POP DX
    POP BP
    
     RET
      ENDP   
    
    
        



DIVISION PROC   ;;OP1 PUIS OP2 FIN:: OP1:RST/OP2/RESU
    PUSH BX
    PUSH AX        
    PUSH DX
    PUSH BP
    
    MOV BP,SP
          
    XOR DX,DX
    MOV AX,[BP+12]
    MOV BX,[BP+10]     
    DIV BX        
    
    MOV [BP+10],AX
    MOV [BP+12],DX 
    
    POP BP
    POP DX
    POP AX
    POP BX        
             
    RET            
ENDP 

String_Hex PROC 
   
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI 
    PUSH BP
          
    MOV BP,SP      
    MOV BX,[BP + 14]
    MOV CH, 0
    MOV CL, [BX + 1]
    MOV DX,0
    MOV SI, 2
    MOV AX,0
    
    LOOP1:            
   
    ADD DX,DX
    ADD DX,DX
    ADD DX,DX
    ADD DX,DX
    
    MOV AL,[BX+SI]
    
    CMP AX, 39h
    JG LETTER
    SUB AX,30h
    ADD DX, AX
    JMP FLETTER
    LETTER:
    SUB AX,37h
    ADD DX, AX 
    FLETTER:
    ADD SI,1
    
    LOOP LOOP1
       
        
        
    MOV [BP+ 14], DX;    
        
          
    POP BP      
    POP SI
    POP DX 
    POP CX
    POP BX
    POP AX
    
    
RET
ENDP

Print_Hexa PROC
    PUSH AX      
    PUSH BX        
    PUSH CX          
    PUSH DX            
    PUSH BP              
    PUSH SI               
    
    MOV BP, SP
    MOV AX, [BP + 14] 
    
    MOV BX, 10h
    MOV CX, 4
    
    print_loop:
    MOV DX, 0
    DIV BX
    PUSH DX
    DEC CX
    CMP CX, 0
    JNZ print_loop
    MOV CX,4
    MOV AH, 2
    print_digit:
    POP DX
    CMP DL, 10
    JL print_decimal
    ADD DL, 7
    print_decimal:
    ADD DL, 30h
    INT 21h
    LOOP print_digit 
    
    
    
    POP SI
    POP BP
    POP DX
    POP CX
    POP BX
    POP AX 
    RET
ENDP

ENDS