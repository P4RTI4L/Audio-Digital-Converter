
******************************************************************
*
* Title:            Audio-Digital Converter
*
* Objective:        CSE472 Homework 10
*
* Revision:         V2.2
*
* Date:             Mar 8, 2015
*
* Programmer:       Alex Good
*
* Company:          Penn State CSE Department
*
*
*
* Input:            Audio cable
*                   Switch SW1 at PORTP bit 0
*
* Output:           Sends 1024 ADCs to SCI
*
*              
* Comments:         This program is developed and simulated using codewarrior
*                   development software targeted for Axiom's APS12C120 board 
*
*******************************************************************

; parameter declearation section

; export symbols
            XDEF Entry            ; export 'Entry' symbol
            ABSENTRY Entry        ; for assembly entry point
  
; include derivative specific macros
PORTA       EQU  $0000
PORTB       EQU  $0001
DDRA        EQU  $0002
DDRB        EQU  $0003

PUCR        EQU         $000C        ; to enable pull-up mode for PORT A, B, E, K

PTP         EQU         $0258        ; PORTP data register
PTIP        EQU         $0259        ; PORTP input register <<====
DDRP        EQU         $025A        ; PORTP data direction register
PERP        EQU         $025C        ; PORTP pull up/down enable
PPSP        EQU         $025D        ; PORTP pull up/down selection

ATDCTL2     EQU  $0082
ATDCTL3     EQU  $0083
ATDCTL4     EQU  $0084
ATDCTL5     EQU  $0085
ATDSTAT0    EQU  $0086
ATDDR0H     EQU  $0090
ATDDR0L     EQU  $0091
ATDDR7H     EQU  $009e
ATDDR7L     EQU  $009f

SCIBDH      EQU  $00c8            ; Serial port (SCI) Baud Rate Register H
SCIBDL      EQU  $00c9            ; Serial port (SCI) BAUD Rate Register L
SCICR1      EQU  $00ca            ; Serial port (SCI) Control Register 1
SCICR2      EQU  $00cb            ; Serial port (SCI) Control Register 2
SCISR1      EQU  $00cc            ; Serial port (SCI) Status Register 1
SCIDRL      EQU  $00cf            ; Serial port (SCI) Data Register

TIOS        EQU  $0040             ;CH2 Output Compare
TIE         EQU  $004C             ;CH2 interrupt Enable
OC7M        EQU  $0042             ;OC7 mask Off
TTOV        EQU  $0047             ;Timer toggle on overflow Off
TSCR1       EQU  $0046             ;enable timer and fast flag clear
TSCR2       EQU  $004D             ;TOI Off, TCRE Off, TCLK = BCLK/1
TFLG1       EQU  $004E             ;timer CH2 interrupt flag Clear
PACTL       EQU  $0060             ;Pulse Accumulator disabled
TC2         EQU  $0054




; following is for the TestTerm debugger simulation only
;SCISR1      EQU  $0203            ; Serial port (SCI) Status Register 1
;SCIDRL      EQU  $0204            ; Serial port (SCI) Data Register

CRGFLG      EQU  $0037            ; Clock and Reset Generator Flags
CRGINT      EQU  $0038            ; Clock and Reset Generator Interrupts
RTICTL      EQU  $003B            ; Real Time Interrupt Control

CR          equ  $0d              ; carriage return, ASCII 'Return' key
LF          equ  $0a              ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG  $3000            ; RAMStart defined as $3000
                                  ; in MC9S12C128 chip

ATDdone     DS.B  1               ; 1 = ATD finished

ctr2p5m     DS.B  1
times       DS.B  1
timem       DS.B  1
timeh       DS.B  1
oc2_count   DS.B  1

Counter1    DC.W  $9000
msg1        DC.B  'Hello', $00
msg2        DC.B  'Please change terminal baud rate to 115.2K baud', $00
msg3        DC.B  'You may type below', $00
msg4        DC.B  'Please connect the audio cable to HCS12 board and press RETURN', $00
msg5        DC.B  'Well> ', $00


                                  
StackSP                           ; Stack space reserved from here to
                                  ; StackST

            ORG  $3FD2            ; ATD interrupt vector setup

            ORG  $3FD6            ; SCI interrupt vector setup

            ORG  $3FF0            ; RTI interrupt vector setup
            DC.W RTIISR
            
            org    $3FEA          ; Interupt vector
            DC.W   ISRTOC2

            ORG  $3100
StackST

;*******************************************************
; code section
Entry
            LDS   #StackST         ; initialize the stack pointer

            LDAA       #%11110000     ; set PORTB bit 7,6,5,4 as output, 3,2,1,0 as input
            STAA       DDRB           ; LED 1,2,3,4 on PORTB bit 4,5,6,7
                                      ; DIP switch 1,2,3,4 on PORTB bit 0,1,2,3.
            BSET       PUCR,%00000010 ; enable PORTB pull up/down feature, for the
                                      ; DIP switch 1,2,3,4 on the bits 0,1,2,3.

            BCLR       DDRP,%00000011 ; Push Button Switch 1 and 2 at PORTP bit 0 and 1
                                      ; set PORTP bit 0 and 1 as input
            BSET       PERP,%00000011 ; enable the pull up/down feature at PORTP bit 0 and 1
            BCLR       PPSP,%00000011 ; select pull up feature at PORTP bit 0 and 1 for the
                                      ; Push Button Switch 1 and 2.

            LDAA       #%11110000     ; Turn off LED 1,2,3,4 at PORTB4 bit 4,5,6,7
            STAA       PORTB


            ldx   #msg1            ; print the first message, 'Hello'
            jsr   printmsg

            jsr   nextline

            ldaa  #$07             ; produce beep sound on the HyperTerminal
            jsr   putchar          ;

            ldx   #msg2            ; print the second message
            jsr   printmsg

            ldaa  #$07             ; beep sound
            jsr   putchar          ;

            jsr   delayMS
            
            LDAA  #$00             ; set the SCI port baud rate to 115.2K
            STAA  SCIBDH
            LDAA  #$0d
            STAA  SCIBDL
            
            

loop1       jsr   getcharw         ; type writer - what is typed on key board
            jsr   putchar          ; is displayed on the terminal window
            cmpa  #CR
            bne   loop1            ; if Enter/Return key is pressed, move the
            ldaa  #LF              ; cursor to next line
            jsr   putchar
            ldaa  #$07             ; beep sound
            jsr   putchar          ;

            ldx   #msg3            ; print the third message
            jsr   printmsg
            jsr   nextline
            ldx   #msg4
            jsr   printmsg
            jsr   nextline
            ldx   #msg5
            jsr   printmsg
            


            jmp   ADCtest


; 8 point ADC channel 5

ADCtest     jsr   getcharw         ; type writer - what is typed on key board
            jsr   putchar          ; is displayed on the terminal window
            cmpa  #CR
            bne   ADCtest          ; if Enter/Return key is pressed, move the
            ldaa  #LF              ; cursor to next line
            jsr   putchar
            ldaa  #$07             ; beep sound
            jsr   putchar          ;
            jsr   adc08            ; RUN the sample ADC program: 8 data acqusitions
            jsr   nextline
            ldx   #msg6            ; messages
            jsr   printmsg
            jsr   nextline
            ldx   #msg7
            jsr   printmsg
            jsr   nextline
            ldx   #msg5
            jsr   printmsg
            jsr   getcharw
            cmpa  #$61              ; if a entered, start SB data recieve setup
            bne   ADCtest
            
            jsr   nextline          ; terminal messages
            ldx   #msg8
            jsr   printmsg
            jsr   nextline
            ldx   #msg9
            jsr   printmsg
            jsr   nextline
            ldx   #msg10
            jsr   printmsg
            jsr   nextline
            ldx   #msg11
            jsr   printmsg
            
buttonloop  ldaa  PTIP              ; check if SW1 pressed
            anda  #%00000001
            bne   buttonloop
            jsr   timerinit         ;enable oc7 interupt service
            
     
            
           
loopp      
            ldx oc2_count
            cpx #8000             ; 8000 count of OC2 interrupt at 125usec rate
            blo loopp             ;    is 1.0 second
            ldx  #0
            stx  oc2_count
            bra  loopp
           
          


;*******************************************************
; subroutine section



;***********ATD interrupt service routine***************
ATDISR      LDAA  ATDCTL2          ; check ATD IF
            BITA  #%00000001
            BEQ   atderr1
            LDAA  #%11000000       ; Turn ON ADC, clear flags, disable interrupt
            STAA  ATDCTL2

            bset  ATDdone,%11111111 ; Set the ATDdone flag
            
            LDAB  PORTB
            EORB  #%00100000       ; Toggle the PORTB bits 5 - LED2
            STAB  PORTB          
            RTI
            
atderr1     LDAA  PORTB
            EORA  #%00010000       ; Toggle the PORTB bit 4
            STAA  PORTB

atddone     RTI
;***********end of ATD interrupt service routine********


;***********OC7 interrupt service routine***************

ISRTOC2     ldaa #%00000100      ; clear OC2 interrupt flag
            staa TFLG1

            ldd  #3000           ; set for the next OC2 interrupt
            addd TC2             ;    3000 at 24MHz rate is 125usec => 8KHz rate
            std  TC2

            ldx oc2_count        ; update OC2 interrupt counter
            inx
            stx  oc2_count          
              
            jsr adc1024               ; do ADC conversions

            ldaa  #%00000000      ; end interupt
            staa  TIE

            rti                  ; done
;***********Timer 2 Output Compare interrupt initialization routine***************

timerinit    

             BSET      TIOS,  #%00000100
    
             BSET      TIE,   #%00000100
             BSET      OC7M,  #%10000000
    
             BSET      TTOV,  #%00000000
    
             BSET      TSCR1, #%10010000
    
             BSET      TSCR2, #%00000000
    
             BSET      TFLG1, #%00000100
    
             BSET      PACTL, #%00000000
             ldx      #0
             stx      oc2_count
             cli
             jsr      loopp
              
    

;***********sample AD conversiton*********************
adc08       ldab  #$08             ; do AD conversions 8 times
adloop      jsr   adc              ; single AD conversion call
            ldaa  ATDDR0H
            jsr   printHx          ; print the result
            ldaa  #$20
            jsr   putchar                          
            decb
            bne   adloop           ; loop 8 times
            rts
            
adc1024     ldx   #1024            ; do AD conversions 1024 times
adloop1     jsr   adc              ; single AD conversion call
            ldaa  ATDDR0H
            jsr   putchar                          
            dex   
            bne   adloop1           ; loop 1024 times
            rts

;***********single AD conversiton*********************

adc         LDAA  #%11000000       ; Turn ON ADC, clear flags, disable interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
            LDAA  #%01000111       ; 10bit, ADCLK=24MHz/16=1.5MHz, sampling time=8*(1/ADCLK)
            STAA  ATDCTL4
            LDAA  #%00000111       ; left justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, channel 5, start the conversion

atdwait     ldaa  ATDSTAT0
            anda  #%10000000       ; check SCF bit, wait for ADC conversion to finish
            beq   atdwait

            rts
;***********end of AD conversiton**************            





;***********printHx***************************
; prinHx: print the content of accumulator A in Hex on SCI port
printHx     psha
            lsra
            lsra
            lsra
            lsra
            cmpa   #$09
            bhi    alpha1
            adda   #$30
            jsr    putchar
            bra    low4bits
alpha1      adda   #$37
            jsr    putchar            
low4bits    pula
            anda   #$0f
            cmpa   #$09
            bhi    alpha2
            adda   #$30
            jsr    putchar
            rts
alpha2      adda   #$37
            jsr    putchar
            rts
;***********end of printhx***************************
 
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* C
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                       ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************getcharw***********************
;* Program: Input one character from SCI port, terminal/keyboard
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;* Registers modified: CCR
;* Algorithm:
;    Wait for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getcharw    brclr SCISR1,#%00100000,getcharw
            ldaa  SCIDRL
            rts
;****************end of getcharw**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

;****************delayMS***********************
; delayMS subroutine
;
; This subroutine cause few msec. delay
;
; Input:  a 16bit count number in 'Counter1'
; Output: time delay, cpu cycle waisted
; Registers in use: X register, as counter
; Memorycations in use: a 16bit input number in 'Counter1'
; 
; Comments: one can add more NOP instructions to lengthen
;           the delay time.
            
delayMS
            LDX   Counter1        ; short delay
dlyMSLoop   NOP                   ; X * NOP
            DEX
            BNE   dlyMSLoop
            RTS
;****************end of delayMS*****************


;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip


msg6        DC.B  '7B-85 = connected, 00-20 = not connected', $00
msg7        DC.B  'Press a to continue', $00
msg8        DC.B  'Please disconnect the HyperTerminal', $00
msg9        DC.B  'Start NCH Tone Generator program', $00
msg10        DC.B  'Start SB Data Receive program', $00
msg11        DC.B  'Then press the switch SW1, for 1024 point analog to digital conversions', $00



            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
