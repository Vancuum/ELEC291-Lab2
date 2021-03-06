; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

;Symbolic constants for pushbuttons on breadboard
BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P0.2
HRS_UP        equ P2.0
MINS_UP		  equ P2.3
SECS_UP 	  equ P2.5
ALRM_MINS_UP  equ P0.3
ALRM_HRS_UP   equ P0.6
ALRM_SET      equ P1.3

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
;BCD counters to store the current time
BCD_hrs: ds 1
BCD_secs:  ds 1
BCD_mins: ds 1 
;BCD counters to store the time the alarm is set to
alarm_hrs: ds 1
alarm_mins: ds 1


cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
morning_flag: dbit 1 ;Set to one if it is the morning
alarm_flag: dbit 1 ;Set to one if the alarm functionality is turned on
alarm_morning_flag: dbit 1 ;Set to one if the alarm is set to a time in the morning

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                         1234567890123456    <- This helps determine the location of the counter
Initial_Message_Row1: db 'Time  xx:xx:xxX ', 0
Initial_Message_Row2: db 'Alarm xx:xxX xxx', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-bit timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    clr TR0  ;Don't want timer to start right away
	ret
	
;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	
	cpl SOUND_OUT ; Connect speaker to P0.2
	reti
	
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret
	
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
	
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1 second has passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know one second has passed
	
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; Increment the seconds counter
	mov a, BCD_secs
	cjne a, #59H, inc_secs ;Check if seconds have "overflowed"
	;Reset seconds counter
	clr a
	mov BCD_secs, a
	
	;Increment minutes counter
	mov a, BCD_mins
	cjne a, #59H, inc_mins ;Check if minutes have "overflowed"
	;Reset minutes counter
	clr a
	mov BCD_mins, a
	
	;Increment hours counter
	mov a, BCD_hrs
	cjne a, #12H, inc_hrs ;Check if hours have "overflowed"
	;Reset hours counter
	clr a
	sjmp inc_hrs ;When hours overflow, BCD_hrs should be reset to 01 and not 00
	
inc_secs:
	add a, #0x01
	da a
	mov BCD_secs, a
	sjmp Timer2_ISR_done
	
inc_mins:
	add a, #0x01
	da a
	mov BCD_mins, a
	sjmp Timer2_ISR_done
	
inc_hrs:
	add a, #0x01
	da a
	mov BCD_hrs, a
	;Morning flag should be changed whenver the hour hits 12
	cjne a, #12H, Timer2_ISR_done
	cpl morning_flag
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
    
    ;Initialize LCD screen
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message_Row1)
    Set_Cursor(2,1)
    Send_Constant_String(#Initial_Message_Row2)
    
    ;Initialize current time. Default is 12:00:00
	mov a, #0x0C
	da a
	mov BCD_hrs, a
	mov BCD_mins, #0x00
	mov BCD_secs, #0x00
	
	;Initialize alarm time. Default is 12:00
	mov a, #0x0C
	da a
	mov alarm_hrs, a
	mov alarm_mins, #0x00
	
	;Initialize flags. Default is morning, alarm off
	setb one_second_flag
	setb morning_flag
	setb alarm_morning_flag
	clr alarm_flag

	; After initialization the program stays in this 'forever' loop
loop:
	;Check if hours are being incremented through buttons
	jb HRS_UP, check_mins
	Wait_Milli_Seconds(#80)
	jb HRS_UP, check_mins
	;Check if hours have overflowed
check_hr:
	mov a, BCD_hrs
	cjne a, #12H, add_hr
	clr a
add_hr:
	add a, #0x01
	da a
	mov BCD_hrs, a
	;Morning flag should be changed whenver the hour hits 12
	cjne a, #12H, jump
	cpl morning_flag
jump:
	ljmp display
	
	;Check if minutes are being incremented through pushbutton
check_mins:
	jb MINS_UP, check_secs
	Wait_Milli_Seconds(#80)
	jb MINS_UP, check_secs
mins_ovf:	
	mov a, BCD_mins
	cjne a, #59H, add_min
	clr a
	mov BCD_mins, a
	sjmp check_hr
add_min:
	add a, #0x01
	da a
	mov BCD_mins, a
	ljmp display
	
check_secs:
	jb SECS_UP, check_alarm_hrs
	Wait_Milli_Seconds(#80)
	jb SECS_UP, check_alarm_hrs
	mov a, BCD_secs
	cjne a, #59H, add_sec
	clr a
	mov BCD_secs, a
	sjmp mins_ovf
add_sec:
	add a, #0x01
	da a
	mov BCD_secs, a
	ljmp display
		
check_alarm_hrs:
	jb ALRM_HRS_UP, check_alarm_mins
	Wait_Milli_Seconds(#80)
	jb ALRM_HRS_UP, check_alarm_mins
check_alarm_hr:
	mov a, alarm_hrs
	cjne a, #12H, add_alarm_hr
	clr a
add_alarm_hr:
	add a, #0x01
	da a
	mov alarm_hrs, a
	;Morning flag should be changed whenver the hour hits 12
	cjne a, #12H, display
	cpl alarm_morning_flag
	sjmp display
	
check_alarm_mins:
	jb ALRM_MINS_UP, check_alarm_set
	Wait_Milli_Seconds(#80)
	jb ALRM_MINS_UP, check_alarm_set
	mov a, alarm_mins
	cjne a, #59H, add_alarm_min
	clr a
	mov alarm_mins, a
	sjmp check_alarm_hr
add_alarm_min:
	add a, #0x01
	da a
	mov alarm_mins, a
	sjmp display
	
check_alarm_set:
	jb ALRM_SET, check_boot
	Wait_Milli_Seconds(#80)
	jb ALRM_SET, check_boot
	jnb ALRM_SET, $ ;Wait until button is released
	cpl alarm_flag
	sjmp display
	
check_boot:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	
	;Reset milliseconds counter
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	;Reset current time
	mov BCD_secs, a
	mov BCD_mins, a
	mov BCD_hrs, a
	
	;Reset alarm time
	mov alarm_hrs, a
	mov alarm_mins, a
	
	;Reset flags
	setb morning_flag
	clr alarm_flag
	
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
loop_a:
	jb one_second_flag, loop_b
	ljmp loop
loop_b:
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
display:    
    ;Display current time
    Set_Cursor(1,7)
	Display_BCD(BCD_hrs)
	Set_Cursor(1, 10)
	Display_BCD(BCD_mins)
	Set_Cursor(1, 13)     
	Display_BCD(BCD_secs) 

	;Display alarm time
	Set_Cursor(2, 7)
	Display_BCD(alarm_hrs)
	Set_Cursor(2,10)
	Display_BCD(alarm_mins)
	
	;Display alarm on/off
	Set_Cursor(2, 14)
	Display_char(#'o')
	Set_Cursor(2,15)
	jnb alarm_flag, alarm_off
	Display_char(#'n')
	Set_Cursor(2,16)
	Display_char(#' ')
	sjmp display_AP_alarm
alarm_off:
	Display_char(#'f')
	Set_Cursor(2,16)
	Display_char(#'f')
	
display_AP_alarm:
	Set_Cursor(2,12)
	jnb alarm_morning_flag, set_alarm_pm
	Display_char(#'A')
	sjmp display_AP
set_alarm_pm:
	Display_char(#'P')
	
display_AP:	
	Set_Cursor(1, 15)
	jnb morning_flag, set_pm
	Display_char(#'A')
	sjmp check_alarm_sound
set_pm:
	Display_char(#'P')
	
	;Check to see if the alarm should be turned on. Conditions to be met:
	;1. Alarm and actual time are both AM or both PM
	;2. Alarm and actual mins are the same
	;3. Alarm and actual hours are the same
	;4. Alarm is activated
check_alarm_sound:
	clr a
	mov b, a
	mov c, morning_flag
	mov b.0, c
	mov c, alarm_morning_flag
	mov acc.0, c
	cjne a, b, alarm_sound_off
	
	mov a, BCD_mins
	cjne a, alarm_mins, alarm_sound_off
	
	mov a, BCD_hrs
	cjne a, alarm_hrs, alarm_sound_off
	jnb alarm_flag, alarm_sound_off
	setb TR0  ; Start timer 0
	sjmp continue
alarm_sound_off:
	clr TR0
continue:
    ljmp loop
END