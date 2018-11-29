TITLE Program Template     (template.asm)

; Author: Carrie Davis
; Last Modified: 11/27/18
; OSU email address: davicarr@oregonstate.edu
; Course number/section: CS 271/400
; Project Number:  6A              Due Date: 12/2/18
; Description: Validates and writes numbers into 32bit register
; The user enters 15 unsigned decimal integers and the program will 
; determine if they fit into 32 bit register. If not will ask user to re-enter
; Displays then the list of integers, sum and average value
; Implementation notes: 
;	This program is implemented using procedures and macros.
;   All variables are passed except constants


INCLUDE Irvine32.inc

;MACROS are placed here
;Macro to display text argument on console
;receives: text string
;returns: none
;preconditions:  none
;registers changed: edx

displayString MACRO text
	LOCAL string				;local label
	.data
	string	BYTE	text,0	;define the string
	.code
	push	edx
	mov		edx,OFFSET string
	call	WriteString
	pop		edx
ENDM
;Macro to get text argument from console
;Reads standard input into buffer
;receives: the name of the buffer
;returns: none
;preconditions: don't pass ecx, edx reg as arguements
;registers changed: edx

getString MACRO arrayStr, lengthStr
	displayString "Please enter an unsigned number: " ;nested macro to prompt user
	push	edx
	push	ecx
	mov		edx,arrayStr
	mov		ecx,lengthStr
	call	ReadString	;read string puts number string in eax
	pop		ecx
	pop		edx
ENDM

; Constants listed here
NUMBEROFINT = 10

.data
; variable definitions
numArray		DWORD	NUMBEROFINT	DUP(0)		;holds array declared using max size
buffer			BYTE	255			DUP(0)			;for getString
number			DWORD	?

texts			EQU	<"Programming Assignment 6: Designing low-level I/O procedures">
prompt			EQU <"Enter an integer: ">
.code
main PROC

; (insert executable instructions here)
displayString texts
call	Crlf
displayString "Written by: Carrie Davis"
call	Crlf
displayString "Please provide 10 unsigned decimal integers"
call	Crlf
displayString "Each number needs to be small enough to fit inside a 32 bit register."
call	Crlf
displayString "After you have finished inputting the raw numbers I will display a list "
call	Crlf
displayString "of integers, their sum, and their average value"
push	number
push	OFFSET buffer
push	SIZEOF buffer
call ReadVal
mov	 edx,number
call WriteDec
; sort  array
;push	OFFSET numArray
;push	NUMBEROFINT
;call	sortList
;call	Crlf
;push	OFFSET numArray
;push	NUMBEROFINT
;call	displayList
call	Crlf

	exit	; exit to operating system
main ENDP


;receives: value
;returns: none
;preconditions:  none, user name can be any keyboard char values input or none
;registers changed: eax,ecx,edx
ReadVal	PROC
; readVal should invoke the getString macro to get the user’s string of digits.
; SET up the stack frame 
push    ebp     ;old value of ebp stored on the system stack
mov ebp, esp    ;ebp now contains value of esp, which is address of top of stack
jmp start
startOver:
	displayString "ERROR: You did not enter an unsigned integer number or your number was too big"
	displayString "Please try again."
start:
;mov		ebx, [ebp + 16]	; reference to int var
mov		edx, [ebp + 12]	; reference to the buffer variable
mov		ecx, [ebp + 8]	; size of buffer pushed to ecx register so it is tracked

	displayString	"Enter an integer: "
	getString edx, ecx
	cld
	mov esi, edx ; output from ReadString
	;mov edi, esi;[ebp + 16]      ; pushed array destignation
	mov eax,0
	mov ecx,0
	mov ebx,10
	;mov ecx, eax			;tracking size of string from ReadString
	;add edi, e
	;dec edi                 ;When DF=1 STOSB starts here
	;cld								;dir forward
	;mov	esi,[edx]						;source OFFSET from ReadString
	;mov	edi,esi					; destingation index 
	;mov	ecx, LENGTHOF eax		;loop counter represents k
	;dec ecx						;subtract 1
	;mov	ebx,0					;represents x
	;mov eax,0
L1:	lodsb						;load [esi] into eax
	cmp	ax,0					; if zero then the end of string is reached
	je	endOf
	.IF (ax >=48) ||(ax <=57)		;if 48<=str[k]<=57 it's numeric
	sub		ax,48
	xchg	eax,ecx
	mul		ebx
	jc		startOver
	add		eax,ecx  
	;stosd				;Store eax into [edi]
	xchg	eax,ecx
	
	;add	esi,4
	;inc edi				;displacement for byte
	.ELSE				;It should then convert th digit string to numeric, while validating the user’s input.
	jmp startOver
	.ENDIF
	jmp L1
endOf:
	xchg	ecx, eax
	;mov		DWORD PTR buffer, eax	;Save int in passed variable
	;popad
	mov	[ebp + 16],eax
	pop ebp
	ret 12


;Intro directions
;mov	ebp,esp
;mov	edx, [ebp + 8]	; reference to the buffer variable
;getString buffer
	;.IF		(eax < 0) || (LENGTHOF buffer > 3) ;conditional on if Entered value neg or > 400

	;.ENDIF					;ends the logic
	;ret
	;ret
;ret 4
ReadVal	ENDP

;Procedure WriteVal will write values to cmd
;receives: value
;returns: none
;preconditions:  none, user name can be any keyboard char values input or none
;registers changed: eax,ecx,edx
WriteVal	PROC
; writeVal should convert a numeric value to a string of digits, 
; and invoke the displayString macro to produce the output
; SET up the stack frame 
push    ebp     ;old value of ebp stored on the system stack
mov ebp, esp    ;ebp now contains value of esp, which is address of top of stack


mov edi, [ebp + 8]      ; EDI = @outString
add edi, NUMBEROFINT
dec edi

std                 ; set direction flag. allows us to add chars to the end of outString


mov eax, [ebp + 12] ; EAX = userInt

convertInt:             ; the int->string conversion is a post-test loop to handle case where user input '0' as a string

    mov ebx, 10            ; EBX = divisor of 10
    cdq                 ; prep for division
    div ebx             ; EAX = quot, EDX = remainder

    add edx, 48         ; convert EDX to ASCII char value

    push    eax             ; store current value in EAX
    mov eax, edx            ; set EAX to the converted ASCII value


    stosb               ; store the converted char at the end of outString

    pop eax             ;restore value of EAX

    call WriteDec
    call CrLf

    cmp eax, 0
    je  finished            ; if eax = 0, it means we have fully converted userInt to a string
    jmp convertInt      ; else, repeat

finished:
;    displayString [ebp + 8] ; print the converted outString

    pop ebp
    ret 8 
WriteVal	ENDP

; procedure sorts array from highest to lowest values using bubble sort algorithm on pg 375 Irvine
; receives:	an integer filled array with MAX size 200
; returns:	nothing
; preconditions: request must be DWORD between MIN(10) - MAX(200), and array is DWORD and filled with MAX size of 200
; registers Changed: eax,ecx,edx, esi (ebp is set back 

sortList PROC
	push ebp
	mov  ebp, esp
	mov	 ecx, [ebp + 8]   ; value of request to ecx pushed 2nd so on top of stack
	dec	 ecx


	L1:	; loop through using count request that was pushed into ebp
		mov		esi, [ebp + 12] ; @array moving to esi as it was pushed 1st and DWORD
		mov		edx,ecx
	
		
		L2:
			mov		eax, [esi]		;get array value
			mov		ebx, [esi+4]
			cmp		eax,ebx
			jge		L3				;if [ESI] >= [ESI+4], no exchange
			xchg	eax,ebx	;exchange the pair
			mov		[esi],eax
			mov		[esi+4],ebx
	L3:	
		add		esi,4
		loop	L2
		;pop		ecx
		mov		ecx,edx		;retrieve outer loop count
		loop	L1

	pop  ebp			; remove request of base pointer off stack
	ret  8				; pop 8 additional bytes because of DWORD parameters pushed

sortList ENDP


;Procedure to gets passed array and displays array values in rows of 10
;receives: passed parameters of an array and request value
;returns: none
;preconditions: array must be DWORD between MAX - MIN size with integer values, request must be DWORD
;registers changed: eax, ebx, ecx, esi (ebp is set back 

displayList	PROC
;indirect referencing push on stack
	push ebp
	mov	 ebp, esp
	mov  esi, [ebp + 12]	;@array moving to esi as it was pushed 1st and DWORD
	mov  ecx, [ebp + 8]		;value of request moving to esi as it was pushed 2nd and DWORD
	mov	 ebx,10				;move count of 10 to ebx to act as a counter
	
	displayValues:	
		
		mov		eax,[esi]	;display value in array
		call	WriteDec
		displayString "  "
		add		esi, 4		;displacement of 4 must be added to ESI as it points to each subsequent array value
		dec		ebx			
		cmp		ebx,0
		ja		skipNewRow		
		call	Crlf
		mov		ebx,10
	skipNewRow:
		loop	displayValues

	pop  ebp		; remove request of base pointer off stack
	ret  8			; pop 8 additional bytes because of DWORD parameters pushed

displayList	ENDP

END main
