; sets the program's load address to 0x0100 (standard for com files in dos)
org 0x0100

; jumps to the 'start' label to begin execution
jmp start

; double word (32-bit) variable to store the original address of the keyboard interrupt service routine (isr)
oldisr: dd 0

; solid block character (█) or your paddle char
paddle_char db 0DBh

; variables
; word (16-bit) variable to store the current video memory offset of the ball
ball_loc: dw 0

; flag (1=yes, 0=no) to indicate the ball should change direction when moving down (e.g., hit a wall)
change_down_direction: dw 0

; flag (1=yes, 0=no) to indicate the ball should change direction when moving up (e.g., hit a wall)
change_up_direction: dw 0

; flag (1=yes, 0=no) set by keyboard isr to initiate paddle movement left
left: dw 0

; flag (1=yes, 0=no) set by keyboard isr to initiate paddle movement right
right: dw 0

; flag (1=yes, 0=no) set by check_player_hit to indicate if the ball hit the paddle
check_hit: dw 0

; flag (1=yes, 0=no) to indicate if the ball should move straight down after a top bounce
straight_down: dw 0

; score variable
score: dw 0

; lives variable
lives: dw 3

; score display string
score_msg: db 'Score: $'

; game over message string
game_over_msg: db 'GAME OVER - Press any key$'

; -------------------------------
; new: display score on screen
; shows score at top right of screen (row 0, starting at column 73)
; -------------------------------
display_score:
    ; push all general-purpose registers onto the stack
    pusha
    ; push extra segment register
    push es
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    
    ; position: row 0, column 73 (offset = 0*160 + 73*2 = 146)
    ; this leaves room for up to 7 digits
    ; set di to display position
    mov di, 156
    
    ; convert score to decimal and display
    ; load score into ax
    mov ax, [score]
    ; initialize digit counter
    mov cx, 0
    ; divisor for decimal conversion
    mov bx, 10
    
    ; handle zero case
    ; check if score is zero
    cmp ax, 0
    ; if not zero, start division loop
    jne .disp_score_divide_loop
    ; display '0' character
    mov byte [es:di], '0'
    ; set attribute to white on black
    mov byte [es:di+1], 0x0F
    ; skip to done
    jmp .disp_score_done
    
.disp_score_divide_loop:
    ; divide by 10 to get digits
    ; check if number is fully processed
    cmp ax, 0
    ; if zero, print the digits
    je .disp_score_print_digits
    ; clear dx for division
    xor dx, dx
    ; divide ax by 10
    div bx
    ; save remainder (digit) on stack
    push dx
    ; count digits
    inc cx
    ; continue dividing
    jmp .disp_score_divide_loop
    
.disp_score_print_digits:
    ; check if all digits printed
    cmp cx, 0
    ; if done, exit
    je .disp_score_done
    ; retrieve digit from stack
    pop dx
    ; convert digit to ascii
    add dl, '0'
    ; write character to video memory
    mov [es:di], dl
    ; set attribute to white on black
    mov byte [es:di+1], 0x0F
    ; move to next character position
    add di, 2
    ; decrement digit counter
    dec cx
    ; continue printing digits
    jmp .disp_score_print_digits
    
.disp_score_done:
    ; restore extra segment
    pop es
    ; restore all general-purpose registers
    popa
    ; return from procedure
    ret

; -------------------------------
; new: display lives on screen
; shows lives at top right (row 1, column 73) in green
; -------------------------------
display_lives:
    ; push all general-purpose registers onto the stack
    pusha
    ; push extra segment register
    push es
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    
    ; position: row 1, column 73 (offset = 1*160 + 73*2 = 306)
    ; set di to display position
    mov di, 316
    
    ; get lives value
    ; load lives into ax
    mov ax, [lives]
    ; convert to ascii (single digit 0-3)
    add al, '0'
    
    ; display with green color (0x0a)
    ; write character to video memory
    mov [es:di], al
    ; set attribute to green on black
    mov byte [es:di+1], 0x0A
    
    ; restore extra segment
    pop es
    ; restore all general-purpose registers
    popa
    ; return from procedure
    ret

; -------------------------------
; new: decrement lives and check for game over
; returns: zf set if game over (lives = 0)
; -------------------------------
decrement_lives:
    ; push all general-purpose registers onto the stack
    pusha
    
    ; decrease lives
    ; decrement lives counter
    dec word [lives]
    
    ; update display
    ; call display routine
    call display_lives
    
    ; check if game over
    ; compare lives with zero
    cmp word [lives], 0
    
    ; restore all general-purpose registers
    popa
    ; return from procedure (zf set if lives=0)
    ret

; -------------------------------
; new: game over screen
; -------------------------------
game_over:
    ; push all general-purpose registers onto the stack
    pusha
    ; push extra segment register
    push es
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    
    ; clear screen area for message (center of screen)
    ; row 12, column 30 (offset = 12*160 + 30*2 = 1980)
    ; set di to message position
    mov di, 1980
    
    ; display "game over" message
    ; load message address
    mov si, game_over_msg
    ; light red on black attribute
    mov ah, 0x0C
    
.gameover_print:
    ; load character from message
    lodsb
    ; check for end of string
    cmp al, '$'
    ; if end, finish printing
    je .gameover_done
    ; write character to video memory
    mov [es:di], al
    ; write attribute to video memory
    mov [es:di+1], ah
    ; move to next character position
    add di, 2
    ; continue printing
    jmp .gameover_print
    
.gameover_done:
    ; restore extra segment
    pop es
    ; restore all general-purpose registers
    popa
    
    ; wait for keypress
    ; bios keyboard input function
    mov ah, 0
    ; call bios keyboard interrupt
    int 0x16
    
    ; exit program
    ; dos terminate program function
    mov ax, 0x4c00
    ; call dos interrupt
    int 0x21

; -------------------------------
; new: increment score and update display
; call this when a brick is destroyed
; -------------------------------
increment_score:
    ; push all general-purpose registers onto the stack
    pusha
    
    ; add 10 points for each brick
    ; add 10 to score
    add word [score], 10
    
    ; update display
    ; call display routine
    call display_score
    
    ; restore all general-purpose registers
    popa
    ; return from procedure
    ret

; -------------------------------
; new: check if position contains a brick character
; input: di = video memory offset to check
; returns: zf set if brick found, zf clear if no brick
; -------------------------------
is_brick:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; save ax register
    push ax
    ; save di register
    push di
    
    ; get position to check
    ; load position from stack
    mov di, [bp+4]
    ; get character at position
    mov al, [es:di]
    
    ; compare with brick character (219 = solid block)
    ; compare with brick ascii code
    cmp al, 219
    
    ; restore di register
    pop di
    ; restore ax register
    pop ax
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; new: erase entire brick starting from hit position
; input: di = video memory offset where brick was hit
; erases left until space, then erases right until space
; also increments score
; -------------------------------
erase_brick:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; get hit position
    ; load hit position from stack
    mov di, [bp+4]
    ; save starting position
    mov si, di
    
    ; first, erase left from hit position until we find a space
.erase_brick_left_loop:
    ; get character at current position
    mov al, [es:di]
    ; is it a brick character?
    cmp al, 219
    ; if not brick, start erasing right
    jne .erase_brick_start_right
    
    ; erase this brick character
    ; write space character
    mov byte [es:di], ' '
    ; write black attribute
    mov byte [es:di+1], 0x00
    
    ; move left (2 bytes per character)
    ; move to previous character
    sub di, 2
    
    ; check if we're at the beginning of a row (prevent wrap-around)
    ; copy di to ax
    mov ax, di
    ; row width in bytes
    mov bx, 160
    ; clear dx for division
    xor dx, dx
    ; divide by row width
    div bx
    ; dx = offset within row, check if at row start
    cmp dx, 0
    ; continue if not at row start
    jge .erase_brick_left_loop
    
.erase_brick_start_right:
    ; now erase right from hit position
    ; restore starting position
    mov di, si
    ; move to next character (we already erased the hit pos)
    add di, 2
    
.erase_brick_right_loop:
    ; get character at current position
    mov al, [es:di]
    ; is it a brick character?
    cmp al, 219
    ; if not brick, update score and done
    jne .erase_brick_score_update
    
    ; erase this brick character
    ; write space character
    mov byte [es:di], ' '
    ; write black attribute
    mov byte [es:di+1], 0x00
    
    ; move right (2 bytes per character)
    ; move to next character
    add di, 2
    
    ; copy di to ax
    mov ax, di
    ; row width in bytes
    mov bx, 160
    ; clear dx for division
    xor dx, dx
    ; divide by row width
    div bx
    ; dx = offset within row, last character position in row
    cmp dx, 158
    ; continue if not at row end
    jle .erase_brick_right_loop
    
.erase_brick_score_update:
    ; brick destroyed, increment score
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; call score increment routine
    call increment_score
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; prints/initializes the screen and static elements (paddle)
; -------------------------------
printscreen:
    ; push all general-purpose registers onto the stack (save context)
    pusha
    
    ; load the segment address of the video ram (text mode)
    mov ax, 0xb800
    ; set es (extra segment) register to video ram segment
    mov es, ax
    ; set di (destination index) to 0 (start of video memory)
    mov di, 0
    
    ; clear screen
    ; load ascii space character into al
    mov al, ' '
    ; load attribute byte 0x00 (black on black) into ah
    mov ah, 0x00
    ; clear the direction flag (df), causing string operations to increment di (forward)
    cld
    ; set cx (loop counter) to 2000 (80 columns * 25 rows = 2000 character/attribute pairs)
    mov cx, 2000
    ; repeat (cx times) store word (al=char, ah=attrib) at [es:di] and increment di by 2
    rep stosw
    
    ; draw bottom paddle at offset 3840 (the second to last row)
    ; push the starting offset for the paddle onto the stack
    push word 3840
    ; call the routine to draw the paddle
    call draw_paddle
    
    ; restore all general-purpose registers from the stack
    popa
    ; return from the procedure
    ret

; draw paddle: 20 dots starting at [bp+4] (the address pushed before the call)
draw_paddle:
    ; save the current bp
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; save all general-purpose registers
    pusha
    
    ; load video ram segment address
    mov ax, 0xb800
    ; set es to video ram segment
    mov es, ax
    
    ; get the starting video memory offset for the paddle from the stack
    mov di, [bp+4]
    ; set cx (loop counter) to 20 (paddle width in characters)
    mov cx, 20
    
; draw loop
lp:
    ; load the word 0x772e ('.' character with light gray on cyan color)
    mov ax, 0x772E
    ; store the word (character/attribute) at the current video memory location
    mov [es:di], ax
    ; increment di by 2 (move to the next character/attribute pair)
    add di, 2
    ; decrement cx and jump back to lp if cx is not zero
    loop lp
    
    ; restore general-purpose registers
    popa
    ; restore bp
    pop bp
    ; return from the procedure and discard 2 bytes (the pushed address) from the stack
    ret 2

; -------------------------------
; draws the ball at the specified location and updates ball_loc
; -------------------------------
print_ball:
    ; save the current bp
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; save all general-purpose registers
    pusha
    
    ; load video ram segment address
    mov ax, 0xb800
    ; set es to video ram segment
    mov es, ax
    ; load attribute byte 0x07 (light gray on black) into ah (for ball and erase)
    mov ah, 0x07
    
    ; erase old ball
    ; load ascii space character into al
    mov al, ' '
    ; load the video offset of the ball's previous position
    mov di, [ball_loc]
    ; erase the old ball position by writing a space character with ah's attribute
    mov [es:di], ax
    
    ; draw new ball
    ; load ascii asterisk character into al
    mov al, '*'
    ; get the new ball offset from the stack
    mov di, [bp+4]
    ; draw the new ball
    mov [es:di], ax
    ; save the new ball offset to the ball_loc variable
    mov [ball_loc], di
    
    ; restore general-purpose registers
    popa
    ; restore bp
    pop bp
    ; return and discard 2 bytes from the stack
    ret 2

; -------------------------------
; a simple time delay loop
; -------------------------------
delay:
    ; save cx
    push cx
    ; load cx with a large value
    mov cx, 0xfff
    
.delay_d:
    ; no operation (wastes time)
    nop
    ; no operation (wastes time)
    nop
    ; decrement cx and loop if not zero
    loop .delay_d
    
    ; load cx with an even larger value
    mov cx, 0xffff
    
.delay_dd:
    ; no operation
    nop
    ; decrement cx and loop if not zero
    loop .delay_dd
    
    ; restore cx
    pop cx
    ; return
    ret

; -------------------------------
; move ball down (corrected with brick collision)
; controls the ball's downward movement until a bounce or miss
; -------------------------------
move_ball_down:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    ; load attribute byte for ball
    mov ah, 0x07
    ; get ball position from stack
    mov di, [bp+4]
    
.move_down_l1:
    ; call delay routine
    call delay
    ; call delay routine again
    call delay
    
    ; --- erase old ball ---
    ; load space character
    mov al, ' '
    ; get current ball location
    mov bx, [ball_loc]
    ; erase ball at old position
    mov [es:bx], ax
    
    ; --- calculate new position ---
    ; check if moving straight down
    cmp word [straight_down], 1
    ; if yes, move straight
    je .move_down_straight
    ; move diagonally down-right (160 + 2)
    add di, 162
    ; check for brick collision
    jmp .move_down_check_brick
    
.move_down_straight:
    ; move straight down (next row)
    add di, 160
    
.move_down_check_brick:
    ; new: check if next position has a brick
    ; push position to check
    push di
    ; call brick detection routine
    call is_brick
    ; if no brick, continue
    jne .move_down_no_brick_hit
    
    ; brick found! erase entire brick and bounce
    ; push position to erase
    push di
    ; call brick erasing routine
    call erase_brick
    
    ; bounce back diagonally - switch to opposite diagonal
    ; set down direction change flag
    mov word [change_down_direction], 1
    ; switch to opposite diagonal
    jmp move_down_sw
    
.move_down_no_brick_hit:
    ; --- draw new ball ---
    ; load ball character
    mov al, '*'
    ; draw ball at new position
    mov [es:di], ax
    ; update ball location
    mov [ball_loc], di
    
    ; --- check wall/paddle collisions ---
    ; push position to check
    push di
    ; check right wall collision
    call check_right_collision
    ; check if direction changed
    cmp word [change_down_direction], 1
    ; if yes, switch diagonal
    je move_down_sw
    
    ; push position to check
    push di
    ; check if ball hit paddle
    call check_player_hit
    ; check paddle hit flag
    cmp word [check_hit], 1
    ; if no hit, continue checking
    jne .move_down_no_paddle_hit
    
    ; paddle hit! reset movement flags
    ; clear straight down flag
    mov word [straight_down], 0
    ; clear up direction change
    mov word [change_up_direction], 0
    ; clear down direction change
    mov word [change_down_direction], 0
    ; move ball upward
    jmp move_down_done
    
.move_down_no_paddle_hit:
    ; check if left key pressed
    cmp word [left], 1
    ; if not, check right
    jne .move_down_r
    ; push paddle width
    push word 24
    ; slide paddle left
    call slide_left
    
.move_down_r:
    ; check if right key pressed
    cmp word [right], 1
    ; if not, continue
    jne .move_down_n
    ; push paddle width
    push word 24
    ; slide paddle right
    call slide_right
    
.move_down_n:
    ; check if ball reached bottom row (paddle row)
    cmp di, 3840
    ; if not, continue moving down
    jl .move_down_l1
    
    ; ball missed paddle - decrement lives
    ; call lives decrement routine
    call decrement_lives
    ; if lives = 0, game over
    jz game_over
    
    ; reset ball position above paddle
    ; push reset position
    push word 3720
    ; draw ball at reset position
    call print_ball
    ; clear straight down flag
    mov word [straight_down], 0
    ; finish downward movement
    jmp move_down_done
    
move_down_sw:
    ; call delay routine
    call delay
    ; call delay routine again
    call delay
    
    ; erase old ball
    ; load space character
    mov al, ' '
    ; get current ball location
    mov bx, [ball_loc]
    ; erase ball at old position
    mov [es:bx], ax
    
    ; move in opposite diagonal direction
    ; check if moving straight down
    cmp word [straight_down], 1
    ; if yes, move straight
    je .move_down_straight2
    ; move diagonally down-left (160 - 2)
    add di, 158
    ; check for brick collision
    jmp .move_down_check_brick2
    
.move_down_straight2:
    ; move straight down (next row)
    add di, 160
    
.move_down_check_brick2:
    ; new: check for brick in opposite diagonal direction
    ; push position to check
    push di
    ; call brick detection routine
    call is_brick
    ; if no brick, continue
    jne .move_down_no_brick_sw
    
    ; brick found! erase entire brick and continue in this diagonal
    ; push position to erase
    push di
    ; call brick erasing routine
    call erase_brick
    
.move_down_no_brick_sw:
    ; draw ball at new position
    ; load ball character
    mov al, '*'
    ; draw ball
    mov [es:di], ax
    ; update ball location
    mov [ball_loc], di
    
    ; check if paddle hit
    ; push position to check
    push di
    ; check paddle collision
    call check_player_hit
    ; check paddle hit flag
    cmp word [check_hit], 1
    ; if no hit, continue
    jne .move_down_no_paddle_sw_hit
    
    ; paddle hit! reset movement flags
    ; clear straight down flag
    mov word [straight_down], 0
    ; clear up direction change
    mov word [change_up_direction], 0
    ; clear down direction change
    mov word [change_down_direction], 0
    ; move ball upward
    jmp move_down_done
    
.move_down_no_paddle_sw_hit:
    ; check if left key pressed
    cmp word [left], 1
    ; if not, check right
    jne .move_down_r1
    ; push paddle width
    push word 24
    ; slide paddle left
    call slide_left
    
.move_down_r1:
    ; check if right key pressed
    cmp word [right], 1
    ; if not, continue
    jne .move_down_n1
    ; push paddle width
    push word 24
    ; slide paddle right
    call slide_right
    
.move_down_n1:
    ; check if ball reached bottom row
    cmp di, 3840
    ; if not, continue in opposite diagonal
    jl move_down_sw
    
    ; ball missed paddle - decrement lives
    ; call lives decrement routine
    call decrement_lives
    ; if lives = 0, game over
    jz game_over
    
    ; reset ball position
    ; push reset position
    push word 260
    ; draw ball at reset position
    call print_ball
    ; clear straight down flag
    mov word [straight_down], 0
    
move_down_done:
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; push current position for upward movement
    push di
    ; start moving ball upward
    call move_ball_up
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; move ball up (corrected with brick collision)
; controls the ball's upward movement until a bounce (top or wall)
; -------------------------------
move_ball_up:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    ; load attribute byte for ball
    mov ah, 0x07
    ; get ball position from stack
    mov di, [bp+4]
    
.move_up_l1:
    ; call delay routine
    call delay
    ; call delay routine again
    call delay
    
    ; erase old ball
    ; load space character
    mov al, ' '
    ; get current ball location
    mov bx, [ball_loc]
    ; erase ball at old position
    mov [es:bx], ax
    
    ; check for collisions before moving
    ; push position to check
    push di
    ; check right wall collision
    call check_right_collision
    ; check if direction changed
    cmp word [change_up_direction], 1
    ; if yes, switch diagonal
    je move_up_switch
    
    ; check if ball reached top row
    cmp di, 160
    ; if yes, bounce down
    jle move_up_bounce_down
    
    ; move diagonally up-right (160 - 2)
    sub di, 158
    
    ; new: check if next position has a brick
    ; push position to check
    push di
    ; call brick detection routine
    call is_brick
    ; if no brick, continue
    jne .move_up_no_brick_up
    
    ; brick found! erase entire brick and bounce down diagonally
    ; push position to erase
    push di
    ; call brick erasing routine
    call erase_brick
    
    ; bounce back down
    ; bounce down after hitting brick
    jmp move_up_bounce_down
    
.move_up_no_brick_up:
    ; draw ball at new position
    ; load ball character
    mov al, '*'
    ; draw ball
    mov [es:di], ax
    ; update ball location
    mov [ball_loc], di
    
    ; check paddle movement keys
    ; check if left key pressed
    cmp word [left], 1
    ; if not, check right
    jne .move_up_r
    ; push paddle width
    push word 24
    ; slide paddle left
    call slide_left
    
.move_up_r:
    ; check if right key pressed
    cmp word [right], 1
    ; if not, continue
    jne .move_up_n
    ; push paddle width
    push word 24
    ; slide paddle right
    call slide_right
    
.move_up_n:
    ; check if ball still above top row
    cmp di, 160
    ; if yes, continue moving up
    jg .move_up_l1
    
move_up_bounce_down:
    ; clear straight down flag
    mov word [straight_down], 0
    ; finish upward movement
    jmp move_up_done
    
move_up_switch:
    ; erase old ball
    ; load space character
    mov al, ' '
    ; get current ball location
    mov bx, [ball_loc]
    ; erase ball at old position
    mov [es:bx], ax
    
    ; move in opposite diagonal (up-left: 160 + 2)
    sub di, 162
    
    ; new: check for brick in opposite diagonal direction
    ; push position to check
    push di
    ; call brick detection routine
    call is_brick
    ; if no brick, continue
    jne .move_up_no_brick_sw
    
    ; brick found! erase entire brick and bounce down
    ; push position to erase
    push di
    ; call brick erasing routine
    call erase_brick
    ; bounce down after hitting brick
    jmp move_up_bounce_down
    
.move_up_no_brick_sw:
    ; draw ball at new position
    ; load ball character
    mov al, '*'
    ; draw ball
    mov [es:di], ax
    ; update ball location
    mov [ball_loc], di
    
    ; check paddle movement keys
    ; check if left key pressed
    cmp word [left], 1
    ; if not, check right
    jne move_up_r1
    ; push paddle width
    push word 24
    ; slide paddle left
    call slide_left
    
move_up_r1:
    ; check if right key pressed
    cmp word [right], 1
    ; if not, continue
    jne move_up_n1
    ; push paddle width
    push word 24
    ; slide paddle right
    call slide_right
    
move_up_n1:
    ; check if ball still above top row
    cmp di, 160
    ; if yes, continue in opposite diagonal
    jg move_up_switch
    
move_up_done:
    ; clear up direction change flag
    mov word [change_up_direction], 0
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; checks if the ball will hit the paddle in the next down step
; -------------------------------
check_player_hit:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; get ball position from stack
    mov di, [bp+4]
    ; calculate position one row below
    add di, 160
    ; get character/attribute at that position
    mov ax, [es:di]
    ; check if attribute is paddle color (0x77)
    cmp ah, 0x77
    ; if not paddle, ball missed
    jne .check_player_miss
    
    ; paddle hit detected
    ; set hit flag to 1
    mov word [check_hit], 1
    ; finish check
    jmp .check_player_done
    
.check_player_miss:
    ; no paddle hit
    ; set hit flag to 0
    mov word [check_hit], 0
    
.check_player_done:
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; checks if the ball is at the right edge of the screen
; -------------------------------
check_right_collision:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; get ball position from stack
    mov di, [bp+4]
    ; start from bottom right corner (row 24, col 79)
    mov bx, 3998
    ; check 23 rows
    mov cx, 23
    
.check_right_l1:
    ; check if ball is at right edge of this row
    cmp di, bx
    ; if not, check next row
    jne .check_right_next
    
    ; collision detected! set both direction flags
    ; set up direction change flag
    mov word [change_up_direction], 1
    ; set down direction change flag
    mov word [change_down_direction], 1
    ; finish check
    jmp .check_right_done
    
.check_right_next:
    ; move to previous row's right edge
    sub bx, 160
    ; decrement row counter and loop
    loop .check_right_l1
    
.check_right_done:
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; slides the paddle one position to the left
; -------------------------------
slide_left:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    ; load paddle character into ah
    mov ah, '.'
    ; get starting position from stack
    mov di, [bp+4]
    ; check 80 positions (full row width)
    mov cx, 80
    
    ; check if di is 0 (uninitialized)
    cmp di, 0
    ; if yes, start from paddle row
    je slide_left_p_left
    ; set to start of paddle row
    mov di, 3840
    
slide_left_p_left:
    ; get character/attribute at current position
    mov ax, [es:di]
    ; check if attribute is paddle color (0x77)
    cmp ah, 0x77
    ; if not paddle, check next position
    jne slide_left_next_left
    
    ; check if paddle is at leftmost position
    cmp di, 3840
    ; if yes, can't move left
    je slide_left_done_left
    
    ; move paddle one position left
    ; move to left position
    sub di, 2
    ; set paddle attribute
    mov ah, 0x77
    ; draw paddle at new position
    mov [es:di], ax
    
    ; erase rightmost paddle character
    ; move to right end of paddle
    add di, 40
    ; set erase attribute
    mov ah, 0x00
    ; erase character
    mov [es:di], ax
    ; finish sliding
    jmp slide_left_done_left
    
slide_left_next_left:
    ; move to next position to check
    add di, 2
    ; decrement counter and loop
    loop slide_left_p_left
    
slide_left_done_left:
    ; clear left key flag
    mov word [left], 0
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; slides the paddle one position to the right
; -------------------------------
slide_right:
    ; save current base pointer
    push bp
    ; set bp to sp (establish stack frame)
    mov bp, sp
    ; push all general-purpose registers onto the stack
    pusha
    
    ; load video memory segment
    mov ax, 0xb800
    ; set es to video memory
    mov es, ax
    ; load paddle character into ah
    mov ah, '.'
    ; get starting position from stack
    mov di, [bp+4]
    ; check 80 positions (full row width)
    mov cx, 80
    
    ; check if di is 0 (uninitialized)
    cmp di, 0
    ; if yes, start from paddle row
    je slide_right_p_right
    ; set to start of paddle row
    mov di, 3840
    
slide_right_p_right:
    ; get character/attribute at current position
    mov ax, [es:di]
    ; check if attribute is paddle color (0x77)
    cmp ah, 0x77
    ; if not paddle, check next position
    jne slide_right_next_right
    
    ; calculate rightmost paddle position
    ; copy di to bx
    mov bx, di
    ; add paddle width (20 chars * 2 bytes)
    add bx, 40
    ; check if at right edge (col 79)
    cmp bx, 3998
    ; if yes, can't move right
    jg slide_right_done_right
    
    ; erase leftmost paddle character
    ; set erase attribute
    mov ah, 0x00
    ; erase character
    mov [es:di], ax
    
    ; move paddle one position right
    ; move to right end of paddle
    add di, 40
    ; set paddle attribute
    mov ah, 0x77
    ; draw paddle at new position
    mov [es:di], ax
    ; finish sliding
    jmp slide_right_done_right
    
slide_right_next_right:
    ; move to next position to check
    add di, 2
    ; decrement counter and loop
    loop slide_right_p_right
    
slide_right_done_right:
    ; clear right key flag
    mov word [right], 0
    ; restore all general-purpose registers
    popa
    ; restore base pointer
    pop bp
    ; return and discard 2 bytes from stack
    ret 2

; -------------------------------
; keyboard interrupt service routine (isr) for key input
; -------------------------------
kbisr:
    ; save ax register
    push ax
    ; save di register
    push di
    
    ; read keyboard scan code from port 0x60
    in al, 0x60
    
    ; check if left arrow key pressed (scan code 0x4b)
    cmp al, 0x4B
    ; if not, check next key
    jne .kbisr_next
    ; set left movement flag
    mov word [left], 1
    ; exit isr
    jmp .kbisr_exit
    
.kbisr_next:
    ; check if right arrow key pressed (scan code 0x4d)
    cmp al, 0x4D
    ; if not, ignore key
    jne .kbisr_ignore
    ; set right movement flag
    mov word [right], 1
    ; exit isr
    jmp .kbisr_exit
    
.kbisr_ignore:
    ; restore di register
    pop di
    ; restore ax register
    pop ax
    ; chain to original keyboard isr
    jmp far [cs:oldisr]
    
.kbisr_exit:
    ; send end of interrupt signal to pic
    mov al, 0x20
    ; write to pic command port
    out 0x20, al
    ; restore di register
    pop di
    ; restore ax register
    pop ax
    ; return from interrupt
    iret

; draw 3 rows of 5-character bricks with 1-character spacing
draw_bricks:
    ; save ax register
    push ax
    ; save bx register
    push bx
    ; save cx register
    push cx
    ; save dx register
    push dx
    ; save di register
    push di
    ; save es register
    push es
    
    ; load video memory segment
    mov ax, 0xB800
    ; set es to video memory
    mov es, ax
    
    ; load brick character (solid block)
    mov al, 219
    ; save brick character in bl
    mov bl, al
    
    ; row 1 (top): attribute 0x0c (light red)
    ; start at row 0
    mov di, 0
    ; light red attribute
    mov ah, 0x0C
    ; 13 bricks per row
    mov cx, 13
    
draw_bricks_row1_loop:
    ; 5 characters per brick
    mov si, 5
    
.draw_bricks_row1:
    ; write brick character
    mov [es:di], bl
    ; write brick attribute
    mov [es:di+1], ah
    ; move to next character
    add di, 2
    ; decrement brick character counter
    dec si
    ; repeat until brick is complete
    jnz .draw_bricks_row1
    
    ; add spacing (1 character) between bricks
    add di, 2
    ; decrement brick counter and loop
    loop draw_bricks_row1_loop
    
    ; row 2: attribute 0x0e (yellow)
    ; start at row 2 (160 * 2)
    mov di, 160*2
    ; yellow attribute
    mov ah, 0x0E
    ; 13 bricks per row
    mov cx, 13
    
draw_bricks_row2_loop:
    ; 5 characters per brick
    mov si, 5
    
.draw_bricks_row2:
    ; write brick character
    mov [es:di], bl
    ; write brick attribute
    mov [es:di+1], ah
    ; move to next character
    add di, 2
    ; decrement brick character counter
    dec si
    ; repeat until brick is complete
    jnz .draw_bricks_row2
    
    ; add spacing (1 character) between bricks
    add di, 2
    ; decrement brick counter and loop
    loop draw_bricks_row2_loop
    
    ; row 3: attribute 0x0b (cyan)
    ; start at row 4 (160 * 4)
    mov di, 160*4
    ; cyan attribute
    mov ah, 0x0B
    ; 13 bricks per row
    mov cx, 13
    
draw_bricks_row3_loop:
    ; 5 characters per brick
    mov si, 5
    
.draw_bricks_row3:
    ; write brick character
    mov [es:di], bl
    ; write brick attribute
    mov [es:di+1], ah
    ; move to next character
    add di, 2
    ; decrement brick character counter
    dec si
    ; repeat until brick is complete
    jnz .draw_bricks_row3
    
    ; add spacing (1 character) between bricks
    add di, 2
    ; decrement brick counter and loop
    loop draw_bricks_row3_loop
    
    ; restore es register
    pop es
    ; restore di register
    pop di
    ; restore dx register
    pop dx
    ; restore cx register
    pop cx
    ; restore bx register
    pop bx
    ; restore ax register
    pop ax
    ; return from procedure
    ret

; -------------------------------
; program entry point
; -------------------------------
start:
    ; zero out ax register
    xor ax, ax
    ; set es to interrupt vector table segment
    mov es, ax
    
    ; save original keyboard interrupt vector
    ; get low word of int 9 vector
    mov ax, [es:9*4]
    ; save original offset
    mov [oldisr], ax
    ; get high word of int 9 vector
    mov ax, [es:9*4+2]
    ; save original segment
    mov [oldisr+2], ax
    
    ; install new keyboard interrupt handler
    ; disable interrupts during vector modification
    cli
    ; set new offset (our isr)
    mov word [es:9*4], kbisr
    ; set new segment (current code segment)
    mov word [es:9*4+2], cs
    ; enable interrupts
    sti
    
    ; initialize game screen
    ; draw paddle and clear screen
    call printscreen
    ; draw brick rows
    call draw_bricks
    
    ; display initial score and lives
    ; show score at top
    call display_score
    ; show lives at top
    call display_lives
    
    ; set initial ball position (above paddle center)
    mov word [ball_loc], 3770
    
    ; wait for keypress to start game
    ; bios keyboard input function
    mov ah, 0
    ; call bios keyboard interrupt
    int 0x16
    
main_loop:
    ; move ball upward phase
    ; push current ball location
    push word [ball_loc]
    ; call upward movement routine
    call move_ball_up
    
    ; move ball downward phase
    ; push current ball location
    push word [ball_loc]
    ; call downward movement routine
    call move_ball_down
    
    ; repeat game loop indefinitely
    jmp main_loop
    
quit:
    ; dos terminate program function
    mov ax, 0x4c00
    ; call dos interrupt
    int 0x21