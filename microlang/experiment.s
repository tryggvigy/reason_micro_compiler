extern printf
extern scanf

section .data
    inf: db '%d', 0
    ouf: db '%d', 10, 0

section .text
    global main

main:
    sub   esp, 1000
    mov   dword[esp+0], 1
    mov   eax, dword[esp+0]
    mov   dword[esp+8], eax
    add   dword[esp+8], 1
    mov   eax, dword[esp+8]
    mov   dword[esp+4], eax
    mov   eax, dword[esp+4]
    mov   dword[esp+8], eax
    add   dword[esp+8], 1
    mov   eax, dword[esp+8]
    mov   dword[esp+4], eax
    push  dword[esp+0]
    push  ouf
    call  printf
    add   esp, 8
    push  dword[esp+4]
    push  ouf
    call  printf
    add   esp, 8
    lea  eax, [esp+0]
    push  eax
    push  inf
    call  scanf
    add   esp, 8
    lea  eax, [esp+4]
    push  eax
    push  inf
    call  scanf
    add   esp, 8
    mov   eax, dword[esp+0]
    mov   dword[esp+8], eax
    add   dword[esp+8], 10
    push  dword[esp+8]
    push  ouf
    call  printf
    add   esp, 8
    mov   eax, dword[esp+4]
    mov   dword[esp+8], eax
    add   dword[esp+8], 10
    push  dword[esp+8]
    push  ouf
    call  printf
    add   esp, 8
    add   esp, 1000
exit:
    mov  eax, 1 ; sys_exit
    mov  ebx, 0
    int  80h
