	.text
	.globl	main
main:
	call .METHOD_Main_main
	xorq %rax, %rax
	ret
.CONS_Main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
.L1:
	movq %rbp, %rsp
	popq %rbp
	ret
.METHOD_Main_main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $8, %rdi
	call my_malloc
	pushq %rax
	leaq .CLASS_A, %rbx
	popq %rax
	movq %rbx, 0(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	pushq %rax
	call .CONS_A
	addq $8, %rsp
	popq %rax
	testq %rax, %rax
	je .L6
	movq 0(%rax), %rbx
	leaq .CLASS_Object, %rcx
.L8:
	cmpq %rcx, %rbx
	je .L7
	movq 0(%rbx), %rbx
	leaq .CLASS_Object, %rdx
	cmpq %rdx, %rbx
	jne .L8
	cmpq %rcx, %rbx
	je .L7
	leaq .STR_CAST_FAIL, %rdi
	call my_printf
	movq $1, %rdi
	call my_exit
.L7:
.L6:
	testq %rax, %rax
	je .L3
	movq 0(%rax), %rbx
	leaq .CLASS_String, %rcx
.L5:
	cmpq %rcx, %rbx
	je .L4
	movq 0(%rbx), %rbx
	leaq .CLASS_Object, %rdx
	cmpq %rdx, %rbx
	jne .L5
	cmpq %rcx, %rbx
	je .L4
	leaq .STR_CAST_FAIL, %rdi
	call my_printf
	movq $1, %rdi
	call my_exit
.L4:
.L3:
	movq %rax, -8(%rbp)
.L2:
	movq %rbp, %rsp
	popq %rbp
	ret
.CONS_A:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
.L9:
	movq %rbp, %rsp
	popq %rbp
	ret
my_malloc:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    malloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_printf:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    printf
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_sprintf:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    sprintf
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_strcmp:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcmp
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_strlen:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strlen
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_strcpy:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcpy
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_strcat:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    strcat
	movq    %rbp, %rsp
	popq    %rbp
	ret
my_exit:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    exit
	movq    %rbp, %rsp
	popq    %rbp
	ret
	.data
.CLASS_Object:
	.quad .CLASS_Object
.CLASS_String:
	.quad .CLASS_Object
.CLASS_Main:
	.quad .CLASS_Object, .METHOD_Main_main
.CLASS_A:
	.quad .CLASS_Object
.STR_CAST_FAIL:
	.string "cast failure\n"
.FMT_INT:
	.string "%ld"
