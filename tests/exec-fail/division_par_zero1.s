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
	movq $1, %rax
	pushq %rax
	movq $0, %rax
	movq %rax, %rcx
	popq %rax
	cqto
	idivq %rcx
	movq %rax, -8(%rbp)
	movq $24, %rdi
	call my_malloc
	leaq .CLASS_String, %rbx
	movq %rbx, 0(%rax)
	pushq %rax
	addq $8, %rax
	movq %rax, %rdi
	leaq .STR0, %rsi
	call my_strcpy
	popq %rax
	addq $8, %rax
	movq %rax, %rdi
	call my_printf
.L2:
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
.STR0:
	.string "hello world\n"
.STR_CAST_FAIL:
	.string "cast failure\n"
.FMT_INT:
	.string "%ld"
