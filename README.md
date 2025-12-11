# mini-java-compiler
a mini java compiler implement with ocaml  
for NTUT Compile cource final project

## introduce
This project is a compiler that compile java code to assembly  
this compiler does not include Java Class Library just some basic function  
Some function is call C standard library eg. System.out.print() -> printf()

## build
you can just type `make` to build this compiler and compile `test.java` and run the output

or you can type:
```
dune build minijava.exe
```
to build this project 

```
./minijava.exe --debug <filename>.java
```
to compile the java to assembly

```
gcc -no-pie -g test.s && ./a.out
```
to linked and run the output
