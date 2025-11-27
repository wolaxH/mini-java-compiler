# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Mini Java compiler written in OCaml that compiles a subset of Java to x86-64 assembly language. The compiler implements a complete compilation pipeline: lexing, parsing, type checking, and code generation.

## Build and Development Commands

### Building
```bash
# Build the compiler executable
dune build minijava.exe

# Or use the Makefile
make minijava.exe
```

### Running the Compiler
```bash
# Compile a Java file (generates .s assembly file)
./minijava.exe test.java

# With debug output
./minijava.exe --debug test.java

# Stop after parsing only
./minijava.exe --parse-only test.java

# Stop after type checking
./minijava.exe --type-only test.java
```

### Testing
```bash
# Build, compile test.java, assemble, and run
make all

# This runs:
# 1. dune build minijava.exe
# 2. ./minijava.exe --debug test.java (generates test.s)
# 3. gcc -no-pie -g test.s && ./a.out
```

### Cleaning
```bash
dune clean
# Or
make clean
```

## Architecture

### Compilation Pipeline

The compiler follows a four-phase architecture:

1. **Lexing** (`lexer.mll`) → **Parsing** (`parser.mly`) → **Typed AST** (`ast.ml`)
2. **Type Checking** (`typing.ml`) → **Typed AST with offsets**
3. **Code Generation** (`compile.ml`) → **x86-64 Assembly** (`x86_64.ml`)
4. **Assembly Output** → `.s` file

### Key Modules

- **`minijava.ml`**: Entry point, orchestrates the compilation pipeline and error handling
- **`ast.ml`**: Defines both parsed AST (`pexpr`, `pstmt`, `pclass`) and typed AST (`expr`, `stmt`, `tclass`). Types include mutable offset fields for code generation.
- **`lexer.mll`**: OCamllex lexer specification
- **`parser.mly`**: Menhir parser specification with operator precedence
- **`typing.ml`**: Multi-phase type checker with critical architecture:
  - Phase 1: Declare all classes in global table
  - Phase 2: Set up inheritance relations and check for cycles
  - Phase 3: Declare members (attributes, constructors, methods) with inheritance
  - Phase 4: Type check method/constructor bodies
  - Sets `attr_ofs` and `meth_ofs` for runtime layout
- **`compile.ml`**: Code generator targeting x86-64:
  - Generates class descriptors (vtables) with proper method ordering
  - Handles method dispatch, object allocation, string operations
  - Uses 16-byte aligned stack and C library wrappers
- **`x86_64.ml[i]`**: x86-64 instruction builder DSL

### Type System Architecture

The type checker maintains:
- **Global class table**: `(string, class_) Hashtbl.t` with predefined `Object` and `String`
- **Current class context**: `current_class` ref for attribute/method resolution
- **Local environment**: `(string, var) Hashtbl.t` for local variables in methods
- **Subtyping**: Implements Java-like subtyping with null compatibility
- **Method override validation**: Checks signature compatibility during member declaration

### Code Generation Details

**Object Layout**:
- Offset 0: Class descriptor pointer (vtable)
- Offset 8+: Attributes in inheritance order

**Method Calling Convention**:
- Stack grows downward, 16-byte aligned
- `this` at `rbp+16`, parameters at `rbp+24+` (reverse order)
- Return value in `%rax`

**Special Handling**:
- String concatenation uses `Badd_s` binop inserted during type checking
- `System.out.print()` mapped to `Eprint` expr with auto-conversion
- `String.equals()` compiled to C `strcmp` call
- Class descriptors contain parent pointer followed by method addresses

### Predefined Classes

- **Object**: Base class for all classes (circular `class_extends` to itself)
- **String**: Extends Object, has `equals(String): boolean` method

## Common Development Patterns

### Adding New Language Features

1. Update `ast.ml` with new AST constructors in both parsed and typed variants
2. Add lexer tokens in `lexer.mll` if needed
3. Update parser rules in `parser.mly` with proper precedence
4. Implement type checking in `typing.ml`:
   - Handle in `type_expr` or `type_stmt`
   - Ensure proper type compatibility and error messages
5. Generate code in `compile.ml`:
   - Handle in `compile_expr` or `compile_stmt`
   - Follow x86-64 calling conventions

### Error Handling

Use `error ~loc "message"` in `typing.ml` which raises `Error of location * string`. The main module catches this and reports file/line/column information.

### Working with the Type System

- Types are structural: use `type_equal` for equality, not `=`
- Class types share references: modifying `class_` affects all references
- Variables, attributes, methods use mutable `_ofs` fields set during typing
- Offsets are relative: attributes to object pointer, variables to `%rbp`

## Dune Build System

- OCaml 3.13+ with Menhir 3.0
- `wrapped_executables false` for simpler module naming
- Executable modules: `minijava ast lexer parser typing compile x86_64`
- Debug symbols enabled with `-g` flag
