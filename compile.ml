
open Format
open X86_64
open Ast

let debug = ref false

(* Label generation *)
let label_counter = ref 0
let new_label () =
  incr label_counter;
  sprintf ".L%d" !label_counter

(* String constants *)
let strings = Hashtbl.create 16
let string_counter = ref 0

let add_string s =
  try Hashtbl.find strings s
  with Not_found ->
    let lbl = sprintf ".STR%d" !string_counter in
    incr string_counter;
    Hashtbl.add strings s lbl;
    lbl

(* Class descriptor labels *)
let class_descriptor_label c = sprintf ".CLASS_%s" c.class_name

(* Method labels *)
let method_label c meth =
  sprintf ".METHOD_%s_%s" c.class_name meth.meth_name

(* Constructor labels *)
let constructor_label c = sprintf ".CONS_%s" c.class_name

(* Phase 4: Build class descriptors *)

(* Generate class descriptor *)
let gen_class_descriptor c =
  let lbl = class_descriptor_label c in

  (* Collect methods in order with proper override handling *)
  let rec collect_meths c =
    if c.class_name = "Object" then []
    else
      let parent_meths = collect_meths c.class_extends in
      (* For each parent method, check if it's overridden in this class *)
      let updated_parent_meths = List.map (fun pm ->
        try
          let override_meth = Hashtbl.find c.class_methods pm.meth_name in
          (* Check if this method was actually defined in this class (not inherited) *)
          match override_meth.meth_defining_class with
          | Some dc when dc.class_name = c.class_name -> override_meth
          | _ -> pm
        with Not_found -> pm
      ) parent_meths in
      (* Add methods that are new in this class *)
      let new_meths = Hashtbl.fold (fun name meth acc ->
        if not (List.exists (fun pm -> pm.meth_name = name) parent_meths) then
          meth :: acc
        else
          acc
      ) c.class_methods [] in
      updated_parent_meths @ new_meths
  in

  let meths = collect_meths c in

  let meth_labels = List.map (fun meth ->
    (* Use the defining class stored in the method record *)
    let impl_class = match meth.meth_defining_class with
      | Some c -> c
      | None -> c  (* Fallback, shouldn't happen *)
    in
    method_label impl_class meth
  ) meths in

  let parent_lbl = if c.class_name = "Object" then
    class_descriptor_label c
  else
    class_descriptor_label c.class_extends
  in

  label lbl ++
  address (parent_lbl :: meth_labels)

(* Phase 6: Compile expressions and statements *)

(* Wrapper functions for C library calls with 16-byte alignment *)
let gen_wrappers () =
  aligned_call_wrapper ~f:"malloc" ~newf:"my_malloc" ++
  aligned_call_wrapper ~f:"printf" ~newf:"my_printf" ++
  aligned_call_wrapper ~f:"sprintf" ~newf:"my_sprintf" ++
  aligned_call_wrapper ~f:"strcmp" ~newf:"my_strcmp" ++
  aligned_call_wrapper ~f:"strlen" ~newf:"my_strlen" ++
  aligned_call_wrapper ~f:"strcpy" ~newf:"my_strcpy" ++
  aligned_call_wrapper ~f:"strcat" ~newf:"my_strcat" ++
  aligned_call_wrapper ~f:"exit" ~newf:"my_exit"

(* Compile expression - result in %rax *)
let rec compile_expr env expr =
  match expr.expr_desc with
  | Econstant (Cint n) ->
      movq (imm32 n) (reg rax)

  | Econstant (Cbool b) ->
      movq (imm (if b then 1 else 0)) (reg rax)

  | Econstant (Cstring s) ->
      let lbl = add_string s in
      let len = String.length s in
      (* Allocate space for String object: 8 bytes for descriptor + n+1 bytes for string *)
      let size = 8 + len + 1 in
      let size_aligned = ((size + 7) / 8) * 8 in (* Round up to 8-byte boundary *)
      movq (imm size_aligned) (reg rdi) ++
      call "my_malloc" ++
      (* Set class descriptor *)
      leaq (lab ".CLASS_String") rbx ++
      movq (reg rbx) (ind rax) ++
      (* Copy string content *)
      pushq (reg rax) ++
      addq (imm 8) (reg rax) ++
      movq (reg rax) (reg rdi) ++
      leaq (lab lbl) rsi ++
      call "my_strcpy" ++
      popq rax

  | Ethis ->
      (* 'this' is always at 16(%rbp) *)
      movq (ind ~ofs:16 rbp) (reg rax)

  | Enull ->
      xorq (reg rax) (reg rax)

  | Evar v ->
      movq (ind ~ofs:v.var_ofs rbp) (reg rax)

  | Eattr (obj, attr) ->
      compile_expr env obj ++
      movq (ind ~ofs:attr.attr_ofs rax) (reg rax)

  | Eassign_var (v, e) ->
      compile_expr env e ++
      movq (reg rax) (ind ~ofs:v.var_ofs rbp)

  | Eassign_attr (obj, attr, e) ->
      compile_expr env obj ++
      pushq (reg rax) ++
      compile_expr env e ++
      popq rbx ++
      movq (reg rax) (ind ~ofs:attr.attr_ofs rbx)

  | Eunop (Uneg, e) ->
      compile_expr env e ++
      negq (reg rax)

  | Eunop (Unot, e) ->
      compile_expr env e ++
      xorq (imm 1) (reg rax)

  | Eunop (Ustring_of_int, e) ->
      compile_expr env e ++
      pushq (reg rax) ++
      (* Allocate buffer: 8 bytes descriptor + 32 bytes for string *)
      movq (imm 40) (reg rdi) ++
      call "my_malloc" ++
      movq (reg rax) (reg rbx) ++
      (* Set class descriptor *)
      leaq (lab ".CLASS_String") rcx ++
      movq (reg rcx) (ind rbx) ++
      (* sprintf *)
      addq (imm 8) (reg rbx) ++
      movq (reg rbx) (reg rdi) ++
      leaq (lab ".FMT_INT") rcx ++
      movq (reg rcx) (reg rsi) ++
      popq rdx ++
      xorq (reg rax) (reg rax) ++
      call "my_sprintf" ++
      movq (reg rbx) (reg rax) ++
      subq (imm 8) (reg rax)

  | Ebinop (Badd, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      addq (reg rbx) (reg rax)

  | Ebinop (Bsub, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      movq (reg rax) (reg rcx) ++
      popq rax ++
      subq (reg rcx) (reg rax)

  | Ebinop (Bmul, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      imulq (reg rbx) (reg rax)

  | Ebinop (Bdiv, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      movq (reg rax) (reg rcx) ++
      popq rax ++
      cqto ++
      idivq (reg rcx)

  | Ebinop (Bmod, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      movq (reg rax) (reg rcx) ++
      popq rax ++
      cqto ++
      idivq (reg rcx) ++
      movq (reg rdx) (reg rax)

  | Ebinop (Beq, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      sete (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Bneq, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      setne (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Blt, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      setl (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Ble, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      setle (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Bgt, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      setg (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Bge, e1, e2) ->
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      popq rbx ++
      cmpq (reg rax) (reg rbx) ++
      setge (reg (register8 rax)) ++
      movzbq (reg (register8 rax)) rax

  | Ebinop (Band, e1, e2) ->
      let lbl_false = new_label () in
      let lbl_end = new_label () in
      compile_expr env e1 ++
      testq (reg rax) (reg rax) ++
      je lbl_false ++
      compile_expr env e2 ++
      testq (reg rax) (reg rax) ++
      je lbl_false ++
      movq (imm 1) (reg rax) ++
      jmp lbl_end ++
      label lbl_false ++
      xorq (reg rax) (reg rax) ++
      label lbl_end

  | Ebinop (Bor, e1, e2) ->
      let lbl_true = new_label () in
      let lbl_end = new_label () in
      compile_expr env e1 ++
      testq (reg rax) (reg rax) ++
      jne lbl_true ++
      compile_expr env e2 ++
      testq (reg rax) (reg rax) ++
      jne lbl_true ++
      xorq (reg rax) (reg rax) ++
      jmp lbl_end ++
      label lbl_true ++
      movq (imm 1) (reg rax) ++
      label lbl_end

  | Ebinop (Badd_s, e1, e2) ->
      (* String concatenation: e1 + e2 *)
      compile_expr env e1 ++
      pushq (reg rax) ++
      compile_expr env e2 ++
      pushq (reg rax) ++
      (* Get lengths: e2 is at rsp+0, e1 is at rsp+8 *)
      movq (ind rsp) (reg rdi) ++
      addq (imm 8) (reg rdi) ++
      call "my_strlen" ++
      movq (reg rax) (reg r12) ++  (* r12 = len(e2) *)
      movq (ind ~ofs:8 rsp) (reg rdi) ++
      addq (imm 8) (reg rdi) ++
      call "my_strlen" ++           (* rax = len(e1) *)
      (* Allocate new string: len(e1) + len(e2) + 1 + 8 for descriptor *)
      addq (reg r12) (reg rax) ++
      addq (imm 9) (reg rax) ++
      movq (reg rax) (reg rdi) ++
      call "my_malloc" ++
      movq (reg rax) (reg r13) ++
      (* Set descriptor *)
      leaq (lab ".CLASS_String") rcx ++
      movq (reg rcx) (ind r13) ++
      (* Copy e1 first, then concatenate e2 *)
      addq (imm 8) (reg r13) ++      (* r13 points to string data *)
      movq (reg r13) (reg rdi) ++
      movq (ind ~ofs:8 rsp) (reg rsi) ++  (* Get e1 from rsp+8 *)
      addq (imm 8) (reg rsi) ++
      call "my_strcpy" ++            (* Copy e1 to result *)
      movq (reg r13) (reg rdi) ++
      movq (ind rsp) (reg rsi) ++    (* Get e2 from rsp+0 *)
      addq (imm 8) (reg rsi) ++
      call "my_strcat" ++            (* Concatenate e2 to result *)
      addq (imm 16) (reg rsp) ++     (* Pop both strings from stack *)
      movq (reg r13) (reg rax) ++
      subq (imm 8) (reg rax)         (* Return pointer to object (with descriptor) *)

  | Enew (c, args) ->
      (* Calculate object size *)
      let num_attrs = Hashtbl.length c.class_attributes in
      let size = 8 * (num_attrs + 1) in
      movq (imm size) (reg rdi) ++
      call "my_malloc" ++
      (* Set class descriptor *)
      pushq (reg rax) ++
      leaq (lab (class_descriptor_label c)) rbx ++
      popq rax ++
      movq (reg rbx) (ind rax) ++
      (* Initialize attributes to null/0 *)
      let init_code = ref nop in
      Hashtbl.iter (fun _ attr ->
        init_code := !init_code ++ movq (imm 0) (ind ~ofs:attr.attr_ofs rax)
      ) c.class_attributes;
      !init_code ++
      (* Call constructor *)
      pushq (reg rax) ++  (* Save object pointer *)
      (* Push arguments in reverse *)
      let push_args = List.fold_right (fun arg code ->
        compile_expr env arg ++
        pushq (reg rax) ++
        code
      ) args nop in
      push_args ++
      movq (ind ~ofs:(8 * List.length args) rsp) (reg rax) ++  (* Get object from stack *)
      pushq (reg rax) ++  (* Push this *)
      call (constructor_label c) ++
      (* Clean up stack *)
      addq (imm (8 * (List.length args + 1))) (reg rsp) ++
      (* Restore object pointer *)
      popq rax

  | Ecall (obj, meth, args) ->
      (* Special case: String.equals() *)
      if meth.meth_name = "equals" &&
         (match obj.expr_type with Tclass c -> c.class_name = "String" | _ -> false) then begin
        (* String.equals(String) - use strcmp *)
        match args with
        | [arg] ->
            (* Compile obj *)
            compile_expr env obj ++
            pushq (reg rax) ++
            (* Compile arg *)
            compile_expr env arg ++
            pushq (reg rax) ++
            (* Call strcmp(obj+8, arg+8) - skip class descriptor *)
            movq (ind ~ofs:8 rsp) (reg rdi) ++  (* obj *)
            addq (imm 8) (reg rdi) ++           (* skip descriptor *)
            movq (ind rsp) (reg rsi) ++         (* arg *)
            addq (imm 8) (reg rsi) ++           (* skip descriptor *)
            call "my_strcmp" ++
            (* strcmp returns 0 if equal, convert to boolean *)
            testq (reg rax) (reg rax) ++
            sete (reg (register8 rax)) ++
            movzbq (reg (register8 rax)) rax ++
            (* Clean up stack *)
            addq (imm 16) (reg rsp)
        | _ -> failwith "String.equals should have exactly one argument"
      end
      else begin
        (* Normal method call *)
        (* Evaluate object *)
        compile_expr env obj ++
        (* Check for null *)
        testq (reg rax) (reg rax) ++
        let lbl_ok = new_label () in
        jne lbl_ok ++
        (* Null pointer error *)
        movq (imm 1) (reg rdi) ++
        call "my_exit" ++
        label lbl_ok ++
        (* Save object pointer on stack *)
        pushq (reg rax) ++
        (* Push arguments in reverse order *)
        let push_args = List.fold_right (fun arg code ->
          compile_expr env arg ++
          pushq (reg rax) ++
          code
        ) args nop in
        push_args ++
        (* Push this (object pointer from bottom of arg stack) *)
        movq (ind ~ofs:(8 * List.length args) rsp) (reg rbx) ++
        pushq (reg rbx) ++
        (* Get method address from vtable *)
        movq (ind rbx) (reg rax) ++
        call_star (ind ~ofs:meth.meth_ofs rax) ++
        (* Clean up stack: args + this + saved object *)
        addq (imm (8 * (List.length args + 2))) (reg rsp)
      end

  | Ecast (c, e) ->
      compile_expr env e ++
      (* Runtime cast check *)
      testq (reg rax) (reg rax) ++
      let lbl_null = new_label () in
      let lbl_ok = new_label () in
      let lbl_loop = new_label () in
      je lbl_null ++
      (* Check class hierarchy *)
      movq (ind rax) (reg rbx) ++
      leaq (lab (class_descriptor_label c)) rcx ++
      label lbl_loop ++
      cmpq (reg rcx) (reg rbx) ++
      je lbl_ok ++
      movq (ind rbx) (reg rbx) ++
      leaq (lab ".CLASS_Object") rdx ++
      cmpq (reg rdx) (reg rbx) ++
      jne lbl_loop ++
      cmpq (reg rcx) (reg rbx) ++
      je lbl_ok ++
      (* Cast failed *)
      leaq (lab ".STR_CAST_FAIL") rdi ++
      call "my_printf" ++
      movq (imm 1) (reg rdi) ++
      call "my_exit" ++
      label lbl_ok ++
      label lbl_null

  | Einstanceof (e, classname) ->
      compile_expr env e ++
      (* Check for null *)
      testq (reg rax) (reg rax) ++
      let lbl_false = new_label () in
      let lbl_true = new_label () in
      let lbl_end = new_label () in
      let lbl_loop = new_label () in
      je lbl_false ++
      (* Check class hierarchy *)
      movq (ind rax) (reg rbx) ++
      leaq (lab (sprintf ".CLASS_%s" classname)) rcx ++
      label lbl_loop ++
      cmpq (reg rcx) (reg rbx) ++
      je lbl_true ++
      movq (ind rbx) (reg rbx) ++
      leaq (lab ".CLASS_Object") rdx ++
      cmpq (reg rdx) (reg rbx) ++
      jne lbl_loop ++
      cmpq (reg rcx) (reg rbx) ++
      je lbl_true ++
      label lbl_false ++
      xorq (reg rax) (reg rax) ++
      jmp lbl_end ++
      label lbl_true ++
      movq (imm 1) (reg rax) ++
      label lbl_end

  | Eprint e ->
      compile_expr env e ++
      addq (imm 8) (reg rax) ++
      movq (reg rax) (reg rdi) ++
      call "my_printf"

(* Compile statement *)
let rec compile_stmt env next_ofs stmt =
  match stmt with
  | Sexpr e ->
      compile_expr env e, next_ofs

  | Svar (v, e) ->
      (* Allocate space on stack *)
      v.var_ofs <- next_ofs;
      let code = compile_expr env e ++
                 movq (reg rax) (ind ~ofs:next_ofs rbp) in
      code, next_ofs - 8

  | Sif (cond, s1, s2) ->
      let lbl_else = new_label () in
      let lbl_end = new_label () in
      let code_cond = compile_expr env cond in
      let code_s1, _ = compile_stmt env next_ofs s1 in
      let code_s2, _ = compile_stmt env next_ofs s2 in
      (code_cond ++
       testq (reg rax) (reg rax) ++
       je lbl_else ++
       code_s1 ++
       jmp lbl_end ++
       label lbl_else ++
       code_s2 ++
       label lbl_end), next_ofs

  | Sreturn None ->
      nop, next_ofs

  | Sreturn (Some e) ->
      compile_expr env e, next_ofs

  | Sblock stmts ->
      let code, ofs = List.fold_left (fun (code_acc, ofs) s ->
        let code_s, new_ofs = compile_stmt env ofs s in
        (code_acc ++ code_s, new_ofs)
      ) (nop, next_ofs) stmts in
      code, ofs

  | Sfor (init, cond, incr, body) ->
      let lbl_cond = new_label () in
      let lbl_end = new_label () in
      let code_init, ofs1 = compile_stmt env next_ofs init in
      let code_cond = compile_expr env cond in
      let code_incr, _ = compile_stmt env ofs1 incr in
      let code_body, _ = compile_stmt env ofs1 body in
      (code_init ++
       label lbl_cond ++
       code_cond ++
       testq (reg rax) (reg rax) ++
       je lbl_end ++
       code_body ++
       code_incr ++
       jmp lbl_cond ++
       label lbl_end), ofs1

(* Compile method or constructor *)
let compile_method c meth_opt params body =
  let lbl = match meth_opt with
    | None -> constructor_label c
    | Some m -> method_label c m
  in

  (* Set up parameter offsets *)
  (* this is at rbp+16, parameters start at rbp+24 *)
  (* Parameters are pushed left-to-right, so first param is at highest offset *)
  let n = List.length params in
  List.iteri (fun i p ->
    p.var_ofs <- 24 + 8 * (n - 1 - i)
  ) params;

  (* Compile body *)
  let code_body, final_ofs = compile_stmt [] (-8) body in

  (* Calculate stack space needed *)
  let stack_space = -final_ofs in
  let stack_space_aligned = ((stack_space + 15) / 16) * 16 in

  label lbl ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  subq (imm stack_space_aligned) (reg rsp) ++
  code_body ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

(* Compile a class *)
let compile_class (c, decls) =
  (* Offsets are already set in typing.ml *)

  (* Check if class has a constructor *)
  let has_constructor = List.exists (function
    | Dconstructor _ -> true
    | _ -> false
  ) decls in

  (* Generate default constructor if needed *)
  let default_cons = if not has_constructor then
    let empty_body = Sblock [] in
    compile_method c None [] empty_body
  else nop in

  let code = List.fold_left (fun acc decl ->
    match decl with
    | Dconstructor (params, body) ->
        acc ++ compile_method c None params body
    | Dmethod (meth, body) ->
        acc ++ compile_method c (Some meth) meth.meth_params body
  ) default_cons decls in

  code

(* Main compilation function *)
let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;

  (* Reset counters *)
  label_counter := 0;
  string_counter := 0;
  Hashtbl.clear strings;

  (* Generate predefined class descriptors *)
  let predefined_descriptors =
    (* Object class descriptor *)
    label ".CLASS_Object" ++
    address [".CLASS_Object"] ++
    (* String class descriptor *)
    label ".CLASS_String" ++
    address [".CLASS_Object"] in

  (* Generate class descriptors *)
  let descriptors = List.fold_left (fun acc (c, _) ->
    acc ++ gen_class_descriptor c
  ) predefined_descriptors p in

  (* Compile all classes *)
  let code = List.fold_left (fun acc cls ->
    acc ++ compile_class cls
  ) nop p in

  (* Generate string constants (after compilation, so strings are collected) *)
  let string_data = Hashtbl.fold (fun str lbl acc ->
    acc ++ label lbl ++ string str
  ) strings nop in

  (* Special strings *)
  let special_strings =
    label ".STR_CAST_FAIL" ++ string "cast failure\n" ++
    label ".FMT_INT" ++ string "%ld" in

  (* Generate wrappers *)
  let wrappers = gen_wrappers () in

  { text = globl "main" ++ label "main" ++
           call ".METHOD_Main_main" ++
           xorq (reg rax) (reg rax) ++
           ret ++
           code ++
           wrappers;
    data = descriptors ++ string_data ++ special_strings }
