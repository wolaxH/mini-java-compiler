
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
     error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* Global class table *)
let classes : (string, class_) Hashtbl.t = Hashtbl.create 16

(* Predefined classes: Object and String *)
let class_Object = {
  class_name = "Object";
  class_extends = Obj.magic (); (* will be set to itself *)
  class_methods = Hashtbl.create 16;
  class_attributes = Hashtbl.create 16;
}

let class_String = {
  class_name = "String";
  class_extends = class_Object;
  class_methods = Hashtbl.create 16;
  class_attributes = Hashtbl.create 16;
}

let () =
  class_Object.class_extends <- class_Object;

  (* Add equals method to String *)
  let equals_method = {
    meth_name = "equals";
    meth_type = Tboolean;
    meth_params = [{var_name = "s"; var_type = Tclass class_String; var_ofs = 0}];
    meth_ofs = 0;
    meth_defining_class = Some class_String;
  } in
  Hashtbl.add class_String.class_methods "equals" equals_method;

  Hashtbl.add classes "Object" class_Object;
  Hashtbl.add classes "String" class_String

(* Current class being type-checked *)
let current_class = ref class_Object

(* Check if a type is well-formed *)
let rec type_wf = function
  | Tvoid | Tboolean | Tint | Tnull -> true
  | Tclass c ->
      c.class_name = "Object" || c.class_name = "String" ||
      Hashtbl.mem classes c.class_name

(* Convert parsed type to actual type *)
let rec ptype_to_type ?(loc=dummy_loc) = function
  | PTboolean -> Tboolean
  | PTint -> Tint
  | PTident id ->
      if not (Hashtbl.mem classes id.id) then
        error ~loc:id.loc "unknown class %s" id.id;
      Tclass (Hashtbl.find classes id.id)

(* Subtyping relation *)
let rec subtype t1 t2 = match t1, t2 with
  | Tboolean, Tboolean | Tint, Tint -> true
  | Tnull, Tclass _ -> true
  | Tclass c1, Tclass c2 ->
      let rec extends c =
        c.class_name = c2.class_name ||
        (c.class_name <> "Object" && extends c.class_extends)
      in
      extends c1
  | _, _ -> false

(* Check if types are compatible *)
let compatible t1 t2 = subtype t1 t2 || subtype t2 t1

(* Type equality check - use this instead of structural equality *)
let type_equal t1 t2 = match t1, t2 with
  | Tvoid, Tvoid | Tnull, Tnull | Tboolean, Tboolean | Tint, Tint -> true
  | Tclass c1, Tclass c2 -> c1.class_name = c2.class_name
  | _, _ -> false

(* Phase 1: Declare all classes and check uniqueness *)
let declare_classes (pfile : pfile) =
  List.iter (fun (id, _, _) ->
    if Hashtbl.mem classes id.id then
      error ~loc:id.loc "class %s already defined" id.id;
    if id.id = "Object" || id.id = "String" then
      error ~loc:id.loc "cannot redefine predefined class %s" id.id;
    let c = {
      class_name = id.id;
      class_extends = class_Object; (* default, will be updated *)
      class_methods = Hashtbl.create 16;
      class_attributes = Hashtbl.create 16;
    } in
    Hashtbl.add classes id.id c
  ) pfile

(* Phase 2: Set up inheritance and declare members *)
let setup_inheritance (pfile : pfile) =
  (* First pass: set up extends relations *)
  List.iter (fun (id, extends_opt, _) ->
    let c = Hashtbl.find classes id.id in
    match extends_opt with
    | None -> c.class_extends <- class_Object
    | Some parent_id ->
        if parent_id.id = "String" then
          error ~loc:parent_id.loc "cannot extend String class";
        if not (Hashtbl.mem classes parent_id.id) then
          error ~loc:parent_id.loc "unknown class %s" parent_id.id;
        if parent_id.id = id.id then
          error ~loc:parent_id.loc "class cannot extend itself";
        c.class_extends <- Hashtbl.find classes parent_id.id
  ) pfile;

  (* Check for cycles in inheritance *)
  Hashtbl.iter (fun name c ->
    let rec check_cycle visited cur =
      if List.mem cur.class_name visited then
        error "cyclic inheritance involving class %s" name;
      if cur.class_name = "Object" then ()
      else check_cycle (cur.class_name :: visited) cur.class_extends
    in
    if name <> "Object" && name <> "String" then
      check_cycle [] c
  ) classes

(* Declare attributes, constructors, and methods *)
let declare_members (pfile : pfile) =
  (* Build a map from class name to its decls *)
  let class_decls = Hashtbl.create 16 in
  List.iter (fun (id, _, decls) ->
    Hashtbl.add class_decls id.id decls
  ) pfile;

  (* Track which classes have been processed *)
  let processed = Hashtbl.create 16 in

  (* Process a class and all its ancestors *)
  let rec process_class class_name =
    if Hashtbl.mem processed class_name then
      ()  (* Already processed *)
    else begin
      let c = Hashtbl.find classes class_name in

      (* Process parent first *)
      if c.class_extends.class_name <> c.class_name then
        process_class c.class_extends.class_name;

      (* Mark as processed before inheriting to avoid cycles *)
      Hashtbl.add processed class_name ();

      current_class := c;

      (* Inherit attributes from parent *)
      if c.class_extends.class_name <> c.class_name then begin
        Hashtbl.iter (fun name attr ->
          if not (Hashtbl.mem c.class_attributes name) then
            Hashtbl.add c.class_attributes name attr
        ) c.class_extends.class_attributes;

        (* Inherit methods from parent *)
        Hashtbl.iter (fun name meth ->
          if not (Hashtbl.mem c.class_methods name) then
            Hashtbl.add c.class_methods name meth
        ) c.class_extends.class_methods
      end;

      (* Now process this class's own members *)
      let decls = try Hashtbl.find class_decls class_name with Not_found -> [] in
      let constructor_count = ref 0 in

      List.iter (fun decl ->
        match decl with
        | PDattribute (pty, attr_id) ->
            if Hashtbl.mem c.class_attributes attr_id.id then
              error ~loc:attr_id.loc "attribute %s already defined in class or parent" attr_id.id;
            let ty = ptype_to_type ~loc:attr_id.loc pty in
            if not (type_wf ty) then
              error ~loc:attr_id.loc "type is not well-formed";
            let attr = {
              attr_name = attr_id.id;
              attr_type = ty;
              attr_ofs = 0; (* will be set later *)
            } in
            Hashtbl.add c.class_attributes attr_id.id attr

        | PDconstructor (cons_id, params, _) ->
            incr constructor_count;
            if !constructor_count > 1 then
              error ~loc:cons_id.loc "class can have at most one constructor";
            if cons_id.id <> c.class_name then
              error ~loc:cons_id.loc "constructor name must match class name"

        | PDmethod (ret_ty_opt, meth_id, params, _) ->
            let ret_ty = match ret_ty_opt with
              | None -> Tvoid
              | Some pty -> ptype_to_type ~loc:meth_id.loc pty
            in

            (* Check parameter types *)
            let param_types = List.map (fun (pty, pid) ->
              ptype_to_type ~loc:pid.loc pty
            ) params in

            (* Check if method already exists (overriding check) *)
            let is_override = Hashtbl.mem c.class_methods meth_id.id in
            if is_override then begin
              let parent_meth = Hashtbl.find c.class_methods meth_id.id in
              (* Check same signature *)
              if parent_meth.meth_type <> ret_ty then
                error ~loc:meth_id.loc "overriding method must have same return type";
              if List.length parent_meth.meth_params <> List.length param_types then
                error ~loc:meth_id.loc "overriding method must have same number of parameters";
              List.iter2 (fun pv pt ->
                if pv.var_type <> pt then
                  error ~loc:meth_id.loc "overriding method must have same parameter types"
              ) parent_meth.meth_params param_types
            end;

            let param_vars = List.map2 (fun (_, pid) pty ->
              {var_name = pid.id; var_type = pty; var_ofs = 0}
            ) params param_types in

            let meth = {
              meth_name = meth_id.id;
              meth_type = ret_ty;
              meth_params = param_vars;
              meth_ofs = 0; (* will be set later *)
              meth_defining_class = Some c; (* Mark which class defines this method *)
            } in
            (* Only replace if overriding, otherwise add new method *)
            if is_override then
              Hashtbl.replace c.class_methods meth_id.id meth
            else
              Hashtbl.add c.class_methods meth_id.id meth
      ) decls
    end
  in

  (* Process all classes *)
  List.iter (fun (id, _, _) ->
    process_class id.id
  ) pfile


(* Typing environment *)
type env = (string, var) Hashtbl.t

let new_env () = Hashtbl.create 16

let add_var env name ty ofs =
  let v = {var_name = name; var_type = ty; var_ofs = ofs} in
  Hashtbl.add env name v;
  v

(* Phase 3: Type check expressions and statements *)
let rec type_expr (env : env) (pe : pexpr) : expr =
  let loc = pe.pexpr_loc in
  let desc, ty = match pe.pexpr_desc with
    | PEconstant c ->
        let ty = match c with
          | Cbool _ -> Tboolean
          | Cint _ -> Tint
          | Cstring _ -> Tclass class_String
        in
        Econstant c, ty

    | PEthis ->
        Ethis, Tclass !current_class

    | PEnull ->
        Enull, Tnull

    | PEident id ->
        (* Look up in local environment first *)
        if Hashtbl.mem env id.id then
          let v = Hashtbl.find env id.id in
          Evar v, v.var_type
        (* Then look in class attributes *)
        else if Hashtbl.mem !current_class.class_attributes id.id then
          let attr = Hashtbl.find !current_class.class_attributes id.id in
          Eattr ({expr_desc = Ethis; expr_type = Tclass !current_class}, attr), attr.attr_type
        else
          error ~loc "unbound variable %s" id.id

    | PEdot (e, id) ->
        let te = type_expr env e in
        (match te.expr_type with
         | Tclass c ->
             if not (Hashtbl.mem c.class_attributes id.id) then
               error ~loc "class %s has no attribute %s" c.class_name id.id;
             let attr = Hashtbl.find c.class_attributes id.id in
             Eattr (te, attr), attr.attr_type
         | _ -> error ~loc "cannot access attribute of non-object type")

    | PEassign_ident (id, e) ->
        let te = type_expr env e in
        if Hashtbl.mem env id.id then begin
          let v = Hashtbl.find env id.id in
          if not (subtype te.expr_type v.var_type) then
            error ~loc "type mismatch in assignment";
          Eassign_var (v, te), v.var_type
        end else if Hashtbl.mem !current_class.class_attributes id.id then begin
          let attr = Hashtbl.find !current_class.class_attributes id.id in
          if not (subtype te.expr_type attr.attr_type) then
            error ~loc "type mismatch in assignment";
          Eassign_attr ({expr_desc = Ethis; expr_type = Tclass !current_class}, attr, te), attr.attr_type
        end else
          error ~loc "unbound variable %s" id.id

    | PEassign_dot (e1, id, e2) ->
        let te1 = type_expr env e1 in
        let te2 = type_expr env e2 in
        (match te1.expr_type with
         | Tclass c ->
             if not (Hashtbl.mem c.class_attributes id.id) then
               error ~loc "class %s has no attribute %s" c.class_name id.id;
             let attr = Hashtbl.find c.class_attributes id.id in
             if not (subtype te2.expr_type attr.attr_type) then
               error ~loc "type mismatch in assignment";
             Eassign_attr (te1, attr, te2), attr.attr_type
         | _ -> error ~loc "cannot access attribute of non-object type")

    | PEunop (op, e) ->
        let te = type_expr env e in
        (match op, te.expr_type with
         | Uneg, Tint -> Eunop (op, te), Tint
         | Unot, Tboolean -> Eunop (op, te), Tboolean
         | Ustring_of_int, Tint -> Eunop (op, te), Tclass class_String
         | _ -> error ~loc "type mismatch in unary operation")

    | PEbinop (op, e1, e2) ->
        let te1 = type_expr env e1 in
        let te2 = type_expr env e2 in
        (match op with
         | Badd | Bsub | Bmul | Bdiv | Bmod ->
             (* Special case: string concatenation *)
             if op = Badd && (type_equal te1.expr_type (Tclass class_String) || type_equal te2.expr_type (Tclass class_String)) then begin
               let check_string_add t = match t with
                 | Tclass c when c.class_name = "String" -> true
                 | Tint -> true
                 | _ -> false
               in
               if not (check_string_add te1.expr_type) || not (check_string_add te2.expr_type) then
                 error ~loc "string concatenation requires int or string operands";
               (* Convert int to string if needed *)
               let te1' = if te1.expr_type = Tint then
                 {expr_desc = Eunop (Ustring_of_int, te1); expr_type = Tclass class_String}
               else te1 in
               let te2' = if te2.expr_type = Tint then
                 {expr_desc = Eunop (Ustring_of_int, te2); expr_type = Tclass class_String}
               else te2 in
               Ebinop (Badd_s, te1', te2'), Tclass class_String
             end else begin
               if te1.expr_type <> Tint || te2.expr_type <> Tint then
                 error ~loc "arithmetic operations require int operands";
               Ebinop (op, te1, te2), Tint
             end
         | Beq | Bneq ->
             if not (compatible te1.expr_type te2.expr_type) then
               error ~loc "incompatible types in comparison";
             Ebinop (op, te1, te2), Tboolean
         | Blt | Ble | Bgt | Bge ->
             if te1.expr_type <> Tint || te2.expr_type <> Tint then
               error ~loc "comparison requires int operands";
             Ebinop (op, te1, te2), Tboolean
         | Band | Bor ->
             if te1.expr_type <> Tboolean || te2.expr_type <> Tboolean then
               error ~loc "logical operations require boolean operands";
             Ebinop (op, te1, te2), Tboolean
         | Badd_s -> (* Should not appear in parsed tree *)
             error ~loc "internal error: Badd_s in parsed tree")

    | PEcall (e, id, args) ->
        (* Check if this is System.out.print pattern BEFORE typing e *)
        let is_system_out_print =
          match e.pexpr_desc with
          | PEdot (e2, id2) when id2.id = "out" && id.id = "print" ->
              (match e2.pexpr_desc with
               | PEident sys_id when sys_id.id = "System" -> true
               | _ -> false)
          | _ -> false
        in
        if is_system_out_print then begin
          (* Handle System.out.print specially *)
          match args with
          | [arg] ->
              let targ = type_expr env arg in
              (* Auto-convert int and boolean to string *)
              let converted_arg = match targ.expr_type with
                | Tclass c when c.class_name = "String" -> targ
                | Tint ->
                    {expr_desc = Eunop (Ustring_of_int, targ); expr_type = Tclass class_String}
                | Tboolean ->
                    (* Boolean to string conversion not supported - would need conditional expression *)
                    error ~loc "System.out.print does not support boolean arguments (use string concatenation instead)"
                | _ ->
                    error ~loc "System.out.print requires string, int, or boolean argument"
              in
              Eprint converted_arg, Tvoid
          | _ ->
              error ~loc "System.out.print requires exactly one argument"
        end
        else begin
          (* Normal method call *)
          let te = type_expr env e in
          match te.expr_type with
          | Tclass c ->
              (* Special case: String.equals *)
              if c.class_name = "String" && id.id = "equals" then begin
                match args with
                | [arg] ->
                    let targ = type_expr env arg in
                    if not (type_equal targ.expr_type (Tclass class_String)) then
                      error ~loc "String.equals requires string argument";
                    let meth = Hashtbl.find c.class_methods "equals" in
                    Ecall (te, meth, [targ]), Tboolean
                | _ -> error ~loc "String.equals requires exactly one argument"
              end
              else begin
                if not (Hashtbl.mem c.class_methods id.id) then
                  error ~loc "class %s has no method %s" c.class_name id.id;
                let meth = Hashtbl.find c.class_methods id.id in
                if List.length args <> List.length meth.meth_params then
                  error ~loc "wrong number of arguments for method %s" id.id;
                let targs = List.map (type_expr env) args in
                List.iter2 (fun targ param ->
                  if not (subtype targ.expr_type param.var_type) then
                    error ~loc "type mismatch in method call"
                ) targs meth.meth_params;
                Ecall (te, meth, targs), meth.meth_type
              end
          | _ ->
              error ~loc "cannot call method on non-object type"
        end

    | PEnew (id, args) ->
        if not (Hashtbl.mem classes id.id) then
          error ~loc "unknown class %s" id.id;
        let c = Hashtbl.find classes id.id in
        let targs = List.map (type_expr env) args in
        (* Constructor type checking would go here *)
        Enew (c, targs), Tclass c

    | PEcast (pty, e) ->
        let te = type_expr env e in
        let target_ty = ptype_to_type ~loc pty in
        if not (compatible te.expr_type target_ty) then
          error ~loc "invalid cast";
        (match target_ty with
         | Tclass c -> Ecast (c, te), Tclass c
         | _ -> te.expr_desc, target_ty)

    | PEinstanceof (e, pty) ->
        let te = type_expr env e in
        let target_ty = ptype_to_type ~loc pty in
        if not (compatible te.expr_type target_ty) then
          error ~loc "invalid instanceof check";
        (match target_ty, te.expr_type with
         | Tclass c, (Tclass _ | Tnull) ->
             Einstanceof (te, c.class_name), Tboolean
         | _ -> error ~loc "instanceof requires class types")

    | PEprint e ->
        let te = type_expr env e in
        if not (type_equal te.expr_type (Tclass class_String)) then
          error ~loc "System.out.print requires string argument";
        Eprint te, Tvoid
  in
  {expr_desc = desc; expr_type = ty}

(* Type check statements *)
let rec type_stmt (env : env) (expected_return : typ option) (ps : pstmt) : stmt * env =
  let loc = ps.pstmt_loc in
  match ps.pstmt_desc with
  | PSexpr pe ->
      let te = type_expr env pe in
      Sexpr te, env

  | PSvar (pty, id, init_opt) ->
      if Hashtbl.mem env id.id then
        error ~loc:id.loc "variable %s already declared" id.id;
      let ty = ptype_to_type ~loc:id.loc pty in
      if not (type_wf ty) then
        error ~loc:id.loc "type is not well-formed";
      let new_env = Hashtbl.copy env in
      let v = add_var new_env id.id ty 0 in
      (match init_opt with
       | None ->
           (* Default initialization *)
           let init_expr = match ty with
             | Tint -> {expr_desc = Econstant (Cint Int32.zero); expr_type = Tint}
             | Tboolean -> {expr_desc = Econstant (Cbool false); expr_type = Tboolean}
             | _ -> {expr_desc = Enull; expr_type = Tnull}
           in
           Svar (v, init_expr), new_env
       | Some e ->
           let te = type_expr env e in
           if not (subtype te.expr_type ty) then
             error ~loc:id.loc "type mismatch in variable initialization";
           Svar (v, te), new_env)

  | PSif (cond, s1, s2) ->
      let tcond = type_expr env cond in
      if tcond.expr_type <> Tboolean then
        error ~loc "if condition must be boolean";
      let ts1, _ = type_stmt env expected_return s1 in
      let ts2, _ = type_stmt env expected_return s2 in
      Sif (tcond, ts1, ts2), env

  | PSreturn e_opt ->
      (match expected_return, e_opt with
       | None, None -> Sreturn None, env
       | Some expected_ty, Some e ->
           let te = type_expr env e in
           if not (subtype te.expr_type expected_ty) then
             error ~loc "return type mismatch";
           Sreturn (Some te), env
       | None, Some _ -> error ~loc "unexpected return value in void method"
       | Some _, None -> error ~loc "missing return value")

  | PSblock stmts ->
      let rec type_stmts env acc = function
        | [] -> List.rev acc, env
        | s :: rest ->
            let ts, new_env = type_stmt env expected_return s in
            type_stmts new_env (ts :: acc) rest
      in
      let tstmts, _ = type_stmts (Hashtbl.copy env) [] stmts in
      Sblock tstmts, env

  | PSfor (init, cond, incr, body) ->
      let tinit, env1 = type_stmt (Hashtbl.copy env) expected_return init in
      let tcond = type_expr env1 cond in
      if tcond.expr_type <> Tboolean then
        error ~loc "for condition must be boolean";
      let tincr, _ = type_stmt env1 expected_return incr in
      let tbody, _ = type_stmt env1 expected_return body in
      Sfor (tinit, tcond, tincr, tbody), env

(* Type check a class *)
let type_class (id, extends_opt, decls : pclass) : tclass =
  let c = Hashtbl.find classes id.id in
  current_class := c;

  let typed_decls = List.filter_map (fun decl ->
    match decl with
    | PDattribute _ -> None (* Already processed *)

    | PDconstructor (cons_id, params, body) ->
        let env = new_env () in
        Hashtbl.add env "this" {var_name = "this"; var_type = Tclass c; var_ofs = 0};

        (* Add parameters to environment *)
        let param_vars = List.mapi (fun i (pty, pid) ->
          let ty = ptype_to_type ~loc:pid.loc pty in
          add_var env pid.id ty (8 * (i + 2)) (* +2 for return addr and saved rbp *)
        ) params in

        (* Type check body *)
        let tbody, _ = type_stmt env None body in
        Some (Dconstructor (param_vars, tbody))

    | PDmethod (ret_ty_opt, meth_id, params, body) ->
        let meth = Hashtbl.find c.class_methods meth_id.id in
        let env = new_env () in
        Hashtbl.add env "this" {var_name = "this"; var_type = Tclass c; var_ofs = 0};

        (* Add parameters to environment *)
        List.iteri (fun i pv ->
          Hashtbl.add env pv.var_name pv
        ) meth.meth_params;

        (* Type check body *)
        let return_ty = if meth.meth_type = Tvoid then None else Some meth.meth_type in
        let tbody, _ = type_stmt env return_ty body in
        Some (Dmethod (meth, tbody))
  ) decls in

  (c, typed_decls)

(* Set attribute offsets *)
let set_attribute_offsets c =
  let ofs = ref 8 in
  let rec collect_attrs c =
    if c.class_name = "Object" then []
    else
      let parent_attrs = collect_attrs c.class_extends in
      let own_attrs = Hashtbl.fold (fun name attr acc ->
        if List.exists (fun a -> a.attr_name = name) parent_attrs then acc
        else attr :: acc
      ) c.class_attributes [] in
      parent_attrs @ own_attrs
  in
  let all_attrs = collect_attrs c in
  List.iter (fun attr ->
    attr.attr_ofs <- !ofs;
    ofs := !ofs + 8
  ) all_attrs

(* Set method offsets in vtable *)
let set_method_offsets c =
  let ofs = ref 8 in
  let rec collect_meths c =
    if c.class_name = "Object" then []
    else
      let parent_meths = collect_meths c.class_extends in
      (* Check each parent method - if overridden, use the overriding version *)
      let updated_parent_meths = List.map (fun pm ->
        try
          let override_meth = Hashtbl.find c.class_methods pm.meth_name in
          match override_meth.meth_defining_class with
          | Some dc when dc.class_name = c.class_name -> override_meth
          | _ -> pm
        with Not_found -> pm
      ) parent_meths in
      (* Add new methods not in parent *)
      let own_meths = Hashtbl.fold (fun name meth acc ->
        if List.exists (fun m -> m.meth_name = name) parent_meths then acc
        else meth :: acc
      ) c.class_methods [] in
      updated_parent_meths @ own_meths
  in
  let all_meths = collect_meths c in
  List.iter (fun meth ->
    meth.meth_ofs <- !ofs;
    ofs := !ofs + 8
  ) all_meths

(* Main type checking function *)
let file ?debug:(b=false) (p: Ast.pfile) : Ast.tfile =
  debug := b;

  (* Clear previous state *)
  Hashtbl.clear classes;
  Hashtbl.add classes "Object" class_Object;
  Hashtbl.add classes "String" class_String;

  (* Phase 1: Declare all classes *)
  declare_classes p;

  (* Phase 2: Set up inheritance and declare members *)
  setup_inheritance p;
  declare_members p;

  (* Set offsets for all classes *)
  Hashtbl.iter (fun _ c ->
    set_attribute_offsets c;
    set_method_offsets c
  ) classes;

  (* Phase 3: Type check all classes *)
  List.map type_class p
