open Syntax 

(* 値の定義 *)

(* exval は式を評価して得られる値．dnval は変数と紐付けられる値．今回
   の言語ではこの両者は同じになるが，この2つが異なる言語もある．教科書
   参照． *)
type exval =
  | IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
      (* 現在の環境で exp1 を評価 *)
      let value = eval_exp env exp1 in
      (* exp1 の評価結果を id の値として環境に追加して exp2 を評価 *)
      eval_exp (Environment.extend id value env) exp2
  (* 現在の環境 env をクロージャ内に保存 *)

  | FunExp (params, exp) -> 
      let dummyenv = ref Environment.empty in dummyenv := env;
      ProcV (params, exp, dummyenv)

  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
      ProcV(id, body, env) -> 
      let eval_env = Environment.extend id arg !env in
      eval_exp eval_env body)
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
      dummyenv := newenv;
      eval_exp newenv exp2
  | _ -> err ("Non-function value is applied")

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl(id, para, e) -> (
      let dummyenv = ref Environment.empty in 
      let newenv = Environment.extend id (ProcV(para, e, dummyenv)) env in 
      dummyenv := newenv;
      (id, newenv, ProcV(para, e, dummyenv))
    )
