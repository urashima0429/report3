open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

let ty_prim op ty1 ty2 = match op with
	| Plus -> 
		(match ty1, ty2 with
		TyInt, TyInt -> TyInt
		| _ -> err ("Argument must be of integer: +"))
	| Mult -> 
		(match ty1, ty2 with
		TyInt, TyInt -> TyInt
		| _ -> err ("Argument must be of integer: +"))
  	| Lt  -> 
		(match ty1, ty2 with
		TyInt, TyInt -> TyBool
		| _ -> err ("Argument must be of integer: +"))
	| _ -> err "Not Implemented!"

let rec ty_exp tyenv = function
	  Var x ->
		(try Environment.lookup x tyenv with
			Environment.Not_bound -> err ("variable not bound: " ^ x))
	| ILit _ -> TyInt
	| BLit _ -> TyBool
	| BinOp (op, exp1, exp2) ->
		let tyarg1 = ty_exp tyenv exp1 in
		let tyarg2 = ty_exp tyenv exp2 in
		ty_prim op tyarg1 tyarg2

	| IfExp (exp1, exp2, exp3) ->
		let tyarg1 = ty_exp tyenv exp1 in
		(match tyarg1 with
			  TyBool -> 
			  	let tyarg2 = ty_exp tyenv exp2 in
      			let tyarg3 = ty_exp tyenv exp3 in
      			(match tyarg2 with
      			  tyarg3 -> tyarg3
      			| _ -> err "Not Implemented!"
      			)
			| _ -> err "Not Implemented!"
		)
	| LetExp (id, exp1, exp2) ->
		let tyarg1 = ty_exp tyenv exp1 in 
		let new_tyenv = Environment.extend id tyarg1 tyenv in
		ty_exp new_tyenv exp2
	| FunExp(params, exp) ->



	| AppExp(exp1, exp2) ->
    	let ty_exp1 = ty_exp tyenv exp1 in
    	let ty_exp2 = ty_exp tyenv exp2 in

    | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
	| Exp e -> ty_exp tyenv e
	| _ -> err ("Not Implemented!")