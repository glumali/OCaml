(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

(* Name:  Greg Umali
   netid: gumali
 *)

open Syntax
open Printing
open EvalUtil

(* Defines the subset of expres|ues but the rec form is not -- this is
   slightly different from the way values are defined in the
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
*)
let rec is_value (e:exp) : bool =
  match e with
      Constant _ -> true
      (* Pair is only a value if fst and snd are both values *)
    | Pair (e1, e2) -> is_value e1 && is_value e2
      (* By definition, this is a value *)
    | EmptyList -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
      (* A function with everything we need to evaluate it *)
    | Closure _ -> true
      (* Functions are not values, can be made into a closure *)
    | _ -> false

(* Helper function, appends two lists *)
let rec append (l:'a list) (r:'a list) : 'a list =
  match l with
  | [] -> r
  | hd::tl -> hd :: append tl r

(* Helper function, returns string list of free variables for an exp *)
let rec fv (e:exp) (par:string) (result:string list): string list =
  match e with
  (* Found a variable not in a let statement *)
  | Var x ->
    if var_eq x par then result
    (* Add to the result list *)
    else
      (* If already in results list, don't add it again *)
      if (List.mem x result) then result
      else (x :: result)
  | Constant _ -> result
  | Op (e1, op, e2) ->
    (* Recursively count free vars in each expression *)
    append (fv e1 par result) (fv e2 par result)
  | If (e1,e2,e3) ->
    (* Recursively count free vars in each of the expressions *)
    append (append (fv e1 par result) (fv e2 par result)) (fv e3 par result)
  (* The first argument is NOT added as a free variable *)
  | Let (x,e1,e2) ->
    (* Recursively count free vars in each expression *)
    append (fv e1 par result) (fv e2 par result)
  | Pair (fst, snd) ->
    (* Recursively count free vars in each expression *)
    append (fv fst par result) (fv snd par result)
  | Fst e1 -> fv e1 par result
  | Snd e2 -> fv e2 par result
  | EmptyList -> result
  | Cons (head, tail) ->
    append (fv head par result) (fv tail par result)
  | Match (e1,e2,hd,tl,e3) ->
    append (append (fv e1 par result) (fv e2 par result)) (fv e3 par result)
  (* Change the parameter argument - will not be added as a free var *)
  | Rec (f, this_par, b) -> fv b this_par result
  | Closure _ -> result
  | App (e1, e2) ->
    append (fv e1 par result) (fv e2 par result)

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp =
  match e with
    | Var x ->
      (* See if x is in our environment *)
      (match lookup_env env x with
  	   None -> raise (UnboundVariable x)
       | Some v -> v)
    | Constant _ -> e
    | Op (e1,op,e2) ->
      (* Eval each, then apply the op *)
      let v1 = eval_loop env e1 in
      let v2 = eval_loop env e2 in
        apply_op v1 op v2
    | If (e1,e2,e3) ->
      (match (eval_loop env e1) with
       (* Decide which expression to evaluate *)
       | Constant (Bool true) -> eval_loop env e2
       | Constant (Bool false) -> eval_loop env e3
       (* All other cases *)
       | v1 -> raise (BadIf v1))
    | Let (x,e1,e2) ->
      (* Update environment with the new binding *)
      let new_env = update_env env x (eval_loop env e1)
      in eval_loop new_env (eval_loop new_env e2)
    | Pair (fst, snd) ->
      let v1 = eval_loop env fst in
      let v2 = eval_loop env snd in
      Pair (v1, v2)
    | Fst e1 ->
      let v1 = eval_loop env e1 in
      (* Make sure this expression is a Pair *)
      (match v1 with
      | Pair (x, _) -> eval_loop env x
      | _ -> raise (BadPair v1))
    | Snd e2 ->
      let v2 = eval_loop env e2 in
      (* Make sure this expression is a Pair *)
      (match v2 with
      | Pair (_, y) -> eval_loop env y
      | _ -> raise (BadPair v2))
    | EmptyList -> e
    | Cons (head, tail) ->
      let v1 = eval_loop env head in
      let v2 = eval_loop env tail in
      Cons (v1, v2)
    | Match (e1,e2,hd,tl,e3) ->
        (match (eval_loop env e1) with
        | EmptyList -> eval_loop env e2
        | Cons (head, tail) ->
          (* Add head and tail to the environment *)
          let new_env = update_env env hd (eval_loop env head) in
          let new_env' = update_env new_env tl (eval_loop env tail) in
          eval_loop new_env' e3
        (* All other cases *)
        | v1 -> raise (BadMatch v1))
    (* f = function name, x = parameter, b = body of expression *)
    | Rec (f, x, b) ->
      (* Helper function, returns true if a function requires a variable *)
      let is_needed (binding:string * exp) : bool =
        let free_vars = fv (Rec (f, x, b)) "" [] in
        let (name, _) = binding in
        if (List.mem name free_vars)
          then true
          else false in
      (* Remove all bindings from the new environment that are not needed *)
      let new_env = List.filter is_needed env in
      (* Must create a closure (e, f, x, b) *)
      Closure (new_env, f, x, b)
    | Closure _ -> e
    | App (e1, e2) ->
      (* f and x are both expressions *)
      (match (eval_loop env e1) with
        (* Make sure this evaluation becomes a Closure *)
        | (Closure (env_clo,f,x,body)) as clo ->
          let v2 = eval_loop env e2 in
          (* Update the env with both the evaluation and your closure *)
          let new_env = update_env env_clo x v2 in
          let new_env' = update_env new_env f clo in
          eval_loop new_env' body
        | v1 -> raise (BadApplication v1))

(* evaluate closed, top-level expression e *)

let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e


(* print out subexpression after each step of evaluation *)
let debug_eval e =
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else
      begin
	Printf.printf "Evaluating %s\n" (string_of_exp e);
	let v = eval_body env loop e in
	Printf.printf
	  "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v);
	v
      end
  in
  loop empty_env e

(* Testing *)
let test_func =
Rec ("test_func", "x",
  If (Op (Var "x", Less, Constant (Int 10)), Var "y", Var "z"))

let rec print_list_string myList = match myList with
| [] -> print_endline "This is the end of the string list!"
| head::body ->
begin
print_endline head;
print_list_string body
end
;;

let _ = print_list_string (fv test_func "" [])
