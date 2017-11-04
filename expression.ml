(*

Name: Greg Umali
Email: gumali@princeton.edu
Minutes Spent on Problem 2: 8 hrs

(You aren't in any way graded on the number of minutes spent;
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

*)

open Ast
open ExpressionLibrary

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
 *)


(*>* Problem 2.1 *>*)

(*
let substitute (n:Num) (var:Var) (e:exp) : exp =
*)

let eval_op (v1:float) (op:binop) (v2:float) : float =
  match op with
  | Add -> v1 +. v2
  | Sub -> v1 -. v2
  | Mul -> v1 *. v2

(* evaluate : evaluates an expression for a particular value of x.
 *  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  (* If constant, already a value so just return *)
  | Num n -> n
  (* Replace all variables with the given x *)
  | Var -> x
  | Binop (op, e1, e2) ->
    let v1 = evaluate e1 x in
    let v2 = evaluate e2 x in
    eval_op v1 op v2

let _ = assert (evaluate (parse "x*x + 3") 2.0 = 7.0);;

(*>* Problem 2.2 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
  (* If constant, derivative is 0 *)
  | Num n -> Num 0.0
  (* Variables have a derivative of 1 *)
  | Var -> Num 1.0
  | Binop (op, e1, e2) ->
    match op with
    | Add -> Binop (Add, (derivative e1), (derivative e2))
    | Sub -> Binop (Sub, (derivative e1), (derivative e2))
    | Mul -> Binop (Add,
              (Binop (Mul, (derivative e1), e2)),
              (Binop (Mul, (derivative e2), e1)) )

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
	print_string "Result of evaluation: ";
	print_float  (evaluate parsed xval);
	print_endline " ";
	print_string "Result of derivative: ";
	print_endline " ";
	print_string (to_string (derivative parsed));
	print_endline " ")

(* let _ = checkexp "2*x*x + 5*x" 2.0 *)

(*>* Problem 2.3 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  let test_val = evaluate e g in
  if lim = 0 then None
  (* If within epsilon bounds, return that value *)
  else if (test_val < epsilon) && (test_val > epsilon *. -1.0)
    then Some g
  else
    (* Adjust new guess *)
    let new_guess = g -. test_val /. (evaluate (derivative e) g) in
    find_zero e new_guess epsilon (lim-1)

let print_answer (fo:float option) =
  match fo with
  | Some f -> print_float f
  | None -> print_string "None"
;;

(* print_answer (find_zero (parse "3*x + 5") 2.0 0.001 5) *)


(*>* Problem 2.4 *>*)

let map : ('a -> 'b) -> 'a list -> 'b list = List.map;;

let filter : ('a -> bool) -> 'a list -> 'a list = List.filter;;

let foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = List.fold_right;;

let foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = List.fold_left;;

(* reduce is equivalent to List.fold_right,
 * only its args are ordered differently *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;

let append (xs: 'a list) (ys: 'a list) : 'a list =
  reduce (fun x xs -> x::xs) ys xs

let flatten (xss:'a list list) : 'a list =
  foldl append [] xss


(* Standard form of each term in a polynomial is ax^b, with a represented by
 * a float and b represented by an int (assuming whole number exponents) *)
type poly_term = (float * int)

(* A polynomial is just a list of these data types *)
type polynomial = poly_term list

(* Negate a term's a coefficient *)
let negate_term (term:poly_term) : poly_term =
  let (a, b) = term in
  (a *. -1.0, b)

(* Negate all terms in a polynomial *)
let negate_poly (poly:polynomial) : polynomial =
  map negate_term poly

(* Multiply two terms together *)
let mul_term (x:poly_term) (y:poly_term) : poly_term =
  let (ax, bx) = x in
  let (ay, by) = y in
    (ax *. ay, bx + by)

(* Multiplies one term by all terms in a polynomial *)
let distribute (term:poly_term) (old_poly:polynomial) : polynomial =
  map (fun (other:poly_term) -> mul_term term other) old_poly

(* Multiply two polynomials together *)
let mul_poly (p1:polynomial) (p2:polynomial) : polynomial =
  (* Multiply all terms of p1 by each term of p2 *)
  (* Returns a list of polynomials. Flatten into one polynomial. *)
  flatten (map (fun (term:poly_term) -> distribute term p2) p1)

(* Add two like terms *)
let add_term (x:poly_term) (y:poly_term) : poly_term =
  let (ax, bx) = x in
  let (ay, by) = y in
    (ax +. ay, bx)

(*
let (a, b) = (add_term (5.0, 3) (-1.0, 3))
let _ = print_string ("(" ^ string_of_float a ^ "," ^ string_of_int b ^ ") ")
*)

(* Degree of function - find maximum b value of terms in list *)
let comp_degree (t:poly_term) (deg:int) : int =
  let (a, b) = t in
  if b > deg && a <> 0.0 then b
  else deg

(* Find the degree of a function *)
let max_degree (poly:polynomial) : int =
  (reduce comp_degree 0 poly)

(* Combining like terms - use filter, make new list *)
let like_terms (poly:polynomial) : polynomial =
  let degree = (max_degree poly) in
  let rec combine (p:polynomial) (deg:int) (result:polynomial): polynomial =
    (* Keeps adding like terms until no more to combine *)
    if deg < 0 then result
    else
      let terms = filter (fun t -> let (_,b) = t in b = deg) p
      in let combined = foldl (fun t1 t2 -> add_term t1 t2) (0.0, deg) terms
      in combine p (deg-1) (combined::result)
  in filter (fun t -> let (a, _) = t in a <> 0.) (combine poly degree [])

(* Creates a new polynomial data type from an expression *)
let rec make_poly (e:expression) (result:polynomial) : polynomial =
  match e with
  (* Return a new list with just that number term *)
  | Num n -> [(n, 0)]
  (* Return a new list with just that variable *)
  | Var -> [(1.0, 1)]
  | Binop (op, e1, e2) ->
    match op with
    | Add -> like_terms (
               append result
                 (append (make_poly e1 []) (make_poly e2 [])))
    | Sub -> like_terms (
               append result
                 (append (make_poly e1 []) (negate_poly (make_poly e2 []))))
    | Mul -> like_terms (
               append result
                 (mul_poly (make_poly e1 []) (make_poly e2 [])))

(* Checks if a polynomial is first-degree *)
let is_first (p:polynomial) : bool =
  max_degree (like_terms p) = 1
  (*let rec is_first_degree (p:polynomial) (first_found:bool): bool =
    let poly = like_terms p in
    match poly with
    (* Checked all terms, did not return false by finding a bad term, but
     * did it find a 1st-degree term? *)
    | [] -> if first_found then true else false
    | hd::tl ->
      let (a, b) = hd in
      (* Found a degree 1 term! *)
      if b = 1 then is_first_degree tl true
      (* Not be first degree, but could have a coefficient of 0 *)
      else false
  in is_first_degree p false *)

(* Calculate the zero of a 1D polynomial *)
let rec eval_1D_poly (poly:polynomial) (deg0:float) (deg1:float) : float =
  match poly with
  (* Answer become -deg0 / deg1, where deg0 and deg1 denote those
   * terms' coefficients, respectively *)
  | [] -> (-1.0 *. deg0 /. deg1)
  | hd::tl ->
    let (a, b) = hd in
    if b = 0 then eval_1D_poly tl a deg1
      (* Must negate if the coefficient is negative *)
      (* if a > 0.0 then
      else eval_1D_poly tl (a *. -1.0) deg1 *)
    else if b = 1 then eval_1D_poly tl deg0 a
      (* if a > 0.0 then eval_1D_poly tl deg0 a
      else eval_1D_poly tl deg0 (a *. -1.0) *)
    else failwith "Not a 1D polynomial"

(* For testing: print polynomial *)
let print_poly_num (poly:polynomial) =
  let print_term (t:poly_term) =
    let (a, b) = t in
    print_string (string_of_float a ^ "x^" ^ string_of_int b ^ "+ ")
  in map (fun t -> print_term t) poly

(* Print a polynomial in tuple form *)
let print_poly_tup (poly:polynomial) =
  let print_term (t:poly_term) =
    let (a, b) = t in
    print_string ("(" ^ string_of_float a ^ "," ^ string_of_int b ^ ") ")
  in map print_term poly



(* See writeup for instructions. *)
let find_zero_exact (e:expression) : expression option =
  let poly = make_poly e [] in
  if (is_first poly) then
    Some (Num (eval_1D_poly poly 0. 0.))
  else None



(* TESTING SECTION *)

(*
let test_first (poly:polynomial) : unit =
  if (is_first poly) then print_string "First degree"
  else print_string "Not first degree"

let check_zero eo =
  match eo with
  | Some e -> print_string (to_string e)
  | None -> print_string "None"

let e = (parse "5*x + 3")
let poly = (make_poly e [])
let _ = test_first poly
let _ = print_endline ""
let _ = print_string "Numeric: "
let _ = print_poly_num poly
let _ = print_endline ""
let _ = print_string "Tuples: "
let _ = print_poly_tup poly
let _ = print_endline ""
let _ = print_string "Degree: "
let _ = print_int (max_degree poly)
let _ = print_endline ""
let _ = print_string "Simplified Numeric: "
let _ = print_poly_num (like_terms poly)
let _ = print_endline ""
let _ = print_string "Simplified Tuples: "
let _ = print_poly_tup (like_terms poly)
let _ = print_endline ""
let answer = (find_zero_exact e)
let _ = check_zero answer
let _ = print_endline ""
let _ = print_string "Zero: "
let _ = print_string (to_string (find_zero_exact e))
let _ = print_endline ""
*)

(*>* Problem 2.5 *>*)

(* See writeup for instructions. This problem is completely optional.
 * Write code here to address the problem, and once finished,
 * fill the variables below with your answers and uncomment them
 *)
(*
let e_order_of_growth : string  =
let iterations_for_56_bits : int =
 *)
