(*

Name: Greg Umali
Netid: gumali
Minutes Spent on Problem 1.1: 40
Minutes Spent on Problem 1.2: 180

(You aren't in any way graded on the number of minutes spent;
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

*)

(* This part of the assignment uses the following functions
 * If you forget how they work, look them up:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *)

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

(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(* Solve each problem in this part using map, reduce, foldl, foldr or
 * filter.  Map, filter, reduce, foldl, foldr are an example of a
 * *combinator library*: a library of higher-order functions used to
 * solve problems in a particular domain.  In this case, the domain is
 * list-processing.  However, there are countless other useful
 * combinator libraries.  The goal is to get used to thinking about how
 * to decompose complex functions in to smaller, simpler, orthogonal
 * functional components.  The smaller, simpler functional components
 * can be constructed directly using the combinator library.
 *
 * Note: foldl is slightly more efficient than foldr because it is tail
 * recursive.  (We will explain what that means later in the course.)
 * Hence, solutions that use foldl are typically superior to solutions
 * that use foldr, all other things being equal.  Thus you should try
 * to use foldl where you can instead of foldr (but take care to retain
 * good style -- a horribly convoluted, incomprehensible function that
 * uses foldl is worse than an elegant one that uses foldr).
 *
 * In these problems, you are not allowed to use the "rec" keyword in
 * your solution.  A solution, even a working one, that uses explicit
 * recursion via "rec" will receive little to no credit.  You may write
 * useful auxiliary functions; they just may not be recursive.
 * (The goal of this part of the assignment is not to simulate
 * everyday programming.  It is to get you think about writing programs
 * in a different style from what you are used to.  We are trying to
 * enlarge your programming toolkit.)
 *
 * You are also not allowed to use other functions from a standard or
 * external library such as sort, concat or flatten, nor the built-in
 * list operator @.  (You are allowed to recode those functions
 * yourself using map, filter, fold if you find it necessary.)
 *
 *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  let negate (num:int) : int = num * (-1) in
  map negate nums
;;

(* Unit test example.  Uncomment after writing negate_all *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;


(*>* Problem 1.1.b *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
  let sum_row (row:int list) : int =
    foldl (+) 0 row
  (* Produces a list  *)
  in map sum_row rows
;;

assert (sum_rows [[1;2]; [3;4]] = [3; 7]);;

(*>* Problem 1.1.c *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
  (* Returns 1 if a hit, 0 otherwise *)
  let is_hit (num:int) : int =
    if num = n then 1 else 0
  in reduce (+) 0 (map is_hit nums)
;;

assert (num_occurs 4 [1;3;4;5;4] = 2);;

(*>* Problem 1.1.d *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
  let sum_list (l:int list) : int =
    foldl (+) 0 l
  in sum_list (map sum_list nlists)
;;

assert (super_sum [[1;2;3];[];[5]; [10;79]] = 100);;

(****************************************************)
(**********       1.2 A Bigger Challenge   **********)
(****************************************************)

(* Note: some of these questions may be challenging. Don't neglect
 * Part 2 of this assignment because you are working on these problems.
 *
 * Again, these problems should be solved using a higher-order function
 * -- we intend that you use one or more folds, though map and filter
 * are acceptable if you find them useful -- and very little to no
 * credit will be given for doing them with explicit recursion.
 *
 *)


(*>* Problem 1.2.a *>*)

(* consec_dedupe : removes consecutive duplicate values from a list.
 * More specifically, consec_dedupe has two arguments:
 *   eq is a function representing an equivalence relation
 *   xs is a list of elements.
 * It returns a list containing the same elements as lst, but without
 * any consecutive duplicates, where two elements are considered
 * duplicates if applying eq to them yields true.
 *
 * Example: consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3].
 *
 * Example:
 *
 * let nocase_eq (s1:string) (s2:string) : bool = (String.uppercase s1)
 * = (String.uppercase s2) ;;
 *
 * consec_dedupe nocase_eq ["hi"; "HI"; "bi"] = ["hi"; "bi"]
 *
 * (When consecutive duplicates are not exactly syntactically equal, as
 * above, it does not matter which of the duplicates are discarded.)
 *
 *)

let consec_dedupe (eq:'a -> 'a -> bool) (xs:'a list) : 'a list =
  (* Use accumulator to track last encountered variable *)
  let is_chain (x:'a) (acc_list:'a list) : 'a list =
    match acc_list with
    | [] -> x::acc_list
    | hd::tl ->
      if (eq x hd) then acc_list
      else x::acc_list
  in reduce is_chain [] xs
;;

(* let _ = print_string
(String.concat " " (List.map string_of_int
  (consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3]) )) *)

assert((consec_dedupe (=) [1; 1; 3; 4; 4; 3; 5; 4; 4]) = [1; 3; 4; 3; 5; 4]);;

(*>* Problem 1.2.b *>*)

(* prefixes: return a list of all non-empty prefixes of a list,
 * ordered from shortest to longest.
 *
 * Example: prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]].
 *
 * There are no non-empty prefixes of an empty list.
*)
let append (xs: 'a list) (ys: 'a list) : 'a list =
  reduce (fun x xs -> x::xs) ys xs

(*
  let _ = print_string
  (String.concat " " (List.map string_of_int
    (append [1; 2; 3] [4; 5; 6]) ))
*)

let reverse (xs: 'a list) : 'a list =
  reduce (fun x xs -> append xs [x]) [] xs

(*
let _ = print_string
(String.concat " " (List.map string_of_int
  (reverse [1; 2; 3; 4; 5; 6]) ))
*)

let prefixes (xs: 'a list) : 'a list list =
  match xs with
  | [] -> []
  | hd::tl ->
    (* Use accumulator list to build prefixes *)
    let build_list (pfx_list:'a list list) (x:'a): 'a list list =
      match pfx_list with
      | [] -> ([x]::pfx_list)
      | hd::tl -> (append hd [x]) :: pfx_list
    in reverse (foldl build_list [] xs)
;;

(*
let rec rowToString r =
  match r with
  | [] -> ""
  | h :: [] -> string_of_int h
  | h :: t -> string_of_int h ^ ";" ^ (rowToString t)

let rec imageToString i =
  match i with
  | [] -> ""
  | h :: t -> "[" ^ (rowToString h) ^ "];\n" ^ (imageToString t)

let pp_my_image s =
  print_string (imageToString s)

let _ = pp_my_image (prefixes [1;2;3;4])
*)


(*>* Problem 1.2.c *>*)
(* flatten : write a function that flattens a list of lists in to a single
 * list with all of the elements in the order they appeared in the original
 * lists.
 *
 * flatten [[1;2;3]; []; [0]; [4;5]] = [1;2;3;0;4;5]
 *
 * In the last assignment you wrote this function with recursion. Now,
 * do it without explicit recursion.
 *)

let flatten (xss:'a list list) : 'a list =
  foldl append [] xss
;;

assert(flatten [[1;2;3]; []; [0]; [4;5]] = [1;2;3;0;4;5]);;
