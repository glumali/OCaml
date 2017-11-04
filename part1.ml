(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)

let exp1a : string = "Commas should be semicolons to create an int list."
let prob1a : int list = [1; 2; 3]

(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)

let exp1b : string = "Compiler was expecting a tuple of a string type, then an
int list type, but we intended a (string * int) tuple list type."
let prob1b : (string * int) list = [("COS", 326); ("COS", 441)]

(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.
   (Do not change the left-hand-side.)
*)

let exp1c : string = "An expression using cons must have the form t :: [t],
in other words, one of type t and one of type t list. Was not the case here."
let prob1c : float list = 2.0 :: 3.0 :: [4.0; 5.0]


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(*>* Problem 2a *>*)

let prob2a : (float * (string * int) option list) list =
(1.0, [Some ("Oceans", 11)])  :: [(2.0, [Some ("Beverly Hills", 90210)])]


(*>* Problem 2b *>*)
(* a student is a (name, age option) pair *)

type student = string * int option
let prob2b : (student list option * int) list = [(Some [("Bob", Some 2)], 4)]


(*>* Problem 2c *>*)
let f (a: int * int) : int = let (x, y) = a in x + y
let g (x:float) (y:float) : unit =
  print_float (x +. y)
let prob2c : (int * int -> int) * (float -> float -> unit) * bool  =
  (f, g, true)


(*>* Problem 2d *>*)
(* Fill in a valid function call to foo to make prob2d typecheck *)


let prob2d =
  let rec foo (bar:(bool * (int * int)) list) : int =
    match bar with
    | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
    | _ -> 0
  in
  foo [(true, (6, 7)); (false, (9, 10))]


(*************)
(* PROBLEM 3 *)
(*************)

(* Consider the following terribly written function: *)

let rec zardoz f ls acc =
  if (((List.length (ls@[])) = 1) = true) then (f (List.hd(ls)) (acc))
  else if (((List.length ls) = 0) = true) then acc
  else
    let hd = List.hd(ls) in
        let tl = List.tl(ls) in
      let ans = f (hd) (acc) in
    let ans = zardoz f tl ans in
        ans

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.
 * Be sure to provide types for the function's arguments and to
 * call itself (not the original zardoz) recursively as needed.
 * You may want to write some assert statements
 * to check that your function is doing the same thing as zardoz.
 * Use the COS 326 style guide. *)

let rec myzardoz (f:'a -> 'b -> 'b) (ls:'a list) (acc:'b) : 'b =
  match ls with
  | [] -> acc
  | hd::[] -> f hd acc
  | hd::tl -> myzardoz f tl (f hd acc)


(*************)
(* PROBLEM 4 *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(*

If l is any list of integers, the look-and-say list of s is obtained by
reading off adjacent groups of identical elements in s. For example, the
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos." Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos."

You will now define a function look_and_say that computes the
look-and-say sequence of its argument. look_and_say of an empty
list is the empty list.

For full credit your solution should be a linear time solution.

CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated
by repeated applications of the "look and say" operation. As look and say
has type int list -> int list, the function can be applied to its own result.
For example, if we start with the list of length one consisting of just the
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay"
(by repeated applications of look and say) into a "compound" made up of
combinations of "primitive elements" (there are 92 of them, plus 2
infinite families) in 24 steps. If you are interested in this sequence,
you may wish to consult [Conway(1987)] or other papers about the
"look and say" operation.

======

Progamming practice aside related to the "look and say" problem. You
may find this useful for constructing your solution to "look and say",
or you may not.

Another interesting list problem is determining "runs"
in a list: maximal length sublists with all equal elements. For
example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not:

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.

*)

let look_and_say (xs: int list) : int list =

  (* Counts multiples of the first number int the list x until it finds a
   * different number. Returns the tuple (count, n). *)
  let rec count_mults (xs:int list) (num:int) (count:int) =
    match xs with
    | [] -> []
    | hd1::[] -> (count+1) :: hd1 :: []
    | hd1::hd2::tl ->
      (* Next two elements match; keep counting *)
      if hd1 = hd2 then count_mults (hd2::tl) num (count + 1)
      (* Next two elements don't match; add to the list, reset count and num *)
      else (count+1) :: hd1 :: (count_mults (hd2::tl) hd2 0)
  in (count_mults xs 0 0)

(* Test Code
let _ = print_string
(String.concat " " (List.map string_of_int (look_and_say [1; 2; 2; 2; 3; 3])))
*)

(*************)
(* PROBLEM 5 *)
(*************)

(* Write a function that flattens a list of lists in to a single
 * list with all of the elements in the same order they appeared in
 * the original list of lists. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6]
 * flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c']
 *)

let rec flatten (xss:'a list list) : 'a list =
  match xss with
  | [] -> []
  (* Only one list; return that list *)
  | hd1 :: [] -> hd1
  | hd1 :: hd2 :: xss ->
    let rec append (l:'a list) (r:'a list) : 'a list =
      match l with
      | [] -> r
      | hd::tl -> hd :: append tl r
    in flatten ((append hd1 hd2) :: xss)

(* Test code
let _ = print_string
(String.concat " " (List.map string_of_int (flatten [[1]; []; [2;2]; [3;4;5]]) ))
*)

(*************************************)
(* PROBLEM 6 -- Warning: Challenging!*)
(*************************************)

(* Return the list of all permutations of the input list. eg:
   perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]]
   The ordering of the permutations does not matter in your solution.
   We will accept either [[]] or [] as the result for perm [].
   NB: test this on small inputs - perm is ~n! which is approximately ~n^n.
 *)

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

let rec append (l:'a list) (r:'a list) : 'a list =
  match l with
  | [] -> r
  | hd::tl -> hd :: append tl r

(* return the first n items from the list *)
(* if there are fewer than n items, return all of them *)
(* call bad_arg if n is negative *)
let rec take (n:int) (l:'a list)  : 'a list =
  if (n < 0) then bad_arg "n is negative."
  else match l with
  | [] -> l
  | hd::tl ->
    if (n = 0) then []
    else hd :: (take (n-1) tl)

let rec drop (n:int) (l:'a list)  : 'a list =
  if (n < 0) then bad_arg "n is negative."
  else match l with
  | [] -> l
  | hd::tl ->
    if (n = 0) then l
    else drop (n-1) tl

let rec length (items:'a list) : int =
  match items with
  | [] -> 0
  | hd::tl -> 1 + (length tl)


let perm (items:'a list) : 'a list list =
  (* Works on the following principle:
   * Say you have list [1; 2; 3; 4].
   * You can generate different starting values in OCaml by
   * 1) Append the list to itself [1; 2; 3; 4; 1; 2; 3; 4]
   * 2) Starting from index 0, then 1, then 2, then 3, split into
   *    [1; 2; 3; 4], [2; 3; 4; 1], [3; 4; 1; 2], and [4; 1; 2; 3]
   * 3) These are then added to the original list.
   *)

 let rec shifts (ll: 'a list list) (lock: int): 'a list list =
   match ll with
   | [] -> ll
   (* Takes the first list *)
   | hd1::tl1 ->
     (* Done recurring when the lock is equal to length-1 *)
     if lock >= (length hd1) then ll else
     (* Append to itself *)
     let locked = (take lock hd1) in
     let unlocked = (drop lock hd1) in
     let len = length unlocked in
     let appended = (append unlocked unlocked) in
     (* Add stuff from appended into the tail *)

     (* n is the number of lists it will split into; equal to length *)
     let rec add_shifts (app:'a list) (result: 'a list list) (len:int) (n:int): 'a list list =
       (* Repeats until n gets to zero *)
       if n <= 0 then result
       else match app with
       | [] -> result
       | hd::tl ->
         (* Pulls off the first (head length), adds it to result *)
         add_shifts tl ( (append locked (take len app)) ::result) len (n-1)

   (* Remove the head, just append new shifts to the tail *)
   in append (shifts (add_shifts appended [] len len) (lock + 1)) (shifts tl1 lock)
  (* Shifts takes in a list of lists *)
 in shifts (items :: []) 0
