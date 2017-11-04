(* Box office analysis *)

(* Contents:
    -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
*)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

(* you may add "rec" after any of the let declarations below that you
 * wish if you find doing so useful. *)


(* find the average gross of the movies in the list                  *)
(* return 0.0 if the list is empty                                   *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)
let average (movies : movie list) : float =
  let rec sum_gross (movies : movie list) (n:int) (sum:float): float * int =
    match movies with
    | [] -> (sum, n)
    | hd::tl ->
      let (_, _, gross, _) = hd in
      sum_gross tl (n + 1) (sum +. gross)
  in let (total, num) = (sum_gross movies 0 0.0)
  in total /. (float_of_int num)
(* CHECKED *)

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
(* Treat 0 as 00 (this is unavoidable as 00 is not represented    *)
(*   differently from 0).                                         *)
(* Note: movies from any years outside the range 1920-2019 will   *)
(* always be discarded but should not raise an error condition    *)
let rec decade (n:int) (ms:movie list) : movie list =
  if (n < 0 || n > 90 || not (n mod 10 = 0))
    then bad_arg "Please choose a valid decade."
  else match ms with
  | [] -> []
  | hd::tl ->
    let (_, _, _, year) = hd in
      if (n = 0 || n = 10) then
        if (year >= (n + 2000) && year < (n + 2010))
          then hd :: (decade n tl)
        else
          decade n tl
      else
        if (year >= (n + 1900) && year < (n + 1910))
          then hd :: (decade n tl)
        else
          decade n tl
(* CHECKED *)

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
(* CHECKED *)

(* return everything but the first n items from the list *)
(* if there are fewer than n items, return the empty list *)
(* call bad_arg if n is negative *)
let rec drop (n:int) (l:'a list)  : 'a list =
  if (n < 0) then bad_arg "n is negative."
  else match l with
  | [] -> l
  | hd::tl ->
    if (n = 0) then l
    else drop (n-1) tl
(* CHECKED *)

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
*)
(* hint: define an auxiliary function "select" *)
type 'a less = 'a -> 'a -> bool

(* Append two lists together *)
let rec append (l:'a list) (r:'a list) : 'a list =
  match l with
  | [] -> r
  | hd::tl -> hd :: append tl r
(* CHECKED *)

(* Iterate through the list, find the maximum element *)
let rec find_max (leq:'a less) (l:'a list) (curr_max:'a): 'a =
  match l with
  (* Once it runs out, will return the maximum *)
  | [] -> curr_max
  | hd::tl ->
    (* Replace current max with the head value *)
    if (leq curr_max hd) then find_max leq tl hd
    (* Current max is unchanged *)
    else find_max leq tl curr_max
(* CHECKED *)

(* Switches the first element with the first instance of a specified value *)
let rec swap_top (leq:'a less) (l:'a list) (value:'a) (pend:'a list) : 'a list =
  match l with
  (* Once it runs out, will return the maximum *)
  | [] -> []
  | hd::tl ->
    (* Test to see if the head value is what we're looking for *)
    if hd = value then
      match pend with
      (* Nothing in pending, so nothing to switch. Just return the list. *)
      | [] -> l
      (* Assemble as: value --> pend.tl --> pend.hd -->  tl *)
      | pend_hd::pend_tl -> (append (value::pend_tl) (pend_hd::tl))
    (* This element is not what we're looking for *)
    else swap_top leq tl value (append pend (hd::[]))
(* CHECKED *)

let rec selection_sort (leq:'a less) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | hd::tl ->
    let swapped = swap_top leq l (find_max leq l hd) [] in
    match swapped with
    | [] -> []
    | hd::tl -> hd :: selection_sort leq tl


(* ASIDE:  Why does this assignment ask you to implement selection sort?
   Insertion sort is almost always preferable to selection sort,
   if you have to implement a quadratic-time sorting algorithm.
   Insertion sort is faster, it's simpler to implement, and it's
   easier to reason about.  For smallish inputs (less than 5 or 8),
   insertion sort is typically faster than quicksort or any
   other NlogN sorting algorithm.  So, why do we ask you to implement
   selection sort?  Answer: we already showed you insertion sort
   in the lecture notes.

   ASIDE 2: But at least selection sort is better than bubble sort.
   Even Barack Obama knows that. https://www.youtube.com/watch?v=k4RRi_ntQc8
*)


(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list =
  let comp (left:movie) (right:movie) : bool =
    let (_, _, l_gross, _) = left in
    let (_, _, r_gross, _) = right in
    (l_gross <= r_gross)
  in selection_sort comp movies

(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list =
  let comp (left:movie) (right:movie) : bool =
    let (_, _, _, l_year) = left in
    let (_, _, _, r_year) = right in
    (l_year <= r_year)
  in selection_sort comp movies

(* sort list of (studio, gross in millions) by gross in millions
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list =
  let comp (left:studio_gross) (right:studio_gross) : bool =
    let (_, l_gross) = left in
    let (_, r_gross) = right in
    (l_gross <= r_gross)
  in selection_sort comp studio_grosses

(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)

(* Does this studio_gross list contain s? *)
let rec contains (s:string) (l:studio_gross list) : bool =
  match l with
  | [] -> false
  | hd::tl ->
    let (studio, gross) = hd in
    if s = studio then true
    else contains s tl

(* Update the total gross of an existing entry *)
let rec augment (s:string) (g:float) (l:studio_gross list): studio_gross list =
  match l with
  | [] -> l
  | hd::tl ->
    let (studio, gross) = hd in
    if studio = s then (studio, gross +. g)::(augment s g tl)
    else hd::(augment s g tl)

(* Takes in a movie list, adds up gross for each studio, assembles into list *)
let by_studio (movies:movie list) : studio_gross list =
  let rec make_list (movies:movie list) (result:studio_gross list) =
    match movies with
    | [] -> result
    | hd::tl ->
      let (_, studio, gross, _) = hd in
      (* Already in the list *)
      if (contains studio result) then
        make_list tl (augment studio gross result)
      (* Not in list, make a new entry *)
      else
        make_list tl ((studio, gross)::result)
  in make_list movies []


(* Testing was primarily done in another file! *)

(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
]

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
]

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
]

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
]

(* Assertion Testing *)

(* Uncomment the following when you are ready to test your take routine *)

let _ = assert(take 0 data4 = [])
let _ = assert(take 1 data1 = data1)
let _ = assert(take 2 data4 = data2)
let _ = assert(take 5 data2 = data2)
let _ = assert(take 2 data2 = data2)


(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
]

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
  else
    failwith ("bad test" ^ string_of_int i)
