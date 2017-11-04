let rec append (l:'a list) (r:'a list) : 'a list =
  match l with
  | [] -> r
  | hd::tl -> hd :: append tl r
