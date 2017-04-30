
open Core.Std

exception Key_not_found 

type ('k, 'v) t =
  | T of 'k * 'v * ('k, 'v) t * ('k, 'v) t
  | E

let empty = E

let rec lookup t k = match t with
  | E -> None
  | T (a, v, l, r) when k = a -> Some v
  | T (a, _, l, r) -> if k > a then lookup r k
                      else lookup l k

let rec insert t k v = match t with
  | E -> T (k, v, E, E)
  | T (a, _, _, _) as t' when k = a -> t'
  | T (a, u, l, r) -> if k > a then T (a, u, l, insert r k v)
                      else T (a, u, insert l k v, r)
                        
let rec update t k v = match t with
  | E -> raise Key_not_found
  | T (a, u, l, r) -> if a = k then T (a, v, l, r)
                      else if k > a then T (a, u, l, update r k v)
                      else T(a, u, update l k v, r)

let rec iter t f = match t with
  | E -> ()
  | T (a, v, l, r) -> f a v;
                      iter l f;
                      iter r f
