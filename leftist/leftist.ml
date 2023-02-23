type 'a queue = | Node of 'a queue * 'a queue * 'a * int
                  (* Wierzchołek składający się z lewego poddrzewa, prawego
                   * poddrzewa, wartości i prawej wysokości drzewa. *)
                | Null;;
                  (* Puste drzewo. *)

exception Empty;;

let empty = Null;;

let is_empty q = q = Null;;

(* Zwraca prawą wysokość drzewa. Gdy drzewo jest puste zwraca -1. *)
let get_height q =
  match q with
  | Null -> -1
  | Node (_, _, _, h) -> h;;

let rec join q1 q2 =
  match q1, q2 with
  | Null, q | q, Null -> q
  | Node (l1, r1, v1, h1), Node (_, _, v2, _) when v1 <= v2 ->
      let a = l1 and b = join r1 q2 in
      let ha = get_height a and hb = get_height b in
      if ha >= hb then Node (a, b, v1, hb + 1) (* warunek lewicowości *)
      else Node (b, a, v1, ha + 1)
  | _ -> join q2 q1;;

let delete_min q =
  match q with
  | Node (q1, q2, v, _) -> (v, join q1 q2)
  | Null -> raise Empty;;

let add v q = join (Node (Null, Null, v, 0)) q;;
