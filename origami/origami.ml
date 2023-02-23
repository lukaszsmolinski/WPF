type point = float * float
(* Punkt na płaszczyźnie. *)

type kartka = point -> int
(* Funkcja reprezentująca poskładaną kartkę. Przyjmuje punkt
   i zwraca ile razy kartkę przebije szpilka wbita w danym punkcie. *)

let eps = 1e-11

let sq x = x *. x

let prostokat (x1, y1) (x2, y2) (x, y) =
  if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1 else 0

let kolko (s_x, s_y) r (x, y) =
  if sqrt (sq (s_x -. x) +. sq (s_y -. y)) <= r +. eps then 1 else 0

(* Zwraca współrzędne punktu symetrycznego do (x, y) względem prostej
   przechodzącej przez (x1, y1) oraz (x2, y2). *)
let sym (x, y) (x1, y1) (x2, y2) =
  let (a, b, c) =          (* wyznaczanie prostej ax + by + c = 0 *)
    (y1 -. y2, x2 -.x1 , x1 *. y2 -. y1 *. x2) in
  ((x *. (sq b -. sq a) -. 2. *. a *. (b *. y +. c)) /. (sq a +. sq b),
   (y *. (sq a -. sq b) -. 2. *. b *. (a *. x +. c)) /. (sq a +. sq b))

(* Zwraca -1 jeśli (x, y) leży po lewej stronie, 0 jeśli leży na oraz 1 jeśli
   leży po prawej stronie prostej przechodzącej przez (x1, y1) i (x2, y2). *)
let strona (x, y) (x1, y1) (x2, y2) =
  let d = (x -. x1) *. (y2 -. y1) -. (y -. y1) *. (x2 -. x1) in
  if d < -.eps then -1 else if d > eps then 1 else 0

let zloz p1 p2 k p =
  let s = strona p p1 p2 in
  if s = -1 then k p + k (sym p p1 p2)
  else if s = 0 then k p
  else 0

let skladaj pl k = List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) k pl
