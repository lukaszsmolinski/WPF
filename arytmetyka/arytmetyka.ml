type wartosc =  Prawidlowy of float * float  | (* Prawidlowy (a, b) = wartości od a do b                       *)
                Dopelnienie of float * float | (* Dopelnienie (a, b) = wartości od -inf do a oraz od b do +inf *)
                Pusty                        | (* Pusty = przedział nie zawierający żadnych wartości           *)
                Pelny;;                        (* Pełny = przedział zawierający wszystkie wartości             *)

let wartosc_od_do x y = Prawidlowy(min x y, max x y);;

let wartosc_dokladnosc x p = wartosc_od_do (x *. (1.0 -. (p /. 100.0))) (x *. (1.0 +. (p /. 100.0)));;

let wartosc_dokladna x = wartosc_od_do x x;;

let in_wartosc x y =
    match x with
    | Prawidlowy (a, b) -> y >= a && y <= b
    | Dopelnienie (a, b) -> y <= a || y >= b
    | Pusty -> false
    | Pelny -> true;;

let min_wartosc x =
    match x with
    | Prawidlowy (a, _) -> a
    | Dopelnienie (_, _) | Pelny -> neg_infinity
    | Pusty -> nan;;

let max_wartosc x =
    match x with
    | Prawidlowy (_, b) -> b
    | Dopelnienie (_, _) | Pelny -> infinity
    | Pusty -> nan;;

let sr_wartosc x = (min_wartosc x +. max_wartosc x) /. 2.0;;

(* Zmienia przedział będący dopełnieniem na przedział pełny jeśli
 * początek przedziału jest większy lub równy niż jego koniec lub
 * przedział prawidłowy na przedział pełny jeśli jest równy (-inf, inf). *)
let popraw_przedzial x =
    match x with
    | Dopelnienie (a, b) -> if a >= b then Pelny else Dopelnienie(a, b)
    | Prawidlowy (a, b) -> if a = neg_infinity && b = infinity then Pelny
                           else Prawidlowy (a, b)
    | _ -> x;;

let rec plus x y =
    match (x, y) with
    | (Prawidlowy (a, b), Prawidlowy (c, d)) -> popraw_przedzial (Prawidlowy(a +. c, b +. d))
    | (Prawidlowy (a, b), Dopelnienie (c, d)) -> popraw_przedzial (Dopelnienie(c +. b, d +. a))
    | (Dopelnienie (_, _), Prawidlowy (_, _)) -> plus y x
    | (Dopelnienie (_, _), Dopelnienie (_, _)) -> Pelny
    | (Pusty, _) | (_, Pusty) -> Pusty
    | (Pelny, _) | (_, Pelny) -> Pelny;;

let minus x y =
    (* Zwraca przedział składający się z wartości przeciwnych do tych w przekazanym przedziale. *)
    let przeciwny x =
        match x with
        | Prawidlowy (a, b) -> Prawidlowy(-1.0 *. b, -1.0 *. a)
        | Dopelnienie (a, b) -> Dopelnienie(-1.0 *. b, -1.0 *. a)
        | Pelny -> Pelny
        | Pusty -> Pusty
    in
    plus x (przeciwny y);;

(* Zwraca przedział, do którego należą wszystkie wartości należące do co najmniej
 * jednego z przekazanych przedziałów. Rozważana jest tylko sytuacja, gdy któryś
 * z przedziałów jest pełny lub oba są prawidłowe lub oba są dopełnieniami,
 * bo w żadnym wywołaniu nie nastąpi inny przypadek. Jeden z przekazanych prawidłowych
 * przedziałów musi być postaci (-inf, x>, a drugi <y, +inf).                          *)
let rec polacz x y =
    match (x, y) with
    | (Pelny, _) | (_, Pelny) -> Pelny
    | (Prawidlowy (a, b), Prawidlowy (c, d)) -> if a > c then polacz y x
                                                else if b < c then Dopelnienie (b, c)
                                                else Pelny
    | (Dopelnienie (a, b), Dopelnienie (c, d)) -> popraw_przedzial (Dopelnienie (max a c, min b d))
    | (_, _) -> Pusty;;   (* tylko dla usunięcia warninga *)

let rec razy x y =
    let mi (a, b, c, d) = min (min a b) (min c d)
    and ma (a, b, c, d) = max (max a b) (max c d)
    and iloczyn a b = if a = 0.0 || b = 0.0 then 0.0 else a *. b
    in
    match (x, y) with
    | (Pusty, _) | (_, Pusty) -> Pusty
    | (_, Prawidlowy (0.0, 0.0)) | (Prawidlowy (0.0, 0.0), _) -> Prawidlowy(0.0, 0.0)
    | (Pelny, _) | (_, Pelny) -> Pelny
    | (Prawidlowy (a, b), Prawidlowy (c, d)) ->
        (* wszystkie możliwe końce przedziału *)
        let z = ((iloczyn a c), (iloczyn a d), (iloczyn b c), (iloczyn b d))
        in popraw_przedzial (Prawidlowy (mi z, ma z))
    | (Prawidlowy (a, b), Dopelnienie (c, d)) -> polacz
        (razy (Prawidlowy (a, b)) (Prawidlowy (neg_infinity, c)))
        (razy (Prawidlowy (a, b)) (Prawidlowy (d, infinity)))
    | (Dopelnienie (_, _), Prawidlowy (_, _)) -> razy y x
    | (Dopelnienie (a, b), Dopelnienie (c, d)) -> polacz
        (razy (Prawidlowy (neg_infinity, a)) (Dopelnienie (c, d)))
        (razy (Prawidlowy (b, infinity))  (Dopelnienie (c, d)));;

(* Zwraca przedział, do którego należą wszystkie wartości odwrotne do wartości w przedziale,
 * który został przekazany. Jeśli przekazany przedział jest dopełnieniem, to szukane są wartości
 * odwrotne do dopełnienia, a następnie wybierany jest przedział, który jest ich dopełnieniem.   *)
let odwrocony x =
    let odwroc (a, b) =
        if a = 0.0 then Prawidlowy (1.0 /. b, infinity)
        else if b = 0.0 then Prawidlowy (neg_infinity, 1.0 /. a)
        else if a *. b > 0.0 then Prawidlowy (1.0 /. b, 1.0 /. a)
        else Dopelnienie (1.0 /. a, 1.0 /. b)
    in
    match x with
    | Prawidlowy (0.0, 0.0) | Pusty -> Pusty
    | Dopelnienie (a, b) -> (match odwroc (a, b) with
                             | Dopelnienie (c, d) -> Prawidlowy (c, d)
                             | Prawidlowy (c, d) -> Dopelnienie (c, d)
                             | _ -> Pusty) (* tylko dla usunięcia warninga *)
    | Prawidlowy (a, b) -> odwroc (a, b)
    | Pelny -> Pelny;;

let podzielic x y = razy x (odwrocony y);;
