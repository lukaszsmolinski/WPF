type t = | Empty
           (* Puste drzewo. *)
         | Node of t * (int * int) * t * int * int
           (* Wierzchołek składający się z lewego poddrzewa, pary liczb
              całkowitych reprezentujących przedział, prawego poddrzewa,
              wysokości drzewa oraz sumy długości przedziałów przechowywanych
              w drzewie.
              Muszą być spełnione niezmienniki: wysokości lewego i prawego
              poddrzewa różnią się maksymalnie o 2 oraz
              wszystkie wartości w przechowywanym przedziale są większe
              (o co najmniej 2) niż te w lewym poddrzewie oraz mniejsze
              (o co najmniej 2) niż te w prawym. *)

let empty = Empty

let is_empty x = x = Empty

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let size = function
  | Node (_, _, _, _, sz) -> sz
  | Empty -> 0

(* Minimum z sumy liczb nieujemnych a + b + c oraz max_int. *)
let sum a b c = if a + b < 0 || a + b + c < 0 then max_int else a + b + c

(* Zwraca minimum z długości przedziału [x, y] oraz max_int. *)
let len (x, y) = if y - x + 1 <= 0 then max_int else y - x + 1

(* Porównuje dwa przedziały. Zwraca
  -1 jeśli pierwszy leży "po lewej" drugiego,
  0 jeśli mają część wspólną oraz
  1 jeśli pierwszy leży "po prawej" drugiego. *)
let cmp_p (a, b) (c, d) = if b < c then -1 else if a > d then 1 else 0

(* Porównuje liczbę i przedział. Zwraca
   -1 jeśli liczba jest mniejsza niż początek przedziału,
   0 jeśli do niego należy oraz
   1 jeśli jest większa niż koniec przedziału. *)
let cmp_s a (b, c) = if (a < b) then -1 else if a > c then 1 else 0

(* Łączy dwa drzewa l, r oraz przedział k. Wysokości drzew l, r muszą
   się różnić maksymalnie o 2. Wszystkie wartości w l muszą być mniejsze
   niż k - 1 oraz wszystkie wartości w r muszą być większe niż k + 1. *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1,
                       sum (size l) (size r) (len k))

(* Działa jak make, ale wysokości drzew l, r mogą się różnić
   maksymalnie o 3. Dokonuje pojedynczego balansowania drzewa
   jeśli jest to konieczne. *)
let bal l k r =
  let hl = height l
  and hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Zwraca parę (k, s), gdzie k to przedział o najmniejszym początku, a
   s to zbiór po usunięciu go. *)
let rec remove_min_elt = function
  | Node (Empty, k, r, _, _) -> (k, r)
  | Node (l, k, r, _, _) ->
      let (k1, n) = remove_min_elt l
      in (k1, bal n k r)
  | Empty -> assert false

(* Dodaje do zbioru przedział x = [a, b]. Żadna wartość z przedziału
   [a - 1, b + 1] nie może początkowo należeć do zbioru. *)
let rec add_disjoint x = function
  | Node (l, k, r, _, _) ->
      let c = cmp_p x k in
      if c < 0 then
        let nl = add_disjoint x l in
        bal nl k r
      else
        let nr = add_disjoint x r in
        bal l k nr
  | Empty -> make Empty x Empty

(* Działa jak make, ale drzewa l, r nie muszą być podobnej wysokości. *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_disjoint v r
  | (_, Empty) -> add_disjoint v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Zwraca (l, pres, r) gdzie l to podzbiór s zawierający wartości mniejsze
   niż x, r wartości większe niż x, a pres = true wtedy i tylko wtedy, gdy
   x należy do s. *)
let split x s =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp_s x v and (a, b) = v in
        if c = 0 then
          let l = if x > a then add_disjoint (a, x - 1) l else l
          and r = if x < b then add_disjoint (x + 1, b) r else r
          in (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x s

(* Łączy dwa drzewa, które nie muszą być podobnej wysokości.
   Wszystkie wartości w t1 muszą być mniejsze niż w t2. *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let (k, s) = remove_min_elt t2 in
      join t1 k s

(* Usuwa liczby z przedziału [x, y] ze zbioru s. *)
let remove (x, y) s =
  (* lrem wskazuje, czy usunięto już jakąś wartość "z lewej strony". Jeśli
     lrem = true oraz [x, y] nie jest rozłączny z [a, b], to można usunąć
     całe lewe poddrzewo. Analogicznie działa rrem. *)
  let rec loop (x, y) s lrem rrem =
    if x > y then s else
    match s with
    | Node (l, k, r, _, _) ->
      let (a, b) = k
      and c = cmp_p (x, y) k in
      if c = 0 then
        if x <= a && y >= b then    (* dwa wywołania loop będą maks. raz *)
          merge (if lrem then Empty else loop (x, a - 1) l lrem true)
                (if rrem then Empty else loop (b + 1, y) r true rrem)
        else if x > a && y < b then
          add_disjoint (a, x - 1) (add_disjoint (y + 1, b) (merge l r))
        else if x > a then    (* x > a,  y >= b *)
          join l (a, x - 1)
          (if rrem then Empty else loop (b + 1, y) r true rrem)
        else                  (* x <= a, y < b *)
          join (if lrem then Empty else loop (x, a - 1) l lrem true)
          (y + 1, b) r
      else if c < 0 then join (loop (x, y) l lrem rrem) k r
      else join l k (loop (x, y) r lrem rrem)
    | Empty -> Empty
    in loop (x, y) s false false

(* Zwraca (pres, (a, b)) gdzie pres = true wtedy i tylko wtedy, gdy do
   zbioru należy element x. Jeśli należy, to para (a, b) reprezentuje
   przedział, do którego należy x. *)
let rec find_elt x = function
  | Node (l, k, r, _, _) ->
      let c = cmp_s x k in
      if c = 0 then (true, k)
      else if c < 0 then find_elt x l
      else find_elt x r
  | Empty -> (false, (0, 0))

(* Dodaje wszystkie wartości z przedziału [x, y] do zbioru s. *)
let add (x, y) s =
  let x = let (pres, (x1, _)) = find_elt (x - 1) s in
          if pres && x <> min_int then x1 else x
  and y = let (pres, (_, y1)) = find_elt (y + 1) s in
          if pres && y <> max_int then y1 else y
  in add_disjoint (x, y) (remove (x, y) s)

(* Sprawdza czy wartość x należy do zbioru s. *)
let mem x s =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = cmp_s x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop s

(* Używa funkcji f na każdym przedziale zbioru s w kolejności rosnącej. *)
let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop s

(* Zwraca (f xN ... (f x2 (f x1 a))...), gdzie x1, x2, ..., xN są wszystkimi
    przedziałami należącymi do zbioru s w kolejności rosnącej. *)
let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc s

(* Zwraca listę wszystkich przedziałów należących do zbioru s w kolejności
   rosnącej. *)
let elements s =
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] s

(* Zwraca minimum z max_int oraz liczby elementów zbioru s, które są
   mniejsze lub równe n. *)
let below n s =
  let rec loop acc = function
  | Empty -> acc
  | Node (l, k, r, _, _) ->
      let c = cmp_s n k and (x, y) = k in
      if c = -1 then loop acc l
      else if c = 0 then sum acc (size l) (len (x, n))
      else loop (sum (size l) (len k) acc) r in
  loop 0 s
