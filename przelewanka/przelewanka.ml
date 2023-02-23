(* Wyjątek podnoszony, gdy zostanie znaleziona odpowiedź. *)
exception Found of int

(* Sprawdza, czy rozwiązanie nie istnieje. *)
let is_not_solvable a =
  let rec gcd x y =
    if y = 0 then x else gcd y (x mod y) in
  let capacities_gcd = Array.fold_left (fun acc (x, _) -> gcd acc x) 0 a in
  not (Array.exists (fun (x, y) -> y = 0 || y = x) a) ||
  not (Array.for_all (fun (_, y) -> y mod capacities_gcd = 0) a)

let przelewanka a1 =
                    (* usuwa z a1 wszystkie pary (0, 0) *)
  let a = Array.of_list (List.filter (( <> ) (0, 0)) (Array.to_list a1)) in
  let n = Array.length a in
  if n = 0 then 0
  else if is_not_solvable a then -1
  else try
    let q = Queue.create ()
    and x = Array.map fst a         (* tablica pojemności *)
    and y = Array.map snd a         (* tablica oczekiwanej ilości wody *)
        (* kluczami w added są wszystkie stany, które zostały dodane do q *)
    and added = Hashtbl.create 42 in
        (* dodaje (state, steps) do q jeśli stan nie był już wcześniej dodany
           oraz sprawdza czy stan jest stanem końcowym *)
    let add state steps =
      if not (Hashtbl.mem added state) then begin
        Hashtbl.add added state true;
        Queue.add (state, steps) q;
        if state = y then raise (Found steps)     (* znaleziono odpowiedź *)
      end in
    add (Array.make n 0) 0;            (* dodanie do q stanu początkowego *)
    while not (Queue.is_empty q) do
      let (state, steps) = Queue.take q in
      for i = 0 to n - 1 do
        if state.(i) <> x.(i) then begin      (* nalewanie do szklanki i *)
          let new_state = Array.copy state in
          new_state.(i) <- x.(i);
          add new_state (steps + 1)
        end;
        if state.(i) <> 0 then begin          (* wylewanie ze szklanki i *)
          let new_state = Array.copy state in
          new_state.(i) <- 0;
          add new_state (steps + 1)
        end;
        for j = 0 to n - 1 do          (* przelewanie ze szklanki i do j *)
          let amount = min state.(i) (x.(j) - state.(j)) in
          if i <> j && amount <> 0 then begin
            let new_state = Array.copy state in
            new_state.(i) <- new_state.(i) - amount;
            new_state.(j) <- new_state.(j) + amount;
            add new_state (steps + 1)
          end
        done
      done
    done;
    -1
  with Found ans -> ans
