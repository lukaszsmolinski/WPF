exception Cykliczne

let topol l =
        (* wierzchołek -> lista sąsiadów *)
  let adj = ref (PMap.create compare)
        (* nieprzetworzony = brak w vis, przetwarzany = 1, przetworzony = 2 *)
  and vis = ref (PMap.create compare)
  and ans = ref [] in
  List.iter (fun (x, y) ->       (* dodawanie wierzchołków do adj *)
    if not (PMap.mem x !adj) then adj := PMap.add x y !adj
    else adj := PMap.add x (y @ PMap.find x !adj) !adj) l;
  let rec dfs v =
    let adj_list = if PMap.mem v !adj then PMap.find v !adj else [] in
    vis := PMap.add v 1 !vis;
    List.iter (fun a ->          (* przetwarzanie sąsiadów v *)
      let status = if PMap.mem a !vis then PMap.find a !vis else 0 in
      if status = 0 then dfs a
      else if status = 1 then raise Cykliczne) adj_list;
    vis := PMap.add v 2 !vis;
    ans := v::!ans in
  List.iter (fun (x, _) -> if not (PMap.mem x !vis) then dfs x) l;
  !ans
