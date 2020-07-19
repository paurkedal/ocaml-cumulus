(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open React

let (%) f g x = f (g x)

let lN_signal ?eq f sxs =
  S.map ?eq (f % List.rev) (S.merge ~eq:(==) (fun acc x -> x :: acc) [] sxs)

type ('a, 'da) change = Init of 'a | Patch of 'a * 'da | Keep of 'a
type ('a, 'da) update = 'a -> ('a, 'da) change

let value_of_change = function Init x | Patch (x, _) | Keep x -> x

type ('a, 'da) t = ('a, 'da) change signal

let const x = S.const (Init x)

let create x = S.create (Init x)

let value s = value_of_change (S.value s)

let signal s = S.map ~eq:(==) value_of_change s

let changes s = S.changes s

let stop ?strong s = S.stop ?strong s

let trace f s = S.trace f s

let integrate f e x0 =
  let f' acc dx = Patch (f dx (value_of_change acc), dx) in
  S.fold ~eq:(==) f' (Init x0) e

let skip_keep s =
  let last_change = ref None in
  let aux c =
    (match c, !last_change with
     | Keep _, Some last_change -> last_change
     | Keep x, None -> let c = Init x in last_change := Some c; c
     | (Init _ | Patch _), _ -> last_change := Some c; c)
  in
  S.l1 ~eq:(==) aux s

let fold f e i =
  let f' acc dx = f dx (value_of_change acc) in
  skip_keep (S.fold f' (Init i) e)

let deduplicator () =
  let seen = ref None in
  fun c ->
    (match !seen with
     | Some seen when seen == c -> Keep (value_of_change c)
     | _ -> seen := Some c; c)

let lift1 ~init ~patch s1 =
  let state = ref None in
  let f cx =
    (match !state with
     | None ->
        let y = init (value_of_change cx) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch cx y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l1 ~eq:(==) f s1

let lift2 ~init ~patch s1 s2 =
  let state = ref None in
  let dd1 = deduplicator () in
  let dd2 = deduplicator () in
  let f cx1 cx2 =
    (match !state with
     | None ->
        let y = init (value_of_change cx1) (value_of_change cx2) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch (dd1 cx1) (dd2 cx2) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l2 ~eq:(==) f s1 s2

let lift3 ~init ~patch s1 s2 s3 =
  let state = ref None in
  let dd1 = deduplicator () in
  let dd2 = deduplicator () in
  let dd3 = deduplicator () in
  let f cx1 cx2 cx3 =
    (match !state with
     | None ->
        let y = init (value_of_change cx1) (value_of_change cx2)
                     (value_of_change cx3) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch (dd1 cx1) (dd2 cx2) (dd3 cx3) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l3 ~eq:(==) f s1 s2 s3

let l1 ~init ~patch s1 =
  let state = ref None in
  let f cx1 =
    (match !state, cx1 with
     | None, (Init x1 | Patch (x1, _))
     | Some _, Init x1 ->
        let y = init x1 in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) as cy'), Patch (x, dx) ->
        (match patch (x, dx) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy))
     | Some (Keep _), _
     | _, Keep _ -> assert false)
  in
  S.l1 ~eq:(==) f s1

let l2 ~init ~patch s1 s2 =
  let state = ref None in
  let f cx1 cx2 =
    (match !state, cx1, cx2 with
     | None, (Init x1 | Patch (x1, _)),
             (Init x2 | Patch (x2, _))
     | Some _, Init x1, (Init x2 | Patch (x2, _))
     | Some _, Patch (x1, _), Init x2 ->
        let y = init x1 x2 in
        let cy = Init y in
        (state := Some (cx1, cx2, cy); cy)
     | Some (cx1', cx2', (Init y' | Patch (y', _) as cy')),
          Patch (x1, dx1), Patch (x2, dx2) ->
        let dx1 = if cx1 == cx1' then None else Some dx1 in
        let dx2 = if cx2 == cx2' then None else Some dx2 in
        (match patch (x1, dx1) (x2, dx2) y' with
         | Keep _ -> cy'
         | cy -> (state := Some (cx1, cx2, cy); cy))
     | Some (_, _, Keep _), _, _
     | _, Keep _, _ | _, _, Keep _ -> assert false)
  in
  S.l2 ~eq:(==) f s1 s2

let l3 ~init ~patch s1 s2 s3 =
  let state = ref None in
  let f cx1 cx2 cx3 =
    (match !state, cx1, cx2, cx3 with
     | None, (Init x1 | Patch (x1, _)),
             (Init x2 | Patch (x2, _)),
             (Init x3 | Patch (x3, _))
     | Some _, Init x1, (Init x2 | Patch (x2, _)), (Init x3 | Patch (x3, _))
     | Some _, Patch (x1, _), Init x2, (Init x3 | Patch (x3, _))
     | Some _, Patch (x1, _), Patch (x2, _), Init x3 ->
        let y = init x1 x2 x3 in
        let cy = Init y in
        (state := Some (cx1, cx2, cx3, cy); cy)
     | Some (cx1', cx2', cx3', (Init y' | Patch (y', _) as cy')),
          Patch (x1, dx1), Patch (x2, dx2), Patch (x3, dx3) ->
        let dx1 = if cx1 == cx1' then None else Some dx1 in
        let dx2 = if cx2 == cx2' then None else Some dx2 in
        let dx3 = if cx3 == cx3' then None else Some dx3 in
        (match patch (x1, dx1) (x2, dx2) (x3, dx3) y' with
         | Keep _ -> cy'
         | cy -> (state := Some (cx1, cx2, cx3, cy); cy))
     | Some (_, _, _, Keep _), _, _, _
     | _, Keep _, _, _ | _, _, Keep _, _ | _, _, _, Keep _ -> assert false)
  in
  S.l3 ~eq:(==) f s1 s2 s3

let lN ~init ~patch sxs =
  let state = ref None in
  let f cxs =
    let reset () =
      let xs = List.map value_of_change cxs in
      let y = init xs in
      let cy = Init y in
      (state := Some (cxs, cy); cy)
    in
    (match !state with
     | None ->
        reset ()
     | Some _ when List.exists (function Init _ -> true | _ -> false) cxs ->
        reset ()
     | Some (cxs', (Init y' | Patch (y', _) as cy'))
            when List.for_all (function Patch _ -> true | _ -> false) cxs ->
        let aux cx' = function
         | Patch (x, dx) as cx -> (x, (if cx == cx' then None else Some dx))
         | _ -> assert false
        in
        (match patch (List.map2 aux cxs' cxs) y' with
         | Keep _ -> cy'
         | cy -> (state := Some (cxs, cy); cy))
     | _ -> assert false)
  in
  lN_signal ~eq:(==) f sxs

let bind s f =
  let epoch_in = ref 0 in
  let on_switch c =
    incr epoch_in;
    let i = !epoch_in in
    S.map (fun c' -> (i, c')) (f c)
  in
  let epoch_out = ref 0 in
  let on_output (i, c) =
    if i = !epoch_out then c else
    begin
      epoch_out := i;
      (match c with
       | Init x | Patch (x, _) -> Init x
       | Keep _ -> assert false)
    end
  in
  S.map on_output (S.bind s on_switch)
