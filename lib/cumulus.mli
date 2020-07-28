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

(** Differential Signals *)

open React

type ('a, 'da) t
type ('a, 'da) change = Init of 'a | Patch of 'a * 'da | Keep of 'a
type ('a, 'da) update = 'a -> ('a, 'da) change

(** {1 Construction and Integration} *)

val const : 'a -> ('a, 'da) t

val create : 'a -> ('a, 'da) t * (?step: Step.t -> ('a, 'da) change -> unit)

val integrate : ('da -> 'a -> 'a) -> 'da event -> 'a -> ('a, 'da) t

val fold : ('b -> ('a, 'da) update) -> 'b event -> 'a -> ('a, 'da) t

(** {1 Stopping} *)

val stop : ?strong: bool -> ('a, 'da) t -> unit

(** {1 Observation} *)

val value : ('a, 'da) t -> 'a

val signal : ('a, 'da) t -> 'a signal

val changes : ('a, 'da) t -> ('a, 'da) change event

val trace : (('a, 'da) change -> unit) -> ('a, 'da) t -> ('a, 'da) t

(** {1 Full Lifting} *)

val lift1 :
  init: ('a -> 'b) ->
  patch: (('a, 'da) change -> ('b, 'db) update) ->
  ('a, 'da) t -> ('b, 'db) t

val lift2 :
  init: ('a -> 'b -> 'c) ->
  patch: (('a, 'da) change -> ('b, 'db) change -> ('c, 'dc) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t

val lift3 :
  init: ('a -> 'b -> 'c -> 'd) ->
  patch: (('a, 'da) change -> ('b, 'db) change -> ('c, 'dc) change ->
          ('d, 'dd) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t

(** {1 Simplified Lifting} *)

val l1 :
  init: ('a -> 'b) ->
  patch: ('a * 'da -> ('b, 'db) update) ->
  ('a, 'da) t -> ('b, 'db) t

val l2 :
  init: ('a -> 'b -> 'c) ->
  patch: ('a * 'da option -> 'b * 'db option -> ('c, 'dc) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t

val l3 :
  init: ('a -> 'b -> 'c -> 'd) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          ('d, 'dd) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t

val l4 :
  init: ('a -> 'b -> 'c -> 'd -> 'e) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> ('e, 'de) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t

val l5 :
  init: ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> 'e * 'de option -> ('f, 'df) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t ->
  ('f, 'df) t

val l6 :
  init: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> 'e * 'de option -> 'f * 'df option ->
          ('g, 'dg) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t ->
  ('f, 'df) t -> ('g, 'dg) t

val lN :
  init: ('a list -> 'b) ->
  patch: (('a * 'da option) list -> ('b, 'db) update) ->
  ('a, 'da) t list -> ('b, 'db) t

(** {1 Higher Order} *)

val bind : ('a, 'da) t -> (('a, 'da) change -> ('b, 'db) t) -> ('b, 'db) t
