open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isval ctx t = match t with
    TmAbs(_,_,_) -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmApp(fi,TmAbs(_,x,t12), v2) when isval ctx v2 ->
      termSubStTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval ctx t2 in
      TmApp(fi,v1,t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval ctx t1' in
      TmApp(f1,t1',t2)


let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t
