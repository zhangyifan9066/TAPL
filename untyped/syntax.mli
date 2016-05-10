open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type binding = NameBind

type context = (string * binding) list

type command =
  | Eval of info * term * context
  | Bind of info * string * binding * context

(* Substitution *)
val termSubstTop: term -> term -> term

(* Binding *)
val name2index: info -> context -> string -> int
val index2name: info -> context -> int -> string
val ctxlength: context -> int
val addBinding: context -> string -> context
val isInContext: context -> string -> bool

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit

(* Misc *)
val tmInfo: term -> info