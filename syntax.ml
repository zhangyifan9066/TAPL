open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type binding = NameBind

type context = (string * binding) list

type command =
  | Eval of info * term * context
  | Bind of info * string * binding * context

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let termShift d t = 
  let rec walk c t = match t with 
    TmVar(fi,x,n) -> if x >= c then TmVar(fi, x+d, n+d)
                     else TmVar(fi, x, n+d);
  | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk (c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)
in walk 0 t


(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi,x,n) -> if x=j+c then termShift c s else TmVar(fi,x,n)
  | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk (c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)
in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Binding *)

let name2index fi ctx name = 
    let rec name2indexRec fi ctx name = 
      match ctx iwth
          [] -> error fi ("Unbinded variable " ^ name)
        | (n,_)::rest -> if n=name 0 else (name2indexRec fi rest name) + 1
    in
    ctxlength - name2indexRec fi ctx name

let addBinding ctx name = (name, NameBind)::ctx

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let rec printtm_Term outer t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false t1;
       print_space();
       pr "then ";
       printtm_Term false t2;
       print_space();
       pr "else ";
       printtm_Term false t3;
       cbox()
  | t -> printtm_AppTerm outer t

and printtm_AppTerm outer t = match t with
    TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false t1
  | t -> printtm_ATerm outer t

and printtm_ATerm outer t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false t1; pr ")")
     in f 1 t1
  | t -> pr "("; printtm_Term outer t; pr ")"

let printtm t = printtm_Term true t 