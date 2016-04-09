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

let rec name2index fi ctx name = 
  match ctx with
      [] -> error fi ("Unbinded variable " ^ name)
    | (n,_)::rest -> if n=name then 0 else (name2index fi rest name) + 1

let index2name fi ctx index =
  try
    fst (List.nth ctx index)
  with Failure _ ->
    error fi ("Index " ^ (string_of_int index) ^ " is not in the current context.")

let ctxlength ctx = List.length ctx

let addBinding ctx name = (name, NameBind)::ctx

let pickfreshname ctx name =
    let rec isInContext ctx name =
      match ctx with
        [] -> false
      | (n,_)::rest -> if n = name then true else isInContext rest name
    in
    if isInContext ctx x then 
      let newName = name ^ "'" in pickfreshname ctx newName
    else addBinding ctx name, name


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

let rec printtm_Term outer ctx t = match t with
    TmAbs(fi, name, t1) ->
       obox0();
       let (ctx', x') = pickfreshname ctx x in
       (pr "lambda"; pr x'; pr ". "; print_Term outer ctx' t1;);
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi,t1,t2) ->
        obox0();
        printtm_AppTerm false ctx t1;
        pr " ";
        printtm_ATerm false ctx t2;
        cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,index,len) -> 
      obox0();
      (if ctxlength ctx = len then pr (index2name fi ctx index)
      else pr "[bad index]");
      cbox();
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 