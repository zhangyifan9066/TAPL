/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> LAMBDA

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
   Syntax.command list.
*/

%start toplevel
%type <Syntax.context -> Syntax.command list > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [] }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd = $1 ctx in
          let ctx' = match cmd with
              Eval(_,_, ctx'') -> ctx''
            | Bind(_,_,_, ctx'') -> ctx'' in
          let cmds = $3 ctx' in
          cmd::cmds }

/* A top-level command */
Command :
  | Term 
      { fun ctx -> let t = $1 ctx in Eval(tmInfo t, t, ctx) }
  | LCID SLASH
      { fun ctx -> Bind($1.i, $1.v, NameBind, addBinding ctx $1.v) }
  | UCID SLASH
      { fun ctx -> Bind($1.i, $1.v, NameBind, addBinding ctx $1.v) }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID DOT Term
      { fun ctx ->
          let ctx' = addBinding ctx $2.v in
          let t = $4 ctx' in
          TmAbs($1, $2.v, t)}
  | LAMBDA UCID DOT Term
      { fun ctx ->
          let ctx' = addBinding ctx $2.v in
          let t = $4 ctx' in
          TmAbs($1, $2.v, t)}

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx -> 
          let t1 = $1 ctx in
          let t2 = $2 ctx in
          TmApp(tmInfo t1, t1, t2)}

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          let index = name2index $1.i ctx $1.v in
          let len = ctxlength ctx in
          TmVar($1.i, index, len)
      }
  | UCID 
      { fun ctx ->
          let index = name2index $1.i ctx $1.v in
          let len = ctxlength ctx in
          TmVar($1.i, index, len)
      }


/*   */
