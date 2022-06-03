(* Project 2:

    Jacynda Alatoma (alato006)
    Dec 4 2021

 *)



(*

  This file contains definitions for the OCaml type THING, and the OCaml module
  SCANNER, both of which are needed to write the module PARSER for Project 2.

*)

(* THING. Types of the usual Lisp objects. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* SCANNER. Lexical scanner for Lisp from Lab 9. It also ignores comments. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;

(*

   *** YOUR CODE GOES HERE! ***

*)

(* Defining the type of the parser function makeParser and exception *)

module type Parsers =
    sig 
    exception Can'tParse of string          
    val makeParser: string -> unit -> thing
    end;;

module Parser : Parsers = 
  struct
    exception Can'tParse of string;; 
    let oops message = raise (Can'tParse message) (* Setting a variable to store the exception in *)

    let makeParser path = 
    let projScanner = Scanner.makeScanner path (* Creating a scanner and a variable for the most recent token *)
      in let token = ref (projScanner())
        in let rec nextThing () = 
            match !token (* create match statements for the token and the type it is *)
            with Scanner.CloseParenToken -> oops "Unexpected end of parentheses" |
            Scanner.EndToken -> oops "Unexpected end of file" | (* exceptions for either CloseParenToekn or EndTokens *)
            Scanner.NumberToken n ->  
              token := projScanner(); (* skip over tokens as listed in project html *)
              Number n |
            Scanner.SymbolToken s ->  
              token := projScanner() ;
              if s = "nil"
              then Nil (* check for both a symbol and nil *)
              else Symbol s |
            Scanner.OpenParenToken ->  
              token := projScanner();
              nextThings ()

          and nextThings () = (* create this mutually recursive so it can call next things*)
            match !token 
            with Scanner.CloseParenToken -> 
                token := projScanner();
                Nil |  
              Scanner.OpenParenToken ->  
                token := projScanner();
                let first = nextThings() (* As in lecture, create these variables for order of execution *)
                in let rest = nextThings()
                in Cons(first, rest) |
              Scanner.EndToken -> oops "Unexpected end of file" |
              Scanner.SymbolToken s -> 
                token := projScanner(); 
                Cons(Symbol s, nextThings()) |
              Scanner.NumberToken n -> 
                token := projScanner();
                Cons(Number n, nextThings())

           in nextThing ;; (* add anonymous function as the makeParser needs to have as lsited in project html *)
end;;

let nextThing = Parser.makeParser "things.txt" ;;

(* Each call to NEXT THING reads a Lisp expression, constructs an equivalent
   OCaml object, and returns that object. The comment following each call shows
   what OCaml will print if NEXT THING works correctly. These are the same
   Lisp expressions that were used to test your print function from Lab 10. *)

nextThing () ;;  (* nil *)

(* - : thing = Nil *)

nextThing () ;;  (* 7734 *)

(* - : thing = Number 7734 *)

nextThing () ;;  (* lobyms *)

(* - : thing = Symbol "lobyms" *)

nextThing () ;;  (* (a) *)

(* - : thing = Cons (Symbol "a", Nil) *)

nextThing () ;;  (* (a b) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Nil)) *)

nextThing () ;;  (* (a b c) *)

(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a) b c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil))) *)

nextThing () ;;  (* ((a b) c) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil)) *)

nextThing () ;;  (* a (b c) *)

(* - : thing =
   Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil)) *)

nextThing () ;;  (* ((a b c)) *)

(* - : thing =
   Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil) *)

nextThing () ;;  (* (define ! (lambda (n) (if (= n 0) 1 (∗ n (! (− n 1)))))) *)

(* - : thing =
   Cons (Symbol "define",
    Cons (Symbol "!",
     Cons
      (Cons (Symbol "lambda",
        Cons (Cons (Symbol "n", Nil),
         Cons
          (Cons (Symbol "if",
            Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
             Cons (Number 1,
              Cons
               (Cons (Symbol "*",
                 Cons (Symbol "n",
                  Cons
                   (Cons (Symbol "!",
                     Cons
                      (Cons (Symbol "-",
                        Cons (Symbol "n", Cons (Number 1, Nil))),
                      Nil)),
                   Nil))),
               Nil)))),
          Nil))),
      Nil))) *)

(* At this point, we've read all the Lisp expressions from "things.txt", so if
   you call NEXT THING again, it should raise the exception CAN'T PARSE. *)

nextThing () ;;

(* Exception: Parser.Can'tParse "Unexpected end of file". *)


(* Output from the testsP2.ml file and running parser file: 
# #use "parser.ml";;
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
module Scanner :
  sig
    type token =
        CloseParenToken
      | EndToken
      | NumberToken of int
      | OpenParenToken
      | SymbolToken of string
    val makeScanner : string -> unit -> token
  end
module type Parsers =
  sig
    exception Can'tParse of string
    val makeParser : string -> unit -> thing
  end
module Parser : Parsers

# #use "testsP2.ml";;
val nextThing : unit -> thing = <fun>
- : thing = Nil
- : thing = Number 7734
- : thing = Symbol "lobyms"
- : thing = Cons (Symbol "a", Nil)
- : thing = Cons (Symbol "a", Cons (Symbol "b", Nil))
- : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil))
- : thing =
Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil)
- : thing =
Cons (Symbol "define",
 Cons (Symbol "!",
  Cons
   (Cons (Symbol "lambda",
     Cons (Cons (Symbol "n", Nil),
      Cons
       (Cons (Symbol "if",
         Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
          Cons (Number 1,
           Cons
            (Cons (Symbol "*",
              Cons (Symbol "n",
               Cons
                (Cons (Symbol "!",
                  Cons
                   (Cons (Symbol "-",
                     Cons (Symbol "n", Cons (Number 1, Nil))),
                   Nil)),
                Nil))),
            Nil)))),
       Nil))),
   Nil)))
Exception: Parser.Can'tParse "Unexpected end of file".
*)

