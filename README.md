# Lisp Interpreter
Second project in CSCI 2041 completed Fall 2021. It is a basic interpreter for Lisp.

## Introduction
In this programming project, you will write an OCaml module whose functions parse Lisp thing’s from a file, and return the internal representations of those thing’s. It will use the token scanner from Lab 9, in a module called Scanner. If we have a function that reads Lisp thing’s, a function that evaluates Lisp thing’s, and a function that prints Lisp thing’s, then we could put them all together to make a complete Lisp system. Writing the evaluator will be the subject of the next few lectures.

## Implementation
For this project, you must write an OCaml module called Parser, whose type is the OCaml signature Parsers. (A parser is a procedure that reads a series of tokens and constructs a representation of what the tokens stand for.) The module Parser will use functions defined in the module Scanner from Lab 9. Although Parser may contain many functions, only two OCaml objects must be visible outside it: the exception Can'tParse, and the function makeParser.

**All the code written is of my own work.**

 - Note: The comments at the bottom of the file are my hand written test cases and the outputs that they produced.
