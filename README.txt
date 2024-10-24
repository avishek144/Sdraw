This program is given in the book "Common LISP: A Gentle Introduction
to Symbolic Computation" by David S. Touretzky.  I modified it by
adding documentation strings and clearing the syntax, etc.  It
contains three user-level functions SDRAW, SDRAW-LOOP and SCRAWL.
SDRAW takes a list as input and draws corresponding cons cell diagram.
SDRAW-LOOP implements a REPL similar to the Common LISP REPL and
SCRAWL is used to "crawl around" the list structure interactively.

This generic version will work in any legal implementation of Common
LISP.

To use it, just load Sdraw.lisp using LOAD function.

It has one bug, which is that the SCRAWL prompt is printed after the
input.
