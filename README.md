# Convert Coq to Scheme Extraction to Common Lisp / Emacs Lisp, and auxiliary macros for them.

## Requirement

R7RS Scheme implementation.

You may run `unschme-impl.scm` on R5RS Scheme implementation
with some additional procedures:

* error
* command-line
* exit
* current-error-port

## How to use.

Extract Scheme code.

    % coqc example.v

Convert extracted code to Common Lisp / Emacs Lisp.
For example, with [chibi-scheme](https://code.google.com/p/chibi-scheme/),

    % chibi-scheme unscheme.scm example.scm example.lisp

or

    % chibi-scheme unscheme.scm example.scm example.el

Output format is automatically determined by the file extension.

And run.

    % sbcl --load macros_extr.lisp --load example.lisp --script test.lisp
