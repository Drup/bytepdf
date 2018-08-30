# A tool to create PDFs that are also OCaml bytecodes 

The `bytepdf` tool allows you to take a PDF `foo.pdf` and an OCaml bytecode `foo.byte` and merges them into a file that is both a valid PDF and a valid bytecode.

```
bytepdf --ml foo.byte --pdf foo.pdf -o bar.pdf
```

The resulting file can both be read as a pdf and executed by the ocaml interpreter:

```
open bar.pdf
ocamlrun bar.pdf
```

Furthermore, if you open the PDF with Acrobat Reader, the PDF will contain the OCaml bytecode as a file attachment. For more details, you can read the help. For an explanation of how this work, consider looking at [this abstract](abstract.pdf).
The only current limitation is that the bytecode should not
have been statically linked with C code.

## Install

```
opam install bytepdf
```
