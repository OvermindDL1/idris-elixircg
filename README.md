Idris Elixir Code Generator
--------------------------

This is a code generator for Idris that outputs Elixir code.

Elixir output was chosen for ease of use of integrating with the Elixir ecosystem.

cabal build && ./dist/build/idris-elixircg/idris-elixircg ../hello_lib.idr --interface -o el && echo "---" && cat el.LDeclsDebug && echo "---" && cat el
