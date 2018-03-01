module IRTS.CodegenElixir(codegenElixir) where

import Idris.Core.TT
import IRTS.CodegenCommon
import IRTS.Elixir.Codegen
import IRTS.Exports
import IRTS.Lang

import qualified Data.Map.Strict as Map

import System.Directory

not_implemented () = putStrLn "Not implemented"

codegenElixir :: CodeGenerator
codegenElixir ci =
  if interfaces ci then
    codegenEx ci
  else
    do
      codegenEx ci
      setPermissions (outputFile ci) (emptyPermissions { readable   = True
                                                       , executable = True
                                                       , writable   = True
                                                       })
