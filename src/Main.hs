module Main where

import Idris.AbsSyntax
import Idris.Core.TT
import Idris.ElabDecls
import Idris.Main
import Idris.ModeCommon
import Idris.Options
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenElixir

import Control.Monad (liftM)

import System.Environment
import System.Exit

import Paths_idris_elixircg

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   interface :: Bool }

showUsage = do putStrLn "A code generator which is intended to be called by the compiler, not by a user."
               putStrLn "Usage: idris-elixircg <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out" False) xs
  where
    process opts ("-o":o:xs)        = process (opts { output = o }) xs
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts (x:xs)             = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cg_main :: Opts -> Idris ()
cg_main opts = do elabPrims
                  loadInputs (inputs opts) Nothing
                  mainProg <- if interface opts
                              then return Nothing
                              else liftM Just elabMain
                  ir <- compile (Via IBCFormat "elixircg") (output opts) mainProg
                  runIO $ codegenElixir ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (cg_main opts)
