{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternGuards #-}

module IRTS.Elixir.AST
  ( ExExpr(..)
  , exExpr2Text
  , exName
  ) where



import Data.Char
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import Numeric

import Idris.Core.TT
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T




data ExExpr
  = ExEmpty
  | ExComment Text
  | ExInt Int
  | ExBool Bool
  | ExDouble Double
  | ExList [ExExpr]
  | ExAtom String
  | ExString String
  | ExProp ExExpr
           Text
  | ExUniOp Text
            ExExpr
  | ExBinOp Text
            ExExpr
            ExExpr
  | ExSeq ExExpr
          ExExpr
  | ExFun Text
          [Text]
          ExExpr



exExpr2Text :: ExExpr -> Text
exExpr2Text ExEmpty = ""
exExpr2Text (ExComment c) = T.unlines $ map ("# " `T.append`) $ T.lines c
exExpr2Text (ExAtom s) = ":\"" `T.append` T.pack (concatMap translateChar s) `T.append` "\"" -- TODO:  Print atoms properly
exExpr2Text (ExString s) = "\"" `T.append` T.pack (concatMap translateChar s) `T.append` "\""
exExpr2Text (ExUniOp op a) = T.concat ["(", op, exExpr2Text a, ")"]
exExpr2Text (ExBinOp op a1 a2) =
  T.concat ["(", exExpr2Text a1, " ", op, " ", exExpr2Text a2, ")"]
exExpr2Text (ExFun name args body) =
  T.concat
    [ "def "
    , name
    , "("
    , T.intercalate ", " args
    , ") do\n"
    , indent $ exExpr2Text body
    , "end\n\n"
    ]
exExpr2Text (ExSeq ExEmpty y) = exExpr2Text y
exExpr2Text (ExSeq x ExEmpty) = exExpr2Text x
exExpr2Text (ExSeq x y) =
  T.concat [exExpr2Text x, "\n", exExpr2Text x]



translateChar :: Char -> String
translateChar ch
  | '\b'   <- ch       = "\\b"
  | '\f'   <- ch       = "\\f"
  | '\n'   <- ch       = "\\n"
  | '\r'   <- ch       = "\\r"
  | '\t'   <- ch       = "\\t"
  | '\v'   <- ch       = "\\v"
  | '\\'   <- ch       = "\\\\"
  | '\"'   <- ch       = "\\\""
  | '\''   <- ch       = "\\\'"
  | ord ch < 0x20      = "\\x" ++ pad 2 (showHex (ord ch) "") -- Fix this up to be Elixir format instead
  | ord ch < 0x7f      = [ch]  -- 0x7f '\DEL'
  | ord ch <= 0xff     = "\\x" ++ pad 2 (showHex (ord ch) "")
  | ord ch <= 0xffff   = "\\u" ++ pad 4 (showHex (ord ch) "")
  | ord ch <= 0x10ffff = "\\u{" ++ showHex (ord ch) "}"
  | otherwise          = error $ "Invalid Unicode code point U+" ++ showHex (ord ch) ""
  where
    pad :: Int -> String -> String
    pad n s = replicate (n - length s) '0' ++ s



indent :: Text -> Text
indent x =
  let l = T.lines x
      il = map (\y -> T.replicate 4 " " `T.append` y) l
  in T.unlines il


exName :: Name -> Text
exName (MN i u) = T.concat ["internal__", T.pack (show i), "_", T.pack $ exEscape $ T.unpack u]
exName n = T.pack $ exEscape $ showCG n

exEscape :: String -> String
exEscape = concatMap exchar
  where
    exchar x
      | isAlpha x || isDigit x = [x]
      | x == '.' = "__"
      | otherwise = "_" ++ show (fromEnum x) ++ "_"
