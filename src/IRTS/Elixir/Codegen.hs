
{-# LANGUAGE OverloadedStrings #-}

module IRTS.Elixir.Codegen( codegenEx
                          ) where


import Idris.Core.TT
import IRTS.CodegenCommon
import IRTS.Elixir.AST
import IRTS.Elixir.LangTransforms
import IRTS.Exports
import IRTS.Lang


import qualified Data.Map.Strict as Map

import Control.Applicative (pure, (<$>))
import Control.Monad
import Control.Monad.Trans.State
import Data.Foldable (foldMap)
import Data.Generics.Uniplate.Data
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath




data CGStats = CGStats { usedBigInt :: Bool
                       --, partialApplications :: Set Partial
                       --, hiddenClasses :: Set HiddenClass
                       }


-- If we generate code for two declarations we want to merge their code
-- generation stats.
instance Monoid CGStats where
  mempty = CGStats { usedBigInt = False
                   --, partialApplications = Set.empty
                   --, hiddenClasses = Set.empty
                   }
  mappend x y = CGStats { usedBigInt = usedBigInt x || usedBigInt y
                        --, partialApplications = partialApplications x `Set.union` partialApplications y
                        --, hiddenClasses = hiddenClasses x `Set.union` hiddenClasses y
                        }




data CGBodyState = CGBodyState { defs :: Map Name LDecl
                               , lastIntName :: Int
                               , reWrittenNames :: Map.Map Name ExExpr
                               , currentFnNameAndArgs :: (Text, [Text])
                               , usedArgsTailCallOptim :: Set (Text, Text)
                               , isTailRec :: Bool
                               , usedITBig :: Bool
                               --, partialApps :: Set Partial
                               --, hiddenCls :: Set HiddenClass
                               }




isYes :: Maybe String -> Bool
isYes (Just "Y") = True
isYes (Just "y") = True
isYes _ = False


codegenEx :: CodeGenerator
codegenEx ci =
  do
    putStrLn "---Start---"
    debug <- isYes <$> lookupEnv "IDRIS_ELIXIR_DEBUG"
    let outfile = outputFile ci
    let defs' = Map.fromList $ liftDecls ci
    let defs = globlToCon defs'
    let iface = interfaces ci
    let used = if iface
               then Map.elems defs -- $ removeDeadCode defs (getExpNames $ exportDecls ci)
               else Map.elems $ removeDeadCode defs [sMN 0 "runMain"]
    when debug $ do
      writeFile (outputFile ci ++ ".LDeclsDebug") $ (unlines $ intersperse "" $ map show used) ++ "\n\n\n"
      putStrLn $ "Finished calculating used"
    let (out, stats) = doCodegen defs used
    let modulename = "UnknownModuleName"
    TIO.writeFile (outputFile ci) $ T.concat
                                      [ "defmodule "
                                      , modulename
                                      , " do\n"
                                      , out
                                      , "\nend\n"
                                      ]
    print ("Vwoop", out, used, defs)
    putStrLn "---Done---"
    --eitherEcg <- runExCodeGen generateEx
    --putStrLn outfile


-- | Generate code for each declaration and collect stats.
-- LFunctions are turned into JS function declarations. They are
-- preceded by a comment that gives their name. Constructor
-- declarations are ignored.
doCodegen :: Map Name LDecl -> [LDecl] -> (Text, CGStats)
doCodegen defs used = foldMap (doCodegenDecl defs) used
  where
    doCodegenDecl :: Map Name LDecl -> LDecl -> (Text, CGStats)
    doCodegenDecl defs (LFun _ name args def) =
      error $ "Vwoop!  " ++ (show name)
    doCodegenDecl defs (LConstructor n i sz) =
      -- error $ "Ack!  " ++ (show (n, i, sz))
      ("", mempty)
--   let _ = print "BLARG!" in
--   let _ = print defs in
--   let _ = print used in
--   ("bloop", mempty)
-- doCodegen defs = foldMap (doCodegenDecl defs)
--   where
--     doCodegenDecl :: Map Name LDecl -> LDecl -> (Text, CGStats)
--     doCodegenDecl defs use =
--       let _ = print "BLARG!" in
--       let _ = print defs in
--       let _ = print use in
--       ("", mempty)
--     -- doCodegenDecl defs (LFun _ name args def) =
--     --   let (ast, stats) = cgFun defs name args def
--     --       fnComment = exExpr2Text (ExComment $ T.pack $ show name)
--     --   in let _ = print $ T.concat ["Blah: ", exExpr2Text ast]
--     --   in (T.concat [fnComment, "\n", exExpr2Text ast, "\n"], stats)
--     -- doCodegenDecl defs (LConstructor n i sz) = let _ = print ("Tester", n, i, sz) in ("", mempty)


seqEx :: [ExExpr] -> ExExpr
seqEx [] = ExEmpty
seqEx (x:xs) = ExSeq x (seqEx xs)


cgFun :: Map Name LDecl -> Name -> [Name] -> LExp -> (ExExpr, CGStats)
cgFun dfs n args def = do
  let fnName = exName n
  let argNames = map exName args
  let ((decs, res),st) = runState
                          (cgBody def)
                          (CGBodyState { defs = dfs
                                       , lastIntName = 0
                                       , reWrittenNames = Map.empty
                                       , currentFnNameAndArgs = (fnName, argNames)
                                       , usedArgsTailCallOptim = Set.empty
                                       , isTailRec = False
                                       , usedITBig = False
                                       --, partialApps = Set.empty
                                       --, hiddenCls = Set.empty
                                       }
                          )
  let body = (seqEx decs) `ExSeq` res
  let fn = ExFun fnName argNames body
  let state' = CGStats { usedBigInt = usedITBig st
                       --, partialApplications = partialApps st
                       --, hiddenClasses = hiddenCls st
                       }
  (fn, state')



cgBody ::  LExp -> State CGBodyState ([ExExpr], ExExpr)
cgBody expr =
  case expr of
    -- (LCase _ (LOp oper [x, y]) [LConstCase (I 0) (LCon _ _ ff []), LDefaultCase (LCon _ _ tt [])])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") ->
    --     case (Map.lookup oper primDB) of
    --       Just (needBI, pti, c) | pti == PTBool -> do
    --         z <- mapM (cgBody GetExpBT) [x, y]
    --         when needBI setUsedITBig
    --         let res = jsPrimCoerce pti PTBool $ c $ map (exExpr2Expr . snd) z
    --         pure $ (concat $ map fst z, addRT res)
    --       _ -> cgBody' expr
    -- (LCase _ e [LConCase _ n _ (LCon _ _ tt []), LDefaultCase (LCon _ _ ff [])])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") -> do
    --        (d, v) <- cgBody GetExpBT e
    --        test <- formConTest n (exExpr2Expr v)
    --        pure $ (d, addRT $ ExUniOp (T.pack "!") $ ExUniOp (T.pack "!") test)
    -- (LCase _ e [LConCase _ n _ (LCon _ _ tt []), LConCase _ _ _ (LCon _ _ ff [])])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") -> do
    --        (d, v) <- cgBody GetExpBT e
    --        test <- formConTest n (exExpr2Expr v)
    --        pure $ (d, addRT $ ExUniOp (T.pack "!") $ ExUniOp (T.pack "!") test)
    -- (LCase _ e [LConCase _ n _ (LCon _ _ ff []), LDefaultCase (LCon _ _ tt [])])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") -> do
    --        (d, v) <- cgBody GetExpBT e
    --        test <- formConTest n (exExpr2Expr v)
    --        pure $ (d, addRT $ ExUniOp (T.pack "!") test)
    -- (LCase _ e [LConCase _ n _ (LCon _ _ ff []), LConCase _ _ _ (LCon _ _ tt [])])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") -> do
    --        (d, v) <- cgBody GetExpBT e
    --        test <- formConTest n (exExpr2Expr v)
    --        pure $ (d, addRT $ ExUniOp (T.pack "!") test)
    -- (LCase f e [LConCase nf ff [] alt, LConCase nt tt [] conseq])
    --   | (ff == qualifyN "Prelude.Bool" "False" &&
    --      tt == qualifyN "Prelude.Bool" "True") ->
    --     cgBody' $ LCase f e [LConCase nt tt [] conseq, LConCase nf ff [] alt]
    expr -> cgBody' expr

cgBody' :: LExp -> State CGBodyState ([ExExpr], ExExpr)
-- cgBody' rt (LV n) =
--   do
--     argsFn <- getArgList n
--     case argsFn of
--       Just a -> cgBody' rt (LApp False (LV n) [])
--       Nothing -> do
--         n' <- cgName n
--         pure $ ([], addRT rt n')
-- cgBody' rt (LApp tailcall (LV fn) args) =
--   do
--     let fname = exName fn
--     st <- get
--     let (currFn, argN) = currentFnNameAndArgs st
--     z <- mapM (cgBody GetExpBT) args
--     let argVals = map (exExpr2Expr . snd) z
--     let preDecs = concat $ map fst z
--     case (fname == currFn && (length args) == (length argN), rt) of
--       (True, ReturnBT) ->
--         do
--           modify (\x-> x {isTailRec = True})
--           let ((y1,y2), y3) = tailCallOptimRefreshArgs (zip argN argVals) Set.empty
--           addUsedArgsTailCallOptim y3
--           pure (preDecs, y1 `JsSeq` y2)
--       _ -> do
--         app <- formApp fn argVals
--         pure (preDecs, addRT rt app)
--
-- cgBody' rt (LForce (LLazyApp n args)) = cgBody rt (LApp False (LV n) args)
-- cgBody' rt (LLazyApp n args) =
--   do
--     (d,v) <- cgBody ReturnBT (LApp False (LV n) args)
--     pure ([], addRT rt $ jsLazy $ exExpr2Expr $ JsSeq (seqJs d) v)
-- cgBody' rt (LForce e) =
--   do
--     (d,v) <- cgBody GetExpBT e
--     pure (d, addRT rt $ JsForce $ exExpr2Expr v)
-- cgBody' rt (LLet n v sc) =
--   do
--     (d1, v1) <- cgBody (DecConstBT $ exName n) v
--     (d2, v2) <- cgBody rt sc
--     pure $ ((d1 ++ v1 : d2), v2)
-- cgBody' rt (LProj e i) =
--   do
--     (d, v) <- cgBody GetExpBT e
--     pure $ (d, addRT rt $ JsArrayProj (JsInt $ i+1) $ exExpr2Expr v)
-- cgBody' rt (LCon _  conId n args) =
--   do
--     z <- mapM (cgBody GetExpBT) args
--     con <- formCon n (map (exExpr2Expr . snd) z)
--     pure $ (concat $ map fst z, addRT rt con)
-- cgBody' rt (LCase _ e alts) = do
--   (d, v) <- cgBody GetExpBT e
--   resName <- getNewCGName
--   (decSw, entry) <-
--     case (all altHasNoProj alts && length alts <= 2, v) of
--       (True, _) -> pure (JsEmpty, exExpr2Expr v)
--       (False, ExExprStmt (JsVar n)) -> pure (JsEmpty, exExpr2Expr v)
--       _ -> do
--         swName <- getNewCGName
--         pure (JsDecConst swName $ exExpr2Expr v, JsVar swName)
--   sw' <- cgIfTree rt resName entry alts
--   let sw =
--         case sw' of
--           (Just x) -> x
--           Nothing -> ExExprStmt JsNull
--   case rt of
--     ReturnBT -> pure (d ++ [decSw], sw)
--     (DecBT nvar) -> pure (d ++ [decSw, JsDecLet nvar JsNull], sw)
--     (DecConstBT nvar) -> pure (d ++ [decSw, JsDecLet nvar JsNull], sw)
--     (SetBT nvar) -> pure (d ++ [decSw], sw)
--     GetExpBT ->
--       pure
--         (d ++ [decSw, JsDecLet resName JsNull, sw], ExExprStmt $ JsVar resName)
-- cgBody' rt (LConst c) =
--   do
--      cst <- cgConst c
--      pure ([], (addRT rt) $ cst)
-- cgBody' rt (LOp op args) =
--   do
--     z <- mapM (cgBody GetExpBT) args
--     res <- cgOp op (map (exExpr2Expr . snd) z)
--     pure $ (concat $ map fst z, addRT rt $ res)
-- cgBody' rt LNothing = pure ([], addRT rt JsNull)
-- cgBody' rt (LError x) = pure ([], JsError $ JsStr x)
-- cgBody' rt x@(LForeign dres (FStr code) args ) =
--   do
--     z <- mapM (cgBody GetExpBT) (map snd args)
--     jsArgs <- sequence $ map cgForeignArg (zip (map fst args) (map (exExpr2Expr . snd) z))
--     jsDres <- cgForeignRes dres $ JsForeign (T.pack code) jsArgs
--     pure $ (concat $ map fst z, addRT rt $ jsDres)
cgBody' x = error $ "Instruction " ++ show x ++ " not compilable yet"



-- formConTest :: Name -> ExExpr -> State CGBodyState ExExpr
-- formConTest n x = do
--   case specialCased n of
--     Just (ctor, test, match) -> pure $ test x
--     Nothing -> do
--       (conId, arity) <- getConsId n
--       pure $ ExBinOp "===" (ExProp x (T.pack "type")) (ExInt conId)
--       -- if (arity > 0)
--       --   then pure $ JsBinOp "===" (JsProp x (T.pack "type")) (JsInt conId)
--       --   else pure $ JsBinOp "===" x (JsInt conId)
--
--
-- getConsId :: Name -> State CGBodyState (Int, Int)
-- getConsId n =
--     do
--       st <- get
--       case Map.lookup n (defs st) of
--         Just (LConstructor _ conId arity) -> pure (conId, arity)
--         _ -> error $ "Internal JS Backend error " ++ showCG n ++ " is not a constructor."
--
-- getArgList' :: Name -> Map Name LDecl -> Maybe [Name]
-- getArgList' n defs =
--     case Map.lookup n defs of
--       Just (LFun _ _ a _) -> Just a
--       _ -> Nothing
--
-- getArgList :: Name -> State CGBodyState (Maybe [Name])
-- getArgList n =
--   do
--     st <- get
--     pure $ getArgList' n (defs st)




split :: Char -> String -> [String]
split c "" = [""]
split c (x:xs)
  | c == x = "" : split c xs
  | otherwise =
    let ~(h:t) = split c xs
    in ((x : h) : t)

qualify :: String -> Name -> Name
qualify "" n = n
qualify ns n = sNS n (reverse $ split '.' ns)

qualifyN :: String -> String -> Name
qualifyN ns n = qualify ns $ sUN n




addRT :: ExExpr -> ExExpr
addRT x = x


exExpr2Expr :: ExExpr -> ExExpr
exExpr2Expr x = x
