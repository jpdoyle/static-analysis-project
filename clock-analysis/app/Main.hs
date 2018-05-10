module Main where

-- initially copied from
-- https://github.com/visq/language-c/blob/master/examples/TypeCheck.hs

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import qualified Language.C.Data.Ident as D

import System.Environment
import System.IO
import System.Exit
import Control.Monad

import Search
import CUtil

{- TODO: move this to a dedicated testing place -}
unwrap (FunctionDef x) = x
unwrap _ = error "unwrap called on non-function"

testFn :: CExpr -> Bool
testFn (CCall (CVar (D.Ident "get_time" _ _) _) _ _) = True
testFn _ = False

getPos (OnlyPos p _) = p
getPos (NodeInfo p _ _) = p

processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do putStrLn $ file ++ ": "
     result <- parseCFile (newGCC "gcc") Nothing cppOpts file
     case result of
       Left err -> do
         hPutStrLn stderr ('\n' : show err)
         hPutStrLn stderr "Failed: Parse Error"
         exitWith (ExitFailure 1)
       Right tu -> case runTrav_ (body tu) of
                     Left errs      -> mapM_ (hPutStrLn stderr)
                                             ("Error" : map show errs)
                     Right (ast,errs) ->
                        -- (mapM_ (\(x,y,(dirty,z)) -> do {
                        --     if dirty || not (null y) || not (S.null z) then do {
                        (mapM_ (\(x,y,dirties,dirty_meets) -> do {
                            if any fst dirties || not (null y) || any (not . S.null . snd) dirties then do {
                                putStrLn ("===== "++show x ++
                                          " =====\n");
                                -- mapM_ (putStrLn . show . getPos) y;
                                forM_ (zip (map getPos y) dirties) $ (\(p,(dirty,s)) -> do {
                                    putStrLn (show p ++ (if dirty then " (dirties)" else "") ++ ":");
                                    forM_ (S.toList s) $ (\x ->
                                        putStrLn ("\t" ++ show x)
                                    );
                                    putStrLn "\n"
                                });

                                forM_ dirty_meets $ (\(a,b,uses) -> do {
                                    putStrLn $ "\nsection:\n " ++ show a ++ "\n^\nv\n " ++ show b ++ "\n  timed by:";
                                    forM_ (S.toList uses) $ (\x ->
                                        putStrLn ("\t" ++ show x)
                                    );
                                });
                                -- sequence_ $ (putStrLn . show) <$> zip (map getPos y) dirties;
                                putStrLn "\n\n"
                            } else return () }
                        ) $
                              map (\(x,y) ->
                                -- (pretty x, map annotation $ markBody testFn $ unwrap y,
                                --  head $ dirtiedBy testFn $ (:[]) $ unwrap y)) ast)
                                let sites = map annotation $ markBody testFn $ unwrap y
                                    dirtied = flip map sites $ \s ->
                                        head $ dirtiedBy ((==s) . annotation) $ (:[]) $ unwrap y
                                    dirty_uses = flip map dirtied $ \(_,s) ->
                                        S.fromList $ map annotation $ markBody (\x -> case x of { (CVar ident _) -> ident `S.member` s; _ -> False}) $ unwrap y
                                    dirty_meets = do
                                        (a_site,a_uses) <- zip sites dirty_uses
                                        (b_site,b_uses) <- zip sites dirty_uses
                                        if a_site >= b_site then [] else do
                                            let isect = S.intersection a_uses b_uses
                                            if (S.null isect) then []
                                                else return (a_site,b_site,isect)
                                in
                                    (pretty x, sites, dirtied, dirty_meets)) ast)
                        >> mapM_ (hPutStrLn stderr)
                                 ("Done" : map show errs)
  where body tu = do modifyOptions (\opts -> opts { language = lang })
                     decls <- analyseAST tu
                     let objs = gObjs decls
                     let fn_objs = (M.filter (\x -> (case x of {
                            FunctionDef _ -> True;
                            _ -> False })) objs)
                     return $ M.toList fn_objs

main :: IO ()
main =
  do args <- getArgs
     let (cppOpts, files) = partition (isPrefixOf "-") args
     mapM_ (processFile GNU99 cppOpts) files


