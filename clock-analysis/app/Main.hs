module Main where

-- initially copied from
-- https://github.com/visq/language-c/blob/master/examples/TypeCheck.hs

import Data.List
import qualified Data.Map as M
import Language.C
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.System.GCC
import System.Environment
import System.IO
import System.Exit

processFile :: CLanguage -> [String] -> FilePath -> IO ()
processFile lang cppOpts file =
  do hPutStr stderr $ file ++ ": "
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
                        (mapM_ (\(x,y) ->
                            putStrLn ("===== "++show x ++ " =====\n") >>
                            print y >> putStrLn "\n\n") $ map (\(x,y) ->
                                            (pretty x,pretty y))
                                           ast)
                        >> mapM_ (hPutStrLn stderr)
                                 ("Success" : map show errs)
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

