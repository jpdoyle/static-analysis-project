
module Search where

import Language.C
import Language.C.Syntax.AST
import Language.C.Analysis.SemRep
import qualified Data.Set as S
import Control.Monad ((<=<))
import Control.Applicative
import Data.Maybe (fromMaybe)

import CUtil

markBody :: (CExpr -> Bool) -> FunDef -> [CExpr]
markBody f (FunDef vdec body pos) = getMatchingExprs (exprFilter f) body

exprFilter :: (CExpr -> Bool) -> CExpr -> [CExpr]
exprFilter f e = if f e then e : exprFilter' f e else exprFilter' f e

exprFilter' :: (CExpr -> Bool) -> CExpr -> [CExpr]
exprFilter' f (CComma es loc) = es >>= exprFilter f
exprFilter' f (CAssign _ e1 e2 loc) = exprFilter f e1 ++ exprFilter f e2
exprFilter' f (CCond e1 (Just e2) e3 loc) = exprFilter f e1 ++ exprFilter f e2 ++ exprFilter f e3
exprFilter' f (CCond e1 Nothing e3 loc) = exprFilter f e1 ++ exprFilter f e3
exprFilter' f (CBinary _ e1 e2 loc) = exprFilter f e1 ++ exprFilter f e2
exprFilter' f (CCast _ e loc) = exprFilter f e
exprFilter' f (CUnary _ e loc) = exprFilter f e
exprFilter' f (CSizeofExpr e loc) = exprFilter f e
exprFilter' f (CAlignofExpr e loc) = exprFilter f e
exprFilter' f (CComplexReal e loc) = exprFilter f e
exprFilter' f (CComplexImag e loc) = exprFilter f e
exprFilter' f (CIndex e1 e2 loc) = exprFilter f e1 ++ exprFilter f e2
exprFilter' f (CCall e1 es loc) = exprFilter f e1 ++ (es >>= exprFilter f)
exprFilter' f (CMember e _ _ loc) = exprFilter f e
exprFilter' f (CVar _ _) = []
exprFilter' f (CConst _) = []
exprFilter' f (CCompoundLit _ _ loc) = [] --error "TODO"
exprFilter' f _ = [] --error "TODO"

blockStmts :: [CCompoundBlockItem a] -> [CStatement a]
blockStmts =
    map (\(CBlockStmt x) -> x) .
    filter (\s -> case s of { CBlockStmt _ -> True;
                              _ -> False })

getStats :: CStat -> [CStat]
getStats s@(CLabel _ st _ _) = s:getStats st
getStats s@(CCase exp cases _) = s:getStats cases
getStats s@(CDefault exp _) = s:getStats exp
getStats s@(CCompound ids items _) = s:(blockStmts items >>= getStats)
getStats s@(CIf e s1 (Just s2) _) = s:(getStats s1 ++ getStats s2)
getStats s@(CIf e s1 Nothing _) = s:getStats s1
getStats s@(CSwitch e body _) = s:getStats body
getStats s@(CWhile e body _ _) = s:getStats body
getStats s@(CFor init cmp step body _) = s:getStats body  --error "TODO"
getStats s = [s]

getMatchingExprs :: (CExpr -> [b]) -> CStat -> [b]
getMatchingExprs f = f <=< (topLevelExps . getStats)

topLevelExps :: [CStat] -> [CExpr]
topLevelExps stats = do
    x <- stats
    case x of
        (CCase exp cases _) -> [exp]
        (CExpr (Just e) _) -> [e]
        (CIf e s1 (Just s2) _) -> [e]
        (CIf e s1 Nothing _) -> [e]
        (CSwitch e body _) -> [e]
        (CWhile e body _ _) -> [e]
        (CFor init cmp step body _) ->
            either (maybe [] (:[])) (\decl -> getDeclExprs decl) init ++
            maybe [] (:[]) cmp ++ maybe [] (:[]) step
        (CReturn (Just e) _) -> [e]
        _ -> []

getSubExprs :: CExpr -> [CExpr]
getSubExprs = exprFilter (\_ -> True)

subExprMatches :: (CExpr -> Bool) -> CExpr -> Bool
subExprMatches f x = not $ null $ exprFilter f x

getDeclExprs :: CDecl -> [CExpr]
getDeclExprs (CStaticAssert _ _ _) = [] -- Static assertions don't affect dataflow
getDeclExprs (CDecl specs decls _) = do
    (_,init,size) <- decls
    maybe [] getInitExprs init ++ maybe [] (:[]) size

getInitExprs :: CInit -> [CExpr]
getInitExprs (CInitExpr e _) = [e]
getInitExprs (CInitList init_list _) = map snd init_list >>= getInitExprs

dirtiedBy :: (CExpr -> Bool) -> [FunDef] -> [(Bool,S.Set Ident)]
dirtiedBy pred fns = snd $ until (uncurry (==))
                                 (\(_,y) -> (y,dirty_iter y))
                                 ([],[(False,S.empty) | _ <- fns])
    where
        fn_varnames = [name | (FunDef (VarDecl name _ _) _ _) <- fns]
        fn_idents = flip map fn_varnames $ \x -> case x of
            (VarName ident _) -> Just ident
            _ -> Nothing

        matches_fn :: Ident -> CExpr -> Bool
        matches_fn fn_id (CCall (CVar id _) _ _) | id == fn_id = True
        matches_fn _ _ = False

        match_fns = flip map fn_idents $
                        fromMaybe (\_ -> False) . fmap matches_fn

        dirty_iter :: [(Bool,S.Set Ident)] -> [(Bool,S.Set Ident)]
        dirty_iter prev_results =
                map (\((b1,s1),(b2,s2)) -> (b1||b2,S.union s1 s2)) $
                    zip prev_results new_results
            where
                new_results = getZipList $
                    shallow_dirtiedBy <$> (ZipList fn_preds)
                                      <*> (ZipList fns)
                uses_dirty_fn x = any ($x) $ map fst $
                                filter (\(_,(dirty,_)) -> dirty) $
                                       zip match_fns prev_results
                uses_dirty_val = flip map (map snd prev_results)
                                      (\s x -> case x of
                                        (CVar ident _)  ->
                                            ident `S.member` s
                                        _ -> False)
                fn_preds = [(\x -> pred x || uses_dirty_fn x
                                          || subExprMatches dirty_val x)
                            | dirty_val <- uses_dirty_val]


shallow_dirtiedBy :: (CExpr -> Bool) -> FunDef -> (Bool,S.Set Ident) -- The bool is whether the return value is dirtied by any of the expressions
shallow_dirtiedBy pred fun@(FunDef vdec body pos) = (dirtyRet,S.fromList assns)
    where
        matching_exps = markBody pred fun
        matching_nodes = S.fromList $ map annotation $ markBody pred fun

        assns = do
            e <- markBody (subExprMatches (\x -> annotation x
                                                 `S.member` matching_nodes))
                          fun

            (CAssign _ (CVar ident _) _ _) <- return e
            return ident

        rets = filter isRet $ getStats body

        isRet x@(CReturn _ _) = True
        isRet _ = False

        dirtyRet = any (\x -> annotation x `S.member` matching_nodes) $ topLevelExps rets


