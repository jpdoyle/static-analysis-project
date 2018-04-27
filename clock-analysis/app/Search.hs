
module Search where

import Language.C
import Language.C.Syntax.AST
import Language.C.Analysis.SemRep

import CUtil

markBody :: (CExpr -> Bool) -> FunDef -> [NodeInfo]
markBody f (FunDef vdec body pos) = traverseAST body
  where
    check :: CExpr -> [NodeInfo]
    check e = if f e then getExprInfo e : check' e else check' e

    check' :: CExpr -> [NodeInfo]
    check' (CComma es loc) = es >>= check'
    check' (CAssign _ e1 e2 loc) = check' e1 ++ check' e2
    check' (CCond e1 (Just e2) e3 loc) = check' e1 ++ check' e2 ++ check' e3
    check' (CCond e1 Nothing e3 loc) = check' e1 ++ check' e3
    check' (CBinary _ e1 e2 loc) = check' e1 ++ check' e2
    check' (CCast _ e loc) = check' e
    check' (CUnary _ e loc) = check' e
    check' (CSizeofExpr e loc) = check' e
    check' (CAlignofExpr e loc) = check' e
    check' (CComplexReal e loc) = check' e
    check' (CComplexImag e loc) = check' e
    check' (CIndex e1 e2 loc) = check' e1 ++ check' e2
    check' (CCall e1 es loc) = check' e1 ++ (es >>= check')
    check' (CMember e _ _ loc) = check' e
    check' (CVar _ _) = []
    check' (CConst _) = []
    check' (CCompoundLit _ _ loc) = [] --error "TODO"
    check' _ = []--error "TODO"

    blockStmts :: [CCompoundBlockItem a] -> [CStatement a]
    blockStmts =
      map (\(CBlockStmt x) -> x) .
        filter (\s -> case s of CBlockStmt _ -> True
                                _ -> False)
    traverseAST :: CStat -> [NodeInfo]
    traverseAST (CLabel _ st _ _) = traverseAST st
    traverseAST (CCase exp cases _) = check exp ++ traverseAST cases
    traverseAST (CDefault exp _) = traverseAST exp
    traverseAST (CExpr (Just e) _) = check e
    traverseAST (CCompound ids items _) = blockStmts items >>= traverseAST
    traverseAST (CIf e s1 (Just s2) _) =
      check e ++ traverseAST s1 ++ traverseAST s2
    traverseAST (CIf e s1 Nothing _) = check e ++ traverseAST s1
    traverseAST (CSwitch e body _) = check e ++ traverseAST body
    traverseAST (CWhile e body _ _) = check e ++ traverseAST body
    traverseAST (CFor _ _ _ _ _) = []--error "TODO"
    traverseAST (CReturn (Just e) _) = check e
    traverseAST _ = []

