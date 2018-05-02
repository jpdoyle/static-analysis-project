
module CUtil where

import Language.C
import Language.C.Syntax.AST

{- TODO: Make a typeclass and encapsulate of these get___ functions in that -}

getConstInfo :: CConstant a -> a
getConstInfo (CIntConst _ x) = x
getConstInfo (CCharConst _ x) = x
getConstInfo (CFloatConst _ x) = x
getConstInfo (CStrConst _ x) = x

getExprInfo :: CExpression a -> a
getExprInfo = annotation
-- getExprInfo (CComma _ x) = x
-- getExprInfo (CAssign _ _ _ x) = x
-- getExprInfo (CBinary _ _ _ x) = x
-- getExprInfo (CCast _ _ x) = x
-- getExprInfo (CUnary  _ _ x) = x
-- getExprInfo (CSizeofExpr _ x) = x
-- getExprInfo (CSizeofType _ x) = x
-- getExprInfo (CAlignofExpr _ x) = x
-- getExprInfo (CAlignofType _ x) = x
-- getExprInfo (CComplexReal _ x) = x
-- getExprInfo (CComplexImag _ x) = x
-- getExprInfo (CIndex _ _ x) = x
-- getExprInfo (CCall _ _ x) = x
-- getExprInfo (CMember _ _ _ x) = x
-- getExprInfo (CVar _ x) = x
-- getExprInfo (CConst c) = getConstInfo c
-- getExprInfo (CCompoundLit _ _ x) = x
-- getExprInfo (CGenericSelection _ _ x) = x
-- getExprInfo (CStatExpr _ a) = a
-- getExprInfo (CLabAddrExpr _ a) = a
-- getExprInfo (CBuiltinExpr c) = error "TODO"

