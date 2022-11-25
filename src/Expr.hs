module Expr where
data Expr = Cons Expr Expr | Symb String
    deriving (Show, Eq)

reprExpr :: Expr -> String
reprExpr (Symb s) = s
reprExpr (Cons a b) = reprExpr a ++ " " ++ reprCons b
    where reprCons (Cons x y) = "(" ++ reprCons x ++ " " ++ reprExpr y ++ ")"
          reprCons x = reprExpr x

subExpr :: String -> Expr -> Expr -> Expr
subExpr p t (Symb s) = if s == p then t else Symb s
subExpr p t (Cons a b) = Cons (subExpr p t a) (subExpr p t b)

type ExprCtx = [(String, Expr)]

subExprCtx :: ExprCtx -> Expr -> Expr
subExprCtx ((s,t):xs) = subExpr s t . subExprCtx xs
subExprCtx [] = id

matchExpr :: Expr -> Expr -> Maybe ExprCtx
matchExpr (Cons a b) (Cons as bs) = do ma <- matchExpr a as
                                       mb <- matchExpr (subExprCtx ma b) bs
                                       return $ ma ++ mb
matchExpr (Symb (':':vs)) x = Just [(':':vs, x)]
matchExpr x y = if x == y then Just [] else Nothing

