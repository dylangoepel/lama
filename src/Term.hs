module Term where

import Data.List (nub)

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

matchExpr :: [String] -> Expr -> Expr -> Maybe ExprCtx
matchExpr ps (Cons a b) (Cons as bs) = do ma <- matchExpr ps a as
                                          mb <- matchExpr ps (subExprCtx ma b) bs
                                          return $ ma ++ mb
matchExpr ps (Symb p) x
    | p `elem` ps = Just [(p,x)]
    | x == Symb p = Just []
    | otherwise = Nothing
matchExpr ps x y = Nothing

data Term = Appl Term Term |
            Fn Term Term |
            Arg Int |
            Sym String |
            IndT Term |
            Constr Term |
            Elim Term |
            UnitT |
            UnitElem |
            BoolT |
            If
    deriving (Show, Eq)
