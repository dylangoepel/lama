module Term where

import Data.List (nub)
import Expr
import Parser

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

-- apply r to the direct subterms of a term
trec :: (Term -> Term) -> Term -> Term
trec r (Appl f x) = Appl (r f) (r x)
trec r (Fn t x) = Fn (r t) (r x)
trec r (Arg i) = Arg i
trec r (Sym s) = Sym s
trec r (IndT t) = IndT (r t)
trec r (Constr t) = Constr (r t)
trec r (Elim t) = Elim (r t)
trec _ x = x

-- recursively substitute (Sym v) with the appropriate debrujin index 
subdeb :: String -> Int -> Term -> Term
subdeb s n (Sym v) = if v == s then Arg n else Sym v
subdeb s n (Fn t x) = Fn (subdeb s n t) (subdeb s (n + 1) x)
subdeb s n x = trec (subdeb s n) x

cases :: [a -> Maybe b] -> a -> Maybe b
cases [] _ = Nothing
cases (f:fs) x = case f x of
    Nothing -> cases fs x
    x -> x

elimForm :: String -> ([Term] -> Maybe Term) -> Expr -> Maybe Term
elimForm p t x = do m <- matchExpr (fparse expr p) x
                    ts <- sequence $ map (elimExpr . snd) m
                    t ts

elimForms :: [(String, [Term] -> Maybe Term)] -> Expr -> Maybe Term
elimForms = cases . map (uncurry elimForm)

unsym :: Term -> Maybe String
unsym (Sym s) = Just s
unsym _ = Nothing

elimExpr :: Expr -> Maybe Term
elimExpr = elimForms [("fn :v :t :y", \[v, t, y] -> unsym v >>= \vv -> return $ Fn t (subdeb vv 0 y)),
                      (":f :x", \[f, x] -> return $ Appl f x)]
