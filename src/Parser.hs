module Parser where

import Data.Bifunctor (first, second)
import Term (Expr(Cons, Symb), matchExpr)

type PRes a = Either [(String, String)] (a, String)
data Parser a = P { parse :: String -> PRes a }

instance Functor Parser where
    fmap f (P p) = P $ second (first f) . p

instance Applicative Parser where
    pure x = P $ Right . (,) x
    (P p) <*> (P q) = P $ \s ->
        do (f, t) <- p s
           (x, tt) <- q t
           return (f x, tt)

instance Monad Parser where
    (P p) >>= f = P $ \s ->
        do (x, t) <- p s
           parse (f x) t

-- some general parsers and parser combinators
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = P $ \s -> case parse p s of
    Left e -> case parse q s of
        Left e' -> Left (e ++ e')
        x -> x
    x -> x

whitespace :: Parser ()
whitespace = P $ \s -> Right ((), dropWhile (== ' ') s)

rep p = fmap (:) p <*> (rep p <|> return [])
wsp p = whitespace *> p <* whitespace

-- Expr-specific parsers
token :: String -> Parser String
token t = P $ \s -> if take (length t) s == t
                    then Right (t, drop (length t) s)
                    else Left [("'" ++ t ++ "'", s)]

symbol :: Parser String
symbol = P $ \s -> case takeWhile (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) s of
    "" -> Left [("<symb>", s)]
    x -> Right (x, drop (length x) s)

var = Symb <$> symbol

expr = foldl1 Cons <$> rep (wsp $ var <|> (token "(" *> expr <* token ")"))

fparse p s = case parse p s of
    Right (r, _) -> r

fmatch a b = matchExpr ["p", "pp", "ppp"] (fparse expr a) (fparse expr b)
