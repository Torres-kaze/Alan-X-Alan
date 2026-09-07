module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
import Data.List (nub)

freeVars :: ASA -> [String]
freeVars (Id x)        = [x]
freeVars (Num _)       = []
freeVars (Boolean _)   = []
freeVars (And es)      = concatMap freeVars es
freeVars (Or es)       = concatMap freeVars es
freeVars (Add es)      = concatMap freeVars es
freeVars (Sub es)      = concatMap freeVars es
freeVars (Mul es)      = concatMap freeVars es
freeVars (Div es)      = concatMap freeVars es
freeVars (Lt es)       = concatMap freeVars es
freeVars (Gt es)       = concatMap freeVars es
freeVars (Le es)       = concatMap freeVars es
freeVars (Ge es)       = concatMap freeVars es
freeVars (Expt a b)    = freeVars a ++ freeVars b
freeVars (EqP a b)     = freeVars a ++ freeVars b
freeVars (Not a)       = freeVars a
freeVars (Add1 a)      = freeVars a
freeVars (Sub1 a)      = freeVars a
freeVars (ZeroP a)     = freeVars a
freeVars (Let bs body) =
  nub (concatMap (freeVars . snd) bs
       ++ filter (`notElem` map fst bs) (freeVars body))
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((x,e):rest) body) =
  nub (freeVars e ++ filter (/= x) (freeVars (LetStar rest body)))

names :: ASA -> [String]
names (Id x)        = [x]
names (Num _)       = []
names (Boolean _)   = []
names (And es)      = concatMap names es
names (Or es)       = concatMap names es
names (Add es)      = concatMap names es
names (Sub es)      = concatMap names es
names (Mul es)      = concatMap names es
names (Div es)      = concatMap names es
names (Lt es)       = concatMap names es
names (Gt es)       = concatMap names es
names (Le es)       = concatMap names es
names (Ge es)       = concatMap names es
names (Expt a b)    = names a ++ names b
names (EqP a b)     = names a ++ names b
names (Not a)       = names a
names (Add1 a)      = names a
names (Sub1 a)      = names a
names (ZeroP a)     = names a
names (Let bs body) = concatMap (\(x,e) -> x : names e) bs ++ names body
names (LetStar bs body) = concatMap (\(x,e) -> x : names e) bs ++ names body

freshName :: [String] -> String

sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
