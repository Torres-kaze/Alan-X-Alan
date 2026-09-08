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
freshName usados = head [n | n <- candidatos, n `notElem` usados]
  where
    candidatos = [ base ++ sufijo
                 | sufijo <- "" : map show [(1 :: Int) ..]
                 , base   <- map (: []) ['a' .. 'z'] ]

sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s
  | y == x    = s
  | otherwise = Id y
sust (Num n) _ _     = Num n
sust (Boolean b) _ _ = Boolean b
sust (And es) x s  = And  [sust e x s | e <- es]
sust (Or es) x s   = Or   [sust e x s | e <- es]
sust (Add es) x s  = Add  [sust e x s | e <- es]
sust (Sub es) x s  = Sub  [sust e x s | e <- es]
sust (Mul es) x s  = Mul  [sust e x s | e <- es]
sust (Div es) x s  = Div  [sust e x s | e <- es]
sust (Lt es) x s   = Lt   [sust e x s | e <- es]
sust (Gt es) x s   = Gt   [sust e x s | e <- es]
sust (Le es) x s   = Le   [sust e x s | e <- es]
sust (Ge es) x s   = Ge   [sust e x s | e <- es]
sust (Expt a b) x s = Expt (sust a x s) (sust b x s)
sust (EqP a b) x s  = EqP  (sust a x s) (sust b x s)
sust (Not a) x s   = Not   (sust a x s)
sust (Add1 a) x s  = Add1  (sust a x s)
sust (Sub1 a) x s  = Sub1  (sust a x s)
sust (ZeroP a) x s = ZeroP (sust a x s)
sust (Let bs body) x s
  | x `elem` map fst bs = Let bs' body
  | otherwise           = Let bs'' (sust cuerpo x s)
  where
    bs' = [(y, sust e x s) | (y, e) <- bs]
    (bs'', cuerpo) = foldl renombra (bs', body) (map fst bs)
    renombra (acc, c) y
      | y `elem` freeVars s && x `elem` freeVars c =
          let z = freshName (x : freeVars s ++ names s ++ names c ++ map fst acc)
          in ([(if w == y then z else w, e) | (w, e) <- acc], sust c y (Id z))
      | otherwise = (acc, c)
sust (LetStar bs body) x s = LetStar bs' cuerpo
  where (bs', cuerpo) = sustLetStar bs body x s

sustLetStar :: [Binding] -> ASA -> String -> ASA -> ([Binding], ASA)
sustLetStar [] body x s = ([], sust body x s)
sustLetStar ((y, e) : bs) body x s
  | y == x = ((y, e') : bs, body)
  | y `elem` freeVars s && x `elem` freeVars (LetStar bs body) =
      let z = freshName (x : y : freeVars s ++ names s
                           ++ names (LetStar ((y, e) : bs) body))
          (bs1, body1) = sustLetStar bs body y (Id z)
          (bs2, body2) = sustLetStar bs1 body1 x s
      in ((z, e') : bs2, body2)
  | otherwise =
      let (bs2, body2) = sustLetStar bs body x s
      in ((y, e') : bs2, body2)
  where e' = sust e x s

sustMany :: ASA -> [Binding] -> ASA
sustMany (Id y) subs = case lookup y subs of
                         Just s  -> s
                         Nothing -> Id y
sustMany (Num n) _     = Num n
sustMany (Boolean b) _ = Boolean b
sustMany (And es) sb  = And  [sustMany e sb | e <- es]
sustMany (Or es) sb   = Or   [sustMany e sb | e <- es]
sustMany (Add es) sb  = Add  [sustMany e sb | e <- es]
sustMany (Sub es) sb  = Sub  [sustMany e sb | e <- es]
sustMany (Mul es) sb  = Mul  [sustMany e sb | e <- es]
sustMany (Div es) sb  = Div  [sustMany e sb | e <- es]
sustMany (Lt es) sb   = Lt   [sustMany e sb | e <- es]
sustMany (Gt es) sb   = Gt   [sustMany e sb | e <- es]
sustMany (Le es) sb   = Le   [sustMany e sb | e <- es]
sustMany (Ge es) sb   = Ge   [sustMany e sb | e <- es]
sustMany (Expt a b) sb = Expt (sustMany a sb) (sustMany b sb)
sustMany (EqP a b) sb  = EqP  (sustMany a sb) (sustMany b sb)
sustMany (Not a) sb   = Not   (sustMany a sb)
sustMany (Add1 a) sb  = Add1  (sustMany a sb)
sustMany (Sub1 a) sb  = Sub1  (sustMany a sb)
sustMany (ZeroP a) sb = ZeroP (sustMany a sb)
sustMany (Let bs body) sb = Let bs'' (sustMany cuerpo sb')
  where
    bs' = [(y, sustMany e sb) | (y, e) <- bs]
    sb' = [(y, s) | (y, s) <- sb, y `notElem` map fst bs]
    libres = concatMap (freeVars . snd) sb'
    (bs'', cuerpo) = foldl renombra (bs', body) (map fst bs)
    renombra (acc, c) w
      | w `elem` libres && any (\(y, _) -> y `elem` freeVars c) sb' =
          let z = freshName (map fst sb' ++ libres
                             ++ concatMap (names . snd) sb' ++ names c ++ map fst acc)
          in ([(if u == w then z else u, e) | (u, e) <- acc], sust c w (Id z))
      | otherwise = (acc, c)
sustMany (LetStar bs body) sb = LetStar bs' cuerpo
  where (bs', cuerpo) = sustManyLetStar bs body sb

sustManyLetStar :: [Binding] -> ASA -> [Binding] -> ([Binding], ASA)
sustManyLetStar [] body sb = ([], sustMany body sb)
sustManyLetStar ((y, e) : bs) body sb
  | y `elem` libres && any (\(w, _) -> w `elem` freeVars (LetStar bs body)) sb' =
      let z = freshName (y : map fst sb' ++ libres ++ concatMap (names . snd) sb'
                           ++ names (LetStar ((y, e) : bs) body))
          (bs1, body1) = sustManyLetStar bs body [(y, Id z)]
          (bs2, body2) = sustManyLetStar bs1 body1 sb'
      in ((z, e') : bs2, body2)
  | otherwise =
      let (bs2, body2) = sustManyLetStar bs body sb'
      in ((y, e') : bs2, body2)
  where
    e'     = sustMany e sb
    sb'    = [(w, s) | (w, s) <- sb, w /= y]
    libres = concatMap (freeVars . snd) sb'

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Num n)     = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _)      = Nothing
bigStep (And es) = nAria es (fmap (Boolean . and) . booleanos)
bigStep (Or es)  = nAria es (fmap (Boolean . or)  . booleanos)
bigStep (Add es) = nAria es (fmap (Num . sum)     . numeros)
bigStep (Mul es) = nAria es (fmap (Num . product) . numeros)
bigStep (Sub es) = nAria es (fmap (Num . foldl1 monus) . numeros)
bigStep (Div es) = nAria es deltaDiv
bigStep (Lt es) = nAria es (fmap (Boolean . encadena (<))  . numeros)
bigStep (Gt es) = nAria es (fmap (Boolean . encadena (>))  . numeros)
bigStep (Le es) = nAria es (fmap (Boolean . encadena (<=)) . numeros)
bigStep (Ge es) = nAria es (fmap (Boolean . encadena (>=)) . numeros)
bigStep (Expt e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num n, Num m) | m >= 0 -> Just (Num (n ^ m))
    _                       -> Nothing
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num n, Num m)         -> Just (Boolean (n == m))
    (Boolean a, Boolean b) -> Just (Boolean (a == b))
    _                      -> Nothing
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean b -> Just (Boolean (not b))
    Num _     -> Just (Boolean False)
    _         -> Nothing
bigStep (Add1 e)  = unaria e (\n -> Num (n + 1))
bigStep (Sub1 e)  = unaria e (\n -> Num (monus n 1))
bigStep (ZeroP e) = unaria e (\n -> Boolean (n == 0))
bigStep (Let bs cuerpo)
  | nub xs /= xs = Nothing
  | otherwise    = do vs <- mapM (bigStep . snd) bs
                      bigStep (sustMany cuerpo (zip xs vs))
  where xs = map fst bs
bigStep (LetStar [] cuerpo) = bigStep cuerpo
bigStep (LetStar ((x, e) : bs) cuerpo) = do
  v <- bigStep e
  bigStep (sust (LetStar bs cuerpo) x v)

-- Auxiliares

numeros :: [ASA] -> Maybe [Int]
numeros = mapM numero
  where
    numero (Num n) = Just n
    numero _       = Nothing

booleanos :: [ASA] -> Maybe [Bool]
booleanos = mapM booleano
  where
    booleano (Boolean b) = Just b
    booleano _           = Nothing

monus :: Int -> Int -> Int
monus n m = max 0 (n - m)

encadena :: (Int -> Int -> Bool) -> [Int] -> Bool
encadena op ns = and (zipWith op ns (tail ns))

nAria :: [ASA] -> ([ASA] -> Maybe ASA) -> Maybe ASA
nAria es delta
  | length es < 2 = Nothing
  | otherwise     = mapM bigStep es >>= delta

deltaDiv :: [ASA] -> Maybe ASA
deltaDiv vs = do
  ns <- numeros vs
  if 0 `elem` tail ns
    then Nothing
    else Just (Num (foldl1 div ns))

unaria :: ASA -> (Int -> ASA) -> Maybe ASA
unaria e delta = do
  v <- bigStep e
  case v of
    Num n -> Just (delta n)
    _     -> Nothing
