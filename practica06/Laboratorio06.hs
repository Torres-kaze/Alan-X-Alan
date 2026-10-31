module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaCola' base exponente 1
  where
    potenciaCola' :: Int -> Int -> Int -> Int
    potenciaCola' _ 0 acc = acc
    potenciaCola' b e acc = potenciaCola' b (e - 1) (b * acc)


potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k =
  potenciaCPS base (exponente - 1) (\v -> k (base * v))


-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudCola' xs 0
  where
    longitudCola' :: [a] -> Int -> Int
    longitudCola' [] acc = acc
    longitudCola' (_ : ys) acc = longitudCola' ys (acc + 1)


longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS xs k = longitudCPS' xs 0
  where
    longitudCPS' [] acc = k acc
    longitudCPS' (_ : ys) acc = longitudCPS' ys (acc + 1)


-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola = undefined

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS = undefined

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola = undefined

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS = undefined

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola = undefined

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS = undefined
