module Laboratorio01 where

-- Reto 1
distanciaOrigen :: Double -> Double -> Double
distanciaOrigen = undefined

-- Reto 2
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares = undefined

-- Reto 3
aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces = undefined

-- Reto 4
varianza2 :: Double -> Double -> Double
varianza2 = undefined

-- Reto 5
clasificaTemperatura :: Int -> String
clasificaTemperatura = undefined

-- Reto 6
intercala :: a -> [a] -> [a]
intercala _ []  = []
intercala _ [x] = [x]
intercala separador (x:xs) = x : separador : intercala separador xs

-- Reto 7
data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
