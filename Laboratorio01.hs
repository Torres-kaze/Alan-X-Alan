module Laboratorio01 where

-- Reto 1
distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt((x^2) + (y^2))

-- Reto 2
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

-- Reto 3
aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

-- Reto 4
varianza2 :: Double -> Double -> Double
varianza2 x y = 
let media = (x + y)/2
in ((x - media)^2 + (y - media)^2) / 2

-- Reto 5
clasificaTemperatura :: Int -> String
clasificaTemperatura n 
|n <= 0 = "Frio extremo"
|n <= 15 = "Frio"
|n <= 25 = "Templado"
|n <= 35 = "Calido"
|n >= 36 = "Calor extremo"

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
