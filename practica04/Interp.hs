module Interp where

import Data.List (nub)
import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun params body
  | nub params /= params = Nothing -- Para cuando hay parametros repetidos
  | otherwise = Just (foldr Fun body params)

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f args = Just (foldl App f args)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op (e1 : e2 : es) = Just (foldl op (op e1 e2) es)
binaryOp _ _ = Nothing -- Para cuando hay menos de dos operandos

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)

desugar (NotS e) = do
  e' <- desugar e
  Just (Not e')

desugar (AddS es) = do
  es' <- mapM desugar es
  binaryOp Add es'

desugar (SubS es) = do
  es' <- mapM desugar es
  binaryOp Sub es'

desugar (FunS params body) = do
  body' <- desugar body
  curryFun params body'

desugar (AppS f args) = do
  f'    <- desugar f
  args' <- mapM desugar args
  curryApp f' args'

desugar (LetS x e1 e2) = do
  e1' <- desugar e1
  e2' <- desugar e2
  Just (App (Fun x e2') e1')

desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e1) : resto) body) = do
  e1' <- desugar e1
  cuerpoRestante <- desugar (LetStarS resto body)
  Just (App (Fun x cuerpoRestante) e1')

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x [] = Nothing
lookupEnv x ((y, v) : xs)
  | x == y = Just v
  | otherwise = lookupEnv x xs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env  --El unico que no se devuelve a si mismo es el Id porque hay que buscarlo
bigStep env (Num n) = Just (NumV n)   --en el ambiente
bigStep env (Boolean b) = Just (BooleanV b)
bigStep env (Add e1 e2) = do
  NumV n1 <- bigStep env e1 -- Como vimos en clase evaluamos cada lado empezando por el izquierdo,
  NumV n2 <- bigStep env e2 -- si no es un numero devolvemos Nothing y regresamos la suma con su forma esperada NumV
  return (NumV (n1 + n2))
bigStep env (Sub e1 e2) = do
  NumV n1 <- bigStep env e1 --Este es analogo al anterior recordando que nuestra resta es truncada
  NumV n2 <- bigStep env e2
  return (NumV (max 0 (n1 - n2)))
bigStep env (Not e) = do
  v <- bigStep env e
  case v of
    NumV _     -> return (BooleanV False) --En clase Manu nos dijo que todo numero se evalua como verdadero, Not lo vuelve False
    BooleanV b -> return (BooleanV (not b)) -- Este es el caso normal, si es un booleano lo negamos
bigStep env (Fun x body) = Just (ClosureV x body env) -- Devolvemos la cerradura con el ambiente actual
bigStep env (App e1 e2) = do
  ClosureV x body closureEnv <- bigStep env e1 --Seguimos la regla vista en clase con Manu, primero evaluamos la 
  argValue <- bigStep env e2  --funcion y despues el argumento, si no es una cerradura devolvemos Nothing 
  bigStep ((x, argValue) : closureEnv) body -- Por ultimo evaluamos el cuerpo de la funcion en el ambiente guardado en la cerradura, agregando la asociacion del parametro con el valor del argumento
