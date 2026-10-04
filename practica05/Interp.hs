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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun params body
  | nub params /= params = Nothing -- Para cuando hay parametros repetidos
  | otherwise = Just (foldr Fun body params)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f args = Just (foldl App f args)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op (e1 : e2 : es) = Just (foldl op (op e1 e2) es)
binaryOp _ _ = Nothing -- Para cuando hay menos de dos operandos


-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa = desugar alternativa
-- Tomamos la primera clausula como un If y en su rama falsa ponemos el
-- desazucarado de las clausulas que quedan.
desugarCond ((c, e) : clausulas) alternativa =
  construyeIf (desugar c) (desugar e) (desugarCond clausulas alternativa)
-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS es) = aplicaOp Add (desugarLista es)
desugar (SubS es) = aplicaOp Sub (desugarLista es)
desugar (NotS e) = construyeNot (desugar e)
desugar (FunS params body) = construyeFun params (desugar body)
desugar (AppS f args) = construyeApp (desugar f) (desugarLista args)
desugar (LetS x e1 e2) = construyeLet x (desugar e1) (desugar e2)
-- Sacamos la primera ligadura como un let y las demas se quedan como un let*
-- dentro del cuerpo, asi cada ligadura puede usar las anteriores.
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e1) : resto) body) =
  desugar (LetS x e1 (LetStarS resto body))
desugar (IfS c e1 e2) = construyeIf (desugar c) (desugar e1) (desugar e2)
desugar (CondS clausulas alternativa) = desugarCond clausulas alternativa
-- Pasamos el letrec a un let donde f queda ligada a (Y (lambda (f) s1)) y
-- desazucaramos ese let.
desugar (LetRecS f s1 s2) =
  desugar (LetS f (AppS (IdS "Y") [FunS [f] s1]) s2)

-- Auxiliares del desazucarado. Reciben el resultado (Maybe) de desazucarar
-- cada parte: si todas las partes son Just construyen el nodo del nucleo, y
-- si alguna es Nothing el resultado completo es Nothing.

desugarLista :: [SASA] -> Maybe [ASA]
desugarLista = foldr (agrega . desugar) (Just [])
 
agrega :: Maybe ASA -> Maybe [ASA] -> Maybe [ASA]
agrega (Just e) (Just es) = Just (e : es)
agrega _ _ = Nothing
 
aplicaOp :: (ASA -> ASA -> ASA) -> Maybe [ASA] -> Maybe ASA
aplicaOp op (Just es) = binaryOp op es
aplicaOp _ Nothing = Nothing
 
construyeNot :: Maybe ASA -> Maybe ASA
construyeNot (Just e) = Just (Not e)
construyeNot Nothing = Nothing
 
construyeFun :: [Nombre] -> Maybe ASA -> Maybe ASA
construyeFun params (Just body) = curryFun params body
construyeFun _ Nothing = Nothing
 
construyeApp :: Maybe ASA -> Maybe [ASA] -> Maybe ASA
construyeApp (Just f) (Just args) = curryApp f args
construyeApp _ _ = Nothing
 
-- LetS x e1 e2 ==> App (Fun x e2') e1'
construyeLet :: Nombre -> Maybe ASA -> Maybe ASA -> Maybe ASA
construyeLet x (Just e1) (Just e2) = Just (App (Fun x e2) e1)
construyeLet _ _ _ = Nothing
 
construyeIf :: Maybe ASA -> Maybe ASA -> Maybe ASA -> Maybe ASA
construyeIf (Just c) (Just e1) (Just e2) = Just (If c e1 e2)
construyeIf _ _ _ = Nothing


-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v) : env)
  | x == y = Just v
  | otherwise = lookupEnv x env

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict :: Value -> Maybe Value
strict (NumV n) = Just (NumV n)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x b env) = Just (ClosureV x b env)
strict (ExprV e env) = bigStep env e >>= strict

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Id x) = lookupEnv x env
bigStep env (Fun p cuerpo) = Just (ClosureV p cuerpo env)
bigStep env (Add e1 e2) = do
  n1 <- fuerzaNum env e1
  n2 <- fuerzaNum env e2
  Just (NumV (n1 + n2))
bigStep env (Sub e1 e2) = do
  n1 <- fuerzaNum env e1
  n2 <- fuerzaNum env e2
  Just (NumV (max 0 (n1 - n2)))
bigStep env (Not e) = do
  b <- fuerzaBool env e
  Just (BooleanV (not b))
bigStep env (If c t e) = do
  b <- fuerzaBool env c
  if b then bigStep env t else bigStep env e
bigStep env (App f a) =
  case bigStep env f >>= strict of
    Just (ClosureV p cuerpo envCierre) ->
      bigStep ((p, ExprV a env) : envCierre) cuerpo
    _ -> Nothing

-- Auxiliares: evaluan una expresion, la fuerzan (punto estricto) y exigen
-- que el valor obtenido sea del tipo que la operacion necesita.
fuerzaNum :: Env -> ASA -> Maybe Int
fuerzaNum env e = case bigStep env e >>= strict of
  Just (NumV n) -> Just n
  _ -> Nothing
 
fuerzaBool :: Env -> ASA -> Maybe Bool
fuerzaBool env e = case bigStep env e >>= strict of
  Just (BooleanV b) -> Just b
  _ -> Nothing
