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
strict (ExprV e env) = evaluaEstricto env e
strict v = Just v

-- Evaluamos la expresion y exigimos lo que resulte. Es lo que usamos en
-- cada punto estricto.
evaluaEstricto :: Env -> ASA -> Maybe Value
evaluaEstricto env e = exige (bigStep env e)
 
exige :: Maybe Value -> Maybe Value
exige (Just w) = strict w
exige Nothing = Nothing

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
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Add e1 e2) =
  suma (evaluaEstricto env e1) (evaluaEstricto env e2)
bigStep env (Sub e1 e2) =
  resta (evaluaEstricto env e1) (evaluaEstricto env e2)
bigStep env (Not e) = niega (evaluaEstricto env e)
bigStep env (If c e1 e2) = eligeRama (evaluaEstricto env c) e1 e2 env
bigStep env (Fun param body) = Just (ClosureV param body env)
-- El argumento no se evalua aqui, lo pasamos tal cual junto con el ambiente
-- de la llamada.
bigStep env (App f arg) = aplica (evaluaEstricto env f) arg env
 
-- Funciones auxiliares de bigStep. Reciben los valores ya exigidos; si no
-- tienen la forma que esperamos la evaluacion queda bloqueada con Nothing.
suma :: Maybe Value -> Maybe Value -> Maybe Value
suma (Just (NumV n1)) (Just (NumV n2)) = Just (NumV (n1 + n2))
suma _ _ = Nothing
 
resta :: Maybe Value -> Maybe Value -> Maybe Value
resta (Just (NumV n1)) (Just (NumV n2)) = Just (NumV (max 0 (n1 - n2)))
resta _ _ = Nothing
 
niega :: Maybe Value -> Maybe Value
niega (Just (BooleanV b)) = Just (BooleanV (not b))
niega (Just (NumV _)) = Just (BooleanV False) -- Todo numero cuenta como verdadero
niega _ = Nothing
 
eligeRama :: Maybe Value -> ASA -> ASA -> Env -> Maybe Value
eligeRama (Just (BooleanV True)) e1 _ env = bigStep env e1
eligeRama (Just (BooleanV False)) _ e2 env = bigStep env e2
eligeRama _ _ _ _ = Nothing
 
-- Evaluamos el cuerpo en el ambiente que guardo la cerradura, agregando el
-- parametro ligado al argumento sin evaluar (ExprV) con el ambiente de la
-- llamada.
aplica :: Maybe Value -> ASA -> Env -> Maybe Value
aplica (Just (ClosureV param body closureEnv)) arg env =
  bigStep ((param, ExprV arg env) : closureEnv) body
aplica _ _ _ = Nothing
