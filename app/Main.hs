module Main where

-- import Lib

main :: IO ()
main = putStrLn $ unlines (runTestCases [
                 (Var "a")
                 , (Con 23)
                 , (Add (Con 23) (Con 45))
                 , (Add (At 6 (Var "a")) (Con 45))
                 , (At 56 (Add (Lam "x" (Var "x")) (Con 45)))
                 , (Lam "x" (Var "x"))
                 , (App (Lam "x" (Add (Var "x") (Con 20))) (Con 34))
                 , (At 109 (App (Con 10) (Con 34)))
                 ]
             )

------ Monad definitions

--- Identity Monad
type I a = a

unitI :: a -> I a
unitI a = a

bindI :: I a -> (a -> I b) -> I b
a `bindI` f = f a

showI :: I Value -> String
showI a = showval a

-- Error Monad
data E a =
  Success a
  | Error String

unitE :: a -> E a
unitE a = Success a

errorE :: String -> E a
errorE a = Error a

bindE :: E a -> (a -> E b) -> E b
bindE (Success a) f = f a
bindE (Error s) _ = Error s

showE :: E Value -> String
showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

-- Positional error Monad
type P a = Position -> E a

unitP :: a -> P a
unitP a = const (unitE a)

errorP :: String -> P a
errorP s = \p -> errorE (showpos p ++ ": " ++ s)

bindP :: P a -> (a -> P b) -> P b
bindP pa f = \p -> pa p `bindE` (\a -> f a p)

showP :: P Value -> String
showP pv = showE (pv 0)

resetP :: Position -> P a -> P a
resetP p pa = const (pa p)

------ Aliases
type M a = P a
unitM = unitP
errorM = errorP
bindM = bindP
showM = showP

------ Functionality (doesn't cause major change due to changing Monad definitions)
type Name = String

type Position = Int

data Term =
  Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term
  | At Position Term

data Value =
  Wrong
  | Num Int
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showpos :: Position -> String
showpos = show

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun _) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup' x e
interp (Con c) _ = unitM (Num c)
interp (Add t1 t2) e = interp t1 e `bindM` (\a ->
                         interp t2 e `bindM` (\b ->
                           add a b
                         )
                       )
interp (Lam n t) e = unitM (Fun (\a -> interp t ((n,a):e)))
interp (App t1 t2) e = interp t1 e `bindM` (\f ->
                         interp t2 e `bindM` (\x ->
                           apply f x
                         )
                       )
interp (At p t) e = resetP p $ interp t e -- for Positional error Monad

lookup' :: Name -> Environment -> M Value
-- lookup' _ [] = unitM Wrong
lookup' x [] = errorM ("Unbound variable: " ++ x) -- for Error Monad
lookup' x ((n,v):r) = if x == n then unitM v else lookup' x r

add :: Value -> Value -> M Value
add (Num a) (Num b) = unitM (Num (a+b))
-- add _ _ = unitM Wrong
add a b = errorM ("Cannot add incompatible values: "
                  ++ showval a ++ " and " ++ showval b) -- for Error Monad

apply :: Value -> Value -> M Value
apply (Fun f) x = f x
-- apply _ _ = unitM Wrong
apply a _ = errorM ("Cannot apply nonfunction: " ++ showval a) -- for Error Monad

test :: Term -> String
test t = showM (interp t [])

runTestCases :: [Term] -> [String]
runTestCases = map test
