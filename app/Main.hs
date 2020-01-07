module Main where

-- import Lib

main :: IO ()
main = putStrLn $ unlines (runTestCases [
                 (Var "a")
                 , (Con 23)
                 , (Add (Con 23) (Con 45))
                 , (Add (Var "a") (Con 45))
                 , (Add (Lam "x" (Var "x")) (Con 45))
                 , (Lam "x" (Var "x"))
                 , (App (Lam "x" (Add (Var "x") (Con 20))) (Con 34))
                 , (App (Con 10) (Con 34))
                 ]
             )

------ Monad definitions

--- Identity Monad
-- type M a = a
--
-- unitM :: a -> M a
-- unitM a = a
--
-- bindM :: M a -> (a -> M b) -> M b
-- a `bindM` f = f a
--
-- showM :: M Value -> String
-- showM a = showval a

-- Error Monad
data M a =
  Success a
  | Error String

unitM :: a -> M a
unitM a = Success a

errorM :: String -> M a
errorM a = Error a

bindM :: M a -> (a -> M b) -> M b
bindM (Success a) f = f a
bindM (Error s) _ = Error s

showM :: M Value -> String
showM (Success a) = "Success: " ++ showval a
showM (Error s) = "Error: " ++ s

------ Functionality (doesn't cause major change due to changing Monad definitions)
type Name = String

data Term =
  Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term

data Value =
  Wrong
  | Num Int
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

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
