--ev1.hs
------------------------------------------
-- Interpreter for EXP1
module Eval (evalExp) where

import Exp
import Err
			   
----Val data tyope----------------------
data Val = VNil
         | VN Int | VB Bool | VOp Oper
         | Partial Oper Val | VList [Val] | VTuple [Val]      
             
instance Show Val where
  show VNil = "[]"
  show (VN n) = show n
  show (VB b) = show b
  show (VOp op) = show op
  show (Partial op v) = "("++show op++" "++show v++")"
  show (VTuple vs) = showTuple (map show vs)
  show (VList bs) = show bs
  
----Env data type------------------------
type Env a = [(String, a)]

emptyEnv :: Env a
emptyEnv = []

-- When a new association (binding) is added, the old association
-- still exists in the list.
updEnv :: String -> a -> Env a -> Env a
updEnv n x e = (n, x):e

findEnv :: String -> Env a -> Maybe a
findEnv n' ((n,x):nxs) =
  if n' == n then  Just x
  else  findEnv n' nxs
findEnv n' [] = Nothing

----ev1 function-----------------------------

ev1 env Nil     = S VNil
ev1 env (N n)   = S (VN n)
ev1 env (B b)   = S (VB b)
ev1 env (Op op) = S (VOp op)
ev1 env (Var x) =
  case (findEnv x env) of
    Just x' -> S x'
    Nothing -> Error $ "unbound variable: " ++ x
ev1 env (If e1 e2 e3) =
  case (ev1 env e1) of
    S (VB c)  -> if c then ev1 env e2 else ev1 env e3
    S _       -> Error "'if' condition not a boolean"
    Error err -> Error err
ev1 env (App e1 e2) =
  case (ev1 env e1) of
    Error err -> Error err
    S v1 -> case (ev1 env e2) of
        Error err -> Error err
        S v2 -> appVals v1 v2	
ev1 env (Let [x] e be) =
  case (ev1 env e) of
    Error err -> Error err
    S v       -> ev1 (updEnv x v env) be	
ev1 env (Tuple es) = Error "tuples not implemented, your assignment 5"
                     --Hint: case mapError (ev1 env) es of ...
						  
-----operator aplications----------------------------------------------
appVals :: Val -> Val -> Error Val
appVals (VOp op)           v2     = appOp op v2
appVals (Partial op v1 )   v2     = appBinOp op v1 v2
appVals v1 v2 = Error $ (show v1)
                        ++ " cannot be applied to " ++ show v2	
						
appOp :: Oper -> Val -> Error Val
appOp Not  (VB b)         = S $ VB $ not b
appOp Not  _              = Error "not applied to non-boolean"
appOp Head (VList (v:vs)) = Error "head not implemented, your assignment 5"
appOp Tail (VList (v:vs)) = Error "tail not implemented, your assignment 5"
appOp op v2               = S $ Partial op v2

appBinOp :: Oper -> Val -> Val -> Error Val
appBinOp Plus  (VN n) (VN n') = S $ VN (n + n')
appBinOp Times (VN n) (VN n') = S $ VN (n * n')
appBinOp Equal (VN n) (VN n') = S $ VB (n == n')
appBinOp And   (VB b) (VB b') = S $ VB (b && b')
appBinOp Or    (VB b) (VB b') = S $ VB (b || b')
appBinOp Cons  v1      v2     = Error "cons not implemented, your assignment 5"
appBinOp op v v' =
  Error $ "binary operator " ++ show op 
           ++ "not defined on arguments " 
           ++ (show v) ++ " and " ++ (show v')
						
-----interpreter and test cases-------------
evalExp e = ev1 emptyEnv e

test1 = App (App (Op Plus) (N 5)) (N 3)

test2 = If (B False) (N 1) (N 0)

test3 = If (App (Op Not) (App (App (Op Equal) (N 3)) (N 3))) (N 10) (N 100)

test4 = Let ["x"] (N 10) (App (Op Times) (Var "x"))
