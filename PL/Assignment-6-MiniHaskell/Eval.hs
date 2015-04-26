--Eval.hs 
------------------------------------------
-- Interpreter for Mini-Haskell
module Eval (evalExp, evalProg) where

import Exp
import Err
			   
----Val data tyope----------------------
data Val = VNil
         | VN Int | VB Bool | VOp Oper
         | Partial Oper Val | VList [Val]  
		 | VTuple [Val]
         | VLam [String] Exp (Env Val)		 
             
instance Show Val where
  show VNil = "[]"
  show (VN n) = show n
  show (VB b) = show b
  show (VOp op) = show op
  show (Partial op v) = "("++show op++" "++show v++")"
  show (VTuple vs) = showTuple (map show vs)
  show (VLam xs e env) = "<closure> for " ++ show e
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
ev1 env (Lam xs e) = Error "Lambda not implemented, your assignment 6"	
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
ev1 env (Tuple es) = Error "Tuples not implemented, your assignment 5"
                     --Hint: case mapError (ev1 env) es of ...
						  
-----operator aplications----------------------------------------------
appVals :: Val -> Val -> Error Val
appVals (VOp op)           v2     = appOp op v2
appVals (Partial op v1 )   v2     = appBinOp op v1 v2
appVals (VLam [x] e env)   v2     = ev1 (updEnv x v2 env) e
appVals v1 v2 = Error $ (show v1)
                        ++ " cannot be applied to " ++ show v2	
						
appOp :: Oper -> Val -> Error Val
appOp Not  (VB b)         = S $ VB $ not b
appOp Not  _              = Error "not applied to non-boolean"
appOp Head (VList (v:vs)) = Error "Listss not implemented, your assignment 5"
appOp Tail (VList (v:vs)) = Error "Lists not implemented, your assignment 5"
appOp op v2               = S $ Partial op v2

appBinOp :: Oper -> Val -> Val -> Error Val
appBinOp Plus  (VN n) (VN n') = S $ VN (n + n')
appBinOp Times (VN n) (VN n') = S $ VN (n * n')
appBinOp Equal (VN n) (VN n') = S $ VB (n == n')
appBinOp And   (VB b) (VB b') = S $ VB (b && b')
appBinOp Or    (VB b) (VB b') = S $ VB (b || b')
appBinOp Cons  v      (VList vs) = Error " Lists implemented, your assignment 5"
appBinOp Cons  v      VNil    = Error "Lists not implemented, your assignment 5"
appBinOp op v v' =
  Error $ "binary operator " ++ show op 
           ++ "not defined on arguments " 
           ++ (show v) ++ " and " ++ (show v')
						
-----interpreter and test cases-------------
evalExp e = ev1 emptyEnv e

evalProg (Program decls e) = Error "evalProg not implemented, your assignment 6"

----some test cases------------------------
test1 = App (App (Op Plus) (N 5)) (N 3)

test2 = If (B False) (N 1) (N 0)

test3 = If (App (Op Not) (App (App (Op Equal) (N 3)) (N 3))) (N 10) (N 100)

test4 = Let ["x"] (N 10) (App (Op Times) (Var "x"))

--new test cases-----

test5 = Lam ["x"] (Var "x")

test6 = App (Lam ["x"] (App (App (Op Times) (Var "x")) (Var "x"))) (N 5)

test7 = Let ["f"] (Lam ["x"] (App (App (Op Times) (Var "x")) (Var "x"))) (Var "f") 

test8 = Let ["f"] (Lam ["x"] (App (App (Op Times) (N 2)) (Var "x"))) (App (Var "f") (N 8))

test9 = (App (Op Tail) (App (App (Op Cons) (N 1)) Nil))

test10 = Let ["twice"] (Lam ["f"] (Lam ["x"] (App (Var "f") (App (Var "f") (Var "x"))))) 
              (App (App (Var "twice") (Lam ["z"] (App (App (Op Plus) (Var "z")) (Var "z")))) (N 5))

testprog1 = Program [ (Decl "x" (N 10)) ] (Var "x")

testprog2 = Program [ (Decl "a" (N 5)), (Decl "b" (N 10))] 
            (If (App (App (Op Equal) (Var "a")) (Var  "b"))  
			    (App (App (Op Times) (Var "a")) (Var  "b"))   (App (App (Op Plus) (Var "a")) (Var  "b")))

testprog3 = Program [ (Decl "f" (Lam ["x"] (App (App (Op Times) (N 2)) (Var "x")))) ] (App (Var "f") (N 5))
 