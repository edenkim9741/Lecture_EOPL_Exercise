module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal
value_of (Const_Exp n) env =
  Num_Val n
-- Bool_Val은 return하지 않아도 되는지 의문

value_of (Var_Exp var) env =
  apply_env env var

value_of (Diff_Exp exp1 exp2) env =
  let n1 = expval_num $ value_of exp1 env
      n2 = expval_num $ value_of exp2 env
  in Num_Val (n1 - n2)
  
value_of (IsZero_Exp exp) env =
  let b1 = expval_num $ value_of exp env
  in  if b1 == 0 then
        Bool_Val True
      else
        Bool_Val False

value_of (If_Exp exp1 exp2 exp3) env =
  let b1 = expval_bool $ value_of exp1 env
  in  if b1 == True then 
        value_of exp2 env
      else
        value_of exp3 env

value_of (Let_Exp var exp1 body) env =
  let env1 = extend_env var (value_of exp1 env) env
  in value_of body env1

value_of (Proc_Exp var body) env =
  Proc_Val (procedure var body env)

value_of (Call_Exp rator rand) env =
  apply_procedure f x
  where f = expval_proc $ value_of rator env
        x = value_of rand env

value_of (Letrec_Exp proc_name bound_var proc_body letrec_body) env =
  let env1 = extend_env_rec proc_name bound_var proc_body env
  in value_of letrec_body env1
      

--
value_of_program :: Exp -> ExpVal
value_of_program exp = 
  value_of exp initEnv
  


--
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))

--
apply_procedure :: Proc -> ExpVal -> ExpVal
apply_procedure (Procedure var body env) arg =
  value_of body $ extend_env var arg env


