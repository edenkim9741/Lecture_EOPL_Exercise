module Interp where

import Expr
import EnvStore

--
value_of :: Exp -> Env -> Store -> (ExpVal, Store)   -- Sec 4.2.1: Store-passing specifications

value_of (Const_Exp n) env store = (Num_Val n, store)

value_of (Var_Exp var) env store = 
  let (loc, store1) = apply_env env store var
  in (deref store1 loc, store1)

value_of (Diff_Exp exp1 exp2) env store = 
  let (val1, store1) = value_of exp1 env store
      (val2, store2) = value_of exp2 env store1
      num1 = expval_num val1
      num2 = expval_num val2

  in  (Num_Val (num1 - num2), store2)

  
value_of (IsZero_Exp exp) env store =
  let (val, store1) = value_of exp env store
      num = expval_num val
  in 
    if num == 0 then (Bool_Val True, store1)
    else (Bool_Val False, store1)


value_of (If_Exp exp1 exp2 exp3) env store =
  let (val, store1) = value_of exp1 env store
      isTrue = expval_bool val
  in
    if isTrue then value_of exp2 env store1
    else value_of exp3 env store1
  


value_of (Let_Exp var exp body) env store =
  let (val, store1) = value_of exp env store
      (loc, store2) = newref store1 val
      env1 = extend_env var loc env
  in value_of body env1 store2


value_of (Letrec_Exp letbindings letrec_body) env store =
  value_of letrec_body (extend_env_rec letbindings env) store

value_of (Proc_Exp var body) env store =
  (Proc_Val (procedure var body env), store)


value_of (Call_Exp rator rand) env store =
  let (proc_val, store1) = value_of rator env store
      (rand_val, store2) = value_of rand env store1
  in  apply_procedure (expval_proc proc_val) rand_val store2

value_of (Block_Exp [exp]) env store =
  value_of exp env store

value_of (Block_Exp (exp:expList)) env store =
  let (_,store1) = value_of exp env store
  in value_of (Block_Exp expList) env store1

value_of (Set_Exp x exp) env store =
  let (val1, store1) = value_of exp env store
      (loc, store2) = apply_env env store1 x
  in  (Num_Val 42, setref store2 loc val1)

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  let (val, _) = value_of exp initEnv initStore
  in val

--
initEnv = empty_env

--
apply_procedure :: Proc -> ExpVal -> Store -> (ExpVal,Store)
apply_procedure proc arg store =
  let (loc, store1) = newref store arg
  in  value_of (body proc) (extend_env (var proc) loc (saved_env proc)) store1
