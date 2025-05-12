module Expr where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp

  -- Exp for ex7_8
  | Add_Exp    Exp Exp
  | Mul_Exp    Exp Exp
  | Quot_Exp   Exp Exp
  | IsEqual_Exp    Exp Exp
  | IsGreater_Exp  Exp Exp
  | IsLess_Exp Exp Exp
  deriving Show

type Identifier = String