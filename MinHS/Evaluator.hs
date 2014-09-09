module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))

applyOp :: Op -> Value -> Value -> Value
applyOp Add (I x) (I y) = I (x + y)
applyOp Sub (I x) (I y) = I (x - y)
applyOp Mul (I x) (I y) = I (x * y)
applyOp Quot (I x) (I y) = I (x `div` y)
applyOp Rem (I x) (I y) = I (x `rem` y)
applyOp Gt (I x) (I y) = B (x > y)
applyOp Ge (I x) (I y) = B (x >= y)
applyOp Lt (I x) (I y) = B (x < y)
applyOp Le (I x) (I y) = B (x <= y)
applyOp Eq (B x) (B y) = B (x == y)
applyOp Ne (B x) (B y) = B (x /= y)
applyOp Eq (I x) (I y) = B (x == y)
applyOp Ne (I x) (I y) = B (x /= y)
applyOp op _ _ = error $ "Fuck " ++ show op

applyUnOp :: Op -> Value -> Value
applyUnOp Head (Cons i v) = I i
applyUnOp Tail (Cons i v) = v
applyUnOp Null Nil = B True
applyUnOp Null (Cons i v) = B False
applyUnOp op v = error $ "Unsupported operation: " ++ show op ++ " " ++ show v

evalE :: VEnv -> Exp -> Value
evalE g (Num i) = I i
evalE g (Con "True") = B True
evalE g (Con "False") = B False
evalE g (Con "Nil") = Nil
evalE g (App (App (Con "Cons") (Num n)) list) = Cons n (evalE g list)
evalE g (App (App (Prim op) e1) e2) = applyOp op (evalE g e1) (evalE g e2)
evalE g (App (Prim Neg) (Num n)) = I (negate n)
evalE g (App (Prim op) e2) = applyUnOp op (evalE g e2)
evalE g (Let [bind] exp) = evalE (boundEnv bind) exp
    where
        boundEnv (Bind id _ _ exp) = E.add g (id, (evalE g exp))
evalE g (Var id) = case E.lookup g id of
                        (Just val) -> val
                        Nothing -> error $ "Error: Var '" ++ show id ++ "' not in scope"
evalE g e = error $ "Not yet handling:\n\t" ++ show e ++ "\nWith Context: \n\t" ++ show g
