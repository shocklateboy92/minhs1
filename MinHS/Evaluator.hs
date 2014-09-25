module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value
data PVal = PVal (Value -> Value) 
instance Show PVal where
    show _ = "<function>"

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | Closure VEnv Bind
           | PApp PVal
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

-- Helper functions for constructing partial application closures
makeBOp :: (Integer -> Integer -> Bool) -> Value
makeBOp f = PApp $ PVal (\(I v1) -> PApp (PVal (\(I v2) -> (B (v1 `f` v2)))))

makeIOp :: (Integer -> Integer -> Integer) -> Value
makeIOp f = PApp $ PVal (\(I v1) -> PApp (PVal (\(I v2) -> (I (v1 `f` v2)))))


-- Start
evalE :: VEnv -> Exp -> Value

-- Primitives
evalE g (Num i) = I i
evalE g (Con "True") = B True
evalE g (Con "False") = B False
evalE g (Con "Nil") = Nil
evalE g (Con "Cons") = PApp $ PVal $ \(I i) -> PApp $ PVal $ \v -> Cons i v

-- Variable lookup
evalE g (Var id) = case E.lookup g id of
                        (Just val) -> val
                        Nothing -> error $ "Error: Var " ++ show id ++ " not in scope"

-- Variable binding
evalE g (Let binds exp) = evalE (boundEnv binds g) exp
    where
        boundEnv ((Bind id _ _ exp):bs) g' = boundEnv bs $ E.add g' (id, (evalE g' exp))
        boundEnv [] g' = g'

-- IfthenElse blocks
evalE g (If e1 e2 e3) = case evalE g e1 of
                            B True -> evalE g e2
                            B False -> evalE g e3

-- Constructing function closures
evalE g l@(Letfun b@(Bind id t args exp)) = doThang g' id args exp
    where
        g' = E.add g (id, (evalE g l))

        doThang g id (arg:args) exp = PApp $ PVal $
            \v -> doThang (E.add g (arg, v)) id (args) exp
        doThang g id [] exp = evalE g exp
--evalE g l@(Letfun b@(Bind id t (arg:args) exp)) = PApp $ PVal $
--    \v -> evalE (E.add g (arg, v)) (Letfun (Bind id t (args) exp))
--evalE g l@(Letfun b@(Bind id _ [] exp)) = PApp $ PVal $ \v ->
--    let
--        g' = E.addAll g [(id, (evalE g l))]
--    in evalE g' exp

-- Constructing PrimOp closures
evalE g (Prim Add) = makeIOp (+)
evalE g (Prim Sub) = makeIOp (-)
evalE g (Prim Mul) = makeIOp (*)
evalE g (Prim Quot) = makeIOp div
evalE g (Prim Rem) = makeIOp rem
evalE g (Prim Gt) = makeBOp (>)
evalE g (Prim Ge) = makeBOp (>=)
evalE g (Prim Lt) = makeBOp (<)
evalE g (Prim Le) = makeBOp (<=)
evalE g (Prim Eq) = makeBOp (==)
evalE g (Prim Ne) = makeBOp (/=)

evalE g (Prim Head) = PApp $ PVal $ \(Cons i v) -> I i
evalE g (Prim Tail) = PApp $ PVal $ \(Cons i v) -> v
evalE g (Prim Null) = PApp $ PVal $ \v -> e v
                                        where
                                            e (Nil)      = B True
                                            e (Cons _ _) = B False
evalE g (Prim Neg) = PApp $ PVal $ \(I v1) -> I (negate v1)

-- Applying partial application closures
evalE g (App e1 e2) = case evalE g e1 of
                        PApp (PVal f) -> f $ evalE g e2
                        x -> error $ "wtf?! how did I get a: " ++ show x

evalE g e = error $ "Not yet handling:\n\t" ++ show e ++ "\nWith Context: \n\t" ++ show g

