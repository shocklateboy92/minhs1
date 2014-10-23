module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))

primOpType :: Op -> QType
primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = Ty $ Base Int `Arrow` Base Int
primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)

constType :: Id -> Maybe QType
constType "True"  = Just $ Ty $ Base Bool
constType "False" = Just $ Ty $ Base Bool
constType "()"    = Just $ Ty $ Base Unit
constType "Pair"  = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
constType "Inl"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType "Inr"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType _       = Nothing

type Gamma = E.Env QType

initialGamma :: Gamma
initialGamma = E.empty

tv :: Type -> [Id]
tv = tv'
 where
   tv' (TypeVar x) = [x]
   tv' (Prod  a b) = tv a `union` tv b
   tv' (Sum   a b) = tv a `union` tv b
   tv' (Arrow a b) = tv a `union` tv b
   tv' (Base c   ) = []

tvQ :: QType -> [Id]
tvQ (Forall x t) = filter (/= x) $ tvQ t
tvQ (Ty t) = tv t

tvGamma :: Gamma -> [Id]
tvGamma = nub . foldMap tvQ

infer :: Program -> Either TypeError Program
infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
                   return p'

unquantify :: QType -> TC Type
{-
Normally this implementation would be possible:

unquantify (Ty t) = return t
unquantify (Forall x t) = do x' <- fresh
                             unquantify (substQType (x =:x') t)

However as our "fresh" names are not checked for collisions with names bound in the type
we avoid capture entirely by first replacing each bound
variable with a guaranteed non-colliding variable with a numeric name,
and then substituting those numeric names for our normal fresh variables
-}

unquantify = unquantify' 0 emptySubst
unquantify' :: Int -> Subst -> QType -> TC Type
unquantify' i s (Ty t) = return $ substitute s t
unquantify' i s (Forall x t) = do x' <- fresh
                                  unquantify' (i + 1)
                                              ((show i =: x') <> s)
                                              (substQType (x =:TypeVar (show i)) t)
doThang t11 t12 t21 t22 = do
    bS <- unify t11 t21
    bS' <- unify t12 t22
    return $ bS <> bS'

unify :: Type -> Type -> TC Subst
unify (TypeVar a) (TypeVar b) | a == b = return emptySubst
                              | otherwise = return $ b =: TypeVar a
unify ta@(Base a) tb@(Base b) | a == b = return emptySubst
                        | otherwise = typeError $ TypeMismatch ta tb
unify (Prod t11 t12) (Prod t21 t22) = doThang t11 t12 t21 t22
unify (Sum t11 t12) (Sum t21 t22) = doThang t11 t12 t21 t22
unify (Arrow t11 t12) (Arrow t21 t22) = doThang t11 t12 t21 t22
unify (TypeVar v) t | notElem v $ tv t = return $ v =: t
                    | otherwise = typeError $ OccursCheckFailed v t
unify a b@(TypeVar _) = unify b a

unify a b = typeError $ TypeMismatch a b

generalise :: Gamma -> Type -> QType
generalise g t = 
    let 
        thang = (tv t) \\ (tvGamma g)
    in foldr Forall (Ty t) thang


inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
-- Since in the base component, this is the only thing we'll handle
inferProgram env [(Bind "main" (Nothing) [] exp)] = do
    (e, t, s) <- inferExp env exp
    return ([Bind "main" (Just $ generalise env t) [] e], t, s)
inferProgram env bs = error "Fuk da Police!! don't forget to run the result substitution on the"
                            "entire expression using allTypes from Syntax.hs"

-- Start
inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)

-- Constant types
inferExp g exp@(Con c) = case constType c of 
                            Just qt -> do 
                                t <- unquantify qt
                                return (exp, t, emptySubst)
                            Nothing -> typeError $ NoSuchConstructor c
inferExp g exp@(Num c) = return (exp, Base Int, emptySubst)

-- Prim Ops
inferExp g exp@(Prim op) = do
    t <- unquantify $ primOpType op
    return (exp, t, emptySubst)

-- Variable lookup
inferExp g exp@(Var id) = case E.lookup g id of
                                Just qt -> do 
                                    t <- unquantify qt 
                                    return (exp, t, emptySubst)
                                Nothing -> typeError $ NoSuchVariable id
--inferExp g (Let [(Bind id _ _ e1)
inferExp g (Let [(Bind id mType args e1)] e2) = do
    (e1', t, fuckYouHaskell) <- inferExp g e1
    let g' = substGamma fuckYouHaskell g
    (e2', t', fuckYouHaskell') <- inferExp (E.add g' (id, generalise g' t)) e2
    return ((Let [(Bind id (Just (generalise g' t)) args e1')] e2'), t', (fuckYouHaskell <> fuckYouHaskell'))
    
inferExp g exp@(App e1 e2) = do
    (e1', t1, bT) <- inferExp g e1
    (e2', t2, bT') <- inferExp (substGamma bT g) e2
    alpha <- fresh
    bU <- unify (substitute bT' t1) (Arrow t2 alpha)
    return (exp, substitute bU alpha, bU <> bT <> bT')

inferExp g exp = error $ "Fuk da police! 3" ++ show exp
-- -- Note: this is the only case you need to handle for case expressions
-- inferExp g (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2])
-- inferExp g (Case e _) = typeError MalformedAlternatives


