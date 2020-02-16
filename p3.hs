--Tsung Yu Ho
--2911531
--Due: 11/27/2019

--Type stack ghci and then type :l p3.hs to compile


{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE e (Num n) = Just(Num n)
evalDynFAE e (Plus l r) = do { (Num l') <- evalDynFAE e l;
                               (Num r') <- evalDynFAE e r;
                               return (Num (l' + r')) }
evalDynFAE env (Minus l r) = do{ (Num l') <- (evalDynFAE env l);
                                  (Num r') <- (evalDynFAE env r);
                                  return (Num (l' - r')) }
evalDynFAE e (Lambda i b) = Just (Lambda i b)
evalDynFAE e (App f a) = do{ (Lambda i b) <- evalDynFAE e f;
                              a' <- evalDynFAE e a;
                              evalDynFAE ((i, a'):e) b}
evalDynFAE e (Id i) = (lookup i e)



data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)

type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE e (Num n) = Just (NumV n)
evalStatFAE e (Plus l r) = do { (NumV l') <- evalStatFAE e l;
                                (NumV r') <- evalStatFAE e r;
                                return (NumV (l' + r')) }
evalStatFAE e (Minus l r) = do { (NumV l') <- evalStatFAE e l;
                                (NumV r') <- evalStatFAE e r;
                                return (NumV (l' - r')) }
evalStatFAE e (Lambda i b) = Just (ClosureV i b e)
evalStatFAE e (App f a) = do{(ClosureV i b e) <- evalStatFAE e f;
                            a'<- evalStatFAE e a;
                            evalStatFAE ((i,a'):e) b}
evalStatFAE e (Id i) = lookup i e


-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = Num n
elabFBAE (PlusD l r) = Plus (elabFBAE l)(elabFBAE r)
elabFBAE (MinusD l r) = Minus (elabFBAE l)(elabFBAE r)
elabFBAE (LambdaD i b) = Lambda i (elabFBAE b)
elabFBAE (AppD f a) = App (elabFBAE f)(elabFBAE a)
elabFBAE (BindD i v b) = (App(Lambda i(elabFBAE b)) (elabFBAE v))
elabFBAE (IdD i) = Id i

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE e x = evalStatFAE e (elabFBAE x)

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC (NumE n) = Num n
elabFBAEC (PlusE l r) = Plus (elabFBAEC l)(elabFBAEC r)
elabFBAEC (MinusE l r) = Minus (elabFBAEC l)(elabFBAEC r)
elabFBAEC (TrueE) = Lambda "t" (Lambda "f" (Id "t"))
elabFBAEC (FalseE) = Lambda "t" (Lambda "f" (Id "f"))
elabFBAEC (AndE l r) = (App(App(Lambda "l"(Lambda "r"(App(App (Id "l")(Id "r")) (elabFBAEC FalseE)))) (elabFBAEC l)) (elabFBAEC r))
elabFBAEC (OrE l r) = (App(App(Lambda "l"(Lambda "r"(App(App (Id "l")(elabFBAEC TrueE))(Id "r"))))(elabFBAEC l)) (elabFBAEC r))
elabFBAEC (NotE l) = (App(Lambda "l"(App(App (Id "l")(elabFBAEC FalseE))(elabFBAEC TrueE)))(elabFBAEC l))
elabFBAEC (IfE c t e) = (Lambda "c"(Lambda "t"(Lambda "e"(App(App (Id "c")(Id "t")) (Id "e")))))
elabFBAEC (LambdaE i b) = Lambda i (elabFBAEC b)
elabFBAEC (AppE f a) = App (elabFBAEC f)(elabFBAEC a)
elabFBAEC (BindE i v b) = (App(Lambda i(elabFBAEC b)) (elabFBAEC v))
elabFBAEC (IdE i) = Id i

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC e x = evalStatFAE e (elabFBAEC x)
