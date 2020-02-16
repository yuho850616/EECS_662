--Tsung Yu Ho
--2911531
--Due: 10/29/2019

--Type :l p2.hs to compile and then type test to run test case
{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num n) = (Num n)
subst i v  (Plus l r) = (Plus (subst i v l)
                        (subst i v r))
subst i v  (Minus l r) = (Minus (subst i v l)
                        (subst i v r))
subst i v (Bind i' v' t') = if (i == i')
                            then (Bind i' (subst i v v') t')
                            else (Bind i' (subst i v v')
                                          (subst i v t'))
subst i v (Id i') = if i == i'
                    then v
                    else (Id i')


evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = if(n<0) then Nothing
                        else Just (Num n)
evalS (Plus l r) = do{ (Num l') <- (evalS l);
                       (Num r') <- (evalS r);
                       return (Num(l'+r'))
}
evalS(Minus l r) = do{ (Num l') <- (evalS l);
                       (Num r') <- (evalS r);
                       if l'>r' then return (Num (l'-r'))
                       else Nothing
}
evalS (Bind i v b) = do{v' <- (evalS v);
                        evalS (subst i v' b)}
evalS (Id s) = Nothing
evalS (Boolean x) = return (Boolean x)
evalS (And l r) = do{ (Boolean l')<- evalS l;
                     (Boolean r')<- evalS r;
                     return(Boolean(l'&&r'))
}
evalS (Leq l r)= do{ (Num l')<- evalS l;
                     (Num r')<- evalS r;
                     return(Boolean(l'<=r'))
}
evalS (IsZero l) = do{ (Num l') <- (evalS l);
                     return(Boolean(l'==0))
}
evalS (If x y z) = do{(Boolean x')<- (evalS x);
                      if x' then (evalS y) else (evalS z)
}

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM e (Num n) = if(n<0) then Nothing
                        else Just (Num n)
evalM e (Plus l r) = do{ (Num l') <- (evalM e l);
                       (Num r') <- (evalM e r);
                       return (Num(l'+r'))
}
evalM e (Minus l r) = do{ (Num l') <- (evalM e l);
                       (Num r') <- (evalM e r);
                       if l'>r' then return (Num (l'-r'))
                       else Nothing
}
evalM e (Bind i v t) = do{v' <- evalM e v;
                          evalM ((i, v'):e) t
}
evalM e (Id i ) = lookup i e
evalM e (Boolean b) = return (Boolean b)
evalM e (Leq l r) = do{ (Num l')<- (evalM e l);
                     (Num r')<- (evalM e r);
                     return(Boolean(l'<=r'))
}
evalM e (IsZero l) = do{ (Num l') <- (evalM e l);
                     return(Boolean(l'==0))
}
evalM e (If x y z) = do{(Boolean x')<- (evalM e x);
                      if x' then (evalM e y) else (evalM e z)
}

testBBAE :: BBAE -> Bool
testBBAE l = (evalS l == evalM [] l)


typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ (Num _) = return TNum
typeofM c (Plus l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum
}
typeofM c (Minus l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum
}
typeofM c (Bind i t1 t2) = do {t1' <- typeofM c t1;
                                typeofM ((i,t1'):c) t2
}
typeofM c (Id i) = lookup i c
typeofM c (Boolean b) = return TBool
typeofM c (And l r) = do { TBool <- typeofM c l;
                           TBool <- typeofM c r;
                           return TBool
}
typeofM c (Leq l r) = do { TNum <- typeofM c l;
                           TNum <- typeofM c r;
                           return TBool
}
typeofM c (IsZero l) = do {TNum <- typeofM c l;
                           return TBool
}
typeofM c (If x y z) = do{ tx <- typeofM c x;
                        ty <- typeofM c y;
                        tz <- typeofM c z;
                        if ty==tz then return ty else Nothing
}

evalT :: BBAE -> (Maybe BBAE)
evalT e = case (typeofM [] e) of
          (Just _) -> (evalM [] e)
          Nothing -> Nothing

test01 = do
            print("evalS 1 will have")
            print(evalS(Num 1))
            print("evalS -1 will have")
            print(evalS(Num (-1)))
            print("evalS 1+2 will have")
            print(evalS(Plus(Num 1)(Num 2)))
            print("evalS 1+(-2) will have")
            print(evalS(Plus(Num 1)(Num (-2))))
            print("evalS 2-1 will have")
            print(evalS(Minus(Num 2)(Num 1)))
            print("evalS 1-2 will have")
            print(evalS(Minus(Num 1)(Num 2)))
            print("evalS -1-2 will have")
            print(evalS(Minus(Num (-1))(Num 2)))
            print("evalS Bind x 1 x+1 will have")
            print(evalS(Bind ("x")(Num 1)(Plus (Num 1) (Num 1))))
            print("evalS True will have")
            print(evalS(Boolean True))
            print("evalS False will have")
            print(evalS(Boolean False))
            print("evalS True&&True will have")
            print(evalS(Boolean (True&&True)))
            print("evalS True&&False will have")
            print(evalS(Boolean (True&&False)))
            print("evalS False&&False will have")
            print(evalS(Boolean (False&&False)))
            print("evalS 3<=5 will have")
            print(evalS(Leq(Num 3)(Num 5)))
            print("evalS Leq 5<=5 will have")
            print(evalS(Leq(Num 5)(Num 5)))
            print("evalS Leq 5<=3 will have")
            print(evalS(Leq(Num 5)(Num 3)))
            print("evalS IsZero 0 will have")
            print(evalS(IsZero(Num 0)))
            print("evalS IsZero 5 will have")
            print(evalS(IsZero(Num 5)))
            print("evalS IsZero -5 will have")
            print(evalS(IsZero(Num (-5))))
            print("evalS If True then 1 else 0 will have")
            print(evalS(If(Boolean True)(Num 1)(Num 0)))
            print("evalS If False then 1 else 0 will have")
            print(evalS(If(Boolean False)(Num 1)(Num 0)))
            print("-------------------------------------")

test02 = do
           print("evalM 1 will have")
           print(evalM [](Num 1))
           print("evalM -1 will have")
           print(evalM [](Num (-1)))
           print("evalM 1+2 will have")
           print(evalM [](Plus(Num 1)(Num 2)))
           print("evalM 1+(-2) will have")
           print(evalM [](Plus(Num 1)(Num (-2))))
           print("evalM 2-1 will have")
           print(evalM [](Minus(Num 2)(Num 1)))
           print("evalM 1-2 will have")
           print(evalM [](Minus(Num 1)(Num 2)))
           print("evalM -1-2 will have")
           print(evalM [](Minus(Num (-1))(Num 2)))
           print("evalM Bind x 1 x+1 will have")
           print(evalM [](Bind ("x")(Num 1)(Plus (Num 1) (Num 1))))
           print("evalM True will have")
           print(evalM [](Boolean True))
           print("evalM False will have")
           print(evalM [](Boolean False))
           print("evalM True&&True will have")
           print(evalM [](Boolean (True&&True)))
           print("evalM True&&False will have")
           print(evalM [](Boolean (True&&False)))
           print("evalM False&&False will have")
           print(evalM [](Boolean (False&&False)))
           print("evalM 3<=5 will have")
           print(evalM [](Leq(Num 3)(Num 5)))
           print("evalM Leq 5<=5 will have")
           print(evalM [](Leq(Num 5)(Num 5)))
           print("evalM Leq 5<=3 will have")
           print(evalM [](Leq(Num 5)(Num 3)))
           print("evalM IsZero 0 will have")
           print(evalM [](IsZero(Num 0)))
           print("evalM IsZero 5 will have")
           print(evalM [](IsZero(Num 5)))
           print("evalM IsZero -5 will have")
           print(evalM [](IsZero(Num (-5))))
           print("evalM If True then 1 else 0 will have")
           print(evalM [](If(Boolean True)(Num 1)(Num 0)))
           print("evalM If False then 1 else 0 will have")
           print(evalM [](If(Boolean False)(Num 1)(Num 0)))
           print("-------------------------------------")

test03 = do
          print("typeofM 1 will have")
          print(typeofM [](Num 1))
          print("typeofM 1+2 will have")
          print(typeofM [](Plus(Num 1)(Num 2)))
          print("typeofM 2-1 will have")
          print(typeofM [](Minus(Num 2)(Num 1)))
          print("typeofM Bind x 1 x+1 will have")
          print(typeofM [](Bind ("x")(Num 1)(Plus (Num 1) (Num 1))))
          print("typeofM True will have")
          print(typeofM [](Boolean True))
          print("typeofM False will have")
          print(typeofM [](Boolean False))
          print("typeofM True&&True will have")
          print(typeofM [](Boolean (True&&True)))
          print("typeofM True&&False will have")
          print(typeofM [](Boolean (True&&False)))
          print("typeofM False&&False will have")
          print(typeofM [](Boolean (False&&False)))
          print("typeofM 3<=5 will have")
          print(typeofM [](Leq(Num 3)(Num 5)))
          print("typeofM Leq 5<=5 will have")
          print(typeofM [](Leq(Num 5)(Num 5)))
          print("typeofM Leq 5<=3 will have")
          print(typeofM [](Leq(Num 5)(Num 3)))
          print("typeofM IsZero 0 will have")
          print(typeofM [](IsZero(Num 0)))
          print("typeofM IsZero 5 will have")
          print(typeofM [](IsZero(Num 5)))
          print("typeofM If True then 1 else 0 will have")
          print(typeofM [](If(Boolean True)(Num 1)(Num 0)))
          print("typeofM If False then 1 else 0 will have")
          print(typeofM [](If(Boolean False)(Num 1)(Num 0)))
          print("-------------------------------------")



test04 = do
           print("evalT 1 will have")
           print(evalT(Num 1))
           print("evalT -1 will have")
           print(evalT(Num (-1)))
           print("evalT 1+2 will have")
           print(evalT(Plus(Num 1)(Num 2)))
           print("evalT 1+(-2) will have")
           print(evalT(Plus(Num 1)(Num (-2))))
           print("evalT 2-1 will have")
           print(evalT(Minus(Num 2)(Num 1)))
           print("evalT 1-2 will have")
           print(evalT(Minus(Num 1)(Num 2)))
           print("evalT -1-2 will have")
           print(evalT(Minus(Num (-1))(Num 2)))
           print("evalT Bind x 1 x+1 will have")
           print(evalT (Bind ("x")(Num 1)(Plus (Num 1) (Num 1))))
           print("evalT True will have")
           print(evalT(Boolean True))
           print("evalT False will have")
           print(evalT(Boolean False))
           print("evalT True&&True will have")
           print(evalT(Boolean (True&&True)))
           print("evalT True&&False will have")
           print(evalT(Boolean (True&&False)))
           print("evalT False&&False will have")
           print(evalT(Boolean (False&&False)))
           print("evalT 3<=5 will have")
           print(evalT(Leq(Num 3)(Num 5)))
           print("evalT Leq 5<=5 will have")
           print(evalT(Leq(Num 5)(Num 5)))
           print("evalT Leq 5<=3 will have")
           print(evalT(Leq(Num 5)(Num 3)))
           print("evalT IsZero 0 will have")
           print(evalT(IsZero(Num 0)))
           print("evalT IsZero 5 will have")
           print(evalT(IsZero(Num 5)))
           print("evalT IsZero -5 will have")
           print(evalT(IsZero(Num (-5))))
           print("evalT If True then 1 else 0 will have")
           print(evalT(If(Boolean True)(Num 1)(Num 0)))
           print("evalT If False then 1 else 0 will have")
           print(evalT(If(Boolean False)(Num 1)(Num 0)))
           print("-------------------------------------")




test = do
       print("-------------------------------------")
       print "evalS Test starts"
       print("-------------------------------------")
       test01
       print "evalM Test starts"
       print("-------------------------------------")
       test02
       print "typeofM Test starts"
       print("-------------------------------------")
       test03
       print "evalT Test starts"
       print("-------------------------------------")
       test04

testfunction :: IO()
testfunction = do
  test
