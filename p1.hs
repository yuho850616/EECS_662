--Tsung Yu Ho
--2911531
--Due: 10/01/2019

--Type :l p1.hs to compile and then type test to run test case

{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)
evalM (Num n) = if(n<0) then Nothing
                        else Just (Num n)
evalM (Plus l r) = do { (Num l') <- evalM l;
                        (Num r') <- evalM r;
                        return (Num(l'+r'))}
evalM (Minus l r) = do { (Num l') <- evalM l;
                        (Num r') <- evalM r;
                        if l'>r' then return (Num (l'-r'))
                        else Nothing }
evalM (Mult l r) = do { (Num l') <- evalM l;
                        (Num r') <- evalM r;
                        return (Num(l'*r'))}
evalM (Div l r) = do { (Num l') <- evalM l;
                       (Num r') <- evalM r;
                       if (r'== 0) then Nothing
                       else return (Num(l' `div` r'))}
evalM (Boolean x) = Just (Boolean x)
evalM (And l r)= do{ (Boolean l')<- evalM l;
                     (Boolean r')<- evalM r;
                     return(Boolean(l'&&r'))}
evalM(Leq l r)= do{ (Num l')<- evalM l;
                     (Num r')<- evalM r;
                     return(Boolean(l'<=r'))}
evalM(IsZero l) = do{ (Num l') <- (evalM l);
                     return(Boolean(l'==0))}
evalM(If x y z) = do{(Boolean x')<- (evalM x);
                      if x' then (evalM y) else (evalM z)}


evalErr :: ABE -> (Maybe ABE)
evalErr (Num n) = if(n<0) then Nothing
                        else Just (Num n)
evalErr (Plus l r) = do { (Num l') <- evalErr l;
                        (Num r') <- evalErr r;
                        return (Num(l'+r'))}
evalErr (Minus l r) = do { (Num l') <- evalErr l;
                        (Num r') <- evalErr r;
                        if l'>r' then return (Num (l'-r'))
                        else Nothing }
evalErr (Mult l r) = do { (Num l') <- evalErr l;
                        (Num r') <- evalErr r;
                        return (Num(l'*r'))}
evalErr (Div l r) = do { (Num l') <- evalErr l;
                       (Num r') <- evalErr r;
                       if r'== 0 then Nothing
                       else return (Num(l' `div` r'))}
evalErr (Boolean x) = Just (Boolean x)
evalErr (And l r)= do{ (Boolean l')<- evalErr l;
                     (Boolean r')<- evalErr r;
                     return(Boolean(l'&&r'))}
evalErr(Leq l r)= do{ (Num l')<- evalErr l;
                     (Num r')<- evalErr r;
                     return(Boolean(l'<=r'))}
evalErr(IsZero l) = do{ (Num l') <- (evalErr l);
                     return(Boolean(l'==0))}
evalErr(If x y z) = do{(Boolean x')<- (evalErr x);
                      if x' then (evalErr y) else (evalErr z)}

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = Just TNum
typeofM (Plus l r)= do{ tl <- typeofM l;
                        tr <- typeofM r;
                        if tl == TNum && tr ==TNum
                        then Just TNum
                        else Nothing}
typeofM (Minus l r)= do{ tl <- typeofM l;
                        tr <- typeofM r;
                        if tl == TNum && tr ==TNum
                        then Just TNum
                        else Nothing}
typeofM (Mult l r)= do{ tl <- typeofM l;
                        tr <- typeofM r;
                        if tl == TNum && tr ==TNum
                        then Just TNum
                        else Nothing}
typeofM (Div l r)= do{ tl <- typeofM l;
                        tr <- typeofM r;
                        if tl == TNum && tr ==TNum
                        then Just TNum
                        else Nothing}
typeofM (Boolean x)= Just TBool
typeofM (And l r) = do{ tl<-typeofM l;
                        tr<-typeofM r;
                        if tl == TBool && tr == TBool
                        then Just TBool
                        else Nothing}
typeofM (Leq l r) = do{ tl<-typeofM l;
                        tr<-typeofM r;
                        if tl == TNum && tr == TNum
                        then Just TBool
                        else Nothing}
typeofM(IsZero l) = do{ tl<- typeofM l;
                        if tl == TNum
                        then Just TBool
                        else Nothing}
typeofM(If x y z) = do{ tx <- typeofM x;
                        ty <- typeofM y;
                        tz <- typeofM z;
                        if(tx == TBool)then (if(ty==tz) then Just ty else Nothing)
                        else Nothing}


-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM x = do {
    x' <- (typeofM x);
    evalM x
}


-- Optimizer

optimize :: ABE -> ABE
optimize (Plus x (Num 0)) = optimize x
optimize (Plus (Num 0) y) = optimize y
optimize (If (Boolean True) x y) = x
optimize (If (Boolean False) x y) = y

interpOptM :: ABE -> Maybe ABE
interpOptM l = evalM (optimize l)

test01 = do
            print("evalM 1 will have")
            print(evalM(Num 1))
            print("evalM -1 will have")
            print(evalM(Num (-1)))
            print("evalM 1+2 will have")
            print(evalM(Plus(Num 1)(Num 2)))
            print("evalM 1+(-2) will have")
            print(evalM(Plus(Num 1)(Num (-2))))
            print("evalM 2-1 will have")
            print(evalM(Minus(Num 2)(Num 1)))
            print("evalM 1-2 will have")
            print(evalM(Minus(Num 1)(Num 2)))
            print("evalM -1-2 will have")
            print(evalM(Minus(Num (-1))(Num 2)))
            print("evalM 1*2 will have")
            print(evalM(Mult(Num 1)(Num 2)))
            print("evalM -1*2 will have")
            print(evalM(Mult(Num (-1))(Num 2)))
            print("evalM 2/1 will have")
            print(evalM(Div(Num 2)(Num 1)))
            print("evalM 2/0 will have")
            print(evalM(Div(Num 2)(Num 0)))
            print("evalM -2/1 will have")
            print(evalM(Div(Num (-2))(Num 1)))
            print("evalM True will have")
            print(evalM(Boolean True))
            print("evalM False will have")
            print(evalM(Boolean False))
            print("evalM True&&True will have")
            print(evalM(And(Boolean True)( Boolean True)))
            print("evalM True&&False will have")
            print(evalM(And(Boolean True)( Boolean False)))
            print("evalM False&& False will have")
            print(evalM(And(Boolean False)( Boolean False)))
            print("evalM 3<=5 will have")
            print(evalM(Leq(Num 3)(Num 5)))
            print("evalM Leq 5<=5 will have")
            print(evalM(Leq(Num 5)(Num 5)))
            print("evalM Leq 5<=3 will have")
            print(evalM(Leq(Num 5)(Num 3)))
            print("evalM Leq -5<=3 will have")
            print(evalM(Leq(Num (-5))(Num 3)))
            print("evalM IsZero 0 will have")
            print(evalM(IsZero(Num 0)))
            print("evalM IsZero 5 will have")
            print(evalM(IsZero(Num 5)))
            print("evalM IsZero -5 will have")
            print(evalM(IsZero(Num (-5))))
            print("evalM If True then 1 else 0 will have")
            print(evalM(If(Boolean True)(Num 1)(Num 0)))
            print("evalM If False then 1 else 0 will have")
            print(evalM(If(Boolean False)(Num 1)(Num 0)))
            print("-------------------------------------")

test02 = do
            print("evalErr 1 will have")
            print(evalErr(Num 1))
            print("evalErr -1 will have")
            print(evalErr(Num (-1)))
            print("evalErr 1+2 will have")
            print(evalErr(Plus(Num 1)(Num 2)))
            print("evalErr 1+(-2) will have")
            print(evalErr(Plus(Num 1)(Num (-2))))
            print("evalErr 1+True will have")
            print(evalErr(Plus(Num 1)(Boolean True)))
            print("evalErr 2-1 will have")
            print(evalErr(Minus(Num 2)(Num 1)))
            print("evalErr 1-2 will have")
            print(evalErr(Minus(Num 1)(Num 2)))
            print("evalErr -1-2 will have")
            print(evalErr(Minus(Num (-1))(Num 2)))
            print("evalErr 1-True will have")
            print(evalErr(Minus(Num 1)(Boolean True)))
            print("evalErr 1*2 will have")
            print(evalErr(Mult(Num 1)(Num 2)))
            print("evalErr -1*2 will have")
            print(evalErr(Mult(Num (-1))(Num 2)))
            print("evalErr 1*True will have")
            print(evalErr(Mult(Num 1)(Boolean True)))
            print("evalErr 2/1 will have")
            print(evalErr(Div(Num 2)(Num 1)))
            print("evalErr 2/0 will have")
            print(evalErr(Div(Num 2)(Num 0)))
            print("evalErr -2/1 will have")
            print(evalErr(Div(Num (-2))(Num 1)))
            print("evalErr 1/True will have")
            print(evalErr(Div(Num 1)(Boolean True)))
            print("evalErr True will have")
            print(evalErr(Boolean True))
            print("evalErr False will have")
            print(evalErr(Boolean False))
            print("evalErr True&&True will have")
            print(evalErr(And(Boolean True)( Boolean True)))
            print("evalErr True&&False will have")
            print(evalErr(And(Boolean True)( Boolean False)))
            print("evalErr False&& False will have")
            print(evalErr(And(Boolean False)( Boolean False)))
            print("evalErr 1&&True will have")
            print(evalErr(And(Num 1)( Boolean True)))
            print("evalErr 3<=5 will have")
            print(evalErr(Leq(Num 3)(Num 5)))
            print("evalErr Leq 5<=5 will have")
            print(evalErr(Leq(Num 5)(Num 5)))
            print("evalErr Leq 5<=3 will have")
            print(evalErr(Leq(Num 5)(Num 3)))
            print("evalErr Leq -5<=3 will have")
            print(evalErr(Leq(Num (-5))(Num 3)))
            print("evalErr Leq 1<=True will have")
            print(evalErr(Leq(Num 1)(Boolean True)))
            print("evalErr IsZero 0 will have")
            print(evalErr(IsZero(Num 0)))
            print("evalErr IsZero 5 will have")
            print(evalErr(IsZero(Num 5)))
            print("evalErr IsZero -5 will have")
            print(evalErr(IsZero(Num (-5))))
            print("evalErr IsZero True will have")
            print(evalErr(IsZero(Boolean (True))))
            print("evalErr If True then 1 else 0 will have")
            print(evalErr(If(Boolean True)(Num 1)(Num 0)))
            print("evalErr If False then 1 else 0 will have")
            print(evalErr(If(Boolean False)(Num 1)(Num 0)))
            print("-------------------------------------")

test03 = do
            print("typeofM 1 will have")
            print(typeofM(Num 1))
            print("typeofM -1 will have")
            print(typeofM(Num (-1)))
            print("typeofM 1+2 will have")
            print(typeofM(Plus(Num 1)(Num 2)))
            print("typeofM 1+(-2) will have")
            print(typeofM(Plus(Num 1)(Num (-2))))
            print("typeofM 1+True will have")
            print(typeofM(Plus(Num 1)(Boolean True)))
            print("typeofM 2-1 will have")
            print(typeofM(Minus(Num 2)(Num 1)))
            print("typeofM 1-2 will have")
            print(typeofM(Minus(Num 1)(Num 2)))
            print("typeofM -1-2 will have")
            print(typeofM(Minus(Num (-1))(Num 2)))
            print("typeofM 1-True will have")
            print(typeofM(Minus(Num 1)(Boolean True)))
            print("typeofM 1*2 will have")
            print(typeofM(Mult(Num 1)(Num 2)))
            print("typeofM -1*2 will have")
            print(typeofM(Mult(Num (-1))(Num 2)))
            print("typeofM 1*True will have")
            print(typeofM(Mult(Num 1)(Boolean True)))
            print("typeofM 2/1 will have")
            print(typeofM(Div(Num 2)(Num 1)))
            print("typeofM 2/0 will have")
            print(typeofM(Div(Num 2)(Num 0)))
            print("typeofM -2/1 will have")
            print(typeofM(Div(Num (-2))(Num 1)))
            print("typeofM 1/True will have")
            print(typeofM(Div(Num 1)(Boolean True)))
            print("typeofM True will have")
            print(typeofM(Boolean True))
            print("typeofM False will have")
            print(typeofM(Boolean False))
            print("typeofM True&&True will have")
            print(typeofM(And(Boolean True)( Boolean True)))
            print("typeofM True&&False will have")
            print(typeofM(And(Boolean True)( Boolean False)))
            print("typeofM False&& False will have")
            print(typeofM(And(Boolean False)( Boolean False)))
            print("typeofM 1&&True will have")
            print(typeofM(And(Num 1)( Boolean True)))
            print("typeofM 3<=5 will have")
            print(typeofM(Leq(Num 3)(Num 5)))
            print("typeofM Leq 5<=5 will have")
            print(typeofM(Leq(Num 5)(Num 5)))
            print("typeofM Leq 5<=3 will have")
            print(typeofM(Leq(Num 5)(Num 3)))
            print("typeofM Leq -5<=3 will have")
            print(typeofM(Leq(Num (-5))(Num 3)))
            print("typeofM Leq 1<=True will have")
            print(typeofM(Leq(Num 1)(Boolean True)))
            print("typeofM IsZero 0 will have")
            print(typeofM(IsZero(Num 0)))
            print("typeofM IsZero 5 will have")
            print(typeofM(IsZero(Num 5)))
            print("typeofM IsZero -5 will have")
            print(typeofM(IsZero(Num (-5))))
            print("typeofM IsZero True will have")
            print(typeofM(IsZero(Boolean (True))))
            print("typeofM If True then 1 else 0 will have")
            print(typeofM(If(Boolean True)(Num 1)(Num 0)))
            print("typeofM If False then 1 else 0 will have")
            print(typeofM(If(Boolean False)(Num 1)(Num 0)))
            print("-------------------------------------")

test04 = do
            print("evalTypeM 1 will have")
            print(evalTypeM(Num 1))
            print("evalTypeM -1 will have")
            print(evalTypeM(Num (-1)))
            print("evalTypeM 1+2 will have")
            print(evalTypeM(Plus(Num 1)(Num 2)))
            print("evalTypeM 1+(-2) will have")
            print(evalTypeM(Plus(Num 1)(Num (-2))))
            print("evalTypeM 1+True will have")
            print(evalTypeM(Plus(Num 1)(Boolean True)))
            print("evalTypeM 2-1 will have")
            print(evalTypeM(Minus(Num 2)(Num 1)))
            print("evalTypeM 1-2 will have")
            print(evalTypeM(Minus(Num 1)(Num 2)))
            print("evalTypeM -1-2 will have")
            print(evalTypeM(Minus(Num (-1))(Num 2)))
            print("evalTypeM 1-True will have")
            print(evalTypeM(Minus(Num 1)(Boolean True)))
            print("evalTypeM 1*2 will have")
            print(evalTypeM(Mult(Num 1)(Num 2)))
            print("evalTypeM -1*2 will have")
            print(evalTypeM(Mult(Num (-1))(Num 2)))
            print("evalTypeM 1*True will have")
            print(evalTypeM(Mult(Num 1)(Boolean True)))
            print("evalTypeM 2/1 will have")
            print(evalTypeM(Div(Num 2)(Num 1)))
            print("evalTypeM 2/0 will have")
            print(evalTypeM(Div(Num 2)(Num 0)))
            print("evalTypeM -2/1 will have")
            print(evalTypeM(Div(Num (-2))(Num 1)))
            print("evalTypeM 1/True will have")
            print(evalTypeM(Div(Num 1)(Boolean True)))
            print("evalTypeM True will have")
            print(evalTypeM(Boolean True))
            print("evalTypeM False will have")
            print(evalTypeM(Boolean False))
            print("evalTypeM True&&True will have")
            print(evalTypeM(And(Boolean True)( Boolean True)))
            print("evalTypeM True&&False will have")
            print(evalTypeM(And(Boolean True)( Boolean False)))
            print("evalTypeM False&& False will have")
            print(evalTypeM(And(Boolean False)( Boolean False)))
            print("evalTypeM 1&&True will have")
            print(evalTypeM(And(Num 1)( Boolean True)))
            print("evalTypeM 3<=5 will have")
            print(evalTypeM(Leq(Num 3)(Num 5)))
            print("evalTypeM Leq 5<=5 will have")
            print(evalTypeM(Leq(Num 5)(Num 5)))
            print("evalTypeM Leq 5<=3 will have")
            print(evalTypeM(Leq(Num 5)(Num 3)))
            print("evalTypeM Leq -5<=3 will have")
            print(evalTypeM(Leq(Num (-5))(Num 3)))
            print("evalTypeM Leq 1<=True will have")
            print(evalTypeM(Leq(Num 1)(Boolean True)))
            print("evalTypeM IsZero 0 will have")
            print(evalTypeM(IsZero(Num 0)))
            print("evalTypeM IsZero 5 will have")
            print(evalTypeM(IsZero(Num 5)))
            print("evalTypeM IsZero -5 will have")
            print(evalTypeM(IsZero(Num (-5))))
            print("evalTypeM IsZero True will have")
            print(evalTypeM(IsZero(Boolean (True))))
            print("evalTypeM If True then 1 else 0 will have")
            print(evalTypeM(If(Boolean True)(Num 1)(Num 0)))
            print("evalTypeM If False then 1 else 0 will have")
            print(evalTypeM(If(Boolean False)(Num 1)(Num 0)))
            print("-------------------------------------")


test = do
       print("-------------------------------------")
       print "evalM Test starts"
       print("-------------------------------------")
       test01
       print "evalErr Test starts"
       print("-------------------------------------")
       test02
       print "typeofM Test starts"
       print("-------------------------------------")
       test03
       print "evalTypeM starts"
       print("-------------------------------------")
       test04
       --print "optimize starts"
       print("-------------------------------------")
       --test05
       print "Test ends"

testfunction :: IO()
testfunction = do
   test
