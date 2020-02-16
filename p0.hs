--Tsung Yu Ho
--2911531
--Due:09/17/2019
--type :l p0.hs hit enter and then type testfunction to test Exercise 1 2 3 4


{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.




--code starts here

--type :l p0.hs and then type testfunction to test 4 Exercises

evalAE :: AE -> Int
evalAE (Num n) = if n<0 then error "It needs to be natural number(positive)."
                else n
evalAE (Plus l r) = (evalAE l) + (evalAE r)
evalAE (Minus l r) = let l' = (evalAE l) in
                      let r' = (evalAE r) in
                        if l'>=r' then l'-r'
                          else error"It needs to be natural number(positive)."
evalAE (Mult l r) = (evalAE l)*(evalAE r)
evalAE (Div l r) = if (evalAE r==0) then error "Can't divided by zero"
                  else (evalAE l) `div` (evalAE r)
evalAE (If0 a b c) = if(evalAE a)==0 then (evalAE b)
                    else(evalAE c)



evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) = if n>=0 then Just n
                      else Nothing
evalAEMaybe (Plus l r) = let l' = (evalAEMaybe l) in
                          let r' = (evalAEMaybe r) in
                            if l'== Nothing then Nothing
                              else if r' == Nothing then Nothing
                                else let Just v1 = l' in
                                     let Just v2 = r' in
                                Just (v1+v2)
evalAEMaybe (Minus l r) = let l' = (evalAEMaybe l) in
                          let r' = (evalAEMaybe r) in
                            if l'== Nothing then Nothing
                              else if r' == Nothing then Nothing
                                else if r'>l' then Nothing
                                  else let Just v1 = l' in
                                       let Just v2 = r' in
                                       Just (v1-v2)

evalAEMaybe (Mult l r) = let l' = (evalAEMaybe l) in
                          let r' = (evalAEMaybe r) in
                            if l'== Nothing then Nothing
                              else if r' == Nothing then Nothing
                                else let Just v1 = l' in
                                     let Just v2 = r' in
                                     Just (v1*v2)
evalAEMaybe (Div l r) = let l' = (evalAEMaybe l) in
                          let r' = (evalAEMaybe r) in
                            if l'== Nothing then Nothing
                              else if r' == Nothing then Nothing
                                else if r'== Just 0 then Nothing
                                  else let Just v1 = l' in
                                       let Just v2 = r' in
                                       Just (v1 `div` v2)
evalAEMaybe (If0 a b c) = let a' = evalAEMaybe a in
                          let b' = evalAEMaybe b in
                            let c' = evalAEMaybe c in
                              if a'==Nothing then Nothing
                                else if ( a' == Just 0 ) then b'
                                  else c'

evalM :: AE -> Maybe Int
evalM (Num n) = if n>=0 then Just n
                      else Nothing
evalM (Plus l r) = do {l' <- (evalM l);
                       r' <- (evalM r);
                       return (l'+r')}
evalM (Minus l r) = do {l' <- (evalM l);
                       r' <- (evalM r);
                       if l' < r' then Nothing
                        else return (l'-r')}
evalM (Mult l r) = do {l' <- (evalM l);
                       r' <- (evalM r);
                       return (l'*r')}
evalM (Div l r) = do {l' <- (evalM l);
                      r' <- (evalM r);
                      if r'==0 then Nothing
                      else return (l' `div` r')}
evalM (If0 a b c) = do{a' <- (evalM a);
                       b' <- (evalM b);
                       c' <- (evalM c);
                       if a'==0 then Just b'
                        else Just c'}


interpAE :: String -> Maybe Int
interpAE i = evalM(parseAE i)

test01 = do
            print("evalAE 1 will have")
            print(evalAE(Num 1))
            --print(evalAE(Num (-1)))
            print("evalAE 1+2 will have")
            print(evalAE(Plus(Num 1)(Num 2)))
            --print(evalAE(Plus(Num 1)(Num (-2))))
            print("evalAE 2-1 will have")
            print(evalAE(Minus(Num 2)(Num 1)))
            --print(evalAE(Minus(Num 1)(Num 2)))
            --print(evalAE(Minus(Num (-1))(Num 2)))
            print("evalAE 1*2 will have")
            print(evalAE(Mult(Num 1)(Num 2)))
            --print(evalAE(Mult(Num (-1))(Num 2)))
            print("evalAE 2/1 will have")
            print(evalAE(Div(Num 2)(Num 1)))
            --print(evalAE(Div(Num 2)(Num 0)))
            --print(evalAE(Div(Num (-2))(Num 1)))
            print("evalAE If0 a=0 b=1 c=2 will have")
            print(evalAE(If0(Num 0)(Num 1)(Num 2)))
            --print(evalAE(If0(Num (-1))(Num 1)(Num 2)))

test02 = do
            print("evalAEMaybe 1 will have")
            print(evalAEMaybe(Num 1))
            print("evalAEMaybe -1 will have")
            print(evalAEMaybe(Num (-1)))
            print("evalAEMaybe 1+2 will have")
            print(evalAEMaybe(Plus(Num 1)(Num 2)))
            print("evalAEMaybe 1+(-2) will have")
            print(evalAEMaybe(Plus(Num 1)(Num (-2))))
            print("evalAEMaybe -1+2 will have")
            print(evalAEMaybe(Plus(Num (-1))(Num (2))))
            print("evalAEMaybe 2-1 will have")
            print(evalAEMaybe(Minus(Num 2)(Num 1)))
            print("evalAEMaybe 1-2 will have")
            print(evalAEMaybe(Minus(Num 1)(Num 2)))
            print("evalAEMaybe -1-2 will have")
            print(evalAEMaybe(Minus(Num (-1))(Num 2)))
            print("evalAEMaybe 1*2 will have")
            print(evalAEMaybe(Mult(Num 1)(Num 2)))
            print("evalAEMaybe -1*2 will have")
            print(evalAEMaybe(Mult(Num (-1))(Num 2)))
            print("evalAEMaybe 1*-2 will have")
            print(evalAEMaybe(Mult(Num (1))(Num (-2))))
            print("evalAEMaybe 2/1 will have")
            print(evalAEMaybe(Div(Num 2)(Num 1)))
            print("evalAEMaybe 2/0 will have")
            print(evalAEMaybe(Div(Num 2)(Num 0)))
            print("evalAEMaybe -2/1 will have")
            print(evalAEMaybe(Div(Num (-2))(Num 1)))
            print("evalAEMaybe 2/-1 will have")
            print(evalAEMaybe(Div(Num (2))(Num (-1))))
            print("evalAEMaybe If0 a=0 b=1 c=2 will have")
            print(evalAEMaybe(If0(Num 0)(Num 1)(Num 2)))
            print("evalAEMaybe If0 a=-1 b=1 c=2 will have")
            print(evalAEMaybe(If0(Num (-1))(Num 1)(Num 2)))

test03 = do
            print("evalM 1 will have")
            print(evalM(Num 1))
            print("evalM -1 will have")
            print(evalM(Num (-1)))
            print("evalM 1+2 will have")
            print(evalM(Plus(Num 1)(Num 2)))
            print("evalM 1+(-2) will have")
            print(evalM(Plus(Num 1)(Num (-2))))
            print("evalM -1+2 will have")
            print(evalM(Plus(Num (-1))(Num 2)))
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
            print("evalM 1*-2 will have")
            print(evalM(Mult(Num 1)(Num (-2))))
            print("evalM 2/1 will have")
            print(evalM(Div(Num 2)(Num 1)))
            print("evalM 2/0 will have")
            print(evalM(Div(Num 2)(Num 0)))
            print("evalM -2/1 will have")
            print(evalM(Div(Num (-2))(Num 1)))
            print("evalM 2/-1 will have")
            print(evalM(Div(Num (2))(Num (-1))))
            print("evalM If0 a=0 b=1 c=2 will have")
            print(evalM(If0(Num 0)(Num 1)(Num 2)))
            print("evalM If0 a=-1 b=1 c=2 will have")
            print(evalM(If0(Num (-1))(Num 1)(Num 2)))

test04 = do
           print("interpAE 1 will have")
           print(interpAE "1")
           print("interpAE -1 will have")
           print(interpAE "-1")
           print("interpAE 1+2 will have")
           print(interpAE "1+2")
           print("interpAE -1+2 will have")
           print(interpAE "-1+2")
           print("interpAE 2-1 will have")
           print(interpAE "2-1")
           print("interpAE 1-2 will have")
           print(interpAE "1-2")
           print("interpAE -1-2 will have")
           print(interpAE "-1-2")
           print("interpAE 1*2 will have")
           print(interpAE "1*2")
           print("interpAE -1*2 will have")
           print(interpAE "-1*2")
           print("interpAE 2/1 will have")
           print(interpAE "2/1")
           print("interpAE 2/0 will have")
           print(interpAE "2/0")
           print("interpAE -2/1 will have")
           print(interpAE "-2/1")

test = do
       print "Test starts"
       print "evalAE Test starts"
       test01
       print "evalAEMaybe Test starts"
       test02
       print "evalM Test starts"
       test03
       print "interpAE Test starts"
       test04
       print "Test ends"

testfunction :: IO()
testfunction = do
   test
