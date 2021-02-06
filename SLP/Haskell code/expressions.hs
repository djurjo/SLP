module Expressions where

import Data.Set as S
import Data.Map as M

newtype Variable = V String
    deriving (Eq, Show)

             
instance Ord(Variable) where
    V var1 <= V var2 = (read (tail var1)::Int) <= (read (tail var2)::Int)
    
    
type Literal = Float
    
toVariable :: String -> Variable
toVariable x = V x
---- Some auxiliar functions related with the Maybe

addM :: Fractional a => Maybe a -> Maybe a -> Maybe a
addM (Just a) (Just b) = Just (a + b)
addM _ _ = Nothing


prodM :: Fractional a =>   Maybe a -> Maybe a -> Maybe a
prodM (Just a) (Just b) = Just (a * b)
prodM _ _ = Nothing


subM :: Fractional a => Maybe a -> Maybe a -> Maybe a
subM (Just a) (Just b) = Just (a - b)
subM _ _ = Nothing

divM :: Fractional a => Maybe a -> Maybe a -> Maybe a
divM (Just a) (Just b) = Just (a / b)
divM _ _ = Nothing


---- This type will be needed to evaluate the Semantics ----
-----------------------------------------------------------------------------
type State = M.Map Variable (Maybe Literal)

getValue :: State -> Variable -> Maybe Literal
getValue sigma var = if (elem var (keys sigma)) then ((sigma ! var)) else Nothing
                    -- else error ("Here :( " ++ show sigma ++"\n" ++show var)

--- First we define the Terminals, that are just Variables or Literals

data Terminal = Lit Literal | Var Variable
                deriving (Eq, Ord, Show)

fvTerminal :: Terminal -> Set Variable
fvTerminal (Lit _) = S.empty
fvTerminal (Var x) = S.singleton x


                     
semanticTerminal :: Terminal -> State -> Maybe Literal
semanticTerminal (Lit n) _ = Just n
semanticTerminal (Var x) sigma = if getValue sigma x == Nothing
                                 then Nothing -- error ("Here " ++ show x ++ " " ++ show sigma) -- Ok here
                                 else getValue sigma x


renameTerminal :: Terminal -> Variable -> Variable -> Terminal
renameTerminal (Lit n) _ _ = Lit n
renameTerminal (Var x) u v
    | x == u = (Var v)
    | otherwise = (Var x)


--- Now the key idea to this... The Expressions!!

data Expression =
                Add Terminal Terminal
               | Sub Terminal Terminal
               | Prod Terminal Terminal
               | Div Terminal Terminal
                 deriving (Eq)

instance Show (Expression) where
  show (Add t1 t2) = "("++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (Prod t1 t2) = "("++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (Sub t1 t2) = "("++ show t1 ++ ") - (" ++ show t2 ++ ")"
  show (Div t1 t2) = "("++ show t1 ++ ") // (" ++ show t2 ++ ")"
                 
fvExpression :: Expression -> Set Variable
fvExpression (Add t1 t2) = S.union (fvTerminal t1) (fvTerminal t2)
fvExpression (Sub t1 t2) = S.union (fvTerminal t1) (fvTerminal t2)
fvExpression (Prod t1 t2) = S.union (fvTerminal t1) (fvTerminal t2)
fvExpression (Div t1 t2) = S.union (fvTerminal t1) (fvTerminal t2)



termsExpression :: Expression -> [Terminal]
termsExpression (Add t1 t2) = [t1, t2]
termsExpression (Sub t1 t2) = [t1, t2]
termsExpression (Prod t1 t2) = [t1, t2]
termsExpression (Div t1 t2) = [t1, t2]
                         
                            
semanticExpression :: Expression -> State -> Maybe Literal
semanticExpression (Add t1 t2) sigma = addM (tValue1) (tValue2)
  where
      tValue1 =  semanticTerminal t1 sigma
      tValue2 =  semanticTerminal t2 sigma
semanticExpression (Sub t1 t2) sigma = subM (tValue1) (tValue2)
    where
      tValue1 = semanticTerminal t1 sigma
      tValue2 = semanticTerminal t2 sigma
semanticExpression (Prod t1 t2) sigma = prodM (tValue1) (tValue2)
    where
      tValue1 = semanticTerminal t1 sigma
      tValue2 = semanticTerminal t2 sigma
semanticExpression (Div t1 t2) sigma = if (tValue2 /= Just 0)
                                       then divM (tValue1) (tValue2)
                                       else Just 1
    where
      tValue1 = semanticTerminal t1 sigma
      tValue2 = semanticTerminal t2 sigma

renameExpression :: Expression -> (Variable, Variable) -> Expression
renameExpression (Add t1 t2) (v1, v2) = Add (renameTerminal t1 v1 v2) (renameTerminal t2 v1 v2)
renameExpression (Prod t1 t2) (v1, v2) = Prod (renameTerminal t1 v1 v2) (renameTerminal t2 v1 v2)
renameExpression (Sub t1 t2) (v1, v2) = Sub (renameTerminal t1 v1 v2) (renameTerminal t2 v1 v2)
renameExpression (Div t1 t2) (v1, v2) = Div (renameTerminal t1 v1 v2) (renameTerminal t2 v1 v2)

                                        
renameExpressionL :: Expression -> [(Variable, Variable)] -> Expression
renameExpressionL e [] = e
renameExpressionL e (x:xs) = renameExpressionL (renameExpression e x) xs 
                     

{-
  A Straight Line Program will be a collection of instructions

-}

{-I consider a different approach where a SLP is a Map -}

type SLP = M.Map Variable Expression
{-
Until now we have been working with SLP as a collection of instructions. This approach
is very readable but bring some other problems... We will avoid that problems considering a SLP as a Map of Ints and Expressions. It makes sense since we use to write:
    u1 := a + b
    u2 := a - b
    u4 := 
    un := ui - uj

So it make sense to consider [(u1, e1), (u2, e2), ...]
-}

slpGamma :: SLP -> [Variable]
slpGamma slp = M.keys slp

{-The relation is defined as:

u ~ v iff u <- fv(u_{exp})

-}

rRelation :: SLP -> Variable -> Variable -> Bool
rRelation slp u v 
    | S.member v (M.keysSet slp) == False = False
    | otherwise = S.member u (fvExpression (slp ! v) )

effectiveVariables :: SLP -> Set Variable -> Set Variable
effectiveVariables slp set
                 | set == nSet = set
                 | otherwise = effectiveVariables slp nSet
    where nSet = S.union (set) (S.fromList [ u  |
                                                                    u <- (slpGamma slp),
                                                                    foldl1 (||) (S.map (rRelation slp u) (set) )])

effectiveSLP :: SLP -> Set Variable -> SLP
effectiveSLP slp set = M.fromList newSlp  --M.fromList  (Prelude.map f oldNewEx)
    where
      effVar = effectiveVariables slp set
      newSlp =  [(k, (slp ! k)) | k <- (S.toList effVar)]
 

slpEx1 :: SLP
slpEx1 = M.fromList [
        (V "u1", Prod (Var (V " x1")) (Lit 1)),
        (V "u2", Sub (Var (V "u1")) (Var (V "x2"))),
        (V "u3", Prod (Var (V "u2")) (Var (V "u2"))),
        (V "u4", Prod (Var (V "u1")) (Var (V "x2"))),
        (V "u5", Prod (Lit 1) (Lit 2))
       ]

sigma1 :: State
sigma1 = M.fromList [(V "x1", Just 1), (V "x2", Just 9.9999)]


semanticPair :: State -> (Variable, Expression) -> State
semanticPair sigma (u, ex) = M.insert u (value) sigma 
    where
      value = semanticExpression ex sigma

                    
semanticEffectiveSLP :: SLP -> State -> State
semanticEffectiveSLP slp sigma  =  Prelude.foldl semanticPair sigma varExList
    where
      varExList =(M.toList slp)
