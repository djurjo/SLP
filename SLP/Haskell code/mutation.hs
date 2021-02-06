module Mutation where

import Expressions
import Fitness
import InitialPop
import Data.Map as M
import System.Random as R

    
mutate :: RandomGen g => g -> (SLP, Maybe Literal) -> [(State,Literal)] -> Variable-> [Variable] -> [Literal] -> (SLP, Maybe Literal)
mutate seed (slp, fit) sigma target vars lits = if nfit < fit then (nslp, nfit) else (slp, fit)
       where
         (k, newseed) = R.randomR (1, (length slp)) seed
         expr =  slp ! (V ("u" ++ show k))
         possvars = if k /=1
                    then  vars ++ [V ("u" ++ show y) |  y <- [1 .. k-1] ]
                    else vars
         (nexpr, _ ) = mutateExpression newseed expr possvars lits
         nslp =  M.update (\ _ -> Just nexpr) (V ("u" ++ show k)) slp
         nfit = fitness sigma target nslp
         --nfit = fitness2A sigma nslp

mutateExpression :: RandomGen g => g -> Expression -> [Variable] -> [Literal] -> (Expression, g)
mutateExpression seed (Add t1 t2) vars lits = if k==1
                                              then (Add (take1 (randomTerminal  seed  vars lits)) (t2), newseed)
                                              else (Add t1(take1 (randomTerminal seed  vars lits)), newseed)
    where (k, newseed) = R.randomR (1::Int, 2) seed

mutateExpression seed (Sub t1 t2) vars lits = if k==1
                                              then( Sub (take1 (randomTerminal  seed  vars lits)) (t2), newseed)
                                              else (Sub  t1 (take1 (randomTerminal seed  vars lits)), newseed)
    where (k, newseed) = R.randomR (1::Int, 2) seed

mutateExpression seed (Prod t1 t2) vars lits = if k==1
                                             then (Prod (take1 (randomTerminal newseed  vars lits)) (t2), newseed)
                                               else (Prod t1 (take1 (randomTerminal newseed  vars lits)), newseed)
    where (k, newseed) = R.randomR (1::Int, 2) seed
mutateExpression seed (Div t1 t2) vars lits = if k==1
                                             then (Div (take1 (randomTerminal newseed  vars lits)) (t2), newseed)
                                               else (Div t1 (take1 (randomTerminal newseed  vars lits)), newseed)
    where (k, newseed) = R.randomR (1::Int, 2) seed
                 
take1 :: (a, b) -> a
take1 (x, _) = x

take2 :: (a, b) -> b
take2 (_, y) = y
