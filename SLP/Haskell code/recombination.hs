module Recombination where

import Expressions as E
import Fitness as F
import Data.Set as S
import Data.Map as M
import System.Random as R

crossover :: RandomGen g => g ->[(State, Literal)] -> Variable -> (SLP, Maybe Literal) -> (SLP, Maybe Literal) -> ((SLP, Maybe Literal), (SLP, Maybe Literal))
crossover seed vals var (slp1, fit1) (slp2, fit2) = (bestfather, bestson)
    where
      (nseed1, nseed2) = R.split seed
      (son1, fson1) = slpCrossover nseed1 vals var (slp1, fit1) (slp2, fit2)
      (son2, fson2) = slpCrossover nseed2 vals var (slp2, fit2) (slp1, fit1)
      bestson = if ( fson1 < fson2) then (son1, fson1) else (son2, fson2)
      bestfather = if (fit1 < fit2) then (slp1, fit1) else (slp2, fit2)

slpCrossover :: RandomGen g => g ->[(State, Literal)] -> Variable -> (SLP, Maybe Literal) -> (SLP, Maybe Literal) -> (SLP, Maybe Literal)
slpCrossover seed vals var (slp1, fit1) (slp2, fit2)
    |  nfit <= fit1 && nfit <= fit2 = (M.fromList (newSLP) , nfit)
    |  otherwise = (slp1, fit1)
    where
      (k, newseed1) = R.randomR  (0, (length slp1) - 1) seed
      (selkey, _) = M.elemAt k slp1
      effvars = S.toList ( effectiveVariables slp1 (S.fromList [selkey]))
 
      (m, _) = R.randomR ((length effvars - 1), (length slp2 - 1)) newseed1
      varsTochange = Prelude.map (\(x, _) -> x )
                     ((Prelude.map g --((flip M.elemAt) slp2)
                                  ([(m- (length effvars - 1)+1) .. m])))
      g x = if (x < length slp2 && x > -1) then ((flip M.elemAt) slp2 x) else (error (show x))
      oldNewL = Prelude.zip varsTochange effvars
      newSLP =  [f (z, t)  | (z, t) <- M.toList  (slp2)]
      f (x, y) = if (elem x varsTochange)
            then (x , renameExpressionL (slp1 ! x) oldNewL)
            else (x, y)

      nfit =  fitness vals var (M.fromList newSLP)


slpCrossoverEx :: RandomGen g => g ->[(State, Literal)] -> Variable -> (SLP, Maybe Literal) -> (SLP, Maybe Literal) -> SLP
slpCrossoverEx seed vals var (slp1, fit1) (slp2, fit2) = M.fromList newSLP
    where
      (k, newseed1) = R.randomR  (0, (length slp1) - 1) seed
      (selkey, _) = M.elemAt k slp1
      effvars = S.toList ( effectiveVariables slp1 (S.fromList [selkey]))
 
      (m, _) = R.randomR ((length effvars - 1), (length slp2 - 1)) newseed1
      varsTochange = Prelude.map (\(x, _) -> x )
                     ((Prelude.map g --((flip M.elemAt) slp2)
                                  ([(m- (length effvars - 1)+1) .. m])))
      g x = if (x < length slp2 && x > -1) then ((flip M.elemAt) slp2 x) else (error (show x))
      oldNewL = Prelude.zip varsTochange effvars
      newSLP =  [f (z, t)  | (z, t) <- M.toList  (slp2)]
      f (x, y) = if (elem x varsTochange)
            then (x , renameExpressionL (slp1 ! x) oldNewL)
            else (x, y)

      nfit =  fitness vals var (M.fromList newSLP)

slpg1 :: SLP
slpg1 = M.fromList [
         (V "u1", Add (Var (V"x1")) (Var (V "x2"))),
         (V"u2", Prod (Var(V "u1")) (Var (V "u2"))),
         (V"u3", Prod (Var (V "u1")) (Var (V "x1"))),
         (V"u4", Add (Var (V "u3")) (Var (V "u2"))),
         (V"u5", Prod (Var (V "u3")) (Var (V "u2"))) ]

slpg2 :: SLP
slpg2 = M.fromList [
         (V"u1", Prod (Var (V "x1")) (Var (V "x1"))),
         (V"u2", Add (Var (V "u1")) (Var (V "x2"))),
         (V"u3", Add (Var (V "u1")) (Var (V "x1"))),
         (V"u4", Prod (Var (V "u2")) (Var (V "x1"))),
         (V"u5", Add (Var ( V "u1")) (Var (V "u4")))
        ]
