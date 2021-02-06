module InitialPop where
import Expressions
import Fitness

import System.Random as R
import Data.Map as M



randomPop :: RandomGen g => g -> [(State, Literal)] -> Variable ->  [Variable] -> [Literal] -> Int -> Int -> [(SLP, Maybe Literal)]
randomPop _      _      _       _      _    _            0            = []
randomPop seed sigmas targ  vars lits slp_size pop_size = (slp, fit): (randomPop newseed2 sigmas targ vars lits slp_size (pop_size - 1))
    where
      (newseed1, newseed2) = R.split seed
      slp = randomSLP newseed1 vars lits (1, slp_size +1)
      fit = fitness sigmas targ slp 
      --fit = fitness2A sigmas slp
{-
ex :: [(SLP, Maybe Literal)]
ex = randomPop (mkStdGen 657890) [(M.fromList [("x", Just 1)], 1)] "u4" ["x"] [1] 4 4
-}   

    
randomSLP :: RandomGen g => g -> [Variable] -> [Literal] -> (Int ,Int) -> SLP
randomSLP seed  vars  lits (n, m)
    | n == m = M.empty
    | otherwise =  M.insert key expr (randomSLP newseed (key : vars) lits (n+1, m))
    where
      key = (V ("u" ++ (show n)))
      (expr, newseed) = randomExpression seed vars lits
                            


randomTerminal :: RandomGen g => g ->  [Variable] -> [Literal] -> (Terminal, g)
randomTerminal seed vars lits
               | k == 0 = (Lit (lits !! m), newseed2)
               | k == 1 =(Var (vars !! n), newseed3)
    where
      (k, newseed1) = R.randomR (0::Int, 1) seed
      (m, newseed2) = R.randomR (0, (length lits) - 1) newseed1
      (n, newseed3) = R.randomR (0, (length vars) -1) newseed1
                      
randomExpression :: RandomGen g => g -> [Variable] -> [Literal] -> (Expression, g)
randomExpression seed vars lits
    | k == (0::Int) = (Add (t1) (t2), newseed3)
    | k ==1 = (Sub (t1) (t2), newseed3)
    | k == 2 = (Prod (t1) (t2), newseed3)
    | k == 3 = (Div t1 t2 , newseed2)
    where
      (k, newseed1) = R.randomR (0, 3) seed
      (t1, newseed2) = randomTerminal newseed1 vars lits
      (t2, newseed3) = randomTerminal newseed2 vars lits
