module Algorithm where


import Expressions
import Recombination
import InitialPop
import Mutation

import System.Random as R
import Data.Map as M
import Debug.Trace
import Data.Time.Clock as T
import Fitness as F

    
algorithm :: RandomGen g => g -> Int ->  (Int, Int) -> [(State, Literal)] -> [Variable] -> [Literal] -> Variable -> [(SLP, Maybe Literal)] -> [(SLP, Maybe Literal)]
algorithm seed counter (prob_cross, prob_mut) sigmas vars lits target pop 
    | counter == 0 = pop
    | otherwise = (algorithm newseed2 (counter - 1) (prob_cross, prob_mut) sigmas vars lits target npop )
    where
      npop = Prelude.map (iteration newseed1 (prob_cross, prob_mut) sigmas vars lits target pop) pop
      (newseed1, newseed2) = R.split seed
    


minT :: (SLP, Maybe Literal) -> (SLP, Maybe Literal) -> (SLP, Maybe Literal)
minT (slp1, a) (slp2, b) = if minimumT == a then (slp1, a) else (slp2, b)
    where
       minimumT = F.minMaybe a b
             
getBest :: [(SLP, Maybe Literal)] -> (SLP, Maybe Literal)
getBest xs = foldr1 minT xs
             
getListFits :: [(SLP, Maybe Literal)] -> [Literal]
getListFits ((_, Nothing):xs) = 100000000 : (getListFits xs)
getListFits ((_ , Just x):xs) = x : (getListFits xs)
getListFits _ = []


      
iteration :: RandomGen g => g -> (Int, Int) -> [(State, Literal)]-> [Variable] -> [Literal] -> Variable -> [(SLP, Maybe Literal)] -> (SLP, Maybe Literal) -> (SLP, Maybe Literal)
iteration seed (prob_cross, prob_mut) sigmas vars lits target pop (slp, fit) 
          | op < prob_cross = slpCrossover newseed4 sigmas target (slp, fit) (pop !! k1)
          | op < prob_cross + prob_mut = mutate newseed4 (slp, fit) sigmas target vars lits 
          | otherwise = (slp, fit)
    where
      (op, newseed1) = R.randomR (0, 100) seed
      (newseed2, _) = R.split newseed1
      (k1 , newseed4) = R.randomR (0, length pop - 1) newseed2


tailE :: [a] -> [a]
tailE [] = []
tailE l = tail l
      
iteration2 :: RandomGen g => g -> (Int, Int) -> [(State, Literal)]-> [Variable] -> [Literal] -> Variable -> [(SLP, Maybe Literal)] -> [(SLP, Maybe Literal)]
iteration2 _ _ _ _ _ _ [] = []
iteration2 seed _  sigmas vars lits target [x] = [mutate seed (x) sigmas target vars lits]
iteration2 seed (prob_cross, prob_mut) sigmas vars lits target (popI)
          | op < prob_cross = father:son:(iteration2 newseed3  (prob_cross, prob_mut) sigmas vars lits target (pop1 ++ tailE pop2))
          | op < prob_cross + prob_mut = (mutate newseed4 (slp, fit) sigmas target vars lits ):iteration2 newseed3  (prob_cross, prob_mut) sigmas vars lits target (popI)
          | otherwise = (slp, fit):(iteration2 newseed3  (prob_cross, prob_mut) sigmas vars lits target (popI))
    where
      (k, newseed8) =  R.randomR (0, length popI - 1) newseed5
      (slp, fit) = popI !! k
      (popI1, popI2) = Prelude.splitAt k popI
      pop =  popI1 ++ tailE popI2
      (op, newseed7) = R.randomR (0, 100) newseed5
      (newseed5, newseed6) = R.split seed
      (newseed2, newseed3) = R.split newseed6
      (k1 , newseed4) = R.randomR (0, length pop - 1) newseed2
      (pop1, pop2) = Prelude.splitAt k1 pop
      (father, son) = crossover newseed4 sigmas target (slp, fit)  (pop !! k1)
      
algorithm2 :: RandomGen g => g -> Int ->  (Int, Int) -> [(State, Literal)] -> [Variable] -> [Literal] -> Variable -> [(SLP, Maybe Literal)] -> [(SLP, Maybe Literal)]
algorithm2 seed counter (prob_cross, prob_mut) sigmas vars lits target pop 
    | counter == 0 = pop
    | otherwise = (algorithm2 newseed2 (counter - 1) (prob_cross, prob_mut) sigmas vars lits target npop )
    where
      npop = iteration2 newseed1 (prob_cross, prob_mut) sigmas vars lits target pop
      (newseed1, newseed2) = R.split seed

                      
getState :: String -> [(State, Literal)]
getState contents = Prelude.map f pairs
    where
      lns = lines contents
      pairs = Prelude.map (words) lns
      f [x, y,z] = (M.fromList [(V "x-1", Just (read x::Float)), (--,
                                         V "x-2", Just (read y :: Float))
                                   --  (V "x-3", Just (read t :: Float))
                                    ], read z :: Float)
{-            
main :: IO ()
main =
    do
      contents <- readFile "points.txt"
      starttime <- T.getCurrentTime
      print $ show starttime
      let
          sigma = getState contents
          target = V "u10"
          (prob_cross, prob_mut) =  (90, 10)
          vars = [V "x-1"] --, V "x-2"]--, V "x-3"]
          lits = [1]
          slp_size = 10
          pop_size1 = 50
          iterations = 50
          pop = randomPop (mkStdGen 8778767576) sigma target vars lits slp_size (pop_size1)
          (slp,fit) = getBest (algorithm2  (mkStdGen 879898987811) iterations (prob_cross, prob_mut) (sigma) vars lits target pop)
      
      print $ "Start process "
      print $ show slp
      print $ "The error is: " ++ show fit
            
      --print $ show slp2
      --print $ "The error is: " ++ show fit2
      time <- T.getCurrentTime
      let
          diff = T.diffUTCTime  time starttime
      print $ time
      print $ "It took this time:" ++  show diff
            
-}
            
      
