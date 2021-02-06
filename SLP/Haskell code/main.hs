module Main where

import Expressions
import Recombination
import InitialPop
import Mutation


import Algorithm
import System.Random as R
import Data.Map as M
import Data.Set as S
import Debug.Trace
import Data.Time.Clock as T
import Fitness as F

leeInt :: IO Int
leeInt = do linea <- getLine
            return (read linea)

leeIntRango :: Int -> Int -> IO Int
leeIntRango  men may = do putStr ("\n Introduce a number betwee " ++ show men ++ " y " ++ show may ++ ": ")
                          n <- leeInt
                          if  (n>may)||(n<men) then leeIntRango men may else return n


interfaz1 :: IO()
interfaz1 = do putStr ("We are going to compute the effective SLP with the targets u3 and u5 for the SLP : \n" ++ (show slpEx1) ++ "\n")
               let sol = effectiveSLP slpEx1 (S.fromList [V "u3", V "u5"])
               do putStr (show sol) 


interfaz2 :: IO()
interfaz2 = do
  putStr( "We are going to compute a slpCrossover of \n" ++ (show slpg1) ++ "\n and \n" ++ (show slpg2 ) ++ "\n")             
  let sol = (slpCrossoverEx (mkStdGen 98589071) [(sigma1, 0)] (V "u5") (slpg1, Nothing) (slpg2, Nothing))
  do putStr (show sol)

interfaz3 :: IO ()
interfaz3 = do
  putStr "We are going to consider as target the polynom f(x, y) = x + y + 1"
  contents <- readFile "points_example.txt"
  starttime <- T.getCurrentTime
  print $ show starttime
  let
      sigma = getState contents
      target = V "u5"
      (prob_cross, prob_mut) =  (90, 10)
      vars = [V "x-1", V "x-2"] 
      lits = [1]
      slp_size = 5
      pop_size1 = 15
      iterations = 30
      pop = randomPop (mkStdGen 8778713) sigma target vars lits slp_size (pop_size1)
      (slp, _) = getBest (algorithm2  (mkStdGen 1118781871) iterations (prob_cross, prob_mut) (sigma) vars lits target pop)
  time <- T.getCurrentTime

  print $ "We obtained " ++  show slp ++ "\n" 
  let
      diff = T.diffUTCTime  time starttime
  print $  "It took "  ++  show diff ++ "seconds \n"

interfaz4 :: IO ()
interfaz4 = do
  putStr "First you have to generate a .txt file with some input points. \n What's the file name?"
  file <- getLine
  putStr "Now we need to know the following. Please notice that this implementation is quite slow...: \n"
  putStr "Population size: "
  pop_size1 <- getLine
  putStr "\n Size of each SLP: "
  slp_size <- getLine
  putStr "\n Number of iterations: "
  iterations <- getLine
  putStr "\n Valid varibles: (Please notice that the format needed is a list of varibales where each variable has the form 'x-n' where n is a natural number)"
  vars <- getLine
  putStr "\n Now a list of literals, this is a list of numerals: "
  lits <- getLine
  putStr "\n \n Now the process will start... It could take a while...\n"
  contents <- readFile file
  starttime <- T.getCurrentTime
  print $ show starttime
  let
      sigma = getState contents
      target = V ("u" ++ show slp_size)
      (prob_cross, prob_mut) =  (90, 10)
      pop = randomPop (mkStdGen 8778713) sigma target (Prelude.map toVariable (read vars)) (read lits) (read slp_size) (read pop_size1)
      (slp,fit) = getBest (algorithm2  (mkStdGen 1118781871) (read iterations) (prob_cross, prob_mut) (sigma) (Prelude.map toVariable (read vars)) (read lits) target pop)
  time <- T.getCurrentTime
  let
      diff = T.diffUTCTime  time starttime
  print $ time
  print $ "It took "  ++  show diff ++ "seconds and obtained \n " ++ show slp 
        
main :: IO ()
main = do putStr    "Hi, you can run some of the followings examples or create your own: \n -1. Execute an effective SLP. \n -2. Crossover of two SLPs. \n -3. A small (and fast) example. \n -4. Introduce your own data. \n -5. Exit"
          opcion <- leeIntRango 1 5
          case opcion of
            1 -> do interfaz1
                    main
            2 -> do interfaz2
                    main
            3 -> do interfaz3
                    main
            4 -> do interfaz4
                    main
            5 -> putStr "See u!"

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
          vars = [V "x-1"] 
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
