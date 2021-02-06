module Fitness where

import Expressions as Ex
import Data.Set as S
import Data.Map as M

mabs :: Num a => Maybe a -> Maybe a
mabs Nothing = Nothing
mabs (Just x) = Just (abs x)


minMaybe :: Maybe Literal -> Maybe Literal -> Maybe Literal
minMaybe Nothing Nothing = Nothing
minMaybe (Just a) Nothing =  Just a
minMaybe Nothing (Just b) = Just b
minMaybe (Just a) (Just b) = Just (min a b)
                
fitness :: [(State, Literal)] -> Variable -> SLP -> Maybe Literal
fitness vals var slp=   mabs ( (*) <$> (Just m)<*>(Prelude.foldl1 addM (Prelude.map (fitness1 slp var) vals)))
                      -- == Nothing then error ":(" else mabs ( (*) <$> (Just m)<*>(Prelude.foldl1 addM (Prelude.map (fitness1 slp var) vals)))
    where
      m = fromRational (1 / (toRational (length vals)))
      
fitness1 :: SLP -> Variable ->(State, Literal) -> Maybe Literal
fitness1  slp var (sigma, v) = (-) <$> val <*> (Just v)
    where
      effective = effectiveSLP slp (S.singleton var)
      val = (getValue (semanticEffectiveSLP effective sigma) var)
      --val1 = if val == Nothing  then error 
       --      ((show var) ++ "\n"++ show effective ++ "\n" ++ show slp)
         --    else val
{-
fitness2 :: [(State, Literal)] -> [Variable] -> SLP -> Maybe Literal
fitness2 _ [] _ = Nothing
fitness2 sigma (x:xs) slp = minMaybe (fitness sigma x slp) (fitness2 sigma xs slp)
                            
fitness2A :: [(State, Literal)] -> SLP -> Maybe Literal
fitness2A sigma slp = fitness2 sigma used slp --(slpGamma slp) slp
    where
      used = [V "u12"]
  -}                    
exslp1 :: SLP
exslp1 = M.fromList [(V "u1",Sub (Var (V "x1")) (Lit 1.0)),
                     (V "u2",Add (Var (V "u1")) (Var (V "x1"))),
                     (V "u3",Add (Var (V "x1")) (Lit 1.0)),
                     (V "u4",Prod (Var (V "y1")) (Lit 1.0)),
                     (V "u5",Prod (Var (V "u3")) (Var (V "y1"))),
                     (V "u6",Sub (Var (V "u4")) (Var (V "u1"))),
                     (V "u7",Sub (Lit 1.0) (Lit 1.0)),
                     (V "u8",Prod (Lit 1.0) (Lit 1.0)),
                     (V "u9",Prod (Var (V "u6")) (Var (V "u7"))),
                     (V "u10",Prod (Var (V "u4")) (Var (V "u7")))]

dummex :: SLP
dummex = M.fromList [ (V "u1", Add (Var (V "x-1")) (Var (V "x-2"))),
                      (V "u2", Prod (Var (V "u1")) (Var (V "u1")) ),
                      (V "u3", Sub (Lit 1) (Lit 0))
         ]
{-
                  
sl ::SLP
sl = M.fromList [("u1",Prod (Lit 1.0)  (Lit 1.0)),
               ("u2",Prod (Var "y")  (Var "x")),
               ("u3",Add (Lit 1.0)  (Lit 1.0)),
               ("u4",Sub (Lit 1.0)  (Var "x")),
               ("u5",Add (Lit 1.0)  (Var "y")),
               ("u6",Sub (Var "y")  (Var "u2")),
               ("u7",Prod (Lit 1.0) (Var "u3")),
               ("u8", Sub (Lit 1.0)  (Var "x")),
               ("u9", Prod (Lit 1.0) (Var "y"))]
-}
