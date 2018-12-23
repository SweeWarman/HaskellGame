{-# LANGUAGE Arrows #-}
module Main where

import qualified FRP.Yampa as Y
import Control.Arrow


diffInput:: Y.SF a (Y.Event (Double,Double))
diffInput = Y.afterEach [(1.0,(1.0,2.0)),(2.0,(1.0,-1.0)),(3.0,(0.0,-3.0))]

incPosition :: Y.SF (Y.Event (Double,Double)) (Double,Double)

{-
incPosition = proc i -> do
  rec e       <- Y.dHold (0.0,0.0) -< i 
      (x',y') <- Y.iPre (0.0,0.0) -< (x,y)
      (x,y) <- Y.arr (\((dx,dy),xp,yp)->(xp+dx,yp+dy)) -< (e,x',y')
  Y.returnA -< (x,y)
-}


movement::Y.SF ((Double,Double),Double,Double) (Double,Double)
movement = Y.arr (\((dx,dy),xp,yp)->(xp+dx,yp+dy)) 

incPosition = proc i -> do
  rec 
      let e1 = fst . Y.fromEvent $ i
      let e2 = snd . Y.fromEvent $ i
      (x,y) <- Y.drSwitch (Y.constant ((0,0),0,0)) >>> movement -< (i,i `Y.tag` Y.constant ((e1,e2),x,y))
  Y.returnA -< (x,y)

movPosition :: Y.SF a (Double,Double)
movPosition = diffInput >>> incPosition

inputSamples = [i|i<-[1..20]]
encodedVals = (Y.deltaEncode 0.5 inputSamples) 

-- testPosition :: [(Double,Double)]
testDiffInputs =Y.embed (diffInput) encodedVals 
testPosition = Y.embed (movPosition) encodedVals
main :: IO ()
main = do
  putStrLn "Testing yampa" 
  print testDiffInputs
  print testPosition

