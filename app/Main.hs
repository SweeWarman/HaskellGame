{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Arrows #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Exit (exitSuccess)
import System.Random
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless) 
import Control.Arrow
import qualified FRP.Yampa as Frp 

-- GLFW boiler plate
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO () 
withWindow width height title f = do
   GLFW.setErrorCallback $ Just simpleErrorCallback 
   r <- GLFW.init
   when r $ do
     m <- GLFW.createWindow width height title Nothing Nothing 
     case m of
       (Just win) -> do
          GLFW.makeContextCurrent m
          f win
          GLFW.setErrorCallback $ Just simpleErrorCallback 
          GLFW.destroyWindow win
       Nothing -> return () 
     GLFW.terminate
     where
       simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

-- Game specific data types

-- Type aliases
type Position      = Point
type UserCommands  = (Bool,Bool,Bool,Bool)

-- Enumerations
data MonsterStatus = Wander Direction Int | Hunting
data Direction     = WalkUp | WalkDown | WalkLeft | WalkRight


data Player        = Player {
                               positionP::Position
                            }
data Monster       = Monster {
                               positionM::Position
                              ,monsterStatus::MonsterStatus
                             }

-- Signal inputs and outputs
data Input  = Input {
                       userInput  :: Frp.Event UserCommands
                      ,monsterInput::Int
                    } deriving (Show)
 

data Output = Output {
                       getPlayer  :: Player
                      ,getMonster :: Monster
                      }


initPlayer::Player 
initPlayer = Player (10,10)

initMonster::Monster
initMonster = Monster (50,50) (Wander WalkUp 1)

main :: IO ()
main = do
  glossState <- initState
  withWindow windowWidth windowHeight "Resurrection" $ \win -> do 
    rh <- Frp.reactInit (getInput win) (renderFrame win glossState) (gameEvolution initPlayer initMonster)
    loop glossState win rh
    exitSuccess
  where 
    loop glossState window rh = do
      threadDelay 2000
      pollEvents
      k <- keyIsPressed window Key'Escape
      input <- getInput window
      flag <- Frp.react rh (1,Just (input))
      unless k $ loop glossState window rh 

renderFrame::Window->State->Frp.ReactHandle Input Output -> Bool -> Output -> IO Bool
renderFrame window glossState rh ch out = do
  let posx = fst . positionP . getPlayer  $ out
  let posy = snd . positionP . getPlayer  $ out
  let monx = fst . positionM . getMonster $ out
  let mony = snd . positionM . getMonster $ out
  displayPicture (windowWidth, windowHeight) white glossState 1.0 $ 
    Pictures [Color red $ translate posx posy $ circleSolid 30
             ,Color blue $ translate monx mony $ rectangleSolid 15 15]
  swapBuffers window
  return True

keyIsPressed::Window-> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress::KeyState->Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True 
isPress _                  = False

getInput::Window->IO Input 
getInput window = do 
      l <- keyIsPressed window Key'Left
      r <- keyIsPressed window Key'Right
      u <- keyIsPressed window Key'Up
      d <- keyIsPressed window Key'Down
      m <- randomRIO (1,4)
      let inputData = Input (Frp.Event (l,r,u,d)) m
      return inputData 

gameEvolution::Player->Monster->Frp.SF Input Output
gameEvolution initPlayer initMonster = proc i -> do
   rec 
       playr     <- Frp.drSwitch (Frp.constant (initPlayer,(False,False,False,False))) >>^ updatePos -< (userInput i,newEvent)
       monIS     <- Frp.iPre (0,initMonster) -< (monsterInput i,monstr)
       monstr    <- arr updateMonsterState -< (fst monIS,snd monIS,playr)
       let posInputs = Frp.fromEvent $ userInput i
           newEvent  = userInput i `Frp.tag` (Frp.constant (playr,posInputs))
           outSignal = Output playr monstr  
   Frp.returnA -< outSignal 

updatePos (oldPos,newInput) = computeNewState oldPos newInput

computeNewState::Player->(Bool,Bool,Bool,Bool)->Player
computeNewState  (Player (x,y)) (True,_,_,_) = Player (x-10,y)
computeNewState  (Player (x,y)) (_,True,_,_) = Player (x+10,y)
computeNewState  (Player (x,y)) (_,_,True,_) = Player (x,y+10)
computeNewState  (Player (x,y)) (_,_,_,True) = Player (x,y-10)
computeNewState  (Player (x,y)) (False,False,False,False) = Player (x,y)

updateMonsterState::(Int,Monster,Player)->Monster
updateMonsterState (val,monstr,playr) = newMonster 
    where
      newMonster = Monster (mx+dx,my+dy) monstatus  
      (px,py) = (fst . positionP $ playr, snd . positionP $ playr)
      (mx,my) = (fst . positionM $ monstr, snd . positionM $ monstr)
      (dx,dy) = if monsterIsNear 
                then ((px - mx)*0.002,(py - my)*0.002)
                else wanderRandomnly  
      monsterIsNear = (sqrt $ (px - mx)^2 + (py - my)^2) < 100
      monstatus = if monsterIsNear
                  then Hunting
                  else
                    case val of
                      1 -> Wander WalkUp 2
                      2 -> Wander WalkDown 2
                      3 -> Wander WalkLeft 2
                      4 -> Wander WalkRight 2
                      _ -> Wander WalkRight 0 
      wanderRandomnly = case monstatus of
                          Wander WalkUp val    -> (0,fromIntegral val) 
                          Wander WalkDown val  -> (0,fromIntegral (-val))
                          Wander WalkLeft val  -> (fromIntegral (-val),0)
                          Wander WalkRight val -> (fromIntegral val,0)
                         
