{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Arrows #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Exit (exitSuccess)
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
type Pos           = Point
type UserCommands  = (Bool,Bool,Bool,Bool)

-- Enumerations
data MonsterStatus = Wander Direction Int | Hunting
data Direction     = WalkUp | WalkDown | WalkLeft | WalkRight


data Player        = Player {
                               position::Pos
                            }
data Monster       = Monster {
                               position::Pos
                              ,monsterStatus::MonsterStatus
                             }

-- Signal inputs and outputs
Data Input  = Input {
                       userInput    :: Frp.Event UserCommands
                      ,monsterInput :: Int
                    }

Data Output = Output {
                       getPlayer  :: Player
                      ,getMonster :: Monster
                      }


main :: IO ()
main = do
  glossState <- initState
  withWindow windowWidth windowHeight "Resurrection" $ \win -> do 
    rh <- Frp.reactInit (getInput win) (renderFrame win glossState) movement 
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

renderFrame::Window->State->Frp.ReactHandle Inp Out -> Bool -> Out -> IO Bool
renderFrame window glossState rh ch out = do
  let posx = fst $ position out
  let posy = snd $ position out
  displayPicture (windowWidth, windowHeight) white glossState 1.0 $ 
    Pictures [Color red $ translate posx posy $ circleSolid 30]
  swapBuffers window
  return True

keyIsPressed::Window-> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress::KeyState->Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True 
isPress _                  = False

getInput::Window->IO (Frp.Event Inpdirection)
getInput window = do 
      l <- keyIsPressed window Key'Left
      r <- keyIsPressed window Key'Right
      u <- keyIsPressed window Key'Up
      d <- keyIsPressed window Key'Down
      return (Frp.Event (l,r,u,d))

movement::Frp.SF Inp Out 
movement = proc i -> do
   rec 
       pos <- Frp.drSwitch (Frp.constant (Player (0.0,0.0),(False,False,False,False))) >>^ updatePos -< (i,newEvent)

       let posInputs = Frp.fromEvent i
           newEvent  = i `Frp.tag` (Frp.constant (pos,posInputs))
   Frp.returnA -< pos

updatePos (oldPos,newInput) = computeNewState oldPos newInput

computeNewState::Player->(Bool,Bool,Bool,Bool)->Player
computeNewState  (Player (x,y)) (True,_,_,_) = Player (x-10,y)
computeNewState  (Player (x,y)) (_,True,_,_) = Player (x+10,y)
computeNewState  (Player (x,y)) (_,_,True,_) = Player (x,y+10)
computeNewState  (Player (x,y)) (_,_,_,True) = Player (x,y-10)
computeNewState  (Player (x,y)) (False,False,False,False) = Player (x,y)
