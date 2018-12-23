{-# LANGUAGE PackageImports #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless) 

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

-- First attempt without monads

type Pos = Point
data Player = Player {position::Pos}

initPlayerState = Player (20.0,20.0)

main :: IO ()
main = do
  glossState <- initState
  withWindow windowWidth windowHeight "Resurrection" $ \win -> do 
    loop glossState win initPlayerState
    exitSuccess
  where 
    loop glossState window playerState = do
      threadDelay 2000
      pollEvents
      renderFrame window glossState playerState
      k <- keyIsPressed window Key'Escape
      l <- keyIsPressed window Key'Left
      r <- keyIsPressed window Key'Right
      u <- keyIsPressed window Key'Up
      d <- keyIsPressed window Key'Down
      let newPlayerState = computeNewState playerState l r u d
      unless k $ loop glossState window newPlayerState

renderFrame::Window->State->Player->IO ()

renderFrame window glossState playerState= do
  let posx = fst $ position playerState
  let posy = snd $ position playerState
  displayPicture (windowWidth, windowHeight) white glossState 1.0 $ 
    Pictures [Color red $ translate posx posy $ circleSolid 30]
  swapBuffers window

keyIsPressed::Window-> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress::KeyState->Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True 
isPress _                  = False

computeNewState::Player->Bool->Bool->Bool->Bool->Player
computeNewState  (Player (x,y)) True _ _ _ = Player (x-10,y)
computeNewState  (Player (x,y)) _ True _ _ = Player (x+10,y)
computeNewState  (Player (x,y)) _ _ True _ = Player (x,y+10)
computeNewState  (Player (x,y)) _ _ _ True = Player (x,y-10)
computeNewState  (Player (x,y)) False False False False = Player (x,y)
