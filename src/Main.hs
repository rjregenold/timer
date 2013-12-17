{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Options.Applicative


data Command = Start Text
             | Stop Text
             | Active
             | List Text

emptyName :: Text
emptyName = "default"

startOpts :: Parser Command
startOpts = Start
  <$> argument auto
      ( metavar "NAME"
     <> help "the timer NAME"
     <> value emptyName 
      )

stopOpts :: Parser Command
stopOpts = Stop
  <$> argument auto
      ( metavar "NAME"
     <> help "the timer NAME"
     <> value emptyName 
      )

listOpts :: Parser Command
listOpts = List
  <$> argument auto
      ( metavar "NAME"
     <> help "the timer NAME"
     <> value emptyName
      )

commands :: Parser Command
commands = subparser
  ( command "start"
    (info startOpts
          (progDesc "Start a timer")) 
 <> command "stop"
    (info stopOpts
          (progDesc "Stop a timer"))
 <> command "active"
    (info (pure Active)
          (progDesc "List active timers"))
 <> command "list"
    (info listOpts
          (progDesc "List timer entries"))
  )

run :: Command -> IO ()
run _ = putStrLn "not implemented yet"

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> header "timer - a simple way to track time" )
