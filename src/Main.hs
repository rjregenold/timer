{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

module Main where

import Control.Monad.State (state)
import Data.Acid
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import Options.Applicative


--------------------------------------------------------------------------------
-- cli
--------------------------------------------------------------------------------

data Command = Start Text
             | Stop Text
             | Active
             | List Text

emptyName :: Text
emptyName = "default"

textArg :: String -> Maybe Text
textArg = Just . T.pack

startOpts :: Parser Command
startOpts = Start
  <$> argument textArg
      ( metavar "NAME"
     <> help "the timer NAME"
     <> value emptyName
      )

stopOpts :: Parser Command
stopOpts = Stop
  <$> argument textArg
      ( metavar "NAME"
     <> help "the timer NAME"
     <> value emptyName 
      )

listOpts :: Parser Command
listOpts = List
  <$> argument textArg
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


--------------------------------------------------------------------------------
-- data
--------------------------------------------------------------------------------

data Entry = Entry
  { _entryStartAt :: UTCTime
  , _entryEndAt   :: Maybe UTCTime
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Entry

data Timer = Timer
  { _timerName :: Text
  , _timerEntries :: [Entry]
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Timer

data Db = Db
  { _dbTimers :: [Timer]
  }
  deriving (Typeable)

deriveSafeCopy 0 'base ''Db

addTimer :: Text -> Update Db Timer
addTimer name = state $ \db@Db{..} -> (timer, db { _dbTimers = timer : _dbTimers })
  where
    timer = Timer name []

makeAcidic ''Db ['addTimer]

emptyDb :: Db
emptyDb = Db
  { _dbTimers = []
  }

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

run :: Command -> IO ()
run cmd = do
  db <- openLocalStateFrom ".timer" emptyDb
  case cmd of
    (Start name) -> do
      timer <- update db $ AddTimer name
      print timer
    _ -> return ()
  putStrLn "not implemented yet"

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> header "timer - a simple way to track time" )
