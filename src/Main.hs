{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

module Main where

import Control.Monad.Reader (ask)
import Control.Monad.State (state)
import Data.Acid
import Data.List
import Data.Maybe
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))


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

lookupTimer :: Text -> Query Db (Maybe Timer)
lookupTimer name = find ((==name) . _timerName) . _dbTimers <$> ask

addTimer :: Text -> Update Db Timer
addTimer name = state $ \db@Db{..} -> (timer, db { _dbTimers = timer : _dbTimers })
  where
    timer = Timer name []

makeAcidic ''Db 
  [ 'lookupTimer
  , 'addTimer
  ]

emptyDb :: Db
emptyDb = Db
  { _dbTimers = []
  }


--------------------------------------------------------------------------------
-- commands
--------------------------------------------------------------------------------

activeEntries :: Timer -> [Entry]
activeEntries = filter isEntryActive . _timerEntries

isEntryActive :: Entry -> Bool
isEntryActive = isNothing . _entryEndAt

closeEntry :: UTCTime -> Entry -> Entry
closeEntry now entry = entry { _entryEndAt = Just now }

cmdStart :: AcidState Db -> Text -> IO ()
cmdStart db name = do
  timer <- findOrCreateTimer
  now <- getCurrentTime
  let entries = map (closeEntry now) $ activeEntries timer
  print timer
  where
    findOrCreateTimer = query db (LookupTimer name)
      >>= maybe (update db $ AddTimer name) return

cmdStop :: AcidState Db -> Text -> IO ()
cmdStop db name = query db (LookupTimer name) >>= print

cmdActive :: AcidState Db -> IO ()
cmdActive db = putStrLn "showing active"

cmdList :: AcidState Db -> Text -> IO ()
cmdList db name = putStrLn "listing details"


--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

timerDir :: FilePath -> FilePath
timerDir = (</> ".timer")

run :: Command -> IO ()
run cmd = do
  dir <- timerDir <$> getHomeDirectory
  db <- openLocalStateFrom dir emptyDb
  case cmd of
    (Start name) -> cmdStart db name
    (Stop name)  -> cmdStop db name
    Active       -> cmdActive db
    (List name)  -> cmdList db name

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> header "timer - a simple way to track time" )
