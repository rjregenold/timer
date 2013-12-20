{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

module Main where

import Control.Error
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, state)
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
  , _entryEndAt   :: UTCTime
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Entry

data Timer = Timer
  { _timerName    :: Text
  , _timerStartAt :: Maybe UTCTime
  , _timerEntries :: [Entry]
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Timer

data Db = Db
  { _dbTimers :: [Timer]
  }
  deriving (Typeable)

deriveSafeCopy 0 'base ''Db

allTimers :: Query Db [Timer]
allTimers = _dbTimers <$> ask

lookupTimerByName :: Text -> Query Db (Maybe Timer)
lookupTimerByName name = find ((==name) . _timerName) . _dbTimers <$> ask

addTimer :: Timer -> Update Db Timer
addTimer timer = state $ \db@Db{..} -> (timer, db { _dbTimers = timer : _dbTimers })

updateTimer :: Timer -> Update Db Timer
updateTimer timer = state $ \db@Db{..} -> (timer, db { _dbTimers = timer : dropTimer _dbTimers})
  where
    dropTimer = filter ((/= (_timerName timer)) . _timerName)

makeAcidic ''Db 
  [ 'allTimers
  , 'lookupTimerByName
  , 'addTimer
  , 'updateTimer
  ]

emptyDb :: Db
emptyDb = Db
  { _dbTimers = []
  }


--------------------------------------------------------------------------------
-- commands
--------------------------------------------------------------------------------

startTimer :: UTCTime -> Timer -> Timer
startTimer now timer@Timer{..} =
  timer { _timerStartAt = Just now }

stopTimer :: UTCTime -> Timer -> Timer
stopTimer endAt timer@Timer{..} =
  maybe timer stopTimer' _timerStartAt
  where
    stopTimer' startAt = timer
      { _timerStartAt = Nothing
      , _timerEntries = Entry startAt endAt : _timerEntries
      }

isTimerActive :: Timer -> Bool
isTimerActive Timer{..} = isJust _timerStartAt

data CommandError = CommandErrorTimerNotFound

cmdStart :: AcidState Db -> Text -> IO Timer
cmdStart db name = do
  now <- getCurrentTime
  findOrCreateTimer
    >>= return . startTimer now
    >>= update db . UpdateTimer
  where
    findOrCreateTimer = query db (LookupTimerByName name)
      >>= maybe (update db $ AddTimer (Timer name Nothing [])) return

cmdStop :: AcidState Db -> Text -> IO (Either CommandError Timer)
cmdStop db name = do
  now <- getCurrentTime
  query db (LookupTimerByName name)
    >>= maybe (return $ Left CommandErrorTimerNotFound) (cmdStop' now)
  where
    cmdStop' now timer =
      update db (UpdateTimer (stopTimer now timer))
        >>= return . Right

cmdActive :: AcidState Db -> IO [Timer]
cmdActive db =
  query db AllTimers
    >>= return . filter isTimerActive

cmdList :: AcidState Db -> Text -> IO (Either CommandError [Entry])
cmdList db name =
  query db (LookupTimerByName name)
    >>= return . maybe (Left CommandErrorTimerNotFound) (Right . _timerEntries)


--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

errDesc :: CommandError -> String
errDesc CommandErrorTimerNotFound = "timer not found"

renderErr :: CommandError -> IO ()
renderErr = putStrLn . errDesc

renderStart :: Timer -> IO ()
renderStart _ = putStrLn "started timer"

renderStop :: Either CommandError Timer -> IO ()
renderStop (Left err) = renderErr err
renderStop (Right timer) = putStrLn "stopped timer"

renderActive :: [Timer] -> IO ()
renderActive = mapM_ renderTimer
  where
    renderTimer Timer{..} = putStrLn (T.unpack _timerName ++ ": " ++ show _timerStartAt)

renderList :: Either CommandError [Entry] -> IO ()
renderList (Left err) = renderErr err
renderList (Right entries) = mapM_ renderEntry entries
  where
    renderEntry Entry{..} = putStrLn $ unwords
      [ "start at: "
      , show _entryStartAt
      , "end at: "
      , show _entryEndAt
      ]


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
    (Start name) -> cmdStart db name >>= renderStart
    (Stop name)  -> cmdStop db name >>= renderStop
    Active       -> cmdActive db >>= renderActive
    (List name)  -> cmdList db name >>= renderList

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> header "timer - a simple way to track time" )
