{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    TemplateHaskell, TypeFamilies #-}

module Main where

import Control.Error
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (State(..), evalState, get, modify, put, state)
import Data.Acid
import Data.List
import Data.Maybe
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme.Time
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

instance SafeCopy Day where
  kind = base
  getCopy = contain $ ModifiedJulianDay <$> safeGet
  putCopy = contain . safePut . toModifiedJulianDay
  errorTypeName = const "Day"

instance SafeCopy DiffTime where
  kind = base
  getCopy = contain $ fromRational <$> safeGet
  putCopy = contain . safePut . toRational
  errorTypeName = const "DiffTime"

instance SafeCopy NominalDiffTime where
  kind = base
  getCopy = contain $ fromRational <$> safeGet
  putCopy = contain . safePut . toRational
  errorTypeName = const "NominalDiffTime"

instance SafeCopy UTCTime where
  kind = base
  getCopy   = contain $ do day      <- safeGet
                           diffTime <- safeGet
                           return (mkUTCTime day diffTime)
  putCopy u = contain $ do safePut (utctDay $ unUTCTime u)
                           safePut (utctDayTime $ unUTCTime u)
  errorTypeName = const "UTCTime"

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

entryDuration :: Entry -> NominalDiffTime
entryDuration Entry{..} = diffUTCTime _entryEndAt _entryStartAt

data CommandError = CommandErrorTimerNotFound Text

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
    >>= maybe (return $ Left $ CommandErrorTimerNotFound name) (cmdStop' now)
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
    >>= return . maybe (Left $ CommandErrorTimerNotFound name) (Right . _timerEntries)


--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

errDesc :: CommandError -> String
errDesc (CommandErrorTimerNotFound name) = "timer [" ++ T.unpack name ++ "] not found"

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
    renderEntry = putStrLn . drawEntry

drawEntry :: Entry -> String
drawEntry entry@Entry{..} = unwords
  [ "duration: "
  , drawDuration $ toDuration $ entryDuration entry
  , "start at: "
  , show _entryStartAt
  , "end at: "
  , show _entryEndAt
  ]

data UIDuration = UIDuration Integer Integer Integer

drawDuration :: UIDuration -> String
drawDuration (UIDuration hour min sec) = intercalate ":" $ map show [hour, min, sec]

hourInSec, minInSec, secInSec :: Integer
hourInSec = 3600
minInSec  = 60
secInSec  = 1

toDuration :: NominalDiffTime -> UIDuration
toDuration diffTime = evalState go (toSeconds diffTime)
  where
    go = do
      hour <- splitDuration hourInSec
      min  <- splitDuration minInSec
      sec  <- splitDuration secInSec
      return $ UIDuration hour min sec

splitDuration :: Integer -> State Double Integer
splitDuration divisor = do
  duration <- get
  let amount = floor $ duration / (fromIntegral divisor)
  put $ duration - (fromIntegral (amount * divisor))
  return amount


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
