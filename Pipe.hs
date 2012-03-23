{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, OverlappingInstances #-}
module Main where
import Control.Concurrent.MState
import Control.Monad.Error
import Data.Binary.Put
import Data.Bits
import Data.Word
import Data.Time
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO

import Network.NineP.Server

import qualified Data.Map as M 
import qualified Data.ByteString.Lazy as L
import qualified Network.NineP.Binary as B

data Pipe = Pipe
    { pStartTime :: !UTCTime
    , pUname     :: !String
    , pAname     :: !String
    , pBuffalo   :: L.ByteString
    }

data Input = Input

data Output = Output

instance CommonFile Input Pipe where
    qidPath _ = return 2
    qidVersion _ = return 0
    create _ _ _ _ = throwError ErrOpPerm
    remove _ = throwError ErrOpPerm
    stat _ =
      do
        Pipe time aname uname _ <- get
        return $ Stat 0o222 time time 0 "input" aname uname aname
    wstat _ _ = throwError ErrWstatProhibited

instance StreamWriterFile Input Pipe where
    hook _ _ = return $ (\buf -> get >>= (\(Pipe x y z _) -> put $ Pipe x y z buf))

instance CommonFile Output Pipe where
    qidPath _ = return 1
    qidVersion _ = return 0
    create _ _ _ _ = throwError ErrOpPerm
    remove _ = throwError ErrOpPerm
    stat _ =
      do
        Pipe time aname uname _ <- get
        return $ Stat 0o444 time time 0 "output" aname uname aname
    wstat _ _ = throwError ErrWstatProhibited

instance StreamReaderFile Output Pipe where
    content _ _ = liftM pBuffalo get

instance CommonFile () Pipe where
    qidPath _ = return 0
    qidVersion _ = return 0
    create _ _ _ _ = throwError ErrOpPerm
    remove _ = throwError ErrRootRemove
    stat _ = 
      do
        Pipe time aname uname _ <- get
        return $ Stat (0o755 .|. dmDir) time time 0 "/" aname uname aname
    wstat _ _ = throwError ErrWstatProhibited

instance Directory () Pipe where
    lookup _ = return $ M.fromList [ ("..", directory' ())
                                   , (".", directory' ())
                                   , ("output", srfile' Output)
                                   , ("input", swfile' Input)
                                   ]

instance Server Pipe where
    attach aname uname = do
      Pipe time _ _ _ <- get
      put $ Pipe time aname uname L.empty
      return $ directory' ()

launchTest :: String -> IO ()
launchTest conn = do
    time <- getCurrentTime
    launchServer (Pipe time "" "" L.empty) conn

data Flag = Address String

flags = 
    [ Option ['a'] [] (ReqArg Address "ADDRESS")
        $ "listen at this dial string (overrides NINEP_ADDRESS environment variable)"
    ]

getEnvDefault :: String -> String -> IO String
getEnvDefault x y = catch (getEnv x) (\_ -> return y)

parseArgs :: [String] -> IO String
parseArgs argv = case getOpt Permute flags argv of
    ([Address s], _, []) -> return s
    ([], _, []) ->
      do
        user <- getEnvDefault "USER" "root"
        display <- getEnvDefault "DISPLAY" ":0"
        address <- getEnvDefault "NINEP_ADDRESS" ("unix!/tmp/ns." ++ user ++ "." ++ display ++ "/m9u")
        return $ address
    (_, _, errs) -> hPutStrLn stderr (concat errs ++ usageInfo hdr flags) >> exitWith (ExitFailure 1)
    where hdr = "Usage: test [-a dialstring]"

main = getArgs >>= parseArgs >>= launchTest

