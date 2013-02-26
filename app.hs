{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
module Main where

import Network.WebSockets 
import Network.WebSockets.Snap 

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_, liftM, forever)
import Control.Applicative
import Control.Concurrent (MVar, newMVar)

import Snap.Core
import Snap.Http.Server.Config
import Snap.Http.Server 
import Snap.Util.FileServe

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

type ServerState = String

simpleConfig :: Config m a
simpleConfig = foldl (\accum new -> new accum) emptyConfig base where
    base = [hostName, accessLog, errorLog, locale, port, ip, verbose]
    hostName = setHostname (bsFromString "localhost")
    accessLog = setAccessLog (ConfigFileLog "log/access.log")
    errorLog = setErrorLog (ConfigFileLog "log/error.log")
    locale = setLocale "US"
    port = setPort 9161
    ip = setBind (bsFromString "127.0.0.1")
    verbose = setVerbose True
    bsFromString = TE.encodeUtf8 . T.pack


meow :: TextProtocol p => WebSockets p ()
meow = forever $ do
      msg <- receiveData
      sendTextData $ msg `T.append` ", meow."

wsApp :: Network.WebSockets.Request -> WebSockets Hybi10 ()
wsApp r = do 
    acceptRequest r
    msg <- receiveData
    liftIO $ T.putStrLn msg
    sendTextData (T.pack "Hello world!")

site :: Snap ()
site = ifTop (serveFile "public/index.html") <|> 
    route [ ("ws", runWebSocketsSnap wsApp ) ] <|>
    route [ ("", (serveDirectory "public")) ] 

main :: IO ()
main = do
  httpServe simpleConfig $ site

