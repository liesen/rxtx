
module Main where

import Prelude hiding (catch)
import Network.Growl.UDP
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad 
import Control.Exception (bracket, finally)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Binary (Binary, encode)
import System.Directory
import System.Exit
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals


registrationPacket = Register "Rx" ["Download finished"] MD5 

sendGrowl :: Binary a => a -> IO Int 
sendGrowl msg = bracket (socket AF_INET Datagram 0) sClose $ \sock -> 
    sendTo sock buf (SockAddrInet 9887 iNADDR_ANY)
  where
    buf = S.concat . L.toChunks . encode $ msg

register :: IO Int
register = sendGrowl registrationPacket 

notify :: String -> String -> IO Int 
notify title description = sendGrowl $ 
    Notify "Rx" "Download finished" Normal True title description MD5

daemonize :: IO () -> IO ()
daemonize action = do
    mask0 <- setFileCreationMask 0
    pid <- forkProcess child
    exitSuccess
  where
    child = do
        sess <- createSession
        installHandler sigHUP Ignore Nothing
        forkProcess $ do
            setCurrentDirectory "/"
            nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
            forM_ [stdInput, stdOutput, stdError] $ \fd -> closeFd fd >> dupTo nullFd fd
            action
        exitSuccess

runServer :: IO ()
runServer = do 
    addr <- liftM head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8192")
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bindSocket sock (addrAddress addr)
    bracket (return sock) sClose loop
  where
    loop sock = do
      (bs, addr) <- recvFrom sock 1024
      notify "Download finished" (C.unpack bs)
      loop sock

main :: IO ()
main = do
    register
    daemonize runServer

