module RxTx.Tx.Tx where

import Char (isSpace)
import Control.Monad (when, liftM)
import Data.List (deleteBy)
import Data.Torrent
import IO (bracket) 
import Maybe
import Network.Curl
import System.Environment (getArgs)
import Text.JSON 
import Text.JSON.String 
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Exception (try)


rpcURL :: String -> Int -> URLString
rpcURL host port = concat ["http://", host, ":", show port, "/transmission/rpc"]


parseTorrentFile :: FilePath -> IO (Either String Torrent)
parseTorrentFile path = catch (L.readFile path >>= return . readTorrent)
                              (return . Left . show)
                         

validateTorrentFile :: FilePath -> IO Bool
validateTorrentFile path = parseTorrentFile path >>= return . either (const False) (const True)


data RpcMethod = Add
               | Set
               | Remove

instance Show RpcMethod where
  show Add = "torrent-add"
  show Set = "torrent-set"
  show Remove = "torrent-remove"


data RpcMessage = RpcRequest { method :: RpcMethod
                             , arguments :: [(String, JSValue)]
                             , tag :: Maybe String
                             }
                | RpcResponse
  deriving (Show)

instance JSON RpcMessage where
  readJSON json = error "readJSON is not implemented for RpcMessage"
  showJSON (RpcRequest message arguments tag) = let
      method' = Just . jsonString . show $ message
      arguments' = case arguments of
        [] -> Nothing
        as -> Just . makeObj $ as
      tag' = fmap jsonString $ tag
    in 
      makeObj $ map (\(key, Just value) -> (key, value)) -- Remove Just constructor
              $ filter (isJust . snd) -- Remove pairs with empty value
              $ zip ["method", "arguments", "tag"] [method', arguments', tag']


-- Helper for converting a String to a JSON value
jsonString :: String -> JSValue
jsonString = JSString . toJSString

-- Create a request that starts a torrent download
torrentAdd :: String -> Maybe String -> RpcMessage
torrentAdd metainfo tag = RpcRequest Add [("metainfo", jsonString metainfo)] tag

buildRpcRequest :: FilePath -> IO RpcMessage
buildRpcRequest f = readFile f >>= return . flip torrentAdd Nothing . filter (not . isSpace) . Base64.encode

executeRpcRequest :: URLString -> RpcMessage -> IO (Either String JSValue)
executeRpcRequest url request = do
    curl <- initialize
    response <- do_curl curl url opts
    return $ case respCurlCode response of
      CurlOK    -> runGetJSON readJSObject $ respBody response
      errorCode -> Left $ show errorCode 
  where
    body = encode $ showJSON request
    opts = (CurlPostFields [body]) : method_POST -- CurlHttpPost does NOT work!


-- | Requests to begin the download of a torrent
tx :: URLString -> FilePath -> IO ()
tx transmission torrent = do
    ok <- validateTorrentFile torrent
    when ok $ do
      request <- buildRpcRequest torrent
      response <- executeRpcRequest transmission request
      either print (print . flip showJSValue "") response


wl500gp = rpcURL "wl500gp" 9091 -- URL of RPC service

main = do
    args <- getArgs
    case args of
      torrent:_ -> tx wl500gp torrent
      _         -> return ()

