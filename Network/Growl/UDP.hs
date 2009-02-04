module Network.Growl.UDP where 

import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Char (ord)
import Data.Digest.MD5
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L


{--
data Growl = { hostname :: String
             , port :: String
             , password :: Maybe String
             }
--}

data Message = Register { applicationName :: String
                        , notifications :: [String]
                        , checksum :: Checksum
                        }
             | Notify { applicationName :: String
                      , notificationName :: String
                      , priority :: Priority
                      , sticky :: Bool
                      , title :: String
                      , description :: String
                      , checksum :: Checksum
                      }
  deriving (Show)

-- | Growl protocol version
--
-- AES128 indicates that the rest of the packet is encrypted using a 128-bit
-- AES key. The AES key is derived from the password (PKCS12-style) and is 
-- used in Cipher Block Chaining mode with a fixed initialization vector and
-- PKCS7 padding.
data ProtocolVersion = Default
                     | AES128
  deriving (Show, Eq)

instance Enum ProtocolVersion where
  fromEnum Default = 1
  fromEnum AES128  = 2
  toEnum 2 = AES128
  toEnum _ = Default

instance Binary ProtocolVersion where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8


-- | Checksum mechanisms
--
-- Checksums are appended to each message sent to Growl.
data Checksum = MD5 | SHA256 | NOAUTH
  deriving (Show, Eq)

-- | Message types
--
-- A message is either a register- or a notify-message. It's type encoding
-- also depends on the checksum mechanism.
data Type = Registration Checksum
          | Notification Checksum

instance Enum Type where
  fromEnum (Registration MD5)    = 0
  fromEnum (Notification MD5)    = 1
  fromEnum (Registration SHA256) = 2
  fromEnum (Notification SHA256) = 3
  fromEnum (Registration NOAUTH) = 4
  fromEnum (Notification NOAUTH) = 5
  toEnum 0 = Registration MD5
  toEnum 1 = Notification MD5
  toEnum 2 = Registration SHA256
  toEnum 3 = Notification SHA256
  toEnum 4 = Registration NOAUTH
  toEnum 5 = Notification NOAUTH
  toEnum _ = toEnum 0 

instance Binary Type where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8


-- | Notification priority levels
data Priority = VeryLow | Moderate | Normal | High | Emergency
  deriving (Show, Eq)

instance Enum Priority where
  fromEnum VeryLow   = -2
  fromEnum Moderate  = -1
  fromEnum Normal    = 0
  fromEnum High      = 1
  fromEnum Emergency = 2
  toEnum (-2) = VeryLow
  toEnum (-1) = Moderate
  toEnum 0    = Normal
  toEnum 1    = High
  toEnum 2    = Emergency
  toEnum _    = Normal


data Flags = Flags Priority Bool

instance Binary Flags where
  put (Flags priority sticky) = let
      p :: Word8
      p = case priority of
            VeryLow   -> 12 -- 1100
            Moderate  -> 10 -- 1010
            Normal    -> 0  -- 0000
            High      -> 2  -- 0010
            Emergency -> 4  -- 0100
    in do 
      putWord8 0
      putWord8 $ p .|. fromIntegral (fromEnum sticky)
  get = do
    getWord8
    tag <- getWord8
    let sticky   = tag `testBit` 0
        sign     = tag `testBit` 4
        priority = ((tag `shiftR` 1) .&. 3) * (if sign then (-1) else 1)
    return $ Flags (toEnum (fromIntegral priority)) sticky

putByte :: Enum a => a -> Put
putByte = putWord8 . fromIntegral . fromEnum

putLength :: [a] -> Put
putLength = putWord16be . fromIntegral . length

getLength :: Get Int
getLength = liftM fromIntegral getWord16be

putString :: String -> Put
putString = mapM_ put

getString :: Int -> Get String
getString = flip replicateM get 

-- | Changes the endianness of a binary serailisation
changeEndianness :: Put -> Put
changeEndianness = mapM_ putWord32be . runGet (replicateM 4 getWord32le) . runPut

-- | Appends a checksum to a 
withChecksum :: Checksum -> Put -> Put
withChecksum NOAUTH put' = put'
withChecksum MD5    put' = let 
      password = S.pack "password" 
      buf      = runPut put'
      md5      = L.pack $ hash $ L.unpack $ L.append buf (L.fromChunks [password])
    in do
      putLazyByteString buf
      putLazyByteString md5
withChecksum SHA256 put' = error "SHA-256"


instance Binary Message where
  put (Register appName notifications checksum) = withChecksum checksum $ do
    let appName' = runPut $ putString appName
    put Default
    put $ Registration checksum
    putLength $ L.unpack appName' -- app name length
    putByte $ length notifications -- nall
    putByte $ length notifications -- ndef
    putLazyByteString appName'
    forM_ notifications $ \n -> do
      let notification = runPut $ putString n
      putWord16be $ fromIntegral $ L.length notification
      putLazyByteString notification
    mapM_ (putWord8 . fromIntegral) [0..(length notifications - 1)]

  put (Notify appName notification priority sticky title description checksum) = withChecksum checksum $ let
      strings = map (runPut . putString) [notification, title, description, appName]
    in do
      put Default
      put $ Notification checksum
      put $ Flags priority sticky
      mapM_ (putWord16be . fromIntegral . L.length) strings
      mapM_ putLazyByteString strings

  get = do
      ver <- get :: Get ProtocolVersion
      typ <- get 
      case typ of
        Registration checksum -> do
          appNameLength <- liftM fromIntegral getWord16be
          nall <- liftM fromIntegral getWord8
          ndef <- liftM fromIntegral getWord8
          appName <- getString appNameLength
          notifs <- replicateM nall (getLength >>= getString)
          skip ndef -- replicateM_ ndef getWord8
          case checksum of
            MD5    -> skip 16
            SHA256 -> skip 32
            NOAUTH -> return ()
          return $ Register appName notifs checksum
        Notification checksum -> do
          Flags prio sticky <- get
          lengths <- replicateM 4 (liftM fromIntegral getWord16be)
          [notif, title, descr, appName] <- mapM getString lengths  
          return $ Notify appName notif prio sticky title descr checksum

