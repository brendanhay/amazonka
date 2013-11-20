{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Internal.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Types where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Aeson                      hiding (Error)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.Foldable                   (Foldable)
import           Data.Monoid
import           Data.String
import           Data.Text                       (Text)
import           GHC.Generics
import           Network.AWS.Headers
import           Network.HTTP.QueryString.Pickle
import           Network.Http.Client             (Method)
import qualified Network.Http.Client             as Client
import           System.IO.Streams               (InputStream)
import qualified System.IO.Streams               as Stream
import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.Read                       as Read
import           Text.XML.Expat.Pickle.Generic

class Rq a where
    type Er a
    type Rs a

    request  :: a -> AWS Signed
    response :: MonadIO m
             => a
             -> Response
             -> m (Either AWSError (Either (Er a) (Rs a)))

    default response :: (MonadIO m, IsXML (Er a), IsXML (Rs a))
                     => a
                     -> Response
                     -> m (Either AWSError (Either (Er a) (Rs a)))
    response _ = defaultResponse

-- FIXME: an erroneous http status code should indicate the Er parser to use
defaultResponse :: (IsXML e, IsXML a, MonadIO m)
                => Response
                -> m (Either AWSError (Either e a))
defaultResponse Response{..} = liftIO $ do
    bs <- BS.concat <$> Stream.toList rsBody
    when rsDebug . liftIO $ print bs
    return . either (failure bs) (Right . Right) $ fromXML bs
  where
    failure bs e = either (\s -> Left . Err $ concat [s, ", ", e])
        (Right . Left) (fromXML bs)

class Pg a where
    next :: a -> Rs a -> Maybe a

data AWSError = Err String | Ex SomeException
    deriving (Show)

instance Error AWSError where
    strMsg = Err

instance IsString AWSError where
    fromString = Err

class ToError a where
    toError :: a -> AWSError

instance ToError AWSError where
    toError = id

instance ToError String where
    toError = Err

instance ToError SomeException where
    toError = Ex

data Credentials
    = FromKeys ByteString ByteString
    | FromRole ByteString
      deriving (Eq, Ord)

instance Show Credentials where
    show (FromKeys acc _) = BS.unpack $ BS.concat ["FromKeys ", acc, "*****"]
    show (FromRole role)  = BS.unpack $ "FromRole " <> role

data Auth = Auth
    { accessKeyId     :: !ByteString
    , secretAccessKey :: !ByteString
    , securityToken   :: Maybe ByteString
    } deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth
        <$> o .: "AccessKeyId"
        <*> o .: "SecretAccessKey"
        <*> o .: "Token"
    parseJSON _ = mzero

data Env = Env
    { awsRegion :: !Region
    , awsDebug  :: !Bool
    , awsAuth   :: !Auth
    }

newtype AWS a = AWS { unwrap :: ReaderT Env (EitherT AWSError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AWSError)

data Service
    = Global
      { svcName     :: !ByteString
      , svcVersion  :: !ByteString
      }
    | Regional
      { svcName     :: !ByteString
      , svcVersion  :: !ByteString
      }
    | Specific
      { svcName     :: !ByteString
      , svcVersion  :: !ByteString
      , svcEndpoint :: !ByteString
      }
    deriving (Show)

endpoint :: Service -> Region -> ByteString
endpoint Global{..}   _ = svcName <> ".amazonaws.com"
endpoint Regional{..} r = BS.intercalate "." $
    [svcName, BS.pack $ show r, "amazonaws.com"]
endpoint Specific{..} _ = svcEndpoint

override :: ByteString -> Service -> Service
override bs svc = Specific (svcName svc) (svcVersion svc) bs

data Body
    = Strict ByteString
    | Streaming (InputStream ByteString)
    | Empty

instance Show Body where
    show (Strict bs)    = BS.unpack $ BS.concat ["Strict ", bs]
    show (Streaming _) = "Streaming <InputStream>"
    show Empty         = "Empty"

data Request = Request
    { rqService :: !Service
    , rqMethod  :: !Method
    , rqPath    :: !ByteString
    , rqHeaders :: [AnyHeader]
    , rqQuery   :: [(ByteString, ByteString)]
    , rqBody    :: !Body
    } deriving (Show)

data Signed = Signed
    { sMeta    :: !Request
    , sHost    :: !ByteString
    , sBody    :: !Body
    , sRequest :: !Client.Request
    } deriving (Show)

-- instance Show Signed where
--     show Signed{..} = unlines
--         [ "Signed:"
--         , "sURL  = " ++ BS.unpack sURL
--         , "sBody = " ++ show sBody
--         , ""
--         ] ++ show sRequest

-- instance ToJSON Request where
--     toJSON Request{..} = object
--         [ "rqMethod"  .= (String . Text.pack $ show rqMethod)
--         , "rqContent" .= ("Content-Type" `lookup` rqQuery)
--         , "rqAction"  .= ("Action" `lookup` rqQuery)
--         , "rqPath"    .= rqPath
--         , "rqQuery"   .= rqQuery
--         ]

data Response = Response
    { rsDebug   :: !Bool
    , rsCode    :: !Int
    , rsMessage :: !ByteString
    , rsHeaders :: [(ByteString, ByteString)]
    , rsBody    :: InputStream ByteString
    }

data Region
    = NorthVirginia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo
      deriving (Eq, Ord)

instance Show Region where
    show reg = case reg of
        NorthVirginia   -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        Ireland         -> "eu-west-1"
        Singapore       -> "ap-southeast-1"
        Tokyo           -> "ap-northeast-1"
        Sydney          -> "ap-southeast-2"
        SaoPaulo        -> "sa-east-1"

instance Read Region where
    readPrec = readAssocList
        [ ("us-east-1",      NorthVirginia)
        , ("us-west-1",      NorthCalifornia)
        , ("us-west-2",      Oregon)
        , ("eu-west-1",      Ireland)
        , ("ap-southeast-1", Singapore)
        , ("ap-northeast-1", Tokyo)
        , ("ap-southeast-2", Sydney)
        , ("sa-east-1",      SaoPaulo)
        ]

instance IsQuery Region where
    queryPickler = qpPrim

instance IsXML Region where
    xmlPickler = xpContent xpPrim

data AvailabilityZone = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord)

instance Show AvailabilityZone where
    show (AZ r z) = show r ++ [z]

instance Read AvailabilityZone where
    readsPrec _ [] = []
    readsPrec _ s  = [(AZ (read $ init s) (last s), "")]

instance IsQuery AvailabilityZone where
    queryPickler = qpPrim

instance IsXML AvailabilityZone where
    xmlPickler = xpContent xpPrim

data InstanceType
    = T1_Micro
    | M1_Small
    | M1_Medium
    | M1_Large
    | M1_XLarge
    | M3_XLarge
    | M3_2XLarge
    | C1_Medium
    | C1_XLarge
    | CC2_8XLarge
    | M2_XLarge
    | M2_2XLarge
    | M2_4XLarge
    | CR1_8XLarge
    | HI1_4XLarge
    | HS1_8XLarge
    | CG1_4XLarge
      deriving (Eq, Ord, Generic)

instance Show InstanceType where
    show typ = case typ of
        T1_Micro    -> "t1.micro"
        M1_Small    -> "m1.small"
        M1_Medium   -> "m1.medium"
        M1_Large    -> "m1.large"
        M1_XLarge   -> "m1.xlarge"
        M3_XLarge   -> "m3.xlarge"
        M3_2XLarge  -> "m3.2xlarge"
        C1_Medium   -> "c1.medium"
        C1_XLarge   -> "c1.xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        M2_XLarge   -> "m2.xlarge"
        M2_2XLarge  -> "m2.2xlarge"
        M2_4XLarge  -> "m2.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"

instance Read InstanceType where
    readPrec = readAssocList
        [ ("t1.micro",    T1_Micro)
        , ("m1.small",    M1_Small)
        , ("m1.medium",   M1_Medium)
        , ("m1.large",    M1_Large)
        , ("m1.xlarge",   M1_XLarge)
        , ("m3.xlarge",   M3_XLarge)
        , ("m3.2xlarge",  M3_2XLarge)
        , ("c1.medium",   C1_Medium)
        , ("c1.xlarge",   C1_XLarge)
        , ("cc2.8xlarge", CC2_8XLarge)
        , ("m2.xlarge",   M2_XLarge)
        , ("m2.2xlarge",  M2_2XLarge)
        , ("m2.4xlarge",  M2_4XLarge)
        , ("cr1.8xlarge", CR1_8XLarge)
        , ("hi1.4xlarge", HI1_4XLarge)
        , ("hs1.8xlarge", HS1_8XLarge)
        , ("cg1.4xlarge", CG1_4XLarge)
        ]

instance IsQuery InstanceType where
    queryPickler = qpPrim

instance IsXML InstanceType where
    xmlPickler = xpContent xpPrim

readAssocList :: [(String, a)] -> Read.ReadPrec a
readAssocList xs = Read.choice $
    map (\(x, y) -> Read.lift $ ReadP.string x >> return y) xs

newtype Items a = Items { items :: [a] }
    deriving (Eq, Show, Generic, Foldable)

newtype Members a = Members { members :: [a] }
    deriving (Eq, Show, Generic, Foldable)
