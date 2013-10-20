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
import           Data.List                       (intercalate)
import           Data.String
import           Data.Strings
import qualified Data.Text                       as Text
import           GHC.Generics
import           Network.AWS.Internal.String
import           Network.HTTP.QueryString.Pickle
import           Network.Http.Client             hiding (ContentType, post, put)
import           System.IO.Streams               (InputStream)
import qualified System.IO.Streams               as Stream
import           Text.ParserCombinators.ReadP    (string)
import qualified Text.Read                       as Read
import           Text.XML.Expat.Pickle.Generic

class Rq a where
    type Er a
    type Rs a

    request  :: MonadIO m => a -> m RawRequest
    response :: MonadIO m
             => a
             -> InputStream ByteString
             -> m (Either AWSError (Either (Er a) (Rs a)))

    -- FIXME: Convert fromXML to use streaming input
    default response :: (MonadIO m, IsXML (Er a), IsXML (Rs a))
                     => a
                     -> InputStream ByteString
                     -> m (Either AWSError (Either (Er a) (Rs a)))
    response _ str = liftIO $ do
        bs <- BS.concat <$> Stream.toList str
        return . either (failure bs) success $ fromXML bs
      where
        failure bs e = either
            (\s -> Left . Err $ concat [s, ", ", e])
            (Right . Left)
            (fromXML bs)

        success = Right . Right

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

class Prefixed a where
    prefixed :: a -> ByteString

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
    { awsRegion :: Maybe Region
    , awsDebug  :: !Bool
    , awsAuth   :: !Auth
    }

newtype AWS a = AWS { unwrap :: ReaderT Env (EitherT AWSError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AWSError)

getAuth :: AWS Auth
getAuth = awsAuth <$> ask

serviceRegion :: Service -> AWS Region
serviceRegion svc
    | svcGlobal svc = return defaultRegion
    | otherwise     = currentRegion

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirginia <$> awsRegion <$> ask

defaultRegion :: Region
defaultRegion = NorthVirginia

debugEnabled :: AWS Bool
debugEnabled = awsDebug <$> ask

whenDebug :: AWS () -> AWS ()
whenDebug action = debugEnabled >>= \p -> when p action

hoistError :: (MonadError e m, Error e) => Either e a -> m a
hoistError = either throwError return

liftEitherT :: ToError e => EitherT e IO a -> AWS a
liftEitherT = AWS . lift . fmapLT toError

newtype ServiceVersion = ServiceVersion ByteString
    deriving (Eq, Show, IsString, Strings)

data SigningVersion
    = SigningVersion2
    | SigningVersion3
    | SigningVersion4
      deriving (Show)

data Endpoint
    = Global !ByteString
    | Regional (Region -> ByteString)

data Service = Service
    { svcName     :: !ByteString
    , svcVersion  :: !ServiceVersion
    , svcSigner   :: !SigningVersion
    , svcEndpoint :: !Endpoint
    }

svcPath :: Strings a => Service -> a -> a
svcPath svc p = sJoin (sFromString "/") [sPack $ svcVersion svc, p]

svcGlobal :: Service -> Bool
svcGlobal Service{svcEndpoint=(Global _)} = True
svcGlobal _                               = False

endpoint :: Service -> Region -> ByteString
endpoint svc =
    case svcEndpoint svc of
        Global end -> const end
        Regional f -> f

instance Show Service where
    show Service{..} = intercalate " "
        [ "Service {"
        , "svcName = "    ++ BS.unpack svcName
        , "svcVersion = " ++ show svcVersion
        , "svcSigner = "  ++ show svcSigner
        , "}"
        ]

data Body
    = Strict ByteString
    | Streaming (InputStream ByteString)
    | Empty

instance Show Body where
    show (Strict _)    = "Strict <ByteString>"
    show (Streaming _) = "Streaming <InputStream>"
    show Empty         = "Empty"

data RawRequest = RawRequest
    { rqService :: !Service
    , rqMethod  :: !Method
    , rqContent :: !ContentType
    , rqPath    :: !ByteString
    , rqHeaders :: [(ByteString, ByteString)]
    , rqQuery   :: [(ByteString, ByteString)]
    , rqBody    :: !Body
    } deriving (Show)

instance ToJSON RawRequest where
    toJSON RawRequest{..} = object
        [ "rqMethod"  .= (String . Text.pack $ show rqMethod)
        , "rqContent" .= (String . Text.pack $ show rqContent)
        , "rqAction"  .= ("Action" `lookup` rqQuery)
        , "rqPath"    .= rqPath
        , "rqQuery"   .= rqQuery
        ]

queryAppend :: RawRequest -> [(ByteString, ByteString)] -> RawRequest
queryAppend rq qry = rq { rqQuery = rqQuery rq ++ qry }

queryRequest :: IsQuery a
             => Service
             -> Method
             -> ByteString
             -> a
             -> RawRequest
queryRequest svc meth path q =
    RawRequest svc meth FormEncoded path [] (toQuery q) Empty

xmlRequest :: IsXML a
           => Service
           -> Method
           -> ByteString
           -> a
           -> RawRequest
xmlRequest svc meth path =
    RawRequest svc meth XML path [] [] . Strict . toXML

data SignedRequest = SignedRequest
    { srqUrl     :: !ByteString
    , srqBody    :: !Body
    , srqRequest :: !Request
    } deriving (Show)

data ContentType
    = FormEncoded
    | XML

instance Show ContentType where
    show FormEncoded = "application/x-www-form-urlencoded"
    show XML         = "application/xml"

data Region
    = NorthVirginia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo
      deriving (Eq)

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
    } deriving (Eq)

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
      deriving (Eq, Generic)

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
    map (\(x, y) -> Read.lift $ string x >> return y) xs
