{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
import           Control.Monad.IO.Class
import           Control.Monad.Reader            hiding (lift)
import           Data.Aeson                      hiding (Error)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Encoding
import           Data.Time
import           GHC.Generics
import           Network.HTTP.QueryString.Pickle
import           Network.Http.Client             hiding (ContentType, post, put)
import           System.Locale                   (defaultTimeLocale)
import           Text.ParserCombinators.ReadP    (string)
import           Text.Read                       hiding (String)
import           Text.XML.Expat.Pickle.Generic

data Region
    = NorthVirgnia
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
        NorthVirgnia    -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        Ireland         -> "eu-west-1"
        Singapore       -> "ap-southeast-1"
        Tokyo           -> "ap-northeast-1"
        Sydney          -> "ap-southeast-2"
        SaoPaulo        -> "sa-east-1"

instance Read Region where
    readPrec = readAssocList
        [ ("us-east-1",      NorthVirgnia)
        , ("us-west-1",      NorthCalifornia)
        , ("us-west-2",      Oregon)
        , ("eu-west-1",      Ireland)
        , ("ap-southeast-1", Singapore)
        , ("ap-northeast-1", Tokyo)
        , ("ap-southeast-2", Sydney)
        , ("sa-east-1",      SaoPaulo)
        ]

instance IsByteString Region where
    toBS = BS.pack . show

instance IsXML Region where
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
        [ ("t1.micro", T1_Micro)
        , ("m1.small", M1_Small)
        , ("m1.medium", M1_Medium)
        , ("m1.large", M1_Large)
        , ("m1.xlarge", M1_XLarge)
        , ("m3.xlarge", M3_XLarge)
        , ("m3.2xlarge", M3_2XLarge)
        , ("c1.medium", C1_Medium)
        , ("c1.xlarge", C1_XLarge)
        , ("cc2.8xlarge", CC2_8XLarge)
        , ("m2.xlarge", M2_XLarge)
        , ("m2.2xlarge", M2_2XLarge)
        , ("m2.4xlarge", M2_4XLarge)
        , ("cr1.8xlarge", CR1_8XLarge)
        , ("hi1.4xlarge", HI1_4XLarge)
        , ("hs1.8xlarge", HS1_8XLarge)
        , ("cg1.4xlarge", CG1_4XLarge)
        ]

instance IsQuery InstanceType where
    queryPickler = qpPrim

instance IsXML InstanceType where
    xmlPickler = xpContent xpPrim

readAssocList :: [(String, a)] -> ReadPrec a
readAssocList xs = choice $ map (\(x, y) -> lift $ string x >> return y) xs

data Auth = Auth
    { accessKey :: !ByteString
    , secretKey :: !ByteString
    } deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth
        <$> o .: "AccessKeyId"
        <*> o .: "SecretAccessKey"
    parseJSON _ = mzero

data Env = Env
    { awsRegion :: Maybe Region
    , awsAuth   :: !Auth
    , awsDebug  :: !Bool
    }

data ContentType
    = FormEncoded
    | XML

instance Show ContentType where
    show = BS.unpack . toBS

instance IsByteString ContentType where
    toBS FormEncoded = "application/x-www-form-urlencoded"
    toBS XML         = "application/xml"

type AWSContext = EitherT Error AWS

data Error = Error String | Ex SomeException

instance Show Error where
    show (Error s) = s
    show (Ex ex)   = show ex

instance IsString Error where
    fromString = Error

newtype AWS a = AWS { unWrap :: EitherT Error (ReaderT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirgnia <$> fmap awsRegion ask

whenDebug :: IO () -> AWSContext ()
whenDebug io = fmap awsDebug ask >>= \p -> liftIO $ when p io

throwError :: String -> AWSContext a
throwError = throwT . Error

fmapError :: Monad m => EitherT String m a -> EitherT Error m a
fmapError = fmapLT Error

hoistError :: Monad m => Either String a -> EitherT Error m a
hoistError = hoistEither . fmapL Error

tryAWS :: IO a -> AWSContext a
tryAWS = fmapLT Ex . syncIO

type EitherXML a b = Either String (Either a b)

eitherXML :: Show b => Either String (Either a b) -> Either Error a
eitherXML = g . f
  where
    f = fmap (h . fmapR (Error . show))
    g = join . fmapL Error

    h (Left a)  = Right a
    h (Right b) = Left b

data RawRequest s b where
    RawRequest :: { rqMethod  :: !Method
                  , rqContent :: !ContentType
                  , rqAction  :: Maybe ByteString
                  , rqPath    :: Maybe ByteString
                  , rqHeaders :: Map ByteString ByteString
                  , rqQuery   :: [(ByteString, ByteString)]
                  , rqBody    :: Maybe ByteString
                  }
                -> RawRequest s b

deriving instance Show (RawRequest s b)

instance ToJSON (RawRequest s b) where
    toJSON RawRequest{..} = object
        [ "rqMethod"  .= (String . Text.pack $ show rqMethod)
        , "rqContent" .= (String $ toText rqContent)
        , "rqAction"  .= rqAction
        , "rqPath"    .= rqPath
        , "rqQuery"   .= rqQuery
        ]

emptyRequest :: Method
             -> ContentType
             -> ByteString
             -> Maybe ByteString
             -> RawRequest s b
emptyRequest meth content path body = RawRequest
    { rqMethod  = meth
    , rqContent = content
    , rqAction  = Nothing
    , rqHeaders = Map.empty
    , rqQuery   = []
    , rqBody    = body
    , rqPath    = if BS.null path then Nothing else Just path
    }

data SignedRequest = SignedRequest
    { rqUrl     :: !ByteString
    , rqPayload :: !(Maybe ByteString)
    , rqRequest :: !Request
    } deriving (Show)

data SigningVersion
    = SigningVersion2
    | SigningVersion3
    | SigningVersion4
      deriving (Show)

data Service = Service
    { svcName     :: !ByteString
    , svcVersion  :: !ByteString
    , svcEndpoint :: !ByteString
    , svcSigner   :: !SigningVersion
    , svcRegion   :: !Region
    }

awsService :: ByteString -> ByteString -> SigningVersion -> AWS Service
awsService name ver signer = do
    reg <- currentRegion
    return $! Service name ver (endpoint reg) signer reg
 where
   endpoint reg = name <> "." <> toBS reg <> ".amazonaws.com"

class AWSService s where
    service :: RawRequest s b -> AWS Service

class AWSRequest s a b | a -> s b where
    request :: a -> RawRequest s b

class AWSResponse s b | b -> s where
    response :: ByteString -> Either Error b

class Template a where
    readTemplate :: a -> ByteString

class IsByteString a where
    toText :: a -> Text
    toBS   :: a -> ByteString

    toText = decodeUtf8 . toBS

instance IsByteString ByteString where
    toBS   = id
    toText = decodeUtf8

instance IsByteString Text where
    toBS   = encodeUtf8
    toText = id

instance IsByteString Int where
    toBS = BS.pack . show

instance IsByteString Integer where
    toBS = BS.pack . show

instance IsByteString UTCTime where
    toBS = BS.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"

instance IsByteString Method where
    toBS = BS.pack . show
