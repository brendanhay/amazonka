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
import           Control.Exception
import           Control.Monad
import           Data.Aeson                      hiding (Error)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.List                       (intercalate)
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

type family Er a
data family Rs a

class Rq a where
    request  :: a -> RawRequest
    response :: ByteString -> Either Error (Either (Er a) (Rs a))
    paginate :: Rs a -> Maybe a

    default response :: (IsXML (Er a), IsXML (Rs a))
                     => ByteString
                     -> Either Error (Either (Er a) (Rs a))
    response bstr = either failure success $ fromXML bstr
      where
        failure = const . either (Left . Error) (Right . Left) $ fromXML bstr
        success = Right . Right

    paginate = const Nothing

class ToError a where
    toError :: a -> Error

instance ToError String where
    toError = Error

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

data Error = Error String | Ex SomeException

instance IsString Error where
    fromString = Error

data Auth = Auth
    { accessKey :: !ByteString
    , secretKey :: !ByteString
    } deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth
        <$> o .: "AccessKeyId"
        <*> o .: "SecretAccessKey"
    parseJSON _ = mzero

newtype ServiceVersion = ServiceVersion ByteString
    deriving (Eq, Show, IsString, IsByteString)

svcPath :: IsByteString a => Service -> a -> ByteString
svcPath svc p = "/" <> toBS (svcVersion svc) <> "/" <> toBS p

data SigningVersion
    = SigningVersion2
    | SigningVersion3
    | SigningVersion4
      deriving (Show)

data Service = Service
    { svcName     :: !ByteString
    , svcVersion  :: !ServiceVersion
    , svcSigner   :: !SigningVersion
    , svcEndpoint :: Region -> ByteString
    }

instance Show Service where
    show Service{..} = intercalate " "
        [ "Service {"
        , "svcName = "    ++ BS.unpack svcName
        , "svcVersion = " ++ show svcVersion
        , "svcSigner = "  ++ show svcSigner
        , "}"
        ]

data RawRequest = RawRequest
    { rqService :: !Service
    , rqMethod  :: !Method
    , rqContent :: !ContentType
    , rqPath    :: ByteString
    , rqHeaders :: [(ByteString, ByteString)]
    , rqQuery   :: [(ByteString, ByteString)]
    , rqBody    :: Maybe ByteString
    }

instance Show RawRequest where
    show RawRequest{..} = unlines
        [ "rqService = " ++ show rqService
        , "rqMethod  = " ++ show rqMethod
        , "rqContent = " ++ show rqContent
        , "rqPath    = " ++ show rqPath
        , "rqHeaders = " ++ show rqHeaders
        , "rqQuery   = " ++ show rqQuery
        ]

instance ToJSON RawRequest where
    toJSON RawRequest{..} = object
        [ "rqMethod"  .= (String . Text.pack $ show rqMethod)
        , "rqContent" .= (String $ toText rqContent)
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
    RawRequest svc meth FormEncoded path [] (toQuery q) Nothing

xmlRequest :: IsXML a
           => Service
           -> Method
           -> ByteString
           -> a
           -> RawRequest
xmlRequest svc meth path =
    RawRequest svc meth XML path [] [] . Just . toXML

data SignedRequest = SignedRequest
    { srqUrl     :: !ByteString
    , srqPayload :: (Maybe ByteString)
    , srqRequest :: !Request
    } deriving (Show)

data ContentType
    = FormEncoded
    | XML

instance Show ContentType where
    show = BS.unpack . toBS

instance IsByteString ContentType where
    toBS FormEncoded = "application/x-www-form-urlencoded"
    toBS XML         = "application/xml"

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

readAssocList :: [(String, a)] -> ReadPrec a
readAssocList xs = choice $ map (\(x, y) -> lift $ string x >> return y) xs
