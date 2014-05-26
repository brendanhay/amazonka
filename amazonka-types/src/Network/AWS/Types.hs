{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types where

import           Control.Applicative
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Attoparsec.Text         as AText
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.CaseInsensitive         (CI)
import           Data.Char
import           Data.Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time
import           Network.AWS.Data
import           Network.HTTP.Client          (RequestBody, Response)
import           Network.HTTP.Types.Method

class AWSService a where
    type Sg a :: *
    data Er a :: *

    service   :: Service a (Sg a)

class AWSRequest a where
    type Sv a :: *
    type Rs a :: *

    request   :: a -> Request (Sv a)
    response  :: MonadResource m
              => Response (ResumableSource m ByteString)
              -> m (Either (Er (Sv a)) (Rs a))

class AWSPager a where
    next :: AWSRequest a => a -> Rs a -> Maybe a

data Auth = Auth
    { authAccess :: !Text
    , authSecret :: !Text
    , authToken  :: Maybe Text
    , authExpiry :: Maybe UTCTime
    }

instance FromJSON Auth where
    parseJSON = withObject "Auth" $ \o -> Auth
        <$> o .:  "AccessKeyId"
        <*> o .:  "SecretAccessKey"
        <*> o .:? "Token"
        <*> o .:? "Expiration"

newtype AuthRef = AuthRef { authRef :: IORef Auth }

data Endpoint
    = Global
    | Regional
    | Custom !ByteString

data Service a s = Service
    { svcEndpoint :: !Endpoint
    , svcName     :: !ByteString
    , svcVersion  :: !ByteString
    , svcTarget   :: Maybe ByteString
    }

endpoint :: Service a s -> Region -> ByteString
endpoint Service{..} reg =
    let suf = ".amazonaws.com"
     in case svcEndpoint of
            Global   -> svcName <> suf
            Regional -> svcName <> "." <> toByteString reg <> suf
            Custom x -> x

data Request a = Request
    { rqMethod  :: !StdMethod
    , rqPath    :: !Text
    , rqQuery   :: Query
    , rqHeaders :: [(CI Text, Text)]
    , rqBody    :: RequestBody
    }

instance Show (Request a) where
    show Request{..} = unlines
        [ "Request:"
        , "rqMethod  = " ++ show rqMethod
        , "rqPath    = " ++ show rqPath
        , "rqQuery   = " ++ show rqQuery
        , "rqHeaders = " ++ show rqHeaders
        ]

data Region
    = Ireland         -- ^ Europe / eu-west-1
    | Tokyo           -- ^ Asia Pacific / ap-northeast-1
    | Singapore       -- ^ Asia Pacific / ap-southeast-1
    | Sydney          -- ^ Asia Pacific / ap-southeast-2
    | Beijing         -- ^ China / cn-north-1
    | NorthVirginia   -- ^ US / us-east-1
    | NorthCalifornia -- ^ US / us-west-1
    | Oregon          -- ^ US / us-west-2
    | GovCloud        -- ^ AWS GovCloud / us-gov-west-1
    | GovCloudFIPS    -- ^ AWS GovCloud (FIPS 140-2) S3 Only / fips-us-gov-west-1
    | SaoPaulo        -- ^ South America / sa-east-1
      deriving (Eq, Ord)

instance Default Region where
    def = NorthVirginia

instance Read Region where
    readsPrec = const readText

instance Show Region where
    show = showText

instance FromText Region where
    parser = AText.choice $ map (\(x, y) -> AText.string x >> return y)
        [ ("eu-west-1",          Ireland)
        , ("ap-northeast-1",     Tokyo)
        , ("ap-southeast-1",     Singapore)
        , ("ap-southeast-2",     Sydney)
        , ("cn-north-1",         Beijing)
        , ("us-east-1",          NorthVirginia)
        , ("us-west-2",          NorthCalifornia)
        , ("us-west-1",          Oregon)
        , ("us-gov-west-1",      GovCloud)
        , ("fips-us-gov-west-1", GovCloudFIPS)
        , ("sa-east-1",          SaoPaulo)
        ]

instance ToText Region where
    toText r = case r of
        Ireland         -> "eu-west-1"
        Tokyo           -> "ap-northeast-1"
        Singapore       -> "ap-southeast-1"
        Sydney          -> "ap-southeast-2"
        Beijing         -> "cn-north-1"
        NorthVirginia   -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        GovCloud        -> "us-gov-west-1"
        GovCloudFIPS    -> "fips-us-gov-west-1"
        SaoPaulo        -> "sa-east-1"

instance ToByteString Region where
    toByteString = toByteString . toText

data AZ = AZ
    { azRegion :: !Region
    , azSuffix :: !Char
    } deriving (Eq, Ord)

instance Read AZ where
    readsPrec = const readText

instance Show AZ where
    show = showText

instance FromText AZ where
    parser = AZ <$> parser <*> AText.satisfy isAlpha <* AText.endOfInput

instance ToText AZ where
    toText AZ{..} = toText azRegion `Text.snoc` azSuffix

newtype Action = Action Text
    deriving (Eq, Show)

instance IsString Action where
    fromString = Action . Text.pack

instance ToQuery Action where
    toQuery (Action a) = toQuery ("Action" :: Text, a)
