{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}

-- |
-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Body where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Conduit
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Query       (QueryString)
import           Network.AWS.Data.XML         (encodeXML)
import           Network.HTTP.Client
import           Text.XML                     (Element)

import           Prelude

-- | A streaming, exception safe response body.
newtype RsBody = RsBody
    { bodyResponse :: ResumableSource (ResourceT IO) ByteString
    }

instance Show RsBody where
    show = const "RsBody { ResumableSource (ResourceT IO) ByteString }"

-- | An opaque request body containing a 'SHA256' hash.
data RqBody = RqBody
    { bodySHA256  :: Digest SHA256
    , bodyRequest :: RequestBody
    }

instance Show RqBody where
    show b = "RqBody { RequestBody " ++ BS8.unpack (toBS (bodySHA256 b)) ++ " }"

instance IsString RqBody where
    fromString = toBody . LBS8.pack

bodyStream :: RqBody -> Bool
bodyStream x =
    case bodyRequest x of
        RequestBodyLBS           {} -> False
        RequestBodyBS            {} -> False
        RequestBodyBuilder       {} -> False
        RequestBodyStream        {} -> True
        RequestBodyStreamChunked {} -> True

bodyCalculateMD5 :: RqBody -> Maybe (Digest MD5)
bodyCalculateMD5 x =
    let md5 = Just . hashMD5
     in case bodyRequest x of
        RequestBodyLBS           lbs -> md5 (toBS lbs)
        RequestBodyBS            bs  -> md5 bs
        RequestBodyBuilder     _ b   -> md5 (toBS b)
        _                            -> Nothing

-- | Anything that can be safely converted to a 'RqBody'.
class ToBody a where
    -- | Convert a value to a request body.
    toBody :: a -> RqBody

instance ToBody RqBody where
    toBody = id

instance ToBody LBS.ByteString where
    toBody x = RqBody (hashlazy x) (RequestBodyLBS x)

instance ToBody ByteString where
    toBody x = RqBody (hash x) (RequestBodyBS x)

instance ToBody Text where
    toBody = toBody . Text.encodeUtf8

instance ToBody LText.Text where
    toBody = toBody . LText.encodeUtf8

instance ToBody Value where
    toBody = toBody . encode

instance ToBody Element where
    toBody = toBody . encodeXML

instance ToBody QueryString where
    toBody = toBody . toBS
