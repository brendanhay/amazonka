{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}

-- |
-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Body where

import           Control.Lens
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Conduit
import           Data.Monoid
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
newtype Stream = Stream
    { streamBody :: ResumableSource (ResourceT IO) ByteString
    }
-- newtype for show/orhpan instance purposes

instance Show Stream where
    show = const "Stream { ResumableSource (ResourceT IO) ByteString }"

data ChunkedBody = ChunkedBody
    { _chunkedBody    :: Source IO ByteString
    , _chunkedRequest :: Source IO ByteString -> RequestBody
    , _chunkedTotal   :: !Integer
    }

-- FIXME: What about chunk size?

instance Show ChunkedBody where
    show = const "ChunkedBody"

-- | An opaque request body containing a 'SHA256' hash.
data HashedBody = HashedBody
    { _hashedDigest :: Digest SHA256
    , _hashedBody   :: RequestBody
    }

instance Show HashedBody where
    show (HashedBody h _) = BS8.unpack $
        "HashedBody { SHA256 = " <> digestToBase Base16 h <> " }"

instance IsString HashedBody where
    fromString = toHashedBody

sha256Base16 :: HashedBody -> ByteString
sha256Base16 = digestToBase Base16 . _hashedDigest

-- Invariant: only services that support chunked encoding can create/specify
-- a Chunked body.
data Body
    = Chunked ChunkedBody
    | Hashed  HashedBody
      deriving (Show)

instance IsString Body where
    fromString = Hashed . fromString

isStreaming :: Body -> Bool
isStreaming = f . bodyRequest
  where
    f RequestBodyLBS           {} = False
    f RequestBodyBS            {} = False
    f RequestBodyBuilder       {} = False
    f RequestBodyStream        {} = True
    f RequestBodyStreamChunked {} = True

bodyRequest :: Body -> RequestBody
bodyRequest = \case
    Chunked x -> _chunkedRequest x (_chunkedBody x)
    Hashed  x -> _hashedBody  x

md5Base64 :: Body -> Maybe ByteString
md5Base64 Chunked {} = Nothing
md5Base64 (Hashed x) =
    let md5 = Just . digestToBase Base64 . hashMD5
     in case _hashedBody x of
        RequestBodyLBS       lbs -> md5 (toBS lbs)
        RequestBodyBS        bs  -> md5 bs
        RequestBodyBuilder _ b   -> md5 (toBS b)
        _                        -> Nothing

-- | Anything that can be safely converted to a 'HashedBody'.
class ToHashedBody a where
    -- | Convert a value to a hashed request body.
    toHashedBody :: a -> HashedBody

instance ToHashedBody HashedBody where
    toHashedBody = id

instance ToHashedBody String where
    toHashedBody = toHashedBody . LBS8.pack

instance ToHashedBody LBS.ByteString where
    toHashedBody x = HashedBody (hashlazy x) (RequestBodyLBS x)

instance ToHashedBody ByteString where
    toHashedBody x = HashedBody (hash x) (RequestBodyBS x)

instance ToHashedBody Text where
    toHashedBody = toHashedBody . Text.encodeUtf8

instance ToHashedBody LText.Text where
    toHashedBody = toHashedBody . LText.encodeUtf8

instance ToHashedBody Value where
    toHashedBody = toHashedBody . encode

instance ToHashedBody Element where
    toHashedBody = toHashedBody . encodeXML

instance ToHashedBody QueryString where
    toHashedBody = toHashedBody . toBS

-- | Anything that can be converted to a streaming request 'Body'.
--
-- Invariant: only services that support chunked signing expose
-- 'ToBody', otherwise use 'ToHashedBody'.
class ToBody a where
    -- | Convert a value to a request body.
    toBody :: a -> Body

    default toBody :: ToHashedBody a => a -> Body
    toBody = Hashed . toHashedBody

instance ToBody HashedBody  where toBody = Hashed
instance ToBody ChunkedBody where toBody = Chunked

instance ToBody String
instance ToBody LBS.ByteString
instance ToBody ByteString
instance ToBody Text
instance ToBody LText.Text
instance ToBody Value
instance ToBody Element
instance ToBody QueryString

_Body :: ToBody a => AReview Body a
_Body = un (to toBody)
