{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
import           Data.ByteString.Builder      (Builder)
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
import           Network.AWS.Data.Log
import           Network.AWS.Data.Query       (QueryString)
import           Network.AWS.Data.XML         (encodeXML)
import           Network.HTTP.Client
import           Text.XML                     (Element)

import           Prelude

default (Builder)

-- | A streaming, exception safe response body.
newtype RsBody = RsBody
    { _streamBody :: ResumableSource (ResourceT IO) ByteString
    }
-- newtype for show/orhpan instance purposes

instance Show RsBody where
    show = const "RsBody { ResumableSource (ResourceT IO) ByteString }"

-- | Specifies the transmitted size of the 'Transfer-Encoding' chunks.
--
-- /See:/ 'defaultChunk'.
newtype ChunkSize = ChunkSize Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance ToLog ChunkSize where
    build = build . show

-- | The default chunk size of 128 KB. The minimum chunk size accepted by
-- AWS is 8 KB, unless the entirety of the request is below this threshold.
--
-- A chunk size of 64 KB or higher is recommended for performance reasons.
defaultChunkSize :: ChunkSize
defaultChunkSize = 128 * 1024

-- | An opaque request body which will be transmitted via
-- @Transfer-Encoding: chunked@.
--
-- /Invariant:/ Only services that support chunked encoding can
-- accept a 'ChunkedBody'. This is enforced by the type signatures
-- emitted by the generator.
data ChunkedBody = ChunkedBody
    { _chunkedRequest  :: Source (ResourceT IO) ByteString -> RequestBody
    , _chunkedBody     :: Source (ResourceT IO) ByteString
    , _chunkedSize     :: !ChunkSize
    , _chunkedOriginal :: !Integer
    }

instance Show ChunkedBody where
    show c = BS8.unpack . toBS $ build
          "ChunkedBody { chunkSize = "
        <> build (_chunkedSize c)
        <> "<> originalLength = "
        <> build (_chunkedOriginal c)
        <> "<> fullChunks = "
        <> build (fullChunks c)
        <> "<> leftoverBytes = "
        <> build (leftoverBytes c)
        <> "}"

fuseChunks :: Conduit ByteString (ResourceT IO) ByteString
           -> ChunkedBody
           -> ChunkedBody
fuseChunks f c = c { _chunkedBody = _chunkedBody c =$= f }

fullChunks :: ChunkedBody -> Integer
fullChunks c = _chunkedOriginal c `div` fromIntegral (_chunkedSize c)

leftoverBytes :: ChunkedBody -> Maybe Integer
leftoverBytes c =
    case _chunkedOriginal c `mod` toInteger (_chunkedSize c) of
         0 -> Nothing
         n -> Just n

-- | An opaque request body containing a 'SHA256' hash.
data HashedBody = HashedBody
    { _hashedDigest :: Digest SHA256
    , _hashedBody   :: RequestBody
    }

instance Show HashedBody where
    show b@(HashedBody h _) = BS8.unpack . toBS $
           "HashedBody { sha256 = "
        <> build (digestToBase Base16 h)
        <> ", streaming = "
        <> build (isStreaming (Hashed b))
        <> " }"

instance IsString HashedBody where
    fromString = toHashedBody

sha256Base16 :: HashedBody -> ByteString
sha256Base16 = digestToBase Base16 . _hashedDigest

data RqBody
    = Chunked ChunkedBody
    | Hashed  HashedBody
      deriving (Show)

instance IsString RqBody where
    fromString = Hashed . fromString

isStreaming :: RqBody -> Bool
isStreaming = f . bodyRequest
  where
    f RequestBodyLBS           {} = False
    f RequestBodyBS            {} = False
    f RequestBodyBuilder       {} = False
    f RequestBodyStream        {} = True
    f RequestBodyStreamChunked {} = True

bodyRequest :: RqBody -> RequestBody
bodyRequest = \case
    Chunked x -> _chunkedRequest x (_chunkedBody x)
    Hashed  x -> _hashedBody  x

md5Base64 :: RqBody -> Maybe ByteString
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
    toBody :: a -> RqBody

    default toBody :: ToHashedBody a => a -> RqBody
    toBody = Hashed . toHashedBody

instance ToBody RqBody        where toBody = id
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

_Body :: ToBody a => AReview RqBody a
_Body = un (to toBody)
