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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Body where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Conduit
import           Data.HashMap.Strict          (HashMap)
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
import           Network.AWS.Lens             (AReview, Lens', lens, to, un)
import           Network.HTTP.Conduit
import           Text.XML                     (Element)

default (Builder)

-- | A streaming, exception safe response body.
newtype RsBody = RsBody
    { _streamBody :: ConduitM () ByteString (ResourceT IO) ()
    } -- newtype for show/orhpan instance purposes.

instance Show RsBody where
    show = const "RsBody { ConduitM () ByteString (ResourceT IO) () }"

fuseStream :: RsBody
           -> ConduitM ByteString ByteString (ResourceT IO) ()
           -> RsBody
fuseStream b f = b { _streamBody = _streamBody b .| f }

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
-- accept a 'ChunkedBody'. (Currently S3.) This is enforced by the type
-- signatures emitted by the generator.
data ChunkedBody = ChunkedBody
    { _chunkedSize   :: !ChunkSize
    , _chunkedLength :: !Integer
    , _chunkedBody   :: ConduitM () ByteString (ResourceT IO) ()
    }

chunkedLength :: Lens' ChunkedBody Integer
chunkedLength = lens _chunkedLength (\s a -> s { _chunkedLength = a })

-- Maybe revert to using Source's, and then enforce the chunk size
-- during conversion from HashedBody -> ChunkedBody

instance Show ChunkedBody where
    show c = BS8.unpack . toBS $ build
          "ChunkedBody { chunkSize = "
        <> build (_chunkedSize c)
        <> "<> originalLength = "
        <> build (_chunkedLength c)
        <> "<> fullChunks = "
        <> build (fullChunks c)
        <> "<> remainderBytes = "
        <> build (remainderBytes c)
        <> "}"

fuseChunks :: ChunkedBody
           -> ConduitM ByteString ByteString (ResourceT IO) ()
           -> ChunkedBody
fuseChunks c f = c { _chunkedBody = _chunkedBody c .| f }

fullChunks :: ChunkedBody -> Integer
fullChunks c = _chunkedLength c `div` fromIntegral (_chunkedSize c)

remainderBytes :: ChunkedBody -> Maybe Integer
remainderBytes c =
    case _chunkedLength c `mod` toInteger (_chunkedSize c) of
         0 -> Nothing
         n -> Just n

-- | An opaque request body containing a 'SHA256' hash.
data HashedBody
    = HashedStream (Digest SHA256) !Integer (ConduitM () ByteString (ResourceT IO) ())
    | HashedBytes  (Digest SHA256) ByteString

instance Show HashedBody where
    show = \case
        HashedStream h n _ -> str "HashedStream" h n
        HashedBytes  h x   -> str "HashedBody"   h (BS.length x)
      where
        str c h n = BS8.unpack . toBS $
            c <> " { sha256 = "
              <> build (digestToBase Base16 h)
              <> ", length = "
              <> build n

instance IsString HashedBody where
    fromString = toHashed

sha256Base16 :: HashedBody -> ByteString
sha256Base16 = digestToBase Base16 . \case
    HashedStream h _ _ -> h
    HashedBytes  h _   -> h

-- | Invariant: only services that support _both_ standard and
-- chunked signing expose 'RqBody' as a parameter.
data RqBody
    = Chunked ChunkedBody
    | Hashed  HashedBody
      deriving (Show)

instance IsString RqBody where
    fromString = Hashed . fromString

md5Base64 :: RqBody -> Maybe ByteString
md5Base64 = \case
    Hashed (HashedBytes _ x) -> Just . digestToBase Base64 $ hashMD5 x
    _                        -> Nothing

isStreaming :: RqBody -> Bool
isStreaming = \case
    Hashed (HashedStream {}) -> True
    _                        -> False

toRequestBody :: RqBody -> RequestBody
toRequestBody = \case
    Chunked x -> requestBodySourceChunked (_chunkedBody x)
    Hashed  x -> case x of
         HashedStream _ n f -> requestBodySource (fromIntegral n) f
         HashedBytes  _ b   -> RequestBodyBS b

contentLength :: RqBody -> Integer
contentLength = \case
    Chunked x -> _chunkedLength x
    Hashed  x -> case x of
        HashedStream _ n _ -> n
        HashedBytes  _ b   -> fromIntegral (BS.length b)

-- | Anything that can be safely converted to a 'HashedBody'.
class ToHashedBody a where
    -- | Convert a value to a hashed request body.
    toHashed :: a -> HashedBody

instance ToHashedBody ByteString where
    toHashed x = HashedBytes (hash x) x

instance ToHashedBody HashedBody     where toHashed = id
instance ToHashedBody String         where toHashed = toHashed . LBS8.pack
instance ToHashedBody LBS.ByteString where toHashed = toHashed . toBS
instance ToHashedBody Text           where toHashed = toHashed . Text.encodeUtf8
instance ToHashedBody LText.Text     where toHashed = toHashed . LText.encodeUtf8
instance ToHashedBody Value          where toHashed = toHashed . encode
instance ToHashedBody Element        where toHashed = toHashed . encodeXML
instance ToHashedBody QueryString    where toHashed = toHashed . toBS

instance ToHashedBody (HashMap Text Value) where
    toHashed = toHashed . Object

-- | Anything that can be converted to a streaming request 'Body'.
class ToBody a where
    -- | Convert a value to a request body.
    toBody :: a -> RqBody

    default toBody :: ToHashedBody a => a -> RqBody
    toBody = Hashed . toHashed

instance ToBody RqBody      where toBody = id
instance ToBody HashedBody  where toBody = Hashed
instance ToBody ChunkedBody where toBody = Chunked

instance ToHashedBody a => ToBody (Maybe a) where
    toBody = Hashed . maybe (toHashed BS.empty) toHashed

instance ToBody String
instance ToBody LBS.ByteString
instance ToBody ByteString
instance ToBody Text
instance ToBody LText.Text
instance ToBody (HashMap Text Value)
instance ToBody Value
instance ToBody Element
instance ToBody QueryString

_Body :: ToBody a => AReview RqBody a
_Body = un (to toBody)
