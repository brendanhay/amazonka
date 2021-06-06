-- |
-- Module      : Network.AWS.Internal.Sign.V4.Chunked
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Internal.Sign.V4.Chunked
  ( chunked,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import qualified Network.AWS.Internal.Bytes as Bytes
import qualified Network.AWS.Internal.Crypto as Crypto
import Network.AWS.Data
import Network.AWS.Internal.Lens ((<>~), (^.))
import Network.AWS.Internal.Prelude
import Network.AWS.Internal.Sign.V4.Base hiding (algorithm)
import Network.AWS.Types
import qualified Network.HTTP.Types as HTTP
import qualified Numeric

chunked :: ChunkedBody -> Algorithm a
chunked c rq a r ts = signRequest meta (toRequestBody body) auth
  where
    (meta, auth) = base (Tag digest) (prepare rq) a r ts

    prepare =
      requestHeaders
        <>~ [ (HTTP.hContentEncoding, "aws-chunked"),
              (hAMZDecodedContentLength, toBS (_chunkedLength c)),
              (HTTP.hContentLength, toBS (metadataLength c))
            ]

    body = Chunked (c `fuseChunks` sign (metaSignature meta))

    sign :: Monad m => Signature -> ConduitM ByteString ByteString m ()
    sign prev = do
      mx <- Conduit.await

      let next = chunkSignature prev (fromMaybe mempty mx)

      case mx of
        Nothing -> Conduit.yield (chunkData next mempty)
        Just x -> Conduit.yield (chunkData next x) >> sign next

    chunkData next x =
      toBS $
        Build.word64Hex (fromIntegral (BS.length x))
          <> Build.byteString chunkSignatureHeader
          <> Build.byteString (toBS next)
          <> Build.byteString crlf
          <> Build.byteString x
          <> Build.byteString crlf

    chunkSignature prev x =
      signature (_authSecretAccessKey a ^. _Sensitive) scope (chunkStringToSign prev x)

    chunkStringToSign prev x =
      Tag $
        BS8.intercalate
          "\n"
          [ algorithm,
            time,
            toBS scope,
            toBS prev,
            sha256Empty,
            sha256 x
          ]

    time :: ByteString
    time = toBS (Time ts :: AWSTime)

    scope :: CredentialScope
    scope = credentialScope (_requestService rq) end ts

    end :: Endpoint
    end = _serviceEndpoint (_requestService rq) r

metadataLength :: ChunkedBody -> Integer
metadataLength c =
  -- Number of full sized chunks.
  fullChunks c * chunkLength (_chunkedSize c)
    -- Non-full chunk preceeding the final chunk.
    + maybe 0 chunkLength (remainderBytes c)
    -- The final empty chunk.
    + chunkLength (0 :: Integer)
  where
    chunkLength :: Integral a => a -> Integer
    chunkLength (toInteger -> n) =
      fromIntegral (length (Numeric.showHex n ""))
        + headerLength
        + signatureLength
        + crlfLength
        + n
        + crlfLength

    headerLength = toInteger (BS.length chunkSignatureHeader)
    crlfLength = toInteger (BS.length crlf)
    signatureLength = 64

sha256 :: ByteString -> ByteString
sha256 = Bytes.encodeBase16 . Crypto.hashSHA256

sha256Empty :: ByteString
sha256Empty = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256-PAYLOAD"

digest :: ByteString
digest = "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"

chunkSignatureHeader :: ByteString
chunkSignatureHeader = ";chunk-signature="

crlf :: ByteString
crlf = "\r\n"
