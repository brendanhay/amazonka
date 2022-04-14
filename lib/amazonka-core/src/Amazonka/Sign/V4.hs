-- |
-- Module      : Amazonka.Sign.V4
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Sign.V4
  ( Base.V4 (..),
    v4,
  )
where

import Amazonka.Bytes
import Amazonka.Data.Body
import Amazonka.Data.ByteString
import Amazonka.Data.Headers
import Amazonka.Data.Query
import Amazonka.Data.Time
import Amazonka.Lens ((%~), (<>~))
import Amazonka.Prelude
import Amazonka.Request
import qualified Amazonka.Sign.V4.Base as Base
import qualified Amazonka.Sign.V4.Chunked as Chunked
import Amazonka.Types
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

v4 :: Signer
v4 = Signer sign presign

-- |
-- Presigns a URL according to the AWS Request Signature V4 spec <https://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html>.
-- In the case that the URL contains a payload that is not signed when sending requests to Amazon S3, a literal `UNSIGNED-PAYLOAD`
-- must be included when constructing the cannonical request. See <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html>
-- In the edge case that the request body is a @Amazonka.Data.Body.ChunkedBody@ we will also use the `UNSIGNED-PAYLOAD` literal as we won't consume the stream
-- to hash it.
presign :: Seconds -> Algorithm a
presign ex rq a r ts = Base.signRequest meta mempty auth
  where
    auth = clientRequestQuery <>~ ("&X-Amz-Signature=" <> toBS (Base.metaSignature meta))

    meta = Base.signMetadata a r ts presigner digest (prepare rq)

    presigner c shs =
      pair (CI.original hAMZAlgorithm) Base.algorithm
        . pair (CI.original hAMZCredential) (toBS c)
        . pair (CI.original hAMZDate) (Time ts :: AWSTime)
        . pair (CI.original hAMZExpires) ex
        . pair (CI.original hAMZSignedHeaders) (toBS shs)
        . pair (CI.original hAMZToken) (toBS <$> _authSessionToken a)

    digest =
      case _requestBody rq of
        Chunked _ -> unsignedPayload
        Hashed (HashedStream h _ _) -> Base.Tag $ encodeBase16 h
        Hashed (HashedBytes h b)
          | BS.null b && _serviceSigningName (_requestService rq) == "s3" -> unsignedPayload
          | otherwise -> Base.Tag $ encodeBase16 h

    unsignedPayload = Base.Tag "UNSIGNED-PAYLOAD"

    prepare = requestHeaders %~ (hdr hHost host)

    host =
      case (_endpointSecure end, _endpointPort end) of
        (False, 80) -> _endpointHost end
        (True, 443) -> _endpointHost end
        (_, port) -> _endpointHost end <> ":" <> toBS port

    end = _serviceEndpoint (_requestService rq) r

sign :: Algorithm a
sign rq a r ts =
  case _requestBody rq of
    Chunked x -> Chunked.chunked x rq a r ts
    Hashed x -> hashed x rq a r ts

hashed :: HashedBody -> Algorithm a
hashed x rq a r ts =
  let (meta, auth) = Base.base (Base.Tag (sha256Base16 x)) rq a r ts
   in Base.signRequest meta (toRequestBody (Hashed x)) auth
