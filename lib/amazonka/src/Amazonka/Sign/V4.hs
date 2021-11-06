-- |
-- Module      : Amazonka.Sign.V4
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Sign.V4
  ( V4 (..),
    v4,
  )
where

import Amazonka.Data.Body
import Amazonka.Data.ByteString
import Amazonka.Data.Headers
import Amazonka.Data.Query
import Amazonka.Data.Time
import Amazonka.Lens ((%~), (<>~))
import Amazonka.Prelude
import Amazonka.Request
import Amazonka.Sign.V4.Base
import Amazonka.Sign.V4.Chunked
import Amazonka.Types
import qualified Data.CaseInsensitive as CI

v4 :: Signer
v4 = Signer sign presign

presign :: Seconds -> Algorithm a
presign ex rq a r ts = signRequest meta mempty auth
  where
    auth = clientRequestQuery <>~ ("&X-Amz-Signature=" <> toBS (metaSignature meta))

    meta = signMetadata a r ts presigner digest (prepare rq)

    presigner c shs =
      pair (CI.original hAMZAlgorithm) algorithm
        . pair (CI.original hAMZCredential) (toBS c)
        . pair (CI.original hAMZDate) (Time ts :: AWSTime)
        . pair (CI.original hAMZExpires) ex
        . pair (CI.original hAMZSignedHeaders) (toBS shs)
        . pair (CI.original hAMZToken) (toBS <$> _authSessionToken a)

    digest = Tag "UNSIGNED-PAYLOAD"

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
    Chunked x -> chunked x rq a r ts
    Hashed x -> hashed x rq a r ts

hashed :: HashedBody -> Algorithm a
hashed x rq a r ts =
  let (meta, auth) = base (Tag (sha256Base16 x)) rq a r ts
   in signRequest meta (toRequestBody (Hashed x)) auth
