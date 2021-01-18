-- |
-- Module      : Network.AWS.Sign.V2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Sign.V2
  ( v2,
  )
where

import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Dynamic as Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text.Encoding
import Network.AWS.Data
import qualified Network.AWS.Hash as Hash
import Network.AWS.Prelude
import qualified Network.AWS.Sign.V2Header.Base as V2
import Network.AWS.Types
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

data V2 = V2
  { metaTime :: UTCTime,
    metaEndpoint :: Endpoint,
    metaSignature :: ByteString
  }

-- instance ToLog V2 where
--   build V2 {..} =
--     buildLines
--       [ "[Version 2 Metadata] {",
--         "  time      = " <> build metaTime,
--         "  endpoint  = " <> build (_endpointHost metaEndpoint),
--         "  signature = " <> build metaSignature,
--         "}"
--       ]

v2 :: Signer
v2 =
  Signer
    { runSigner = sign,
      runPresigner = const sign -- FIXME: revisit v2 presigning.
    }

sign :: SigningAlgorithm request
sign Request {..} AuthEnv {..} region time =
  SignedRequest
    { signedMetadata = Dynamic.toDyn metadata,
      signedRequest = request
    }
  where
    Service {..} = requestService

    metadata =
      V2
        { metaTime = time,
          metaEndpoint = endpoint,
          metaSignature = signature'
        }

    request =
      (newClientRequest endpoint serviceTimeout)
        { Client.method = method,
          Client.path = requestPath,
          Client.queryString = encodeQueryBuilder True authorizedQuery,
          Client.requestHeaders = Map.toList headers,
          Client.requestBody = toRequestBody requestBody
        }

    method = HTTP.renderStdMethod requestMethod

    endpoint@Endpoint {endpointHost} =
      serviceEndpoint region

    authorizedQuery =
      toQueryPair "Signature" signature'
        <> canonicalQuery

    signature' =
      Hash.digestToBase Hash.Base64
        . Hash.hmacSHA256 (toUTF8 authSecretAccessKey)
        $ ByteString.Char8.intercalate
          "\n"
          [ method,
            endpointHost,
            requestPath,
            encodeQueryBuilder True canonicalQuery
          ]

    canonicalQuery =
      toQueryPair "Version" serviceVersion
        <> toQueryPair "SignatureVersion" ("2" :: ByteString)
        <> toQueryPair "SignatureMethod" ("HmacSHA256" :: ByteString)
        <> toQueryPair "Timestamp" time
        <> toQueryPair "AWSAccessKeyId" (toUTF8 authAccessKeyId)
        <> requestQuery
        <> maybe mempty (toQueryPair "SecurityToken" . toUTF8) authSessionToken

    headers = Map.insert HTTP.hDate date requestHeaders

    date = toUTF8 (formatDateTime iso8601Format time)
