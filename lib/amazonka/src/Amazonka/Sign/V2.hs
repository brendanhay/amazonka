-- |
-- Module      : Amazonka.Sign.V2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Sign.V2
  ( v2,
  )
where

import qualified Amazonka.Bytes as Bytes
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.URI as URI

data V2 = V2
  { metaTime :: UTCTime,
    metaEndpoint :: Endpoint,
    metaSignature :: ByteString
  }

instance ToLog V2 where
  build V2 {..} =
    buildLines
      [ "[Version 2 Metadata] {",
        "  time      = " <> build metaTime,
        "  endpoint  = " <> build (_endpointHost metaEndpoint),
        "  signature = " <> build metaSignature,
        "}"
      ]

v2 :: Signer
v2 = Signer sign (const sign) -- FIXME: revisit v2 presigning.

sign :: Algorithm a
sign Request {..} AuthEnv {..} r t = Signed meta rq
  where
    meta = Meta (V2 t end signature)

    rq =
      (newClientRequest end _serviceTimeout)
        { Client.method = meth,
          Client.path = path',
          Client.queryString = toBS authorised,
          Client.requestHeaders = headers,
          Client.requestBody = toRequestBody _requestBody
        }

    meth = toBS _requestMethod
    path' = toBS (escapePath _requestPath)

    end@Endpoint {..} = _serviceEndpoint r

    Service {..} = _requestService

    authorised = pair "Signature" (URI.urlEncode True signature) query

    signature =
      Bytes.encodeBase64
        . Crypto.hmacSHA256 (toBS _authSecretAccessKey)
        $ BS8.intercalate
          "\n"
          [ meth,
            _endpointHost,
            path',
            toBS query
          ]

    query =
      pair "Version" _serviceVersion
        . pair "SignatureVersion" ("2" :: ByteString)
        . pair "SignatureMethod" ("HmacSHA256" :: ByteString)
        . pair "Timestamp" time
        . pair "AWSAccessKeyId" (toBS _authAccessKeyId)
        $ _requestQuery <> maybe mempty toQuery token

    token = ("SecurityToken" :: ByteString,) . toBS <$> _authSessionToken

    headers = hdr HTTP.hDate time _requestHeaders

    time = toBS (Time t :: ISO8601)
