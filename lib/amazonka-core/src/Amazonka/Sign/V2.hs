-- |
-- Module      : Amazonka.Sign.V2
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import Amazonka.Prelude hiding (error)
import Amazonka.Types hiding (presign, sign)
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
        "  endpoint  = " <> build (host metaEndpoint),
        "  signature = " <> build metaSignature,
        "}"
      ]

v2 :: Signer
v2 = Signer sign (const sign) -- FIXME: revisit v2 presigning.

sign :: Algorithm a
sign Request {service = Service {..}, ..} AuthEnv {..} r t = Signed meta rq
  where
    meta = Meta (V2 t end signature)

    rq =
      (newClientRequest end timeout)
        { Client.method = meth,
          Client.path = path',
          Client.queryString = toBS authorised,
          Client.requestHeaders = headers',
          Client.requestBody = toRequestBody body
        }

    meth = toBS method
    path' = toBS (escapePath $ basePath <> path)

    end@Endpoint {..} = endpoint r

    authorised = pair "Signature" (URI.urlEncode True signature) query

    signature =
      Bytes.encodeBase64
        . Crypto.hmacSHA256 (toBS secretAccessKey)
        $ BS8.intercalate
          "\n"
          [ meth,
            host,
            path',
            toBS query'
          ]

    query' =
      pair "Version" version
        . pair "SignatureVersion" ("2" :: ByteString)
        . pair "SignatureMethod" ("HmacSHA256" :: ByteString)
        . pair "Timestamp" time
        . pair "AWSAccessKeyId" (toBS accessKeyId)
        $ query <> maybe mempty toQuery token

    token = ("SecurityToken" :: ByteString,) . toBS <$> sessionToken

    headers' = hdr HTTP.hDate time headers

    time = toBS (Time t :: ISO8601)
