-- |
-- Module      : Amazonka.Sign.V2Header
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module provides an AWS compliant V2 Header request signer. It is based
-- heavily on <https://github.com/boto/boto boto>, specifically boto's
-- @HmacAuthV1Handler@ AWS capable signer. AWS documentation is available
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html here>.
--
-- Notice: Limitations include an inability to sign with a security token and
-- inability to overwrite the @Date@ header with an expiry.
module Amazonka.Sign.V2Header
  ( v2Header,
  )
where

import qualified Amazonka.Bytes as Bytes
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data
import Amazonka.Prelude
import qualified Amazonka.Sign.V2Header.Base as V2
import Amazonka.Types
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

data V2Header = V2Header
  { metaTime :: UTCTime,
    metaEndpoint :: Endpoint,
    metaSignature :: ByteString,
    headers :: HTTP.RequestHeaders,
    signer :: ByteString
  }

instance ToLog V2Header where
  build V2Header {..} =
    buildLines
      [ "[Version 2 Header Metadata] {",
        "  time      = " <> build metaTime,
        "  endpoint  = " <> build (_endpointHost metaEndpoint),
        "  signature = " <> build metaSignature,
        "  headers = " <> build headers,
        "  signer = " <> build signer,
        "}"
      ]

v2Header :: Signer
v2Header = Signer sign (const sign)

sign :: Algorithm a
sign Request {..} AuthEnv {..} r t = Signed meta rq
  where
    meta = Meta (V2Header t end signature headers signer)

    signer = V2.newSigner headers meth path' _requestQuery

    rq =
      (newClientRequest end _serviceTimeout)
        { Client.method = meth,
          Client.path = path',
          Client.queryString = toBS _requestQuery,
          Client.requestHeaders = headers,
          Client.requestBody = toRequestBody _requestBody
        }

    meth = toBS _requestMethod
    path' = toBS (escapePath _requestPath)

    end@Endpoint {} = _serviceEndpoint r

    Service {..} = _requestService

    signature =
      Bytes.encodeBase64
        . Crypto.hmacSHA1 (toBS _authSecretAccessKey)
        $ signer

    headers =
      hdr HTTP.hDate time
        . hdr HTTP.hAuthorization ("AWS " <> toBS _authAccessKeyId <> ":" <> signature)
        $ _requestHeaders

    time = toBS (Time t :: RFC822)
