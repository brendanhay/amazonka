-- |
-- Module      : Network.AWS.Sign.V2Header
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides an AWS compliant V2 Header request signer. It is based
-- heavily on boto (https://github.com/boto/boto), specifically boto's
-- @HmacAuthV1Handler@ AWS capable signer. AWS documentation is available
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html here>.
--
-- /Notice/: Limitations include an inability to sign with a security token and
-- inability to overwrite the @Date@ header with an expiry.
module Network.AWS.Sign.V2Header
  ( v2Header,
  )
where

import qualified Data.Dynamic as Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.AWS.Bytes as Bytes
import qualified Network.AWS.Crypt as Crypt
import Network.AWS.Data
import Network.AWS.Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Sign.V2Header.Base as V2
import Network.AWS.Types
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

data V2Header = V2Header
  { metaTime :: UTCTime,
    metaEndpoint :: Endpoint,
    metaSignature :: ByteString,
    metaHeaders :: Headers,
    metaSigner :: ByteString
  }

-- instance ToLog V2Header where
--   build V2Header {..} =
--     buildLines
--       [ "[Version 2 Header Metadata] {",
--         "  time      = " <> build metaTime,
--         "  endpoint  = " <> build (_endpointHost metaEndpoint),
--         "  signature = " <> build metaSignature,
--         "  headers = " <> build headers,
--         "  signer = " <> build signer,
--         "}"
--       ]

v2Header :: Signer
v2Header =
  Signer
    { runSigner = sign,
      runPresigner = const sign
    }

sign :: SigningAlgorithm request
sign Request {..} AuthEnv {..} region time =
  SignedRequest
    { signedMetadata = Dynamic.toDyn metadata,
      signedRequest = request
    }
  where
    Service {..} = requestService

    signer =
      V2.newSigner headers method requestPath requestQuery

    metadata =
      V2Header
        { metaTime = time,
          metaEndpoint = endpoint,
          metaSignature = signature',
          metaHeaders = headers,
          metaSigner = signer
        }

    request =
      (newClientRequest endpoint serviceTimeout)
        { Client.method = method,
          Client.path = Request.escapePath requestPath,
          Client.queryString = encodeQueryBuilder True requestQuery,
          Client.requestHeaders = Map.toList headers,
          Client.requestBody = toRequestBody requestBody
        }

    method = HTTP.renderStdMethod requestMethod

    endpoint = serviceEndpoint region

    headers =
      Map.insert HTTP.hDate date
        . Map.insert HTTP.hAuthorization authorization
        $ requestHeaders

    authorization = "AWS " <> toUTF8 authAccessKeyId <> ":" <> signature'

    signature' =
      Bytes.encodeBase64
        . Crypt.hmacSHA1 (Crypt.Key (toUTF8 authSecretAccessKey))
        $ signer

    date = toUTF8 (formatDateTime rfc822Format time)
