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

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.AWS.Hash as Hash
import qualified Network.AWS.Sign.V2Header.Base as V2
import Network.AWS.Types
import Network.AWS.Data 
import qualified Network.HTTP.Client as Client
import Network.AWS.Prelude
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
v2Header = Signer sign (const sign)

sign :: Algorithm a
sign Request {..} AuthEnv {..} region time =
  Signed meta request
  where
    meta = Meta (V2Header time endpoint signature' headers signer)

    signer = V2.newSigner headers method _rqPath _rqQuery

    request =
      (clientRequest endpoint _svcTimeout)
        { Client.method = method,
          Client.path = _rqPath,
          Client.queryString = encodeQuery True _rqQuery,
          Client.requestHeaders = Map.toList headers,
          Client.requestBody = toRequestBody _rqBody
        }

    method = HTTP.renderStdMethod _rqMethod

    endpoint = _svcEndpoint region

    Service {..} = _rqService


    headers =
        Map.insert HTTP.hDate date
      . Map.insert HTTP.hAuthorization authorization
      $ _rqHeaders

    authorization =
      "AWS "
         <> Text.Encoding.encodeUtf8 (fromAccessKey _authAccess)
         <> ":"
          <> signature'

    signature' =
      Hash.digestToBase Hash.Base64
        . Hash.hmacSHA1 (Text.Encoding.encodeUtf8 (fromSecretKey _authSecret))
        $ signer

    date =
       Text.Encoding.encodeUtf8 (formatDateTime rfc822Format time)
