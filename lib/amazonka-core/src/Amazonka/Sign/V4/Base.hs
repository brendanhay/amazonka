-- |
-- Module      : Amazonka.Sign.V4.Base
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Sign.V4.Base where

import qualified Amazonka.Bytes as Bytes
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data hiding (Path)
import Amazonka.Lens ((%~), (<>~), (^.))
import Amazonka.Prelude
import Amazonka.Request
import Amazonka.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

data V4 = V4
  { metaTime :: UTCTime,
    metaMethod :: Method,
    metaPath :: Path,
    metaEndpoint :: Endpoint,
    metaCredential :: Credential,
    metaCanonicalQuery :: CanonicalQuery,
    metaCanonicalRequest :: CanonicalRequest,
    metaCanonicalHeaders :: CanonicalHeaders,
    metaSignedHeaders :: SignedHeaders,
    metaStringToSign :: StringToSign,
    metaSignature :: Signature,
    metaHeaders :: [Header],
    metaTimeout :: Maybe Seconds
  }

instance ToLog V4 where
  build V4 {..} =
    buildLines
      [ "[Version 4 Metadata] {",
        "  time              = " <> build metaTime,
        "  endpoint          = " <> build (_endpointHost metaEndpoint),
        "  credential        = " <> build metaCredential,
        "  signed headers    = " <> build metaSignedHeaders,
        "  signature         = " <> build metaSignature,
        "  string to sign    = {",
        build metaStringToSign,
        "}",
        "  canonical request = {",
        build metaCanonicalRequest,
        "  }",
        "}"
      ]

base ::
  Hash ->
  Request a ->
  AuthEnv ->
  Region ->
  UTCTime ->
  (V4, ClientRequest -> ClientRequest)
base h rq a r ts = (meta, auth)
  where
    auth = clientRequestHeaders <>~ [(HTTP.hAuthorization, authorisation meta)]

    meta = signMetadata a r ts presigner h (prepare rq)

    presigner _ _ = id

    prepare =
      requestHeaders
        %~ ( hdr hHost host
               . hdr hAMZDate (toBS (Time ts :: AWSTime))
               . hdr hAMZContentSHA256 (toBS h)
               . maybe id (hdr hAMZToken . toBS) (_authSessionToken a)
           )

    host =
      case (_endpointSecure end, _endpointPort end) of
        (False, 80) -> _endpointHost end
        (True, 443) -> _endpointHost end
        (_, port) -> _endpointHost end <> ":" <> toBS port

    end = _serviceEndpoint (_requestService rq) r

-- | Used to tag provenance. This allows keeping the same layout as
-- the signing documentation, passing 'ByteString's everywhere, with
-- some type guarantees.
--
-- Data.Tagged is not used for no reason other than the dependency, syntactic length,
-- and the ToByteString instance.
newtype Tag (s :: Symbol) a = Tag {untag :: a}
  deriving stock (Show)

instance ToByteString (Tag s ByteString) where toBS = untag

instance ToLog (Tag s ByteString) where build = build . untag

instance ToByteString CredentialScope where
  toBS = BS8.intercalate "/" . untag

type Hash = Tag "body-digest" ByteString

type StringToSign = Tag "string-to-sign" ByteString

type Credential = Tag "credential" ByteString

type CredentialScope = Tag "credential-scope" [ByteString]

type CanonicalRequest = Tag "canonical-request" ByteString

type CanonicalHeaders = Tag "canonical-headers" ByteString

type CanonicalQuery = Tag "canonical-query" ByteString

type SignedHeaders = Tag "signed-headers" ByteString

type NormalisedHeaders = Tag "normalised-headers" [(ByteString, ByteString)]

type Method = Tag "method" ByteString

type Path = Tag "path" ByteString

type Signature = Tag "signature" ByteString

authorisation :: V4 -> ByteString
authorisation V4 {..} =
  mconcat
    [ algorithm,
      " Credential=",
      toBS metaCredential,
      ", SignedHeaders=",
      toBS metaSignedHeaders,
      ", Signature=",
      toBS metaSignature
    ]

signRequest ::
  -- | Pre-signRequestd signing metadata.
  V4 ->
  -- | The request body.
  Client.RequestBody ->
  -- | Insert authentication information.
  (ClientRequest -> ClientRequest) ->
  Signed a
signRequest m@V4 {..} b auth = Signed (Meta m) (auth rq)
  where
    rq =
      (newClientRequest metaEndpoint metaTimeout)
        { Client.method = toBS metaMethod,
          Client.path = toBS metaPath,
          Client.queryString = qry,
          Client.requestHeaders = metaHeaders,
          Client.requestBody = b
        }

    qry
      | BS.null x = x
      | otherwise = '?' `BS8.cons` x
      where
        x = toBS metaCanonicalQuery

signMetadata ::
  AuthEnv ->
  Region ->
  UTCTime ->
  (Credential -> SignedHeaders -> QueryString -> QueryString) ->
  Hash ->
  Request a ->
  V4
signMetadata a r ts presign digest rq =
  V4
    { metaTime = ts,
      metaMethod = method,
      metaPath = path,
      metaEndpoint = end,
      metaCredential = cred,
      metaCanonicalQuery = query,
      metaCanonicalRequest = crq,
      metaCanonicalHeaders = chs,
      metaSignedHeaders = shs,
      metaStringToSign = sts,
      metaSignature = signature (_authSecretAccessKey a ^. _Sensitive) scope sts,
      metaHeaders = _requestHeaders rq,
      metaTimeout = _serviceTimeout svc
    }
  where
    query = canonicalQuery . presign cred shs $ _requestQuery rq

    sts = stringToSign ts scope crq
    cred = credential (_authAccessKeyId a) scope
    scope = credentialScope svc end ts
    crq = canonicalRequest method path digest query chs shs

    chs = canonicalHeaders headers
    shs = signedHeaders headers
    headers = normaliseHeaders (_requestHeaders rq)

    end = _serviceEndpoint svc r
    method = Tag . toBS $ _requestMethod rq
    path = escapedPath rq

    svc = _requestService rq

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256"

signature :: SecretKey -> CredentialScope -> StringToSign -> Signature
signature k c = Tag . Bytes.encodeBase16 . Crypto.hmacSHA256 signingKey . untag
  where
    signingKey = Foldable.foldl' hmac ("AWS4" <> toBS k) (untag c)

    hmac x y = Bytes.convert (Crypto.hmacSHA256 x y)

stringToSign :: UTCTime -> CredentialScope -> CanonicalRequest -> StringToSign
stringToSign t c r =
  Tag $
    BS8.intercalate
      "\n"
      [ algorithm,
        toBS (Time t :: AWSTime),
        toBS c,
        Bytes.encodeBase16 . Crypto.hashSHA256 $ toBS r
      ]

credential :: AccessKey -> CredentialScope -> Credential
credential k c = Tag (toBS k <> "/" <> toBS c)

credentialScope :: Service -> Endpoint -> UTCTime -> CredentialScope
credentialScope s e t =
  Tag
    [ toBS (Time t :: BasicTime),
      toBS (_endpointScope e),
      toBS (_serviceSigningName s),
      "aws4_request"
    ]

canonicalRequest ::
  Method ->
  Path ->
  Hash ->
  CanonicalQuery ->
  CanonicalHeaders ->
  SignedHeaders ->
  CanonicalRequest
canonicalRequest meth path digest query chs shs =
  Tag $
    BS8.intercalate
      "\n"
      [ toBS meth,
        toBS path,
        toBS query,
        toBS chs,
        toBS shs,
        toBS digest
      ]

escapedPath :: Request a -> Path
escapedPath r = Tag . toBS . escapePath $
  case _serviceAbbrev (_requestService r) of
    "S3" -> _requestPath r
    _ -> collapsePath (_requestPath r)

canonicalQuery :: QueryString -> CanonicalQuery
canonicalQuery = Tag . toBS

-- FIXME: the following use of stripBS is too naive, should remove
-- all internal whitespace, replacing with a single space char,
-- unless quoted with \"...\"
canonicalHeaders :: NormalisedHeaders -> CanonicalHeaders
canonicalHeaders = Tag . BSL.toStrict . BSB.toLazyByteString . Foldable.foldMap (uncurry f) . untag
  where
    f k v = BSB.byteString k <> BSB.char7 ':' <> BSB.byteString (stripBS v) <> BSB.char7 '\n'

signedHeaders :: NormalisedHeaders -> SignedHeaders
signedHeaders = Tag . BS8.intercalate ";" . map fst . untag

normaliseHeaders :: [Header] -> NormalisedHeaders
normaliseHeaders =
  Tag
    . map (first CI.foldedCase)
    . Map.toList
    . Map.delete "authorization"
    . Map.delete "content-length"
    . Map.fromListWith const
