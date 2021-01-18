-- |
-- Module      : Network.AWS.Sign.V4.Base
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Sign.V4.Base
  ( )
where

import qualified Data.Char as Char
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Dynamic as Dynamic 
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Foldable as Foldable
import qualified Data.Function  as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.TypeLits
import Network.AWS.Data
-- import Network.AWS.Lens ((%~), (<>~), (^.))
import Network.AWS.Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.Types
import qualified Network.AWS.Crypt as Crypt
import qualified Network.AWS.Bytes as Bytes
import qualified Network.HTTP.Conduit as Client
import qualified Network.HTTP.Types.Header as Header
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
    metaHeaders :: Headers,
    metaTimeout :: (Maybe Seconds)
  }

-- instance ToLog V4 where
--   build V4 {..} =
--     buildLines
--       [ "[Version 4 Metadata] {",
--         "  time              = " <> build metaTime,
--         "  endpoint          = " <> build (endpointHost metaEndpoint),
--         "  credential        = " <> build metaCredential,
--         "  signed headers    = " <> build metaSignedHeaders,
--         "  signature         = " <> build metaSignature,
--         "  string to sign    = {",
--         build metaStringToSign,
--         "}",
--         "  canonical request = {",
--         build metaCanonicalRequest,
--         "  }",
--         "}"
--       ]

base ::
  Hash ->
  Request a ->
  AuthEnv ->
  Region ->
  UTCTime ->
  (V4, ClientRequest -> ClientRequest)
base hash rq@Request{..} auth@AuthEnv {..} region time =
  (metadata,
   prepareRequest
  )
  where
    prepareRequest x =
      x { requestHeaders =
          Map.insert Header.hAuthorization (authorisation metadata) $
            requestHeaders x
        }

    metadata =
      signMetadata auth region time presigner hash $
        rq { requestHeaders =
      Map.insert Header.hHost endpointHost
          . Map.insert hAMZDate (formatAWSTime time)
          . hAMZContentSHA256 (fromTag hash)
          . addTokenHeader
          $ requestHeaders
           }

    addTokenHeader =
      case authSessionToken of
        Nothing -> id
        Just token -> Map.insert hAMZToken (fromSessionToken token)
 
    presigner _ _ = id

    endpoint@Endpoint {endpointHost} =
      serviceEndpoint requestService region

-- | Used to tag provenance. This allows keeping the same layout as
-- the signing documentation, passing 'ByteString's everywhere, with
-- some type guarantees.
--
-- Data.Tagged is not used for no reason other than syntactic length and
-- the ToByteString instance.
newtype Tag (s :: Symbol) a = Tag {fromTag :: a} deriving (Show)

-- instance ToByteString (Tag s ByteString) where = fromTag

-- instance ToLog (Tag s ByteString) where build = build . fromTag

-- instance ToByteString CredentialScope where
--   = ByteString.Char8.intercalate "/" . fromTag

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
  algorithm
    <> " Credential="
    <> metaCredential
    <> ", SignedHeaders="
    <> metaSignedHeaders
    <> ", Signature="
    <> metaSignature

signRequest ::
  -- | Pre-request signing metadata.
  V4 ->
  -- | The request body.
  Client.RequestBody ->
  -- | Insert authentication information.
  (ClientRequest -> ClientRequest) ->
  SignedRequest a
signRequest metadata@V4 {..} b auth =
    SignedRequest
    { signedMetadata = Dynamic.toDyn metadata,
      signedRequest = request
    }
  where
    request =
      (newClientRequest metaEndpoint metaTimeout)
        { Client.method = fromTag metaMethod,
          Client.path = fromTag metaPath,
          Client.queryString = query,
          Client.requestHeaders = Map.toList metaHeaders,
          Client.requestBody = b
        }

    query
      | ByteString.null x = x
      | otherwise = '?' `ByteString.Char8.cons` x
      where
        x = metaCanonicalQuery

signMetadata ::
  AuthEnv ->
  Region ->
  UTCTime ->
  (Credential -> SignedHeaders -> QueryString -> QueryString) ->
  Hash ->
  Request a ->
  V4
signMetadata AuthEnv{..} region time presign digest rq =
  V4
    { metaTime = time,
      metaMethod = method,
      metaPath = path,
      metaEndpoint = endpoint,
      metaCredential = cred,
      metaCanonicalQuery = query,
      metaCanonicalRequest = crq,
      metaCanonicalHeaders = chs,
      metaSignedHeaders = shs,
      metaStringToSign = sts,
      metaSignature = signature authSecretAccessKey scope sts,
      metaHeaders = requestHeaders rq,
      metaTimeout = serviceTimeout service
    }
  where
    query = canonicalQuery . presign cred shs $ requestQuery rq

    sts = stringToSign time scope crq
    cred = credential authAccessKeyId scope
    scope = credentialScope service endpoint time
    crq = canonicalRequest method path digest query chs shs

    chs = canonicalHeaders headers
    shs = signedHeaders headers
    headers = normaliseHeaders (requestHeaders rq)

    endpoint = serviceEndpoint service region
    method = Tag (HTTP.renderStdMethod (requestMethod rq))
    path = escapedPath rq
    service = requestService rq

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256"

signature :: SecretKey -> CredentialScope -> StringToSign -> Signature
signature key scope =
    Tag . Bytes.encodeBase16 . Crypt.hmacSHA256 signingKey . fromTag
  where
    signingKey =
       Crypt.Key (Foldable.foldl' hmac ("AWS4" <> key) (fromTag scope))

    hmac key message =
       Bytes.convert (Crypt.hmacSHA256 (Crypt.Key key) message)

stringToSign :: UTCTime -> CredentialScope -> CanonicalRequest -> StringToSign
stringToSign time scope region =
  Tag $
    ByteString.Char8.intercalate
      "\n"
      [ algorithm,
        formatAWSTime time,
        scope,
        Bytes.encodeBase16 (Crypt.hashSHA256 region)
      ]

credential :: AccessKey -> CredentialScope -> Credential
credential key scope =
  Tag $
     fromAccessKey key
     <> "/"
     <> ByteString.Char8.intercalate "/" (fromTag scope)
  
credentialScope :: Service -> Endpoint -> UTCTime -> CredentialScope
credentialScope s e t =
  Tag
    [ formatBasicTime t,
      (endpointScope e),
      (servicePrefix s),
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
    ByteString.Char8.intercalate
      "\n"
      [ meth,
        path,
        query,
        chs,
        shs,
        digest
      ]

escapedPath :: Request a -> Path
escapedPath r =
 Tag . escapePath $
  case serviceAbbrev (requestService r) of
    "S3" -> requestPath r
    _ -> collapsePath (requestPath r)

canonicalQuery :: QueryString -> CanonicalQuery
canonicalQuery = Tag . encodeQuery True

-- FIXME: the following use of stripBS is too naive, should remove
-- all internal whitespace, replacing with a single space char,
-- unless quoted with \"...\"
canonicalHeaders :: NormalisedHeaders -> CanonicalHeaders
canonicalHeaders = Tag . foldMap (uncurry f) . fromTag
  where
    f k v =
      k <> ":" <> stripBS v <> "\n"

    stripBS =
      ByteString.Char8.dropWhile Char.isSpace
        . fst
        . ByteString.Char8.spanEnd Char.isSpace
    
signedHeaders :: NormalisedHeaders -> SignedHeaders
signedHeaders = Tag . ByteString.Char8.intercalate ";" . map fst . fromTag

normaliseHeaders :: Headers -> NormalisedHeaders
normaliseHeaders =
  Tag
    . map (Bifunctor.first CI.foldedCase)
    . Map.toAscList
    . Map.delete "authorization"
    . Map.delete "content-length"

formatAWSTime, formatBasicTime :: UTCTime -> ByteString
formatAWSTime = Text.Encoding.encodeUtf8 . formatDateTime awsFormat
formatBasicTime = Text.Encoding.encodeUtf8 . formatDateTime basicFormat
