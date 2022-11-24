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
    newSigner,
    toSignerQueryBS,
    constructSigningHeader,
    constructSigningQuery,
    constructFullPath,
    unionNecessaryHeaders,
  )
where

import qualified Amazonka.Bytes as Bytes
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data
import qualified Amazonka.Data.Query as Query
import Amazonka.Prelude
import Amazonka.Types hiding (presign, sign)
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.URI as URI

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
        "  endpoint  = " <> build (host metaEndpoint),
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

    signer = newSigner headers' meth path' query

    rq =
      (newClientRequest end timeout)
        { Client.method = meth,
          Client.path = path',
          Client.queryString = toBS query,
          Client.requestHeaders = headers',
          Client.requestBody = toRequestBody body
        }

    meth = toBS method
    path' = toBS (escapePath path)

    end = endpoint r

    Service {timeout, endpoint} = service

    signature =
      Bytes.encodeBase64
        . Crypto.hmacSHA1 (toBS secretAccessKey)
        $ signer

    headers' =
      headers
        & hdr HTTP.hAuthorization ("AWS " <> toBS accessKeyId <> ":" <> signature)
        & hdr HTTP.hDate time

    time = toBS (Time t :: RFC822)

-- | Construct a full header signer following the V2 Header scheme
newSigner ::
  HTTP.RequestHeaders ->
  ByteString ->
  ByteString ->
  Query.QueryString ->
  ByteString
newSigner headers method path query = signer
  where
    signer =
      BS8.intercalate
        "\n"
        ( method :
          map constructSigningHeader (List.sort filteredHeaders)
            ++ [constructFullPath path (toSignerQueryBS filteredQuery)]
        )

    filteredHeaders = unionNecessaryHeaders (filter isInterestingHeader headers)

    filteredQuery = constructSigningQuery query

-- | The following function mostly follows the toBS in amazonka QueryString
-- except for single QValue or single QPair keys not being suffixed with
-- an equals.
toSignerQueryBS :: Query.QueryString -> ByteString
toSignerQueryBS =
  LBS.toStrict . Build.toLazyByteString . cat . List.sort . enc Nothing
  where
    enc :: Maybe ByteString -> Query.QueryString -> [ByteString]
    enc p = \case
      Query.QList xs -> concatMap (enc p) xs
      Query.QPair (URI.urlEncode True -> k) x
        | Just n <- p -> enc (Just (n <> kdelim <> k)) x -- <prev>.key <recur>
        | otherwise -> enc (Just k) x -- key <recur>
      Query.QValue (Just (URI.urlEncode True -> v))
        | Just n <- p -> [n <> vsep <> v] -- key=value
        | otherwise -> [v]
      _
        | Just n <- p -> [n]
        | otherwise -> []

    cat :: [ByteString] -> ByteStringBuilder
    cat [] = mempty
    cat [x] = Build.byteString x
    cat (x : xs) = Build.byteString x <> ksep <> cat xs

    kdelim = "."
    ksep = "&"
    vsep = "="

hasAWSPrefix :: CI.CI ByteString -> Bool
hasAWSPrefix = BS8.isPrefixOf "aws-" . CI.foldedCase

-- | Filter for 'interesting' keys within a QueryString
isInterestingQueryKey :: ByteString -> Bool
isInterestingQueryKey = \case
  "acl" -> True
  "cors" -> True
  "defaultObjectAcl" -> True
  "location" -> True
  "logging" -> True
  "partNumber" -> True
  "policy" -> True
  "requestPayment" -> True
  "torrent" -> True
  "versioning" -> True
  "versionId" -> True
  "versions" -> True
  "website" -> True
  "uploads" -> True
  "uploadId" -> True
  "response-content-type" -> True
  "response-content-language" -> True
  "response-expires" -> True
  "response-cache-control" -> True
  "response-content-disposition" -> True
  "response-content-encoding" -> True
  "delete" -> True
  "lifecycle" -> True
  "tagging" -> True
  "restore" -> True
  "storageClass" -> True
  "websiteConfig" -> True
  "compose" -> True
  _ -> False

-- | Filter for 'interesting' header fields
isInterestingHeader :: HTTP.Header -> Bool
isInterestingHeader (name, _)
  | name == HTTP.hDate = True
  | name == HTTP.hContentMD5 = True
  | name == HTTP.hContentType = True
  | hasAWSPrefix name = True
  | otherwise = False

-- | Constructs a query string for signing
constructSigningQuery :: Query.QueryString -> Query.QueryString
constructSigningQuery = \case
  Query.QValue {} -> Query.QValue Nothing
  Query.QList qs -> Query.QList (map constructSigningQuery qs)
  Query.QPair k v
    | isInterestingQueryKey k -> Query.QPair k v
    | otherwise -> Query.QValue Nothing

-- | Construct a header string for signing
constructSigningHeader :: HTTP.Header -> ByteString
constructSigningHeader (name, value)
  | hasAWSPrefix name = CI.foldedCase name <> ":" <> value
  | otherwise = value

constructFullPath :: ByteString -> ByteString -> ByteString
constructFullPath path q
  | BS8.null q = path
  | otherwise = path <> "?" <> q

unionNecessaryHeaders :: [HTTP.Header] -> [HTTP.Header]
unionNecessaryHeaders =
  flip
    (List.unionBy (Function.on (==) fst))
    [ (HTTP.hContentMD5, ""),
      (HTTP.hContentType, "")
    ]
