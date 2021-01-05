-- |
-- Module      : Network.AWS.Sign.V2Header.Base
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides auxiliary functions necessary for the AWS compliant V2
-- Header request signer.
-- /See/: "Network.AWS.Sign.V2Header"
module Network.AWS.Sign.V2Header.Base
  ( newSigner,

    -- * Testing
    toSignerQueryBS,
    constructSigningHeader,
    constructSigningQuery,
    constructFullPath,
    unionNecessaryHeaders,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DList
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
-- import Network.HTTP.Types.URI (urlEncode)

import Network.AWS.Data
import qualified Network.AWS.Data.Headers as Headers
import qualified Network.AWS.Data.Query as Query
import Network.AWS.Prelude
import qualified Network.HTTP.Types as HTTP

-- | Construct a full header signer following the V2 Header scheme
newSigner ::
  Headers ->
  ByteString ->
  ByteString ->
  QueryString ->
  ByteString
newSigner headers method path query = signer
  where
    signer =
      ByteString.Char8.intercalate "\n"
        . DList.toList
        $ DList.singleton method
          <> fmap constructSigningHeader (DList.fromList filteredHeaders)
          <> DList.singleton (constructFullPath path filteredQuery)

    filteredHeaders =
      Map.toList
        . unionNecessaryHeaders
        . Map.filterWithKey (const . isInterestingHeader)
        $ headers

    filteredQuery =
      encodeQuery False (constructSigningQuery query)

-- | Encode a QueryString without using @=@ for empty query values
toSignerQueryBS :: QueryString -> ByteString
toSignerQueryBS = encodeQuery False

hasAWSPrefix :: CI ByteString -> Bool
hasAWSPrefix = ByteString.Char8.isPrefixOf "aws-" . CI.foldedCase

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
isInterestingHeader :: HeaderName -> Bool
isInterestingHeader name
  | name == HTTP.hDate = True
  | name == HTTP.hContentMD5 = True
  | name == HTTP.hContentType = True
  | hasAWSPrefix name = True
  | otherwise = False

-- | Constructs a query string for signing
constructSigningQuery :: QueryString -> QueryString
constructSigningQuery = filter (isInterestingQueryKey . fst)

-- | Construct a header string for signing
constructSigningHeader :: Header -> ByteString
constructSigningHeader (name, value)
  | hasAWSPrefix name = CI.foldedCase name <> ":" <> value
  | otherwise = value

constructFullPath :: ByteString -> ByteString -> ByteString
constructFullPath path q
  | ByteString.null q = path
  | otherwise = path <> "?" <> q

unionNecessaryHeaders :: Headers -> Headers
unionNecessaryHeaders =
  Map.union $
    Map.fromList
      [ (HTTP.hContentMD5, ""),
        (HTTP.hContentType, "")
      ]
