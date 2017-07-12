{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Sign.V2Header.Base
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides auxiliary functions necessary for the AWS compliant V2
-- Header request signer.
-- /See/: "Network.AWS.Sign.V2Header"
module Network.AWS.Sign.V2Header.Base
    ( newSigner

    -- * Testing
    , toSignerQueryBS

    , constructSigningHeader
    , constructSigningQuery
    , constructFullPath

    , unionNecessaryHeaders
    ) where

import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.CaseInsensitive    as CI
import           Data.Function           (on)
import qualified Data.List               as List
import           Data.Monoid             (mempty, (<>))
import qualified Network.AWS.Data.Query  as Query
import qualified Network.HTTP.Types      as HTTP
import           Network.HTTP.Types.URI  (urlEncode)

-- | Construct a full header signer following the V2 Header scheme
newSigner :: HTTP.RequestHeaders
          -> ByteString
          -> ByteString
          -> Query.QueryString
          -> ByteString
newSigner headers method path query = signer
  where
    signer =
        BS8.intercalate "\n"
            ( method
            : map constructSigningHeader (List.sort filteredHeaders)
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
      Query.QList xs    -> concatMap (enc p) xs

      Query.QPair (urlEncode True -> k) x
          | Just n <- p -> enc (Just (n <> kdelim <> k)) x -- <prev>.key <recur>
          | otherwise   -> enc (Just k)                  x -- key <recur>

      Query.QValue (Just (urlEncode True -> v))
          | Just n <- p -> [n <> vsep <> v] -- key=value
          | otherwise   -> [v]

      _   | Just n <- p -> [n]
          | otherwise   -> []

    cat :: [ByteString] -> Builder
    cat []     = mempty
    cat [x]    = Build.byteString x
    cat (x:xs) = Build.byteString x <> ksep <> cat xs

    kdelim = "."
    ksep   = "&"
    vsep   = "="

hasAWSPrefix :: CI.CI ByteString -> Bool
hasAWSPrefix = BS8.isPrefixOf "aws-" . CI.foldedCase

-- | Filter for 'interesting' keys within a QueryString
isInterestingQueryKey :: ByteString -> Bool
isInterestingQueryKey = \case
    "acl"                          -> True
    "cors"                         -> True
    "defaultObjectAcl"             -> True
    "location"                     -> True
    "logging"                      -> True
    "partNumber"                   -> True
    "policy"                       -> True
    "requestPayment"               -> True
    "torrent"                      -> True
    "versioning"                   -> True
    "versionId"                    -> True
    "versions"                     -> True
    "website"                      -> True
    "uploads"                      -> True
    "uploadId"                     -> True
    "response-content-type"        -> True
    "response-content-language"    -> True
    "response-expires"             -> True
    "response-cache-control"       -> True
    "response-content-disposition" -> True
    "response-content-encoding"    -> True
    "delete"                       -> True
    "lifecycle"                    -> True
    "tagging"                      -> True
    "restore"                      -> True
    "storageClass"                 -> True
    "websiteConfig"                -> True
    "compose"                      -> True
    _                              -> False

-- | Filter for 'interesting' header fields
isInterestingHeader :: HTTP.Header -> Bool
isInterestingHeader (name, _)
    | name == HTTP.hDate        = True
    | name == HTTP.hContentMD5  = True
    | name == HTTP.hContentType = True
    | hasAWSPrefix name         = True
    | otherwise                 = False

-- | Constructs a query string for signing
constructSigningQuery :: Query.QueryString -> Query.QueryString
constructSigningQuery = \case
    Query.QValue {} -> Query.QValue Nothing
    Query.QList  qs -> Query.QList (map constructSigningQuery qs)
    Query.QPair  k v
        | isInterestingQueryKey k -> Query.QPair k v
        | otherwise               -> Query.QValue Nothing

-- | Construct a header string for signing
constructSigningHeader :: HTTP.Header -> ByteString
constructSigningHeader (name, value)
    | hasAWSPrefix name = CI.foldedCase name <> ":" <> value
    | otherwise         = value

constructFullPath :: ByteString -> ByteString -> ByteString
constructFullPath path q
    | BS8.null q = path
    | otherwise  = path <> "?" <> q

unionNecessaryHeaders :: [HTTP.Header] -> [HTTP.Header]
unionNecessaryHeaders =
    flip (List.unionBy (on (==) fst))
        [ (HTTP.hContentMD5,  "")
        , (HTTP.hContentType, "")
        ]
