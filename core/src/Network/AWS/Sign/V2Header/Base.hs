{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Sign.V2Header.Base
-- Description : This module provides auxiliary functions necessary for the AWS compliant V2 Header request signer.
--               See also Network.AWS.Sign.V2Header
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.AWS.Sign.V2Header.Base
  (
  constructHeaderSigner
  , constructFullPath
  , constructHeaderStringForSigning
  , constructQueryStringForSigning
  , filterHeaders
  , sortHeaders
  , toSignerQBS
  , unionNecessaryHeaders
  ) where


import           Data.ByteString.Builder     (Builder)
import           Network.HTTP.Types.URI      (urlEncode)

import qualified Data.ByteString.Builder      as Build
import qualified Data.ByteString.Char8        as DBC
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.CaseInsensitive         as DC
import qualified Data.List                    as DL
import           Data.Monoid                  ((<>), mempty)

import qualified Network.AWS.Data.ByteString  as NADB
import qualified Network.AWS.Data.Query       as NADQ
import qualified Network.HTTP.Types           as NHT

-- | Filter for 'interesting' keys within a QueryString
isInterestingQueryKey :: NADB.ByteString -> Bool
isInterestingQueryKey key
  | key == "acl" = True
  | key == "cors" = True
  | key == "defaultObjectAcl" = True
  | key == "location" = True
  | key == "logging" = True
  | key == "partNumber" = True
  | key == "policy" = True
  | key == "requestPayment" = True
  | key == "torrent" = True
  | key == "versioning" = True
  | key == "versionId" = True
  | key == "versions" = True
  | key == "website" = True
  | key == "uploads" = True
  | key == "uploadId" = True
  | key == "response-content-type" = True
  | key == "response-content-language" = True
  | key == "response-expires" = True
  | key == "response-cache-control" = True
  | key == "response-content-disposition" = True
  | key == "response-content-encoding" = True
  | key == "delete" = True
  | key == "lifecycle" = True
  | key == "tagging" = True
  | key == "restore" = True
  | key == "storageClass" = True
  | key == "websiteConfig" = True
  | key == "compose" = True
  | otherwise = False

-- | Filter for 'interesting' header fields
isInterestingHeader :: NHT.Header -> Bool
isInterestingHeader header
  | headerName == NHT.hDate = True
  | headerName == NHT.hContentMD5 = True
  | headerName == NHT.hContentType = True
  | "aws-" `DBC.isPrefixOf` DC.foldedCase headerName = True
  | otherwise = False
  where headerName = fst header

-- | Constructs a query string for signing
constructQueryStringForSigning :: NADQ.QueryString -> NADQ.QueryString
constructQueryStringForSigning (NADQ.QValue _) = NADQ.QValue Nothing
constructQueryStringForSigning (NADQ.QList qList) = NADQ.QList filtered
  where
    filtered = map constructQueryStringForSigning qList
constructQueryStringForSigning (NADQ.QPair key value)
  | isInterestingQueryKey key = NADQ.QPair key value
  | otherwise = NADQ.QValue Nothing

-- | Construct a header string for signing
constructHeaderStringForSigning :: NHT.Header -> NADB.ByteString
constructHeaderStringForSigning header
  | "aws-" `DBC.isPrefixOf` DC.foldedCase headerName = DBC.append (DC.foldedCase headerName) $ DBC.append ":" headerValue
  | otherwise = headerValue
  where
    headerName = fst header
    headerValue = snd header

filterHeaders :: [NHT.Header] -> [NHT.Header]
filterHeaders = filter isInterestingHeader

unionNecessaryHeaders :: [NHT.Header] -> [NHT.Header]
unionNecessaryHeaders headers = DL.unionBy (\x y -> fst x == fst y) headers [(NHT.hContentMD5, ""), (NHT.hContentType, "")]

sortHeaders :: [NHT.Header] -> [NHT.Header]
sortHeaders = DL.sort

-- | The following function mostly follows the toBS in amazonka QueryString
--   except for single QValue or single QPair keys not being suffixed with
--   an equals.
toSignerQBS :: NADQ.QueryString -> NADB.ByteString
toSignerQBS = LBS.toStrict . Build.toLazyByteString . cat . DL.sort . enc Nothing
  where
    enc :: Maybe NADB.ByteString -> NADQ.QueryString -> [NADB.ByteString]
    enc p = \case
      NADQ.QList xs          -> concatMap (enc p) xs

      NADQ.QPair (urlEncode True -> k) x
        | Just n <- p -> enc (Just (n <> kdelim <> k)) x -- <prev>.key <recur>
        | otherwise   -> enc (Just k)                  x -- key <recur>

      NADQ.QValue (Just (urlEncode True -> v))
        | Just n <- p -> [n <> vsep <> v] -- key=value
        | otherwise   -> [v]

      _   | Just n <- p -> [n]
          | otherwise   -> []

    cat :: [NADB.ByteString] -> Builder
    cat []     = mempty
    cat [x]    = Build.byteString x
    cat (x:xs) = Build.byteString x <> ksep <> cat xs

    kdelim = "."
    ksep   = "&"
    vsep   = "="

constructFullPath :: NADB.ByteString -> NADB.ByteString -> NADB.ByteString
constructFullPath path q
  | q == "" = path
  | otherwise = path `DBC.append` "?" `DBC.append` q

-- | Construct a full header signer following the V2 Header scheme
constructHeaderSigner :: NHT.RequestHeaders -> NADB.ByteString -> NADB.ByteString -> NADQ.QueryString -> NADB.ByteString
constructHeaderSigner headers method path query = signer
  where

    signer = DBC.intercalate "\n" $ [method] ++ map constructHeaderStringForSigning sortedHeaders ++ [constructFullPath path (toSignerQBS filteredQuery)]

    sortedHeaders = sortHeaders allInterestingHeaders

    allInterestingHeaders = unionNecessaryHeaders filterList

    filterList = filterHeaders headers

    filteredQuery = constructQueryStringForSigning query
