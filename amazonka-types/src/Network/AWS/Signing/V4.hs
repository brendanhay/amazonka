{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.Signing.V4
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.V4 (V4) where

import           Control.Applicative
import           Control.Lens
import           Crypto.Hash
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Bifunctor
import qualified Data.ByteString.Base16    as Base16
import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as Build
import qualified Data.ByteString.Char8     as BS
import           Data.CaseInsensitive      (CI)
import qualified Data.CaseInsensitive      as CI
import qualified Data.Foldable             as Fold
import           Data.List                 (intersperse, sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as Text
import           Network.AWS.Data
import           Network.AWS.Request.Lens
import           Network.AWS.Signing.Types
import           Network.AWS.Types
import           Network.HTTP.Types.Header

data V4

instance SigningAlgorithm V4 where
    finalise s@Service{..} rq@Request{..} Auth{..} r t = Signed $
        rq & hdrs <>~ ((hAuthorization, authorisation) : token)
      where
        authorisation =
            [ algorithm
            , " Credential="
            , authAccess
            , "/"
            , mconcat (intersperse "/" credentialScope)
            , ",SignedHeaders="
            , signedHeaders
            , ",Signature="
            , signature
            ]

        signature = Base16.encode (hmacSHA256 signingKey stringToSign)

        signingKey = Fold.foldl1 hmacSHA256 $
            ("AWS4" <> authSecret) : credentialScope

        stringToSign = mconcat $ intersperse "\n"
            [ algorithm
            , toByteString (ISO8601Time t)
            , BS.intercalate "/" credentialScope
            , Base16.encode (SHA256.hash canonicalRequest)
            ]

        credentialScope =
            [ toByteString (BasicTime t)
            , toByteString r
            , toByteString svcName
            , "aws4_request"
            ]

        canonicalRequest = mconcat $ intersperse "\n"
           [ build rqMethod
           , canonicalURI rq
           , canonicalQuery query
           , canonicalHeaders headers
           , signed
           , hashedPayload rq
           ]

        algorithm = "AWS4-HMAC-SHA256"

        query   = encodedQuery  rq
        signed  = signedHeaders headers
        headers = sortedHeaders (rqHeaders ++ token)
        token   = maybeToList $ (hAMZToken,) <$> authToken
        host    = endpoint s r

canonicalURI :: Request a -> Builder
canonicalURI = encodeURI False . rqPath

canonicalHeaders :: Host -> [Header] -> Builder
canonicalHeaders h = Fold.foldMap collapse mempty . sortedHeaders
  where
    collapse (k, v) = build (CI.foldedCase k)
        <> ":"
        <> build (Text.strip v)
        <> "\n"

signedHeaders :: [Header] -> Builder
signedHeaders = mconcat . intersperse ";" . map (build . CI.foldedCase . fst)

sortedHeaders :: Request a -> [Header]
sortedHeaders = sortBy (comparing fst) . filter valid . rqHeaders
  where
    valid x = prefix `BS.isPrefixOf` CI.foldedCase x || x == hContentType
    prefix  = CI.foldedCase hMetaPrefix

canonicalQuery :: Query -> Builder
canonicalQuery = renderQuery "&" "=" build . over valuesOf (maybe (Just "") Just)
    -- ^ Set subresource key's value to an empty string.

encodedQuery :: Request a -> Query
encodedQuery = encodeQuery (buildByteString . encodeURI True) . rqQuery

hashedPayload :: Request a -> Builder
hashedPayload = build . Base16.encode $ SHA256.hash "" -- hex(sha256hash(payload))
