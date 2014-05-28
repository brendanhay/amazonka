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
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base16    as Base16
import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as Build
import qualified Data.ByteString.Char8     as BS
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
        authorisation = BS.concat
            [ algorithm
            , " Credential="
            , authAccess
            , "/"
            , credentialScope'
            , ",SignedHeaders="
            , buildBS signed
            , ",Signature="
            , signature
            ]

        signature = Base16.encode (hmacSHA256 signingKey stringToSign)

        signingKey = Fold.foldl1 hmacSHA256 $
            ("AWS4" <> authSecret) : credentialScope

        stringToSign = mconcat $ intersperse "\n"
            [ algorithm
            , toBS (ISO8601Time t)
            , credentialScope'
            , Base16.encode (SHA256.hash canonicalRequest)
            ]

        credentialScope' = BS.intercalate "/" credentialScope
        credentialScope  =
            [ toBS (BasicTime t)
            , toBS r
            , toBS svcName
            , "aws4_request"
            ]

        canonicalRequest = buildBS . mconcat $ intersperse "\n"
           [ build rqMethod
           , canonicalURI rq
           , canonicalQuery query
           , canonicalHeaders host headers
           , signed
           , hashedPayload rq
           ]

        query   = encodedQuery  rq
        signed  = signedHeaders headers
        headers = sortedHeaders (rqHeaders ++ token)
        host    = endpoint s r
        token   = maybeToList $ (hAMZToken,) <$> authToken

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256"

canonicalURI :: Request a -> Builder
canonicalURI = encodeURI False . rqPath

canonicalHeaders :: Host -> [Header] -> Builder
canonicalHeaders h = Fold.foldMap f
  where
    f (k, v) = build (CI.foldedCase k)
        <> ":"
        <> build (stripBS v)
        <> "\n"

signedHeaders :: [Header] -> Builder
signedHeaders = mconcat . intersperse ";" . map (build . CI.foldedCase . fst)

sortedHeaders :: [Header] -> [Header]
sortedHeaders = sortBy (comparing fst) . filter valid
  where
    valid (x, _) = prefix `BS.isPrefixOf` CI.foldedCase x || x == hContentType

    prefix = CI.foldedCase hMetaPrefix

canonicalQuery :: Query -> Builder
canonicalQuery = renderQuery "&" "=" build . over valuesOf (maybe (Just "") Just)
    -- ^ Set subresource key's value to an empty string.

encodedQuery :: Request a -> Query
encodedQuery = encodeQuery (toBS . encodeURI True) . rqQuery

hashedPayload :: Request a -> Builder
hashedPayload = const (build . Base16.encode $ SHA256.hash "")
