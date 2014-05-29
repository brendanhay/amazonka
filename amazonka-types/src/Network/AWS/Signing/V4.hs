{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

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
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.Foldable             as Fold
import           Data.List                 (intersperse, sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Types
import           Network.HTTP.Types.Header

data V4

data instance Meta V4 = Meta
    { mAuth    :: ByteString -- ^ Authorisation header.
    , mHeaders :: ByteString -- ^ Signed headers.
    , mRequest :: ByteString -- ^ Canonical request.
    , mSTS     :: ByteString -- ^ String to sign.
    }

instance SigningAlgorithm V4 where
    finalise s@Service{..} Request{..} Auth{..} r t = Signed host
        (Request
            { rqMethod  = rqMethod
            , rqPath    = toBS path
            , rqQuery   = query
            , rqHeaders = (hAuthorization, authorisation) : headers
            , rqBody    = rqBody
            })
        (Meta
            { mAuth     = authorisation
            , mHeaders  = signedHeaders'
            , mRequest  = canonicalRequest
            , mSTS      = stringToSign
            })
      where
        host    = endpoint s r
        path    = encodeURI False rqPath
        query   = encodeQuery (toBS . encodeURI True) rqQuery

        headers = (hHost, toBS host)
            : (hDate, toBS $ RFC822Time t)
            : (rqHeaders ++ token)

        token   = maybeToList $ (hAMZToken,) <$> authToken

        canonicalQuery = renderQuery "&" "=" build
            $ over valuesOf (maybe (Just "") Just) query
            -- ^ Set subresource key's value to an empty string.

        canonicalHeaders = Fold.foldMap f sortedHeaders
          where
            f (k, v) = build (CI.foldedCase k)
                <> ":"
                <> build (stripBS v)
                <> "\n"

        signedHeaders' = toBS signedHeaders
        signedHeaders  = mconcat
            . intersperse ";"
            $ map (build . CI.foldedCase . fst) sortedHeaders

        sortedHeaders = sortBy (comparing fst) $ filter f headers
          where
            f (x, _) = prefix `BS.isPrefixOf` CI.foldedCase x
                || x == hContentType

            prefix = CI.foldedCase hMetaPrefix

        payloadHash = build . Base16.encode $ SHA256.hash ""

        canonicalRequest = buildBS . mconcat $ intersperse "\n"
           [ build rqMethod
           , path
           , canonicalQuery
           , canonicalHeaders
           , signedHeaders
           , payloadHash
           ]

        algorithm = "AWS4-HMAC-SHA256"

        credentialScope' = BS.intercalate "/" credentialScope
        credentialScope  =
            [ toBS (BasicTime t)
            , toBS r
            , toBS svcName
            , "aws4_request"
            ]

        signingKey = Fold.foldl1 hmacSHA256 $
            ("AWS4" <> authSecret) : credentialScope

        stringToSign = mconcat $ intersperse "\n"
            [ algorithm
            , toBS (ISO8601Time t)
            , credentialScope'
            , Base16.encode (SHA256.hash canonicalRequest)
            ]

        signature = Base16.encode (hmacSHA256 signingKey stringToSign)

        authorisation = BS.concat
            [ algorithm
            , " Credential="
            , authAccess
            , "/"
            , credentialScope'
            , ", SignedHeaders="
            , signedHeaders'
            , ", Signature="
            , signature
            ]
