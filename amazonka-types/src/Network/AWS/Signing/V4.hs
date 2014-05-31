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

module Network.AWS.Signing.V4
    (
    -- * Version 4 Signatures
      V4
    , Meta             (..)

    -- * Re-exports
    , SigningAlgorithm (..)
    , Signed           (..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.Foldable             as Fold
import           Data.Function
import           Data.List                 (groupBy, intersperse, sortBy, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Types
import           Network.HTTP.Types.Header

data V4

data instance Meta V4 = Meta
    { mCanon :: ByteString -- ^ Canonical request.
    , mAuth  :: ByteString -- ^ Authorisation header.
    , mSTS   :: ByteString -- ^ String to sign.
    }

instance SigningAlgorithm V4 where
    finalise s@Service{..} Auth{..} r Request{..} l t = Signed host rq meta
      where
        rq = Request
            { rqMethod  = rqMethod
            , rqPath    = rqPath
            , rqQuery   = rqQuery
            , rqHeaders = append hAuthorization authorisation headers
            , rqBody    = rqBody
            , rqSHA256  = rqSHA256
            }

        meta = Meta
            { mAuth  = authorisation
            , mCanon = canonicalRequest
            , mSTS   = stringToSign
            }

        host  = endpoint s r
        token = maybeToList $ (hAMZToken,) <$> authToken

        headers = sortBy (comparing fst)
            . append hHost (toBS host)
            . append hDate (toBS $ RFC822Time l t)
            $ (rqHeaders ++ token)

        canonicalQuery = renderQuery
            . over valuesOf (maybe (Just "") (Just . encodeURI True))
            $ over keysOf (encodeURI False) rqQuery

        joinedHeaders = map f $ groupBy ((==) `on` fst) headers
          where
            f []     = ("", "")
            f (h:hs) = (fst h, g $ h : hs)

            g = BS.intercalate "," . sort . map snd

        canonicalHeaders = Fold.foldMap f joinedHeaders
          where
            f (k, v) = CI.foldedCase k
                <> ":"
                <> stripBS v
                <> "\n"

        signedHeaders' = toBS signedHeaders
        signedHeaders  = mconcat
            . intersperse ";"
            $ map (CI.foldedCase . fst) joinedHeaders

        canonicalRequest = mconcat $ intersperse "\n"
           [ toBS rqMethod
           , collapseURI (encodeURI False rqPath)
           , canonicalQuery
           , canonicalHeaders
           , signedHeaders
           , toBS rqSHA256
           ]

        algorithm = "AWS4-HMAC-SHA256"

        credentialScope' = BS.intercalate "/" credentialScope
        credentialScope  =
            [ toBS (BasicTime l t)
            , toBS r
            , toBS svcName
            , "aws4_request"
            ]

        signingKey = Fold.foldl1 hmacSHA256 $
            ("AWS4" <> authSecret) : credentialScope

        stringToSign = mconcat $ intersperse "\n"
            [ algorithm
            , toBS (AWSTime l t)
            , credentialScope'
            , sha256 canonicalRequest
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
