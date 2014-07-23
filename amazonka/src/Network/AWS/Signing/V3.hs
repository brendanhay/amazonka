{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.V3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.V3
    (
    -- * Types
      V3
    , Meta (..)
    , authorisation

    -- * Re-exports
    , module Network.AWS.Signing.Common
    ) where

import           Control.Applicative
import           Control.Lens
import qualified Crypto.Hash.SHA256        as SHA256
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
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.Common
import           Network.AWS.Types
import           Network.HTTP.Client.Lens
import           Network.HTTP.Types.Header
import           System.Locale

data V3

data instance Meta V3 = Meta
    { _mAlgorithm :: ByteString
    , _mSignature :: ByteString
    }

instance AWSSigner V3 where
    signed s a r rq l t = Signed (Meta sig) (rq & rqHeaders <>~ headers)
      where
        headers = hdr hDate rfc822 : hdr hAMZAuth authorisation : maybeToList token

        token = (hAMZToken,) . toBS <$> _authToken a

        authorisation = "AWS3-HTTPS AWSAccessKeyId="
            <> _authAccess a
            <> ", Algorithm=HmacSHA256, Signature="
            <> Base64.encode (hmacSHA256 (_authSecret a) rfc822)

        rfc822 = toBS (RFC822Time t)

authorisation :: AuthEnv -> Meta V3 -> ByteString
authorisation AuthEnv{..} Meta{..} = BS.concat
    [ " AWSAccessKeyId="
    , _authAccess
    , ", Algorithm="
    , _mAlgorithm
    , ", Signature="
    , _mSignature
    ]

algorithm :: ByteString
algorithm = "HmacSHA256"

finalise :: Maybe ByteString
         -> (ByteString -> ByteString -> Query -> Query)
         -> Service (Sv a)
         -> AuthEnv
         -> Region
         -> Request a
         -> TimeLocale
         -> UTCTime
         -> Signed a V3
finalise p qry s@Service{..} AuthEnv{..} r Request{..} l t = Signed meta rq
  where
    meta = Meta
        { _mAlgorithm = algorithm
        , _mCReq      = canonicalRequest
        , _mScope     = toBS _authAccess <> "/" <> credentialScope
        , _mSigned    = signedHeaders
        , _mSTS       = stringToSign
        , _mSignature = signature
        }

    rq = clientRequest
        & method         .~ meth
        & host           .~ host'
        & path           .~ _rqPath
        & queryString    .~ renderQuery query
        & requestHeaders .~ headers
        & requestBody    .~ clientBody _rqBody

    meth  = toBS _rqMethod
    host' = toBS (endpoint s r)
    query = qry credentialScope signedHeaders _rqQuery

    canonicalQuery = renderQuery $ query
        & valuesOf %~ Just . maybe "" (encodeURI True)
        & keysOf   %~ encodeURI False

    headers = sortBy (comparing fst)
        . hdr hHost host'
        . hdr hDate (toBS $ RFC822Time l t)
        $ _rqHeaders

    joinedHeaders = map f $ groupBy ((==) `on` fst) headers
      where
        f []     = ("", "")
        f (h:hs) = (fst h, g $ h : hs)

        g = BS.intercalate "," . sort . map snd

    signedHeaders = mconcat
        . intersperse ";"
        $ map (CI.foldedCase . fst) joinedHeaders

    canonicalHeaders = Fold.foldMap f joinedHeaders
      where
        f (k, v) = CI.foldedCase k
            <> ":"
            <> stripBS v
            <> "\n"

    canonicalRequest = mconcat $ intersperse "\n"
       [ meth
       , collapseURI (encodeURI False _rqPath)
       , canonicalQuery
       , canonicalHeaders
       , signedHeaders
       , payloadHash _rqBody
       ]

    scope =
        [ toBS (BasicTime l t)
        , toBS r
        , toBS _svcName
        , "aws4_request"
        ]

    credentialScope = BS.intercalate "/" scope

    signingKey = Fold.foldl1 hmacSHA256 $
        maybe (toBS _authSecret) (<> toBS _authSecret) p : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , toBS (AWSTime l t)
        , credentialScope
        , Base16.encode (SHA256.hash canonicalRequest)
        ]

    signature = Base16.encode (hmacSHA256 signingKey stringToSign)
