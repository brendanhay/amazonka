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
    -- * Types
      V4
    , Meta (..)

    -- * Re-exports
    , module Network.AWS.Signing.Types
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
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Signing.Types
import           Network.AWS.Types
import           Network.HTTP.Client.Lens
import           Network.HTTP.Types.Header
import           System.Locale

data V4

data instance Meta V4 = Meta
    { _mAlgorithm :: ByteString
    , _mCReq      :: ByteString
    , _mScope     :: ByteString
    , _mSigned    :: ByteString
    , _mSTS       :: ByteString
    , _mSignature :: ByteString
    }

instance AWSPresigner V4 where
    presigned s a r rq l e t =
        out & sgRequest . queryString <>~ auth (out ^. sgMeta)
      where
        out = finalise Nothing qry s a r rq l t

        -- FIXME: add security token query param
        qry cs sh =
              pair "X-AMZ-Algorithm" algorithm
            . pair "X-AMZ-Credential" cs
            . pair "X-AMZ-Date" (ISO8601Time l t)
            . pair "X-AMZ-Expires" e
            . pair "X-AMZ-SignedHeaders" sh

        auth = mappend "&X-AMZ-Signature=" . _mSignature

instance AWSSigner V4 where
    signed s a r rq l t =
        out & sgRequest %~ requestHeaders %~ auth (out ^. sgMeta)
      where
        out = finalise (Just "AWS4") (\_ _ -> id) s a r inp l t

        inp = rq & rqHeaders %~ hdrs (maybeToList tok)

        tok = (hAMZToken,) <$> _authToken a

        auth Meta{..} = hdr hAuthorization $ BS.concat
            [ _mAlgorithm
            , " Credential="
            , _mScope
            , ", SignedHeaders="
            , _mSigned
            , ", Signature="
            , _mSignature
            ]

algorithm :: ByteString
algorithm = "AWS4-HMAC-SHA256"

finalise :: Maybe ByteString
         -> (ByteString -> ByteString -> Query -> Query)
         -> Service (Service' a)
         -> Auth
         -> Region
         -> Request a
         -> TimeLocale
         -> UTCTime
         -> Signed a V4
finalise p qry s@Service{..} Auth{..} r Request{..} l t = Signed meta rq
  where
    meta = Meta
        { _mAlgorithm = algorithm
        , _mCReq      = canonicalRequest
        , _mScope     = _authAccess <> "/" <> credentialScope
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
        & valuesOf %~ (maybe (Just "") (Just . encodeURI True))
        & keysOf   %~ (encodeURI False)

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
        maybe _authSecret (<> _authSecret) p : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , toBS (AWSTime l t)
        , credentialScope
        , sha256 canonicalRequest
        ]

    signature = Base16.encode (hmacSHA256 signingKey stringToSign)
