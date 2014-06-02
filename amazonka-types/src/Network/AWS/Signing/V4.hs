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

module Network.AWS.Signing.V4 where
--    (
    -- -- * Version 4 Signatures
    --   V4
    -- , Ctx              (..)

    -- -- * Re-exports
    -- , SigningAlgorithm (..)
    -- , Signed           (..)
--    ) where

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
import           Network.AWS.Request.Lens
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
    presigned s as r rq l t =

      where
        inp = rq 

        -- rq = undefined -- signed
        --     -- { method         = toBS rqMethod
        --     -- , host           = toBS host
        --     -- , path           = rqPath
        --     -- , queryString    = renderQuery rqQuery
        --     -- , requestHeaders = hdr hAuthorization _auth headers
        --     -- , requestBody    = rqBody
        --     -- }

        -- meta = Meta
        --     { mCReq = canonicalRequest
        --     , mAuth = _auth
        --     , mSTS  = stringToSign
        --     }

        query = [ ("X-AMZ-Algorithm", )
                , ("X-AMZ-Credential", credentialScope)
                , ("X-AMZ-Date", ISO8601Time t)
                , ("X-AMZ-Expires", seconds)
                , ("X-AMZ-SignedHeaders", )
                , ("X-AMZ-Signature", )
                ]

instance AWSSigner V4 where
    signed s as r rq l t =
        out & sgRequest %~ requestHeaders %~ auth (out ^. sgMeta)
      where
        out = finalise (Just "AWS4") s inp as r l t
        inp = rq & rqHeaders %~ hdrs (maybeToList $ authTokenHeader as)

        auth Meta{..} = hdr hAuthorization $ BS.concat
            [ _mAlgorithm
            , " Credential="
            , _mScope
            , ", SignedHeaders="
            , _mSigned
            , ", Signature="
            , _mSignature
            ]


context :: Maybe ByteString
        -> Service (Sv a)
        -> Request a
        -> AuthState
        -> Region
        -> TimeLocale
        -> UTCTime
        -> Signed a V4
context p s@Service{..} Request{..} AuthState{..} r l t = Signed meta rq
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
        & queryString    .~ renderQuery _rqQuery
        & requestHeaders .~ headers
        & requestBody    .~ _rqBody

    meth  = toBS _rqMethod
    host' = toBS (endpoint s r)

    canonicalQuery = renderQuery $ _rqQuery
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
       , _rqPayload
       ]

    scope =
        [ toBS (BasicTime l t)
        , toBS r
        , toBS _svcName
        , "aws4_request"
        ]

    algorithm = "AWS4-HMAC-SHA256"

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
