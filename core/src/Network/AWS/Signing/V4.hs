{-# LANGUAGE FlexibleInstances #-}
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
    , authorisation

    -- * Re-exports
    , module Network.AWS.Signing.Internal
    ) where

import           Control.Applicative
import           Control.Lens
import           Crypto.Hash                  (digestToHexByteString)
import qualified Crypto.Hash.SHA256           as SHA256
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Char8        as BS
import qualified Data.CaseInsensitive         as CI
import qualified Data.Foldable                as Fold
import           Data.Function
import           Data.List                    (groupBy, intersperse, sortBy, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Request.Internal
import           Network.AWS.Signing.Internal
import           Network.AWS.Types
import           Network.HTTP.Types.Header
import           System.Locale

data V4

data instance Meta V4 = Meta
    { _mAlgorithm :: ByteString
    , _mScope     :: ByteString
    , _mSigned    :: ByteString
    , _mCReq      :: ByteString
    , _mSTS       :: ByteString
    , _mSignature :: ByteString
    , _mTime      :: UTCTime
    }

instance Show (Meta V4) where
    show Meta{..} = BS.unpack $ BS.unlines
        [ "Version 4 Metadata:"
        , "_mAlgorithm " <> _mAlgorithm
        , "_mScope     " <> _mScope
        , "_mSigned    " <> _mSigned
        , "_mCReq      " <> _mCReq
        , "_mSTS       " <> _mSTS
        , "_mSignature " <> _mSignature
        , "_mTime      " <> toBS _mTime
        ]

instance AWSPresigner V4 where
    presigned a r rq l t x = out
        & sgRequest . queryString <>~ auth (out ^. sgMeta)
      where
        out = finalise Nothing qry service a r rq l t

        qry cs sh =
              pair "X-AMZ-Algorithm"      algorithm
            . pair "X-AMZ-Credential"     cs
            . pair "X-AMZ-Date"           (LocaleTime l t :: ISO8601)
            . pair "X-AMZ-Expires"        x
            . pair "X-AMZ-SignedHeaders"  sh
            . pair "X-AMZ-Security-Token" (toBS <$> _authToken a)

        auth = mappend "&X-AMZ-Signature=" . _mSignature

instance AWSSigner V4 where
    signed a r rq l t = out
        & sgRequest
        %~ requestHeaders
        %~ hdr hAuthorization (authorisation $ out ^. sgMeta)
      where
        out = finalise (Just "AWS4") (\_ _ -> id) service a r inp l t

        inp = rq & rqHeaders %~ hdrs (maybeToList tok)

        tok = (hAMZToken,) . toBS <$> _authToken a

authorisation :: Meta V4 -> ByteString
authorisation Meta{..} = BS.concat
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
         -> Service (Sv a)
         -> AuthEnv
         -> Region
         -> Request a
         -> TimeLocale
         -> UTCTime
         -> Signed a V4
finalise p qry s@Service{..} AuthEnv{..} r Request{..} l t = Signed meta rq
  where
    meta = Meta
        { _mAlgorithm = algorithm
        , _mCReq      = canonicalRequest
        , _mScope     = toBS _authAccess <> "/" <> credentialScope
        , _mSigned    = signedHeaders
        , _mSTS       = stringToSign
        , _mSignature = signature
        , _mTime      = t
        }

    rq = clientRequest
        & method         .~ meth
        & host           .~ host'
        & path           .~ _rqPath
        & queryString    .~ toBS query
        & requestHeaders .~ headers
        & requestBody    .~ _bdyBody _rqBody

    meth  = toBS _rqMethod
    host' = toBS (endpoint s r)
    query = qry credentialScope signedHeaders _rqQuery

    canonicalQuery = toBS $ query
        & valuesOf %~ Just . maybe "" (encodeURI True)
        & keysOf   %~ encodeURI False

    headers = sortBy (comparing fst)
        . hdr hHost host'
        . hdr hDate (toBS (LocaleTime l t :: RFC822))
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
       , digestToHexByteString (_bdyHash _rqBody)
       ]

    scope =
        [ toBS (LocaleTime l t :: BasicTime)
        , toBS r
        , toBS _svcPrefix
        , "aws4_request"
        ]

    credentialScope = BS.intercalate "/" scope

    signingKey = Fold.foldl1 hmacSHA256 $
        maybe (toBS _authSecret) (<> toBS _authSecret) p : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , toBS (LocaleTime l t :: AWSTime)
        , credentialScope
        , Base16.encode (SHA256.hash canonicalRequest)
        ]

    signature = Base16.encode (hmacSHA256 signingKey stringToSign)
