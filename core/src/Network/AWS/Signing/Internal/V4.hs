{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Internal.V4
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Internal.V4
    ( V4
    ) where

import           Control.Applicative
import           Control.Lens
import qualified Crypto.Hash.SHA256           as SHA256
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base16       as Base16
import qualified Data.ByteString.Char8        as BS
import qualified Data.CaseInsensitive         as CI
import qualified Data.Foldable                as Fold
import           Data.Function                hiding ((&))
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

instance ToBuilder (Meta V4) where
    build Meta{..} = mconcat $ intersperse "\n"
        [ "[Version 4 Metadata] {"
        , "  algorithm         = " <> build _mAlgorithm
        , "  credential scope  = " <> build _mScope
        , "  signed headers    = " <> build _mSigned
        , "  canonical request = {"
        , build _mCReq
        , "  }"
        , "  string to sign    = " <> build _mSTS
        , "  signature         = " <> build _mSignature
        , "  time              = " <> build _mTime
        , "}"
        ]

instance AWSPresigner V4 where
    presigned a r rq t ex = out & sgRequest
        . queryString <>~ auth (out ^. sgMeta)
      where
        out = finalise qry hash r service a inp t

        qry cs sh =
              pair (CI.original hAMZAlgorithm)     algorithm
            . pair (CI.original hAMZCredential)    cs
            . pair (CI.original hAMZDate)          (Time t :: AWSTime)
            . pair (CI.original hAMZExpires)       ex
            . pair (CI.original hAMZSignedHeaders) sh
            . pair (CI.original hAMZToken)         (toBS <$> _authToken a)

        inp = rq & rqHeaders .~ []

        auth = mappend "&X-Amz-Signature=" . _mSignature

        hash = "UNSIGNED-PAYLOAD"

instance AWSSigner V4 where
    signed a r rq t = out & sgRequest
        %~ requestHeaders
        %~ hdr hAuthorization (authorisation $ out ^. sgMeta)
      where
        out = finalise (\_ _ -> id) hash r service a inp t

        inp = rq & rqHeaders %~ hdr hAMZDate date . hdrs (maybeToList tok)

        date = toBS (Time t :: AWSTime)
        tok  = (hAMZToken,) . toBS <$> _authToken a
        hash = bodyHash (_rqBody rq)

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

finalise :: (ByteString -> ByteString -> Query -> Query)
         -> ByteString
         -> Region
         -> Service (Sv a)
         -> AuthEnv
         -> Request a
         -> UTCTime
         -> Signed a V4
finalise qry hash r s@Service{..} AuthEnv{..} Request{..} t =
    Signed meta rq
  where
    meta = Meta
        { _mAlgorithm = algorithm
        , _mCReq      = canonicalRequest
        , _mScope     = accessScope
        , _mSigned    = signedHeaders
        , _mSTS       = stringToSign
        , _mSignature = signature
        , _mTime      = t
        }

    rq = clientRequest
        & method         .~ meth
        & host           .~ _endpointHost
        & path           .~ _rqPath
        & queryString    .~ BS.cons '?' (toBS query)
        & requestHeaders .~ headers
        & requestBody    .~ _bdyBody _rqBody

    meth  = toBS _rqMethod
    query = qry accessScope signedHeaders _rqQuery

    Endpoint{..} = endpoint s r

    canonicalQuery = toBS (query & valuesOf %~ Just . fromMaybe "")

    headers = sortBy (comparing fst) (hdr hHost _endpointHost _rqHeaders)

    joinedHeaders = map f $ groupBy ((==) `on` fst) headers
      where
        f []     = ("", "")
        f (h:hs) = (fst h, g (h : hs))

        g = BS.intercalate "," . sort . map snd

    signedHeaders = mconcat
        . intersperse ";"
        . map (CI.foldedCase . fst)
        $ joinedHeaders

    canonicalHeaders = Fold.foldMap f joinedHeaders
      where
        f (k, v) = CI.foldedCase k
            <> ":"
            <> stripBS v
            <> "\n"

    canonicalRequest = mconcat $ intersperse "\n"
       [ meth
       , collapsePath _rqPath
       , canonicalQuery
       , canonicalHeaders
       , signedHeaders
       , hash
       ]

    scope =
        [ toBS (Time t :: BasicTime)
        , toBS _endpointScope
        , toBS _svcPrefix
        , "aws4_request"
        ]

    credentialScope = BS.intercalate "/" scope

    accessScope = toBS _authAccess <> "/" <> credentialScope

    signingKey = Fold.foldl1 hmacSHA256 $ ("AWS4" <> toBS _authSecret) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , toBS (Time t :: AWSTime)
        , credentialScope
        , Base16.encode (SHA256.hash canonicalRequest)
        ]

    signature = Base16.encode (hmacSHA256 signingKey stringToSign)
