{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Sign.V4
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V4 where

import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import qualified Data.Foldable               as Fold
import           Data.Function               hiding ((&))
import           Data.List                   (groupBy, intersperse, sort,
                                              sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Time
import           Network.AWS.Request
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
        , "  string to sign    = " <> build _mSTS
        , "  signature         = " <> build _mSignature
        , "  time              = " <> build _mTime
        , "  canonical request = {"
        , build _mCReq
        , "  }"
        , "}"
        ]

instance AWSPresigner V4 where
    presigned a r t ex svc rq =
        out & sgRequest . queryString <>~ auth (out ^. sgMeta)
      where
        out = finalise a r t svc inp qry hashed

        inp = rq & rqHeaders .~ []

        qry cs sh =
              pair (CI.original hAMZAlgorithm)     algorithm
            . pair (CI.original hAMZCredential)    cs
            . pair (CI.original hAMZDate)          (Time t :: AWSTime)
            . pair (CI.original hAMZExpires)       ex
            . pair (CI.original hAMZSignedHeaders) sh
            . pair (CI.original hAMZToken)         (toBS <$> _authToken a)

        auth   = mappend "&X-Amz-Signature=" . _mSignature
        hashed = "UNSIGNED-PAYLOAD"

instance AWSSigner V4 where
    signed a r t svc rq = out & sgRequest
        %~ requestHeaders
        %~ hdr hAuthorization (authorisation $ out ^. sgMeta)
      where
        out = finalise a r t svc inp (\_ _ -> id) hashed

        inp = rq & rqHeaders %~ hdr hAMZDate date . hdrs (maybeToList tok)

        date   = toBS (Time t :: AWSTime)
        tok    = (hAMZToken,) . toBS <$> _authToken a
        hashed = rq ^. rqBody . to bodySHA256

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

finalise :: AuthEnv
         -> Region
         -> UTCTime
         -> Service s
         -> Request a
         -> (ByteString -> ByteString -> QueryString -> QueryString)
         -> ByteString
         -> Signed V4 a
finalise AuthEnv{..} r t Service{..} Request{..} qry hashed =
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
        & requestBody    .~ bodyRequest _rqBody

    meth  = toBS _rqMethod
    query = qry accessScope signedHeaders _rqQuery

    Endpoint {..} = _svcEndpoint r

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
       , hashed
       ]

    scope =
        [ toBS (Time t :: BasicTime)
        , toBS _endpointScope
        , toBS _svcPrefix
        , "aws4_request"
        ]

    credentialScope = BS.intercalate "/" scope

    accessScope = toBS _authAccess <> "/" <> credentialScope

    signingKey = Fold.foldl1 (\k -> digestToBS . hmacSHA256 k) $
        ("AWS4" <> toBS _authSecret) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , toBS (Time t :: AWSTime)
        , credentialScope
        , digestToBase Base16 (hashSHA256 canonicalRequest)
        ]

    signature = digestToBase Base16 (hmacSHA256 signingKey stringToSign)
