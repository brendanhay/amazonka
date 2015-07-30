{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Sign.V2
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Sign.V2 where

import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8       as BS8
import           Data.List                   (intersperse)
import           Data.Monoid
import           Data.Time
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Crypto
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.Time
import           Network.AWS.Logger
import           Network.AWS.Request
import           Network.AWS.Types
import           Network.HTTP.Types          hiding (toQuery)

data V2

data instance Meta V2 = Meta
    { _mSignature :: ByteString
    , _mTime      :: UTCTime
    }

instance ToLog (Meta V2) where
    message Meta{..} = mconcat $ intersperse "\n"
        [ "[Version 2 Metadata] {"
        , "  signature = " <> message _mSignature
        , "  time      = " <> message _mTime
        , "}"
        ]

instance AWSSigner V2 where
    signed AuthEnv{..} r t Service{..} Request{..} = Signed meta rq
      where
        meta = Meta
            { _mSignature = signature
            , _mTime      = t
            }

        rq = clientRequest
            & method         .~ meth
            & host           .~ _endpointHost
            & path           .~ path'
            & queryString    .~ toBS authorised
            & requestHeaders .~ headers
            & requestBody    .~ bodyRequest _rqBody

        meth  = toBS _rqMethod
        path' = toBS (escapePath _rqPath)

        Endpoint {..} = _svcEndpoint r

        authorised = pair "Signature" (urlEncode True signature) query

        signature = digestToBase Base64
            . hmacSHA256 (toBS _authSecret)
            $ BS8.intercalate "\n"
                [ meth
                , _endpointHost
                , path'
                , toBS query
                ]

        query =
             pair "Version"          _svcVersion
           . pair "SignatureVersion" ("2"          :: ByteString)
           . pair "SignatureMethod"  ("HmacSHA256" :: ByteString)
           . pair "Timestamp"        time
           . pair "AWSAccessKeyId"   (toBS _authAccess)
           $ _rqQuery <> maybe mempty toQuery token

        token = ("SecurityToken" :: ByteString,) . toBS <$> _authToken

        headers = hdr hDate time _rqHeaders

        time = toBS (Time t :: ISO8601)
