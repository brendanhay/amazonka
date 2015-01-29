{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signaing.Internal.V2
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Internal.V2
    ( V2
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as BS
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Request.Internal
import           Network.AWS.Signing.Internal
import           Network.AWS.Types
import           Network.HTTP.Types           hiding (toQuery)

data V2

data instance Meta V2 = Meta
    { _mSignature :: ByteString
    , _mTime      :: UTCTime
    }

instance ToBuilder (Meta V2) where
    build Meta{..} = mconcat $ intersperse "\n"
        [ "[Version 2 Metadata] {"
        , "  signature = " <> build _mSignature
        , "  time      = " <> build _mTime
        , "}"
        ]

instance AWSSigner V2 where
    signed AuthEnv{..} r x@Request{..} t = Signed meta rq
      where
        meta = Meta
            { _mSignature = signature
            , _mTime      = t
            }

        rq = clientRequest
            & method         .~ meth
            & host           .~ _endpointHost
            & path           .~ _rqPath
            & queryString    .~ toBS authorised
            & requestHeaders .~ headers
            & requestBody    .~ _bdyBody _rqBody

        meth = toBS _rqMethod

        Endpoint{..} = endpoint svc r

        authorised = pair "Signature" (urlEncode True signature) query

        signature = Base64.encode
            . hmacSHA256 (toBS _authSecret)
            $ BS.intercalate "\n"
                [ meth
                , _endpointHost
                , _rqPath
                , toBS query
                ]

        query =
             pair "Version"          (_svcVersion svc)
           . pair "SignatureVersion" ("2"          :: ByteString)
           . pair "SignatureMethod"  ("HmacSHA256" :: ByteString)
           . pair "Timestamp"        time
           . pair "AWSAccessKeyId"   (toBS _authAccess)
           $ _rqQuery <> maybe mempty toQuery token

        token = ("SecurityToken" :: ByteString,) . toBS <$> _authToken

        headers = hdr hDate time _rqHeaders

        time = toBS (Time t :: ISO8601)

        svc = serviceOf x
