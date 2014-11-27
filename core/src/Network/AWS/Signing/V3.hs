{-# LANGUAGE FlexibleInstances #-}
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

    -- * Re-exports
    , module Network.AWS.Signing.Internal
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base64       as Base64
import           Data.List                    (sortBy, intersperse)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Request.Internal
import           Network.AWS.Signing.Internal
import           Network.AWS.Types
import           Network.HTTP.Types.Header

data V3

data instance Meta V3 = Meta
    { _mSignature :: ByteString
    , _mTime      :: UTCTime
    }

instance ToBuilder (Meta V3) where
    build Meta{..} = mconcat $ intersperse "\n"
        [ "Version 3 Metadata {"
        , "  signature " <> build _mSignature
        , "  time      " <> build _mTime
        , "}"
        ]

instance AWSSigner V3 where
    signed AuthEnv{..} r x@Request{..} l t = Signed meta rq
      where
        meta = Meta
            { _mSignature = signature
            , _mTime      = t
            }

        rq = clientRequest
            & method         .~ toBS _rqMethod
            & host           .~ _endpointHost
            & path           .~ _rqPath
            & queryString    .~ toBS _rqQuery
            & requestHeaders .~ headers
            & requestBody    .~ _bdyBody _rqBody

        Endpoint{..} = endpoint (serviceOf x) r

        headers = sortBy (comparing fst)
            . hdr hAMZAuth authorisation
            . hdr hHost _endpointHost
            . hdr hDate (toBS (LocaleTime l t :: RFC822))
            $ _rqHeaders
                ++ maybeToList ((hAMZToken,) . toBS <$> _authToken)

        authorisation = "AWS3-HTTPS AWSAccessKeyId="
            <> toBS _authAccess
            <> ", Algorithm=HmacSHA256, Signature="
            <> signature

        signature = Base64.encode
            $ hmacSHA256 (toBS _authSecret) (toBS (LocaleTime l t :: AWSTime))
