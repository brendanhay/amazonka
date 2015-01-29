{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.ResetCache
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation resets all cache disks and makes the disks available for
-- reconfiguration as cache storage. When a cache is reset, the gateway loses
-- its cache storage. At this point you can reconfigure the disks as cache
-- disks.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ResetCache.html>
module Network.AWS.StorageGateway.ResetCache
    (
    -- * Request
      ResetCache
    -- ** Request constructor
    , resetCache
    -- ** Request lenses
    , rcGatewayARN

    -- * Response
    , ResetCacheResponse
    -- ** Response constructor
    , resetCacheResponse
    -- ** Response lenses
    , rcrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype ResetCache = ResetCache
    { _rcGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'ResetCache' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcGatewayARN' @::@ 'Text'
--
resetCache :: Text -- ^ 'rcGatewayARN'
           -> ResetCache
resetCache p1 = ResetCache
    { _rcGatewayARN = p1
    }

rcGatewayARN :: Lens' ResetCache Text
rcGatewayARN = lens _rcGatewayARN (\s a -> s { _rcGatewayARN = a })

newtype ResetCacheResponse = ResetCacheResponse
    { _rcrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ResetCacheResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrGatewayARN' @::@ 'Maybe' 'Text'
--
resetCacheResponse :: ResetCacheResponse
resetCacheResponse = ResetCacheResponse
    { _rcrGatewayARN = Nothing
    }

rcrGatewayARN :: Lens' ResetCacheResponse (Maybe Text)
rcrGatewayARN = lens _rcrGatewayARN (\s a -> s { _rcrGatewayARN = a })

instance ToPath ResetCache where
    toPath = const "/"

instance ToQuery ResetCache where
    toQuery = const mempty

instance ToHeaders ResetCache

instance ToJSON ResetCache where
    toJSON ResetCache{..} = object
        [ "GatewayARN" .= _rcGatewayARN
        ]

instance AWSRequest ResetCache where
    type Sv ResetCache = StorageGateway
    type Rs ResetCache = ResetCacheResponse

    request  = post "ResetCache"
    response = jsonResponse

instance FromJSON ResetCacheResponse where
    parseJSON = withObject "ResetCacheResponse" $ \o -> ResetCacheResponse
        <$> o .:? "GatewayARN"
