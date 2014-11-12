{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RebootCacheCluster operation reboots some, or all, of the cache nodes
-- within a provisioned cache cluster. This API will apply any modified cache
-- parameter groups to the cache cluster. The reboot action takes place as
-- soon as possible, and results in a momentary outage to the cache cluster.
-- During the reboot, the cache cluster status is set to REBOOTING. The reboot
-- causes the contents of the cache (for each cache node being rebooted) to be
-- lost. When the reboot is complete, a cache cluster event is created.
module Network.AWS.ElastiCache.RebootCacheCluster
    (
    -- * Request
      RebootCacheClusterMessage
    -- ** Request constructor
    , rebootCacheClusterMessage
    -- ** Request lenses
    , rccmCacheClusterId
    , rccmCacheNodeIdsToReboot

    -- * Response
    , RebootCacheClusterResult
    -- ** Response constructor
    , rebootCacheClusterResult
    -- ** Response lenses
    , rccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data RebootCacheClusterMessage = RebootCacheClusterMessage
    { _rccmCacheClusterId       :: Text
    , _rccmCacheNodeIdsToReboot :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebootCacheClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccmCacheClusterId' @::@ 'Text'
--
-- * 'rccmCacheNodeIdsToReboot' @::@ ['Text']
--
rebootCacheClusterMessage :: Text -- ^ 'rccmCacheClusterId'
                          -> RebootCacheClusterMessage
rebootCacheClusterMessage p1 = RebootCacheClusterMessage
    { _rccmCacheClusterId       = p1
    , _rccmCacheNodeIdsToReboot = mempty
    }

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string.
rccmCacheClusterId :: Lens' RebootCacheClusterMessage Text
rccmCacheClusterId =
    lens _rccmCacheClusterId (\s a -> s { _rccmCacheClusterId = a })

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cache cluster, specify all of the
-- cache node IDs.
rccmCacheNodeIdsToReboot :: Lens' RebootCacheClusterMessage [Text]
rccmCacheNodeIdsToReboot =
    lens _rccmCacheNodeIdsToReboot
        (\s a -> s { _rccmCacheNodeIdsToReboot = a })

instance ToQuery RebootCacheClusterMessage

instance ToPath RebootCacheClusterMessage where
    toPath = const "/"

newtype RebootCacheClusterResult = RebootCacheClusterResult
    { _rccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'RebootCacheClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
rebootCacheClusterResult :: RebootCacheClusterResult
rebootCacheClusterResult = RebootCacheClusterResult
    { _rccrCacheCluster = Nothing
    }

rccrCacheCluster :: Lens' RebootCacheClusterResult (Maybe CacheCluster)
rccrCacheCluster = lens _rccrCacheCluster (\s a -> s { _rccrCacheCluster = a })

instance FromXML RebootCacheClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebootCacheClusterResult"

instance AWSRequest RebootCacheClusterMessage where
    type Sv RebootCacheClusterMessage = ElastiCache
    type Rs RebootCacheClusterMessage = RebootCacheClusterResult

    request  = post "RebootCacheCluster"
    response = xmlResponse $ \h x -> RebootCacheClusterResult
        <$> x %| "CacheCluster"
