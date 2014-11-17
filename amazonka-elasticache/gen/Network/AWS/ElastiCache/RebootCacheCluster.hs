{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      RebootCacheCluster
    -- ** Request constructor
    , rebootCacheCluster
    -- ** Request lenses
    , rccCacheClusterId
    , rccCacheNodeIdsToReboot

    -- * Response
    , RebootCacheClusterResponse
    -- ** Response constructor
    , rebootCacheClusterResponse
    -- ** Response lenses
    , rccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data RebootCacheCluster = RebootCacheCluster
    { _rccCacheClusterId       :: Text
    , _rccCacheNodeIdsToReboot :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebootCacheCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccCacheClusterId' @::@ 'Text'
--
-- * 'rccCacheNodeIdsToReboot' @::@ ['Text']
--
rebootCacheCluster :: Text -- ^ 'rccCacheClusterId'
                   -> RebootCacheCluster
rebootCacheCluster p1 = RebootCacheCluster
    { _rccCacheClusterId       = p1
    , _rccCacheNodeIdsToReboot = mempty
    }

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string.
rccCacheClusterId :: Lens' RebootCacheCluster Text
rccCacheClusterId =
    lens _rccCacheClusterId (\s a -> s { _rccCacheClusterId = a })

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier
-- (0001, 0002, etc.). To reboot an entire cache cluster, specify all of the
-- cache node IDs.
rccCacheNodeIdsToReboot :: Lens' RebootCacheCluster [Text]
rccCacheNodeIdsToReboot =
    lens _rccCacheNodeIdsToReboot (\s a -> s { _rccCacheNodeIdsToReboot = a })

newtype RebootCacheClusterResponse = RebootCacheClusterResponse
    { _rccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'RebootCacheClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
rebootCacheClusterResponse :: RebootCacheClusterResponse
rebootCacheClusterResponse = RebootCacheClusterResponse
    { _rccrCacheCluster = Nothing
    }

rccrCacheCluster :: Lens' RebootCacheClusterResponse (Maybe CacheCluster)
rccrCacheCluster = lens _rccrCacheCluster (\s a -> s { _rccrCacheCluster = a })

instance AWSRequest RebootCacheCluster where
    type Sv RebootCacheCluster = ElastiCache
    type Rs RebootCacheCluster = RebootCacheClusterResponse

    request  = post "RebootCacheCluster"
    response = xmlResponse

instance FromXML RebootCacheClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebootCacheClusterResponse"

instance ToPath RebootCacheCluster where
    toPath = const "/"

instance ToHeaders RebootCacheCluster

instance ToQuery RebootCacheCluster
