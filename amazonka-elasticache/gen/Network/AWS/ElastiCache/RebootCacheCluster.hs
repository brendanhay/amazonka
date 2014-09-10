{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache
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
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=RebootCacheCluster
-- &CacheClusterId=mycache &CacheNodeIdsToReboot.member.1=0001
-- &CacheNodeIdsToReboot.member.2=0002 &CacheNodeIdsToReboot.member.3=0003
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= rebooting cache cluster
-- nodes default.memcached1.4 in-sync mycache 11211
-- mycache.q68zge.cfg.use1devo.elmo-dev.amazonaws.com cache.m1.small memcached
-- us-east-1b 2014-04-01T19:04:12.812Z 1.4.17 true wed:09:00-wed:10:00
-- https://console.aws.amazon.com/elasticache/home#client-download: default
-- active 3 cf7e6fc4-b9d1-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache
    (
    -- * Request
      RebootCacheCluster
    -- ** Request constructor
    , mkRebootCacheCluster
    -- ** Request lenses
    , rccCacheClusterId
    , rccCacheNodeIdsToReboot

    -- * Response
    , RebootCacheClusterResponse
    -- ** Response constructor
    , mkRebootCacheClusterResponse
    -- ** Response lenses
    , rccrCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a RebootCacheCluster operation.
data RebootCacheCluster = RebootCacheCluster
    { _rccCacheClusterId :: Text
    , _rccCacheNodeIdsToReboot :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebootCacheCluster' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Text@
--
-- * @CacheNodeIdsToReboot ::@ @[Text]@
--
mkRebootCacheCluster :: Text -- ^ 'rccCacheClusterId'
                     -> [Text] -- ^ 'rccCacheNodeIdsToReboot'
                     -> RebootCacheCluster
mkRebootCacheCluster p1 p2 = RebootCacheCluster
    { _rccCacheClusterId = p1
    , _rccCacheNodeIdsToReboot = p2
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
    lens _rccCacheNodeIdsToReboot
         (\s a -> s { _rccCacheNodeIdsToReboot = a })

instance ToQuery RebootCacheCluster where
    toQuery = genericQuery def

newtype RebootCacheClusterResponse = RebootCacheClusterResponse
    { _rccrCacheCluster :: Maybe CacheCluster
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebootCacheClusterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheCluster ::@ @Maybe CacheCluster@
--
mkRebootCacheClusterResponse :: RebootCacheClusterResponse
mkRebootCacheClusterResponse = RebootCacheClusterResponse
    { _rccrCacheCluster = Nothing
    }

-- | Contains all of the attributes of a specific cache cluster.
rccrCacheCluster :: Lens' RebootCacheClusterResponse (Maybe CacheCluster)
rccrCacheCluster =
    lens _rccrCacheCluster (\s a -> s { _rccrCacheCluster = a })

instance FromXML RebootCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RebootCacheCluster where
    type Sv RebootCacheCluster = ElastiCache
    type Rs RebootCacheCluster = RebootCacheClusterResponse

    request = post "RebootCacheCluster"
    response _ = xmlResponse
