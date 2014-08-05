{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RebootCacheCluster operation reboots some, or all, of the cache cluster
-- nodes within a provisioned cache cluster. This API will apply any modified
-- cache parameter groups to the cache cluster. The reboot action takes place
-- as soon as possible, and results in a momentary outage to the cache
-- cluster. During the reboot, the cache cluster status is set to REBOOTING.
-- The reboot causes the contents of the cache (for each cache cluster node
-- being rebooted) to be lost. When the reboot is complete, a cache cluster
-- event is created.
module Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data RebootCacheCluster = RebootCacheCluster
    { _rccmCacheNodeIdsToReboot :: [Text]
      -- ^ A list of cache cluster node IDs to reboot. A node ID is a
      -- numeric identifier (0001, 0002, etc.). To reboot an entire cache
      -- cluster, specify all of the cache cluster node IDs.
    , _rccmCacheClusterId :: Text
      -- ^ The cache cluster identifier. This parameter is stored as a
      -- lowercase string.
    } deriving (Show, Generic)

makeLenses ''RebootCacheCluster

instance ToQuery RebootCacheCluster where
    toQuery = genericToQuery def

data RebootCacheClusterResponse = RebootCacheClusterResponse
    { _ccxCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Show, Generic)

makeLenses ''RebootCacheClusterResponse

instance AWSRequest RebootCacheCluster where
    type Sv RebootCacheCluster = ElastiCache
    type Rs RebootCacheCluster = RebootCacheClusterResponse

    request = post "RebootCacheCluster"
    response _ = cursorResponse $ \hs xml ->
        pure RebootCacheClusterResponse
            <*> xml %|? "CacheCluster"
