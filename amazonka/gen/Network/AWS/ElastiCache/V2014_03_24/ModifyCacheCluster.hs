{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheCluster operation modifies the settings for a cache cluster.
-- You can use this operation to change one or more cluster configuration
-- parameters by specifying the parameters and the new values.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=ModifyCacheCluster
-- &NumCacheNodes=5 &CacheClusterId=simcoprod01 &Version=2014-03-24
-- &ApplyImmediately=true &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T03%3A16%3A34.601Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE in-sync default.memcached1.4 simcoprod01
-- available 11211 simcoprod01.m2st2p.cfg.cache.amazonaws.com cache.m1.large
-- memcached 5 us-east-1b 2014-03-26T23:45:20.937Z 1.4.5 true
-- fri:04:30-fri:05:00 default active 3 d5786c6d-b7fe-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCacheCluster' request.
modifyCacheCluster :: Text -- ^ '_mccmCacheClusterId'
                   -> ModifyCacheCluster
modifyCacheCluster p1 = ModifyCacheCluster
    { _mccmCacheClusterId = p1
    , _mccmApplyImmediately = Nothing
    , _mccmAutoMinorVersionUpgrade = Nothing
    , _mccmCacheNodeIdsToRemove = mempty
    , _mccmCacheSecurityGroupNames = mempty
    , _mccmSnapshotRetentionLimit = Nothing
    , _mccmNumCacheNodes = Nothing
    , _mccmSecurityGroupIds = mempty
    , _mccmEngineVersion = Nothing
    , _mccmCacheParameterGroupName = Nothing
    , _mccmSnapshotWindow = Nothing
    , _mccmPreferredMaintenanceWindow = Nothing
    , _mccmNotificationTopicStatus = Nothing
    , _mccmNotificationTopicArn = Nothing
    }

data ModifyCacheCluster = ModifyCacheCluster
    { _mccmCacheClusterId :: Text
      -- ^ The cache cluster identifier. This value is stored as a lowercase
      -- string.
    , _mccmApplyImmediately :: Maybe Bool
      -- ^ If true, this parameter causes the modifications in this request
      -- and any pending modifications to be applied, asynchronously and
      -- as soon as possible, regardless of the PreferredMaintenanceWindow
      -- setting for the cache cluster. If false, then changes to the
      -- cache cluster are applied on the next maintenance reboot, or the
      -- next failure reboot, whichever occurs first. Valid values: true |
      -- false Default: false.
    , _mccmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor engine upgrades will be applied automatically
      -- to the cache cluster during the maintenance window. Valid values:
      -- true | false Default: true.
    , _mccmCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs to be removed. A node ID is a numeric
      -- identifier (0001, 0002, etc.). This parameter is only valid when
      -- NumCacheNodes is less than the existing number of cache nodes.
      -- The number of cache node IDs supplied in this parameter must
      -- match the difference between the existing number of cache nodes
      -- in the cluster and the value of NumCacheNodes in the request.
    , _mccmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to authorize on this cache
      -- cluster. This change is asynchronously applied as soon as
      -- possible. This parameter can be used only with clusters that are
      -- created outside of an Amazon Virtual Private Cloud (VPC).
      -- Constraints: Must contain no more than 255 alphanumeric
      -- characters. Must not be "Default".
    , _mccmSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted.
    , _mccmNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes that the cache cluster should have. If
      -- the value for NumCacheNodes is greater than the existing number
      -- of cache nodes, then more nodes will be added. If the value is
      -- less than the existing number of cache nodes, then cache nodes
      -- will be removed. If you are removing cache nodes, you must use
      -- the CacheNodeIdsToRemove parameter to provide the IDs of the
      -- specific cache nodes to be removed.
    , _mccmSecurityGroupIds :: [Text]
      -- ^ Specifies the VPC Security Groups associated with the cache
      -- cluster. This parameter can be used only with clusters that are
      -- created in an Amazon Virtual Private Cloud (VPC).
    , _mccmEngineVersion :: Maybe Text
      -- ^ The upgraded version of the cache engine to be run on the cache
      -- cluster nodes.
    , _mccmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to apply to this cache
      -- cluster. This change is asynchronously applied as soon as
      -- possible for parameters when the ApplyImmediately parameter is
      -- specified as true for this request.
    , _mccmSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of your cache cluster.
    , _mccmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur. Note that system maintenance may result in an outage.
      -- This change is made immediately. If you are moving this window to
      -- the current time, there must be at least 120 minutes between the
      -- current time and end of the window to ensure that pending changes
      -- are applied.
    , _mccmNotificationTopicStatus :: Maybe Text
      -- ^ The status of the Amazon SNS notification topic. Notifications
      -- are sent only if the status is active. Valid values: active |
      -- inactive.
    , _mccmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic to which
      -- notifications will be sent. The SNS topic owner must be same as
      -- the cache cluster owner.
    } deriving (Generic)

makeLenses ''ModifyCacheCluster

instance ToQuery ModifyCacheCluster where
    toQuery = genericToQuery def

data ModifyCacheClusterResponse = ModifyCacheClusterResponse
    { _cczCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Generic)

makeLenses ''ModifyCacheClusterResponse

instance FromXML ModifyCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheCluster where
    type Sv ModifyCacheCluster = ElastiCache
    type Rs ModifyCacheCluster = ModifyCacheClusterResponse

    request = post "ModifyCacheCluster"
    response _ = xmlResponse
