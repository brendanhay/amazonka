{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.ModifyCacheCluster
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
-- &NumCacheNodes=5 &CacheClusterId=simcoprod01 &ApplyImmediately=true
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= in-sync default.memcached1.4
-- simcoprod01 available 11211 simcoprod01.m2st2p.cfg.cache.amazonaws.com
-- cache.m1.large memcached 5 us-east-1b 2014-03-26T23:45:20.937Z 1.4.5 true
-- fri:04:30-fri:05:00 default active 3 d5786c6d-b7fe-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheCluster
    (
    -- * Request
      ModifyCacheCluster
    -- ** Request constructor
    , mkModifyCacheClusterMessage
    -- ** Request lenses
    , mccmCacheClusterId
    , mccmNumCacheNodes
    , mccmCacheNodeIdsToRemove
    , mccmCacheSecurityGroupNames
    , mccmSecurityGroupIds
    , mccmPreferredMaintenanceWindow
    , mccmNotificationTopicArn
    , mccmCacheParameterGroupName
    , mccmNotificationTopicStatus
    , mccmApplyImmediately
    , mccmEngineVersion
    , mccmAutoMinorVersionUpgrade
    , mccmSnapshotRetentionLimit
    , mccmSnapshotWindow
    , mccmAZMode
    , mccmNewAvailabilityZones

    -- * Response
    , ModifyCacheClusterResponse
    -- ** Response lenses
    , ccyCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheCluster' request.
mkModifyCacheClusterMessage :: Text -- ^ 'mccmCacheClusterId'
                            -> ModifyCacheCluster
mkModifyCacheClusterMessage p1 = ModifyCacheCluster
    { _mccmCacheClusterId = p1
    , _mccmNumCacheNodes = Nothing
    , _mccmCacheNodeIdsToRemove = mempty
    , _mccmCacheSecurityGroupNames = mempty
    , _mccmSecurityGroupIds = mempty
    , _mccmPreferredMaintenanceWindow = Nothing
    , _mccmNotificationTopicArn = Nothing
    , _mccmCacheParameterGroupName = Nothing
    , _mccmNotificationTopicStatus = Nothing
    , _mccmApplyImmediately = Nothing
    , _mccmEngineVersion = Nothing
    , _mccmAutoMinorVersionUpgrade = Nothing
    , _mccmSnapshotRetentionLimit = Nothing
    , _mccmSnapshotWindow = Nothing
    , _mccmAZMode = Nothing
    , _mccmNewAvailabilityZones = mempty
    }
{-# INLINE mkModifyCacheClusterMessage #-}

data ModifyCacheCluster = ModifyCacheCluster
    { _mccmCacheClusterId :: Text
      -- ^ The cache cluster identifier. This value is stored as a lowercase
      -- string.
    , _mccmNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes that the cache cluster should have. If
      -- the value for NumCacheNodes is greater than the sum of the number
      -- of current cache nodes and the number of cache nodes pending
      -- creation (which may be zero), then more nodes will be added. If
      -- the value is less than the number of existing cache nodes, then
      -- nodes will be removed. If the value is equal to the number of
      -- current cache nodes, then any pending add or remove requests are
      -- canceled. If you are removing cache nodes, you must use the
      -- CacheNodeIdsToRemove parameter to provide the IDs of the specific
      -- cache nodes to remove. For cache clusters running Redis, the
      -- value of NumCacheNodesmust be 1. Note:Adding or removing
      -- Memcached cache nodes can be applied immediately or as a pending
      -- action. See ApplyImmediately. A pending action to modify the
      -- number of cache nodes in a cluster during its maintenance window,
      -- whether by adding or removing nodes in accordance with the scale
      -- out architecture, is not queued. The customer's latest request to
      -- add or remove nodes to the cluster overrides any previous pending
      -- actions to modify the number of cache nodes in the cluster. For
      -- example, a request to remove 2 nodes would override a previous
      -- pending action to remove 3 nodes. Similarly, a request to add 2
      -- nodes would override a previous pending action to remove 3 nodes
      -- and vice versa. As Memcached cache nodes may now be provisioned
      -- in different Availability Zones with flexible cache node
      -- placement, a request to add nodes does not automatically override
      -- a previous pending action to add nodes. The customer can modify
      -- the previous pending action to add more nodes or explicitly
      -- cancel the pending request and retry the new request. To cancel
      -- pending actions to modify the number of cache nodes in a cluster,
      -- use the ModifyCacheCluster request and set NumCacheNodes equal to
      -- the number of cache nodes currently in the cache cluster.
    , _mccmCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs to be removed. A node ID is a numeric
      -- identifier (0001, 0002, etc.). This parameter is only valid when
      -- NumCacheNodes is less than the existing number of cache nodes.
      -- The number of cache node IDs supplied in this parameter must
      -- match the difference between the existing number of cache nodes
      -- in the cluster or pending cache nodes, whichever is greater, and
      -- the value of NumCacheNodes in the request. For example: If you
      -- have 3 active cache nodes, 7 pending cache nodes, and the number
      -- of cache nodes in this ModifyCacheCluser call is 5, you must list
      -- 2 (7 - 5) cache node IDs to remove.
    , _mccmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to authorize on this cache
      -- cluster. This change is asynchronously applied as soon as
      -- possible. This parameter can be used only with clusters that are
      -- created outside of an Amazon Virtual Private Cloud (VPC).
      -- Constraints: Must contain no more than 255 alphanumeric
      -- characters. Must not be "Default".
    , _mccmSecurityGroupIds :: [Text]
      -- ^ Specifies the VPC Security Groups associated with the cache
      -- cluster. This parameter can be used only with clusters that are
      -- created in an Amazon Virtual Private Cloud (VPC).
    , _mccmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur. Note that system maintenance may result in an outage.
      -- This change is made immediately. If you are moving this window to
      -- the current time, there must be at least 120 minutes between the
      -- current time and end of the window to ensure that pending changes
      -- are applied.
    , _mccmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to which
      -- notifications will be sent. The Amazon SNS topic owner must be
      -- same as the cache cluster owner.
    , _mccmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to apply to this cache
      -- cluster. This change is asynchronously applied as soon as
      -- possible for parameters when the ApplyImmediately parameter is
      -- specified as true for this request.
    , _mccmNotificationTopicStatus :: Maybe Text
      -- ^ The status of the Amazon SNS notification topic. Notifications
      -- are sent only if the status is active. Valid values: active |
      -- inactive.
    , _mccmApplyImmediately :: Maybe Bool
      -- ^ If true, this parameter causes the modifications in this request
      -- and any pending modifications to be applied, asynchronously and
      -- as soon as possible, regardless of the PreferredMaintenanceWindow
      -- setting for the cache cluster. If false, then changes to the
      -- cache cluster are applied on the next maintenance reboot, or the
      -- next failure reboot, whichever occurs first. If you perform a
      -- ModifyCacheCluster before a pending modification is applied, the
      -- pending modification is replaced by the newer modification. Valid
      -- values: true | false Default: false.
    , _mccmEngineVersion :: Maybe Text
      -- ^ The upgraded version of the cache engine to be run on the cache
      -- nodes.
    , _mccmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor engine upgrades will be applied automatically
      -- to the cache cluster during the maintenance window. Valid values:
      -- true | false Default: true.
    , _mccmSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted.
      -- ImportantIf the value of SnapshotRetentionLimit is set to zero
      -- (0), backups are turned off.
    , _mccmSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of your cache cluster.
    , _mccmAZMode :: Maybe Text
      -- ^ Specifies whether the new nodes in this Memcached cache cluster
      -- are all created in a single Availability Zone or created across
      -- multiple Availability Zones. Valid values: single-az | cross-az.
      -- This option is only supported for Memcached cache clusters. You
      -- cannot specify single-az if the Memcached cache cluster already
      -- has cache nodes in different Availability Zones. If cross-az is
      -- specified, existing Memcached nodes remain in their current
      -- Availability Zone. Only newly created nodes will be located in
      -- different Availability Zones. For instructions on how to move
      -- existing Memcached nodes to different Availability Zones, see the
      -- Availability Zone Considerations section of Cache Node
      -- Considerations for Memcached.
    , _mccmNewAvailabilityZones :: [Text]
      -- ^ The list of Availability Zones where the new Memcached cache
      -- nodes will be created. This parameter is only valid when
      -- NumCacheNodes in the request is greater than the sum of the
      -- number of active cache nodes and the number of cache nodes
      -- pending creation (which may be zero). The number of Availability
      -- Zones supplied in this list must match the cache nodes being
      -- added in this request. This option is only supported on Memcached
      -- clusters. Scenarios: Scenario 1: You have 3 active nodes and wish
      -- to add 2 nodes. Specify NumCacheNodes=5 (3 + 2) and opitonally
      -- specify two Availability Zones for the two new nodes. Scenario 2:
      -- You have 3 active nodes and 2 nodes pending creation (from the
      -- scenario 1 call) and want to add 1 more node. Specify
      -- NumCacheNodes=6 ((3 + 2) + 1) and optionally specify an
      -- Availability Zone for the new node. Scenario 3: You want to
      -- cancel all pending actions. You want to cancel all pending
      -- actions. Specify NumCacheNodes=3 (the original number of nodes).
      -- You cannot cancel just some pending operations. If you want to
      -- cancel a subset of the pending operations, cancel all of them,
      -- then re-request those you want. The Availability Zone placement
      -- of nodes pending creation cannot be modified. If you wish to
      -- cancel any nodes pending creation, add 0 nodes by setting
      -- NumCacheNodes to the number of current nodes. If cross-az is
      -- specified, existing Memcached nodes remain in their current
      -- Availability Zone. Only newly created nodes can be located in
      -- different Availability Zones. For guidance on how to move
      -- existing Memcached nodes to different Availability Zones, see the
      -- Availability Zone Considerations section of Cache Node
      -- Considerations for Memcached. Example:
      -- NewAvailabilityZones.member.1=us-east-1a&amp;NewAvailabilityZones.member.2=us-east-1b&amp;NewAvailabilityZones.member.3=us-east-1d.
      -- 
    } deriving (Show, Generic)

-- | The cache cluster identifier. This value is stored as a lowercase string.
mccmCacheClusterId :: Lens' ModifyCacheCluster (Text)
mccmCacheClusterId = lens _mccmCacheClusterId (\s a -> s { _mccmCacheClusterId = a })
{-# INLINE mccmCacheClusterId #-}

-- | The number of cache nodes that the cache cluster should have. If the value
-- for NumCacheNodes is greater than the sum of the number of current cache
-- nodes and the number of cache nodes pending creation (which may be zero),
-- then more nodes will be added. If the value is less than the number of
-- existing cache nodes, then nodes will be removed. If the value is equal to
-- the number of current cache nodes, then any pending add or remove requests
-- are canceled. If you are removing cache nodes, you must use the
-- CacheNodeIdsToRemove parameter to provide the IDs of the specific cache
-- nodes to remove. For cache clusters running Redis, the value of
-- NumCacheNodesmust be 1. Note:Adding or removing Memcached cache nodes can
-- be applied immediately or as a pending action. See ApplyImmediately. A
-- pending action to modify the number of cache nodes in a cluster during its
-- maintenance window, whether by adding or removing nodes in accordance with
-- the scale out architecture, is not queued. The customer's latest request to
-- add or remove nodes to the cluster overrides any previous pending actions
-- to modify the number of cache nodes in the cluster. For example, a request
-- to remove 2 nodes would override a previous pending action to remove 3
-- nodes. Similarly, a request to add 2 nodes would override a previous
-- pending action to remove 3 nodes and vice versa. As Memcached cache nodes
-- may now be provisioned in different Availability Zones with flexible cache
-- node placement, a request to add nodes does not automatically override a
-- previous pending action to add nodes. The customer can modify the previous
-- pending action to add more nodes or explicitly cancel the pending request
-- and retry the new request. To cancel pending actions to modify the number
-- of cache nodes in a cluster, use the ModifyCacheCluster request and set
-- NumCacheNodes equal to the number of cache nodes currently in the cache
-- cluster.
mccmNumCacheNodes :: Lens' ModifyCacheCluster (Maybe Integer)
mccmNumCacheNodes = lens _mccmNumCacheNodes (\s a -> s { _mccmNumCacheNodes = a })
{-# INLINE mccmNumCacheNodes #-}

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier
-- (0001, 0002, etc.). This parameter is only valid when NumCacheNodes is less
-- than the existing number of cache nodes. The number of cache node IDs
-- supplied in this parameter must match the difference between the existing
-- number of cache nodes in the cluster or pending cache nodes, whichever is
-- greater, and the value of NumCacheNodes in the request. For example: If you
-- have 3 active cache nodes, 7 pending cache nodes, and the number of cache
-- nodes in this ModifyCacheCluser call is 5, you must list 2 (7 - 5) cache
-- node IDs to remove.
mccmCacheNodeIdsToRemove :: Lens' ModifyCacheCluster ([Text])
mccmCacheNodeIdsToRemove = lens _mccmCacheNodeIdsToRemove (\s a -> s { _mccmCacheNodeIdsToRemove = a })
{-# INLINE mccmCacheNodeIdsToRemove #-}

-- | A list of cache security group names to authorize on this cache cluster.
-- This change is asynchronously applied as soon as possible. This parameter
-- can be used only with clusters that are created outside of an Amazon
-- Virtual Private Cloud (VPC). Constraints: Must contain no more than 255
-- alphanumeric characters. Must not be "Default".
mccmCacheSecurityGroupNames :: Lens' ModifyCacheCluster ([Text])
mccmCacheSecurityGroupNames = lens _mccmCacheSecurityGroupNames (\s a -> s { _mccmCacheSecurityGroupNames = a })
{-# INLINE mccmCacheSecurityGroupNames #-}

-- | Specifies the VPC Security Groups associated with the cache cluster. This
-- parameter can be used only with clusters that are created in an Amazon
-- Virtual Private Cloud (VPC).
mccmSecurityGroupIds :: Lens' ModifyCacheCluster ([Text])
mccmSecurityGroupIds = lens _mccmSecurityGroupIds (\s a -> s { _mccmSecurityGroupIds = a })
{-# INLINE mccmSecurityGroupIds #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Note that system maintenance may result in an outage. This change is made
-- immediately. If you are moving this window to the current time, there must
-- be at least 120 minutes between the current time and end of the window to
-- ensure that pending changes are applied.
mccmPreferredMaintenanceWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccmPreferredMaintenanceWindow = lens _mccmPreferredMaintenanceWindow (\s a -> s { _mccmPreferredMaintenanceWindow = a })
{-# INLINE mccmPreferredMaintenanceWindow #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent. The Amazon SNS topic owner must be same as the
-- cache cluster owner.
mccmNotificationTopicArn :: Lens' ModifyCacheCluster (Maybe Text)
mccmNotificationTopicArn = lens _mccmNotificationTopicArn (\s a -> s { _mccmNotificationTopicArn = a })
{-# INLINE mccmNotificationTopicArn #-}

-- | The name of the cache parameter group to apply to this cache cluster. This
-- change is asynchronously applied as soon as possible for parameters when
-- the ApplyImmediately parameter is specified as true for this request.
mccmCacheParameterGroupName :: Lens' ModifyCacheCluster (Maybe Text)
mccmCacheParameterGroupName = lens _mccmCacheParameterGroupName (\s a -> s { _mccmCacheParameterGroupName = a })
{-# INLINE mccmCacheParameterGroupName #-}

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active. Valid values: active | inactive.
mccmNotificationTopicStatus :: Lens' ModifyCacheCluster (Maybe Text)
mccmNotificationTopicStatus = lens _mccmNotificationTopicStatus (\s a -> s { _mccmNotificationTopicStatus = a })
{-# INLINE mccmNotificationTopicStatus #-}

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- cache cluster. If false, then changes to the cache cluster are applied on
-- the next maintenance reboot, or the next failure reboot, whichever occurs
-- first. If you perform a ModifyCacheCluster before a pending modification is
-- applied, the pending modification is replaced by the newer modification.
-- Valid values: true | false Default: false.
mccmApplyImmediately :: Lens' ModifyCacheCluster (Maybe Bool)
mccmApplyImmediately = lens _mccmApplyImmediately (\s a -> s { _mccmApplyImmediately = a })
{-# INLINE mccmApplyImmediately #-}

-- | The upgraded version of the cache engine to be run on the cache nodes.
mccmEngineVersion :: Lens' ModifyCacheCluster (Maybe Text)
mccmEngineVersion = lens _mccmEngineVersion (\s a -> s { _mccmEngineVersion = a })
{-# INLINE mccmEngineVersion #-}

-- | If true, then minor engine upgrades will be applied automatically to the
-- cache cluster during the maintenance window. Valid values: true | false
-- Default: true.
mccmAutoMinorVersionUpgrade :: Lens' ModifyCacheCluster (Maybe Bool)
mccmAutoMinorVersionUpgrade = lens _mccmAutoMinorVersionUpgrade (\s a -> s { _mccmAutoMinorVersionUpgrade = a })
{-# INLINE mccmAutoMinorVersionUpgrade #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mccmSnapshotRetentionLimit :: Lens' ModifyCacheCluster (Maybe Integer)
mccmSnapshotRetentionLimit = lens _mccmSnapshotRetentionLimit (\s a -> s { _mccmSnapshotRetentionLimit = a })
{-# INLINE mccmSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster.
mccmSnapshotWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccmSnapshotWindow = lens _mccmSnapshotWindow (\s a -> s { _mccmSnapshotWindow = a })
{-# INLINE mccmSnapshotWindow #-}

-- | Specifies whether the new nodes in this Memcached cache cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones. Valid values: single-az | cross-az. This option is only
-- supported for Memcached cache clusters. You cannot specify single-az if the
-- Memcached cache cluster already has cache nodes in different Availability
-- Zones. If cross-az is specified, existing Memcached nodes remain in their
-- current Availability Zone. Only newly created nodes will be located in
-- different Availability Zones. For instructions on how to move existing
-- Memcached nodes to different Availability Zones, see the Availability Zone
-- Considerations section of Cache Node Considerations for Memcached.
mccmAZMode :: Lens' ModifyCacheCluster (Maybe Text)
mccmAZMode = lens _mccmAZMode (\s a -> s { _mccmAZMode = a })
{-# INLINE mccmAZMode #-}

-- | The list of Availability Zones where the new Memcached cache nodes will be
-- created. This parameter is only valid when NumCacheNodes in the request is
-- greater than the sum of the number of active cache nodes and the number of
-- cache nodes pending creation (which may be zero). The number of
-- Availability Zones supplied in this list must match the cache nodes being
-- added in this request. This option is only supported on Memcached clusters.
-- Scenarios: Scenario 1: You have 3 active nodes and wish to add 2 nodes.
-- Specify NumCacheNodes=5 (3 + 2) and opitonally specify two Availability
-- Zones for the two new nodes. Scenario 2: You have 3 active nodes and 2
-- nodes pending creation (from the scenario 1 call) and want to add 1 more
-- node. Specify NumCacheNodes=6 ((3 + 2) + 1) and optionally specify an
-- Availability Zone for the new node. Scenario 3: You want to cancel all
-- pending actions. You want to cancel all pending actions. Specify
-- NumCacheNodes=3 (the original number of nodes). You cannot cancel just some
-- pending operations. If you want to cancel a subset of the pending
-- operations, cancel all of them, then re-request those you want. The
-- Availability Zone placement of nodes pending creation cannot be modified.
-- If you wish to cancel any nodes pending creation, add 0 nodes by setting
-- NumCacheNodes to the number of current nodes. If cross-az is specified,
-- existing Memcached nodes remain in their current Availability Zone. Only
-- newly created nodes can be located in different Availability Zones. For
-- guidance on how to move existing Memcached nodes to different Availability
-- Zones, see the Availability Zone Considerations section of Cache Node
-- Considerations for Memcached. Example:
-- NewAvailabilityZones.member.1=us-east-1a&amp;NewAvailabilityZones.member.2=us-east-1b&amp;NewAvailabilityZones.member.3=us-east-1d.
-- 
mccmNewAvailabilityZones :: Lens' ModifyCacheCluster ([Text])
mccmNewAvailabilityZones = lens _mccmNewAvailabilityZones (\s a -> s { _mccmNewAvailabilityZones = a })
{-# INLINE mccmNewAvailabilityZones #-}

instance ToQuery ModifyCacheCluster where
    toQuery = genericQuery def

newtype ModifyCacheClusterResponse = ModifyCacheClusterResponse
    { _ccyCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific cache cluster.
ccyCacheCluster :: Lens' ModifyCacheClusterResponse (Maybe CacheCluster)
ccyCacheCluster = lens _ccyCacheCluster (\s a -> s { _ccyCacheCluster = a })
{-# INLINE ccyCacheCluster #-}

instance FromXML ModifyCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheCluster where
    type Sv ModifyCacheCluster = ElastiCache
    type Rs ModifyCacheCluster = ModifyCacheClusterResponse

    request = post "ModifyCacheCluster"
    response _ = xmlResponse
