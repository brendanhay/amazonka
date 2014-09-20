{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheCluster
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
module Network.AWS.ElastiCache.ModifyCacheCluster
    (
    -- * Request
      ModifyCacheCluster
    -- ** Request constructor
    , modifyCacheCluster
    -- ** Request lenses
    , mccCacheClusterId
    , mccNumCacheNodes
    , mccCacheNodeIdsToRemove
    , mccCacheSecurityGroupNames
    , mccSecurityGroupIds
    , mccPreferredMaintenanceWindow
    , mccNotificationTopicArn
    , mccCacheParameterGroupName
    , mccNotificationTopicStatus
    , mccApplyImmediately
    , mccEngineVersion
    , mccAutoMinorVersionUpgrade
    , mccSnapshotRetentionLimit
    , mccSnapshotWindow
    , mccAZMode
    , mccNewAvailabilityZones

    -- * Response
    , ModifyCacheClusterResponse
    -- ** Response constructor
    , modifyCacheClusterResponse
    -- ** Response lenses
    , mccrCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a ModifyCacheCluster operation.
data ModifyCacheCluster = ModifyCacheCluster
    { _mccCacheClusterId :: Text
    , _mccNumCacheNodes :: Maybe Integer
    , _mccCacheNodeIdsToRemove :: [Text]
    , _mccCacheSecurityGroupNames :: [Text]
    , _mccSecurityGroupIds :: [Text]
    , _mccPreferredMaintenanceWindow :: Maybe Text
    , _mccNotificationTopicArn :: Maybe Text
    , _mccCacheParameterGroupName :: Maybe Text
    , _mccNotificationTopicStatus :: Maybe Text
    , _mccApplyImmediately :: Maybe Bool
    , _mccEngineVersion :: Maybe Text
    , _mccAutoMinorVersionUpgrade :: Maybe Bool
    , _mccSnapshotRetentionLimit :: Maybe Integer
    , _mccSnapshotWindow :: Maybe Text
    , _mccAZMode :: Maybe Text
    , _mccNewAvailabilityZones :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheCluster' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Text@
--
-- * @NumCacheNodes ::@ @Maybe Integer@
--
-- * @CacheNodeIdsToRemove ::@ @[Text]@
--
-- * @CacheSecurityGroupNames ::@ @[Text]@
--
-- * @SecurityGroupIds ::@ @[Text]@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @NotificationTopicArn ::@ @Maybe Text@
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
-- * @NotificationTopicStatus ::@ @Maybe Text@
--
-- * @ApplyImmediately ::@ @Maybe Bool@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @SnapshotRetentionLimit ::@ @Maybe Integer@
--
-- * @SnapshotWindow ::@ @Maybe Text@
--
-- * @AZMode ::@ @Maybe Text@
--
-- * @NewAvailabilityZones ::@ @[Text]@
--
modifyCacheCluster :: Text -- ^ 'mccCacheClusterId'
                   -> ModifyCacheCluster
modifyCacheCluster p1 = ModifyCacheCluster
    { _mccCacheClusterId = p1
    , _mccNumCacheNodes = Nothing
    , _mccCacheNodeIdsToRemove = mempty
    , _mccCacheSecurityGroupNames = mempty
    , _mccSecurityGroupIds = mempty
    , _mccPreferredMaintenanceWindow = Nothing
    , _mccNotificationTopicArn = Nothing
    , _mccCacheParameterGroupName = Nothing
    , _mccNotificationTopicStatus = Nothing
    , _mccApplyImmediately = Nothing
    , _mccEngineVersion = Nothing
    , _mccAutoMinorVersionUpgrade = Nothing
    , _mccSnapshotRetentionLimit = Nothing
    , _mccSnapshotWindow = Nothing
    , _mccAZMode = Nothing
    , _mccNewAvailabilityZones = mempty
    }

-- | The cache cluster identifier. This value is stored as a lowercase string.
mccCacheClusterId :: Lens' ModifyCacheCluster Text
mccCacheClusterId =
    lens _mccCacheClusterId (\s a -> s { _mccCacheClusterId = a })

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
mccNumCacheNodes :: Lens' ModifyCacheCluster (Maybe Integer)
mccNumCacheNodes =
    lens _mccNumCacheNodes (\s a -> s { _mccNumCacheNodes = a })

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier
-- (0001, 0002, etc.). This parameter is only valid when NumCacheNodes is less
-- than the existing number of cache nodes. The number of cache node IDs
-- supplied in this parameter must match the difference between the existing
-- number of cache nodes in the cluster or pending cache nodes, whichever is
-- greater, and the value of NumCacheNodes in the request. For example: If you
-- have 3 active cache nodes, 7 pending cache nodes, and the number of cache
-- nodes in this ModifyCacheCluser call is 5, you must list 2 (7 - 5) cache
-- node IDs to remove.
mccCacheNodeIdsToRemove :: Lens' ModifyCacheCluster [Text]
mccCacheNodeIdsToRemove =
    lens _mccCacheNodeIdsToRemove
         (\s a -> s { _mccCacheNodeIdsToRemove = a })

-- | A list of cache security group names to authorize on this cache cluster.
-- This change is asynchronously applied as soon as possible. This parameter
-- can be used only with clusters that are created outside of an Amazon
-- Virtual Private Cloud (VPC). Constraints: Must contain no more than 255
-- alphanumeric characters. Must not be "Default".
mccCacheSecurityGroupNames :: Lens' ModifyCacheCluster [Text]
mccCacheSecurityGroupNames =
    lens _mccCacheSecurityGroupNames
         (\s a -> s { _mccCacheSecurityGroupNames = a })

-- | Specifies the VPC Security Groups associated with the cache cluster. This
-- parameter can be used only with clusters that are created in an Amazon
-- Virtual Private Cloud (VPC).
mccSecurityGroupIds :: Lens' ModifyCacheCluster [Text]
mccSecurityGroupIds =
    lens _mccSecurityGroupIds (\s a -> s { _mccSecurityGroupIds = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Note that system maintenance may result in an outage. This change is made
-- immediately. If you are moving this window to the current time, there must
-- be at least 120 minutes between the current time and end of the window to
-- ensure that pending changes are applied.
mccPreferredMaintenanceWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccPreferredMaintenanceWindow =
    lens _mccPreferredMaintenanceWindow
         (\s a -> s { _mccPreferredMaintenanceWindow = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent. The Amazon SNS topic owner must be same as the
-- cache cluster owner.
mccNotificationTopicArn :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicArn =
    lens _mccNotificationTopicArn
         (\s a -> s { _mccNotificationTopicArn = a })

-- | The name of the cache parameter group to apply to this cache cluster. This
-- change is asynchronously applied as soon as possible for parameters when
-- the ApplyImmediately parameter is specified as true for this request.
mccCacheParameterGroupName :: Lens' ModifyCacheCluster (Maybe Text)
mccCacheParameterGroupName =
    lens _mccCacheParameterGroupName
         (\s a -> s { _mccCacheParameterGroupName = a })

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active. Valid values: active | inactive.
mccNotificationTopicStatus :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicStatus =
    lens _mccNotificationTopicStatus
         (\s a -> s { _mccNotificationTopicStatus = a })

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- cache cluster. If false, then changes to the cache cluster are applied on
-- the next maintenance reboot, or the next failure reboot, whichever occurs
-- first. If you perform a ModifyCacheCluster before a pending modification is
-- applied, the pending modification is replaced by the newer modification.
-- Valid values: true | false Default: false.
mccApplyImmediately :: Lens' ModifyCacheCluster (Maybe Bool)
mccApplyImmediately =
    lens _mccApplyImmediately (\s a -> s { _mccApplyImmediately = a })

-- | The upgraded version of the cache engine to be run on the cache nodes.
mccEngineVersion :: Lens' ModifyCacheCluster (Maybe Text)
mccEngineVersion =
    lens _mccEngineVersion (\s a -> s { _mccEngineVersion = a })

-- | If true, then minor engine upgrades will be applied automatically to the
-- cache cluster during the maintenance window. Valid values: true | false
-- Default: true.
mccAutoMinorVersionUpgrade :: Lens' ModifyCacheCluster (Maybe Bool)
mccAutoMinorVersionUpgrade =
    lens _mccAutoMinorVersionUpgrade
         (\s a -> s { _mccAutoMinorVersionUpgrade = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mccSnapshotRetentionLimit :: Lens' ModifyCacheCluster (Maybe Integer)
mccSnapshotRetentionLimit =
    lens _mccSnapshotRetentionLimit
         (\s a -> s { _mccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster.
mccSnapshotWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccSnapshotWindow =
    lens _mccSnapshotWindow (\s a -> s { _mccSnapshotWindow = a })

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
mccAZMode :: Lens' ModifyCacheCluster (Maybe Text)
mccAZMode = lens _mccAZMode (\s a -> s { _mccAZMode = a })

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
mccNewAvailabilityZones :: Lens' ModifyCacheCluster [Text]
mccNewAvailabilityZones =
    lens _mccNewAvailabilityZones
         (\s a -> s { _mccNewAvailabilityZones = a })

instance ToQuery ModifyCacheCluster where
    toQuery = genericQuery def

newtype ModifyCacheClusterResponse = ModifyCacheClusterResponse
    { _mccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheClusterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheCluster ::@ @Maybe CacheCluster@
--
modifyCacheClusterResponse :: ModifyCacheClusterResponse
modifyCacheClusterResponse = ModifyCacheClusterResponse
    { _mccrCacheCluster = Nothing
    }

-- | Contains all of the attributes of a specific cache cluster.
mccrCacheCluster :: Lens' ModifyCacheClusterResponse (Maybe CacheCluster)
mccrCacheCluster =
    lens _mccrCacheCluster (\s a -> s { _mccrCacheCluster = a })

instance FromXML ModifyCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheCluster where
    type Sv ModifyCacheCluster = ElastiCache
    type Rs ModifyCacheCluster = ModifyCacheClusterResponse

    request = post "ModifyCacheCluster"
    response _ = xmlResponse
