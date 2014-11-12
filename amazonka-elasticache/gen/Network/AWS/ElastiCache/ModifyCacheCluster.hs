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
module Network.AWS.ElastiCache.ModifyCacheCluster
    (
    -- * Request
      ModifyCacheClusterMessage
    -- ** Request constructor
    , modifyCacheClusterMessage
    -- ** Request lenses
    , mccmAZMode
    , mccmApplyImmediately
    , mccmAutoMinorVersionUpgrade
    , mccmCacheClusterId
    , mccmCacheNodeIdsToRemove
    , mccmCacheParameterGroupName
    , mccmCacheSecurityGroupNames
    , mccmEngineVersion
    , mccmNewAvailabilityZones
    , mccmNotificationTopicArn
    , mccmNotificationTopicStatus
    , mccmNumCacheNodes
    , mccmPreferredMaintenanceWindow
    , mccmSecurityGroupIds
    , mccmSnapshotRetentionLimit
    , mccmSnapshotWindow

    -- * Response
    , ModifyCacheClusterResult
    -- ** Response constructor
    , modifyCacheClusterResult
    -- ** Response lenses
    , mccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data ModifyCacheClusterMessage = ModifyCacheClusterMessage
    { _mccmAZMode                     :: Maybe Text
    , _mccmApplyImmediately           :: Maybe Bool
    , _mccmAutoMinorVersionUpgrade    :: Maybe Bool
    , _mccmCacheClusterId             :: Text
    , _mccmCacheNodeIdsToRemove       :: [Text]
    , _mccmCacheParameterGroupName    :: Maybe Text
    , _mccmCacheSecurityGroupNames    :: [Text]
    , _mccmEngineVersion              :: Maybe Text
    , _mccmNewAvailabilityZones       :: [Text]
    , _mccmNotificationTopicArn       :: Maybe Text
    , _mccmNotificationTopicStatus    :: Maybe Text
    , _mccmNumCacheNodes              :: Maybe Int
    , _mccmPreferredMaintenanceWindow :: Maybe Text
    , _mccmSecurityGroupIds           :: [Text]
    , _mccmSnapshotRetentionLimit     :: Maybe Int
    , _mccmSnapshotWindow             :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'ModifyCacheClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccmAZMode' @::@ 'Maybe' 'Text'
--
-- * 'mccmApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mccmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mccmCacheClusterId' @::@ 'Text'
--
-- * 'mccmCacheNodeIdsToRemove' @::@ ['Text']
--
-- * 'mccmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mccmCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'mccmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mccmNewAvailabilityZones' @::@ ['Text']
--
-- * 'mccmNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'mccmNotificationTopicStatus' @::@ 'Maybe' 'Text'
--
-- * 'mccmNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'mccmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mccmSecurityGroupIds' @::@ ['Text']
--
-- * 'mccmSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'mccmSnapshotWindow' @::@ 'Maybe' 'Text'
--
modifyCacheClusterMessage :: Text -- ^ 'mccmCacheClusterId'
                          -> ModifyCacheClusterMessage
modifyCacheClusterMessage p1 = ModifyCacheClusterMessage
    { _mccmCacheClusterId             = p1
    , _mccmNumCacheNodes              = Nothing
    , _mccmCacheNodeIdsToRemove       = mempty
    , _mccmAZMode                     = Nothing
    , _mccmNewAvailabilityZones       = mempty
    , _mccmCacheSecurityGroupNames    = mempty
    , _mccmSecurityGroupIds           = mempty
    , _mccmPreferredMaintenanceWindow = Nothing
    , _mccmNotificationTopicArn       = Nothing
    , _mccmCacheParameterGroupName    = Nothing
    , _mccmNotificationTopicStatus    = Nothing
    , _mccmApplyImmediately           = Nothing
    , _mccmEngineVersion              = Nothing
    , _mccmAutoMinorVersionUpgrade    = Nothing
    , _mccmSnapshotRetentionLimit     = Nothing
    , _mccmSnapshotWindow             = Nothing
    }

-- | Specifies whether the new nodes in this Memcached cache cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones. Valid values: single-az | cross-az. This option is
-- only supported for Memcached cache clusters.
mccmAZMode :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmAZMode = lens _mccmAZMode (\s a -> s { _mccmAZMode = a })

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- cache cluster. If false, then changes to the cache cluster are applied on
-- the next maintenance reboot, or the next failure reboot, whichever occurs
-- first. If you perform a ModifyCacheCluster before a pending modification
-- is applied, the pending modification is replaced by the newer
-- modification. Valid values: true | false Default: false.
mccmApplyImmediately :: Lens' ModifyCacheClusterMessage (Maybe Bool)
mccmApplyImmediately =
    lens _mccmApplyImmediately (\s a -> s { _mccmApplyImmediately = a })

-- | If true, then minor engine upgrades will be applied automatically to the
-- cache cluster during the maintenance window. Valid values: true | false
-- Default: true.
mccmAutoMinorVersionUpgrade :: Lens' ModifyCacheClusterMessage (Maybe Bool)
mccmAutoMinorVersionUpgrade =
    lens _mccmAutoMinorVersionUpgrade
        (\s a -> s { _mccmAutoMinorVersionUpgrade = a })

-- | The cache cluster identifier. This value is stored as a lowercase string.
mccmCacheClusterId :: Lens' ModifyCacheClusterMessage Text
mccmCacheClusterId =
    lens _mccmCacheClusterId (\s a -> s { _mccmCacheClusterId = a })

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier
-- (0001, 0002, etc.). This parameter is only valid when NumCacheNodes is
-- less than the existing number of cache nodes. The number of cache node
-- IDs supplied in this parameter must match the difference between the
-- existing number of cache nodes in the cluster or pending cache nodes,
-- whichever is greater, and the value of NumCacheNodes in the request. For
-- example: If you have 3 active cache nodes, 7 pending cache nodes, and the
-- number of cache nodes in this ModifyCacheCluser call is 5, you must list
-- 2 (7 - 5) cache node IDs to remove.
mccmCacheNodeIdsToRemove :: Lens' ModifyCacheClusterMessage [Text]
mccmCacheNodeIdsToRemove =
    lens _mccmCacheNodeIdsToRemove
        (\s a -> s { _mccmCacheNodeIdsToRemove = a })

-- | The name of the cache parameter group to apply to this cache cluster.
-- This change is asynchronously applied as soon as possible for parameters
-- when the ApplyImmediately parameter is specified as true for this
-- request.
mccmCacheParameterGroupName :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmCacheParameterGroupName =
    lens _mccmCacheParameterGroupName
        (\s a -> s { _mccmCacheParameterGroupName = a })

-- | A list of cache security group names to authorize on this cache cluster.
-- This change is asynchronously applied as soon as possible. This parameter
-- can be used only with clusters that are created outside of an Amazon
-- Virtual Private Cloud (VPC). Constraints: Must contain no more than 255
-- alphanumeric characters. Must not be "Default".
mccmCacheSecurityGroupNames :: Lens' ModifyCacheClusterMessage [Text]
mccmCacheSecurityGroupNames =
    lens _mccmCacheSecurityGroupNames
        (\s a -> s { _mccmCacheSecurityGroupNames = a })

-- | The upgraded version of the cache engine to be run on the cache nodes.
mccmEngineVersion :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmEngineVersion =
    lens _mccmEngineVersion (\s a -> s { _mccmEngineVersion = a })

-- | The list of Availability Zones where the new Memcached cache nodes will
-- be created. This parameter is only valid when NumCacheNodes in the
-- request is greater than the sum of the number of active cache nodes and
-- the number of cache nodes pending creation (which may be zero). The
-- number of Availability Zones supplied in this list must match the cache
-- nodes being added in this request. This option is only supported on
-- Memcached clusters. Scenarios: Scenario 1: You have 3 active nodes and
-- wish to add 2 nodes. Specify NumCacheNodes=5 (3 + 2) and optionally
-- specify two Availability Zones for the two new nodes. Scenario 2: You
-- have 3 active nodes and 2 nodes pending creation (from the scenario 1
-- call) and want to add 1 more node. Specify NumCacheNodes=6 ((3 + 2) + 1)
-- and optionally specify an Availability Zone for the new node. Scenario 3:
-- You want to cancel all pending actions. Specify NumCacheNodes=3 to cancel
-- all pending actions. The Availability Zone placement of nodes pending
-- creation cannot be modified. If you wish to cancel any nodes pending
-- creation, add 0 nodes by setting NumCacheNodes to the number of current
-- nodes. If cross-az is specified, existing Memcached nodes remain in their
-- current Availability Zone. Only newly created nodes can be located in
-- different Availability Zones. For guidance on how to move existing
-- Memcached nodes to different Availability Zones, see the Availability
-- Zone Considerations section of Cache Node Considerations for Memcached.
-- Impact of new add/remove requests upon pending requests Scenarios Pending
-- Operation New Request Results Scenario-1 Delete Delete The new delete,
-- pending or immediate, replaces the pending delete. Scenario-2 Delete
-- Create The new create, pending or immediate, replaces the pending delete.
-- Scenario-3 Create Delete The new delete, pending or immediate, replaces
-- the pending create. Scenario-4 Create Create The new create is added to
-- the pending create. Important:If the new create request is Apply
-- Immediately - Yes, all creates are performed immediately. If the new
-- create request is Apply Immediately - No, all creates are pending.
-- Example:
-- NewAvailabilityZones.member.1=us-east-1a&amp;NewAvailabilityZones.member.2=us-east-1b&amp;NewAvailabilityZones.member.3=us-east-1d.
-- 
mccmNewAvailabilityZones :: Lens' ModifyCacheClusterMessage [Text]
mccmNewAvailabilityZones =
    lens _mccmNewAvailabilityZones
        (\s a -> s { _mccmNewAvailabilityZones = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
mccmNotificationTopicArn :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmNotificationTopicArn =
    lens _mccmNotificationTopicArn
        (\s a -> s { _mccmNotificationTopicArn = a })

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active. Valid values: active | inactive.
mccmNotificationTopicStatus :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmNotificationTopicStatus =
    lens _mccmNotificationTopicStatus
        (\s a -> s { _mccmNotificationTopicStatus = a })

-- | The number of cache nodes that the cache cluster should have. If the
-- value for NumCacheNodes is greater than the sum of the number of current
-- cache nodes and the number of cache nodes pending creation (which may be
-- zero), then more nodes will be added. If the value is less than the
-- number of existing cache nodes, then nodes will be removed. If the value
-- is equal to the number of current cache nodes, then any pending add or
-- remove requests are canceled. If you are removing cache nodes, you must
-- use the CacheNodeIdsToRemove parameter to provide the IDs of the specific
-- cache nodes to remove. For cache clusters running Redis, the value of
-- NumCacheNodesmust be 1. Note:Adding or removing Memcached cache nodes can
-- be applied immediately or as a pending action. See ApplyImmediately. A
-- pending action to modify the number of cache nodes in a cluster during
-- its maintenance window, whether by adding or removing nodes in accordance
-- with the scale out architecture, is not queued. The customer's latest
-- request to add or remove nodes to the cluster overrides any previous
-- pending actions to modify the number of cache nodes in the cluster. For
-- example, a request to remove 2 nodes would override a previous pending
-- action to remove 3 nodes. Similarly, a request to add 2 nodes would
-- override a previous pending action to remove 3 nodes and vice versa. As
-- Memcached cache nodes may now be provisioned in different Availability
-- Zones with flexible cache node placement, a request to add nodes does not
-- automatically override a previous pending action to add nodes. The
-- customer can modify the previous pending action to add more nodes or
-- explicitly cancel the pending request and retry the new request. To
-- cancel pending actions to modify the number of cache nodes in a cluster,
-- use the ModifyCacheCluster request and set NumCacheNodes equal to the
-- number of cache nodes currently in the cache cluster.
mccmNumCacheNodes :: Lens' ModifyCacheClusterMessage (Maybe Int)
mccmNumCacheNodes =
    lens _mccmNumCacheNodes (\s a -> s { _mccmNumCacheNodes = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Note that system maintenance may result in an outage. This change is made
-- immediately. If you are moving this window to the current time, there
-- must be at least 120 minutes between the current time and end of the
-- window to ensure that pending changes are applied.
mccmPreferredMaintenanceWindow :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmPreferredMaintenanceWindow =
    lens _mccmPreferredMaintenanceWindow
        (\s a -> s { _mccmPreferredMaintenanceWindow = a })

-- | Specifies the VPC Security Groups associated with the cache cluster. This
-- parameter can be used only with clusters that are created in an Amazon
-- Virtual Private Cloud (VPC).
mccmSecurityGroupIds :: Lens' ModifyCacheClusterMessage [Text]
mccmSecurityGroupIds =
    lens _mccmSecurityGroupIds (\s a -> s { _mccmSecurityGroupIds = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mccmSnapshotRetentionLimit :: Lens' ModifyCacheClusterMessage (Maybe Int)
mccmSnapshotRetentionLimit =
    lens _mccmSnapshotRetentionLimit
        (\s a -> s { _mccmSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your cache cluster.
mccmSnapshotWindow :: Lens' ModifyCacheClusterMessage (Maybe Text)
mccmSnapshotWindow =
    lens _mccmSnapshotWindow (\s a -> s { _mccmSnapshotWindow = a })
instance ToQuery ModifyCacheClusterMessage

instance ToPath ModifyCacheClusterMessage where
    toPath = const "/"

newtype ModifyCacheClusterResult = ModifyCacheClusterResult
    { _mccrCacheCluster :: Maybe CacheCluster
    } (Eq, Show, Generic)

-- | 'ModifyCacheClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
modifyCacheClusterResult :: ModifyCacheClusterResult
modifyCacheClusterResult = ModifyCacheClusterResult
    { _mccrCacheCluster = Nothing
    }

mccrCacheCluster :: Lens' ModifyCacheClusterResult (Maybe CacheCluster)
mccrCacheCluster = lens _mccrCacheCluster (\s a -> s { _mccrCacheCluster = a })

instance FromXML ModifyCacheClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyCacheClusterResult"

instance AWSRequest ModifyCacheClusterMessage where
    type Sv ModifyCacheClusterMessage = ElastiCache
    type Rs ModifyCacheClusterMessage = ModifyCacheClusterResult

    request  = post "ModifyCacheCluster"
    response = xmlResponse $ \h x -> ModifyCacheClusterResult
        <$> x %| "CacheCluster"
