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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheCluster.html>
module Network.AWS.ElastiCache.ModifyCacheCluster
    (
    -- * Request
      ModifyCacheCluster
    -- ** Request constructor
    , modifyCacheCluster
    -- ** Request lenses
    , mccAZMode
    , mccApplyImmediately
    , mccAutoMinorVersionUpgrade
    , mccCacheClusterId
    , mccCacheNodeIdsToRemove
    , mccCacheParameterGroupName
    , mccCacheSecurityGroupNames
    , mccEngineVersion
    , mccNewAvailabilityZones
    , mccNotificationTopicArn
    , mccNotificationTopicStatus
    , mccNumCacheNodes
    , mccPreferredMaintenanceWindow
    , mccSecurityGroupIds
    , mccSnapshotRetentionLimit
    , mccSnapshotWindow

    -- * Response
    , ModifyCacheClusterResponse
    -- ** Response constructor
    , modifyCacheClusterResponse
    -- ** Response lenses
    , mccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data ModifyCacheCluster = ModifyCacheCluster
    { _mccAZMode                     :: Maybe AZMode
    , _mccApplyImmediately           :: Maybe Bool
    , _mccAutoMinorVersionUpgrade    :: Maybe Bool
    , _mccCacheClusterId             :: Text
    , _mccCacheNodeIdsToRemove       :: List "CacheNodeId" Text
    , _mccCacheParameterGroupName    :: Maybe Text
    , _mccCacheSecurityGroupNames    :: List "CacheSecurityGroupName" Text
    , _mccEngineVersion              :: Maybe Text
    , _mccNewAvailabilityZones       :: List "PreferredAvailabilityZone" Text
    , _mccNotificationTopicArn       :: Maybe Text
    , _mccNotificationTopicStatus    :: Maybe Text
    , _mccNumCacheNodes              :: Maybe Int
    , _mccPreferredMaintenanceWindow :: Maybe Text
    , _mccSecurityGroupIds           :: List "SecurityGroupId" Text
    , _mccSnapshotRetentionLimit     :: Maybe Int
    , _mccSnapshotWindow             :: Maybe Text
    } deriving (Eq, Show)

-- | 'ModifyCacheCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccAZMode' @::@ 'Maybe' 'AZMode'
--
-- * 'mccApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mccAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mccCacheClusterId' @::@ 'Text'
--
-- * 'mccCacheNodeIdsToRemove' @::@ ['Text']
--
-- * 'mccCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mccCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'mccEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mccNewAvailabilityZones' @::@ ['Text']
--
-- * 'mccNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'mccNotificationTopicStatus' @::@ 'Maybe' 'Text'
--
-- * 'mccNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'mccPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mccSecurityGroupIds' @::@ ['Text']
--
-- * 'mccSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'mccSnapshotWindow' @::@ 'Maybe' 'Text'
--
modifyCacheCluster :: Text -- ^ 'mccCacheClusterId'
                   -> ModifyCacheCluster
modifyCacheCluster p1 = ModifyCacheCluster
    { _mccCacheClusterId             = p1
    , _mccNumCacheNodes              = Nothing
    , _mccCacheNodeIdsToRemove       = mempty
    , _mccAZMode                     = Nothing
    , _mccNewAvailabilityZones       = mempty
    , _mccCacheSecurityGroupNames    = mempty
    , _mccSecurityGroupIds           = mempty
    , _mccPreferredMaintenanceWindow = Nothing
    , _mccNotificationTopicArn       = Nothing
    , _mccCacheParameterGroupName    = Nothing
    , _mccNotificationTopicStatus    = Nothing
    , _mccApplyImmediately           = Nothing
    , _mccEngineVersion              = Nothing
    , _mccAutoMinorVersionUpgrade    = Nothing
    , _mccSnapshotRetentionLimit     = Nothing
    , _mccSnapshotWindow             = Nothing
    }

-- | Specifies whether the new nodes in this Memcached cache cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones. Valid values: single-az | cross-az. This option is
-- only supported for Memcached cache clusters.
mccAZMode :: Lens' ModifyCacheCluster (Maybe AZMode)
mccAZMode = lens _mccAZMode (\s a -> s { _mccAZMode = a })

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- cache cluster. If false, then changes to the cache cluster are applied on
-- the next maintenance reboot, or the next failure reboot, whichever occurs
-- first. If you perform a ModifyCacheCluster before a pending modification
-- is applied, the pending modification is replaced by the newer
-- modification. Valid values: true | false Default: false.
mccApplyImmediately :: Lens' ModifyCacheCluster (Maybe Bool)
mccApplyImmediately =
    lens _mccApplyImmediately (\s a -> s { _mccApplyImmediately = a })

-- | If true, then minor engine upgrades will be applied automatically to the
-- cache cluster during the maintenance window. Valid values: true | false
-- Default: true.
mccAutoMinorVersionUpgrade :: Lens' ModifyCacheCluster (Maybe Bool)
mccAutoMinorVersionUpgrade =
    lens _mccAutoMinorVersionUpgrade
        (\s a -> s { _mccAutoMinorVersionUpgrade = a })

-- | The cache cluster identifier. This value is stored as a lowercase string.
mccCacheClusterId :: Lens' ModifyCacheCluster Text
mccCacheClusterId =
    lens _mccCacheClusterId (\s a -> s { _mccCacheClusterId = a })

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier
-- (0001, 0002, etc.). This parameter is only valid when NumCacheNodes is
-- less than the existing number of cache nodes. The number of cache node
-- IDs supplied in this parameter must match the difference between the
-- existing number of cache nodes in the cluster or pending cache nodes,
-- whichever is greater, and the value of NumCacheNodes in the request. For
-- example: If you have 3 active cache nodes, 7 pending cache nodes, and the
-- number of cache nodes in this ModifyCacheCluser call is 5, you must list
-- 2 (7 - 5) cache node IDs to remove.
mccCacheNodeIdsToRemove :: Lens' ModifyCacheCluster [Text]
mccCacheNodeIdsToRemove =
    lens _mccCacheNodeIdsToRemove (\s a -> s { _mccCacheNodeIdsToRemove = a })
        . _List

-- | The name of the cache parameter group to apply to this cache cluster.
-- This change is asynchronously applied as soon as possible for parameters
-- when the ApplyImmediately parameter is specified as true for this
-- request.
mccCacheParameterGroupName :: Lens' ModifyCacheCluster (Maybe Text)
mccCacheParameterGroupName =
    lens _mccCacheParameterGroupName
        (\s a -> s { _mccCacheParameterGroupName = a })

-- | A list of cache security group names to authorize on this cache cluster.
-- This change is asynchronously applied as soon as possible. This parameter
-- can be used only with clusters that are created outside of an Amazon
-- Virtual Private Cloud (VPC). Constraints: Must contain no more than 255
-- alphanumeric characters. Must not be "Default".
mccCacheSecurityGroupNames :: Lens' ModifyCacheCluster [Text]
mccCacheSecurityGroupNames =
    lens _mccCacheSecurityGroupNames
        (\s a -> s { _mccCacheSecurityGroupNames = a })
            . _List

-- | The upgraded version of the cache engine to be run on the cache nodes.
mccEngineVersion :: Lens' ModifyCacheCluster (Maybe Text)
mccEngineVersion = lens _mccEngineVersion (\s a -> s { _mccEngineVersion = a })

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
mccNewAvailabilityZones :: Lens' ModifyCacheCluster [Text]
mccNewAvailabilityZones =
    lens _mccNewAvailabilityZones (\s a -> s { _mccNewAvailabilityZones = a })
        . _List

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
mccNotificationTopicArn :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicArn =
    lens _mccNotificationTopicArn (\s a -> s { _mccNotificationTopicArn = a })

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is active. Valid values: active | inactive.
mccNotificationTopicStatus :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicStatus =
    lens _mccNotificationTopicStatus
        (\s a -> s { _mccNotificationTopicStatus = a })

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
mccNumCacheNodes :: Lens' ModifyCacheCluster (Maybe Int)
mccNumCacheNodes = lens _mccNumCacheNodes (\s a -> s { _mccNumCacheNodes = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Note that system maintenance may result in an outage. This change is made
-- immediately. If you are moving this window to the current time, there
-- must be at least 120 minutes between the current time and end of the
-- window to ensure that pending changes are applied.
mccPreferredMaintenanceWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccPreferredMaintenanceWindow =
    lens _mccPreferredMaintenanceWindow
        (\s a -> s { _mccPreferredMaintenanceWindow = a })

-- | Specifies the VPC Security Groups associated with the cache cluster. This
-- parameter can be used only with clusters that are created in an Amazon
-- Virtual Private Cloud (VPC).
mccSecurityGroupIds :: Lens' ModifyCacheCluster [Text]
mccSecurityGroupIds =
    lens _mccSecurityGroupIds (\s a -> s { _mccSecurityGroupIds = a })
        . _List

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mccSnapshotRetentionLimit :: Lens' ModifyCacheCluster (Maybe Int)
mccSnapshotRetentionLimit =
    lens _mccSnapshotRetentionLimit
        (\s a -> s { _mccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your cache cluster.
mccSnapshotWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccSnapshotWindow =
    lens _mccSnapshotWindow (\s a -> s { _mccSnapshotWindow = a })

newtype ModifyCacheClusterResponse = ModifyCacheClusterResponse
    { _mccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show)

-- | 'ModifyCacheClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
modifyCacheClusterResponse :: ModifyCacheClusterResponse
modifyCacheClusterResponse = ModifyCacheClusterResponse
    { _mccrCacheCluster = Nothing
    }

mccrCacheCluster :: Lens' ModifyCacheClusterResponse (Maybe CacheCluster)
mccrCacheCluster = lens _mccrCacheCluster (\s a -> s { _mccrCacheCluster = a })

instance ToPath ModifyCacheCluster where
    toPath = const "/"

instance ToQuery ModifyCacheCluster where
    toQuery ModifyCacheCluster{..} = mconcat
        [ "AZMode"                     =? _mccAZMode
        , "ApplyImmediately"           =? _mccApplyImmediately
        , "AutoMinorVersionUpgrade"    =? _mccAutoMinorVersionUpgrade
        , "CacheClusterId"             =? _mccCacheClusterId
        , "CacheNodeIdsToRemove"       =? _mccCacheNodeIdsToRemove
        , "CacheParameterGroupName"    =? _mccCacheParameterGroupName
        , "CacheSecurityGroupNames"    =? _mccCacheSecurityGroupNames
        , "EngineVersion"              =? _mccEngineVersion
        , "NewAvailabilityZones"       =? _mccNewAvailabilityZones
        , "NotificationTopicArn"       =? _mccNotificationTopicArn
        , "NotificationTopicStatus"    =? _mccNotificationTopicStatus
        , "NumCacheNodes"              =? _mccNumCacheNodes
        , "PreferredMaintenanceWindow" =? _mccPreferredMaintenanceWindow
        , "SecurityGroupIds"           =? _mccSecurityGroupIds
        , "SnapshotRetentionLimit"     =? _mccSnapshotRetentionLimit
        , "SnapshotWindow"             =? _mccSnapshotWindow
        ]

instance ToHeaders ModifyCacheCluster

instance AWSRequest ModifyCacheCluster where
    type Sv ModifyCacheCluster = ElastiCache
    type Rs ModifyCacheCluster = ModifyCacheClusterResponse

    request  = post "ModifyCacheCluster"
    response = xmlResponse

instance FromXML ModifyCacheClusterResponse where
    parseXML = withElement "ModifyCacheClusterResult" $ \x -> ModifyCacheClusterResponse
        <$> x .@? "CacheCluster"
