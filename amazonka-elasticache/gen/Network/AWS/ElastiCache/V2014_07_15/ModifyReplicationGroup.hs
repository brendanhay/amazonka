{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.ModifyReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyReplicationGroup operation modifies the settings for a
-- replication group. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyReplicationGroup &ApplyImmediately=false
-- &ReplicationGroupId=my-repgroup &PrimaryClusterId=my-replica-1
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= my-redis-primary
-- my-redis-primary my-replica-1 0001 6379
-- my-repgroup.q68zge.ng.0001.use1devo.elmo-dev.amazonaws.com available
-- my-redis-primary 6379
-- my-redis-primary.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1d
-- 0001 primary my-replica-1 6379
-- my-replica-1.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1e 0001
-- replica my-repgroup available my-replica-1 My replication group
-- 6fd0aad6-b9d7-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.ModifyReplicationGroup
    (
    -- * Request
      ModifyReplicationGroup
    -- ** Request constructor
    , mkModifyReplicationGroup
    -- ** Request lenses
    , mrgReplicationGroupId
    , mrgReplicationGroupDescription
    , mrgCacheSecurityGroupNames
    , mrgSecurityGroupIds
    , mrgPreferredMaintenanceWindow
    , mrgNotificationTopicArn
    , mrgCacheParameterGroupName
    , mrgNotificationTopicStatus
    , mrgApplyImmediately
    , mrgEngineVersion
    , mrgAutoMinorVersionUpgrade
    , mrgPrimaryClusterId
    , mrgSnapshotRetentionLimit
    , mrgSnapshotWindow
    , mrgSnapshottingClusterId

    -- * Response
    , ModifyReplicationGroupResponse
    -- ** Response lenses
    , mrgrsReplicationGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a ModifyReplicationGroups operation.
data ModifyReplicationGroup = ModifyReplicationGroup
    { _mrgReplicationGroupId :: Text
    , _mrgReplicationGroupDescription :: Maybe Text
    , _mrgCacheSecurityGroupNames :: [Text]
    , _mrgSecurityGroupIds :: [Text]
    , _mrgPreferredMaintenanceWindow :: Maybe Text
    , _mrgNotificationTopicArn :: Maybe Text
    , _mrgCacheParameterGroupName :: Maybe Text
    , _mrgNotificationTopicStatus :: Maybe Text
    , _mrgApplyImmediately :: Maybe Bool
    , _mrgEngineVersion :: Maybe Text
    , _mrgAutoMinorVersionUpgrade :: Maybe Bool
    , _mrgPrimaryClusterId :: Maybe Text
    , _mrgSnapshotRetentionLimit :: Maybe Integer
    , _mrgSnapshotWindow :: Maybe Text
    , _mrgSnapshottingClusterId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyReplicationGroup' request.
mkModifyReplicationGroup :: Text -- ^ 'mrgReplicationGroupId'
                         -> ModifyReplicationGroup
mkModifyReplicationGroup p1 = ModifyReplicationGroup
    { _mrgReplicationGroupId = p1
    , _mrgReplicationGroupDescription = Nothing
    , _mrgCacheSecurityGroupNames = mempty
    , _mrgSecurityGroupIds = mempty
    , _mrgPreferredMaintenanceWindow = Nothing
    , _mrgNotificationTopicArn = Nothing
    , _mrgCacheParameterGroupName = Nothing
    , _mrgNotificationTopicStatus = Nothing
    , _mrgApplyImmediately = Nothing
    , _mrgEngineVersion = Nothing
    , _mrgAutoMinorVersionUpgrade = Nothing
    , _mrgPrimaryClusterId = Nothing
    , _mrgSnapshotRetentionLimit = Nothing
    , _mrgSnapshotWindow = Nothing
    , _mrgSnapshottingClusterId = Nothing
    }

-- | The identifier of the replication group to modify.
mrgReplicationGroupId :: Lens' ModifyReplicationGroup Text
mrgReplicationGroupId =
    lens _mrgReplicationGroupId (\s a -> s { _mrgReplicationGroupId = a })

-- | A description for the replication group. Maximum length is 255 characters.
mrgReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgReplicationGroupDescription =
    lens _mrgReplicationGroupDescription
         (\s a -> s { _mrgReplicationGroupDescription = a })

-- | A list of cache security group names to authorize for the clusters in this
-- replication group. This change is asynchronously applied as soon as
-- possible. This parameter can be used only with replication groups
-- containing cache clusters running outside of an Amazon Virtual Private
-- Cloud (VPC). Constraints: Must contain no more than 255 alphanumeric
-- characters. Must not be "Default".
mrgCacheSecurityGroupNames :: Lens' ModifyReplicationGroup [Text]
mrgCacheSecurityGroupNames =
    lens _mrgCacheSecurityGroupNames
         (\s a -> s { _mrgCacheSecurityGroupNames = a })

-- | Specifies the VPC Security Groups associated with the cache clusters in the
-- replication group. This parameter can be used only with replication groups
-- containing cache clusters running in an Amazon Virtual Private Cloud (VPC).
mrgSecurityGroupIds :: Lens' ModifyReplicationGroup [Text]
mrgSecurityGroupIds =
    lens _mrgSecurityGroupIds (\s a -> s { _mrgSecurityGroupIds = a })

-- | The weekly time range (in UTC) during which replication group system
-- maintenance can occur. Note that system maintenance may result in an
-- outage. This change is made immediately. If you are moving this window to
-- the current time, there must be at least 120 minutes between the current
-- time and end of the window to ensure that pending changes are applied.
mrgPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPreferredMaintenanceWindow =
    lens _mrgPreferredMaintenanceWindow
         (\s a -> s { _mrgPreferredMaintenanceWindow = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent. The Amazon SNS topic owner must be same as the
-- replication group owner.
mrgNotificationTopicArn :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicArn =
    lens _mrgNotificationTopicArn
         (\s a -> s { _mrgNotificationTopicArn = a })

-- | The name of the cache parameter group to apply to all of the cache nodes in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the ApplyImmediately parameter is specified as
-- true for this request.
mrgCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgCacheParameterGroupName =
    lens _mrgCacheParameterGroupName
         (\s a -> s { _mrgCacheParameterGroupName = a })

-- | The status of the Amazon SNS notification topic for the replication group.
-- Notifications are sent only if the status is active. Valid values: active |
-- inactive.
mrgNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicStatus =
    lens _mrgNotificationTopicStatus
         (\s a -> s { _mrgNotificationTopicStatus = a })

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- replication group. If false, then changes to the nodes in the replication
-- group are applied on the next maintenance reboot, or the next failure
-- reboot, whichever occurs first. Valid values: true | false Default: false.
mrgApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgApplyImmediately =
    lens _mrgApplyImmediately (\s a -> s { _mrgApplyImmediately = a })

-- | The upgraded version of the cache engine to be run on the nodes in the
-- replication group..
mrgEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgEngineVersion =
    lens _mrgEngineVersion (\s a -> s { _mrgEngineVersion = a })

-- | Determines whether minor engine upgrades will be applied automatically to
-- all of the cache nodes in the replication group during the maintenance
-- window. A value of true allows these upgrades to occur; false disables
-- automatic upgrades.
mrgAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutoMinorVersionUpgrade =
    lens _mrgAutoMinorVersionUpgrade
         (\s a -> s { _mrgAutoMinorVersionUpgrade = a })

-- | If this parameter is specified, ElastiCache will promote each of the nodes
-- in the specified cache cluster to the primary role. The nodes of all other
-- clusters in the replication group will be read replicas.
mrgPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPrimaryClusterId =
    lens _mrgPrimaryClusterId (\s a -> s { _mrgPrimaryClusterId = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mrgSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Integer)
mrgSnapshotRetentionLimit =
    lens _mrgSnapshotRetentionLimit
         (\s a -> s { _mrgSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of the cache cluster specified by SnapshottingClusterId.
-- Example: 05:00-09:00 If you do not specify this parameter, then ElastiCache
-- will automatically choose an appropriate time range.
mrgSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshotWindow =
    lens _mrgSnapshotWindow (\s a -> s { _mrgSnapshotWindow = a })

-- | The cache cluster ID that will be used as the daily snapshot source for the
-- replication group.
mrgSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshottingClusterId =
    lens _mrgSnapshottingClusterId
         (\s a -> s { _mrgSnapshottingClusterId = a })

instance ToQuery ModifyReplicationGroup where
    toQuery = genericQuery def

newtype ModifyReplicationGroupResponse = ModifyReplicationGroupResponse
    { _mrgrsReplicationGroup :: Maybe ReplicationGroup
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific replication group.
mrgrsReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
mrgrsReplicationGroup =
    lens _mrgrsReplicationGroup (\s a -> s { _mrgrsReplicationGroup = a })

instance FromXML ModifyReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyReplicationGroup where
    type Sv ModifyReplicationGroup = ElastiCache
    type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse

    request = post "ModifyReplicationGroup"
    response _ = xmlResponse
