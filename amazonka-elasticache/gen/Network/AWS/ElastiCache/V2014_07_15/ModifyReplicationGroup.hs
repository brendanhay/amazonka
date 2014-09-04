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
    , mkModifyReplicationGroupMessage
    -- ** Request lenses
    , mrgmReplicationGroupId
    , mrgmReplicationGroupDescription
    , mrgmCacheSecurityGroupNames
    , mrgmSecurityGroupIds
    , mrgmPreferredMaintenanceWindow
    , mrgmNotificationTopicArn
    , mrgmCacheParameterGroupName
    , mrgmNotificationTopicStatus
    , mrgmApplyImmediately
    , mrgmEngineVersion
    , mrgmAutoMinorVersionUpgrade
    , mrgmPrimaryClusterId
    , mrgmSnapshotRetentionLimit
    , mrgmSnapshotWindow
    , mrgmSnapshottingClusterId

    -- * Response
    , ModifyReplicationGroupResponse
    -- ** Response lenses
    , rgyReplicationGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyReplicationGroup' request.
mkModifyReplicationGroupMessage :: Text -- ^ 'mrgmReplicationGroupId'
                                -> ModifyReplicationGroup
mkModifyReplicationGroupMessage p1 = ModifyReplicationGroup
    { _mrgmReplicationGroupId = p1
    , _mrgmReplicationGroupDescription = Nothing
    , _mrgmCacheSecurityGroupNames = mempty
    , _mrgmSecurityGroupIds = mempty
    , _mrgmPreferredMaintenanceWindow = Nothing
    , _mrgmNotificationTopicArn = Nothing
    , _mrgmCacheParameterGroupName = Nothing
    , _mrgmNotificationTopicStatus = Nothing
    , _mrgmApplyImmediately = Nothing
    , _mrgmEngineVersion = Nothing
    , _mrgmAutoMinorVersionUpgrade = Nothing
    , _mrgmPrimaryClusterId = Nothing
    , _mrgmSnapshotRetentionLimit = Nothing
    , _mrgmSnapshotWindow = Nothing
    , _mrgmSnapshottingClusterId = Nothing
    }
{-# INLINE mkModifyReplicationGroupMessage #-}

data ModifyReplicationGroup = ModifyReplicationGroup
    { _mrgmReplicationGroupId :: Text
      -- ^ The identifier of the replication group to modify.
    , _mrgmReplicationGroupDescription :: Maybe Text
      -- ^ A description for the replication group. Maximum length is 255
      -- characters.
    , _mrgmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to authorize for the
      -- clusters in this replication group. This change is asynchronously
      -- applied as soon as possible. This parameter can be used only with
      -- replication groups containing cache clusters running outside of
      -- an Amazon Virtual Private Cloud (VPC). Constraints: Must contain
      -- no more than 255 alphanumeric characters. Must not be "Default".
    , _mrgmSecurityGroupIds :: [Text]
      -- ^ Specifies the VPC Security Groups associated with the cache
      -- clusters in the replication group. This parameter can be used
      -- only with replication groups containing cache clusters running in
      -- an Amazon Virtual Private Cloud (VPC).
    , _mrgmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which replication group
      -- system maintenance can occur. Note that system maintenance may
      -- result in an outage. This change is made immediately. If you are
      -- moving this window to the current time, there must be at least
      -- 120 minutes between the current time and end of the window to
      -- ensure that pending changes are applied.
    , _mrgmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to which
      -- notifications will be sent. The Amazon SNS topic owner must be
      -- same as the replication group owner.
    , _mrgmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to apply to all of the
      -- cache nodes in this replication group. This change is
      -- asynchronously applied as soon as possible for parameters when
      -- the ApplyImmediately parameter is specified as true for this
      -- request.
    , _mrgmNotificationTopicStatus :: Maybe Text
      -- ^ The status of the Amazon SNS notification topic for the
      -- replication group. Notifications are sent only if the status is
      -- active. Valid values: active | inactive.
    , _mrgmApplyImmediately :: Maybe Bool
      -- ^ If true, this parameter causes the modifications in this request
      -- and any pending modifications to be applied, asynchronously and
      -- as soon as possible, regardless of the PreferredMaintenanceWindow
      -- setting for the replication group. If false, then changes to the
      -- nodes in the replication group are applied on the next
      -- maintenance reboot, or the next failure reboot, whichever occurs
      -- first. Valid values: true | false Default: false.
    , _mrgmEngineVersion :: Maybe Text
      -- ^ The upgraded version of the cache engine to be run on the nodes
      -- in the replication group..
    , _mrgmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Determines whether minor engine upgrades will be applied
      -- automatically to all of the cache nodes in the replication group
      -- during the maintenance window. A value of true allows these
      -- upgrades to occur; false disables automatic upgrades.
    , _mrgmPrimaryClusterId :: Maybe Text
      -- ^ If this parameter is specified, ElastiCache will promote each of
      -- the nodes in the specified cache cluster to the primary role. The
      -- nodes of all other clusters in the replication group will be read
      -- replicas.
    , _mrgmSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted.
      -- ImportantIf the value of SnapshotRetentionLimit is set to zero
      -- (0), backups are turned off.
    , _mrgmSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of the cache cluster specified by
      -- SnapshottingClusterId. Example: 05:00-09:00 If you do not specify
      -- this parameter, then ElastiCache will automatically choose an
      -- appropriate time range.
    , _mrgmSnapshottingClusterId :: Maybe Text
      -- ^ The cache cluster ID that will be used as the daily snapshot
      -- source for the replication group.
    } deriving (Show, Generic)

-- | The identifier of the replication group to modify.
mrgmReplicationGroupId :: Lens' ModifyReplicationGroup (Text)
mrgmReplicationGroupId = lens _mrgmReplicationGroupId (\s a -> s { _mrgmReplicationGroupId = a })
{-# INLINE mrgmReplicationGroupId #-}

-- | A description for the replication group. Maximum length is 255 characters.
mrgmReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmReplicationGroupDescription = lens _mrgmReplicationGroupDescription (\s a -> s { _mrgmReplicationGroupDescription = a })
{-# INLINE mrgmReplicationGroupDescription #-}

-- | A list of cache security group names to authorize for the clusters in this
-- replication group. This change is asynchronously applied as soon as
-- possible. This parameter can be used only with replication groups
-- containing cache clusters running outside of an Amazon Virtual Private
-- Cloud (VPC). Constraints: Must contain no more than 255 alphanumeric
-- characters. Must not be "Default".
mrgmCacheSecurityGroupNames :: Lens' ModifyReplicationGroup ([Text])
mrgmCacheSecurityGroupNames = lens _mrgmCacheSecurityGroupNames (\s a -> s { _mrgmCacheSecurityGroupNames = a })
{-# INLINE mrgmCacheSecurityGroupNames #-}

-- | Specifies the VPC Security Groups associated with the cache clusters in the
-- replication group. This parameter can be used only with replication groups
-- containing cache clusters running in an Amazon Virtual Private Cloud (VPC).
mrgmSecurityGroupIds :: Lens' ModifyReplicationGroup ([Text])
mrgmSecurityGroupIds = lens _mrgmSecurityGroupIds (\s a -> s { _mrgmSecurityGroupIds = a })
{-# INLINE mrgmSecurityGroupIds #-}

-- | The weekly time range (in UTC) during which replication group system
-- maintenance can occur. Note that system maintenance may result in an
-- outage. This change is made immediately. If you are moving this window to
-- the current time, there must be at least 120 minutes between the current
-- time and end of the window to ensure that pending changes are applied.
mrgmPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmPreferredMaintenanceWindow = lens _mrgmPreferredMaintenanceWindow (\s a -> s { _mrgmPreferredMaintenanceWindow = a })
{-# INLINE mrgmPreferredMaintenanceWindow #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent. The Amazon SNS topic owner must be same as the
-- replication group owner.
mrgmNotificationTopicArn :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmNotificationTopicArn = lens _mrgmNotificationTopicArn (\s a -> s { _mrgmNotificationTopicArn = a })
{-# INLINE mrgmNotificationTopicArn #-}

-- | The name of the cache parameter group to apply to all of the cache nodes in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the ApplyImmediately parameter is specified as
-- true for this request.
mrgmCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmCacheParameterGroupName = lens _mrgmCacheParameterGroupName (\s a -> s { _mrgmCacheParameterGroupName = a })
{-# INLINE mrgmCacheParameterGroupName #-}

-- | The status of the Amazon SNS notification topic for the replication group.
-- Notifications are sent only if the status is active. Valid values: active |
-- inactive.
mrgmNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmNotificationTopicStatus = lens _mrgmNotificationTopicStatus (\s a -> s { _mrgmNotificationTopicStatus = a })
{-# INLINE mrgmNotificationTopicStatus #-}

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- replication group. If false, then changes to the nodes in the replication
-- group are applied on the next maintenance reboot, or the next failure
-- reboot, whichever occurs first. Valid values: true | false Default: false.
mrgmApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgmApplyImmediately = lens _mrgmApplyImmediately (\s a -> s { _mrgmApplyImmediately = a })
{-# INLINE mrgmApplyImmediately #-}

-- | The upgraded version of the cache engine to be run on the nodes in the
-- replication group..
mrgmEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmEngineVersion = lens _mrgmEngineVersion (\s a -> s { _mrgmEngineVersion = a })
{-# INLINE mrgmEngineVersion #-}

-- | Determines whether minor engine upgrades will be applied automatically to
-- all of the cache nodes in the replication group during the maintenance
-- window. A value of true allows these upgrades to occur; false disables
-- automatic upgrades.
mrgmAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgmAutoMinorVersionUpgrade = lens _mrgmAutoMinorVersionUpgrade (\s a -> s { _mrgmAutoMinorVersionUpgrade = a })
{-# INLINE mrgmAutoMinorVersionUpgrade #-}

-- | If this parameter is specified, ElastiCache will promote each of the nodes
-- in the specified cache cluster to the primary role. The nodes of all other
-- clusters in the replication group will be read replicas.
mrgmPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmPrimaryClusterId = lens _mrgmPrimaryClusterId (\s a -> s { _mrgmPrimaryClusterId = a })
{-# INLINE mrgmPrimaryClusterId #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mrgmSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Integer)
mrgmSnapshotRetentionLimit = lens _mrgmSnapshotRetentionLimit (\s a -> s { _mrgmSnapshotRetentionLimit = a })
{-# INLINE mrgmSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of the cache cluster specified by SnapshottingClusterId.
-- Example: 05:00-09:00 If you do not specify this parameter, then ElastiCache
-- will automatically choose an appropriate time range.
mrgmSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmSnapshotWindow = lens _mrgmSnapshotWindow (\s a -> s { _mrgmSnapshotWindow = a })
{-# INLINE mrgmSnapshotWindow #-}

-- | The cache cluster ID that will be used as the daily snapshot source for the
-- replication group.
mrgmSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgmSnapshottingClusterId = lens _mrgmSnapshottingClusterId (\s a -> s { _mrgmSnapshottingClusterId = a })
{-# INLINE mrgmSnapshottingClusterId #-}

instance ToQuery ModifyReplicationGroup where
    toQuery = genericQuery def

newtype ModifyReplicationGroupResponse = ModifyReplicationGroupResponse
    { _rgyReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific replication group.
rgyReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
rgyReplicationGroup = lens _rgyReplicationGroup (\s a -> s { _rgyReplicationGroup = a })
{-# INLINE rgyReplicationGroup #-}

instance FromXML ModifyReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyReplicationGroup where
    type Sv ModifyReplicationGroup = ElastiCache
    type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse

    request = post "ModifyReplicationGroup"
    response _ = xmlResponse
