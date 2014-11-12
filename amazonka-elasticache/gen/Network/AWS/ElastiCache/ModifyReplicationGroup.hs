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

-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyReplicationGroup operation modifies the settings for a
-- replication group.
module Network.AWS.ElastiCache.ModifyReplicationGroup
    (
    -- * Request
      ModifyReplicationGroupMessage
    -- ** Request constructor
    , modifyReplicationGroupMessage
    -- ** Request lenses
    , mrgmApplyImmediately
    , mrgmAutoMinorVersionUpgrade
    , mrgmAutomaticFailoverEnabled
    , mrgmCacheParameterGroupName
    , mrgmCacheSecurityGroupNames
    , mrgmEngineVersion
    , mrgmNotificationTopicArn
    , mrgmNotificationTopicStatus
    , mrgmPreferredMaintenanceWindow
    , mrgmPrimaryClusterId
    , mrgmReplicationGroupDescription
    , mrgmReplicationGroupId
    , mrgmSecurityGroupIds
    , mrgmSnapshotRetentionLimit
    , mrgmSnapshotWindow
    , mrgmSnapshottingClusterId

    -- * Response
    , ModifyReplicationGroupResult
    -- ** Response constructor
    , modifyReplicationGroupResult
    -- ** Response lenses
    , mrgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data ModifyReplicationGroupMessage = ModifyReplicationGroupMessage
    { _mrgmApplyImmediately            :: Maybe Bool
    , _mrgmAutoMinorVersionUpgrade     :: Maybe Bool
    , _mrgmAutomaticFailoverEnabled    :: Maybe Bool
    , _mrgmCacheParameterGroupName     :: Maybe Text
    , _mrgmCacheSecurityGroupNames     :: [Text]
    , _mrgmEngineVersion               :: Maybe Text
    , _mrgmNotificationTopicArn        :: Maybe Text
    , _mrgmNotificationTopicStatus     :: Maybe Text
    , _mrgmPreferredMaintenanceWindow  :: Maybe Text
    , _mrgmPrimaryClusterId            :: Maybe Text
    , _mrgmReplicationGroupDescription :: Maybe Text
    , _mrgmReplicationGroupId          :: Text
    , _mrgmSecurityGroupIds            :: [Text]
    , _mrgmSnapshotRetentionLimit      :: Maybe Int
    , _mrgmSnapshotWindow              :: Maybe Text
    , _mrgmSnapshottingClusterId       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyReplicationGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgmApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mrgmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mrgmAutomaticFailoverEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'mrgmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mrgmCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'mrgmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mrgmNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'mrgmNotificationTopicStatus' @::@ 'Maybe' 'Text'
--
-- * 'mrgmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mrgmPrimaryClusterId' @::@ 'Maybe' 'Text'
--
-- * 'mrgmReplicationGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mrgmReplicationGroupId' @::@ 'Text'
--
-- * 'mrgmSecurityGroupIds' @::@ ['Text']
--
-- * 'mrgmSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'mrgmSnapshotWindow' @::@ 'Maybe' 'Text'
--
-- * 'mrgmSnapshottingClusterId' @::@ 'Maybe' 'Text'
--
modifyReplicationGroupMessage :: Text -- ^ 'mrgmReplicationGroupId'
                              -> ModifyReplicationGroupMessage
modifyReplicationGroupMessage p1 = ModifyReplicationGroupMessage
    { _mrgmReplicationGroupId          = p1
    , _mrgmReplicationGroupDescription = Nothing
    , _mrgmPrimaryClusterId            = Nothing
    , _mrgmSnapshottingClusterId       = Nothing
    , _mrgmAutomaticFailoverEnabled    = Nothing
    , _mrgmCacheSecurityGroupNames     = mempty
    , _mrgmSecurityGroupIds            = mempty
    , _mrgmPreferredMaintenanceWindow  = Nothing
    , _mrgmNotificationTopicArn        = Nothing
    , _mrgmCacheParameterGroupName     = Nothing
    , _mrgmNotificationTopicStatus     = Nothing
    , _mrgmApplyImmediately            = Nothing
    , _mrgmEngineVersion               = Nothing
    , _mrgmAutoMinorVersionUpgrade     = Nothing
    , _mrgmSnapshotRetentionLimit      = Nothing
    , _mrgmSnapshotWindow              = Nothing
    }

-- | If true, this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the PreferredMaintenanceWindow setting for the
-- replication group. If false, then changes to the nodes in the replication
-- group are applied on the next maintenance reboot, or the next failure
-- reboot, whichever occurs first. Valid values: true | false Default:
-- false.
mrgmApplyImmediately :: Lens' ModifyReplicationGroupMessage (Maybe Bool)
mrgmApplyImmediately =
    lens _mrgmApplyImmediately (\s a -> s { _mrgmApplyImmediately = a })

-- | Determines whether minor engine upgrades will be applied automatically to
-- all of the clusters in the replication group during the maintenance
-- window. A value of true allows these upgrades to occur; false disables
-- automatic upgrades.
mrgmAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroupMessage (Maybe Bool)
mrgmAutoMinorVersionUpgrade =
    lens _mrgmAutoMinorVersionUpgrade
        (\s a -> s { _mrgmAutoMinorVersionUpgrade = a })

-- | Whether a read replica will be automatically promoted to read/write
-- primary if the existing primary encounters a failure. Valid values: true
-- | false.
mrgmAutomaticFailoverEnabled :: Lens' ModifyReplicationGroupMessage (Maybe Bool)
mrgmAutomaticFailoverEnabled =
    lens _mrgmAutomaticFailoverEnabled
        (\s a -> s { _mrgmAutomaticFailoverEnabled = a })

-- | The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the ApplyImmediately parameter is specified
-- as true for this request.
mrgmCacheParameterGroupName :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmCacheParameterGroupName =
    lens _mrgmCacheParameterGroupName
        (\s a -> s { _mrgmCacheParameterGroupName = a })

-- | A list of cache security group names to authorize for the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible. This parameter can be used only with replication group
-- containing cache clusters running outside of an Amazon Virtual Private
-- Cloud (VPC). Constraints: Must contain no more than 255 alphanumeric
-- characters. Must not be "Default".
mrgmCacheSecurityGroupNames :: Lens' ModifyReplicationGroupMessage [Text]
mrgmCacheSecurityGroupNames =
    lens _mrgmCacheSecurityGroupNames
        (\s a -> s { _mrgmCacheSecurityGroupNames = a })

-- | The upgraded version of the cache engine to be run on the cache clusters
-- in the replication group.
mrgmEngineVersion :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmEngineVersion =
    lens _mrgmEngineVersion (\s a -> s { _mrgmEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
mrgmNotificationTopicArn :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmNotificationTopicArn =
    lens _mrgmNotificationTopicArn
        (\s a -> s { _mrgmNotificationTopicArn = a })

-- | The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is active. Valid values:
-- active | inactive.
mrgmNotificationTopicStatus :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmNotificationTopicStatus =
    lens _mrgmNotificationTopicStatus
        (\s a -> s { _mrgmNotificationTopicStatus = a })

-- | The weekly time range (in UTC) during which replication group system
-- maintenance can occur. Note that system maintenance may result in an
-- outage. This change is made immediately. If you are moving this window to
-- the current time, there must be at least 120 minutes between the current
-- time and end of the window to ensure that pending changes are applied.
mrgmPreferredMaintenanceWindow :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmPreferredMaintenanceWindow =
    lens _mrgmPreferredMaintenanceWindow
        (\s a -> s { _mrgmPreferredMaintenanceWindow = a })

-- | If this parameter is specified, ElastiCache will promote each of the
-- cache clusters in the specified replication group to the primary role.
-- The nodes of all other cache clusters in the replication group will be
-- read replicas.
mrgmPrimaryClusterId :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmPrimaryClusterId =
    lens _mrgmPrimaryClusterId (\s a -> s { _mrgmPrimaryClusterId = a })

-- | A description for the replication group. Maximum length is 255
-- characters.
mrgmReplicationGroupDescription :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmReplicationGroupDescription =
    lens _mrgmReplicationGroupDescription
        (\s a -> s { _mrgmReplicationGroupDescription = a })

-- | The identifier of the replication group to modify.
mrgmReplicationGroupId :: Lens' ModifyReplicationGroupMessage Text
mrgmReplicationGroupId =
    lens _mrgmReplicationGroupId (\s a -> s { _mrgmReplicationGroupId = a })

-- | Specifies the VPC Security Groups associated with the cache clusters in
-- the replication group. This parameter can be used only with replication
-- group containing cache clusters running in an Amazon Virtual Private
-- Cloud (VPC).
mrgmSecurityGroupIds :: Lens' ModifyReplicationGroupMessage [Text]
mrgmSecurityGroupIds =
    lens _mrgmSecurityGroupIds (\s a -> s { _mrgmSecurityGroupIds = a })

-- | The number of days for which ElastiCache will retain automatic node group
-- snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
mrgmSnapshotRetentionLimit :: Lens' ModifyReplicationGroupMessage (Maybe Int)
mrgmSnapshotRetentionLimit =
    lens _mrgmSnapshotRetentionLimit
        (\s a -> s { _mrgmSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of the node group specified by SnapshottingClusterId.
-- Example: 05:00-09:00 If you do not specify this parameter, then
-- ElastiCache will automatically choose an appropriate time range.
mrgmSnapshotWindow :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmSnapshotWindow =
    lens _mrgmSnapshotWindow (\s a -> s { _mrgmSnapshotWindow = a })

-- | The cache cluster ID that will be used as the daily snapshot source for
-- the replication group.
mrgmSnapshottingClusterId :: Lens' ModifyReplicationGroupMessage (Maybe Text)
mrgmSnapshottingClusterId =
    lens _mrgmSnapshottingClusterId
        (\s a -> s { _mrgmSnapshottingClusterId = a })

instance ToQuery ModifyReplicationGroupMessage

instance ToPath ModifyReplicationGroupMessage where
    toPath = const "/"

newtype ModifyReplicationGroupResult = ModifyReplicationGroupResult
    { _mrgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyReplicationGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
modifyReplicationGroupResult :: ModifyReplicationGroupResult
modifyReplicationGroupResult = ModifyReplicationGroupResult
    { _mrgrReplicationGroup = Nothing
    }

mrgrReplicationGroup :: Lens' ModifyReplicationGroupResult (Maybe ReplicationGroup)
mrgrReplicationGroup =
    lens _mrgrReplicationGroup (\s a -> s { _mrgrReplicationGroup = a })

instance FromXML ModifyReplicationGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyReplicationGroupResult"

instance AWSRequest ModifyReplicationGroupMessage where
    type Sv ModifyReplicationGroupMessage = ElastiCache
    type Rs ModifyReplicationGroupMessage = ModifyReplicationGroupResult

    request  = post "ModifyReplicationGroup"
    response = xmlResponse $ \h x -> ModifyReplicationGroupResult
        <$> x %| "ReplicationGroup"
