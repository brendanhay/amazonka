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

-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /ModifyReplicationGroup/ action modifies the settings for a replication
-- group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyReplicationGroup.html>
module Network.AWS.ElastiCache.ModifyReplicationGroup
    (
    -- * Request
      ModifyReplicationGroup
    -- ** Request constructor
    , modifyReplicationGroup
    -- ** Request lenses
    , mrgApplyImmediately
    , mrgAutoMinorVersionUpgrade
    , mrgAutomaticFailoverEnabled
    , mrgCacheParameterGroupName
    , mrgCacheSecurityGroupNames
    , mrgEngineVersion
    , mrgNotificationTopicArn
    , mrgNotificationTopicStatus
    , mrgPreferredMaintenanceWindow
    , mrgPrimaryClusterId
    , mrgReplicationGroupDescription
    , mrgReplicationGroupId
    , mrgSecurityGroupIds
    , mrgSnapshotRetentionLimit
    , mrgSnapshotWindow
    , mrgSnapshottingClusterId

    -- * Response
    , ModifyReplicationGroupResponse
    -- ** Response constructor
    , modifyReplicationGroupResponse
    -- ** Response lenses
    , mrgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data ModifyReplicationGroup = ModifyReplicationGroup
    { _mrgApplyImmediately            :: Maybe Bool
    , _mrgAutoMinorVersionUpgrade     :: Maybe Bool
    , _mrgAutomaticFailoverEnabled    :: Maybe Bool
    , _mrgCacheParameterGroupName     :: Maybe Text
    , _mrgCacheSecurityGroupNames     :: List "member" Text
    , _mrgEngineVersion               :: Maybe Text
    , _mrgNotificationTopicArn        :: Maybe Text
    , _mrgNotificationTopicStatus     :: Maybe Text
    , _mrgPreferredMaintenanceWindow  :: Maybe Text
    , _mrgPrimaryClusterId            :: Maybe Text
    , _mrgReplicationGroupDescription :: Maybe Text
    , _mrgReplicationGroupId          :: Text
    , _mrgSecurityGroupIds            :: List "member" Text
    , _mrgSnapshotRetentionLimit      :: Maybe Int
    , _mrgSnapshotWindow              :: Maybe Text
    , _mrgSnapshottingClusterId       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyReplicationGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mrgAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mrgAutomaticFailoverEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'mrgCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mrgCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'mrgEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mrgNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'mrgNotificationTopicStatus' @::@ 'Maybe' 'Text'
--
-- * 'mrgPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mrgPrimaryClusterId' @::@ 'Maybe' 'Text'
--
-- * 'mrgReplicationGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mrgReplicationGroupId' @::@ 'Text'
--
-- * 'mrgSecurityGroupIds' @::@ ['Text']
--
-- * 'mrgSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'mrgSnapshotWindow' @::@ 'Maybe' 'Text'
--
-- * 'mrgSnapshottingClusterId' @::@ 'Maybe' 'Text'
--
modifyReplicationGroup :: Text -- ^ 'mrgReplicationGroupId'
                       -> ModifyReplicationGroup
modifyReplicationGroup p1 = ModifyReplicationGroup
    { _mrgReplicationGroupId          = p1
    , _mrgReplicationGroupDescription = Nothing
    , _mrgPrimaryClusterId            = Nothing
    , _mrgSnapshottingClusterId       = Nothing
    , _mrgAutomaticFailoverEnabled    = Nothing
    , _mrgCacheSecurityGroupNames     = mempty
    , _mrgSecurityGroupIds            = mempty
    , _mrgPreferredMaintenanceWindow  = Nothing
    , _mrgNotificationTopicArn        = Nothing
    , _mrgCacheParameterGroupName     = Nothing
    , _mrgNotificationTopicStatus     = Nothing
    , _mrgApplyImmediately            = Nothing
    , _mrgEngineVersion               = Nothing
    , _mrgAutoMinorVersionUpgrade     = Nothing
    , _mrgSnapshotRetentionLimit      = Nothing
    , _mrgSnapshotWindow              = Nothing
    }

-- | If 'true', this parameter causes the modifications in this request and any
-- pending modifications to be applied, asynchronously and as soon as possible,
-- regardless of the /PreferredMaintenanceWindow/ setting for the replication
-- group.
--
-- If 'false', then changes to the nodes in the replication group are applied on
-- the next maintenance reboot, or the next failure reboot, whichever occurs
-- first.
--
-- Valid values: 'true' | 'false'
--
-- Default: 'false'
mrgApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgApplyImmediately =
    lens _mrgApplyImmediately (\s a -> s { _mrgApplyImmediately = a })

-- | This parameter is currently disabled.
mrgAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutoMinorVersionUpgrade =
    lens _mrgAutoMinorVersionUpgrade
        (\s a -> s { _mrgAutoMinorVersionUpgrade = a })

-- | Whether a read replica will be automatically promoted to read/write primary
-- if the existing primary encounters a failure.
--
-- Valid values: 'true' | 'false'
--
-- ElastiCache Multi-AZ replication groups are not supported on:
--
-- Redis versions earlier than 2.8.6. T1 and T2 cache node types.
mrgAutomaticFailoverEnabled :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutomaticFailoverEnabled =
    lens _mrgAutomaticFailoverEnabled
        (\s a -> s { _mrgAutomaticFailoverEnabled = a })

-- | The name of the cache parameter group to apply to all of the clusters in this
-- replication group. This change is asynchronously applied as soon as possible
-- for parameters when the /ApplyImmediately/ parameter is specified as /true/ for
-- this request.
mrgCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgCacheParameterGroupName =
    lens _mrgCacheParameterGroupName
        (\s a -> s { _mrgCacheParameterGroupName = a })

-- | A list of cache security group names to authorize for the clusters in this
-- replication group. This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with replication group containing cache
-- clusters running outside of an Amazon Virtual Private Cloud (VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not
-- be "Default".
mrgCacheSecurityGroupNames :: Lens' ModifyReplicationGroup [Text]
mrgCacheSecurityGroupNames =
    lens _mrgCacheSecurityGroupNames
        (\s a -> s { _mrgCacheSecurityGroupNames = a })
            . _List

-- | The upgraded version of the cache engine to be run on the cache clusters in
-- the replication group.
mrgEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgEngineVersion = lens _mrgEngineVersion (\s a -> s { _mrgEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications
-- will be sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
mrgNotificationTopicArn :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicArn =
    lens _mrgNotificationTopicArn (\s a -> s { _mrgNotificationTopicArn = a })

-- | The status of the Amazon SNS notification topic for the replication group.
-- Notifications are sent only if the status is /active/.
--
-- Valid values: 'active' | 'inactive'
mrgNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicStatus =
    lens _mrgNotificationTopicStatus
        (\s a -> s { _mrgNotificationTopicStatus = a })

-- | The weekly time range (in UTC) during which replication group system
-- maintenance can occur. Note that system maintenance may result in an outage.
-- This change is made immediately. If you are moving this window to the current
-- time, there must be at least 120 minutes between the current time and end of
-- the window to ensure that pending changes are applied.
mrgPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPreferredMaintenanceWindow =
    lens _mrgPreferredMaintenanceWindow
        (\s a -> s { _mrgPreferredMaintenanceWindow = a })

-- | If this parameter is specified, ElastiCache will promote each of the cache
-- clusters in the specified replication group to the primary role. The nodes of
-- all other cache clusters in the replication group will be read replicas.
mrgPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPrimaryClusterId =
    lens _mrgPrimaryClusterId (\s a -> s { _mrgPrimaryClusterId = a })

-- | A description for the replication group. Maximum length is 255 characters.
mrgReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgReplicationGroupDescription =
    lens _mrgReplicationGroupDescription
        (\s a -> s { _mrgReplicationGroupDescription = a })

-- | The identifier of the replication group to modify.
mrgReplicationGroupId :: Lens' ModifyReplicationGroup Text
mrgReplicationGroupId =
    lens _mrgReplicationGroupId (\s a -> s { _mrgReplicationGroupId = a })

-- | Specifies the VPC Security Groups associated with the cache clusters in the
-- replication group.
--
-- This parameter can be used only with replication group containing cache
-- clusters running in an Amazon Virtual Private Cloud (VPC).
mrgSecurityGroupIds :: Lens' ModifyReplicationGroup [Text]
mrgSecurityGroupIds =
    lens _mrgSecurityGroupIds (\s a -> s { _mrgSecurityGroupIds = a })
        . _List

-- | The number of days for which ElastiCache will retain automatic node group
-- snapshots before deleting them. For example, if you set /SnapshotRetentionLimit/
-- to 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted.
--
-- Important
-- If the value of SnapshotRetentionLimit is set to zero (0), backups
-- are turned off.
mrgSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Int)
mrgSnapshotRetentionLimit =
    lens _mrgSnapshotRetentionLimit
        (\s a -> s { _mrgSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of the node group specified by /SnapshottingClusterId/.
--
-- Example: '05:00-09:00'
--
-- If you do not specify this parameter, then ElastiCache will automatically
-- choose an appropriate time range.
mrgSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshotWindow =
    lens _mrgSnapshotWindow (\s a -> s { _mrgSnapshotWindow = a })

-- | The cache cluster ID that will be used as the daily snapshot source for the
-- replication group.
mrgSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshottingClusterId =
    lens _mrgSnapshottingClusterId
        (\s a -> s { _mrgSnapshottingClusterId = a })

newtype ModifyReplicationGroupResponse = ModifyReplicationGroupResponse
    { _mrgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Eq, Read, Show)

-- | 'ModifyReplicationGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
modifyReplicationGroupResponse :: ModifyReplicationGroupResponse
modifyReplicationGroupResponse = ModifyReplicationGroupResponse
    { _mrgrReplicationGroup = Nothing
    }

mrgrReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
mrgrReplicationGroup =
    lens _mrgrReplicationGroup (\s a -> s { _mrgrReplicationGroup = a })

instance ToPath ModifyReplicationGroup where
    toPath = const "/"

instance ToQuery ModifyReplicationGroup where
    toQuery ModifyReplicationGroup{..} = mconcat
        [ "ApplyImmediately"            =? _mrgApplyImmediately
        , "AutoMinorVersionUpgrade"     =? _mrgAutoMinorVersionUpgrade
        , "AutomaticFailoverEnabled"    =? _mrgAutomaticFailoverEnabled
        , "CacheParameterGroupName"     =? _mrgCacheParameterGroupName
        , "CacheSecurityGroupNames"     =? _mrgCacheSecurityGroupNames
        , "EngineVersion"               =? _mrgEngineVersion
        , "NotificationTopicArn"        =? _mrgNotificationTopicArn
        , "NotificationTopicStatus"     =? _mrgNotificationTopicStatus
        , "PreferredMaintenanceWindow"  =? _mrgPreferredMaintenanceWindow
        , "PrimaryClusterId"            =? _mrgPrimaryClusterId
        , "ReplicationGroupDescription" =? _mrgReplicationGroupDescription
        , "ReplicationGroupId"          =? _mrgReplicationGroupId
        , "SecurityGroupIds"            =? _mrgSecurityGroupIds
        , "SnapshotRetentionLimit"      =? _mrgSnapshotRetentionLimit
        , "SnapshotWindow"              =? _mrgSnapshotWindow
        , "SnapshottingClusterId"       =? _mrgSnapshottingClusterId
        ]

instance ToHeaders ModifyReplicationGroup

instance AWSRequest ModifyReplicationGroup where
    type Sv ModifyReplicationGroup = ElastiCache
    type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse

    request  = post "ModifyReplicationGroup"
    response = xmlResponse

instance FromXML ModifyReplicationGroupResponse where
    parseXML = withElement "ModifyReplicationGroupResult" $ \x -> ModifyReplicationGroupResponse
        <$> x .@? "ReplicationGroup"
