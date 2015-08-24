{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /ModifyReplicationGroup/ action modifies the settings for a
-- replication group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyReplicationGroup.html AWS API Reference> for ModifyReplicationGroup.
module Network.AWS.ElastiCache.ModifyReplicationGroup
    (
    -- * Creating a Request
      modifyReplicationGroup
    , ModifyReplicationGroup
    -- * Request Lenses
    , mrgAutomaticFailoverEnabled
    , mrgEngineVersion
    , mrgSnapshottingClusterId
    , mrgSecurityGroupIds
    , mrgAutoMinorVersionUpgrade
    , mrgReplicationGroupDescription
    , mrgCacheParameterGroupName
    , mrgSnapshotWindow
    , mrgPrimaryClusterId
    , mrgPreferredMaintenanceWindow
    , mrgSnapshotRetentionLimit
    , mrgNotificationTopicStatus
    , mrgApplyImmediately
    , mrgNotificationTopicARN
    , mrgCacheSecurityGroupNames
    , mrgReplicationGroupId

    -- * Destructuring the Response
    , modifyReplicationGroupResponse
    , ModifyReplicationGroupResponse
    -- * Response Lenses
    , mrgrsReplicationGroup
    , mrgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ModifyReplicationGroups/ action.
--
-- /See:/ 'modifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
    { _mrgAutomaticFailoverEnabled    :: !(Maybe Bool)
    , _mrgEngineVersion               :: !(Maybe Text)
    , _mrgSnapshottingClusterId       :: !(Maybe Text)
    , _mrgSecurityGroupIds            :: !(Maybe [Text])
    , _mrgAutoMinorVersionUpgrade     :: !(Maybe Bool)
    , _mrgReplicationGroupDescription :: !(Maybe Text)
    , _mrgCacheParameterGroupName     :: !(Maybe Text)
    , _mrgSnapshotWindow              :: !(Maybe Text)
    , _mrgPrimaryClusterId            :: !(Maybe Text)
    , _mrgPreferredMaintenanceWindow  :: !(Maybe Text)
    , _mrgSnapshotRetentionLimit      :: !(Maybe Int)
    , _mrgNotificationTopicStatus     :: !(Maybe Text)
    , _mrgApplyImmediately            :: !(Maybe Bool)
    , _mrgNotificationTopicARN        :: !(Maybe Text)
    , _mrgCacheSecurityGroupNames     :: !(Maybe [Text])
    , _mrgReplicationGroupId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgAutomaticFailoverEnabled'
--
-- * 'mrgEngineVersion'
--
-- * 'mrgSnapshottingClusterId'
--
-- * 'mrgSecurityGroupIds'
--
-- * 'mrgAutoMinorVersionUpgrade'
--
-- * 'mrgReplicationGroupDescription'
--
-- * 'mrgCacheParameterGroupName'
--
-- * 'mrgSnapshotWindow'
--
-- * 'mrgPrimaryClusterId'
--
-- * 'mrgPreferredMaintenanceWindow'
--
-- * 'mrgSnapshotRetentionLimit'
--
-- * 'mrgNotificationTopicStatus'
--
-- * 'mrgApplyImmediately'
--
-- * 'mrgNotificationTopicARN'
--
-- * 'mrgCacheSecurityGroupNames'
--
-- * 'mrgReplicationGroupId'
modifyReplicationGroup
    :: Text -- ^ 'mrgReplicationGroupId'
    -> ModifyReplicationGroup
modifyReplicationGroup pReplicationGroupId_ =
    ModifyReplicationGroup'
    { _mrgAutomaticFailoverEnabled = Nothing
    , _mrgEngineVersion = Nothing
    , _mrgSnapshottingClusterId = Nothing
    , _mrgSecurityGroupIds = Nothing
    , _mrgAutoMinorVersionUpgrade = Nothing
    , _mrgReplicationGroupDescription = Nothing
    , _mrgCacheParameterGroupName = Nothing
    , _mrgSnapshotWindow = Nothing
    , _mrgPrimaryClusterId = Nothing
    , _mrgPreferredMaintenanceWindow = Nothing
    , _mrgSnapshotRetentionLimit = Nothing
    , _mrgNotificationTopicStatus = Nothing
    , _mrgApplyImmediately = Nothing
    , _mrgNotificationTopicARN = Nothing
    , _mrgCacheSecurityGroupNames = Nothing
    , _mrgReplicationGroupId = pReplicationGroupId_
    }

-- | Whether a read replica will be automatically promoted to read\/write
-- primary if the existing primary encounters a failure.
--
-- Valid values: 'true' | 'false'
--
-- ElastiCache Multi-AZ replication groups are not supported on:
--
-- -   Redis versions earlier than 2.8.6.
-- -   T1 and T2 cache node types.
mrgAutomaticFailoverEnabled :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutomaticFailoverEnabled = lens _mrgAutomaticFailoverEnabled (\ s a -> s{_mrgAutomaticFailoverEnabled = a});

-- | The upgraded version of the cache engine to be run on the cache clusters
-- in the replication group.
mrgEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgEngineVersion = lens _mrgEngineVersion (\ s a -> s{_mrgEngineVersion = a});

-- | The cache cluster ID that will be used as the daily snapshot source for
-- the replication group.
mrgSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshottingClusterId = lens _mrgSnapshottingClusterId (\ s a -> s{_mrgSnapshottingClusterId = a});

-- | Specifies the VPC Security Groups associated with the cache clusters in
-- the replication group.
--
-- This parameter can be used only with replication group containing cache
-- clusters running in an Amazon Virtual Private Cloud (VPC).
mrgSecurityGroupIds :: Lens' ModifyReplicationGroup [Text]
mrgSecurityGroupIds = lens _mrgSecurityGroupIds (\ s a -> s{_mrgSecurityGroupIds = a}) . _Default . _Coerce;

-- | This parameter is currently disabled.
mrgAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutoMinorVersionUpgrade = lens _mrgAutoMinorVersionUpgrade (\ s a -> s{_mrgAutoMinorVersionUpgrade = a});

-- | A description for the replication group. Maximum length is 255
-- characters.
mrgReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgReplicationGroupDescription = lens _mrgReplicationGroupDescription (\ s a -> s{_mrgReplicationGroupDescription = a});

-- | The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the /ApplyImmediately/ parameter is
-- specified as /true/ for this request.
mrgCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgCacheParameterGroupName = lens _mrgCacheParameterGroupName (\ s a -> s{_mrgCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of the node group specified by /SnapshottingClusterId/.
--
-- Example: '05:00-09:00'
--
-- If you do not specify this parameter, then ElastiCache will
-- automatically choose an appropriate time range.
mrgSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshotWindow = lens _mrgSnapshotWindow (\ s a -> s{_mrgSnapshotWindow = a});

-- | If this parameter is specified, ElastiCache will promote each of the
-- cache clusters in the specified replication group to the primary role.
-- The nodes of all other cache clusters in the replication group will be
-- read replicas.
mrgPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPrimaryClusterId = lens _mrgPrimaryClusterId (\ s a -> s{_mrgPrimaryClusterId = a});

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for 'ddd' are:
--
-- -   'sun'
-- -   'mon'
-- -   'tue'
-- -   'wed'
-- -   'thu'
-- -   'fri'
-- -   'sat'
--
-- Example: 'sun:05:00-sun:09:00'
mrgPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPreferredMaintenanceWindow = lens _mrgPreferredMaintenanceWindow (\ s a -> s{_mrgPreferredMaintenanceWindow = a});

-- | The number of days for which ElastiCache will retain automatic node
-- group snapshots before deleting them. For example, if you set
-- /SnapshotRetentionLimit/ to 5, then a snapshot that was taken today will
-- be retained for 5 days before being deleted.
--
-- __Important__
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
mrgSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Int)
mrgSnapshotRetentionLimit = lens _mrgSnapshotRetentionLimit (\ s a -> s{_mrgSnapshotRetentionLimit = a});

-- | The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is /active/.
--
-- Valid values: 'active' | 'inactive'
mrgNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicStatus = lens _mrgNotificationTopicStatus (\ s a -> s{_mrgNotificationTopicStatus = a});

-- | If 'true', this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the /PreferredMaintenanceWindow/ setting for the
-- replication group.
--
-- If 'false', then changes to the nodes in the replication group are
-- applied on the next maintenance reboot, or the next failure reboot,
-- whichever occurs first.
--
-- Valid values: 'true' | 'false'
--
-- Default: 'false'
mrgApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgApplyImmediately = lens _mrgApplyImmediately (\ s a -> s{_mrgApplyImmediately = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
mrgNotificationTopicARN :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicARN = lens _mrgNotificationTopicARN (\ s a -> s{_mrgNotificationTopicARN = a});

-- | A list of cache security group names to authorize for the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible.
--
-- This parameter can be used only with replication group containing cache
-- clusters running outside of an Amazon Virtual Private Cloud (VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be \"Default\".
mrgCacheSecurityGroupNames :: Lens' ModifyReplicationGroup [Text]
mrgCacheSecurityGroupNames = lens _mrgCacheSecurityGroupNames (\ s a -> s{_mrgCacheSecurityGroupNames = a}) . _Default . _Coerce;

-- | The identifier of the replication group to modify.
mrgReplicationGroupId :: Lens' ModifyReplicationGroup Text
mrgReplicationGroupId = lens _mrgReplicationGroupId (\ s a -> s{_mrgReplicationGroupId = a});

instance AWSRequest ModifyReplicationGroup where
        type Rs ModifyReplicationGroup =
             ModifyReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ModifyReplicationGroupResult"
              (\ s h x ->
                 ModifyReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance ToHeaders ModifyReplicationGroup where
        toHeaders = const mempty

instance ToPath ModifyReplicationGroup where
        toPath = const "/"

instance ToQuery ModifyReplicationGroup where
        toQuery ModifyReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "AutomaticFailoverEnabled" =:
                 _mrgAutomaticFailoverEnabled,
               "EngineVersion" =: _mrgEngineVersion,
               "SnapshottingClusterId" =: _mrgSnapshottingClusterId,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _mrgSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _mrgAutoMinorVersionUpgrade,
               "ReplicationGroupDescription" =:
                 _mrgReplicationGroupDescription,
               "CacheParameterGroupName" =:
                 _mrgCacheParameterGroupName,
               "SnapshotWindow" =: _mrgSnapshotWindow,
               "PrimaryClusterId" =: _mrgPrimaryClusterId,
               "PreferredMaintenanceWindow" =:
                 _mrgPreferredMaintenanceWindow,
               "SnapshotRetentionLimit" =:
                 _mrgSnapshotRetentionLimit,
               "NotificationTopicStatus" =:
                 _mrgNotificationTopicStatus,
               "ApplyImmediately" =: _mrgApplyImmediately,
               "NotificationTopicArn" =: _mrgNotificationTopicARN,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _mrgCacheSecurityGroupNames),
               "ReplicationGroupId" =: _mrgReplicationGroupId]

-- | /See:/ 'modifyReplicationGroupResponse' smart constructor.
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
    { _mrgrsReplicationGroup :: !(Maybe ReplicationGroup)
    , _mrgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgrsReplicationGroup'
--
-- * 'mrgrsStatus'
modifyReplicationGroupResponse
    :: Int -- ^ 'mrgrsStatus'
    -> ModifyReplicationGroupResponse
modifyReplicationGroupResponse pStatus_ =
    ModifyReplicationGroupResponse'
    { _mrgrsReplicationGroup = Nothing
    , _mrgrsStatus = pStatus_
    }

-- | Undocumented member.
mrgrsReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
mrgrsReplicationGroup = lens _mrgrsReplicationGroup (\ s a -> s{_mrgrsReplicationGroup = a});

-- | The response status code.
mrgrsStatus :: Lens' ModifyReplicationGroupResponse Int
mrgrsStatus = lens _mrgrsStatus (\ s a -> s{_mrgrsStatus = a});
