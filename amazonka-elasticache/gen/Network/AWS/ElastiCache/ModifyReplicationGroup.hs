{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /ModifyReplicationGroup/ action modifies the settings for a
-- replication group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyReplicationGroup.html>
module Network.AWS.ElastiCache.ModifyReplicationGroup
    (
    -- * Request
      ModifyReplicationGroup
    -- ** Request constructor
    , modifyReplicationGroup
    -- ** Request lenses
    , mrgrqAutomaticFailoverEnabled
    , mrgrqEngineVersion
    , mrgrqSnapshottingClusterId
    , mrgrqSecurityGroupIds
    , mrgrqAutoMinorVersionUpgrade
    , mrgrqReplicationGroupDescription
    , mrgrqCacheParameterGroupName
    , mrgrqSnapshotWindow
    , mrgrqPrimaryClusterId
    , mrgrqPreferredMaintenanceWindow
    , mrgrqSnapshotRetentionLimit
    , mrgrqNotificationTopicStatus
    , mrgrqApplyImmediately
    , mrgrqNotificationTopicARN
    , mrgrqCacheSecurityGroupNames
    , mrgrqReplicationGroupId

    -- * Response
    , ModifyReplicationGroupResponse
    -- ** Response constructor
    , modifyReplicationGroupResponse
    -- ** Response lenses
    , mrgrsReplicationGroup
    , mrgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ModifyReplicationGroups/ action.
--
-- /See:/ 'modifyReplicationGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgrqAutomaticFailoverEnabled'
--
-- * 'mrgrqEngineVersion'
--
-- * 'mrgrqSnapshottingClusterId'
--
-- * 'mrgrqSecurityGroupIds'
--
-- * 'mrgrqAutoMinorVersionUpgrade'
--
-- * 'mrgrqReplicationGroupDescription'
--
-- * 'mrgrqCacheParameterGroupName'
--
-- * 'mrgrqSnapshotWindow'
--
-- * 'mrgrqPrimaryClusterId'
--
-- * 'mrgrqPreferredMaintenanceWindow'
--
-- * 'mrgrqSnapshotRetentionLimit'
--
-- * 'mrgrqNotificationTopicStatus'
--
-- * 'mrgrqApplyImmediately'
--
-- * 'mrgrqNotificationTopicARN'
--
-- * 'mrgrqCacheSecurityGroupNames'
--
-- * 'mrgrqReplicationGroupId'
data ModifyReplicationGroup = ModifyReplicationGroup'
    { _mrgrqAutomaticFailoverEnabled    :: !(Maybe Bool)
    , _mrgrqEngineVersion               :: !(Maybe Text)
    , _mrgrqSnapshottingClusterId       :: !(Maybe Text)
    , _mrgrqSecurityGroupIds            :: !(Maybe [Text])
    , _mrgrqAutoMinorVersionUpgrade     :: !(Maybe Bool)
    , _mrgrqReplicationGroupDescription :: !(Maybe Text)
    , _mrgrqCacheParameterGroupName     :: !(Maybe Text)
    , _mrgrqSnapshotWindow              :: !(Maybe Text)
    , _mrgrqPrimaryClusterId            :: !(Maybe Text)
    , _mrgrqPreferredMaintenanceWindow  :: !(Maybe Text)
    , _mrgrqSnapshotRetentionLimit      :: !(Maybe Int)
    , _mrgrqNotificationTopicStatus     :: !(Maybe Text)
    , _mrgrqApplyImmediately            :: !(Maybe Bool)
    , _mrgrqNotificationTopicARN        :: !(Maybe Text)
    , _mrgrqCacheSecurityGroupNames     :: !(Maybe [Text])
    , _mrgrqReplicationGroupId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyReplicationGroup' smart constructor.
modifyReplicationGroup :: Text -> ModifyReplicationGroup
modifyReplicationGroup pReplicationGroupId_ =
    ModifyReplicationGroup'
    { _mrgrqAutomaticFailoverEnabled = Nothing
    , _mrgrqEngineVersion = Nothing
    , _mrgrqSnapshottingClusterId = Nothing
    , _mrgrqSecurityGroupIds = Nothing
    , _mrgrqAutoMinorVersionUpgrade = Nothing
    , _mrgrqReplicationGroupDescription = Nothing
    , _mrgrqCacheParameterGroupName = Nothing
    , _mrgrqSnapshotWindow = Nothing
    , _mrgrqPrimaryClusterId = Nothing
    , _mrgrqPreferredMaintenanceWindow = Nothing
    , _mrgrqSnapshotRetentionLimit = Nothing
    , _mrgrqNotificationTopicStatus = Nothing
    , _mrgrqApplyImmediately = Nothing
    , _mrgrqNotificationTopicARN = Nothing
    , _mrgrqCacheSecurityGroupNames = Nothing
    , _mrgrqReplicationGroupId = pReplicationGroupId_
    }

-- | Whether a read replica will be automatically promoted to read\/write
-- primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
--
-- ElastiCache Multi-AZ replication groups are not supported on:
--
-- -   Redis versions earlier than 2.8.6.
-- -   T1 and T2 cache node types.
mrgrqAutomaticFailoverEnabled :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgrqAutomaticFailoverEnabled = lens _mrgrqAutomaticFailoverEnabled (\ s a -> s{_mrgrqAutomaticFailoverEnabled = a});

-- | The upgraded version of the cache engine to be run on the cache clusters
-- in the replication group.
mrgrqEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqEngineVersion = lens _mrgrqEngineVersion (\ s a -> s{_mrgrqEngineVersion = a});

-- | The cache cluster ID that will be used as the daily snapshot source for
-- the replication group.
mrgrqSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqSnapshottingClusterId = lens _mrgrqSnapshottingClusterId (\ s a -> s{_mrgrqSnapshottingClusterId = a});

-- | Specifies the VPC Security Groups associated with the cache clusters in
-- the replication group.
--
-- This parameter can be used only with replication group containing cache
-- clusters running in an Amazon Virtual Private Cloud (VPC).
mrgrqSecurityGroupIds :: Lens' ModifyReplicationGroup [Text]
mrgrqSecurityGroupIds = lens _mrgrqSecurityGroupIds (\ s a -> s{_mrgrqSecurityGroupIds = a}) . _Default;

-- | This parameter is currently disabled.
mrgrqAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgrqAutoMinorVersionUpgrade = lens _mrgrqAutoMinorVersionUpgrade (\ s a -> s{_mrgrqAutoMinorVersionUpgrade = a});

-- | A description for the replication group. Maximum length is 255
-- characters.
mrgrqReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqReplicationGroupDescription = lens _mrgrqReplicationGroupDescription (\ s a -> s{_mrgrqReplicationGroupDescription = a});

-- | The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the /ApplyImmediately/ parameter is
-- specified as /true/ for this request.
mrgrqCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqCacheParameterGroupName = lens _mrgrqCacheParameterGroupName (\ s a -> s{_mrgrqCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of the node group specified by /SnapshottingClusterId/.
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, then ElastiCache will
-- automatically choose an appropriate time range.
mrgrqSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqSnapshotWindow = lens _mrgrqSnapshotWindow (\ s a -> s{_mrgrqSnapshotWindow = a});

-- | If this parameter is specified, ElastiCache will promote each of the
-- cache clusters in the specified replication group to the primary role.
-- The nodes of all other cache clusters in the replication group will be
-- read replicas.
mrgrqPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqPrimaryClusterId = lens _mrgrqPrimaryClusterId (\ s a -> s{_mrgrqPrimaryClusterId = a});

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- -   @sun@
-- -   @mon@
-- -   @tue@
-- -   @wed@
-- -   @thu@
-- -   @fri@
-- -   @sat@
--
-- Example: @sun:05:00-sun:09:00@
mrgrqPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqPreferredMaintenanceWindow = lens _mrgrqPreferredMaintenanceWindow (\ s a -> s{_mrgrqPreferredMaintenanceWindow = a});

-- | The number of days for which ElastiCache will retain automatic node
-- group snapshots before deleting them. For example, if you set
-- /SnapshotRetentionLimit/ to 5, then a snapshot that was taken today will
-- be retained for 5 days before being deleted.
--
-- __Important__
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
mrgrqSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Int)
mrgrqSnapshotRetentionLimit = lens _mrgrqSnapshotRetentionLimit (\ s a -> s{_mrgrqSnapshotRetentionLimit = a});

-- | The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is /active/.
--
-- Valid values: @active@ | @inactive@
mrgrqNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqNotificationTopicStatus = lens _mrgrqNotificationTopicStatus (\ s a -> s{_mrgrqNotificationTopicStatus = a});

-- | If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the /PreferredMaintenanceWindow/ setting for the
-- replication group.
--
-- If @false@, then changes to the nodes in the replication group are
-- applied on the next maintenance reboot, or the next failure reboot,
-- whichever occurs first.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
mrgrqApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgrqApplyImmediately = lens _mrgrqApplyImmediately (\ s a -> s{_mrgrqApplyImmediately = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
mrgrqNotificationTopicARN :: Lens' ModifyReplicationGroup (Maybe Text)
mrgrqNotificationTopicARN = lens _mrgrqNotificationTopicARN (\ s a -> s{_mrgrqNotificationTopicARN = a});

-- | A list of cache security group names to authorize for the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible.
--
-- This parameter can be used only with replication group containing cache
-- clusters running outside of an Amazon Virtual Private Cloud (VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be \"Default\".
mrgrqCacheSecurityGroupNames :: Lens' ModifyReplicationGroup [Text]
mrgrqCacheSecurityGroupNames = lens _mrgrqCacheSecurityGroupNames (\ s a -> s{_mrgrqCacheSecurityGroupNames = a}) . _Default;

-- | The identifier of the replication group to modify.
mrgrqReplicationGroupId :: Lens' ModifyReplicationGroup Text
mrgrqReplicationGroupId = lens _mrgrqReplicationGroupId (\ s a -> s{_mrgrqReplicationGroupId = a});

instance AWSRequest ModifyReplicationGroup where
        type Sv ModifyReplicationGroup = ElastiCache
        type Rs ModifyReplicationGroup =
             ModifyReplicationGroupResponse
        request = post
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
                 _mrgrqAutomaticFailoverEnabled,
               "EngineVersion" =: _mrgrqEngineVersion,
               "SnapshottingClusterId" =:
                 _mrgrqSnapshottingClusterId,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _mrgrqSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _mrgrqAutoMinorVersionUpgrade,
               "ReplicationGroupDescription" =:
                 _mrgrqReplicationGroupDescription,
               "CacheParameterGroupName" =:
                 _mrgrqCacheParameterGroupName,
               "SnapshotWindow" =: _mrgrqSnapshotWindow,
               "PrimaryClusterId" =: _mrgrqPrimaryClusterId,
               "PreferredMaintenanceWindow" =:
                 _mrgrqPreferredMaintenanceWindow,
               "SnapshotRetentionLimit" =:
                 _mrgrqSnapshotRetentionLimit,
               "NotificationTopicStatus" =:
                 _mrgrqNotificationTopicStatus,
               "ApplyImmediately" =: _mrgrqApplyImmediately,
               "NotificationTopicArn" =: _mrgrqNotificationTopicARN,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _mrgrqCacheSecurityGroupNames),
               "ReplicationGroupId" =: _mrgrqReplicationGroupId]

-- | /See:/ 'modifyReplicationGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrgrsReplicationGroup'
--
-- * 'mrgrsStatus'
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
    { _mrgrsReplicationGroup :: !(Maybe ReplicationGroup)
    , _mrgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyReplicationGroupResponse' smart constructor.
modifyReplicationGroupResponse :: Int -> ModifyReplicationGroupResponse
modifyReplicationGroupResponse pStatus_ =
    ModifyReplicationGroupResponse'
    { _mrgrsReplicationGroup = Nothing
    , _mrgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
mrgrsReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
mrgrsReplicationGroup = lens _mrgrsReplicationGroup (\ s a -> s{_mrgrsReplicationGroup = a});

-- | FIXME: Undocumented member.
mrgrsStatus :: Lens' ModifyReplicationGroupResponse Int
mrgrsStatus = lens _mrgrsStatus (\ s a -> s{_mrgrsStatus = a});
