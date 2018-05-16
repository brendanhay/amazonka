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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a replication group.
--
--
-- /Important:/ Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.
--
module Network.AWS.ElastiCache.ModifyReplicationGroup
    (
    -- * Creating a Request
      modifyReplicationGroup
    , ModifyReplicationGroup
    -- * Request Lenses
    , mrgAutomaticFailoverEnabled
    , mrgEngineVersion
    , mrgCacheNodeType
    , mrgSnapshottingClusterId
    , mrgSecurityGroupIds
    , mrgAutoMinorVersionUpgrade
    , mrgCacheParameterGroupName
    , mrgReplicationGroupDescription
    , mrgSnapshotWindow
    , mrgPrimaryClusterId
    , mrgPreferredMaintenanceWindow
    , mrgNodeGroupId
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
    , mrgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @ModifyReplicationGroups@ operation.
--
--
--
-- /See:/ 'modifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
  { _mrgAutomaticFailoverEnabled    :: !(Maybe Bool)
  , _mrgEngineVersion               :: !(Maybe Text)
  , _mrgCacheNodeType               :: !(Maybe Text)
  , _mrgSnapshottingClusterId       :: !(Maybe Text)
  , _mrgSecurityGroupIds            :: !(Maybe [Text])
  , _mrgAutoMinorVersionUpgrade     :: !(Maybe Bool)
  , _mrgCacheParameterGroupName     :: !(Maybe Text)
  , _mrgReplicationGroupDescription :: !(Maybe Text)
  , _mrgSnapshotWindow              :: !(Maybe Text)
  , _mrgPrimaryClusterId            :: !(Maybe Text)
  , _mrgPreferredMaintenanceWindow  :: !(Maybe Text)
  , _mrgNodeGroupId                 :: !(Maybe Text)
  , _mrgSnapshotRetentionLimit      :: !(Maybe Int)
  , _mrgNotificationTopicStatus     :: !(Maybe Text)
  , _mrgApplyImmediately            :: !(Maybe Bool)
  , _mrgNotificationTopicARN        :: !(Maybe Text)
  , _mrgCacheSecurityGroupNames     :: !(Maybe [Text])
  , _mrgReplicationGroupId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgAutomaticFailoverEnabled' - Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure. Valid values: @true@ | @false@  Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
--
-- * 'mrgEngineVersion' - The upgraded version of the cache engine to be run on the clusters in the replication group. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
--
-- * 'mrgCacheNodeType' - A valid cache node type that you want to scale this replication group to.
--
-- * 'mrgSnapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
--
-- * 'mrgSecurityGroupIds' - Specifies the VPC Security Groups associated with the clusters in the replication group. This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- * 'mrgAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'mrgCacheParameterGroupName' - The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
--
-- * 'mrgReplicationGroupDescription' - A description for the replication group. Maximum length is 255 characters.
--
-- * 'mrgSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ . Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- * 'mrgPrimaryClusterId' - For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
--
-- * 'mrgPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
--
-- * 'mrgNodeGroupId' - The name of the Node Group (called shard in the console).
--
-- * 'mrgSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- * 'mrgNotificationTopicStatus' - The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ . Valid values: @active@ | @inactive@
--
-- * 'mrgApplyImmediately' - If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group. If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first. Valid values: @true@ | @false@  Default: @false@
--
-- * 'mrgNotificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
--
-- * 'mrgCacheSecurityGroupNames' - A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible. This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC). Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
--
-- * 'mrgReplicationGroupId' - The identifier of the replication group to modify.
modifyReplicationGroup
    :: Text -- ^ 'mrgReplicationGroupId'
    -> ModifyReplicationGroup
modifyReplicationGroup pReplicationGroupId_ =
  ModifyReplicationGroup'
    { _mrgAutomaticFailoverEnabled = Nothing
    , _mrgEngineVersion = Nothing
    , _mrgCacheNodeType = Nothing
    , _mrgSnapshottingClusterId = Nothing
    , _mrgSecurityGroupIds = Nothing
    , _mrgAutoMinorVersionUpgrade = Nothing
    , _mrgCacheParameterGroupName = Nothing
    , _mrgReplicationGroupDescription = Nothing
    , _mrgSnapshotWindow = Nothing
    , _mrgPrimaryClusterId = Nothing
    , _mrgPreferredMaintenanceWindow = Nothing
    , _mrgNodeGroupId = Nothing
    , _mrgSnapshotRetentionLimit = Nothing
    , _mrgNotificationTopicStatus = Nothing
    , _mrgApplyImmediately = Nothing
    , _mrgNotificationTopicARN = Nothing
    , _mrgCacheSecurityGroupNames = Nothing
    , _mrgReplicationGroupId = pReplicationGroupId_
    }


-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure. Valid values: @true@ | @false@  Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
mrgAutomaticFailoverEnabled :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutomaticFailoverEnabled = lens _mrgAutomaticFailoverEnabled (\ s a -> s{_mrgAutomaticFailoverEnabled = a})

-- | The upgraded version of the cache engine to be run on the clusters in the replication group. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
mrgEngineVersion :: Lens' ModifyReplicationGroup (Maybe Text)
mrgEngineVersion = lens _mrgEngineVersion (\ s a -> s{_mrgEngineVersion = a})

-- | A valid cache node type that you want to scale this replication group to.
mrgCacheNodeType :: Lens' ModifyReplicationGroup (Maybe Text)
mrgCacheNodeType = lens _mrgCacheNodeType (\ s a -> s{_mrgCacheNodeType = a})

-- | The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
mrgSnapshottingClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshottingClusterId = lens _mrgSnapshottingClusterId (\ s a -> s{_mrgSnapshottingClusterId = a})

-- | Specifies the VPC Security Groups associated with the clusters in the replication group. This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
mrgSecurityGroupIds :: Lens' ModifyReplicationGroup [Text]
mrgSecurityGroupIds = lens _mrgSecurityGroupIds (\ s a -> s{_mrgSecurityGroupIds = a}) . _Default . _Coerce

-- | This parameter is currently disabled.
mrgAutoMinorVersionUpgrade :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgAutoMinorVersionUpgrade = lens _mrgAutoMinorVersionUpgrade (\ s a -> s{_mrgAutoMinorVersionUpgrade = a})

-- | The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
mrgCacheParameterGroupName :: Lens' ModifyReplicationGroup (Maybe Text)
mrgCacheParameterGroupName = lens _mrgCacheParameterGroupName (\ s a -> s{_mrgCacheParameterGroupName = a})

-- | A description for the replication group. Maximum length is 255 characters.
mrgReplicationGroupDescription :: Lens' ModifyReplicationGroup (Maybe Text)
mrgReplicationGroupDescription = lens _mrgReplicationGroupDescription (\ s a -> s{_mrgReplicationGroupDescription = a})

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ . Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
mrgSnapshotWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgSnapshotWindow = lens _mrgSnapshotWindow (\ s a -> s{_mrgSnapshotWindow = a})

-- | For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
mrgPrimaryClusterId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPrimaryClusterId = lens _mrgPrimaryClusterId (\ s a -> s{_mrgPrimaryClusterId = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
mrgPreferredMaintenanceWindow :: Lens' ModifyReplicationGroup (Maybe Text)
mrgPreferredMaintenanceWindow = lens _mrgPreferredMaintenanceWindow (\ s a -> s{_mrgPreferredMaintenanceWindow = a})

-- | The name of the Node Group (called shard in the console).
mrgNodeGroupId :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNodeGroupId = lens _mrgNodeGroupId (\ s a -> s{_mrgNodeGroupId = a})

-- | The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
mrgSnapshotRetentionLimit :: Lens' ModifyReplicationGroup (Maybe Int)
mrgSnapshotRetentionLimit = lens _mrgSnapshotRetentionLimit (\ s a -> s{_mrgSnapshotRetentionLimit = a})

-- | The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ . Valid values: @active@ | @inactive@
mrgNotificationTopicStatus :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicStatus = lens _mrgNotificationTopicStatus (\ s a -> s{_mrgNotificationTopicStatus = a})

-- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group. If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first. Valid values: @true@ | @false@  Default: @false@
mrgApplyImmediately :: Lens' ModifyReplicationGroup (Maybe Bool)
mrgApplyImmediately = lens _mrgApplyImmediately (\ s a -> s{_mrgApplyImmediately = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
mrgNotificationTopicARN :: Lens' ModifyReplicationGroup (Maybe Text)
mrgNotificationTopicARN = lens _mrgNotificationTopicARN (\ s a -> s{_mrgNotificationTopicARN = a})

-- | A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible. This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC). Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
mrgCacheSecurityGroupNames :: Lens' ModifyReplicationGroup [Text]
mrgCacheSecurityGroupNames = lens _mrgCacheSecurityGroupNames (\ s a -> s{_mrgCacheSecurityGroupNames = a}) . _Default . _Coerce

-- | The identifier of the replication group to modify.
mrgReplicationGroupId :: Lens' ModifyReplicationGroup Text
mrgReplicationGroupId = lens _mrgReplicationGroupId (\ s a -> s{_mrgReplicationGroupId = a})

instance AWSRequest ModifyReplicationGroup where
        type Rs ModifyReplicationGroup =
             ModifyReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ModifyReplicationGroupResult"
              (\ s h x ->
                 ModifyReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable ModifyReplicationGroup where

instance NFData ModifyReplicationGroup where

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
               "CacheNodeType" =: _mrgCacheNodeType,
               "SnapshottingClusterId" =: _mrgSnapshottingClusterId,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _mrgSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _mrgAutoMinorVersionUpgrade,
               "CacheParameterGroupName" =:
                 _mrgCacheParameterGroupName,
               "ReplicationGroupDescription" =:
                 _mrgReplicationGroupDescription,
               "SnapshotWindow" =: _mrgSnapshotWindow,
               "PrimaryClusterId" =: _mrgPrimaryClusterId,
               "PreferredMaintenanceWindow" =:
                 _mrgPreferredMaintenanceWindow,
               "NodeGroupId" =: _mrgNodeGroupId,
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
  , _mrgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgrsReplicationGroup' - Undocumented member.
--
-- * 'mrgrsResponseStatus' - -- | The response status code.
modifyReplicationGroupResponse
    :: Int -- ^ 'mrgrsResponseStatus'
    -> ModifyReplicationGroupResponse
modifyReplicationGroupResponse pResponseStatus_ =
  ModifyReplicationGroupResponse'
    {_mrgrsReplicationGroup = Nothing, _mrgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mrgrsReplicationGroup :: Lens' ModifyReplicationGroupResponse (Maybe ReplicationGroup)
mrgrsReplicationGroup = lens _mrgrsReplicationGroup (\ s a -> s{_mrgrsReplicationGroup = a})

-- | -- | The response status code.
mrgrsResponseStatus :: Lens' ModifyReplicationGroupResponse Int
mrgrsResponseStatus = lens _mrgrsResponseStatus (\ s a -> s{_mrgrsResponseStatus = a})

instance NFData ModifyReplicationGroupResponse where
