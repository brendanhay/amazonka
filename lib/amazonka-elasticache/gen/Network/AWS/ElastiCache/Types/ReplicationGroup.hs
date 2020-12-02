{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroup where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.MultiAZStatus
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains all of the attributes of a specific Redis replication group.
--
--
--
-- /See:/ 'replicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { _rgAuthTokenLastModifiedDate ::
      !(Maybe ISO8601),
    _rgStatus :: !(Maybe Text),
    _rgCacheNodeType :: !(Maybe Text),
    _rgNodeGroups :: !(Maybe [NodeGroup]),
    _rgSnapshottingClusterId :: !(Maybe Text),
    _rgClusterEnabled :: !(Maybe Bool),
    _rgAtRestEncryptionEnabled :: !(Maybe Bool),
    _rgARN :: !(Maybe Text),
    _rgTransitEncryptionEnabled :: !(Maybe Bool),
    _rgUserGroupIds :: !(Maybe [Text]),
    _rgSnapshotWindow :: !(Maybe Text),
    _rgConfigurationEndpoint :: !(Maybe Endpoint),
    _rgAuthTokenEnabled :: !(Maybe Bool),
    _rgMemberClusters :: !(Maybe [Text]),
    _rgKMSKeyId :: !(Maybe Text),
    _rgMultiAZ :: !(Maybe MultiAZStatus),
    _rgSnapshotRetentionLimit :: !(Maybe Int),
    _rgDescription :: !(Maybe Text),
    _rgReplicationGroupId :: !(Maybe Text),
    _rgPendingModifiedValues ::
      !(Maybe ReplicationGroupPendingModifiedValues),
    _rgGlobalReplicationGroupInfo ::
      !(Maybe GlobalReplicationGroupInfo),
    _rgMemberClustersOutpostARNs :: !(Maybe [Text]),
    _rgAutomaticFailover :: !(Maybe AutomaticFailoverStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgAuthTokenLastModifiedDate' - The date the auth token was last modified
--
-- * 'rgStatus' - The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
--
-- * 'rgCacheNodeType' - The name of the compute and memory capacity node type for each node in the replication group.
--
-- * 'rgNodeGroups' - A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
--
-- * 'rgSnapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the replication group.
--
-- * 'rgClusterEnabled' - A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups). Valid values: @true@ | @false@
--
-- * 'rgAtRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later. Default: @false@
--
-- * 'rgARN' - The ARN (Amazon Resource Name) of the replication group.
--
-- * 'rgTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later. Default: @false@
--
-- * 'rgUserGroupIds' - The list of user group IDs that have access to the replication group.
--
-- * 'rgSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- * 'rgConfigurationEndpoint' - The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
--
-- * 'rgAuthTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
--
-- * 'rgMemberClusters' - The names of all the cache clusters that are part of this replication group.
--
-- * 'rgKMSKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
--
-- * 'rgMultiAZ' - A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
--
-- * 'rgSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
--
-- * 'rgDescription' - The user supplied description of the replication group.
--
-- * 'rgReplicationGroupId' - The identifier for the replication group.
--
-- * 'rgPendingModifiedValues' - A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
--
-- * 'rgGlobalReplicationGroupInfo' - The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- * 'rgMemberClustersOutpostARNs' - The outpost ARNs of the replication group's member clusters.
--
-- * 'rgAutomaticFailover' - Indicates the status of automatic failover for this Redis replication group.
replicationGroup ::
  ReplicationGroup
replicationGroup =
  ReplicationGroup'
    { _rgAuthTokenLastModifiedDate = Nothing,
      _rgStatus = Nothing,
      _rgCacheNodeType = Nothing,
      _rgNodeGroups = Nothing,
      _rgSnapshottingClusterId = Nothing,
      _rgClusterEnabled = Nothing,
      _rgAtRestEncryptionEnabled = Nothing,
      _rgARN = Nothing,
      _rgTransitEncryptionEnabled = Nothing,
      _rgUserGroupIds = Nothing,
      _rgSnapshotWindow = Nothing,
      _rgConfigurationEndpoint = Nothing,
      _rgAuthTokenEnabled = Nothing,
      _rgMemberClusters = Nothing,
      _rgKMSKeyId = Nothing,
      _rgMultiAZ = Nothing,
      _rgSnapshotRetentionLimit = Nothing,
      _rgDescription = Nothing,
      _rgReplicationGroupId = Nothing,
      _rgPendingModifiedValues = Nothing,
      _rgGlobalReplicationGroupInfo = Nothing,
      _rgMemberClustersOutpostARNs = Nothing,
      _rgAutomaticFailover = Nothing
    }

-- | The date the auth token was last modified
rgAuthTokenLastModifiedDate :: Lens' ReplicationGroup (Maybe UTCTime)
rgAuthTokenLastModifiedDate = lens _rgAuthTokenLastModifiedDate (\s a -> s {_rgAuthTokenLastModifiedDate = a}) . mapping _Time

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\s a -> s {_rgStatus = a})

-- | The name of the compute and memory capacity node type for each node in the replication group.
rgCacheNodeType :: Lens' ReplicationGroup (Maybe Text)
rgCacheNodeType = lens _rgCacheNodeType (\s a -> s {_rgCacheNodeType = a})

-- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
rgNodeGroups :: Lens' ReplicationGroup [NodeGroup]
rgNodeGroups = lens _rgNodeGroups (\s a -> s {_rgNodeGroups = a}) . _Default . _Coerce

-- | The cluster ID that is used as the daily snapshot source for the replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId = lens _rgSnapshottingClusterId (\s a -> s {_rgSnapshottingClusterId = a})

-- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups). Valid values: @true@ | @false@
rgClusterEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgClusterEnabled = lens _rgClusterEnabled (\s a -> s {_rgClusterEnabled = a})

-- | A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later. Default: @false@
rgAtRestEncryptionEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgAtRestEncryptionEnabled = lens _rgAtRestEncryptionEnabled (\s a -> s {_rgAtRestEncryptionEnabled = a})

-- | The ARN (Amazon Resource Name) of the replication group.
rgARN :: Lens' ReplicationGroup (Maybe Text)
rgARN = lens _rgARN (\s a -> s {_rgARN = a})

-- | A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later. Default: @false@
rgTransitEncryptionEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgTransitEncryptionEnabled = lens _rgTransitEncryptionEnabled (\s a -> s {_rgTransitEncryptionEnabled = a})

-- | The list of user group IDs that have access to the replication group.
rgUserGroupIds :: Lens' ReplicationGroup [Text]
rgUserGroupIds = lens _rgUserGroupIds (\s a -> s {_rgUserGroupIds = a}) . _Default . _Coerce

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
rgSnapshotWindow :: Lens' ReplicationGroup (Maybe Text)
rgSnapshotWindow = lens _rgSnapshotWindow (\s a -> s {_rgSnapshotWindow = a})

-- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
rgConfigurationEndpoint :: Lens' ReplicationGroup (Maybe Endpoint)
rgConfigurationEndpoint = lens _rgConfigurationEndpoint (\s a -> s {_rgConfigurationEndpoint = a})

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
rgAuthTokenEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgAuthTokenEnabled = lens _rgAuthTokenEnabled (\s a -> s {_rgAuthTokenEnabled = a})

-- | The names of all the cache clusters that are part of this replication group.
rgMemberClusters :: Lens' ReplicationGroup [Text]
rgMemberClusters = lens _rgMemberClusters (\s a -> s {_rgMemberClusters = a}) . _Default . _Coerce

-- | The ID of the KMS key used to encrypt the disk in the cluster.
rgKMSKeyId :: Lens' ReplicationGroup (Maybe Text)
rgKMSKeyId = lens _rgKMSKeyId (\s a -> s {_rgKMSKeyId = a})

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
rgMultiAZ :: Lens' ReplicationGroup (Maybe MultiAZStatus)
rgMultiAZ = lens _rgMultiAZ (\s a -> s {_rgMultiAZ = a})

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
rgSnapshotRetentionLimit :: Lens' ReplicationGroup (Maybe Int)
rgSnapshotRetentionLimit = lens _rgSnapshotRetentionLimit (\s a -> s {_rgSnapshotRetentionLimit = a})

-- | The user supplied description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\s a -> s {_rgDescription = a})

-- | The identifier for the replication group.
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId = lens _rgReplicationGroupId (\s a -> s {_rgReplicationGroupId = a})

-- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = lens _rgPendingModifiedValues (\s a -> s {_rgPendingModifiedValues = a})

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
rgGlobalReplicationGroupInfo :: Lens' ReplicationGroup (Maybe GlobalReplicationGroupInfo)
rgGlobalReplicationGroupInfo = lens _rgGlobalReplicationGroupInfo (\s a -> s {_rgGlobalReplicationGroupInfo = a})

-- | The outpost ARNs of the replication group's member clusters.
rgMemberClustersOutpostARNs :: Lens' ReplicationGroup [Text]
rgMemberClustersOutpostARNs = lens _rgMemberClustersOutpostARNs (\s a -> s {_rgMemberClustersOutpostARNs = a}) . _Default . _Coerce

-- | Indicates the status of automatic failover for this Redis replication group.
rgAutomaticFailover :: Lens' ReplicationGroup (Maybe AutomaticFailoverStatus)
rgAutomaticFailover = lens _rgAutomaticFailover (\s a -> s {_rgAutomaticFailover = a})

instance FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      <$> (x .@? "AuthTokenLastModifiedDate")
      <*> (x .@? "Status")
      <*> (x .@? "CacheNodeType")
      <*> (x .@? "NodeGroups" .!@ mempty >>= may (parseXMLList "NodeGroup"))
      <*> (x .@? "SnapshottingClusterId")
      <*> (x .@? "ClusterEnabled")
      <*> (x .@? "AtRestEncryptionEnabled")
      <*> (x .@? "ARN")
      <*> (x .@? "TransitEncryptionEnabled")
      <*> (x .@? "UserGroupIds" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "SnapshotWindow")
      <*> (x .@? "ConfigurationEndpoint")
      <*> (x .@? "AuthTokenEnabled")
      <*> ( x .@? "MemberClusters" .!@ mempty
              >>= may (parseXMLList "ClusterId")
          )
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "MultiAZ")
      <*> (x .@? "SnapshotRetentionLimit")
      <*> (x .@? "Description")
      <*> (x .@? "ReplicationGroupId")
      <*> (x .@? "PendingModifiedValues")
      <*> (x .@? "GlobalReplicationGroupInfo")
      <*> ( x .@? "MemberClustersOutpostArns" .!@ mempty
              >>= may (parseXMLList "ReplicationGroupOutpostArn")
          )
      <*> (x .@? "AutomaticFailover")

instance Hashable ReplicationGroup

instance NFData ReplicationGroup
