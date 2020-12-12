{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroup
  ( ReplicationGroup (..),

    -- * Smart constructor
    mkReplicationGroup,

    -- * Lenses
    rgAuthTokenLastModifiedDate,
    rgStatus,
    rgCacheNodeType,
    rgNodeGroups,
    rgSnapshottingClusterId,
    rgClusterEnabled,
    rgAtRestEncryptionEnabled,
    rgARN,
    rgTransitEncryptionEnabled,
    rgUserGroupIds,
    rgSnapshotWindow,
    rgConfigurationEndpoint,
    rgAuthTokenEnabled,
    rgMemberClusters,
    rgKMSKeyId,
    rgMultiAZ,
    rgSnapshotRetentionLimit,
    rgDescription,
    rgReplicationGroupId,
    rgPendingModifiedValues,
    rgGlobalReplicationGroupInfo,
    rgMemberClustersOutpostARNs,
    rgAutomaticFailover,
  )
where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.MultiAZStatus
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'mkReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { authTokenLastModifiedDate ::
      Lude.Maybe Lude.DateTime,
    status :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    nodeGroups :: Lude.Maybe [NodeGroup],
    snapshottingClusterId :: Lude.Maybe Lude.Text,
    clusterEnabled :: Lude.Maybe Lude.Bool,
    atRestEncryptionEnabled :: Lude.Maybe Lude.Bool,
    arn :: Lude.Maybe Lude.Text,
    transitEncryptionEnabled :: Lude.Maybe Lude.Bool,
    userGroupIds :: Lude.Maybe [Lude.Text],
    snapshotWindow :: Lude.Maybe Lude.Text,
    configurationEndpoint :: Lude.Maybe Endpoint,
    authTokenEnabled :: Lude.Maybe Lude.Bool,
    memberClusters :: Lude.Maybe [Lude.Text],
    kmsKeyId :: Lude.Maybe Lude.Text,
    multiAZ :: Lude.Maybe MultiAZStatus,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    replicationGroupId :: Lude.Maybe Lude.Text,
    pendingModifiedValues ::
      Lude.Maybe ReplicationGroupPendingModifiedValues,
    globalReplicationGroupInfo ::
      Lude.Maybe GlobalReplicationGroupInfo,
    memberClustersOutpostARNs :: Lude.Maybe [Lude.Text],
    automaticFailover :: Lude.Maybe AutomaticFailoverStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon Resource Name) of the replication group.
-- * 'atRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- * 'authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
-- * 'authTokenLastModifiedDate' - The date the auth token was last modified
-- * 'automaticFailover' - Indicates the status of automatic failover for this Redis replication group.
-- * 'cacheNodeType' - The name of the compute and memory capacity node type for each node in the replication group.
-- * 'clusterEnabled' - A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
--
-- Valid values: @true@ | @false@
-- * 'configurationEndpoint' - The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
-- * 'description' - The user supplied description of the replication group.
-- * 'globalReplicationGroupInfo' - The name of the Global Datastore and role of this replication group in the Global Datastore.
-- * 'kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
-- * 'memberClusters' - The names of all the cache clusters that are part of this replication group.
-- * 'memberClustersOutpostARNs' - The outpost ARNs of the replication group's member clusters.
-- * 'multiAZ' - A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
-- * 'nodeGroups' - A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
-- * 'pendingModifiedValues' - A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
-- * 'replicationGroupId' - The identifier for the replication group.
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
-- * 'snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the replication group.
-- * 'status' - The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
-- * 'transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- * 'userGroupIds' - The list of user group IDs that have access to the replication group.
mkReplicationGroup ::
  ReplicationGroup
mkReplicationGroup =
  ReplicationGroup'
    { authTokenLastModifiedDate = Lude.Nothing,
      status = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      nodeGroups = Lude.Nothing,
      snapshottingClusterId = Lude.Nothing,
      clusterEnabled = Lude.Nothing,
      atRestEncryptionEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      transitEncryptionEnabled = Lude.Nothing,
      userGroupIds = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      configurationEndpoint = Lude.Nothing,
      authTokenEnabled = Lude.Nothing,
      memberClusters = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      multiAZ = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      description = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing,
      globalReplicationGroupInfo = Lude.Nothing,
      memberClustersOutpostARNs = Lude.Nothing,
      automaticFailover = Lude.Nothing
    }

-- | The date the auth token was last modified
--
-- /Note:/ Consider using 'authTokenLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.DateTime)
rgAuthTokenLastModifiedDate = Lens.lens (authTokenLastModifiedDate :: ReplicationGroup -> Lude.Maybe Lude.DateTime) (\s a -> s {authTokenLastModifiedDate = a} :: ReplicationGroup)
{-# DEPRECATED rgAuthTokenLastModifiedDate "Use generic-lens or generic-optics with 'authTokenLastModifiedDate' instead." #-}

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgStatus = Lens.lens (status :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ReplicationGroup)
{-# DEPRECATED rgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the compute and memory capacity node type for each node in the replication group.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCacheNodeType :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgCacheNodeType = Lens.lens (cacheNodeType :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ReplicationGroup)
{-# DEPRECATED rgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
--
-- /Note:/ Consider using 'nodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgNodeGroups :: Lens.Lens' ReplicationGroup (Lude.Maybe [NodeGroup])
rgNodeGroups = Lens.lens (nodeGroups :: ReplicationGroup -> Lude.Maybe [NodeGroup]) (\s a -> s {nodeGroups = a} :: ReplicationGroup)
{-# DEPRECATED rgNodeGroups "Use generic-lens or generic-optics with 'nodeGroups' instead." #-}

-- | The cluster ID that is used as the daily snapshot source for the replication group.
--
-- /Note:/ Consider using 'snapshottingClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshottingClusterId :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgSnapshottingClusterId = Lens.lens (snapshottingClusterId :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshottingClusterId = a} :: ReplicationGroup)
{-# DEPRECATED rgSnapshottingClusterId "Use generic-lens or generic-optics with 'snapshottingClusterId' instead." #-}

-- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgClusterEnabled :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Bool)
rgClusterEnabled = Lens.lens (clusterEnabled :: ReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {clusterEnabled = a} :: ReplicationGroup)
{-# DEPRECATED rgClusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead." #-}

-- | A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAtRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Bool)
rgAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: ReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: ReplicationGroup)
{-# DEPRECATED rgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | The ARN (Amazon Resource Name) of the replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgARN :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgARN = Lens.lens (arn :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ReplicationGroup)
{-# DEPRECATED rgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTransitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Bool)
rgTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: ReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: ReplicationGroup)
{-# DEPRECATED rgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The list of user group IDs that have access to the replication group.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgUserGroupIds :: Lens.Lens' ReplicationGroup (Lude.Maybe [Lude.Text])
rgUserGroupIds = Lens.lens (userGroupIds :: ReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIds = a} :: ReplicationGroup)
{-# DEPRECATED rgUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotWindow :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgSnapshotWindow = Lens.lens (snapshotWindow :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: ReplicationGroup)
{-# DEPRECATED rgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
--
-- /Note:/ Consider using 'configurationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgConfigurationEndpoint :: Lens.Lens' ReplicationGroup (Lude.Maybe Endpoint)
rgConfigurationEndpoint = Lens.lens (configurationEndpoint :: ReplicationGroup -> Lude.Maybe Endpoint) (\s a -> s {configurationEndpoint = a} :: ReplicationGroup)
{-# DEPRECATED rgConfigurationEndpoint "Use generic-lens or generic-optics with 'configurationEndpoint' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenEnabled :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Bool)
rgAuthTokenEnabled = Lens.lens (authTokenEnabled :: ReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {authTokenEnabled = a} :: ReplicationGroup)
{-# DEPRECATED rgAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | The names of all the cache clusters that are part of this replication group.
--
-- /Note:/ Consider using 'memberClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClusters :: Lens.Lens' ReplicationGroup (Lude.Maybe [Lude.Text])
rgMemberClusters = Lens.lens (memberClusters :: ReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {memberClusters = a} :: ReplicationGroup)
{-# DEPRECATED rgMemberClusters "Use generic-lens or generic-optics with 'memberClusters' instead." #-}

-- | The ID of the KMS key used to encrypt the disk in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgKMSKeyId :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgKMSKeyId = Lens.lens (kmsKeyId :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ReplicationGroup)
{-# DEPRECATED rgKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMultiAZ :: Lens.Lens' ReplicationGroup (Lude.Maybe MultiAZStatus)
rgMultiAZ = Lens.lens (multiAZ :: ReplicationGroup -> Lude.Maybe MultiAZStatus) (\s a -> s {multiAZ = a} :: ReplicationGroup)
{-# DEPRECATED rgMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Int)
rgSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: ReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: ReplicationGroup)
{-# DEPRECATED rgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The user supplied description of the replication group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgDescription :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgDescription = Lens.lens (description :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ReplicationGroup)
{-# DEPRECATED rgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier for the replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgReplicationGroupId :: Lens.Lens' ReplicationGroup (Lude.Maybe Lude.Text)
rgReplicationGroupId = Lens.lens (replicationGroupId :: ReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: ReplicationGroup)
{-# DEPRECATED rgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgPendingModifiedValues :: Lens.Lens' ReplicationGroup (Lude.Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = Lens.lens (pendingModifiedValues :: ReplicationGroup -> Lude.Maybe ReplicationGroupPendingModifiedValues) (\s a -> s {pendingModifiedValues = a} :: ReplicationGroup)
{-# DEPRECATED rgPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- /Note:/ Consider using 'globalReplicationGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGlobalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Lude.Maybe GlobalReplicationGroupInfo)
rgGlobalReplicationGroupInfo = Lens.lens (globalReplicationGroupInfo :: ReplicationGroup -> Lude.Maybe GlobalReplicationGroupInfo) (\s a -> s {globalReplicationGroupInfo = a} :: ReplicationGroup)
{-# DEPRECATED rgGlobalReplicationGroupInfo "Use generic-lens or generic-optics with 'globalReplicationGroupInfo' instead." #-}

-- | The outpost ARNs of the replication group's member clusters.
--
-- /Note:/ Consider using 'memberClustersOutpostARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClustersOutpostARNs :: Lens.Lens' ReplicationGroup (Lude.Maybe [Lude.Text])
rgMemberClustersOutpostARNs = Lens.lens (memberClustersOutpostARNs :: ReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {memberClustersOutpostARNs = a} :: ReplicationGroup)
{-# DEPRECATED rgMemberClustersOutpostARNs "Use generic-lens or generic-optics with 'memberClustersOutpostARNs' instead." #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAutomaticFailover :: Lens.Lens' ReplicationGroup (Lude.Maybe AutomaticFailoverStatus)
rgAutomaticFailover = Lens.lens (automaticFailover :: ReplicationGroup -> Lude.Maybe AutomaticFailoverStatus) (\s a -> s {automaticFailover = a} :: ReplicationGroup)
{-# DEPRECATED rgAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

instance Lude.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Lude.<$> (x Lude..@? "AuthTokenLastModifiedDate")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CacheNodeType")
      Lude.<*> ( x Lude..@? "NodeGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeGroup")
               )
      Lude.<*> (x Lude..@? "SnapshottingClusterId")
      Lude.<*> (x Lude..@? "ClusterEnabled")
      Lude.<*> (x Lude..@? "AtRestEncryptionEnabled")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "TransitEncryptionEnabled")
      Lude.<*> ( x Lude..@? "UserGroupIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "SnapshotWindow")
      Lude.<*> (x Lude..@? "ConfigurationEndpoint")
      Lude.<*> (x Lude..@? "AuthTokenEnabled")
      Lude.<*> ( x Lude..@? "MemberClusters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ClusterId")
               )
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "MultiAZ")
      Lude.<*> (x Lude..@? "SnapshotRetentionLimit")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
      Lude.<*> (x Lude..@? "PendingModifiedValues")
      Lude.<*> (x Lude..@? "GlobalReplicationGroupInfo")
      Lude.<*> ( x Lude..@? "MemberClustersOutpostArns" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ReplicationGroupOutpostArn")
               )
      Lude.<*> (x Lude..@? "AutomaticFailover")
