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
    rgARN,
    rgAtRestEncryptionEnabled,
    rgAuthTokenEnabled,
    rgAuthTokenLastModifiedDate,
    rgAutomaticFailover,
    rgCacheNodeType,
    rgClusterEnabled,
    rgConfigurationEndpoint,
    rgDescription,
    rgGlobalReplicationGroupInfo,
    rgKmsKeyId,
    rgMemberClusters,
    rgMemberClustersOutpostArns,
    rgMultiAZ,
    rgNodeGroups,
    rgPendingModifiedValues,
    rgReplicationGroupId,
    rgSnapshotRetentionLimit,
    rgSnapshotWindow,
    rgSnapshottingClusterId,
    rgStatus,
    rgTransitEncryptionEnabled,
    rgUserGroupIds,
  )
where

import qualified Network.AWS.ElastiCache.Types.AutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo as Types
import qualified Network.AWS.ElastiCache.Types.MultiAZStatus as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroup as Types
import qualified Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'mkReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { -- | The ARN (Amazon Resource Name) of the replication group.
    arn :: Core.Maybe Types.String,
    -- | A flag that enables encryption at-rest when set to @true@ .
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
    --
    -- Default: @false@
    authTokenEnabled :: Core.Maybe Core.Bool,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Core.Maybe Core.UTCTime,
    -- | Indicates the status of automatic failover for this Redis replication group.
    automaticFailover :: Core.Maybe Types.AutomaticFailoverStatus,
    -- | The name of the compute and memory capacity node type for each node in the replication group.
    cacheNodeType :: Core.Maybe Types.String,
    -- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
    --
    -- Valid values: @true@ | @false@
    clusterEnabled :: Core.Maybe Core.Bool,
    -- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
    configurationEndpoint :: Core.Maybe Types.Endpoint,
    -- | The user supplied description of the replication group.
    description :: Core.Maybe Types.String,
    -- | The name of the Global Datastore and role of this replication group in the Global Datastore.
    globalReplicationGroupInfo :: Core.Maybe Types.GlobalReplicationGroupInfo,
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The names of all the cache clusters that are part of this replication group.
    memberClusters :: Core.Maybe [Types.String],
    -- | The outpost ARNs of the replication group's member clusters.
    memberClustersOutpostArns :: Core.Maybe [Types.String],
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
    multiAZ :: Core.Maybe Types.MultiAZStatus,
    -- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
    nodeGroups :: Core.Maybe [Types.NodeGroup],
    -- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
    pendingModifiedValues :: Core.Maybe Types.ReplicationGroupPendingModifiedValues,
    -- | The identifier for the replication group.
    replicationGroupId :: Core.Maybe Types.String,
    -- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
    --
    -- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    -- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
    snapshotWindow :: Core.Maybe Types.String,
    -- | The cluster ID that is used as the daily snapshot source for the replication group.
    snapshottingClusterId :: Core.Maybe Types.String,
    -- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
    status :: Core.Maybe Types.String,
    -- | A flag that enables in-transit encryption when set to @true@ .
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    transitEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The list of user group IDs that have access to the replication group.
    userGroupIds :: Core.Maybe [Types.UserGroupId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicationGroup' value with any optional fields omitted.
mkReplicationGroup ::
  ReplicationGroup
mkReplicationGroup =
  ReplicationGroup'
    { arn = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      authTokenEnabled = Core.Nothing,
      authTokenLastModifiedDate = Core.Nothing,
      automaticFailover = Core.Nothing,
      cacheNodeType = Core.Nothing,
      clusterEnabled = Core.Nothing,
      configurationEndpoint = Core.Nothing,
      description = Core.Nothing,
      globalReplicationGroupInfo = Core.Nothing,
      kmsKeyId = Core.Nothing,
      memberClusters = Core.Nothing,
      memberClustersOutpostArns = Core.Nothing,
      multiAZ = Core.Nothing,
      nodeGroups = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      replicationGroupId = Core.Nothing,
      snapshotRetentionLimit = Core.Nothing,
      snapshotWindow = Core.Nothing,
      snapshottingClusterId = Core.Nothing,
      status = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing,
      userGroupIds = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgARN :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgARN = Lens.field @"arn"
{-# DEPRECATED rgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAtRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# DEPRECATED rgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgAuthTokenEnabled = Lens.field @"authTokenEnabled"
{-# DEPRECATED rgAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | The date the auth token was last modified
--
-- /Note:/ Consider using 'authTokenLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Core.Maybe Core.UTCTime)
rgAuthTokenLastModifiedDate = Lens.field @"authTokenLastModifiedDate"
{-# DEPRECATED rgAuthTokenLastModifiedDate "Use generic-lens or generic-optics with 'authTokenLastModifiedDate' instead." #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAutomaticFailover :: Lens.Lens' ReplicationGroup (Core.Maybe Types.AutomaticFailoverStatus)
rgAutomaticFailover = Lens.field @"automaticFailover"
{-# DEPRECATED rgAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

-- | The name of the compute and memory capacity node type for each node in the replication group.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCacheNodeType :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED rgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgClusterEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgClusterEnabled = Lens.field @"clusterEnabled"
{-# DEPRECATED rgClusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead." #-}

-- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
--
-- /Note:/ Consider using 'configurationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgConfigurationEndpoint :: Lens.Lens' ReplicationGroup (Core.Maybe Types.Endpoint)
rgConfigurationEndpoint = Lens.field @"configurationEndpoint"
{-# DEPRECATED rgConfigurationEndpoint "Use generic-lens or generic-optics with 'configurationEndpoint' instead." #-}

-- | The user supplied description of the replication group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgDescription :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgDescription = Lens.field @"description"
{-# DEPRECATED rgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- /Note:/ Consider using 'globalReplicationGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGlobalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Core.Maybe Types.GlobalReplicationGroupInfo)
rgGlobalReplicationGroupInfo = Lens.field @"globalReplicationGroupInfo"
{-# DEPRECATED rgGlobalReplicationGroupInfo "Use generic-lens or generic-optics with 'globalReplicationGroupInfo' instead." #-}

-- | The ID of the KMS key used to encrypt the disk in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgKmsKeyId :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The names of all the cache clusters that are part of this replication group.
--
-- /Note:/ Consider using 'memberClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClusters :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.String])
rgMemberClusters = Lens.field @"memberClusters"
{-# DEPRECATED rgMemberClusters "Use generic-lens or generic-optics with 'memberClusters' instead." #-}

-- | The outpost ARNs of the replication group's member clusters.
--
-- /Note:/ Consider using 'memberClustersOutpostArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.String])
rgMemberClustersOutpostArns = Lens.field @"memberClustersOutpostArns"
{-# DEPRECATED rgMemberClustersOutpostArns "Use generic-lens or generic-optics with 'memberClustersOutpostArns' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMultiAZ :: Lens.Lens' ReplicationGroup (Core.Maybe Types.MultiAZStatus)
rgMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED rgMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
--
-- /Note:/ Consider using 'nodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgNodeGroups :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.NodeGroup])
rgNodeGroups = Lens.field @"nodeGroups"
{-# DEPRECATED rgNodeGroups "Use generic-lens or generic-optics with 'nodeGroups' instead." #-}

-- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgPendingModifiedValues :: Lens.Lens' ReplicationGroup (Core.Maybe Types.ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# DEPRECATED rgPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The identifier for the replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgReplicationGroupId :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED rgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Int)
rgSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# DEPRECATED rgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotWindow :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgSnapshotWindow = Lens.field @"snapshotWindow"
{-# DEPRECATED rgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The cluster ID that is used as the daily snapshot source for the replication group.
--
-- /Note:/ Consider using 'snapshottingClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshottingClusterId :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgSnapshottingClusterId = Lens.field @"snapshottingClusterId"
{-# DEPRECATED rgSnapshottingClusterId "Use generic-lens or generic-optics with 'snapshottingClusterId' instead." #-}

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReplicationGroup (Core.Maybe Types.String)
rgStatus = Lens.field @"status"
{-# DEPRECATED rgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTransitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# DEPRECATED rgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The list of user group IDs that have access to the replication group.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgUserGroupIds :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.UserGroupId])
rgUserGroupIds = Lens.field @"userGroupIds"
{-# DEPRECATED rgUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

instance Core.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "AtRestEncryptionEnabled")
      Core.<*> (x Core..@? "AuthTokenEnabled")
      Core.<*> (x Core..@? "AuthTokenLastModifiedDate")
      Core.<*> (x Core..@? "AutomaticFailover")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "ClusterEnabled")
      Core.<*> (x Core..@? "ConfigurationEndpoint")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "GlobalReplicationGroupInfo")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> ( x Core..@? "MemberClusters"
                   Core..<@> Core.parseXMLList "ClusterId"
               )
      Core.<*> ( x Core..@? "MemberClustersOutpostArns"
                   Core..<@> Core.parseXMLList "ReplicationGroupOutpostArn"
               )
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "NodeGroups" Core..<@> Core.parseXMLList "NodeGroup")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "SnapshotRetentionLimit")
      Core.<*> (x Core..@? "SnapshotWindow")
      Core.<*> (x Core..@? "SnapshottingClusterId")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TransitEncryptionEnabled")
      Core.<*> (x Core..@? "UserGroupIds" Core..<@> Core.parseXMLList "member")
