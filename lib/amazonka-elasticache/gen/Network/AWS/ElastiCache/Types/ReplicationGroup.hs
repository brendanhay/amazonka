{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ReplicationGroup
  ( ReplicationGroup (..)
  -- * Smart constructor
  , mkReplicationGroup
  -- * Lenses
  , rgARN
  , rgAtRestEncryptionEnabled
  , rgAuthTokenEnabled
  , rgAuthTokenLastModifiedDate
  , rgAutomaticFailover
  , rgCacheNodeType
  , rgClusterEnabled
  , rgConfigurationEndpoint
  , rgDescription
  , rgGlobalReplicationGroupInfo
  , rgKmsKeyId
  , rgMemberClusters
  , rgMemberClustersOutpostArns
  , rgMultiAZ
  , rgNodeGroups
  , rgPendingModifiedValues
  , rgReplicationGroupId
  , rgSnapshotRetentionLimit
  , rgSnapshotWindow
  , rgSnapshottingClusterId
  , rgStatus
  , rgTransitEncryptionEnabled
  , rgUserGroupIds
  ) where

import qualified Network.AWS.ElastiCache.Types.AutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo as Types
import qualified Network.AWS.ElastiCache.Types.MultiAZStatus as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroup as Types
import qualified Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'mkReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN (Amazon Resource Name) of the replication group.
  , atRestEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@ 
  , authTokenEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@ 
  , authTokenLastModifiedDate :: Core.Maybe Core.UTCTime
    -- ^ The date the auth token was last modified
  , automaticFailover :: Core.Maybe Types.AutomaticFailoverStatus
    -- ^ Indicates the status of automatic failover for this Redis replication group.
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ The name of the compute and memory capacity node type for each node in the replication group.
  , clusterEnabled :: Core.Maybe Core.Bool
    -- ^ A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
--
-- Valid values: @true@ | @false@ 
  , configurationEndpoint :: Core.Maybe Types.Endpoint
    -- ^ The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
  , description :: Core.Maybe Core.Text
    -- ^ The user supplied description of the replication group.
  , globalReplicationGroupInfo :: Core.Maybe Types.GlobalReplicationGroupInfo
    -- ^ The name of the Global Datastore and role of this replication group in the Global Datastore.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The ID of the KMS key used to encrypt the disk in the cluster.
  , memberClusters :: Core.Maybe [Core.Text]
    -- ^ The names of all the cache clusters that are part of this replication group.
  , memberClustersOutpostArns :: Core.Maybe [Core.Text]
    -- ^ The outpost ARNs of the replication group's member clusters.
  , multiAZ :: Core.Maybe Types.MultiAZStatus
    -- ^ A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> 
  , nodeGroups :: Core.Maybe [Types.NodeGroup]
    -- ^ A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
  , pendingModifiedValues :: Core.Maybe Types.ReplicationGroupPendingModifiedValues
    -- ^ A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The identifier for the replication group.
  , snapshotRetentionLimit :: Core.Maybe Core.Int
    -- ^ The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
  , snapshotWindow :: Core.Maybe Core.Text
    -- ^ The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@ 
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
  , snapshottingClusterId :: Core.Maybe Core.Text
    -- ^ The cluster ID that is used as the daily snapshot source for the replication group.
  , status :: Core.Maybe Core.Text
    -- ^ The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
  , transitEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@ 
  , userGroupIds :: Core.Maybe [Types.UserGroupId]
    -- ^ The list of user group IDs that have access to the replication group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicationGroup' value with any optional fields omitted.
mkReplicationGroup
    :: ReplicationGroup
mkReplicationGroup
  = ReplicationGroup'{arn = Core.Nothing,
                      atRestEncryptionEnabled = Core.Nothing,
                      authTokenEnabled = Core.Nothing,
                      authTokenLastModifiedDate = Core.Nothing,
                      automaticFailover = Core.Nothing, cacheNodeType = Core.Nothing,
                      clusterEnabled = Core.Nothing,
                      configurationEndpoint = Core.Nothing, description = Core.Nothing,
                      globalReplicationGroupInfo = Core.Nothing, kmsKeyId = Core.Nothing,
                      memberClusters = Core.Nothing,
                      memberClustersOutpostArns = Core.Nothing, multiAZ = Core.Nothing,
                      nodeGroups = Core.Nothing, pendingModifiedValues = Core.Nothing,
                      replicationGroupId = Core.Nothing,
                      snapshotRetentionLimit = Core.Nothing,
                      snapshotWindow = Core.Nothing,
                      snapshottingClusterId = Core.Nothing, status = Core.Nothing,
                      transitEncryptionEnabled = Core.Nothing,
                      userGroupIds = Core.Nothing}

-- | The ARN (Amazon Resource Name) of the replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgARN :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgARN = Lens.field @"arn"
{-# INLINEABLE rgARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@ 
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAtRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# INLINEABLE rgAtRestEncryptionEnabled #-}
{-# DEPRECATED atRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead"  #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgAuthTokenEnabled = Lens.field @"authTokenEnabled"
{-# INLINEABLE rgAuthTokenEnabled #-}
{-# DEPRECATED authTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead"  #-}

-- | The date the auth token was last modified
--
-- /Note:/ Consider using 'authTokenLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAuthTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Core.Maybe Core.UTCTime)
rgAuthTokenLastModifiedDate = Lens.field @"authTokenLastModifiedDate"
{-# INLINEABLE rgAuthTokenLastModifiedDate #-}
{-# DEPRECATED authTokenLastModifiedDate "Use generic-lens or generic-optics with 'authTokenLastModifiedDate' instead"  #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAutomaticFailover :: Lens.Lens' ReplicationGroup (Core.Maybe Types.AutomaticFailoverStatus)
rgAutomaticFailover = Lens.field @"automaticFailover"
{-# INLINEABLE rgAutomaticFailover #-}
{-# DEPRECATED automaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead"  #-}

-- | The name of the compute and memory capacity node type for each node in the replication group.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCacheNodeType :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE rgCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups).
--
-- Valid values: @true@ | @false@ 
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgClusterEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgClusterEnabled = Lens.field @"clusterEnabled"
{-# INLINEABLE rgClusterEnabled #-}
{-# DEPRECATED clusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead"  #-}

-- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
--
-- /Note:/ Consider using 'configurationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgConfigurationEndpoint :: Lens.Lens' ReplicationGroup (Core.Maybe Types.Endpoint)
rgConfigurationEndpoint = Lens.field @"configurationEndpoint"
{-# INLINEABLE rgConfigurationEndpoint #-}
{-# DEPRECATED configurationEndpoint "Use generic-lens or generic-optics with 'configurationEndpoint' instead"  #-}

-- | The user supplied description of the replication group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgDescription :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgDescription = Lens.field @"description"
{-# INLINEABLE rgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- /Note:/ Consider using 'globalReplicationGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGlobalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Core.Maybe Types.GlobalReplicationGroupInfo)
rgGlobalReplicationGroupInfo = Lens.field @"globalReplicationGroupInfo"
{-# INLINEABLE rgGlobalReplicationGroupInfo #-}
{-# DEPRECATED globalReplicationGroupInfo "Use generic-lens or generic-optics with 'globalReplicationGroupInfo' instead"  #-}

-- | The ID of the KMS key used to encrypt the disk in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgKmsKeyId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE rgKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The names of all the cache clusters that are part of this replication group.
--
-- /Note:/ Consider using 'memberClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClusters :: Lens.Lens' ReplicationGroup (Core.Maybe [Core.Text])
rgMemberClusters = Lens.field @"memberClusters"
{-# INLINEABLE rgMemberClusters #-}
{-# DEPRECATED memberClusters "Use generic-lens or generic-optics with 'memberClusters' instead"  #-}

-- | The outpost ARNs of the replication group's member clusters.
--
-- /Note:/ Consider using 'memberClustersOutpostArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMemberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Core.Maybe [Core.Text])
rgMemberClustersOutpostArns = Lens.field @"memberClustersOutpostArns"
{-# INLINEABLE rgMemberClustersOutpostArns #-}
{-# DEPRECATED memberClustersOutpostArns "Use generic-lens or generic-optics with 'memberClustersOutpostArns' instead"  #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> 
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgMultiAZ :: Lens.Lens' ReplicationGroup (Core.Maybe Types.MultiAZStatus)
rgMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rgMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
--
-- /Note:/ Consider using 'nodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgNodeGroups :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.NodeGroup])
rgNodeGroups = Lens.field @"nodeGroups"
{-# INLINEABLE rgNodeGroups #-}
{-# DEPRECATED nodeGroups "Use generic-lens or generic-optics with 'nodeGroups' instead"  #-}

-- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgPendingModifiedValues :: Lens.Lens' ReplicationGroup (Core.Maybe Types.ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# INLINEABLE rgPendingModifiedValues #-}
{-# DEPRECATED pendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead"  #-}

-- | The identifier for the replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgReplicationGroupId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE rgReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Int)
rgSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# INLINEABLE rgSnapshotRetentionLimit #-}
{-# DEPRECATED snapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead"  #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@ 
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshotWindow :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgSnapshotWindow = Lens.field @"snapshotWindow"
{-# INLINEABLE rgSnapshotWindow #-}
{-# DEPRECATED snapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead"  #-}

-- | The cluster ID that is used as the daily snapshot source for the replication group.
--
-- /Note:/ Consider using 'snapshottingClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgSnapshottingClusterId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgSnapshottingClusterId = Lens.field @"snapshottingClusterId"
{-# INLINEABLE rgSnapshottingClusterId #-}
{-# DEPRECATED snapshottingClusterId "Use generic-lens or generic-optics with 'snapshottingClusterId' instead"  #-}

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
rgStatus = Lens.field @"status"
{-# INLINEABLE rgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@ 
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTransitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
rgTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# INLINEABLE rgTransitEncryptionEnabled #-}
{-# DEPRECATED transitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead"  #-}

-- | The list of user group IDs that have access to the replication group.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgUserGroupIds :: Lens.Lens' ReplicationGroup (Core.Maybe [Types.UserGroupId])
rgUserGroupIds = Lens.field @"userGroupIds"
{-# INLINEABLE rgUserGroupIds #-}
{-# DEPRECATED userGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead"  #-}

instance Core.FromXML ReplicationGroup where
        parseXML x
          = ReplicationGroup' Core.<$>
              (x Core..@? "ARN") Core.<*> x Core..@? "AtRestEncryptionEnabled"
                Core.<*> x Core..@? "AuthTokenEnabled"
                Core.<*> x Core..@? "AuthTokenLastModifiedDate"
                Core.<*> x Core..@? "AutomaticFailover"
                Core.<*> x Core..@? "CacheNodeType"
                Core.<*> x Core..@? "ClusterEnabled"
                Core.<*> x Core..@? "ConfigurationEndpoint"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "GlobalReplicationGroupInfo"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*>
                x Core..@? "MemberClusters" Core..<@> Core.parseXMLList "ClusterId"
                Core.<*>
                x Core..@? "MemberClustersOutpostArns" Core..<@>
                  Core.parseXMLList "ReplicationGroupOutpostArn"
                Core.<*> x Core..@? "MultiAZ"
                Core.<*>
                x Core..@? "NodeGroups" Core..<@> Core.parseXMLList "NodeGroup"
                Core.<*> x Core..@? "PendingModifiedValues"
                Core.<*> x Core..@? "ReplicationGroupId"
                Core.<*> x Core..@? "SnapshotRetentionLimit"
                Core.<*> x Core..@? "SnapshotWindow"
                Core.<*> x Core..@? "SnapshottingClusterId"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "TransitEncryptionEnabled"
                Core.<*>
                x Core..@? "UserGroupIds" Core..<@> Core.parseXMLList "member"
