{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Snapshot
  ( Snapshot (..),

    -- * Smart constructor
    mkSnapshot,

    -- * Lenses
    sARN,
    sAutoMinorVersionUpgrade,
    sAutomaticFailover,
    sCacheClusterCreateTime,
    sCacheClusterId,
    sCacheNodeType,
    sCacheParameterGroupName,
    sCacheSubnetGroupName,
    sEngine,
    sEngineVersion,
    sKmsKeyId,
    sNodeSnapshots,
    sNumCacheNodes,
    sNumNodeGroups,
    sPort,
    sPreferredAvailabilityZone,
    sPreferredMaintenanceWindow,
    sPreferredOutpostArn,
    sReplicationGroupDescription,
    sReplicationGroupId,
    sSnapshotName,
    sSnapshotRetentionLimit,
    sSnapshotSource,
    sSnapshotStatus,
    sSnapshotWindow,
    sTopicArn,
    sVpcId,
  )
where

import qualified Network.AWS.ElastiCache.Types.ARN as Types
import qualified Network.AWS.ElastiCache.Types.AutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.CacheClusterId as Types
import qualified Network.AWS.ElastiCache.Types.CacheNodeType as Types
import qualified Network.AWS.ElastiCache.Types.CacheParameterGroupName as Types
import qualified Network.AWS.ElastiCache.Types.CacheSubnetGroupName as Types
import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.EngineVersion as Types
import qualified Network.AWS.ElastiCache.Types.KmsKeyId as Types
import qualified Network.AWS.ElastiCache.Types.NodeSnapshot as Types
import qualified Network.AWS.ElastiCache.Types.PreferredAvailabilityZone as Types
import qualified Network.AWS.ElastiCache.Types.PreferredMaintenanceWindow as Types
import qualified Network.AWS.ElastiCache.Types.PreferredOutpostArn as Types
import qualified Network.AWS.ElastiCache.Types.ReplicationGroupDescription as Types
import qualified Network.AWS.ElastiCache.Types.ReplicationGroupId as Types
import qualified Network.AWS.ElastiCache.Types.SnapshotName as Types
import qualified Network.AWS.ElastiCache.Types.SnapshotSource as Types
import qualified Network.AWS.ElastiCache.Types.SnapshotStatus as Types
import qualified Network.AWS.ElastiCache.Types.SnapshotWindow as Types
import qualified Network.AWS.ElastiCache.Types.TopicArn as Types
import qualified Network.AWS.ElastiCache.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The ARN (Amazon Resource Name) of the snapshot.
    arn :: Core.Maybe Types.ARN,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Indicates the status of automatic failover for the source Redis replication group.
    automaticFailover :: Core.Maybe Types.AutomaticFailoverStatus,
    -- | The date and time when the source cluster was created.
    cacheClusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | The user-supplied identifier of the source cluster.
    cacheClusterId :: Core.Maybe Types.CacheClusterId,
    -- | The name of the compute and memory capacity node type for the source cluster.
    --
    -- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
    --
    --     * General purpose:
    --
    --     * Current generation:
    -- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
    -- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
    -- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
    -- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
    -- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
    -- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
    --
    --
    --     * Previous generation: (not recommended)
    -- __T1 node types:__ @cache.t1.micro@
    -- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
    -- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
    --
    --
    --
    --
    --     * Compute optimized:
    --
    --     * Previous generation: (not recommended)
    -- __C1 node types:__ @cache.c1.xlarge@
    --
    --
    --
    --
    --     * Memory optimized:
    --
    --     * Current generation:
    -- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
    -- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
    -- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
    -- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
    --
    --
    --     * Previous generation: (not recommended)
    -- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
    -- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
    --
    --
    --
    --
    -- __Additional node type info__
    --
    --     * All current generation instance types are created in Amazon VPC by default.
    --
    --
    --     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
    --
    --
    --     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
    --
    --
    --     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
    cacheNodeType :: Core.Maybe Types.CacheNodeType,
    -- | The cache parameter group that is associated with the source cluster.
    cacheParameterGroupName :: Core.Maybe Types.CacheParameterGroupName,
    -- | The name of the cache subnet group associated with the source cluster.
    cacheSubnetGroupName :: Core.Maybe Types.CacheSubnetGroupName,
    -- | The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
    engine :: Core.Maybe Types.Engine,
    -- | The version of the cache engine version that is used by the source cluster.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | A list of the cache nodes in the source cluster.
    nodeSnapshots :: Core.Maybe [Types.NodeSnapshot],
    -- | The number of cache nodes in the source cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
    numCacheNodes :: Core.Maybe Core.Int,
    -- | The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
    numNodeGroups :: Core.Maybe Core.Int,
    -- | The port number used by each cache nodes in the source cluster.
    port :: Core.Maybe Core.Int,
    -- | The name of the Availability Zone in which the source cluster is located.
    preferredAvailabilityZone :: Core.Maybe Types.PreferredAvailabilityZone,
    -- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
    --
    -- Valid values for @ddd@ are:
    --
    --     * @sun@
    --
    --
    --     * @mon@
    --
    --
    --     * @tue@
    --
    --
    --     * @wed@
    --
    --
    --     * @thu@
    --
    --
    --     * @fri@
    --
    --
    --     * @sat@
    --
    --
    -- Example: @sun:23:00-mon:01:30@
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | The ARN (Amazon Resource Name) of the preferred outpost.
    preferredOutpostArn :: Core.Maybe Types.PreferredOutpostArn,
    -- | A description of the source replication group.
    replicationGroupDescription :: Core.Maybe Types.ReplicationGroupDescription,
    -- | The unique identifier of the source replication group.
    replicationGroupId :: Core.Maybe Types.ReplicationGroupId,
    -- | The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
    snapshotName :: Core.Maybe Types.SnapshotName,
    -- | For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it.
    --
    -- For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.
    -- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
    snapshotSource :: Core.Maybe Types.SnapshotSource,
    -- | The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
    snapshotStatus :: Core.Maybe Types.SnapshotStatus,
    -- | The daily time range during which ElastiCache takes daily snapshots of the source cluster.
    snapshotWindow :: Core.Maybe Types.SnapshotWindow,
    -- | The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
    topicArn :: Core.Maybe Types.TopicArn,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { arn = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      automaticFailover = Core.Nothing,
      cacheClusterCreateTime = Core.Nothing,
      cacheClusterId = Core.Nothing,
      cacheNodeType = Core.Nothing,
      cacheParameterGroupName = Core.Nothing,
      cacheSubnetGroupName = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      kmsKeyId = Core.Nothing,
      nodeSnapshots = Core.Nothing,
      numCacheNodes = Core.Nothing,
      numNodeGroups = Core.Nothing,
      port = Core.Nothing,
      preferredAvailabilityZone = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      preferredOutpostArn = Core.Nothing,
      replicationGroupDescription = Core.Nothing,
      replicationGroupId = Core.Nothing,
      snapshotName = Core.Nothing,
      snapshotRetentionLimit = Core.Nothing,
      snapshotSource = Core.Nothing,
      snapshotStatus = Core.Nothing,
      snapshotWindow = Core.Nothing,
      topicArn = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' Snapshot (Core.Maybe Types.ARN)
sARN = Lens.field @"arn"
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoMinorVersionUpgrade :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED sAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Indicates the status of automatic failover for the source Redis replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutomaticFailover :: Lens.Lens' Snapshot (Core.Maybe Types.AutomaticFailoverStatus)
sAutomaticFailover = Lens.field @"automaticFailover"
{-# DEPRECATED sAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

-- | The date and time when the source cluster was created.
--
-- /Note:/ Consider using 'cacheClusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sCacheClusterCreateTime = Lens.field @"cacheClusterCreateTime"
{-# DEPRECATED sCacheClusterCreateTime "Use generic-lens or generic-optics with 'cacheClusterCreateTime' instead." #-}

-- | The user-supplied identifier of the source cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterId :: Lens.Lens' Snapshot (Core.Maybe Types.CacheClusterId)
sCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED sCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The name of the compute and memory capacity node type for the source cluster.
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
-- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
-- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
-- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
--
--     * Previous generation: (not recommended)
-- __T1 node types:__ @cache.t1.micro@
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--
--     * Memory optimized:
--
--     * Current generation:
-- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
-- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
-- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
--
--
--     * Previous generation: (not recommended)
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--
--
--
-- __Additional node type info__
--
--     * All current generation instance types are created in Amazon VPC by default.
--
--
--     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
--
--
--     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
--
--
--     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
--
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheNodeType :: Lens.Lens' Snapshot (Core.Maybe Types.CacheNodeType)
sCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED sCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The cache parameter group that is associated with the source cluster.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheParameterGroupName :: Lens.Lens' Snapshot (Core.Maybe Types.CacheParameterGroupName)
sCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED sCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | The name of the cache subnet group associated with the source cluster.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheSubnetGroupName :: Lens.Lens' Snapshot (Core.Maybe Types.CacheSubnetGroupName)
sCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# DEPRECATED sCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEngine :: Lens.Lens' Snapshot (Core.Maybe Types.Engine)
sEngine = Lens.field @"engine"
{-# DEPRECATED sEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version of the cache engine version that is used by the source cluster.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEngineVersion :: Lens.Lens' Snapshot (Core.Maybe Types.EngineVersion)
sEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED sEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The ID of the KMS key used to encrypt the snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Types.KmsKeyId)
sKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED sKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of the cache nodes in the source cluster.
--
-- /Note:/ Consider using 'nodeSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNodeSnapshots :: Lens.Lens' Snapshot (Core.Maybe [Types.NodeSnapshot])
sNodeSnapshots = Lens.field @"nodeSnapshots"
{-# DEPRECATED sNodeSnapshots "Use generic-lens or generic-optics with 'nodeSnapshots' instead." #-}

-- | The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumCacheNodes :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sNumCacheNodes = Lens.field @"numCacheNodes"
{-# DEPRECATED sNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

-- | The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
--
-- /Note:/ Consider using 'numNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumNodeGroups :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sNumNodeGroups = Lens.field @"numNodeGroups"
{-# DEPRECATED sNumNodeGroups "Use generic-lens or generic-optics with 'numNodeGroups' instead." #-}

-- | The port number used by each cache nodes in the source cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPort :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sPort = Lens.field @"port"
{-# DEPRECATED sPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The name of the Availability Zone in which the source cluster is located.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPreferredAvailabilityZone :: Lens.Lens' Snapshot (Core.Maybe Types.PreferredAvailabilityZone)
sPreferredAvailabilityZone = Lens.field @"preferredAvailabilityZone"
{-# DEPRECATED sPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
--     * @sun@
--
--
--     * @mon@
--
--
--     * @tue@
--
--
--     * @wed@
--
--
--     * @thu@
--
--
--     * @fri@
--
--
--     * @sat@
--
--
-- Example: @sun:23:00-mon:01:30@
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPreferredMaintenanceWindow :: Lens.Lens' Snapshot (Core.Maybe Types.PreferredMaintenanceWindow)
sPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED sPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The ARN (Amazon Resource Name) of the preferred outpost.
--
-- /Note:/ Consider using 'preferredOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPreferredOutpostArn :: Lens.Lens' Snapshot (Core.Maybe Types.PreferredOutpostArn)
sPreferredOutpostArn = Lens.field @"preferredOutpostArn"
{-# DEPRECATED sPreferredOutpostArn "Use generic-lens or generic-optics with 'preferredOutpostArn' instead." #-}

-- | A description of the source replication group.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationGroupDescription :: Lens.Lens' Snapshot (Core.Maybe Types.ReplicationGroupDescription)
sReplicationGroupDescription = Lens.field @"replicationGroupDescription"
{-# DEPRECATED sReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

-- | The unique identifier of the source replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationGroupId :: Lens.Lens' Snapshot (Core.Maybe Types.ReplicationGroupId)
sReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED sReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotName :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotName)
sSnapshotName = Lens.field @"snapshotName"
{-# DEPRECATED sSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotRetentionLimit :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# DEPRECATED sSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
--
-- /Note:/ Consider using 'snapshotSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotSource :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotSource)
sSnapshotSource = Lens.field @"snapshotSource"
{-# DEPRECATED sSnapshotSource "Use generic-lens or generic-optics with 'snapshotSource' instead." #-}

-- | The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
--
-- /Note:/ Consider using 'snapshotStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotStatus :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotStatus)
sSnapshotStatus = Lens.field @"snapshotStatus"
{-# DEPRECATED sSnapshotStatus "Use generic-lens or generic-optics with 'snapshotStatus' instead." #-}

-- | The daily time range during which ElastiCache takes daily snapshots of the source cluster.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotWindow :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotWindow)
sSnapshotWindow = Lens.field @"snapshotWindow"
{-# DEPRECATED sSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicArn :: Lens.Lens' Snapshot (Core.Maybe Types.TopicArn)
sTopicArn = Lens.field @"topicArn"
{-# DEPRECATED sTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVpcId :: Lens.Lens' Snapshot (Core.Maybe Types.VpcId)
sVpcId = Lens.field @"vpcId"
{-# DEPRECATED sVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Core.<*> (x Core..@? "AutomaticFailover")
      Core.<*> (x Core..@? "CacheClusterCreateTime")
      Core.<*> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "CacheParameterGroupName")
      Core.<*> (x Core..@? "CacheSubnetGroupName")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> ( x Core..@? "NodeSnapshots"
                   Core..<@> Core.parseXMLList "NodeSnapshot"
               )
      Core.<*> (x Core..@? "NumCacheNodes")
      Core.<*> (x Core..@? "NumNodeGroups")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "PreferredAvailabilityZone")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "PreferredOutpostArn")
      Core.<*> (x Core..@? "ReplicationGroupDescription")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "SnapshotName")
      Core.<*> (x Core..@? "SnapshotRetentionLimit")
      Core.<*> (x Core..@? "SnapshotSource")
      Core.<*> (x Core..@? "SnapshotStatus")
      Core.<*> (x Core..@? "SnapshotWindow")
      Core.<*> (x Core..@? "TopicArn")
      Core.<*> (x Core..@? "VpcId")
