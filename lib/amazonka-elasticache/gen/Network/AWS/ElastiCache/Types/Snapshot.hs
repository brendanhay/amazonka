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
    sEngineVersion,
    sCacheNodeType,
    sCacheClusterCreateTime,
    sAutoMinorVersionUpgrade,
    sARN,
    sCacheParameterGroupName,
    sReplicationGroupDescription,
    sVPCId,
    sSnapshotStatus,
    sSnapshotWindow,
    sCacheClusterId,
    sEngine,
    sPreferredMaintenanceWindow,
    sTopicARN,
    sKMSKeyId,
    sNodeSnapshots,
    sCacheSubnetGroupName,
    sPreferredAvailabilityZone,
    sNumNodeGroups,
    sSnapshotRetentionLimit,
    sSnapshotName,
    sPreferredOutpostARN,
    sReplicationGroupId,
    sNumCacheNodes,
    sPort,
    sAutomaticFailover,
    sSnapshotSource,
  )
where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.NodeSnapshot
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { engineVersion :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    cacheClusterCreateTime :: Lude.Maybe Lude.ISO8601,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    arn :: Lude.Maybe Lude.Text,
    cacheParameterGroupName :: Lude.Maybe Lude.Text,
    replicationGroupDescription :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    snapshotStatus :: Lude.Maybe Lude.Text,
    snapshotWindow :: Lude.Maybe Lude.Text,
    cacheClusterId :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    topicARN :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    nodeSnapshots :: Lude.Maybe [NodeSnapshot],
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
    preferredAvailabilityZone :: Lude.Maybe Lude.Text,
    numNodeGroups :: Lude.Maybe Lude.Int,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    snapshotName :: Lude.Maybe Lude.Text,
    preferredOutpostARN :: Lude.Maybe Lude.Text,
    replicationGroupId :: Lude.Maybe Lude.Text,
    numCacheNodes :: Lude.Maybe Lude.Int,
    port :: Lude.Maybe Lude.Int,
    automaticFailover :: Lude.Maybe AutomaticFailoverStatus,
    snapshotSource :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon Resource Name) of the snapshot.
-- * 'autoMinorVersionUpgrade' - This parameter is currently disabled.
-- * 'automaticFailover' - Indicates the status of automatic failover for the source Redis replication group.
-- * 'cacheClusterCreateTime' - The date and time when the source cluster was created.
-- * 'cacheClusterId' - The user-supplied identifier of the source cluster.
-- * 'cacheNodeType' - The name of the compute and memory capacity node type for the source cluster.
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
-- * 'cacheParameterGroupName' - The cache parameter group that is associated with the source cluster.
-- * 'cacheSubnetGroupName' - The name of the cache subnet group associated with the source cluster.
-- * 'engine' - The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
-- * 'engineVersion' - The version of the cache engine version that is used by the source cluster.
-- * 'kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
-- * 'nodeSnapshots' - A list of the cache nodes in the source cluster.
-- * 'numCacheNodes' - The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- * 'numNodeGroups' - The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
-- * 'port' - The port number used by each cache nodes in the source cluster.
-- * 'preferredAvailabilityZone' - The name of the Availability Zone in which the source cluster is located.
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
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
-- * 'preferredOutpostARN' - The ARN (Amazon Resource Name) of the preferred outpost.
-- * 'replicationGroupDescription' - A description of the source replication group.
-- * 'replicationGroupId' - The unique identifier of the source replication group.
-- * 'snapshotName' - The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
-- * 'snapshotRetentionLimit' - For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
-- * 'snapshotSource' - Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
-- * 'snapshotStatus' - The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
-- * 'snapshotWindow' - The daily time range during which ElastiCache takes daily snapshots of the source cluster.
-- * 'topicARN' - The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
-- * 'vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      cacheClusterCreateTime = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      arn = Lude.Nothing,
      cacheParameterGroupName = Lude.Nothing,
      replicationGroupDescription = Lude.Nothing,
      vpcId = Lude.Nothing,
      snapshotStatus = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      cacheClusterId = Lude.Nothing,
      engine = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      topicARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      nodeSnapshots = Lude.Nothing,
      cacheSubnetGroupName = Lude.Nothing,
      preferredAvailabilityZone = Lude.Nothing,
      numNodeGroups = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      snapshotName = Lude.Nothing,
      preferredOutpostARN = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      numCacheNodes = Lude.Nothing,
      port = Lude.Nothing,
      automaticFailover = Lude.Nothing,
      snapshotSource = Lude.Nothing
    }

-- | The version of the cache engine version that is used by the source cluster.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEngineVersion :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sEngineVersion = Lens.lens (engineVersion :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: Snapshot)
{-# DEPRECATED sEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
sCacheNodeType :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sCacheNodeType = Lens.lens (cacheNodeType :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: Snapshot)
{-# DEPRECATED sCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The date and time when the source cluster was created.
--
-- /Note:/ Consider using 'cacheClusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterCreateTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.ISO8601)
sCacheClusterCreateTime = Lens.lens (cacheClusterCreateTime :: Snapshot -> Lude.Maybe Lude.ISO8601) (\s a -> s {cacheClusterCreateTime = a} :: Snapshot)
{-# DEPRECATED sCacheClusterCreateTime "Use generic-lens or generic-optics with 'cacheClusterCreateTime' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutoMinorVersionUpgrade :: Lens.Lens' Snapshot (Lude.Maybe Lude.Bool)
sAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: Snapshot -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: Snapshot)
{-# DEPRECATED sAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The ARN (Amazon Resource Name) of the snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sARN = Lens.lens (arn :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Snapshot)
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The cache parameter group that is associated with the source cluster.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheParameterGroupName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: Snapshot)
{-# DEPRECATED sCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A description of the source replication group.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationGroupDescription :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sReplicationGroupDescription = Lens.lens (replicationGroupDescription :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupDescription = a} :: Snapshot)
{-# DEPRECATED sReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVPCId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sVPCId = Lens.lens (vpcId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Snapshot)
{-# DEPRECATED sVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
--
-- /Note:/ Consider using 'snapshotStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotStatus :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotStatus = Lens.lens (snapshotStatus :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotStatus = a} :: Snapshot)
{-# DEPRECATED sSnapshotStatus "Use generic-lens or generic-optics with 'snapshotStatus' instead." #-}

-- | The daily time range during which ElastiCache takes daily snapshots of the source cluster.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotWindow :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotWindow = Lens.lens (snapshotWindow :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: Snapshot)
{-# DEPRECATED sSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The user-supplied identifier of the source cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sCacheClusterId = Lens.lens (cacheClusterId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: Snapshot)
{-# DEPRECATED sCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEngine :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sEngine = Lens.lens (engine :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: Snapshot)
{-# DEPRECATED sEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

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
sPreferredMaintenanceWindow :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: Snapshot)
{-# DEPRECATED sPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicARN :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sTopicARN = Lens.lens (topicARN :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: Snapshot)
{-# DEPRECATED sTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The ID of the KMS key used to encrypt the snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKMSKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sKMSKeyId = Lens.lens (kmsKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Snapshot)
{-# DEPRECATED sKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of the cache nodes in the source cluster.
--
-- /Note:/ Consider using 'nodeSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNodeSnapshots :: Lens.Lens' Snapshot (Lude.Maybe [NodeSnapshot])
sNodeSnapshots = Lens.lens (nodeSnapshots :: Snapshot -> Lude.Maybe [NodeSnapshot]) (\s a -> s {nodeSnapshots = a} :: Snapshot)
{-# DEPRECATED sNodeSnapshots "Use generic-lens or generic-optics with 'nodeSnapshots' instead." #-}

-- | The name of the cache subnet group associated with the source cluster.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheSubnetGroupName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: Snapshot)
{-# DEPRECATED sCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The name of the Availability Zone in which the source cluster is located.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPreferredAvailabilityZone :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sPreferredAvailabilityZone = Lens.lens (preferredAvailabilityZone :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {preferredAvailabilityZone = a} :: Snapshot)
{-# DEPRECATED sPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
--
-- /Note:/ Consider using 'numNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumNodeGroups :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sNumNodeGroups = Lens.lens (numNodeGroups :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {numNodeGroups = a} :: Snapshot)
{-# DEPRECATED sNumNodeGroups "Use generic-lens or generic-optics with 'numNodeGroups' instead." #-}

-- | For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotRetentionLimit :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: Snapshot)
{-# DEPRECATED sSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotName = Lens.lens (snapshotName :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotName = a} :: Snapshot)
{-# DEPRECATED sSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The ARN (Amazon Resource Name) of the preferred outpost.
--
-- /Note:/ Consider using 'preferredOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPreferredOutpostARN :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sPreferredOutpostARN = Lens.lens (preferredOutpostARN :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {preferredOutpostARN = a} :: Snapshot)
{-# DEPRECATED sPreferredOutpostARN "Use generic-lens or generic-optics with 'preferredOutpostARN' instead." #-}

-- | The unique identifier of the source replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationGroupId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sReplicationGroupId = Lens.lens (replicationGroupId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: Snapshot)
{-# DEPRECATED sReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumCacheNodes :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sNumCacheNodes = Lens.lens (numCacheNodes :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {numCacheNodes = a} :: Snapshot)
{-# DEPRECATED sNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

-- | The port number used by each cache nodes in the source cluster.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPort :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sPort = Lens.lens (port :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Snapshot)
{-# DEPRECATED sPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Indicates the status of automatic failover for the source Redis replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAutomaticFailover :: Lens.Lens' Snapshot (Lude.Maybe AutomaticFailoverStatus)
sAutomaticFailover = Lens.lens (automaticFailover :: Snapshot -> Lude.Maybe AutomaticFailoverStatus) (\s a -> s {automaticFailover = a} :: Snapshot)
{-# DEPRECATED sAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

-- | Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
--
-- /Note:/ Consider using 'snapshotSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotSource :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotSource = Lens.lens (snapshotSource :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotSource = a} :: Snapshot)
{-# DEPRECATED sSnapshotSource "Use generic-lens or generic-optics with 'snapshotSource' instead." #-}

instance Lude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "CacheNodeType")
      Lude.<*> (x Lude..@? "CacheClusterCreateTime")
      Lude.<*> (x Lude..@? "AutoMinorVersionUpgrade")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "CacheParameterGroupName")
      Lude.<*> (x Lude..@? "ReplicationGroupDescription")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "SnapshotStatus")
      Lude.<*> (x Lude..@? "SnapshotWindow")
      Lude.<*> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "PreferredMaintenanceWindow")
      Lude.<*> (x Lude..@? "TopicArn")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> ( x Lude..@? "NodeSnapshots" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeSnapshot")
               )
      Lude.<*> (x Lude..@? "CacheSubnetGroupName")
      Lude.<*> (x Lude..@? "PreferredAvailabilityZone")
      Lude.<*> (x Lude..@? "NumNodeGroups")
      Lude.<*> (x Lude..@? "SnapshotRetentionLimit")
      Lude.<*> (x Lude..@? "SnapshotName")
      Lude.<*> (x Lude..@? "PreferredOutpostArn")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
      Lude.<*> (x Lude..@? "NumCacheNodes")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@? "AutomaticFailover")
      Lude.<*> (x Lude..@? "SnapshotSource")
