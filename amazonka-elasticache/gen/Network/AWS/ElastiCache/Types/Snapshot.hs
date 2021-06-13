{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Snapshot where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.NodeSnapshot
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a copy of an entire Redis cluster as of the time when the
-- snapshot was taken.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The date and time when the source cluster was created.
    cacheClusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | A list of the cache nodes in the source cluster.
    nodeSnapshots :: Prelude.Maybe [NodeSnapshot],
    -- | Indicates the status of automatic failover for the source Redis
    -- replication group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus,
    -- | The number of cache nodes in the source cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 20.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the source replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied identifier of the source cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which ElastiCache takes daily snapshots of
    -- the source cluster.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The status of the snapshot. Valid values: @creating@ | @available@ |
    -- @restoring@ | @copying@ | @deleting@.
    snapshotStatus :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the source replication group.
    replicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The cache parameter group that is associated with the source cluster.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | For an automatic snapshot, the number of days for which ElastiCache
    -- retains the snapshot before deleting it.
    --
    -- For manual snapshots, this field reflects the @SnapshotRetentionLimit@
    -- for the source cluster when the snapshot was created. This field is
    -- otherwise ignored: Manual snapshots do not expire, and can only be
    -- deleted using the @DeleteSnapshot@ operation.
    --
    -- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
    -- backups are turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The number of node groups (shards) in this snapshot. When restoring from
    -- a snapshot, the number of node groups (shards) in the snapshot and in
    -- the restored replication group must be the same.
    numNodeGroups :: Prelude.Maybe Prelude.Int,
    -- | The name of the Availability Zone in which the source cluster is
    -- located.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache subnet group associated with the source cluster.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The version of the cache engine version that is used by the source
    -- cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    --
    -- Valid values for @ddd@ are:
    --
    -- -   @sun@
    --
    -- -   @mon@
    --
    -- -   @tue@
    --
    -- -   @wed@
    --
    -- -   @thu@
    --
    -- -   @fri@
    --
    -- -   @sat@
    --
    -- Example: @sun:23:00-mon:01:30@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for the source
    -- cluster.
    --
    -- The following node types are supported by ElastiCache. Generally
    -- speaking, the current generation types provide more memory and
    -- computational power at lower cost when compared to their equivalent
    -- previous generation counterparts.
    --
    -- -   General purpose:
    --
    --     -   Current generation:
    --
    --         __M6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
    --         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
    --         @cache.m6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
    --         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
    --         @cache.m5.24xlarge@
    --
    --         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
    --         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __T1 node types:__ @cache.t1.micro@
    --
    --         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
    --         @cache.m1.large@, @cache.m1.xlarge@
    --
    --         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
    --         @cache.m3.xlarge@, @cache.m3.2xlarge@
    --
    -- -   Compute optimized:
    --
    --     -   Previous generation: (not recommended)
    --
    --         __C1 node types:__ @cache.c1.xlarge@
    --
    -- -   Memory optimized:
    --
    --     -   Current generation:
    --
    --         __R6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
    --         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
    --         @cache.r6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
    --         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
    --         @cache.r5.24xlarge@
    --
    --         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
    --         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
    --         @cache.r4.16xlarge@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
    --         @cache.m2.4xlarge@
    --
    --         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
    --         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
    --
    -- __Additional node type info__
    --
    -- -   All current generation instance types are created in Amazon VPC by
    --     default.
    --
    -- -   Redis append-only files (AOF) are not supported for T1 or T2
    --     instances.
    --
    -- -   Redis Multi-AZ with automatic failover is not supported on T1
    --     instances.
    --
    -- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
    --     supported on Redis version 2.8.22 and later.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the topic used by the source cluster
    -- for publishing notifications.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The port number used by each cache nodes in the source cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the snapshot is from an automatic backup (@automated@)
    -- or was created manually (@manual@).
    snapshotSource :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache engine (@memcached@ or @redis@) used by the source
    -- cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the preferred outpost.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a snapshot. For an automatic snapshot, the name is
    -- system-generated. For a manual snapshot, this is the user-provided name.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
    -- group for the source cluster.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterCreateTime', 'snapshot_cacheClusterCreateTime' - The date and time when the source cluster was created.
--
-- 'nodeSnapshots', 'snapshot_nodeSnapshots' - A list of the cache nodes in the source cluster.
--
-- 'automaticFailover', 'snapshot_automaticFailover' - Indicates the status of automatic failover for the source Redis
-- replication group.
--
-- 'numCacheNodes', 'snapshot_numCacheNodes' - The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- 'replicationGroupId', 'snapshot_replicationGroupId' - The unique identifier of the source replication group.
--
-- 'cacheClusterId', 'snapshot_cacheClusterId' - The user-supplied identifier of the source cluster.
--
-- 'snapshotWindow', 'snapshot_snapshotWindow' - The daily time range during which ElastiCache takes daily snapshots of
-- the source cluster.
--
-- 'snapshotStatus', 'snapshot_snapshotStatus' - The status of the snapshot. Valid values: @creating@ | @available@ |
-- @restoring@ | @copying@ | @deleting@.
--
-- 'arn', 'snapshot_arn' - The ARN (Amazon Resource Name) of the snapshot.
--
-- 'replicationGroupDescription', 'snapshot_replicationGroupDescription' - A description of the source replication group.
--
-- 'cacheParameterGroupName', 'snapshot_cacheParameterGroupName' - The cache parameter group that is associated with the source cluster.
--
-- 'snapshotRetentionLimit', 'snapshot_snapshotRetentionLimit' - For an automatic snapshot, the number of days for which ElastiCache
-- retains the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the @SnapshotRetentionLimit@
-- for the source cluster when the snapshot was created. This field is
-- otherwise ignored: Manual snapshots do not expire, and can only be
-- deleted using the @DeleteSnapshot@ operation.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
--
-- 'numNodeGroups', 'snapshot_numNodeGroups' - The number of node groups (shards) in this snapshot. When restoring from
-- a snapshot, the number of node groups (shards) in the snapshot and in
-- the restored replication group must be the same.
--
-- 'preferredAvailabilityZone', 'snapshot_preferredAvailabilityZone' - The name of the Availability Zone in which the source cluster is
-- located.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'cacheSubnetGroupName', 'snapshot_cacheSubnetGroupName' - The name of the cache subnet group associated with the source cluster.
--
-- 'engineVersion', 'snapshot_engineVersion' - The version of the cache engine version that is used by the source
-- cluster.
--
-- 'preferredMaintenanceWindow', 'snapshot_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
--
-- 'cacheNodeType', 'snapshot_cacheNodeType' - The name of the compute and memory capacity node type for the source
-- cluster.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
--
-- 'topicArn', 'snapshot_topicArn' - The Amazon Resource Name (ARN) for the topic used by the source cluster
-- for publishing notifications.
--
-- 'port', 'snapshot_port' - The port number used by each cache nodes in the source cluster.
--
-- 'snapshotSource', 'snapshot_snapshotSource' - Indicates whether the snapshot is from an automatic backup (@automated@)
-- or was created manually (@manual@).
--
-- 'engine', 'snapshot_engine' - The name of the cache engine (@memcached@ or @redis@) used by the source
-- cluster.
--
-- 'preferredOutpostArn', 'snapshot_preferredOutpostArn' - The ARN (Amazon Resource Name) of the preferred outpost.
--
-- 'snapshotName', 'snapshot_snapshotName' - The name of a snapshot. For an automatic snapshot, the name is
-- system-generated. For a manual snapshot, this is the user-provided name.
--
-- 'vpcId', 'snapshot_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cluster.
--
-- 'autoMinorVersionUpgrade', 'snapshot_autoMinorVersionUpgrade' - This parameter is currently disabled.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { cacheClusterCreateTime = Prelude.Nothing,
      nodeSnapshots = Prelude.Nothing,
      automaticFailover = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshotStatus = Prelude.Nothing,
      arn = Prelude.Nothing,
      replicationGroupDescription = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      numNodeGroups = Prelude.Nothing,
      preferredAvailabilityZone = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      port = Prelude.Nothing,
      snapshotSource = Prelude.Nothing,
      engine = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing
    }

-- | The date and time when the source cluster was created.
snapshot_cacheClusterCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_cacheClusterCreateTime = Lens.lens (\Snapshot' {cacheClusterCreateTime} -> cacheClusterCreateTime) (\s@Snapshot' {} a -> s {cacheClusterCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Core._Time

-- | A list of the cache nodes in the source cluster.
snapshot_nodeSnapshots :: Lens.Lens' Snapshot (Prelude.Maybe [NodeSnapshot])
snapshot_nodeSnapshots = Lens.lens (\Snapshot' {nodeSnapshots} -> nodeSnapshots) (\s@Snapshot' {} a -> s {nodeSnapshots = a} :: Snapshot) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates the status of automatic failover for the source Redis
-- replication group.
snapshot_automaticFailover :: Lens.Lens' Snapshot (Prelude.Maybe AutomaticFailoverStatus)
snapshot_automaticFailover = Lens.lens (\Snapshot' {automaticFailover} -> automaticFailover) (\s@Snapshot' {} a -> s {automaticFailover = a} :: Snapshot)

-- | The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
snapshot_numCacheNodes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numCacheNodes = Lens.lens (\Snapshot' {numCacheNodes} -> numCacheNodes) (\s@Snapshot' {} a -> s {numCacheNodes = a} :: Snapshot)

-- | The unique identifier of the source replication group.
snapshot_replicationGroupId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_replicationGroupId = Lens.lens (\Snapshot' {replicationGroupId} -> replicationGroupId) (\s@Snapshot' {} a -> s {replicationGroupId = a} :: Snapshot)

-- | The user-supplied identifier of the source cluster.
snapshot_cacheClusterId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheClusterId = Lens.lens (\Snapshot' {cacheClusterId} -> cacheClusterId) (\s@Snapshot' {} a -> s {cacheClusterId = a} :: Snapshot)

-- | The daily time range during which ElastiCache takes daily snapshots of
-- the source cluster.
snapshot_snapshotWindow :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotWindow = Lens.lens (\Snapshot' {snapshotWindow} -> snapshotWindow) (\s@Snapshot' {} a -> s {snapshotWindow = a} :: Snapshot)

-- | The status of the snapshot. Valid values: @creating@ | @available@ |
-- @restoring@ | @copying@ | @deleting@.
snapshot_snapshotStatus :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotStatus = Lens.lens (\Snapshot' {snapshotStatus} -> snapshotStatus) (\s@Snapshot' {} a -> s {snapshotStatus = a} :: Snapshot)

-- | The ARN (Amazon Resource Name) of the snapshot.
snapshot_arn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_arn = Lens.lens (\Snapshot' {arn} -> arn) (\s@Snapshot' {} a -> s {arn = a} :: Snapshot)

-- | A description of the source replication group.
snapshot_replicationGroupDescription :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_replicationGroupDescription = Lens.lens (\Snapshot' {replicationGroupDescription} -> replicationGroupDescription) (\s@Snapshot' {} a -> s {replicationGroupDescription = a} :: Snapshot)

-- | The cache parameter group that is associated with the source cluster.
snapshot_cacheParameterGroupName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheParameterGroupName = Lens.lens (\Snapshot' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@Snapshot' {} a -> s {cacheParameterGroupName = a} :: Snapshot)

-- | For an automatic snapshot, the number of days for which ElastiCache
-- retains the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the @SnapshotRetentionLimit@
-- for the source cluster when the snapshot was created. This field is
-- otherwise ignored: Manual snapshots do not expire, and can only be
-- deleted using the @DeleteSnapshot@ operation.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
snapshot_snapshotRetentionLimit :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_snapshotRetentionLimit = Lens.lens (\Snapshot' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@Snapshot' {} a -> s {snapshotRetentionLimit = a} :: Snapshot)

-- | The number of node groups (shards) in this snapshot. When restoring from
-- a snapshot, the number of node groups (shards) in the snapshot and in
-- the restored replication group must be the same.
snapshot_numNodeGroups :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numNodeGroups = Lens.lens (\Snapshot' {numNodeGroups} -> numNodeGroups) (\s@Snapshot' {} a -> s {numNodeGroups = a} :: Snapshot)

-- | The name of the Availability Zone in which the source cluster is
-- located.
snapshot_preferredAvailabilityZone :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_preferredAvailabilityZone = Lens.lens (\Snapshot' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@Snapshot' {} a -> s {preferredAvailabilityZone = a} :: Snapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The name of the cache subnet group associated with the source cluster.
snapshot_cacheSubnetGroupName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheSubnetGroupName = Lens.lens (\Snapshot' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@Snapshot' {} a -> s {cacheSubnetGroupName = a} :: Snapshot)

-- | The version of the cache engine version that is used by the source
-- cluster.
snapshot_engineVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engineVersion = Lens.lens (\Snapshot' {engineVersion} -> engineVersion) (\s@Snapshot' {} a -> s {engineVersion = a} :: Snapshot)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
snapshot_preferredMaintenanceWindow :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_preferredMaintenanceWindow = Lens.lens (\Snapshot' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@Snapshot' {} a -> s {preferredMaintenanceWindow = a} :: Snapshot)

-- | The name of the compute and memory capacity node type for the source
-- cluster.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
snapshot_cacheNodeType :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheNodeType = Lens.lens (\Snapshot' {cacheNodeType} -> cacheNodeType) (\s@Snapshot' {} a -> s {cacheNodeType = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster
-- for publishing notifications.
snapshot_topicArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_topicArn = Lens.lens (\Snapshot' {topicArn} -> topicArn) (\s@Snapshot' {} a -> s {topicArn = a} :: Snapshot)

-- | The port number used by each cache nodes in the source cluster.
snapshot_port :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_port = Lens.lens (\Snapshot' {port} -> port) (\s@Snapshot' {} a -> s {port = a} :: Snapshot)

-- | Indicates whether the snapshot is from an automatic backup (@automated@)
-- or was created manually (@manual@).
snapshot_snapshotSource :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotSource = Lens.lens (\Snapshot' {snapshotSource} -> snapshotSource) (\s@Snapshot' {} a -> s {snapshotSource = a} :: Snapshot)

-- | The name of the cache engine (@memcached@ or @redis@) used by the source
-- cluster.
snapshot_engine :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engine = Lens.lens (\Snapshot' {engine} -> engine) (\s@Snapshot' {} a -> s {engine = a} :: Snapshot)

-- | The ARN (Amazon Resource Name) of the preferred outpost.
snapshot_preferredOutpostArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_preferredOutpostArn = Lens.lens (\Snapshot' {preferredOutpostArn} -> preferredOutpostArn) (\s@Snapshot' {} a -> s {preferredOutpostArn = a} :: Snapshot)

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated. For a manual snapshot, this is the user-provided name.
snapshot_snapshotName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotName = Lens.lens (\Snapshot' {snapshotName} -> snapshotName) (\s@Snapshot' {} a -> s {snapshotName = a} :: Snapshot)

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cluster.
snapshot_vpcId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_vpcId = Lens.lens (\Snapshot' {vpcId} -> vpcId) (\s@Snapshot' {} a -> s {vpcId = a} :: Snapshot)

-- | This parameter is currently disabled.
snapshot_autoMinorVersionUpgrade :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_autoMinorVersionUpgrade = Lens.lens (\Snapshot' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@Snapshot' {} a -> s {autoMinorVersionUpgrade = a} :: Snapshot)

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> (x Core..@? "CacheClusterCreateTime")
      Prelude.<*> ( x Core..@? "NodeSnapshots" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "NodeSnapshot")
                  )
      Prelude.<*> (x Core..@? "AutomaticFailover")
      Prelude.<*> (x Core..@? "NumCacheNodes")
      Prelude.<*> (x Core..@? "ReplicationGroupId")
      Prelude.<*> (x Core..@? "CacheClusterId")
      Prelude.<*> (x Core..@? "SnapshotWindow")
      Prelude.<*> (x Core..@? "SnapshotStatus")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "ReplicationGroupDescription")
      Prelude.<*> (x Core..@? "CacheParameterGroupName")
      Prelude.<*> (x Core..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Core..@? "NumNodeGroups")
      Prelude.<*> (x Core..@? "PreferredAvailabilityZone")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "CacheSubnetGroupName")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "CacheNodeType")
      Prelude.<*> (x Core..@? "TopicArn")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "SnapshotSource")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "PreferredOutpostArn")
      Prelude.<*> (x Core..@? "SnapshotName")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> (x Core..@? "AutoMinorVersionUpgrade")

instance Prelude.Hashable Snapshot

instance Prelude.NFData Snapshot
