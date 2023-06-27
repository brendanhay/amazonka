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
-- Module      : Amazonka.ElastiCache.Types.Snapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.AutomaticFailoverStatus
import Amazonka.ElastiCache.Types.DataTieringStatus
import Amazonka.ElastiCache.Types.NodeSnapshot
import qualified Amazonka.Prelude as Prelude

-- | Represents a copy of an entire Redis cluster as of the time when the
-- snapshot was taken.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The ARN (Amazon Resource Name) of the snapshot.
    arn :: Prelude.Maybe Prelude.Text,
    -- | If you are running Redis engine version 6.0 or later, set this
    -- parameter to yes if you want to opt-in to the next auto minor version
    -- upgrade campaign. This parameter is disabled for previous versions.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the status of automatic failover for the source Redis
    -- replication group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus,
    -- | The date and time when the source cluster was created.
    cacheClusterCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The user-supplied identifier of the source cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
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
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
    --         __T4g node types__ (available only for Redis engine version
    --         5.0.6 onward and Memcached engine version 1.5.16 onward):
    --         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    -- | The cache parameter group that is associated with the source cluster.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache subnet group associated with the source cluster.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Enables data tiering. Data tiering is only supported for replication
    -- groups using the r6gd node type. This parameter must be set to true when
    -- using r6gd nodes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
    dataTiering :: Prelude.Maybe DataTieringStatus,
    -- | The name of the cache engine (@memcached@ or @redis@) used by the source
    -- cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The version of the cache engine version that is used by the source
    -- cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of the cache nodes in the source cluster.
    nodeSnapshots :: Prelude.Maybe [NodeSnapshot],
    -- | The number of cache nodes in the source cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 40.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
    -- | The number of node groups (shards) in this snapshot. When restoring from
    -- a snapshot, the number of node groups (shards) in the snapshot and in
    -- the restored replication group must be the same.
    numNodeGroups :: Prelude.Maybe Prelude.Int,
    -- | The port number used by each cache nodes in the source cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the Availability Zone in which the source cluster is
    -- located.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
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
    -- | The ARN (Amazon Resource Name) of the preferred outpost.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the source replication group.
    replicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the source replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of a snapshot. For an automatic snapshot, the name is
    -- system-generated. For a manual snapshot, this is the user-provided name.
    snapshotName :: Prelude.Maybe Prelude.Text,
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
    -- | Indicates whether the snapshot is from an automatic backup (@automated@)
    -- or was created manually (@manual@).
    snapshotSource :: Prelude.Maybe Prelude.Text,
    -- | The status of the snapshot. Valid values: @creating@ | @available@ |
    -- @restoring@ | @copying@ | @deleting@.
    snapshotStatus :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which ElastiCache takes daily snapshots of
    -- the source cluster.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the topic used by the source cluster
    -- for publishing notifications.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
    -- group for the source cluster.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'snapshot_arn' - The ARN (Amazon Resource Name) of the snapshot.
--
-- 'autoMinorVersionUpgrade', 'snapshot_autoMinorVersionUpgrade' - If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
--
-- 'automaticFailover', 'snapshot_automaticFailover' - Indicates the status of automatic failover for the source Redis
-- replication group.
--
-- 'cacheClusterCreateTime', 'snapshot_cacheClusterCreateTime' - The date and time when the source cluster was created.
--
-- 'cacheClusterId', 'snapshot_cacheClusterId' - The user-supplied identifier of the source cluster.
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
--         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
--         __T4g node types__ (available only for Redis engine version
--         5.0.6 onward and Memcached engine version 1.5.16 onward):
--         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
-- 'cacheParameterGroupName', 'snapshot_cacheParameterGroupName' - The cache parameter group that is associated with the source cluster.
--
-- 'cacheSubnetGroupName', 'snapshot_cacheSubnetGroupName' - The name of the cache subnet group associated with the source cluster.
--
-- 'dataTiering', 'snapshot_dataTiering' - Enables data tiering. Data tiering is only supported for replication
-- groups using the r6gd node type. This parameter must be set to true when
-- using r6gd nodes. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
--
-- 'engine', 'snapshot_engine' - The name of the cache engine (@memcached@ or @redis@) used by the source
-- cluster.
--
-- 'engineVersion', 'snapshot_engineVersion' - The version of the cache engine version that is used by the source
-- cluster.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- 'nodeSnapshots', 'snapshot_nodeSnapshots' - A list of the cache nodes in the source cluster.
--
-- 'numCacheNodes', 'snapshot_numCacheNodes' - The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
--
-- 'numNodeGroups', 'snapshot_numNodeGroups' - The number of node groups (shards) in this snapshot. When restoring from
-- a snapshot, the number of node groups (shards) in the snapshot and in
-- the restored replication group must be the same.
--
-- 'port', 'snapshot_port' - The port number used by each cache nodes in the source cluster.
--
-- 'preferredAvailabilityZone', 'snapshot_preferredAvailabilityZone' - The name of the Availability Zone in which the source cluster is
-- located.
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
-- 'preferredOutpostArn', 'snapshot_preferredOutpostArn' - The ARN (Amazon Resource Name) of the preferred outpost.
--
-- 'replicationGroupDescription', 'snapshot_replicationGroupDescription' - A description of the source replication group.
--
-- 'replicationGroupId', 'snapshot_replicationGroupId' - The unique identifier of the source replication group.
--
-- 'snapshotName', 'snapshot_snapshotName' - The name of a snapshot. For an automatic snapshot, the name is
-- system-generated. For a manual snapshot, this is the user-provided name.
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
-- 'snapshotSource', 'snapshot_snapshotSource' - Indicates whether the snapshot is from an automatic backup (@automated@)
-- or was created manually (@manual@).
--
-- 'snapshotStatus', 'snapshot_snapshotStatus' - The status of the snapshot. Valid values: @creating@ | @available@ |
-- @restoring@ | @copying@ | @deleting@.
--
-- 'snapshotWindow', 'snapshot_snapshotWindow' - The daily time range during which ElastiCache takes daily snapshots of
-- the source cluster.
--
-- 'topicArn', 'snapshot_topicArn' - The Amazon Resource Name (ARN) for the topic used by the source cluster
-- for publishing notifications.
--
-- 'vpcId', 'snapshot_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cluster.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { arn = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      automaticFailover = Prelude.Nothing,
      cacheClusterCreateTime = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      dataTiering = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      nodeSnapshots = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing,
      numNodeGroups = Prelude.Nothing,
      port = Prelude.Nothing,
      preferredAvailabilityZone = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      replicationGroupDescription = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snapshotSource = Prelude.Nothing,
      snapshotStatus = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the snapshot.
snapshot_arn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_arn = Lens.lens (\Snapshot' {arn} -> arn) (\s@Snapshot' {} a -> s {arn = a} :: Snapshot)

-- | If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
snapshot_autoMinorVersionUpgrade :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_autoMinorVersionUpgrade = Lens.lens (\Snapshot' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@Snapshot' {} a -> s {autoMinorVersionUpgrade = a} :: Snapshot)

-- | Indicates the status of automatic failover for the source Redis
-- replication group.
snapshot_automaticFailover :: Lens.Lens' Snapshot (Prelude.Maybe AutomaticFailoverStatus)
snapshot_automaticFailover = Lens.lens (\Snapshot' {automaticFailover} -> automaticFailover) (\s@Snapshot' {} a -> s {automaticFailover = a} :: Snapshot)

-- | The date and time when the source cluster was created.
snapshot_cacheClusterCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_cacheClusterCreateTime = Lens.lens (\Snapshot' {cacheClusterCreateTime} -> cacheClusterCreateTime) (\s@Snapshot' {} a -> s {cacheClusterCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

-- | The user-supplied identifier of the source cluster.
snapshot_cacheClusterId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheClusterId = Lens.lens (\Snapshot' {cacheClusterId} -> cacheClusterId) (\s@Snapshot' {} a -> s {cacheClusterId = a} :: Snapshot)

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
--         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
--         __T4g node types__ (available only for Redis engine version
--         5.0.6 onward and Memcached engine version 1.5.16 onward):
--         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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

-- | The cache parameter group that is associated with the source cluster.
snapshot_cacheParameterGroupName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheParameterGroupName = Lens.lens (\Snapshot' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@Snapshot' {} a -> s {cacheParameterGroupName = a} :: Snapshot)

-- | The name of the cache subnet group associated with the source cluster.
snapshot_cacheSubnetGroupName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_cacheSubnetGroupName = Lens.lens (\Snapshot' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@Snapshot' {} a -> s {cacheSubnetGroupName = a} :: Snapshot)

-- | Enables data tiering. Data tiering is only supported for replication
-- groups using the r6gd node type. This parameter must be set to true when
-- using r6gd nodes. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
snapshot_dataTiering :: Lens.Lens' Snapshot (Prelude.Maybe DataTieringStatus)
snapshot_dataTiering = Lens.lens (\Snapshot' {dataTiering} -> dataTiering) (\s@Snapshot' {} a -> s {dataTiering = a} :: Snapshot)

-- | The name of the cache engine (@memcached@ or @redis@) used by the source
-- cluster.
snapshot_engine :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engine = Lens.lens (\Snapshot' {engine} -> engine) (\s@Snapshot' {} a -> s {engine = a} :: Snapshot)

-- | The version of the cache engine version that is used by the source
-- cluster.
snapshot_engineVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engineVersion = Lens.lens (\Snapshot' {engineVersion} -> engineVersion) (\s@Snapshot' {} a -> s {engineVersion = a} :: Snapshot)

-- | The ID of the KMS key used to encrypt the snapshot.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | A list of the cache nodes in the source cluster.
snapshot_nodeSnapshots :: Lens.Lens' Snapshot (Prelude.Maybe [NodeSnapshot])
snapshot_nodeSnapshots = Lens.lens (\Snapshot' {nodeSnapshots} -> nodeSnapshots) (\s@Snapshot' {} a -> s {nodeSnapshots = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The number of cache nodes in the source cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
snapshot_numCacheNodes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numCacheNodes = Lens.lens (\Snapshot' {numCacheNodes} -> numCacheNodes) (\s@Snapshot' {} a -> s {numCacheNodes = a} :: Snapshot)

-- | The number of node groups (shards) in this snapshot. When restoring from
-- a snapshot, the number of node groups (shards) in the snapshot and in
-- the restored replication group must be the same.
snapshot_numNodeGroups :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numNodeGroups = Lens.lens (\Snapshot' {numNodeGroups} -> numNodeGroups) (\s@Snapshot' {} a -> s {numNodeGroups = a} :: Snapshot)

-- | The port number used by each cache nodes in the source cluster.
snapshot_port :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_port = Lens.lens (\Snapshot' {port} -> port) (\s@Snapshot' {} a -> s {port = a} :: Snapshot)

-- | The name of the Availability Zone in which the source cluster is
-- located.
snapshot_preferredAvailabilityZone :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_preferredAvailabilityZone = Lens.lens (\Snapshot' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@Snapshot' {} a -> s {preferredAvailabilityZone = a} :: Snapshot)

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

-- | The ARN (Amazon Resource Name) of the preferred outpost.
snapshot_preferredOutpostArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_preferredOutpostArn = Lens.lens (\Snapshot' {preferredOutpostArn} -> preferredOutpostArn) (\s@Snapshot' {} a -> s {preferredOutpostArn = a} :: Snapshot)

-- | A description of the source replication group.
snapshot_replicationGroupDescription :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_replicationGroupDescription = Lens.lens (\Snapshot' {replicationGroupDescription} -> replicationGroupDescription) (\s@Snapshot' {} a -> s {replicationGroupDescription = a} :: Snapshot)

-- | The unique identifier of the source replication group.
snapshot_replicationGroupId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_replicationGroupId = Lens.lens (\Snapshot' {replicationGroupId} -> replicationGroupId) (\s@Snapshot' {} a -> s {replicationGroupId = a} :: Snapshot)

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated. For a manual snapshot, this is the user-provided name.
snapshot_snapshotName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotName = Lens.lens (\Snapshot' {snapshotName} -> snapshotName) (\s@Snapshot' {} a -> s {snapshotName = a} :: Snapshot)

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

-- | Indicates whether the snapshot is from an automatic backup (@automated@)
-- or was created manually (@manual@).
snapshot_snapshotSource :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotSource = Lens.lens (\Snapshot' {snapshotSource} -> snapshotSource) (\s@Snapshot' {} a -> s {snapshotSource = a} :: Snapshot)

-- | The status of the snapshot. Valid values: @creating@ | @available@ |
-- @restoring@ | @copying@ | @deleting@.
snapshot_snapshotStatus :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotStatus = Lens.lens (\Snapshot' {snapshotStatus} -> snapshotStatus) (\s@Snapshot' {} a -> s {snapshotStatus = a} :: Snapshot)

-- | The daily time range during which ElastiCache takes daily snapshots of
-- the source cluster.
snapshot_snapshotWindow :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotWindow = Lens.lens (\Snapshot' {snapshotWindow} -> snapshotWindow) (\s@Snapshot' {} a -> s {snapshotWindow = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster
-- for publishing notifications.
snapshot_topicArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_topicArn = Lens.lens (\Snapshot' {topicArn} -> topicArn) (\s@Snapshot' {} a -> s {topicArn = a} :: Snapshot)

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cluster.
snapshot_vpcId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_vpcId = Lens.lens (\Snapshot' {vpcId} -> vpcId) (\s@Snapshot' {} a -> s {vpcId = a} :: Snapshot)

instance Data.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "AutomaticFailover")
      Prelude.<*> (x Data..@? "CacheClusterCreateTime")
      Prelude.<*> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "CacheNodeType")
      Prelude.<*> (x Data..@? "CacheParameterGroupName")
      Prelude.<*> (x Data..@? "CacheSubnetGroupName")
      Prelude.<*> (x Data..@? "DataTiering")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> ( x
                      Data..@? "NodeSnapshots"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "NodeSnapshot")
                  )
      Prelude.<*> (x Data..@? "NumCacheNodes")
      Prelude.<*> (x Data..@? "NumNodeGroups")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "PreferredAvailabilityZone")
      Prelude.<*> (x Data..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Data..@? "PreferredOutpostArn")
      Prelude.<*> (x Data..@? "ReplicationGroupDescription")
      Prelude.<*> (x Data..@? "ReplicationGroupId")
      Prelude.<*> (x Data..@? "SnapshotName")
      Prelude.<*> (x Data..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Data..@? "SnapshotSource")
      Prelude.<*> (x Data..@? "SnapshotStatus")
      Prelude.<*> (x Data..@? "SnapshotWindow")
      Prelude.<*> (x Data..@? "TopicArn")
      Prelude.<*> (x Data..@? "VpcId")

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` automaticFailover
      `Prelude.hashWithSalt` cacheClusterCreateTime
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` dataTiering
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` nodeSnapshots
      `Prelude.hashWithSalt` numCacheNodes
      `Prelude.hashWithSalt` numNodeGroups
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredAvailabilityZone
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` preferredOutpostArn
      `Prelude.hashWithSalt` replicationGroupDescription
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snapshotSource
      `Prelude.hashWithSalt` snapshotStatus
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf automaticFailover
      `Prelude.seq` Prelude.rnf cacheClusterCreateTime
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf dataTiering
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf nodeSnapshots
      `Prelude.seq` Prelude.rnf numCacheNodes
      `Prelude.seq` Prelude.rnf numNodeGroups
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        preferredAvailabilityZone
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf preferredOutpostArn
      `Prelude.seq` Prelude.rnf
        replicationGroupDescription
      `Prelude.seq` Prelude.rnf
        replicationGroupId
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf
        snapshotSource
      `Prelude.seq` Prelude.rnf
        snapshotStatus
      `Prelude.seq` Prelude.rnf
        snapshotWindow
      `Prelude.seq` Prelude.rnf
        topicArn
      `Prelude.seq` Prelude.rnf
        vpcId
