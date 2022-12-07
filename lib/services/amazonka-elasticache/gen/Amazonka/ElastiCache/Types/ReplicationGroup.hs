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
-- Module      : Amazonka.ElastiCache.Types.ReplicationGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ReplicationGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.AutomaticFailoverStatus
import Amazonka.ElastiCache.Types.DataTieringStatus
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo
import Amazonka.ElastiCache.Types.IpDiscovery
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.MultiAZStatus
import Amazonka.ElastiCache.Types.NetworkType
import Amazonka.ElastiCache.Types.NodeGroup
import Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'newReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { -- | A flag that enables in-transit encryption when set to @true@.
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the
    -- cluster is created. To enable in-transit encryption on a cluster you
    -- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The outpost ARNs of the replication group\'s member clusters.
    memberClustersOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Global datastore and role of this replication group in
    -- the Global datastore.
    globalReplicationGroupInfo :: Prelude.Maybe GlobalReplicationGroupInfo,
    -- | A flag indicating whether or not this replication group is cluster
    -- enabled; i.e., whether its data can be partitioned across multiple
    -- shards (API\/CLI: node groups).
    --
    -- Valid values: @true@ | @false@
    clusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If you are running Redis engine version 6.0 or later, set this parameter
    -- to yes if you want to opt-in to the next auto minor version upgrade
    -- campaign. This parameter is disabled for previous versions.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the cluster was created.
    replicationGroupCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus,
    -- | The ARN (Amazon Resource Name) of the replication group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group.
    snapshottingClusterId :: Prelude.Maybe Prelude.Text,
    -- | Returns the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfiguration],
    -- | A flag that enables encryption at-rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- cluster is created. To enable encryption at-rest on a cluster you must
    -- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The network type you choose when modifying a cluster, either @ipv4@ |
    -- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
    -- onward or Memcached engine version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    ipDiscovery :: Prelude.Maybe IpDiscovery,
    -- | The current state of this replication group - @creating@, @available@,
    -- @modifying@, @deleting@, @create-failed@, @snapshotting@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for each node in
    -- the replication group.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The user supplied description of the replication group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of node groups in this replication group. For Redis (cluster mode
    -- disabled) replication groups, this is a single-element list. For Redis
    -- (cluster mode enabled) replication groups, the list contains an entry
    -- for each node group (shard).
    nodeGroups :: Prelude.Maybe [NodeGroup],
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The ID of the user group associated to the replication group.
    userGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A group of settings to be applied to the replication group, either
    -- immediately or during the next maintenance window.
    pendingModifiedValues :: Prelude.Maybe ReplicationGroupPendingModifiedValues,
    -- | Enables data tiering. Data tiering is only supported for replication
    -- groups using the r6gd node type. This parameter must be set to true when
    -- using r6gd nodes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
    dataTiering :: Prelude.Maybe DataTieringStatus,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Prelude.Maybe Data.ISO8601,
    -- | The identifier for the replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The names of all the cache clusters that are part of this replication
    -- group.
    memberClusters :: Prelude.Maybe [Prelude.Text],
    -- | Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
    -- workloads using Redis engine version 6.2 onward or Memcached engine
    -- version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    networkType :: Prelude.Maybe NetworkType,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
    multiAZ :: Prelude.Maybe MultiAZStatus,
    -- | The configuration endpoint for this replication group. Use the
    -- configuration endpoint to connect to this replication group.
    configurationEndpoint :: Prelude.Maybe Endpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitEncryptionEnabled', 'replicationGroup_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'memberClustersOutpostArns', 'replicationGroup_memberClustersOutpostArns' - The outpost ARNs of the replication group\'s member clusters.
--
-- 'globalReplicationGroupInfo', 'replicationGroup_globalReplicationGroupInfo' - The name of the Global datastore and role of this replication group in
-- the Global datastore.
--
-- 'clusterEnabled', 'replicationGroup_clusterEnabled' - A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
--
-- 'autoMinorVersionUpgrade', 'replicationGroup_autoMinorVersionUpgrade' - If you are running Redis engine version 6.0 or later, set this parameter
-- to yes if you want to opt-in to the next auto minor version upgrade
-- campaign. This parameter is disabled for previous versions.
--
-- 'replicationGroupCreateTime', 'replicationGroup_replicationGroupCreateTime' - The date and time when the cluster was created.
--
-- 'automaticFailover', 'replicationGroup_automaticFailover' - Indicates the status of automatic failover for this Redis replication
-- group.
--
-- 'arn', 'replicationGroup_arn' - The ARN (Amazon Resource Name) of the replication group.
--
-- 'snapshottingClusterId', 'replicationGroup_snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the
-- replication group.
--
-- 'logDeliveryConfigurations', 'replicationGroup_logDeliveryConfigurations' - Returns the destination, format and type of the logs.
--
-- 'atRestEncryptionEnabled', 'replicationGroup_atRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable encryption at-rest on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'ipDiscovery', 'replicationGroup_ipDiscovery' - The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'status', 'replicationGroup_status' - The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
--
-- 'cacheNodeType', 'replicationGroup_cacheNodeType' - The name of the compute and memory capacity node type for each node in
-- the replication group.
--
-- 'description', 'replicationGroup_description' - The user supplied description of the replication group.
--
-- 'authTokenEnabled', 'replicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'nodeGroups', 'replicationGroup_nodeGroups' - A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
--
-- 'snapshotWindow', 'replicationGroup_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- 'snapshotRetentionLimit', 'replicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
--
-- 'userGroupIds', 'replicationGroup_userGroupIds' - The ID of the user group associated to the replication group.
--
-- 'kmsKeyId', 'replicationGroup_kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
--
-- 'pendingModifiedValues', 'replicationGroup_pendingModifiedValues' - A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
--
-- 'dataTiering', 'replicationGroup_dataTiering' - Enables data tiering. Data tiering is only supported for replication
-- groups using the r6gd node type. This parameter must be set to true when
-- using r6gd nodes. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
--
-- 'authTokenLastModifiedDate', 'replicationGroup_authTokenLastModifiedDate' - The date the auth token was last modified
--
-- 'replicationGroupId', 'replicationGroup_replicationGroupId' - The identifier for the replication group.
--
-- 'memberClusters', 'replicationGroup_memberClusters' - The names of all the cache clusters that are part of this replication
-- group.
--
-- 'networkType', 'replicationGroup_networkType' - Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
-- workloads using Redis engine version 6.2 onward or Memcached engine
-- version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'multiAZ', 'replicationGroup_multiAZ' - A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
--
-- 'configurationEndpoint', 'replicationGroup_configurationEndpoint' - The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
newReplicationGroup ::
  ReplicationGroup
newReplicationGroup =
  ReplicationGroup'
    { transitEncryptionEnabled =
        Prelude.Nothing,
      memberClustersOutpostArns = Prelude.Nothing,
      globalReplicationGroupInfo = Prelude.Nothing,
      clusterEnabled = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      replicationGroupCreateTime = Prelude.Nothing,
      automaticFailover = Prelude.Nothing,
      arn = Prelude.Nothing,
      snapshottingClusterId = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      ipDiscovery = Prelude.Nothing,
      status = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      description = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      nodeGroups = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      userGroupIds = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      dataTiering = Prelude.Nothing,
      authTokenLastModifiedDate = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      memberClusters = Prelude.Nothing,
      networkType = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      configurationEndpoint = Prelude.Nothing
    }

-- | A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
replicationGroup_transitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_transitEncryptionEnabled = Lens.lens (\ReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: ReplicationGroup)

-- | The outpost ARNs of the replication group\'s member clusters.
replicationGroup_memberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClustersOutpostArns = Lens.lens (\ReplicationGroup' {memberClustersOutpostArns} -> memberClustersOutpostArns) (\s@ReplicationGroup' {} a -> s {memberClustersOutpostArns = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Global datastore and role of this replication group in
-- the Global datastore.
replicationGroup_globalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Prelude.Maybe GlobalReplicationGroupInfo)
replicationGroup_globalReplicationGroupInfo = Lens.lens (\ReplicationGroup' {globalReplicationGroupInfo} -> globalReplicationGroupInfo) (\s@ReplicationGroup' {} a -> s {globalReplicationGroupInfo = a} :: ReplicationGroup)

-- | A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
replicationGroup_clusterEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_clusterEnabled = Lens.lens (\ReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@ReplicationGroup' {} a -> s {clusterEnabled = a} :: ReplicationGroup)

-- | If you are running Redis engine version 6.0 or later, set this parameter
-- to yes if you want to opt-in to the next auto minor version upgrade
-- campaign. This parameter is disabled for previous versions.
replicationGroup_autoMinorVersionUpgrade :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_autoMinorVersionUpgrade = Lens.lens (\ReplicationGroup' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ReplicationGroup' {} a -> s {autoMinorVersionUpgrade = a} :: ReplicationGroup)

-- | The date and time when the cluster was created.
replicationGroup_replicationGroupCreateTime :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.UTCTime)
replicationGroup_replicationGroupCreateTime = Lens.lens (\ReplicationGroup' {replicationGroupCreateTime} -> replicationGroupCreateTime) (\s@ReplicationGroup' {} a -> s {replicationGroupCreateTime = a} :: ReplicationGroup) Prelude.. Lens.mapping Data._Time

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroup_automaticFailover :: Lens.Lens' ReplicationGroup (Prelude.Maybe AutomaticFailoverStatus)
replicationGroup_automaticFailover = Lens.lens (\ReplicationGroup' {automaticFailover} -> automaticFailover) (\s@ReplicationGroup' {} a -> s {automaticFailover = a} :: ReplicationGroup)

-- | The ARN (Amazon Resource Name) of the replication group.
replicationGroup_arn :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_arn = Lens.lens (\ReplicationGroup' {arn} -> arn) (\s@ReplicationGroup' {} a -> s {arn = a} :: ReplicationGroup)

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group.
replicationGroup_snapshottingClusterId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_snapshottingClusterId = Lens.lens (\ReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ReplicationGroup)

-- | Returns the destination, format and type of the logs.
replicationGroup_logDeliveryConfigurations :: Lens.Lens' ReplicationGroup (Prelude.Maybe [LogDeliveryConfiguration])
replicationGroup_logDeliveryConfigurations = Lens.lens (\ReplicationGroup' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ReplicationGroup' {} a -> s {logDeliveryConfigurations = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable encryption at-rest on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
replicationGroup_atRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_atRestEncryptionEnabled = Lens.lens (\ReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: ReplicationGroup)

-- | The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
replicationGroup_ipDiscovery :: Lens.Lens' ReplicationGroup (Prelude.Maybe IpDiscovery)
replicationGroup_ipDiscovery = Lens.lens (\ReplicationGroup' {ipDiscovery} -> ipDiscovery) (\s@ReplicationGroup' {} a -> s {ipDiscovery = a} :: ReplicationGroup)

-- | The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
replicationGroup_status :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_status = Lens.lens (\ReplicationGroup' {status} -> status) (\s@ReplicationGroup' {} a -> s {status = a} :: ReplicationGroup)

-- | The name of the compute and memory capacity node type for each node in
-- the replication group.
replicationGroup_cacheNodeType :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_cacheNodeType = Lens.lens (\ReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ReplicationGroup' {} a -> s {cacheNodeType = a} :: ReplicationGroup)

-- | The user supplied description of the replication group.
replicationGroup_description :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_description = Lens.lens (\ReplicationGroup' {description} -> description) (\s@ReplicationGroup' {} a -> s {description = a} :: ReplicationGroup)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
replicationGroup_authTokenEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_authTokenEnabled = Lens.lens (\ReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@ReplicationGroup' {} a -> s {authTokenEnabled = a} :: ReplicationGroup)

-- | A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
replicationGroup_nodeGroups :: Lens.Lens' ReplicationGroup (Prelude.Maybe [NodeGroup])
replicationGroup_nodeGroups = Lens.lens (\ReplicationGroup' {nodeGroups} -> nodeGroups) (\s@ReplicationGroup' {} a -> s {nodeGroups = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
replicationGroup_snapshotWindow :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_snapshotWindow = Lens.lens (\ReplicationGroup' {snapshotWindow} -> snapshotWindow) (\s@ReplicationGroup' {} a -> s {snapshotWindow = a} :: ReplicationGroup)

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
replicationGroup_snapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Int)
replicationGroup_snapshotRetentionLimit = Lens.lens (\ReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ReplicationGroup)

-- | The ID of the user group associated to the replication group.
replicationGroup_userGroupIds :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_userGroupIds = Lens.lens (\ReplicationGroup' {userGroupIds} -> userGroupIds) (\s@ReplicationGroup' {} a -> s {userGroupIds = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the KMS key used to encrypt the disk in the cluster.
replicationGroup_kmsKeyId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_kmsKeyId = Lens.lens (\ReplicationGroup' {kmsKeyId} -> kmsKeyId) (\s@ReplicationGroup' {} a -> s {kmsKeyId = a} :: ReplicationGroup)

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
replicationGroup_pendingModifiedValues :: Lens.Lens' ReplicationGroup (Prelude.Maybe ReplicationGroupPendingModifiedValues)
replicationGroup_pendingModifiedValues = Lens.lens (\ReplicationGroup' {pendingModifiedValues} -> pendingModifiedValues) (\s@ReplicationGroup' {} a -> s {pendingModifiedValues = a} :: ReplicationGroup)

-- | Enables data tiering. Data tiering is only supported for replication
-- groups using the r6gd node type. This parameter must be set to true when
-- using r6gd nodes. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/data-tiering.html Data tiering>.
replicationGroup_dataTiering :: Lens.Lens' ReplicationGroup (Prelude.Maybe DataTieringStatus)
replicationGroup_dataTiering = Lens.lens (\ReplicationGroup' {dataTiering} -> dataTiering) (\s@ReplicationGroup' {} a -> s {dataTiering = a} :: ReplicationGroup)

-- | The date the auth token was last modified
replicationGroup_authTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.UTCTime)
replicationGroup_authTokenLastModifiedDate = Lens.lens (\ReplicationGroup' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@ReplicationGroup' {} a -> s {authTokenLastModifiedDate = a} :: ReplicationGroup) Prelude.. Lens.mapping Data._Time

-- | The identifier for the replication group.
replicationGroup_replicationGroupId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_replicationGroupId = Lens.lens (\ReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ReplicationGroup' {} a -> s {replicationGroupId = a} :: ReplicationGroup)

-- | The names of all the cache clusters that are part of this replication
-- group.
replicationGroup_memberClusters :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClusters = Lens.lens (\ReplicationGroup' {memberClusters} -> memberClusters) (\s@ReplicationGroup' {} a -> s {memberClusters = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
-- workloads using Redis engine version 6.2 onward or Memcached engine
-- version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
replicationGroup_networkType :: Lens.Lens' ReplicationGroup (Prelude.Maybe NetworkType)
replicationGroup_networkType = Lens.lens (\ReplicationGroup' {networkType} -> networkType) (\s@ReplicationGroup' {} a -> s {networkType = a} :: ReplicationGroup)

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
replicationGroup_multiAZ :: Lens.Lens' ReplicationGroup (Prelude.Maybe MultiAZStatus)
replicationGroup_multiAZ = Lens.lens (\ReplicationGroup' {multiAZ} -> multiAZ) (\s@ReplicationGroup' {} a -> s {multiAZ = a} :: ReplicationGroup)

-- | The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
replicationGroup_configurationEndpoint :: Lens.Lens' ReplicationGroup (Prelude.Maybe Endpoint)
replicationGroup_configurationEndpoint = Lens.lens (\ReplicationGroup' {configurationEndpoint} -> configurationEndpoint) (\s@ReplicationGroup' {} a -> s {configurationEndpoint = a} :: ReplicationGroup)

instance Data.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Prelude.<$> (x Data..@? "TransitEncryptionEnabled")
      Prelude.<*> ( x Data..@? "MemberClustersOutpostArns"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "ReplicationGroupOutpostArn")
                  )
      Prelude.<*> (x Data..@? "GlobalReplicationGroupInfo")
      Prelude.<*> (x Data..@? "ClusterEnabled")
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "ReplicationGroupCreateTime")
      Prelude.<*> (x Data..@? "AutomaticFailover")
      Prelude.<*> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "SnapshottingClusterId")
      Prelude.<*> ( x Data..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "LogDeliveryConfiguration")
                  )
      Prelude.<*> (x Data..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Data..@? "IpDiscovery")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "CacheNodeType")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "AuthTokenEnabled")
      Prelude.<*> ( x Data..@? "NodeGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "NodeGroup")
                  )
      Prelude.<*> (x Data..@? "SnapshotWindow")
      Prelude.<*> (x Data..@? "SnapshotRetentionLimit")
      Prelude.<*> ( x Data..@? "UserGroupIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "PendingModifiedValues")
      Prelude.<*> (x Data..@? "DataTiering")
      Prelude.<*> (x Data..@? "AuthTokenLastModifiedDate")
      Prelude.<*> (x Data..@? "ReplicationGroupId")
      Prelude.<*> ( x Data..@? "MemberClusters" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ClusterId")
                  )
      Prelude.<*> (x Data..@? "NetworkType")
      Prelude.<*> (x Data..@? "MultiAZ")
      Prelude.<*> (x Data..@? "ConfigurationEndpoint")

instance Prelude.Hashable ReplicationGroup where
  hashWithSalt _salt ReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` memberClustersOutpostArns
      `Prelude.hashWithSalt` globalReplicationGroupInfo
      `Prelude.hashWithSalt` clusterEnabled
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` replicationGroupCreateTime
      `Prelude.hashWithSalt` automaticFailover
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` snapshottingClusterId
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` ipDiscovery
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` authTokenEnabled
      `Prelude.hashWithSalt` nodeGroups
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` userGroupIds
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` dataTiering
      `Prelude.hashWithSalt` authTokenLastModifiedDate
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` memberClusters
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` configurationEndpoint

instance Prelude.NFData ReplicationGroup where
  rnf ReplicationGroup' {..} =
    Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf memberClustersOutpostArns
      `Prelude.seq` Prelude.rnf globalReplicationGroupInfo
      `Prelude.seq` Prelude.rnf clusterEnabled
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf replicationGroupCreateTime
      `Prelude.seq` Prelude.rnf automaticFailover
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf snapshottingClusterId
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf ipDiscovery
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf authTokenEnabled
      `Prelude.seq` Prelude.rnf nodeGroups
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf userGroupIds
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf dataTiering
      `Prelude.seq` Prelude.rnf
        authTokenLastModifiedDate
      `Prelude.seq` Prelude.rnf
        replicationGroupId
      `Prelude.seq` Prelude.rnf
        memberClusters
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        configurationEndpoint
