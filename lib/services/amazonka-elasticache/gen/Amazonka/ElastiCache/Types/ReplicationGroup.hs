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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ReplicationGroup where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.AutomaticFailoverStatus
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.MultiAZStatus
import Amazonka.ElastiCache.Types.NodeGroup
import Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'newReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Prelude.Maybe Core.ISO8601,
    -- | The current state of this replication group - @creating@, @available@,
    -- @modifying@, @deleting@, @create-failed@, @snapshotting@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for each node in
    -- the replication group.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | A list of node groups in this replication group. For Redis (cluster mode
    -- disabled) replication groups, this is a single-element list. For Redis
    -- (cluster mode enabled) replication groups, the list contains an entry
    -- for each node group (shard).
    nodeGroups :: Prelude.Maybe [NodeGroup],
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group.
    snapshottingClusterId :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating whether or not this replication group is cluster
    -- enabled; i.e., whether its data can be partitioned across multiple
    -- shards (API\/CLI: node groups).
    --
    -- Valid values: @true@ | @false@
    clusterEnabled :: Prelude.Maybe Prelude.Bool,
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
    -- | The date and time when the cluster was created.
    replicationGroupCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The ARN (Amazon Resource Name) of the replication group.
    arn :: Prelude.Maybe Prelude.Text,
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
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the user group associated to the replication group.
    userGroupIds :: Prelude.Maybe [Prelude.Text],
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
    -- | Returns the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfiguration],
    -- | The configuration endpoint for this replication group. Use the
    -- configuration endpoint to connect to this replication group.
    configurationEndpoint :: Prelude.Maybe Endpoint,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The names of all the cache clusters that are part of this replication
    -- group.
    memberClusters :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
    multiAZ :: Prelude.Maybe MultiAZStatus,
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The user supplied description of the replication group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | A group of settings to be applied to the replication group, either
    -- immediately or during the next maintenance window.
    pendingModifiedValues :: Prelude.Maybe ReplicationGroupPendingModifiedValues,
    -- | The name of the Global datastore and role of this replication group in
    -- the Global datastore.
    globalReplicationGroupInfo :: Prelude.Maybe GlobalReplicationGroupInfo,
    -- | The outpost ARNs of the replication group\'s member clusters.
    memberClustersOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus
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
-- 'authTokenLastModifiedDate', 'replicationGroup_authTokenLastModifiedDate' - The date the auth token was last modified
--
-- 'status', 'replicationGroup_status' - The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
--
-- 'cacheNodeType', 'replicationGroup_cacheNodeType' - The name of the compute and memory capacity node type for each node in
-- the replication group.
--
-- 'nodeGroups', 'replicationGroup_nodeGroups' - A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
--
-- 'snapshottingClusterId', 'replicationGroup_snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the
-- replication group.
--
-- 'clusterEnabled', 'replicationGroup_clusterEnabled' - A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
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
-- 'replicationGroupCreateTime', 'replicationGroup_replicationGroupCreateTime' - The date and time when the cluster was created.
--
-- 'arn', 'replicationGroup_arn' - The ARN (Amazon Resource Name) of the replication group.
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
-- 'userGroupIds', 'replicationGroup_userGroupIds' - The ID of the user group associated to the replication group.
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
-- 'logDeliveryConfigurations', 'replicationGroup_logDeliveryConfigurations' - Returns the destination, format and type of the logs.
--
-- 'configurationEndpoint', 'replicationGroup_configurationEndpoint' - The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
--
-- 'authTokenEnabled', 'replicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'memberClusters', 'replicationGroup_memberClusters' - The names of all the cache clusters that are part of this replication
-- group.
--
-- 'kmsKeyId', 'replicationGroup_kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
--
-- 'multiAZ', 'replicationGroup_multiAZ' - A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
--
-- 'snapshotRetentionLimit', 'replicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
--
-- 'description', 'replicationGroup_description' - The user supplied description of the replication group.
--
-- 'replicationGroupId', 'replicationGroup_replicationGroupId' - The identifier for the replication group.
--
-- 'pendingModifiedValues', 'replicationGroup_pendingModifiedValues' - A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
--
-- 'globalReplicationGroupInfo', 'replicationGroup_globalReplicationGroupInfo' - The name of the Global datastore and role of this replication group in
-- the Global datastore.
--
-- 'memberClustersOutpostArns', 'replicationGroup_memberClustersOutpostArns' - The outpost ARNs of the replication group\'s member clusters.
--
-- 'automaticFailover', 'replicationGroup_automaticFailover' - Indicates the status of automatic failover for this Redis replication
-- group.
newReplicationGroup ::
  ReplicationGroup
newReplicationGroup =
  ReplicationGroup'
    { authTokenLastModifiedDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      nodeGroups = Prelude.Nothing,
      snapshottingClusterId = Prelude.Nothing,
      clusterEnabled = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      replicationGroupCreateTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing,
      userGroupIds = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      configurationEndpoint = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      memberClusters = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      description = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      globalReplicationGroupInfo = Prelude.Nothing,
      memberClustersOutpostArns = Prelude.Nothing,
      automaticFailover = Prelude.Nothing
    }

-- | The date the auth token was last modified
replicationGroup_authTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.UTCTime)
replicationGroup_authTokenLastModifiedDate = Lens.lens (\ReplicationGroup' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@ReplicationGroup' {} a -> s {authTokenLastModifiedDate = a} :: ReplicationGroup) Prelude.. Lens.mapping Core._Time

-- | The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
replicationGroup_status :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_status = Lens.lens (\ReplicationGroup' {status} -> status) (\s@ReplicationGroup' {} a -> s {status = a} :: ReplicationGroup)

-- | The name of the compute and memory capacity node type for each node in
-- the replication group.
replicationGroup_cacheNodeType :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_cacheNodeType = Lens.lens (\ReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ReplicationGroup' {} a -> s {cacheNodeType = a} :: ReplicationGroup)

-- | A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
replicationGroup_nodeGroups :: Lens.Lens' ReplicationGroup (Prelude.Maybe [NodeGroup])
replicationGroup_nodeGroups = Lens.lens (\ReplicationGroup' {nodeGroups} -> nodeGroups) (\s@ReplicationGroup' {} a -> s {nodeGroups = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group.
replicationGroup_snapshottingClusterId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_snapshottingClusterId = Lens.lens (\ReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ReplicationGroup)

-- | A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
replicationGroup_clusterEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_clusterEnabled = Lens.lens (\ReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@ReplicationGroup' {} a -> s {clusterEnabled = a} :: ReplicationGroup)

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

-- | The date and time when the cluster was created.
replicationGroup_replicationGroupCreateTime :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.UTCTime)
replicationGroup_replicationGroupCreateTime = Lens.lens (\ReplicationGroup' {replicationGroupCreateTime} -> replicationGroupCreateTime) (\s@ReplicationGroup' {} a -> s {replicationGroupCreateTime = a} :: ReplicationGroup) Prelude.. Lens.mapping Core._Time

-- | The ARN (Amazon Resource Name) of the replication group.
replicationGroup_arn :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_arn = Lens.lens (\ReplicationGroup' {arn} -> arn) (\s@ReplicationGroup' {} a -> s {arn = a} :: ReplicationGroup)

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

-- | The ID of the user group associated to the replication group.
replicationGroup_userGroupIds :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_userGroupIds = Lens.lens (\ReplicationGroup' {userGroupIds} -> userGroupIds) (\s@ReplicationGroup' {} a -> s {userGroupIds = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

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

-- | Returns the destination, format and type of the logs.
replicationGroup_logDeliveryConfigurations :: Lens.Lens' ReplicationGroup (Prelude.Maybe [LogDeliveryConfiguration])
replicationGroup_logDeliveryConfigurations = Lens.lens (\ReplicationGroup' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ReplicationGroup' {} a -> s {logDeliveryConfigurations = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
replicationGroup_configurationEndpoint :: Lens.Lens' ReplicationGroup (Prelude.Maybe Endpoint)
replicationGroup_configurationEndpoint = Lens.lens (\ReplicationGroup' {configurationEndpoint} -> configurationEndpoint) (\s@ReplicationGroup' {} a -> s {configurationEndpoint = a} :: ReplicationGroup)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
replicationGroup_authTokenEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_authTokenEnabled = Lens.lens (\ReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@ReplicationGroup' {} a -> s {authTokenEnabled = a} :: ReplicationGroup)

-- | The names of all the cache clusters that are part of this replication
-- group.
replicationGroup_memberClusters :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClusters = Lens.lens (\ReplicationGroup' {memberClusters} -> memberClusters) (\s@ReplicationGroup' {} a -> s {memberClusters = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the KMS key used to encrypt the disk in the cluster.
replicationGroup_kmsKeyId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_kmsKeyId = Lens.lens (\ReplicationGroup' {kmsKeyId} -> kmsKeyId) (\s@ReplicationGroup' {} a -> s {kmsKeyId = a} :: ReplicationGroup)

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
replicationGroup_multiAZ :: Lens.Lens' ReplicationGroup (Prelude.Maybe MultiAZStatus)
replicationGroup_multiAZ = Lens.lens (\ReplicationGroup' {multiAZ} -> multiAZ) (\s@ReplicationGroup' {} a -> s {multiAZ = a} :: ReplicationGroup)

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
replicationGroup_snapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Int)
replicationGroup_snapshotRetentionLimit = Lens.lens (\ReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ReplicationGroup)

-- | The user supplied description of the replication group.
replicationGroup_description :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_description = Lens.lens (\ReplicationGroup' {description} -> description) (\s@ReplicationGroup' {} a -> s {description = a} :: ReplicationGroup)

-- | The identifier for the replication group.
replicationGroup_replicationGroupId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_replicationGroupId = Lens.lens (\ReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ReplicationGroup' {} a -> s {replicationGroupId = a} :: ReplicationGroup)

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
replicationGroup_pendingModifiedValues :: Lens.Lens' ReplicationGroup (Prelude.Maybe ReplicationGroupPendingModifiedValues)
replicationGroup_pendingModifiedValues = Lens.lens (\ReplicationGroup' {pendingModifiedValues} -> pendingModifiedValues) (\s@ReplicationGroup' {} a -> s {pendingModifiedValues = a} :: ReplicationGroup)

-- | The name of the Global datastore and role of this replication group in
-- the Global datastore.
replicationGroup_globalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Prelude.Maybe GlobalReplicationGroupInfo)
replicationGroup_globalReplicationGroupInfo = Lens.lens (\ReplicationGroup' {globalReplicationGroupInfo} -> globalReplicationGroupInfo) (\s@ReplicationGroup' {} a -> s {globalReplicationGroupInfo = a} :: ReplicationGroup)

-- | The outpost ARNs of the replication group\'s member clusters.
replicationGroup_memberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClustersOutpostArns = Lens.lens (\ReplicationGroup' {memberClustersOutpostArns} -> memberClustersOutpostArns) (\s@ReplicationGroup' {} a -> s {memberClustersOutpostArns = a} :: ReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroup_automaticFailover :: Lens.Lens' ReplicationGroup (Prelude.Maybe AutomaticFailoverStatus)
replicationGroup_automaticFailover = Lens.lens (\ReplicationGroup' {automaticFailover} -> automaticFailover) (\s@ReplicationGroup' {} a -> s {automaticFailover = a} :: ReplicationGroup)

instance Core.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Prelude.<$> (x Core..@? "AuthTokenLastModifiedDate")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "CacheNodeType")
      Prelude.<*> ( x Core..@? "NodeGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "NodeGroup")
                  )
      Prelude.<*> (x Core..@? "SnapshottingClusterId")
      Prelude.<*> (x Core..@? "ClusterEnabled")
      Prelude.<*> (x Core..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Core..@? "ReplicationGroupCreateTime")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "TransitEncryptionEnabled")
      Prelude.<*> ( x Core..@? "UserGroupIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "SnapshotWindow")
      Prelude.<*> ( x Core..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "LogDeliveryConfiguration")
                  )
      Prelude.<*> (x Core..@? "ConfigurationEndpoint")
      Prelude.<*> (x Core..@? "AuthTokenEnabled")
      Prelude.<*> ( x Core..@? "MemberClusters" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ClusterId")
                  )
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "MultiAZ")
      Prelude.<*> (x Core..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "ReplicationGroupId")
      Prelude.<*> (x Core..@? "PendingModifiedValues")
      Prelude.<*> (x Core..@? "GlobalReplicationGroupInfo")
      Prelude.<*> ( x Core..@? "MemberClustersOutpostArns"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "ReplicationGroupOutpostArn")
                  )
      Prelude.<*> (x Core..@? "AutomaticFailover")

instance Prelude.Hashable ReplicationGroup where
  hashWithSalt _salt ReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` authTokenLastModifiedDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` nodeGroups
      `Prelude.hashWithSalt` snapshottingClusterId
      `Prelude.hashWithSalt` clusterEnabled
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` replicationGroupCreateTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` userGroupIds
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` configurationEndpoint
      `Prelude.hashWithSalt` authTokenEnabled
      `Prelude.hashWithSalt` memberClusters
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` globalReplicationGroupInfo
      `Prelude.hashWithSalt` memberClustersOutpostArns
      `Prelude.hashWithSalt` automaticFailover

instance Prelude.NFData ReplicationGroup where
  rnf ReplicationGroup' {..} =
    Prelude.rnf authTokenLastModifiedDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf nodeGroups
      `Prelude.seq` Prelude.rnf snapshottingClusterId
      `Prelude.seq` Prelude.rnf clusterEnabled
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf replicationGroupCreateTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf userGroupIds
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf configurationEndpoint
      `Prelude.seq` Prelude.rnf authTokenEnabled
      `Prelude.seq` Prelude.rnf memberClusters
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf
        replicationGroupId
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        globalReplicationGroupInfo
      `Prelude.seq` Prelude.rnf
        memberClustersOutpostArns
      `Prelude.seq` Prelude.rnf
        automaticFailover
