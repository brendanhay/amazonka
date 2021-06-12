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
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.MultiAZStatus
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import qualified Network.AWS.Lens as Lens

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'newReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { -- | A flag indicating whether or not this replication group is cluster
    -- enabled; i.e., whether its data can be partitioned across multiple
    -- shards (API\/CLI: node groups).
    --
    -- Valid values: @true@ | @false@
    clusterEnabled :: Core.Maybe Core.Bool,
    -- | The current state of this replication group - @creating@, @available@,
    -- @modifying@, @deleting@, @create-failed@, @snapshotting@.
    status :: Core.Maybe Core.Text,
    -- | A list of node groups in this replication group. For Redis (cluster mode
    -- disabled) replication groups, this is a single-element list. For Redis
    -- (cluster mode enabled) replication groups, the list contains an entry
    -- for each node group (shard).
    nodeGroups :: Core.Maybe [NodeGroup],
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailover :: Core.Maybe AutomaticFailoverStatus,
    -- | The outpost ARNs of the replication group\'s member clusters.
    memberClustersOutpostArns :: Core.Maybe [Core.Text],
    -- | The names of all the cache clusters that are part of this replication
    -- group.
    memberClusters :: Core.Maybe [Core.Text],
    -- | The name of the Global Datastore and role of this replication group in
    -- the Global Datastore.
    globalReplicationGroupInfo :: Core.Maybe GlobalReplicationGroupInfo,
    -- | The identifier for the replication group.
    replicationGroupId :: Core.Maybe Core.Text,
    -- | The list of user group IDs that have access to the replication group.
    userGroupIds :: Core.Maybe [Core.Text],
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    snapshotWindow :: Core.Maybe Core.Text,
    -- | The ARN (Amazon Resource Name) of the replication group.
    arn :: Core.Maybe Core.Text,
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
    multiAZ :: Core.Maybe MultiAZStatus,
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
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group.
    snapshottingClusterId :: Core.Maybe Core.Text,
    -- | The name of the compute and memory capacity node type for each node in
    -- the replication group.
    cacheNodeType :: Core.Maybe Core.Text,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Core.Maybe Core.ISO8601,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Core.Maybe Core.Bool,
    -- | The user supplied description of the replication group.
    description :: Core.Maybe Core.Text,
    -- | A group of settings to be applied to the replication group, either
    -- immediately or during the next maintenance window.
    pendingModifiedValues :: Core.Maybe ReplicationGroupPendingModifiedValues,
    -- | The configuration endpoint for this replication group. Use the
    -- configuration endpoint to connect to this replication group.
    configurationEndpoint :: Core.Maybe Endpoint,
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
    transitEncryptionEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterEnabled', 'replicationGroup_clusterEnabled' - A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
--
-- 'status', 'replicationGroup_status' - The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
--
-- 'nodeGroups', 'replicationGroup_nodeGroups' - A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
--
-- 'automaticFailover', 'replicationGroup_automaticFailover' - Indicates the status of automatic failover for this Redis replication
-- group.
--
-- 'memberClustersOutpostArns', 'replicationGroup_memberClustersOutpostArns' - The outpost ARNs of the replication group\'s member clusters.
--
-- 'memberClusters', 'replicationGroup_memberClusters' - The names of all the cache clusters that are part of this replication
-- group.
--
-- 'globalReplicationGroupInfo', 'replicationGroup_globalReplicationGroupInfo' - The name of the Global Datastore and role of this replication group in
-- the Global Datastore.
--
-- 'replicationGroupId', 'replicationGroup_replicationGroupId' - The identifier for the replication group.
--
-- 'userGroupIds', 'replicationGroup_userGroupIds' - The list of user group IDs that have access to the replication group.
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
-- 'arn', 'replicationGroup_arn' - The ARN (Amazon Resource Name) of the replication group.
--
-- 'snapshotRetentionLimit', 'replicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
--
-- 'multiAZ', 'replicationGroup_multiAZ' - A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
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
-- 'kmsKeyId', 'replicationGroup_kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
--
-- 'snapshottingClusterId', 'replicationGroup_snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the
-- replication group.
--
-- 'cacheNodeType', 'replicationGroup_cacheNodeType' - The name of the compute and memory capacity node type for each node in
-- the replication group.
--
-- 'authTokenLastModifiedDate', 'replicationGroup_authTokenLastModifiedDate' - The date the auth token was last modified
--
-- 'authTokenEnabled', 'replicationGroup_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'description', 'replicationGroup_description' - The user supplied description of the replication group.
--
-- 'pendingModifiedValues', 'replicationGroup_pendingModifiedValues' - A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
--
-- 'configurationEndpoint', 'replicationGroup_configurationEndpoint' - The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
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
newReplicationGroup ::
  ReplicationGroup
newReplicationGroup =
  ReplicationGroup'
    { clusterEnabled = Core.Nothing,
      status = Core.Nothing,
      nodeGroups = Core.Nothing,
      automaticFailover = Core.Nothing,
      memberClustersOutpostArns = Core.Nothing,
      memberClusters = Core.Nothing,
      globalReplicationGroupInfo = Core.Nothing,
      replicationGroupId = Core.Nothing,
      userGroupIds = Core.Nothing,
      snapshotWindow = Core.Nothing,
      arn = Core.Nothing,
      snapshotRetentionLimit = Core.Nothing,
      multiAZ = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      kmsKeyId = Core.Nothing,
      snapshottingClusterId = Core.Nothing,
      cacheNodeType = Core.Nothing,
      authTokenLastModifiedDate = Core.Nothing,
      authTokenEnabled = Core.Nothing,
      description = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      configurationEndpoint = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing
    }

-- | A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
replicationGroup_clusterEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
replicationGroup_clusterEnabled = Lens.lens (\ReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@ReplicationGroup' {} a -> s {clusterEnabled = a} :: ReplicationGroup)

-- | The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
replicationGroup_status :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_status = Lens.lens (\ReplicationGroup' {status} -> status) (\s@ReplicationGroup' {} a -> s {status = a} :: ReplicationGroup)

-- | A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
replicationGroup_nodeGroups :: Lens.Lens' ReplicationGroup (Core.Maybe [NodeGroup])
replicationGroup_nodeGroups = Lens.lens (\ReplicationGroup' {nodeGroups} -> nodeGroups) (\s@ReplicationGroup' {} a -> s {nodeGroups = a} :: ReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroup_automaticFailover :: Lens.Lens' ReplicationGroup (Core.Maybe AutomaticFailoverStatus)
replicationGroup_automaticFailover = Lens.lens (\ReplicationGroup' {automaticFailover} -> automaticFailover) (\s@ReplicationGroup' {} a -> s {automaticFailover = a} :: ReplicationGroup)

-- | The outpost ARNs of the replication group\'s member clusters.
replicationGroup_memberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Core.Maybe [Core.Text])
replicationGroup_memberClustersOutpostArns = Lens.lens (\ReplicationGroup' {memberClustersOutpostArns} -> memberClustersOutpostArns) (\s@ReplicationGroup' {} a -> s {memberClustersOutpostArns = a} :: ReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | The names of all the cache clusters that are part of this replication
-- group.
replicationGroup_memberClusters :: Lens.Lens' ReplicationGroup (Core.Maybe [Core.Text])
replicationGroup_memberClusters = Lens.lens (\ReplicationGroup' {memberClusters} -> memberClusters) (\s@ReplicationGroup' {} a -> s {memberClusters = a} :: ReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the Global Datastore and role of this replication group in
-- the Global Datastore.
replicationGroup_globalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Core.Maybe GlobalReplicationGroupInfo)
replicationGroup_globalReplicationGroupInfo = Lens.lens (\ReplicationGroup' {globalReplicationGroupInfo} -> globalReplicationGroupInfo) (\s@ReplicationGroup' {} a -> s {globalReplicationGroupInfo = a} :: ReplicationGroup)

-- | The identifier for the replication group.
replicationGroup_replicationGroupId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_replicationGroupId = Lens.lens (\ReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ReplicationGroup' {} a -> s {replicationGroupId = a} :: ReplicationGroup)

-- | The list of user group IDs that have access to the replication group.
replicationGroup_userGroupIds :: Lens.Lens' ReplicationGroup (Core.Maybe [Core.Text])
replicationGroup_userGroupIds = Lens.lens (\ReplicationGroup' {userGroupIds} -> userGroupIds) (\s@ReplicationGroup' {} a -> s {userGroupIds = a} :: ReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
replicationGroup_snapshotWindow :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_snapshotWindow = Lens.lens (\ReplicationGroup' {snapshotWindow} -> snapshotWindow) (\s@ReplicationGroup' {} a -> s {snapshotWindow = a} :: ReplicationGroup)

-- | The ARN (Amazon Resource Name) of the replication group.
replicationGroup_arn :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_arn = Lens.lens (\ReplicationGroup' {arn} -> arn) (\s@ReplicationGroup' {} a -> s {arn = a} :: ReplicationGroup)

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
replicationGroup_snapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Int)
replicationGroup_snapshotRetentionLimit = Lens.lens (\ReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ReplicationGroup)

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
replicationGroup_multiAZ :: Lens.Lens' ReplicationGroup (Core.Maybe MultiAZStatus)
replicationGroup_multiAZ = Lens.lens (\ReplicationGroup' {multiAZ} -> multiAZ) (\s@ReplicationGroup' {} a -> s {multiAZ = a} :: ReplicationGroup)

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
replicationGroup_atRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
replicationGroup_atRestEncryptionEnabled = Lens.lens (\ReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: ReplicationGroup)

-- | The ID of the KMS key used to encrypt the disk in the cluster.
replicationGroup_kmsKeyId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_kmsKeyId = Lens.lens (\ReplicationGroup' {kmsKeyId} -> kmsKeyId) (\s@ReplicationGroup' {} a -> s {kmsKeyId = a} :: ReplicationGroup)

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group.
replicationGroup_snapshottingClusterId :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_snapshottingClusterId = Lens.lens (\ReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ReplicationGroup)

-- | The name of the compute and memory capacity node type for each node in
-- the replication group.
replicationGroup_cacheNodeType :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_cacheNodeType = Lens.lens (\ReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ReplicationGroup' {} a -> s {cacheNodeType = a} :: ReplicationGroup)

-- | The date the auth token was last modified
replicationGroup_authTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Core.Maybe Core.UTCTime)
replicationGroup_authTokenLastModifiedDate = Lens.lens (\ReplicationGroup' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@ReplicationGroup' {} a -> s {authTokenLastModifiedDate = a} :: ReplicationGroup) Core.. Lens.mapping Core._Time

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
replicationGroup_authTokenEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
replicationGroup_authTokenEnabled = Lens.lens (\ReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@ReplicationGroup' {} a -> s {authTokenEnabled = a} :: ReplicationGroup)

-- | The user supplied description of the replication group.
replicationGroup_description :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Text)
replicationGroup_description = Lens.lens (\ReplicationGroup' {description} -> description) (\s@ReplicationGroup' {} a -> s {description = a} :: ReplicationGroup)

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
replicationGroup_pendingModifiedValues :: Lens.Lens' ReplicationGroup (Core.Maybe ReplicationGroupPendingModifiedValues)
replicationGroup_pendingModifiedValues = Lens.lens (\ReplicationGroup' {pendingModifiedValues} -> pendingModifiedValues) (\s@ReplicationGroup' {} a -> s {pendingModifiedValues = a} :: ReplicationGroup)

-- | The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
replicationGroup_configurationEndpoint :: Lens.Lens' ReplicationGroup (Core.Maybe Endpoint)
replicationGroup_configurationEndpoint = Lens.lens (\ReplicationGroup' {configurationEndpoint} -> configurationEndpoint) (\s@ReplicationGroup' {} a -> s {configurationEndpoint = a} :: ReplicationGroup)

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
replicationGroup_transitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Core.Maybe Core.Bool)
replicationGroup_transitEncryptionEnabled = Lens.lens (\ReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: ReplicationGroup)

instance Core.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Core.<$> (x Core..@? "ClusterEnabled")
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x Core..@? "NodeGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "NodeGroup")
               )
      Core.<*> (x Core..@? "AutomaticFailover")
      Core.<*> ( x Core..@? "MemberClustersOutpostArns"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "ReplicationGroupOutpostArn")
               )
      Core.<*> ( x Core..@? "MemberClusters" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "ClusterId")
               )
      Core.<*> (x Core..@? "GlobalReplicationGroupInfo")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> ( x Core..@? "UserGroupIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "SnapshotWindow")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "SnapshotRetentionLimit")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "AtRestEncryptionEnabled")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "SnapshottingClusterId")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "AuthTokenLastModifiedDate")
      Core.<*> (x Core..@? "AuthTokenEnabled")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "ConfigurationEndpoint")
      Core.<*> (x Core..@? "TransitEncryptionEnabled")

instance Core.Hashable ReplicationGroup

instance Core.NFData ReplicationGroup
