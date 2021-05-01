{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.MultiAZStatus
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains all of the attributes of a specific Redis replication group.
--
-- /See:/ 'newReplicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { -- | A flag indicating whether or not this replication group is cluster
    -- enabled; i.e., whether its data can be partitioned across multiple
    -- shards (API\/CLI: node groups).
    --
    -- Valid values: @true@ | @false@
    clusterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The current state of this replication group - @creating@, @available@,
    -- @modifying@, @deleting@, @create-failed@, @snapshotting@.
    status :: Prelude.Maybe Prelude.Text,
    -- | A list of node groups in this replication group. For Redis (cluster mode
    -- disabled) replication groups, this is a single-element list. For Redis
    -- (cluster mode enabled) replication groups, the list contains an entry
    -- for each node group (shard).
    nodeGroups :: Prelude.Maybe [NodeGroup],
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus,
    -- | The outpost ARNs of the replication group\'s member clusters.
    memberClustersOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | The names of all the cache clusters that are part of this replication
    -- group.
    memberClusters :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Global Datastore and role of this replication group in
    -- the Global Datastore.
    globalReplicationGroupInfo :: Prelude.Maybe GlobalReplicationGroupInfo,
    -- | The identifier for the replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The list of user group IDs that have access to the replication group.
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
    -- | The ARN (Amazon Resource Name) of the replication group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
    multiAZ :: Prelude.Maybe MultiAZStatus,
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
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group.
    snapshottingClusterId :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for each node in
    -- the replication group.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Prelude.Maybe Prelude.ISO8601,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The user supplied description of the replication group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A group of settings to be applied to the replication group, either
    -- immediately or during the next maintenance window.
    pendingModifiedValues :: Prelude.Maybe ReplicationGroupPendingModifiedValues,
    -- | The configuration endpoint for this replication group. Use the
    -- configuration endpoint to connect to this replication group.
    configurationEndpoint :: Prelude.Maybe Endpoint,
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
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { clusterEnabled = Prelude.Nothing,
      status = Prelude.Nothing,
      nodeGroups = Prelude.Nothing,
      automaticFailover = Prelude.Nothing,
      memberClustersOutpostArns = Prelude.Nothing,
      memberClusters = Prelude.Nothing,
      globalReplicationGroupInfo = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      userGroupIds = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      arn = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshottingClusterId = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      authTokenLastModifiedDate = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      description = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      configurationEndpoint = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing
    }

-- | A flag indicating whether or not this replication group is cluster
-- enabled; i.e., whether its data can be partitioned across multiple
-- shards (API\/CLI: node groups).
--
-- Valid values: @true@ | @false@
replicationGroup_clusterEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_clusterEnabled = Lens.lens (\ReplicationGroup' {clusterEnabled} -> clusterEnabled) (\s@ReplicationGroup' {} a -> s {clusterEnabled = a} :: ReplicationGroup)

-- | The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@, @create-failed@, @snapshotting@.
replicationGroup_status :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_status = Lens.lens (\ReplicationGroup' {status} -> status) (\s@ReplicationGroup' {} a -> s {status = a} :: ReplicationGroup)

-- | A list of node groups in this replication group. For Redis (cluster mode
-- disabled) replication groups, this is a single-element list. For Redis
-- (cluster mode enabled) replication groups, the list contains an entry
-- for each node group (shard).
replicationGroup_nodeGroups :: Lens.Lens' ReplicationGroup (Prelude.Maybe [NodeGroup])
replicationGroup_nodeGroups = Lens.lens (\ReplicationGroup' {nodeGroups} -> nodeGroups) (\s@ReplicationGroup' {} a -> s {nodeGroups = a} :: ReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroup_automaticFailover :: Lens.Lens' ReplicationGroup (Prelude.Maybe AutomaticFailoverStatus)
replicationGroup_automaticFailover = Lens.lens (\ReplicationGroup' {automaticFailover} -> automaticFailover) (\s@ReplicationGroup' {} a -> s {automaticFailover = a} :: ReplicationGroup)

-- | The outpost ARNs of the replication group\'s member clusters.
replicationGroup_memberClustersOutpostArns :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClustersOutpostArns = Lens.lens (\ReplicationGroup' {memberClustersOutpostArns} -> memberClustersOutpostArns) (\s@ReplicationGroup' {} a -> s {memberClustersOutpostArns = a} :: ReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The names of all the cache clusters that are part of this replication
-- group.
replicationGroup_memberClusters :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_memberClusters = Lens.lens (\ReplicationGroup' {memberClusters} -> memberClusters) (\s@ReplicationGroup' {} a -> s {memberClusters = a} :: ReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Global Datastore and role of this replication group in
-- the Global Datastore.
replicationGroup_globalReplicationGroupInfo :: Lens.Lens' ReplicationGroup (Prelude.Maybe GlobalReplicationGroupInfo)
replicationGroup_globalReplicationGroupInfo = Lens.lens (\ReplicationGroup' {globalReplicationGroupInfo} -> globalReplicationGroupInfo) (\s@ReplicationGroup' {} a -> s {globalReplicationGroupInfo = a} :: ReplicationGroup)

-- | The identifier for the replication group.
replicationGroup_replicationGroupId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_replicationGroupId = Lens.lens (\ReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ReplicationGroup' {} a -> s {replicationGroupId = a} :: ReplicationGroup)

-- | The list of user group IDs that have access to the replication group.
replicationGroup_userGroupIds :: Lens.Lens' ReplicationGroup (Prelude.Maybe [Prelude.Text])
replicationGroup_userGroupIds = Lens.lens (\ReplicationGroup' {userGroupIds} -> userGroupIds) (\s@ReplicationGroup' {} a -> s {userGroupIds = a} :: ReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The ARN (Amazon Resource Name) of the replication group.
replicationGroup_arn :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_arn = Lens.lens (\ReplicationGroup' {arn} -> arn) (\s@ReplicationGroup' {} a -> s {arn = a} :: ReplicationGroup)

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
replicationGroup_snapshotRetentionLimit :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Int)
replicationGroup_snapshotRetentionLimit = Lens.lens (\ReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ReplicationGroup)

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>
replicationGroup_multiAZ :: Lens.Lens' ReplicationGroup (Prelude.Maybe MultiAZStatus)
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
replicationGroup_atRestEncryptionEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_atRestEncryptionEnabled = Lens.lens (\ReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: ReplicationGroup)

-- | The ID of the KMS key used to encrypt the disk in the cluster.
replicationGroup_kmsKeyId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_kmsKeyId = Lens.lens (\ReplicationGroup' {kmsKeyId} -> kmsKeyId) (\s@ReplicationGroup' {} a -> s {kmsKeyId = a} :: ReplicationGroup)

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group.
replicationGroup_snapshottingClusterId :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_snapshottingClusterId = Lens.lens (\ReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ReplicationGroup)

-- | The name of the compute and memory capacity node type for each node in
-- the replication group.
replicationGroup_cacheNodeType :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_cacheNodeType = Lens.lens (\ReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ReplicationGroup' {} a -> s {cacheNodeType = a} :: ReplicationGroup)

-- | The date the auth token was last modified
replicationGroup_authTokenLastModifiedDate :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.UTCTime)
replicationGroup_authTokenLastModifiedDate = Lens.lens (\ReplicationGroup' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@ReplicationGroup' {} a -> s {authTokenLastModifiedDate = a} :: ReplicationGroup) Prelude.. Lens.mapping Prelude._Time

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
replicationGroup_authTokenEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_authTokenEnabled = Lens.lens (\ReplicationGroup' {authTokenEnabled} -> authTokenEnabled) (\s@ReplicationGroup' {} a -> s {authTokenEnabled = a} :: ReplicationGroup)

-- | The user supplied description of the replication group.
replicationGroup_description :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Text)
replicationGroup_description = Lens.lens (\ReplicationGroup' {description} -> description) (\s@ReplicationGroup' {} a -> s {description = a} :: ReplicationGroup)

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
replicationGroup_pendingModifiedValues :: Lens.Lens' ReplicationGroup (Prelude.Maybe ReplicationGroupPendingModifiedValues)
replicationGroup_pendingModifiedValues = Lens.lens (\ReplicationGroup' {pendingModifiedValues} -> pendingModifiedValues) (\s@ReplicationGroup' {} a -> s {pendingModifiedValues = a} :: ReplicationGroup)

-- | The configuration endpoint for this replication group. Use the
-- configuration endpoint to connect to this replication group.
replicationGroup_configurationEndpoint :: Lens.Lens' ReplicationGroup (Prelude.Maybe Endpoint)
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
replicationGroup_transitEncryptionEnabled :: Lens.Lens' ReplicationGroup (Prelude.Maybe Prelude.Bool)
replicationGroup_transitEncryptionEnabled = Lens.lens (\ReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: ReplicationGroup)

instance Prelude.FromXML ReplicationGroup where
  parseXML x =
    ReplicationGroup'
      Prelude.<$> (x Prelude..@? "ClusterEnabled")
      Prelude.<*> (x Prelude..@? "Status")
      Prelude.<*> ( x Prelude..@? "NodeGroups"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "NodeGroup")
                  )
      Prelude.<*> (x Prelude..@? "AutomaticFailover")
      Prelude.<*> ( x Prelude..@? "MemberClustersOutpostArns"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "ReplicationGroupOutpostArn")
                  )
      Prelude.<*> ( x Prelude..@? "MemberClusters"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "ClusterId")
                  )
      Prelude.<*> (x Prelude..@? "GlobalReplicationGroupInfo")
      Prelude.<*> (x Prelude..@? "ReplicationGroupId")
      Prelude.<*> ( x Prelude..@? "UserGroupIds"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "SnapshotWindow")
      Prelude.<*> (x Prelude..@? "ARN")
      Prelude.<*> (x Prelude..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Prelude..@? "MultiAZ")
      Prelude.<*> (x Prelude..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Prelude..@? "KmsKeyId")
      Prelude.<*> (x Prelude..@? "SnapshottingClusterId")
      Prelude.<*> (x Prelude..@? "CacheNodeType")
      Prelude.<*> (x Prelude..@? "AuthTokenLastModifiedDate")
      Prelude.<*> (x Prelude..@? "AuthTokenEnabled")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "PendingModifiedValues")
      Prelude.<*> (x Prelude..@? "ConfigurationEndpoint")
      Prelude.<*> (x Prelude..@? "TransitEncryptionEnabled")

instance Prelude.Hashable ReplicationGroup

instance Prelude.NFData ReplicationGroup
