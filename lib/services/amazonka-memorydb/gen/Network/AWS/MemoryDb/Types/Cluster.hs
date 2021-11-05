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
-- Module      : Amazonka.MemoryDb.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types.AZStatus
import Amazonka.MemoryDb.Types.ClusterPendingUpdates
import Amazonka.MemoryDb.Types.Endpoint
import Amazonka.MemoryDb.Types.SecurityGroupMembership
import Amazonka.MemoryDb.Types.Shard
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The Redis engine version used by the cluster
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster. For example, Available, Updating, Creating.
    status :: Prelude.Maybe Prelude.Text,
    -- | When set to true, the cluster will automatically receive minor engine
    -- version upgrades after launch.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the SNS notification topic
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | A list of security groups used by the cluster
    securityGroups :: Prelude.Maybe [SecurityGroupMembership],
    -- | Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
    -- (singleaz).
    availabilityMode :: Prelude.Maybe AZStatus,
    -- | The status of the parameter group used by the cluster, for example
    -- \'active\' or \'applying\'.
    parameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A group of settings that are currently being applied.
    pendingUpdates :: Prelude.Maybe ClusterPendingUpdates,
    -- | The number of shards in the cluster
    numberOfShards :: Prelude.Maybe Prelude.Int,
    -- | The daily time range (in UTC) during which MemoryDB begins taking a
    -- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
    -- this parameter, MemoryDB automatically chooses an appropriate time
    -- range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group used by the cluster
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A flag to indicate if In-transit encryption is enabled
    tLSEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    maintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the cluster
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of shards that are members of the cluster.
    shards :: Prelude.Maybe [Shard],
    -- | The user-supplied name of the cluster. This identifier is a unique key
    -- that identifies a cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Redis engine patch version used by the cluster
    enginePatchVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which MemoryDB retains automatic snapshots before
    -- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
    -- snapshot that was taken today is retained for 5 days before being
    -- deleted.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The cluster\'s node type
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The SNS topic must be in Active status to receive notifications
    snsTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | A description of the cluster
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the Access Control List associated with this cluster.
    aCLName :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s configuration endpoint
    clusterEndpoint :: Prelude.Maybe Endpoint,
    -- | The name of the parameter group used by the cluster
    parameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'cluster_engineVersion' - The Redis engine version used by the cluster
--
-- 'status', 'cluster_status' - The status of the cluster. For example, Available, Updating, Creating.
--
-- 'autoMinorVersionUpgrade', 'cluster_autoMinorVersionUpgrade' - When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
--
-- 'snsTopicArn', 'cluster_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS notification topic
--
-- 'securityGroups', 'cluster_securityGroups' - A list of security groups used by the cluster
--
-- 'availabilityMode', 'cluster_availabilityMode' - Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
-- (singleaz).
--
-- 'parameterGroupStatus', 'cluster_parameterGroupStatus' - The status of the parameter group used by the cluster, for example
-- \'active\' or \'applying\'.
--
-- 'arn', 'cluster_arn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'pendingUpdates', 'cluster_pendingUpdates' - A group of settings that are currently being applied.
--
-- 'numberOfShards', 'cluster_numberOfShards' - The number of shards in the cluster
--
-- 'snapshotWindow', 'cluster_snapshotWindow' - The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
-- this parameter, MemoryDB automatically chooses an appropriate time
-- range.
--
-- 'subnetGroupName', 'cluster_subnetGroupName' - The name of the subnet group used by the cluster
--
-- 'tLSEnabled', 'cluster_tLSEnabled' - A flag to indicate if In-transit encryption is enabled
--
-- 'maintenanceWindow', 'cluster_maintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- 'kmsKeyId', 'cluster_kmsKeyId' - The ID of the KMS key used to encrypt the cluster
--
-- 'shards', 'cluster_shards' - A list of shards that are members of the cluster.
--
-- 'name', 'cluster_name' - The user-supplied name of the cluster. This identifier is a unique key
-- that identifies a cluster.
--
-- 'enginePatchVersion', 'cluster_enginePatchVersion' - The Redis engine patch version used by the cluster
--
-- 'snapshotRetentionLimit', 'cluster_snapshotRetentionLimit' - The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
--
-- 'nodeType', 'cluster_nodeType' - The cluster\'s node type
--
-- 'snsTopicStatus', 'cluster_snsTopicStatus' - The SNS topic must be in Active status to receive notifications
--
-- 'description', 'cluster_description' - A description of the cluster
--
-- 'aCLName', 'cluster_aCLName' - The name of the Access Control List associated with this cluster.
--
-- 'clusterEndpoint', 'cluster_clusterEndpoint' - The cluster\'s configuration endpoint
--
-- 'parameterGroupName', 'cluster_parameterGroupName' - The name of the parameter group used by the cluster
newCluster ::
  Cluster
newCluster =
  Cluster'
    { engineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      availabilityMode = Prelude.Nothing,
      parameterGroupStatus = Prelude.Nothing,
      arn = Prelude.Nothing,
      pendingUpdates = Prelude.Nothing,
      numberOfShards = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing,
      tLSEnabled = Prelude.Nothing,
      maintenanceWindow = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      shards = Prelude.Nothing,
      name = Prelude.Nothing,
      enginePatchVersion = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      snsTopicStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      aCLName = Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing
    }

-- | The Redis engine version used by the cluster
cluster_engineVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_engineVersion = Lens.lens (\Cluster' {engineVersion} -> engineVersion) (\s@Cluster' {} a -> s {engineVersion = a} :: Cluster)

-- | The status of the cluster. For example, Available, Updating, Creating.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
cluster_autoMinorVersionUpgrade :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_autoMinorVersionUpgrade = Lens.lens (\Cluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@Cluster' {} a -> s {autoMinorVersionUpgrade = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the SNS notification topic
cluster_snsTopicArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snsTopicArn = Lens.lens (\Cluster' {snsTopicArn} -> snsTopicArn) (\s@Cluster' {} a -> s {snsTopicArn = a} :: Cluster)

-- | A list of security groups used by the cluster
cluster_securityGroups :: Lens.Lens' Cluster (Prelude.Maybe [SecurityGroupMembership])
cluster_securityGroups = Lens.lens (\Cluster' {securityGroups} -> securityGroups) (\s@Cluster' {} a -> s {securityGroups = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
-- (singleaz).
cluster_availabilityMode :: Lens.Lens' Cluster (Prelude.Maybe AZStatus)
cluster_availabilityMode = Lens.lens (\Cluster' {availabilityMode} -> availabilityMode) (\s@Cluster' {} a -> s {availabilityMode = a} :: Cluster)

-- | The status of the parameter group used by the cluster, for example
-- \'active\' or \'applying\'.
cluster_parameterGroupStatus :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_parameterGroupStatus = Lens.lens (\Cluster' {parameterGroupStatus} -> parameterGroupStatus) (\s@Cluster' {} a -> s {parameterGroupStatus = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the cluster.
cluster_arn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_arn = Lens.lens (\Cluster' {arn} -> arn) (\s@Cluster' {} a -> s {arn = a} :: Cluster)

-- | A group of settings that are currently being applied.
cluster_pendingUpdates :: Lens.Lens' Cluster (Prelude.Maybe ClusterPendingUpdates)
cluster_pendingUpdates = Lens.lens (\Cluster' {pendingUpdates} -> pendingUpdates) (\s@Cluster' {} a -> s {pendingUpdates = a} :: Cluster)

-- | The number of shards in the cluster
cluster_numberOfShards :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_numberOfShards = Lens.lens (\Cluster' {numberOfShards} -> numberOfShards) (\s@Cluster' {} a -> s {numberOfShards = a} :: Cluster)

-- | The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
-- this parameter, MemoryDB automatically chooses an appropriate time
-- range.
cluster_snapshotWindow :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snapshotWindow = Lens.lens (\Cluster' {snapshotWindow} -> snapshotWindow) (\s@Cluster' {} a -> s {snapshotWindow = a} :: Cluster)

-- | The name of the subnet group used by the cluster
cluster_subnetGroupName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_subnetGroupName = Lens.lens (\Cluster' {subnetGroupName} -> subnetGroupName) (\s@Cluster' {} a -> s {subnetGroupName = a} :: Cluster)

-- | A flag to indicate if In-transit encryption is enabled
cluster_tLSEnabled :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_tLSEnabled = Lens.lens (\Cluster' {tLSEnabled} -> tLSEnabled) (\s@Cluster' {} a -> s {tLSEnabled = a} :: Cluster)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
cluster_maintenanceWindow :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_maintenanceWindow = Lens.lens (\Cluster' {maintenanceWindow} -> maintenanceWindow) (\s@Cluster' {} a -> s {maintenanceWindow = a} :: Cluster)

-- | The ID of the KMS key used to encrypt the cluster
cluster_kmsKeyId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_kmsKeyId = Lens.lens (\Cluster' {kmsKeyId} -> kmsKeyId) (\s@Cluster' {} a -> s {kmsKeyId = a} :: Cluster)

-- | A list of shards that are members of the cluster.
cluster_shards :: Lens.Lens' Cluster (Prelude.Maybe [Shard])
cluster_shards = Lens.lens (\Cluster' {shards} -> shards) (\s@Cluster' {} a -> s {shards = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The user-supplied name of the cluster. This identifier is a unique key
-- that identifies a cluster.
cluster_name :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | The Redis engine patch version used by the cluster
cluster_enginePatchVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_enginePatchVersion = Lens.lens (\Cluster' {enginePatchVersion} -> enginePatchVersion) (\s@Cluster' {} a -> s {enginePatchVersion = a} :: Cluster)

-- | The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
cluster_snapshotRetentionLimit :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_snapshotRetentionLimit = Lens.lens (\Cluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@Cluster' {} a -> s {snapshotRetentionLimit = a} :: Cluster)

-- | The cluster\'s node type
cluster_nodeType :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_nodeType = Lens.lens (\Cluster' {nodeType} -> nodeType) (\s@Cluster' {} a -> s {nodeType = a} :: Cluster)

-- | The SNS topic must be in Active status to receive notifications
cluster_snsTopicStatus :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snsTopicStatus = Lens.lens (\Cluster' {snsTopicStatus} -> snsTopicStatus) (\s@Cluster' {} a -> s {snsTopicStatus = a} :: Cluster)

-- | A description of the cluster
cluster_description :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_description = Lens.lens (\Cluster' {description} -> description) (\s@Cluster' {} a -> s {description = a} :: Cluster)

-- | The name of the Access Control List associated with this cluster.
cluster_aCLName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_aCLName = Lens.lens (\Cluster' {aCLName} -> aCLName) (\s@Cluster' {} a -> s {aCLName = a} :: Cluster)

-- | The cluster\'s configuration endpoint
cluster_clusterEndpoint :: Lens.Lens' Cluster (Prelude.Maybe Endpoint)
cluster_clusterEndpoint = Lens.lens (\Cluster' {clusterEndpoint} -> clusterEndpoint) (\s@Cluster' {} a -> s {clusterEndpoint = a} :: Cluster)

-- | The name of the parameter group used by the cluster
cluster_parameterGroupName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_parameterGroupName = Lens.lens (\Cluster' {parameterGroupName} -> parameterGroupName) (\s@Cluster' {} a -> s {parameterGroupName = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AutoMinorVersionUpgrade")
            Prelude.<*> (x Core..:? "SnsTopicArn")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AvailabilityMode")
            Prelude.<*> (x Core..:? "ParameterGroupStatus")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "PendingUpdates")
            Prelude.<*> (x Core..:? "NumberOfShards")
            Prelude.<*> (x Core..:? "SnapshotWindow")
            Prelude.<*> (x Core..:? "SubnetGroupName")
            Prelude.<*> (x Core..:? "TLSEnabled")
            Prelude.<*> (x Core..:? "MaintenanceWindow")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Shards" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "EnginePatchVersion")
            Prelude.<*> (x Core..:? "SnapshotRetentionLimit")
            Prelude.<*> (x Core..:? "NodeType")
            Prelude.<*> (x Core..:? "SnsTopicStatus")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ACLName")
            Prelude.<*> (x Core..:? "ClusterEndpoint")
            Prelude.<*> (x Core..:? "ParameterGroupName")
      )

instance Prelude.Hashable Cluster

instance Prelude.NFData Cluster
