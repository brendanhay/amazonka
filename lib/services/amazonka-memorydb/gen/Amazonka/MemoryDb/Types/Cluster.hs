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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Cluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.AZStatus
import Amazonka.MemoryDb.Types.ClusterPendingUpdates
import Amazonka.MemoryDb.Types.DataTieringStatus
import Amazonka.MemoryDb.Types.Endpoint
import Amazonka.MemoryDb.Types.SecurityGroupMembership
import Amazonka.MemoryDb.Types.Shard
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The name of the subnet group used by the cluster
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group used by the cluster
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied name of the cluster. This identifier is a unique key
    -- that identifies a cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the Access Control List associated with this cluster.
    aCLName :: Prelude.Maybe Prelude.Text,
    -- | When set to true, the cluster will automatically receive minor engine
    -- version upgrades after launch.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Redis engine patch version used by the cluster
    enginePatchVersion :: Prelude.Maybe Prelude.Text,
    -- | The SNS topic must be in Active status to receive notifications
    snsTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A flag to indicate if In-transit encryption is enabled
    tLSEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The cluster\'s configuration endpoint
    clusterEndpoint :: Prelude.Maybe Endpoint,
    -- | Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
    -- (singleaz).
    availabilityMode :: Prelude.Maybe AZStatus,
    -- | The status of the cluster. For example, Available, Updating, Creating.
    status :: Prelude.Maybe Prelude.Text,
    -- | A description of the cluster
    description :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s node type
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    maintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The daily time range (in UTC) during which MemoryDB begins taking a
    -- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
    -- this parameter, MemoryDB automatically chooses an appropriate time
    -- range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of shards in the cluster
    numberOfShards :: Prelude.Maybe Prelude.Int,
    -- | The number of days for which MemoryDB retains automatic snapshots before
    -- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
    -- snapshot that was taken today is retained for 5 days before being
    -- deleted.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the SNS notification topic
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | A group of settings that are currently being applied.
    pendingUpdates :: Prelude.Maybe ClusterPendingUpdates,
    -- | A list of security groups used by the cluster
    securityGroups :: Prelude.Maybe [SecurityGroupMembership],
    -- | A list of shards that are members of the cluster.
    shards :: Prelude.Maybe [Shard],
    -- | The ID of the KMS key used to encrypt the cluster
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The status of the parameter group used by the cluster, for example
    -- \'active\' or \'applying\'.
    parameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | Enables data tiering. Data tiering is only supported for clusters using
    -- the r6gd node type. This parameter must be set when using r6gd nodes.
    -- For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
    dataTiering :: Prelude.Maybe DataTieringStatus,
    -- | The Redis engine version used by the cluster
    engineVersion :: Prelude.Maybe Prelude.Text
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
-- 'subnetGroupName', 'cluster_subnetGroupName' - The name of the subnet group used by the cluster
--
-- 'parameterGroupName', 'cluster_parameterGroupName' - The name of the parameter group used by the cluster
--
-- 'name', 'cluster_name' - The user-supplied name of the cluster. This identifier is a unique key
-- that identifies a cluster.
--
-- 'aCLName', 'cluster_aCLName' - The name of the Access Control List associated with this cluster.
--
-- 'autoMinorVersionUpgrade', 'cluster_autoMinorVersionUpgrade' - When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
--
-- 'enginePatchVersion', 'cluster_enginePatchVersion' - The Redis engine patch version used by the cluster
--
-- 'snsTopicStatus', 'cluster_snsTopicStatus' - The SNS topic must be in Active status to receive notifications
--
-- 'arn', 'cluster_arn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'tLSEnabled', 'cluster_tLSEnabled' - A flag to indicate if In-transit encryption is enabled
--
-- 'clusterEndpoint', 'cluster_clusterEndpoint' - The cluster\'s configuration endpoint
--
-- 'availabilityMode', 'cluster_availabilityMode' - Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
-- (singleaz).
--
-- 'status', 'cluster_status' - The status of the cluster. For example, Available, Updating, Creating.
--
-- 'description', 'cluster_description' - A description of the cluster
--
-- 'nodeType', 'cluster_nodeType' - The cluster\'s node type
--
-- 'maintenanceWindow', 'cluster_maintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- 'snapshotWindow', 'cluster_snapshotWindow' - The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
-- this parameter, MemoryDB automatically chooses an appropriate time
-- range.
--
-- 'numberOfShards', 'cluster_numberOfShards' - The number of shards in the cluster
--
-- 'snapshotRetentionLimit', 'cluster_snapshotRetentionLimit' - The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
--
-- 'snsTopicArn', 'cluster_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS notification topic
--
-- 'pendingUpdates', 'cluster_pendingUpdates' - A group of settings that are currently being applied.
--
-- 'securityGroups', 'cluster_securityGroups' - A list of security groups used by the cluster
--
-- 'shards', 'cluster_shards' - A list of shards that are members of the cluster.
--
-- 'kmsKeyId', 'cluster_kmsKeyId' - The ID of the KMS key used to encrypt the cluster
--
-- 'parameterGroupStatus', 'cluster_parameterGroupStatus' - The status of the parameter group used by the cluster, for example
-- \'active\' or \'applying\'.
--
-- 'dataTiering', 'cluster_dataTiering' - Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
--
-- 'engineVersion', 'cluster_engineVersion' - The Redis engine version used by the cluster
newCluster ::
  Cluster
newCluster =
  Cluster'
    { subnetGroupName = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      name = Prelude.Nothing,
      aCLName = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      enginePatchVersion = Prelude.Nothing,
      snsTopicStatus = Prelude.Nothing,
      arn = Prelude.Nothing,
      tLSEnabled = Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      availabilityMode = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      maintenanceWindow = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      numberOfShards = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      pendingUpdates = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      shards = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      parameterGroupStatus = Prelude.Nothing,
      dataTiering = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The name of the subnet group used by the cluster
cluster_subnetGroupName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_subnetGroupName = Lens.lens (\Cluster' {subnetGroupName} -> subnetGroupName) (\s@Cluster' {} a -> s {subnetGroupName = a} :: Cluster)

-- | The name of the parameter group used by the cluster
cluster_parameterGroupName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_parameterGroupName = Lens.lens (\Cluster' {parameterGroupName} -> parameterGroupName) (\s@Cluster' {} a -> s {parameterGroupName = a} :: Cluster)

-- | The user-supplied name of the cluster. This identifier is a unique key
-- that identifies a cluster.
cluster_name :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | The name of the Access Control List associated with this cluster.
cluster_aCLName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_aCLName = Lens.lens (\Cluster' {aCLName} -> aCLName) (\s@Cluster' {} a -> s {aCLName = a} :: Cluster)

-- | When set to true, the cluster will automatically receive minor engine
-- version upgrades after launch.
cluster_autoMinorVersionUpgrade :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_autoMinorVersionUpgrade = Lens.lens (\Cluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@Cluster' {} a -> s {autoMinorVersionUpgrade = a} :: Cluster)

-- | The Redis engine patch version used by the cluster
cluster_enginePatchVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_enginePatchVersion = Lens.lens (\Cluster' {enginePatchVersion} -> enginePatchVersion) (\s@Cluster' {} a -> s {enginePatchVersion = a} :: Cluster)

-- | The SNS topic must be in Active status to receive notifications
cluster_snsTopicStatus :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snsTopicStatus = Lens.lens (\Cluster' {snsTopicStatus} -> snsTopicStatus) (\s@Cluster' {} a -> s {snsTopicStatus = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the cluster.
cluster_arn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_arn = Lens.lens (\Cluster' {arn} -> arn) (\s@Cluster' {} a -> s {arn = a} :: Cluster)

-- | A flag to indicate if In-transit encryption is enabled
cluster_tLSEnabled :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_tLSEnabled = Lens.lens (\Cluster' {tLSEnabled} -> tLSEnabled) (\s@Cluster' {} a -> s {tLSEnabled = a} :: Cluster)

-- | The cluster\'s configuration endpoint
cluster_clusterEndpoint :: Lens.Lens' Cluster (Prelude.Maybe Endpoint)
cluster_clusterEndpoint = Lens.lens (\Cluster' {clusterEndpoint} -> clusterEndpoint) (\s@Cluster' {} a -> s {clusterEndpoint = a} :: Cluster)

-- | Indicates if the cluster has a Multi-AZ configuration (multiaz) or not
-- (singleaz).
cluster_availabilityMode :: Lens.Lens' Cluster (Prelude.Maybe AZStatus)
cluster_availabilityMode = Lens.lens (\Cluster' {availabilityMode} -> availabilityMode) (\s@Cluster' {} a -> s {availabilityMode = a} :: Cluster)

-- | The status of the cluster. For example, Available, Updating, Creating.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | A description of the cluster
cluster_description :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_description = Lens.lens (\Cluster' {description} -> description) (\s@Cluster' {} a -> s {description = a} :: Cluster)

-- | The cluster\'s node type
cluster_nodeType :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_nodeType = Lens.lens (\Cluster' {nodeType} -> nodeType) (\s@Cluster' {} a -> s {nodeType = a} :: Cluster)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
cluster_maintenanceWindow :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_maintenanceWindow = Lens.lens (\Cluster' {maintenanceWindow} -> maintenanceWindow) (\s@Cluster' {} a -> s {maintenanceWindow = a} :: Cluster)

-- | The daily time range (in UTC) during which MemoryDB begins taking a
-- daily snapshot of your shard. Example: 05:00-09:00 If you do not specify
-- this parameter, MemoryDB automatically chooses an appropriate time
-- range.
cluster_snapshotWindow :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snapshotWindow = Lens.lens (\Cluster' {snapshotWindow} -> snapshotWindow) (\s@Cluster' {} a -> s {snapshotWindow = a} :: Cluster)

-- | The number of shards in the cluster
cluster_numberOfShards :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_numberOfShards = Lens.lens (\Cluster' {numberOfShards} -> numberOfShards) (\s@Cluster' {} a -> s {numberOfShards = a} :: Cluster)

-- | The number of days for which MemoryDB retains automatic snapshots before
-- deleting them. For example, if you set SnapshotRetentionLimit to 5, a
-- snapshot that was taken today is retained for 5 days before being
-- deleted.
cluster_snapshotRetentionLimit :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_snapshotRetentionLimit = Lens.lens (\Cluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@Cluster' {} a -> s {snapshotRetentionLimit = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the SNS notification topic
cluster_snsTopicArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_snsTopicArn = Lens.lens (\Cluster' {snsTopicArn} -> snsTopicArn) (\s@Cluster' {} a -> s {snsTopicArn = a} :: Cluster)

-- | A group of settings that are currently being applied.
cluster_pendingUpdates :: Lens.Lens' Cluster (Prelude.Maybe ClusterPendingUpdates)
cluster_pendingUpdates = Lens.lens (\Cluster' {pendingUpdates} -> pendingUpdates) (\s@Cluster' {} a -> s {pendingUpdates = a} :: Cluster)

-- | A list of security groups used by the cluster
cluster_securityGroups :: Lens.Lens' Cluster (Prelude.Maybe [SecurityGroupMembership])
cluster_securityGroups = Lens.lens (\Cluster' {securityGroups} -> securityGroups) (\s@Cluster' {} a -> s {securityGroups = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of shards that are members of the cluster.
cluster_shards :: Lens.Lens' Cluster (Prelude.Maybe [Shard])
cluster_shards = Lens.lens (\Cluster' {shards} -> shards) (\s@Cluster' {} a -> s {shards = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the KMS key used to encrypt the cluster
cluster_kmsKeyId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_kmsKeyId = Lens.lens (\Cluster' {kmsKeyId} -> kmsKeyId) (\s@Cluster' {} a -> s {kmsKeyId = a} :: Cluster)

-- | The status of the parameter group used by the cluster, for example
-- \'active\' or \'applying\'.
cluster_parameterGroupStatus :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_parameterGroupStatus = Lens.lens (\Cluster' {parameterGroupStatus} -> parameterGroupStatus) (\s@Cluster' {} a -> s {parameterGroupStatus = a} :: Cluster)

-- | Enables data tiering. Data tiering is only supported for clusters using
-- the r6gd node type. This parameter must be set when using r6gd nodes.
-- For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/data-tiering.html Data tiering>.
cluster_dataTiering :: Lens.Lens' Cluster (Prelude.Maybe DataTieringStatus)
cluster_dataTiering = Lens.lens (\Cluster' {dataTiering} -> dataTiering) (\s@Cluster' {} a -> s {dataTiering = a} :: Cluster)

-- | The Redis engine version used by the cluster
cluster_engineVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_engineVersion = Lens.lens (\Cluster' {engineVersion} -> engineVersion) (\s@Cluster' {} a -> s {engineVersion = a} :: Cluster)

instance Data.FromJSON Cluster where
  parseJSON =
    Data.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Data..:? "SubnetGroupName")
            Prelude.<*> (x Data..:? "ParameterGroupName")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ACLName")
            Prelude.<*> (x Data..:? "AutoMinorVersionUpgrade")
            Prelude.<*> (x Data..:? "EnginePatchVersion")
            Prelude.<*> (x Data..:? "SnsTopicStatus")
            Prelude.<*> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "TLSEnabled")
            Prelude.<*> (x Data..:? "ClusterEndpoint")
            Prelude.<*> (x Data..:? "AvailabilityMode")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "MaintenanceWindow")
            Prelude.<*> (x Data..:? "SnapshotWindow")
            Prelude.<*> (x Data..:? "NumberOfShards")
            Prelude.<*> (x Data..:? "SnapshotRetentionLimit")
            Prelude.<*> (x Data..:? "SnsTopicArn")
            Prelude.<*> (x Data..:? "PendingUpdates")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Shards" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "ParameterGroupStatus")
            Prelude.<*> (x Data..:? "DataTiering")
            Prelude.<*> (x Data..:? "EngineVersion")
      )

instance Prelude.Hashable Cluster where
  hashWithSalt _salt Cluster' {..} =
    _salt `Prelude.hashWithSalt` subnetGroupName
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` aCLName
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` enginePatchVersion
      `Prelude.hashWithSalt` snsTopicStatus
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tLSEnabled
      `Prelude.hashWithSalt` clusterEndpoint
      `Prelude.hashWithSalt` availabilityMode
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` maintenanceWindow
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` numberOfShards
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` pendingUpdates
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` shards
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` parameterGroupStatus
      `Prelude.hashWithSalt` dataTiering
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf subnetGroupName
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf aCLName
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf enginePatchVersion
      `Prelude.seq` Prelude.rnf snsTopicStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf tLSEnabled
      `Prelude.seq` Prelude.rnf clusterEndpoint
      `Prelude.seq` Prelude.rnf availabilityMode
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf maintenanceWindow
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf numberOfShards
      `Prelude.seq` Prelude.rnf snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf pendingUpdates
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf shards
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf
        parameterGroupStatus
      `Prelude.seq` Prelude.rnf
        dataTiering
      `Prelude.seq` Prelude.rnf
        engineVersion
