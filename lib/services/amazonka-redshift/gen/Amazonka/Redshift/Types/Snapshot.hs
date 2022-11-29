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
-- Module      : Amazonka.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AccountWithRestoreAccess
import Amazonka.Redshift.Types.Tag

-- | Describes a snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The list of tags for the cluster snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The port that the cluster is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the cluster for which the snapshot was taken.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of days until a manual snapshot will pass its retention
    -- period.
    manualSnapshotRemainingDays :: Prelude.Maybe Prelude.Int,
    -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The admin user name for the cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The source region from which the snapshot was copied.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | The number of megabytes per second being transferred to the snapshot
    -- backup. Returns @0@ for a completed backup.
    currentBackupRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The cluster version of the cluster used to create the snapshot. For
    -- example, 1.0.15503.
    engineFullVersion :: Prelude.Maybe Prelude.Text,
    -- | The snapshot identifier that is provided in the request.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The list of node types that this cluster snapshot is able to restore
    -- into.
    restorableNodeTypes :: Prelude.Maybe [Prelude.Text],
    -- | The version ID of the Amazon Redshift engine that is running on the
    -- cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance track for the snapshot.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | A boolean that indicates whether the snapshot data is encrypted using
    -- the HSM keys of the source cluster. @true@ indicates that the data is
    -- encrypted using HSM keys.
    encryptedWithHSM :: Prelude.Maybe Prelude.Bool,
    -- | The snapshot status. The value of the status depends on the API
    -- operation used:
    --
    -- -   CreateClusterSnapshot and CopyClusterSnapshot returns status as
    --     \"creating\".
    --
    -- -   DescribeClusterSnapshots returns status as \"creating\",
    --     \"available\", \"final snapshot\", or \"failed\".
    --
    -- -   DeleteClusterSnapshot returns status as \"deleted\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The amount of time an in-progress snapshot backup has been running, or
    -- the amount of time it took a completed backup to finish.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | A timestamp representing the start of the retention period for the
    -- snapshot.
    snapshotRetentionStartTime :: Prelude.Maybe Core.ISO8601,
    -- | The Availability Zone in which the cluster was created.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The time (in UTC format) when Amazon Redshift began the snapshot. A
    -- snapshot contains a copy of the cluster data as of this exact time.
    snapshotCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The node type of the nodes in the cluster.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the data in the snapshot is encrypted at rest.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The estimate of the time remaining before the snapshot backup will
    -- complete. Returns @0@ for a completed backup.
    estimatedSecondsToCompletion :: Prelude.Maybe Prelude.Integer,
    -- | The number of nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The Key Management Service (KMS) key ID of the encryption key that was
    -- used to encrypt data in the cluster from which the snapshot was taken.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The VPC identifier of the cluster if the snapshot is from a cluster in a
    -- VPC. Otherwise, this field is not in the output.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The size of the complete set of backup data that would be used to
    -- restore the cluster.
    totalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | A list of the Amazon Web Services accounts authorized to restore the
    -- snapshot. Returns @null@ if no accounts are authorized. Visible only to
    -- the snapshot owner.
    accountsWithRestoreAccess :: Prelude.Maybe [AccountWithRestoreAccess],
    -- | For manual snapshots, the Amazon Web Services account used to create or
    -- copy the snapshot. For automatic snapshots, the owner of the cluster.
    -- The owner can perform all snapshot actions, such as sharing a manual
    -- snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The time (UTC) when the cluster was originally created.
    clusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | The number of megabytes that have been transferred to the snapshot
    -- backup.
    backupProgressInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The name of the database that was created when the cluster was created.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The size of the incremental backup.
    actualIncrementalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The snapshot type. Snapshots created using CreateClusterSnapshot and
    -- CopyClusterSnapshot are of type \"manual\".
    snapshotType :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'snapshot_tags' - The list of tags for the cluster snapshot.
--
-- 'port', 'snapshot_port' - The port that the cluster is listening on.
--
-- 'clusterIdentifier', 'snapshot_clusterIdentifier' - The identifier of the cluster for which the snapshot was taken.
--
-- 'manualSnapshotRemainingDays', 'snapshot_manualSnapshotRemainingDays' - The number of days until a manual snapshot will pass its retention
-- period.
--
-- 'manualSnapshotRetentionPeriod', 'snapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'masterUsername', 'snapshot_masterUsername' - The admin user name for the cluster.
--
-- 'sourceRegion', 'snapshot_sourceRegion' - The source region from which the snapshot was copied.
--
-- 'currentBackupRateInMegaBytesPerSecond', 'snapshot_currentBackupRateInMegaBytesPerSecond' - The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
--
-- 'engineFullVersion', 'snapshot_engineFullVersion' - The cluster version of the cluster used to create the snapshot. For
-- example, 1.0.15503.
--
-- 'snapshotIdentifier', 'snapshot_snapshotIdentifier' - The snapshot identifier that is provided in the request.
--
-- 'restorableNodeTypes', 'snapshot_restorableNodeTypes' - The list of node types that this cluster snapshot is able to restore
-- into.
--
-- 'clusterVersion', 'snapshot_clusterVersion' - The version ID of the Amazon Redshift engine that is running on the
-- cluster.
--
-- 'maintenanceTrackName', 'snapshot_maintenanceTrackName' - The name of the maintenance track for the snapshot.
--
-- 'encryptedWithHSM', 'snapshot_encryptedWithHSM' - A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
--
-- 'status', 'snapshot_status' - The snapshot status. The value of the status depends on the API
-- operation used:
--
-- -   CreateClusterSnapshot and CopyClusterSnapshot returns status as
--     \"creating\".
--
-- -   DescribeClusterSnapshots returns status as \"creating\",
--     \"available\", \"final snapshot\", or \"failed\".
--
-- -   DeleteClusterSnapshot returns status as \"deleted\".
--
-- 'elapsedTimeInSeconds', 'snapshot_elapsedTimeInSeconds' - The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
--
-- 'snapshotRetentionStartTime', 'snapshot_snapshotRetentionStartTime' - A timestamp representing the start of the retention period for the
-- snapshot.
--
-- 'availabilityZone', 'snapshot_availabilityZone' - The Availability Zone in which the cluster was created.
--
-- 'snapshotCreateTime', 'snapshot_snapshotCreateTime' - The time (in UTC format) when Amazon Redshift began the snapshot. A
-- snapshot contains a copy of the cluster data as of this exact time.
--
-- 'nodeType', 'snapshot_nodeType' - The node type of the nodes in the cluster.
--
-- 'encrypted', 'snapshot_encrypted' - If @true@, the data in the snapshot is encrypted at rest.
--
-- 'estimatedSecondsToCompletion', 'snapshot_estimatedSecondsToCompletion' - The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
--
-- 'numberOfNodes', 'snapshot_numberOfNodes' - The number of nodes in the cluster.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The Key Management Service (KMS) key ID of the encryption key that was
-- used to encrypt data in the cluster from which the snapshot was taken.
--
-- 'enhancedVpcRouting', 'snapshot_enhancedVpcRouting' - An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
--
-- 'vpcId', 'snapshot_vpcId' - The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
--
-- 'totalBackupSizeInMegaBytes', 'snapshot_totalBackupSizeInMegaBytes' - The size of the complete set of backup data that would be used to
-- restore the cluster.
--
-- 'accountsWithRestoreAccess', 'snapshot_accountsWithRestoreAccess' - A list of the Amazon Web Services accounts authorized to restore the
-- snapshot. Returns @null@ if no accounts are authorized. Visible only to
-- the snapshot owner.
--
-- 'ownerAccount', 'snapshot_ownerAccount' - For manual snapshots, the Amazon Web Services account used to create or
-- copy the snapshot. For automatic snapshots, the owner of the cluster.
-- The owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
--
-- 'clusterCreateTime', 'snapshot_clusterCreateTime' - The time (UTC) when the cluster was originally created.
--
-- 'backupProgressInMegaBytes', 'snapshot_backupProgressInMegaBytes' - The number of megabytes that have been transferred to the snapshot
-- backup.
--
-- 'dbName', 'snapshot_dbName' - The name of the database that was created when the cluster was created.
--
-- 'actualIncrementalBackupSizeInMegaBytes', 'snapshot_actualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup.
--
-- 'snapshotType', 'snapshot_snapshotType' - The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot are of type \"manual\".
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { tags = Prelude.Nothing,
      port = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      manualSnapshotRemainingDays = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      currentBackupRateInMegaBytesPerSecond =
        Prelude.Nothing,
      engineFullVersion = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      restorableNodeTypes = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      encryptedWithHSM = Prelude.Nothing,
      status = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      snapshotRetentionStartTime = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      estimatedSecondsToCompletion = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      totalBackupSizeInMegaBytes = Prelude.Nothing,
      accountsWithRestoreAccess = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      backupProgressInMegaBytes = Prelude.Nothing,
      dbName = Prelude.Nothing,
      actualIncrementalBackupSizeInMegaBytes =
        Prelude.Nothing,
      snapshotType = Prelude.Nothing
    }

-- | The list of tags for the cluster snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The port that the cluster is listening on.
snapshot_port :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_port = Lens.lens (\Snapshot' {port} -> port) (\s@Snapshot' {} a -> s {port = a} :: Snapshot)

-- | The identifier of the cluster for which the snapshot was taken.
snapshot_clusterIdentifier :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_clusterIdentifier = Lens.lens (\Snapshot' {clusterIdentifier} -> clusterIdentifier) (\s@Snapshot' {} a -> s {clusterIdentifier = a} :: Snapshot)

-- | The number of days until a manual snapshot will pass its retention
-- period.
snapshot_manualSnapshotRemainingDays :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_manualSnapshotRemainingDays = Lens.lens (\Snapshot' {manualSnapshotRemainingDays} -> manualSnapshotRemainingDays) (\s@Snapshot' {} a -> s {manualSnapshotRemainingDays = a} :: Snapshot)

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
snapshot_manualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_manualSnapshotRetentionPeriod = Lens.lens (\Snapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@Snapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: Snapshot)

-- | The admin user name for the cluster.
snapshot_masterUsername :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_masterUsername = Lens.lens (\Snapshot' {masterUsername} -> masterUsername) (\s@Snapshot' {} a -> s {masterUsername = a} :: Snapshot)

-- | The source region from which the snapshot was copied.
snapshot_sourceRegion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_sourceRegion = Lens.lens (\Snapshot' {sourceRegion} -> sourceRegion) (\s@Snapshot' {} a -> s {sourceRegion = a} :: Snapshot)

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
snapshot_currentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_currentBackupRateInMegaBytesPerSecond = Lens.lens (\Snapshot' {currentBackupRateInMegaBytesPerSecond} -> currentBackupRateInMegaBytesPerSecond) (\s@Snapshot' {} a -> s {currentBackupRateInMegaBytesPerSecond = a} :: Snapshot)

-- | The cluster version of the cluster used to create the snapshot. For
-- example, 1.0.15503.
snapshot_engineFullVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engineFullVersion = Lens.lens (\Snapshot' {engineFullVersion} -> engineFullVersion) (\s@Snapshot' {} a -> s {engineFullVersion = a} :: Snapshot)

-- | The snapshot identifier that is provided in the request.
snapshot_snapshotIdentifier :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotIdentifier = Lens.lens (\Snapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@Snapshot' {} a -> s {snapshotIdentifier = a} :: Snapshot)

-- | The list of node types that this cluster snapshot is able to restore
-- into.
snapshot_restorableNodeTypes :: Lens.Lens' Snapshot (Prelude.Maybe [Prelude.Text])
snapshot_restorableNodeTypes = Lens.lens (\Snapshot' {restorableNodeTypes} -> restorableNodeTypes) (\s@Snapshot' {} a -> s {restorableNodeTypes = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
snapshot_clusterVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_clusterVersion = Lens.lens (\Snapshot' {clusterVersion} -> clusterVersion) (\s@Snapshot' {} a -> s {clusterVersion = a} :: Snapshot)

-- | The name of the maintenance track for the snapshot.
snapshot_maintenanceTrackName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_maintenanceTrackName = Lens.lens (\Snapshot' {maintenanceTrackName} -> maintenanceTrackName) (\s@Snapshot' {} a -> s {maintenanceTrackName = a} :: Snapshot)

-- | A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
snapshot_encryptedWithHSM :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_encryptedWithHSM = Lens.lens (\Snapshot' {encryptedWithHSM} -> encryptedWithHSM) (\s@Snapshot' {} a -> s {encryptedWithHSM = a} :: Snapshot)

-- | The snapshot status. The value of the status depends on the API
-- operation used:
--
-- -   CreateClusterSnapshot and CopyClusterSnapshot returns status as
--     \"creating\".
--
-- -   DescribeClusterSnapshots returns status as \"creating\",
--     \"available\", \"final snapshot\", or \"failed\".
--
-- -   DeleteClusterSnapshot returns status as \"deleted\".
snapshot_status :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
snapshot_elapsedTimeInSeconds :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_elapsedTimeInSeconds = Lens.lens (\Snapshot' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@Snapshot' {} a -> s {elapsedTimeInSeconds = a} :: Snapshot)

-- | A timestamp representing the start of the retention period for the
-- snapshot.
snapshot_snapshotRetentionStartTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotRetentionStartTime = Lens.lens (\Snapshot' {snapshotRetentionStartTime} -> snapshotRetentionStartTime) (\s@Snapshot' {} a -> s {snapshotRetentionStartTime = a} :: Snapshot) Prelude.. Lens.mapping Core._Time

-- | The Availability Zone in which the cluster was created.
snapshot_availabilityZone :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_availabilityZone = Lens.lens (\Snapshot' {availabilityZone} -> availabilityZone) (\s@Snapshot' {} a -> s {availabilityZone = a} :: Snapshot)

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A
-- snapshot contains a copy of the cluster data as of this exact time.
snapshot_snapshotCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotCreateTime = Lens.lens (\Snapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@Snapshot' {} a -> s {snapshotCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Core._Time

-- | The node type of the nodes in the cluster.
snapshot_nodeType :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_nodeType = Lens.lens (\Snapshot' {nodeType} -> nodeType) (\s@Snapshot' {} a -> s {nodeType = a} :: Snapshot)

-- | If @true@, the data in the snapshot is encrypted at rest.
snapshot_encrypted :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
snapshot_estimatedSecondsToCompletion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_estimatedSecondsToCompletion = Lens.lens (\Snapshot' {estimatedSecondsToCompletion} -> estimatedSecondsToCompletion) (\s@Snapshot' {} a -> s {estimatedSecondsToCompletion = a} :: Snapshot)

-- | The number of nodes in the cluster.
snapshot_numberOfNodes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numberOfNodes = Lens.lens (\Snapshot' {numberOfNodes} -> numberOfNodes) (\s@Snapshot' {} a -> s {numberOfNodes = a} :: Snapshot)

-- | The Key Management Service (KMS) key ID of the encryption key that was
-- used to encrypt data in the cluster from which the snapshot was taken.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
snapshot_enhancedVpcRouting :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_enhancedVpcRouting = Lens.lens (\Snapshot' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@Snapshot' {} a -> s {enhancedVpcRouting = a} :: Snapshot)

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
snapshot_vpcId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_vpcId = Lens.lens (\Snapshot' {vpcId} -> vpcId) (\s@Snapshot' {} a -> s {vpcId = a} :: Snapshot)

-- | The size of the complete set of backup data that would be used to
-- restore the cluster.
snapshot_totalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_totalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {totalBackupSizeInMegaBytes = a} :: Snapshot)

-- | A list of the Amazon Web Services accounts authorized to restore the
-- snapshot. Returns @null@ if no accounts are authorized. Visible only to
-- the snapshot owner.
snapshot_accountsWithRestoreAccess :: Lens.Lens' Snapshot (Prelude.Maybe [AccountWithRestoreAccess])
snapshot_accountsWithRestoreAccess = Lens.lens (\Snapshot' {accountsWithRestoreAccess} -> accountsWithRestoreAccess) (\s@Snapshot' {} a -> s {accountsWithRestoreAccess = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | For manual snapshots, the Amazon Web Services account used to create or
-- copy the snapshot. For automatic snapshots, the owner of the cluster.
-- The owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
snapshot_ownerAccount :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_ownerAccount = Lens.lens (\Snapshot' {ownerAccount} -> ownerAccount) (\s@Snapshot' {} a -> s {ownerAccount = a} :: Snapshot)

-- | The time (UTC) when the cluster was originally created.
snapshot_clusterCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_clusterCreateTime = Lens.lens (\Snapshot' {clusterCreateTime} -> clusterCreateTime) (\s@Snapshot' {} a -> s {clusterCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Core._Time

-- | The number of megabytes that have been transferred to the snapshot
-- backup.
snapshot_backupProgressInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_backupProgressInMegaBytes = Lens.lens (\Snapshot' {backupProgressInMegaBytes} -> backupProgressInMegaBytes) (\s@Snapshot' {} a -> s {backupProgressInMegaBytes = a} :: Snapshot)

-- | The name of the database that was created when the cluster was created.
snapshot_dbName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_dbName = Lens.lens (\Snapshot' {dbName} -> dbName) (\s@Snapshot' {} a -> s {dbName = a} :: Snapshot)

-- | The size of the incremental backup.
snapshot_actualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_actualIncrementalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {actualIncrementalBackupSizeInMegaBytes} -> actualIncrementalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {actualIncrementalBackupSizeInMegaBytes = a} :: Snapshot)

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot are of type \"manual\".
snapshot_snapshotType :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotType = Lens.lens (\Snapshot' {snapshotType} -> snapshotType) (\s@Snapshot' {} a -> s {snapshotType = a} :: Snapshot)

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "ClusterIdentifier")
      Prelude.<*> (x Core..@? "ManualSnapshotRemainingDays")
      Prelude.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Prelude.<*> (x Core..@? "MasterUsername")
      Prelude.<*> (x Core..@? "SourceRegion")
      Prelude.<*> (x Core..@? "CurrentBackupRateInMegaBytesPerSecond")
      Prelude.<*> (x Core..@? "EngineFullVersion")
      Prelude.<*> (x Core..@? "SnapshotIdentifier")
      Prelude.<*> ( x Core..@? "RestorableNodeTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "NodeType")
                  )
      Prelude.<*> (x Core..@? "ClusterVersion")
      Prelude.<*> (x Core..@? "MaintenanceTrackName")
      Prelude.<*> (x Core..@? "EncryptedWithHSM")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "ElapsedTimeInSeconds")
      Prelude.<*> (x Core..@? "SnapshotRetentionStartTime")
      Prelude.<*> (x Core..@? "AvailabilityZone")
      Prelude.<*> (x Core..@? "SnapshotCreateTime")
      Prelude.<*> (x Core..@? "NodeType")
      Prelude.<*> (x Core..@? "Encrypted")
      Prelude.<*> (x Core..@? "EstimatedSecondsToCompletion")
      Prelude.<*> (x Core..@? "NumberOfNodes")
      Prelude.<*> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "EnhancedVpcRouting")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> (x Core..@? "TotalBackupSizeInMegaBytes")
      Prelude.<*> ( x Core..@? "AccountsWithRestoreAccess"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "AccountWithRestoreAccess")
                  )
      Prelude.<*> (x Core..@? "OwnerAccount")
      Prelude.<*> (x Core..@? "ClusterCreateTime")
      Prelude.<*> (x Core..@? "BackupProgressInMegaBytes")
      Prelude.<*> (x Core..@? "DBName")
      Prelude.<*> (x Core..@? "ActualIncrementalBackupSizeInMegaBytes")
      Prelude.<*> (x Core..@? "SnapshotType")

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` manualSnapshotRemainingDays
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` currentBackupRateInMegaBytesPerSecond
      `Prelude.hashWithSalt` engineFullVersion
      `Prelude.hashWithSalt` snapshotIdentifier
      `Prelude.hashWithSalt` restorableNodeTypes
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` encryptedWithHSM
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` elapsedTimeInSeconds
      `Prelude.hashWithSalt` snapshotRetentionStartTime
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` estimatedSecondsToCompletion
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` totalBackupSizeInMegaBytes
      `Prelude.hashWithSalt` accountsWithRestoreAccess
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` backupProgressInMegaBytes
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` actualIncrementalBackupSizeInMegaBytes
      `Prelude.hashWithSalt` snapshotType

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf manualSnapshotRemainingDays
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf currentBackupRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf engineFullVersion
      `Prelude.seq` Prelude.rnf snapshotIdentifier
      `Prelude.seq` Prelude.rnf restorableNodeTypes
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf encryptedWithHSM
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf
        snapshotRetentionStartTime
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf
        estimatedSecondsToCompletion
      `Prelude.seq` Prelude.rnf
        numberOfNodes
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf
        enhancedVpcRouting
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf
        totalBackupSizeInMegaBytes
      `Prelude.seq` Prelude.rnf
        accountsWithRestoreAccess
      `Prelude.seq` Prelude.rnf
        ownerAccount
      `Prelude.seq` Prelude.rnf
        clusterCreateTime
      `Prelude.seq` Prelude.rnf
        backupProgressInMegaBytes
      `Prelude.seq` Prelude.rnf
        dbName
      `Prelude.seq` Prelude.rnf
        actualIncrementalBackupSizeInMegaBytes
      `Prelude.seq` Prelude.rnf
        snapshotType
