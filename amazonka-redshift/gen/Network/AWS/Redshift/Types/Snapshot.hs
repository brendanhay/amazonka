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
-- Module      : Network.AWS.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Snapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.Tag

-- | Describes a snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | An option that specifies whether to create the cluster with enhanced VPC
    -- routing enabled. To create a cluster that uses enhanced VPC routing, the
    -- cluster must be in a VPC. For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
    -- in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@, enhanced VPC routing is enabled.
    --
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | The snapshot identifier that is provided in the request.
    snapshotIdentifier :: Core.Maybe Core.Text,
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
    status :: Core.Maybe Core.Text,
    -- | The estimate of the time remaining before the snapshot backup will
    -- complete. Returns @0@ for a completed backup.
    estimatedSecondsToCompletion :: Core.Maybe Core.Integer,
    -- | If @true@, the data in the snapshot is encrypted at rest.
    encrypted :: Core.Maybe Core.Bool,
    -- | The time (UTC) when the cluster was originally created.
    clusterCreateTime :: Core.Maybe Core.ISO8601,
    -- | The number of days until a manual snapshot will pass its retention
    -- period.
    manualSnapshotRemainingDays :: Core.Maybe Core.Int,
    -- | The time (in UTC format) when Amazon Redshift began the snapshot. A
    -- snapshot contains a copy of the cluster data as of this exact time.
    snapshotCreateTime :: Core.Maybe Core.ISO8601,
    -- | The number of megabytes per second being transferred to the snapshot
    -- backup. Returns @0@ for a completed backup.
    currentBackupRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | The master user name for the cluster.
    masterUsername :: Core.Maybe Core.Text,
    -- | A boolean that indicates whether the snapshot data is encrypted using
    -- the HSM keys of the source cluster. @true@ indicates that the data is
    -- encrypted using HSM keys.
    encryptedWithHSM :: Core.Maybe Core.Bool,
    -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that
    -- was used to encrypt data in the cluster from which the snapshot was
    -- taken.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The cluster version of the cluster used to create the snapshot. For
    -- example, 1.0.15503.
    engineFullVersion :: Core.Maybe Core.Text,
    -- | The Availability Zone in which the cluster was created.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The list of node types that this cluster snapshot is able to restore
    -- into.
    restorableNodeTypes :: Core.Maybe [Core.Text],
    -- | The snapshot type. Snapshots created using CreateClusterSnapshot and
    -- CopyClusterSnapshot are of type \"manual\".
    snapshotType :: Core.Maybe Core.Text,
    -- | A list of the AWS customer accounts authorized to restore the snapshot.
    -- Returns @null@ if no accounts are authorized. Visible only to the
    -- snapshot owner.
    accountsWithRestoreAccess :: Core.Maybe [AccountWithRestoreAccess],
    -- | The size of the incremental backup.
    actualIncrementalBackupSizeInMegaBytes :: Core.Maybe Core.Double,
    -- | The identifier of the cluster for which the snapshot was taken.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The list of tags for the cluster snapshot.
    tags :: Core.Maybe [Tag],
    -- | The number of nodes in the cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The port that the cluster is listening on.
    port :: Core.Maybe Core.Int,
    -- | The size of the complete set of backup data that would be used to
    -- restore the cluster.
    totalBackupSizeInMegaBytes :: Core.Maybe Core.Double,
    -- | The name of the database that was created when the cluster was created.
    dbName :: Core.Maybe Core.Text,
    -- | The number of megabytes that have been transferred to the snapshot
    -- backup.
    backupProgressInMegaBytes :: Core.Maybe Core.Double,
    -- | The amount of time an in-progress snapshot backup has been running, or
    -- the amount of time it took a completed backup to finish.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | The node type of the nodes in the cluster.
    nodeType :: Core.Maybe Core.Text,
    -- | For manual snapshots, the AWS customer account used to create or copy
    -- the snapshot. For automatic snapshots, the owner of the cluster. The
    -- owner can perform all snapshot actions, such as sharing a manual
    -- snapshot.
    ownerAccount :: Core.Maybe Core.Text,
    -- | The version ID of the Amazon Redshift engine that is running on the
    -- cluster.
    clusterVersion :: Core.Maybe Core.Text,
    -- | The VPC identifier of the cluster if the snapshot is from a cluster in a
    -- VPC. Otherwise, this field is not in the output.
    vpcId :: Core.Maybe Core.Text,
    -- | The source region from which the snapshot was copied.
    sourceRegion :: Core.Maybe Core.Text,
    -- | A timestamp representing the start of the retention period for the
    -- snapshot.
    snapshotRetentionStartTime :: Core.Maybe Core.ISO8601,
    -- | The name of the maintenance track for the snapshot.
    maintenanceTrackName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'snapshotIdentifier', 'snapshot_snapshotIdentifier' - The snapshot identifier that is provided in the request.
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
-- 'estimatedSecondsToCompletion', 'snapshot_estimatedSecondsToCompletion' - The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
--
-- 'encrypted', 'snapshot_encrypted' - If @true@, the data in the snapshot is encrypted at rest.
--
-- 'clusterCreateTime', 'snapshot_clusterCreateTime' - The time (UTC) when the cluster was originally created.
--
-- 'manualSnapshotRemainingDays', 'snapshot_manualSnapshotRemainingDays' - The number of days until a manual snapshot will pass its retention
-- period.
--
-- 'snapshotCreateTime', 'snapshot_snapshotCreateTime' - The time (in UTC format) when Amazon Redshift began the snapshot. A
-- snapshot contains a copy of the cluster data as of this exact time.
--
-- 'currentBackupRateInMegaBytesPerSecond', 'snapshot_currentBackupRateInMegaBytesPerSecond' - The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
--
-- 'masterUsername', 'snapshot_masterUsername' - The master user name for the cluster.
--
-- 'encryptedWithHSM', 'snapshot_encryptedWithHSM' - A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
--
-- 'manualSnapshotRetentionPeriod', 'snapshot_manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that
-- was used to encrypt data in the cluster from which the snapshot was
-- taken.
--
-- 'engineFullVersion', 'snapshot_engineFullVersion' - The cluster version of the cluster used to create the snapshot. For
-- example, 1.0.15503.
--
-- 'availabilityZone', 'snapshot_availabilityZone' - The Availability Zone in which the cluster was created.
--
-- 'restorableNodeTypes', 'snapshot_restorableNodeTypes' - The list of node types that this cluster snapshot is able to restore
-- into.
--
-- 'snapshotType', 'snapshot_snapshotType' - The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot are of type \"manual\".
--
-- 'accountsWithRestoreAccess', 'snapshot_accountsWithRestoreAccess' - A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns @null@ if no accounts are authorized. Visible only to the
-- snapshot owner.
--
-- 'actualIncrementalBackupSizeInMegaBytes', 'snapshot_actualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup.
--
-- 'clusterIdentifier', 'snapshot_clusterIdentifier' - The identifier of the cluster for which the snapshot was taken.
--
-- 'tags', 'snapshot_tags' - The list of tags for the cluster snapshot.
--
-- 'numberOfNodes', 'snapshot_numberOfNodes' - The number of nodes in the cluster.
--
-- 'port', 'snapshot_port' - The port that the cluster is listening on.
--
-- 'totalBackupSizeInMegaBytes', 'snapshot_totalBackupSizeInMegaBytes' - The size of the complete set of backup data that would be used to
-- restore the cluster.
--
-- 'dbName', 'snapshot_dbName' - The name of the database that was created when the cluster was created.
--
-- 'backupProgressInMegaBytes', 'snapshot_backupProgressInMegaBytes' - The number of megabytes that have been transferred to the snapshot
-- backup.
--
-- 'elapsedTimeInSeconds', 'snapshot_elapsedTimeInSeconds' - The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
--
-- 'nodeType', 'snapshot_nodeType' - The node type of the nodes in the cluster.
--
-- 'ownerAccount', 'snapshot_ownerAccount' - For manual snapshots, the AWS customer account used to create or copy
-- the snapshot. For automatic snapshots, the owner of the cluster. The
-- owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
--
-- 'clusterVersion', 'snapshot_clusterVersion' - The version ID of the Amazon Redshift engine that is running on the
-- cluster.
--
-- 'vpcId', 'snapshot_vpcId' - The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
--
-- 'sourceRegion', 'snapshot_sourceRegion' - The source region from which the snapshot was copied.
--
-- 'snapshotRetentionStartTime', 'snapshot_snapshotRetentionStartTime' - A timestamp representing the start of the retention period for the
-- snapshot.
--
-- 'maintenanceTrackName', 'snapshot_maintenanceTrackName' - The name of the maintenance track for the snapshot.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { enhancedVpcRouting = Core.Nothing,
      snapshotIdentifier = Core.Nothing,
      status = Core.Nothing,
      estimatedSecondsToCompletion = Core.Nothing,
      encrypted = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      manualSnapshotRemainingDays = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      currentBackupRateInMegaBytesPerSecond = Core.Nothing,
      masterUsername = Core.Nothing,
      encryptedWithHSM = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      kmsKeyId = Core.Nothing,
      engineFullVersion = Core.Nothing,
      availabilityZone = Core.Nothing,
      restorableNodeTypes = Core.Nothing,
      snapshotType = Core.Nothing,
      accountsWithRestoreAccess = Core.Nothing,
      actualIncrementalBackupSizeInMegaBytes =
        Core.Nothing,
      clusterIdentifier = Core.Nothing,
      tags = Core.Nothing,
      numberOfNodes = Core.Nothing,
      port = Core.Nothing,
      totalBackupSizeInMegaBytes = Core.Nothing,
      dbName = Core.Nothing,
      backupProgressInMegaBytes = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      nodeType = Core.Nothing,
      ownerAccount = Core.Nothing,
      clusterVersion = Core.Nothing,
      vpcId = Core.Nothing,
      sourceRegion = Core.Nothing,
      snapshotRetentionStartTime = Core.Nothing,
      maintenanceTrackName = Core.Nothing
    }

-- | An option that specifies whether to create the cluster with enhanced VPC
-- routing enabled. To create a cluster that uses enhanced VPC routing, the
-- cluster must be in a VPC. For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@, enhanced VPC routing is enabled.
--
-- Default: false
snapshot_enhancedVpcRouting :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
snapshot_enhancedVpcRouting = Lens.lens (\Snapshot' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@Snapshot' {} a -> s {enhancedVpcRouting = a} :: Snapshot)

-- | The snapshot identifier that is provided in the request.
snapshot_snapshotIdentifier :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_snapshotIdentifier = Lens.lens (\Snapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@Snapshot' {} a -> s {snapshotIdentifier = a} :: Snapshot)

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
snapshot_status :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
snapshot_estimatedSecondsToCompletion :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
snapshot_estimatedSecondsToCompletion = Lens.lens (\Snapshot' {estimatedSecondsToCompletion} -> estimatedSecondsToCompletion) (\s@Snapshot' {} a -> s {estimatedSecondsToCompletion = a} :: Snapshot)

-- | If @true@, the data in the snapshot is encrypted at rest.
snapshot_encrypted :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

-- | The time (UTC) when the cluster was originally created.
snapshot_clusterCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
snapshot_clusterCreateTime = Lens.lens (\Snapshot' {clusterCreateTime} -> clusterCreateTime) (\s@Snapshot' {} a -> s {clusterCreateTime = a} :: Snapshot) Core.. Lens.mapping Core._Time

-- | The number of days until a manual snapshot will pass its retention
-- period.
snapshot_manualSnapshotRemainingDays :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
snapshot_manualSnapshotRemainingDays = Lens.lens (\Snapshot' {manualSnapshotRemainingDays} -> manualSnapshotRemainingDays) (\s@Snapshot' {} a -> s {manualSnapshotRemainingDays = a} :: Snapshot)

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A
-- snapshot contains a copy of the cluster data as of this exact time.
snapshot_snapshotCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
snapshot_snapshotCreateTime = Lens.lens (\Snapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@Snapshot' {} a -> s {snapshotCreateTime = a} :: Snapshot) Core.. Lens.mapping Core._Time

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
snapshot_currentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
snapshot_currentBackupRateInMegaBytesPerSecond = Lens.lens (\Snapshot' {currentBackupRateInMegaBytesPerSecond} -> currentBackupRateInMegaBytesPerSecond) (\s@Snapshot' {} a -> s {currentBackupRateInMegaBytesPerSecond = a} :: Snapshot)

-- | The master user name for the cluster.
snapshot_masterUsername :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_masterUsername = Lens.lens (\Snapshot' {masterUsername} -> masterUsername) (\s@Snapshot' {} a -> s {masterUsername = a} :: Snapshot)

-- | A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
snapshot_encryptedWithHSM :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
snapshot_encryptedWithHSM = Lens.lens (\Snapshot' {encryptedWithHSM} -> encryptedWithHSM) (\s@Snapshot' {} a -> s {encryptedWithHSM = a} :: Snapshot)

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
snapshot_manualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
snapshot_manualSnapshotRetentionPeriod = Lens.lens (\Snapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@Snapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: Snapshot)

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- was used to encrypt data in the cluster from which the snapshot was
-- taken.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The cluster version of the cluster used to create the snapshot. For
-- example, 1.0.15503.
snapshot_engineFullVersion :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_engineFullVersion = Lens.lens (\Snapshot' {engineFullVersion} -> engineFullVersion) (\s@Snapshot' {} a -> s {engineFullVersion = a} :: Snapshot)

-- | The Availability Zone in which the cluster was created.
snapshot_availabilityZone :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_availabilityZone = Lens.lens (\Snapshot' {availabilityZone} -> availabilityZone) (\s@Snapshot' {} a -> s {availabilityZone = a} :: Snapshot)

-- | The list of node types that this cluster snapshot is able to restore
-- into.
snapshot_restorableNodeTypes :: Lens.Lens' Snapshot (Core.Maybe [Core.Text])
snapshot_restorableNodeTypes = Lens.lens (\Snapshot' {restorableNodeTypes} -> restorableNodeTypes) (\s@Snapshot' {} a -> s {restorableNodeTypes = a} :: Snapshot) Core.. Lens.mapping Lens._Coerce

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot are of type \"manual\".
snapshot_snapshotType :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_snapshotType = Lens.lens (\Snapshot' {snapshotType} -> snapshotType) (\s@Snapshot' {} a -> s {snapshotType = a} :: Snapshot)

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns @null@ if no accounts are authorized. Visible only to the
-- snapshot owner.
snapshot_accountsWithRestoreAccess :: Lens.Lens' Snapshot (Core.Maybe [AccountWithRestoreAccess])
snapshot_accountsWithRestoreAccess = Lens.lens (\Snapshot' {accountsWithRestoreAccess} -> accountsWithRestoreAccess) (\s@Snapshot' {} a -> s {accountsWithRestoreAccess = a} :: Snapshot) Core.. Lens.mapping Lens._Coerce

-- | The size of the incremental backup.
snapshot_actualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
snapshot_actualIncrementalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {actualIncrementalBackupSizeInMegaBytes} -> actualIncrementalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {actualIncrementalBackupSizeInMegaBytes = a} :: Snapshot)

-- | The identifier of the cluster for which the snapshot was taken.
snapshot_clusterIdentifier :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_clusterIdentifier = Lens.lens (\Snapshot' {clusterIdentifier} -> clusterIdentifier) (\s@Snapshot' {} a -> s {clusterIdentifier = a} :: Snapshot)

-- | The list of tags for the cluster snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Core.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Core.. Lens.mapping Lens._Coerce

-- | The number of nodes in the cluster.
snapshot_numberOfNodes :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
snapshot_numberOfNodes = Lens.lens (\Snapshot' {numberOfNodes} -> numberOfNodes) (\s@Snapshot' {} a -> s {numberOfNodes = a} :: Snapshot)

-- | The port that the cluster is listening on.
snapshot_port :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
snapshot_port = Lens.lens (\Snapshot' {port} -> port) (\s@Snapshot' {} a -> s {port = a} :: Snapshot)

-- | The size of the complete set of backup data that would be used to
-- restore the cluster.
snapshot_totalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
snapshot_totalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {totalBackupSizeInMegaBytes = a} :: Snapshot)

-- | The name of the database that was created when the cluster was created.
snapshot_dbName :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_dbName = Lens.lens (\Snapshot' {dbName} -> dbName) (\s@Snapshot' {} a -> s {dbName = a} :: Snapshot)

-- | The number of megabytes that have been transferred to the snapshot
-- backup.
snapshot_backupProgressInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
snapshot_backupProgressInMegaBytes = Lens.lens (\Snapshot' {backupProgressInMegaBytes} -> backupProgressInMegaBytes) (\s@Snapshot' {} a -> s {backupProgressInMegaBytes = a} :: Snapshot)

-- | The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
snapshot_elapsedTimeInSeconds :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
snapshot_elapsedTimeInSeconds = Lens.lens (\Snapshot' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@Snapshot' {} a -> s {elapsedTimeInSeconds = a} :: Snapshot)

-- | The node type of the nodes in the cluster.
snapshot_nodeType :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_nodeType = Lens.lens (\Snapshot' {nodeType} -> nodeType) (\s@Snapshot' {} a -> s {nodeType = a} :: Snapshot)

-- | For manual snapshots, the AWS customer account used to create or copy
-- the snapshot. For automatic snapshots, the owner of the cluster. The
-- owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
snapshot_ownerAccount :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_ownerAccount = Lens.lens (\Snapshot' {ownerAccount} -> ownerAccount) (\s@Snapshot' {} a -> s {ownerAccount = a} :: Snapshot)

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
snapshot_clusterVersion :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_clusterVersion = Lens.lens (\Snapshot' {clusterVersion} -> clusterVersion) (\s@Snapshot' {} a -> s {clusterVersion = a} :: Snapshot)

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
snapshot_vpcId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_vpcId = Lens.lens (\Snapshot' {vpcId} -> vpcId) (\s@Snapshot' {} a -> s {vpcId = a} :: Snapshot)

-- | The source region from which the snapshot was copied.
snapshot_sourceRegion :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_sourceRegion = Lens.lens (\Snapshot' {sourceRegion} -> sourceRegion) (\s@Snapshot' {} a -> s {sourceRegion = a} :: Snapshot)

-- | A timestamp representing the start of the retention period for the
-- snapshot.
snapshot_snapshotRetentionStartTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
snapshot_snapshotRetentionStartTime = Lens.lens (\Snapshot' {snapshotRetentionStartTime} -> snapshotRetentionStartTime) (\s@Snapshot' {} a -> s {snapshotRetentionStartTime = a} :: Snapshot) Core.. Lens.mapping Core._Time

-- | The name of the maintenance track for the snapshot.
snapshot_maintenanceTrackName :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_maintenanceTrackName = Lens.lens (\Snapshot' {maintenanceTrackName} -> maintenanceTrackName) (\s@Snapshot' {} a -> s {maintenanceTrackName = a} :: Snapshot)

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Core.<$> (x Core..@? "EnhancedVpcRouting")
      Core.<*> (x Core..@? "SnapshotIdentifier")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "EstimatedSecondsToCompletion")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "ManualSnapshotRemainingDays")
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "CurrentBackupRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "EncryptedWithHSM")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "EngineFullVersion")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> ( x Core..@? "RestorableNodeTypes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "NodeType")
               )
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> ( x Core..@? "AccountsWithRestoreAccess"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "AccountWithRestoreAccess")
               )
      Core.<*> (x Core..@? "ActualIncrementalBackupSizeInMegaBytes")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "TotalBackupSizeInMegaBytes")
      Core.<*> (x Core..@? "DBName")
      Core.<*> (x Core..@? "BackupProgressInMegaBytes")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "OwnerAccount")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "SourceRegion")
      Core.<*> (x Core..@? "SnapshotRetentionStartTime")
      Core.<*> (x Core..@? "MaintenanceTrackName")

instance Core.Hashable Snapshot

instance Core.NFData Snapshot
