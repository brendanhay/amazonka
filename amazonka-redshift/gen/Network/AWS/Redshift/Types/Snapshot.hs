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
-- Module      : Network.AWS.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Snapshot where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The snapshot identifier that is provided in the request.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
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
    -- | The estimate of the time remaining before the snapshot backup will
    -- complete. Returns @0@ for a completed backup.
    estimatedSecondsToCompletion :: Prelude.Maybe Prelude.Integer,
    -- | If @true@, the data in the snapshot is encrypted at rest.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The time (UTC) when the cluster was originally created.
    clusterCreateTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The number of days until a manual snapshot will pass its retention
    -- period.
    manualSnapshotRemainingDays :: Prelude.Maybe Prelude.Int,
    -- | The time (in UTC format) when Amazon Redshift began the snapshot. A
    -- snapshot contains a copy of the cluster data as of this exact time.
    snapshotCreateTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The number of megabytes per second being transferred to the snapshot
    -- backup. Returns @0@ for a completed backup.
    currentBackupRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The master user name for the cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | A boolean that indicates whether the snapshot data is encrypted using
    -- the HSM keys of the source cluster. @true@ indicates that the data is
    -- encrypted using HSM keys.
    encryptedWithHSM :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that
    -- was used to encrypt data in the cluster from which the snapshot was
    -- taken.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The cluster version of the cluster used to create the snapshot. For
    -- example, 1.0.15503.
    engineFullVersion :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the cluster was created.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The list of node types that this cluster snapshot is able to restore
    -- into.
    restorableNodeTypes :: Prelude.Maybe [Prelude.Text],
    -- | The snapshot type. Snapshots created using CreateClusterSnapshot and
    -- CopyClusterSnapshot are of type \"manual\".
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | A list of the AWS customer accounts authorized to restore the snapshot.
    -- Returns @null@ if no accounts are authorized. Visible only to the
    -- snapshot owner.
    accountsWithRestoreAccess :: Prelude.Maybe [AccountWithRestoreAccess],
    -- | The size of the incremental backup.
    actualIncrementalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The identifier of the cluster for which the snapshot was taken.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the cluster snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The number of nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The port that the cluster is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | The size of the complete set of backup data that would be used to
    -- restore the cluster.
    totalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The name of the database that was created when the cluster was created.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The number of megabytes that have been transferred to the snapshot
    -- backup.
    backupProgressInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The amount of time an in-progress snapshot backup has been running, or
    -- the amount of time it took a completed backup to finish.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The node type of the nodes in the cluster.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | For manual snapshots, the AWS customer account used to create or copy
    -- the snapshot. For automatic snapshots, the owner of the cluster. The
    -- owner can perform all snapshot actions, such as sharing a manual
    -- snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the Amazon Redshift engine that is running on the
    -- cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The VPC identifier of the cluster if the snapshot is from a cluster in a
    -- VPC. Otherwise, this field is not in the output.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The source region from which the snapshot was copied.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp representing the start of the retention period for the
    -- snapshot.
    snapshotRetentionStartTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the maintenance track for the snapshot.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { enhancedVpcRouting = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      status = Prelude.Nothing,
      estimatedSecondsToCompletion = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      manualSnapshotRemainingDays = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      currentBackupRateInMegaBytesPerSecond =
        Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      encryptedWithHSM = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engineFullVersion = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      restorableNodeTypes = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      accountsWithRestoreAccess = Prelude.Nothing,
      actualIncrementalBackupSizeInMegaBytes =
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      tags = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      port = Prelude.Nothing,
      totalBackupSizeInMegaBytes = Prelude.Nothing,
      dbName = Prelude.Nothing,
      backupProgressInMegaBytes = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      snapshotRetentionStartTime = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing
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
snapshot_enhancedVpcRouting :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_enhancedVpcRouting = Lens.lens (\Snapshot' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@Snapshot' {} a -> s {enhancedVpcRouting = a} :: Snapshot)

-- | The snapshot identifier that is provided in the request.
snapshot_snapshotIdentifier :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
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
snapshot_status :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The estimate of the time remaining before the snapshot backup will
-- complete. Returns @0@ for a completed backup.
snapshot_estimatedSecondsToCompletion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_estimatedSecondsToCompletion = Lens.lens (\Snapshot' {estimatedSecondsToCompletion} -> estimatedSecondsToCompletion) (\s@Snapshot' {} a -> s {estimatedSecondsToCompletion = a} :: Snapshot)

-- | If @true@, the data in the snapshot is encrypted at rest.
snapshot_encrypted :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

-- | The time (UTC) when the cluster was originally created.
snapshot_clusterCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_clusterCreateTime = Lens.lens (\Snapshot' {clusterCreateTime} -> clusterCreateTime) (\s@Snapshot' {} a -> s {clusterCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Time

-- | The number of days until a manual snapshot will pass its retention
-- period.
snapshot_manualSnapshotRemainingDays :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_manualSnapshotRemainingDays = Lens.lens (\Snapshot' {manualSnapshotRemainingDays} -> manualSnapshotRemainingDays) (\s@Snapshot' {} a -> s {manualSnapshotRemainingDays = a} :: Snapshot)

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A
-- snapshot contains a copy of the cluster data as of this exact time.
snapshot_snapshotCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotCreateTime = Lens.lens (\Snapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@Snapshot' {} a -> s {snapshotCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Time

-- | The number of megabytes per second being transferred to the snapshot
-- backup. Returns @0@ for a completed backup.
snapshot_currentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_currentBackupRateInMegaBytesPerSecond = Lens.lens (\Snapshot' {currentBackupRateInMegaBytesPerSecond} -> currentBackupRateInMegaBytesPerSecond) (\s@Snapshot' {} a -> s {currentBackupRateInMegaBytesPerSecond = a} :: Snapshot)

-- | The master user name for the cluster.
snapshot_masterUsername :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_masterUsername = Lens.lens (\Snapshot' {masterUsername} -> masterUsername) (\s@Snapshot' {} a -> s {masterUsername = a} :: Snapshot)

-- | A boolean that indicates whether the snapshot data is encrypted using
-- the HSM keys of the source cluster. @true@ indicates that the data is
-- encrypted using HSM keys.
snapshot_encryptedWithHSM :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Bool)
snapshot_encryptedWithHSM = Lens.lens (\Snapshot' {encryptedWithHSM} -> encryptedWithHSM) (\s@Snapshot' {} a -> s {encryptedWithHSM = a} :: Snapshot)

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
snapshot_manualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_manualSnapshotRetentionPeriod = Lens.lens (\Snapshot' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@Snapshot' {} a -> s {manualSnapshotRetentionPeriod = a} :: Snapshot)

-- | The AWS Key Management Service (KMS) key ID of the encryption key that
-- was used to encrypt data in the cluster from which the snapshot was
-- taken.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The cluster version of the cluster used to create the snapshot. For
-- example, 1.0.15503.
snapshot_engineFullVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_engineFullVersion = Lens.lens (\Snapshot' {engineFullVersion} -> engineFullVersion) (\s@Snapshot' {} a -> s {engineFullVersion = a} :: Snapshot)

-- | The Availability Zone in which the cluster was created.
snapshot_availabilityZone :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_availabilityZone = Lens.lens (\Snapshot' {availabilityZone} -> availabilityZone) (\s@Snapshot' {} a -> s {availabilityZone = a} :: Snapshot)

-- | The list of node types that this cluster snapshot is able to restore
-- into.
snapshot_restorableNodeTypes :: Lens.Lens' Snapshot (Prelude.Maybe [Prelude.Text])
snapshot_restorableNodeTypes = Lens.lens (\Snapshot' {restorableNodeTypes} -> restorableNodeTypes) (\s@Snapshot' {} a -> s {restorableNodeTypes = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The snapshot type. Snapshots created using CreateClusterSnapshot and
-- CopyClusterSnapshot are of type \"manual\".
snapshot_snapshotType :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotType = Lens.lens (\Snapshot' {snapshotType} -> snapshotType) (\s@Snapshot' {} a -> s {snapshotType = a} :: Snapshot)

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns @null@ if no accounts are authorized. Visible only to the
-- snapshot owner.
snapshot_accountsWithRestoreAccess :: Lens.Lens' Snapshot (Prelude.Maybe [AccountWithRestoreAccess])
snapshot_accountsWithRestoreAccess = Lens.lens (\Snapshot' {accountsWithRestoreAccess} -> accountsWithRestoreAccess) (\s@Snapshot' {} a -> s {accountsWithRestoreAccess = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The size of the incremental backup.
snapshot_actualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_actualIncrementalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {actualIncrementalBackupSizeInMegaBytes} -> actualIncrementalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {actualIncrementalBackupSizeInMegaBytes = a} :: Snapshot)

-- | The identifier of the cluster for which the snapshot was taken.
snapshot_clusterIdentifier :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_clusterIdentifier = Lens.lens (\Snapshot' {clusterIdentifier} -> clusterIdentifier) (\s@Snapshot' {} a -> s {clusterIdentifier = a} :: Snapshot)

-- | The list of tags for the cluster snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of nodes in the cluster.
snapshot_numberOfNodes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_numberOfNodes = Lens.lens (\Snapshot' {numberOfNodes} -> numberOfNodes) (\s@Snapshot' {} a -> s {numberOfNodes = a} :: Snapshot)

-- | The port that the cluster is listening on.
snapshot_port :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_port = Lens.lens (\Snapshot' {port} -> port) (\s@Snapshot' {} a -> s {port = a} :: Snapshot)

-- | The size of the complete set of backup data that would be used to
-- restore the cluster.
snapshot_totalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_totalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {totalBackupSizeInMegaBytes = a} :: Snapshot)

-- | The name of the database that was created when the cluster was created.
snapshot_dbName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_dbName = Lens.lens (\Snapshot' {dbName} -> dbName) (\s@Snapshot' {} a -> s {dbName = a} :: Snapshot)

-- | The number of megabytes that have been transferred to the snapshot
-- backup.
snapshot_backupProgressInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_backupProgressInMegaBytes = Lens.lens (\Snapshot' {backupProgressInMegaBytes} -> backupProgressInMegaBytes) (\s@Snapshot' {} a -> s {backupProgressInMegaBytes = a} :: Snapshot)

-- | The amount of time an in-progress snapshot backup has been running, or
-- the amount of time it took a completed backup to finish.
snapshot_elapsedTimeInSeconds :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_elapsedTimeInSeconds = Lens.lens (\Snapshot' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@Snapshot' {} a -> s {elapsedTimeInSeconds = a} :: Snapshot)

-- | The node type of the nodes in the cluster.
snapshot_nodeType :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_nodeType = Lens.lens (\Snapshot' {nodeType} -> nodeType) (\s@Snapshot' {} a -> s {nodeType = a} :: Snapshot)

-- | For manual snapshots, the AWS customer account used to create or copy
-- the snapshot. For automatic snapshots, the owner of the cluster. The
-- owner can perform all snapshot actions, such as sharing a manual
-- snapshot.
snapshot_ownerAccount :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_ownerAccount = Lens.lens (\Snapshot' {ownerAccount} -> ownerAccount) (\s@Snapshot' {} a -> s {ownerAccount = a} :: Snapshot)

-- | The version ID of the Amazon Redshift engine that is running on the
-- cluster.
snapshot_clusterVersion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_clusterVersion = Lens.lens (\Snapshot' {clusterVersion} -> clusterVersion) (\s@Snapshot' {} a -> s {clusterVersion = a} :: Snapshot)

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a
-- VPC. Otherwise, this field is not in the output.
snapshot_vpcId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_vpcId = Lens.lens (\Snapshot' {vpcId} -> vpcId) (\s@Snapshot' {} a -> s {vpcId = a} :: Snapshot)

-- | The source region from which the snapshot was copied.
snapshot_sourceRegion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_sourceRegion = Lens.lens (\Snapshot' {sourceRegion} -> sourceRegion) (\s@Snapshot' {} a -> s {sourceRegion = a} :: Snapshot)

-- | A timestamp representing the start of the retention period for the
-- snapshot.
snapshot_snapshotRetentionStartTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotRetentionStartTime = Lens.lens (\Snapshot' {snapshotRetentionStartTime} -> snapshotRetentionStartTime) (\s@Snapshot' {} a -> s {snapshotRetentionStartTime = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Time

-- | The name of the maintenance track for the snapshot.
snapshot_maintenanceTrackName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_maintenanceTrackName = Lens.lens (\Snapshot' {maintenanceTrackName} -> maintenanceTrackName) (\s@Snapshot' {} a -> s {maintenanceTrackName = a} :: Snapshot)

instance Prelude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> (x Prelude..@? "EnhancedVpcRouting")
      Prelude.<*> (x Prelude..@? "SnapshotIdentifier")
      Prelude.<*> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "EstimatedSecondsToCompletion")
      Prelude.<*> (x Prelude..@? "Encrypted")
      Prelude.<*> (x Prelude..@? "ClusterCreateTime")
      Prelude.<*> (x Prelude..@? "ManualSnapshotRemainingDays")
      Prelude.<*> (x Prelude..@? "SnapshotCreateTime")
      Prelude.<*> ( x
                      Prelude..@? "CurrentBackupRateInMegaBytesPerSecond"
                  )
      Prelude.<*> (x Prelude..@? "MasterUsername")
      Prelude.<*> (x Prelude..@? "EncryptedWithHSM")
      Prelude.<*> (x Prelude..@? "ManualSnapshotRetentionPeriod")
      Prelude.<*> (x Prelude..@? "KmsKeyId")
      Prelude.<*> (x Prelude..@? "EngineFullVersion")
      Prelude.<*> (x Prelude..@? "AvailabilityZone")
      Prelude.<*> ( x Prelude..@? "RestorableNodeTypes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "NodeType")
                  )
      Prelude.<*> (x Prelude..@? "SnapshotType")
      Prelude.<*> ( x Prelude..@? "AccountsWithRestoreAccess"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "AccountWithRestoreAccess")
                  )
      Prelude.<*> ( x
                      Prelude..@? "ActualIncrementalBackupSizeInMegaBytes"
                  )
      Prelude.<*> (x Prelude..@? "ClusterIdentifier")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )
      Prelude.<*> (x Prelude..@? "NumberOfNodes")
      Prelude.<*> (x Prelude..@? "Port")
      Prelude.<*> (x Prelude..@? "TotalBackupSizeInMegaBytes")
      Prelude.<*> (x Prelude..@? "DBName")
      Prelude.<*> (x Prelude..@? "BackupProgressInMegaBytes")
      Prelude.<*> (x Prelude..@? "ElapsedTimeInSeconds")
      Prelude.<*> (x Prelude..@? "NodeType")
      Prelude.<*> (x Prelude..@? "OwnerAccount")
      Prelude.<*> (x Prelude..@? "ClusterVersion")
      Prelude.<*> (x Prelude..@? "VpcId")
      Prelude.<*> (x Prelude..@? "SourceRegion")
      Prelude.<*> (x Prelude..@? "SnapshotRetentionStartTime")
      Prelude.<*> (x Prelude..@? "MaintenanceTrackName")

instance Prelude.Hashable Snapshot

instance Prelude.NFData Snapshot
