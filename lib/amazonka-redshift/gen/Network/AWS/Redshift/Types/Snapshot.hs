{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Snapshot
  ( Snapshot (..),

    -- * Smart constructor
    mkSnapshot,

    -- * Lenses
    sStatus,
    sRestorableNodeTypes,
    sAccountsWithRestoreAccess,
    sManualSnapshotRetentionPeriod,
    sEnhancedVPCRouting,
    sSnapshotIdentifier,
    sEncryptedWithHSM,
    sMasterUsername,
    sSourceRegion,
    sMaintenanceTrackName,
    sSnapshotRetentionStartTime,
    sManualSnapshotRemainingDays,
    sVPCId,
    sBackupProgressInMegaBytes,
    sEncrypted,
    sClusterIdentifier,
    sNumberOfNodes,
    sSnapshotType,
    sKMSKeyId,
    sAvailabilityZone,
    sCurrentBackupRateInMegaBytesPerSecond,
    sSnapshotCreateTime,
    sClusterVersion,
    sOwnerAccount,
    sNodeType,
    sElapsedTimeInSeconds,
    sClusterCreateTime,
    sEstimatedSecondsToCompletion,
    sActualIncrementalBackupSizeInMegaBytes,
    sTags,
    sPort,
    sTotalBackupSizeInMegaBytes,
    sDBName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.Tag

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { status :: Lude.Maybe Lude.Text,
    restorableNodeTypes :: Lude.Maybe [Lude.Text],
    accountsWithRestoreAccess :: Lude.Maybe [AccountWithRestoreAccess],
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    enhancedVPCRouting :: Lude.Maybe Lude.Bool,
    snapshotIdentifier :: Lude.Maybe Lude.Text,
    encryptedWithHSM :: Lude.Maybe Lude.Bool,
    masterUsername :: Lude.Maybe Lude.Text,
    sourceRegion :: Lude.Maybe Lude.Text,
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    snapshotRetentionStartTime :: Lude.Maybe Lude.DateTime,
    manualSnapshotRemainingDays :: Lude.Maybe Lude.Int,
    vpcId :: Lude.Maybe Lude.Text,
    backupProgressInMegaBytes :: Lude.Maybe Lude.Double,
    encrypted :: Lude.Maybe Lude.Bool,
    clusterIdentifier :: Lude.Maybe Lude.Text,
    numberOfNodes :: Lude.Maybe Lude.Int,
    snapshotType :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    currentBackupRateInMegaBytesPerSecond :: Lude.Maybe Lude.Double,
    snapshotCreateTime :: Lude.Maybe Lude.DateTime,
    clusterVersion :: Lude.Maybe Lude.Text,
    ownerAccount :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text,
    elapsedTimeInSeconds :: Lude.Maybe Lude.Integer,
    clusterCreateTime :: Lude.Maybe Lude.DateTime,
    estimatedSecondsToCompletion :: Lude.Maybe Lude.Integer,
    actualIncrementalBackupSizeInMegaBytes :: Lude.Maybe Lude.Double,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    totalBackupSizeInMegaBytes :: Lude.Maybe Lude.Double,
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'accountsWithRestoreAccess' - A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
-- * 'actualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup.
-- * 'availabilityZone' - The Availability Zone in which the cluster was created.
-- * 'backupProgressInMegaBytes' - The number of megabytes that have been transferred to the snapshot backup.
-- * 'clusterCreateTime' - The time (UTC) when the cluster was originally created.
-- * 'clusterIdentifier' - The identifier of the cluster for which the snapshot was taken.
-- * 'clusterVersion' - The version ID of the Amazon Redshift engine that is running on the cluster.
-- * 'currentBackupRateInMegaBytesPerSecond' - The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
-- * 'dbName' - The name of the database that was created when the cluster was created.
-- * 'elapsedTimeInSeconds' - The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
-- * 'encrypted' - If @true@ , the data in the snapshot is encrypted at rest.
-- * 'encryptedWithHSM' - A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
-- * 'enhancedVPCRouting' - An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
-- * 'estimatedSecondsToCompletion' - The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
-- * 'kmsKeyId' - The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
-- * 'maintenanceTrackName' - The name of the maintenance track for the snapshot.
-- * 'manualSnapshotRemainingDays' - The number of days until a manual snapshot will pass its retention period.
-- * 'manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'masterUsername' - The master user name for the cluster.
-- * 'nodeType' - The node type of the nodes in the cluster.
-- * 'numberOfNodes' - The number of nodes in the cluster.
-- * 'ownerAccount' - For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
-- * 'port' - The port that the cluster is listening on.
-- * 'restorableNodeTypes' - The list of node types that this cluster snapshot is able to restore into.
-- * 'snapshotCreateTime' - The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
-- * 'snapshotIdentifier' - The snapshot identifier that is provided in the request.
-- * 'snapshotRetentionStartTime' - A timestamp representing the start of the retention period for the snapshot.
-- * 'snapshotType' - The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
-- * 'sourceRegion' - The source region from which the snapshot was copied.
-- * 'status' - The snapshot status. The value of the status depends on the API operation used:
--
--
--     * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".
--
--
--     * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".
--
--
--     * 'DeleteClusterSnapshot' returns status as "deleted".
--
--
-- * 'tags' - The list of tags for the cluster snapshot.
-- * 'totalBackupSizeInMegaBytes' - The size of the complete set of backup data that would be used to restore the cluster.
-- * 'vpcId' - The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { status = Lude.Nothing,
      restorableNodeTypes = Lude.Nothing,
      accountsWithRestoreAccess = Lude.Nothing,
      manualSnapshotRetentionPeriod = Lude.Nothing,
      enhancedVPCRouting = Lude.Nothing,
      snapshotIdentifier = Lude.Nothing,
      encryptedWithHSM = Lude.Nothing,
      masterUsername = Lude.Nothing,
      sourceRegion = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      snapshotRetentionStartTime = Lude.Nothing,
      manualSnapshotRemainingDays = Lude.Nothing,
      vpcId = Lude.Nothing,
      backupProgressInMegaBytes = Lude.Nothing,
      encrypted = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      snapshotType = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      currentBackupRateInMegaBytesPerSecond = Lude.Nothing,
      snapshotCreateTime = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      nodeType = Lude.Nothing,
      elapsedTimeInSeconds = Lude.Nothing,
      clusterCreateTime = Lude.Nothing,
      estimatedSecondsToCompletion = Lude.Nothing,
      actualIncrementalBackupSizeInMegaBytes = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing,
      totalBackupSizeInMegaBytes = Lude.Nothing,
      dbName = Lude.Nothing
    }

-- | The snapshot status. The value of the status depends on the API operation used:
--
--
--     * 'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as "creating".
--
--
--     * 'DescribeClusterSnapshots' returns status as "creating", "available", "final snapshot", or "failed".
--
--
--     * 'DeleteClusterSnapshot' returns status as "deleted".
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sStatus = Lens.lens (status :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Snapshot)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of node types that this cluster snapshot is able to restore into.
--
-- /Note:/ Consider using 'restorableNodeTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRestorableNodeTypes :: Lens.Lens' Snapshot (Lude.Maybe [Lude.Text])
sRestorableNodeTypes = Lens.lens (restorableNodeTypes :: Snapshot -> Lude.Maybe [Lude.Text]) (\s a -> s {restorableNodeTypes = a} :: Snapshot)
{-# DEPRECATED sRestorableNodeTypes "Use generic-lens or generic-optics with 'restorableNodeTypes' instead." #-}

-- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
--
-- /Note:/ Consider using 'accountsWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountsWithRestoreAccess :: Lens.Lens' Snapshot (Lude.Maybe [AccountWithRestoreAccess])
sAccountsWithRestoreAccess = Lens.lens (accountsWithRestoreAccess :: Snapshot -> Lude.Maybe [AccountWithRestoreAccess]) (\s a -> s {accountsWithRestoreAccess = a} :: Snapshot)
{-# DEPRECATED sAccountsWithRestoreAccess "Use generic-lens or generic-optics with 'accountsWithRestoreAccess' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: Snapshot)
{-# DEPRECATED sManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVPCRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnhancedVPCRouting :: Lens.Lens' Snapshot (Lude.Maybe Lude.Bool)
sEnhancedVPCRouting = Lens.lens (enhancedVPCRouting :: Snapshot -> Lude.Maybe Lude.Bool) (\s a -> s {enhancedVPCRouting = a} :: Snapshot)
{-# DEPRECATED sEnhancedVPCRouting "Use generic-lens or generic-optics with 'enhancedVPCRouting' instead." #-}

-- | The snapshot identifier that is provided in the request.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotIdentifier :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotIdentifier = Lens.lens (snapshotIdentifier :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotIdentifier = a} :: Snapshot)
{-# DEPRECATED sSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
--
-- /Note:/ Consider using 'encryptedWithHSM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptedWithHSM :: Lens.Lens' Snapshot (Lude.Maybe Lude.Bool)
sEncryptedWithHSM = Lens.lens (encryptedWithHSM :: Snapshot -> Lude.Maybe Lude.Bool) (\s a -> s {encryptedWithHSM = a} :: Snapshot)
{-# DEPRECATED sEncryptedWithHSM "Use generic-lens or generic-optics with 'encryptedWithHSM' instead." #-}

-- | The master user name for the cluster.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMasterUsername :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sMasterUsername = Lens.lens (masterUsername :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: Snapshot)
{-# DEPRECATED sMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The source region from which the snapshot was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceRegion :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSourceRegion = Lens.lens (sourceRegion :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceRegion = a} :: Snapshot)
{-# DEPRECATED sSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The name of the maintenance track for the snapshot.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaintenanceTrackName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sMaintenanceTrackName = Lens.lens (maintenanceTrackName :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: Snapshot)
{-# DEPRECATED sMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | A timestamp representing the start of the retention period for the snapshot.
--
-- /Note:/ Consider using 'snapshotRetentionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotRetentionStartTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.DateTime)
sSnapshotRetentionStartTime = Lens.lens (snapshotRetentionStartTime :: Snapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {snapshotRetentionStartTime = a} :: Snapshot)
{-# DEPRECATED sSnapshotRetentionStartTime "Use generic-lens or generic-optics with 'snapshotRetentionStartTime' instead." #-}

-- | The number of days until a manual snapshot will pass its retention period.
--
-- /Note:/ Consider using 'manualSnapshotRemainingDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRemainingDays :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sManualSnapshotRemainingDays = Lens.lens (manualSnapshotRemainingDays :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRemainingDays = a} :: Snapshot)
{-# DEPRECATED sManualSnapshotRemainingDays "Use generic-lens or generic-optics with 'manualSnapshotRemainingDays' instead." #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVPCId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sVPCId = Lens.lens (vpcId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Snapshot)
{-# DEPRECATED sVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
--
-- /Note:/ Consider using 'backupProgressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBackupProgressInMegaBytes :: Lens.Lens' Snapshot (Lude.Maybe Lude.Double)
sBackupProgressInMegaBytes = Lens.lens (backupProgressInMegaBytes :: Snapshot -> Lude.Maybe Lude.Double) (\s a -> s {backupProgressInMegaBytes = a} :: Snapshot)
{-# DEPRECATED sBackupProgressInMegaBytes "Use generic-lens or generic-optics with 'backupProgressInMegaBytes' instead." #-}

-- | If @true@ , the data in the snapshot is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot (Lude.Maybe Lude.Bool)
sEncrypted = Lens.lens (encrypted :: Snapshot -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: Snapshot)
{-# DEPRECATED sEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The identifier of the cluster for which the snapshot was taken.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterIdentifier :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sClusterIdentifier = Lens.lens (clusterIdentifier :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: Snapshot)
{-# DEPRECATED sClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumberOfNodes :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sNumberOfNodes = Lens.lens (numberOfNodes :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: Snapshot)
{-# DEPRECATED sNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotType :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sSnapshotType = Lens.lens (snapshotType :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: Snapshot)
{-# DEPRECATED sSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKMSKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sKMSKeyId = Lens.lens (kmsKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Snapshot)
{-# DEPRECATED sKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone in which the cluster was created.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZone :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sAvailabilityZone = Lens.lens (availabilityZone :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Snapshot)
{-# DEPRECATED sAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
--
-- /Note:/ Consider using 'currentBackupRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCurrentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Lude.Maybe Lude.Double)
sCurrentBackupRateInMegaBytesPerSecond = Lens.lens (currentBackupRateInMegaBytesPerSecond :: Snapshot -> Lude.Maybe Lude.Double) (\s a -> s {currentBackupRateInMegaBytesPerSecond = a} :: Snapshot)
{-# DEPRECATED sCurrentBackupRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentBackupRateInMegaBytesPerSecond' instead." #-}

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotCreateTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.DateTime)
sSnapshotCreateTime = Lens.lens (snapshotCreateTime :: Snapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {snapshotCreateTime = a} :: Snapshot)
{-# DEPRECATED sSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterVersion :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sClusterVersion = Lens.lens (clusterVersion :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: Snapshot)
{-# DEPRECATED sClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAccount :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sOwnerAccount = Lens.lens (ownerAccount :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: Snapshot)
{-# DEPRECATED sOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The node type of the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNodeType :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sNodeType = Lens.lens (nodeType :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: Snapshot)
{-# DEPRECATED sNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sElapsedTimeInSeconds :: Lens.Lens' Snapshot (Lude.Maybe Lude.Integer)
sElapsedTimeInSeconds = Lens.lens (elapsedTimeInSeconds :: Snapshot -> Lude.Maybe Lude.Integer) (\s a -> s {elapsedTimeInSeconds = a} :: Snapshot)
{-# DEPRECATED sElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

-- | The time (UTC) when the cluster was originally created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterCreateTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.DateTime)
sClusterCreateTime = Lens.lens (clusterCreateTime :: Snapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {clusterCreateTime = a} :: Snapshot)
{-# DEPRECATED sClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
--
-- /Note:/ Consider using 'estimatedSecondsToCompletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEstimatedSecondsToCompletion :: Lens.Lens' Snapshot (Lude.Maybe Lude.Integer)
sEstimatedSecondsToCompletion = Lens.lens (estimatedSecondsToCompletion :: Snapshot -> Lude.Maybe Lude.Integer) (\s a -> s {estimatedSecondsToCompletion = a} :: Snapshot)
{-# DEPRECATED sEstimatedSecondsToCompletion "Use generic-lens or generic-optics with 'estimatedSecondsToCompletion' instead." #-}

-- | The size of the incremental backup.
--
-- /Note:/ Consider using 'actualIncrementalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Lude.Maybe Lude.Double)
sActualIncrementalBackupSizeInMegaBytes = Lens.lens (actualIncrementalBackupSizeInMegaBytes :: Snapshot -> Lude.Maybe Lude.Double) (\s a -> s {actualIncrementalBackupSizeInMegaBytes = a} :: Snapshot)
{-# DEPRECATED sActualIncrementalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'actualIncrementalBackupSizeInMegaBytes' instead." #-}

-- | The list of tags for the cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Lude.Maybe [Tag])
sTags = Lens.lens (tags :: Snapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Snapshot)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port that the cluster is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPort :: Lens.Lens' Snapshot (Lude.Maybe Lude.Int)
sPort = Lens.lens (port :: Snapshot -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Snapshot)
{-# DEPRECATED sPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The size of the complete set of backup data that would be used to restore the cluster.
--
-- /Note:/ Consider using 'totalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Lude.Maybe Lude.Double)
sTotalBackupSizeInMegaBytes = Lens.lens (totalBackupSizeInMegaBytes :: Snapshot -> Lude.Maybe Lude.Double) (\s a -> s {totalBackupSizeInMegaBytes = a} :: Snapshot)
{-# DEPRECATED sTotalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'totalBackupSizeInMegaBytes' instead." #-}

-- | The name of the database that was created when the cluster was created.
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDBName :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sDBName = Lens.lens (dbName :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: Snapshot)
{-# DEPRECATED sDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> ( x Lude..@? "RestorableNodeTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeType")
               )
      Lude.<*> ( x Lude..@? "AccountsWithRestoreAccess" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AccountWithRestoreAccess")
               )
      Lude.<*> (x Lude..@? "ManualSnapshotRetentionPeriod")
      Lude.<*> (x Lude..@? "EnhancedVpcRouting")
      Lude.<*> (x Lude..@? "SnapshotIdentifier")
      Lude.<*> (x Lude..@? "EncryptedWithHSM")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "SourceRegion")
      Lude.<*> (x Lude..@? "MaintenanceTrackName")
      Lude.<*> (x Lude..@? "SnapshotRetentionStartTime")
      Lude.<*> (x Lude..@? "ManualSnapshotRemainingDays")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "BackupProgressInMegaBytes")
      Lude.<*> (x Lude..@? "Encrypted")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> (x Lude..@? "NumberOfNodes")
      Lude.<*> (x Lude..@? "SnapshotType")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> (x Lude..@? "CurrentBackupRateInMegaBytesPerSecond")
      Lude.<*> (x Lude..@? "SnapshotCreateTime")
      Lude.<*> (x Lude..@? "ClusterVersion")
      Lude.<*> (x Lude..@? "OwnerAccount")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@? "ElapsedTimeInSeconds")
      Lude.<*> (x Lude..@? "ClusterCreateTime")
      Lude.<*> (x Lude..@? "EstimatedSecondsToCompletion")
      Lude.<*> (x Lude..@? "ActualIncrementalBackupSizeInMegaBytes")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@? "TotalBackupSizeInMegaBytes")
      Lude.<*> (x Lude..@? "DBName")
