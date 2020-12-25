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
    sAccountsWithRestoreAccess,
    sActualIncrementalBackupSizeInMegaBytes,
    sAvailabilityZone,
    sBackupProgressInMegaBytes,
    sClusterCreateTime,
    sClusterIdentifier,
    sClusterVersion,
    sCurrentBackupRateInMegaBytesPerSecond,
    sDBName,
    sElapsedTimeInSeconds,
    sEncrypted,
    sEncryptedWithHSM,
    sEnhancedVpcRouting,
    sEstimatedSecondsToCompletion,
    sKmsKeyId,
    sMaintenanceTrackName,
    sManualSnapshotRemainingDays,
    sManualSnapshotRetentionPeriod,
    sMasterUsername,
    sNodeType,
    sNumberOfNodes,
    sOwnerAccount,
    sPort,
    sRestorableNodeTypes,
    sSnapshotCreateTime,
    sSnapshotIdentifier,
    sSnapshotRetentionStartTime,
    sSnapshotType,
    sSourceRegion,
    sStatus,
    sTags,
    sTotalBackupSizeInMegaBytes,
    sVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AccountWithRestoreAccess as Types
import qualified Network.AWS.Redshift.Types.ClusterIdentifier as Types
import qualified Network.AWS.Redshift.Types.DBName as Types
import qualified Network.AWS.Redshift.Types.KmsKeyId as Types
import qualified Network.AWS.Redshift.Types.MaintenanceTrackName as Types
import qualified Network.AWS.Redshift.Types.MasterUsername as Types
import qualified Network.AWS.Redshift.Types.NodeType as Types
import qualified Network.AWS.Redshift.Types.OwnerAccount as Types
import qualified Network.AWS.Redshift.Types.SnapshotIdentifier as Types
import qualified Network.AWS.Redshift.Types.SnapshotType as Types
import qualified Network.AWS.Redshift.Types.SourceRegion as Types
import qualified Network.AWS.Redshift.Types.Status as Types
import qualified Network.AWS.Redshift.Types.String as Types
import qualified Network.AWS.Redshift.Types.Tag as Types
import qualified Network.AWS.Redshift.Types.VpcId as Types

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
    accountsWithRestoreAccess :: Core.Maybe [Types.AccountWithRestoreAccess],
    -- | The size of the incremental backup.
    actualIncrementalBackupSizeInMegaBytes :: Core.Maybe Core.Double,
    -- | The Availability Zone in which the cluster was created.
    availabilityZone :: Core.Maybe Types.String,
    -- | The number of megabytes that have been transferred to the snapshot backup.
    backupProgressInMegaBytes :: Core.Maybe Core.Double,
    -- | The time (UTC) when the cluster was originally created.
    clusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | The identifier of the cluster for which the snapshot was taken.
    clusterIdentifier :: Core.Maybe Types.ClusterIdentifier,
    -- | The version ID of the Amazon Redshift engine that is running on the cluster.
    clusterVersion :: Core.Maybe Types.String,
    -- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
    currentBackupRateInMegaBytesPerSecond :: Core.Maybe Core.Double,
    -- | The name of the database that was created when the cluster was created.
    dBName :: Core.Maybe Types.DBName,
    -- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
    elapsedTimeInSeconds :: Core.Maybe Core.Integer,
    -- | If @true@ , the data in the snapshot is encrypted at rest.
    encrypted :: Core.Maybe Core.Bool,
    -- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
    encryptedWithHSM :: Core.Maybe Core.Bool,
    -- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
    --
    -- If this option is @true@ , enhanced VPC routing is enabled.
    -- Default: false
    enhancedVpcRouting :: Core.Maybe Core.Bool,
    -- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
    estimatedSecondsToCompletion :: Core.Maybe Core.Integer,
    -- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of the maintenance track for the snapshot.
    maintenanceTrackName :: Core.Maybe Types.MaintenanceTrackName,
    -- | The number of days until a manual snapshot will pass its retention period.
    manualSnapshotRemainingDays :: Core.Maybe Core.Int,
    -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The master user name for the cluster.
    masterUsername :: Core.Maybe Types.MasterUsername,
    -- | The node type of the nodes in the cluster.
    nodeType :: Core.Maybe Types.NodeType,
    -- | The number of nodes in the cluster.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
    ownerAccount :: Core.Maybe Types.OwnerAccount,
    -- | The port that the cluster is listening on.
    port :: Core.Maybe Core.Int,
    -- | The list of node types that this cluster snapshot is able to restore into.
    restorableNodeTypes :: Core.Maybe [Types.String],
    -- | The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
    snapshotCreateTime :: Core.Maybe Core.UTCTime,
    -- | The snapshot identifier that is provided in the request.
    snapshotIdentifier :: Core.Maybe Types.SnapshotIdentifier,
    -- | A timestamp representing the start of the retention period for the snapshot.
    snapshotRetentionStartTime :: Core.Maybe Core.UTCTime,
    -- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
    snapshotType :: Core.Maybe Types.SnapshotType,
    -- | The source region from which the snapshot was copied.
    sourceRegion :: Core.Maybe Types.SourceRegion,
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
    status :: Core.Maybe Types.Status,
    -- | The list of tags for the cluster snapshot.
    tags :: Core.Maybe [Types.Tag],
    -- | The size of the complete set of backup data that would be used to restore the cluster.
    totalBackupSizeInMegaBytes :: Core.Maybe Core.Double,
    -- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot ::
  Snapshot
mkSnapshot =
  Snapshot'
    { accountsWithRestoreAccess = Core.Nothing,
      actualIncrementalBackupSizeInMegaBytes = Core.Nothing,
      availabilityZone = Core.Nothing,
      backupProgressInMegaBytes = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      clusterVersion = Core.Nothing,
      currentBackupRateInMegaBytesPerSecond = Core.Nothing,
      dBName = Core.Nothing,
      elapsedTimeInSeconds = Core.Nothing,
      encrypted = Core.Nothing,
      encryptedWithHSM = Core.Nothing,
      enhancedVpcRouting = Core.Nothing,
      estimatedSecondsToCompletion = Core.Nothing,
      kmsKeyId = Core.Nothing,
      maintenanceTrackName = Core.Nothing,
      manualSnapshotRemainingDays = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      masterUsername = Core.Nothing,
      nodeType = Core.Nothing,
      numberOfNodes = Core.Nothing,
      ownerAccount = Core.Nothing,
      port = Core.Nothing,
      restorableNodeTypes = Core.Nothing,
      snapshotCreateTime = Core.Nothing,
      snapshotIdentifier = Core.Nothing,
      snapshotRetentionStartTime = Core.Nothing,
      snapshotType = Core.Nothing,
      sourceRegion = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      totalBackupSizeInMegaBytes = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner.
--
-- /Note:/ Consider using 'accountsWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountsWithRestoreAccess :: Lens.Lens' Snapshot (Core.Maybe [Types.AccountWithRestoreAccess])
sAccountsWithRestoreAccess = Lens.field @"accountsWithRestoreAccess"
{-# DEPRECATED sAccountsWithRestoreAccess "Use generic-lens or generic-optics with 'accountsWithRestoreAccess' instead." #-}

-- | The size of the incremental backup.
--
-- /Note:/ Consider using 'actualIncrementalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sActualIncrementalBackupSizeInMegaBytes = Lens.field @"actualIncrementalBackupSizeInMegaBytes"
{-# DEPRECATED sActualIncrementalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'actualIncrementalBackupSizeInMegaBytes' instead." #-}

-- | The Availability Zone in which the cluster was created.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZone :: Lens.Lens' Snapshot (Core.Maybe Types.String)
sAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED sAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
--
-- /Note:/ Consider using 'backupProgressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBackupProgressInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sBackupProgressInMegaBytes = Lens.field @"backupProgressInMegaBytes"
{-# DEPRECATED sBackupProgressInMegaBytes "Use generic-lens or generic-optics with 'backupProgressInMegaBytes' instead." #-}

-- | The time (UTC) when the cluster was originally created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sClusterCreateTime = Lens.field @"clusterCreateTime"
{-# DEPRECATED sClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | The identifier of the cluster for which the snapshot was taken.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterIdentifier :: Lens.Lens' Snapshot (Core.Maybe Types.ClusterIdentifier)
sClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED sClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterVersion :: Lens.Lens' Snapshot (Core.Maybe Types.String)
sClusterVersion = Lens.field @"clusterVersion"
{-# DEPRECATED sClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup.
--
-- /Note:/ Consider using 'currentBackupRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCurrentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sCurrentBackupRateInMegaBytesPerSecond = Lens.field @"currentBackupRateInMegaBytesPerSecond"
{-# DEPRECATED sCurrentBackupRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentBackupRateInMegaBytesPerSecond' instead." #-}

-- | The name of the database that was created when the cluster was created.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDBName :: Lens.Lens' Snapshot (Core.Maybe Types.DBName)
sDBName = Lens.field @"dBName"
{-# DEPRECATED sDBName "Use generic-lens or generic-optics with 'dBName' instead." #-}

-- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sElapsedTimeInSeconds :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
sElapsedTimeInSeconds = Lens.field @"elapsedTimeInSeconds"
{-# DEPRECATED sElapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead." #-}

-- | If @true@ , the data in the snapshot is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEncrypted = Lens.field @"encrypted"
{-# DEPRECATED sEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
--
-- /Note:/ Consider using 'encryptedWithHSM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptedWithHSM :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEncryptedWithHSM = Lens.field @"encryptedWithHSM"
{-# DEPRECATED sEncryptedWithHSM "Use generic-lens or generic-optics with 'encryptedWithHSM' instead." #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled.
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnhancedVpcRouting :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# DEPRECATED sEnhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead." #-}

-- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup.
--
-- /Note:/ Consider using 'estimatedSecondsToCompletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEstimatedSecondsToCompletion :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
sEstimatedSecondsToCompletion = Lens.field @"estimatedSecondsToCompletion"
{-# DEPRECATED sEstimatedSecondsToCompletion "Use generic-lens or generic-optics with 'estimatedSecondsToCompletion' instead." #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Types.KmsKeyId)
sKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED sKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the maintenance track for the snapshot.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaintenanceTrackName :: Lens.Lens' Snapshot (Core.Maybe Types.MaintenanceTrackName)
sMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# DEPRECATED sMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | The number of days until a manual snapshot will pass its retention period.
--
-- /Note:/ Consider using 'manualSnapshotRemainingDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRemainingDays :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sManualSnapshotRemainingDays = Lens.field @"manualSnapshotRemainingDays"
{-# DEPRECATED sManualSnapshotRemainingDays "Use generic-lens or generic-optics with 'manualSnapshotRemainingDays' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED sManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The master user name for the cluster.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMasterUsername :: Lens.Lens' Snapshot (Core.Maybe Types.MasterUsername)
sMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED sMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The node type of the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNodeType :: Lens.Lens' Snapshot (Core.Maybe Types.NodeType)
sNodeType = Lens.field @"nodeType"
{-# DEPRECATED sNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The number of nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumberOfNodes :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sNumberOfNodes = Lens.field @"numberOfNodes"
{-# DEPRECATED sNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAccount :: Lens.Lens' Snapshot (Core.Maybe Types.OwnerAccount)
sOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED sOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The port that the cluster is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPort :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sPort = Lens.field @"port"
{-# DEPRECATED sPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The list of node types that this cluster snapshot is able to restore into.
--
-- /Note:/ Consider using 'restorableNodeTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRestorableNodeTypes :: Lens.Lens' Snapshot (Core.Maybe [Types.String])
sRestorableNodeTypes = Lens.field @"restorableNodeTypes"
{-# DEPRECATED sRestorableNodeTypes "Use generic-lens or generic-optics with 'restorableNodeTypes' instead." #-}

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# DEPRECATED sSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | The snapshot identifier that is provided in the request.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotIdentifier :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotIdentifier)
sSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED sSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A timestamp representing the start of the retention period for the snapshot.
--
-- /Note:/ Consider using 'snapshotRetentionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotRetentionStartTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sSnapshotRetentionStartTime = Lens.field @"snapshotRetentionStartTime"
{-# DEPRECATED sSnapshotRetentionStartTime "Use generic-lens or generic-optics with 'snapshotRetentionStartTime' instead." #-}

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual".
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotType :: Lens.Lens' Snapshot (Core.Maybe Types.SnapshotType)
sSnapshotType = Lens.field @"snapshotType"
{-# DEPRECATED sSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | The source region from which the snapshot was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceRegion :: Lens.Lens' Snapshot (Core.Maybe Types.SourceRegion)
sSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED sSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

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
sStatus :: Lens.Lens' Snapshot (Core.Maybe Types.Status)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of tags for the cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Core.Maybe [Types.Tag])
sTags = Lens.field @"tags"
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The size of the complete set of backup data that would be used to restore the cluster.
--
-- /Note:/ Consider using 'totalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sTotalBackupSizeInMegaBytes = Lens.field @"totalBackupSizeInMegaBytes"
{-# DEPRECATED sTotalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'totalBackupSizeInMegaBytes' instead." #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVpcId :: Lens.Lens' Snapshot (Core.Maybe Types.VpcId)
sVpcId = Lens.field @"vpcId"
{-# DEPRECATED sVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Core.<$> ( x Core..@? "AccountsWithRestoreAccess"
                   Core..<@> Core.parseXMLList "AccountWithRestoreAccess"
               )
      Core.<*> (x Core..@? "ActualIncrementalBackupSizeInMegaBytes")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "BackupProgressInMegaBytes")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "CurrentBackupRateInMegaBytesPerSecond")
      Core.<*> (x Core..@? "DBName")
      Core.<*> (x Core..@? "ElapsedTimeInSeconds")
      Core.<*> (x Core..@? "Encrypted")
      Core.<*> (x Core..@? "EncryptedWithHSM")
      Core.<*> (x Core..@? "EnhancedVpcRouting")
      Core.<*> (x Core..@? "EstimatedSecondsToCompletion")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "MaintenanceTrackName")
      Core.<*> (x Core..@? "ManualSnapshotRemainingDays")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "OwnerAccount")
      Core.<*> (x Core..@? "Port")
      Core.<*> ( x Core..@? "RestorableNodeTypes"
                   Core..<@> Core.parseXMLList "NodeType"
               )
      Core.<*> (x Core..@? "SnapshotCreateTime")
      Core.<*> (x Core..@? "SnapshotIdentifier")
      Core.<*> (x Core..@? "SnapshotRetentionStartTime")
      Core.<*> (x Core..@? "SnapshotType")
      Core.<*> (x Core..@? "SourceRegion")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
      Core.<*> (x Core..@? "TotalBackupSizeInMegaBytes")
      Core.<*> (x Core..@? "VpcId")
