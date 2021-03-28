{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Snapshot
  ( Snapshot (..)
  -- * Smart constructor
  , mkSnapshot
  -- * Lenses
  , sAccountsWithRestoreAccess
  , sActualIncrementalBackupSizeInMegaBytes
  , sAvailabilityZone
  , sBackupProgressInMegaBytes
  , sClusterCreateTime
  , sClusterIdentifier
  , sClusterVersion
  , sCurrentBackupRateInMegaBytesPerSecond
  , sDBName
  , sElapsedTimeInSeconds
  , sEncrypted
  , sEncryptedWithHSM
  , sEnhancedVpcRouting
  , sEstimatedSecondsToCompletion
  , sKmsKeyId
  , sMaintenanceTrackName
  , sManualSnapshotRemainingDays
  , sManualSnapshotRetentionPeriod
  , sMasterUsername
  , sNodeType
  , sNumberOfNodes
  , sOwnerAccount
  , sPort
  , sRestorableNodeTypes
  , sSnapshotCreateTime
  , sSnapshotIdentifier
  , sSnapshotRetentionStartTime
  , sSnapshotType
  , sSourceRegion
  , sStatus
  , sTags
  , sTotalBackupSizeInMegaBytes
  , sVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AccountWithRestoreAccess as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { accountsWithRestoreAccess :: Core.Maybe [Types.AccountWithRestoreAccess]
    -- ^ A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner. 
  , actualIncrementalBackupSizeInMegaBytes :: Core.Maybe Core.Double
    -- ^ The size of the incremental backup.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which the cluster was created.
  , backupProgressInMegaBytes :: Core.Maybe Core.Double
    -- ^ The number of megabytes that have been transferred to the snapshot backup.
  , clusterCreateTime :: Core.Maybe Core.UTCTime
    -- ^ The time (UTC) when the cluster was originally created.
  , clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the cluster for which the snapshot was taken.
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The version ID of the Amazon Redshift engine that is running on the cluster.
  , currentBackupRateInMegaBytesPerSecond :: Core.Maybe Core.Double
    -- ^ The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup. 
  , dBName :: Core.Maybe Core.Text
    -- ^ The name of the database that was created when the cluster was created.
  , elapsedTimeInSeconds :: Core.Maybe Core.Integer
    -- ^ The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ If @true@ , the data in the snapshot is encrypted at rest.
  , encryptedWithHSM :: Core.Maybe Core.Bool
    -- ^ A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
  , enhancedVpcRouting :: Core.Maybe Core.Bool
    -- ^ An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
  , estimatedSecondsToCompletion :: Core.Maybe Core.Integer
    -- ^ The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup. 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the maintenance track for the snapshot.
  , manualSnapshotRemainingDays :: Core.Maybe Core.Int
    -- ^ The number of days until a manual snapshot will pass its retention period.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
  , masterUsername :: Core.Maybe Core.Text
    -- ^ The master user name for the cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type of the nodes in the cluster.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of nodes in the cluster.
  , ownerAccount :: Core.Maybe Core.Text
    -- ^ For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
  , port :: Core.Maybe Core.Int
    -- ^ The port that the cluster is listening on.
  , restorableNodeTypes :: Core.Maybe [Core.Text]
    -- ^ The list of node types that this cluster snapshot is able to restore into.
  , snapshotCreateTime :: Core.Maybe Core.UTCTime
    -- ^ The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
  , snapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The snapshot identifier that is provided in the request.
  , snapshotRetentionStartTime :: Core.Maybe Core.UTCTime
    -- ^ A timestamp representing the start of the retention period for the snapshot.
  , snapshotType :: Core.Maybe Core.Text
    -- ^ The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual". 
  , sourceRegion :: Core.Maybe Core.Text
    -- ^ The source region from which the snapshot was copied.
  , status :: Core.Maybe Core.Text
    -- ^ The snapshot status. The value of the status depends on the API operation used: 
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
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the cluster snapshot.
  , totalBackupSizeInMegaBytes :: Core.Maybe Core.Double
    -- ^ The size of the complete set of backup data that would be used to restore the cluster.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot
    :: Snapshot
mkSnapshot
  = Snapshot'{accountsWithRestoreAccess = Core.Nothing,
              actualIncrementalBackupSizeInMegaBytes = Core.Nothing,
              availabilityZone = Core.Nothing,
              backupProgressInMegaBytes = Core.Nothing,
              clusterCreateTime = Core.Nothing, clusterIdentifier = Core.Nothing,
              clusterVersion = Core.Nothing,
              currentBackupRateInMegaBytesPerSecond = Core.Nothing,
              dBName = Core.Nothing, elapsedTimeInSeconds = Core.Nothing,
              encrypted = Core.Nothing, encryptedWithHSM = Core.Nothing,
              enhancedVpcRouting = Core.Nothing,
              estimatedSecondsToCompletion = Core.Nothing,
              kmsKeyId = Core.Nothing, maintenanceTrackName = Core.Nothing,
              manualSnapshotRemainingDays = Core.Nothing,
              manualSnapshotRetentionPeriod = Core.Nothing,
              masterUsername = Core.Nothing, nodeType = Core.Nothing,
              numberOfNodes = Core.Nothing, ownerAccount = Core.Nothing,
              port = Core.Nothing, restorableNodeTypes = Core.Nothing,
              snapshotCreateTime = Core.Nothing,
              snapshotIdentifier = Core.Nothing,
              snapshotRetentionStartTime = Core.Nothing,
              snapshotType = Core.Nothing, sourceRegion = Core.Nothing,
              status = Core.Nothing, tags = Core.Nothing,
              totalBackupSizeInMegaBytes = Core.Nothing, vpcId = Core.Nothing}

-- | A list of the AWS customer accounts authorized to restore the snapshot. Returns @null@ if no accounts are authorized. Visible only to the snapshot owner. 
--
-- /Note:/ Consider using 'accountsWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountsWithRestoreAccess :: Lens.Lens' Snapshot (Core.Maybe [Types.AccountWithRestoreAccess])
sAccountsWithRestoreAccess = Lens.field @"accountsWithRestoreAccess"
{-# INLINEABLE sAccountsWithRestoreAccess #-}
{-# DEPRECATED accountsWithRestoreAccess "Use generic-lens or generic-optics with 'accountsWithRestoreAccess' instead"  #-}

-- | The size of the incremental backup.
--
-- /Note:/ Consider using 'actualIncrementalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sActualIncrementalBackupSizeInMegaBytes = Lens.field @"actualIncrementalBackupSizeInMegaBytes"
{-# INLINEABLE sActualIncrementalBackupSizeInMegaBytes #-}
{-# DEPRECATED actualIncrementalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'actualIncrementalBackupSizeInMegaBytes' instead"  #-}

-- | The Availability Zone in which the cluster was created.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZone :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE sAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The number of megabytes that have been transferred to the snapshot backup.
--
-- /Note:/ Consider using 'backupProgressInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBackupProgressInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sBackupProgressInMegaBytes = Lens.field @"backupProgressInMegaBytes"
{-# INLINEABLE sBackupProgressInMegaBytes #-}
{-# DEPRECATED backupProgressInMegaBytes "Use generic-lens or generic-optics with 'backupProgressInMegaBytes' instead"  #-}

-- | The time (UTC) when the cluster was originally created.
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sClusterCreateTime = Lens.field @"clusterCreateTime"
{-# INLINEABLE sClusterCreateTime #-}
{-# DEPRECATED clusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead"  #-}

-- | The identifier of the cluster for which the snapshot was taken.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterIdentifier :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE sClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClusterVersion :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE sClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The number of megabytes per second being transferred to the snapshot backup. Returns @0@ for a completed backup. 
--
-- /Note:/ Consider using 'currentBackupRateInMegaBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCurrentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sCurrentBackupRateInMegaBytesPerSecond = Lens.field @"currentBackupRateInMegaBytesPerSecond"
{-# INLINEABLE sCurrentBackupRateInMegaBytesPerSecond #-}
{-# DEPRECATED currentBackupRateInMegaBytesPerSecond "Use generic-lens or generic-optics with 'currentBackupRateInMegaBytesPerSecond' instead"  #-}

-- | The name of the database that was created when the cluster was created.
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDBName :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sDBName = Lens.field @"dBName"
{-# INLINEABLE sDBName #-}
{-# DEPRECATED dBName "Use generic-lens or generic-optics with 'dBName' instead"  #-}

-- | The amount of time an in-progress snapshot backup has been running, or the amount of time it took a completed backup to finish.
--
-- /Note:/ Consider using 'elapsedTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sElapsedTimeInSeconds :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
sElapsedTimeInSeconds = Lens.field @"elapsedTimeInSeconds"
{-# INLINEABLE sElapsedTimeInSeconds #-}
{-# DEPRECATED elapsedTimeInSeconds "Use generic-lens or generic-optics with 'elapsedTimeInSeconds' instead"  #-}

-- | If @true@ , the data in the snapshot is encrypted at rest.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEncrypted = Lens.field @"encrypted"
{-# INLINEABLE sEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM keys of the source cluster. @true@ indicates that the data is encrypted using HSM keys.
--
-- /Note:/ Consider using 'encryptedWithHSM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptedWithHSM :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEncryptedWithHSM = Lens.field @"encryptedWithHSM"
{-# INLINEABLE sEncryptedWithHSM #-}
{-# DEPRECATED encryptedWithHSM "Use generic-lens or generic-optics with 'encryptedWithHSM' instead"  #-}

-- | An option that specifies whether to create the cluster with enhanced VPC routing enabled. To create a cluster that uses enhanced VPC routing, the cluster must be in a VPC. For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/enhanced-vpc-routing.html Enhanced VPC Routing> in the Amazon Redshift Cluster Management Guide.
--
-- If this option is @true@ , enhanced VPC routing is enabled. 
-- Default: false
--
-- /Note:/ Consider using 'enhancedVpcRouting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnhancedVpcRouting :: Lens.Lens' Snapshot (Core.Maybe Core.Bool)
sEnhancedVpcRouting = Lens.field @"enhancedVpcRouting"
{-# INLINEABLE sEnhancedVpcRouting #-}
{-# DEPRECATED enhancedVpcRouting "Use generic-lens or generic-optics with 'enhancedVpcRouting' instead"  #-}

-- | The estimate of the time remaining before the snapshot backup will complete. Returns @0@ for a completed backup. 
--
-- /Note:/ Consider using 'estimatedSecondsToCompletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEstimatedSecondsToCompletion :: Lens.Lens' Snapshot (Core.Maybe Core.Integer)
sEstimatedSecondsToCompletion = Lens.field @"estimatedSecondsToCompletion"
{-# INLINEABLE sEstimatedSecondsToCompletion #-}
{-# DEPRECATED estimatedSecondsToCompletion "Use generic-lens or generic-optics with 'estimatedSecondsToCompletion' instead"  #-}

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was used to encrypt data in the cluster from which the snapshot was taken.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE sKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the maintenance track for the snapshot.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaintenanceTrackName :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE sMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | The number of days until a manual snapshot will pass its retention period.
--
-- /Note:/ Consider using 'manualSnapshotRemainingDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRemainingDays :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sManualSnapshotRemainingDays = Lens.field @"manualSnapshotRemainingDays"
{-# INLINEABLE sManualSnapshotRemainingDays #-}
{-# DEPRECATED manualSnapshotRemainingDays "Use generic-lens or generic-optics with 'manualSnapshotRemainingDays' instead"  #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sManualSnapshotRetentionPeriod :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE sManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The master user name for the cluster.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMasterUsername :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sMasterUsername = Lens.field @"masterUsername"
{-# INLINEABLE sMasterUsername #-}
{-# DEPRECATED masterUsername "Use generic-lens or generic-optics with 'masterUsername' instead"  #-}

-- | The node type of the nodes in the cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNodeType :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sNodeType = Lens.field @"nodeType"
{-# INLINEABLE sNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The number of nodes in the cluster.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNumberOfNodes :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE sNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | For manual snapshots, the AWS customer account used to create or copy the snapshot. For automatic snapshots, the owner of the cluster. The owner can perform all snapshot actions, such as sharing a manual snapshot.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAccount :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE sOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The port that the cluster is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPort :: Lens.Lens' Snapshot (Core.Maybe Core.Int)
sPort = Lens.field @"port"
{-# INLINEABLE sPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The list of node types that this cluster snapshot is able to restore into.
--
-- /Note:/ Consider using 'restorableNodeTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRestorableNodeTypes :: Lens.Lens' Snapshot (Core.Maybe [Core.Text])
sRestorableNodeTypes = Lens.field @"restorableNodeTypes"
{-# INLINEABLE sRestorableNodeTypes #-}
{-# DEPRECATED restorableNodeTypes "Use generic-lens or generic-optics with 'restorableNodeTypes' instead"  #-}

-- | The time (in UTC format) when Amazon Redshift began the snapshot. A snapshot contains a copy of the cluster data as of this exact time.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotCreateTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# INLINEABLE sSnapshotCreateTime #-}
{-# DEPRECATED snapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead"  #-}

-- | The snapshot identifier that is provided in the request.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotIdentifier :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE sSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | A timestamp representing the start of the retention period for the snapshot.
--
-- /Note:/ Consider using 'snapshotRetentionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotRetentionStartTime :: Lens.Lens' Snapshot (Core.Maybe Core.UTCTime)
sSnapshotRetentionStartTime = Lens.field @"snapshotRetentionStartTime"
{-# INLINEABLE sSnapshotRetentionStartTime #-}
{-# DEPRECATED snapshotRetentionStartTime "Use generic-lens or generic-optics with 'snapshotRetentionStartTime' instead"  #-}

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' are of type "manual". 
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotType :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sSnapshotType = Lens.field @"snapshotType"
{-# INLINEABLE sSnapshotType #-}
{-# DEPRECATED snapshotType "Use generic-lens or generic-optics with 'snapshotType' instead"  #-}

-- | The source region from which the snapshot was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceRegion :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sSourceRegion = Lens.field @"sourceRegion"
{-# INLINEABLE sSourceRegion #-}
{-# DEPRECATED sourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead"  #-}

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
sStatus :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sStatus = Lens.field @"status"
{-# INLINEABLE sStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The list of tags for the cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Core.Maybe [Types.Tag])
sTags = Lens.field @"tags"
{-# INLINEABLE sTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The size of the complete set of backup data that would be used to restore the cluster.
--
-- /Note:/ Consider using 'totalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Core.Maybe Core.Double)
sTotalBackupSizeInMegaBytes = Lens.field @"totalBackupSizeInMegaBytes"
{-# INLINEABLE sTotalBackupSizeInMegaBytes #-}
{-# DEPRECATED totalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'totalBackupSizeInMegaBytes' instead"  #-}

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC. Otherwise, this field is not in the output.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVpcId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sVpcId = Lens.field @"vpcId"
{-# INLINEABLE sVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML Snapshot where
        parseXML x
          = Snapshot' Core.<$>
              (x Core..@? "AccountsWithRestoreAccess" Core..<@>
                 Core.parseXMLList "AccountWithRestoreAccess")
                Core.<*> x Core..@? "ActualIncrementalBackupSizeInMegaBytes"
                Core.<*> x Core..@? "AvailabilityZone"
                Core.<*> x Core..@? "BackupProgressInMegaBytes"
                Core.<*> x Core..@? "ClusterCreateTime"
                Core.<*> x Core..@? "ClusterIdentifier"
                Core.<*> x Core..@? "ClusterVersion"
                Core.<*> x Core..@? "CurrentBackupRateInMegaBytesPerSecond"
                Core.<*> x Core..@? "DBName"
                Core.<*> x Core..@? "ElapsedTimeInSeconds"
                Core.<*> x Core..@? "Encrypted"
                Core.<*> x Core..@? "EncryptedWithHSM"
                Core.<*> x Core..@? "EnhancedVpcRouting"
                Core.<*> x Core..@? "EstimatedSecondsToCompletion"
                Core.<*> x Core..@? "KmsKeyId"
                Core.<*> x Core..@? "MaintenanceTrackName"
                Core.<*> x Core..@? "ManualSnapshotRemainingDays"
                Core.<*> x Core..@? "ManualSnapshotRetentionPeriod"
                Core.<*> x Core..@? "MasterUsername"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "NumberOfNodes"
                Core.<*> x Core..@? "OwnerAccount"
                Core.<*> x Core..@? "Port"
                Core.<*>
                x Core..@? "RestorableNodeTypes" Core..<@>
                  Core.parseXMLList "NodeType"
                Core.<*> x Core..@? "SnapshotCreateTime"
                Core.<*> x Core..@? "SnapshotIdentifier"
                Core.<*> x Core..@? "SnapshotRetentionStartTime"
                Core.<*> x Core..@? "SnapshotType"
                Core.<*> x Core..@? "SourceRegion"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
                Core.<*> x Core..@? "TotalBackupSizeInMegaBytes"
                Core.<*> x Core..@? "VpcId"
