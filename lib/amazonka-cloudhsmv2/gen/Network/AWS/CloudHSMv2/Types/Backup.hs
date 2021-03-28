{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Backup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.Backup
  ( Backup (..)
  -- * Smart constructor
  , mkBackup
  -- * Lenses
  , bBackupId
  , bBackupState
  , bClusterId
  , bCopyTimestamp
  , bCreateTimestamp
  , bDeleteTimestamp
  , bNeverExpires
  , bSourceBackup
  , bSourceCluster
  , bSourceRegion
  , bTagList
  ) where

import qualified Network.AWS.CloudHSMv2.Types.BackupId as Types
import qualified Network.AWS.CloudHSMv2.Types.BackupState as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterId as Types
import qualified Network.AWS.CloudHSMv2.Types.SourceCluster as Types
import qualified Network.AWS.CloudHSMv2.Types.SourceRegion as Types
import qualified Network.AWS.CloudHSMv2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a backup of an AWS CloudHSM cluster. All backup objects contain the @BackupId@ , @BackupState@ , @ClusterId@ , and @CreateTimestamp@ parameters. Backups that were copied into a destination region additionally contain the @CopyTimestamp@ , @SourceBackup@ , @SourceCluster@ , and @SourceRegion@ parameters. A backup that is pending deletion will include the @DeleteTimestamp@ parameter.
--
-- /See:/ 'mkBackup' smart constructor.
data Backup = Backup'
  { backupId :: Types.BackupId
    -- ^ The identifier (ID) of the backup.
  , backupState :: Core.Maybe Types.BackupState
    -- ^ The state of the backup.
  , clusterId :: Core.Maybe Types.ClusterId
    -- ^ The identifier (ID) of the cluster that was backed up.
  , copyTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the backup was copied from a source backup.
  , createTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the backup was created.
  , deleteTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the backup will be permanently deleted.
  , neverExpires :: Core.Maybe Core.Bool
    -- ^ Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
  , sourceBackup :: Core.Maybe Types.BackupId
    -- ^ The identifier (ID) of the source backup from which the new backup was copied.
  , sourceCluster :: Core.Maybe Types.SourceCluster
    -- ^ The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
  , sourceRegion :: Core.Maybe Types.SourceRegion
    -- ^ The AWS Region that contains the source backup from which the new backup was copied.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the backup.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Backup' value with any optional fields omitted.
mkBackup
    :: Types.BackupId -- ^ 'backupId'
    -> Backup
mkBackup backupId
  = Backup'{backupId, backupState = Core.Nothing,
            clusterId = Core.Nothing, copyTimestamp = Core.Nothing,
            createTimestamp = Core.Nothing, deleteTimestamp = Core.Nothing,
            neverExpires = Core.Nothing, sourceBackup = Core.Nothing,
            sourceCluster = Core.Nothing, sourceRegion = Core.Nothing,
            tagList = Core.Nothing}

-- | The identifier (ID) of the backup.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupId :: Lens.Lens' Backup Types.BackupId
bBackupId = Lens.field @"backupId"
{-# INLINEABLE bBackupId #-}
{-# DEPRECATED backupId "Use generic-lens or generic-optics with 'backupId' instead"  #-}

-- | The state of the backup.
--
-- /Note:/ Consider using 'backupState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBackupState :: Lens.Lens' Backup (Core.Maybe Types.BackupState)
bBackupState = Lens.field @"backupState"
{-# INLINEABLE bBackupState #-}
{-# DEPRECATED backupState "Use generic-lens or generic-optics with 'backupState' instead"  #-}

-- | The identifier (ID) of the cluster that was backed up.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bClusterId :: Lens.Lens' Backup (Core.Maybe Types.ClusterId)
bClusterId = Lens.field @"clusterId"
{-# INLINEABLE bClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The date and time when the backup was copied from a source backup.
--
-- /Note:/ Consider using 'copyTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCopyTimestamp :: Lens.Lens' Backup (Core.Maybe Core.NominalDiffTime)
bCopyTimestamp = Lens.field @"copyTimestamp"
{-# INLINEABLE bCopyTimestamp #-}
{-# DEPRECATED copyTimestamp "Use generic-lens or generic-optics with 'copyTimestamp' instead"  #-}

-- | The date and time when the backup was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreateTimestamp :: Lens.Lens' Backup (Core.Maybe Core.NominalDiffTime)
bCreateTimestamp = Lens.field @"createTimestamp"
{-# INLINEABLE bCreateTimestamp #-}
{-# DEPRECATED createTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead"  #-}

-- | The date and time when the backup will be permanently deleted.
--
-- /Note:/ Consider using 'deleteTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDeleteTimestamp :: Lens.Lens' Backup (Core.Maybe Core.NominalDiffTime)
bDeleteTimestamp = Lens.field @"deleteTimestamp"
{-# INLINEABLE bDeleteTimestamp #-}
{-# DEPRECATED deleteTimestamp "Use generic-lens or generic-optics with 'deleteTimestamp' instead"  #-}

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'neverExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNeverExpires :: Lens.Lens' Backup (Core.Maybe Core.Bool)
bNeverExpires = Lens.field @"neverExpires"
{-# INLINEABLE bNeverExpires #-}
{-# DEPRECATED neverExpires "Use generic-lens or generic-optics with 'neverExpires' instead"  #-}

-- | The identifier (ID) of the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceBackup :: Lens.Lens' Backup (Core.Maybe Types.BackupId)
bSourceBackup = Lens.field @"sourceBackup"
{-# INLINEABLE bSourceBackup #-}
{-# DEPRECATED sourceBackup "Use generic-lens or generic-optics with 'sourceBackup' instead"  #-}

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceCluster :: Lens.Lens' Backup (Core.Maybe Types.SourceCluster)
bSourceCluster = Lens.field @"sourceCluster"
{-# INLINEABLE bSourceCluster #-}
{-# DEPRECATED sourceCluster "Use generic-lens or generic-optics with 'sourceCluster' instead"  #-}

-- | The AWS Region that contains the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceRegion :: Lens.Lens' Backup (Core.Maybe Types.SourceRegion)
bSourceRegion = Lens.field @"sourceRegion"
{-# INLINEABLE bSourceRegion #-}
{-# DEPRECATED sourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead"  #-}

-- | The list of tags for the backup.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTagList :: Lens.Lens' Backup (Core.Maybe [Types.Tag])
bTagList = Lens.field @"tagList"
{-# INLINEABLE bTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.FromJSON Backup where
        parseJSON
          = Core.withObject "Backup" Core.$
              \ x ->
                Backup' Core.<$>
                  (x Core..: "BackupId") Core.<*> x Core..:? "BackupState" Core.<*>
                    x Core..:? "ClusterId"
                    Core.<*> x Core..:? "CopyTimestamp"
                    Core.<*> x Core..:? "CreateTimestamp"
                    Core.<*> x Core..:? "DeleteTimestamp"
                    Core.<*> x Core..:? "NeverExpires"
                    Core.<*> x Core..:? "SourceBackup"
                    Core.<*> x Core..:? "SourceCluster"
                    Core.<*> x Core..:? "SourceRegion"
                    Core.<*> x Core..:? "TagList"
