{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupSummary
  ( BackupSummary (..),

    -- * Smart constructor
    mkBackupSummary,

    -- * Lenses
    bsBackupArn,
    bsBackupCreationDateTime,
    bsBackupExpiryDateTime,
    bsBackupName,
    bsBackupSizeBytes,
    bsBackupStatus,
    bsBackupType,
    bsTableArn,
    bsTableId,
    bsTableName,
  )
where

import qualified Network.AWS.DynamoDB.Types.BackupArn as Types
import qualified Network.AWS.DynamoDB.Types.BackupName as Types
import qualified Network.AWS.DynamoDB.Types.BackupStatus as Types
import qualified Network.AWS.DynamoDB.Types.BackupType as Types
import qualified Network.AWS.DynamoDB.Types.TableArn as Types
import qualified Network.AWS.DynamoDB.Types.TableId as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details for the backup.
--
-- /See:/ 'mkBackupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { -- | ARN associated with the backup.
    backupArn :: Core.Maybe Types.BackupArn,
    -- | Time at which the backup was created.
    backupCreationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
    backupExpiryDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | Name of the specified backup.
    backupName :: Core.Maybe Types.BackupName,
    -- | Size of the backup in bytes.
    backupSizeBytes :: Core.Maybe Core.Natural,
    -- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
    backupStatus :: Core.Maybe Types.BackupStatus,
    -- | BackupType:
    --
    --
    --     * @USER@ - You create and manage these using the on-demand backup feature.
    --
    --
    --     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.
    --
    --
    --     * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
    backupType :: Core.Maybe Types.BackupType,
    -- | ARN associated with the table.
    tableArn :: Core.Maybe Types.TableArn,
    -- | Unique identifier for the table.
    tableId :: Core.Maybe Types.TableId,
    -- | Name of the table.
    tableName :: Core.Maybe Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BackupSummary' value with any optional fields omitted.
mkBackupSummary ::
  BackupSummary
mkBackupSummary =
  BackupSummary'
    { backupArn = Core.Nothing,
      backupCreationDateTime = Core.Nothing,
      backupExpiryDateTime = Core.Nothing,
      backupName = Core.Nothing,
      backupSizeBytes = Core.Nothing,
      backupStatus = Core.Nothing,
      backupType = Core.Nothing,
      tableArn = Core.Nothing,
      tableId = Core.Nothing,
      tableName = Core.Nothing
    }

-- | ARN associated with the backup.
--
-- /Note:/ Consider using 'backupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupArn :: Lens.Lens' BackupSummary (Core.Maybe Types.BackupArn)
bsBackupArn = Lens.field @"backupArn"
{-# DEPRECATED bsBackupArn "Use generic-lens or generic-optics with 'backupArn' instead." #-}

-- | Time at which the backup was created.
--
-- /Note:/ Consider using 'backupCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupCreationDateTime :: Lens.Lens' BackupSummary (Core.Maybe Core.NominalDiffTime)
bsBackupCreationDateTime = Lens.field @"backupCreationDateTime"
{-# DEPRECATED bsBackupCreationDateTime "Use generic-lens or generic-optics with 'backupCreationDateTime' instead." #-}

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- /Note:/ Consider using 'backupExpiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupExpiryDateTime :: Lens.Lens' BackupSummary (Core.Maybe Core.NominalDiffTime)
bsBackupExpiryDateTime = Lens.field @"backupExpiryDateTime"
{-# DEPRECATED bsBackupExpiryDateTime "Use generic-lens or generic-optics with 'backupExpiryDateTime' instead." #-}

-- | Name of the specified backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupName :: Lens.Lens' BackupSummary (Core.Maybe Types.BackupName)
bsBackupName = Lens.field @"backupName"
{-# DEPRECATED bsBackupName "Use generic-lens or generic-optics with 'backupName' instead." #-}

-- | Size of the backup in bytes.
--
-- /Note:/ Consider using 'backupSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupSizeBytes :: Lens.Lens' BackupSummary (Core.Maybe Core.Natural)
bsBackupSizeBytes = Lens.field @"backupSizeBytes"
{-# DEPRECATED bsBackupSizeBytes "Use generic-lens or generic-optics with 'backupSizeBytes' instead." #-}

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- /Note:/ Consider using 'backupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupStatus :: Lens.Lens' BackupSummary (Core.Maybe Types.BackupStatus)
bsBackupStatus = Lens.field @"backupStatus"
{-# DEPRECATED bsBackupStatus "Use generic-lens or generic-optics with 'backupStatus' instead." #-}

-- | BackupType:
--
--
--     * @USER@ - You create and manage these using the on-demand backup feature.
--
--
--     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.
--
--
--     * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
--
--
-- /Note:/ Consider using 'backupType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupType :: Lens.Lens' BackupSummary (Core.Maybe Types.BackupType)
bsBackupType = Lens.field @"backupType"
{-# DEPRECATED bsBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | ARN associated with the table.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableArn :: Lens.Lens' BackupSummary (Core.Maybe Types.TableArn)
bsTableArn = Lens.field @"tableArn"
{-# DEPRECATED bsTableArn "Use generic-lens or generic-optics with 'tableArn' instead." #-}

-- | Unique identifier for the table.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableId :: Lens.Lens' BackupSummary (Core.Maybe Types.TableId)
bsTableId = Lens.field @"tableId"
{-# DEPRECATED bsTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | Name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableName :: Lens.Lens' BackupSummary (Core.Maybe Types.TableName)
bsTableName = Lens.field @"tableName"
{-# DEPRECATED bsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON BackupSummary where
  parseJSON =
    Core.withObject "BackupSummary" Core.$
      \x ->
        BackupSummary'
          Core.<$> (x Core..:? "BackupArn")
          Core.<*> (x Core..:? "BackupCreationDateTime")
          Core.<*> (x Core..:? "BackupExpiryDateTime")
          Core.<*> (x Core..:? "BackupName")
          Core.<*> (x Core..:? "BackupSizeBytes")
          Core.<*> (x Core..:? "BackupStatus")
          Core.<*> (x Core..:? "BackupType")
          Core.<*> (x Core..:? "TableArn")
          Core.<*> (x Core..:? "TableId")
          Core.<*> (x Core..:? "TableName")
