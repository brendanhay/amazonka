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
    bsBackupExpiryDateTime,
    bsTableARN,
    bsBackupName,
    bsBackupStatus,
    bsBackupSizeBytes,
    bsBackupARN,
    bsTableId,
    bsBackupCreationDateTime,
    bsBackupType,
    bsTableName,
  )
where

import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details for the backup.
--
-- /See:/ 'mkBackupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { -- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
    backupExpiryDateTime :: Lude.Maybe Lude.Timestamp,
    -- | ARN associated with the table.
    tableARN :: Lude.Maybe Lude.Text,
    -- | Name of the specified backup.
    backupName :: Lude.Maybe Lude.Text,
    -- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
    backupStatus :: Lude.Maybe BackupStatus,
    -- | Size of the backup in bytes.
    backupSizeBytes :: Lude.Maybe Lude.Natural,
    -- | ARN associated with the backup.
    backupARN :: Lude.Maybe Lude.Text,
    -- | Unique identifier for the table.
    tableId :: Lude.Maybe Lude.Text,
    -- | Time at which the backup was created.
    backupCreationDateTime :: Lude.Maybe Lude.Timestamp,
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
    backupType :: Lude.Maybe BackupType,
    -- | Name of the table.
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupSummary' with the minimum fields required to make a request.
--
-- * 'backupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
-- * 'tableARN' - ARN associated with the table.
-- * 'backupName' - Name of the specified backup.
-- * 'backupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
-- * 'backupSizeBytes' - Size of the backup in bytes.
-- * 'backupARN' - ARN associated with the backup.
-- * 'tableId' - Unique identifier for the table.
-- * 'backupCreationDateTime' - Time at which the backup was created.
-- * 'backupType' - BackupType:
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
-- * 'tableName' - Name of the table.
mkBackupSummary ::
  BackupSummary
mkBackupSummary =
  BackupSummary'
    { backupExpiryDateTime = Lude.Nothing,
      tableARN = Lude.Nothing,
      backupName = Lude.Nothing,
      backupStatus = Lude.Nothing,
      backupSizeBytes = Lude.Nothing,
      backupARN = Lude.Nothing,
      tableId = Lude.Nothing,
      backupCreationDateTime = Lude.Nothing,
      backupType = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- /Note:/ Consider using 'backupExpiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupExpiryDateTime :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Timestamp)
bsBackupExpiryDateTime = Lens.lens (backupExpiryDateTime :: BackupSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {backupExpiryDateTime = a} :: BackupSummary)
{-# DEPRECATED bsBackupExpiryDateTime "Use generic-lens or generic-optics with 'backupExpiryDateTime' instead." #-}

-- | ARN associated with the table.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableARN :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Text)
bsTableARN = Lens.lens (tableARN :: BackupSummary -> Lude.Maybe Lude.Text) (\s a -> s {tableARN = a} :: BackupSummary)
{-# DEPRECATED bsTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

-- | Name of the specified backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupName :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Text)
bsBackupName = Lens.lens (backupName :: BackupSummary -> Lude.Maybe Lude.Text) (\s a -> s {backupName = a} :: BackupSummary)
{-# DEPRECATED bsBackupName "Use generic-lens or generic-optics with 'backupName' instead." #-}

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- /Note:/ Consider using 'backupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupStatus :: Lens.Lens' BackupSummary (Lude.Maybe BackupStatus)
bsBackupStatus = Lens.lens (backupStatus :: BackupSummary -> Lude.Maybe BackupStatus) (\s a -> s {backupStatus = a} :: BackupSummary)
{-# DEPRECATED bsBackupStatus "Use generic-lens or generic-optics with 'backupStatus' instead." #-}

-- | Size of the backup in bytes.
--
-- /Note:/ Consider using 'backupSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupSizeBytes :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Natural)
bsBackupSizeBytes = Lens.lens (backupSizeBytes :: BackupSummary -> Lude.Maybe Lude.Natural) (\s a -> s {backupSizeBytes = a} :: BackupSummary)
{-# DEPRECATED bsBackupSizeBytes "Use generic-lens or generic-optics with 'backupSizeBytes' instead." #-}

-- | ARN associated with the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupARN :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Text)
bsBackupARN = Lens.lens (backupARN :: BackupSummary -> Lude.Maybe Lude.Text) (\s a -> s {backupARN = a} :: BackupSummary)
{-# DEPRECATED bsBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

-- | Unique identifier for the table.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableId :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Text)
bsTableId = Lens.lens (tableId :: BackupSummary -> Lude.Maybe Lude.Text) (\s a -> s {tableId = a} :: BackupSummary)
{-# DEPRECATED bsTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | Time at which the backup was created.
--
-- /Note:/ Consider using 'backupCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBackupCreationDateTime :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Timestamp)
bsBackupCreationDateTime = Lens.lens (backupCreationDateTime :: BackupSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {backupCreationDateTime = a} :: BackupSummary)
{-# DEPRECATED bsBackupCreationDateTime "Use generic-lens or generic-optics with 'backupCreationDateTime' instead." #-}

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
bsBackupType :: Lens.Lens' BackupSummary (Lude.Maybe BackupType)
bsBackupType = Lens.lens (backupType :: BackupSummary -> Lude.Maybe BackupType) (\s a -> s {backupType = a} :: BackupSummary)
{-# DEPRECATED bsBackupType "Use generic-lens or generic-optics with 'backupType' instead." #-}

-- | Name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsTableName :: Lens.Lens' BackupSummary (Lude.Maybe Lude.Text)
bsTableName = Lens.lens (tableName :: BackupSummary -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: BackupSummary)
{-# DEPRECATED bsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON BackupSummary where
  parseJSON =
    Lude.withObject
      "BackupSummary"
      ( \x ->
          BackupSummary'
            Lude.<$> (x Lude..:? "BackupExpiryDateTime")
            Lude.<*> (x Lude..:? "TableArn")
            Lude.<*> (x Lude..:? "BackupName")
            Lude.<*> (x Lude..:? "BackupStatus")
            Lude.<*> (x Lude..:? "BackupSizeBytes")
            Lude.<*> (x Lude..:? "BackupArn")
            Lude.<*> (x Lude..:? "TableId")
            Lude.<*> (x Lude..:? "BackupCreationDateTime")
            Lude.<*> (x Lude..:? "BackupType")
            Lude.<*> (x Lude..:? "TableName")
      )
