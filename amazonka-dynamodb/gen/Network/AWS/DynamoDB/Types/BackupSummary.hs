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
-- Module      : Network.AWS.DynamoDB.Types.BackupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import qualified Network.AWS.Lens as Lens

-- | Contains details for the backup.
--
-- /See:/ 'newBackupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { -- | Name of the table.
    tableName :: Core.Maybe Core.Text,
    -- | Name of the specified backup.
    backupName :: Core.Maybe Core.Text,
    -- | BackupType:
    --
    -- -   @USER@ - You create and manage these using the on-demand backup
    --     feature.
    --
    -- -   @SYSTEM@ - If you delete a table with point-in-time recovery
    --     enabled, a @SYSTEM@ backup is automatically created and is retained
    --     for 35 days (at no additional cost). System backups allow you to
    --     restore the deleted table to the state it was in just before the
    --     point of deletion.
    --
    -- -   @AWS_BACKUP@ - On-demand backup created by you from AWS Backup
    --     service.
    backupType :: Core.Maybe BackupType,
    -- | Time at which the backup was created.
    backupCreationDateTime :: Core.Maybe Core.POSIX,
    -- | ARN associated with the table.
    tableArn :: Core.Maybe Core.Text,
    -- | Unique identifier for the table.
    tableId :: Core.Maybe Core.Text,
    -- | ARN associated with the backup.
    backupArn :: Core.Maybe Core.Text,
    -- | Time at which the automatic on-demand backup created by DynamoDB will
    -- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
    -- after its creation.
    backupExpiryDateTime :: Core.Maybe Core.POSIX,
    -- | Size of the backup in bytes.
    backupSizeBytes :: Core.Maybe Core.Natural,
    -- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
    backupStatus :: Core.Maybe BackupStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BackupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'backupSummary_tableName' - Name of the table.
--
-- 'backupName', 'backupSummary_backupName' - Name of the specified backup.
--
-- 'backupType', 'backupSummary_backupType' - BackupType:
--
-- -   @USER@ - You create and manage these using the on-demand backup
--     feature.
--
-- -   @SYSTEM@ - If you delete a table with point-in-time recovery
--     enabled, a @SYSTEM@ backup is automatically created and is retained
--     for 35 days (at no additional cost). System backups allow you to
--     restore the deleted table to the state it was in just before the
--     point of deletion.
--
-- -   @AWS_BACKUP@ - On-demand backup created by you from AWS Backup
--     service.
--
-- 'backupCreationDateTime', 'backupSummary_backupCreationDateTime' - Time at which the backup was created.
--
-- 'tableArn', 'backupSummary_tableArn' - ARN associated with the table.
--
-- 'tableId', 'backupSummary_tableId' - Unique identifier for the table.
--
-- 'backupArn', 'backupSummary_backupArn' - ARN associated with the backup.
--
-- 'backupExpiryDateTime', 'backupSummary_backupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
--
-- 'backupSizeBytes', 'backupSummary_backupSizeBytes' - Size of the backup in bytes.
--
-- 'backupStatus', 'backupSummary_backupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
newBackupSummary ::
  BackupSummary
newBackupSummary =
  BackupSummary'
    { tableName = Core.Nothing,
      backupName = Core.Nothing,
      backupType = Core.Nothing,
      backupCreationDateTime = Core.Nothing,
      tableArn = Core.Nothing,
      tableId = Core.Nothing,
      backupArn = Core.Nothing,
      backupExpiryDateTime = Core.Nothing,
      backupSizeBytes = Core.Nothing,
      backupStatus = Core.Nothing
    }

-- | Name of the table.
backupSummary_tableName :: Lens.Lens' BackupSummary (Core.Maybe Core.Text)
backupSummary_tableName = Lens.lens (\BackupSummary' {tableName} -> tableName) (\s@BackupSummary' {} a -> s {tableName = a} :: BackupSummary)

-- | Name of the specified backup.
backupSummary_backupName :: Lens.Lens' BackupSummary (Core.Maybe Core.Text)
backupSummary_backupName = Lens.lens (\BackupSummary' {backupName} -> backupName) (\s@BackupSummary' {} a -> s {backupName = a} :: BackupSummary)

-- | BackupType:
--
-- -   @USER@ - You create and manage these using the on-demand backup
--     feature.
--
-- -   @SYSTEM@ - If you delete a table with point-in-time recovery
--     enabled, a @SYSTEM@ backup is automatically created and is retained
--     for 35 days (at no additional cost). System backups allow you to
--     restore the deleted table to the state it was in just before the
--     point of deletion.
--
-- -   @AWS_BACKUP@ - On-demand backup created by you from AWS Backup
--     service.
backupSummary_backupType :: Lens.Lens' BackupSummary (Core.Maybe BackupType)
backupSummary_backupType = Lens.lens (\BackupSummary' {backupType} -> backupType) (\s@BackupSummary' {} a -> s {backupType = a} :: BackupSummary)

-- | Time at which the backup was created.
backupSummary_backupCreationDateTime :: Lens.Lens' BackupSummary (Core.Maybe Core.UTCTime)
backupSummary_backupCreationDateTime = Lens.lens (\BackupSummary' {backupCreationDateTime} -> backupCreationDateTime) (\s@BackupSummary' {} a -> s {backupCreationDateTime = a} :: BackupSummary) Core.. Lens.mapping Core._Time

-- | ARN associated with the table.
backupSummary_tableArn :: Lens.Lens' BackupSummary (Core.Maybe Core.Text)
backupSummary_tableArn = Lens.lens (\BackupSummary' {tableArn} -> tableArn) (\s@BackupSummary' {} a -> s {tableArn = a} :: BackupSummary)

-- | Unique identifier for the table.
backupSummary_tableId :: Lens.Lens' BackupSummary (Core.Maybe Core.Text)
backupSummary_tableId = Lens.lens (\BackupSummary' {tableId} -> tableId) (\s@BackupSummary' {} a -> s {tableId = a} :: BackupSummary)

-- | ARN associated with the backup.
backupSummary_backupArn :: Lens.Lens' BackupSummary (Core.Maybe Core.Text)
backupSummary_backupArn = Lens.lens (\BackupSummary' {backupArn} -> backupArn) (\s@BackupSummary' {} a -> s {backupArn = a} :: BackupSummary)

-- | Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
backupSummary_backupExpiryDateTime :: Lens.Lens' BackupSummary (Core.Maybe Core.UTCTime)
backupSummary_backupExpiryDateTime = Lens.lens (\BackupSummary' {backupExpiryDateTime} -> backupExpiryDateTime) (\s@BackupSummary' {} a -> s {backupExpiryDateTime = a} :: BackupSummary) Core.. Lens.mapping Core._Time

-- | Size of the backup in bytes.
backupSummary_backupSizeBytes :: Lens.Lens' BackupSummary (Core.Maybe Core.Natural)
backupSummary_backupSizeBytes = Lens.lens (\BackupSummary' {backupSizeBytes} -> backupSizeBytes) (\s@BackupSummary' {} a -> s {backupSizeBytes = a} :: BackupSummary)

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
backupSummary_backupStatus :: Lens.Lens' BackupSummary (Core.Maybe BackupStatus)
backupSummary_backupStatus = Lens.lens (\BackupSummary' {backupStatus} -> backupStatus) (\s@BackupSummary' {} a -> s {backupStatus = a} :: BackupSummary)

instance Core.FromJSON BackupSummary where
  parseJSON =
    Core.withObject
      "BackupSummary"
      ( \x ->
          BackupSummary'
            Core.<$> (x Core..:? "TableName")
            Core.<*> (x Core..:? "BackupName")
            Core.<*> (x Core..:? "BackupType")
            Core.<*> (x Core..:? "BackupCreationDateTime")
            Core.<*> (x Core..:? "TableArn")
            Core.<*> (x Core..:? "TableId")
            Core.<*> (x Core..:? "BackupArn")
            Core.<*> (x Core..:? "BackupExpiryDateTime")
            Core.<*> (x Core..:? "BackupSizeBytes")
            Core.<*> (x Core..:? "BackupStatus")
      )

instance Core.Hashable BackupSummary

instance Core.NFData BackupSummary
