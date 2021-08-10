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
import qualified Network.AWS.Prelude as Prelude

-- | Contains details for the backup.
--
-- /See:/ 'newBackupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { -- | Name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Name of the specified backup.
    backupName :: Prelude.Maybe Prelude.Text,
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
    backupType :: Prelude.Maybe BackupType,
    -- | Time at which the backup was created.
    backupCreationDateTime :: Prelude.Maybe Core.POSIX,
    -- | ARN associated with the table.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for the table.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | ARN associated with the backup.
    backupArn :: Prelude.Maybe Prelude.Text,
    -- | Time at which the automatic on-demand backup created by DynamoDB will
    -- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
    -- after its creation.
    backupExpiryDateTime :: Prelude.Maybe Core.POSIX,
    -- | Size of the backup in bytes.
    backupSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
    backupStatus :: Prelude.Maybe BackupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { tableName = Prelude.Nothing,
      backupName = Prelude.Nothing,
      backupType = Prelude.Nothing,
      backupCreationDateTime = Prelude.Nothing,
      tableArn = Prelude.Nothing,
      tableId = Prelude.Nothing,
      backupArn = Prelude.Nothing,
      backupExpiryDateTime = Prelude.Nothing,
      backupSizeBytes = Prelude.Nothing,
      backupStatus = Prelude.Nothing
    }

-- | Name of the table.
backupSummary_tableName :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Text)
backupSummary_tableName = Lens.lens (\BackupSummary' {tableName} -> tableName) (\s@BackupSummary' {} a -> s {tableName = a} :: BackupSummary)

-- | Name of the specified backup.
backupSummary_backupName :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Text)
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
backupSummary_backupType :: Lens.Lens' BackupSummary (Prelude.Maybe BackupType)
backupSummary_backupType = Lens.lens (\BackupSummary' {backupType} -> backupType) (\s@BackupSummary' {} a -> s {backupType = a} :: BackupSummary)

-- | Time at which the backup was created.
backupSummary_backupCreationDateTime :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.UTCTime)
backupSummary_backupCreationDateTime = Lens.lens (\BackupSummary' {backupCreationDateTime} -> backupCreationDateTime) (\s@BackupSummary' {} a -> s {backupCreationDateTime = a} :: BackupSummary) Prelude.. Lens.mapping Core._Time

-- | ARN associated with the table.
backupSummary_tableArn :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Text)
backupSummary_tableArn = Lens.lens (\BackupSummary' {tableArn} -> tableArn) (\s@BackupSummary' {} a -> s {tableArn = a} :: BackupSummary)

-- | Unique identifier for the table.
backupSummary_tableId :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Text)
backupSummary_tableId = Lens.lens (\BackupSummary' {tableId} -> tableId) (\s@BackupSummary' {} a -> s {tableId = a} :: BackupSummary)

-- | ARN associated with the backup.
backupSummary_backupArn :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Text)
backupSummary_backupArn = Lens.lens (\BackupSummary' {backupArn} -> backupArn) (\s@BackupSummary' {} a -> s {backupArn = a} :: BackupSummary)

-- | Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
backupSummary_backupExpiryDateTime :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.UTCTime)
backupSummary_backupExpiryDateTime = Lens.lens (\BackupSummary' {backupExpiryDateTime} -> backupExpiryDateTime) (\s@BackupSummary' {} a -> s {backupExpiryDateTime = a} :: BackupSummary) Prelude.. Lens.mapping Core._Time

-- | Size of the backup in bytes.
backupSummary_backupSizeBytes :: Lens.Lens' BackupSummary (Prelude.Maybe Prelude.Natural)
backupSummary_backupSizeBytes = Lens.lens (\BackupSummary' {backupSizeBytes} -> backupSizeBytes) (\s@BackupSummary' {} a -> s {backupSizeBytes = a} :: BackupSummary)

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
backupSummary_backupStatus :: Lens.Lens' BackupSummary (Prelude.Maybe BackupStatus)
backupSummary_backupStatus = Lens.lens (\BackupSummary' {backupStatus} -> backupStatus) (\s@BackupSummary' {} a -> s {backupStatus = a} :: BackupSummary)

instance Core.FromJSON BackupSummary where
  parseJSON =
    Core.withObject
      "BackupSummary"
      ( \x ->
          BackupSummary'
            Prelude.<$> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "BackupName")
            Prelude.<*> (x Core..:? "BackupType")
            Prelude.<*> (x Core..:? "BackupCreationDateTime")
            Prelude.<*> (x Core..:? "TableArn")
            Prelude.<*> (x Core..:? "TableId")
            Prelude.<*> (x Core..:? "BackupArn")
            Prelude.<*> (x Core..:? "BackupExpiryDateTime")
            Prelude.<*> (x Core..:? "BackupSizeBytes")
            Prelude.<*> (x Core..:? "BackupStatus")
      )

instance Prelude.Hashable BackupSummary

instance Prelude.NFData BackupSummary
