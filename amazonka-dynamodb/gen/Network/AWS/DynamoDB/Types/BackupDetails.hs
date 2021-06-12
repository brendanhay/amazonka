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
-- Module      : Network.AWS.DynamoDB.Types.BackupDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import qualified Network.AWS.Lens as Lens

-- | Contains the details of the backup created for the table.
--
-- /See:/ 'newBackupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { -- | Time at which the automatic on-demand backup created by DynamoDB will
    -- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
    -- after its creation.
    backupExpiryDateTime :: Core.Maybe Core.POSIX,
    -- | Size of the backup in bytes.
    backupSizeBytes :: Core.Maybe Core.Natural,
    -- | ARN associated with the backup.
    backupArn :: Core.Text,
    -- | Name of the requested backup.
    backupName :: Core.Text,
    -- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
    backupStatus :: BackupStatus,
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
    backupType :: BackupType,
    -- | Time at which the backup was created. This is the request time of the
    -- backup.
    backupCreationDateTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BackupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupExpiryDateTime', 'backupDetails_backupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
--
-- 'backupSizeBytes', 'backupDetails_backupSizeBytes' - Size of the backup in bytes.
--
-- 'backupArn', 'backupDetails_backupArn' - ARN associated with the backup.
--
-- 'backupName', 'backupDetails_backupName' - Name of the requested backup.
--
-- 'backupStatus', 'backupDetails_backupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- 'backupType', 'backupDetails_backupType' - BackupType:
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
-- 'backupCreationDateTime', 'backupDetails_backupCreationDateTime' - Time at which the backup was created. This is the request time of the
-- backup.
newBackupDetails ::
  -- | 'backupArn'
  Core.Text ->
  -- | 'backupName'
  Core.Text ->
  -- | 'backupStatus'
  BackupStatus ->
  -- | 'backupType'
  BackupType ->
  -- | 'backupCreationDateTime'
  Core.UTCTime ->
  BackupDetails
newBackupDetails
  pBackupArn_
  pBackupName_
  pBackupStatus_
  pBackupType_
  pBackupCreationDateTime_ =
    BackupDetails'
      { backupExpiryDateTime = Core.Nothing,
        backupSizeBytes = Core.Nothing,
        backupArn = pBackupArn_,
        backupName = pBackupName_,
        backupStatus = pBackupStatus_,
        backupType = pBackupType_,
        backupCreationDateTime =
          Core._Time Lens.# pBackupCreationDateTime_
      }

-- | Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
backupDetails_backupExpiryDateTime :: Lens.Lens' BackupDetails (Core.Maybe Core.UTCTime)
backupDetails_backupExpiryDateTime = Lens.lens (\BackupDetails' {backupExpiryDateTime} -> backupExpiryDateTime) (\s@BackupDetails' {} a -> s {backupExpiryDateTime = a} :: BackupDetails) Core.. Lens.mapping Core._Time

-- | Size of the backup in bytes.
backupDetails_backupSizeBytes :: Lens.Lens' BackupDetails (Core.Maybe Core.Natural)
backupDetails_backupSizeBytes = Lens.lens (\BackupDetails' {backupSizeBytes} -> backupSizeBytes) (\s@BackupDetails' {} a -> s {backupSizeBytes = a} :: BackupDetails)

-- | ARN associated with the backup.
backupDetails_backupArn :: Lens.Lens' BackupDetails Core.Text
backupDetails_backupArn = Lens.lens (\BackupDetails' {backupArn} -> backupArn) (\s@BackupDetails' {} a -> s {backupArn = a} :: BackupDetails)

-- | Name of the requested backup.
backupDetails_backupName :: Lens.Lens' BackupDetails Core.Text
backupDetails_backupName = Lens.lens (\BackupDetails' {backupName} -> backupName) (\s@BackupDetails' {} a -> s {backupName = a} :: BackupDetails)

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
backupDetails_backupStatus :: Lens.Lens' BackupDetails BackupStatus
backupDetails_backupStatus = Lens.lens (\BackupDetails' {backupStatus} -> backupStatus) (\s@BackupDetails' {} a -> s {backupStatus = a} :: BackupDetails)

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
backupDetails_backupType :: Lens.Lens' BackupDetails BackupType
backupDetails_backupType = Lens.lens (\BackupDetails' {backupType} -> backupType) (\s@BackupDetails' {} a -> s {backupType = a} :: BackupDetails)

-- | Time at which the backup was created. This is the request time of the
-- backup.
backupDetails_backupCreationDateTime :: Lens.Lens' BackupDetails Core.UTCTime
backupDetails_backupCreationDateTime = Lens.lens (\BackupDetails' {backupCreationDateTime} -> backupCreationDateTime) (\s@BackupDetails' {} a -> s {backupCreationDateTime = a} :: BackupDetails) Core.. Core._Time

instance Core.FromJSON BackupDetails where
  parseJSON =
    Core.withObject
      "BackupDetails"
      ( \x ->
          BackupDetails'
            Core.<$> (x Core..:? "BackupExpiryDateTime")
            Core.<*> (x Core..:? "BackupSizeBytes")
            Core.<*> (x Core..: "BackupArn")
            Core.<*> (x Core..: "BackupName")
            Core.<*> (x Core..: "BackupStatus")
            Core.<*> (x Core..: "BackupType")
            Core.<*> (x Core..: "BackupCreationDateTime")
      )

instance Core.Hashable BackupDetails

instance Core.NFData BackupDetails
