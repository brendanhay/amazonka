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
-- Module      : Amazonka.DynamoDB.Types.BackupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BackupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BackupStatus
import Amazonka.DynamoDB.Types.BackupType
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the backup created for the table.
--
-- /See:/ 'newBackupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { -- | Size of the backup in bytes. DynamoDB updates this value approximately
    -- every six hours. Recent changes might not be reflected in this value.
    backupSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | Time at which the automatic on-demand backup created by DynamoDB will
    -- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
    -- after its creation.
    backupExpiryDateTime :: Prelude.Maybe Core.POSIX,
    -- | ARN associated with the backup.
    backupArn :: Prelude.Text,
    -- | Name of the requested backup.
    backupName :: Prelude.Text,
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
    -- -   @AWS_BACKUP@ - On-demand backup created by you from Backup service.
    backupType :: BackupType,
    -- | Time at which the backup was created. This is the request time of the
    -- backup.
    backupCreationDateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupSizeBytes', 'backupDetails_backupSizeBytes' - Size of the backup in bytes. DynamoDB updates this value approximately
-- every six hours. Recent changes might not be reflected in this value.
--
-- 'backupExpiryDateTime', 'backupDetails_backupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
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
-- -   @AWS_BACKUP@ - On-demand backup created by you from Backup service.
--
-- 'backupCreationDateTime', 'backupDetails_backupCreationDateTime' - Time at which the backup was created. This is the request time of the
-- backup.
newBackupDetails ::
  -- | 'backupArn'
  Prelude.Text ->
  -- | 'backupName'
  Prelude.Text ->
  -- | 'backupStatus'
  BackupStatus ->
  -- | 'backupType'
  BackupType ->
  -- | 'backupCreationDateTime'
  Prelude.UTCTime ->
  BackupDetails
newBackupDetails
  pBackupArn_
  pBackupName_
  pBackupStatus_
  pBackupType_
  pBackupCreationDateTime_ =
    BackupDetails'
      { backupSizeBytes = Prelude.Nothing,
        backupExpiryDateTime = Prelude.Nothing,
        backupArn = pBackupArn_,
        backupName = pBackupName_,
        backupStatus = pBackupStatus_,
        backupType = pBackupType_,
        backupCreationDateTime =
          Core._Time Lens.# pBackupCreationDateTime_
      }

-- | Size of the backup in bytes. DynamoDB updates this value approximately
-- every six hours. Recent changes might not be reflected in this value.
backupDetails_backupSizeBytes :: Lens.Lens' BackupDetails (Prelude.Maybe Prelude.Natural)
backupDetails_backupSizeBytes = Lens.lens (\BackupDetails' {backupSizeBytes} -> backupSizeBytes) (\s@BackupDetails' {} a -> s {backupSizeBytes = a} :: BackupDetails)

-- | Time at which the automatic on-demand backup created by DynamoDB will
-- expire. This @SYSTEM@ on-demand backup expires automatically 35 days
-- after its creation.
backupDetails_backupExpiryDateTime :: Lens.Lens' BackupDetails (Prelude.Maybe Prelude.UTCTime)
backupDetails_backupExpiryDateTime = Lens.lens (\BackupDetails' {backupExpiryDateTime} -> backupExpiryDateTime) (\s@BackupDetails' {} a -> s {backupExpiryDateTime = a} :: BackupDetails) Prelude.. Lens.mapping Core._Time

-- | ARN associated with the backup.
backupDetails_backupArn :: Lens.Lens' BackupDetails Prelude.Text
backupDetails_backupArn = Lens.lens (\BackupDetails' {backupArn} -> backupArn) (\s@BackupDetails' {} a -> s {backupArn = a} :: BackupDetails)

-- | Name of the requested backup.
backupDetails_backupName :: Lens.Lens' BackupDetails Prelude.Text
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
-- -   @AWS_BACKUP@ - On-demand backup created by you from Backup service.
backupDetails_backupType :: Lens.Lens' BackupDetails BackupType
backupDetails_backupType = Lens.lens (\BackupDetails' {backupType} -> backupType) (\s@BackupDetails' {} a -> s {backupType = a} :: BackupDetails)

-- | Time at which the backup was created. This is the request time of the
-- backup.
backupDetails_backupCreationDateTime :: Lens.Lens' BackupDetails Prelude.UTCTime
backupDetails_backupCreationDateTime = Lens.lens (\BackupDetails' {backupCreationDateTime} -> backupCreationDateTime) (\s@BackupDetails' {} a -> s {backupCreationDateTime = a} :: BackupDetails) Prelude.. Core._Time

instance Core.FromJSON BackupDetails where
  parseJSON =
    Core.withObject
      "BackupDetails"
      ( \x ->
          BackupDetails'
            Prelude.<$> (x Core..:? "BackupSizeBytes")
            Prelude.<*> (x Core..:? "BackupExpiryDateTime")
            Prelude.<*> (x Core..: "BackupArn")
            Prelude.<*> (x Core..: "BackupName")
            Prelude.<*> (x Core..: "BackupStatus")
            Prelude.<*> (x Core..: "BackupType")
            Prelude.<*> (x Core..: "BackupCreationDateTime")
      )

instance Prelude.Hashable BackupDetails where
  hashWithSalt _salt BackupDetails' {..} =
    _salt `Prelude.hashWithSalt` backupSizeBytes
      `Prelude.hashWithSalt` backupExpiryDateTime
      `Prelude.hashWithSalt` backupArn
      `Prelude.hashWithSalt` backupName
      `Prelude.hashWithSalt` backupStatus
      `Prelude.hashWithSalt` backupType
      `Prelude.hashWithSalt` backupCreationDateTime

instance Prelude.NFData BackupDetails where
  rnf BackupDetails' {..} =
    Prelude.rnf backupSizeBytes
      `Prelude.seq` Prelude.rnf backupExpiryDateTime
      `Prelude.seq` Prelude.rnf backupArn
      `Prelude.seq` Prelude.rnf backupName
      `Prelude.seq` Prelude.rnf backupStatus
      `Prelude.seq` Prelude.rnf backupType
      `Prelude.seq` Prelude.rnf backupCreationDateTime
