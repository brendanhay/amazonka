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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupVaultNotificationsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupVaultNotificationsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the Amazon SNS event notifications for the
-- specified backup vault.
--
-- /See:/ 'newAwsBackupBackupVaultNotificationsDetails' smart constructor.
data AwsBackupBackupVaultNotificationsDetails = AwsBackupBackupVaultNotificationsDetails'
  { -- | An array of events that indicate the status of jobs to back up resources
    -- to the backup vault. The following events are supported:
    --
    -- -   @BACKUP_JOB_STARTED | BACKUP_JOB_COMPLETED@
    --
    -- -   @COPY_JOB_STARTED | COPY_JOB_SUCCESSFUL | COPY_JOB_FAILED@
    --
    -- -   @RESTORE_JOB_STARTED | RESTORE_JOB_COMPLETED | RECOVERY_POINT_MODIFIED@
    --
    -- -   @S3_BACKUP_OBJECT_FAILED | S3_RESTORE_OBJECT_FAILED@
    backupVaultEvents :: Prelude.Maybe [Prelude.Text],
    -- | An ARN that uniquely identifies the Amazon SNS topic for a backup
    -- vault’s events.
    snsTopicArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupVaultNotificationsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultEvents', 'awsBackupBackupVaultNotificationsDetails_backupVaultEvents' - An array of events that indicate the status of jobs to back up resources
-- to the backup vault. The following events are supported:
--
-- -   @BACKUP_JOB_STARTED | BACKUP_JOB_COMPLETED@
--
-- -   @COPY_JOB_STARTED | COPY_JOB_SUCCESSFUL | COPY_JOB_FAILED@
--
-- -   @RESTORE_JOB_STARTED | RESTORE_JOB_COMPLETED | RECOVERY_POINT_MODIFIED@
--
-- -   @S3_BACKUP_OBJECT_FAILED | S3_RESTORE_OBJECT_FAILED@
--
-- 'snsTopicArn', 'awsBackupBackupVaultNotificationsDetails_snsTopicArn' - An ARN that uniquely identifies the Amazon SNS topic for a backup
-- vault’s events.
newAwsBackupBackupVaultNotificationsDetails ::
  AwsBackupBackupVaultNotificationsDetails
newAwsBackupBackupVaultNotificationsDetails =
  AwsBackupBackupVaultNotificationsDetails'
    { backupVaultEvents =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing
    }

-- | An array of events that indicate the status of jobs to back up resources
-- to the backup vault. The following events are supported:
--
-- -   @BACKUP_JOB_STARTED | BACKUP_JOB_COMPLETED@
--
-- -   @COPY_JOB_STARTED | COPY_JOB_SUCCESSFUL | COPY_JOB_FAILED@
--
-- -   @RESTORE_JOB_STARTED | RESTORE_JOB_COMPLETED | RECOVERY_POINT_MODIFIED@
--
-- -   @S3_BACKUP_OBJECT_FAILED | S3_RESTORE_OBJECT_FAILED@
awsBackupBackupVaultNotificationsDetails_backupVaultEvents :: Lens.Lens' AwsBackupBackupVaultNotificationsDetails (Prelude.Maybe [Prelude.Text])
awsBackupBackupVaultNotificationsDetails_backupVaultEvents = Lens.lens (\AwsBackupBackupVaultNotificationsDetails' {backupVaultEvents} -> backupVaultEvents) (\s@AwsBackupBackupVaultNotificationsDetails' {} a -> s {backupVaultEvents = a} :: AwsBackupBackupVaultNotificationsDetails) Prelude.. Lens.mapping Lens.coerced

-- | An ARN that uniquely identifies the Amazon SNS topic for a backup
-- vault’s events.
awsBackupBackupVaultNotificationsDetails_snsTopicArn :: Lens.Lens' AwsBackupBackupVaultNotificationsDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupVaultNotificationsDetails_snsTopicArn = Lens.lens (\AwsBackupBackupVaultNotificationsDetails' {snsTopicArn} -> snsTopicArn) (\s@AwsBackupBackupVaultNotificationsDetails' {} a -> s {snsTopicArn = a} :: AwsBackupBackupVaultNotificationsDetails)

instance
  Data.FromJSON
    AwsBackupBackupVaultNotificationsDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupBackupVaultNotificationsDetails"
      ( \x ->
          AwsBackupBackupVaultNotificationsDetails'
            Prelude.<$> ( x
                            Data..:? "BackupVaultEvents"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SnsTopicArn")
      )

instance
  Prelude.Hashable
    AwsBackupBackupVaultNotificationsDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupVaultNotificationsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` backupVaultEvents
        `Prelude.hashWithSalt` snsTopicArn

instance
  Prelude.NFData
    AwsBackupBackupVaultNotificationsDetails
  where
  rnf AwsBackupBackupVaultNotificationsDetails' {..} =
    Prelude.rnf backupVaultEvents
      `Prelude.seq` Prelude.rnf snsTopicArn

instance
  Data.ToJSON
    AwsBackupBackupVaultNotificationsDetails
  where
  toJSON AwsBackupBackupVaultNotificationsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupVaultEvents" Data..=)
              Prelude.<$> backupVaultEvents,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn
          ]
      )
