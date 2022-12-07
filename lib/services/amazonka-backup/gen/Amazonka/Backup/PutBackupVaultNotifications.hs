{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.PutBackupVaultNotifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on notifications on a backup vault for the specified topic and
-- events.
module Amazonka.Backup.PutBackupVaultNotifications
  ( -- * Creating a Request
    PutBackupVaultNotifications (..),
    newPutBackupVaultNotifications,

    -- * Request Lenses
    putBackupVaultNotifications_backupVaultName,
    putBackupVaultNotifications_sNSTopicArn,
    putBackupVaultNotifications_backupVaultEvents,

    -- * Destructuring the Response
    PutBackupVaultNotificationsResponse (..),
    newPutBackupVaultNotificationsResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBackupVaultNotifications' smart constructor.
data PutBackupVaultNotifications = PutBackupVaultNotifications'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that specifies the topic for a backup
    -- vault’s events; for example,
    -- @arn:aws:sns:us-west-2:111122223333:MyVaultTopic@.
    sNSTopicArn :: Prelude.Text,
    -- | An array of events that indicate the status of jobs to back up resources
    -- to the backup vault.
    --
    -- For common use cases and code samples, see
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/sns-notifications.html Using Amazon SNS to track Backup events>.
    --
    -- The following events are supported:
    --
    -- -   @BACKUP_JOB_STARTED@ | @BACKUP_JOB_COMPLETED@
    --
    -- -   @COPY_JOB_STARTED@ | @COPY_JOB_SUCCESSFUL@ | @COPY_JOB_FAILED@
    --
    -- -   @RESTORE_JOB_STARTED@ | @RESTORE_JOB_COMPLETED@ |
    --     @RECOVERY_POINT_MODIFIED@
    --
    -- -   @S3_BACKUP_OBJECT_FAILED@ | @S3_RESTORE_OBJECT_FAILED@
    --
    -- Ignore the list below because it includes deprecated events. Refer to
    -- the list above.
    backupVaultEvents :: [BackupVaultEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'putBackupVaultNotifications_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'sNSTopicArn', 'putBackupVaultNotifications_sNSTopicArn' - The Amazon Resource Name (ARN) that specifies the topic for a backup
-- vault’s events; for example,
-- @arn:aws:sns:us-west-2:111122223333:MyVaultTopic@.
--
-- 'backupVaultEvents', 'putBackupVaultNotifications_backupVaultEvents' - An array of events that indicate the status of jobs to back up resources
-- to the backup vault.
--
-- For common use cases and code samples, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/sns-notifications.html Using Amazon SNS to track Backup events>.
--
-- The following events are supported:
--
-- -   @BACKUP_JOB_STARTED@ | @BACKUP_JOB_COMPLETED@
--
-- -   @COPY_JOB_STARTED@ | @COPY_JOB_SUCCESSFUL@ | @COPY_JOB_FAILED@
--
-- -   @RESTORE_JOB_STARTED@ | @RESTORE_JOB_COMPLETED@ |
--     @RECOVERY_POINT_MODIFIED@
--
-- -   @S3_BACKUP_OBJECT_FAILED@ | @S3_RESTORE_OBJECT_FAILED@
--
-- Ignore the list below because it includes deprecated events. Refer to
-- the list above.
newPutBackupVaultNotifications ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'sNSTopicArn'
  Prelude.Text ->
  PutBackupVaultNotifications
newPutBackupVaultNotifications
  pBackupVaultName_
  pSNSTopicArn_ =
    PutBackupVaultNotifications'
      { backupVaultName =
          pBackupVaultName_,
        sNSTopicArn = pSNSTopicArn_,
        backupVaultEvents = Prelude.mempty
      }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
putBackupVaultNotifications_backupVaultName :: Lens.Lens' PutBackupVaultNotifications Prelude.Text
putBackupVaultNotifications_backupVaultName = Lens.lens (\PutBackupVaultNotifications' {backupVaultName} -> backupVaultName) (\s@PutBackupVaultNotifications' {} a -> s {backupVaultName = a} :: PutBackupVaultNotifications)

-- | The Amazon Resource Name (ARN) that specifies the topic for a backup
-- vault’s events; for example,
-- @arn:aws:sns:us-west-2:111122223333:MyVaultTopic@.
putBackupVaultNotifications_sNSTopicArn :: Lens.Lens' PutBackupVaultNotifications Prelude.Text
putBackupVaultNotifications_sNSTopicArn = Lens.lens (\PutBackupVaultNotifications' {sNSTopicArn} -> sNSTopicArn) (\s@PutBackupVaultNotifications' {} a -> s {sNSTopicArn = a} :: PutBackupVaultNotifications)

-- | An array of events that indicate the status of jobs to back up resources
-- to the backup vault.
--
-- For common use cases and code samples, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/sns-notifications.html Using Amazon SNS to track Backup events>.
--
-- The following events are supported:
--
-- -   @BACKUP_JOB_STARTED@ | @BACKUP_JOB_COMPLETED@
--
-- -   @COPY_JOB_STARTED@ | @COPY_JOB_SUCCESSFUL@ | @COPY_JOB_FAILED@
--
-- -   @RESTORE_JOB_STARTED@ | @RESTORE_JOB_COMPLETED@ |
--     @RECOVERY_POINT_MODIFIED@
--
-- -   @S3_BACKUP_OBJECT_FAILED@ | @S3_RESTORE_OBJECT_FAILED@
--
-- Ignore the list below because it includes deprecated events. Refer to
-- the list above.
putBackupVaultNotifications_backupVaultEvents :: Lens.Lens' PutBackupVaultNotifications [BackupVaultEvent]
putBackupVaultNotifications_backupVaultEvents = Lens.lens (\PutBackupVaultNotifications' {backupVaultEvents} -> backupVaultEvents) (\s@PutBackupVaultNotifications' {} a -> s {backupVaultEvents = a} :: PutBackupVaultNotifications) Prelude.. Lens.coerced

instance Core.AWSRequest PutBackupVaultNotifications where
  type
    AWSResponse PutBackupVaultNotifications =
      PutBackupVaultNotificationsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutBackupVaultNotificationsResponse'

instance Prelude.Hashable PutBackupVaultNotifications where
  hashWithSalt _salt PutBackupVaultNotifications' {..} =
    _salt `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` sNSTopicArn
      `Prelude.hashWithSalt` backupVaultEvents

instance Prelude.NFData PutBackupVaultNotifications where
  rnf PutBackupVaultNotifications' {..} =
    Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf sNSTopicArn
      `Prelude.seq` Prelude.rnf backupVaultEvents

instance Data.ToHeaders PutBackupVaultNotifications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutBackupVaultNotifications where
  toJSON PutBackupVaultNotifications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SNSTopicArn" Data..= sNSTopicArn),
            Prelude.Just
              ("BackupVaultEvents" Data..= backupVaultEvents)
          ]
      )

instance Data.ToPath PutBackupVaultNotifications where
  toPath PutBackupVaultNotifications' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/notification-configuration"
      ]

instance Data.ToQuery PutBackupVaultNotifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBackupVaultNotificationsResponse' smart constructor.
data PutBackupVaultNotificationsResponse = PutBackupVaultNotificationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBackupVaultNotificationsResponse ::
  PutBackupVaultNotificationsResponse
newPutBackupVaultNotificationsResponse =
  PutBackupVaultNotificationsResponse'

instance
  Prelude.NFData
    PutBackupVaultNotificationsResponse
  where
  rnf _ = ()
