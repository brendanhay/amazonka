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
-- Module      : Amazonka.Backup.StartBackupJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand backup job for the specified resource.
module Amazonka.Backup.StartBackupJob
  ( -- * Creating a Request
    StartBackupJob (..),
    newStartBackupJob,

    -- * Request Lenses
    startBackupJob_backupOptions,
    startBackupJob_completeWindowMinutes,
    startBackupJob_idempotencyToken,
    startBackupJob_lifecycle,
    startBackupJob_recoveryPointTags,
    startBackupJob_startWindowMinutes,
    startBackupJob_backupVaultName,
    startBackupJob_resourceArn,
    startBackupJob_iamRoleArn,

    -- * Destructuring the Response
    StartBackupJobResponse (..),
    newStartBackupJobResponse,

    -- * Response Lenses
    startBackupJobResponse_backupJobId,
    startBackupJobResponse_creationDate,
    startBackupJobResponse_isParent,
    startBackupJobResponse_recoveryPointArn,
    startBackupJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBackupJob' smart constructor.
data StartBackupJob = StartBackupJob'
  { -- | Specifies the backup option for a selected resource. This option is only
    -- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
    --
    -- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
    -- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
    -- @\"WindowsVSS\"\"disabled\"@ to create a regular backup. The
    -- @WindowsVSS@ option is not enabled by default.
    backupOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A value in minutes during which a successfully started backup must
    -- complete, or else Backup will cancel the job. This value is optional.
    -- This value begins counting down from when the backup was scheduled. It
    -- does not add additional time for @StartWindowMinutes@, or if the backup
    -- started later than scheduled.
    completeWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @StartBackupJob@. Retrying a successful
    -- request with the same idempotency token results in a success message
    -- with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup will transition and expire backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
    -- greater than the “transition to cold after days” setting. The
    -- “transition to cold after days” setting cannot be changed after a backup
    -- has been transitioned to cold.
    --
    -- Resource types that are able to be transitioned to cold storage are
    -- listed in the \"Lifecycle to cold storage\" section of the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
    -- table. Backup ignores this expression for other resource types.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | To help organize your resources, you can assign your own metadata to the
    -- resources that you create. Each tag is a key-value pair.
    recoveryPointTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A value in minutes after a backup is scheduled before a job will be
    -- canceled if it doesn\'t start successfully. This value is optional, and
    -- the default is 8 hours. If this value is included, it must be at least
    -- 60 minutes to avoid errors.
    startWindowMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    resourceArn :: Prelude.Text,
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBackupJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupOptions', 'startBackupJob_backupOptions' - Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
--
-- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
-- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
-- @\"WindowsVSS\"\"disabled\"@ to create a regular backup. The
-- @WindowsVSS@ option is not enabled by default.
--
-- 'completeWindowMinutes', 'startBackupJob_completeWindowMinutes' - A value in minutes during which a successfully started backup must
-- complete, or else Backup will cancel the job. This value is optional.
-- This value begins counting down from when the backup was scheduled. It
-- does not add additional time for @StartWindowMinutes@, or if the backup
-- started later than scheduled.
--
-- 'idempotencyToken', 'startBackupJob_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartBackupJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
--
-- 'lifecycle', 'startBackupJob_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- 'recoveryPointTags', 'startBackupJob_recoveryPointTags' - To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
--
-- 'startWindowMinutes', 'startBackupJob_startWindowMinutes' - A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional, and
-- the default is 8 hours. If this value is included, it must be at least
-- 60 minutes to avoid errors.
--
-- 'backupVaultName', 'startBackupJob_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'resourceArn', 'startBackupJob_resourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'iamRoleArn', 'startBackupJob_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
newStartBackupJob ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  StartBackupJob
newStartBackupJob
  pBackupVaultName_
  pResourceArn_
  pIamRoleArn_ =
    StartBackupJob'
      { backupOptions = Prelude.Nothing,
        completeWindowMinutes = Prelude.Nothing,
        idempotencyToken = Prelude.Nothing,
        lifecycle = Prelude.Nothing,
        recoveryPointTags = Prelude.Nothing,
        startWindowMinutes = Prelude.Nothing,
        backupVaultName = pBackupVaultName_,
        resourceArn = pResourceArn_,
        iamRoleArn = pIamRoleArn_
      }

-- | Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
--
-- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
-- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
-- @\"WindowsVSS\"\"disabled\"@ to create a regular backup. The
-- @WindowsVSS@ option is not enabled by default.
startBackupJob_backupOptions :: Lens.Lens' StartBackupJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startBackupJob_backupOptions = Lens.lens (\StartBackupJob' {backupOptions} -> backupOptions) (\s@StartBackupJob' {} a -> s {backupOptions = a} :: StartBackupJob) Prelude.. Lens.mapping Lens.coerced

-- | A value in minutes during which a successfully started backup must
-- complete, or else Backup will cancel the job. This value is optional.
-- This value begins counting down from when the backup was scheduled. It
-- does not add additional time for @StartWindowMinutes@, or if the backup
-- started later than scheduled.
startBackupJob_completeWindowMinutes :: Lens.Lens' StartBackupJob (Prelude.Maybe Prelude.Integer)
startBackupJob_completeWindowMinutes = Lens.lens (\StartBackupJob' {completeWindowMinutes} -> completeWindowMinutes) (\s@StartBackupJob' {} a -> s {completeWindowMinutes = a} :: StartBackupJob)

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartBackupJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
startBackupJob_idempotencyToken :: Lens.Lens' StartBackupJob (Prelude.Maybe Prelude.Text)
startBackupJob_idempotencyToken = Lens.lens (\StartBackupJob' {idempotencyToken} -> idempotencyToken) (\s@StartBackupJob' {} a -> s {idempotencyToken = a} :: StartBackupJob)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup will transition and expire backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
startBackupJob_lifecycle :: Lens.Lens' StartBackupJob (Prelude.Maybe Lifecycle)
startBackupJob_lifecycle = Lens.lens (\StartBackupJob' {lifecycle} -> lifecycle) (\s@StartBackupJob' {} a -> s {lifecycle = a} :: StartBackupJob)

-- | To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair.
startBackupJob_recoveryPointTags :: Lens.Lens' StartBackupJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startBackupJob_recoveryPointTags = Lens.lens (\StartBackupJob' {recoveryPointTags} -> recoveryPointTags) (\s@StartBackupJob' {} a -> s {recoveryPointTags = a} :: StartBackupJob) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A value in minutes after a backup is scheduled before a job will be
-- canceled if it doesn\'t start successfully. This value is optional, and
-- the default is 8 hours. If this value is included, it must be at least
-- 60 minutes to avoid errors.
startBackupJob_startWindowMinutes :: Lens.Lens' StartBackupJob (Prelude.Maybe Prelude.Integer)
startBackupJob_startWindowMinutes = Lens.lens (\StartBackupJob' {startWindowMinutes} -> startWindowMinutes) (\s@StartBackupJob' {} a -> s {startWindowMinutes = a} :: StartBackupJob)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
startBackupJob_backupVaultName :: Lens.Lens' StartBackupJob Prelude.Text
startBackupJob_backupVaultName = Lens.lens (\StartBackupJob' {backupVaultName} -> backupVaultName) (\s@StartBackupJob' {} a -> s {backupVaultName = a} :: StartBackupJob)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
startBackupJob_resourceArn :: Lens.Lens' StartBackupJob Prelude.Text
startBackupJob_resourceArn = Lens.lens (\StartBackupJob' {resourceArn} -> resourceArn) (\s@StartBackupJob' {} a -> s {resourceArn = a} :: StartBackupJob)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
startBackupJob_iamRoleArn :: Lens.Lens' StartBackupJob Prelude.Text
startBackupJob_iamRoleArn = Lens.lens (\StartBackupJob' {iamRoleArn} -> iamRoleArn) (\s@StartBackupJob' {} a -> s {iamRoleArn = a} :: StartBackupJob)

instance Core.AWSRequest StartBackupJob where
  type
    AWSResponse StartBackupJob =
      StartBackupJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBackupJobResponse'
            Prelude.<$> (x Data..?> "BackupJobId")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "IsParent")
            Prelude.<*> (x Data..?> "RecoveryPointArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBackupJob where
  hashWithSalt _salt StartBackupJob' {..} =
    _salt
      `Prelude.hashWithSalt` backupOptions
      `Prelude.hashWithSalt` completeWindowMinutes
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointTags
      `Prelude.hashWithSalt` startWindowMinutes
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData StartBackupJob where
  rnf StartBackupJob' {..} =
    Prelude.rnf backupOptions `Prelude.seq`
      Prelude.rnf completeWindowMinutes `Prelude.seq`
        Prelude.rnf idempotencyToken `Prelude.seq`
          Prelude.rnf lifecycle `Prelude.seq`
            Prelude.rnf recoveryPointTags `Prelude.seq`
              Prelude.rnf startWindowMinutes `Prelude.seq`
                Prelude.rnf backupVaultName `Prelude.seq`
                  Prelude.rnf resourceArn `Prelude.seq`
                    Prelude.rnf iamRoleArn

instance Data.ToHeaders StartBackupJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartBackupJob where
  toJSON StartBackupJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupOptions" Data..=) Prelude.<$> backupOptions,
            ("CompleteWindowMinutes" Data..=)
              Prelude.<$> completeWindowMinutes,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            ("RecoveryPointTags" Data..=)
              Prelude.<$> recoveryPointTags,
            ("StartWindowMinutes" Data..=)
              Prelude.<$> startWindowMinutes,
            Prelude.Just
              ("BackupVaultName" Data..= backupVaultName),
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("IamRoleArn" Data..= iamRoleArn)
          ]
      )

instance Data.ToPath StartBackupJob where
  toPath = Prelude.const "/backup-jobs"

instance Data.ToQuery StartBackupJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBackupJobResponse' smart constructor.
data StartBackupJobResponse = StartBackupJobResponse'
  { -- | Uniquely identifies a request to Backup to back up a resource.
    backupJobId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a backup job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | This is a returned boolean value indicating this is a parent (composite)
    -- backup job.
    isParent :: Prelude.Maybe Prelude.Bool,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBackupJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobId', 'startBackupJobResponse_backupJobId' - Uniquely identifies a request to Backup to back up a resource.
--
-- 'creationDate', 'startBackupJobResponse_creationDate' - The date and time that a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'isParent', 'startBackupJobResponse_isParent' - This is a returned boolean value indicating this is a parent (composite)
-- backup job.
--
-- 'recoveryPointArn', 'startBackupJobResponse_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'httpStatus', 'startBackupJobResponse_httpStatus' - The response's http status code.
newStartBackupJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBackupJobResponse
newStartBackupJobResponse pHttpStatus_ =
  StartBackupJobResponse'
    { backupJobId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      isParent = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Uniquely identifies a request to Backup to back up a resource.
startBackupJobResponse_backupJobId :: Lens.Lens' StartBackupJobResponse (Prelude.Maybe Prelude.Text)
startBackupJobResponse_backupJobId = Lens.lens (\StartBackupJobResponse' {backupJobId} -> backupJobId) (\s@StartBackupJobResponse' {} a -> s {backupJobId = a} :: StartBackupJobResponse)

-- | The date and time that a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
startBackupJobResponse_creationDate :: Lens.Lens' StartBackupJobResponse (Prelude.Maybe Prelude.UTCTime)
startBackupJobResponse_creationDate = Lens.lens (\StartBackupJobResponse' {creationDate} -> creationDate) (\s@StartBackupJobResponse' {} a -> s {creationDate = a} :: StartBackupJobResponse) Prelude.. Lens.mapping Data._Time

-- | This is a returned boolean value indicating this is a parent (composite)
-- backup job.
startBackupJobResponse_isParent :: Lens.Lens' StartBackupJobResponse (Prelude.Maybe Prelude.Bool)
startBackupJobResponse_isParent = Lens.lens (\StartBackupJobResponse' {isParent} -> isParent) (\s@StartBackupJobResponse' {} a -> s {isParent = a} :: StartBackupJobResponse)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
startBackupJobResponse_recoveryPointArn :: Lens.Lens' StartBackupJobResponse (Prelude.Maybe Prelude.Text)
startBackupJobResponse_recoveryPointArn = Lens.lens (\StartBackupJobResponse' {recoveryPointArn} -> recoveryPointArn) (\s@StartBackupJobResponse' {} a -> s {recoveryPointArn = a} :: StartBackupJobResponse)

-- | The response's http status code.
startBackupJobResponse_httpStatus :: Lens.Lens' StartBackupJobResponse Prelude.Int
startBackupJobResponse_httpStatus = Lens.lens (\StartBackupJobResponse' {httpStatus} -> httpStatus) (\s@StartBackupJobResponse' {} a -> s {httpStatus = a} :: StartBackupJobResponse)

instance Prelude.NFData StartBackupJobResponse where
  rnf StartBackupJobResponse' {..} =
    Prelude.rnf backupJobId `Prelude.seq`
      Prelude.rnf creationDate `Prelude.seq`
        Prelude.rnf isParent `Prelude.seq`
          Prelude.rnf recoveryPointArn `Prelude.seq`
            Prelude.rnf httpStatus
