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
-- Module      : Amazonka.Backup.DescribeRestoreJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with a restore job that is specified by a
-- job ID.
module Amazonka.Backup.DescribeRestoreJob
  ( -- * Creating a Request
    DescribeRestoreJob (..),
    newDescribeRestoreJob,

    -- * Request Lenses
    describeRestoreJob_restoreJobId,

    -- * Destructuring the Response
    DescribeRestoreJobResponse (..),
    newDescribeRestoreJobResponse,

    -- * Response Lenses
    describeRestoreJobResponse_accountId,
    describeRestoreJobResponse_backupSizeInBytes,
    describeRestoreJobResponse_completionDate,
    describeRestoreJobResponse_createdResourceArn,
    describeRestoreJobResponse_creationDate,
    describeRestoreJobResponse_expectedCompletionTimeMinutes,
    describeRestoreJobResponse_iamRoleArn,
    describeRestoreJobResponse_percentDone,
    describeRestoreJobResponse_recoveryPointArn,
    describeRestoreJobResponse_resourceType,
    describeRestoreJobResponse_restoreJobId,
    describeRestoreJobResponse_status,
    describeRestoreJobResponse_statusMessage,
    describeRestoreJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRestoreJob' smart constructor.
data DescribeRestoreJob = DescribeRestoreJob'
  { -- | Uniquely identifies the job that restores a recovery point.
    restoreJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRestoreJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreJobId', 'describeRestoreJob_restoreJobId' - Uniquely identifies the job that restores a recovery point.
newDescribeRestoreJob ::
  -- | 'restoreJobId'
  Prelude.Text ->
  DescribeRestoreJob
newDescribeRestoreJob pRestoreJobId_ =
  DescribeRestoreJob' {restoreJobId = pRestoreJobId_}

-- | Uniquely identifies the job that restores a recovery point.
describeRestoreJob_restoreJobId :: Lens.Lens' DescribeRestoreJob Prelude.Text
describeRestoreJob_restoreJobId = Lens.lens (\DescribeRestoreJob' {restoreJobId} -> restoreJobId) (\s@DescribeRestoreJob' {} a -> s {restoreJobId = a} :: DescribeRestoreJob)

instance Core.AWSRequest DescribeRestoreJob where
  type
    AWSResponse DescribeRestoreJob =
      DescribeRestoreJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRestoreJobResponse'
            Prelude.<$> (x Data..?> "AccountId")
            Prelude.<*> (x Data..?> "BackupSizeInBytes")
            Prelude.<*> (x Data..?> "CompletionDate")
            Prelude.<*> (x Data..?> "CreatedResourceArn")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "ExpectedCompletionTimeMinutes")
            Prelude.<*> (x Data..?> "IamRoleArn")
            Prelude.<*> (x Data..?> "PercentDone")
            Prelude.<*> (x Data..?> "RecoveryPointArn")
            Prelude.<*> (x Data..?> "ResourceType")
            Prelude.<*> (x Data..?> "RestoreJobId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRestoreJob where
  hashWithSalt _salt DescribeRestoreJob' {..} =
    _salt `Prelude.hashWithSalt` restoreJobId

instance Prelude.NFData DescribeRestoreJob where
  rnf DescribeRestoreJob' {..} =
    Prelude.rnf restoreJobId

instance Data.ToHeaders DescribeRestoreJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRestoreJob where
  toPath DescribeRestoreJob' {..} =
    Prelude.mconcat
      ["/restore-jobs/", Data.toBS restoreJobId]

instance Data.ToQuery DescribeRestoreJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRestoreJobResponse' smart constructor.
data DescribeRestoreJobResponse = DescribeRestoreJobResponse'
  { -- | Returns the account ID that owns the restore job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the restored resource.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date and time that a job to restore a recovery point is completed,
    -- in Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource whose
    -- recovery point is being restored. The format of the ARN depends on the
    -- resource type of the backed-up resource.
    createdResourceArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a restore job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The amount of time in minutes that a job restoring a recovery point is
    -- expected to take.
    expectedCompletionTimeMinutes :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Contains an estimated percentage that is complete of a job at the time
    -- the job status was queried.
    percentDone :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | Returns metadata associated with a restore job listed by resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies the job that restores a recovery point.
    restoreJobId :: Prelude.Maybe Prelude.Text,
    -- | Status code specifying the state of the job that is initiated by Backup
    -- to restore a recovery point.
    status :: Prelude.Maybe RestoreJobStatus,
    -- | A message showing the status of a job to restore a recovery point.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRestoreJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeRestoreJobResponse_accountId' - Returns the account ID that owns the restore job.
--
-- 'backupSizeInBytes', 'describeRestoreJobResponse_backupSizeInBytes' - The size, in bytes, of the restored resource.
--
-- 'completionDate', 'describeRestoreJobResponse_completionDate' - The date and time that a job to restore a recovery point is completed,
-- in Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'createdResourceArn', 'describeRestoreJobResponse_createdResourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource whose
-- recovery point is being restored. The format of the ARN depends on the
-- resource type of the backed-up resource.
--
-- 'creationDate', 'describeRestoreJobResponse_creationDate' - The date and time that a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'expectedCompletionTimeMinutes', 'describeRestoreJobResponse_expectedCompletionTimeMinutes' - The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
--
-- 'iamRoleArn', 'describeRestoreJobResponse_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'percentDone', 'describeRestoreJobResponse_percentDone' - Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
--
-- 'recoveryPointArn', 'describeRestoreJobResponse_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'resourceType', 'describeRestoreJobResponse_resourceType' - Returns metadata associated with a restore job listed by resource type.
--
-- 'restoreJobId', 'describeRestoreJobResponse_restoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- 'status', 'describeRestoreJobResponse_status' - Status code specifying the state of the job that is initiated by Backup
-- to restore a recovery point.
--
-- 'statusMessage', 'describeRestoreJobResponse_statusMessage' - A message showing the status of a job to restore a recovery point.
--
-- 'httpStatus', 'describeRestoreJobResponse_httpStatus' - The response's http status code.
newDescribeRestoreJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRestoreJobResponse
newDescribeRestoreJobResponse pHttpStatus_ =
  DescribeRestoreJobResponse'
    { accountId =
        Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      createdResourceArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      expectedCompletionTimeMinutes = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      percentDone = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      restoreJobId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the account ID that owns the restore job.
describeRestoreJobResponse_accountId :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_accountId = Lens.lens (\DescribeRestoreJobResponse' {accountId} -> accountId) (\s@DescribeRestoreJobResponse' {} a -> s {accountId = a} :: DescribeRestoreJobResponse)

-- | The size, in bytes, of the restored resource.
describeRestoreJobResponse_backupSizeInBytes :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Integer)
describeRestoreJobResponse_backupSizeInBytes = Lens.lens (\DescribeRestoreJobResponse' {backupSizeInBytes} -> backupSizeInBytes) (\s@DescribeRestoreJobResponse' {} a -> s {backupSizeInBytes = a} :: DescribeRestoreJobResponse)

-- | The date and time that a job to restore a recovery point is completed,
-- in Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeRestoreJobResponse_completionDate :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.UTCTime)
describeRestoreJobResponse_completionDate = Lens.lens (\DescribeRestoreJobResponse' {completionDate} -> completionDate) (\s@DescribeRestoreJobResponse' {} a -> s {completionDate = a} :: DescribeRestoreJobResponse) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource whose
-- recovery point is being restored. The format of the ARN depends on the
-- resource type of the backed-up resource.
describeRestoreJobResponse_createdResourceArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_createdResourceArn = Lens.lens (\DescribeRestoreJobResponse' {createdResourceArn} -> createdResourceArn) (\s@DescribeRestoreJobResponse' {} a -> s {createdResourceArn = a} :: DescribeRestoreJobResponse)

-- | The date and time that a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeRestoreJobResponse_creationDate :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.UTCTime)
describeRestoreJobResponse_creationDate = Lens.lens (\DescribeRestoreJobResponse' {creationDate} -> creationDate) (\s@DescribeRestoreJobResponse' {} a -> s {creationDate = a} :: DescribeRestoreJobResponse) Prelude.. Lens.mapping Data._Time

-- | The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
describeRestoreJobResponse_expectedCompletionTimeMinutes :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Integer)
describeRestoreJobResponse_expectedCompletionTimeMinutes = Lens.lens (\DescribeRestoreJobResponse' {expectedCompletionTimeMinutes} -> expectedCompletionTimeMinutes) (\s@DescribeRestoreJobResponse' {} a -> s {expectedCompletionTimeMinutes = a} :: DescribeRestoreJobResponse)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
describeRestoreJobResponse_iamRoleArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_iamRoleArn = Lens.lens (\DescribeRestoreJobResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeRestoreJobResponse' {} a -> s {iamRoleArn = a} :: DescribeRestoreJobResponse)

-- | Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
describeRestoreJobResponse_percentDone :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_percentDone = Lens.lens (\DescribeRestoreJobResponse' {percentDone} -> percentDone) (\s@DescribeRestoreJobResponse' {} a -> s {percentDone = a} :: DescribeRestoreJobResponse)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
describeRestoreJobResponse_recoveryPointArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_recoveryPointArn = Lens.lens (\DescribeRestoreJobResponse' {recoveryPointArn} -> recoveryPointArn) (\s@DescribeRestoreJobResponse' {} a -> s {recoveryPointArn = a} :: DescribeRestoreJobResponse)

-- | Returns metadata associated with a restore job listed by resource type.
describeRestoreJobResponse_resourceType :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_resourceType = Lens.lens (\DescribeRestoreJobResponse' {resourceType} -> resourceType) (\s@DescribeRestoreJobResponse' {} a -> s {resourceType = a} :: DescribeRestoreJobResponse)

-- | Uniquely identifies the job that restores a recovery point.
describeRestoreJobResponse_restoreJobId :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_restoreJobId = Lens.lens (\DescribeRestoreJobResponse' {restoreJobId} -> restoreJobId) (\s@DescribeRestoreJobResponse' {} a -> s {restoreJobId = a} :: DescribeRestoreJobResponse)

-- | Status code specifying the state of the job that is initiated by Backup
-- to restore a recovery point.
describeRestoreJobResponse_status :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe RestoreJobStatus)
describeRestoreJobResponse_status = Lens.lens (\DescribeRestoreJobResponse' {status} -> status) (\s@DescribeRestoreJobResponse' {} a -> s {status = a} :: DescribeRestoreJobResponse)

-- | A message showing the status of a job to restore a recovery point.
describeRestoreJobResponse_statusMessage :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_statusMessage = Lens.lens (\DescribeRestoreJobResponse' {statusMessage} -> statusMessage) (\s@DescribeRestoreJobResponse' {} a -> s {statusMessage = a} :: DescribeRestoreJobResponse)

-- | The response's http status code.
describeRestoreJobResponse_httpStatus :: Lens.Lens' DescribeRestoreJobResponse Prelude.Int
describeRestoreJobResponse_httpStatus = Lens.lens (\DescribeRestoreJobResponse' {httpStatus} -> httpStatus) (\s@DescribeRestoreJobResponse' {} a -> s {httpStatus = a} :: DescribeRestoreJobResponse)

instance Prelude.NFData DescribeRestoreJobResponse where
  rnf DescribeRestoreJobResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf createdResourceArn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf expectedCompletionTimeMinutes
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf percentDone
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf restoreJobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
