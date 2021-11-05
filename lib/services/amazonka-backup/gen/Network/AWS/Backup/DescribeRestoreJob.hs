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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeRestoreJobResponse_status,
    describeRestoreJobResponse_iamRoleArn,
    describeRestoreJobResponse_expectedCompletionTimeMinutes,
    describeRestoreJobResponse_restoreJobId,
    describeRestoreJobResponse_resourceType,
    describeRestoreJobResponse_percentDone,
    describeRestoreJobResponse_accountId,
    describeRestoreJobResponse_createdResourceArn,
    describeRestoreJobResponse_statusMessage,
    describeRestoreJobResponse_recoveryPointArn,
    describeRestoreJobResponse_backupSizeInBytes,
    describeRestoreJobResponse_creationDate,
    describeRestoreJobResponse_completionDate,
    describeRestoreJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRestoreJobResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "IamRoleArn")
            Prelude.<*> (x Core..?> "ExpectedCompletionTimeMinutes")
            Prelude.<*> (x Core..?> "RestoreJobId")
            Prelude.<*> (x Core..?> "ResourceType")
            Prelude.<*> (x Core..?> "PercentDone")
            Prelude.<*> (x Core..?> "AccountId")
            Prelude.<*> (x Core..?> "CreatedResourceArn")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "RecoveryPointArn")
            Prelude.<*> (x Core..?> "BackupSizeInBytes")
            Prelude.<*> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "CompletionDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRestoreJob

instance Prelude.NFData DescribeRestoreJob

instance Core.ToHeaders DescribeRestoreJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeRestoreJob where
  toPath DescribeRestoreJob' {..} =
    Prelude.mconcat
      ["/restore-jobs/", Core.toBS restoreJobId]

instance Core.ToQuery DescribeRestoreJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRestoreJobResponse' smart constructor.
data DescribeRestoreJobResponse = DescribeRestoreJobResponse'
  { -- | Status code specifying the state of the job that is initiated by Backup
    -- to restore a recovery point.
    status :: Prelude.Maybe RestoreJobStatus,
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of time in minutes that a job restoring a recovery point is
    -- expected to take.
    expectedCompletionTimeMinutes :: Prelude.Maybe Prelude.Integer,
    -- | Uniquely identifies the job that restores a recovery point.
    restoreJobId :: Prelude.Maybe Prelude.Text,
    -- | Returns metadata associated with a restore job listed by resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Contains an estimated percentage that is complete of a job at the time
    -- the job status was queried.
    percentDone :: Prelude.Maybe Prelude.Text,
    -- | Returns the account ID that owns the restore job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource whose
    -- recovery point is being restored. The format of the ARN depends on the
    -- resource type of the backed-up resource.
    createdResourceArn :: Prelude.Maybe Prelude.Text,
    -- | A message showing the status of a job to restore a recovery point.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the restored resource.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date and time that a restore job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time that a job to restore a recovery point is completed,
    -- in Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Core.POSIX,
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
-- 'status', 'describeRestoreJobResponse_status' - Status code specifying the state of the job that is initiated by Backup
-- to restore a recovery point.
--
-- 'iamRoleArn', 'describeRestoreJobResponse_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'expectedCompletionTimeMinutes', 'describeRestoreJobResponse_expectedCompletionTimeMinutes' - The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
--
-- 'restoreJobId', 'describeRestoreJobResponse_restoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- 'resourceType', 'describeRestoreJobResponse_resourceType' - Returns metadata associated with a restore job listed by resource type.
--
-- 'percentDone', 'describeRestoreJobResponse_percentDone' - Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
--
-- 'accountId', 'describeRestoreJobResponse_accountId' - Returns the account ID that owns the restore job.
--
-- 'createdResourceArn', 'describeRestoreJobResponse_createdResourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource whose
-- recovery point is being restored. The format of the ARN depends on the
-- resource type of the backed-up resource.
--
-- 'statusMessage', 'describeRestoreJobResponse_statusMessage' - A message showing the status of a job to restore a recovery point.
--
-- 'recoveryPointArn', 'describeRestoreJobResponse_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'backupSizeInBytes', 'describeRestoreJobResponse_backupSizeInBytes' - The size, in bytes, of the restored resource.
--
-- 'creationDate', 'describeRestoreJobResponse_creationDate' - The date and time that a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'completionDate', 'describeRestoreJobResponse_completionDate' - The date and time that a job to restore a recovery point is completed,
-- in Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'httpStatus', 'describeRestoreJobResponse_httpStatus' - The response's http status code.
newDescribeRestoreJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRestoreJobResponse
newDescribeRestoreJobResponse pHttpStatus_ =
  DescribeRestoreJobResponse'
    { status =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      expectedCompletionTimeMinutes = Prelude.Nothing,
      restoreJobId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      percentDone = Prelude.Nothing,
      accountId = Prelude.Nothing,
      createdResourceArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Status code specifying the state of the job that is initiated by Backup
-- to restore a recovery point.
describeRestoreJobResponse_status :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe RestoreJobStatus)
describeRestoreJobResponse_status = Lens.lens (\DescribeRestoreJobResponse' {status} -> status) (\s@DescribeRestoreJobResponse' {} a -> s {status = a} :: DescribeRestoreJobResponse)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
describeRestoreJobResponse_iamRoleArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_iamRoleArn = Lens.lens (\DescribeRestoreJobResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeRestoreJobResponse' {} a -> s {iamRoleArn = a} :: DescribeRestoreJobResponse)

-- | The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
describeRestoreJobResponse_expectedCompletionTimeMinutes :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Integer)
describeRestoreJobResponse_expectedCompletionTimeMinutes = Lens.lens (\DescribeRestoreJobResponse' {expectedCompletionTimeMinutes} -> expectedCompletionTimeMinutes) (\s@DescribeRestoreJobResponse' {} a -> s {expectedCompletionTimeMinutes = a} :: DescribeRestoreJobResponse)

-- | Uniquely identifies the job that restores a recovery point.
describeRestoreJobResponse_restoreJobId :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_restoreJobId = Lens.lens (\DescribeRestoreJobResponse' {restoreJobId} -> restoreJobId) (\s@DescribeRestoreJobResponse' {} a -> s {restoreJobId = a} :: DescribeRestoreJobResponse)

-- | Returns metadata associated with a restore job listed by resource type.
describeRestoreJobResponse_resourceType :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_resourceType = Lens.lens (\DescribeRestoreJobResponse' {resourceType} -> resourceType) (\s@DescribeRestoreJobResponse' {} a -> s {resourceType = a} :: DescribeRestoreJobResponse)

-- | Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
describeRestoreJobResponse_percentDone :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_percentDone = Lens.lens (\DescribeRestoreJobResponse' {percentDone} -> percentDone) (\s@DescribeRestoreJobResponse' {} a -> s {percentDone = a} :: DescribeRestoreJobResponse)

-- | Returns the account ID that owns the restore job.
describeRestoreJobResponse_accountId :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_accountId = Lens.lens (\DescribeRestoreJobResponse' {accountId} -> accountId) (\s@DescribeRestoreJobResponse' {} a -> s {accountId = a} :: DescribeRestoreJobResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource whose
-- recovery point is being restored. The format of the ARN depends on the
-- resource type of the backed-up resource.
describeRestoreJobResponse_createdResourceArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_createdResourceArn = Lens.lens (\DescribeRestoreJobResponse' {createdResourceArn} -> createdResourceArn) (\s@DescribeRestoreJobResponse' {} a -> s {createdResourceArn = a} :: DescribeRestoreJobResponse)

-- | A message showing the status of a job to restore a recovery point.
describeRestoreJobResponse_statusMessage :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_statusMessage = Lens.lens (\DescribeRestoreJobResponse' {statusMessage} -> statusMessage) (\s@DescribeRestoreJobResponse' {} a -> s {statusMessage = a} :: DescribeRestoreJobResponse)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
describeRestoreJobResponse_recoveryPointArn :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Text)
describeRestoreJobResponse_recoveryPointArn = Lens.lens (\DescribeRestoreJobResponse' {recoveryPointArn} -> recoveryPointArn) (\s@DescribeRestoreJobResponse' {} a -> s {recoveryPointArn = a} :: DescribeRestoreJobResponse)

-- | The size, in bytes, of the restored resource.
describeRestoreJobResponse_backupSizeInBytes :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.Integer)
describeRestoreJobResponse_backupSizeInBytes = Lens.lens (\DescribeRestoreJobResponse' {backupSizeInBytes} -> backupSizeInBytes) (\s@DescribeRestoreJobResponse' {} a -> s {backupSizeInBytes = a} :: DescribeRestoreJobResponse)

-- | The date and time that a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeRestoreJobResponse_creationDate :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.UTCTime)
describeRestoreJobResponse_creationDate = Lens.lens (\DescribeRestoreJobResponse' {creationDate} -> creationDate) (\s@DescribeRestoreJobResponse' {} a -> s {creationDate = a} :: DescribeRestoreJobResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that a job to restore a recovery point is completed,
-- in Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeRestoreJobResponse_completionDate :: Lens.Lens' DescribeRestoreJobResponse (Prelude.Maybe Prelude.UTCTime)
describeRestoreJobResponse_completionDate = Lens.lens (\DescribeRestoreJobResponse' {completionDate} -> completionDate) (\s@DescribeRestoreJobResponse' {} a -> s {completionDate = a} :: DescribeRestoreJobResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeRestoreJobResponse_httpStatus :: Lens.Lens' DescribeRestoreJobResponse Prelude.Int
describeRestoreJobResponse_httpStatus = Lens.lens (\DescribeRestoreJobResponse' {httpStatus} -> httpStatus) (\s@DescribeRestoreJobResponse' {} a -> s {httpStatus = a} :: DescribeRestoreJobResponse)

instance Prelude.NFData DescribeRestoreJobResponse
