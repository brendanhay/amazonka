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
-- Module      : Amazonka.Backup.DescribeBackupJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns backup job details for the specified @BackupJobId@.
module Amazonka.Backup.DescribeBackupJob
  ( -- * Creating a Request
    DescribeBackupJob (..),
    newDescribeBackupJob,

    -- * Request Lenses
    describeBackupJob_backupJobId,

    -- * Destructuring the Response
    DescribeBackupJobResponse (..),
    newDescribeBackupJobResponse,

    -- * Response Lenses
    describeBackupJobResponse_resourceType,
    describeBackupJobResponse_recoveryPointArn,
    describeBackupJobResponse_completionDate,
    describeBackupJobResponse_backupVaultName,
    describeBackupJobResponse_state,
    describeBackupJobResponse_creationDate,
    describeBackupJobResponse_backupSizeInBytes,
    describeBackupJobResponse_backupVaultArn,
    describeBackupJobResponse_backupOptions,
    describeBackupJobResponse_iamRoleArn,
    describeBackupJobResponse_accountId,
    describeBackupJobResponse_backupType,
    describeBackupJobResponse_percentDone,
    describeBackupJobResponse_expectedCompletionDate,
    describeBackupJobResponse_backupJobId,
    describeBackupJobResponse_resourceArn,
    describeBackupJobResponse_startBy,
    describeBackupJobResponse_statusMessage,
    describeBackupJobResponse_createdBy,
    describeBackupJobResponse_bytesTransferred,
    describeBackupJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBackupJob' smart constructor.
data DescribeBackupJob = DescribeBackupJob'
  { -- | Uniquely identifies a request to Backup to back up a resource.
    backupJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobId', 'describeBackupJob_backupJobId' - Uniquely identifies a request to Backup to back up a resource.
newDescribeBackupJob ::
  -- | 'backupJobId'
  Prelude.Text ->
  DescribeBackupJob
newDescribeBackupJob pBackupJobId_ =
  DescribeBackupJob' {backupJobId = pBackupJobId_}

-- | Uniquely identifies a request to Backup to back up a resource.
describeBackupJob_backupJobId :: Lens.Lens' DescribeBackupJob Prelude.Text
describeBackupJob_backupJobId = Lens.lens (\DescribeBackupJob' {backupJobId} -> backupJobId) (\s@DescribeBackupJob' {} a -> s {backupJobId = a} :: DescribeBackupJob)

instance Core.AWSRequest DescribeBackupJob where
  type
    AWSResponse DescribeBackupJob =
      DescribeBackupJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupJobResponse'
            Prelude.<$> (x Data..?> "ResourceType")
            Prelude.<*> (x Data..?> "RecoveryPointArn")
            Prelude.<*> (x Data..?> "CompletionDate")
            Prelude.<*> (x Data..?> "BackupVaultName")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "BackupSizeInBytes")
            Prelude.<*> (x Data..?> "BackupVaultArn")
            Prelude.<*> (x Data..?> "BackupOptions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "IamRoleArn")
            Prelude.<*> (x Data..?> "AccountId")
            Prelude.<*> (x Data..?> "BackupType")
            Prelude.<*> (x Data..?> "PercentDone")
            Prelude.<*> (x Data..?> "ExpectedCompletionDate")
            Prelude.<*> (x Data..?> "BackupJobId")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "StartBy")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "BytesTransferred")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackupJob where
  hashWithSalt _salt DescribeBackupJob' {..} =
    _salt `Prelude.hashWithSalt` backupJobId

instance Prelude.NFData DescribeBackupJob where
  rnf DescribeBackupJob' {..} = Prelude.rnf backupJobId

instance Data.ToHeaders DescribeBackupJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeBackupJob where
  toPath DescribeBackupJob' {..} =
    Prelude.mconcat
      ["/backup-jobs/", Data.toBS backupJobId]

instance Data.ToQuery DescribeBackupJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupJobResponse' smart constructor.
data DescribeBackupJobResponse = DescribeBackupJobResponse'
  { -- | The type of Amazon Web Services resource to be backed up; for example,
    -- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
    -- Relational Database Service (Amazon RDS) database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a job to create a backup job is completed, in
    -- Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The current state of a resource recovery point.
    state :: Prelude.Maybe BackupJobState,
    -- | The date and time that a backup job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The size, in bytes, of a backup.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | Represents the options specified as part of backup plan or on-demand
    -- backup job.
    backupOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Returns the account ID that owns the backup job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Represents the actual backup type selected for a backup job. For
    -- example, if a successful Windows Volume Shadow Copy Service (VSS) backup
    -- was taken, @BackupType@ returns @\"WindowsVSS\"@. If @BackupType@ is
    -- empty, then the backup type was a regular backup.
    backupType :: Prelude.Maybe Prelude.Text,
    -- | Contains an estimated percentage that is complete of a job at the time
    -- the job status was queried.
    percentDone :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a job to back up resources is expected to be
    -- completed, in Unix format and Coordinated Universal Time (UTC). The
    -- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
    -- example, the value 1516925490.087 represents Friday, January 26, 2018
    -- 12:11:30.087 AM.
    expectedCompletionDate :: Prelude.Maybe Data.POSIX,
    -- | Uniquely identifies a request to Backup to back up a resource.
    backupJobId :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a saved resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in Unix format and Coordinated Universal Time (UTC)
    -- when a backup job must be started before it is canceled. The value is
    -- calculated by adding the start window to the scheduled time. So if the
    -- scheduled time were 6:00 PM and the start window is 2 hours, the
    -- @StartBy@ time would be 8:00 PM on the date specified. The value of
    -- @StartBy@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    startBy :: Prelude.Maybe Data.POSIX,
    -- | A detailed message explaining the status of the job to back up a
    -- resource.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains identifying information about the creation of a backup job,
    -- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
    -- @BackupRuleId@ of the backup plan that is used to create it.
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | The size in bytes transferred to a backup vault at the time that the job
    -- status was queried.
    bytesTransferred :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'describeBackupJobResponse_resourceType' - The type of Amazon Web Services resource to be backed up; for example,
-- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database.
--
-- 'recoveryPointArn', 'describeBackupJobResponse_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'completionDate', 'describeBackupJobResponse_completionDate' - The date and time that a job to create a backup job is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'describeBackupJobResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'state', 'describeBackupJobResponse_state' - The current state of a resource recovery point.
--
-- 'creationDate', 'describeBackupJobResponse_creationDate' - The date and time that a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupSizeInBytes', 'describeBackupJobResponse_backupSizeInBytes' - The size, in bytes, of a backup.
--
-- 'backupVaultArn', 'describeBackupJobResponse_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'backupOptions', 'describeBackupJobResponse_backupOptions' - Represents the options specified as part of backup plan or on-demand
-- backup job.
--
-- 'iamRoleArn', 'describeBackupJobResponse_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'accountId', 'describeBackupJobResponse_accountId' - Returns the account ID that owns the backup job.
--
-- 'backupType', 'describeBackupJobResponse_backupType' - Represents the actual backup type selected for a backup job. For
-- example, if a successful Windows Volume Shadow Copy Service (VSS) backup
-- was taken, @BackupType@ returns @\"WindowsVSS\"@. If @BackupType@ is
-- empty, then the backup type was a regular backup.
--
-- 'percentDone', 'describeBackupJobResponse_percentDone' - Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
--
-- 'expectedCompletionDate', 'describeBackupJobResponse_expectedCompletionDate' - The date and time that a job to back up resources is expected to be
-- completed, in Unix format and Coordinated Universal Time (UTC). The
-- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
--
-- 'backupJobId', 'describeBackupJobResponse_backupJobId' - Uniquely identifies a request to Backup to back up a resource.
--
-- 'resourceArn', 'describeBackupJobResponse_resourceArn' - An ARN that uniquely identifies a saved resource. The format of the ARN
-- depends on the resource type.
--
-- 'startBy', 'describeBackupJobResponse_startBy' - Specifies the time in Unix format and Coordinated Universal Time (UTC)
-- when a backup job must be started before it is canceled. The value is
-- calculated by adding the start window to the scheduled time. So if the
-- scheduled time were 6:00 PM and the start window is 2 hours, the
-- @StartBy@ time would be 8:00 PM on the date specified. The value of
-- @StartBy@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'statusMessage', 'describeBackupJobResponse_statusMessage' - A detailed message explaining the status of the job to back up a
-- resource.
--
-- 'createdBy', 'describeBackupJobResponse_createdBy' - Contains identifying information about the creation of a backup job,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
--
-- 'bytesTransferred', 'describeBackupJobResponse_bytesTransferred' - The size in bytes transferred to a backup vault at the time that the job
-- status was queried.
--
-- 'httpStatus', 'describeBackupJobResponse_httpStatus' - The response's http status code.
newDescribeBackupJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBackupJobResponse
newDescribeBackupJobResponse pHttpStatus_ =
  DescribeBackupJobResponse'
    { resourceType =
        Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      state = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      backupOptions = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      accountId = Prelude.Nothing,
      backupType = Prelude.Nothing,
      percentDone = Prelude.Nothing,
      expectedCompletionDate = Prelude.Nothing,
      backupJobId = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      startBy = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      bytesTransferred = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of Amazon Web Services resource to be backed up; for example,
-- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database.
describeBackupJobResponse_resourceType :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_resourceType = Lens.lens (\DescribeBackupJobResponse' {resourceType} -> resourceType) (\s@DescribeBackupJobResponse' {} a -> s {resourceType = a} :: DescribeBackupJobResponse)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
describeBackupJobResponse_recoveryPointArn :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_recoveryPointArn = Lens.lens (\DescribeBackupJobResponse' {recoveryPointArn} -> recoveryPointArn) (\s@DescribeBackupJobResponse' {} a -> s {recoveryPointArn = a} :: DescribeBackupJobResponse)

-- | The date and time that a job to create a backup job is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeBackupJobResponse_completionDate :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupJobResponse_completionDate = Lens.lens (\DescribeBackupJobResponse' {completionDate} -> completionDate) (\s@DescribeBackupJobResponse' {} a -> s {completionDate = a} :: DescribeBackupJobResponse) Prelude.. Lens.mapping Data._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
describeBackupJobResponse_backupVaultName :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_backupVaultName = Lens.lens (\DescribeBackupJobResponse' {backupVaultName} -> backupVaultName) (\s@DescribeBackupJobResponse' {} a -> s {backupVaultName = a} :: DescribeBackupJobResponse)

-- | The current state of a resource recovery point.
describeBackupJobResponse_state :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe BackupJobState)
describeBackupJobResponse_state = Lens.lens (\DescribeBackupJobResponse' {state} -> state) (\s@DescribeBackupJobResponse' {} a -> s {state = a} :: DescribeBackupJobResponse)

-- | The date and time that a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeBackupJobResponse_creationDate :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupJobResponse_creationDate = Lens.lens (\DescribeBackupJobResponse' {creationDate} -> creationDate) (\s@DescribeBackupJobResponse' {} a -> s {creationDate = a} :: DescribeBackupJobResponse) Prelude.. Lens.mapping Data._Time

-- | The size, in bytes, of a backup.
describeBackupJobResponse_backupSizeInBytes :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Integer)
describeBackupJobResponse_backupSizeInBytes = Lens.lens (\DescribeBackupJobResponse' {backupSizeInBytes} -> backupSizeInBytes) (\s@DescribeBackupJobResponse' {} a -> s {backupSizeInBytes = a} :: DescribeBackupJobResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
describeBackupJobResponse_backupVaultArn :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_backupVaultArn = Lens.lens (\DescribeBackupJobResponse' {backupVaultArn} -> backupVaultArn) (\s@DescribeBackupJobResponse' {} a -> s {backupVaultArn = a} :: DescribeBackupJobResponse)

-- | Represents the options specified as part of backup plan or on-demand
-- backup job.
describeBackupJobResponse_backupOptions :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeBackupJobResponse_backupOptions = Lens.lens (\DescribeBackupJobResponse' {backupOptions} -> backupOptions) (\s@DescribeBackupJobResponse' {} a -> s {backupOptions = a} :: DescribeBackupJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
describeBackupJobResponse_iamRoleArn :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_iamRoleArn = Lens.lens (\DescribeBackupJobResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeBackupJobResponse' {} a -> s {iamRoleArn = a} :: DescribeBackupJobResponse)

-- | Returns the account ID that owns the backup job.
describeBackupJobResponse_accountId :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_accountId = Lens.lens (\DescribeBackupJobResponse' {accountId} -> accountId) (\s@DescribeBackupJobResponse' {} a -> s {accountId = a} :: DescribeBackupJobResponse)

-- | Represents the actual backup type selected for a backup job. For
-- example, if a successful Windows Volume Shadow Copy Service (VSS) backup
-- was taken, @BackupType@ returns @\"WindowsVSS\"@. If @BackupType@ is
-- empty, then the backup type was a regular backup.
describeBackupJobResponse_backupType :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_backupType = Lens.lens (\DescribeBackupJobResponse' {backupType} -> backupType) (\s@DescribeBackupJobResponse' {} a -> s {backupType = a} :: DescribeBackupJobResponse)

-- | Contains an estimated percentage that is complete of a job at the time
-- the job status was queried.
describeBackupJobResponse_percentDone :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_percentDone = Lens.lens (\DescribeBackupJobResponse' {percentDone} -> percentDone) (\s@DescribeBackupJobResponse' {} a -> s {percentDone = a} :: DescribeBackupJobResponse)

-- | The date and time that a job to back up resources is expected to be
-- completed, in Unix format and Coordinated Universal Time (UTC). The
-- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
describeBackupJobResponse_expectedCompletionDate :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupJobResponse_expectedCompletionDate = Lens.lens (\DescribeBackupJobResponse' {expectedCompletionDate} -> expectedCompletionDate) (\s@DescribeBackupJobResponse' {} a -> s {expectedCompletionDate = a} :: DescribeBackupJobResponse) Prelude.. Lens.mapping Data._Time

-- | Uniquely identifies a request to Backup to back up a resource.
describeBackupJobResponse_backupJobId :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_backupJobId = Lens.lens (\DescribeBackupJobResponse' {backupJobId} -> backupJobId) (\s@DescribeBackupJobResponse' {} a -> s {backupJobId = a} :: DescribeBackupJobResponse)

-- | An ARN that uniquely identifies a saved resource. The format of the ARN
-- depends on the resource type.
describeBackupJobResponse_resourceArn :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_resourceArn = Lens.lens (\DescribeBackupJobResponse' {resourceArn} -> resourceArn) (\s@DescribeBackupJobResponse' {} a -> s {resourceArn = a} :: DescribeBackupJobResponse)

-- | Specifies the time in Unix format and Coordinated Universal Time (UTC)
-- when a backup job must be started before it is canceled. The value is
-- calculated by adding the start window to the scheduled time. So if the
-- scheduled time were 6:00 PM and the start window is 2 hours, the
-- @StartBy@ time would be 8:00 PM on the date specified. The value of
-- @StartBy@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeBackupJobResponse_startBy :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.UTCTime)
describeBackupJobResponse_startBy = Lens.lens (\DescribeBackupJobResponse' {startBy} -> startBy) (\s@DescribeBackupJobResponse' {} a -> s {startBy = a} :: DescribeBackupJobResponse) Prelude.. Lens.mapping Data._Time

-- | A detailed message explaining the status of the job to back up a
-- resource.
describeBackupJobResponse_statusMessage :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Text)
describeBackupJobResponse_statusMessage = Lens.lens (\DescribeBackupJobResponse' {statusMessage} -> statusMessage) (\s@DescribeBackupJobResponse' {} a -> s {statusMessage = a} :: DescribeBackupJobResponse)

-- | Contains identifying information about the creation of a backup job,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
describeBackupJobResponse_createdBy :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe RecoveryPointCreator)
describeBackupJobResponse_createdBy = Lens.lens (\DescribeBackupJobResponse' {createdBy} -> createdBy) (\s@DescribeBackupJobResponse' {} a -> s {createdBy = a} :: DescribeBackupJobResponse)

-- | The size in bytes transferred to a backup vault at the time that the job
-- status was queried.
describeBackupJobResponse_bytesTransferred :: Lens.Lens' DescribeBackupJobResponse (Prelude.Maybe Prelude.Integer)
describeBackupJobResponse_bytesTransferred = Lens.lens (\DescribeBackupJobResponse' {bytesTransferred} -> bytesTransferred) (\s@DescribeBackupJobResponse' {} a -> s {bytesTransferred = a} :: DescribeBackupJobResponse)

-- | The response's http status code.
describeBackupJobResponse_httpStatus :: Lens.Lens' DescribeBackupJobResponse Prelude.Int
describeBackupJobResponse_httpStatus = Lens.lens (\DescribeBackupJobResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupJobResponse' {} a -> s {httpStatus = a} :: DescribeBackupJobResponse)

instance Prelude.NFData DescribeBackupJobResponse where
  rnf DescribeBackupJobResponse' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf backupOptions
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf backupType
      `Prelude.seq` Prelude.rnf percentDone
      `Prelude.seq` Prelude.rnf expectedCompletionDate
      `Prelude.seq` Prelude.rnf backupJobId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf startBy
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf bytesTransferred
      `Prelude.seq` Prelude.rnf httpStatus
