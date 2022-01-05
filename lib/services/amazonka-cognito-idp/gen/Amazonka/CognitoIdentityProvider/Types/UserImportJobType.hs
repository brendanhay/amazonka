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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserImportJobType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserImportJobType where

import Amazonka.CognitoIdentityProvider.Types.UserImportJobStatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The user import job type.
--
-- /See:/ 'newUserImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
  { -- | The status of the user import job. One of the following:
    --
    -- -   @Created@ - The job was created but not started.
    --
    -- -   @Pending@ - A transition state. You have started the job, but it has
    --     not begun importing users yet.
    --
    -- -   @InProgress@ - The job has started, and users are being imported.
    --
    -- -   @Stopping@ - You have stopped the job, but the job has not stopped
    --     importing users yet.
    --
    -- -   @Stopped@ - You have stopped the job, and the job has stopped
    --     importing users.
    --
    -- -   @Succeeded@ - The job has completed successfully.
    --
    -- -   @Failed@ - The job has stopped due to an error.
    --
    -- -   @Expired@ - You created a job, but did not start the job within
    --     24-48 hours. All data associated with the job was deleted, and the
    --     job cannot be started.
    status :: Prelude.Maybe UserImportJobStatusType,
    -- | The number of users that were skipped.
    skippedUsers :: Prelude.Maybe Prelude.Integer,
    -- | The job ID for the user import job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The job name for the user import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The pre-signed URL to be used to upload the @.csv@ file.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The number of users that could not be imported.
    failedUsers :: Prelude.Maybe Prelude.Integer,
    -- | The date when the user import job was started.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | The message returned when the user import job is completed.
    completionMessage :: Prelude.Maybe Prelude.Text,
    -- | The date the user import job was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date when the user import job was completed.
    completionDate :: Prelude.Maybe Core.POSIX,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import
    -- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
    -- in the Amazon Cognito Developer Guide.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The number of users that were successfully imported.
    importedUsers :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserImportJobType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'userImportJobType_status' - The status of the user import job. One of the following:
--
-- -   @Created@ - The job was created but not started.
--
-- -   @Pending@ - A transition state. You have started the job, but it has
--     not begun importing users yet.
--
-- -   @InProgress@ - The job has started, and users are being imported.
--
-- -   @Stopping@ - You have stopped the job, but the job has not stopped
--     importing users yet.
--
-- -   @Stopped@ - You have stopped the job, and the job has stopped
--     importing users.
--
-- -   @Succeeded@ - The job has completed successfully.
--
-- -   @Failed@ - The job has stopped due to an error.
--
-- -   @Expired@ - You created a job, but did not start the job within
--     24-48 hours. All data associated with the job was deleted, and the
--     job cannot be started.
--
-- 'skippedUsers', 'userImportJobType_skippedUsers' - The number of users that were skipped.
--
-- 'jobId', 'userImportJobType_jobId' - The job ID for the user import job.
--
-- 'userPoolId', 'userImportJobType_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'jobName', 'userImportJobType_jobName' - The job name for the user import job.
--
-- 'preSignedUrl', 'userImportJobType_preSignedUrl' - The pre-signed URL to be used to upload the @.csv@ file.
--
-- 'failedUsers', 'userImportJobType_failedUsers' - The number of users that could not be imported.
--
-- 'startDate', 'userImportJobType_startDate' - The date when the user import job was started.
--
-- 'completionMessage', 'userImportJobType_completionMessage' - The message returned when the user import job is completed.
--
-- 'creationDate', 'userImportJobType_creationDate' - The date the user import job was created.
--
-- 'completionDate', 'userImportJobType_completionDate' - The date when the user import job was completed.
--
-- 'cloudWatchLogsRoleArn', 'userImportJobType_cloudWatchLogsRoleArn' - The role ARN for the Amazon CloudWatch Logging role for the user import
-- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
-- in the Amazon Cognito Developer Guide.
--
-- 'importedUsers', 'userImportJobType_importedUsers' - The number of users that were successfully imported.
newUserImportJobType ::
  UserImportJobType
newUserImportJobType =
  UserImportJobType'
    { status = Prelude.Nothing,
      skippedUsers = Prelude.Nothing,
      jobId = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      preSignedUrl = Prelude.Nothing,
      failedUsers = Prelude.Nothing,
      startDate = Prelude.Nothing,
      completionMessage = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      cloudWatchLogsRoleArn = Prelude.Nothing,
      importedUsers = Prelude.Nothing
    }

-- | The status of the user import job. One of the following:
--
-- -   @Created@ - The job was created but not started.
--
-- -   @Pending@ - A transition state. You have started the job, but it has
--     not begun importing users yet.
--
-- -   @InProgress@ - The job has started, and users are being imported.
--
-- -   @Stopping@ - You have stopped the job, but the job has not stopped
--     importing users yet.
--
-- -   @Stopped@ - You have stopped the job, and the job has stopped
--     importing users.
--
-- -   @Succeeded@ - The job has completed successfully.
--
-- -   @Failed@ - The job has stopped due to an error.
--
-- -   @Expired@ - You created a job, but did not start the job within
--     24-48 hours. All data associated with the job was deleted, and the
--     job cannot be started.
userImportJobType_status :: Lens.Lens' UserImportJobType (Prelude.Maybe UserImportJobStatusType)
userImportJobType_status = Lens.lens (\UserImportJobType' {status} -> status) (\s@UserImportJobType' {} a -> s {status = a} :: UserImportJobType)

-- | The number of users that were skipped.
userImportJobType_skippedUsers :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Integer)
userImportJobType_skippedUsers = Lens.lens (\UserImportJobType' {skippedUsers} -> skippedUsers) (\s@UserImportJobType' {} a -> s {skippedUsers = a} :: UserImportJobType)

-- | The job ID for the user import job.
userImportJobType_jobId :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_jobId = Lens.lens (\UserImportJobType' {jobId} -> jobId) (\s@UserImportJobType' {} a -> s {jobId = a} :: UserImportJobType)

-- | The user pool ID for the user pool that the users are being imported
-- into.
userImportJobType_userPoolId :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_userPoolId = Lens.lens (\UserImportJobType' {userPoolId} -> userPoolId) (\s@UserImportJobType' {} a -> s {userPoolId = a} :: UserImportJobType)

-- | The job name for the user import job.
userImportJobType_jobName :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_jobName = Lens.lens (\UserImportJobType' {jobName} -> jobName) (\s@UserImportJobType' {} a -> s {jobName = a} :: UserImportJobType)

-- | The pre-signed URL to be used to upload the @.csv@ file.
userImportJobType_preSignedUrl :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_preSignedUrl = Lens.lens (\UserImportJobType' {preSignedUrl} -> preSignedUrl) (\s@UserImportJobType' {} a -> s {preSignedUrl = a} :: UserImportJobType)

-- | The number of users that could not be imported.
userImportJobType_failedUsers :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Integer)
userImportJobType_failedUsers = Lens.lens (\UserImportJobType' {failedUsers} -> failedUsers) (\s@UserImportJobType' {} a -> s {failedUsers = a} :: UserImportJobType)

-- | The date when the user import job was started.
userImportJobType_startDate :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.UTCTime)
userImportJobType_startDate = Lens.lens (\UserImportJobType' {startDate} -> startDate) (\s@UserImportJobType' {} a -> s {startDate = a} :: UserImportJobType) Prelude.. Lens.mapping Core._Time

-- | The message returned when the user import job is completed.
userImportJobType_completionMessage :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_completionMessage = Lens.lens (\UserImportJobType' {completionMessage} -> completionMessage) (\s@UserImportJobType' {} a -> s {completionMessage = a} :: UserImportJobType)

-- | The date the user import job was created.
userImportJobType_creationDate :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.UTCTime)
userImportJobType_creationDate = Lens.lens (\UserImportJobType' {creationDate} -> creationDate) (\s@UserImportJobType' {} a -> s {creationDate = a} :: UserImportJobType) Prelude.. Lens.mapping Core._Time

-- | The date when the user import job was completed.
userImportJobType_completionDate :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.UTCTime)
userImportJobType_completionDate = Lens.lens (\UserImportJobType' {completionDate} -> completionDate) (\s@UserImportJobType' {} a -> s {completionDate = a} :: UserImportJobType) Prelude.. Lens.mapping Core._Time

-- | The role ARN for the Amazon CloudWatch Logging role for the user import
-- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
-- in the Amazon Cognito Developer Guide.
userImportJobType_cloudWatchLogsRoleArn :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Text)
userImportJobType_cloudWatchLogsRoleArn = Lens.lens (\UserImportJobType' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UserImportJobType' {} a -> s {cloudWatchLogsRoleArn = a} :: UserImportJobType)

-- | The number of users that were successfully imported.
userImportJobType_importedUsers :: Lens.Lens' UserImportJobType (Prelude.Maybe Prelude.Integer)
userImportJobType_importedUsers = Lens.lens (\UserImportJobType' {importedUsers} -> importedUsers) (\s@UserImportJobType' {} a -> s {importedUsers = a} :: UserImportJobType)

instance Core.FromJSON UserImportJobType where
  parseJSON =
    Core.withObject
      "UserImportJobType"
      ( \x ->
          UserImportJobType'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SkippedUsers")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "PreSignedUrl")
            Prelude.<*> (x Core..:? "FailedUsers")
            Prelude.<*> (x Core..:? "StartDate")
            Prelude.<*> (x Core..:? "CompletionMessage")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "CompletionDate")
            Prelude.<*> (x Core..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Core..:? "ImportedUsers")
      )

instance Prelude.Hashable UserImportJobType where
  hashWithSalt _salt UserImportJobType' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` skippedUsers
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` failedUsers
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` completionMessage
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn
      `Prelude.hashWithSalt` importedUsers

instance Prelude.NFData UserImportJobType where
  rnf UserImportJobType' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf skippedUsers
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf failedUsers
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf completionMessage
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf importedUsers
