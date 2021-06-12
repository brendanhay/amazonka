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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobType where

import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The user import job type.
--
-- /See:/ 'newUserImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
  { -- | The message returned when the user import job is completed.
    completionMessage :: Core.Maybe Core.Text,
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
    status :: Core.Maybe UserImportJobStatusType,
    -- | The date when the user import job was started.
    startDate :: Core.Maybe Core.POSIX,
    -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Core.Maybe Core.Text,
    -- | The date the user import job was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The number of users that were skipped.
    skippedUsers :: Core.Maybe Core.Integer,
    -- | The number of users that could not be imported.
    failedUsers :: Core.Maybe Core.Integer,
    -- | The number of users that were successfully imported.
    importedUsers :: Core.Maybe Core.Integer,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import
    -- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
    -- in the Amazon Cognito Developer Guide.
    cloudWatchLogsRoleArn :: Core.Maybe Core.Text,
    -- | The pre-signed URL to be used to upload the @.csv@ file.
    preSignedUrl :: Core.Maybe Core.Text,
    -- | The date when the user import job was completed.
    completionDate :: Core.Maybe Core.POSIX,
    -- | The job name for the user import job.
    jobName :: Core.Maybe Core.Text,
    -- | The job ID for the user import job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserImportJobType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionMessage', 'userImportJobType_completionMessage' - The message returned when the user import job is completed.
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
-- 'startDate', 'userImportJobType_startDate' - The date when the user import job was started.
--
-- 'userPoolId', 'userImportJobType_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'creationDate', 'userImportJobType_creationDate' - The date the user import job was created.
--
-- 'skippedUsers', 'userImportJobType_skippedUsers' - The number of users that were skipped.
--
-- 'failedUsers', 'userImportJobType_failedUsers' - The number of users that could not be imported.
--
-- 'importedUsers', 'userImportJobType_importedUsers' - The number of users that were successfully imported.
--
-- 'cloudWatchLogsRoleArn', 'userImportJobType_cloudWatchLogsRoleArn' - The role ARN for the Amazon CloudWatch Logging role for the user import
-- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
-- in the Amazon Cognito Developer Guide.
--
-- 'preSignedUrl', 'userImportJobType_preSignedUrl' - The pre-signed URL to be used to upload the @.csv@ file.
--
-- 'completionDate', 'userImportJobType_completionDate' - The date when the user import job was completed.
--
-- 'jobName', 'userImportJobType_jobName' - The job name for the user import job.
--
-- 'jobId', 'userImportJobType_jobId' - The job ID for the user import job.
newUserImportJobType ::
  UserImportJobType
newUserImportJobType =
  UserImportJobType'
    { completionMessage =
        Core.Nothing,
      status = Core.Nothing,
      startDate = Core.Nothing,
      userPoolId = Core.Nothing,
      creationDate = Core.Nothing,
      skippedUsers = Core.Nothing,
      failedUsers = Core.Nothing,
      importedUsers = Core.Nothing,
      cloudWatchLogsRoleArn = Core.Nothing,
      preSignedUrl = Core.Nothing,
      completionDate = Core.Nothing,
      jobName = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The message returned when the user import job is completed.
userImportJobType_completionMessage :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_completionMessage = Lens.lens (\UserImportJobType' {completionMessage} -> completionMessage) (\s@UserImportJobType' {} a -> s {completionMessage = a} :: UserImportJobType)

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
userImportJobType_status :: Lens.Lens' UserImportJobType (Core.Maybe UserImportJobStatusType)
userImportJobType_status = Lens.lens (\UserImportJobType' {status} -> status) (\s@UserImportJobType' {} a -> s {status = a} :: UserImportJobType)

-- | The date when the user import job was started.
userImportJobType_startDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.UTCTime)
userImportJobType_startDate = Lens.lens (\UserImportJobType' {startDate} -> startDate) (\s@UserImportJobType' {} a -> s {startDate = a} :: UserImportJobType) Core.. Lens.mapping Core._Time

-- | The user pool ID for the user pool that the users are being imported
-- into.
userImportJobType_userPoolId :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_userPoolId = Lens.lens (\UserImportJobType' {userPoolId} -> userPoolId) (\s@UserImportJobType' {} a -> s {userPoolId = a} :: UserImportJobType)

-- | The date the user import job was created.
userImportJobType_creationDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.UTCTime)
userImportJobType_creationDate = Lens.lens (\UserImportJobType' {creationDate} -> creationDate) (\s@UserImportJobType' {} a -> s {creationDate = a} :: UserImportJobType) Core.. Lens.mapping Core._Time

-- | The number of users that were skipped.
userImportJobType_skippedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
userImportJobType_skippedUsers = Lens.lens (\UserImportJobType' {skippedUsers} -> skippedUsers) (\s@UserImportJobType' {} a -> s {skippedUsers = a} :: UserImportJobType)

-- | The number of users that could not be imported.
userImportJobType_failedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
userImportJobType_failedUsers = Lens.lens (\UserImportJobType' {failedUsers} -> failedUsers) (\s@UserImportJobType' {} a -> s {failedUsers = a} :: UserImportJobType)

-- | The number of users that were successfully imported.
userImportJobType_importedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
userImportJobType_importedUsers = Lens.lens (\UserImportJobType' {importedUsers} -> importedUsers) (\s@UserImportJobType' {} a -> s {importedUsers = a} :: UserImportJobType)

-- | The role ARN for the Amazon CloudWatch Logging role for the user import
-- job. For more information, see \"Creating the CloudWatch Logs IAM Role\"
-- in the Amazon Cognito Developer Guide.
userImportJobType_cloudWatchLogsRoleArn :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_cloudWatchLogsRoleArn = Lens.lens (\UserImportJobType' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@UserImportJobType' {} a -> s {cloudWatchLogsRoleArn = a} :: UserImportJobType)

-- | The pre-signed URL to be used to upload the @.csv@ file.
userImportJobType_preSignedUrl :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_preSignedUrl = Lens.lens (\UserImportJobType' {preSignedUrl} -> preSignedUrl) (\s@UserImportJobType' {} a -> s {preSignedUrl = a} :: UserImportJobType)

-- | The date when the user import job was completed.
userImportJobType_completionDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.UTCTime)
userImportJobType_completionDate = Lens.lens (\UserImportJobType' {completionDate} -> completionDate) (\s@UserImportJobType' {} a -> s {completionDate = a} :: UserImportJobType) Core.. Lens.mapping Core._Time

-- | The job name for the user import job.
userImportJobType_jobName :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_jobName = Lens.lens (\UserImportJobType' {jobName} -> jobName) (\s@UserImportJobType' {} a -> s {jobName = a} :: UserImportJobType)

-- | The job ID for the user import job.
userImportJobType_jobId :: Lens.Lens' UserImportJobType (Core.Maybe Core.Text)
userImportJobType_jobId = Lens.lens (\UserImportJobType' {jobId} -> jobId) (\s@UserImportJobType' {} a -> s {jobId = a} :: UserImportJobType)

instance Core.FromJSON UserImportJobType where
  parseJSON =
    Core.withObject
      "UserImportJobType"
      ( \x ->
          UserImportJobType'
            Core.<$> (x Core..:? "CompletionMessage")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartDate")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "SkippedUsers")
            Core.<*> (x Core..:? "FailedUsers")
            Core.<*> (x Core..:? "ImportedUsers")
            Core.<*> (x Core..:? "CloudWatchLogsRoleArn")
            Core.<*> (x Core..:? "PreSignedUrl")
            Core.<*> (x Core..:? "CompletionDate")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable UserImportJobType

instance Core.NFData UserImportJobType
