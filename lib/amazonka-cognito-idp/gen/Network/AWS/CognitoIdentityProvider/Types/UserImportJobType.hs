{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
  ( UserImportJobType (..),

    -- * Smart constructor
    mkUserImportJobType,

    -- * Lenses
    uijtStatus,
    uijtSkippedUsers,
    uijtJobId,
    uijtUserPoolId,
    uijtJobName,
    uijtPreSignedURL,
    uijtFailedUsers,
    uijtStartDate,
    uijtCompletionMessage,
    uijtCreationDate,
    uijtCompletionDate,
    uijtCloudWatchLogsRoleARN,
    uijtImportedUsers,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user import job type.
--
-- /See:/ 'mkUserImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
  { -- | The status of the user import job. One of the following:
    --
    --
    --     * @Created@ - The job was created but not started.
    --
    --
    --     * @Pending@ - A transition state. You have started the job, but it has not begun importing users yet.
    --
    --
    --     * @InProgress@ - The job has started, and users are being imported.
    --
    --
    --     * @Stopping@ - You have stopped the job, but the job has not stopped importing users yet.
    --
    --
    --     * @Stopped@ - You have stopped the job, and the job has stopped importing users.
    --
    --
    --     * @Succeeded@ - The job has completed successfully.
    --
    --
    --     * @Failed@ - The job has stopped due to an error.
    --
    --
    --     * @Expired@ - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
    status :: Lude.Maybe UserImportJobStatusType,
    -- | The number of users that were skipped.
    skippedUsers :: Lude.Maybe Lude.Integer,
    -- | The job ID for the user import job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Lude.Maybe Lude.Text,
    -- | The job name for the user import job.
    jobName :: Lude.Maybe Lude.Text,
    -- | The pre-signed URL to be used to upload the @.csv@ file.
    preSignedURL :: Lude.Maybe Lude.Text,
    -- | The number of users that could not be imported.
    failedUsers :: Lude.Maybe Lude.Integer,
    -- | The date when the user import job was started.
    startDate :: Lude.Maybe Lude.Timestamp,
    -- | The message returned when the user import job is completed.
    completionMessage :: Lude.Maybe Lude.Text,
    -- | The date the user import job was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The date when the user import job was completed.
    completionDate :: Lude.Maybe Lude.Timestamp,
    -- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
    cloudWatchLogsRoleARN :: Lude.Maybe Lude.Text,
    -- | The number of users that were successfully imported.
    importedUsers :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserImportJobType' with the minimum fields required to make a request.
--
-- * 'status' - The status of the user import job. One of the following:
--
--
--     * @Created@ - The job was created but not started.
--
--
--     * @Pending@ - A transition state. You have started the job, but it has not begun importing users yet.
--
--
--     * @InProgress@ - The job has started, and users are being imported.
--
--
--     * @Stopping@ - You have stopped the job, but the job has not stopped importing users yet.
--
--
--     * @Stopped@ - You have stopped the job, and the job has stopped importing users.
--
--
--     * @Succeeded@ - The job has completed successfully.
--
--
--     * @Failed@ - The job has stopped due to an error.
--
--
--     * @Expired@ - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
--
--
-- * 'skippedUsers' - The number of users that were skipped.
-- * 'jobId' - The job ID for the user import job.
-- * 'userPoolId' - The user pool ID for the user pool that the users are being imported into.
-- * 'jobName' - The job name for the user import job.
-- * 'preSignedURL' - The pre-signed URL to be used to upload the @.csv@ file.
-- * 'failedUsers' - The number of users that could not be imported.
-- * 'startDate' - The date when the user import job was started.
-- * 'completionMessage' - The message returned when the user import job is completed.
-- * 'creationDate' - The date the user import job was created.
-- * 'completionDate' - The date when the user import job was completed.
-- * 'cloudWatchLogsRoleARN' - The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
-- * 'importedUsers' - The number of users that were successfully imported.
mkUserImportJobType ::
  UserImportJobType
mkUserImportJobType =
  UserImportJobType'
    { status = Lude.Nothing,
      skippedUsers = Lude.Nothing,
      jobId = Lude.Nothing,
      userPoolId = Lude.Nothing,
      jobName = Lude.Nothing,
      preSignedURL = Lude.Nothing,
      failedUsers = Lude.Nothing,
      startDate = Lude.Nothing,
      completionMessage = Lude.Nothing,
      creationDate = Lude.Nothing,
      completionDate = Lude.Nothing,
      cloudWatchLogsRoleARN = Lude.Nothing,
      importedUsers = Lude.Nothing
    }

-- | The status of the user import job. One of the following:
--
--
--     * @Created@ - The job was created but not started.
--
--
--     * @Pending@ - A transition state. You have started the job, but it has not begun importing users yet.
--
--
--     * @InProgress@ - The job has started, and users are being imported.
--
--
--     * @Stopping@ - You have stopped the job, but the job has not stopped importing users yet.
--
--
--     * @Stopped@ - You have stopped the job, and the job has stopped importing users.
--
--
--     * @Succeeded@ - The job has completed successfully.
--
--
--     * @Failed@ - The job has stopped due to an error.
--
--
--     * @Expired@ - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtStatus :: Lens.Lens' UserImportJobType (Lude.Maybe UserImportJobStatusType)
uijtStatus = Lens.lens (status :: UserImportJobType -> Lude.Maybe UserImportJobStatusType) (\s a -> s {status = a} :: UserImportJobType)
{-# DEPRECATED uijtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of users that were skipped.
--
-- /Note:/ Consider using 'skippedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtSkippedUsers :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Integer)
uijtSkippedUsers = Lens.lens (skippedUsers :: UserImportJobType -> Lude.Maybe Lude.Integer) (\s a -> s {skippedUsers = a} :: UserImportJobType)
{-# DEPRECATED uijtSkippedUsers "Use generic-lens or generic-optics with 'skippedUsers' instead." #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtJobId :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtJobId = Lens.lens (jobId :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: UserImportJobType)
{-# DEPRECATED uijtJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtUserPoolId :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtUserPoolId = Lens.lens (userPoolId :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: UserImportJobType)
{-# DEPRECATED uijtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The job name for the user import job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtJobName :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtJobName = Lens.lens (jobName :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: UserImportJobType)
{-# DEPRECATED uijtJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The pre-signed URL to be used to upload the @.csv@ file.
--
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtPreSignedURL :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtPreSignedURL = Lens.lens (preSignedURL :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: UserImportJobType)
{-# DEPRECATED uijtPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

-- | The number of users that could not be imported.
--
-- /Note:/ Consider using 'failedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtFailedUsers :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Integer)
uijtFailedUsers = Lens.lens (failedUsers :: UserImportJobType -> Lude.Maybe Lude.Integer) (\s a -> s {failedUsers = a} :: UserImportJobType)
{-# DEPRECATED uijtFailedUsers "Use generic-lens or generic-optics with 'failedUsers' instead." #-}

-- | The date when the user import job was started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtStartDate :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Timestamp)
uijtStartDate = Lens.lens (startDate :: UserImportJobType -> Lude.Maybe Lude.Timestamp) (\s a -> s {startDate = a} :: UserImportJobType)
{-# DEPRECATED uijtStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The message returned when the user import job is completed.
--
-- /Note:/ Consider using 'completionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCompletionMessage :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtCompletionMessage = Lens.lens (completionMessage :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {completionMessage = a} :: UserImportJobType)
{-# DEPRECATED uijtCompletionMessage "Use generic-lens or generic-optics with 'completionMessage' instead." #-}

-- | The date the user import job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCreationDate :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Timestamp)
uijtCreationDate = Lens.lens (creationDate :: UserImportJobType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UserImportJobType)
{-# DEPRECATED uijtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The date when the user import job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCompletionDate :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Timestamp)
uijtCompletionDate = Lens.lens (completionDate :: UserImportJobType -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionDate = a} :: UserImportJobType)
{-# DEPRECATED uijtCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCloudWatchLogsRoleARN :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Text)
uijtCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: UserImportJobType -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: UserImportJobType)
{-# DEPRECATED uijtCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

-- | The number of users that were successfully imported.
--
-- /Note:/ Consider using 'importedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtImportedUsers :: Lens.Lens' UserImportJobType (Lude.Maybe Lude.Integer)
uijtImportedUsers = Lens.lens (importedUsers :: UserImportJobType -> Lude.Maybe Lude.Integer) (\s a -> s {importedUsers = a} :: UserImportJobType)
{-# DEPRECATED uijtImportedUsers "Use generic-lens or generic-optics with 'importedUsers' instead." #-}

instance Lude.FromJSON UserImportJobType where
  parseJSON =
    Lude.withObject
      "UserImportJobType"
      ( \x ->
          UserImportJobType'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "SkippedUsers")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "PreSignedUrl")
            Lude.<*> (x Lude..:? "FailedUsers")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "CompletionMessage")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "CompletionDate")
            Lude.<*> (x Lude..:? "CloudWatchLogsRoleArn")
            Lude.<*> (x Lude..:? "ImportedUsers")
      )
