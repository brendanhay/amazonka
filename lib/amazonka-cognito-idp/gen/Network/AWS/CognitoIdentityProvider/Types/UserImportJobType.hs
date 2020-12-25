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
    uijtCloudWatchLogsRoleArn,
    uijtCompletionDate,
    uijtCompletionMessage,
    uijtCreationDate,
    uijtFailedUsers,
    uijtImportedUsers,
    uijtJobId,
    uijtJobName,
    uijtPreSignedUrl,
    uijtSkippedUsers,
    uijtStartDate,
    uijtStatus,
    uijtUserPoolId,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.ArnType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CompletionMessageType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.JobId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PreSignedUrl as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserImportJobNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The user import job type.
--
-- /See:/ 'mkUserImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
  { -- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
    cloudWatchLogsRoleArn :: Core.Maybe Types.ArnType,
    -- | The date when the user import job was completed.
    completionDate :: Core.Maybe Core.NominalDiffTime,
    -- | The message returned when the user import job is completed.
    completionMessage :: Core.Maybe Types.CompletionMessageType,
    -- | The date the user import job was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The number of users that could not be imported.
    failedUsers :: Core.Maybe Core.Integer,
    -- | The number of users that were successfully imported.
    importedUsers :: Core.Maybe Core.Integer,
    -- | The job ID for the user import job.
    jobId :: Core.Maybe Types.JobId,
    -- | The job name for the user import job.
    jobName :: Core.Maybe Types.UserImportJobNameType,
    -- | The pre-signed URL to be used to upload the @.csv@ file.
    preSignedUrl :: Core.Maybe Types.PreSignedUrl,
    -- | The number of users that were skipped.
    skippedUsers :: Core.Maybe Core.Integer,
    -- | The date when the user import job was started.
    startDate :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.UserImportJobStatusType,
    -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Core.Maybe Types.UserPoolIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UserImportJobType' value with any optional fields omitted.
mkUserImportJobType ::
  UserImportJobType
mkUserImportJobType =
  UserImportJobType'
    { cloudWatchLogsRoleArn = Core.Nothing,
      completionDate = Core.Nothing,
      completionMessage = Core.Nothing,
      creationDate = Core.Nothing,
      failedUsers = Core.Nothing,
      importedUsers = Core.Nothing,
      jobId = Core.Nothing,
      jobName = Core.Nothing,
      preSignedUrl = Core.Nothing,
      skippedUsers = Core.Nothing,
      startDate = Core.Nothing,
      status = Core.Nothing,
      userPoolId = Core.Nothing
    }

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCloudWatchLogsRoleArn :: Lens.Lens' UserImportJobType (Core.Maybe Types.ArnType)
uijtCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED uijtCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | The date when the user import job was completed.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCompletionDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.NominalDiffTime)
uijtCompletionDate = Lens.field @"completionDate"
{-# DEPRECATED uijtCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | The message returned when the user import job is completed.
--
-- /Note:/ Consider using 'completionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCompletionMessage :: Lens.Lens' UserImportJobType (Core.Maybe Types.CompletionMessageType)
uijtCompletionMessage = Lens.field @"completionMessage"
{-# DEPRECATED uijtCompletionMessage "Use generic-lens or generic-optics with 'completionMessage' instead." #-}

-- | The date the user import job was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtCreationDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.NominalDiffTime)
uijtCreationDate = Lens.field @"creationDate"
{-# DEPRECATED uijtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The number of users that could not be imported.
--
-- /Note:/ Consider using 'failedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtFailedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
uijtFailedUsers = Lens.field @"failedUsers"
{-# DEPRECATED uijtFailedUsers "Use generic-lens or generic-optics with 'failedUsers' instead." #-}

-- | The number of users that were successfully imported.
--
-- /Note:/ Consider using 'importedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtImportedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
uijtImportedUsers = Lens.field @"importedUsers"
{-# DEPRECATED uijtImportedUsers "Use generic-lens or generic-optics with 'importedUsers' instead." #-}

-- | The job ID for the user import job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtJobId :: Lens.Lens' UserImportJobType (Core.Maybe Types.JobId)
uijtJobId = Lens.field @"jobId"
{-# DEPRECATED uijtJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The job name for the user import job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtJobName :: Lens.Lens' UserImportJobType (Core.Maybe Types.UserImportJobNameType)
uijtJobName = Lens.field @"jobName"
{-# DEPRECATED uijtJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The pre-signed URL to be used to upload the @.csv@ file.
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtPreSignedUrl :: Lens.Lens' UserImportJobType (Core.Maybe Types.PreSignedUrl)
uijtPreSignedUrl = Lens.field @"preSignedUrl"
{-# DEPRECATED uijtPreSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead." #-}

-- | The number of users that were skipped.
--
-- /Note:/ Consider using 'skippedUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtSkippedUsers :: Lens.Lens' UserImportJobType (Core.Maybe Core.Integer)
uijtSkippedUsers = Lens.field @"skippedUsers"
{-# DEPRECATED uijtSkippedUsers "Use generic-lens or generic-optics with 'skippedUsers' instead." #-}

-- | The date when the user import job was started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtStartDate :: Lens.Lens' UserImportJobType (Core.Maybe Core.NominalDiffTime)
uijtStartDate = Lens.field @"startDate"
{-# DEPRECATED uijtStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

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
uijtStatus :: Lens.Lens' UserImportJobType (Core.Maybe Types.UserImportJobStatusType)
uijtStatus = Lens.field @"status"
{-# DEPRECATED uijtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uijtUserPoolId :: Lens.Lens' UserImportJobType (Core.Maybe Types.UserPoolIdType)
uijtUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED uijtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON UserImportJobType where
  parseJSON =
    Core.withObject "UserImportJobType" Core.$
      \x ->
        UserImportJobType'
          Core.<$> (x Core..:? "CloudWatchLogsRoleArn")
          Core.<*> (x Core..:? "CompletionDate")
          Core.<*> (x Core..:? "CompletionMessage")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "FailedUsers")
          Core.<*> (x Core..:? "ImportedUsers")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "PreSignedUrl")
          Core.<*> (x Core..:? "SkippedUsers")
          Core.<*> (x Core..:? "StartDate")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UserPoolId")
