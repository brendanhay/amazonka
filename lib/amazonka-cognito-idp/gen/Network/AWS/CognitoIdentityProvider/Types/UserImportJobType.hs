{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobType where

import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user import job type.
--
--
--
-- /See:/ 'userImportJobType' smart constructor.
data UserImportJobType = UserImportJobType'
  { _uijtStatus ::
      !(Maybe UserImportJobStatusType),
    _uijtSkippedUsers :: !(Maybe Integer),
    _uijtJobId :: !(Maybe Text),
    _uijtUserPoolId :: !(Maybe Text),
    _uijtJobName :: !(Maybe Text),
    _uijtPreSignedURL :: !(Maybe Text),
    _uijtFailedUsers :: !(Maybe Integer),
    _uijtStartDate :: !(Maybe POSIX),
    _uijtCompletionMessage :: !(Maybe Text),
    _uijtCreationDate :: !(Maybe POSIX),
    _uijtCompletionDate :: !(Maybe POSIX),
    _uijtCloudWatchLogsRoleARN :: !(Maybe Text),
    _uijtImportedUsers :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserImportJobType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uijtStatus' - The status of the user import job. One of the following:     * @Created@ - The job was created but not started.     * @Pending@ - A transition state. You have started the job, but it has not begun importing users yet.     * @InProgress@ - The job has started, and users are being imported.     * @Stopping@ - You have stopped the job, but the job has not stopped importing users yet.     * @Stopped@ - You have stopped the job, and the job has stopped importing users.     * @Succeeded@ - The job has completed successfully.     * @Failed@ - The job has stopped due to an error.     * @Expired@ - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
--
-- * 'uijtSkippedUsers' - The number of users that were skipped.
--
-- * 'uijtJobId' - The job ID for the user import job.
--
-- * 'uijtUserPoolId' - The user pool ID for the user pool that the users are being imported into.
--
-- * 'uijtJobName' - The job name for the user import job.
--
-- * 'uijtPreSignedURL' - The pre-signed URL to be used to upload the @.csv@ file.
--
-- * 'uijtFailedUsers' - The number of users that could not be imported.
--
-- * 'uijtStartDate' - The date when the user import job was started.
--
-- * 'uijtCompletionMessage' - The message returned when the user import job is completed.
--
-- * 'uijtCreationDate' - The date the user import job was created.
--
-- * 'uijtCompletionDate' - The date when the user import job was completed.
--
-- * 'uijtCloudWatchLogsRoleARN' - The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
--
-- * 'uijtImportedUsers' - The number of users that were successfully imported.
userImportJobType ::
  UserImportJobType
userImportJobType =
  UserImportJobType'
    { _uijtStatus = Nothing,
      _uijtSkippedUsers = Nothing,
      _uijtJobId = Nothing,
      _uijtUserPoolId = Nothing,
      _uijtJobName = Nothing,
      _uijtPreSignedURL = Nothing,
      _uijtFailedUsers = Nothing,
      _uijtStartDate = Nothing,
      _uijtCompletionMessage = Nothing,
      _uijtCreationDate = Nothing,
      _uijtCompletionDate = Nothing,
      _uijtCloudWatchLogsRoleARN = Nothing,
      _uijtImportedUsers = Nothing
    }

-- | The status of the user import job. One of the following:     * @Created@ - The job was created but not started.     * @Pending@ - A transition state. You have started the job, but it has not begun importing users yet.     * @InProgress@ - The job has started, and users are being imported.     * @Stopping@ - You have stopped the job, but the job has not stopped importing users yet.     * @Stopped@ - You have stopped the job, and the job has stopped importing users.     * @Succeeded@ - The job has completed successfully.     * @Failed@ - The job has stopped due to an error.     * @Expired@ - You created a job, but did not start the job within 24-48 hours. All data associated with the job was deleted, and the job cannot be started.
uijtStatus :: Lens' UserImportJobType (Maybe UserImportJobStatusType)
uijtStatus = lens _uijtStatus (\s a -> s {_uijtStatus = a})

-- | The number of users that were skipped.
uijtSkippedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtSkippedUsers = lens _uijtSkippedUsers (\s a -> s {_uijtSkippedUsers = a})

-- | The job ID for the user import job.
uijtJobId :: Lens' UserImportJobType (Maybe Text)
uijtJobId = lens _uijtJobId (\s a -> s {_uijtJobId = a})

-- | The user pool ID for the user pool that the users are being imported into.
uijtUserPoolId :: Lens' UserImportJobType (Maybe Text)
uijtUserPoolId = lens _uijtUserPoolId (\s a -> s {_uijtUserPoolId = a})

-- | The job name for the user import job.
uijtJobName :: Lens' UserImportJobType (Maybe Text)
uijtJobName = lens _uijtJobName (\s a -> s {_uijtJobName = a})

-- | The pre-signed URL to be used to upload the @.csv@ file.
uijtPreSignedURL :: Lens' UserImportJobType (Maybe Text)
uijtPreSignedURL = lens _uijtPreSignedURL (\s a -> s {_uijtPreSignedURL = a})

-- | The number of users that could not be imported.
uijtFailedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtFailedUsers = lens _uijtFailedUsers (\s a -> s {_uijtFailedUsers = a})

-- | The date when the user import job was started.
uijtStartDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtStartDate = lens _uijtStartDate (\s a -> s {_uijtStartDate = a}) . mapping _Time

-- | The message returned when the user import job is completed.
uijtCompletionMessage :: Lens' UserImportJobType (Maybe Text)
uijtCompletionMessage = lens _uijtCompletionMessage (\s a -> s {_uijtCompletionMessage = a})

-- | The date the user import job was created.
uijtCreationDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtCreationDate = lens _uijtCreationDate (\s a -> s {_uijtCreationDate = a}) . mapping _Time

-- | The date when the user import job was completed.
uijtCompletionDate :: Lens' UserImportJobType (Maybe UTCTime)
uijtCompletionDate = lens _uijtCompletionDate (\s a -> s {_uijtCompletionDate = a}) . mapping _Time

-- | The role ARN for the Amazon CloudWatch Logging role for the user import job. For more information, see "Creating the CloudWatch Logs IAM Role" in the Amazon Cognito Developer Guide.
uijtCloudWatchLogsRoleARN :: Lens' UserImportJobType (Maybe Text)
uijtCloudWatchLogsRoleARN = lens _uijtCloudWatchLogsRoleARN (\s a -> s {_uijtCloudWatchLogsRoleARN = a})

-- | The number of users that were successfully imported.
uijtImportedUsers :: Lens' UserImportJobType (Maybe Integer)
uijtImportedUsers = lens _uijtImportedUsers (\s a -> s {_uijtImportedUsers = a})

instance FromJSON UserImportJobType where
  parseJSON =
    withObject
      "UserImportJobType"
      ( \x ->
          UserImportJobType'
            <$> (x .:? "Status")
            <*> (x .:? "SkippedUsers")
            <*> (x .:? "JobId")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "JobName")
            <*> (x .:? "PreSignedUrl")
            <*> (x .:? "FailedUsers")
            <*> (x .:? "StartDate")
            <*> (x .:? "CompletionMessage")
            <*> (x .:? "CreationDate")
            <*> (x .:? "CompletionDate")
            <*> (x .:? "CloudWatchLogsRoleArn")
            <*> (x .:? "ImportedUsers")
      )

instance Hashable UserImportJobType

instance NFData UserImportJobType
