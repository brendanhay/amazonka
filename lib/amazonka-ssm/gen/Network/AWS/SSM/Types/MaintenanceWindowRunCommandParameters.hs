{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.NotificationConfig

-- | The parameters for a RUN_COMMAND task type.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { _mwrcpServiceRoleARN ::
      !(Maybe Text),
    _mwrcpNotificationConfig ::
      !( Maybe
           NotificationConfig
       ),
    _mwrcpDocumentHashType ::
      !( Maybe
           DocumentHashType
       ),
    _mwrcpCloudWatchOutputConfig ::
      !( Maybe
           CloudWatchOutputConfig
       ),
    _mwrcpOutputS3KeyPrefix ::
      !(Maybe Text),
    _mwrcpParameters ::
      !( Maybe
           ( Map
               Text
               ([Text])
           )
       ),
    _mwrcpDocumentHash ::
      !(Maybe Text),
    _mwrcpDocumentVersion ::
      !(Maybe Text),
    _mwrcpTimeoutSeconds ::
      !(Maybe Nat),
    _mwrcpComment ::
      !(Maybe Text),
    _mwrcpOutputS3BucketName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowRunCommandParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwrcpServiceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- * 'mwrcpNotificationConfig' - Configurations for sending notifications about command status changes on a per-instance basis.
--
-- * 'mwrcpDocumentHashType' - SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- * 'mwrcpCloudWatchOutputConfig' - Undocumented member.
--
-- * 'mwrcpOutputS3KeyPrefix' - The S3 bucket subfolder.
--
-- * 'mwrcpParameters' - The parameters for the RUN_COMMAND task execution.
--
-- * 'mwrcpDocumentHash' - The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
--
-- * 'mwrcpDocumentVersion' - The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example: --document-version "\$DEFAULT" --document-version "\$LATEST" --document-version "3"
--
-- * 'mwrcpTimeoutSeconds' - If this time is reached and the command has not already started running, it doesn't run.
--
-- * 'mwrcpComment' - Information about the commands to run.
--
-- * 'mwrcpOutputS3BucketName' - The name of the S3 bucket.
maintenanceWindowRunCommandParameters ::
  MaintenanceWindowRunCommandParameters
maintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { _mwrcpServiceRoleARN =
        Nothing,
      _mwrcpNotificationConfig = Nothing,
      _mwrcpDocumentHashType = Nothing,
      _mwrcpCloudWatchOutputConfig = Nothing,
      _mwrcpOutputS3KeyPrefix = Nothing,
      _mwrcpParameters = Nothing,
      _mwrcpDocumentHash = Nothing,
      _mwrcpDocumentVersion = Nothing,
      _mwrcpTimeoutSeconds = Nothing,
      _mwrcpComment = Nothing,
      _mwrcpOutputS3BucketName = Nothing
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
mwrcpServiceRoleARN :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpServiceRoleARN = lens _mwrcpServiceRoleARN (\s a -> s {_mwrcpServiceRoleARN = a})

-- | Configurations for sending notifications about command status changes on a per-instance basis.
mwrcpNotificationConfig :: Lens' MaintenanceWindowRunCommandParameters (Maybe NotificationConfig)
mwrcpNotificationConfig = lens _mwrcpNotificationConfig (\s a -> s {_mwrcpNotificationConfig = a})

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
mwrcpDocumentHashType :: Lens' MaintenanceWindowRunCommandParameters (Maybe DocumentHashType)
mwrcpDocumentHashType = lens _mwrcpDocumentHashType (\s a -> s {_mwrcpDocumentHashType = a})

-- | Undocumented member.
mwrcpCloudWatchOutputConfig :: Lens' MaintenanceWindowRunCommandParameters (Maybe CloudWatchOutputConfig)
mwrcpCloudWatchOutputConfig = lens _mwrcpCloudWatchOutputConfig (\s a -> s {_mwrcpCloudWatchOutputConfig = a})

-- | The S3 bucket subfolder.
mwrcpOutputS3KeyPrefix :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpOutputS3KeyPrefix = lens _mwrcpOutputS3KeyPrefix (\s a -> s {_mwrcpOutputS3KeyPrefix = a})

-- | The parameters for the RUN_COMMAND task execution.
mwrcpParameters :: Lens' MaintenanceWindowRunCommandParameters (HashMap Text ([Text]))
mwrcpParameters = lens _mwrcpParameters (\s a -> s {_mwrcpParameters = a}) . _Default . _Map

-- | The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
mwrcpDocumentHash :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpDocumentHash = lens _mwrcpDocumentHash (\s a -> s {_mwrcpDocumentHash = a})

-- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example: --document-version "\$DEFAULT" --document-version "\$LATEST" --document-version "3"
mwrcpDocumentVersion :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpDocumentVersion = lens _mwrcpDocumentVersion (\s a -> s {_mwrcpDocumentVersion = a})

-- | If this time is reached and the command has not already started running, it doesn't run.
mwrcpTimeoutSeconds :: Lens' MaintenanceWindowRunCommandParameters (Maybe Natural)
mwrcpTimeoutSeconds = lens _mwrcpTimeoutSeconds (\s a -> s {_mwrcpTimeoutSeconds = a}) . mapping _Nat

-- | Information about the commands to run.
mwrcpComment :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpComment = lens _mwrcpComment (\s a -> s {_mwrcpComment = a})

-- | The name of the S3 bucket.
mwrcpOutputS3BucketName :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpOutputS3BucketName = lens _mwrcpOutputS3BucketName (\s a -> s {_mwrcpOutputS3BucketName = a})

instance FromJSON MaintenanceWindowRunCommandParameters where
  parseJSON =
    withObject
      "MaintenanceWindowRunCommandParameters"
      ( \x ->
          MaintenanceWindowRunCommandParameters'
            <$> (x .:? "ServiceRoleArn")
            <*> (x .:? "NotificationConfig")
            <*> (x .:? "DocumentHashType")
            <*> (x .:? "CloudWatchOutputConfig")
            <*> (x .:? "OutputS3KeyPrefix")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "DocumentHash")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "TimeoutSeconds")
            <*> (x .:? "Comment")
            <*> (x .:? "OutputS3BucketName")
      )

instance Hashable MaintenanceWindowRunCommandParameters

instance NFData MaintenanceWindowRunCommandParameters

instance ToJSON MaintenanceWindowRunCommandParameters where
  toJSON MaintenanceWindowRunCommandParameters' {..} =
    object
      ( catMaybes
          [ ("ServiceRoleArn" .=) <$> _mwrcpServiceRoleARN,
            ("NotificationConfig" .=) <$> _mwrcpNotificationConfig,
            ("DocumentHashType" .=) <$> _mwrcpDocumentHashType,
            ("CloudWatchOutputConfig" .=) <$> _mwrcpCloudWatchOutputConfig,
            ("OutputS3KeyPrefix" .=) <$> _mwrcpOutputS3KeyPrefix,
            ("Parameters" .=) <$> _mwrcpParameters,
            ("DocumentHash" .=) <$> _mwrcpDocumentHash,
            ("DocumentVersion" .=) <$> _mwrcpDocumentVersion,
            ("TimeoutSeconds" .=) <$> _mwrcpTimeoutSeconds,
            ("Comment" .=) <$> _mwrcpComment,
            ("OutputS3BucketName" .=) <$> _mwrcpOutputS3BucketName
          ]
      )
