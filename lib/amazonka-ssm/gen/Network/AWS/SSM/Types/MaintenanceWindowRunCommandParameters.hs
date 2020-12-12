{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
  ( MaintenanceWindowRunCommandParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowRunCommandParameters,

    -- * Lenses
    mwrcpServiceRoleARN,
    mwrcpNotificationConfig,
    mwrcpDocumentHashType,
    mwrcpCloudWatchOutputConfig,
    mwrcpOutputS3KeyPrefix,
    mwrcpParameters,
    mwrcpDocumentHash,
    mwrcpDocumentVersion,
    mwrcpTimeoutSeconds,
    mwrcpComment,
    mwrcpOutputS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.NotificationConfig

-- | The parameters for a RUN_COMMAND task type.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { serviceRoleARN ::
      Lude.Maybe
        Lude.Text,
    notificationConfig ::
      Lude.Maybe
        NotificationConfig,
    documentHashType ::
      Lude.Maybe
        DocumentHashType,
    cloudWatchOutputConfig ::
      Lude.Maybe
        CloudWatchOutputConfig,
    outputS3KeyPrefix ::
      Lude.Maybe
        Lude.Text,
    parameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([Lude.Text])
        ),
    documentHash ::
      Lude.Maybe
        Lude.Text,
    documentVersion ::
      Lude.Maybe
        Lude.Text,
    timeoutSeconds ::
      Lude.Maybe
        Lude.Natural,
    comment ::
      Lude.Maybe
        Lude.Text,
    outputS3BucketName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowRunCommandParameters' with the minimum fields required to make a request.
--
-- * 'cloudWatchOutputConfig' - Undocumented field.
-- * 'comment' - Information about the commands to run.
-- * 'documentHash' - The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
-- * 'documentHashType' - SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
-- * 'documentVersion' - The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
-- * 'notificationConfig' - Configurations for sending notifications about command status changes on a per-instance basis.
-- * 'outputS3BucketName' - The name of the S3 bucket.
-- * 'outputS3KeyPrefix' - The S3 bucket subfolder.
-- * 'parameters' - The parameters for the RUN_COMMAND task execution.
-- * 'serviceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
-- * 'timeoutSeconds' - If this time is reached and the command has not already started running, it doesn't run.
mkMaintenanceWindowRunCommandParameters ::
  MaintenanceWindowRunCommandParameters
mkMaintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { serviceRoleARN =
        Lude.Nothing,
      notificationConfig = Lude.Nothing,
      documentHashType = Lude.Nothing,
      cloudWatchOutputConfig = Lude.Nothing,
      outputS3KeyPrefix = Lude.Nothing,
      parameters = Lude.Nothing,
      documentHash = Lude.Nothing,
      documentVersion = Lude.Nothing,
      timeoutSeconds = Lude.Nothing,
      comment = Lude.Nothing,
      outputS3BucketName = Lude.Nothing
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpServiceRoleARN :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpServiceRoleARN = Lens.lens (serviceRoleARN :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Configurations for sending notifications about command status changes on a per-instance basis.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpNotificationConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe NotificationConfig)
mwrcpNotificationConfig = Lens.lens (notificationConfig :: MaintenanceWindowRunCommandParameters -> Lude.Maybe NotificationConfig) (\s a -> s {notificationConfig = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- /Note:/ Consider using 'documentHashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentHashType :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe DocumentHashType)
mwrcpDocumentHashType = Lens.lens (documentHashType :: MaintenanceWindowRunCommandParameters -> Lude.Maybe DocumentHashType) (\s a -> s {documentHashType = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpDocumentHashType "Use generic-lens or generic-optics with 'documentHashType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpCloudWatchOutputConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe CloudWatchOutputConfig)
mwrcpCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: MaintenanceWindowRunCommandParameters -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The S3 bucket subfolder.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpOutputS3KeyPrefix :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | The parameters for the RUN_COMMAND task execution.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpParameters :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
mwrcpParameters = Lens.lens (parameters :: MaintenanceWindowRunCommandParameters -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
--
-- /Note:/ Consider using 'documentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentHash :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpDocumentHash = Lens.lens (documentHash :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {documentHash = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpDocumentHash "Use generic-lens or generic-optics with 'documentHash' instead." #-}

-- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentVersion :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpDocumentVersion = Lens.lens (documentVersion :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | If this time is reached and the command has not already started running, it doesn't run.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpTimeoutSeconds :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Natural)
mwrcpTimeoutSeconds = Lens.lens (timeoutSeconds :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutSeconds = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | Information about the commands to run.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpComment :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpComment = Lens.lens (comment :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpOutputS3BucketName :: Lens.Lens' MaintenanceWindowRunCommandParameters (Lude.Maybe Lude.Text)
mwrcpOutputS3BucketName = Lens.lens (outputS3BucketName :: MaintenanceWindowRunCommandParameters -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: MaintenanceWindowRunCommandParameters)
{-# DEPRECATED mwrcpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

instance Lude.FromJSON MaintenanceWindowRunCommandParameters where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowRunCommandParameters"
      ( \x ->
          MaintenanceWindowRunCommandParameters'
            Lude.<$> (x Lude..:? "ServiceRoleArn")
            Lude.<*> (x Lude..:? "NotificationConfig")
            Lude.<*> (x Lude..:? "DocumentHashType")
            Lude.<*> (x Lude..:? "CloudWatchOutputConfig")
            Lude.<*> (x Lude..:? "OutputS3KeyPrefix")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentHash")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "TimeoutSeconds")
            Lude.<*> (x Lude..:? "Comment")
            Lude.<*> (x Lude..:? "OutputS3BucketName")
      )

instance Lude.ToJSON MaintenanceWindowRunCommandParameters where
  toJSON MaintenanceWindowRunCommandParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("NotificationConfig" Lude..=) Lude.<$> notificationConfig,
            ("DocumentHashType" Lude..=) Lude.<$> documentHashType,
            ("CloudWatchOutputConfig" Lude..=) Lude.<$> cloudWatchOutputConfig,
            ("OutputS3KeyPrefix" Lude..=) Lude.<$> outputS3KeyPrefix,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentHash" Lude..=) Lude.<$> documentHash,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            ("TimeoutSeconds" Lude..=) Lude.<$> timeoutSeconds,
            ("Comment" Lude..=) Lude.<$> comment,
            ("OutputS3BucketName" Lude..=) Lude.<$> outputS3BucketName
          ]
      )
