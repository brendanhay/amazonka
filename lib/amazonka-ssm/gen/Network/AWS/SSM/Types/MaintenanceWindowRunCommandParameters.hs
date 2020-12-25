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
    mwrcpCloudWatchOutputConfig,
    mwrcpComment,
    mwrcpDocumentHash,
    mwrcpDocumentHashType,
    mwrcpDocumentVersion,
    mwrcpNotificationConfig,
    mwrcpOutputS3BucketName,
    mwrcpOutputS3KeyPrefix,
    mwrcpParameters,
    mwrcpServiceRoleArn,
    mwrcpTimeoutSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CloudWatchOutputConfig as Types
import qualified Network.AWS.SSM.Types.Comment as Types
import qualified Network.AWS.SSM.Types.DocumentHash as Types
import qualified Network.AWS.SSM.Types.DocumentHashType as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.NotificationConfig as Types
import qualified Network.AWS.SSM.Types.OutputS3BucketName as Types
import qualified Network.AWS.SSM.Types.ParameterName as Types
import qualified Network.AWS.SSM.Types.ParameterValue as Types
import qualified Network.AWS.SSM.Types.S3KeyPrefix as Types
import qualified Network.AWS.SSM.Types.ServiceRoleArn as Types

-- | The parameters for a RUN_COMMAND task type.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { cloudWatchOutputConfig :: Core.Maybe Types.CloudWatchOutputConfig,
    -- | Information about the commands to run.
    comment :: Core.Maybe Types.Comment,
    -- | The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
    documentHash :: Core.Maybe Types.DocumentHash,
    -- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
    documentHashType :: Core.Maybe Types.DocumentHashType,
    -- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
    --
    -- --document-version "\$DEFAULT"
    -- --document-version "\$LATEST"
    -- --document-version "3"
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | Configurations for sending notifications about command status changes on a per-instance basis.
    notificationConfig :: Core.Maybe Types.NotificationConfig,
    -- | The name of the S3 bucket.
    outputS3BucketName :: Core.Maybe Types.OutputS3BucketName,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Core.Maybe Types.S3KeyPrefix,
    -- | The parameters for the RUN_COMMAND task execution.
    parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]),
    -- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Core.Maybe Types.ServiceRoleArn,
    -- | If this time is reached and the command has not already started running, it doesn't run.
    timeoutSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowRunCommandParameters' value with any optional fields omitted.
mkMaintenanceWindowRunCommandParameters ::
  MaintenanceWindowRunCommandParameters
mkMaintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { cloudWatchOutputConfig =
        Core.Nothing,
      comment = Core.Nothing,
      documentHash = Core.Nothing,
      documentHashType = Core.Nothing,
      documentVersion = Core.Nothing,
      notificationConfig = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      parameters = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      timeoutSeconds = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpCloudWatchOutputConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.CloudWatchOutputConfig)
mwrcpCloudWatchOutputConfig = Lens.field @"cloudWatchOutputConfig"
{-# DEPRECATED mwrcpCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | Information about the commands to run.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpComment :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.Comment)
mwrcpComment = Lens.field @"comment"
{-# DEPRECATED mwrcpComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
--
-- /Note:/ Consider using 'documentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentHash :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.DocumentHash)
mwrcpDocumentHash = Lens.field @"documentHash"
{-# DEPRECATED mwrcpDocumentHash "Use generic-lens or generic-optics with 'documentHash' instead." #-}

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- /Note:/ Consider using 'documentHashType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentHashType :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.DocumentHashType)
mwrcpDocumentHashType = Lens.field @"documentHashType"
{-# DEPRECATED mwrcpDocumentHashType "Use generic-lens or generic-optics with 'documentHashType' instead." #-}

-- | The SSM document version to use in the request. You can specify $DEFAULT, $LATEST, or a specific version number. If you run commands by using the AWS CLI, then you must escape the first two options by using a backslash. If you specify a version number, then you don't need to use the backslash. For example:
--
-- --document-version "\$DEFAULT"
-- --document-version "\$LATEST"
-- --document-version "3"
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpDocumentVersion :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.DocumentVersion)
mwrcpDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED mwrcpDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Configurations for sending notifications about command status changes on a per-instance basis.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpNotificationConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.NotificationConfig)
mwrcpNotificationConfig = Lens.field @"notificationConfig"
{-# DEPRECATED mwrcpNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpOutputS3BucketName :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.OutputS3BucketName)
mwrcpOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED mwrcpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The S3 bucket subfolder.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpOutputS3KeyPrefix :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.S3KeyPrefix)
mwrcpOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# DEPRECATED mwrcpOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | The parameters for the RUN_COMMAND task execution.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpParameters :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
mwrcpParameters = Lens.field @"parameters"
{-# DEPRECATED mwrcpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpServiceRoleArn :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Types.ServiceRoleArn)
mwrcpServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED mwrcpServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | If this time is reached and the command has not already started running, it doesn't run.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwrcpTimeoutSeconds :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Natural)
mwrcpTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# DEPRECATED mwrcpTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

instance Core.FromJSON MaintenanceWindowRunCommandParameters where
  toJSON MaintenanceWindowRunCommandParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchOutputConfig" Core..=)
              Core.<$> cloudWatchOutputConfig,
            ("Comment" Core..=) Core.<$> comment,
            ("DocumentHash" Core..=) Core.<$> documentHash,
            ("DocumentHashType" Core..=) Core.<$> documentHashType,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("NotificationConfig" Core..=) Core.<$> notificationConfig,
            ("OutputS3BucketName" Core..=) Core.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Core..=) Core.<$> outputS3KeyPrefix,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("TimeoutSeconds" Core..=) Core.<$> timeoutSeconds
          ]
      )

instance Core.FromJSON MaintenanceWindowRunCommandParameters where
  parseJSON =
    Core.withObject "MaintenanceWindowRunCommandParameters" Core.$
      \x ->
        MaintenanceWindowRunCommandParameters'
          Core.<$> (x Core..:? "CloudWatchOutputConfig")
          Core.<*> (x Core..:? "Comment")
          Core.<*> (x Core..:? "DocumentHash")
          Core.<*> (x Core..:? "DocumentHashType")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "NotificationConfig")
          Core.<*> (x Core..:? "OutputS3BucketName")
          Core.<*> (x Core..:? "OutputS3KeyPrefix")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "ServiceRoleArn")
          Core.<*> (x Core..:? "TimeoutSeconds")
