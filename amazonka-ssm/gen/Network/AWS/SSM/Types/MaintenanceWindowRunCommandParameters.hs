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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.NotificationConfig

-- | The parameters for a RUN_COMMAND task type.
--
-- For information about specifying and updating task parameters, see
-- RegisterTaskWithMaintenanceWindow and UpdateMaintenanceWindowTask.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- For Run Command tasks, Systems Manager uses specified values for
-- @TaskParameters@ and @LoggingInfo@ only if no values are specified for
-- @TaskInvocationParameters@.
--
-- /See:/ 'newMaintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { -- | Configurations for sending notifications about command status changes on
    -- a per-instance basis.
    notificationConfig :: Core.Maybe NotificationConfig,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for maintenance window
    -- Run Command tasks.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The name of the S3 bucket.
    outputS3BucketName :: Core.Maybe Core.Text,
    -- | Information about the commands to run.
    comment :: Core.Maybe Core.Text,
    -- | The SHA-256 or SHA-1 hash created by the system when the document was
    -- created. SHA-1 hashes have been deprecated.
    documentHash :: Core.Maybe Core.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Core.Maybe Core.Text,
    -- | If this time is reached and the command has not already started running,
    -- it doesn\'t run.
    timeoutSeconds :: Core.Maybe Core.Natural,
    cloudWatchOutputConfig :: Core.Maybe CloudWatchOutputConfig,
    -- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
    documentHashType :: Core.Maybe DocumentHashType,
    -- | The SSM document version to use in the request. You can specify
    -- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
    -- using the AWS CLI, then you must escape the first two options by using a
    -- backslash. If you specify a version number, then you don\'t need to use
    -- the backslash. For example:
    --
    -- --document-version \"\\$DEFAULT\"
    --
    -- --document-version \"\\$LATEST\"
    --
    -- --document-version \"3\"
    documentVersion :: Core.Maybe Core.Text,
    -- | The parameters for the RUN_COMMAND task execution.
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowRunCommandParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfig', 'maintenanceWindowRunCommandParameters_notificationConfig' - Configurations for sending notifications about command status changes on
-- a per-instance basis.
--
-- 'serviceRoleArn', 'maintenanceWindowRunCommandParameters_serviceRoleArn' - The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
--
-- 'outputS3BucketName', 'maintenanceWindowRunCommandParameters_outputS3BucketName' - The name of the S3 bucket.
--
-- 'comment', 'maintenanceWindowRunCommandParameters_comment' - Information about the commands to run.
--
-- 'documentHash', 'maintenanceWindowRunCommandParameters_documentHash' - The SHA-256 or SHA-1 hash created by the system when the document was
-- created. SHA-1 hashes have been deprecated.
--
-- 'outputS3KeyPrefix', 'maintenanceWindowRunCommandParameters_outputS3KeyPrefix' - The S3 bucket subfolder.
--
-- 'timeoutSeconds', 'maintenanceWindowRunCommandParameters_timeoutSeconds' - If this time is reached and the command has not already started running,
-- it doesn\'t run.
--
-- 'cloudWatchOutputConfig', 'maintenanceWindowRunCommandParameters_cloudWatchOutputConfig' - Undocumented member.
--
-- 'documentHashType', 'maintenanceWindowRunCommandParameters_documentHashType' - SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- 'documentVersion', 'maintenanceWindowRunCommandParameters_documentVersion' - The SSM document version to use in the request. You can specify
-- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
-- using the AWS CLI, then you must escape the first two options by using a
-- backslash. If you specify a version number, then you don\'t need to use
-- the backslash. For example:
--
-- --document-version \"\\$DEFAULT\"
--
-- --document-version \"\\$LATEST\"
--
-- --document-version \"3\"
--
-- 'parameters', 'maintenanceWindowRunCommandParameters_parameters' - The parameters for the RUN_COMMAND task execution.
newMaintenanceWindowRunCommandParameters ::
  MaintenanceWindowRunCommandParameters
newMaintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { notificationConfig =
        Core.Nothing,
      serviceRoleArn = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      comment = Core.Nothing,
      documentHash = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      cloudWatchOutputConfig =
        Core.Nothing,
      documentHashType = Core.Nothing,
      documentVersion = Core.Nothing,
      parameters = Core.Nothing
    }

-- | Configurations for sending notifications about command status changes on
-- a per-instance basis.
maintenanceWindowRunCommandParameters_notificationConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe NotificationConfig)
maintenanceWindowRunCommandParameters_notificationConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {notificationConfig} -> notificationConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {notificationConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
maintenanceWindowRunCommandParameters_serviceRoleArn :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_serviceRoleArn = Lens.lens (\MaintenanceWindowRunCommandParameters' {serviceRoleArn} -> serviceRoleArn) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {serviceRoleArn = a} :: MaintenanceWindowRunCommandParameters)

-- | The name of the S3 bucket.
maintenanceWindowRunCommandParameters_outputS3BucketName :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_outputS3BucketName = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3BucketName} -> outputS3BucketName) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3BucketName = a} :: MaintenanceWindowRunCommandParameters)

-- | Information about the commands to run.
maintenanceWindowRunCommandParameters_comment :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_comment = Lens.lens (\MaintenanceWindowRunCommandParameters' {comment} -> comment) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {comment = a} :: MaintenanceWindowRunCommandParameters)

-- | The SHA-256 or SHA-1 hash created by the system when the document was
-- created. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHash :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_documentHash = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentHash} -> documentHash) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentHash = a} :: MaintenanceWindowRunCommandParameters)

-- | The S3 bucket subfolder.
maintenanceWindowRunCommandParameters_outputS3KeyPrefix :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_outputS3KeyPrefix = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3KeyPrefix = a} :: MaintenanceWindowRunCommandParameters)

-- | If this time is reached and the command has not already started running,
-- it doesn\'t run.
maintenanceWindowRunCommandParameters_timeoutSeconds :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Natural)
maintenanceWindowRunCommandParameters_timeoutSeconds = Lens.lens (\MaintenanceWindowRunCommandParameters' {timeoutSeconds} -> timeoutSeconds) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {timeoutSeconds = a} :: MaintenanceWindowRunCommandParameters)

-- | Undocumented member.
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe CloudWatchOutputConfig)
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {cloudWatchOutputConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHashType :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe DocumentHashType)
maintenanceWindowRunCommandParameters_documentHashType = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentHashType} -> documentHashType) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentHashType = a} :: MaintenanceWindowRunCommandParameters)

-- | The SSM document version to use in the request. You can specify
-- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
-- using the AWS CLI, then you must escape the first two options by using a
-- backslash. If you specify a version number, then you don\'t need to use
-- the backslash. For example:
--
-- --document-version \"\\$DEFAULT\"
--
-- --document-version \"\\$LATEST\"
--
-- --document-version \"3\"
maintenanceWindowRunCommandParameters_documentVersion :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe Core.Text)
maintenanceWindowRunCommandParameters_documentVersion = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentVersion} -> documentVersion) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentVersion = a} :: MaintenanceWindowRunCommandParameters)

-- | The parameters for the RUN_COMMAND task execution.
maintenanceWindowRunCommandParameters_parameters :: Lens.Lens' MaintenanceWindowRunCommandParameters (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
maintenanceWindowRunCommandParameters_parameters = Lens.lens (\MaintenanceWindowRunCommandParameters' {parameters} -> parameters) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {parameters = a} :: MaintenanceWindowRunCommandParameters) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    MaintenanceWindowRunCommandParameters
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowRunCommandParameters"
      ( \x ->
          MaintenanceWindowRunCommandParameters'
            Core.<$> (x Core..:? "NotificationConfig")
            Core.<*> (x Core..:? "ServiceRoleArn")
            Core.<*> (x Core..:? "OutputS3BucketName")
            Core.<*> (x Core..:? "Comment")
            Core.<*> (x Core..:? "DocumentHash")
            Core.<*> (x Core..:? "OutputS3KeyPrefix")
            Core.<*> (x Core..:? "TimeoutSeconds")
            Core.<*> (x Core..:? "CloudWatchOutputConfig")
            Core.<*> (x Core..:? "DocumentHashType")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    MaintenanceWindowRunCommandParameters

instance
  Core.NFData
    MaintenanceWindowRunCommandParameters

instance
  Core.ToJSON
    MaintenanceWindowRunCommandParameters
  where
  toJSON MaintenanceWindowRunCommandParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationConfig" Core..=)
              Core.<$> notificationConfig,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("OutputS3BucketName" Core..=)
              Core.<$> outputS3BucketName,
            ("Comment" Core..=) Core.<$> comment,
            ("DocumentHash" Core..=) Core.<$> documentHash,
            ("OutputS3KeyPrefix" Core..=)
              Core.<$> outputS3KeyPrefix,
            ("TimeoutSeconds" Core..=) Core.<$> timeoutSeconds,
            ("CloudWatchOutputConfig" Core..=)
              Core.<$> cloudWatchOutputConfig,
            ("DocumentHashType" Core..=)
              Core.<$> documentHashType,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )
