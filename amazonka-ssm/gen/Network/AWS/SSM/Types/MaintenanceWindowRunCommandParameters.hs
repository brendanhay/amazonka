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
import qualified Network.AWS.Prelude as Prelude
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
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for maintenance window
    -- Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Information about the commands to run.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The SHA-256 or SHA-1 hash created by the system when the document was
    -- created. SHA-1 hashes have been deprecated.
    documentHash :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | If this time is reached and the command has not already started running,
    -- it doesn\'t run.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
    documentHashType :: Prelude.Maybe DocumentHashType,
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
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the RUN_COMMAND task execution.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      comment = Prelude.Nothing,
      documentHash = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      cloudWatchOutputConfig =
        Prelude.Nothing,
      documentHashType = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | Configurations for sending notifications about command status changes on
-- a per-instance basis.
maintenanceWindowRunCommandParameters_notificationConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe NotificationConfig)
maintenanceWindowRunCommandParameters_notificationConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {notificationConfig} -> notificationConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {notificationConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for maintenance window
-- Run Command tasks.
maintenanceWindowRunCommandParameters_serviceRoleArn :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_serviceRoleArn = Lens.lens (\MaintenanceWindowRunCommandParameters' {serviceRoleArn} -> serviceRoleArn) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {serviceRoleArn = a} :: MaintenanceWindowRunCommandParameters)

-- | The name of the S3 bucket.
maintenanceWindowRunCommandParameters_outputS3BucketName :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_outputS3BucketName = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3BucketName} -> outputS3BucketName) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3BucketName = a} :: MaintenanceWindowRunCommandParameters)

-- | Information about the commands to run.
maintenanceWindowRunCommandParameters_comment :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_comment = Lens.lens (\MaintenanceWindowRunCommandParameters' {comment} -> comment) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {comment = a} :: MaintenanceWindowRunCommandParameters)

-- | The SHA-256 or SHA-1 hash created by the system when the document was
-- created. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHash :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_documentHash = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentHash} -> documentHash) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentHash = a} :: MaintenanceWindowRunCommandParameters)

-- | The S3 bucket subfolder.
maintenanceWindowRunCommandParameters_outputS3KeyPrefix :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_outputS3KeyPrefix = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3KeyPrefix = a} :: MaintenanceWindowRunCommandParameters)

-- | If this time is reached and the command has not already started running,
-- it doesn\'t run.
maintenanceWindowRunCommandParameters_timeoutSeconds :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Natural)
maintenanceWindowRunCommandParameters_timeoutSeconds = Lens.lens (\MaintenanceWindowRunCommandParameters' {timeoutSeconds} -> timeoutSeconds) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {timeoutSeconds = a} :: MaintenanceWindowRunCommandParameters)

-- | Undocumented member.
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe CloudWatchOutputConfig)
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {cloudWatchOutputConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHashType :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe DocumentHashType)
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
maintenanceWindowRunCommandParameters_documentVersion :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_documentVersion = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentVersion} -> documentVersion) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentVersion = a} :: MaintenanceWindowRunCommandParameters)

-- | The parameters for the RUN_COMMAND task execution.
maintenanceWindowRunCommandParameters_parameters :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
maintenanceWindowRunCommandParameters_parameters = Lens.lens (\MaintenanceWindowRunCommandParameters' {parameters} -> parameters) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {parameters = a} :: MaintenanceWindowRunCommandParameters) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    MaintenanceWindowRunCommandParameters
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowRunCommandParameters"
      ( \x ->
          MaintenanceWindowRunCommandParameters'
            Prelude.<$> (x Core..:? "NotificationConfig")
            Prelude.<*> (x Core..:? "ServiceRoleArn")
            Prelude.<*> (x Core..:? "OutputS3BucketName")
            Prelude.<*> (x Core..:? "Comment")
            Prelude.<*> (x Core..:? "DocumentHash")
            Prelude.<*> (x Core..:? "OutputS3KeyPrefix")
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "CloudWatchOutputConfig")
            Prelude.<*> (x Core..:? "DocumentHashType")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    MaintenanceWindowRunCommandParameters

instance
  Prelude.NFData
    MaintenanceWindowRunCommandParameters

instance
  Core.ToJSON
    MaintenanceWindowRunCommandParameters
  where
  toJSON MaintenanceWindowRunCommandParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationConfig" Core..=)
              Prelude.<$> notificationConfig,
            ("ServiceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("OutputS3BucketName" Core..=)
              Prelude.<$> outputS3BucketName,
            ("Comment" Core..=) Prelude.<$> comment,
            ("DocumentHash" Core..=) Prelude.<$> documentHash,
            ("OutputS3KeyPrefix" Core..=)
              Prelude.<$> outputS3KeyPrefix,
            ("TimeoutSeconds" Core..=)
              Prelude.<$> timeoutSeconds,
            ("CloudWatchOutputConfig" Core..=)
              Prelude.<$> cloudWatchOutputConfig,
            ("DocumentHashType" Core..=)
              Prelude.<$> documentHashType,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("Parameters" Core..=) Prelude.<$> parameters
          ]
      )
