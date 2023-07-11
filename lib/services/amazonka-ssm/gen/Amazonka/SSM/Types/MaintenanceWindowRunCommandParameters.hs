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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowRunCommandParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowRunCommandParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CloudWatchOutputConfig
import Amazonka.SSM.Types.DocumentHashType
import Amazonka.SSM.Types.NotificationConfig

-- | The parameters for a @RUN_COMMAND@ task type.
--
-- For information about specifying and updating task parameters, see
-- RegisterTaskWithMaintenanceWindow and UpdateMaintenanceWindowTask.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- @TaskParameters@ has been deprecated. To specify parameters to pass to a
-- task when it runs, instead use the @Parameters@ option in the
-- @TaskInvocationParameters@ structure. For information about how Systems
-- Manager handles these options for the supported maintenance window task
-- types, see MaintenanceWindowTaskInvocationParameters.
--
-- For @RUN_COMMAND@ tasks, Systems Manager uses specified values for
-- @TaskParameters@ and @LoggingInfo@ only if no values are specified for
-- @TaskInvocationParameters@.
--
-- /See:/ 'newMaintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | Information about the commands to run.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The SHA-256 or SHA-1 hash created by the system when the document was
    -- created. SHA-1 hashes have been deprecated.
    documentHash :: Prelude.Maybe Prelude.Text,
    -- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
    documentHashType :: Prelude.Maybe DocumentHashType,
    -- | The Amazon Web Services Systems Manager document (SSM document) version
    -- to use in the request. You can specify @$DEFAULT@, @$LATEST@, or a
    -- specific version number. If you run commands by using the Amazon Web
    -- Services CLI, then you must escape the first two options by using a
    -- backslash. If you specify a version number, then you don\'t need to use
    -- the backslash. For example:
    --
    -- @--document-version \"\\$DEFAULT\"@
    --
    -- @--document-version \"\\$LATEST\"@
    --
    -- @--document-version \"3\"@
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Configurations for sending notifications about command status changes on
    -- a per-managed node basis.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The name of the Amazon Simple Storage Service (Amazon S3) bucket.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the @RUN_COMMAND@ task execution.
    parameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) service role to use to publish Amazon Simple Notification Service
    -- (Amazon SNS) notifications for maintenance window Run Command tasks.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | If this time is reached and the command hasn\'t already started running,
    -- it doesn\'t run.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowRunCommandParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchOutputConfig', 'maintenanceWindowRunCommandParameters_cloudWatchOutputConfig' - Undocumented member.
--
-- 'comment', 'maintenanceWindowRunCommandParameters_comment' - Information about the commands to run.
--
-- 'documentHash', 'maintenanceWindowRunCommandParameters_documentHash' - The SHA-256 or SHA-1 hash created by the system when the document was
-- created. SHA-1 hashes have been deprecated.
--
-- 'documentHashType', 'maintenanceWindowRunCommandParameters_documentHashType' - SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- 'documentVersion', 'maintenanceWindowRunCommandParameters_documentVersion' - The Amazon Web Services Systems Manager document (SSM document) version
-- to use in the request. You can specify @$DEFAULT@, @$LATEST@, or a
-- specific version number. If you run commands by using the Amazon Web
-- Services CLI, then you must escape the first two options by using a
-- backslash. If you specify a version number, then you don\'t need to use
-- the backslash. For example:
--
-- @--document-version \"\\$DEFAULT\"@
--
-- @--document-version \"\\$LATEST\"@
--
-- @--document-version \"3\"@
--
-- 'notificationConfig', 'maintenanceWindowRunCommandParameters_notificationConfig' - Configurations for sending notifications about command status changes on
-- a per-managed node basis.
--
-- 'outputS3BucketName', 'maintenanceWindowRunCommandParameters_outputS3BucketName' - The name of the Amazon Simple Storage Service (Amazon S3) bucket.
--
-- 'outputS3KeyPrefix', 'maintenanceWindowRunCommandParameters_outputS3KeyPrefix' - The S3 bucket subfolder.
--
-- 'parameters', 'maintenanceWindowRunCommandParameters_parameters' - The parameters for the @RUN_COMMAND@ task execution.
--
-- 'serviceRoleArn', 'maintenanceWindowRunCommandParameters_serviceRoleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- 'timeoutSeconds', 'maintenanceWindowRunCommandParameters_timeoutSeconds' - If this time is reached and the command hasn\'t already started running,
-- it doesn\'t run.
newMaintenanceWindowRunCommandParameters ::
  MaintenanceWindowRunCommandParameters
newMaintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { cloudWatchOutputConfig =
        Prelude.Nothing,
      comment = Prelude.Nothing,
      documentHash = Prelude.Nothing,
      documentHashType = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      notificationConfig = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      parameters = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing
    }

-- | Undocumented member.
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe CloudWatchOutputConfig)
maintenanceWindowRunCommandParameters_cloudWatchOutputConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {cloudWatchOutputConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | Information about the commands to run.
maintenanceWindowRunCommandParameters_comment :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_comment = Lens.lens (\MaintenanceWindowRunCommandParameters' {comment} -> comment) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {comment = a} :: MaintenanceWindowRunCommandParameters)

-- | The SHA-256 or SHA-1 hash created by the system when the document was
-- created. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHash :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_documentHash = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentHash} -> documentHash) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentHash = a} :: MaintenanceWindowRunCommandParameters)

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
maintenanceWindowRunCommandParameters_documentHashType :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe DocumentHashType)
maintenanceWindowRunCommandParameters_documentHashType = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentHashType} -> documentHashType) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentHashType = a} :: MaintenanceWindowRunCommandParameters)

-- | The Amazon Web Services Systems Manager document (SSM document) version
-- to use in the request. You can specify @$DEFAULT@, @$LATEST@, or a
-- specific version number. If you run commands by using the Amazon Web
-- Services CLI, then you must escape the first two options by using a
-- backslash. If you specify a version number, then you don\'t need to use
-- the backslash. For example:
--
-- @--document-version \"\\$DEFAULT\"@
--
-- @--document-version \"\\$LATEST\"@
--
-- @--document-version \"3\"@
maintenanceWindowRunCommandParameters_documentVersion :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_documentVersion = Lens.lens (\MaintenanceWindowRunCommandParameters' {documentVersion} -> documentVersion) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {documentVersion = a} :: MaintenanceWindowRunCommandParameters)

-- | Configurations for sending notifications about command status changes on
-- a per-managed node basis.
maintenanceWindowRunCommandParameters_notificationConfig :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe NotificationConfig)
maintenanceWindowRunCommandParameters_notificationConfig = Lens.lens (\MaintenanceWindowRunCommandParameters' {notificationConfig} -> notificationConfig) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {notificationConfig = a} :: MaintenanceWindowRunCommandParameters)

-- | The name of the Amazon Simple Storage Service (Amazon S3) bucket.
maintenanceWindowRunCommandParameters_outputS3BucketName :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_outputS3BucketName = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3BucketName} -> outputS3BucketName) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3BucketName = a} :: MaintenanceWindowRunCommandParameters)

-- | The S3 bucket subfolder.
maintenanceWindowRunCommandParameters_outputS3KeyPrefix :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_outputS3KeyPrefix = Lens.lens (\MaintenanceWindowRunCommandParameters' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {outputS3KeyPrefix = a} :: MaintenanceWindowRunCommandParameters)

-- | The parameters for the @RUN_COMMAND@ task execution.
maintenanceWindowRunCommandParameters_parameters :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
maintenanceWindowRunCommandParameters_parameters = Lens.lens (\MaintenanceWindowRunCommandParameters' {parameters} -> parameters) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {parameters = a} :: MaintenanceWindowRunCommandParameters) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) service role to use to publish Amazon Simple Notification Service
-- (Amazon SNS) notifications for maintenance window Run Command tasks.
maintenanceWindowRunCommandParameters_serviceRoleArn :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Text)
maintenanceWindowRunCommandParameters_serviceRoleArn = Lens.lens (\MaintenanceWindowRunCommandParameters' {serviceRoleArn} -> serviceRoleArn) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {serviceRoleArn = a} :: MaintenanceWindowRunCommandParameters)

-- | If this time is reached and the command hasn\'t already started running,
-- it doesn\'t run.
maintenanceWindowRunCommandParameters_timeoutSeconds :: Lens.Lens' MaintenanceWindowRunCommandParameters (Prelude.Maybe Prelude.Natural)
maintenanceWindowRunCommandParameters_timeoutSeconds = Lens.lens (\MaintenanceWindowRunCommandParameters' {timeoutSeconds} -> timeoutSeconds) (\s@MaintenanceWindowRunCommandParameters' {} a -> s {timeoutSeconds = a} :: MaintenanceWindowRunCommandParameters)

instance
  Data.FromJSON
    MaintenanceWindowRunCommandParameters
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowRunCommandParameters"
      ( \x ->
          MaintenanceWindowRunCommandParameters'
            Prelude.<$> (x Data..:? "CloudWatchOutputConfig")
            Prelude.<*> (x Data..:? "Comment")
            Prelude.<*> (x Data..:? "DocumentHash")
            Prelude.<*> (x Data..:? "DocumentHashType")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "NotificationConfig")
            Prelude.<*> (x Data..:? "OutputS3BucketName")
            Prelude.<*> (x Data..:? "OutputS3KeyPrefix")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ServiceRoleArn")
            Prelude.<*> (x Data..:? "TimeoutSeconds")
      )

instance
  Prelude.Hashable
    MaintenanceWindowRunCommandParameters
  where
  hashWithSalt
    _salt
    MaintenanceWindowRunCommandParameters' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchOutputConfig
        `Prelude.hashWithSalt` comment
        `Prelude.hashWithSalt` documentHash
        `Prelude.hashWithSalt` documentHashType
        `Prelude.hashWithSalt` documentVersion
        `Prelude.hashWithSalt` notificationConfig
        `Prelude.hashWithSalt` outputS3BucketName
        `Prelude.hashWithSalt` outputS3KeyPrefix
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` serviceRoleArn
        `Prelude.hashWithSalt` timeoutSeconds

instance
  Prelude.NFData
    MaintenanceWindowRunCommandParameters
  where
  rnf MaintenanceWindowRunCommandParameters' {..} =
    Prelude.rnf cloudWatchOutputConfig
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf documentHash
      `Prelude.seq` Prelude.rnf documentHashType
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf notificationConfig
      `Prelude.seq` Prelude.rnf outputS3BucketName
      `Prelude.seq` Prelude.rnf outputS3KeyPrefix
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf timeoutSeconds

instance
  Data.ToJSON
    MaintenanceWindowRunCommandParameters
  where
  toJSON MaintenanceWindowRunCommandParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchOutputConfig" Data..=)
              Prelude.<$> cloudWatchOutputConfig,
            ("Comment" Data..=) Prelude.<$> comment,
            ("DocumentHash" Data..=) Prelude.<$> documentHash,
            ("DocumentHashType" Data..=)
              Prelude.<$> documentHashType,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("NotificationConfig" Data..=)
              Prelude.<$> notificationConfig,
            ("OutputS3BucketName" Data..=)
              Prelude.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Data..=)
              Prelude.<$> outputS3KeyPrefix,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("ServiceRoleArn" Data..=)
              Prelude.<$> serviceRoleArn,
            ("TimeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds
          ]
      )
