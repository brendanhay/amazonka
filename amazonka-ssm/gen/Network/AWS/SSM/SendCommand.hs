{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.SendCommand
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs commands on one or more managed instances.
module Network.AWS.SSM.SendCommand
  ( -- * Creating a Request
    SendCommand (..),
    newSendCommand,

    -- * Request Lenses
    sendCommand_notificationConfig,
    sendCommand_instanceIds,
    sendCommand_maxErrors,
    sendCommand_serviceRoleArn,
    sendCommand_outputS3BucketName,
    sendCommand_comment,
    sendCommand_documentHash,
    sendCommand_targets,
    sendCommand_outputS3Region,
    sendCommand_maxConcurrency,
    sendCommand_outputS3KeyPrefix,
    sendCommand_timeoutSeconds,
    sendCommand_cloudWatchOutputConfig,
    sendCommand_documentHashType,
    sendCommand_documentVersion,
    sendCommand_parameters,
    sendCommand_documentName,

    -- * Destructuring the Response
    SendCommandResponse (..),
    newSendCommandResponse,

    -- * Response Lenses
    sendCommandResponse_command,
    sendCommandResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newSendCommand' smart constructor.
data SendCommand = SendCommand'
  { -- | Configurations for sending notifications.
    notificationConfig :: Core.Maybe NotificationConfig,
    -- | The IDs of the instances where the command should run. Specifying
    -- instance IDs is most useful when you are targeting a limited number of
    -- instances, though you can specify up to 50 IDs.
    --
    -- To target a larger number of instances, or if you prefer not to list
    -- individual instance IDs, we recommend using the @Targets@ option
    -- instead. Using @Targets@, which accepts tag key-value pairs to identify
    -- the instances to send commands to, you can a send command to tens,
    -- hundreds, or thousands of instances at once.
    --
    -- For more information about how to use targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
    -- in the /AWS Systems Manager User Guide/.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The maximum number of errors allowed without the command failing. When
    -- the command fails one more time beyond the value of MaxErrors, the
    -- systems stops sending the command to additional targets. You can specify
    -- a number like 10 or a percentage like 10%. The default value is 0. For
    -- more information about how to use MaxErrors, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
    -- in the /AWS Systems Manager User Guide/.
    maxErrors :: Core.Maybe Core.Text,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for Run Command
    -- commands.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The name of the S3 bucket where command execution responses should be
    -- stored.
    outputS3BucketName :: Core.Maybe Core.Text,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Core.Maybe Core.Text,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    documentHash :: Core.Maybe Core.Text,
    -- | An array of search criteria that targets instances using a @Key,Value@
    -- combination that you specify. Specifying targets is most useful when you
    -- want to send a command to a large number of instances at once. Using
    -- @Targets@, which accepts tag key-value pairs to identify instances, you
    -- can send a command to tens, hundreds, or thousands of instances at once.
    --
    -- To send a command to a smaller number of instances, you can use the
    -- @InstanceIds@ option instead.
    --
    -- For more information about how to use targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
    -- in the /AWS Systems Manager User Guide/.
    targets :: Core.Maybe [Target],
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Core.Maybe Core.Text,
    -- | (Optional) The maximum number of instances that are allowed to run the
    -- command at the same time. You can specify a number such as 10 or a
    -- percentage such as 10%. The default value is 50. For more information
    -- about how to use MaxConcurrency, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
    -- in the /AWS Systems Manager User Guide/.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The directory structure within the S3 bucket where the responses should
    -- be stored.
    outputS3KeyPrefix :: Core.Maybe Core.Text,
    -- | If this time is reached and the command has not already started running,
    -- it will not run.
    timeoutSeconds :: Core.Maybe Core.Natural,
    -- | Enables Systems Manager to send Run Command output to Amazon CloudWatch
    -- Logs.
    cloudWatchOutputConfig :: Core.Maybe CloudWatchOutputConfig,
    -- | Sha256 or Sha1.
    --
    -- Sha1 hashes have been deprecated.
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
    -- | The required and optional parameters specified in the document being
    -- run.
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The name of the Systems Manager document to run. This can be a public
    -- document or a custom document. To run a shared document belonging to
    -- another account, specify the document ARN. For more information about
    -- how to use shared documents, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
    -- in the /AWS Systems Manager User Guide/.
    documentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SendCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfig', 'sendCommand_notificationConfig' - Configurations for sending notifications.
--
-- 'instanceIds', 'sendCommand_instanceIds' - The IDs of the instances where the command should run. Specifying
-- instance IDs is most useful when you are targeting a limited number of
-- instances, though you can specify up to 50 IDs.
--
-- To target a larger number of instances, or if you prefer not to list
-- individual instance IDs, we recommend using the @Targets@ option
-- instead. Using @Targets@, which accepts tag key-value pairs to identify
-- the instances to send commands to, you can a send command to tens,
-- hundreds, or thousands of instances at once.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
-- in the /AWS Systems Manager User Guide/.
--
-- 'maxErrors', 'sendCommand_maxErrors' - The maximum number of errors allowed without the command failing. When
-- the command fails one more time beyond the value of MaxErrors, the
-- systems stops sending the command to additional targets. You can specify
-- a number like 10 or a percentage like 10%. The default value is 0. For
-- more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
-- in the /AWS Systems Manager User Guide/.
--
-- 'serviceRoleArn', 'sendCommand_serviceRoleArn' - The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for Run Command
-- commands.
--
-- 'outputS3BucketName', 'sendCommand_outputS3BucketName' - The name of the S3 bucket where command execution responses should be
-- stored.
--
-- 'comment', 'sendCommand_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'documentHash', 'sendCommand_documentHash' - The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
--
-- 'targets', 'sendCommand_targets' - An array of search criteria that targets instances using a @Key,Value@
-- combination that you specify. Specifying targets is most useful when you
-- want to send a command to a large number of instances at once. Using
-- @Targets@, which accepts tag key-value pairs to identify instances, you
-- can send a command to tens, hundreds, or thousands of instances at once.
--
-- To send a command to a smaller number of instances, you can use the
-- @InstanceIds@ option instead.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
-- in the /AWS Systems Manager User Guide/.
--
-- 'outputS3Region', 'sendCommand_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
--
-- 'maxConcurrency', 'sendCommand_maxConcurrency' - (Optional) The maximum number of instances that are allowed to run the
-- command at the same time. You can specify a number such as 10 or a
-- percentage such as 10%. The default value is 50. For more information
-- about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
-- in the /AWS Systems Manager User Guide/.
--
-- 'outputS3KeyPrefix', 'sendCommand_outputS3KeyPrefix' - The directory structure within the S3 bucket where the responses should
-- be stored.
--
-- 'timeoutSeconds', 'sendCommand_timeoutSeconds' - If this time is reached and the command has not already started running,
-- it will not run.
--
-- 'cloudWatchOutputConfig', 'sendCommand_cloudWatchOutputConfig' - Enables Systems Manager to send Run Command output to Amazon CloudWatch
-- Logs.
--
-- 'documentHashType', 'sendCommand_documentHashType' - Sha256 or Sha1.
--
-- Sha1 hashes have been deprecated.
--
-- 'documentVersion', 'sendCommand_documentVersion' - The SSM document version to use in the request. You can specify
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
-- 'parameters', 'sendCommand_parameters' - The required and optional parameters specified in the document being
-- run.
--
-- 'documentName', 'sendCommand_documentName' - The name of the Systems Manager document to run. This can be a public
-- document or a custom document. To run a shared document belonging to
-- another account, specify the document ARN. For more information about
-- how to use shared documents, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
-- in the /AWS Systems Manager User Guide/.
newSendCommand ::
  -- | 'documentName'
  Core.Text ->
  SendCommand
newSendCommand pDocumentName_ =
  SendCommand'
    { notificationConfig = Core.Nothing,
      instanceIds = Core.Nothing,
      maxErrors = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      comment = Core.Nothing,
      documentHash = Core.Nothing,
      targets = Core.Nothing,
      outputS3Region = Core.Nothing,
      maxConcurrency = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      cloudWatchOutputConfig = Core.Nothing,
      documentHashType = Core.Nothing,
      documentVersion = Core.Nothing,
      parameters = Core.Nothing,
      documentName = pDocumentName_
    }

-- | Configurations for sending notifications.
sendCommand_notificationConfig :: Lens.Lens' SendCommand (Core.Maybe NotificationConfig)
sendCommand_notificationConfig = Lens.lens (\SendCommand' {notificationConfig} -> notificationConfig) (\s@SendCommand' {} a -> s {notificationConfig = a} :: SendCommand)

-- | The IDs of the instances where the command should run. Specifying
-- instance IDs is most useful when you are targeting a limited number of
-- instances, though you can specify up to 50 IDs.
--
-- To target a larger number of instances, or if you prefer not to list
-- individual instance IDs, we recommend using the @Targets@ option
-- instead. Using @Targets@, which accepts tag key-value pairs to identify
-- the instances to send commands to, you can a send command to tens,
-- hundreds, or thousands of instances at once.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
-- in the /AWS Systems Manager User Guide/.
sendCommand_instanceIds :: Lens.Lens' SendCommand (Core.Maybe [Core.Text])
sendCommand_instanceIds = Lens.lens (\SendCommand' {instanceIds} -> instanceIds) (\s@SendCommand' {} a -> s {instanceIds = a} :: SendCommand) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of errors allowed without the command failing. When
-- the command fails one more time beyond the value of MaxErrors, the
-- systems stops sending the command to additional targets. You can specify
-- a number like 10 or a percentage like 10%. The default value is 0. For
-- more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
-- in the /AWS Systems Manager User Guide/.
sendCommand_maxErrors :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_maxErrors = Lens.lens (\SendCommand' {maxErrors} -> maxErrors) (\s@SendCommand' {} a -> s {maxErrors = a} :: SendCommand)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for Run Command
-- commands.
sendCommand_serviceRoleArn :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_serviceRoleArn = Lens.lens (\SendCommand' {serviceRoleArn} -> serviceRoleArn) (\s@SendCommand' {} a -> s {serviceRoleArn = a} :: SendCommand)

-- | The name of the S3 bucket where command execution responses should be
-- stored.
sendCommand_outputS3BucketName :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_outputS3BucketName = Lens.lens (\SendCommand' {outputS3BucketName} -> outputS3BucketName) (\s@SendCommand' {} a -> s {outputS3BucketName = a} :: SendCommand)

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
sendCommand_comment :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_comment = Lens.lens (\SendCommand' {comment} -> comment) (\s@SendCommand' {} a -> s {comment = a} :: SendCommand)

-- | The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
sendCommand_documentHash :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_documentHash = Lens.lens (\SendCommand' {documentHash} -> documentHash) (\s@SendCommand' {} a -> s {documentHash = a} :: SendCommand)

-- | An array of search criteria that targets instances using a @Key,Value@
-- combination that you specify. Specifying targets is most useful when you
-- want to send a command to a large number of instances at once. Using
-- @Targets@, which accepts tag key-value pairs to identify instances, you
-- can send a command to tens, hundreds, or thousands of instances at once.
--
-- To send a command to a smaller number of instances, you can use the
-- @InstanceIds@ option instead.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
-- in the /AWS Systems Manager User Guide/.
sendCommand_targets :: Lens.Lens' SendCommand (Core.Maybe [Target])
sendCommand_targets = Lens.lens (\SendCommand' {targets} -> targets) (\s@SendCommand' {} a -> s {targets = a} :: SendCommand) Core.. Lens.mapping Lens._Coerce

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
sendCommand_outputS3Region :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_outputS3Region = Lens.lens (\SendCommand' {outputS3Region} -> outputS3Region) (\s@SendCommand' {} a -> s {outputS3Region = a} :: SendCommand)

-- | (Optional) The maximum number of instances that are allowed to run the
-- command at the same time. You can specify a number such as 10 or a
-- percentage such as 10%. The default value is 50. For more information
-- about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
-- in the /AWS Systems Manager User Guide/.
sendCommand_maxConcurrency :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_maxConcurrency = Lens.lens (\SendCommand' {maxConcurrency} -> maxConcurrency) (\s@SendCommand' {} a -> s {maxConcurrency = a} :: SendCommand)

-- | The directory structure within the S3 bucket where the responses should
-- be stored.
sendCommand_outputS3KeyPrefix :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_outputS3KeyPrefix = Lens.lens (\SendCommand' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@SendCommand' {} a -> s {outputS3KeyPrefix = a} :: SendCommand)

-- | If this time is reached and the command has not already started running,
-- it will not run.
sendCommand_timeoutSeconds :: Lens.Lens' SendCommand (Core.Maybe Core.Natural)
sendCommand_timeoutSeconds = Lens.lens (\SendCommand' {timeoutSeconds} -> timeoutSeconds) (\s@SendCommand' {} a -> s {timeoutSeconds = a} :: SendCommand)

-- | Enables Systems Manager to send Run Command output to Amazon CloudWatch
-- Logs.
sendCommand_cloudWatchOutputConfig :: Lens.Lens' SendCommand (Core.Maybe CloudWatchOutputConfig)
sendCommand_cloudWatchOutputConfig = Lens.lens (\SendCommand' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@SendCommand' {} a -> s {cloudWatchOutputConfig = a} :: SendCommand)

-- | Sha256 or Sha1.
--
-- Sha1 hashes have been deprecated.
sendCommand_documentHashType :: Lens.Lens' SendCommand (Core.Maybe DocumentHashType)
sendCommand_documentHashType = Lens.lens (\SendCommand' {documentHashType} -> documentHashType) (\s@SendCommand' {} a -> s {documentHashType = a} :: SendCommand)

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
sendCommand_documentVersion :: Lens.Lens' SendCommand (Core.Maybe Core.Text)
sendCommand_documentVersion = Lens.lens (\SendCommand' {documentVersion} -> documentVersion) (\s@SendCommand' {} a -> s {documentVersion = a} :: SendCommand)

-- | The required and optional parameters specified in the document being
-- run.
sendCommand_parameters :: Lens.Lens' SendCommand (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
sendCommand_parameters = Lens.lens (\SendCommand' {parameters} -> parameters) (\s@SendCommand' {} a -> s {parameters = a} :: SendCommand) Core.. Lens.mapping Lens._Coerce

-- | The name of the Systems Manager document to run. This can be a public
-- document or a custom document. To run a shared document belonging to
-- another account, specify the document ARN. For more information about
-- how to use shared documents, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
-- in the /AWS Systems Manager User Guide/.
sendCommand_documentName :: Lens.Lens' SendCommand Core.Text
sendCommand_documentName = Lens.lens (\SendCommand' {documentName} -> documentName) (\s@SendCommand' {} a -> s {documentName = a} :: SendCommand)

instance Core.AWSRequest SendCommand where
  type AWSResponse SendCommand = SendCommandResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendCommandResponse'
            Core.<$> (x Core..?> "Command")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SendCommand

instance Core.NFData SendCommand

instance Core.ToHeaders SendCommand where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.SendCommand" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SendCommand where
  toJSON SendCommand' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationConfig" Core..=)
              Core.<$> notificationConfig,
            ("InstanceIds" Core..=) Core.<$> instanceIds,
            ("MaxErrors" Core..=) Core.<$> maxErrors,
            ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
            ("OutputS3BucketName" Core..=)
              Core.<$> outputS3BucketName,
            ("Comment" Core..=) Core.<$> comment,
            ("DocumentHash" Core..=) Core.<$> documentHash,
            ("Targets" Core..=) Core.<$> targets,
            ("OutputS3Region" Core..=) Core.<$> outputS3Region,
            ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
            ("OutputS3KeyPrefix" Core..=)
              Core.<$> outputS3KeyPrefix,
            ("TimeoutSeconds" Core..=) Core.<$> timeoutSeconds,
            ("CloudWatchOutputConfig" Core..=)
              Core.<$> cloudWatchOutputConfig,
            ("DocumentHashType" Core..=)
              Core.<$> documentHashType,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("DocumentName" Core..= documentName)
          ]
      )

instance Core.ToPath SendCommand where
  toPath = Core.const "/"

instance Core.ToQuery SendCommand where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { -- | The request as it was received by Systems Manager. Also provides the
    -- command ID which can be used future references to this request.
    command :: Core.Maybe Command,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SendCommandResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'sendCommandResponse_command' - The request as it was received by Systems Manager. Also provides the
-- command ID which can be used future references to this request.
--
-- 'httpStatus', 'sendCommandResponse_httpStatus' - The response's http status code.
newSendCommandResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SendCommandResponse
newSendCommandResponse pHttpStatus_ =
  SendCommandResponse'
    { command = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request as it was received by Systems Manager. Also provides the
-- command ID which can be used future references to this request.
sendCommandResponse_command :: Lens.Lens' SendCommandResponse (Core.Maybe Command)
sendCommandResponse_command = Lens.lens (\SendCommandResponse' {command} -> command) (\s@SendCommandResponse' {} a -> s {command = a} :: SendCommandResponse)

-- | The response's http status code.
sendCommandResponse_httpStatus :: Lens.Lens' SendCommandResponse Core.Int
sendCommandResponse_httpStatus = Lens.lens (\SendCommandResponse' {httpStatus} -> httpStatus) (\s@SendCommandResponse' {} a -> s {httpStatus = a} :: SendCommandResponse)

instance Core.NFData SendCommandResponse
