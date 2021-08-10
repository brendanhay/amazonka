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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newSendCommand' smart constructor.
data SendCommand = SendCommand'
  { -- | Configurations for sending notifications.
    notificationConfig :: Prelude.Maybe NotificationConfig,
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
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of errors allowed without the command failing. When
    -- the command fails one more time beyond the value of MaxErrors, the
    -- systems stops sending the command to additional targets. You can specify
    -- a number like 10 or a percentage like 10%. The default value is 0. For
    -- more information about how to use MaxErrors, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
    -- in the /AWS Systems Manager User Guide/.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM service role to use to publish Amazon Simple
    -- Notification Service (Amazon SNS) notifications for Run Command
    -- commands.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where command execution responses should be
    -- stored.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    documentHash :: Prelude.Maybe Prelude.Text,
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
    targets :: Prelude.Maybe [Target],
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The maximum number of instances that are allowed to run the
    -- command at the same time. You can specify a number such as 10 or a
    -- percentage such as 10%. The default value is 50. For more information
    -- about how to use MaxConcurrency, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
    -- in the /AWS Systems Manager User Guide/.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The directory structure within the S3 bucket where the responses should
    -- be stored.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | If this time is reached and the command has not already started running,
    -- it will not run.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Enables Systems Manager to send Run Command output to Amazon CloudWatch
    -- Logs.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | Sha256 or Sha1.
    --
    -- Sha1 hashes have been deprecated.
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
    -- | The required and optional parameters specified in the document being
    -- run.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The name of the Systems Manager document to run. This can be a public
    -- document or a custom document. To run a shared document belonging to
    -- another account, specify the document ARN. For more information about
    -- how to use shared documents, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
    -- in the /AWS Systems Manager User Guide/.
    documentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  SendCommand
newSendCommand pDocumentName_ =
  SendCommand'
    { notificationConfig = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      comment = Prelude.Nothing,
      documentHash = Prelude.Nothing,
      targets = Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      documentHashType = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentName = pDocumentName_
    }

-- | Configurations for sending notifications.
sendCommand_notificationConfig :: Lens.Lens' SendCommand (Prelude.Maybe NotificationConfig)
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
sendCommand_instanceIds :: Lens.Lens' SendCommand (Prelude.Maybe [Prelude.Text])
sendCommand_instanceIds = Lens.lens (\SendCommand' {instanceIds} -> instanceIds) (\s@SendCommand' {} a -> s {instanceIds = a} :: SendCommand) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of errors allowed without the command failing. When
-- the command fails one more time beyond the value of MaxErrors, the
-- systems stops sending the command to additional targets. You can specify
-- a number like 10 or a percentage like 10%. The default value is 0. For
-- more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
-- in the /AWS Systems Manager User Guide/.
sendCommand_maxErrors :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_maxErrors = Lens.lens (\SendCommand' {maxErrors} -> maxErrors) (\s@SendCommand' {} a -> s {maxErrors = a} :: SendCommand)

-- | The ARN of the IAM service role to use to publish Amazon Simple
-- Notification Service (Amazon SNS) notifications for Run Command
-- commands.
sendCommand_serviceRoleArn :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_serviceRoleArn = Lens.lens (\SendCommand' {serviceRoleArn} -> serviceRoleArn) (\s@SendCommand' {} a -> s {serviceRoleArn = a} :: SendCommand)

-- | The name of the S3 bucket where command execution responses should be
-- stored.
sendCommand_outputS3BucketName :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3BucketName = Lens.lens (\SendCommand' {outputS3BucketName} -> outputS3BucketName) (\s@SendCommand' {} a -> s {outputS3BucketName = a} :: SendCommand)

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
sendCommand_comment :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_comment = Lens.lens (\SendCommand' {comment} -> comment) (\s@SendCommand' {} a -> s {comment = a} :: SendCommand)

-- | The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
sendCommand_documentHash :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
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
sendCommand_targets :: Lens.Lens' SendCommand (Prelude.Maybe [Target])
sendCommand_targets = Lens.lens (\SendCommand' {targets} -> targets) (\s@SendCommand' {} a -> s {targets = a} :: SendCommand) Prelude.. Lens.mapping Lens._Coerce

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
sendCommand_outputS3Region :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3Region = Lens.lens (\SendCommand' {outputS3Region} -> outputS3Region) (\s@SendCommand' {} a -> s {outputS3Region = a} :: SendCommand)

-- | (Optional) The maximum number of instances that are allowed to run the
-- command at the same time. You can specify a number such as 10 or a
-- percentage such as 10%. The default value is 50. For more information
-- about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
-- in the /AWS Systems Manager User Guide/.
sendCommand_maxConcurrency :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_maxConcurrency = Lens.lens (\SendCommand' {maxConcurrency} -> maxConcurrency) (\s@SendCommand' {} a -> s {maxConcurrency = a} :: SendCommand)

-- | The directory structure within the S3 bucket where the responses should
-- be stored.
sendCommand_outputS3KeyPrefix :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3KeyPrefix = Lens.lens (\SendCommand' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@SendCommand' {} a -> s {outputS3KeyPrefix = a} :: SendCommand)

-- | If this time is reached and the command has not already started running,
-- it will not run.
sendCommand_timeoutSeconds :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Natural)
sendCommand_timeoutSeconds = Lens.lens (\SendCommand' {timeoutSeconds} -> timeoutSeconds) (\s@SendCommand' {} a -> s {timeoutSeconds = a} :: SendCommand)

-- | Enables Systems Manager to send Run Command output to Amazon CloudWatch
-- Logs.
sendCommand_cloudWatchOutputConfig :: Lens.Lens' SendCommand (Prelude.Maybe CloudWatchOutputConfig)
sendCommand_cloudWatchOutputConfig = Lens.lens (\SendCommand' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@SendCommand' {} a -> s {cloudWatchOutputConfig = a} :: SendCommand)

-- | Sha256 or Sha1.
--
-- Sha1 hashes have been deprecated.
sendCommand_documentHashType :: Lens.Lens' SendCommand (Prelude.Maybe DocumentHashType)
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
sendCommand_documentVersion :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_documentVersion = Lens.lens (\SendCommand' {documentVersion} -> documentVersion) (\s@SendCommand' {} a -> s {documentVersion = a} :: SendCommand)

-- | The required and optional parameters specified in the document being
-- run.
sendCommand_parameters :: Lens.Lens' SendCommand (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
sendCommand_parameters = Lens.lens (\SendCommand' {parameters} -> parameters) (\s@SendCommand' {} a -> s {parameters = a} :: SendCommand) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Systems Manager document to run. This can be a public
-- document or a custom document. To run a shared document belonging to
-- another account, specify the document ARN. For more information about
-- how to use shared documents, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
-- in the /AWS Systems Manager User Guide/.
sendCommand_documentName :: Lens.Lens' SendCommand Prelude.Text
sendCommand_documentName = Lens.lens (\SendCommand' {documentName} -> documentName) (\s@SendCommand' {} a -> s {documentName = a} :: SendCommand)

instance Core.AWSRequest SendCommand where
  type AWSResponse SendCommand = SendCommandResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendCommandResponse'
            Prelude.<$> (x Core..?> "Command")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendCommand

instance Prelude.NFData SendCommand

instance Core.ToHeaders SendCommand where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.SendCommand" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendCommand where
  toJSON SendCommand' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationConfig" Core..=)
              Prelude.<$> notificationConfig,
            ("InstanceIds" Core..=) Prelude.<$> instanceIds,
            ("MaxErrors" Core..=) Prelude.<$> maxErrors,
            ("ServiceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("OutputS3BucketName" Core..=)
              Prelude.<$> outputS3BucketName,
            ("Comment" Core..=) Prelude.<$> comment,
            ("DocumentHash" Core..=) Prelude.<$> documentHash,
            ("Targets" Core..=) Prelude.<$> targets,
            ("OutputS3Region" Core..=)
              Prelude.<$> outputS3Region,
            ("MaxConcurrency" Core..=)
              Prelude.<$> maxConcurrency,
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
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("DocumentName" Core..= documentName)
          ]
      )

instance Core.ToPath SendCommand where
  toPath = Prelude.const "/"

instance Core.ToQuery SendCommand where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { -- | The request as it was received by Systems Manager. Also provides the
    -- command ID which can be used future references to this request.
    command :: Prelude.Maybe Command,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SendCommandResponse
newSendCommandResponse pHttpStatus_ =
  SendCommandResponse'
    { command = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request as it was received by Systems Manager. Also provides the
-- command ID which can be used future references to this request.
sendCommandResponse_command :: Lens.Lens' SendCommandResponse (Prelude.Maybe Command)
sendCommandResponse_command = Lens.lens (\SendCommandResponse' {command} -> command) (\s@SendCommandResponse' {} a -> s {command = a} :: SendCommandResponse)

-- | The response's http status code.
sendCommandResponse_httpStatus :: Lens.Lens' SendCommandResponse Prelude.Int
sendCommandResponse_httpStatus = Lens.lens (\SendCommandResponse' {httpStatus} -> httpStatus) (\s@SendCommandResponse' {} a -> s {httpStatus = a} :: SendCommandResponse)

instance Prelude.NFData SendCommandResponse
