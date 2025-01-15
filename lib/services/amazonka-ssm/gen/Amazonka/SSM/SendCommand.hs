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
-- Module      : Amazonka.SSM.SendCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs commands on one or more managed nodes.
module Amazonka.SSM.SendCommand
  ( -- * Creating a Request
    SendCommand (..),
    newSendCommand,

    -- * Request Lenses
    sendCommand_alarmConfiguration,
    sendCommand_cloudWatchOutputConfig,
    sendCommand_comment,
    sendCommand_documentHash,
    sendCommand_documentHashType,
    sendCommand_documentVersion,
    sendCommand_instanceIds,
    sendCommand_maxConcurrency,
    sendCommand_maxErrors,
    sendCommand_notificationConfig,
    sendCommand_outputS3BucketName,
    sendCommand_outputS3KeyPrefix,
    sendCommand_outputS3Region,
    sendCommand_parameters,
    sendCommand_serviceRoleArn,
    sendCommand_targets,
    sendCommand_timeoutSeconds,
    sendCommand_documentName,

    -- * Destructuring the Response
    SendCommandResponse (..),
    newSendCommandResponse,

    -- * Response Lenses
    sendCommandResponse_command,
    sendCommandResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newSendCommand' smart constructor.
data SendCommand = SendCommand'
  { -- | The CloudWatch alarm you want to apply to your command.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | Enables Amazon Web Services Systems Manager to send Run Command output
    -- to Amazon CloudWatch Logs. Run Command is a capability of Amazon Web
    -- Services Systems Manager.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The Sha256 or Sha1 hash created by the system when the document was
    -- created.
    --
    -- Sha1 hashes have been deprecated.
    documentHash :: Prelude.Maybe Prelude.Text,
    -- | Sha256 or Sha1.
    --
    -- Sha1 hashes have been deprecated.
    documentHashType :: Prelude.Maybe DocumentHashType,
    -- | The SSM document version to use in the request. You can specify
    -- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
    -- using the Command Line Interface (Amazon Web Services CLI), then you
    -- must escape the first two options by using a backslash. If you specify a
    -- version number, then you don\'t need to use the backslash. For example:
    --
    -- --document-version \"\\$DEFAULT\"
    --
    -- --document-version \"\\$LATEST\"
    --
    -- --document-version \"3\"
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the managed nodes where the command should run. Specifying
    -- managed node IDs is most useful when you are targeting a limited number
    -- of managed nodes, though you can specify up to 50 IDs.
    --
    -- To target a larger number of managed nodes, or if you prefer not to list
    -- individual node IDs, we recommend using the @Targets@ option instead.
    -- Using @Targets@, which accepts tag key-value pairs to identify the
    -- managed nodes to send commands to, you can a send command to tens,
    -- hundreds, or thousands of nodes at once.
    --
    -- For more information about how to use targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | (Optional) The maximum number of managed nodes that are allowed to run
    -- the command at the same time. You can specify a number such as 10 or a
    -- percentage such as 10%. The default value is @50@. For more information
    -- about how to use @MaxConcurrency@, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of errors allowed without the command failing. When
    -- the command fails one more time beyond the value of @MaxErrors@, the
    -- systems stops sending the command to additional targets. You can specify
    -- a number like 10 or a percentage like 10%. The default value is @0@. For
    -- more information about how to use @MaxErrors@, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | Configurations for sending notifications.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The name of the S3 bucket where command execution responses should be
    -- stored.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The directory structure within the S3 bucket where the responses should
    -- be stored.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Amazon
    -- Web Services Region of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | The required and optional parameters specified in the document being
    -- run.
    parameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | The ARN of the Identity and Access Management (IAM) service role to use
    -- to publish Amazon Simple Notification Service (Amazon SNS) notifications
    -- for Run Command commands.
    --
    -- This role must provide the @sns:Publish@ permission for your
    -- notification topic. For information about creating and using this
    -- service role, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | An array of search criteria that targets managed nodes using a
    -- @Key,Value@ combination that you specify. Specifying targets is most
    -- useful when you want to send a command to a large number of managed
    -- nodes at once. Using @Targets@, which accepts tag key-value pairs to
    -- identify managed nodes, you can send a command to tens, hundreds, or
    -- thousands of nodes at once.
    --
    -- To send a command to a smaller number of managed nodes, you can use the
    -- @InstanceIds@ option instead.
    --
    -- For more information about how to use targets, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    targets :: Prelude.Maybe [Target],
    -- | If this time is reached and the command hasn\'t already started running,
    -- it won\'t run.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Amazon Web Services Systems Manager document (SSM
    -- document) to run. This can be a public document or a custom document. To
    -- run a shared document belonging to another account, specify the document
    -- Amazon Resource Name (ARN). For more information about how to use shared
    -- documents, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- If you specify a document name or ARN that hasn\'t been shared with your
    -- account, you receive an @InvalidDocument@ error.
    documentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'sendCommand_alarmConfiguration' - The CloudWatch alarm you want to apply to your command.
--
-- 'cloudWatchOutputConfig', 'sendCommand_cloudWatchOutputConfig' - Enables Amazon Web Services Systems Manager to send Run Command output
-- to Amazon CloudWatch Logs. Run Command is a capability of Amazon Web
-- Services Systems Manager.
--
-- 'comment', 'sendCommand_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'documentHash', 'sendCommand_documentHash' - The Sha256 or Sha1 hash created by the system when the document was
-- created.
--
-- Sha1 hashes have been deprecated.
--
-- 'documentHashType', 'sendCommand_documentHashType' - Sha256 or Sha1.
--
-- Sha1 hashes have been deprecated.
--
-- 'documentVersion', 'sendCommand_documentVersion' - The SSM document version to use in the request. You can specify
-- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
-- using the Command Line Interface (Amazon Web Services CLI), then you
-- must escape the first two options by using a backslash. If you specify a
-- version number, then you don\'t need to use the backslash. For example:
--
-- --document-version \"\\$DEFAULT\"
--
-- --document-version \"\\$LATEST\"
--
-- --document-version \"3\"
--
-- 'instanceIds', 'sendCommand_instanceIds' - The IDs of the managed nodes where the command should run. Specifying
-- managed node IDs is most useful when you are targeting a limited number
-- of managed nodes, though you can specify up to 50 IDs.
--
-- To target a larger number of managed nodes, or if you prefer not to list
-- individual node IDs, we recommend using the @Targets@ option instead.
-- Using @Targets@, which accepts tag key-value pairs to identify the
-- managed nodes to send commands to, you can a send command to tens,
-- hundreds, or thousands of nodes at once.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'maxConcurrency', 'sendCommand_maxConcurrency' - (Optional) The maximum number of managed nodes that are allowed to run
-- the command at the same time. You can specify a number such as 10 or a
-- percentage such as 10%. The default value is @50@. For more information
-- about how to use @MaxConcurrency@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'maxErrors', 'sendCommand_maxErrors' - The maximum number of errors allowed without the command failing. When
-- the command fails one more time beyond the value of @MaxErrors@, the
-- systems stops sending the command to additional targets. You can specify
-- a number like 10 or a percentage like 10%. The default value is @0@. For
-- more information about how to use @MaxErrors@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'notificationConfig', 'sendCommand_notificationConfig' - Configurations for sending notifications.
--
-- 'outputS3BucketName', 'sendCommand_outputS3BucketName' - The name of the S3 bucket where command execution responses should be
-- stored.
--
-- 'outputS3KeyPrefix', 'sendCommand_outputS3KeyPrefix' - The directory structure within the S3 bucket where the responses should
-- be stored.
--
-- 'outputS3Region', 'sendCommand_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Amazon
-- Web Services Region of the S3 bucket.
--
-- 'parameters', 'sendCommand_parameters' - The required and optional parameters specified in the document being
-- run.
--
-- 'serviceRoleArn', 'sendCommand_serviceRoleArn' - The ARN of the Identity and Access Management (IAM) service role to use
-- to publish Amazon Simple Notification Service (Amazon SNS) notifications
-- for Run Command commands.
--
-- This role must provide the @sns:Publish@ permission for your
-- notification topic. For information about creating and using this
-- service role, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'targets', 'sendCommand_targets' - An array of search criteria that targets managed nodes using a
-- @Key,Value@ combination that you specify. Specifying targets is most
-- useful when you want to send a command to a large number of managed
-- nodes at once. Using @Targets@, which accepts tag key-value pairs to
-- identify managed nodes, you can send a command to tens, hundreds, or
-- thousands of nodes at once.
--
-- To send a command to a smaller number of managed nodes, you can use the
-- @InstanceIds@ option instead.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'timeoutSeconds', 'sendCommand_timeoutSeconds' - If this time is reached and the command hasn\'t already started running,
-- it won\'t run.
--
-- 'documentName', 'sendCommand_documentName' - The name of the Amazon Web Services Systems Manager document (SSM
-- document) to run. This can be a public document or a custom document. To
-- run a shared document belonging to another account, specify the document
-- Amazon Resource Name (ARN). For more information about how to use shared
-- documents, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- If you specify a document name or ARN that hasn\'t been shared with your
-- account, you receive an @InvalidDocument@ error.
newSendCommand ::
  -- | 'documentName'
  Prelude.Text ->
  SendCommand
newSendCommand pDocumentName_ =
  SendCommand'
    { alarmConfiguration = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      comment = Prelude.Nothing,
      documentHash = Prelude.Nothing,
      documentHashType = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      notificationConfig = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      parameters = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      targets = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      documentName = pDocumentName_
    }

-- | The CloudWatch alarm you want to apply to your command.
sendCommand_alarmConfiguration :: Lens.Lens' SendCommand (Prelude.Maybe AlarmConfiguration)
sendCommand_alarmConfiguration = Lens.lens (\SendCommand' {alarmConfiguration} -> alarmConfiguration) (\s@SendCommand' {} a -> s {alarmConfiguration = a} :: SendCommand)

-- | Enables Amazon Web Services Systems Manager to send Run Command output
-- to Amazon CloudWatch Logs. Run Command is a capability of Amazon Web
-- Services Systems Manager.
sendCommand_cloudWatchOutputConfig :: Lens.Lens' SendCommand (Prelude.Maybe CloudWatchOutputConfig)
sendCommand_cloudWatchOutputConfig = Lens.lens (\SendCommand' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@SendCommand' {} a -> s {cloudWatchOutputConfig = a} :: SendCommand)

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

-- | Sha256 or Sha1.
--
-- Sha1 hashes have been deprecated.
sendCommand_documentHashType :: Lens.Lens' SendCommand (Prelude.Maybe DocumentHashType)
sendCommand_documentHashType = Lens.lens (\SendCommand' {documentHashType} -> documentHashType) (\s@SendCommand' {} a -> s {documentHashType = a} :: SendCommand)

-- | The SSM document version to use in the request. You can specify
-- \$DEFAULT, $LATEST, or a specific version number. If you run commands by
-- using the Command Line Interface (Amazon Web Services CLI), then you
-- must escape the first two options by using a backslash. If you specify a
-- version number, then you don\'t need to use the backslash. For example:
--
-- --document-version \"\\$DEFAULT\"
--
-- --document-version \"\\$LATEST\"
--
-- --document-version \"3\"
sendCommand_documentVersion :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_documentVersion = Lens.lens (\SendCommand' {documentVersion} -> documentVersion) (\s@SendCommand' {} a -> s {documentVersion = a} :: SendCommand)

-- | The IDs of the managed nodes where the command should run. Specifying
-- managed node IDs is most useful when you are targeting a limited number
-- of managed nodes, though you can specify up to 50 IDs.
--
-- To target a larger number of managed nodes, or if you prefer not to list
-- individual node IDs, we recommend using the @Targets@ option instead.
-- Using @Targets@, which accepts tag key-value pairs to identify the
-- managed nodes to send commands to, you can a send command to tens,
-- hundreds, or thousands of nodes at once.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Using targets and rate controls to send commands to a fleet>
-- in the /Amazon Web Services Systems Manager User Guide/.
sendCommand_instanceIds :: Lens.Lens' SendCommand (Prelude.Maybe [Prelude.Text])
sendCommand_instanceIds = Lens.lens (\SendCommand' {instanceIds} -> instanceIds) (\s@SendCommand' {} a -> s {instanceIds = a} :: SendCommand) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The maximum number of managed nodes that are allowed to run
-- the command at the same time. You can specify a number such as 10 or a
-- percentage such as 10%. The default value is @50@. For more information
-- about how to use @MaxConcurrency@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-velocity Using concurrency controls>
-- in the /Amazon Web Services Systems Manager User Guide/.
sendCommand_maxConcurrency :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_maxConcurrency = Lens.lens (\SendCommand' {maxConcurrency} -> maxConcurrency) (\s@SendCommand' {} a -> s {maxConcurrency = a} :: SendCommand)

-- | The maximum number of errors allowed without the command failing. When
-- the command fails one more time beyond the value of @MaxErrors@, the
-- systems stops sending the command to additional targets. You can specify
-- a number like 10 or a percentage like 10%. The default value is @0@. For
-- more information about how to use @MaxErrors@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-maxerrors Using error controls>
-- in the /Amazon Web Services Systems Manager User Guide/.
sendCommand_maxErrors :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_maxErrors = Lens.lens (\SendCommand' {maxErrors} -> maxErrors) (\s@SendCommand' {} a -> s {maxErrors = a} :: SendCommand)

-- | Configurations for sending notifications.
sendCommand_notificationConfig :: Lens.Lens' SendCommand (Prelude.Maybe NotificationConfig)
sendCommand_notificationConfig = Lens.lens (\SendCommand' {notificationConfig} -> notificationConfig) (\s@SendCommand' {} a -> s {notificationConfig = a} :: SendCommand)

-- | The name of the S3 bucket where command execution responses should be
-- stored.
sendCommand_outputS3BucketName :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3BucketName = Lens.lens (\SendCommand' {outputS3BucketName} -> outputS3BucketName) (\s@SendCommand' {} a -> s {outputS3BucketName = a} :: SendCommand)

-- | The directory structure within the S3 bucket where the responses should
-- be stored.
sendCommand_outputS3KeyPrefix :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3KeyPrefix = Lens.lens (\SendCommand' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@SendCommand' {} a -> s {outputS3KeyPrefix = a} :: SendCommand)

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Amazon
-- Web Services Region of the S3 bucket.
sendCommand_outputS3Region :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_outputS3Region = Lens.lens (\SendCommand' {outputS3Region} -> outputS3Region) (\s@SendCommand' {} a -> s {outputS3Region = a} :: SendCommand)

-- | The required and optional parameters specified in the document being
-- run.
sendCommand_parameters :: Lens.Lens' SendCommand (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
sendCommand_parameters = Lens.lens (\SendCommand' {parameters} -> parameters) (\s@SendCommand' {} a -> s {parameters = a} :: SendCommand) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ARN of the Identity and Access Management (IAM) service role to use
-- to publish Amazon Simple Notification Service (Amazon SNS) notifications
-- for Run Command commands.
--
-- This role must provide the @sns:Publish@ permission for your
-- notification topic. For information about creating and using this
-- service role, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitoring-sns-notifications.html Monitoring Systems Manager status changes using Amazon SNS notifications>
-- in the /Amazon Web Services Systems Manager User Guide/.
sendCommand_serviceRoleArn :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Text)
sendCommand_serviceRoleArn = Lens.lens (\SendCommand' {serviceRoleArn} -> serviceRoleArn) (\s@SendCommand' {} a -> s {serviceRoleArn = a} :: SendCommand)

-- | An array of search criteria that targets managed nodes using a
-- @Key,Value@ combination that you specify. Specifying targets is most
-- useful when you want to send a command to a large number of managed
-- nodes at once. Using @Targets@, which accepts tag key-value pairs to
-- identify managed nodes, you can send a command to tens, hundreds, or
-- thousands of nodes at once.
--
-- To send a command to a smaller number of managed nodes, you can use the
-- @InstanceIds@ option instead.
--
-- For more information about how to use targets, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending commands to a fleet>
-- in the /Amazon Web Services Systems Manager User Guide/.
sendCommand_targets :: Lens.Lens' SendCommand (Prelude.Maybe [Target])
sendCommand_targets = Lens.lens (\SendCommand' {targets} -> targets) (\s@SendCommand' {} a -> s {targets = a} :: SendCommand) Prelude.. Lens.mapping Lens.coerced

-- | If this time is reached and the command hasn\'t already started running,
-- it won\'t run.
sendCommand_timeoutSeconds :: Lens.Lens' SendCommand (Prelude.Maybe Prelude.Natural)
sendCommand_timeoutSeconds = Lens.lens (\SendCommand' {timeoutSeconds} -> timeoutSeconds) (\s@SendCommand' {} a -> s {timeoutSeconds = a} :: SendCommand)

-- | The name of the Amazon Web Services Systems Manager document (SSM
-- document) to run. This can be a public document or a custom document. To
-- run a shared document belonging to another account, specify the document
-- Amazon Resource Name (ARN). For more information about how to use shared
-- documents, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ssm-using-shared.html Using shared SSM documents>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- If you specify a document name or ARN that hasn\'t been shared with your
-- account, you receive an @InvalidDocument@ error.
sendCommand_documentName :: Lens.Lens' SendCommand Prelude.Text
sendCommand_documentName = Lens.lens (\SendCommand' {documentName} -> documentName) (\s@SendCommand' {} a -> s {documentName = a} :: SendCommand)

instance Core.AWSRequest SendCommand where
  type AWSResponse SendCommand = SendCommandResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendCommandResponse'
            Prelude.<$> (x Data..?> "Command")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendCommand where
  hashWithSalt _salt SendCommand' {..} =
    _salt
      `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` cloudWatchOutputConfig
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` documentHash
      `Prelude.hashWithSalt` documentHashType
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` notificationConfig
      `Prelude.hashWithSalt` outputS3BucketName
      `Prelude.hashWithSalt` outputS3KeyPrefix
      `Prelude.hashWithSalt` outputS3Region
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` documentName

instance Prelude.NFData SendCommand where
  rnf SendCommand' {..} =
    Prelude.rnf alarmConfiguration `Prelude.seq`
      Prelude.rnf cloudWatchOutputConfig `Prelude.seq`
        Prelude.rnf comment `Prelude.seq`
          Prelude.rnf documentHash `Prelude.seq`
            Prelude.rnf documentHashType `Prelude.seq`
              Prelude.rnf documentVersion `Prelude.seq`
                Prelude.rnf instanceIds `Prelude.seq`
                  Prelude.rnf maxConcurrency `Prelude.seq`
                    Prelude.rnf maxErrors `Prelude.seq`
                      Prelude.rnf notificationConfig `Prelude.seq`
                        Prelude.rnf outputS3BucketName `Prelude.seq`
                          Prelude.rnf outputS3KeyPrefix `Prelude.seq`
                            Prelude.rnf outputS3Region `Prelude.seq`
                              Prelude.rnf parameters `Prelude.seq`
                                Prelude.rnf serviceRoleArn `Prelude.seq`
                                  Prelude.rnf targets `Prelude.seq`
                                    Prelude.rnf timeoutSeconds `Prelude.seq`
                                      Prelude.rnf documentName

instance Data.ToHeaders SendCommand where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.SendCommand" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendCommand where
  toJSON SendCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlarmConfiguration" Data..=)
              Prelude.<$> alarmConfiguration,
            ("CloudWatchOutputConfig" Data..=)
              Prelude.<$> cloudWatchOutputConfig,
            ("Comment" Data..=) Prelude.<$> comment,
            ("DocumentHash" Data..=) Prelude.<$> documentHash,
            ("DocumentHashType" Data..=)
              Prelude.<$> documentHashType,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("InstanceIds" Data..=) Prelude.<$> instanceIds,
            ("MaxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("MaxErrors" Data..=) Prelude.<$> maxErrors,
            ("NotificationConfig" Data..=)
              Prelude.<$> notificationConfig,
            ("OutputS3BucketName" Data..=)
              Prelude.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Data..=)
              Prelude.<$> outputS3KeyPrefix,
            ("OutputS3Region" Data..=)
              Prelude.<$> outputS3Region,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("ServiceRoleArn" Data..=)
              Prelude.<$> serviceRoleArn,
            ("Targets" Data..=) Prelude.<$> targets,
            ("TimeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            Prelude.Just ("DocumentName" Data..= documentName)
          ]
      )

instance Data.ToPath SendCommand where
  toPath = Prelude.const "/"

instance Data.ToQuery SendCommand where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { -- | The request as it was received by Systems Manager. Also provides the
    -- command ID which can be used future references to this request.
    command :: Prelude.Maybe Command,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData SendCommandResponse where
  rnf SendCommandResponse' {..} =
    Prelude.rnf command `Prelude.seq`
      Prelude.rnf httpStatus
