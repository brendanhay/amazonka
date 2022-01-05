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
-- Module      : Amazonka.SSM.Types.Command
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Command where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CloudWatchOutputConfig
import Amazonka.SSM.Types.CommandStatus
import Amazonka.SSM.Types.NotificationConfig
import Amazonka.SSM.Types.Target

-- | Describes a command request.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | The status of the command.
    status :: Prelude.Maybe CommandStatus,
    -- | If this time is reached and the command hasn\'t already started running,
    -- it won\'t run. Calculated based on the @ExpiresAfter@ user input
    -- provided as part of the @SendCommand@ API operation.
    expiresAfter :: Prelude.Maybe Core.POSIX,
    -- | Configurations for sending notifications about command status changes.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The number of targets for the command.
    targetCount :: Prelude.Maybe Prelude.Int,
    -- | Amazon CloudWatch Logs information where you want Amazon Web Services
    -- Systems Manager to send the command output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | The number of targets for which the status is Delivery Timed Out.
    deliveryTimedOutCount :: Prelude.Maybe Prelude.Int,
    -- | The S3 directory path inside the bucket where the responses to the
    -- command executions should be stored. This was requested when issuing the
    -- command.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the document requested for execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The number of targets for which the status is Failed or Execution Timed
    -- Out.
    errorCount :: Prelude.Maybe Prelude.Int,
    -- | A detailed status of the command execution. @StatusDetails@ includes
    -- more information than @Status@ because it includes states resulting from
    -- error and concurrency control parameters. @StatusDetails@ can show
    -- different results than Status. For more information about these
    -- statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
    -- can be one of the following values:
    --
    -- -   Pending: The command hasn\'t been sent to any instances.
    --
    -- -   In Progress: The command has been sent to at least one instance but
    --     hasn\'t reached a final state on all instances.
    --
    -- -   Success: The command successfully ran on all invocations. This is a
    --     terminal state.
    --
    -- -   Delivery Timed Out: The value of MaxErrors or more command
    --     invocations shows a status of Delivery Timed Out. This is a terminal
    --     state.
    --
    -- -   Execution Timed Out: The value of MaxErrors or more command
    --     invocations shows a status of Execution Timed Out. This is a
    --     terminal state.
    --
    -- -   Failed: The value of MaxErrors or more command invocations shows a
    --     status of Failed. This is a terminal state.
    --
    -- -   Incomplete: The command was attempted on all instances and one or
    --     more invocations doesn\'t have a value of Success but not enough
    --     invocations failed for the status to be Failed. This is a terminal
    --     state.
    --
    -- -   Canceled: The command was terminated before it was completed. This
    --     is a terminal state.
    --
    -- -   Rate Exceeded: The number of instances targeted by the command
    --     exceeded the account limit for pending invocations. The system has
    --     canceled the command before running it on any instance. This is a
    --     terminal state.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of errors allowed before the system stops sending the
    -- command to additional targets. You can specify a number of errors, such
    -- as 10, or a percentage or errors, such as 10%. The default value is @0@.
    -- For more information about how to use @MaxErrors@, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The instance IDs against which this command was requested.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Amazon
    -- Web Services Region of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | An array of search criteria that targets instances using a Key,Value
    -- combination that you specify. Targets is required if you don\'t provide
    -- one or more instance IDs in the call.
    targets :: Prelude.Maybe [Target],
    -- | A unique identifier for this command.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | The parameter values to be inserted in the document when running the
    -- command.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The Systems Manager document (SSM document) version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The @TimeoutSeconds@ value specified for a command.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The number of targets for which the command invocation reached a
    -- terminal state. Terminal states include the following: Success, Failed,
    -- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
    -- Undeliverable.
    completedCount :: Prelude.Maybe Prelude.Int,
    -- | The S3 bucket where the responses to the command executions should be
    -- stored. This was requested when issuing the command.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances that are allowed to run the command at
    -- the same time. You can specify a number of instances, such as 10, or a
    -- percentage of instances, such as 10%. The default value is 50. For more
    -- information about how to use @MaxConcurrency@, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The date and time the command was requested.
    requestedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Identity and Access Management (IAM) service role that Run Command,
    -- a capability of Amazon Web Services Systems Manager, uses to act on your
    -- behalf when sending notifications about command status changes.
    serviceRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Command' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'command_status' - The status of the command.
--
-- 'expiresAfter', 'command_expiresAfter' - If this time is reached and the command hasn\'t already started running,
-- it won\'t run. Calculated based on the @ExpiresAfter@ user input
-- provided as part of the @SendCommand@ API operation.
--
-- 'notificationConfig', 'command_notificationConfig' - Configurations for sending notifications about command status changes.
--
-- 'targetCount', 'command_targetCount' - The number of targets for the command.
--
-- 'cloudWatchOutputConfig', 'command_cloudWatchOutputConfig' - Amazon CloudWatch Logs information where you want Amazon Web Services
-- Systems Manager to send the command output.
--
-- 'deliveryTimedOutCount', 'command_deliveryTimedOutCount' - The number of targets for which the status is Delivery Timed Out.
--
-- 'outputS3KeyPrefix', 'command_outputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
--
-- 'documentName', 'command_documentName' - The name of the document requested for execution.
--
-- 'errorCount', 'command_errorCount' - The number of targets for which the status is Failed or Execution Timed
-- Out.
--
-- 'statusDetails', 'command_statusDetails' - A detailed status of the command execution. @StatusDetails@ includes
-- more information than @Status@ because it includes states resulting from
-- error and concurrency control parameters. @StatusDetails@ can show
-- different results than Status. For more information about these
-- statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to any instances.
--
-- -   In Progress: The command has been sent to at least one instance but
--     hasn\'t reached a final state on all instances.
--
-- -   Success: The command successfully ran on all invocations. This is a
--     terminal state.
--
-- -   Delivery Timed Out: The value of MaxErrors or more command
--     invocations shows a status of Delivery Timed Out. This is a terminal
--     state.
--
-- -   Execution Timed Out: The value of MaxErrors or more command
--     invocations shows a status of Execution Timed Out. This is a
--     terminal state.
--
-- -   Failed: The value of MaxErrors or more command invocations shows a
--     status of Failed. This is a terminal state.
--
-- -   Incomplete: The command was attempted on all instances and one or
--     more invocations doesn\'t have a value of Success but not enough
--     invocations failed for the status to be Failed. This is a terminal
--     state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Rate Exceeded: The number of instances targeted by the command
--     exceeded the account limit for pending invocations. The system has
--     canceled the command before running it on any instance. This is a
--     terminal state.
--
-- 'maxErrors', 'command_maxErrors' - The maximum number of errors allowed before the system stops sending the
-- command to additional targets. You can specify a number of errors, such
-- as 10, or a percentage or errors, such as 10%. The default value is @0@.
-- For more information about how to use @MaxErrors@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'instanceIds', 'command_instanceIds' - The instance IDs against which this command was requested.
--
-- 'outputS3Region', 'command_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Amazon
-- Web Services Region of the S3 bucket.
--
-- 'targets', 'command_targets' - An array of search criteria that targets instances using a Key,Value
-- combination that you specify. Targets is required if you don\'t provide
-- one or more instance IDs in the call.
--
-- 'commandId', 'command_commandId' - A unique identifier for this command.
--
-- 'parameters', 'command_parameters' - The parameter values to be inserted in the document when running the
-- command.
--
-- 'documentVersion', 'command_documentVersion' - The Systems Manager document (SSM document) version.
--
-- 'timeoutSeconds', 'command_timeoutSeconds' - The @TimeoutSeconds@ value specified for a command.
--
-- 'comment', 'command_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'completedCount', 'command_completedCount' - The number of targets for which the command invocation reached a
-- terminal state. Terminal states include the following: Success, Failed,
-- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
-- Undeliverable.
--
-- 'outputS3BucketName', 'command_outputS3BucketName' - The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
--
-- 'maxConcurrency', 'command_maxConcurrency' - The maximum number of instances that are allowed to run the command at
-- the same time. You can specify a number of instances, such as 10, or a
-- percentage of instances, such as 10%. The default value is 50. For more
-- information about how to use @MaxConcurrency@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'requestedDateTime', 'command_requestedDateTime' - The date and time the command was requested.
--
-- 'serviceRole', 'command_serviceRole' - The Identity and Access Management (IAM) service role that Run Command,
-- a capability of Amazon Web Services Systems Manager, uses to act on your
-- behalf when sending notifications about command status changes.
newCommand ::
  Command
newCommand =
  Command'
    { status = Prelude.Nothing,
      expiresAfter = Prelude.Nothing,
      notificationConfig = Prelude.Nothing,
      targetCount = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      deliveryTimedOutCount = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      documentName = Prelude.Nothing,
      errorCount = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      targets = Prelude.Nothing,
      commandId = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      comment = Prelude.Nothing,
      completedCount = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      requestedDateTime = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | The status of the command.
command_status :: Lens.Lens' Command (Prelude.Maybe CommandStatus)
command_status = Lens.lens (\Command' {status} -> status) (\s@Command' {} a -> s {status = a} :: Command)

-- | If this time is reached and the command hasn\'t already started running,
-- it won\'t run. Calculated based on the @ExpiresAfter@ user input
-- provided as part of the @SendCommand@ API operation.
command_expiresAfter :: Lens.Lens' Command (Prelude.Maybe Prelude.UTCTime)
command_expiresAfter = Lens.lens (\Command' {expiresAfter} -> expiresAfter) (\s@Command' {} a -> s {expiresAfter = a} :: Command) Prelude.. Lens.mapping Core._Time

-- | Configurations for sending notifications about command status changes.
command_notificationConfig :: Lens.Lens' Command (Prelude.Maybe NotificationConfig)
command_notificationConfig = Lens.lens (\Command' {notificationConfig} -> notificationConfig) (\s@Command' {} a -> s {notificationConfig = a} :: Command)

-- | The number of targets for the command.
command_targetCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_targetCount = Lens.lens (\Command' {targetCount} -> targetCount) (\s@Command' {} a -> s {targetCount = a} :: Command)

-- | Amazon CloudWatch Logs information where you want Amazon Web Services
-- Systems Manager to send the command output.
command_cloudWatchOutputConfig :: Lens.Lens' Command (Prelude.Maybe CloudWatchOutputConfig)
command_cloudWatchOutputConfig = Lens.lens (\Command' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@Command' {} a -> s {cloudWatchOutputConfig = a} :: Command)

-- | The number of targets for which the status is Delivery Timed Out.
command_deliveryTimedOutCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_deliveryTimedOutCount = Lens.lens (\Command' {deliveryTimedOutCount} -> deliveryTimedOutCount) (\s@Command' {} a -> s {deliveryTimedOutCount = a} :: Command)

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
command_outputS3KeyPrefix :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_outputS3KeyPrefix = Lens.lens (\Command' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@Command' {} a -> s {outputS3KeyPrefix = a} :: Command)

-- | The name of the document requested for execution.
command_documentName :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_documentName = Lens.lens (\Command' {documentName} -> documentName) (\s@Command' {} a -> s {documentName = a} :: Command)

-- | The number of targets for which the status is Failed or Execution Timed
-- Out.
command_errorCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_errorCount = Lens.lens (\Command' {errorCount} -> errorCount) (\s@Command' {} a -> s {errorCount = a} :: Command)

-- | A detailed status of the command execution. @StatusDetails@ includes
-- more information than @Status@ because it includes states resulting from
-- error and concurrency control parameters. @StatusDetails@ can show
-- different results than Status. For more information about these
-- statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. StatusDetails
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to any instances.
--
-- -   In Progress: The command has been sent to at least one instance but
--     hasn\'t reached a final state on all instances.
--
-- -   Success: The command successfully ran on all invocations. This is a
--     terminal state.
--
-- -   Delivery Timed Out: The value of MaxErrors or more command
--     invocations shows a status of Delivery Timed Out. This is a terminal
--     state.
--
-- -   Execution Timed Out: The value of MaxErrors or more command
--     invocations shows a status of Execution Timed Out. This is a
--     terminal state.
--
-- -   Failed: The value of MaxErrors or more command invocations shows a
--     status of Failed. This is a terminal state.
--
-- -   Incomplete: The command was attempted on all instances and one or
--     more invocations doesn\'t have a value of Success but not enough
--     invocations failed for the status to be Failed. This is a terminal
--     state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Rate Exceeded: The number of instances targeted by the command
--     exceeded the account limit for pending invocations. The system has
--     canceled the command before running it on any instance. This is a
--     terminal state.
command_statusDetails :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_statusDetails = Lens.lens (\Command' {statusDetails} -> statusDetails) (\s@Command' {} a -> s {statusDetails = a} :: Command)

-- | The maximum number of errors allowed before the system stops sending the
-- command to additional targets. You can specify a number of errors, such
-- as 10, or a percentage or errors, such as 10%. The default value is @0@.
-- For more information about how to use @MaxErrors@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
command_maxErrors :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_maxErrors = Lens.lens (\Command' {maxErrors} -> maxErrors) (\s@Command' {} a -> s {maxErrors = a} :: Command)

-- | The instance IDs against which this command was requested.
command_instanceIds :: Lens.Lens' Command (Prelude.Maybe [Prelude.Text])
command_instanceIds = Lens.lens (\Command' {instanceIds} -> instanceIds) (\s@Command' {} a -> s {instanceIds = a} :: Command) Prelude.. Lens.mapping Lens.coerced

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Amazon
-- Web Services Region of the S3 bucket.
command_outputS3Region :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_outputS3Region = Lens.lens (\Command' {outputS3Region} -> outputS3Region) (\s@Command' {} a -> s {outputS3Region = a} :: Command)

-- | An array of search criteria that targets instances using a Key,Value
-- combination that you specify. Targets is required if you don\'t provide
-- one or more instance IDs in the call.
command_targets :: Lens.Lens' Command (Prelude.Maybe [Target])
command_targets = Lens.lens (\Command' {targets} -> targets) (\s@Command' {} a -> s {targets = a} :: Command) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for this command.
command_commandId :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_commandId = Lens.lens (\Command' {commandId} -> commandId) (\s@Command' {} a -> s {commandId = a} :: Command)

-- | The parameter values to be inserted in the document when running the
-- command.
command_parameters :: Lens.Lens' Command (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
command_parameters = Lens.lens (\Command' {parameters} -> parameters) (\s@Command' {} a -> s {parameters = a} :: Command) Prelude.. Lens.mapping Lens.coerced

-- | The Systems Manager document (SSM document) version.
command_documentVersion :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_documentVersion = Lens.lens (\Command' {documentVersion} -> documentVersion) (\s@Command' {} a -> s {documentVersion = a} :: Command)

-- | The @TimeoutSeconds@ value specified for a command.
command_timeoutSeconds :: Lens.Lens' Command (Prelude.Maybe Prelude.Natural)
command_timeoutSeconds = Lens.lens (\Command' {timeoutSeconds} -> timeoutSeconds) (\s@Command' {} a -> s {timeoutSeconds = a} :: Command)

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
command_comment :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_comment = Lens.lens (\Command' {comment} -> comment) (\s@Command' {} a -> s {comment = a} :: Command)

-- | The number of targets for which the command invocation reached a
-- terminal state. Terminal states include the following: Success, Failed,
-- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
-- Undeliverable.
command_completedCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_completedCount = Lens.lens (\Command' {completedCount} -> completedCount) (\s@Command' {} a -> s {completedCount = a} :: Command)

-- | The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
command_outputS3BucketName :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_outputS3BucketName = Lens.lens (\Command' {outputS3BucketName} -> outputS3BucketName) (\s@Command' {} a -> s {outputS3BucketName = a} :: Command)

-- | The maximum number of instances that are allowed to run the command at
-- the same time. You can specify a number of instances, such as 10, or a
-- percentage of instances, such as 10%. The default value is 50. For more
-- information about how to use @MaxConcurrency@, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /Amazon Web Services Systems Manager User Guide/.
command_maxConcurrency :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_maxConcurrency = Lens.lens (\Command' {maxConcurrency} -> maxConcurrency) (\s@Command' {} a -> s {maxConcurrency = a} :: Command)

-- | The date and time the command was requested.
command_requestedDateTime :: Lens.Lens' Command (Prelude.Maybe Prelude.UTCTime)
command_requestedDateTime = Lens.lens (\Command' {requestedDateTime} -> requestedDateTime) (\s@Command' {} a -> s {requestedDateTime = a} :: Command) Prelude.. Lens.mapping Core._Time

-- | The Identity and Access Management (IAM) service role that Run Command,
-- a capability of Amazon Web Services Systems Manager, uses to act on your
-- behalf when sending notifications about command status changes.
command_serviceRole :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_serviceRole = Lens.lens (\Command' {serviceRole} -> serviceRole) (\s@Command' {} a -> s {serviceRole = a} :: Command)

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject
      "Command"
      ( \x ->
          Command'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExpiresAfter")
            Prelude.<*> (x Core..:? "NotificationConfig")
            Prelude.<*> (x Core..:? "TargetCount")
            Prelude.<*> (x Core..:? "CloudWatchOutputConfig")
            Prelude.<*> (x Core..:? "DeliveryTimedOutCount")
            Prelude.<*> (x Core..:? "OutputS3KeyPrefix")
            Prelude.<*> (x Core..:? "DocumentName")
            Prelude.<*> (x Core..:? "ErrorCount")
            Prelude.<*> (x Core..:? "StatusDetails")
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "InstanceIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputS3Region")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CommandId")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "Comment")
            Prelude.<*> (x Core..:? "CompletedCount")
            Prelude.<*> (x Core..:? "OutputS3BucketName")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "RequestedDateTime")
            Prelude.<*> (x Core..:? "ServiceRole")
      )

instance Prelude.Hashable Command where
  hashWithSalt _salt Command' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` expiresAfter
      `Prelude.hashWithSalt` notificationConfig
      `Prelude.hashWithSalt` targetCount
      `Prelude.hashWithSalt` cloudWatchOutputConfig
      `Prelude.hashWithSalt` deliveryTimedOutCount
      `Prelude.hashWithSalt` outputS3KeyPrefix
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` errorCount
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` outputS3Region
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` completedCount
      `Prelude.hashWithSalt` outputS3BucketName
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` requestedDateTime
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData Command where
  rnf Command' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf expiresAfter
      `Prelude.seq` Prelude.rnf notificationConfig
      `Prelude.seq` Prelude.rnf targetCount
      `Prelude.seq` Prelude.rnf cloudWatchOutputConfig
      `Prelude.seq` Prelude.rnf deliveryTimedOutCount
      `Prelude.seq` Prelude.rnf outputS3KeyPrefix
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf errorCount
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf outputS3Region
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf completedCount
      `Prelude.seq` Prelude.rnf
        outputS3BucketName
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf
        requestedDateTime
      `Prelude.seq` Prelude.rnf
        serviceRole
