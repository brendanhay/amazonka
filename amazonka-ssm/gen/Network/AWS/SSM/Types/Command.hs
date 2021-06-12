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
-- Module      : Network.AWS.SSM.Types.Command
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Command where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandStatus
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.Target

-- | Describes a command request.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | Configurations for sending notifications about command status changes.
    notificationConfig :: Core.Maybe NotificationConfig,
    -- | The instance IDs against which this command was requested.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The maximum number of errors allowed before the system stops sending the
    -- command to additional targets. You can specify a number of errors, such
    -- as 10, or a percentage or errors, such as 10%. The default value is 0.
    -- For more information about how to use MaxErrors, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /AWS Systems Manager User Guide/.
    maxErrors :: Core.Maybe Core.Text,
    -- | If this time is reached and the command has not already started running,
    -- it will not run. Calculated based on the ExpiresAfter user input
    -- provided as part of the SendCommand API.
    expiresAfter :: Core.Maybe Core.POSIX,
    -- | The status of the command.
    status :: Core.Maybe CommandStatus,
    -- | The IAM service role that Run Command uses to act on your behalf when
    -- sending notifications about command status changes.
    serviceRole :: Core.Maybe Core.Text,
    -- | The date and time the command was requested.
    requestedDateTime :: Core.Maybe Core.POSIX,
    -- | A detailed status of the command execution. StatusDetails includes more
    -- information than Status because it includes states resulting from error
    -- and concurrency control parameters. StatusDetails can show different
    -- results than Status. For more information about these statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
    -- following values:
    --
    -- -   Pending: The command has not been sent to any instances.
    --
    -- -   In Progress: The command has been sent to at least one instance but
    --     has not reached a final state on all instances.
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
    --     more invocations does not have a value of Success but not enough
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
    statusDetails :: Core.Maybe Core.Text,
    -- | The number of targets for which the command invocation reached a
    -- terminal state. Terminal states include the following: Success, Failed,
    -- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
    -- Undeliverable.
    completedCount :: Core.Maybe Core.Int,
    -- | The S3 bucket where the responses to the command executions should be
    -- stored. This was requested when issuing the command.
    outputS3BucketName :: Core.Maybe Core.Text,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Core.Maybe Core.Text,
    -- | The number of targets for which the status is Failed or Execution Timed
    -- Out.
    errorCount :: Core.Maybe Core.Int,
    -- | The name of the document requested for execution.
    documentName :: Core.Maybe Core.Text,
    -- | A unique identifier for this command.
    commandId :: Core.Maybe Core.Text,
    -- | An array of search criteria that targets instances using a Key,Value
    -- combination that you specify. Targets is required if you don\'t provide
    -- one or more instance IDs in the call.
    targets :: Core.Maybe [Target],
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Core.Maybe Core.Text,
    -- | The maximum number of instances that are allowed to run the command at
    -- the same time. You can specify a number of instances, such as 10, or a
    -- percentage of instances, such as 10%. The default value is 50. For more
    -- information about how to use MaxConcurrency, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /AWS Systems Manager User Guide/.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The S3 directory path inside the bucket where the responses to the
    -- command executions should be stored. This was requested when issuing the
    -- command.
    outputS3KeyPrefix :: Core.Maybe Core.Text,
    -- | The @TimeoutSeconds@ value specified for a command.
    timeoutSeconds :: Core.Maybe Core.Natural,
    -- | The number of targets for which the status is Delivery Timed Out.
    deliveryTimedOutCount :: Core.Maybe Core.Int,
    -- | CloudWatch Logs information where you want Systems Manager to send the
    -- command output.
    cloudWatchOutputConfig :: Core.Maybe CloudWatchOutputConfig,
    -- | The SSM document version.
    documentVersion :: Core.Maybe Core.Text,
    -- | The parameter values to be inserted in the document when running the
    -- command.
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The number of targets for the command.
    targetCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Command' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationConfig', 'command_notificationConfig' - Configurations for sending notifications about command status changes.
--
-- 'instanceIds', 'command_instanceIds' - The instance IDs against which this command was requested.
--
-- 'maxErrors', 'command_maxErrors' - The maximum number of errors allowed before the system stops sending the
-- command to additional targets. You can specify a number of errors, such
-- as 10, or a percentage or errors, such as 10%. The default value is 0.
-- For more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
--
-- 'expiresAfter', 'command_expiresAfter' - If this time is reached and the command has not already started running,
-- it will not run. Calculated based on the ExpiresAfter user input
-- provided as part of the SendCommand API.
--
-- 'status', 'command_status' - The status of the command.
--
-- 'serviceRole', 'command_serviceRole' - The IAM service role that Run Command uses to act on your behalf when
-- sending notifications about command status changes.
--
-- 'requestedDateTime', 'command_requestedDateTime' - The date and time the command was requested.
--
-- 'statusDetails', 'command_statusDetails' - A detailed status of the command execution. StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to any instances.
--
-- -   In Progress: The command has been sent to at least one instance but
--     has not reached a final state on all instances.
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
--     more invocations does not have a value of Success but not enough
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
-- 'completedCount', 'command_completedCount' - The number of targets for which the command invocation reached a
-- terminal state. Terminal states include the following: Success, Failed,
-- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
-- Undeliverable.
--
-- 'outputS3BucketName', 'command_outputS3BucketName' - The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
--
-- 'comment', 'command_comment' - User-specified information about the command, such as a brief
-- description of what the command should do.
--
-- 'errorCount', 'command_errorCount' - The number of targets for which the status is Failed or Execution Timed
-- Out.
--
-- 'documentName', 'command_documentName' - The name of the document requested for execution.
--
-- 'commandId', 'command_commandId' - A unique identifier for this command.
--
-- 'targets', 'command_targets' - An array of search criteria that targets instances using a Key,Value
-- combination that you specify. Targets is required if you don\'t provide
-- one or more instance IDs in the call.
--
-- 'outputS3Region', 'command_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
--
-- 'maxConcurrency', 'command_maxConcurrency' - The maximum number of instances that are allowed to run the command at
-- the same time. You can specify a number of instances, such as 10, or a
-- percentage of instances, such as 10%. The default value is 50. For more
-- information about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
--
-- 'outputS3KeyPrefix', 'command_outputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
--
-- 'timeoutSeconds', 'command_timeoutSeconds' - The @TimeoutSeconds@ value specified for a command.
--
-- 'deliveryTimedOutCount', 'command_deliveryTimedOutCount' - The number of targets for which the status is Delivery Timed Out.
--
-- 'cloudWatchOutputConfig', 'command_cloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the
-- command output.
--
-- 'documentVersion', 'command_documentVersion' - The SSM document version.
--
-- 'parameters', 'command_parameters' - The parameter values to be inserted in the document when running the
-- command.
--
-- 'targetCount', 'command_targetCount' - The number of targets for the command.
newCommand ::
  Command
newCommand =
  Command'
    { notificationConfig = Core.Nothing,
      instanceIds = Core.Nothing,
      maxErrors = Core.Nothing,
      expiresAfter = Core.Nothing,
      status = Core.Nothing,
      serviceRole = Core.Nothing,
      requestedDateTime = Core.Nothing,
      statusDetails = Core.Nothing,
      completedCount = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      comment = Core.Nothing,
      errorCount = Core.Nothing,
      documentName = Core.Nothing,
      commandId = Core.Nothing,
      targets = Core.Nothing,
      outputS3Region = Core.Nothing,
      maxConcurrency = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      deliveryTimedOutCount = Core.Nothing,
      cloudWatchOutputConfig = Core.Nothing,
      documentVersion = Core.Nothing,
      parameters = Core.Nothing,
      targetCount = Core.Nothing
    }

-- | Configurations for sending notifications about command status changes.
command_notificationConfig :: Lens.Lens' Command (Core.Maybe NotificationConfig)
command_notificationConfig = Lens.lens (\Command' {notificationConfig} -> notificationConfig) (\s@Command' {} a -> s {notificationConfig = a} :: Command)

-- | The instance IDs against which this command was requested.
command_instanceIds :: Lens.Lens' Command (Core.Maybe [Core.Text])
command_instanceIds = Lens.lens (\Command' {instanceIds} -> instanceIds) (\s@Command' {} a -> s {instanceIds = a} :: Command) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of errors allowed before the system stops sending the
-- command to additional targets. You can specify a number of errors, such
-- as 10, or a percentage or errors, such as 10%. The default value is 0.
-- For more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
command_maxErrors :: Lens.Lens' Command (Core.Maybe Core.Text)
command_maxErrors = Lens.lens (\Command' {maxErrors} -> maxErrors) (\s@Command' {} a -> s {maxErrors = a} :: Command)

-- | If this time is reached and the command has not already started running,
-- it will not run. Calculated based on the ExpiresAfter user input
-- provided as part of the SendCommand API.
command_expiresAfter :: Lens.Lens' Command (Core.Maybe Core.UTCTime)
command_expiresAfter = Lens.lens (\Command' {expiresAfter} -> expiresAfter) (\s@Command' {} a -> s {expiresAfter = a} :: Command) Core.. Lens.mapping Core._Time

-- | The status of the command.
command_status :: Lens.Lens' Command (Core.Maybe CommandStatus)
command_status = Lens.lens (\Command' {status} -> status) (\s@Command' {} a -> s {status = a} :: Command)

-- | The IAM service role that Run Command uses to act on your behalf when
-- sending notifications about command status changes.
command_serviceRole :: Lens.Lens' Command (Core.Maybe Core.Text)
command_serviceRole = Lens.lens (\Command' {serviceRole} -> serviceRole) (\s@Command' {} a -> s {serviceRole = a} :: Command)

-- | The date and time the command was requested.
command_requestedDateTime :: Lens.Lens' Command (Core.Maybe Core.UTCTime)
command_requestedDateTime = Lens.lens (\Command' {requestedDateTime} -> requestedDateTime) (\s@Command' {} a -> s {requestedDateTime = a} :: Command) Core.. Lens.mapping Core._Time

-- | A detailed status of the command execution. StatusDetails includes more
-- information than Status because it includes states resulting from error
-- and concurrency control parameters. StatusDetails can show different
-- results than Status. For more information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to any instances.
--
-- -   In Progress: The command has been sent to at least one instance but
--     has not reached a final state on all instances.
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
--     more invocations does not have a value of Success but not enough
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
command_statusDetails :: Lens.Lens' Command (Core.Maybe Core.Text)
command_statusDetails = Lens.lens (\Command' {statusDetails} -> statusDetails) (\s@Command' {} a -> s {statusDetails = a} :: Command)

-- | The number of targets for which the command invocation reached a
-- terminal state. Terminal states include the following: Success, Failed,
-- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
-- Undeliverable.
command_completedCount :: Lens.Lens' Command (Core.Maybe Core.Int)
command_completedCount = Lens.lens (\Command' {completedCount} -> completedCount) (\s@Command' {} a -> s {completedCount = a} :: Command)

-- | The S3 bucket where the responses to the command executions should be
-- stored. This was requested when issuing the command.
command_outputS3BucketName :: Lens.Lens' Command (Core.Maybe Core.Text)
command_outputS3BucketName = Lens.lens (\Command' {outputS3BucketName} -> outputS3BucketName) (\s@Command' {} a -> s {outputS3BucketName = a} :: Command)

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
command_comment :: Lens.Lens' Command (Core.Maybe Core.Text)
command_comment = Lens.lens (\Command' {comment} -> comment) (\s@Command' {} a -> s {comment = a} :: Command)

-- | The number of targets for which the status is Failed or Execution Timed
-- Out.
command_errorCount :: Lens.Lens' Command (Core.Maybe Core.Int)
command_errorCount = Lens.lens (\Command' {errorCount} -> errorCount) (\s@Command' {} a -> s {errorCount = a} :: Command)

-- | The name of the document requested for execution.
command_documentName :: Lens.Lens' Command (Core.Maybe Core.Text)
command_documentName = Lens.lens (\Command' {documentName} -> documentName) (\s@Command' {} a -> s {documentName = a} :: Command)

-- | A unique identifier for this command.
command_commandId :: Lens.Lens' Command (Core.Maybe Core.Text)
command_commandId = Lens.lens (\Command' {commandId} -> commandId) (\s@Command' {} a -> s {commandId = a} :: Command)

-- | An array of search criteria that targets instances using a Key,Value
-- combination that you specify. Targets is required if you don\'t provide
-- one or more instance IDs in the call.
command_targets :: Lens.Lens' Command (Core.Maybe [Target])
command_targets = Lens.lens (\Command' {targets} -> targets) (\s@Command' {} a -> s {targets = a} :: Command) Core.. Lens.mapping Lens._Coerce

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
command_outputS3Region :: Lens.Lens' Command (Core.Maybe Core.Text)
command_outputS3Region = Lens.lens (\Command' {outputS3Region} -> outputS3Region) (\s@Command' {} a -> s {outputS3Region = a} :: Command)

-- | The maximum number of instances that are allowed to run the command at
-- the same time. You can specify a number of instances, such as 10, or a
-- percentage of instances, such as 10%. The default value is 50. For more
-- information about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
command_maxConcurrency :: Lens.Lens' Command (Core.Maybe Core.Text)
command_maxConcurrency = Lens.lens (\Command' {maxConcurrency} -> maxConcurrency) (\s@Command' {} a -> s {maxConcurrency = a} :: Command)

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
command_outputS3KeyPrefix :: Lens.Lens' Command (Core.Maybe Core.Text)
command_outputS3KeyPrefix = Lens.lens (\Command' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@Command' {} a -> s {outputS3KeyPrefix = a} :: Command)

-- | The @TimeoutSeconds@ value specified for a command.
command_timeoutSeconds :: Lens.Lens' Command (Core.Maybe Core.Natural)
command_timeoutSeconds = Lens.lens (\Command' {timeoutSeconds} -> timeoutSeconds) (\s@Command' {} a -> s {timeoutSeconds = a} :: Command)

-- | The number of targets for which the status is Delivery Timed Out.
command_deliveryTimedOutCount :: Lens.Lens' Command (Core.Maybe Core.Int)
command_deliveryTimedOutCount = Lens.lens (\Command' {deliveryTimedOutCount} -> deliveryTimedOutCount) (\s@Command' {} a -> s {deliveryTimedOutCount = a} :: Command)

-- | CloudWatch Logs information where you want Systems Manager to send the
-- command output.
command_cloudWatchOutputConfig :: Lens.Lens' Command (Core.Maybe CloudWatchOutputConfig)
command_cloudWatchOutputConfig = Lens.lens (\Command' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@Command' {} a -> s {cloudWatchOutputConfig = a} :: Command)

-- | The SSM document version.
command_documentVersion :: Lens.Lens' Command (Core.Maybe Core.Text)
command_documentVersion = Lens.lens (\Command' {documentVersion} -> documentVersion) (\s@Command' {} a -> s {documentVersion = a} :: Command)

-- | The parameter values to be inserted in the document when running the
-- command.
command_parameters :: Lens.Lens' Command (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
command_parameters = Lens.lens (\Command' {parameters} -> parameters) (\s@Command' {} a -> s {parameters = a} :: Command) Core.. Lens.mapping Lens._Coerce

-- | The number of targets for the command.
command_targetCount :: Lens.Lens' Command (Core.Maybe Core.Int)
command_targetCount = Lens.lens (\Command' {targetCount} -> targetCount) (\s@Command' {} a -> s {targetCount = a} :: Command)

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject
      "Command"
      ( \x ->
          Command'
            Core.<$> (x Core..:? "NotificationConfig")
            Core.<*> (x Core..:? "InstanceIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MaxErrors")
            Core.<*> (x Core..:? "ExpiresAfter")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "ServiceRole")
            Core.<*> (x Core..:? "RequestedDateTime")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "CompletedCount")
            Core.<*> (x Core..:? "OutputS3BucketName")
            Core.<*> (x Core..:? "Comment")
            Core.<*> (x Core..:? "ErrorCount")
            Core.<*> (x Core..:? "DocumentName")
            Core.<*> (x Core..:? "CommandId")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "OutputS3Region")
            Core.<*> (x Core..:? "MaxConcurrency")
            Core.<*> (x Core..:? "OutputS3KeyPrefix")
            Core.<*> (x Core..:? "TimeoutSeconds")
            Core.<*> (x Core..:? "DeliveryTimedOutCount")
            Core.<*> (x Core..:? "CloudWatchOutputConfig")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TargetCount")
      )

instance Core.Hashable Command

instance Core.NFData Command
