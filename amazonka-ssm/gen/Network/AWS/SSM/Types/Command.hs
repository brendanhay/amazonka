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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandStatus
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.Target

-- | Describes a command request.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | Configurations for sending notifications about command status changes.
    notificationConfig :: Prelude.Maybe NotificationConfig,
    -- | The instance IDs against which this command was requested.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of errors allowed before the system stops sending the
    -- command to additional targets. You can specify a number of errors, such
    -- as 10, or a percentage or errors, such as 10%. The default value is 0.
    -- For more information about how to use MaxErrors, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /AWS Systems Manager User Guide/.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | If this time is reached and the command has not already started running,
    -- it will not run. Calculated based on the ExpiresAfter user input
    -- provided as part of the SendCommand API.
    expiresAfter :: Prelude.Maybe Core.POSIX,
    -- | The status of the command.
    status :: Prelude.Maybe CommandStatus,
    -- | The IAM service role that Run Command uses to act on your behalf when
    -- sending notifications about command status changes.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The date and time the command was requested.
    requestedDateTime :: Prelude.Maybe Core.POSIX,
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
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The number of targets for which the command invocation reached a
    -- terminal state. Terminal states include the following: Success, Failed,
    -- Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or
    -- Undeliverable.
    completedCount :: Prelude.Maybe Prelude.Int,
    -- | The S3 bucket where the responses to the command executions should be
    -- stored. This was requested when issuing the command.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | User-specified information about the command, such as a brief
    -- description of what the command should do.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The number of targets for which the status is Failed or Execution Timed
    -- Out.
    errorCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the document requested for execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this command.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | An array of search criteria that targets instances using a Key,Value
    -- combination that you specify. Targets is required if you don\'t provide
    -- one or more instance IDs in the call.
    targets :: Prelude.Maybe [Target],
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances that are allowed to run the command at
    -- the same time. You can specify a number of instances, such as 10, or a
    -- percentage of instances, such as 10%. The default value is 50. For more
    -- information about how to use MaxConcurrency, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
    -- in the /AWS Systems Manager User Guide/.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The S3 directory path inside the bucket where the responses to the
    -- command executions should be stored. This was requested when issuing the
    -- command.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The @TimeoutSeconds@ value specified for a command.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of targets for which the status is Delivery Timed Out.
    deliveryTimedOutCount :: Prelude.Maybe Prelude.Int,
    -- | CloudWatch Logs information where you want Systems Manager to send the
    -- command output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | The SSM document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The parameter values to be inserted in the document when running the
    -- command.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The number of targets for the command.
    targetCount :: Prelude.Maybe Prelude.Int
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
    { notificationConfig = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      expiresAfter = Prelude.Nothing,
      status = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      requestedDateTime = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      completedCount = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      comment = Prelude.Nothing,
      errorCount = Prelude.Nothing,
      documentName = Prelude.Nothing,
      commandId = Prelude.Nothing,
      targets = Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      deliveryTimedOutCount = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing,
      targetCount = Prelude.Nothing
    }

-- | Configurations for sending notifications about command status changes.
command_notificationConfig :: Lens.Lens' Command (Prelude.Maybe NotificationConfig)
command_notificationConfig = Lens.lens (\Command' {notificationConfig} -> notificationConfig) (\s@Command' {} a -> s {notificationConfig = a} :: Command)

-- | The instance IDs against which this command was requested.
command_instanceIds :: Lens.Lens' Command (Prelude.Maybe [Prelude.Text])
command_instanceIds = Lens.lens (\Command' {instanceIds} -> instanceIds) (\s@Command' {} a -> s {instanceIds = a} :: Command) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of errors allowed before the system stops sending the
-- command to additional targets. You can specify a number of errors, such
-- as 10, or a percentage or errors, such as 10%. The default value is 0.
-- For more information about how to use MaxErrors, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
command_maxErrors :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_maxErrors = Lens.lens (\Command' {maxErrors} -> maxErrors) (\s@Command' {} a -> s {maxErrors = a} :: Command)

-- | If this time is reached and the command has not already started running,
-- it will not run. Calculated based on the ExpiresAfter user input
-- provided as part of the SendCommand API.
command_expiresAfter :: Lens.Lens' Command (Prelude.Maybe Prelude.UTCTime)
command_expiresAfter = Lens.lens (\Command' {expiresAfter} -> expiresAfter) (\s@Command' {} a -> s {expiresAfter = a} :: Command) Prelude.. Lens.mapping Core._Time

-- | The status of the command.
command_status :: Lens.Lens' Command (Prelude.Maybe CommandStatus)
command_status = Lens.lens (\Command' {status} -> status) (\s@Command' {} a -> s {status = a} :: Command)

-- | The IAM service role that Run Command uses to act on your behalf when
-- sending notifications about command status changes.
command_serviceRole :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_serviceRole = Lens.lens (\Command' {serviceRole} -> serviceRole) (\s@Command' {} a -> s {serviceRole = a} :: Command)

-- | The date and time the command was requested.
command_requestedDateTime :: Lens.Lens' Command (Prelude.Maybe Prelude.UTCTime)
command_requestedDateTime = Lens.lens (\Command' {requestedDateTime} -> requestedDateTime) (\s@Command' {} a -> s {requestedDateTime = a} :: Command) Prelude.. Lens.mapping Core._Time

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
command_statusDetails :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_statusDetails = Lens.lens (\Command' {statusDetails} -> statusDetails) (\s@Command' {} a -> s {statusDetails = a} :: Command)

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

-- | User-specified information about the command, such as a brief
-- description of what the command should do.
command_comment :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_comment = Lens.lens (\Command' {comment} -> comment) (\s@Command' {} a -> s {comment = a} :: Command)

-- | The number of targets for which the status is Failed or Execution Timed
-- Out.
command_errorCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_errorCount = Lens.lens (\Command' {errorCount} -> errorCount) (\s@Command' {} a -> s {errorCount = a} :: Command)

-- | The name of the document requested for execution.
command_documentName :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_documentName = Lens.lens (\Command' {documentName} -> documentName) (\s@Command' {} a -> s {documentName = a} :: Command)

-- | A unique identifier for this command.
command_commandId :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_commandId = Lens.lens (\Command' {commandId} -> commandId) (\s@Command' {} a -> s {commandId = a} :: Command)

-- | An array of search criteria that targets instances using a Key,Value
-- combination that you specify. Targets is required if you don\'t provide
-- one or more instance IDs in the call.
command_targets :: Lens.Lens' Command (Prelude.Maybe [Target])
command_targets = Lens.lens (\Command' {targets} -> targets) (\s@Command' {} a -> s {targets = a} :: Command) Prelude.. Lens.mapping Lens._Coerce

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
command_outputS3Region :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_outputS3Region = Lens.lens (\Command' {outputS3Region} -> outputS3Region) (\s@Command' {} a -> s {outputS3Region = a} :: Command)

-- | The maximum number of instances that are allowed to run the command at
-- the same time. You can specify a number of instances, such as 10, or a
-- percentage of instances, such as 10%. The default value is 50. For more
-- information about how to use MaxConcurrency, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command>
-- in the /AWS Systems Manager User Guide/.
command_maxConcurrency :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_maxConcurrency = Lens.lens (\Command' {maxConcurrency} -> maxConcurrency) (\s@Command' {} a -> s {maxConcurrency = a} :: Command)

-- | The S3 directory path inside the bucket where the responses to the
-- command executions should be stored. This was requested when issuing the
-- command.
command_outputS3KeyPrefix :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_outputS3KeyPrefix = Lens.lens (\Command' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@Command' {} a -> s {outputS3KeyPrefix = a} :: Command)

-- | The @TimeoutSeconds@ value specified for a command.
command_timeoutSeconds :: Lens.Lens' Command (Prelude.Maybe Prelude.Natural)
command_timeoutSeconds = Lens.lens (\Command' {timeoutSeconds} -> timeoutSeconds) (\s@Command' {} a -> s {timeoutSeconds = a} :: Command)

-- | The number of targets for which the status is Delivery Timed Out.
command_deliveryTimedOutCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_deliveryTimedOutCount = Lens.lens (\Command' {deliveryTimedOutCount} -> deliveryTimedOutCount) (\s@Command' {} a -> s {deliveryTimedOutCount = a} :: Command)

-- | CloudWatch Logs information where you want Systems Manager to send the
-- command output.
command_cloudWatchOutputConfig :: Lens.Lens' Command (Prelude.Maybe CloudWatchOutputConfig)
command_cloudWatchOutputConfig = Lens.lens (\Command' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@Command' {} a -> s {cloudWatchOutputConfig = a} :: Command)

-- | The SSM document version.
command_documentVersion :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_documentVersion = Lens.lens (\Command' {documentVersion} -> documentVersion) (\s@Command' {} a -> s {documentVersion = a} :: Command)

-- | The parameter values to be inserted in the document when running the
-- command.
command_parameters :: Lens.Lens' Command (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
command_parameters = Lens.lens (\Command' {parameters} -> parameters) (\s@Command' {} a -> s {parameters = a} :: Command) Prelude.. Lens.mapping Lens._Coerce

-- | The number of targets for the command.
command_targetCount :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_targetCount = Lens.lens (\Command' {targetCount} -> targetCount) (\s@Command' {} a -> s {targetCount = a} :: Command)

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject
      "Command"
      ( \x ->
          Command'
            Prelude.<$> (x Core..:? "NotificationConfig")
            Prelude.<*> (x Core..:? "InstanceIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "ExpiresAfter")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ServiceRole")
            Prelude.<*> (x Core..:? "RequestedDateTime")
            Prelude.<*> (x Core..:? "StatusDetails")
            Prelude.<*> (x Core..:? "CompletedCount")
            Prelude.<*> (x Core..:? "OutputS3BucketName")
            Prelude.<*> (x Core..:? "Comment")
            Prelude.<*> (x Core..:? "ErrorCount")
            Prelude.<*> (x Core..:? "DocumentName")
            Prelude.<*> (x Core..:? "CommandId")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputS3Region")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "OutputS3KeyPrefix")
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "DeliveryTimedOutCount")
            Prelude.<*> (x Core..:? "CloudWatchOutputConfig")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TargetCount")
      )

instance Prelude.Hashable Command

instance Prelude.NFData Command
