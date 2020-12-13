{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Command
  ( Command (..),

    -- * Smart constructor
    mkCommand,

    -- * Lenses
    cStatus,
    cExpiresAfter,
    cNotificationConfig,
    cTargetCount,
    cCloudWatchOutputConfig,
    cDeliveryTimedOutCount,
    cOutputS3KeyPrefix,
    cDocumentName,
    cErrorCount,
    cStatusDetails,
    cMaxErrors,
    cInstanceIds,
    cOutputS3Region,
    cTargets,
    cCommandId,
    cParameters,
    cDocumentVersion,
    cTimeoutSeconds,
    cComment,
    cCompletedCount,
    cOutputS3BucketName,
    cMaxConcurrency,
    cRequestedDateTime,
    cServiceRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandStatus
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.Target

-- | Describes a command request.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { -- | The status of the command.
    status :: Lude.Maybe CommandStatus,
    -- | If this time is reached and the command has not already started running, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
    expiresAfter :: Lude.Maybe Lude.Timestamp,
    -- | Configurations for sending notifications about command status changes.
    notificationConfig :: Lude.Maybe NotificationConfig,
    -- | The number of targets for the command.
    targetCount :: Lude.Maybe Lude.Int,
    -- | CloudWatch Logs information where you want Systems Manager to send the command output.
    cloudWatchOutputConfig :: Lude.Maybe CloudWatchOutputConfig,
    -- | The number of targets for which the status is Delivery Timed Out.
    deliveryTimedOutCount :: Lude.Maybe Lude.Int,
    -- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
    outputS3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | The name of the document requested for execution.
    documentName :: Lude.Maybe Lude.Text,
    -- | The number of targets for which the status is Failed or Execution Timed Out.
    errorCount :: Lude.Maybe Lude.Int,
    -- | A detailed status of the command execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
    --
    --
    --     * Pending: The command has not been sent to any instances.
    --
    --
    --     * In Progress: The command has been sent to at least one instance but has not reached a final state on all instances.
    --
    --
    --     * Success: The command successfully ran on all invocations. This is a terminal state.
    --
    --
    --     * Delivery Timed Out: The value of MaxErrors or more command invocations shows a status of Delivery Timed Out. This is a terminal state.
    --
    --
    --     * Execution Timed Out: The value of MaxErrors or more command invocations shows a status of Execution Timed Out. This is a terminal state.
    --
    --
    --     * Failed: The value of MaxErrors or more command invocations shows a status of Failed. This is a terminal state.
    --
    --
    --     * Incomplete: The command was attempted on all instances and one or more invocations does not have a value of Success but not enough invocations failed for the status to be Failed. This is a terminal state.
    --
    --
    --     * Canceled: The command was terminated before it was completed. This is a terminal state.
    --
    --
    --     * Rate Exceeded: The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before running it on any instance. This is a terminal state.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
    maxErrors :: Lude.Maybe Lude.Text,
    -- | The instance IDs against which this command was requested.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
    outputS3Region :: Lude.Maybe Lude.Text,
    -- | An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
    targets :: Lude.Maybe [Target],
    -- | A unique identifier for this command.
    commandId :: Lude.Maybe Lude.Text,
    -- | The parameter values to be inserted in the document when running the command.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The SSM document version.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | The @TimeoutSeconds@ value specified for a command.
    timeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | User-specified information about the command, such as a brief description of what the command should do.
    comment :: Lude.Maybe Lude.Text,
    -- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
    completedCount :: Lude.Maybe Lude.Int,
    -- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
    outputS3BucketName :: Lude.Maybe Lude.Text,
    -- | The maximum number of instances that are allowed to run the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
    maxConcurrency :: Lude.Maybe Lude.Text,
    -- | The date and time the command was requested.
    requestedDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- * 'status' - The status of the command.
-- * 'expiresAfter' - If this time is reached and the command has not already started running, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
-- * 'notificationConfig' - Configurations for sending notifications about command status changes.
-- * 'targetCount' - The number of targets for the command.
-- * 'cloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the command output.
-- * 'deliveryTimedOutCount' - The number of targets for which the status is Delivery Timed Out.
-- * 'outputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
-- * 'documentName' - The name of the document requested for execution.
-- * 'errorCount' - The number of targets for which the status is Failed or Execution Timed Out.
-- * 'statusDetails' - A detailed status of the command execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to any instances.
--
--
--     * In Progress: The command has been sent to at least one instance but has not reached a final state on all instances.
--
--
--     * Success: The command successfully ran on all invocations. This is a terminal state.
--
--
--     * Delivery Timed Out: The value of MaxErrors or more command invocations shows a status of Delivery Timed Out. This is a terminal state.
--
--
--     * Execution Timed Out: The value of MaxErrors or more command invocations shows a status of Execution Timed Out. This is a terminal state.
--
--
--     * Failed: The value of MaxErrors or more command invocations shows a status of Failed. This is a terminal state.
--
--
--     * Incomplete: The command was attempted on all instances and one or more invocations does not have a value of Success but not enough invocations failed for the status to be Failed. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Rate Exceeded: The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before running it on any instance. This is a terminal state.
--
--
-- * 'maxErrors' - The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
-- * 'instanceIds' - The instance IDs against which this command was requested.
-- * 'outputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
-- * 'targets' - An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
-- * 'commandId' - A unique identifier for this command.
-- * 'parameters' - The parameter values to be inserted in the document when running the command.
-- * 'documentVersion' - The SSM document version.
-- * 'timeoutSeconds' - The @TimeoutSeconds@ value specified for a command.
-- * 'comment' - User-specified information about the command, such as a brief description of what the command should do.
-- * 'completedCount' - The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
-- * 'outputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
-- * 'maxConcurrency' - The maximum number of instances that are allowed to run the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
-- * 'requestedDateTime' - The date and time the command was requested.
-- * 'serviceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
mkCommand ::
  Command
mkCommand =
  Command'
    { status = Lude.Nothing,
      expiresAfter = Lude.Nothing,
      notificationConfig = Lude.Nothing,
      targetCount = Lude.Nothing,
      cloudWatchOutputConfig = Lude.Nothing,
      deliveryTimedOutCount = Lude.Nothing,
      outputS3KeyPrefix = Lude.Nothing,
      documentName = Lude.Nothing,
      errorCount = Lude.Nothing,
      statusDetails = Lude.Nothing,
      maxErrors = Lude.Nothing,
      instanceIds = Lude.Nothing,
      outputS3Region = Lude.Nothing,
      targets = Lude.Nothing,
      commandId = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      timeoutSeconds = Lude.Nothing,
      comment = Lude.Nothing,
      completedCount = Lude.Nothing,
      outputS3BucketName = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      requestedDateTime = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The status of the command.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Command (Lude.Maybe CommandStatus)
cStatus = Lens.lens (status :: Command -> Lude.Maybe CommandStatus) (\s a -> s {status = a} :: Command)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If this time is reached and the command has not already started running, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
--
-- /Note:/ Consider using 'expiresAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiresAfter :: Lens.Lens' Command (Lude.Maybe Lude.Timestamp)
cExpiresAfter = Lens.lens (expiresAfter :: Command -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiresAfter = a} :: Command)
{-# DEPRECATED cExpiresAfter "Use generic-lens or generic-optics with 'expiresAfter' instead." #-}

-- | Configurations for sending notifications about command status changes.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotificationConfig :: Lens.Lens' Command (Lude.Maybe NotificationConfig)
cNotificationConfig = Lens.lens (notificationConfig :: Command -> Lude.Maybe NotificationConfig) (\s a -> s {notificationConfig = a} :: Command)
{-# DEPRECATED cNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | The number of targets for the command.
--
-- /Note:/ Consider using 'targetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetCount :: Lens.Lens' Command (Lude.Maybe Lude.Int)
cTargetCount = Lens.lens (targetCount :: Command -> Lude.Maybe Lude.Int) (\s a -> s {targetCount = a} :: Command)
{-# DEPRECATED cTargetCount "Use generic-lens or generic-optics with 'targetCount' instead." #-}

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCloudWatchOutputConfig :: Lens.Lens' Command (Lude.Maybe CloudWatchOutputConfig)
cCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: Command -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: Command)
{-# DEPRECATED cCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The number of targets for which the status is Delivery Timed Out.
--
-- /Note:/ Consider using 'deliveryTimedOutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeliveryTimedOutCount :: Lens.Lens' Command (Lude.Maybe Lude.Int)
cDeliveryTimedOutCount = Lens.lens (deliveryTimedOutCount :: Command -> Lude.Maybe Lude.Int) (\s a -> s {deliveryTimedOutCount = a} :: Command)
{-# DEPRECATED cDeliveryTimedOutCount "Use generic-lens or generic-optics with 'deliveryTimedOutCount' instead." #-}

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3KeyPrefix :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: Command -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: Command)
{-# DEPRECATED cOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | The name of the document requested for execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDocumentName :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cDocumentName = Lens.lens (documentName :: Command -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: Command)
{-# DEPRECATED cDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The number of targets for which the status is Failed or Execution Timed Out.
--
-- /Note:/ Consider using 'errorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cErrorCount :: Lens.Lens' Command (Lude.Maybe Lude.Int)
cErrorCount = Lens.lens (errorCount :: Command -> Lude.Maybe Lude.Int) (\s a -> s {errorCount = a} :: Command)
{-# DEPRECATED cErrorCount "Use generic-lens or generic-optics with 'errorCount' instead." #-}

-- | A detailed status of the command execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to any instances.
--
--
--     * In Progress: The command has been sent to at least one instance but has not reached a final state on all instances.
--
--
--     * Success: The command successfully ran on all invocations. This is a terminal state.
--
--
--     * Delivery Timed Out: The value of MaxErrors or more command invocations shows a status of Delivery Timed Out. This is a terminal state.
--
--
--     * Execution Timed Out: The value of MaxErrors or more command invocations shows a status of Execution Timed Out. This is a terminal state.
--
--
--     * Failed: The value of MaxErrors or more command invocations shows a status of Failed. This is a terminal state.
--
--
--     * Incomplete: The command was attempted on all instances and one or more invocations does not have a value of Success but not enough invocations failed for the status to be Failed. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Rate Exceeded: The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before running it on any instance. This is a terminal state.
--
--
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatusDetails :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cStatusDetails = Lens.lens (statusDetails :: Command -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: Command)
{-# DEPRECATED cStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxErrors :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cMaxErrors = Lens.lens (maxErrors :: Command -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: Command)
{-# DEPRECATED cMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The instance IDs against which this command was requested.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceIds :: Lens.Lens' Command (Lude.Maybe [Lude.Text])
cInstanceIds = Lens.lens (instanceIds :: Command -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: Command)
{-# DEPRECATED cInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3Region :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cOutputS3Region = Lens.lens (outputS3Region :: Command -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Region = a} :: Command)
{-# DEPRECATED cOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargets :: Lens.Lens' Command (Lude.Maybe [Target])
cTargets = Lens.lens (targets :: Command -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: Command)
{-# DEPRECATED cTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A unique identifier for this command.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommandId :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cCommandId = Lens.lens (commandId :: Command -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: Command)
{-# DEPRECATED cCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The parameter values to be inserted in the document when running the command.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameters :: Lens.Lens' Command (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
cParameters = Lens.lens (parameters :: Command -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: Command)
{-# DEPRECATED cParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The SSM document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDocumentVersion :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cDocumentVersion = Lens.lens (documentVersion :: Command -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: Command)
{-# DEPRECATED cDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The @TimeoutSeconds@ value specified for a command.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeoutSeconds :: Lens.Lens' Command (Lude.Maybe Lude.Natural)
cTimeoutSeconds = Lens.lens (timeoutSeconds :: Command -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutSeconds = a} :: Command)
{-# DEPRECATED cTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cComment = Lens.lens (comment :: Command -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: Command)
{-# DEPRECATED cComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
--
-- /Note:/ Consider using 'completedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedCount :: Lens.Lens' Command (Lude.Maybe Lude.Int)
cCompletedCount = Lens.lens (completedCount :: Command -> Lude.Maybe Lude.Int) (\s a -> s {completedCount = a} :: Command)
{-# DEPRECATED cCompletedCount "Use generic-lens or generic-optics with 'completedCount' instead." #-}

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3BucketName :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cOutputS3BucketName = Lens.lens (outputS3BucketName :: Command -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: Command)
{-# DEPRECATED cOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The maximum number of instances that are allowed to run the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxConcurrency :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cMaxConcurrency = Lens.lens (maxConcurrency :: Command -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: Command)
{-# DEPRECATED cMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The date and time the command was requested.
--
-- /Note:/ Consider using 'requestedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRequestedDateTime :: Lens.Lens' Command (Lude.Maybe Lude.Timestamp)
cRequestedDateTime = Lens.lens (requestedDateTime :: Command -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedDateTime = a} :: Command)
{-# DEPRECATED cRequestedDateTime "Use generic-lens or generic-optics with 'requestedDateTime' instead." #-}

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cServiceRole :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cServiceRole = Lens.lens (serviceRole :: Command -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Command)
{-# DEPRECATED cServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON Command where
  parseJSON =
    Lude.withObject
      "Command"
      ( \x ->
          Command'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ExpiresAfter")
            Lude.<*> (x Lude..:? "NotificationConfig")
            Lude.<*> (x Lude..:? "TargetCount")
            Lude.<*> (x Lude..:? "CloudWatchOutputConfig")
            Lude.<*> (x Lude..:? "DeliveryTimedOutCount")
            Lude.<*> (x Lude..:? "OutputS3KeyPrefix")
            Lude.<*> (x Lude..:? "DocumentName")
            Lude.<*> (x Lude..:? "ErrorCount")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "InstanceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OutputS3Region")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CommandId")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "TimeoutSeconds")
            Lude.<*> (x Lude..:? "Comment")
            Lude.<*> (x Lude..:? "CompletedCount")
            Lude.<*> (x Lude..:? "OutputS3BucketName")
            Lude.<*> (x Lude..:? "MaxConcurrency")
            Lude.<*> (x Lude..:? "RequestedDateTime")
            Lude.<*> (x Lude..:? "ServiceRole")
      )
