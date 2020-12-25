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
    cCloudWatchOutputConfig,
    cCommandId,
    cComment,
    cCompletedCount,
    cDeliveryTimedOutCount,
    cDocumentName,
    cDocumentVersion,
    cErrorCount,
    cExpiresAfter,
    cInstanceIds,
    cMaxConcurrency,
    cMaxErrors,
    cNotificationConfig,
    cOutputS3BucketName,
    cOutputS3KeyPrefix,
    cOutputS3Region,
    cParameters,
    cRequestedDateTime,
    cServiceRole,
    cStatus,
    cStatusDetails,
    cTargetCount,
    cTargets,
    cTimeoutSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CloudWatchOutputConfig as Types
import qualified Network.AWS.SSM.Types.CommandId as Types
import qualified Network.AWS.SSM.Types.CommandStatus as Types
import qualified Network.AWS.SSM.Types.Comment as Types
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.MaxConcurrency as Types
import qualified Network.AWS.SSM.Types.MaxErrors as Types
import qualified Network.AWS.SSM.Types.NotificationConfig as Types
import qualified Network.AWS.SSM.Types.OutputS3BucketName as Types
import qualified Network.AWS.SSM.Types.OutputS3KeyPrefix as Types
import qualified Network.AWS.SSM.Types.OutputS3Region as Types
import qualified Network.AWS.SSM.Types.ParameterName as Types
import qualified Network.AWS.SSM.Types.ParameterValue as Types
import qualified Network.AWS.SSM.Types.ServiceRole as Types
import qualified Network.AWS.SSM.Types.StatusDetails as Types
import qualified Network.AWS.SSM.Types.Target as Types

-- | Describes a command request.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { -- | CloudWatch Logs information where you want Systems Manager to send the command output.
    cloudWatchOutputConfig :: Core.Maybe Types.CloudWatchOutputConfig,
    -- | A unique identifier for this command.
    commandId :: Core.Maybe Types.CommandId,
    -- | User-specified information about the command, such as a brief description of what the command should do.
    comment :: Core.Maybe Types.Comment,
    -- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
    completedCount :: Core.Maybe Core.Int,
    -- | The number of targets for which the status is Delivery Timed Out.
    deliveryTimedOutCount :: Core.Maybe Core.Int,
    -- | The name of the document requested for execution.
    documentName :: Core.Maybe Types.DocumentName,
    -- | The SSM document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The number of targets for which the status is Failed or Execution Timed Out.
    errorCount :: Core.Maybe Core.Int,
    -- | If this time is reached and the command has not already started running, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
    expiresAfter :: Core.Maybe Core.NominalDiffTime,
    -- | The instance IDs against which this command was requested.
    instanceIds :: Core.Maybe [Types.InstanceId],
    -- | The maximum number of instances that are allowed to run the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
    maxConcurrency :: Core.Maybe Types.MaxConcurrency,
    -- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
    maxErrors :: Core.Maybe Types.MaxErrors,
    -- | Configurations for sending notifications about command status changes.
    notificationConfig :: Core.Maybe Types.NotificationConfig,
    -- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
    outputS3BucketName :: Core.Maybe Types.OutputS3BucketName,
    -- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
    outputS3KeyPrefix :: Core.Maybe Types.OutputS3KeyPrefix,
    -- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
    outputS3Region :: Core.Maybe Types.OutputS3Region,
    -- | The parameter values to be inserted in the document when running the command.
    parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]),
    -- | The date and time the command was requested.
    requestedDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
    serviceRole :: Core.Maybe Types.ServiceRole,
    -- | The status of the command.
    status :: Core.Maybe Types.CommandStatus,
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
    statusDetails :: Core.Maybe Types.StatusDetails,
    -- | The number of targets for the command.
    targetCount :: Core.Maybe Core.Int,
    -- | An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
    targets :: Core.Maybe [Types.Target],
    -- | The @TimeoutSeconds@ value specified for a command.
    timeoutSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Command' value with any optional fields omitted.
mkCommand ::
  Command
mkCommand =
  Command'
    { cloudWatchOutputConfig = Core.Nothing,
      commandId = Core.Nothing,
      comment = Core.Nothing,
      completedCount = Core.Nothing,
      deliveryTimedOutCount = Core.Nothing,
      documentName = Core.Nothing,
      documentVersion = Core.Nothing,
      errorCount = Core.Nothing,
      expiresAfter = Core.Nothing,
      instanceIds = Core.Nothing,
      maxConcurrency = Core.Nothing,
      maxErrors = Core.Nothing,
      notificationConfig = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing,
      outputS3Region = Core.Nothing,
      parameters = Core.Nothing,
      requestedDateTime = Core.Nothing,
      serviceRole = Core.Nothing,
      status = Core.Nothing,
      statusDetails = Core.Nothing,
      targetCount = Core.Nothing,
      targets = Core.Nothing,
      timeoutSeconds = Core.Nothing
    }

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCloudWatchOutputConfig :: Lens.Lens' Command (Core.Maybe Types.CloudWatchOutputConfig)
cCloudWatchOutputConfig = Lens.field @"cloudWatchOutputConfig"
{-# DEPRECATED cCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | A unique identifier for this command.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommandId :: Lens.Lens' Command (Core.Maybe Types.CommandId)
cCommandId = Lens.field @"commandId"
{-# DEPRECATED cCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' Command (Core.Maybe Types.Comment)
cComment = Lens.field @"comment"
{-# DEPRECATED cComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
--
-- /Note:/ Consider using 'completedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompletedCount :: Lens.Lens' Command (Core.Maybe Core.Int)
cCompletedCount = Lens.field @"completedCount"
{-# DEPRECATED cCompletedCount "Use generic-lens or generic-optics with 'completedCount' instead." #-}

-- | The number of targets for which the status is Delivery Timed Out.
--
-- /Note:/ Consider using 'deliveryTimedOutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeliveryTimedOutCount :: Lens.Lens' Command (Core.Maybe Core.Int)
cDeliveryTimedOutCount = Lens.field @"deliveryTimedOutCount"
{-# DEPRECATED cDeliveryTimedOutCount "Use generic-lens or generic-optics with 'deliveryTimedOutCount' instead." #-}

-- | The name of the document requested for execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDocumentName :: Lens.Lens' Command (Core.Maybe Types.DocumentName)
cDocumentName = Lens.field @"documentName"
{-# DEPRECATED cDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The SSM document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDocumentVersion :: Lens.Lens' Command (Core.Maybe Types.DocumentVersion)
cDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED cDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The number of targets for which the status is Failed or Execution Timed Out.
--
-- /Note:/ Consider using 'errorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cErrorCount :: Lens.Lens' Command (Core.Maybe Core.Int)
cErrorCount = Lens.field @"errorCount"
{-# DEPRECATED cErrorCount "Use generic-lens or generic-optics with 'errorCount' instead." #-}

-- | If this time is reached and the command has not already started running, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
--
-- /Note:/ Consider using 'expiresAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiresAfter :: Lens.Lens' Command (Core.Maybe Core.NominalDiffTime)
cExpiresAfter = Lens.field @"expiresAfter"
{-# DEPRECATED cExpiresAfter "Use generic-lens or generic-optics with 'expiresAfter' instead." #-}

-- | The instance IDs against which this command was requested.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceIds :: Lens.Lens' Command (Core.Maybe [Types.InstanceId])
cInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED cInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of instances that are allowed to run the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxConcurrency :: Lens.Lens' Command (Core.Maybe Types.MaxConcurrency)
cMaxConcurrency = Lens.field @"maxConcurrency"
{-# DEPRECATED cMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Running commands using Systems Manager Run Command> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxErrors :: Lens.Lens' Command (Core.Maybe Types.MaxErrors)
cMaxErrors = Lens.field @"maxErrors"
{-# DEPRECATED cMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | Configurations for sending notifications about command status changes.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNotificationConfig :: Lens.Lens' Command (Core.Maybe Types.NotificationConfig)
cNotificationConfig = Lens.field @"notificationConfig"
{-# DEPRECATED cNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3BucketName :: Lens.Lens' Command (Core.Maybe Types.OutputS3BucketName)
cOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED cOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3KeyPrefix :: Lens.Lens' Command (Core.Maybe Types.OutputS3KeyPrefix)
cOutputS3KeyPrefix = Lens.field @"outputS3KeyPrefix"
{-# DEPRECATED cOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOutputS3Region :: Lens.Lens' Command (Core.Maybe Types.OutputS3Region)
cOutputS3Region = Lens.field @"outputS3Region"
{-# DEPRECATED cOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | The parameter values to be inserted in the document when running the command.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameters :: Lens.Lens' Command (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
cParameters = Lens.field @"parameters"
{-# DEPRECATED cParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The date and time the command was requested.
--
-- /Note:/ Consider using 'requestedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRequestedDateTime :: Lens.Lens' Command (Core.Maybe Core.NominalDiffTime)
cRequestedDateTime = Lens.field @"requestedDateTime"
{-# DEPRECATED cRequestedDateTime "Use generic-lens or generic-optics with 'requestedDateTime' instead." #-}

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cServiceRole :: Lens.Lens' Command (Core.Maybe Types.ServiceRole)
cServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED cServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The status of the command.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Command (Core.Maybe Types.CommandStatus)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
cStatusDetails :: Lens.Lens' Command (Core.Maybe Types.StatusDetails)
cStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED cStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The number of targets for the command.
--
-- /Note:/ Consider using 'targetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetCount :: Lens.Lens' Command (Core.Maybe Core.Int)
cTargetCount = Lens.field @"targetCount"
{-# DEPRECATED cTargetCount "Use generic-lens or generic-optics with 'targetCount' instead." #-}

-- | An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargets :: Lens.Lens' Command (Core.Maybe [Types.Target])
cTargets = Lens.field @"targets"
{-# DEPRECATED cTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The @TimeoutSeconds@ value specified for a command.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeoutSeconds :: Lens.Lens' Command (Core.Maybe Core.Natural)
cTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# DEPRECATED cTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject "Command" Core.$
      \x ->
        Command'
          Core.<$> (x Core..:? "CloudWatchOutputConfig")
          Core.<*> (x Core..:? "CommandId")
          Core.<*> (x Core..:? "Comment")
          Core.<*> (x Core..:? "CompletedCount")
          Core.<*> (x Core..:? "DeliveryTimedOutCount")
          Core.<*> (x Core..:? "DocumentName")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "ErrorCount")
          Core.<*> (x Core..:? "ExpiresAfter")
          Core.<*> (x Core..:? "InstanceIds")
          Core.<*> (x Core..:? "MaxConcurrency")
          Core.<*> (x Core..:? "MaxErrors")
          Core.<*> (x Core..:? "NotificationConfig")
          Core.<*> (x Core..:? "OutputS3BucketName")
          Core.<*> (x Core..:? "OutputS3KeyPrefix")
          Core.<*> (x Core..:? "OutputS3Region")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "RequestedDateTime")
          Core.<*> (x Core..:? "ServiceRole")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetails")
          Core.<*> (x Core..:? "TargetCount")
          Core.<*> (x Core..:? "Targets")
          Core.<*> (x Core..:? "TimeoutSeconds")
