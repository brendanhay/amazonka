{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandInvocation
  ( CommandInvocation (..),

    -- * Smart constructor
    mkCommandInvocation,

    -- * Lenses
    ciCloudWatchOutputConfig,
    ciCommandId,
    ciCommandPlugins,
    ciComment,
    ciDocumentName,
    ciDocumentVersion,
    ciInstanceId,
    ciInstanceName,
    ciNotificationConfig,
    ciRequestedDateTime,
    ciServiceRole,
    ciStandardErrorUrl,
    ciStandardOutputUrl,
    ciStatus,
    ciStatusDetails,
    ciTraceOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CloudWatchOutputConfig as Types
import qualified Network.AWS.SSM.Types.CommandId as Types
import qualified Network.AWS.SSM.Types.CommandInvocationStatus as Types
import qualified Network.AWS.SSM.Types.CommandPlugin as Types
import qualified Network.AWS.SSM.Types.Comment as Types
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.InstanceTagName as Types
import qualified Network.AWS.SSM.Types.InvocationTraceOutput as Types
import qualified Network.AWS.SSM.Types.NotificationConfig as Types
import qualified Network.AWS.SSM.Types.ServiceRole as Types
import qualified Network.AWS.SSM.Types.StatusDetails as Types
import qualified Network.AWS.SSM.Types.Url as Types

-- | An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user runs SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you ran.
--
-- /See:/ 'mkCommandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { -- | CloudWatch Logs information where you want Systems Manager to send the command output.
    cloudWatchOutputConfig :: Core.Maybe Types.CloudWatchOutputConfig,
    -- | The command against which this invocation was requested.
    commandId :: Core.Maybe Types.CommandId,
    commandPlugins :: Core.Maybe [Types.CommandPlugin],
    -- | User-specified information about the command, such as a brief description of what the command should do.
    comment :: Core.Maybe Types.Comment,
    -- | The document name that was requested for execution.
    documentName :: Core.Maybe Types.DocumentName,
    -- | The SSM document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The instance ID in which this invocation was requested.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
    instanceName :: Core.Maybe Types.InstanceTagName,
    -- | Configurations for sending notifications about command status changes on a per instance basis.
    notificationConfig :: Core.Maybe Types.NotificationConfig,
    -- | The time and date the request was sent to this instance.
    requestedDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
    serviceRole :: Core.Maybe Types.ServiceRole,
    -- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
    standardErrorUrl :: Core.Maybe Types.Url,
    -- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
    standardOutputUrl :: Core.Maybe Types.Url,
    -- | Whether or not the invocation succeeded, failed, or is pending.
    status :: Core.Maybe Types.CommandInvocationStatus,
    -- | A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
    --
    --
    --     * Pending: The command has not been sent to the instance.
    --
    --
    --     * In Progress: The command has been sent to the instance but has not reached a terminal state.
    --
    --
    --     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.
    --
    --
    --     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
    --
    --
    --     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
    --
    --
    --     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
    --
    --
    --     * Canceled: The command was terminated before it was completed. This is a terminal state.
    --
    --
    --     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
    --
    --
    --     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
    statusDetails :: Core.Maybe Types.StatusDetails,
    -- | Gets the trace output sent by the agent.
    traceOutput :: Core.Maybe Types.InvocationTraceOutput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CommandInvocation' value with any optional fields omitted.
mkCommandInvocation ::
  CommandInvocation
mkCommandInvocation =
  CommandInvocation'
    { cloudWatchOutputConfig = Core.Nothing,
      commandId = Core.Nothing,
      commandPlugins = Core.Nothing,
      comment = Core.Nothing,
      documentName = Core.Nothing,
      documentVersion = Core.Nothing,
      instanceId = Core.Nothing,
      instanceName = Core.Nothing,
      notificationConfig = Core.Nothing,
      requestedDateTime = Core.Nothing,
      serviceRole = Core.Nothing,
      standardErrorUrl = Core.Nothing,
      standardOutputUrl = Core.Nothing,
      status = Core.Nothing,
      statusDetails = Core.Nothing,
      traceOutput = Core.Nothing
    }

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCloudWatchOutputConfig :: Lens.Lens' CommandInvocation (Core.Maybe Types.CloudWatchOutputConfig)
ciCloudWatchOutputConfig = Lens.field @"cloudWatchOutputConfig"
{-# DEPRECATED ciCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The command against which this invocation was requested.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCommandId :: Lens.Lens' CommandInvocation (Core.Maybe Types.CommandId)
ciCommandId = Lens.field @"commandId"
{-# DEPRECATED ciCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'commandPlugins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCommandPlugins :: Lens.Lens' CommandInvocation (Core.Maybe [Types.CommandPlugin])
ciCommandPlugins = Lens.field @"commandPlugins"
{-# DEPRECATED ciCommandPlugins "Use generic-lens or generic-optics with 'commandPlugins' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComment :: Lens.Lens' CommandInvocation (Core.Maybe Types.Comment)
ciComment = Lens.field @"comment"
{-# DEPRECATED ciComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The document name that was requested for execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDocumentName :: Lens.Lens' CommandInvocation (Core.Maybe Types.DocumentName)
ciDocumentName = Lens.field @"documentName"
{-# DEPRECATED ciDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The SSM document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDocumentVersion :: Lens.Lens' CommandInvocation (Core.Maybe Types.DocumentVersion)
ciDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED ciDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The instance ID in which this invocation was requested.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceId :: Lens.Lens' CommandInvocation (Core.Maybe Types.InstanceId)
ciInstanceId = Lens.field @"instanceId"
{-# DEPRECATED ciInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceName :: Lens.Lens' CommandInvocation (Core.Maybe Types.InstanceTagName)
ciInstanceName = Lens.field @"instanceName"
{-# DEPRECATED ciInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | Configurations for sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciNotificationConfig :: Lens.Lens' CommandInvocation (Core.Maybe Types.NotificationConfig)
ciNotificationConfig = Lens.field @"notificationConfig"
{-# DEPRECATED ciNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | The time and date the request was sent to this instance.
--
-- /Note:/ Consider using 'requestedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRequestedDateTime :: Lens.Lens' CommandInvocation (Core.Maybe Core.NominalDiffTime)
ciRequestedDateTime = Lens.field @"requestedDateTime"
{-# DEPRECATED ciRequestedDateTime "Use generic-lens or generic-optics with 'requestedDateTime' instead." #-}

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciServiceRole :: Lens.Lens' CommandInvocation (Core.Maybe Types.ServiceRole)
ciServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED ciServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardErrorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStandardErrorUrl :: Lens.Lens' CommandInvocation (Core.Maybe Types.Url)
ciStandardErrorUrl = Lens.field @"standardErrorUrl"
{-# DEPRECATED ciStandardErrorUrl "Use generic-lens or generic-optics with 'standardErrorUrl' instead." #-}

-- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardOutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStandardOutputUrl :: Lens.Lens' CommandInvocation (Core.Maybe Types.Url)
ciStandardOutputUrl = Lens.field @"standardOutputUrl"
{-# DEPRECATED ciStandardOutputUrl "Use generic-lens or generic-optics with 'standardOutputUrl' instead." #-}

-- | Whether or not the invocation succeeded, failed, or is pending.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' CommandInvocation (Core.Maybe Types.CommandInvocationStatus)
ciStatus = Lens.field @"status"
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to the instance.
--
--
--     * In Progress: The command has been sent to the instance but has not reached a terminal state.
--
--
--     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.
--
--
--     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
--
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatusDetails :: Lens.Lens' CommandInvocation (Core.Maybe Types.StatusDetails)
ciStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED ciStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | Gets the trace output sent by the agent.
--
-- /Note:/ Consider using 'traceOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTraceOutput :: Lens.Lens' CommandInvocation (Core.Maybe Types.InvocationTraceOutput)
ciTraceOutput = Lens.field @"traceOutput"
{-# DEPRECATED ciTraceOutput "Use generic-lens or generic-optics with 'traceOutput' instead." #-}

instance Core.FromJSON CommandInvocation where
  parseJSON =
    Core.withObject "CommandInvocation" Core.$
      \x ->
        CommandInvocation'
          Core.<$> (x Core..:? "CloudWatchOutputConfig")
          Core.<*> (x Core..:? "CommandId")
          Core.<*> (x Core..:? "CommandPlugins")
          Core.<*> (x Core..:? "Comment")
          Core.<*> (x Core..:? "DocumentName")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "InstanceName")
          Core.<*> (x Core..:? "NotificationConfig")
          Core.<*> (x Core..:? "RequestedDateTime")
          Core.<*> (x Core..:? "ServiceRole")
          Core.<*> (x Core..:? "StandardErrorUrl")
          Core.<*> (x Core..:? "StandardOutputUrl")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetails")
          Core.<*> (x Core..:? "TraceOutput")
