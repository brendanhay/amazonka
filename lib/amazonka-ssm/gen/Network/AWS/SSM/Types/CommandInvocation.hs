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
    comInstanceId,
    comStatus,
    comNotificationConfig,
    comCommandPlugins,
    comCloudWatchOutputConfig,
    comDocumentName,
    comStandardErrorURL,
    comStatusDetails,
    comStandardOutputURL,
    comCommandId,
    comDocumentVersion,
    comComment,
    comTraceOutput,
    comInstanceName,
    comRequestedDateTime,
    comServiceRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.CommandInvocationStatus
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.NotificationConfig

-- | An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user runs SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you ran.
--
-- /See:/ 'mkCommandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { instanceId ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe CommandInvocationStatus,
    notificationConfig :: Lude.Maybe NotificationConfig,
    commandPlugins :: Lude.Maybe [CommandPlugin],
    cloudWatchOutputConfig ::
      Lude.Maybe CloudWatchOutputConfig,
    documentName :: Lude.Maybe Lude.Text,
    standardErrorURL :: Lude.Maybe Lude.Text,
    statusDetails :: Lude.Maybe Lude.Text,
    standardOutputURL :: Lude.Maybe Lude.Text,
    commandId :: Lude.Maybe Lude.Text,
    documentVersion :: Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    traceOutput :: Lude.Maybe Lude.Text,
    instanceName :: Lude.Maybe Lude.Text,
    requestedDateTime :: Lude.Maybe Lude.Timestamp,
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- * 'cloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the command output.
-- * 'commandId' - The command against which this invocation was requested.
-- * 'commandPlugins' - Undocumented field.
-- * 'comment' - User-specified information about the command, such as a brief description of what the command should do.
-- * 'documentName' - The document name that was requested for execution.
-- * 'documentVersion' - The SSM document version.
-- * 'instanceId' - The instance ID in which this invocation was requested.
-- * 'instanceName' - The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
-- * 'notificationConfig' - Configurations for sending notifications about command status changes on a per instance basis.
-- * 'requestedDateTime' - The time and date the request was sent to this instance.
-- * 'serviceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
-- * 'standardErrorURL' - The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
-- * 'standardOutputURL' - The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
-- * 'status' - Whether or not the invocation succeeded, failed, or is pending.
-- * 'statusDetails' - A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
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
-- * 'traceOutput' - Gets the trace output sent by the agent.
mkCommandInvocation ::
  CommandInvocation
mkCommandInvocation =
  CommandInvocation'
    { instanceId = Lude.Nothing,
      status = Lude.Nothing,
      notificationConfig = Lude.Nothing,
      commandPlugins = Lude.Nothing,
      cloudWatchOutputConfig = Lude.Nothing,
      documentName = Lude.Nothing,
      standardErrorURL = Lude.Nothing,
      statusDetails = Lude.Nothing,
      standardOutputURL = Lude.Nothing,
      commandId = Lude.Nothing,
      documentVersion = Lude.Nothing,
      comment = Lude.Nothing,
      traceOutput = Lude.Nothing,
      instanceName = Lude.Nothing,
      requestedDateTime = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The instance ID in which this invocation was requested.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comInstanceId :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comInstanceId = Lens.lens (instanceId :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CommandInvocation)
{-# DEPRECATED comInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Whether or not the invocation succeeded, failed, or is pending.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comStatus :: Lens.Lens' CommandInvocation (Lude.Maybe CommandInvocationStatus)
comStatus = Lens.lens (status :: CommandInvocation -> Lude.Maybe CommandInvocationStatus) (\s a -> s {status = a} :: CommandInvocation)
{-# DEPRECATED comStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Configurations for sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comNotificationConfig :: Lens.Lens' CommandInvocation (Lude.Maybe NotificationConfig)
comNotificationConfig = Lens.lens (notificationConfig :: CommandInvocation -> Lude.Maybe NotificationConfig) (\s a -> s {notificationConfig = a} :: CommandInvocation)
{-# DEPRECATED comNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'commandPlugins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comCommandPlugins :: Lens.Lens' CommandInvocation (Lude.Maybe [CommandPlugin])
comCommandPlugins = Lens.lens (commandPlugins :: CommandInvocation -> Lude.Maybe [CommandPlugin]) (\s a -> s {commandPlugins = a} :: CommandInvocation)
{-# DEPRECATED comCommandPlugins "Use generic-lens or generic-optics with 'commandPlugins' instead." #-}

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comCloudWatchOutputConfig :: Lens.Lens' CommandInvocation (Lude.Maybe CloudWatchOutputConfig)
comCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: CommandInvocation -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: CommandInvocation)
{-# DEPRECATED comCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The document name that was requested for execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comDocumentName :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comDocumentName = Lens.lens (documentName :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: CommandInvocation)
{-# DEPRECATED comDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardErrorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comStandardErrorURL :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comStandardErrorURL = Lens.lens (standardErrorURL :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {standardErrorURL = a} :: CommandInvocation)
{-# DEPRECATED comStandardErrorURL "Use generic-lens or generic-optics with 'standardErrorURL' instead." #-}

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
comStatusDetails :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comStatusDetails = Lens.lens (statusDetails :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: CommandInvocation)
{-# DEPRECATED comStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardOutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comStandardOutputURL :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comStandardOutputURL = Lens.lens (standardOutputURL :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {standardOutputURL = a} :: CommandInvocation)
{-# DEPRECATED comStandardOutputURL "Use generic-lens or generic-optics with 'standardOutputURL' instead." #-}

-- | The command against which this invocation was requested.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comCommandId :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comCommandId = Lens.lens (commandId :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: CommandInvocation)
{-# DEPRECATED comCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The SSM document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comDocumentVersion :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comDocumentVersion = Lens.lens (documentVersion :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: CommandInvocation)
{-# DEPRECATED comDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comComment :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comComment = Lens.lens (comment :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CommandInvocation)
{-# DEPRECATED comComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | Gets the trace output sent by the agent.
--
-- /Note:/ Consider using 'traceOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comTraceOutput :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comTraceOutput = Lens.lens (traceOutput :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {traceOutput = a} :: CommandInvocation)
{-# DEPRECATED comTraceOutput "Use generic-lens or generic-optics with 'traceOutput' instead." #-}

-- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comInstanceName :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comInstanceName = Lens.lens (instanceName :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: CommandInvocation)
{-# DEPRECATED comInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The time and date the request was sent to this instance.
--
-- /Note:/ Consider using 'requestedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comRequestedDateTime :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Timestamp)
comRequestedDateTime = Lens.lens (requestedDateTime :: CommandInvocation -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedDateTime = a} :: CommandInvocation)
{-# DEPRECATED comRequestedDateTime "Use generic-lens or generic-optics with 'requestedDateTime' instead." #-}

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comServiceRole :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
comServiceRole = Lens.lens (serviceRole :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: CommandInvocation)
{-# DEPRECATED comServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON CommandInvocation where
  parseJSON =
    Lude.withObject
      "CommandInvocation"
      ( \x ->
          CommandInvocation'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "NotificationConfig")
            Lude.<*> (x Lude..:? "CommandPlugins" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CloudWatchOutputConfig")
            Lude.<*> (x Lude..:? "DocumentName")
            Lude.<*> (x Lude..:? "StandardErrorUrl")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "StandardOutputUrl")
            Lude.<*> (x Lude..:? "CommandId")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "Comment")
            Lude.<*> (x Lude..:? "TraceOutput")
            Lude.<*> (x Lude..:? "InstanceName")
            Lude.<*> (x Lude..:? "RequestedDateTime")
            Lude.<*> (x Lude..:? "ServiceRole")
      )
