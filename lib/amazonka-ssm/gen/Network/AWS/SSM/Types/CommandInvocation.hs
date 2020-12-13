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
    cifInstanceId,
    cifStatus,
    cifNotificationConfig,
    cifCommandPlugins,
    cifCloudWatchOutputConfig,
    cifDocumentName,
    cifStandardErrorURL,
    cifStatusDetails,
    cifStandardOutputURL,
    cifCommandId,
    cifDocumentVersion,
    cifComment,
    cifTraceOutput,
    cifInstanceName,
    cifRequestedDateTime,
    cifServiceRole,
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
  { -- | The instance ID in which this invocation was requested.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Whether or not the invocation succeeded, failed, or is pending.
    status :: Lude.Maybe CommandInvocationStatus,
    -- | Configurations for sending notifications about command status changes on a per instance basis.
    notificationConfig :: Lude.Maybe NotificationConfig,
    commandPlugins :: Lude.Maybe [CommandPlugin],
    -- | CloudWatch Logs information where you want Systems Manager to send the command output.
    cloudWatchOutputConfig :: Lude.Maybe CloudWatchOutputConfig,
    -- | The document name that was requested for execution.
    documentName :: Lude.Maybe Lude.Text,
    -- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
    standardErrorURL :: Lude.Maybe Lude.Text,
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
    statusDetails :: Lude.Maybe Lude.Text,
    -- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
    standardOutputURL :: Lude.Maybe Lude.Text,
    -- | The command against which this invocation was requested.
    commandId :: Lude.Maybe Lude.Text,
    -- | The SSM document version.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | User-specified information about the command, such as a brief description of what the command should do.
    comment :: Lude.Maybe Lude.Text,
    -- | Gets the trace output sent by the agent.
    traceOutput :: Lude.Maybe Lude.Text,
    -- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
    instanceName :: Lude.Maybe Lude.Text,
    -- | The time and date the request was sent to this instance.
    requestedDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID in which this invocation was requested.
-- * 'status' - Whether or not the invocation succeeded, failed, or is pending.
-- * 'notificationConfig' - Configurations for sending notifications about command status changes on a per instance basis.
-- * 'commandPlugins' -
-- * 'cloudWatchOutputConfig' - CloudWatch Logs information where you want Systems Manager to send the command output.
-- * 'documentName' - The document name that was requested for execution.
-- * 'standardErrorURL' - The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
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
-- * 'standardOutputURL' - The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
-- * 'commandId' - The command against which this invocation was requested.
-- * 'documentVersion' - The SSM document version.
-- * 'comment' - User-specified information about the command, such as a brief description of what the command should do.
-- * 'traceOutput' - Gets the trace output sent by the agent.
-- * 'instanceName' - The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
-- * 'requestedDateTime' - The time and date the request was sent to this instance.
-- * 'serviceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
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
cifInstanceId :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifInstanceId = Lens.lens (instanceId :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CommandInvocation)
{-# DEPRECATED cifInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Whether or not the invocation succeeded, failed, or is pending.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifStatus :: Lens.Lens' CommandInvocation (Lude.Maybe CommandInvocationStatus)
cifStatus = Lens.lens (status :: CommandInvocation -> Lude.Maybe CommandInvocationStatus) (\s a -> s {status = a} :: CommandInvocation)
{-# DEPRECATED cifStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Configurations for sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'notificationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifNotificationConfig :: Lens.Lens' CommandInvocation (Lude.Maybe NotificationConfig)
cifNotificationConfig = Lens.lens (notificationConfig :: CommandInvocation -> Lude.Maybe NotificationConfig) (\s a -> s {notificationConfig = a} :: CommandInvocation)
{-# DEPRECATED cifNotificationConfig "Use generic-lens or generic-optics with 'notificationConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'commandPlugins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifCommandPlugins :: Lens.Lens' CommandInvocation (Lude.Maybe [CommandPlugin])
cifCommandPlugins = Lens.lens (commandPlugins :: CommandInvocation -> Lude.Maybe [CommandPlugin]) (\s a -> s {commandPlugins = a} :: CommandInvocation)
{-# DEPRECATED cifCommandPlugins "Use generic-lens or generic-optics with 'commandPlugins' instead." #-}

-- | CloudWatch Logs information where you want Systems Manager to send the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifCloudWatchOutputConfig :: Lens.Lens' CommandInvocation (Lude.Maybe CloudWatchOutputConfig)
cifCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: CommandInvocation -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: CommandInvocation)
{-# DEPRECATED cifCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The document name that was requested for execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDocumentName :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifDocumentName = Lens.lens (documentName :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: CommandInvocation)
{-# DEPRECATED cifDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The URL to the plugin's StdErr file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardErrorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifStandardErrorURL :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifStandardErrorURL = Lens.lens (standardErrorURL :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {standardErrorURL = a} :: CommandInvocation)
{-# DEPRECATED cifStandardErrorURL "Use generic-lens or generic-optics with 'standardErrorURL' instead." #-}

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
cifStatusDetails :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifStatusDetails = Lens.lens (statusDetails :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: CommandInvocation)
{-# DEPRECATED cifStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The URL to the plugin's StdOut file in Amazon S3, if the S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the S3 bucket was defined for the command.
--
-- /Note:/ Consider using 'standardOutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifStandardOutputURL :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifStandardOutputURL = Lens.lens (standardOutputURL :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {standardOutputURL = a} :: CommandInvocation)
{-# DEPRECATED cifStandardOutputURL "Use generic-lens or generic-optics with 'standardOutputURL' instead." #-}

-- | The command against which this invocation was requested.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifCommandId :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifCommandId = Lens.lens (commandId :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: CommandInvocation)
{-# DEPRECATED cifCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The SSM document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDocumentVersion :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifDocumentVersion = Lens.lens (documentVersion :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: CommandInvocation)
{-# DEPRECATED cifDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | User-specified information about the command, such as a brief description of what the command should do.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifComment :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifComment = Lens.lens (comment :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CommandInvocation)
{-# DEPRECATED cifComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | Gets the trace output sent by the agent.
--
-- /Note:/ Consider using 'traceOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifTraceOutput :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifTraceOutput = Lens.lens (traceOutput :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {traceOutput = a} :: CommandInvocation)
{-# DEPRECATED cifTraceOutput "Use generic-lens or generic-optics with 'traceOutput' instead." #-}

-- | The name of the invocation target. For EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifInstanceName :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifInstanceName = Lens.lens (instanceName :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: CommandInvocation)
{-# DEPRECATED cifInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The time and date the request was sent to this instance.
--
-- /Note:/ Consider using 'requestedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifRequestedDateTime :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Timestamp)
cifRequestedDateTime = Lens.lens (requestedDateTime :: CommandInvocation -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedDateTime = a} :: CommandInvocation)
{-# DEPRECATED cifRequestedDateTime "Use generic-lens or generic-optics with 'requestedDateTime' instead." #-}

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifServiceRole :: Lens.Lens' CommandInvocation (Lude.Maybe Lude.Text)
cifServiceRole = Lens.lens (serviceRole :: CommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: CommandInvocation)
{-# DEPRECATED cifServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

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
