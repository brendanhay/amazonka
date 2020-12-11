{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetCommandInvocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about command execution for an invocation or plugin.
module Network.AWS.SSM.GetCommandInvocation
  ( -- * Creating a request
    GetCommandInvocation (..),
    mkGetCommandInvocation,

    -- ** Request lenses
    gciPluginName,
    gciCommandId,
    gciInstanceId,

    -- * Destructuring the response
    GetCommandInvocationResponse (..),
    mkGetCommandInvocationResponse,

    -- ** Response lenses
    gcirsInstanceId,
    gcirsStatus,
    gcirsStandardErrorContent,
    gcirsCloudWatchOutputConfig,
    gcirsExecutionElapsedTime,
    gcirsDocumentName,
    gcirsStandardErrorURL,
    gcirsExecutionStartDateTime,
    gcirsResponseCode,
    gcirsStatusDetails,
    gcirsExecutionEndDateTime,
    gcirsStandardOutputURL,
    gcirsCommandId,
    gcirsDocumentVersion,
    gcirsStandardOutputContent,
    gcirsComment,
    gcirsPluginName,
    gcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetCommandInvocation' smart constructor.
data GetCommandInvocation = GetCommandInvocation'
  { pluginName ::
      Lude.Maybe Lude.Text,
    commandId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommandInvocation' with the minimum fields required to make a request.
--
-- * 'commandId' - (Required) The parent command ID of the invocation plugin.
-- * 'instanceId' - (Required) The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
-- * 'pluginName' - (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
--
-- Plugin names are also referred to as step names in Systems Manager documents.
mkGetCommandInvocation ::
  -- | 'commandId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  GetCommandInvocation
mkGetCommandInvocation pCommandId_ pInstanceId_ =
  GetCommandInvocation'
    { pluginName = Lude.Nothing,
      commandId = pCommandId_,
      instanceId = pInstanceId_
    }

-- | (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
--
-- Plugin names are also referred to as step names in Systems Manager documents.
--
-- /Note:/ Consider using 'pluginName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciPluginName :: Lens.Lens' GetCommandInvocation (Lude.Maybe Lude.Text)
gciPluginName = Lens.lens (pluginName :: GetCommandInvocation -> Lude.Maybe Lude.Text) (\s a -> s {pluginName = a} :: GetCommandInvocation)
{-# DEPRECATED gciPluginName "Use generic-lens or generic-optics with 'pluginName' instead." #-}

-- | (Required) The parent command ID of the invocation plugin.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciCommandId :: Lens.Lens' GetCommandInvocation Lude.Text
gciCommandId = Lens.lens (commandId :: GetCommandInvocation -> Lude.Text) (\s a -> s {commandId = a} :: GetCommandInvocation)
{-# DEPRECATED gciCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Required) The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciInstanceId :: Lens.Lens' GetCommandInvocation Lude.Text
gciInstanceId = Lens.lens (instanceId :: GetCommandInvocation -> Lude.Text) (\s a -> s {instanceId = a} :: GetCommandInvocation)
{-# DEPRECATED gciInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest GetCommandInvocation where
  type Rs GetCommandInvocation = GetCommandInvocationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommandInvocationResponse'
            Lude.<$> (x Lude..?> "InstanceId")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "StandardErrorContent")
            Lude.<*> (x Lude..?> "CloudWatchOutputConfig")
            Lude.<*> (x Lude..?> "ExecutionElapsedTime")
            Lude.<*> (x Lude..?> "DocumentName")
            Lude.<*> (x Lude..?> "StandardErrorUrl")
            Lude.<*> (x Lude..?> "ExecutionStartDateTime")
            Lude.<*> (x Lude..?> "ResponseCode")
            Lude.<*> (x Lude..?> "StatusDetails")
            Lude.<*> (x Lude..?> "ExecutionEndDateTime")
            Lude.<*> (x Lude..?> "StandardOutputUrl")
            Lude.<*> (x Lude..?> "CommandId")
            Lude.<*> (x Lude..?> "DocumentVersion")
            Lude.<*> (x Lude..?> "StandardOutputContent")
            Lude.<*> (x Lude..?> "Comment")
            Lude.<*> (x Lude..?> "PluginName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCommandInvocation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetCommandInvocation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCommandInvocation where
  toJSON GetCommandInvocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PluginName" Lude..=) Lude.<$> pluginName,
            Lude.Just ("CommandId" Lude..= commandId),
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath GetCommandInvocation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCommandInvocation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCommandInvocationResponse' smart constructor.
data GetCommandInvocationResponse = GetCommandInvocationResponse'
  { instanceId ::
      Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe
        CommandInvocationStatus,
    standardErrorContent ::
      Lude.Maybe Lude.Text,
    cloudWatchOutputConfig ::
      Lude.Maybe CloudWatchOutputConfig,
    executionElapsedTime ::
      Lude.Maybe Lude.Text,
    documentName ::
      Lude.Maybe Lude.Text,
    standardErrorURL ::
      Lude.Maybe Lude.Text,
    executionStartDateTime ::
      Lude.Maybe Lude.Text,
    responseCode ::
      Lude.Maybe Lude.Int,
    statusDetails ::
      Lude.Maybe Lude.Text,
    executionEndDateTime ::
      Lude.Maybe Lude.Text,
    standardOutputURL ::
      Lude.Maybe Lude.Text,
    commandId :: Lude.Maybe Lude.Text,
    documentVersion ::
      Lude.Maybe Lude.Text,
    standardOutputContent ::
      Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    pluginName ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommandInvocationResponse' with the minimum fields required to make a request.
--
-- * 'cloudWatchOutputConfig' - CloudWatch Logs information where Systems Manager sent the command output.
-- * 'commandId' - The parent command ID of the invocation plugin.
-- * 'comment' - The comment text for the command.
-- * 'documentName' - The name of the document that was run. For example, AWS-RunShellScript.
-- * 'documentVersion' - The SSM document version used in the request.
-- * 'executionElapsedTime' - Duration since ExecutionStartDateTime.
-- * 'executionEndDateTime' - The date and time the plugin was finished running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
-- * 'executionStartDateTime' - The date and time the plugin started running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
-- * 'instanceId' - The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
-- * 'pluginName' - The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
-- * 'responseCode' - The error level response code for the plugin script. If the response code is -1, then the command has not started running on the instance, or it was not received by the instance.
-- * 'responseStatus' - The response status code.
-- * 'standardErrorContent' - The first 8,000 characters written by the plugin to stderr. If the command has not finished running, then this string is empty.
-- * 'standardErrorURL' - The URL for the complete text written by the plugin to stderr. If the command has not finished running, then this string is empty.
-- * 'standardOutputContent' - The first 24,000 characters written by the plugin to stdout. If the command has not finished running, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
-- * 'standardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If an S3 bucket was not specified, then this string is empty.
-- * 'status' - The status of this invocation plugin. This status can be different than StatusDetails.
-- * 'statusDetails' - A detailed status of the command execution for an invocation. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to the instance.
--
--
--     * In Progress: The command has been sent to the instance but has not reached a terminal state.
--
--
--     * Delayed: The system attempted to send the command to the target, but the target was not available. The instance might not be available because of network issues, because the instance was stopped, or for similar reasons. The system will try to send the command again.
--
--
--     * Success: The command or plugin ran successfully. This is a terminal state.
--
--
--     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Execution Timed Out: The command started to run on the instance, but the execution was not complete before the timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Failed: The command wasn't run successfully on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Canceled: The command was terminated before it was completed. This is a terminal state.
--
--
--     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
mkGetCommandInvocationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCommandInvocationResponse
mkGetCommandInvocationResponse pResponseStatus_ =
  GetCommandInvocationResponse'
    { instanceId = Lude.Nothing,
      status = Lude.Nothing,
      standardErrorContent = Lude.Nothing,
      cloudWatchOutputConfig = Lude.Nothing,
      executionElapsedTime = Lude.Nothing,
      documentName = Lude.Nothing,
      standardErrorURL = Lude.Nothing,
      executionStartDateTime = Lude.Nothing,
      responseCode = Lude.Nothing,
      statusDetails = Lude.Nothing,
      executionEndDateTime = Lude.Nothing,
      standardOutputURL = Lude.Nothing,
      commandId = Lude.Nothing,
      documentVersion = Lude.Nothing,
      standardOutputContent = Lude.Nothing,
      comment = Lude.Nothing,
      pluginName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsInstanceId :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsInstanceId = Lens.lens (instanceId :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The status of this invocation plugin. This status can be different than StatusDetails.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsStatus :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe CommandInvocationStatus)
gcirsStatus = Lens.lens (status :: GetCommandInvocationResponse -> Lude.Maybe CommandInvocationStatus) (\s a -> s {status = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The first 8,000 characters written by the plugin to stderr. If the command has not finished running, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsStandardErrorContent :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsStandardErrorContent = Lens.lens (standardErrorContent :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {standardErrorContent = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStandardErrorContent "Use generic-lens or generic-optics with 'standardErrorContent' instead." #-}

-- | CloudWatch Logs information where Systems Manager sent the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsCloudWatchOutputConfig :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe CloudWatchOutputConfig)
gcirsCloudWatchOutputConfig = Lens.lens (cloudWatchOutputConfig :: GetCommandInvocationResponse -> Lude.Maybe CloudWatchOutputConfig) (\s a -> s {cloudWatchOutputConfig = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | Duration since ExecutionStartDateTime.
--
-- /Note:/ Consider using 'executionElapsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsExecutionElapsedTime :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsExecutionElapsedTime = Lens.lens (executionElapsedTime :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {executionElapsedTime = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsExecutionElapsedTime "Use generic-lens or generic-optics with 'executionElapsedTime' instead." #-}

-- | The name of the document that was run. For example, AWS-RunShellScript.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsDocumentName :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsDocumentName = Lens.lens (documentName :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The URL for the complete text written by the plugin to stderr. If the command has not finished running, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsStandardErrorURL :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsStandardErrorURL = Lens.lens (standardErrorURL :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {standardErrorURL = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStandardErrorURL "Use generic-lens or generic-optics with 'standardErrorURL' instead." #-}

-- | The date and time the plugin started running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
--
-- /Note:/ Consider using 'executionStartDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsExecutionStartDateTime :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsExecutionStartDateTime = Lens.lens (executionStartDateTime :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {executionStartDateTime = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsExecutionStartDateTime "Use generic-lens or generic-optics with 'executionStartDateTime' instead." #-}

-- | The error level response code for the plugin script. If the response code is -1, then the command has not started running on the instance, or it was not received by the instance.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseCode :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Int)
gcirsResponseCode = Lens.lens (responseCode :: GetCommandInvocationResponse -> Lude.Maybe Lude.Int) (\s a -> s {responseCode = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | A detailed status of the command execution for an invocation. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses> in the /AWS Systems Manager User Guide/ . StatusDetails can be one of the following values:
--
--
--     * Pending: The command has not been sent to the instance.
--
--
--     * In Progress: The command has been sent to the instance but has not reached a terminal state.
--
--
--     * Delayed: The system attempted to send the command to the target, but the target was not available. The instance might not be available because of network issues, because the instance was stopped, or for similar reasons. The system will try to send the command again.
--
--
--     * Success: The command or plugin ran successfully. This is a terminal state.
--
--
--     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.
--
--
--     * Execution Timed Out: The command started to run on the instance, but the execution was not complete before the timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.
--
--
--     * Failed: The command wasn't run successfully on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.
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
gcirsStatusDetails :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsStatusDetails = Lens.lens (statusDetails :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The date and time the plugin was finished running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
--
-- /Note:/ Consider using 'executionEndDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsExecutionEndDateTime :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsExecutionEndDateTime = Lens.lens (executionEndDateTime :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {executionEndDateTime = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsExecutionEndDateTime "Use generic-lens or generic-optics with 'executionEndDateTime' instead." #-}

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If an S3 bucket was not specified, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsStandardOutputURL :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsStandardOutputURL = Lens.lens (standardOutputURL :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {standardOutputURL = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStandardOutputURL "Use generic-lens or generic-optics with 'standardOutputURL' instead." #-}

-- | The parent command ID of the invocation plugin.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsCommandId :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsCommandId = Lens.lens (commandId :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {commandId = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The SSM document version used in the request.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsDocumentVersion :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsDocumentVersion = Lens.lens (documentVersion :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The first 24,000 characters written by the plugin to stdout. If the command has not finished running, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsStandardOutputContent :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsStandardOutputContent = Lens.lens (standardOutputContent :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {standardOutputContent = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsStandardOutputContent "Use generic-lens or generic-optics with 'standardOutputContent' instead." #-}

-- | The comment text for the command.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsComment :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsComment = Lens.lens (comment :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
--
-- /Note:/ Consider using 'pluginName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsPluginName :: Lens.Lens' GetCommandInvocationResponse (Lude.Maybe Lude.Text)
gcirsPluginName = Lens.lens (pluginName :: GetCommandInvocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {pluginName = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsPluginName "Use generic-lens or generic-optics with 'pluginName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseStatus :: Lens.Lens' GetCommandInvocationResponse Lude.Int
gcirsResponseStatus = Lens.lens (responseStatus :: GetCommandInvocationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommandInvocationResponse)
{-# DEPRECATED gcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
