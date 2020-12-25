{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gciCommandId,
    gciInstanceId,
    gciPluginName,

    -- * Destructuring the response
    GetCommandInvocationResponse (..),
    mkGetCommandInvocationResponse,

    -- ** Response lenses
    gcirrsCloudWatchOutputConfig,
    gcirrsCommandId,
    gcirrsComment,
    gcirrsDocumentName,
    gcirrsDocumentVersion,
    gcirrsExecutionElapsedTime,
    gcirrsExecutionEndDateTime,
    gcirrsExecutionStartDateTime,
    gcirrsInstanceId,
    gcirrsPluginName,
    gcirrsResponseCode,
    gcirrsStandardErrorContent,
    gcirrsStandardErrorUrl,
    gcirrsStandardOutputContent,
    gcirrsStandardOutputUrl,
    gcirrsStatus,
    gcirrsStatusDetails,
    gcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetCommandInvocation' smart constructor.
data GetCommandInvocation = GetCommandInvocation'
  { -- | (Required) The parent command ID of the invocation plugin.
    commandId :: Types.CommandId,
    -- | (Required) The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
    instanceId :: Types.InstanceId,
    -- | (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
    --
    -- Plugin names are also referred to as step names in Systems Manager documents.
    pluginName :: Core.Maybe Types.PluginName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommandInvocation' value with any optional fields omitted.
mkGetCommandInvocation ::
  -- | 'commandId'
  Types.CommandId ->
  -- | 'instanceId'
  Types.InstanceId ->
  GetCommandInvocation
mkGetCommandInvocation commandId instanceId =
  GetCommandInvocation'
    { commandId,
      instanceId,
      pluginName = Core.Nothing
    }

-- | (Required) The parent command ID of the invocation plugin.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciCommandId :: Lens.Lens' GetCommandInvocation Types.CommandId
gciCommandId = Lens.field @"commandId"
{-# DEPRECATED gciCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Required) The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciInstanceId :: Lens.Lens' GetCommandInvocation Types.InstanceId
gciInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gciInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | (Optional) The name of the plugin for which you want detailed results. If the document contains only one plugin, the name can be omitted and the details will be returned.
--
-- Plugin names are also referred to as step names in Systems Manager documents.
--
-- /Note:/ Consider using 'pluginName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciPluginName :: Lens.Lens' GetCommandInvocation (Core.Maybe Types.PluginName)
gciPluginName = Lens.field @"pluginName"
{-# DEPRECATED gciPluginName "Use generic-lens or generic-optics with 'pluginName' instead." #-}

instance Core.FromJSON GetCommandInvocation where
  toJSON GetCommandInvocation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CommandId" Core..= commandId),
            Core.Just ("InstanceId" Core..= instanceId),
            ("PluginName" Core..=) Core.<$> pluginName
          ]
      )

instance Core.AWSRequest GetCommandInvocation where
  type Rs GetCommandInvocation = GetCommandInvocationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetCommandInvocation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommandInvocationResponse'
            Core.<$> (x Core..:? "CloudWatchOutputConfig")
            Core.<*> (x Core..:? "CommandId")
            Core.<*> (x Core..:? "Comment")
            Core.<*> (x Core..:? "DocumentName")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "ExecutionElapsedTime")
            Core.<*> (x Core..:? "ExecutionEndDateTime")
            Core.<*> (x Core..:? "ExecutionStartDateTime")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "PluginName")
            Core.<*> (x Core..:? "ResponseCode")
            Core.<*> (x Core..:? "StandardErrorContent")
            Core.<*> (x Core..:? "StandardErrorUrl")
            Core.<*> (x Core..:? "StandardOutputContent")
            Core.<*> (x Core..:? "StandardOutputUrl")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCommandInvocationResponse' smart constructor.
data GetCommandInvocationResponse = GetCommandInvocationResponse'
  { -- | CloudWatch Logs information where Systems Manager sent the command output.
    cloudWatchOutputConfig :: Core.Maybe Types.CloudWatchOutputConfig,
    -- | The parent command ID of the invocation plugin.
    commandId :: Core.Maybe Types.CommandId,
    -- | The comment text for the command.
    comment :: Core.Maybe Types.Comment,
    -- | The name of the document that was run. For example, AWS-RunShellScript.
    documentName :: Core.Maybe Types.DocumentName,
    -- | The SSM document version used in the request.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | Duration since ExecutionStartDateTime.
    executionElapsedTime :: Core.Maybe Types.ExecutionElapsedTime,
    -- | The date and time the plugin was finished running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter.
    --
    -- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
    -- If the plugin has not started to run, the string is empty.
    executionEndDateTime :: Core.Maybe Types.ExecutionEndDateTime,
    -- | The date and time the plugin started running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter.
    --
    -- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
    -- If the plugin has not started to run, the string is empty.
    executionStartDateTime :: Core.Maybe Types.ExecutionStartDateTime,
    -- | The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
    pluginName :: Core.Maybe Types.PluginName,
    -- | The error level response code for the plugin script. If the response code is -1, then the command has not started running on the instance, or it was not received by the instance.
    responseCode :: Core.Maybe Core.Int,
    -- | The first 8,000 characters written by the plugin to stderr. If the command has not finished running, then this string is empty.
    standardErrorContent :: Core.Maybe Types.StandardErrorContent,
    -- | The URL for the complete text written by the plugin to stderr. If the command has not finished running, then this string is empty.
    standardErrorUrl :: Core.Maybe Types.StandardErrorUrl,
    -- | The first 24,000 characters written by the plugin to stdout. If the command has not finished running, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
    standardOutputContent :: Core.Maybe Types.StandardOutputContent,
    -- | The URL for the complete text written by the plugin to stdout in Amazon S3. If an S3 bucket was not specified, then this string is empty.
    standardOutputUrl :: Core.Maybe Types.StandardOutputUrl,
    -- | The status of this invocation plugin. This status can be different than StatusDetails.
    status :: Core.Maybe Types.CommandInvocationStatus,
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
    statusDetails :: Core.Maybe Types.StatusDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommandInvocationResponse' value with any optional fields omitted.
mkGetCommandInvocationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCommandInvocationResponse
mkGetCommandInvocationResponse responseStatus =
  GetCommandInvocationResponse'
    { cloudWatchOutputConfig =
        Core.Nothing,
      commandId = Core.Nothing,
      comment = Core.Nothing,
      documentName = Core.Nothing,
      documentVersion = Core.Nothing,
      executionElapsedTime = Core.Nothing,
      executionEndDateTime = Core.Nothing,
      executionStartDateTime = Core.Nothing,
      instanceId = Core.Nothing,
      pluginName = Core.Nothing,
      responseCode = Core.Nothing,
      standardErrorContent = Core.Nothing,
      standardErrorUrl = Core.Nothing,
      standardOutputContent = Core.Nothing,
      standardOutputUrl = Core.Nothing,
      status = Core.Nothing,
      statusDetails = Core.Nothing,
      responseStatus
    }

-- | CloudWatch Logs information where Systems Manager sent the command output.
--
-- /Note:/ Consider using 'cloudWatchOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsCloudWatchOutputConfig :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.CloudWatchOutputConfig)
gcirrsCloudWatchOutputConfig = Lens.field @"cloudWatchOutputConfig"
{-# DEPRECATED gcirrsCloudWatchOutputConfig "Use generic-lens or generic-optics with 'cloudWatchOutputConfig' instead." #-}

-- | The parent command ID of the invocation plugin.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsCommandId :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.CommandId)
gcirrsCommandId = Lens.field @"commandId"
{-# DEPRECATED gcirrsCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | The comment text for the command.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsComment :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.Comment)
gcirrsComment = Lens.field @"comment"
{-# DEPRECATED gcirrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The name of the document that was run. For example, AWS-RunShellScript.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsDocumentName :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.DocumentName)
gcirrsDocumentName = Lens.field @"documentName"
{-# DEPRECATED gcirrsDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The SSM document version used in the request.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsDocumentVersion :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.DocumentVersion)
gcirrsDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED gcirrsDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Duration since ExecutionStartDateTime.
--
-- /Note:/ Consider using 'executionElapsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsExecutionElapsedTime :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.ExecutionElapsedTime)
gcirrsExecutionElapsedTime = Lens.field @"executionElapsedTime"
{-# DEPRECATED gcirrsExecutionElapsedTime "Use generic-lens or generic-optics with 'executionElapsedTime' instead." #-}

-- | The date and time the plugin was finished running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@ filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
--
-- /Note:/ Consider using 'executionEndDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsExecutionEndDateTime :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.ExecutionEndDateTime)
gcirrsExecutionEndDateTime = Lens.field @"executionEndDateTime"
{-# DEPRECATED gcirrsExecutionEndDateTime "Use generic-lens or generic-optics with 'executionEndDateTime' instead." #-}

-- | The date and time the plugin started running. Date and time are written in ISO 8601 format. For example, June 7, 2017 is represented as 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@ filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
-- If the plugin has not started to run, the string is empty.
--
-- /Note:/ Consider using 'executionStartDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsExecutionStartDateTime :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.ExecutionStartDateTime)
gcirrsExecutionStartDateTime = Lens.field @"executionStartDateTime"
{-# DEPRECATED gcirrsExecutionStartDateTime "Use generic-lens or generic-optics with 'executionStartDateTime' instead." #-}

-- | The ID of the managed instance targeted by the command. A managed instance can be an EC2 instance or an instance in your hybrid environment that is configured for Systems Manager.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsInstanceId :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.InstanceId)
gcirrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gcirrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the plugin for which you want detailed results. For example, aws:RunShellScript is a plugin.
--
-- /Note:/ Consider using 'pluginName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsPluginName :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.PluginName)
gcirrsPluginName = Lens.field @"pluginName"
{-# DEPRECATED gcirrsPluginName "Use generic-lens or generic-optics with 'pluginName' instead." #-}

-- | The error level response code for the plugin script. If the response code is -1, then the command has not started running on the instance, or it was not received by the instance.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseCode :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Core.Int)
gcirrsResponseCode = Lens.field @"responseCode"
{-# DEPRECATED gcirrsResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The first 8,000 characters written by the plugin to stderr. If the command has not finished running, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsStandardErrorContent :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.StandardErrorContent)
gcirrsStandardErrorContent = Lens.field @"standardErrorContent"
{-# DEPRECATED gcirrsStandardErrorContent "Use generic-lens or generic-optics with 'standardErrorContent' instead." #-}

-- | The URL for the complete text written by the plugin to stderr. If the command has not finished running, then this string is empty.
--
-- /Note:/ Consider using 'standardErrorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsStandardErrorUrl :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.StandardErrorUrl)
gcirrsStandardErrorUrl = Lens.field @"standardErrorUrl"
{-# DEPRECATED gcirrsStandardErrorUrl "Use generic-lens or generic-optics with 'standardErrorUrl' instead." #-}

-- | The first 24,000 characters written by the plugin to stdout. If the command has not finished running, if ExecutionStatus is neither Succeeded nor Failed, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsStandardOutputContent :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.StandardOutputContent)
gcirrsStandardOutputContent = Lens.field @"standardOutputContent"
{-# DEPRECATED gcirrsStandardOutputContent "Use generic-lens or generic-optics with 'standardOutputContent' instead." #-}

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If an S3 bucket was not specified, then this string is empty.
--
-- /Note:/ Consider using 'standardOutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsStandardOutputUrl :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.StandardOutputUrl)
gcirrsStandardOutputUrl = Lens.field @"standardOutputUrl"
{-# DEPRECATED gcirrsStandardOutputUrl "Use generic-lens or generic-optics with 'standardOutputUrl' instead." #-}

-- | The status of this invocation plugin. This status can be different than StatusDetails.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsStatus :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.CommandInvocationStatus)
gcirrsStatus = Lens.field @"status"
{-# DEPRECATED gcirrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
gcirrsStatusDetails :: Lens.Lens' GetCommandInvocationResponse (Core.Maybe Types.StatusDetails)
gcirrsStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED gcirrsStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetCommandInvocationResponse Core.Int
gcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
