{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetCommandInvocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about command execution for an invocation
-- or plugin.
module Network.AWS.SSM.GetCommandInvocation
  ( -- * Creating a Request
    GetCommandInvocation (..),
    newGetCommandInvocation,

    -- * Request Lenses
    getCommandInvocation_pluginName,
    getCommandInvocation_commandId,
    getCommandInvocation_instanceId,

    -- * Destructuring the Response
    GetCommandInvocationResponse (..),
    newGetCommandInvocationResponse,

    -- * Response Lenses
    getCommandInvocationResponse_standardOutputUrl,
    getCommandInvocationResponse_status,
    getCommandInvocationResponse_instanceId,
    getCommandInvocationResponse_statusDetails,
    getCommandInvocationResponse_pluginName,
    getCommandInvocationResponse_comment,
    getCommandInvocationResponse_executionStartDateTime,
    getCommandInvocationResponse_standardErrorUrl,
    getCommandInvocationResponse_documentName,
    getCommandInvocationResponse_commandId,
    getCommandInvocationResponse_standardErrorContent,
    getCommandInvocationResponse_executionEndDateTime,
    getCommandInvocationResponse_responseCode,
    getCommandInvocationResponse_cloudWatchOutputConfig,
    getCommandInvocationResponse_executionElapsedTime,
    getCommandInvocationResponse_documentVersion,
    getCommandInvocationResponse_standardOutputContent,
    getCommandInvocationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetCommandInvocation' smart constructor.
data GetCommandInvocation = GetCommandInvocation'
  { -- | (Optional) The name of the plugin for which you want detailed results.
    -- If the document contains only one plugin, the name can be omitted and
    -- the details will be returned.
    --
    -- Plugin names are also referred to as step names in Systems Manager
    -- documents.
    pluginName :: Prelude.Maybe Prelude.Text,
    -- | (Required) The parent command ID of the invocation plugin.
    commandId :: Prelude.Text,
    -- | (Required) The ID of the managed instance targeted by the command. A
    -- managed instance can be an EC2 instance or an instance in your hybrid
    -- environment that is configured for Systems Manager.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCommandInvocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pluginName', 'getCommandInvocation_pluginName' - (Optional) The name of the plugin for which you want detailed results.
-- If the document contains only one plugin, the name can be omitted and
-- the details will be returned.
--
-- Plugin names are also referred to as step names in Systems Manager
-- documents.
--
-- 'commandId', 'getCommandInvocation_commandId' - (Required) The parent command ID of the invocation plugin.
--
-- 'instanceId', 'getCommandInvocation_instanceId' - (Required) The ID of the managed instance targeted by the command. A
-- managed instance can be an EC2 instance or an instance in your hybrid
-- environment that is configured for Systems Manager.
newGetCommandInvocation ::
  -- | 'commandId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  GetCommandInvocation
newGetCommandInvocation pCommandId_ pInstanceId_ =
  GetCommandInvocation'
    { pluginName = Prelude.Nothing,
      commandId = pCommandId_,
      instanceId = pInstanceId_
    }

-- | (Optional) The name of the plugin for which you want detailed results.
-- If the document contains only one plugin, the name can be omitted and
-- the details will be returned.
--
-- Plugin names are also referred to as step names in Systems Manager
-- documents.
getCommandInvocation_pluginName :: Lens.Lens' GetCommandInvocation (Prelude.Maybe Prelude.Text)
getCommandInvocation_pluginName = Lens.lens (\GetCommandInvocation' {pluginName} -> pluginName) (\s@GetCommandInvocation' {} a -> s {pluginName = a} :: GetCommandInvocation)

-- | (Required) The parent command ID of the invocation plugin.
getCommandInvocation_commandId :: Lens.Lens' GetCommandInvocation Prelude.Text
getCommandInvocation_commandId = Lens.lens (\GetCommandInvocation' {commandId} -> commandId) (\s@GetCommandInvocation' {} a -> s {commandId = a} :: GetCommandInvocation)

-- | (Required) The ID of the managed instance targeted by the command. A
-- managed instance can be an EC2 instance or an instance in your hybrid
-- environment that is configured for Systems Manager.
getCommandInvocation_instanceId :: Lens.Lens' GetCommandInvocation Prelude.Text
getCommandInvocation_instanceId = Lens.lens (\GetCommandInvocation' {instanceId} -> instanceId) (\s@GetCommandInvocation' {} a -> s {instanceId = a} :: GetCommandInvocation)

instance Prelude.AWSRequest GetCommandInvocation where
  type
    Rs GetCommandInvocation =
      GetCommandInvocationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommandInvocationResponse'
            Prelude.<$> (x Prelude..?> "StandardOutputUrl")
            Prelude.<*> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "InstanceId")
            Prelude.<*> (x Prelude..?> "StatusDetails")
            Prelude.<*> (x Prelude..?> "PluginName")
            Prelude.<*> (x Prelude..?> "Comment")
            Prelude.<*> (x Prelude..?> "ExecutionStartDateTime")
            Prelude.<*> (x Prelude..?> "StandardErrorUrl")
            Prelude.<*> (x Prelude..?> "DocumentName")
            Prelude.<*> (x Prelude..?> "CommandId")
            Prelude.<*> (x Prelude..?> "StandardErrorContent")
            Prelude.<*> (x Prelude..?> "ExecutionEndDateTime")
            Prelude.<*> (x Prelude..?> "ResponseCode")
            Prelude.<*> (x Prelude..?> "CloudWatchOutputConfig")
            Prelude.<*> (x Prelude..?> "ExecutionElapsedTime")
            Prelude.<*> (x Prelude..?> "DocumentVersion")
            Prelude.<*> (x Prelude..?> "StandardOutputContent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCommandInvocation

instance Prelude.NFData GetCommandInvocation

instance Prelude.ToHeaders GetCommandInvocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetCommandInvocation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCommandInvocation where
  toJSON GetCommandInvocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PluginName" Prelude..=) Prelude.<$> pluginName,
            Prelude.Just ("CommandId" Prelude..= commandId),
            Prelude.Just ("InstanceId" Prelude..= instanceId)
          ]
      )

instance Prelude.ToPath GetCommandInvocation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCommandInvocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommandInvocationResponse' smart constructor.
data GetCommandInvocationResponse = GetCommandInvocationResponse'
  { -- | The URL for the complete text written by the plugin to stdout in Amazon
    -- S3. If an S3 bucket was not specified, then this string is empty.
    standardOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of this invocation plugin. This status can be different than
    -- StatusDetails.
    status :: Prelude.Maybe CommandInvocationStatus,
    -- | The ID of the managed instance targeted by the command. A managed
    -- instance can be an EC2 instance or an instance in your hybrid
    -- environment that is configured for Systems Manager.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A detailed status of the command execution for an invocation.
    -- StatusDetails includes more information than Status because it includes
    -- states resulting from error and concurrency control parameters.
    -- StatusDetails can show different results than Status. For more
    -- information about these statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
    -- following values:
    --
    -- -   Pending: The command has not been sent to the instance.
    --
    -- -   In Progress: The command has been sent to the instance but has not
    --     reached a terminal state.
    --
    -- -   Delayed: The system attempted to send the command to the target, but
    --     the target was not available. The instance might not be available
    --     because of network issues, because the instance was stopped, or for
    --     similar reasons. The system will try to send the command again.
    --
    -- -   Success: The command or plugin ran successfully. This is a terminal
    --     state.
    --
    -- -   Delivery Timed Out: The command was not delivered to the instance
    --     before the delivery timeout expired. Delivery timeouts do not count
    --     against the parent command\'s MaxErrors limit, but they do
    --     contribute to whether the parent command status is Success or
    --     Incomplete. This is a terminal state.
    --
    -- -   Execution Timed Out: The command started to run on the instance, but
    --     the execution was not complete before the timeout expired. Execution
    --     timeouts count against the MaxErrors limit of the parent command.
    --     This is a terminal state.
    --
    -- -   Failed: The command wasn\'t run successfully on the instance. For a
    --     plugin, this indicates that the result code was not zero. For a
    --     command invocation, this indicates that the result code for one or
    --     more plugins was not zero. Invocation failures count against the
    --     MaxErrors limit of the parent command. This is a terminal state.
    --
    -- -   Canceled: The command was terminated before it was completed. This
    --     is a terminal state.
    --
    -- -   Undeliverable: The command can\'t be delivered to the instance. The
    --     instance might not exist or might not be responding. Undeliverable
    --     invocations don\'t count against the parent command\'s MaxErrors
    --     limit and don\'t contribute to whether the parent command status is
    --     Success or Incomplete. This is a terminal state.
    --
    -- -   Terminated: The parent command exceeded its MaxErrors limit and
    --     subsequent command invocations were canceled by the system. This is
    --     a terminal state.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The name of the plugin for which you want detailed results. For example,
    -- aws:RunShellScript is a plugin.
    pluginName :: Prelude.Maybe Prelude.Text,
    -- | The comment text for the command.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The date and time the plugin started running. Date and time are written
    -- in ISO 8601 format. For example, June 7, 2017 is represented as
    -- 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@
    -- filter.
    --
    -- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
    --
    -- If the plugin has not started to run, the string is empty.
    executionStartDateTime :: Prelude.Maybe Prelude.Text,
    -- | The URL for the complete text written by the plugin to stderr. If the
    -- command has not finished running, then this string is empty.
    standardErrorUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the document that was run. For example, AWS-RunShellScript.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The parent command ID of the invocation plugin.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | The first 8,000 characters written by the plugin to stderr. If the
    -- command has not finished running, then this string is empty.
    standardErrorContent :: Prelude.Maybe Prelude.Text,
    -- | The date and time the plugin was finished running. Date and time are
    -- written in ISO 8601 format. For example, June 7, 2017 is represented as
    -- 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@
    -- filter.
    --
    -- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
    --
    -- If the plugin has not started to run, the string is empty.
    executionEndDateTime :: Prelude.Maybe Prelude.Text,
    -- | The error level response code for the plugin script. If the response
    -- code is -1, then the command has not started running on the instance, or
    -- it was not received by the instance.
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | CloudWatch Logs information where Systems Manager sent the command
    -- output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | Duration since ExecutionStartDateTime.
    executionElapsedTime :: Prelude.Maybe Prelude.Text,
    -- | The SSM document version used in the request.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The first 24,000 characters written by the plugin to stdout. If the
    -- command has not finished running, if ExecutionStatus is neither
    -- Succeeded nor Failed, then this string is empty.
    standardOutputContent :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCommandInvocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardOutputUrl', 'getCommandInvocationResponse_standardOutputUrl' - The URL for the complete text written by the plugin to stdout in Amazon
-- S3. If an S3 bucket was not specified, then this string is empty.
--
-- 'status', 'getCommandInvocationResponse_status' - The status of this invocation plugin. This status can be different than
-- StatusDetails.
--
-- 'instanceId', 'getCommandInvocationResponse_instanceId' - The ID of the managed instance targeted by the command. A managed
-- instance can be an EC2 instance or an instance in your hybrid
-- environment that is configured for Systems Manager.
--
-- 'statusDetails', 'getCommandInvocationResponse_statusDetails' - A detailed status of the command execution for an invocation.
-- StatusDetails includes more information than Status because it includes
-- states resulting from error and concurrency control parameters.
-- StatusDetails can show different results than Status. For more
-- information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to the instance.
--
-- -   In Progress: The command has been sent to the instance but has not
--     reached a terminal state.
--
-- -   Delayed: The system attempted to send the command to the target, but
--     the target was not available. The instance might not be available
--     because of network issues, because the instance was stopped, or for
--     similar reasons. The system will try to send the command again.
--
-- -   Success: The command or plugin ran successfully. This is a terminal
--     state.
--
-- -   Delivery Timed Out: The command was not delivered to the instance
--     before the delivery timeout expired. Delivery timeouts do not count
--     against the parent command\'s MaxErrors limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: The command started to run on the instance, but
--     the execution was not complete before the timeout expired. Execution
--     timeouts count against the MaxErrors limit of the parent command.
--     This is a terminal state.
--
-- -   Failed: The command wasn\'t run successfully on the instance. For a
--     plugin, this indicates that the result code was not zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins was not zero. Invocation failures count against the
--     MaxErrors limit of the parent command. This is a terminal state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the instance. The
--     instance might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s MaxErrors
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- 'pluginName', 'getCommandInvocationResponse_pluginName' - The name of the plugin for which you want detailed results. For example,
-- aws:RunShellScript is a plugin.
--
-- 'comment', 'getCommandInvocationResponse_comment' - The comment text for the command.
--
-- 'executionStartDateTime', 'getCommandInvocationResponse_executionStartDateTime' - The date and time the plugin started running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@
-- filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
--
-- If the plugin has not started to run, the string is empty.
--
-- 'standardErrorUrl', 'getCommandInvocationResponse_standardErrorUrl' - The URL for the complete text written by the plugin to stderr. If the
-- command has not finished running, then this string is empty.
--
-- 'documentName', 'getCommandInvocationResponse_documentName' - The name of the document that was run. For example, AWS-RunShellScript.
--
-- 'commandId', 'getCommandInvocationResponse_commandId' - The parent command ID of the invocation plugin.
--
-- 'standardErrorContent', 'getCommandInvocationResponse_standardErrorContent' - The first 8,000 characters written by the plugin to stderr. If the
-- command has not finished running, then this string is empty.
--
-- 'executionEndDateTime', 'getCommandInvocationResponse_executionEndDateTime' - The date and time the plugin was finished running. Date and time are
-- written in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@
-- filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
--
-- If the plugin has not started to run, the string is empty.
--
-- 'responseCode', 'getCommandInvocationResponse_responseCode' - The error level response code for the plugin script. If the response
-- code is -1, then the command has not started running on the instance, or
-- it was not received by the instance.
--
-- 'cloudWatchOutputConfig', 'getCommandInvocationResponse_cloudWatchOutputConfig' - CloudWatch Logs information where Systems Manager sent the command
-- output.
--
-- 'executionElapsedTime', 'getCommandInvocationResponse_executionElapsedTime' - Duration since ExecutionStartDateTime.
--
-- 'documentVersion', 'getCommandInvocationResponse_documentVersion' - The SSM document version used in the request.
--
-- 'standardOutputContent', 'getCommandInvocationResponse_standardOutputContent' - The first 24,000 characters written by the plugin to stdout. If the
-- command has not finished running, if ExecutionStatus is neither
-- Succeeded nor Failed, then this string is empty.
--
-- 'httpStatus', 'getCommandInvocationResponse_httpStatus' - The response's http status code.
newGetCommandInvocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCommandInvocationResponse
newGetCommandInvocationResponse pHttpStatus_ =
  GetCommandInvocationResponse'
    { standardOutputUrl =
        Prelude.Nothing,
      status = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      pluginName = Prelude.Nothing,
      comment = Prelude.Nothing,
      executionStartDateTime = Prelude.Nothing,
      standardErrorUrl = Prelude.Nothing,
      documentName = Prelude.Nothing,
      commandId = Prelude.Nothing,
      standardErrorContent = Prelude.Nothing,
      executionEndDateTime = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      cloudWatchOutputConfig = Prelude.Nothing,
      executionElapsedTime = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      standardOutputContent = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL for the complete text written by the plugin to stdout in Amazon
-- S3. If an S3 bucket was not specified, then this string is empty.
getCommandInvocationResponse_standardOutputUrl :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardOutputUrl = Lens.lens (\GetCommandInvocationResponse' {standardOutputUrl} -> standardOutputUrl) (\s@GetCommandInvocationResponse' {} a -> s {standardOutputUrl = a} :: GetCommandInvocationResponse)

-- | The status of this invocation plugin. This status can be different than
-- StatusDetails.
getCommandInvocationResponse_status :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe CommandInvocationStatus)
getCommandInvocationResponse_status = Lens.lens (\GetCommandInvocationResponse' {status} -> status) (\s@GetCommandInvocationResponse' {} a -> s {status = a} :: GetCommandInvocationResponse)

-- | The ID of the managed instance targeted by the command. A managed
-- instance can be an EC2 instance or an instance in your hybrid
-- environment that is configured for Systems Manager.
getCommandInvocationResponse_instanceId :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_instanceId = Lens.lens (\GetCommandInvocationResponse' {instanceId} -> instanceId) (\s@GetCommandInvocationResponse' {} a -> s {instanceId = a} :: GetCommandInvocationResponse)

-- | A detailed status of the command execution for an invocation.
-- StatusDetails includes more information than Status because it includes
-- states resulting from error and concurrency control parameters.
-- StatusDetails can show different results than Status. For more
-- information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /AWS Systems Manager User Guide/. StatusDetails can be one of the
-- following values:
--
-- -   Pending: The command has not been sent to the instance.
--
-- -   In Progress: The command has been sent to the instance but has not
--     reached a terminal state.
--
-- -   Delayed: The system attempted to send the command to the target, but
--     the target was not available. The instance might not be available
--     because of network issues, because the instance was stopped, or for
--     similar reasons. The system will try to send the command again.
--
-- -   Success: The command or plugin ran successfully. This is a terminal
--     state.
--
-- -   Delivery Timed Out: The command was not delivered to the instance
--     before the delivery timeout expired. Delivery timeouts do not count
--     against the parent command\'s MaxErrors limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: The command started to run on the instance, but
--     the execution was not complete before the timeout expired. Execution
--     timeouts count against the MaxErrors limit of the parent command.
--     This is a terminal state.
--
-- -   Failed: The command wasn\'t run successfully on the instance. For a
--     plugin, this indicates that the result code was not zero. For a
--     command invocation, this indicates that the result code for one or
--     more plugins was not zero. Invocation failures count against the
--     MaxErrors limit of the parent command. This is a terminal state.
--
-- -   Canceled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the instance. The
--     instance might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s MaxErrors
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its MaxErrors limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
getCommandInvocationResponse_statusDetails :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_statusDetails = Lens.lens (\GetCommandInvocationResponse' {statusDetails} -> statusDetails) (\s@GetCommandInvocationResponse' {} a -> s {statusDetails = a} :: GetCommandInvocationResponse)

-- | The name of the plugin for which you want detailed results. For example,
-- aws:RunShellScript is a plugin.
getCommandInvocationResponse_pluginName :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_pluginName = Lens.lens (\GetCommandInvocationResponse' {pluginName} -> pluginName) (\s@GetCommandInvocationResponse' {} a -> s {pluginName = a} :: GetCommandInvocationResponse)

-- | The comment text for the command.
getCommandInvocationResponse_comment :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_comment = Lens.lens (\GetCommandInvocationResponse' {comment} -> comment) (\s@GetCommandInvocationResponse' {} a -> s {comment = a} :: GetCommandInvocationResponse)

-- | The date and time the plugin started running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample AWS CLI command uses the @InvokedBefore@
-- filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
--
-- If the plugin has not started to run, the string is empty.
getCommandInvocationResponse_executionStartDateTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionStartDateTime = Lens.lens (\GetCommandInvocationResponse' {executionStartDateTime} -> executionStartDateTime) (\s@GetCommandInvocationResponse' {} a -> s {executionStartDateTime = a} :: GetCommandInvocationResponse)

-- | The URL for the complete text written by the plugin to stderr. If the
-- command has not finished running, then this string is empty.
getCommandInvocationResponse_standardErrorUrl :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardErrorUrl = Lens.lens (\GetCommandInvocationResponse' {standardErrorUrl} -> standardErrorUrl) (\s@GetCommandInvocationResponse' {} a -> s {standardErrorUrl = a} :: GetCommandInvocationResponse)

-- | The name of the document that was run. For example, AWS-RunShellScript.
getCommandInvocationResponse_documentName :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_documentName = Lens.lens (\GetCommandInvocationResponse' {documentName} -> documentName) (\s@GetCommandInvocationResponse' {} a -> s {documentName = a} :: GetCommandInvocationResponse)

-- | The parent command ID of the invocation plugin.
getCommandInvocationResponse_commandId :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_commandId = Lens.lens (\GetCommandInvocationResponse' {commandId} -> commandId) (\s@GetCommandInvocationResponse' {} a -> s {commandId = a} :: GetCommandInvocationResponse)

-- | The first 8,000 characters written by the plugin to stderr. If the
-- command has not finished running, then this string is empty.
getCommandInvocationResponse_standardErrorContent :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardErrorContent = Lens.lens (\GetCommandInvocationResponse' {standardErrorContent} -> standardErrorContent) (\s@GetCommandInvocationResponse' {} a -> s {standardErrorContent = a} :: GetCommandInvocationResponse)

-- | The date and time the plugin was finished running. Date and time are
-- written in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample AWS CLI command uses the @InvokedAfter@
-- filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
--
-- If the plugin has not started to run, the string is empty.
getCommandInvocationResponse_executionEndDateTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionEndDateTime = Lens.lens (\GetCommandInvocationResponse' {executionEndDateTime} -> executionEndDateTime) (\s@GetCommandInvocationResponse' {} a -> s {executionEndDateTime = a} :: GetCommandInvocationResponse)

-- | The error level response code for the plugin script. If the response
-- code is -1, then the command has not started running on the instance, or
-- it was not received by the instance.
getCommandInvocationResponse_responseCode :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Int)
getCommandInvocationResponse_responseCode = Lens.lens (\GetCommandInvocationResponse' {responseCode} -> responseCode) (\s@GetCommandInvocationResponse' {} a -> s {responseCode = a} :: GetCommandInvocationResponse)

-- | CloudWatch Logs information where Systems Manager sent the command
-- output.
getCommandInvocationResponse_cloudWatchOutputConfig :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe CloudWatchOutputConfig)
getCommandInvocationResponse_cloudWatchOutputConfig = Lens.lens (\GetCommandInvocationResponse' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@GetCommandInvocationResponse' {} a -> s {cloudWatchOutputConfig = a} :: GetCommandInvocationResponse)

-- | Duration since ExecutionStartDateTime.
getCommandInvocationResponse_executionElapsedTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionElapsedTime = Lens.lens (\GetCommandInvocationResponse' {executionElapsedTime} -> executionElapsedTime) (\s@GetCommandInvocationResponse' {} a -> s {executionElapsedTime = a} :: GetCommandInvocationResponse)

-- | The SSM document version used in the request.
getCommandInvocationResponse_documentVersion :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_documentVersion = Lens.lens (\GetCommandInvocationResponse' {documentVersion} -> documentVersion) (\s@GetCommandInvocationResponse' {} a -> s {documentVersion = a} :: GetCommandInvocationResponse)

-- | The first 24,000 characters written by the plugin to stdout. If the
-- command has not finished running, if ExecutionStatus is neither
-- Succeeded nor Failed, then this string is empty.
getCommandInvocationResponse_standardOutputContent :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardOutputContent = Lens.lens (\GetCommandInvocationResponse' {standardOutputContent} -> standardOutputContent) (\s@GetCommandInvocationResponse' {} a -> s {standardOutputContent = a} :: GetCommandInvocationResponse)

-- | The response's http status code.
getCommandInvocationResponse_httpStatus :: Lens.Lens' GetCommandInvocationResponse Prelude.Int
getCommandInvocationResponse_httpStatus = Lens.lens (\GetCommandInvocationResponse' {httpStatus} -> httpStatus) (\s@GetCommandInvocationResponse' {} a -> s {httpStatus = a} :: GetCommandInvocationResponse)

instance Prelude.NFData GetCommandInvocationResponse
