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
-- Module      : Amazonka.SSM.GetCommandInvocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about command execution for an invocation
-- or plugin.
--
-- @GetCommandInvocation@ only gives the execution status of a plugin in a
-- document. To get the command execution status on a specific managed
-- node, use ListCommandInvocations. To get the command execution status
-- across managed nodes, use ListCommands.
module Amazonka.SSM.GetCommandInvocation
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
    getCommandInvocationResponse_cloudWatchOutputConfig,
    getCommandInvocationResponse_commandId,
    getCommandInvocationResponse_comment,
    getCommandInvocationResponse_documentName,
    getCommandInvocationResponse_documentVersion,
    getCommandInvocationResponse_executionElapsedTime,
    getCommandInvocationResponse_executionEndDateTime,
    getCommandInvocationResponse_executionStartDateTime,
    getCommandInvocationResponse_instanceId,
    getCommandInvocationResponse_pluginName,
    getCommandInvocationResponse_responseCode,
    getCommandInvocationResponse_standardErrorContent,
    getCommandInvocationResponse_standardErrorUrl,
    getCommandInvocationResponse_standardOutputContent,
    getCommandInvocationResponse_standardOutputUrl,
    getCommandInvocationResponse_status,
    getCommandInvocationResponse_statusDetails,
    getCommandInvocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetCommandInvocation' smart constructor.
data GetCommandInvocation = GetCommandInvocation'
  { -- | The name of the step for which you want detailed results. If the
    -- document contains only one step, you can omit the name and details for
    -- that step. If the document contains more than one step, you must specify
    -- the name of the step for which you want to view details. Be sure to
    -- specify the name of the step, not the name of a plugin like
    -- @aws:RunShellScript@.
    --
    -- To find the @PluginName@, check the document content and find the name
    -- of the step you want details for. Alternatively, use
    -- ListCommandInvocations with the @CommandId@ and @Details@ parameters.
    -- The @PluginName@ is the @Name@ attribute of the @CommandPlugin@ object
    -- in the @CommandPlugins@ list.
    pluginName :: Prelude.Maybe Prelude.Text,
    -- | (Required) The parent command ID of the invocation plugin.
    commandId :: Prelude.Text,
    -- | (Required) The ID of the managed node targeted by the command. A
    -- /managed node/ can be an Amazon Elastic Compute Cloud (Amazon EC2)
    -- instance, edge device, and on-premises server or VM in your hybrid
    -- environment that is configured for Amazon Web Services Systems Manager.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommandInvocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pluginName', 'getCommandInvocation_pluginName' - The name of the step for which you want detailed results. If the
-- document contains only one step, you can omit the name and details for
-- that step. If the document contains more than one step, you must specify
-- the name of the step for which you want to view details. Be sure to
-- specify the name of the step, not the name of a plugin like
-- @aws:RunShellScript@.
--
-- To find the @PluginName@, check the document content and find the name
-- of the step you want details for. Alternatively, use
-- ListCommandInvocations with the @CommandId@ and @Details@ parameters.
-- The @PluginName@ is the @Name@ attribute of the @CommandPlugin@ object
-- in the @CommandPlugins@ list.
--
-- 'commandId', 'getCommandInvocation_commandId' - (Required) The parent command ID of the invocation plugin.
--
-- 'instanceId', 'getCommandInvocation_instanceId' - (Required) The ID of the managed node targeted by the command. A
-- /managed node/ can be an Amazon Elastic Compute Cloud (Amazon EC2)
-- instance, edge device, and on-premises server or VM in your hybrid
-- environment that is configured for Amazon Web Services Systems Manager.
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

-- | The name of the step for which you want detailed results. If the
-- document contains only one step, you can omit the name and details for
-- that step. If the document contains more than one step, you must specify
-- the name of the step for which you want to view details. Be sure to
-- specify the name of the step, not the name of a plugin like
-- @aws:RunShellScript@.
--
-- To find the @PluginName@, check the document content and find the name
-- of the step you want details for. Alternatively, use
-- ListCommandInvocations with the @CommandId@ and @Details@ parameters.
-- The @PluginName@ is the @Name@ attribute of the @CommandPlugin@ object
-- in the @CommandPlugins@ list.
getCommandInvocation_pluginName :: Lens.Lens' GetCommandInvocation (Prelude.Maybe Prelude.Text)
getCommandInvocation_pluginName = Lens.lens (\GetCommandInvocation' {pluginName} -> pluginName) (\s@GetCommandInvocation' {} a -> s {pluginName = a} :: GetCommandInvocation)

-- | (Required) The parent command ID of the invocation plugin.
getCommandInvocation_commandId :: Lens.Lens' GetCommandInvocation Prelude.Text
getCommandInvocation_commandId = Lens.lens (\GetCommandInvocation' {commandId} -> commandId) (\s@GetCommandInvocation' {} a -> s {commandId = a} :: GetCommandInvocation)

-- | (Required) The ID of the managed node targeted by the command. A
-- /managed node/ can be an Amazon Elastic Compute Cloud (Amazon EC2)
-- instance, edge device, and on-premises server or VM in your hybrid
-- environment that is configured for Amazon Web Services Systems Manager.
getCommandInvocation_instanceId :: Lens.Lens' GetCommandInvocation Prelude.Text
getCommandInvocation_instanceId = Lens.lens (\GetCommandInvocation' {instanceId} -> instanceId) (\s@GetCommandInvocation' {} a -> s {instanceId = a} :: GetCommandInvocation)

instance Core.AWSRequest GetCommandInvocation where
  type
    AWSResponse GetCommandInvocation =
      GetCommandInvocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommandInvocationResponse'
            Prelude.<$> (x Data..?> "CloudWatchOutputConfig")
            Prelude.<*> (x Data..?> "CommandId")
            Prelude.<*> (x Data..?> "Comment")
            Prelude.<*> (x Data..?> "DocumentName")
            Prelude.<*> (x Data..?> "DocumentVersion")
            Prelude.<*> (x Data..?> "ExecutionElapsedTime")
            Prelude.<*> (x Data..?> "ExecutionEndDateTime")
            Prelude.<*> (x Data..?> "ExecutionStartDateTime")
            Prelude.<*> (x Data..?> "InstanceId")
            Prelude.<*> (x Data..?> "PluginName")
            Prelude.<*> (x Data..?> "ResponseCode")
            Prelude.<*> (x Data..?> "StandardErrorContent")
            Prelude.<*> (x Data..?> "StandardErrorUrl")
            Prelude.<*> (x Data..?> "StandardOutputContent")
            Prelude.<*> (x Data..?> "StandardOutputUrl")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCommandInvocation where
  hashWithSalt _salt GetCommandInvocation' {..} =
    _salt `Prelude.hashWithSalt` pluginName
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetCommandInvocation where
  rnf GetCommandInvocation' {..} =
    Prelude.rnf pluginName
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders GetCommandInvocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetCommandInvocation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCommandInvocation where
  toJSON GetCommandInvocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PluginName" Data..=) Prelude.<$> pluginName,
            Prelude.Just ("CommandId" Data..= commandId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath GetCommandInvocation where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCommandInvocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommandInvocationResponse' smart constructor.
data GetCommandInvocationResponse = GetCommandInvocationResponse'
  { -- | Amazon CloudWatch Logs information where Systems Manager sent the
    -- command output.
    cloudWatchOutputConfig :: Prelude.Maybe CloudWatchOutputConfig,
    -- | The parent command ID of the invocation plugin.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | The comment text for the command.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The name of the document that was run. For example,
    -- @AWS-RunShellScript@.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The Systems Manager document (SSM document) version used in the request.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Duration since @ExecutionStartDateTime@.
    executionElapsedTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time the plugin finished running. Date and time are written
    -- in ISO 8601 format. For example, June 7, 2017 is represented as
    -- 2017-06-7. The following sample Amazon Web Services CLI command uses the
    -- @InvokedAfter@ filter.
    --
    -- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
    --
    -- If the plugin hasn\'t started to run, the string is empty.
    executionEndDateTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time the plugin started running. Date and time are written
    -- in ISO 8601 format. For example, June 7, 2017 is represented as
    -- 2017-06-7. The following sample Amazon Web Services CLI command uses the
    -- @InvokedBefore@ filter.
    --
    -- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
    --
    -- If the plugin hasn\'t started to run, the string is empty.
    executionStartDateTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the managed node targeted by the command. A /managed node/ can
    -- be an Amazon Elastic Compute Cloud (Amazon EC2) instance, edge device,
    -- or on-premises server or VM in your hybrid environment that is
    -- configured for Amazon Web Services Systems Manager.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the plugin, or /step name/, for which details are reported.
    -- For example, @aws:RunShellScript@ is a plugin.
    pluginName :: Prelude.Maybe Prelude.Text,
    -- | The error level response code for the plugin script. If the response
    -- code is @-1@, then the command hasn\'t started running on the managed
    -- node, or it wasn\'t received by the node.
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | The first 8,000 characters written by the plugin to @stderr@. If the
    -- command hasn\'t finished running, then this string is empty.
    standardErrorContent :: Prelude.Maybe Prelude.Text,
    -- | The URL for the complete text written by the plugin to @stderr@. If the
    -- command hasn\'t finished running, then this string is empty.
    standardErrorUrl :: Prelude.Maybe Prelude.Text,
    -- | The first 24,000 characters written by the plugin to @stdout@. If the
    -- command hasn\'t finished running, if @ExecutionStatus@ is neither
    -- Succeeded nor Failed, then this string is empty.
    standardOutputContent :: Prelude.Maybe Prelude.Text,
    -- | The URL for the complete text written by the plugin to @stdout@ in
    -- Amazon Simple Storage Service (Amazon S3). If an S3 bucket wasn\'t
    -- specified, then this string is empty.
    standardOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of this invocation plugin. This status can be different than
    -- @StatusDetails@.
    status :: Prelude.Maybe CommandInvocationStatus,
    -- | A detailed status of the command execution for an invocation.
    -- @StatusDetails@ includes more information than @Status@ because it
    -- includes states resulting from error and concurrency control parameters.
    -- @StatusDetails@ can show different results than @Status@. For more
    -- information about these statuses, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
    -- in the /Amazon Web Services Systems Manager User Guide/. @StatusDetails@
    -- can be one of the following values:
    --
    -- -   Pending: The command hasn\'t been sent to the managed node.
    --
    -- -   In Progress: The command has been sent to the managed node but
    --     hasn\'t reached a terminal state.
    --
    -- -   Delayed: The system attempted to send the command to the target, but
    --     the target wasn\'t available. The managed node might not be
    --     available because of network issues, because the node was stopped,
    --     or for similar reasons. The system will try to send the command
    --     again.
    --
    -- -   Success: The command or plugin ran successfully. This is a terminal
    --     state.
    --
    -- -   Delivery Timed Out: The command wasn\'t delivered to the managed
    --     node before the delivery timeout expired. Delivery timeouts don\'t
    --     count against the parent command\'s @MaxErrors@ limit, but they do
    --     contribute to whether the parent command status is Success or
    --     Incomplete. This is a terminal state.
    --
    -- -   Execution Timed Out: The command started to run on the managed node,
    --     but the execution wasn\'t complete before the timeout expired.
    --     Execution timeouts count against the @MaxErrors@ limit of the parent
    --     command. This is a terminal state.
    --
    -- -   Failed: The command wasn\'t run successfully on the managed node.
    --     For a plugin, this indicates that the result code wasn\'t zero. For
    --     a command invocation, this indicates that the result code for one or
    --     more plugins wasn\'t zero. Invocation failures count against the
    --     @MaxErrors@ limit of the parent command. This is a terminal state.
    --
    -- -   Cancelled: The command was terminated before it was completed. This
    --     is a terminal state.
    --
    -- -   Undeliverable: The command can\'t be delivered to the managed node.
    --     The node might not exist or might not be responding. Undeliverable
    --     invocations don\'t count against the parent command\'s @MaxErrors@
    --     limit and don\'t contribute to whether the parent command status is
    --     Success or Incomplete. This is a terminal state.
    --
    -- -   Terminated: The parent command exceeded its @MaxErrors@ limit and
    --     subsequent command invocations were canceled by the system. This is
    --     a terminal state.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCommandInvocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchOutputConfig', 'getCommandInvocationResponse_cloudWatchOutputConfig' - Amazon CloudWatch Logs information where Systems Manager sent the
-- command output.
--
-- 'commandId', 'getCommandInvocationResponse_commandId' - The parent command ID of the invocation plugin.
--
-- 'comment', 'getCommandInvocationResponse_comment' - The comment text for the command.
--
-- 'documentName', 'getCommandInvocationResponse_documentName' - The name of the document that was run. For example,
-- @AWS-RunShellScript@.
--
-- 'documentVersion', 'getCommandInvocationResponse_documentVersion' - The Systems Manager document (SSM document) version used in the request.
--
-- 'executionElapsedTime', 'getCommandInvocationResponse_executionElapsedTime' - Duration since @ExecutionStartDateTime@.
--
-- 'executionEndDateTime', 'getCommandInvocationResponse_executionEndDateTime' - The date and time the plugin finished running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample Amazon Web Services CLI command uses the
-- @InvokedAfter@ filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
--
-- If the plugin hasn\'t started to run, the string is empty.
--
-- 'executionStartDateTime', 'getCommandInvocationResponse_executionStartDateTime' - The date and time the plugin started running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample Amazon Web Services CLI command uses the
-- @InvokedBefore@ filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
--
-- If the plugin hasn\'t started to run, the string is empty.
--
-- 'instanceId', 'getCommandInvocationResponse_instanceId' - The ID of the managed node targeted by the command. A /managed node/ can
-- be an Amazon Elastic Compute Cloud (Amazon EC2) instance, edge device,
-- or on-premises server or VM in your hybrid environment that is
-- configured for Amazon Web Services Systems Manager.
--
-- 'pluginName', 'getCommandInvocationResponse_pluginName' - The name of the plugin, or /step name/, for which details are reported.
-- For example, @aws:RunShellScript@ is a plugin.
--
-- 'responseCode', 'getCommandInvocationResponse_responseCode' - The error level response code for the plugin script. If the response
-- code is @-1@, then the command hasn\'t started running on the managed
-- node, or it wasn\'t received by the node.
--
-- 'standardErrorContent', 'getCommandInvocationResponse_standardErrorContent' - The first 8,000 characters written by the plugin to @stderr@. If the
-- command hasn\'t finished running, then this string is empty.
--
-- 'standardErrorUrl', 'getCommandInvocationResponse_standardErrorUrl' - The URL for the complete text written by the plugin to @stderr@. If the
-- command hasn\'t finished running, then this string is empty.
--
-- 'standardOutputContent', 'getCommandInvocationResponse_standardOutputContent' - The first 24,000 characters written by the plugin to @stdout@. If the
-- command hasn\'t finished running, if @ExecutionStatus@ is neither
-- Succeeded nor Failed, then this string is empty.
--
-- 'standardOutputUrl', 'getCommandInvocationResponse_standardOutputUrl' - The URL for the complete text written by the plugin to @stdout@ in
-- Amazon Simple Storage Service (Amazon S3). If an S3 bucket wasn\'t
-- specified, then this string is empty.
--
-- 'status', 'getCommandInvocationResponse_status' - The status of this invocation plugin. This status can be different than
-- @StatusDetails@.
--
-- 'statusDetails', 'getCommandInvocationResponse_statusDetails' - A detailed status of the command execution for an invocation.
-- @StatusDetails@ includes more information than @Status@ because it
-- includes states resulting from error and concurrency control parameters.
-- @StatusDetails@ can show different results than @Status@. For more
-- information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. @StatusDetails@
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to the managed node.
--
-- -   In Progress: The command has been sent to the managed node but
--     hasn\'t reached a terminal state.
--
-- -   Delayed: The system attempted to send the command to the target, but
--     the target wasn\'t available. The managed node might not be
--     available because of network issues, because the node was stopped,
--     or for similar reasons. The system will try to send the command
--     again.
--
-- -   Success: The command or plugin ran successfully. This is a terminal
--     state.
--
-- -   Delivery Timed Out: The command wasn\'t delivered to the managed
--     node before the delivery timeout expired. Delivery timeouts don\'t
--     count against the parent command\'s @MaxErrors@ limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: The command started to run on the managed node,
--     but the execution wasn\'t complete before the timeout expired.
--     Execution timeouts count against the @MaxErrors@ limit of the parent
--     command. This is a terminal state.
--
-- -   Failed: The command wasn\'t run successfully on the managed node.
--     For a plugin, this indicates that the result code wasn\'t zero. For
--     a command invocation, this indicates that the result code for one or
--     more plugins wasn\'t zero. Invocation failures count against the
--     @MaxErrors@ limit of the parent command. This is a terminal state.
--
-- -   Cancelled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the managed node.
--     The node might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s @MaxErrors@
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its @MaxErrors@ limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
--
-- 'httpStatus', 'getCommandInvocationResponse_httpStatus' - The response's http status code.
newGetCommandInvocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCommandInvocationResponse
newGetCommandInvocationResponse pHttpStatus_ =
  GetCommandInvocationResponse'
    { cloudWatchOutputConfig =
        Prelude.Nothing,
      commandId = Prelude.Nothing,
      comment = Prelude.Nothing,
      documentName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      executionElapsedTime = Prelude.Nothing,
      executionEndDateTime = Prelude.Nothing,
      executionStartDateTime = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      pluginName = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      standardErrorContent = Prelude.Nothing,
      standardErrorUrl = Prelude.Nothing,
      standardOutputContent = Prelude.Nothing,
      standardOutputUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon CloudWatch Logs information where Systems Manager sent the
-- command output.
getCommandInvocationResponse_cloudWatchOutputConfig :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe CloudWatchOutputConfig)
getCommandInvocationResponse_cloudWatchOutputConfig = Lens.lens (\GetCommandInvocationResponse' {cloudWatchOutputConfig} -> cloudWatchOutputConfig) (\s@GetCommandInvocationResponse' {} a -> s {cloudWatchOutputConfig = a} :: GetCommandInvocationResponse)

-- | The parent command ID of the invocation plugin.
getCommandInvocationResponse_commandId :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_commandId = Lens.lens (\GetCommandInvocationResponse' {commandId} -> commandId) (\s@GetCommandInvocationResponse' {} a -> s {commandId = a} :: GetCommandInvocationResponse)

-- | The comment text for the command.
getCommandInvocationResponse_comment :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_comment = Lens.lens (\GetCommandInvocationResponse' {comment} -> comment) (\s@GetCommandInvocationResponse' {} a -> s {comment = a} :: GetCommandInvocationResponse)

-- | The name of the document that was run. For example,
-- @AWS-RunShellScript@.
getCommandInvocationResponse_documentName :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_documentName = Lens.lens (\GetCommandInvocationResponse' {documentName} -> documentName) (\s@GetCommandInvocationResponse' {} a -> s {documentName = a} :: GetCommandInvocationResponse)

-- | The Systems Manager document (SSM document) version used in the request.
getCommandInvocationResponse_documentVersion :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_documentVersion = Lens.lens (\GetCommandInvocationResponse' {documentVersion} -> documentVersion) (\s@GetCommandInvocationResponse' {} a -> s {documentVersion = a} :: GetCommandInvocationResponse)

-- | Duration since @ExecutionStartDateTime@.
getCommandInvocationResponse_executionElapsedTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionElapsedTime = Lens.lens (\GetCommandInvocationResponse' {executionElapsedTime} -> executionElapsedTime) (\s@GetCommandInvocationResponse' {} a -> s {executionElapsedTime = a} :: GetCommandInvocationResponse)

-- | The date and time the plugin finished running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample Amazon Web Services CLI command uses the
-- @InvokedAfter@ filter.
--
-- @aws ssm list-commands --filters key=InvokedAfter,value=2017-06-07T00:00:00Z@
--
-- If the plugin hasn\'t started to run, the string is empty.
getCommandInvocationResponse_executionEndDateTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionEndDateTime = Lens.lens (\GetCommandInvocationResponse' {executionEndDateTime} -> executionEndDateTime) (\s@GetCommandInvocationResponse' {} a -> s {executionEndDateTime = a} :: GetCommandInvocationResponse)

-- | The date and time the plugin started running. Date and time are written
-- in ISO 8601 format. For example, June 7, 2017 is represented as
-- 2017-06-7. The following sample Amazon Web Services CLI command uses the
-- @InvokedBefore@ filter.
--
-- @aws ssm list-commands --filters key=InvokedBefore,value=2017-06-07T00:00:00Z@
--
-- If the plugin hasn\'t started to run, the string is empty.
getCommandInvocationResponse_executionStartDateTime :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_executionStartDateTime = Lens.lens (\GetCommandInvocationResponse' {executionStartDateTime} -> executionStartDateTime) (\s@GetCommandInvocationResponse' {} a -> s {executionStartDateTime = a} :: GetCommandInvocationResponse)

-- | The ID of the managed node targeted by the command. A /managed node/ can
-- be an Amazon Elastic Compute Cloud (Amazon EC2) instance, edge device,
-- or on-premises server or VM in your hybrid environment that is
-- configured for Amazon Web Services Systems Manager.
getCommandInvocationResponse_instanceId :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_instanceId = Lens.lens (\GetCommandInvocationResponse' {instanceId} -> instanceId) (\s@GetCommandInvocationResponse' {} a -> s {instanceId = a} :: GetCommandInvocationResponse)

-- | The name of the plugin, or /step name/, for which details are reported.
-- For example, @aws:RunShellScript@ is a plugin.
getCommandInvocationResponse_pluginName :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_pluginName = Lens.lens (\GetCommandInvocationResponse' {pluginName} -> pluginName) (\s@GetCommandInvocationResponse' {} a -> s {pluginName = a} :: GetCommandInvocationResponse)

-- | The error level response code for the plugin script. If the response
-- code is @-1@, then the command hasn\'t started running on the managed
-- node, or it wasn\'t received by the node.
getCommandInvocationResponse_responseCode :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Int)
getCommandInvocationResponse_responseCode = Lens.lens (\GetCommandInvocationResponse' {responseCode} -> responseCode) (\s@GetCommandInvocationResponse' {} a -> s {responseCode = a} :: GetCommandInvocationResponse)

-- | The first 8,000 characters written by the plugin to @stderr@. If the
-- command hasn\'t finished running, then this string is empty.
getCommandInvocationResponse_standardErrorContent :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardErrorContent = Lens.lens (\GetCommandInvocationResponse' {standardErrorContent} -> standardErrorContent) (\s@GetCommandInvocationResponse' {} a -> s {standardErrorContent = a} :: GetCommandInvocationResponse)

-- | The URL for the complete text written by the plugin to @stderr@. If the
-- command hasn\'t finished running, then this string is empty.
getCommandInvocationResponse_standardErrorUrl :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardErrorUrl = Lens.lens (\GetCommandInvocationResponse' {standardErrorUrl} -> standardErrorUrl) (\s@GetCommandInvocationResponse' {} a -> s {standardErrorUrl = a} :: GetCommandInvocationResponse)

-- | The first 24,000 characters written by the plugin to @stdout@. If the
-- command hasn\'t finished running, if @ExecutionStatus@ is neither
-- Succeeded nor Failed, then this string is empty.
getCommandInvocationResponse_standardOutputContent :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardOutputContent = Lens.lens (\GetCommandInvocationResponse' {standardOutputContent} -> standardOutputContent) (\s@GetCommandInvocationResponse' {} a -> s {standardOutputContent = a} :: GetCommandInvocationResponse)

-- | The URL for the complete text written by the plugin to @stdout@ in
-- Amazon Simple Storage Service (Amazon S3). If an S3 bucket wasn\'t
-- specified, then this string is empty.
getCommandInvocationResponse_standardOutputUrl :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_standardOutputUrl = Lens.lens (\GetCommandInvocationResponse' {standardOutputUrl} -> standardOutputUrl) (\s@GetCommandInvocationResponse' {} a -> s {standardOutputUrl = a} :: GetCommandInvocationResponse)

-- | The status of this invocation plugin. This status can be different than
-- @StatusDetails@.
getCommandInvocationResponse_status :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe CommandInvocationStatus)
getCommandInvocationResponse_status = Lens.lens (\GetCommandInvocationResponse' {status} -> status) (\s@GetCommandInvocationResponse' {} a -> s {status = a} :: GetCommandInvocationResponse)

-- | A detailed status of the command execution for an invocation.
-- @StatusDetails@ includes more information than @Status@ because it
-- includes states resulting from error and concurrency control parameters.
-- @StatusDetails@ can show different results than @Status@. For more
-- information about these statuses, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Understanding command statuses>
-- in the /Amazon Web Services Systems Manager User Guide/. @StatusDetails@
-- can be one of the following values:
--
-- -   Pending: The command hasn\'t been sent to the managed node.
--
-- -   In Progress: The command has been sent to the managed node but
--     hasn\'t reached a terminal state.
--
-- -   Delayed: The system attempted to send the command to the target, but
--     the target wasn\'t available. The managed node might not be
--     available because of network issues, because the node was stopped,
--     or for similar reasons. The system will try to send the command
--     again.
--
-- -   Success: The command or plugin ran successfully. This is a terminal
--     state.
--
-- -   Delivery Timed Out: The command wasn\'t delivered to the managed
--     node before the delivery timeout expired. Delivery timeouts don\'t
--     count against the parent command\'s @MaxErrors@ limit, but they do
--     contribute to whether the parent command status is Success or
--     Incomplete. This is a terminal state.
--
-- -   Execution Timed Out: The command started to run on the managed node,
--     but the execution wasn\'t complete before the timeout expired.
--     Execution timeouts count against the @MaxErrors@ limit of the parent
--     command. This is a terminal state.
--
-- -   Failed: The command wasn\'t run successfully on the managed node.
--     For a plugin, this indicates that the result code wasn\'t zero. For
--     a command invocation, this indicates that the result code for one or
--     more plugins wasn\'t zero. Invocation failures count against the
--     @MaxErrors@ limit of the parent command. This is a terminal state.
--
-- -   Cancelled: The command was terminated before it was completed. This
--     is a terminal state.
--
-- -   Undeliverable: The command can\'t be delivered to the managed node.
--     The node might not exist or might not be responding. Undeliverable
--     invocations don\'t count against the parent command\'s @MaxErrors@
--     limit and don\'t contribute to whether the parent command status is
--     Success or Incomplete. This is a terminal state.
--
-- -   Terminated: The parent command exceeded its @MaxErrors@ limit and
--     subsequent command invocations were canceled by the system. This is
--     a terminal state.
getCommandInvocationResponse_statusDetails :: Lens.Lens' GetCommandInvocationResponse (Prelude.Maybe Prelude.Text)
getCommandInvocationResponse_statusDetails = Lens.lens (\GetCommandInvocationResponse' {statusDetails} -> statusDetails) (\s@GetCommandInvocationResponse' {} a -> s {statusDetails = a} :: GetCommandInvocationResponse)

-- | The response's http status code.
getCommandInvocationResponse_httpStatus :: Lens.Lens' GetCommandInvocationResponse Prelude.Int
getCommandInvocationResponse_httpStatus = Lens.lens (\GetCommandInvocationResponse' {httpStatus} -> httpStatus) (\s@GetCommandInvocationResponse' {} a -> s {httpStatus = a} :: GetCommandInvocationResponse)

instance Prelude.NFData GetCommandInvocationResponse where
  rnf GetCommandInvocationResponse' {..} =
    Prelude.rnf cloudWatchOutputConfig
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf executionElapsedTime
      `Prelude.seq` Prelude.rnf executionEndDateTime
      `Prelude.seq` Prelude.rnf executionStartDateTime
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf pluginName
      `Prelude.seq` Prelude.rnf responseCode
      `Prelude.seq` Prelude.rnf standardErrorContent
      `Prelude.seq` Prelude.rnf standardErrorUrl
      `Prelude.seq` Prelude.rnf standardOutputContent
      `Prelude.seq` Prelude.rnf standardOutputUrl
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf httpStatus
