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
-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invokes a Lambda function. You can invoke a function synchronously (and
-- wait for the response), or asynchronously. To invoke a function
-- asynchronously, set @InvocationType@ to @Event@.
--
-- For
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-sync.html synchronous invocation>,
-- details about the function response, including errors, are included in
-- the response body and headers. For either invocation type, you can find
-- more information in the
-- <https://docs.aws.amazon.com/lambda/latest/dg/monitoring-functions.html execution log>
-- and
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-x-ray.html trace>.
--
-- When an error occurs, your function may be invoked multiple times. Retry
-- behavior varies by error type, client, event source, and invocation
-- type. For example, if you invoke a function asynchronously and it
-- returns an error, Lambda executes the function up to two more times. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/retries-on-errors.html Retry Behavior>.
--
-- For
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html asynchronous invocation>,
-- Lambda adds events to a queue before sending them to your function. If
-- your function does not have enough capacity to keep up with the queue,
-- events may be lost. Occasionally, your function may receive the same
-- event multiple times, even if no error occurs. To retain events that
-- were not processed, configure your function with a
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue>.
--
-- The status code in the API response doesn\'t reflect function errors.
-- Error codes are reserved for errors that prevent your function from
-- executing, such as permissions errors,
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limit errors>,
-- or issues with your function\'s code and configuration. For example,
-- Lambda returns @TooManyRequestsException@ if executing the function
-- would cause you to exceed a concurrency limit at either the account
-- level (@ConcurrentInvocationLimitExceeded@) or function level
-- (@ReservedFunctionConcurrentInvocationLimitExceeded@).
--
-- For functions with a long timeout, your client might be disconnected
-- during synchronous invocation while it waits for a response. Configure
-- your HTTP client, SDK, firewall, proxy, or operating system to allow for
-- long connections with timeout or keep-alive settings.
--
-- This operation requires permission for the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_awslambda.html lambda:InvokeFunction>
-- action.
module Network.AWS.Lambda.Invoke
  ( -- * Creating a Request
    Invoke (..),
    newInvoke,

    -- * Request Lenses
    invoke_logType,
    invoke_invocationType,
    invoke_qualifier,
    invoke_clientContext,
    invoke_functionName,
    invoke_payload,

    -- * Destructuring the Response
    InvokeResponse (..),
    newInvokeResponse,

    -- * Response Lenses
    invokeResponse_payload,
    invokeResponse_logResult,
    invokeResponse_executedVersion,
    invokeResponse_functionError,
    invokeResponse_statusCode,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInvoke' smart constructor.
data Invoke = Invoke'
  { -- | Set to @Tail@ to include the execution log in the response.
    logType :: Core.Maybe LogType,
    -- | Choose from the following options.
    --
    -- -   @RequestResponse@ (default) - Invoke the function synchronously.
    --     Keep the connection open until the function returns a response or
    --     times out. The API response includes the function response and
    --     additional data.
    --
    -- -   @Event@ - Invoke the function asynchronously. Send events that fail
    --     multiple times to the function\'s dead-letter queue (if it\'s
    --     configured). The API response only includes a status code.
    --
    -- -   @DryRun@ - Validate parameter values and verify that the user or
    --     role has permission to invoke the function.
    invocationType :: Core.Maybe InvocationType,
    -- | Specify a version or alias to invoke a published version of the
    -- function.
    qualifier :: Core.Maybe Core.Text,
    -- | Up to 3583 bytes of base64-encoded data about the invoking client to
    -- pass to the function in the context object.
    clientContext :: Core.Maybe Core.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Core.Text,
    -- | The JSON that you want to provide to your Lambda function as input.
    payload :: Core.ByteString
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Invoke' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logType', 'invoke_logType' - Set to @Tail@ to include the execution log in the response.
--
-- 'invocationType', 'invoke_invocationType' - Choose from the following options.
--
-- -   @RequestResponse@ (default) - Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API response includes the function response and
--     additional data.
--
-- -   @Event@ - Invoke the function asynchronously. Send events that fail
--     multiple times to the function\'s dead-letter queue (if it\'s
--     configured). The API response only includes a status code.
--
-- -   @DryRun@ - Validate parameter values and verify that the user or
--     role has permission to invoke the function.
--
-- 'qualifier', 'invoke_qualifier' - Specify a version or alias to invoke a published version of the
-- function.
--
-- 'clientContext', 'invoke_clientContext' - Up to 3583 bytes of base64-encoded data about the invoking client to
-- pass to the function in the context object.
--
-- 'functionName', 'invoke_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
--
-- 'payload', 'invoke_payload' - The JSON that you want to provide to your Lambda function as input.
newInvoke ::
  -- | 'functionName'
  Core.Text ->
  -- | 'payload'
  Core.ByteString ->
  Invoke
newInvoke pFunctionName_ pPayload_ =
  Invoke'
    { logType = Core.Nothing,
      invocationType = Core.Nothing,
      qualifier = Core.Nothing,
      clientContext = Core.Nothing,
      functionName = pFunctionName_,
      payload = pPayload_
    }

-- | Set to @Tail@ to include the execution log in the response.
invoke_logType :: Lens.Lens' Invoke (Core.Maybe LogType)
invoke_logType = Lens.lens (\Invoke' {logType} -> logType) (\s@Invoke' {} a -> s {logType = a} :: Invoke)

-- | Choose from the following options.
--
-- -   @RequestResponse@ (default) - Invoke the function synchronously.
--     Keep the connection open until the function returns a response or
--     times out. The API response includes the function response and
--     additional data.
--
-- -   @Event@ - Invoke the function asynchronously. Send events that fail
--     multiple times to the function\'s dead-letter queue (if it\'s
--     configured). The API response only includes a status code.
--
-- -   @DryRun@ - Validate parameter values and verify that the user or
--     role has permission to invoke the function.
invoke_invocationType :: Lens.Lens' Invoke (Core.Maybe InvocationType)
invoke_invocationType = Lens.lens (\Invoke' {invocationType} -> invocationType) (\s@Invoke' {} a -> s {invocationType = a} :: Invoke)

-- | Specify a version or alias to invoke a published version of the
-- function.
invoke_qualifier :: Lens.Lens' Invoke (Core.Maybe Core.Text)
invoke_qualifier = Lens.lens (\Invoke' {qualifier} -> qualifier) (\s@Invoke' {} a -> s {qualifier = a} :: Invoke)

-- | Up to 3583 bytes of base64-encoded data about the invoking client to
-- pass to the function in the context object.
invoke_clientContext :: Lens.Lens' Invoke (Core.Maybe Core.Text)
invoke_clientContext = Lens.lens (\Invoke' {clientContext} -> clientContext) (\s@Invoke' {} a -> s {clientContext = a} :: Invoke)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
invoke_functionName :: Lens.Lens' Invoke Core.Text
invoke_functionName = Lens.lens (\Invoke' {functionName} -> functionName) (\s@Invoke' {} a -> s {functionName = a} :: Invoke)

-- | The JSON that you want to provide to your Lambda function as input.
invoke_payload :: Lens.Lens' Invoke Core.ByteString
invoke_payload = Lens.lens (\Invoke' {payload} -> payload) (\s@Invoke' {} a -> s {payload = a} :: Invoke)

instance Core.AWSRequest Invoke where
  type AWSResponse Invoke = InvokeResponse
  request = Request.postBody defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          InvokeResponse'
            Core.<$> (Core.pure (Core.Just x))
            Core.<*> (h Core..#? "X-Amz-Log-Result")
            Core.<*> (h Core..#? "X-Amz-Executed-Version")
            Core.<*> (h Core..#? "X-Amz-Function-Error")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable Invoke

instance Core.NFData Invoke

instance Core.ToBody Invoke where
  toBody Invoke' {..} = Core.toBody payload

instance Core.ToHeaders Invoke where
  toHeaders Invoke' {..} =
    Core.mconcat
      [ "X-Amz-Log-Type" Core.=# logType,
        "X-Amz-Invocation-Type" Core.=# invocationType,
        "X-Amz-Client-Context" Core.=# clientContext
      ]

instance Core.ToPath Invoke where
  toPath Invoke' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/invocations"
      ]

instance Core.ToQuery Invoke where
  toQuery Invoke' {..} =
    Core.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newInvokeResponse' smart constructor.
data InvokeResponse = InvokeResponse'
  { -- | The response from the function, or an error object.
    payload :: Core.Maybe Core.ByteString,
    -- | The last 4 KB of the execution log, which is base64 encoded.
    logResult :: Core.Maybe Core.Text,
    -- | The version of the function that executed. When you invoke a function
    -- with an alias, this indicates which version the alias resolved to.
    executedVersion :: Core.Maybe Core.Text,
    -- | If present, indicates that an error occurred during function execution.
    -- Details about the error are included in the response payload.
    functionError :: Core.Maybe Core.Text,
    -- | The HTTP status code is in the 200 range for a successful request. For
    -- the @RequestResponse@ invocation type, this status code is 200. For the
    -- @Event@ invocation type, this status code is 202. For the @DryRun@
    -- invocation type, the status code is 204.
    statusCode :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InvokeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'invokeResponse_payload' - The response from the function, or an error object.
--
-- 'logResult', 'invokeResponse_logResult' - The last 4 KB of the execution log, which is base64 encoded.
--
-- 'executedVersion', 'invokeResponse_executedVersion' - The version of the function that executed. When you invoke a function
-- with an alias, this indicates which version the alias resolved to.
--
-- 'functionError', 'invokeResponse_functionError' - If present, indicates that an error occurred during function execution.
-- Details about the error are included in the response payload.
--
-- 'statusCode', 'invokeResponse_statusCode' - The HTTP status code is in the 200 range for a successful request. For
-- the @RequestResponse@ invocation type, this status code is 200. For the
-- @Event@ invocation type, this status code is 202. For the @DryRun@
-- invocation type, the status code is 204.
newInvokeResponse ::
  -- | 'statusCode'
  Core.Int ->
  InvokeResponse
newInvokeResponse pStatusCode_ =
  InvokeResponse'
    { payload = Core.Nothing,
      logResult = Core.Nothing,
      executedVersion = Core.Nothing,
      functionError = Core.Nothing,
      statusCode = pStatusCode_
    }

-- | The response from the function, or an error object.
invokeResponse_payload :: Lens.Lens' InvokeResponse (Core.Maybe Core.ByteString)
invokeResponse_payload = Lens.lens (\InvokeResponse' {payload} -> payload) (\s@InvokeResponse' {} a -> s {payload = a} :: InvokeResponse)

-- | The last 4 KB of the execution log, which is base64 encoded.
invokeResponse_logResult :: Lens.Lens' InvokeResponse (Core.Maybe Core.Text)
invokeResponse_logResult = Lens.lens (\InvokeResponse' {logResult} -> logResult) (\s@InvokeResponse' {} a -> s {logResult = a} :: InvokeResponse)

-- | The version of the function that executed. When you invoke a function
-- with an alias, this indicates which version the alias resolved to.
invokeResponse_executedVersion :: Lens.Lens' InvokeResponse (Core.Maybe Core.Text)
invokeResponse_executedVersion = Lens.lens (\InvokeResponse' {executedVersion} -> executedVersion) (\s@InvokeResponse' {} a -> s {executedVersion = a} :: InvokeResponse)

-- | If present, indicates that an error occurred during function execution.
-- Details about the error are included in the response payload.
invokeResponse_functionError :: Lens.Lens' InvokeResponse (Core.Maybe Core.Text)
invokeResponse_functionError = Lens.lens (\InvokeResponse' {functionError} -> functionError) (\s@InvokeResponse' {} a -> s {functionError = a} :: InvokeResponse)

-- | The HTTP status code is in the 200 range for a successful request. For
-- the @RequestResponse@ invocation type, this status code is 200. For the
-- @Event@ invocation type, this status code is 202. For the @DryRun@
-- invocation type, the status code is 204.
invokeResponse_statusCode :: Lens.Lens' InvokeResponse Core.Int
invokeResponse_statusCode = Lens.lens (\InvokeResponse' {statusCode} -> statusCode) (\s@InvokeResponse' {} a -> s {statusCode = a} :: InvokeResponse)

instance Core.NFData InvokeResponse
