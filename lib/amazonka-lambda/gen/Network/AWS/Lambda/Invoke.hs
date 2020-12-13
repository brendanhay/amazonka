{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invokes a Lambda function. You can invoke a function synchronously (and wait for the response), or asynchronously. To invoke a function asynchronously, set @InvocationType@ to @Event@ .
--
-- For <https://docs.aws.amazon.com/lambda/latest/dg/invocation-sync.html synchronous invocation> , details about the function response, including errors, are included in the response body and headers. For either invocation type, you can find more information in the <https://docs.aws.amazon.com/lambda/latest/dg/monitoring-functions.html execution log> and <https://docs.aws.amazon.com/lambda/latest/dg/lambda-x-ray.html trace> .
-- When an error occurs, your function may be invoked multiple times. Retry behavior varies by error type, client, event source, and invocation type. For example, if you invoke a function asynchronously and it returns an error, Lambda executes the function up to two more times. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/retries-on-errors.html Retry Behavior> .
-- For <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html asynchronous invocation> , Lambda adds events to a queue before sending them to your function. If your function does not have enough capacity to keep up with the queue, events may be lost. Occasionally, your function may receive the same event multiple times, even if no error occurs. To retain events that were not processed, configure your function with a <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue> .
-- The status code in the API response doesn't reflect function errors. Error codes are reserved for errors that prevent your function from executing, such as permissions errors, <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limit errors> , or issues with your function's code and configuration. For example, Lambda returns @TooManyRequestsException@ if executing the function would cause you to exceed a concurrency limit at either the account level (@ConcurrentInvocationLimitExceeded@ ) or function level (@ReservedFunctionConcurrentInvocationLimitExceeded@ ).
-- For functions with a long timeout, your client might be disconnected during synchronous invocation while it waits for a response. Configure your HTTP client, SDK, firewall, proxy, or operating system to allow for long connections with timeout or keep-alive settings.
-- This operation requires permission for the <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_awslambda.html lambda:InvokeFunction> action.
module Network.AWS.Lambda.Invoke
  ( -- * Creating a request
    Invoke (..),
    mkInvoke,

    -- ** Request lenses
    iInvocationType,
    iPayload,
    iLogType,
    iFunctionName,
    iQualifier,
    iClientContext,

    -- * Destructuring the response
    InvokeResponse (..),
    mkInvokeResponse,

    -- ** Response lenses
    irsFunctionError,
    irsLogResult,
    irsPayload,
    irsExecutedVersion,
    irsStatusCode,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInvoke' smart constructor.
data Invoke = Invoke'
  { -- | Choose from the following options.
    --
    --
    --     * @RequestResponse@ (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.
    --
    --
    --     * @Event@ - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.
    --
    --
    --     * @DryRun@ - Validate parameter values and verify that the user or role has permission to invoke the function.
    invocationType :: Lude.Maybe InvocationType,
    -- | The JSON that you want to provide to your Lambda function as input.
    payload :: Lude.ByteString,
    -- | Set to @Tail@ to include the execution log in the response.
    logType :: Lude.Maybe LogType,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Lude.Text,
    -- | Specify a version or alias to invoke a published version of the function.
    qualifier :: Lude.Maybe Lude.Text,
    -- | Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
    clientContext :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Invoke' with the minimum fields required to make a request.
--
-- * 'invocationType' - Choose from the following options.
--
--
--     * @RequestResponse@ (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.
--
--
--     * @Event@ - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.
--
--
--     * @DryRun@ - Validate parameter values and verify that the user or role has permission to invoke the function.
--
--
-- * 'payload' - The JSON that you want to provide to your Lambda function as input.
-- * 'logType' - Set to @Tail@ to include the execution log in the response.
-- * 'functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - Specify a version or alias to invoke a published version of the function.
-- * 'clientContext' - Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
mkInvoke ::
  -- | 'payload'
  Lude.ByteString ->
  -- | 'functionName'
  Lude.Text ->
  Invoke
mkInvoke pPayload_ pFunctionName_ =
  Invoke'
    { invocationType = Lude.Nothing,
      payload = pPayload_,
      logType = Lude.Nothing,
      functionName = pFunctionName_,
      qualifier = Lude.Nothing,
      clientContext = Lude.Nothing
    }

-- | Choose from the following options.
--
--
--     * @RequestResponse@ (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.
--
--
--     * @Event@ - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.
--
--
--     * @DryRun@ - Validate parameter values and verify that the user or role has permission to invoke the function.
--
--
--
-- /Note:/ Consider using 'invocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvocationType :: Lens.Lens' Invoke (Lude.Maybe InvocationType)
iInvocationType = Lens.lens (invocationType :: Invoke -> Lude.Maybe InvocationType) (\s a -> s {invocationType = a} :: Invoke)
{-# DEPRECATED iInvocationType "Use generic-lens or generic-optics with 'invocationType' instead." #-}

-- | The JSON that you want to provide to your Lambda function as input.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPayload :: Lens.Lens' Invoke Lude.ByteString
iPayload = Lens.lens (payload :: Invoke -> Lude.ByteString) (\s a -> s {payload = a} :: Invoke)
{-# DEPRECATED iPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | Set to @Tail@ to include the execution log in the response.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLogType :: Lens.Lens' Invoke (Lude.Maybe LogType)
iLogType = Lens.lens (logType :: Invoke -> Lude.Maybe LogType) (\s a -> s {logType = a} :: Invoke)
{-# DEPRECATED iLogType "Use generic-lens or generic-optics with 'logType' instead." #-}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFunctionName :: Lens.Lens' Invoke Lude.Text
iFunctionName = Lens.lens (functionName :: Invoke -> Lude.Text) (\s a -> s {functionName = a} :: Invoke)
{-# DEPRECATED iFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to invoke a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iQualifier :: Lens.Lens' Invoke (Lude.Maybe Lude.Text)
iQualifier = Lens.lens (qualifier :: Invoke -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: Invoke)
{-# DEPRECATED iQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientContext :: Lens.Lens' Invoke (Lude.Maybe Lude.Text)
iClientContext = Lens.lens (clientContext :: Invoke -> Lude.Maybe Lude.Text) (\s a -> s {clientContext = a} :: Invoke)
{-# DEPRECATED iClientContext "Use generic-lens or generic-optics with 'clientContext' instead." #-}

instance Lude.AWSRequest Invoke where
  type Rs Invoke = InvokeResponse
  request = Req.postBody lambdaService
  response =
    Res.receiveBytes
      ( \s h x ->
          InvokeResponse'
            Lude.<$> (h Lude..#? "X-Amz-Function-Error")
            Lude.<*> (h Lude..#? "X-Amz-Log-Result")
            Lude.<*> (Lude.pure (Lude.Just x))
            Lude.<*> (h Lude..#? "X-Amz-Executed-Version")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody Invoke where
  toBody = Lude.toBody Lude.. payload

instance Lude.ToHeaders Invoke where
  toHeaders Invoke' {..} =
    Lude.mconcat
      [ "X-Amz-Invocation-Type" Lude.=# invocationType,
        "X-Amz-Log-Type" Lude.=# logType,
        "X-Amz-Client-Context" Lude.=# clientContext
      ]

instance Lude.ToPath Invoke where
  toPath Invoke' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/invocations"]

instance Lude.ToQuery Invoke where
  toQuery Invoke' {..} = Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkInvokeResponse' smart constructor.
data InvokeResponse = InvokeResponse'
  { -- | If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.
    functionError :: Lude.Maybe Lude.Text,
    -- | The last 4 KB of the execution log, which is base64 encoded.
    logResult :: Lude.Maybe Lude.Text,
    -- | The response from the function, or an error object.
    payload :: Lude.Maybe Lude.ByteString,
    -- | The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
    executedVersion :: Lude.Maybe Lude.Text,
    -- | The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
    statusCode :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvokeResponse' with the minimum fields required to make a request.
--
-- * 'functionError' - If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.
-- * 'logResult' - The last 4 KB of the execution log, which is base64 encoded.
-- * 'payload' - The response from the function, or an error object.
-- * 'executedVersion' - The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
-- * 'statusCode' - The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
mkInvokeResponse ::
  -- | 'statusCode'
  Lude.Int ->
  InvokeResponse
mkInvokeResponse pStatusCode_ =
  InvokeResponse'
    { functionError = Lude.Nothing,
      logResult = Lude.Nothing,
      payload = Lude.Nothing,
      executedVersion = Lude.Nothing,
      statusCode = pStatusCode_
    }

-- | If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.
--
-- /Note:/ Consider using 'functionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsFunctionError :: Lens.Lens' InvokeResponse (Lude.Maybe Lude.Text)
irsFunctionError = Lens.lens (functionError :: InvokeResponse -> Lude.Maybe Lude.Text) (\s a -> s {functionError = a} :: InvokeResponse)
{-# DEPRECATED irsFunctionError "Use generic-lens or generic-optics with 'functionError' instead." #-}

-- | The last 4 KB of the execution log, which is base64 encoded.
--
-- /Note:/ Consider using 'logResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsLogResult :: Lens.Lens' InvokeResponse (Lude.Maybe Lude.Text)
irsLogResult = Lens.lens (logResult :: InvokeResponse -> Lude.Maybe Lude.Text) (\s a -> s {logResult = a} :: InvokeResponse)
{-# DEPRECATED irsLogResult "Use generic-lens or generic-optics with 'logResult' instead." #-}

-- | The response from the function, or an error object.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsPayload :: Lens.Lens' InvokeResponse (Lude.Maybe Lude.ByteString)
irsPayload = Lens.lens (payload :: InvokeResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {payload = a} :: InvokeResponse)
{-# DEPRECATED irsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
--
-- /Note:/ Consider using 'executedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsExecutedVersion :: Lens.Lens' InvokeResponse (Lude.Maybe Lude.Text)
irsExecutedVersion = Lens.lens (executedVersion :: InvokeResponse -> Lude.Maybe Lude.Text) (\s a -> s {executedVersion = a} :: InvokeResponse)
{-# DEPRECATED irsExecutedVersion "Use generic-lens or generic-optics with 'executedVersion' instead." #-}

-- | The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsStatusCode :: Lens.Lens' InvokeResponse Lude.Int
irsStatusCode = Lens.lens (statusCode :: InvokeResponse -> Lude.Int) (\s a -> s {statusCode = a} :: InvokeResponse)
{-# DEPRECATED irsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}
