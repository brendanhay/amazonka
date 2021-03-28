{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      Invoke (..)
    , mkInvoke
    -- ** Request lenses
    , iFunctionName
    , iClientContext
    , iInvocationType
    , iLogType
    , iPayload
    , iQualifier

    -- * Destructuring the response
    , InvokeResponse (..)
    , mkInvokeResponse
    -- ** Response lenses
    , irrsExecutedVersion
    , irrsFunctionError
    , irrsLogResult
    , irrsPayload
    , irrsStatusCode
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInvoke' smart constructor.
data Invoke = Invoke'
  { functionName :: Types.NamespacedFunctionName
    -- ^ The name of the Lambda function, version, or alias.
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
  , clientContext :: Core.Maybe Core.Text
    -- ^ Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
  , invocationType :: Core.Maybe Types.InvocationType
    -- ^ Choose from the following options.
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
  , logType :: Core.Maybe Types.LogType
    -- ^ Set to @Tail@ to include the execution log in the response.
  , payload :: Core.ByteString
    -- ^ The JSON that you want to provide to your Lambda function as input.
  , qualifier :: Core.Maybe Types.Qualifier
    -- ^ Specify a version or alias to invoke a published version of the function.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Invoke' value with any optional fields omitted.
mkInvoke
    :: Types.NamespacedFunctionName -- ^ 'functionName'
    -> Core.ByteString -- ^ 'payload'
    -> Invoke
mkInvoke functionName payload
  = Invoke'{functionName, clientContext = Core.Nothing,
            invocationType = Core.Nothing, logType = Core.Nothing, payload,
            qualifier = Core.Nothing}

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
iFunctionName :: Lens.Lens' Invoke Types.NamespacedFunctionName
iFunctionName = Lens.field @"functionName"
{-# INLINEABLE iFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientContext :: Lens.Lens' Invoke (Core.Maybe Core.Text)
iClientContext = Lens.field @"clientContext"
{-# INLINEABLE iClientContext #-}
{-# DEPRECATED clientContext "Use generic-lens or generic-optics with 'clientContext' instead"  #-}

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
iInvocationType :: Lens.Lens' Invoke (Core.Maybe Types.InvocationType)
iInvocationType = Lens.field @"invocationType"
{-# INLINEABLE iInvocationType #-}
{-# DEPRECATED invocationType "Use generic-lens or generic-optics with 'invocationType' instead"  #-}

-- | Set to @Tail@ to include the execution log in the response.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLogType :: Lens.Lens' Invoke (Core.Maybe Types.LogType)
iLogType = Lens.field @"logType"
{-# INLINEABLE iLogType #-}
{-# DEPRECATED logType "Use generic-lens or generic-optics with 'logType' instead"  #-}

-- | The JSON that you want to provide to your Lambda function as input.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPayload :: Lens.Lens' Invoke Core.ByteString
iPayload = Lens.field @"payload"
{-# INLINEABLE iPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | Specify a version or alias to invoke a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iQualifier :: Lens.Lens' Invoke (Core.Maybe Types.Qualifier)
iQualifier = Lens.field @"qualifier"
{-# INLINEABLE iQualifier #-}
{-# DEPRECATED qualifier "Use generic-lens or generic-optics with 'qualifier' instead"  #-}

instance Core.ToQuery Invoke where
        toQuery Invoke{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Qualifier") qualifier

instance Core.ToHeaders Invoke where
        toHeaders Invoke{..}
          = Core.toHeaders "X-Amz-Client-Context" clientContext Core.<>
              Core.toHeaders "X-Amz-Invocation-Type" invocationType
              Core.<> Core.toHeaders "X-Amz-Log-Type" logType

instance Core.AWSRequest Invoke where
        type Rs Invoke = InvokeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/invocations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody payload}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 InvokeResponse' Core.<$>
                   (Core.parseHeaderMaybe "X-Amz-Executed-Version" h) Core.<*>
                     Core.parseHeaderMaybe "X-Amz-Function-Error" h
                     Core.<*> Core.parseHeaderMaybe "X-Amz-Log-Result" h
                     Core.<*> Core.pure x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkInvokeResponse' smart constructor.
data InvokeResponse = InvokeResponse'
  { executedVersion :: Core.Maybe Types.Version
    -- ^ The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
  , functionError :: Core.Maybe Core.Text
    -- ^ If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.
  , logResult :: Core.Maybe Core.Text
    -- ^ The last 4 KB of the execution log, which is base64 encoded.
  , payload :: Core.Maybe Core.ByteString
    -- ^ The response from the function, or an error object.
  , statusCode :: Core.Int
    -- ^ The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InvokeResponse' value with any optional fields omitted.
mkInvokeResponse
    :: Core.Int -- ^ 'statusCode'
    -> InvokeResponse
mkInvokeResponse statusCode
  = InvokeResponse'{executedVersion = Core.Nothing,
                    functionError = Core.Nothing, logResult = Core.Nothing,
                    payload = Core.Nothing, statusCode}

-- | The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
--
-- /Note:/ Consider using 'executedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irrsExecutedVersion :: Lens.Lens' InvokeResponse (Core.Maybe Types.Version)
irrsExecutedVersion = Lens.field @"executedVersion"
{-# INLINEABLE irrsExecutedVersion #-}
{-# DEPRECATED executedVersion "Use generic-lens or generic-optics with 'executedVersion' instead"  #-}

-- | If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.
--
-- /Note:/ Consider using 'functionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irrsFunctionError :: Lens.Lens' InvokeResponse (Core.Maybe Core.Text)
irrsFunctionError = Lens.field @"functionError"
{-# INLINEABLE irrsFunctionError #-}
{-# DEPRECATED functionError "Use generic-lens or generic-optics with 'functionError' instead"  #-}

-- | The last 4 KB of the execution log, which is base64 encoded.
--
-- /Note:/ Consider using 'logResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irrsLogResult :: Lens.Lens' InvokeResponse (Core.Maybe Core.Text)
irrsLogResult = Lens.field @"logResult"
{-# INLINEABLE irrsLogResult #-}
{-# DEPRECATED logResult "Use generic-lens or generic-optics with 'logResult' instead"  #-}

-- | The response from the function, or an error object.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irrsPayload :: Lens.Lens' InvokeResponse (Core.Maybe Core.ByteString)
irrsPayload = Lens.field @"payload"
{-# INLINEABLE irrsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irrsStatusCode :: Lens.Lens' InvokeResponse Core.Int
irrsStatusCode = Lens.field @"statusCode"
{-# INLINEABLE irrsStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}
