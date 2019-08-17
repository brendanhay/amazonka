{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invokes a Lambda function. You can invoke a function synchronously (and wait for the response), or asynchronously. To invoke a function asynchronously, set @InvocationType@ to @Event@ .
--
--
-- For synchronous invocation, details about the function response, including errors, are included in the response body and headers. For either invocation type, you can find more information in the <https://docs.aws.amazon.com/lambda/latest/dg/monitoring-functions.html execution log> and <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html trace> . To record function errors for asynchronous invocations, configure your function with a <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html dead letter queue> .
--
-- When an error occurs, your function may be invoked multiple times. Retry behavior varies by error type, client, event source, and invocation type. For example, if you invoke a function asynchronously and it returns an error, Lambda executes the function up to two more times. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/retries-on-errors.html Retry Behavior> .
--
-- The status code in the API response doesn't reflect function errors. Error codes are reserved for errors that prevent your function from executing, such as permissions errors, <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limit errors> , or issues with your function's code and configuration. For example, Lambda returns @TooManyRequestsException@ if executing the function would cause you to exceed a concurrency limit at either the account level (@ConcurrentInvocationLimitExceeded@ ) or function level (@ReservedFunctionConcurrentInvocationLimitExceeded@ ).
--
-- For functions with a long timeout, your client might be disconnected during synchronous invocation while it waits for a response. Configure your HTTP client, SDK, firewall, proxy, or operating system to allow for long connections with timeout or keep-alive settings.
--
-- This operation requires permission for the @lambda:InvokeFunction@ action.
--
module Network.AWS.Lambda.Invoke
    (
    -- * Creating a Request
      invoke
    , Invoke
    -- * Request Lenses
    , iInvocationType
    , iLogType
    , iQualifier
    , iClientContext
    , iFunctionName
    , iPayload

    -- * Destructuring the Response
    , invokeResponse
    , InvokeResponse
    -- * Response Lenses
    , irsFunctionError
    , irsLogResult
    , irsPayload
    , irsExecutedVersion
    , irsStatusCode
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'invoke' smart constructor.
data Invoke = Invoke'
  { _iInvocationType :: !(Maybe InvocationType)
  , _iLogType        :: !(Maybe LogType)
  , _iQualifier      :: !(Maybe Text)
  , _iClientContext  :: !(Maybe Text)
  , _iFunctionName   :: !Text
  , _iPayload        :: !ByteString
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Invoke' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInvocationType' - Choose from the following options.     * @RequestResponse@ (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.     * @Event@ - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.     * @DryRun@ - Validate parameter values and verify that the user or role has permission to invoke the function.
--
-- * 'iLogType' - Set to @Tail@ to include the execution log in the response.
--
-- * 'iQualifier' - Specify a version or alias to invoke a published version of the function.
--
-- * 'iClientContext' - Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
--
-- * 'iFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'iPayload' - The JSON that you want to provide to your Lambda function as input.
invoke
    :: Text -- ^ 'iFunctionName'
    -> ByteString -- ^ 'iPayload'
    -> Invoke
invoke pFunctionName_ pPayload_ =
  Invoke'
    { _iInvocationType = Nothing
    , _iLogType = Nothing
    , _iQualifier = Nothing
    , _iClientContext = Nothing
    , _iFunctionName = pFunctionName_
    , _iPayload = pPayload_
    }


-- | Choose from the following options.     * @RequestResponse@ (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.     * @Event@ - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.     * @DryRun@ - Validate parameter values and verify that the user or role has permission to invoke the function.
iInvocationType :: Lens' Invoke (Maybe InvocationType)
iInvocationType = lens _iInvocationType (\ s a -> s{_iInvocationType = a})

-- | Set to @Tail@ to include the execution log in the response.
iLogType :: Lens' Invoke (Maybe LogType)
iLogType = lens _iLogType (\ s a -> s{_iLogType = a})

-- | Specify a version or alias to invoke a published version of the function.
iQualifier :: Lens' Invoke (Maybe Text)
iQualifier = lens _iQualifier (\ s a -> s{_iQualifier = a})

-- | Up to 3583 bytes of base64-encoded data about the invoking client to pass to the function in the context object.
iClientContext :: Lens' Invoke (Maybe Text)
iClientContext = lens _iClientContext (\ s a -> s{_iClientContext = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
iFunctionName :: Lens' Invoke Text
iFunctionName = lens _iFunctionName (\ s a -> s{_iFunctionName = a})

-- | The JSON that you want to provide to your Lambda function as input.
iPayload :: Lens' Invoke ByteString
iPayload = lens _iPayload (\ s a -> s{_iPayload = a})

instance AWSRequest Invoke where
        type Rs Invoke = InvokeResponse
        request = postBody lambda
        response
          = receiveBytes
              (\ s h x ->
                 InvokeResponse' <$>
                   (h .#? "X-Amz-Function-Error") <*>
                     (h .#? "X-Amz-Log-Result")
                     <*> (pure (Just x))
                     <*> (h .#? "X-Amz-Executed-Version")
                     <*> (pure (fromEnum s)))

instance Hashable Invoke where

instance NFData Invoke where

instance ToBody Invoke where
        toBody = toBody . _iPayload

instance ToHeaders Invoke where
        toHeaders Invoke'{..}
          = mconcat
              ["X-Amz-Invocation-Type" =# _iInvocationType,
               "X-Amz-Log-Type" =# _iLogType,
               "X-Amz-Client-Context" =# _iClientContext]

instance ToPath Invoke where
        toPath Invoke'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _iFunctionName,
               "/invocations"]

instance ToQuery Invoke where
        toQuery Invoke'{..}
          = mconcat ["Qualifier" =: _iQualifier]

-- | /See:/ 'invokeResponse' smart constructor.
data InvokeResponse = InvokeResponse'
  { _irsFunctionError   :: !(Maybe Text)
  , _irsLogResult       :: !(Maybe Text)
  , _irsPayload         :: !(Maybe ByteString)
  , _irsExecutedVersion :: !(Maybe Text)
  , _irsStatusCode      :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvokeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irsFunctionError' - If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.     * @Handled@ - The runtime caught an error thrown by the function and formatted it into a JSON document.     * @Unhandled@ - The runtime didn't handle the error. For example, the function ran out of memory or timed out.
--
-- * 'irsLogResult' - The last 4 KB of the execution log, which is base64 encoded.
--
-- * 'irsPayload' - The response from the function, or an error object.
--
-- * 'irsExecutedVersion' - The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
--
-- * 'irsStatusCode' - The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
invokeResponse
    :: Int -- ^ 'irsStatusCode'
    -> InvokeResponse
invokeResponse pStatusCode_ =
  InvokeResponse'
    { _irsFunctionError = Nothing
    , _irsLogResult = Nothing
    , _irsPayload = Nothing
    , _irsExecutedVersion = Nothing
    , _irsStatusCode = pStatusCode_
    }


-- | If present, indicates that an error occurred during function execution. Details about the error are included in the response payload.     * @Handled@ - The runtime caught an error thrown by the function and formatted it into a JSON document.     * @Unhandled@ - The runtime didn't handle the error. For example, the function ran out of memory or timed out.
irsFunctionError :: Lens' InvokeResponse (Maybe Text)
irsFunctionError = lens _irsFunctionError (\ s a -> s{_irsFunctionError = a})

-- | The last 4 KB of the execution log, which is base64 encoded.
irsLogResult :: Lens' InvokeResponse (Maybe Text)
irsLogResult = lens _irsLogResult (\ s a -> s{_irsLogResult = a})

-- | The response from the function, or an error object.
irsPayload :: Lens' InvokeResponse (Maybe ByteString)
irsPayload = lens _irsPayload (\ s a -> s{_irsPayload = a})

-- | The version of the function that executed. When you invoke a function with an alias, this indicates which version the alias resolved to.
irsExecutedVersion :: Lens' InvokeResponse (Maybe Text)
irsExecutedVersion = lens _irsExecutedVersion (\ s a -> s{_irsExecutedVersion = a})

-- | The HTTP status code is in the 200 range for a successful request. For the @RequestResponse@ invocation type, this status code is 200. For the @Event@ invocation type, this status code is 202. For the @DryRun@ invocation type, the status code is 204.
irsStatusCode :: Lens' InvokeResponse Int
irsStatusCode = lens _irsStatusCode (\ s a -> s{_irsStatusCode = a})

instance NFData InvokeResponse where
