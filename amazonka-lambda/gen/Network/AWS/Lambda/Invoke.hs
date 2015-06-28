{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Invokes a specified Lambda function.
--
-- This operation requires permission for the @lambda:InvokeFunction@
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html>
module Network.AWS.Lambda.Invoke
    (
    -- * Request
      Invoke
    -- ** Request constructor
    , invoke
    -- ** Request lenses
    , invInvocationType
    , invPayload
    , invLogType
    , invClientContext
    , invFunctionName

    -- * Response
    , InvokeResponse
    -- ** Response constructor
    , invokeResponse
    -- ** Response lenses
    , irFunctionError
    , irLogResult
    , irPayload
    , irStatusCode
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'invoke' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'invInvocationType'
--
-- * 'invPayload'
--
-- * 'invLogType'
--
-- * 'invClientContext'
--
-- * 'invFunctionName'
data Invoke = Invoke'
    { _invInvocationType :: !(Maybe InvocationType)
    , _invPayload        :: !(Maybe Base64)
    , _invLogType        :: !(Maybe LogType)
    , _invClientContext  :: !(Maybe Text)
    , _invFunctionName   :: !Text
    } deriving (Eq,Read,Show)

-- | 'Invoke' smart constructor.
invoke :: Text -> Invoke
invoke pFunctionName =
    Invoke'
    { _invInvocationType = Nothing
    , _invPayload = Nothing
    , _invLogType = Nothing
    , _invClientContext = Nothing
    , _invFunctionName = pFunctionName
    }

-- | By default, the @Invoke@ API assumes \"RequestResponse\" invocation
-- type. You can optionally request asynchronous execution by specifying
-- \"Event\" as the @InvocationType@. You can also use this parameter to
-- request AWS Lambda to not execute the function but do some verification,
-- such as if the caller is authorized to invoke the function and if the
-- inputs are valid. You request this by specifying \"DryRun\" as the
-- @InvocationType@. This is useful in a cross-account scenario when you
-- want to verify access to a function without running it.
invInvocationType :: Lens' Invoke (Maybe InvocationType)
invInvocationType = lens _invInvocationType (\ s a -> s{_invInvocationType = a});

-- | JSON that you want to provide to your Lambda function as input.
invPayload :: Lens' Invoke (Maybe Base64)
invPayload = lens _invPayload (\ s a -> s{_invPayload = a});

-- | You can set this optional parameter to \"Tail\" in the request only if
-- you specify the @InvocationType@ parameter with value
-- \"RequestResponse\". In this case, AWS Lambda returns the base64-encoded
-- last 4 KB of log data produced by your Lambda function in the
-- @x-amz-log-results@ header.
invLogType :: Lens' Invoke (Maybe LogType)
invLogType = lens _invLogType (\ s a -> s{_invLogType = a});

-- | Using the @ClientContext@ you can pass client-specific information to
-- the Lambda function you are invoking. You can then process the client
-- information in your Lambda function as you choose through the context
-- variable. For an example of a ClientContext JSON, go to
-- <http://docs.aws.amazon.com/mobileanalytics/latest/ug/PutEvents.html PutEvents>
-- in the /Amazon Mobile Analytics API Reference and User Guide/.
--
-- The ClientContext JSON must be base64-encoded.
invClientContext :: Lens' Invoke (Maybe Text)
invClientContext = lens _invClientContext (\ s a -> s{_invClientContext = a});

-- | The Lambda function name.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
invFunctionName :: Lens' Invoke Text
invFunctionName = lens _invFunctionName (\ s a -> s{_invFunctionName = a});

instance AWSRequest Invoke where
        type Sv Invoke = Lambda
        type Rs Invoke = InvokeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 InvokeResponse' <$>
                   (h .#? "X-Amz-Function-Error") <*>
                     (h .#? "X-Amz-Log-Result")
                     <*> (x .?> "Payload")
                     <*> (pure s))

instance ToHeaders Invoke where
        toHeaders Invoke'{..}
          = mconcat
              ["X-Amz-Invocation-Type" =# _invInvocationType,
               "X-Amz-Log-Type" =# _invLogType,
               "X-Amz-Client-Context" =# _invClientContext]

instance ToJSON Invoke where
        toJSON Invoke'{..}
          = object ["Payload" .= _invPayload]

instance ToPath Invoke where
        toPath Invoke'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _invFunctionName,
               "/invocations"]

instance ToQuery Invoke where
        toQuery = const mempty

-- | Upon success, returns an empty response. Otherwise, throws an exception.
--
-- /See:/ 'invokeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irFunctionError'
--
-- * 'irLogResult'
--
-- * 'irPayload'
--
-- * 'irStatusCode'
data InvokeResponse = InvokeResponse'
    { _irFunctionError :: !(Maybe Text)
    , _irLogResult     :: !(Maybe Text)
    , _irPayload       :: !(Maybe Base64)
    , _irStatusCode    :: !Int
    } deriving (Eq,Read,Show)

-- | 'InvokeResponse' smart constructor.
invokeResponse :: Int -> InvokeResponse
invokeResponse pStatusCode =
    InvokeResponse'
    { _irFunctionError = Nothing
    , _irLogResult = Nothing
    , _irPayload = Nothing
    , _irStatusCode = pStatusCode
    }

-- | Indicates whether an error occurred while executing the Lambda function.
-- If an error occurred this field will have one of two values; @Handled@
-- or @Unhandled@. @Handled@ errors are errors that are reported by the
-- function while the @Unhandled@ errors are those detected and reported by
-- AWS Lambda. Unhandled errors include out of memory errors and function
-- timeouts. For information about how to report an @Handled@ error, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/programming-model.html Programming Model>.
irFunctionError :: Lens' InvokeResponse (Maybe Text)
irFunctionError = lens _irFunctionError (\ s a -> s{_irFunctionError = a});

-- | It is the base64-encoded logs for the Lambda function invocation. This
-- is present only if the invocation type is \"RequestResponse\" and the
-- logs were requested.
irLogResult :: Lens' InvokeResponse (Maybe Text)
irLogResult = lens _irLogResult (\ s a -> s{_irLogResult = a});

-- | It is the JSON representation of the object returned by the Lambda
-- function. In This is present only if the invocation type is
-- \"RequestResponse\".
--
-- In the event of a function error this field contains a message
-- describing the error. For the @Handled@ errors the Lambda function will
-- report this message. For @Unhandled@ errors AWS Lambda reports the
-- message.
irPayload :: Lens' InvokeResponse (Maybe Base64)
irPayload = lens _irPayload (\ s a -> s{_irPayload = a});

-- | The HTTP status code will be in the 200 range for successful request.
-- For the \"RequestResonse\" invocation type this status code will be 200.
-- For the \"Event\" invocation type this status code will be 202. For the
-- \"DryRun\" invocation type the status code will be 204.
irStatusCode :: Lens' InvokeResponse Int
irStatusCode = lens _irStatusCode (\ s a -> s{_irStatusCode = a});
