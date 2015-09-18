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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invokes a specified Lambda function.
--
-- This operation requires permission for the 'lambda:InvokeFunction'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS API Reference> for Invoke.
module Network.AWS.Lambda.Invoke
    (
    -- * Creating a Request
      invoke
    , Invoke
    -- * Request Lenses
    , iInvocationType
    , iLogType
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
    , irsStatusCode
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'invoke' smart constructor.
data Invoke = Invoke'
    { _iInvocationType :: !(Maybe InvocationType)
    , _iLogType        :: !(Maybe LogType)
    , _iClientContext  :: !(Maybe Text)
    , _iFunctionName   :: !Text
    , _iPayload        :: !(HashMap Text Value)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Invoke' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInvocationType'
--
-- * 'iLogType'
--
-- * 'iClientContext'
--
-- * 'iFunctionName'
--
-- * 'iPayload'
invoke
    :: Text -- ^ 'iFunctionName'
    -> HashMap Text Value -- ^ 'iPayload'
    -> Invoke
invoke pFunctionName_ pPayload_ =
    Invoke'
    { _iInvocationType = Nothing
    , _iLogType = Nothing
    , _iClientContext = Nothing
    , _iFunctionName = pFunctionName_
    , _iPayload = pPayload_
    }

-- | By default, the 'Invoke' API assumes \"RequestResponse\" invocation
-- type. You can optionally request asynchronous execution by specifying
-- \"Event\" as the 'InvocationType'. You can also use this parameter to
-- request AWS Lambda to not execute the function but do some verification,
-- such as if the caller is authorized to invoke the function and if the
-- inputs are valid. You request this by specifying \"DryRun\" as the
-- 'InvocationType'. This is useful in a cross-account scenario when you
-- want to verify access to a function without running it.
iInvocationType :: Lens' Invoke (Maybe InvocationType)
iInvocationType = lens _iInvocationType (\ s a -> s{_iInvocationType = a});

-- | You can set this optional parameter to \"Tail\" in the request only if
-- you specify the 'InvocationType' parameter with value
-- \"RequestResponse\". In this case, AWS Lambda returns the base64-encoded
-- last 4 KB of log data produced by your Lambda function in the
-- 'x-amz-log-results' header.
iLogType :: Lens' Invoke (Maybe LogType)
iLogType = lens _iLogType (\ s a -> s{_iLogType = a});

-- | Using the 'ClientContext' you can pass client-specific information to
-- the Lambda function you are invoking. You can then process the client
-- information in your Lambda function as you choose through the context
-- variable. For an example of a ClientContext JSON, go to
-- <http://docs.aws.amazon.com/mobileanalytics/latest/ug/PutEvents.html PutEvents>
-- in the /Amazon Mobile Analytics API Reference and User Guide/.
--
-- The ClientContext JSON must be base64-encoded.
iClientContext :: Lens' Invoke (Maybe Text)
iClientContext = lens _iClientContext (\ s a -> s{_iClientContext = a});

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
iFunctionName :: Lens' Invoke Text
iFunctionName = lens _iFunctionName (\ s a -> s{_iFunctionName = a});

-- | JSON that you want to provide to your Lambda function as input.
iPayload :: Lens' Invoke (HashMap Text Value)
iPayload = lens _iPayload (\ s a -> s{_iPayload = a});

instance AWSRequest Invoke where
        type Rs Invoke = InvokeResponse
        request = postBody lambda
        response
          = receiveJSON
              (\ s h x ->
                 InvokeResponse' <$>
                   (h .#? "X-Amz-Function-Error") <*>
                     (h .#? "X-Amz-Log-Result")
                     <*> (pure (Just x))
                     <*> (pure (fromEnum s)))

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
        toQuery = const mempty

-- | Upon success, returns an empty response. Otherwise, throws an exception.
--
-- /See:/ 'invokeResponse' smart constructor.
data InvokeResponse = InvokeResponse'
    { _irsFunctionError :: !(Maybe Text)
    , _irsLogResult     :: !(Maybe Text)
    , _irsPayload       :: !(Maybe (HashMap Text Value))
    , _irsStatusCode    :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InvokeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irsFunctionError'
--
-- * 'irsLogResult'
--
-- * 'irsPayload'
--
-- * 'irsStatusCode'
invokeResponse
    :: Int -- ^ 'irsStatusCode'
    -> InvokeResponse
invokeResponse pStatusCode_ =
    InvokeResponse'
    { _irsFunctionError = Nothing
    , _irsLogResult = Nothing
    , _irsPayload = Nothing
    , _irsStatusCode = pStatusCode_
    }

-- | Indicates whether an error occurred while executing the Lambda function.
-- If an error occurred this field will have one of two values; 'Handled'
-- or 'Unhandled'. 'Handled' errors are errors that are reported by the
-- function while the 'Unhandled' errors are those detected and reported by
-- AWS Lambda. Unhandled errors include out of memory errors and function
-- timeouts. For information about how to report an 'Handled' error, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/programming-model.html Programming Model>.
irsFunctionError :: Lens' InvokeResponse (Maybe Text)
irsFunctionError = lens _irsFunctionError (\ s a -> s{_irsFunctionError = a});

-- | It is the base64-encoded logs for the Lambda function invocation. This
-- is present only if the invocation type is \"RequestResponse\" and the
-- logs were requested.
irsLogResult :: Lens' InvokeResponse (Maybe Text)
irsLogResult = lens _irsLogResult (\ s a -> s{_irsLogResult = a});

-- | It is the JSON representation of the object returned by the Lambda
-- function. In This is present only if the invocation type is
-- \"RequestResponse\".
--
-- In the event of a function error this field contains a message
-- describing the error. For the 'Handled' errors the Lambda function will
-- report this message. For 'Unhandled' errors AWS Lambda reports the
-- message.
irsPayload :: Lens' InvokeResponse (Maybe (HashMap Text Value))
irsPayload = lens _irsPayload (\ s a -> s{_irsPayload = a});

-- | The HTTP status code will be in the 200 range for successful request.
-- For the \"RequestResonse\" invocation type this status code will be 200.
-- For the \"Event\" invocation type this status code will be 202. For the
-- \"DryRun\" invocation type the status code will be 204.
irsStatusCode :: Lens' InvokeResponse Int
irsStatusCode = lens _irsStatusCode (\ s a -> s{_irsStatusCode = a});
