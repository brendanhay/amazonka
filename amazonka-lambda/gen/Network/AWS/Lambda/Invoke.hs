{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Invokes a specified Lambda function.
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
    , irqInvocationType
    , irqPayload
    , irqLogType
    , irqClientContext
    , irqFunctionName

    -- * Response
    , InvokeResponse
    -- ** Response constructor
    , invokeResponse
    -- ** Response lenses
    , irsFunctionError
    , irsLogResult
    , irsPayload
    , irsStatusCode
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'invoke' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irqInvocationType'
--
-- * 'irqPayload'
--
-- * 'irqLogType'
--
-- * 'irqClientContext'
--
-- * 'irqFunctionName'
data Invoke = Invoke'
    { _irqInvocationType :: !(Maybe InvocationType)
    , _irqPayload        :: !(Maybe Base64)
    , _irqLogType        :: !(Maybe LogType)
    , _irqClientContext  :: !(Maybe Text)
    , _irqFunctionName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Invoke' smart constructor.
invoke :: Text -> Invoke
invoke pFunctionName =
    Invoke'
    { _irqInvocationType = Nothing
    , _irqPayload = Nothing
    , _irqLogType = Nothing
    , _irqClientContext = Nothing
    , _irqFunctionName = pFunctionName
    }

-- | By default, the @Invoke@ API assumes \"RequestResponse\" invocation
-- type. You can optionally request asynchronous execution by specifying
-- \"Event\" as the @InvocationType@. You can also use this parameter to
-- request AWS Lambda to not execute the function but do some verification,
-- such as if the caller is authorized to invoke the function and if the
-- inputs are valid. You request this by specifying \"DryRun\" as the
-- @InvocationType@. This is useful in a cross-account scenario when you
-- want to verify access to a function without running it.
irqInvocationType :: Lens' Invoke (Maybe InvocationType)
irqInvocationType = lens _irqInvocationType (\ s a -> s{_irqInvocationType = a});

-- | JSON that you want to provide to your Lambda function as input.
irqPayload :: Lens' Invoke (Maybe Base64)
irqPayload = lens _irqPayload (\ s a -> s{_irqPayload = a});

-- | You can set this optional parameter to \"Tail\" in the request only if
-- you specify the @InvocationType@ parameter with value
-- \"RequestResponse\". In this case, AWS Lambda returns the base64-encoded
-- last 4 KB of log data produced by your Lambda function in the
-- @x-amz-log-results@ header.
irqLogType :: Lens' Invoke (Maybe LogType)
irqLogType = lens _irqLogType (\ s a -> s{_irqLogType = a});

-- | Using the @ClientContext@ you can pass client-specific information to
-- the Lambda function you are invoking. You can then process the client
-- information in your Lambda function as you choose through the context
-- variable. For an example of a ClientContext JSON, go to
-- <http://docs.aws.amazon.com/mobileanalytics/latest/ug/PutEvents.html PutEvents>
-- in the /Amazon Mobile Analytics API Reference and User Guide/.
--
-- The ClientContext JSON must be base64-encoded.
irqClientContext :: Lens' Invoke (Maybe Text)
irqClientContext = lens _irqClientContext (\ s a -> s{_irqClientContext = a});

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
irqFunctionName :: Lens' Invoke Text
irqFunctionName = lens _irqFunctionName (\ s a -> s{_irqFunctionName = a});

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
                     <*> (pure (fromEnum s)))

instance ToHeaders Invoke where
        toHeaders Invoke'{..}
          = mconcat
              ["X-Amz-Invocation-Type" =# _irqInvocationType,
               "X-Amz-Log-Type" =# _irqLogType,
               "X-Amz-Client-Context" =# _irqClientContext]

instance ToJSON Invoke where
        toJSON Invoke'{..}
          = object ["Payload" .= _irqPayload]

instance ToPath Invoke where
        toPath Invoke'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _irqFunctionName,
               "/invocations"]

instance ToQuery Invoke where
        toQuery = const mempty

-- | Upon success, returns an empty response. Otherwise, throws an exception.
--
-- /See:/ 'invokeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irsFunctionError'
--
-- * 'irsLogResult'
--
-- * 'irsPayload'
--
-- * 'irsStatusCode'
data InvokeResponse = InvokeResponse'
    { _irsFunctionError :: !(Maybe Text)
    , _irsLogResult     :: !(Maybe Text)
    , _irsPayload       :: !(Maybe Base64)
    , _irsStatusCode    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InvokeResponse' smart constructor.
invokeResponse :: Int -> InvokeResponse
invokeResponse pStatusCode =
    InvokeResponse'
    { _irsFunctionError = Nothing
    , _irsLogResult = Nothing
    , _irsPayload = Nothing
    , _irsStatusCode = pStatusCode
    }

-- | Indicates whether an error occurred while executing the Lambda function.
-- If an error occurred this field will have one of two values; @Handled@
-- or @Unhandled@. @Handled@ errors are errors that are reported by the
-- function while the @Unhandled@ errors are those detected and reported by
-- AWS Lambda. Unhandled errors include out of memory errors and function
-- timeouts. For information about how to report an @Handled@ error, see
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
-- describing the error. For the @Handled@ errors the Lambda function will
-- report this message. For @Unhandled@ errors AWS Lambda reports the
-- message.
irsPayload :: Lens' InvokeResponse (Maybe Base64)
irsPayload = lens _irsPayload (\ s a -> s{_irsPayload = a});

-- | The HTTP status code will be in the 200 range for successful request.
-- For the \"RequestResonse\" invocation type this status code will be 200.
-- For the \"Event\" invocation type this status code will be 202. For the
-- \"DryRun\" invocation type the status code will be 204.
irsStatusCode :: Lens' InvokeResponse Int
irsStatusCode = lens _irsStatusCode (\ s a -> s{_irsStatusCode = a});
