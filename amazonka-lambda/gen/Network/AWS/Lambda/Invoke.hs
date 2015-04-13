{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.Invoke
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- This operation requires permission for the 'lambda:Invoke' action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html>
module Network.AWS.Lambda.Invoke
    (
    -- * Request
      Invoke
    -- ** Request constructor
    , invoke
    -- ** Request lenses
    , iClientContext
    , iFunctionName
    , iInvocationType
    , iLogType
    , iPayload

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

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data Invoke = Invoke
    { _iClientContext  :: Maybe Text
    , _iFunctionName   :: Text
    , _iInvocationType :: Maybe InvocationType
    , _iLogType        :: Maybe LogType
    , _iPayload        :: Maybe Object
    } deriving (Eq, Show)

-- | 'Invoke' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iClientContext' @::@ 'Maybe' 'Text'
--
-- * 'iFunctionName' @::@ 'Text'
--
-- * 'iInvocationType' @::@ 'Maybe' 'InvocationType'
--
-- * 'iLogType' @::@ 'Maybe' 'LogType'
--
-- * 'iPayload' @::@ 'Maybe' 'Object'
--
invoke :: Text -- ^ 'iFunctionName'
       -> Invoke
invoke p1 = Invoke
    { _iFunctionName   = p1
    , _iInvocationType = Nothing
    , _iLogType        = Nothing
    , _iClientContext  = Nothing
    , _iPayload        = Nothing
    }

-- | Using the 'ClientContext' you can pass client-specific information to the
-- Lambda function you are invoking. You can then process the client information
-- in your Lambda function as you choose through the context variable. For an
-- example of a ClientContext JSON, go to <http://docs.aws.amazon.com/mobileanalytics/latest/ug/PutEvents.html PutEvents> in the /Amazon MobileAnalytics API Reference and User Guide/.
--
-- The ClientContext JSON must be base64-encoded.
iClientContext :: Lens' Invoke (Maybe Text)
iClientContext = lens _iClientContext (\s a -> s { _iClientContext = a })

-- | The Lambda function name.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
iFunctionName :: Lens' Invoke Text
iFunctionName = lens _iFunctionName (\s a -> s { _iFunctionName = a })

-- | By default, the 'Invoke' API assumes "RequestResponse" invocation type. You can
-- optionally request asynchronous execution by specifying "Event" as the 'InvocationType'. You can also use this parameter to request AWS Lambda to not execute the
-- function but do some verification, such as if the caller is authorized to
-- invoke the function and if the inputs are valid. You request this by
-- specifying "DryRun" as the 'InvocationType'. This is useful in a cross-account
-- scenario when you want to verify access to a function without running it.
iInvocationType :: Lens' Invoke (Maybe InvocationType)
iInvocationType = lens _iInvocationType (\s a -> s { _iInvocationType = a })

-- | You can set this optional parameter to "Tail" in the request only if you
-- specify the 'InvocationType' parameter with value "RequestResponse". In this
-- case, AWS Lambda returns the base64-encoded last 4 KB of log data produced by
-- your Lambda function in the 'x-amz-log-results' header.
iLogType :: Lens' Invoke (Maybe LogType)
iLogType = lens _iLogType (\s a -> s { _iLogType = a })

-- | JSON that you want to provide to your Lambda function as input.
iPayload :: Lens' Invoke (Maybe Object)
iPayload = lens _iPayload (\s a -> s { _iPayload = a })

data InvokeResponse = InvokeResponse
    { _irFunctionError :: Maybe Text
    , _irLogResult     :: Maybe Text
    , _irPayload       :: Maybe Object
    , _irStatusCode    :: Maybe Int
    } deriving (Eq, Show)

-- | 'InvokeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irFunctionError' @::@ 'Maybe' 'Text'
--
-- * 'irLogResult' @::@ 'Maybe' 'Text'
--
-- * 'irPayload' @::@ 'Maybe' 'Object'
--
-- * 'irStatusCode' @::@ 'Maybe' 'Int'
--
invokeResponse :: InvokeResponse
invokeResponse = InvokeResponse
    { _irStatusCode    = Nothing
    , _irFunctionError = Nothing
    , _irLogResult     = Nothing
    , _irPayload       = Nothing
    }

-- | Indicates whether an error occurred while executing the Lambda function. If
-- an error occurred this field will have one of two values; 'Handled' or 'Unhandled'
-- . 'Handled' errors are errors that are reported by the function while the 'Unhandled' errors are those detected and reported by AWS Lambda. Unhandled errors
-- include out of memory errors and function timeouts. For information about how
-- to report an 'Handled' error, see <http://docs.aws.amazon.com/lambda/latest/dg/programming-model.html Programming Model>.
irFunctionError :: Lens' InvokeResponse (Maybe Text)
irFunctionError = lens _irFunctionError (\s a -> s { _irFunctionError = a })

-- | It is the base64-encoded logs for the Lambda function invocation. This is
-- present only if the invocation type is "RequestResponse" and the logs were
-- requested.
irLogResult :: Lens' InvokeResponse (Maybe Text)
irLogResult = lens _irLogResult (\s a -> s { _irLogResult = a })

-- | It is the JSON representation of the object returned by the Lambda function.
-- In This is present only if the invocation type is "RequestResponse".
--
-- In the event of a function error this field contains a message describing
-- the error. For the 'Handled' errors the Lambda function will report this
-- message. For 'Unhandled' errors AWS Lambda reports the message.
irPayload :: Lens' InvokeResponse (Maybe Object)
irPayload = lens _irPayload (\s a -> s { _irPayload = a })

-- | The HTTP status code will be in the 200 range for successful request. For the
-- "RequestResonse" invocation type this status code will be 200. For the
-- "Event" invocation type this status code will be 202. For the "DryRun"
-- invocation type the status code will be 204.
irStatusCode :: Lens' InvokeResponse (Maybe Int)
irStatusCode = lens _irStatusCode (\s a -> s { _irStatusCode = a })

instance ToPath Invoke where
    toPath Invoke{..} = mconcat
        [ "/2015-03-31/functions/"
        , toText _iFunctionName
        , "/invocations"
        ]

instance ToQuery Invoke where
    toQuery = const mempty

instance ToHeaders Invoke where
    toHeaders Invoke{..} = mconcat
        [ "X-Amz-Invocation-Type" =: _iInvocationType
        , "X-Amz-Log-Type"        =: _iLogType
        , "X-Amz-Client-Context"  =: _iClientContext
        ]

instance ToJSON Invoke where
    toJSON Invoke{..} = object
        [ "Payload" .= _iPayload
        ]

instance AWSRequest Invoke where
    type Sv Invoke = Lambda
    type Rs Invoke = InvokeResponse

    request  = post
    response = jsonHeaderResponse $ \h s o -> InvokeResponse
        <$> h ~:? "X-Amz-Function-Error"
        <*> h ~:? "X-Amz-Log-Result"
        <*> pure (Just o)
        <*> pure (Just s)
