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
-- Module      : Network.AWS.Lambda.CreateFunction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Lambda function. The function metadata is created from the
-- request parameters, and the code for the function is provided by a .zip
-- file in the request body. If the function name already exists, the
-- operation will fail. Note that the function name is case-sensitive.
--
-- This operation requires permission for the 'lambda:CreateFunction'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_CreateFunction.html AWS API Reference> for CreateFunction.
module Network.AWS.Lambda.CreateFunction
    (
    -- * Creating a Request
      createFunction
    , CreateFunction
    -- * Request Lenses
    , cfMemorySize
    , cfTimeout
    , cfDescription
    , cfFunctionName
    , cfRuntime
    , cfRole
    , cfHandler
    , cfCode

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createFunction' smart constructor.
data CreateFunction = CreateFunction'
    { _cfMemorySize   :: !(Maybe Nat)
    , _cfTimeout      :: !(Maybe Nat)
    , _cfDescription  :: !(Maybe Text)
    , _cfFunctionName :: !Text
    , _cfRuntime      :: !Runtime
    , _cfRole         :: !Text
    , _cfHandler      :: !Text
    , _cfCode         :: !FunctionCode
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfMemorySize'
--
-- * 'cfTimeout'
--
-- * 'cfDescription'
--
-- * 'cfFunctionName'
--
-- * 'cfRuntime'
--
-- * 'cfRole'
--
-- * 'cfHandler'
--
-- * 'cfCode'
createFunction
    :: Text -- ^ 'cfFunctionName'
    -> Runtime -- ^ 'cfRuntime'
    -> Text -- ^ 'cfRole'
    -> Text -- ^ 'cfHandler'
    -> FunctionCode -- ^ 'cfCode'
    -> CreateFunction
createFunction pFunctionName_ pRuntime_ pRole_ pHandler_ pCode_ =
    CreateFunction'
    { _cfMemorySize = Nothing
    , _cfTimeout = Nothing
    , _cfDescription = Nothing
    , _cfFunctionName = pFunctionName_
    , _cfRuntime = pRuntime_
    , _cfRole = pRole_
    , _cfHandler = pHandler_
    , _cfCode = pCode_
    }

-- | The amount of memory, in MB, your Lambda function is given. Lambda uses
-- this memory size to infer the amount of CPU and memory allocated to your
-- function. Your function use-case determines your CPU and memory
-- requirements. For example, a database operation might need less memory
-- compared to an image processing function. The default value is 128 MB.
-- The value must be a multiple of 64 MB.
cfMemorySize :: Lens' CreateFunction (Maybe Natural)
cfMemorySize = lens _cfMemorySize (\ s a -> s{_cfMemorySize = a}) . mapping _Nat;

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
cfTimeout :: Lens' CreateFunction (Maybe Natural)
cfTimeout = lens _cfTimeout (\ s a -> s{_cfTimeout = a}) . mapping _Nat;

-- | A short, user-defined function description. Lambda does not use this
-- value. Assign a meaningful description as you see fit.
cfDescription :: Lens' CreateFunction (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a});

-- | The name you want to assign to the function you are uploading. You can
-- specify an unqualified function name (for example, \"Thumbnail\") or you
-- can specify Amazon Resource Name (ARN) of the function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length. The function names appear in the console and are
-- returned in the ListFunctions API. Function names are used to specify
-- functions to other AWS Lambda APIs, such as Invoke.
cfFunctionName :: Lens' CreateFunction Text
cfFunctionName = lens _cfFunctionName (\ s a -> s{_cfFunctionName = a});

-- | The runtime environment for the Lambda function you are uploading.
-- Currently, Lambda supports \"java\" and \"nodejs\" as the runtime.
cfRuntime :: Lens' CreateFunction Runtime
cfRuntime = lens _cfRuntime (\ s a -> s{_cfRuntime = a});

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources. For more information, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>
cfRole :: Lens' CreateFunction Text
cfRole = lens _cfRole (\ s a -> s{_cfRole = a});

-- | The function within your code that Lambda calls to begin execution. For
-- Node.js, it is the /module-name/./export/ value in your function. For
-- Java, it can be 'package.class-name::handler' or 'package.class-name'.
-- For more information, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/java-programming-model-handler-types.html Lambda Function Handler (Java)>.
cfHandler :: Lens' CreateFunction Text
cfHandler = lens _cfHandler (\ s a -> s{_cfHandler = a});

-- | The code for the Lambda function.
cfCode :: Lens' CreateFunction FunctionCode
cfCode = lens _cfCode (\ s a -> s{_cfCode = a});

instance AWSRequest CreateFunction where
        type Sv CreateFunction = Lambda
        type Rs CreateFunction = FunctionConfiguration
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateFunction where
        toHeaders = const mempty

instance ToJSON CreateFunction where
        toJSON CreateFunction'{..}
          = object
              ["MemorySize" .= _cfMemorySize,
               "Timeout" .= _cfTimeout,
               "Description" .= _cfDescription,
               "FunctionName" .= _cfFunctionName,
               "Runtime" .= _cfRuntime, "Role" .= _cfRole,
               "Handler" .= _cfHandler, "Code" .= _cfCode]

instance ToPath CreateFunction where
        toPath = const "/2015-03-31/functions"

instance ToQuery CreateFunction where
        toQuery = const mempty
