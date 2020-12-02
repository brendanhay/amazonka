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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Lambda function. The function metadata is created from the request parameters, and the code for the function is provided by a .zip file in the request body. If the function name already exists, the operation will fail. Note that the function name is case-sensitive.
--
--
-- If you are using versioning, you can also publish a version of the Lambda function you are creating using the @Publish@ parameter. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:CreateFunction@ action.
--
module Network.AWS.Lambda.CreateFunction
    (
    -- * Creating a Request
      createFunction
    , CreateFunction
    -- * Request Lenses
    , cfMemorySize
    , cfKMSKeyARN
    , cfEnvironment
    , cfDeadLetterConfig
    , cfVPCConfig
    , cfTimeout
    , cfTracingConfig
    , cfDescription
    , cfTags
    , cfPublish
    , cfFunctionName
    , cfRuntime
    , cfRole
    , cfHandler
    , cfCode

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcMemorySize
    , fcRuntime
    , fcFunctionARN
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcDescription
    , fcRevisionId
    , fcMasterARN
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createFunction' smart constructor.
data CreateFunction = CreateFunction'
  { _cfMemorySize       :: !(Maybe Nat)
  , _cfKMSKeyARN        :: !(Maybe Text)
  , _cfEnvironment      :: !(Maybe Environment)
  , _cfDeadLetterConfig :: !(Maybe DeadLetterConfig)
  , _cfVPCConfig        :: !(Maybe VPCConfig)
  , _cfTimeout          :: !(Maybe Nat)
  , _cfTracingConfig    :: !(Maybe TracingConfig)
  , _cfDescription      :: !(Maybe Text)
  , _cfTags             :: !(Maybe (Map Text Text))
  , _cfPublish          :: !(Maybe Bool)
  , _cfFunctionName     :: !Text
  , _cfRuntime          :: !Runtime
  , _cfRole             :: !Text
  , _cfHandler          :: !Text
  , _cfCode             :: !FunctionCode
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfMemorySize' - The amount of memory, in MB, your Lambda function is given. Lambda uses this memory size to infer the amount of CPU and memory allocated to your function. Your function use-case determines your CPU and memory requirements. For example, a database operation might need less memory compared to an image processing function. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- * 'cfKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If not provided, AWS Lambda will use a default service key.
--
-- * 'cfEnvironment' - Undocumented member.
--
-- * 'cfDeadLetterConfig' - The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
--
-- * 'cfVPCConfig' - If your Lambda function accesses resources in a VPC, you provide this parameter identifying the list of security group IDs and subnet IDs. These must belong to the same VPC. You must provide at least one security group and one subnet ID.
--
-- * 'cfTimeout' - The function execution time at which Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
--
-- * 'cfTracingConfig' - The parent object that contains your function's tracing settings.
--
-- * 'cfDescription' - A short, user-defined function description. Lambda does not use this value. Assign a meaningful description as you see fit.
--
-- * 'cfTags' - The list of tags (key-value pairs) assigned to the new function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
--
-- * 'cfPublish' - This boolean parameter can be used to request AWS Lambda to create the Lambda function and publish a version as an atomic operation.
--
-- * 'cfFunctionName' - The name you want to assign to the function you are uploading. The function names appear in the console and are returned in the 'ListFunctions' API. Function names are used to specify functions to other AWS Lambda API operations, such as 'Invoke' . Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'cfRuntime' - The runtime environment for the Lambda function you are uploading. To use the Python runtime v3.6, set the value to "python3.6". To use the Python runtime v2.7, set the value to "python2.7". To use the Node.js runtime v6.10, set the value to "nodejs6.10". To use the Node.js runtime v4.3, set the value to "nodejs4.3". To use the .NET Core runtime v1.0, set the value to "dotnetcore1.0". To use the .NET Core runtime v2.0, set the value to "dotnetcore2.0".
--
-- * 'cfRole' - The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it executes your function to access any other Amazon Web Services (AWS) resources. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works> .
--
-- * 'cfHandler' - The function within your code that Lambda calls to begin execution. For Node.js, it is the /module-name/ ./export/ value in your function. For Java, it can be @package.class-name::handler@ or @package.class-name@ . For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/java-programming-model-handler-types.html Lambda Function Handler (Java)> .
--
-- * 'cfCode' - The code for the Lambda function.
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
    , _cfKMSKeyARN = Nothing
    , _cfEnvironment = Nothing
    , _cfDeadLetterConfig = Nothing
    , _cfVPCConfig = Nothing
    , _cfTimeout = Nothing
    , _cfTracingConfig = Nothing
    , _cfDescription = Nothing
    , _cfTags = Nothing
    , _cfPublish = Nothing
    , _cfFunctionName = pFunctionName_
    , _cfRuntime = pRuntime_
    , _cfRole = pRole_
    , _cfHandler = pHandler_
    , _cfCode = pCode_
    }


-- | The amount of memory, in MB, your Lambda function is given. Lambda uses this memory size to infer the amount of CPU and memory allocated to your function. Your function use-case determines your CPU and memory requirements. For example, a database operation might need less memory compared to an image processing function. The default value is 128 MB. The value must be a multiple of 64 MB.
cfMemorySize :: Lens' CreateFunction (Maybe Natural)
cfMemorySize = lens _cfMemorySize (\ s a -> s{_cfMemorySize = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If not provided, AWS Lambda will use a default service key.
cfKMSKeyARN :: Lens' CreateFunction (Maybe Text)
cfKMSKeyARN = lens _cfKMSKeyARN (\ s a -> s{_cfKMSKeyARN = a})

-- | Undocumented member.
cfEnvironment :: Lens' CreateFunction (Maybe Environment)
cfEnvironment = lens _cfEnvironment (\ s a -> s{_cfEnvironment = a})

-- | The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
cfDeadLetterConfig :: Lens' CreateFunction (Maybe DeadLetterConfig)
cfDeadLetterConfig = lens _cfDeadLetterConfig (\ s a -> s{_cfDeadLetterConfig = a})

-- | If your Lambda function accesses resources in a VPC, you provide this parameter identifying the list of security group IDs and subnet IDs. These must belong to the same VPC. You must provide at least one security group and one subnet ID.
cfVPCConfig :: Lens' CreateFunction (Maybe VPCConfig)
cfVPCConfig = lens _cfVPCConfig (\ s a -> s{_cfVPCConfig = a})

-- | The function execution time at which Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
cfTimeout :: Lens' CreateFunction (Maybe Natural)
cfTimeout = lens _cfTimeout (\ s a -> s{_cfTimeout = a}) . mapping _Nat

-- | The parent object that contains your function's tracing settings.
cfTracingConfig :: Lens' CreateFunction (Maybe TracingConfig)
cfTracingConfig = lens _cfTracingConfig (\ s a -> s{_cfTracingConfig = a})

-- | A short, user-defined function description. Lambda does not use this value. Assign a meaningful description as you see fit.
cfDescription :: Lens' CreateFunction (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | The list of tags (key-value pairs) assigned to the new function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
cfTags :: Lens' CreateFunction (HashMap Text Text)
cfTags = lens _cfTags (\ s a -> s{_cfTags = a}) . _Default . _Map

-- | This boolean parameter can be used to request AWS Lambda to create the Lambda function and publish a version as an atomic operation.
cfPublish :: Lens' CreateFunction (Maybe Bool)
cfPublish = lens _cfPublish (\ s a -> s{_cfPublish = a})

-- | The name you want to assign to the function you are uploading. The function names appear in the console and are returned in the 'ListFunctions' API. Function names are used to specify functions to other AWS Lambda API operations, such as 'Invoke' . Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
cfFunctionName :: Lens' CreateFunction Text
cfFunctionName = lens _cfFunctionName (\ s a -> s{_cfFunctionName = a})

-- | The runtime environment for the Lambda function you are uploading. To use the Python runtime v3.6, set the value to "python3.6". To use the Python runtime v2.7, set the value to "python2.7". To use the Node.js runtime v6.10, set the value to "nodejs6.10". To use the Node.js runtime v4.3, set the value to "nodejs4.3". To use the .NET Core runtime v1.0, set the value to "dotnetcore1.0". To use the .NET Core runtime v2.0, set the value to "dotnetcore2.0".
cfRuntime :: Lens' CreateFunction Runtime
cfRuntime = lens _cfRuntime (\ s a -> s{_cfRuntime = a})

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it executes your function to access any other Amazon Web Services (AWS) resources. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works> .
cfRole :: Lens' CreateFunction Text
cfRole = lens _cfRole (\ s a -> s{_cfRole = a})

-- | The function within your code that Lambda calls to begin execution. For Node.js, it is the /module-name/ ./export/ value in your function. For Java, it can be @package.class-name::handler@ or @package.class-name@ . For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/java-programming-model-handler-types.html Lambda Function Handler (Java)> .
cfHandler :: Lens' CreateFunction Text
cfHandler = lens _cfHandler (\ s a -> s{_cfHandler = a})

-- | The code for the Lambda function.
cfCode :: Lens' CreateFunction FunctionCode
cfCode = lens _cfCode (\ s a -> s{_cfCode = a})

instance AWSRequest CreateFunction where
        type Rs CreateFunction = FunctionConfiguration
        request = postJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateFunction where

instance NFData CreateFunction where

instance ToHeaders CreateFunction where
        toHeaders = const mempty

instance ToJSON CreateFunction where
        toJSON CreateFunction'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _cfMemorySize,
                  ("KMSKeyArn" .=) <$> _cfKMSKeyARN,
                  ("Environment" .=) <$> _cfEnvironment,
                  ("DeadLetterConfig" .=) <$> _cfDeadLetterConfig,
                  ("VpcConfig" .=) <$> _cfVPCConfig,
                  ("Timeout" .=) <$> _cfTimeout,
                  ("TracingConfig" .=) <$> _cfTracingConfig,
                  ("Description" .=) <$> _cfDescription,
                  ("Tags" .=) <$> _cfTags,
                  ("Publish" .=) <$> _cfPublish,
                  Just ("FunctionName" .= _cfFunctionName),
                  Just ("Runtime" .= _cfRuntime),
                  Just ("Role" .= _cfRole),
                  Just ("Handler" .= _cfHandler),
                  Just ("Code" .= _cfCode)])

instance ToPath CreateFunction where
        toPath = const "/2015-03-31/functions"

instance ToQuery CreateFunction where
        toQuery = const mempty
