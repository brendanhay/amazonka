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
-- Creates a Lambda function. To create a function, you need a <https://docs.aws.amazon.com/lambda/latest/dg/deployment-package-v2.html deployment package> and an <https://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role execution role> . The deployment package contains your function code. The execution role grants the function permission to use AWS services, such as Amazon CloudWatch Logs for log streaming and AWS X-Ray for request tracing.
--
--
-- A function has an unpublished version, and can have published versions and aliases. The unpublished version changes when you update your function's code and configuration. A published version is a snapshot of your function code and configuration that can't be changed. An alias is a named resource that maps to a version, and can be changed to map to a different version. Use the @Publish@ parameter to create version @1@ of your function from its initial configuration.
--
-- The other parameters let you configure version-specific and function-level settings. You can modify version-specific settings later with 'UpdateFunctionConfiguration' . Function-level settings apply to both the unpublished and published versions of the function, and include tags ('TagResource' ) and per-function concurrency limits ('PutFunctionConcurrency' ).
--
-- If another account or an AWS service invokes your function, use 'AddPermission' to grant permission by creating a resource-based IAM policy. You can grant permissions at the function level, on a version, or on an alias.
--
-- To invoke your function directly, use 'Invoke' . To invoke your function in response to events in other AWS services, create an event source mapping ('CreateEventSourceMapping' ), or configure a function trigger in the other service. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/invoking-lambda-functions.html Invoking Functions> .
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
    , cfLayers
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
    , fcLayers
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

-- | /See:/ 'createFunction' smart constructor.
data CreateFunction = CreateFunction'
  { _cfMemorySize       :: !(Maybe Nat)
  , _cfKMSKeyARN        :: !(Maybe Text)
  , _cfEnvironment      :: !(Maybe Environment)
  , _cfDeadLetterConfig :: !(Maybe DeadLetterConfig)
  , _cfVPCConfig        :: !(Maybe VPCConfig)
  , _cfLayers           :: !(Maybe [Text])
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
-- * 'cfMemorySize' - The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- * 'cfKMSKeyARN' - The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- * 'cfEnvironment' - Environment variables that are accessible from function code during execution.
--
-- * 'cfDeadLetterConfig' - A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html Dead Letter Queues> .
--
-- * 'cfVPCConfig' - For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/vpc.html VPC Settings> .
--
-- * 'cfLayers' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- * 'cfTimeout' - The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- * 'cfTracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- * 'cfDescription' - A description of the function.
--
-- * 'cfTags' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
--
-- * 'cfPublish' - Set to true to publish the first version of the function during creation.
--
-- * 'cfFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'cfRuntime' - The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- * 'cfRole' - The Amazon Resource Name (ARN) of the function's execution role.
--
-- * 'cfHandler' - The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- * 'cfCode' - The code for the function.
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
    , _cfLayers = Nothing
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


-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
cfMemorySize :: Lens' CreateFunction (Maybe Natural)
cfMemorySize = lens _cfMemorySize (\ s a -> s{_cfMemorySize = a}) . mapping _Nat

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
cfKMSKeyARN :: Lens' CreateFunction (Maybe Text)
cfKMSKeyARN = lens _cfKMSKeyARN (\ s a -> s{_cfKMSKeyARN = a})

-- | Environment variables that are accessible from function code during execution.
cfEnvironment :: Lens' CreateFunction (Maybe Environment)
cfEnvironment = lens _cfEnvironment (\ s a -> s{_cfEnvironment = a})

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html Dead Letter Queues> .
cfDeadLetterConfig :: Lens' CreateFunction (Maybe DeadLetterConfig)
cfDeadLetterConfig = lens _cfDeadLetterConfig (\ s a -> s{_cfDeadLetterConfig = a})

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/vpc.html VPC Settings> .
cfVPCConfig :: Lens' CreateFunction (Maybe VPCConfig)
cfVPCConfig = lens _cfVPCConfig (\ s a -> s{_cfVPCConfig = a})

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
cfLayers :: Lens' CreateFunction [Text]
cfLayers = lens _cfLayers (\ s a -> s{_cfLayers = a}) . _Default . _Coerce

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
cfTimeout :: Lens' CreateFunction (Maybe Natural)
cfTimeout = lens _cfTimeout (\ s a -> s{_cfTimeout = a}) . mapping _Nat

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
cfTracingConfig :: Lens' CreateFunction (Maybe TracingConfig)
cfTracingConfig = lens _cfTracingConfig (\ s a -> s{_cfTracingConfig = a})

-- | A description of the function.
cfDescription :: Lens' CreateFunction (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> to apply to the function.
cfTags :: Lens' CreateFunction (HashMap Text Text)
cfTags = lens _cfTags (\ s a -> s{_cfTags = a}) . _Default . _Map

-- | Set to true to publish the first version of the function during creation.
cfPublish :: Lens' CreateFunction (Maybe Bool)
cfPublish = lens _cfPublish (\ s a -> s{_cfPublish = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
cfFunctionName :: Lens' CreateFunction Text
cfFunctionName = lens _cfFunctionName (\ s a -> s{_cfFunctionName = a})

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
cfRuntime :: Lens' CreateFunction Runtime
cfRuntime = lens _cfRuntime (\ s a -> s{_cfRuntime = a})

-- | The Amazon Resource Name (ARN) of the function's execution role.
cfRole :: Lens' CreateFunction Text
cfRole = lens _cfRole (\ s a -> s{_cfRole = a})

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
cfHandler :: Lens' CreateFunction Text
cfHandler = lens _cfHandler (\ s a -> s{_cfHandler = a})

-- | The code for the function.
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
                  ("Layers" .=) <$> _cfLayers,
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
