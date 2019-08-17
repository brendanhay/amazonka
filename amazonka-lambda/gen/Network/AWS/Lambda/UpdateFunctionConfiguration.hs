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
-- Module      : Network.AWS.Lambda.UpdateFunctionConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the version-specific settings of a Lambda function.
--
--
-- These settings can vary between versions of a function and are locked when you publish a version. You can't modify the configuration of a published version, only the unpublished version.
--
-- To configure function concurrency, use 'PutFunctionConcurrency' . To grant invoke permissions to an account or AWS service, use 'AddPermission' .
--
module Network.AWS.Lambda.UpdateFunctionConfiguration
    (
    -- * Creating a Request
      updateFunctionConfiguration
    , UpdateFunctionConfiguration
    -- * Request Lenses
    , ufcMemorySize
    , ufcRuntime
    , ufcKMSKeyARN
    , ufcEnvironment
    , ufcDeadLetterConfig
    , ufcRole
    , ufcVPCConfig
    , ufcLayers
    , ufcHandler
    , ufcTimeout
    , ufcTracingConfig
    , ufcDescription
    , ufcRevisionId
    , ufcFunctionName

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

-- | /See:/ 'updateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { _ufcMemorySize       :: !(Maybe Nat)
  , _ufcRuntime          :: !(Maybe Runtime)
  , _ufcKMSKeyARN        :: !(Maybe Text)
  , _ufcEnvironment      :: !(Maybe Environment)
  , _ufcDeadLetterConfig :: !(Maybe DeadLetterConfig)
  , _ufcRole             :: !(Maybe Text)
  , _ufcVPCConfig        :: !(Maybe VPCConfig)
  , _ufcLayers           :: !(Maybe [Text])
  , _ufcHandler          :: !(Maybe Text)
  , _ufcTimeout          :: !(Maybe Nat)
  , _ufcTracingConfig    :: !(Maybe TracingConfig)
  , _ufcDescription      :: !(Maybe Text)
  , _ufcRevisionId       :: !(Maybe Text)
  , _ufcFunctionName     :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufcMemorySize' - The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- * 'ufcRuntime' - The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
--
-- * 'ufcKMSKeyARN' - The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
--
-- * 'ufcEnvironment' - Environment variables that are accessible from function code during execution.
--
-- * 'ufcDeadLetterConfig' - A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html Dead Letter Queues> .
--
-- * 'ufcRole' - The Amazon Resource Name (ARN) of the function's execution role.
--
-- * 'ufcVPCConfig' - For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/vpc.html VPC Settings> .
--
-- * 'ufcLayers' - A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
--
-- * 'ufcHandler' - The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
--
-- * 'ufcTimeout' - The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
--
-- * 'ufcTracingConfig' - Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
--
-- * 'ufcDescription' - A description of the function.
--
-- * 'ufcRevisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- * 'ufcFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
updateFunctionConfiguration
    :: Text -- ^ 'ufcFunctionName'
    -> UpdateFunctionConfiguration
updateFunctionConfiguration pFunctionName_ =
  UpdateFunctionConfiguration'
    { _ufcMemorySize = Nothing
    , _ufcRuntime = Nothing
    , _ufcKMSKeyARN = Nothing
    , _ufcEnvironment = Nothing
    , _ufcDeadLetterConfig = Nothing
    , _ufcRole = Nothing
    , _ufcVPCConfig = Nothing
    , _ufcLayers = Nothing
    , _ufcHandler = Nothing
    , _ufcTimeout = Nothing
    , _ufcTracingConfig = Nothing
    , _ufcDescription = Nothing
    , _ufcRevisionId = Nothing
    , _ufcFunctionName = pFunctionName_
    }


-- | The amount of memory that your function has access to. Increasing the function's memory also increases its CPU allocation. The default value is 128 MB. The value must be a multiple of 64 MB.
ufcMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcMemorySize = lens _ufcMemorySize (\ s a -> s{_ufcMemorySize = a}) . mapping _Nat

-- | The identifier of the function's <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime> .
ufcRuntime :: Lens' UpdateFunctionConfiguration (Maybe Runtime)
ufcRuntime = lens _ufcRuntime (\ s a -> s{_ufcRuntime = a})

-- | The ARN of the AWS Key Management Service (AWS KMS) key that's used to encrypt your function's environment variables. If it's not provided, AWS Lambda uses a default service key.
ufcKMSKeyARN :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcKMSKeyARN = lens _ufcKMSKeyARN (\ s a -> s{_ufcKMSKeyARN = a})

-- | Environment variables that are accessible from function code during execution.
ufcEnvironment :: Lens' UpdateFunctionConfiguration (Maybe Environment)
ufcEnvironment = lens _ufcEnvironment (\ s a -> s{_ufcEnvironment = a})

-- | A dead letter queue configuration that specifies the queue or topic where Lambda sends asynchronous events when they fail processing. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html Dead Letter Queues> .
ufcDeadLetterConfig :: Lens' UpdateFunctionConfiguration (Maybe DeadLetterConfig)
ufcDeadLetterConfig = lens _ufcDeadLetterConfig (\ s a -> s{_ufcDeadLetterConfig = a})

-- | The Amazon Resource Name (ARN) of the function's execution role.
ufcRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRole = lens _ufcRole (\ s a -> s{_ufcRole = a})

-- | For network connectivity to AWS resources in a VPC, specify a list of security groups and subnets in the VPC. When you connect a function to a VPC, it can only access resources and the internet through that VPC. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/vpc.html VPC Settings> .
ufcVPCConfig :: Lens' UpdateFunctionConfiguration (Maybe VPCConfig)
ufcVPCConfig = lens _ufcVPCConfig (\ s a -> s{_ufcVPCConfig = a})

-- | A list of <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html function layers> to add to the function's execution environment. Specify each layer by its ARN, including the version.
ufcLayers :: Lens' UpdateFunctionConfiguration [Text]
ufcLayers = lens _ufcLayers (\ s a -> s{_ufcLayers = a}) . _Default . _Coerce

-- | The name of the method within your code that Lambda calls to execute your function. The format includes the file name. It can also include namespaces and other qualifiers, depending on the runtime. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/programming-model-v2.html Programming Model> .
ufcHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcHandler = lens _ufcHandler (\ s a -> s{_ufcHandler = a})

-- | The amount of time that Lambda allows a function to run before stopping it. The default is 3 seconds. The maximum allowed value is 900 seconds.
ufcTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcTimeout = lens _ufcTimeout (\ s a -> s{_ufcTimeout = a}) . mapping _Nat

-- | Set @Mode@ to @Active@ to sample and trace a subset of incoming requests with AWS X-Ray.
ufcTracingConfig :: Lens' UpdateFunctionConfiguration (Maybe TracingConfig)
ufcTracingConfig = lens _ufcTracingConfig (\ s a -> s{_ufcTracingConfig = a})

-- | A description of the function.
ufcDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcDescription = lens _ufcDescription (\ s a -> s{_ufcDescription = a})

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
ufcRevisionId :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRevisionId = lens _ufcRevisionId (\ s a -> s{_ufcRevisionId = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
ufcFunctionName :: Lens' UpdateFunctionConfiguration Text
ufcFunctionName = lens _ufcFunctionName (\ s a -> s{_ufcFunctionName = a})

instance AWSRequest UpdateFunctionConfiguration where
        type Rs UpdateFunctionConfiguration =
             FunctionConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateFunctionConfiguration where

instance NFData UpdateFunctionConfiguration where

instance ToHeaders UpdateFunctionConfiguration where
        toHeaders = const mempty

instance ToJSON UpdateFunctionConfiguration where
        toJSON UpdateFunctionConfiguration'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _ufcMemorySize,
                  ("Runtime" .=) <$> _ufcRuntime,
                  ("KMSKeyArn" .=) <$> _ufcKMSKeyARN,
                  ("Environment" .=) <$> _ufcEnvironment,
                  ("DeadLetterConfig" .=) <$> _ufcDeadLetterConfig,
                  ("Role" .=) <$> _ufcRole,
                  ("VpcConfig" .=) <$> _ufcVPCConfig,
                  ("Layers" .=) <$> _ufcLayers,
                  ("Handler" .=) <$> _ufcHandler,
                  ("Timeout" .=) <$> _ufcTimeout,
                  ("TracingConfig" .=) <$> _ufcTracingConfig,
                  ("Description" .=) <$> _ufcDescription,
                  ("RevisionId" .=) <$> _ufcRevisionId])

instance ToPath UpdateFunctionConfiguration where
        toPath UpdateFunctionConfiguration'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _ufcFunctionName,
               "/configuration"]

instance ToQuery UpdateFunctionConfiguration where
        toQuery = const mempty
