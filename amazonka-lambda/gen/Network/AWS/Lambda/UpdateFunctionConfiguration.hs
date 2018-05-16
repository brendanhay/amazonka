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
-- Updates the configuration parameters for the specified Lambda function by using the values provided in the request. You provide only the parameters you want to change. This operation must only be used on an existing Lambda function and cannot be used to update the function's code.
--
--
-- If you are using the versioning feature, note this API will always update the $LATEST version of your Lambda function. For information about the versioning feature, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:UpdateFunctionConfiguration@ action.
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
-- /See:/ 'updateFunctionConfiguration' smart constructor.
data UpdateFunctionConfiguration = UpdateFunctionConfiguration'
  { _ufcMemorySize       :: !(Maybe Nat)
  , _ufcRuntime          :: !(Maybe Runtime)
  , _ufcKMSKeyARN        :: !(Maybe Text)
  , _ufcEnvironment      :: !(Maybe Environment)
  , _ufcDeadLetterConfig :: !(Maybe DeadLetterConfig)
  , _ufcRole             :: !(Maybe Text)
  , _ufcVPCConfig        :: !(Maybe VPCConfig)
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
-- * 'ufcMemorySize' - The amount of memory, in MB, your Lambda function is given. AWS Lambda uses this memory size to infer the amount of CPU allocated to your function. Your function use-case determines your CPU and memory requirements. For example, a database operation might need less memory compared to an image processing function. The default value is 128 MB. The value must be a multiple of 64 MB.
--
-- * 'ufcRuntime' - The runtime environment for the Lambda function. To use the Python runtime v3.6, set the value to "python3.6". To use the Python runtime v2.7, set the value to "python2.7". To use the Node.js runtime v6.10, set the value to "nodejs6.10". To use the Node.js runtime v4.3, set the value to "nodejs4.3". To use the .NET Core runtime v1.0, set the value to "dotnetcore1.0". To use the .NET Core runtime v2.0, set the value to "dotnetcore2.0".
--
-- * 'ufcKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If you elect to use the AWS Lambda default service key, pass in an empty string ("") for this parameter.
--
-- * 'ufcEnvironment' - The parent object that contains your environment's configuration settings.
--
-- * 'ufcDeadLetterConfig' - The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
--
-- * 'ufcRole' - The Amazon Resource Name (ARN) of the IAM role that Lambda will assume when it executes your function.
--
-- * 'ufcVPCConfig' - Undocumented member.
--
-- * 'ufcHandler' - The function that Lambda calls to begin executing your function. For Node.js, it is the @module-name.export@ value in your function.
--
-- * 'ufcTimeout' - The function execution time at which AWS Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
--
-- * 'ufcTracingConfig' - The parent object that contains your function's tracing settings.
--
-- * 'ufcDescription' - A short user-defined function description. AWS Lambda does not use this value. Assign a meaningful description as you see fit.
--
-- * 'ufcRevisionId' - An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
--
-- * 'ufcFunctionName' - The name of the Lambda function. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 character in length.
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
    , _ufcHandler = Nothing
    , _ufcTimeout = Nothing
    , _ufcTracingConfig = Nothing
    , _ufcDescription = Nothing
    , _ufcRevisionId = Nothing
    , _ufcFunctionName = pFunctionName_
    }


-- | The amount of memory, in MB, your Lambda function is given. AWS Lambda uses this memory size to infer the amount of CPU allocated to your function. Your function use-case determines your CPU and memory requirements. For example, a database operation might need less memory compared to an image processing function. The default value is 128 MB. The value must be a multiple of 64 MB.
ufcMemorySize :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcMemorySize = lens _ufcMemorySize (\ s a -> s{_ufcMemorySize = a}) . mapping _Nat

-- | The runtime environment for the Lambda function. To use the Python runtime v3.6, set the value to "python3.6". To use the Python runtime v2.7, set the value to "python2.7". To use the Node.js runtime v6.10, set the value to "nodejs6.10". To use the Node.js runtime v4.3, set the value to "nodejs4.3". To use the .NET Core runtime v1.0, set the value to "dotnetcore1.0". To use the .NET Core runtime v2.0, set the value to "dotnetcore2.0".
ufcRuntime :: Lens' UpdateFunctionConfiguration (Maybe Runtime)
ufcRuntime = lens _ufcRuntime (\ s a -> s{_ufcRuntime = a})

-- | The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If you elect to use the AWS Lambda default service key, pass in an empty string ("") for this parameter.
ufcKMSKeyARN :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcKMSKeyARN = lens _ufcKMSKeyARN (\ s a -> s{_ufcKMSKeyARN = a})

-- | The parent object that contains your environment's configuration settings.
ufcEnvironment :: Lens' UpdateFunctionConfiguration (Maybe Environment)
ufcEnvironment = lens _ufcEnvironment (\ s a -> s{_ufcEnvironment = a})

-- | The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
ufcDeadLetterConfig :: Lens' UpdateFunctionConfiguration (Maybe DeadLetterConfig)
ufcDeadLetterConfig = lens _ufcDeadLetterConfig (\ s a -> s{_ufcDeadLetterConfig = a})

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda will assume when it executes your function.
ufcRole :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRole = lens _ufcRole (\ s a -> s{_ufcRole = a})

-- | Undocumented member.
ufcVPCConfig :: Lens' UpdateFunctionConfiguration (Maybe VPCConfig)
ufcVPCConfig = lens _ufcVPCConfig (\ s a -> s{_ufcVPCConfig = a})

-- | The function that Lambda calls to begin executing your function. For Node.js, it is the @module-name.export@ value in your function.
ufcHandler :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcHandler = lens _ufcHandler (\ s a -> s{_ufcHandler = a})

-- | The function execution time at which AWS Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
ufcTimeout :: Lens' UpdateFunctionConfiguration (Maybe Natural)
ufcTimeout = lens _ufcTimeout (\ s a -> s{_ufcTimeout = a}) . mapping _Nat

-- | The parent object that contains your function's tracing settings.
ufcTracingConfig :: Lens' UpdateFunctionConfiguration (Maybe TracingConfig)
ufcTracingConfig = lens _ufcTracingConfig (\ s a -> s{_ufcTracingConfig = a})

-- | A short user-defined function description. AWS Lambda does not use this value. Assign a meaningful description as you see fit.
ufcDescription :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcDescription = lens _ufcDescription (\ s a -> s{_ufcDescription = a})

-- | An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
ufcRevisionId :: Lens' UpdateFunctionConfiguration (Maybe Text)
ufcRevisionId = lens _ufcRevisionId (\ s a -> s{_ufcRevisionId = a})

-- | The name of the Lambda function. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 character in length.
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
