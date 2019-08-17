{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Product where

import Network.AWS.Lambda.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Limits that are related to concurrency and code storage. All file and storage sizes are in bytes.
--
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { _alConcurrentExecutions           :: !(Maybe Int)
  , _alTotalCodeSize                  :: !(Maybe Integer)
  , _alUnreservedConcurrentExecutions :: !(Maybe Nat)
  , _alCodeSizeUnzipped               :: !(Maybe Integer)
  , _alCodeSizeZipped                 :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alConcurrentExecutions' - The maximum number of simultaneous function executions.
--
-- * 'alTotalCodeSize' - The amount of storage space that you can use for all deployment packages and layer archives.
--
-- * 'alUnreservedConcurrentExecutions' - The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
--
-- * 'alCodeSizeUnzipped' - The maximum size of your function's code and layers when they're extracted.
--
-- * 'alCodeSizeZipped' - The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
accountLimit
    :: AccountLimit
accountLimit =
  AccountLimit'
    { _alConcurrentExecutions = Nothing
    , _alTotalCodeSize = Nothing
    , _alUnreservedConcurrentExecutions = Nothing
    , _alCodeSizeUnzipped = Nothing
    , _alCodeSizeZipped = Nothing
    }


-- | The maximum number of simultaneous function executions.
alConcurrentExecutions :: Lens' AccountLimit (Maybe Int)
alConcurrentExecutions = lens _alConcurrentExecutions (\ s a -> s{_alConcurrentExecutions = a})

-- | The amount of storage space that you can use for all deployment packages and layer archives.
alTotalCodeSize :: Lens' AccountLimit (Maybe Integer)
alTotalCodeSize = lens _alTotalCodeSize (\ s a -> s{_alTotalCodeSize = a})

-- | The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
alUnreservedConcurrentExecutions :: Lens' AccountLimit (Maybe Natural)
alUnreservedConcurrentExecutions = lens _alUnreservedConcurrentExecutions (\ s a -> s{_alUnreservedConcurrentExecutions = a}) . mapping _Nat

-- | The maximum size of your function's code and layers when they're extracted.
alCodeSizeUnzipped :: Lens' AccountLimit (Maybe Integer)
alCodeSizeUnzipped = lens _alCodeSizeUnzipped (\ s a -> s{_alCodeSizeUnzipped = a})

-- | The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
alCodeSizeZipped :: Lens' AccountLimit (Maybe Integer)
alCodeSizeZipped = lens _alCodeSizeZipped (\ s a -> s{_alCodeSizeZipped = a})

instance FromJSON AccountLimit where
        parseJSON
          = withObject "AccountLimit"
              (\ x ->
                 AccountLimit' <$>
                   (x .:? "ConcurrentExecutions") <*>
                     (x .:? "TotalCodeSize")
                     <*> (x .:? "UnreservedConcurrentExecutions")
                     <*> (x .:? "CodeSizeUnzipped")
                     <*> (x .:? "CodeSizeZipped"))

instance Hashable AccountLimit where

instance NFData AccountLimit where

-- | The number of functions and amount of storage in use.
--
--
--
-- /See:/ 'accountUsage' smart constructor.
data AccountUsage = AccountUsage'
  { _auTotalCodeSize :: !(Maybe Integer)
  , _auFunctionCount :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auTotalCodeSize' - The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
--
-- * 'auFunctionCount' - The number of Lambda functions.
accountUsage
    :: AccountUsage
accountUsage =
  AccountUsage' {_auTotalCodeSize = Nothing, _auFunctionCount = Nothing}


-- | The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
auTotalCodeSize :: Lens' AccountUsage (Maybe Integer)
auTotalCodeSize = lens _auTotalCodeSize (\ s a -> s{_auTotalCodeSize = a})

-- | The number of Lambda functions.
auFunctionCount :: Lens' AccountUsage (Maybe Integer)
auFunctionCount = lens _auFunctionCount (\ s a -> s{_auFunctionCount = a})

instance FromJSON AccountUsage where
        parseJSON
          = withObject "AccountUsage"
              (\ x ->
                 AccountUsage' <$>
                   (x .:? "TotalCodeSize") <*> (x .:? "FunctionCount"))

instance Hashable AccountUsage where

instance NFData AccountUsage where

-- | Provides configuration information about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
--
--
-- /See:/ 'aliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { _acRoutingConfig   :: !(Maybe AliasRoutingConfiguration)
  , _acName            :: !(Maybe Text)
  , _acFunctionVersion :: !(Maybe Text)
  , _acAliasARN        :: !(Maybe Text)
  , _acDescription     :: !(Maybe Text)
  , _acRevisionId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AliasConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acRoutingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- * 'acName' - The name of the alias.
--
-- * 'acFunctionVersion' - The function version that the alias invokes.
--
-- * 'acAliasARN' - The Amazon Resource Name (ARN) of the alias.
--
-- * 'acDescription' - A description of the alias.
--
-- * 'acRevisionId' - A unique identifier that changes when you update the alias.
aliasConfiguration
    :: AliasConfiguration
aliasConfiguration =
  AliasConfiguration'
    { _acRoutingConfig = Nothing
    , _acName = Nothing
    , _acFunctionVersion = Nothing
    , _acAliasARN = Nothing
    , _acDescription = Nothing
    , _acRevisionId = Nothing
    }


-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
acRoutingConfig :: Lens' AliasConfiguration (Maybe AliasRoutingConfiguration)
acRoutingConfig = lens _acRoutingConfig (\ s a -> s{_acRoutingConfig = a})

-- | The name of the alias.
acName :: Lens' AliasConfiguration (Maybe Text)
acName = lens _acName (\ s a -> s{_acName = a})

-- | The function version that the alias invokes.
acFunctionVersion :: Lens' AliasConfiguration (Maybe Text)
acFunctionVersion = lens _acFunctionVersion (\ s a -> s{_acFunctionVersion = a})

-- | The Amazon Resource Name (ARN) of the alias.
acAliasARN :: Lens' AliasConfiguration (Maybe Text)
acAliasARN = lens _acAliasARN (\ s a -> s{_acAliasARN = a})

-- | A description of the alias.
acDescription :: Lens' AliasConfiguration (Maybe Text)
acDescription = lens _acDescription (\ s a -> s{_acDescription = a})

-- | A unique identifier that changes when you update the alias.
acRevisionId :: Lens' AliasConfiguration (Maybe Text)
acRevisionId = lens _acRevisionId (\ s a -> s{_acRevisionId = a})

instance FromJSON AliasConfiguration where
        parseJSON
          = withObject "AliasConfiguration"
              (\ x ->
                 AliasConfiguration' <$>
                   (x .:? "RoutingConfig") <*> (x .:? "Name") <*>
                     (x .:? "FunctionVersion")
                     <*> (x .:? "AliasArn")
                     <*> (x .:? "Description")
                     <*> (x .:? "RevisionId"))

instance Hashable AliasConfiguration where

instance NFData AliasConfiguration where

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting> configuration of a Lambda function alias.
--
--
--
-- /See:/ 'aliasRoutingConfiguration' smart constructor.
newtype AliasRoutingConfiguration = AliasRoutingConfiguration'
  { _arcAdditionalVersionWeights :: Maybe (Map Text Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AliasRoutingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arcAdditionalVersionWeights' - The name of the second alias, and the percentage of traffic that's routed to it.
aliasRoutingConfiguration
    :: AliasRoutingConfiguration
aliasRoutingConfiguration =
  AliasRoutingConfiguration' {_arcAdditionalVersionWeights = Nothing}


-- | The name of the second alias, and the percentage of traffic that's routed to it.
arcAdditionalVersionWeights :: Lens' AliasRoutingConfiguration (HashMap Text Double)
arcAdditionalVersionWeights = lens _arcAdditionalVersionWeights (\ s a -> s{_arcAdditionalVersionWeights = a}) . _Default . _Map

instance FromJSON AliasRoutingConfiguration where
        parseJSON
          = withObject "AliasRoutingConfiguration"
              (\ x ->
                 AliasRoutingConfiguration' <$>
                   (x .:? "AdditionalVersionWeights" .!= mempty))

instance Hashable AliasRoutingConfiguration where

instance NFData AliasRoutingConfiguration where

instance ToJSON AliasRoutingConfiguration where
        toJSON AliasRoutingConfiguration'{..}
          = object
              (catMaybes
                 [("AdditionalVersionWeights" .=) <$>
                    _arcAdditionalVersionWeights])

-- | /See:/ 'concurrency' smart constructor.
newtype Concurrency = Concurrency'
  { _cReservedConcurrentExecutions :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Concurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cReservedConcurrentExecutions' - The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
concurrency
    :: Concurrency
concurrency = Concurrency' {_cReservedConcurrentExecutions = Nothing}


-- | The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
cReservedConcurrentExecutions :: Lens' Concurrency (Maybe Natural)
cReservedConcurrentExecutions = lens _cReservedConcurrentExecutions (\ s a -> s{_cReservedConcurrentExecutions = a}) . mapping _Nat

instance FromJSON Concurrency where
        parseJSON
          = withObject "Concurrency"
              (\ x ->
                 Concurrency' <$>
                   (x .:? "ReservedConcurrentExecutions"))

instance Hashable Concurrency where

instance NFData Concurrency where

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/dlq.html dead letter queue> for failed asynchronous invocations.
--
--
--
-- /See:/ 'deadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { _dlcTargetARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeadLetterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcTargetARN' - The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
deadLetterConfig
    :: DeadLetterConfig
deadLetterConfig = DeadLetterConfig' {_dlcTargetARN = Nothing}


-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
dlcTargetARN :: Lens' DeadLetterConfig (Maybe Text)
dlcTargetARN = lens _dlcTargetARN (\ s a -> s{_dlcTargetARN = a})

instance FromJSON DeadLetterConfig where
        parseJSON
          = withObject "DeadLetterConfig"
              (\ x -> DeadLetterConfig' <$> (x .:? "TargetArn"))

instance Hashable DeadLetterConfig where

instance NFData DeadLetterConfig where

instance ToJSON DeadLetterConfig where
        toJSON DeadLetterConfig'{..}
          = object
              (catMaybes [("TargetArn" .=) <$> _dlcTargetARN])

-- | A function's environment variable settings.
--
--
--
-- /See:/ 'environment' smart constructor.
newtype Environment = Environment'
  { _eVariables :: Maybe (Sensitive (Map Text (Sensitive Text)))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Environment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eVariables' - Environment variable key-value pairs.
environment
    :: Environment
environment = Environment' {_eVariables = Nothing}


-- | Environment variable key-value pairs.
eVariables :: Lens' Environment (Maybe (HashMap Text Text))
eVariables = lens _eVariables (\ s a -> s{_eVariables = a}) . mapping (_Sensitive . _Map)

instance Hashable Environment where

instance NFData Environment where

instance ToJSON Environment where
        toJSON Environment'{..}
          = object
              (catMaybes [("Variables" .=) <$> _eVariables])

-- | Error messages for environment variables that couldn't be applied.
--
--
--
-- /See:/ 'environmentError' smart constructor.
data EnvironmentError = EnvironmentError'
  { _eeErrorCode :: !(Maybe Text)
  , _eeMessage   :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeErrorCode' - The error code.
--
-- * 'eeMessage' - The error message.
environmentError
    :: EnvironmentError
environmentError =
  EnvironmentError' {_eeErrorCode = Nothing, _eeMessage = Nothing}


-- | The error code.
eeErrorCode :: Lens' EnvironmentError (Maybe Text)
eeErrorCode = lens _eeErrorCode (\ s a -> s{_eeErrorCode = a})

-- | The error message.
eeMessage :: Lens' EnvironmentError (Maybe Text)
eeMessage = lens _eeMessage (\ s a -> s{_eeMessage = a}) . mapping _Sensitive

instance FromJSON EnvironmentError where
        parseJSON
          = withObject "EnvironmentError"
              (\ x ->
                 EnvironmentError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "Message"))

instance Hashable EnvironmentError where

instance NFData EnvironmentError where

-- | The results of a configuration update that applied environment variables.
--
--
--
-- /See:/ 'environmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { _envVariables :: !(Maybe (Sensitive (Map Text (Sensitive Text))))
  , _envError     :: !(Maybe EnvironmentError)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'envVariables' - Environment variable key-value pairs.
--
-- * 'envError' - Error messages for environment variables that couldn't be applied.
environmentResponse
    :: EnvironmentResponse
environmentResponse =
  EnvironmentResponse' {_envVariables = Nothing, _envError = Nothing}


-- | Environment variable key-value pairs.
envVariables :: Lens' EnvironmentResponse (Maybe (HashMap Text Text))
envVariables = lens _envVariables (\ s a -> s{_envVariables = a}) . mapping (_Sensitive . _Map)

-- | Error messages for environment variables that couldn't be applied.
envError :: Lens' EnvironmentResponse (Maybe EnvironmentError)
envError = lens _envError (\ s a -> s{_envError = a})

instance FromJSON EnvironmentResponse where
        parseJSON
          = withObject "EnvironmentResponse"
              (\ x ->
                 EnvironmentResponse' <$>
                   (x .:? "Variables" .!= mempty) <*> (x .:? "Error"))

instance Hashable EnvironmentResponse where

instance NFData EnvironmentResponse where

-- | A mapping between an AWS resource and an AWS Lambda function. See 'CreateEventSourceMapping' for details.
--
--
--
-- /See:/ 'eventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { _esmcEventSourceARN        :: !(Maybe Text)
  , _esmcState                 :: !(Maybe Text)
  , _esmcFunctionARN           :: !(Maybe Text)
  , _esmcUUId                  :: !(Maybe Text)
  , _esmcLastProcessingResult  :: !(Maybe Text)
  , _esmcBatchSize             :: !(Maybe Nat)
  , _esmcStateTransitionReason :: !(Maybe Text)
  , _esmcLastModified          :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSourceMappingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esmcEventSourceARN' - The Amazon Resource Name (ARN) of the event source.
--
-- * 'esmcState' - The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
--
-- * 'esmcFunctionARN' - The ARN of the Lambda function.
--
-- * 'esmcUUId' - The identifier of the event source mapping.
--
-- * 'esmcLastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
--
-- * 'esmcBatchSize' - The maximum number of items to retrieve in a single batch.
--
-- * 'esmcStateTransitionReason' - The cause of the last state change, either @User initiated@ or @Lambda initiated@ .
--
-- * 'esmcLastModified' - The date that the event source mapping was last updated.
eventSourceMappingConfiguration
    :: EventSourceMappingConfiguration
eventSourceMappingConfiguration =
  EventSourceMappingConfiguration'
    { _esmcEventSourceARN = Nothing
    , _esmcState = Nothing
    , _esmcFunctionARN = Nothing
    , _esmcUUId = Nothing
    , _esmcLastProcessingResult = Nothing
    , _esmcBatchSize = Nothing
    , _esmcStateTransitionReason = Nothing
    , _esmcLastModified = Nothing
    }


-- | The Amazon Resource Name (ARN) of the event source.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\ s a -> s{_esmcEventSourceARN = a})

-- | The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\ s a -> s{_esmcState = a})

-- | The ARN of the Lambda function.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\ s a -> s{_esmcFunctionARN = a})

-- | The identifier of the event source mapping.
esmcUUId :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUId = lens _esmcUUId (\ s a -> s{_esmcUUId = a})

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\ s a -> s{_esmcLastProcessingResult = a})

-- | The maximum number of items to retrieve in a single batch.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\ s a -> s{_esmcBatchSize = a}) . mapping _Nat

-- | The cause of the last state change, either @User initiated@ or @Lambda initiated@ .
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\ s a -> s{_esmcStateTransitionReason = a})

-- | The date that the event source mapping was last updated.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\ s a -> s{_esmcLastModified = a}) . mapping _Time

instance FromJSON EventSourceMappingConfiguration
         where
        parseJSON
          = withObject "EventSourceMappingConfiguration"
              (\ x ->
                 EventSourceMappingConfiguration' <$>
                   (x .:? "EventSourceArn") <*> (x .:? "State") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "UUID")
                     <*> (x .:? "LastProcessingResult")
                     <*> (x .:? "BatchSize")
                     <*> (x .:? "StateTransitionReason")
                     <*> (x .:? "LastModified"))

instance Hashable EventSourceMappingConfiguration
         where

instance NFData EventSourceMappingConfiguration where

-- | The code for the Lambda function. You can specify either an object in Amazon S3, or upload a deployment package directly.
--
--
--
-- /See:/ 'functionCode' smart constructor.
data FunctionCode = FunctionCode'
  { _fcS3ObjectVersion :: !(Maybe Text)
  , _fcS3Key           :: !(Maybe Text)
  , _fcZipFile         :: !(Maybe (Sensitive Base64))
  , _fcS3Bucket        :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcS3ObjectVersion' - For versioned objects, the version of the deployment package object to use.
--
-- * 'fcS3Key' - The Amazon S3 key of the deployment package.
--
-- * 'fcZipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'fcS3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
functionCode
    :: FunctionCode
functionCode =
  FunctionCode'
    { _fcS3ObjectVersion = Nothing
    , _fcS3Key = Nothing
    , _fcZipFile = Nothing
    , _fcS3Bucket = Nothing
    }


-- | For versioned objects, the version of the deployment package object to use.
fcS3ObjectVersion :: Lens' FunctionCode (Maybe Text)
fcS3ObjectVersion = lens _fcS3ObjectVersion (\ s a -> s{_fcS3ObjectVersion = a})

-- | The Amazon S3 key of the deployment package.
fcS3Key :: Lens' FunctionCode (Maybe Text)
fcS3Key = lens _fcS3Key (\ s a -> s{_fcS3Key = a})

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
fcZipFile :: Lens' FunctionCode (Maybe ByteString)
fcZipFile = lens _fcZipFile (\ s a -> s{_fcZipFile = a}) . mapping (_Sensitive . _Base64)

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
fcS3Bucket :: Lens' FunctionCode (Maybe Text)
fcS3Bucket = lens _fcS3Bucket (\ s a -> s{_fcS3Bucket = a})

instance Hashable FunctionCode where

instance NFData FunctionCode where

instance ToJSON FunctionCode where
        toJSON FunctionCode'{..}
          = object
              (catMaybes
                 [("S3ObjectVersion" .=) <$> _fcS3ObjectVersion,
                  ("S3Key" .=) <$> _fcS3Key,
                  ("ZipFile" .=) <$> _fcZipFile,
                  ("S3Bucket" .=) <$> _fcS3Bucket])

-- | Details about a function's deployment package.
--
--
--
-- /See:/ 'functionCodeLocation' smart constructor.
data FunctionCodeLocation = FunctionCodeLocation'
  { _fclLocation       :: !(Maybe Text)
  , _fclRepositoryType :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionCodeLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fclLocation' - A presigned URL that you can use to download the deployment package.
--
-- * 'fclRepositoryType' - The service that's hosting the file.
functionCodeLocation
    :: FunctionCodeLocation
functionCodeLocation =
  FunctionCodeLocation' {_fclLocation = Nothing, _fclRepositoryType = Nothing}


-- | A presigned URL that you can use to download the deployment package.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\ s a -> s{_fclLocation = a})

-- | The service that's hosting the file.
fclRepositoryType :: Lens' FunctionCodeLocation (Maybe Text)
fclRepositoryType = lens _fclRepositoryType (\ s a -> s{_fclRepositoryType = a})

instance FromJSON FunctionCodeLocation where
        parseJSON
          = withObject "FunctionCodeLocation"
              (\ x ->
                 FunctionCodeLocation' <$>
                   (x .:? "Location") <*> (x .:? "RepositoryType"))

instance Hashable FunctionCodeLocation where

instance NFData FunctionCodeLocation where

-- | Details about a function's configuration.
--
--
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { _fcMemorySize       :: !(Maybe Nat)
  , _fcRuntime          :: !(Maybe Runtime)
  , _fcFunctionARN      :: !(Maybe Text)
  , _fcKMSKeyARN        :: !(Maybe Text)
  , _fcEnvironment      :: !(Maybe EnvironmentResponse)
  , _fcDeadLetterConfig :: !(Maybe DeadLetterConfig)
  , _fcRole             :: !(Maybe Text)
  , _fcVPCConfig        :: !(Maybe VPCConfigResponse)
  , _fcVersion          :: !(Maybe Text)
  , _fcFunctionName     :: !(Maybe Text)
  , _fcLayers           :: !(Maybe [Layer])
  , _fcCodeSize         :: !(Maybe Integer)
  , _fcHandler          :: !(Maybe Text)
  , _fcTimeout          :: !(Maybe Nat)
  , _fcLastModified     :: !(Maybe Text)
  , _fcCodeSha256       :: !(Maybe Text)
  , _fcTracingConfig    :: !(Maybe TracingConfigResponse)
  , _fcDescription      :: !(Maybe Text)
  , _fcRevisionId       :: !(Maybe Text)
  , _fcMasterARN        :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize' - The memory that's allocated to the function.
--
-- * 'fcRuntime' - The runtime environment for the Lambda function.
--
-- * 'fcFunctionARN' - The function's Amazon Resource Name (ARN).
--
-- * 'fcKMSKeyARN' - The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer-managed CMK.
--
-- * 'fcEnvironment' - The function's environment variables.
--
-- * 'fcDeadLetterConfig' - The function's dead letter queue.
--
-- * 'fcRole' - The function's execution role.
--
-- * 'fcVPCConfig' - The function's networking configuration.
--
-- * 'fcVersion' - The version of the Lambda function.
--
-- * 'fcFunctionName' - The name of the function.
--
-- * 'fcLayers' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
--
-- * 'fcCodeSize' - The size of the function's deployment package, in bytes.
--
-- * 'fcHandler' - The function that Lambda calls to begin executing your function.
--
-- * 'fcTimeout' - The amount of time that Lambda allows a function to run before stopping it.
--
-- * 'fcLastModified' - The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- * 'fcCodeSha256' - The SHA256 hash of the function's deployment package.
--
-- * 'fcTracingConfig' - The function's AWS X-Ray tracing configuration.
--
-- * 'fcDescription' - The function's description.
--
-- * 'fcRevisionId' - The latest updated revision of the function or alias.
--
-- * 'fcMasterARN' - For Lambda@Edge functions, the ARN of the master function.
functionConfiguration
    :: FunctionConfiguration
functionConfiguration =
  FunctionConfiguration'
    { _fcMemorySize = Nothing
    , _fcRuntime = Nothing
    , _fcFunctionARN = Nothing
    , _fcKMSKeyARN = Nothing
    , _fcEnvironment = Nothing
    , _fcDeadLetterConfig = Nothing
    , _fcRole = Nothing
    , _fcVPCConfig = Nothing
    , _fcVersion = Nothing
    , _fcFunctionName = Nothing
    , _fcLayers = Nothing
    , _fcCodeSize = Nothing
    , _fcHandler = Nothing
    , _fcTimeout = Nothing
    , _fcLastModified = Nothing
    , _fcCodeSha256 = Nothing
    , _fcTracingConfig = Nothing
    , _fcDescription = Nothing
    , _fcRevisionId = Nothing
    , _fcMasterARN = Nothing
    }


-- | The memory that's allocated to the function.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a}) . mapping _Nat

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Runtime)
fcRuntime = lens _fcRuntime (\ s a -> s{_fcRuntime = a})

-- | The function's Amazon Resource Name (ARN).
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\ s a -> s{_fcFunctionARN = a})

-- | The KMS key that's used to encrypt the function's environment variables. This key is only returned if you've configured a customer-managed CMK.
fcKMSKeyARN :: Lens' FunctionConfiguration (Maybe Text)
fcKMSKeyARN = lens _fcKMSKeyARN (\ s a -> s{_fcKMSKeyARN = a})

-- | The function's environment variables.
fcEnvironment :: Lens' FunctionConfiguration (Maybe EnvironmentResponse)
fcEnvironment = lens _fcEnvironment (\ s a -> s{_fcEnvironment = a})

-- | The function's dead letter queue.
fcDeadLetterConfig :: Lens' FunctionConfiguration (Maybe DeadLetterConfig)
fcDeadLetterConfig = lens _fcDeadLetterConfig (\ s a -> s{_fcDeadLetterConfig = a})

-- | The function's execution role.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\ s a -> s{_fcRole = a})

-- | The function's networking configuration.
fcVPCConfig :: Lens' FunctionConfiguration (Maybe VPCConfigResponse)
fcVPCConfig = lens _fcVPCConfig (\ s a -> s{_fcVPCConfig = a})

-- | The version of the Lambda function.
fcVersion :: Lens' FunctionConfiguration (Maybe Text)
fcVersion = lens _fcVersion (\ s a -> s{_fcVersion = a})

-- | The name of the function.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\ s a -> s{_fcFunctionName = a})

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers> .
fcLayers :: Lens' FunctionConfiguration [Layer]
fcLayers = lens _fcLayers (\ s a -> s{_fcLayers = a}) . _Default . _Coerce

-- | The size of the function's deployment package, in bytes.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\ s a -> s{_fcCodeSize = a})

-- | The function that Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\ s a -> s{_fcHandler = a})

-- | The amount of time that Lambda allows a function to run before stopping it.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a}) . mapping _Nat

-- | The date and time that the function was last updated, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
fcLastModified :: Lens' FunctionConfiguration (Maybe Text)
fcLastModified = lens _fcLastModified (\ s a -> s{_fcLastModified = a})

-- | The SHA256 hash of the function's deployment package.
fcCodeSha256 :: Lens' FunctionConfiguration (Maybe Text)
fcCodeSha256 = lens _fcCodeSha256 (\ s a -> s{_fcCodeSha256 = a})

-- | The function's AWS X-Ray tracing configuration.
fcTracingConfig :: Lens' FunctionConfiguration (Maybe TracingConfigResponse)
fcTracingConfig = lens _fcTracingConfig (\ s a -> s{_fcTracingConfig = a})

-- | The function's description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\ s a -> s{_fcDescription = a})

-- | The latest updated revision of the function or alias.
fcRevisionId :: Lens' FunctionConfiguration (Maybe Text)
fcRevisionId = lens _fcRevisionId (\ s a -> s{_fcRevisionId = a})

-- | For Lambda@Edge functions, the ARN of the master function.
fcMasterARN :: Lens' FunctionConfiguration (Maybe Text)
fcMasterARN = lens _fcMasterARN (\ s a -> s{_fcMasterARN = a})

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "MemorySize") <*> (x .:? "Runtime") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "KMSKeyArn")
                     <*> (x .:? "Environment")
                     <*> (x .:? "DeadLetterConfig")
                     <*> (x .:? "Role")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "Version")
                     <*> (x .:? "FunctionName")
                     <*> (x .:? "Layers" .!= mempty)
                     <*> (x .:? "CodeSize")
                     <*> (x .:? "Handler")
                     <*> (x .:? "Timeout")
                     <*> (x .:? "LastModified")
                     <*> (x .:? "CodeSha256")
                     <*> (x .:? "TracingConfig")
                     <*> (x .:? "Description")
                     <*> (x .:? "RevisionId")
                     <*> (x .:? "MasterArn"))

instance Hashable FunctionConfiguration where

instance NFData FunctionConfiguration where

-- | /See:/ 'getLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { _glvLayerVersionARN    :: !(Maybe Text)
  , _glvContent            :: !(Maybe LayerVersionContentOutput)
  , _glvCreatedDate        :: !(Maybe Text)
  , _glvVersion            :: !(Maybe Integer)
  , _glvLicenseInfo        :: !(Maybe Text)
  , _glvLayerARN           :: !(Maybe Text)
  , _glvDescription        :: !(Maybe Text)
  , _glvCompatibleRuntimes :: !(Maybe [Runtime])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLayerVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvLayerVersionARN' - The ARN of the layer version.
--
-- * 'glvContent' - Details about the layer version.
--
-- * 'glvCreatedDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- * 'glvVersion' - The version number.
--
-- * 'glvLicenseInfo' - The layer's software license.
--
-- * 'glvLayerARN' - The ARN of the layer.
--
-- * 'glvDescription' - The description of the version.
--
-- * 'glvCompatibleRuntimes' - The layer's compatible runtimes.
getLayerVersionResponse
    :: GetLayerVersionResponse
getLayerVersionResponse =
  GetLayerVersionResponse'
    { _glvLayerVersionARN = Nothing
    , _glvContent = Nothing
    , _glvCreatedDate = Nothing
    , _glvVersion = Nothing
    , _glvLicenseInfo = Nothing
    , _glvLayerARN = Nothing
    , _glvDescription = Nothing
    , _glvCompatibleRuntimes = Nothing
    }


-- | The ARN of the layer version.
glvLayerVersionARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvLayerVersionARN = lens _glvLayerVersionARN (\ s a -> s{_glvLayerVersionARN = a})

-- | Details about the layer version.
glvContent :: Lens' GetLayerVersionResponse (Maybe LayerVersionContentOutput)
glvContent = lens _glvContent (\ s a -> s{_glvContent = a})

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
glvCreatedDate :: Lens' GetLayerVersionResponse (Maybe Text)
glvCreatedDate = lens _glvCreatedDate (\ s a -> s{_glvCreatedDate = a})

-- | The version number.
glvVersion :: Lens' GetLayerVersionResponse (Maybe Integer)
glvVersion = lens _glvVersion (\ s a -> s{_glvVersion = a})

-- | The layer's software license.
glvLicenseInfo :: Lens' GetLayerVersionResponse (Maybe Text)
glvLicenseInfo = lens _glvLicenseInfo (\ s a -> s{_glvLicenseInfo = a})

-- | The ARN of the layer.
glvLayerARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvLayerARN = lens _glvLayerARN (\ s a -> s{_glvLayerARN = a})

-- | The description of the version.
glvDescription :: Lens' GetLayerVersionResponse (Maybe Text)
glvDescription = lens _glvDescription (\ s a -> s{_glvDescription = a})

-- | The layer's compatible runtimes.
glvCompatibleRuntimes :: Lens' GetLayerVersionResponse [Runtime]
glvCompatibleRuntimes = lens _glvCompatibleRuntimes (\ s a -> s{_glvCompatibleRuntimes = a}) . _Default . _Coerce

instance FromJSON GetLayerVersionResponse where
        parseJSON
          = withObject "GetLayerVersionResponse"
              (\ x ->
                 GetLayerVersionResponse' <$>
                   (x .:? "LayerVersionArn") <*> (x .:? "Content") <*>
                     (x .:? "CreatedDate")
                     <*> (x .:? "Version")
                     <*> (x .:? "LicenseInfo")
                     <*> (x .:? "LayerArn")
                     <*> (x .:? "Description")
                     <*> (x .:? "CompatibleRuntimes" .!= mempty))

instance Hashable GetLayerVersionResponse where

instance NFData GetLayerVersionResponse where

-- | An <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layer' smart constructor.
data Layer = Layer'
  { _lARN      :: !(Maybe Text)
  , _lCodeSize :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lARN' - The Amazon Resource Name (ARN) of the function layer.
--
-- * 'lCodeSize' - The size of the layer archive in bytes.
layer
    :: Layer
layer = Layer' {_lARN = Nothing, _lCodeSize = Nothing}


-- | The Amazon Resource Name (ARN) of the function layer.
lARN :: Lens' Layer (Maybe Text)
lARN = lens _lARN (\ s a -> s{_lARN = a})

-- | The size of the layer archive in bytes.
lCodeSize :: Lens' Layer (Maybe Integer)
lCodeSize = lens _lCodeSize (\ s a -> s{_lCodeSize = a})

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$> (x .:? "Arn") <*> (x .:? "CodeSize"))

instance Hashable Layer where

instance NFData Layer where

-- | A ZIP archive that contains the contents of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . You can specify either an Amazon S3 location, or upload a layer archive directly.
--
--
--
-- /See:/ 'layerVersionContentInput' smart constructor.
data LayerVersionContentInput = LayerVersionContentInput'
  { _lvciS3ObjectVersion :: !(Maybe Text)
  , _lvciS3Key           :: !(Maybe Text)
  , _lvciZipFile         :: !(Maybe (Sensitive Base64))
  , _lvciS3Bucket        :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'LayerVersionContentInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvciS3ObjectVersion' - For versioned objects, the version of the layer archive object to use.
--
-- * 'lvciS3Key' - The Amazon S3 key of the layer archive.
--
-- * 'lvciZipFile' - The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'lvciS3Bucket' - The Amazon S3 bucket of the layer archive.
layerVersionContentInput
    :: LayerVersionContentInput
layerVersionContentInput =
  LayerVersionContentInput'
    { _lvciS3ObjectVersion = Nothing
    , _lvciS3Key = Nothing
    , _lvciZipFile = Nothing
    , _lvciS3Bucket = Nothing
    }


-- | For versioned objects, the version of the layer archive object to use.
lvciS3ObjectVersion :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3ObjectVersion = lens _lvciS3ObjectVersion (\ s a -> s{_lvciS3ObjectVersion = a})

-- | The Amazon S3 key of the layer archive.
lvciS3Key :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3Key = lens _lvciS3Key (\ s a -> s{_lvciS3Key = a})

-- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
lvciZipFile :: Lens' LayerVersionContentInput (Maybe ByteString)
lvciZipFile = lens _lvciZipFile (\ s a -> s{_lvciZipFile = a}) . mapping (_Sensitive . _Base64)

-- | The Amazon S3 bucket of the layer archive.
lvciS3Bucket :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3Bucket = lens _lvciS3Bucket (\ s a -> s{_lvciS3Bucket = a})

instance Hashable LayerVersionContentInput where

instance NFData LayerVersionContentInput where

instance ToJSON LayerVersionContentInput where
        toJSON LayerVersionContentInput'{..}
          = object
              (catMaybes
                 [("S3ObjectVersion" .=) <$> _lvciS3ObjectVersion,
                  ("S3Key" .=) <$> _lvciS3Key,
                  ("ZipFile" .=) <$> _lvciZipFile,
                  ("S3Bucket" .=) <$> _lvciS3Bucket])

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { _lvcoLocation   :: !(Maybe Text)
  , _lvcoCodeSize   :: !(Maybe Integer)
  , _lvcoCodeSha256 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LayerVersionContentOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvcoLocation' - A link to the layer archive in Amazon S3 that is valid for 10 minutes.
--
-- * 'lvcoCodeSize' - The size of the layer archive in bytes.
--
-- * 'lvcoCodeSha256' - The SHA-256 hash of the layer archive.
layerVersionContentOutput
    :: LayerVersionContentOutput
layerVersionContentOutput =
  LayerVersionContentOutput'
    { _lvcoLocation = Nothing
    , _lvcoCodeSize = Nothing
    , _lvcoCodeSha256 = Nothing
    }


-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
lvcoLocation :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoLocation = lens _lvcoLocation (\ s a -> s{_lvcoLocation = a})

-- | The size of the layer archive in bytes.
lvcoCodeSize :: Lens' LayerVersionContentOutput (Maybe Integer)
lvcoCodeSize = lens _lvcoCodeSize (\ s a -> s{_lvcoCodeSize = a})

-- | The SHA-256 hash of the layer archive.
lvcoCodeSha256 :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoCodeSha256 = lens _lvcoCodeSha256 (\ s a -> s{_lvcoCodeSha256 = a})

instance FromJSON LayerVersionContentOutput where
        parseJSON
          = withObject "LayerVersionContentOutput"
              (\ x ->
                 LayerVersionContentOutput' <$>
                   (x .:? "Location") <*> (x .:? "CodeSize") <*>
                     (x .:? "CodeSha256"))

instance Hashable LayerVersionContentOutput where

instance NFData LayerVersionContentOutput where

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { _lvliLayerVersionARN    :: !(Maybe Text)
  , _lvliCreatedDate        :: !(Maybe Text)
  , _lvliVersion            :: !(Maybe Integer)
  , _lvliLicenseInfo        :: !(Maybe Text)
  , _lvliDescription        :: !(Maybe Text)
  , _lvliCompatibleRuntimes :: !(Maybe [Runtime])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LayerVersionsListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvliLayerVersionARN' - The ARN of the layer version.
--
-- * 'lvliCreatedDate' - The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
--
-- * 'lvliVersion' - The version number.
--
-- * 'lvliLicenseInfo' - The layer's open-source license.
--
-- * 'lvliDescription' - The description of the version.
--
-- * 'lvliCompatibleRuntimes' - The layer's compatible runtimes.
layerVersionsListItem
    :: LayerVersionsListItem
layerVersionsListItem =
  LayerVersionsListItem'
    { _lvliLayerVersionARN = Nothing
    , _lvliCreatedDate = Nothing
    , _lvliVersion = Nothing
    , _lvliLicenseInfo = Nothing
    , _lvliDescription = Nothing
    , _lvliCompatibleRuntimes = Nothing
    }


-- | The ARN of the layer version.
lvliLayerVersionARN :: Lens' LayerVersionsListItem (Maybe Text)
lvliLayerVersionARN = lens _lvliLayerVersionARN (\ s a -> s{_lvliLayerVersionARN = a})

-- | The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
lvliCreatedDate :: Lens' LayerVersionsListItem (Maybe Text)
lvliCreatedDate = lens _lvliCreatedDate (\ s a -> s{_lvliCreatedDate = a})

-- | The version number.
lvliVersion :: Lens' LayerVersionsListItem (Maybe Integer)
lvliVersion = lens _lvliVersion (\ s a -> s{_lvliVersion = a})

-- | The layer's open-source license.
lvliLicenseInfo :: Lens' LayerVersionsListItem (Maybe Text)
lvliLicenseInfo = lens _lvliLicenseInfo (\ s a -> s{_lvliLicenseInfo = a})

-- | The description of the version.
lvliDescription :: Lens' LayerVersionsListItem (Maybe Text)
lvliDescription = lens _lvliDescription (\ s a -> s{_lvliDescription = a})

-- | The layer's compatible runtimes.
lvliCompatibleRuntimes :: Lens' LayerVersionsListItem [Runtime]
lvliCompatibleRuntimes = lens _lvliCompatibleRuntimes (\ s a -> s{_lvliCompatibleRuntimes = a}) . _Default . _Coerce

instance FromJSON LayerVersionsListItem where
        parseJSON
          = withObject "LayerVersionsListItem"
              (\ x ->
                 LayerVersionsListItem' <$>
                   (x .:? "LayerVersionArn") <*> (x .:? "CreatedDate")
                     <*> (x .:? "Version")
                     <*> (x .:? "LicenseInfo")
                     <*> (x .:? "Description")
                     <*> (x .:? "CompatibleRuntimes" .!= mempty))

instance Hashable LayerVersionsListItem where

instance NFData LayerVersionsListItem where

-- | Details about an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { _lliLayerName             :: !(Maybe Text)
  , _lliLatestMatchingVersion :: !(Maybe LayerVersionsListItem)
  , _lliLayerARN              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LayersListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lliLayerName' - The name of the layer.
--
-- * 'lliLatestMatchingVersion' - The newest version of the layer.
--
-- * 'lliLayerARN' - The Amazon Resource Name (ARN) of the function layer.
layersListItem
    :: LayersListItem
layersListItem =
  LayersListItem'
    { _lliLayerName = Nothing
    , _lliLatestMatchingVersion = Nothing
    , _lliLayerARN = Nothing
    }


-- | The name of the layer.
lliLayerName :: Lens' LayersListItem (Maybe Text)
lliLayerName = lens _lliLayerName (\ s a -> s{_lliLayerName = a})

-- | The newest version of the layer.
lliLatestMatchingVersion :: Lens' LayersListItem (Maybe LayerVersionsListItem)
lliLatestMatchingVersion = lens _lliLatestMatchingVersion (\ s a -> s{_lliLatestMatchingVersion = a})

-- | The Amazon Resource Name (ARN) of the function layer.
lliLayerARN :: Lens' LayersListItem (Maybe Text)
lliLayerARN = lens _lliLayerARN (\ s a -> s{_lliLayerARN = a})

instance FromJSON LayersListItem where
        parseJSON
          = withObject "LayersListItem"
              (\ x ->
                 LayersListItem' <$>
                   (x .:? "LayerName") <*>
                     (x .:? "LatestMatchingVersion")
                     <*> (x .:? "LayerArn"))

instance Hashable LayersListItem where

instance NFData LayersListItem where

-- | The function's AWS X-Ray tracing configuration.
--
--
--
-- /See:/ 'tracingConfig' smart constructor.
newtype TracingConfig = TracingConfig'
  { _tMode :: Maybe TracingMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TracingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tMode' - The tracing mode.
tracingConfig
    :: TracingConfig
tracingConfig = TracingConfig' {_tMode = Nothing}


-- | The tracing mode.
tMode :: Lens' TracingConfig (Maybe TracingMode)
tMode = lens _tMode (\ s a -> s{_tMode = a})

instance Hashable TracingConfig where

instance NFData TracingConfig where

instance ToJSON TracingConfig where
        toJSON TracingConfig'{..}
          = object (catMaybes [("Mode" .=) <$> _tMode])

-- | The function's AWS X-Ray tracing configuration.
--
--
--
-- /See:/ 'tracingConfigResponse' smart constructor.
newtype TracingConfigResponse = TracingConfigResponse'
  { _tcMode :: Maybe TracingMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TracingConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcMode' - The tracing mode.
tracingConfigResponse
    :: TracingConfigResponse
tracingConfigResponse = TracingConfigResponse' {_tcMode = Nothing}


-- | The tracing mode.
tcMode :: Lens' TracingConfigResponse (Maybe TracingMode)
tcMode = lens _tcMode (\ s a -> s{_tcMode = a})

instance FromJSON TracingConfigResponse where
        parseJSON
          = withObject "TracingConfigResponse"
              (\ x -> TracingConfigResponse' <$> (x .:? "Mode"))

instance Hashable TracingConfigResponse where

instance NFData TracingConfigResponse where

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vpccSecurityGroupIds :: !(Maybe [Text])
  , _vpccSubnetIds        :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpccSecurityGroupIds' - A list of VPC security groups IDs.
--
-- * 'vpccSubnetIds' - A list of VPC subnet IDs.
vpcConfig
    :: VPCConfig
vpcConfig =
  VPCConfig' {_vpccSecurityGroupIds = Nothing, _vpccSubnetIds = Nothing}


-- | A list of VPC security groups IDs.
vpccSecurityGroupIds :: Lens' VPCConfig [Text]
vpccSecurityGroupIds = lens _vpccSecurityGroupIds (\ s a -> s{_vpccSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of VPC subnet IDs.
vpccSubnetIds :: Lens' VPCConfig [Text]
vpccSubnetIds = lens _vpccSubnetIds (\ s a -> s{_vpccSubnetIds = a}) . _Default . _Coerce

instance Hashable VPCConfig where

instance NFData VPCConfig where

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _vpccSecurityGroupIds,
                  ("SubnetIds" .=) <$> _vpccSubnetIds])

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
--
--
-- /See:/ 'vpcConfigResponse' smart constructor.
data VPCConfigResponse = VPCConfigResponse'
  { _vcSecurityGroupIds :: !(Maybe [Text])
  , _vcSubnetIds        :: !(Maybe [Text])
  , _vcVPCId            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - A list of VPC security groups IDs.
--
-- * 'vcSubnetIds' - A list of VPC subnet IDs.
--
-- * 'vcVPCId' - The ID of the VPC.
vpcConfigResponse
    :: VPCConfigResponse
vpcConfigResponse =
  VPCConfigResponse'
    {_vcSecurityGroupIds = Nothing, _vcSubnetIds = Nothing, _vcVPCId = Nothing}


-- | A list of VPC security groups IDs.
vcSecurityGroupIds :: Lens' VPCConfigResponse [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of VPC subnet IDs.
vcSubnetIds :: Lens' VPCConfigResponse [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce

-- | The ID of the VPC.
vcVPCId :: Lens' VPCConfigResponse (Maybe Text)
vcVPCId = lens _vcVPCId (\ s a -> s{_vcVPCId = a})

instance FromJSON VPCConfigResponse where
        parseJSON
          = withObject "VPCConfigResponse"
              (\ x ->
                 VPCConfigResponse' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "VpcId"))

instance Hashable VPCConfigResponse where

instance NFData VPCConfigResponse where
