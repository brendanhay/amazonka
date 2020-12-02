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

-- | Provides limits of code size and concurrency associated with the current account and region.
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
-- * 'alConcurrentExecutions' - Number of simultaneous executions of your function per region. For more information or to request a limit increase for concurrent executions, see <http://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Lambda Function Concurrent Executions> . The default limit is 1000.
--
-- * 'alTotalCodeSize' - Maximum size, in bytes, of a code package you can upload per region. The default size is 75 GB.
--
-- * 'alUnreservedConcurrentExecutions' - The number of concurrent executions available to functions that do not have concurrency limits set. For more information, see 'concurrent-executions' .
--
-- * 'alCodeSizeUnzipped' - Size, in bytes, of code/dependencies that you can zip into a deployment package (uncompressed zip/jar size) for uploading. The default limit is 250 MB.
--
-- * 'alCodeSizeZipped' - Size, in bytes, of a single zipped code/dependencies package you can upload for your Lambda function(.zip/.jar file). Try using Amazon S3 for uploading larger files. Default limit is 50 MB.
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


-- | Number of simultaneous executions of your function per region. For more information or to request a limit increase for concurrent executions, see <http://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Lambda Function Concurrent Executions> . The default limit is 1000.
alConcurrentExecutions :: Lens' AccountLimit (Maybe Int)
alConcurrentExecutions = lens _alConcurrentExecutions (\ s a -> s{_alConcurrentExecutions = a})

-- | Maximum size, in bytes, of a code package you can upload per region. The default size is 75 GB.
alTotalCodeSize :: Lens' AccountLimit (Maybe Integer)
alTotalCodeSize = lens _alTotalCodeSize (\ s a -> s{_alTotalCodeSize = a})

-- | The number of concurrent executions available to functions that do not have concurrency limits set. For more information, see 'concurrent-executions' .
alUnreservedConcurrentExecutions :: Lens' AccountLimit (Maybe Natural)
alUnreservedConcurrentExecutions = lens _alUnreservedConcurrentExecutions (\ s a -> s{_alUnreservedConcurrentExecutions = a}) . mapping _Nat

-- | Size, in bytes, of code/dependencies that you can zip into a deployment package (uncompressed zip/jar size) for uploading. The default limit is 250 MB.
alCodeSizeUnzipped :: Lens' AccountLimit (Maybe Integer)
alCodeSizeUnzipped = lens _alCodeSizeUnzipped (\ s a -> s{_alCodeSizeUnzipped = a})

-- | Size, in bytes, of a single zipped code/dependencies package you can upload for your Lambda function(.zip/.jar file). Try using Amazon S3 for uploading larger files. Default limit is 50 MB.
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

-- | Provides code size usage and function count associated with the current account and region.
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
-- * 'auTotalCodeSize' - Total size, in bytes, of the account's deployment packages per region.
--
-- * 'auFunctionCount' - The number of your account's existing functions per region.
accountUsage
    :: AccountUsage
accountUsage =
  AccountUsage' {_auTotalCodeSize = Nothing, _auFunctionCount = Nothing}


-- | Total size, in bytes, of the account's deployment packages per region.
auTotalCodeSize :: Lens' AccountUsage (Maybe Integer)
auTotalCodeSize = lens _auTotalCodeSize (\ s a -> s{_auTotalCodeSize = a})

-- | The number of your account's existing functions per region.
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

-- | Provides configuration information about a Lambda function version alias.
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
-- * 'acRoutingConfig' - Specifies an additional function versions the alias points to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
--
-- * 'acName' - Alias name.
--
-- * 'acFunctionVersion' - Function version to which the alias points.
--
-- * 'acAliasARN' - Lambda function ARN that is qualified using the alias name as the suffix. For example, if you create an alias called @BETA@ that points to a helloworld function version, the ARN is @arn:aws:lambda:aws-regions:acct-id:function:helloworld:BETA@ .
--
-- * 'acDescription' - Alias description.
--
-- * 'acRevisionId' - Represents the latest updated revision of the function or alias.
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


-- | Specifies an additional function versions the alias points to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
acRoutingConfig :: Lens' AliasConfiguration (Maybe AliasRoutingConfiguration)
acRoutingConfig = lens _acRoutingConfig (\ s a -> s{_acRoutingConfig = a})

-- | Alias name.
acName :: Lens' AliasConfiguration (Maybe Text)
acName = lens _acName (\ s a -> s{_acName = a})

-- | Function version to which the alias points.
acFunctionVersion :: Lens' AliasConfiguration (Maybe Text)
acFunctionVersion = lens _acFunctionVersion (\ s a -> s{_acFunctionVersion = a})

-- | Lambda function ARN that is qualified using the alias name as the suffix. For example, if you create an alias called @BETA@ that points to a helloworld function version, the ARN is @arn:aws:lambda:aws-regions:acct-id:function:helloworld:BETA@ .
acAliasARN :: Lens' AliasConfiguration (Maybe Text)
acAliasARN = lens _acAliasARN (\ s a -> s{_acAliasARN = a})

-- | Alias description.
acDescription :: Lens' AliasConfiguration (Maybe Text)
acDescription = lens _acDescription (\ s a -> s{_acDescription = a})

-- | Represents the latest updated revision of the function or alias.
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

-- | The parent object that implements what percentage of traffic will invoke each function version. For more information, see 'lambda-traffic-shifting-using-aliases' .
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
-- * 'arcAdditionalVersionWeights' - Set this value to dictate what percentage of traffic will invoke the updated function version. If set to an empty string, 100 percent of traffic will invoke @function-version@ . For more information, see 'lambda-traffic-shifting-using-aliases' .
aliasRoutingConfiguration
    :: AliasRoutingConfiguration
aliasRoutingConfiguration =
  AliasRoutingConfiguration' {_arcAdditionalVersionWeights = Nothing}


-- | Set this value to dictate what percentage of traffic will invoke the updated function version. If set to an empty string, 100 percent of traffic will invoke @function-version@ . For more information, see 'lambda-traffic-shifting-using-aliases' .
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
-- * 'cReservedConcurrentExecutions' - The number of concurrent executions reserved for this function. For more information, see 'concurrent-executions' .
concurrency
    :: Concurrency
concurrency = Concurrency' {_cReservedConcurrentExecutions = Nothing}


-- | The number of concurrent executions reserved for this function. For more information, see 'concurrent-executions' .
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

-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic you specify as your Dead Letter Queue (DLQ). For more information, see 'dlq' .
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
-- * 'dlcTargetARN' - The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic you specify as your Dead Letter Queue (DLQ). 'dlq' . For more information, see 'dlq' .
deadLetterConfig
    :: DeadLetterConfig
deadLetterConfig = DeadLetterConfig' {_dlcTargetARN = Nothing}


-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic you specify as your Dead Letter Queue (DLQ). 'dlq' . For more information, see 'dlq' .
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

-- | The parent object that contains your environment's configuration settings.
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
-- * 'eVariables' - The key-value pairs that represent your environment's configuration settings.
environment
    :: Environment
environment = Environment' {_eVariables = Nothing}


-- | The key-value pairs that represent your environment's configuration settings.
eVariables :: Lens' Environment (Maybe (HashMap Text Text))
eVariables = lens _eVariables (\ s a -> s{_eVariables = a}) . mapping (_Sensitive . _Map)

instance Hashable Environment where

instance NFData Environment where

instance ToJSON Environment where
        toJSON Environment'{..}
          = object
              (catMaybes [("Variables" .=) <$> _eVariables])

-- | The parent object that contains error information associated with your configuration settings.
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
-- * 'eeErrorCode' - The error code returned by the environment error object.
--
-- * 'eeMessage' - The message returned by the environment error object.
environmentError
    :: EnvironmentError
environmentError =
  EnvironmentError' {_eeErrorCode = Nothing, _eeMessage = Nothing}


-- | The error code returned by the environment error object.
eeErrorCode :: Lens' EnvironmentError (Maybe Text)
eeErrorCode = lens _eeErrorCode (\ s a -> s{_eeErrorCode = a})

-- | The message returned by the environment error object.
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

-- | The parent object returned that contains your environment's configuration settings or any error information associated with your configuration settings.
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
-- * 'envVariables' - The key-value pairs returned that represent your environment's configuration settings or error information.
--
-- * 'envError' - Undocumented member.
environmentResponse
    :: EnvironmentResponse
environmentResponse =
  EnvironmentResponse' {_envVariables = Nothing, _envError = Nothing}


-- | The key-value pairs returned that represent your environment's configuration settings or error information.
envVariables :: Lens' EnvironmentResponse (Maybe (HashMap Text Text))
envVariables = lens _envVariables (\ s a -> s{_envVariables = a}) . mapping (_Sensitive . _Map)

-- | Undocumented member.
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

-- | Describes mapping between an Amazon Kinesis stream and a Lambda function.
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
-- * 'esmcEventSourceARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the source of events.
--
-- * 'esmcState' - The state of the event source mapping. It can be @Creating@ , @Enabled@ , @Disabled@ , @Enabling@ , @Disabling@ , @Updating@ , or @Deleting@ .
--
-- * 'esmcFunctionARN' - The Lambda function to invoke when AWS Lambda detects an event on the stream.
--
-- * 'esmcUUId' - The AWS Lambda assigned opaque identifier for the mapping.
--
-- * 'esmcLastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
--
-- * 'esmcBatchSize' - The largest number of records that AWS Lambda will retrieve from your event source at the time of invoking your function. Your function receives an event with all the retrieved records.
--
-- * 'esmcStateTransitionReason' - The reason the event source mapping is in its current state. It is either user-requested or an AWS Lambda-initiated state transition.
--
-- * 'esmcLastModified' - The UTC time string indicating the last time the event mapping was updated.
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


-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the source of events.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\ s a -> s{_esmcEventSourceARN = a})

-- | The state of the event source mapping. It can be @Creating@ , @Enabled@ , @Disabled@ , @Enabling@ , @Disabling@ , @Updating@ , or @Deleting@ .
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\ s a -> s{_esmcState = a})

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\ s a -> s{_esmcFunctionARN = a})

-- | The AWS Lambda assigned opaque identifier for the mapping.
esmcUUId :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUId = lens _esmcUUId (\ s a -> s{_esmcUUId = a})

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\ s a -> s{_esmcLastProcessingResult = a})

-- | The largest number of records that AWS Lambda will retrieve from your event source at the time of invoking your function. Your function receives an event with all the retrieved records.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\ s a -> s{_esmcBatchSize = a}) . mapping _Nat

-- | The reason the event source mapping is in its current state. It is either user-requested or an AWS Lambda-initiated state transition.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\ s a -> s{_esmcStateTransitionReason = a})

-- | The UTC time string indicating the last time the event mapping was updated.
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

-- | The code for the Lambda function.
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
-- * 'fcS3ObjectVersion' - The Amazon S3 object (the deployment package) version you want to upload.
--
-- * 'fcS3Key' - The Amazon S3 object (the deployment package) key name you want to upload.
--
-- * 'fcZipFile' - The contents of your zip file containing your deployment package. If you are using the web API directly, the contents of the zip file must be base64-encoded. If you are using the AWS SDKs or the AWS CLI, the SDKs or CLI will do the encoding for you. For more information about creating a .zip file, see <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions> in the __AWS Lambda Developer Guide__ . -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'fcS3Bucket' - Amazon S3 bucket name where the .zip file containing your deployment package is stored. This bucket must reside in the same AWS region where you are creating the Lambda function.
functionCode
    :: FunctionCode
functionCode =
  FunctionCode'
    { _fcS3ObjectVersion = Nothing
    , _fcS3Key = Nothing
    , _fcZipFile = Nothing
    , _fcS3Bucket = Nothing
    }


-- | The Amazon S3 object (the deployment package) version you want to upload.
fcS3ObjectVersion :: Lens' FunctionCode (Maybe Text)
fcS3ObjectVersion = lens _fcS3ObjectVersion (\ s a -> s{_fcS3ObjectVersion = a})

-- | The Amazon S3 object (the deployment package) key name you want to upload.
fcS3Key :: Lens' FunctionCode (Maybe Text)
fcS3Key = lens _fcS3Key (\ s a -> s{_fcS3Key = a})

-- | The contents of your zip file containing your deployment package. If you are using the web API directly, the contents of the zip file must be base64-encoded. If you are using the AWS SDKs or the AWS CLI, the SDKs or CLI will do the encoding for you. For more information about creating a .zip file, see <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions> in the __AWS Lambda Developer Guide__ . -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
fcZipFile :: Lens' FunctionCode (Maybe ByteString)
fcZipFile = lens _fcZipFile (\ s a -> s{_fcZipFile = a}) . mapping (_Sensitive . _Base64)

-- | Amazon S3 bucket name where the .zip file containing your deployment package is stored. This bucket must reside in the same AWS region where you are creating the Lambda function.
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

-- | The object for the Lambda function location.
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
-- * 'fclLocation' - The presigned URL you can use to download the function's .zip file that you previously uploaded. The URL is valid for up to 10 minutes.
--
-- * 'fclRepositoryType' - The repository from which you can download the function.
functionCodeLocation
    :: FunctionCodeLocation
functionCodeLocation =
  FunctionCodeLocation' {_fclLocation = Nothing, _fclRepositoryType = Nothing}


-- | The presigned URL you can use to download the function's .zip file that you previously uploaded. The URL is valid for up to 10 minutes.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\ s a -> s{_fclLocation = a})

-- | The repository from which you can download the function.
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

-- | A complex type that describes function metadata.
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
-- * 'fcMemorySize' - The memory size, in MB, you configured for the function. Must be a multiple of 64 MB.
--
-- * 'fcRuntime' - The runtime environment for the Lambda function.
--
-- * 'fcFunctionARN' - The Amazon Resource Name (ARN) assigned to the function.
--
-- * 'fcKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If empty, it means you are using the AWS Lambda default service key.
--
-- * 'fcEnvironment' - The parent object that contains your environment's configuration settings.
--
-- * 'fcDeadLetterConfig' - The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
--
-- * 'fcRole' - The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it executes your function to access any other Amazon Web Services (AWS) resources.
--
-- * 'fcVPCConfig' - VPC configuration associated with your Lambda function.
--
-- * 'fcVersion' - The version of the Lambda function.
--
-- * 'fcFunctionName' - The name of the function. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'fcCodeSize' - The size, in bytes, of the function .zip file you uploaded.
--
-- * 'fcHandler' - The function Lambda calls to begin executing your function.
--
-- * 'fcTimeout' - The function execution time at which Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
--
-- * 'fcLastModified' - The time stamp of the last time you updated the function. The time stamp is conveyed as a string complying with ISO-8601 in this way YYYY-MM-DDThh:mm:ssTZD (e.g., 1997-07-16T19:20:30+01:00). For more information, see <https://www.w3.org/TR/NOTE-datetime Date and Time Formats> .
--
-- * 'fcCodeSha256' - It is the SHA256 hash of your function deployment package.
--
-- * 'fcTracingConfig' - The parent object that contains your function's tracing settings.
--
-- * 'fcDescription' - The user-provided description.
--
-- * 'fcRevisionId' - Represents the latest updated revision of the function or alias.
--
-- * 'fcMasterARN' - Returns the ARN (Amazon Resource Name) of the master function.
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


-- | The memory size, in MB, you configured for the function. Must be a multiple of 64 MB.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a}) . mapping _Nat

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Runtime)
fcRuntime = lens _fcRuntime (\ s a -> s{_fcRuntime = a})

-- | The Amazon Resource Name (ARN) assigned to the function.
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\ s a -> s{_fcFunctionARN = a})

-- | The Amazon Resource Name (ARN) of the KMS key used to encrypt your function's environment variables. If empty, it means you are using the AWS Lambda default service key.
fcKMSKeyARN :: Lens' FunctionConfiguration (Maybe Text)
fcKMSKeyARN = lens _fcKMSKeyARN (\ s a -> s{_fcKMSKeyARN = a})

-- | The parent object that contains your environment's configuration settings.
fcEnvironment :: Lens' FunctionConfiguration (Maybe EnvironmentResponse)
fcEnvironment = lens _fcEnvironment (\ s a -> s{_fcEnvironment = a})

-- | The parent object that contains the target ARN (Amazon Resource Name) of an Amazon SQS queue or Amazon SNS topic. For more information, see 'dlq' .
fcDeadLetterConfig :: Lens' FunctionConfiguration (Maybe DeadLetterConfig)
fcDeadLetterConfig = lens _fcDeadLetterConfig (\ s a -> s{_fcDeadLetterConfig = a})

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it executes your function to access any other Amazon Web Services (AWS) resources.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\ s a -> s{_fcRole = a})

-- | VPC configuration associated with your Lambda function.
fcVPCConfig :: Lens' FunctionConfiguration (Maybe VPCConfigResponse)
fcVPCConfig = lens _fcVPCConfig (\ s a -> s{_fcVPCConfig = a})

-- | The version of the Lambda function.
fcVersion :: Lens' FunctionConfiguration (Maybe Text)
fcVersion = lens _fcVersion (\ s a -> s{_fcVersion = a})

-- | The name of the function. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\ s a -> s{_fcFunctionName = a})

-- | The size, in bytes, of the function .zip file you uploaded.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\ s a -> s{_fcCodeSize = a})

-- | The function Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\ s a -> s{_fcHandler = a})

-- | The function execution time at which Lambda should terminate the function. Because the execution time has cost implications, we recommend you set this value based on your expected execution time. The default is 3 seconds.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a}) . mapping _Nat

-- | The time stamp of the last time you updated the function. The time stamp is conveyed as a string complying with ISO-8601 in this way YYYY-MM-DDThh:mm:ssTZD (e.g., 1997-07-16T19:20:30+01:00). For more information, see <https://www.w3.org/TR/NOTE-datetime Date and Time Formats> .
fcLastModified :: Lens' FunctionConfiguration (Maybe Text)
fcLastModified = lens _fcLastModified (\ s a -> s{_fcLastModified = a})

-- | It is the SHA256 hash of your function deployment package.
fcCodeSha256 :: Lens' FunctionConfiguration (Maybe Text)
fcCodeSha256 = lens _fcCodeSha256 (\ s a -> s{_fcCodeSha256 = a})

-- | The parent object that contains your function's tracing settings.
fcTracingConfig :: Lens' FunctionConfiguration (Maybe TracingConfigResponse)
fcTracingConfig = lens _fcTracingConfig (\ s a -> s{_fcTracingConfig = a})

-- | The user-provided description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\ s a -> s{_fcDescription = a})

-- | Represents the latest updated revision of the function or alias.
fcRevisionId :: Lens' FunctionConfiguration (Maybe Text)
fcRevisionId = lens _fcRevisionId (\ s a -> s{_fcRevisionId = a})

-- | Returns the ARN (Amazon Resource Name) of the master function.
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

-- | The parent object that contains your function's tracing settings.
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
-- * 'tMode' - Can be either PassThrough or Active. If PassThrough, Lambda will only trace the request from an upstream service if it contains a tracing header with "sampled=1". If Active, Lambda will respect any tracing header it receives from an upstream service. If no tracing header is received, Lambda will call X-Ray for a tracing decision.
tracingConfig
    :: TracingConfig
tracingConfig = TracingConfig' {_tMode = Nothing}


-- | Can be either PassThrough or Active. If PassThrough, Lambda will only trace the request from an upstream service if it contains a tracing header with "sampled=1". If Active, Lambda will respect any tracing header it receives from an upstream service. If no tracing header is received, Lambda will call X-Ray for a tracing decision.
tMode :: Lens' TracingConfig (Maybe TracingMode)
tMode = lens _tMode (\ s a -> s{_tMode = a})

instance Hashable TracingConfig where

instance NFData TracingConfig where

instance ToJSON TracingConfig where
        toJSON TracingConfig'{..}
          = object (catMaybes [("Mode" .=) <$> _tMode])

-- | Parent object of the tracing information associated with your Lambda function.
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
-- * 'tcMode' - The tracing mode associated with your Lambda function.
tracingConfigResponse
    :: TracingConfigResponse
tracingConfigResponse = TracingConfigResponse' {_tcMode = Nothing}


-- | The tracing mode associated with your Lambda function.
tcMode :: Lens' TracingConfigResponse (Maybe TracingMode)
tcMode = lens _tcMode (\ s a -> s{_tcMode = a})

instance FromJSON TracingConfigResponse where
        parseJSON
          = withObject "TracingConfigResponse"
              (\ x -> TracingConfigResponse' <$> (x .:? "Mode"))

instance Hashable TracingConfigResponse where

instance NFData TracingConfigResponse where

-- | If your Lambda function accesses resources in a VPC, you provide this parameter identifying the list of security group IDs and subnet IDs. These must belong to the same VPC. You must provide at least one security group and one subnet ID.
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
-- * 'vpccSecurityGroupIds' - A list of one or more security groups IDs in your VPC.
--
-- * 'vpccSubnetIds' - A list of one or more subnet IDs in your VPC.
vpcConfig
    :: VPCConfig
vpcConfig =
  VPCConfig' {_vpccSecurityGroupIds = Nothing, _vpccSubnetIds = Nothing}


-- | A list of one or more security groups IDs in your VPC.
vpccSecurityGroupIds :: Lens' VPCConfig [Text]
vpccSecurityGroupIds = lens _vpccSecurityGroupIds (\ s a -> s{_vpccSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of one or more subnet IDs in your VPC.
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

-- | VPC configuration associated with your Lambda function.
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
-- * 'vcSecurityGroupIds' - A list of security group IDs associated with the Lambda function.
--
-- * 'vcSubnetIds' - A list of subnet IDs associated with the Lambda function.
--
-- * 'vcVPCId' - The VPC ID associated with you Lambda function.
vpcConfigResponse
    :: VPCConfigResponse
vpcConfigResponse =
  VPCConfigResponse'
    {_vcSecurityGroupIds = Nothing, _vcSubnetIds = Nothing, _vcVPCId = Nothing}


-- | A list of security group IDs associated with the Lambda function.
vcSecurityGroupIds :: Lens' VPCConfigResponse [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of subnet IDs associated with the Lambda function.
vcSubnetIds :: Lens' VPCConfigResponse [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce

-- | The VPC ID associated with you Lambda function.
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
