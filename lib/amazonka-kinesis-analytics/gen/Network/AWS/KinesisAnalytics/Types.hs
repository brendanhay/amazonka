{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types
    (
    -- * Service Configuration
      kinesisAnalytics

    -- * Errors
    , _InvalidApplicationConfigurationException
    , _ResourceProvisionedThroughputExceededException
    , _InvalidArgumentException
    , _CodeValidationException
    , _ConcurrentModificationException
    , _ServiceUnavailableException
    , _UnableToDetectSchemaException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ApplicationStatus
    , ApplicationStatus (..)

    -- * InputStartingPosition
    , InputStartingPosition (..)

    -- * RecordFormatType
    , RecordFormatType (..)

    -- * ApplicationDetail
    , ApplicationDetail
    , applicationDetail
    , adApplicationDescription
    , adOutputDescriptions
    , adCloudWatchLoggingOptionDescriptions
    , adReferenceDataSourceDescriptions
    , adInputDescriptions
    , adApplicationCode
    , adCreateTimestamp
    , adLastUpdateTimestamp
    , adApplicationName
    , adApplicationARN
    , adApplicationStatus
    , adApplicationVersionId

    -- * ApplicationSummary
    , ApplicationSummary
    , applicationSummary
    , asApplicationName
    , asApplicationARN
    , asApplicationStatus

    -- * ApplicationUpdate
    , ApplicationUpdate
    , applicationUpdate
    , auReferenceDataSourceUpdates
    , auInputUpdates
    , auCloudWatchLoggingOptionUpdates
    , auOutputUpdates
    , auApplicationCodeUpdate

    -- * CSVMappingParameters
    , CSVMappingParameters
    , csvMappingParameters
    , cmpRecordRowDelimiter
    , cmpRecordColumnDelimiter

    -- * CloudWatchLoggingOption
    , CloudWatchLoggingOption
    , cloudWatchLoggingOption
    , cwloLogStreamARN
    , cwloRoleARN

    -- * CloudWatchLoggingOptionDescription
    , CloudWatchLoggingOptionDescription
    , cloudWatchLoggingOptionDescription
    , cwlodCloudWatchLoggingOptionId
    , cwlodLogStreamARN
    , cwlodRoleARN

    -- * CloudWatchLoggingOptionUpdate
    , CloudWatchLoggingOptionUpdate
    , cloudWatchLoggingOptionUpdate
    , cwlouRoleARNUpdate
    , cwlouLogStreamARNUpdate
    , cwlouCloudWatchLoggingOptionId

    -- * DestinationSchema
    , DestinationSchema
    , destinationSchema
    , dsRecordFormatType

    -- * Input
    , Input
    , input
    , iInputParallelism
    , iInputProcessingConfiguration
    , iKinesisStreamsInput
    , iKinesisFirehoseInput
    , iNamePrefix
    , iInputSchema

    -- * InputConfiguration
    , InputConfiguration
    , inputConfiguration
    , icId
    , icInputStartingPositionConfiguration

    -- * InputDescription
    , InputDescription
    , inputDescription
    , idInputStartingPositionConfiguration
    , idInputParallelism
    , idInputId
    , idInAppStreamNames
    , idKinesisFirehoseInputDescription
    , idInputSchema
    , idKinesisStreamsInputDescription
    , idNamePrefix
    , idInputProcessingConfigurationDescription

    -- * InputLambdaProcessor
    , InputLambdaProcessor
    , inputLambdaProcessor
    , ilpResourceARN
    , ilpRoleARN

    -- * InputLambdaProcessorDescription
    , InputLambdaProcessorDescription
    , inputLambdaProcessorDescription
    , ilpdResourceARN
    , ilpdRoleARN

    -- * InputLambdaProcessorUpdate
    , InputLambdaProcessorUpdate
    , inputLambdaProcessorUpdate
    , ilpuRoleARNUpdate
    , ilpuResourceARNUpdate

    -- * InputParallelism
    , InputParallelism
    , inputParallelism
    , ipCount

    -- * InputParallelismUpdate
    , InputParallelismUpdate
    , inputParallelismUpdate
    , ipuCountUpdate

    -- * InputProcessingConfiguration
    , InputProcessingConfiguration
    , inputProcessingConfiguration
    , ipcInputLambdaProcessor

    -- * InputProcessingConfigurationDescription
    , InputProcessingConfigurationDescription
    , inputProcessingConfigurationDescription
    , ipcdInputLambdaProcessorDescription

    -- * InputProcessingConfigurationUpdate
    , InputProcessingConfigurationUpdate
    , inputProcessingConfigurationUpdate
    , ipcuInputLambdaProcessorUpdate

    -- * InputSchemaUpdate
    , InputSchemaUpdate
    , inputSchemaUpdate
    , isuRecordFormatUpdate
    , isuRecordEncodingUpdate
    , isuRecordColumnUpdates

    -- * InputStartingPositionConfiguration
    , InputStartingPositionConfiguration
    , inputStartingPositionConfiguration
    , ispcInputStartingPosition

    -- * InputUpdate
    , InputUpdate
    , inputUpdate
    , iuInputProcessingConfigurationUpdate
    , iuKinesisStreamsInputUpdate
    , iuInputParallelismUpdate
    , iuNamePrefixUpdate
    , iuInputSchemaUpdate
    , iuKinesisFirehoseInputUpdate
    , iuInputId

    -- * JSONMappingParameters
    , JSONMappingParameters
    , jsonMappingParameters
    , jmpRecordRowPath

    -- * KinesisFirehoseInput
    , KinesisFirehoseInput
    , kinesisFirehoseInput
    , kfiResourceARN
    , kfiRoleARN

    -- * KinesisFirehoseInputDescription
    , KinesisFirehoseInputDescription
    , kinesisFirehoseInputDescription
    , kfidResourceARN
    , kfidRoleARN

    -- * KinesisFirehoseInputUpdate
    , KinesisFirehoseInputUpdate
    , kinesisFirehoseInputUpdate
    , kfiuRoleARNUpdate
    , kfiuResourceARNUpdate

    -- * KinesisFirehoseOutput
    , KinesisFirehoseOutput
    , kinesisFirehoseOutput
    , kfoResourceARN
    , kfoRoleARN

    -- * KinesisFirehoseOutputDescription
    , KinesisFirehoseOutputDescription
    , kinesisFirehoseOutputDescription
    , kfodResourceARN
    , kfodRoleARN

    -- * KinesisFirehoseOutputUpdate
    , KinesisFirehoseOutputUpdate
    , kinesisFirehoseOutputUpdate
    , kfouRoleARNUpdate
    , kfouResourceARNUpdate

    -- * KinesisStreamsInput
    , KinesisStreamsInput
    , kinesisStreamsInput
    , ksiResourceARN
    , ksiRoleARN

    -- * KinesisStreamsInputDescription
    , KinesisStreamsInputDescription
    , kinesisStreamsInputDescription
    , ksidResourceARN
    , ksidRoleARN

    -- * KinesisStreamsInputUpdate
    , KinesisStreamsInputUpdate
    , kinesisStreamsInputUpdate
    , ksiuRoleARNUpdate
    , ksiuResourceARNUpdate

    -- * KinesisStreamsOutput
    , KinesisStreamsOutput
    , kinesisStreamsOutput
    , ksoResourceARN
    , ksoRoleARN

    -- * KinesisStreamsOutputDescription
    , KinesisStreamsOutputDescription
    , kinesisStreamsOutputDescription
    , ksodResourceARN
    , ksodRoleARN

    -- * KinesisStreamsOutputUpdate
    , KinesisStreamsOutputUpdate
    , kinesisStreamsOutputUpdate
    , ksouRoleARNUpdate
    , ksouResourceARNUpdate

    -- * LambdaOutput
    , LambdaOutput
    , lambdaOutput
    , loResourceARN
    , loRoleARN

    -- * LambdaOutputDescription
    , LambdaOutputDescription
    , lambdaOutputDescription
    , lodResourceARN
    , lodRoleARN

    -- * LambdaOutputUpdate
    , LambdaOutputUpdate
    , lambdaOutputUpdate
    , louRoleARNUpdate
    , louResourceARNUpdate

    -- * MappingParameters
    , MappingParameters
    , mappingParameters
    , mpCSVMappingParameters
    , mpJSONMappingParameters

    -- * Output
    , Output
    , output
    , oLambdaOutput
    , oKinesisStreamsOutput
    , oKinesisFirehoseOutput
    , oName
    , oDestinationSchema

    -- * OutputDescription
    , OutputDescription
    , outputDescription
    , odOutputId
    , odDestinationSchema
    , odKinesisFirehoseOutputDescription
    , odKinesisStreamsOutputDescription
    , odName
    , odLambdaOutputDescription

    -- * OutputUpdate
    , OutputUpdate
    , outputUpdate
    , ouKinesisStreamsOutputUpdate
    , ouDestinationSchemaUpdate
    , ouKinesisFirehoseOutputUpdate
    , ouNameUpdate
    , ouLambdaOutputUpdate
    , ouOutputId

    -- * RecordColumn
    , RecordColumn
    , recordColumn
    , rcMapping
    , rcName
    , rcSqlType

    -- * RecordFormat
    , RecordFormat
    , recordFormat
    , rfMappingParameters
    , rfRecordFormatType

    -- * ReferenceDataSource
    , ReferenceDataSource
    , referenceDataSource
    , rdsS3ReferenceDataSource
    , rdsTableName
    , rdsReferenceSchema

    -- * ReferenceDataSourceDescription
    , ReferenceDataSourceDescription
    , referenceDataSourceDescription
    , rdsdReferenceSchema
    , rdsdReferenceId
    , rdsdTableName
    , rdsdS3ReferenceDataSourceDescription

    -- * ReferenceDataSourceUpdate
    , ReferenceDataSourceUpdate
    , referenceDataSourceUpdate
    , rdsuTableNameUpdate
    , rdsuS3ReferenceDataSourceUpdate
    , rdsuReferenceSchemaUpdate
    , rdsuReferenceId

    -- * S3Configuration
    , S3Configuration
    , s3Configuration
    , scRoleARN
    , scBucketARN
    , scFileKey

    -- * S3ReferenceDataSource
    , S3ReferenceDataSource
    , s3ReferenceDataSource
    , srdsBucketARN
    , srdsFileKey
    , srdsReferenceRoleARN

    -- * S3ReferenceDataSourceDescription
    , S3ReferenceDataSourceDescription
    , s3ReferenceDataSourceDescription
    , srdsdBucketARN
    , srdsdFileKey
    , srdsdReferenceRoleARN

    -- * S3ReferenceDataSourceUpdate
    , S3ReferenceDataSourceUpdate
    , s3ReferenceDataSourceUpdate
    , srdsuBucketARNUpdate
    , srdsuFileKeyUpdate
    , srdsuReferenceRoleARNUpdate

    -- * SourceSchema
    , SourceSchema
    , sourceSchema
    , ssRecordEncoding
    , ssRecordFormat
    , ssRecordColumns
    ) where

import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.KinesisAnalytics.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
kinesisAnalytics :: Service
kinesisAnalytics =
  Service
    { _svcAbbrev = "KinesisAnalytics"
    , _svcSigner = v4
    , _svcPrefix = "kinesisanalytics"
    , _svcVersion = "2015-08-14"
    , _svcEndpoint = defaultEndpoint kinesisAnalytics
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "KinesisAnalytics"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | User-provided application configuration is not valid.
--
--
_InvalidApplicationConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApplicationConfigurationException =
  _MatchServiceError kinesisAnalytics "InvalidApplicationConfigurationException"


-- | Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords> in the Amazon Kinesis Streams API Reference.
--
--
_ResourceProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceProvisionedThroughputExceededException =
  _MatchServiceError
    kinesisAnalytics
    "ResourceProvisionedThroughputExceededException"


-- | Specified input parameter value is invalid.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException =
  _MatchServiceError kinesisAnalytics "InvalidArgumentException"


-- | User-provided application code (query) is invalid. This can be a simple syntax error.
--
--
_CodeValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeValidationException =
  _MatchServiceError kinesisAnalytics "CodeValidationException"


-- | Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError kinesisAnalytics "ConcurrentModificationException"


-- | The service is unavailable, back off and retry the operation.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError kinesisAnalytics "ServiceUnavailableException"


-- | Data format is not valid, Amazon Kinesis Analytics is not able to detect schema for the given streaming source.
--
--
_UnableToDetectSchemaException :: AsError a => Getting (First ServiceError) a ServiceError
_UnableToDetectSchemaException =
  _MatchServiceError kinesisAnalytics "UnableToDetectSchemaException"


-- | Specified application can't be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError kinesisAnalytics "ResourceNotFoundException"


-- | Exceeded the number of applications allowed.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError kinesisAnalytics "LimitExceededException"


-- | Application is not available for this operation.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError kinesisAnalytics "ResourceInUseException"

