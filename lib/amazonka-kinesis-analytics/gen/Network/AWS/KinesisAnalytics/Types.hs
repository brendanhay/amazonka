-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidApplicationConfigurationException
    , _ResourceProvisionedThroughputExceededException
    , _UnsupportedOperationException
    , _InvalidArgumentException
    , _TooManyTagsException
    , _CodeValidationException
    , _ConcurrentModificationException
    , _ServiceUnavailableException
    , _UnableToDetectSchemaException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ApplicationDescription
    , ApplicationDescription (..)

    -- * RecordColumnName
    , RecordColumnName (..)

    -- * InputStartingPosition
    , InputStartingPosition (..)

    -- * InputUpdate
    , InputUpdate (..)
    , mkInputUpdate
    , iuInputId
    , iuInputParallelismUpdate
    , iuInputProcessingConfigurationUpdate
    , iuInputSchemaUpdate
    , iuKinesisFirehoseInputUpdate
    , iuKinesisStreamsInputUpdate
    , iuNamePrefixUpdate

    -- * InputLambdaProcessor
    , InputLambdaProcessor (..)
    , mkInputLambdaProcessor
    , ilpResourceARN
    , ilpRoleARN

    -- * InputStartingPositionConfiguration
    , InputStartingPositionConfiguration (..)
    , mkInputStartingPositionConfiguration
    , ispcInputStartingPosition

    -- * RawInputRecord
    , RawInputRecord (..)

    -- * ReferenceDataSourceUpdate
    , ReferenceDataSourceUpdate (..)
    , mkReferenceDataSourceUpdate
    , rdsuReferenceId
    , rdsuReferenceSchemaUpdate
    , rdsuS3ReferenceDataSourceUpdate
    , rdsuTableNameUpdate

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * InputParallelism
    , InputParallelism (..)
    , mkInputParallelism
    , ipCount

    -- * LogStreamARN
    , LogStreamARN (..)

    -- * DestinationSchema
    , DestinationSchema (..)
    , mkDestinationSchema
    , dsRecordFormatType

    -- * InputProcessingConfiguration
    , InputProcessingConfiguration (..)
    , mkInputProcessingConfiguration
    , ipcInputLambdaProcessor

    -- * LambdaOutput
    , LambdaOutput (..)
    , mkLambdaOutput
    , loResourceARN
    , loRoleARN

    -- * S3Configuration
    , S3Configuration (..)
    , mkS3Configuration
    , scRoleARN
    , scBucketARN
    , scFileKey

    -- * ParsedInputRecordField
    , ParsedInputRecordField (..)

    -- * KinesisFirehoseOutputDescription
    , KinesisFirehoseOutputDescription (..)
    , mkKinesisFirehoseOutputDescription
    , kfodResourceARN
    , kfodRoleARN

    -- * S3ReferenceDataSourceDescription
    , S3ReferenceDataSourceDescription (..)
    , mkS3ReferenceDataSourceDescription
    , srdsdBucketARN
    , srdsdFileKey
    , srdsdReferenceRoleARN

    -- * KinesisAnalyticsARN
    , KinesisAnalyticsARN (..)

    -- * ReferenceDataSource
    , ReferenceDataSource (..)
    , mkReferenceDataSource
    , rdsTableName
    , rdsReferenceSchema
    , rdsS3ReferenceDataSource

    -- * RecordFormat
    , RecordFormat (..)
    , mkRecordFormat
    , rfRecordFormatType
    , rfMappingParameters

    -- * InputLambdaProcessorUpdate
    , InputLambdaProcessorUpdate (..)
    , mkInputLambdaProcessorUpdate
    , ilpuResourceARNUpdate
    , ilpuRoleARNUpdate

    -- * CloudWatchLoggingOptionDescription
    , CloudWatchLoggingOptionDescription (..)
    , mkCloudWatchLoggingOptionDescription
    , cwlodLogStreamARN
    , cwlodRoleARN
    , cwlodCloudWatchLoggingOptionId

    -- * Input
    , Input (..)
    , mkInput
    , iNamePrefix
    , iInputSchema
    , iInputParallelism
    , iInputProcessingConfiguration
    , iKinesisFirehoseInput
    , iKinesisStreamsInput

    -- * KinesisStreamsOutputDescription
    , KinesisStreamsOutputDescription (..)
    , mkKinesisStreamsOutputDescription
    , ksodResourceARN
    , ksodRoleARN

    -- * OutputDescription
    , OutputDescription (..)
    , mkOutputDescription
    , odDestinationSchema
    , odKinesisFirehoseOutputDescription
    , odKinesisStreamsOutputDescription
    , odLambdaOutputDescription
    , odName
    , odOutputId

    -- * KinesisStreamsOutputUpdate
    , KinesisStreamsOutputUpdate (..)
    , mkKinesisStreamsOutputUpdate
    , ksouResourceARNUpdate
    , ksouRoleARNUpdate

    -- * KinesisFirehoseInputDescription
    , KinesisFirehoseInputDescription (..)
    , mkKinesisFirehoseInputDescription
    , kfidResourceARN
    , kfidRoleARN

    -- * CloudWatchLoggingOptionUpdate
    , CloudWatchLoggingOptionUpdate (..)
    , mkCloudWatchLoggingOptionUpdate
    , cwlouCloudWatchLoggingOptionId
    , cwlouLogStreamARNUpdate
    , cwlouRoleARNUpdate

    -- * KinesisStreamsInput
    , KinesisStreamsInput (..)
    , mkKinesisStreamsInput
    , ksiResourceARN
    , ksiRoleARN

    -- * InputProcessingConfigurationUpdate
    , InputProcessingConfigurationUpdate (..)
    , mkInputProcessingConfigurationUpdate
    , ipcuInputLambdaProcessorUpdate

    -- * SourceSchema
    , SourceSchema (..)
    , mkSourceSchema
    , ssRecordFormat
    , ssRecordColumns
    , ssRecordEncoding

    -- * RecordRowDelimiter
    , RecordRowDelimiter (..)

    -- * Output
    , Output (..)
    , mkOutput
    , oName
    , oDestinationSchema
    , oKinesisFirehoseOutput
    , oKinesisStreamsOutput
    , oLambdaOutput

    -- * InAppTableName
    , InAppTableName (..)

    -- * KinesisFirehoseInput
    , KinesisFirehoseInput (..)
    , mkKinesisFirehoseInput
    , kfiResourceARN
    , kfiRoleARN

    -- * ResourceARN
    , ResourceARN (..)

    -- * S3ReferenceDataSourceUpdate
    , S3ReferenceDataSourceUpdate (..)
    , mkS3ReferenceDataSourceUpdate
    , srdsuBucketARNUpdate
    , srdsuFileKeyUpdate
    , srdsuReferenceRoleARNUpdate

    -- * RecordColumnMapping
    , RecordColumnMapping (..)

    -- * KinesisFirehoseOutputUpdate
    , KinesisFirehoseOutputUpdate (..)
    , mkKinesisFirehoseOutputUpdate
    , kfouResourceARNUpdate
    , kfouRoleARNUpdate

    -- * ApplicationCode
    , ApplicationCode (..)

    -- * KinesisStreamsInputDescription
    , KinesisStreamsInputDescription (..)
    , mkKinesisStreamsInputDescription
    , ksidResourceARN
    , ksidRoleARN

    -- * RecordRowPath
    , RecordRowPath (..)

    -- * ApplicationName
    , ApplicationName (..)

    -- * Id
    , Id (..)

    -- * InAppStreamName
    , InAppStreamName (..)

    -- * RecordColumnSqlType
    , RecordColumnSqlType (..)

    -- * TagKey
    , TagKey (..)

    -- * ReferenceDataSourceDescription
    , ReferenceDataSourceDescription (..)
    , mkReferenceDataSourceDescription
    , rdsdReferenceId
    , rdsdTableName
    , rdsdS3ReferenceDataSourceDescription
    , rdsdReferenceSchema

    -- * InputDescription
    , InputDescription (..)
    , mkInputDescription
    , idInAppStreamNames
    , idInputId
    , idInputParallelism
    , idInputProcessingConfigurationDescription
    , idInputSchema
    , idInputStartingPositionConfiguration
    , idKinesisFirehoseInputDescription
    , idKinesisStreamsInputDescription
    , idNamePrefix

    -- * CSVMappingParameters
    , CSVMappingParameters (..)
    , mkCSVMappingParameters
    , csvmpRecordRowDelimiter
    , csvmpRecordColumnDelimiter

    -- * RecordColumnDelimiter
    , RecordColumnDelimiter (..)

    -- * KinesisStreamsOutput
    , KinesisStreamsOutput (..)
    , mkKinesisStreamsOutput
    , ksoResourceARN
    , ksoRoleARN

    -- * CloudWatchLoggingOption
    , CloudWatchLoggingOption (..)
    , mkCloudWatchLoggingOption
    , cwloLogStreamARN
    , cwloRoleARN

    -- * KinesisStreamsInputUpdate
    , KinesisStreamsInputUpdate (..)
    , mkKinesisStreamsInputUpdate
    , ksiuResourceARNUpdate
    , ksiuRoleARNUpdate

    -- * RecordColumn
    , RecordColumn (..)
    , mkRecordColumn
    , rcName
    , rcSqlType
    , rcMapping

    -- * ApplicationUpdate
    , ApplicationUpdate (..)
    , mkApplicationUpdate
    , auApplicationCodeUpdate
    , auCloudWatchLoggingOptionUpdates
    , auInputUpdates
    , auOutputUpdates
    , auReferenceDataSourceUpdates

    -- * BucketARN
    , BucketARN (..)

    -- * InputParallelismUpdate
    , InputParallelismUpdate (..)
    , mkInputParallelismUpdate
    , ipuCountUpdate

    -- * ApplicationDetail
    , ApplicationDetail (..)
    , mkApplicationDetail
    , adApplicationName
    , adApplicationARN
    , adApplicationStatus
    , adApplicationVersionId
    , adApplicationCode
    , adApplicationDescription
    , adCloudWatchLoggingOptionDescriptions
    , adCreateTimestamp
    , adInputDescriptions
    , adLastUpdateTimestamp
    , adOutputDescriptions
    , adReferenceDataSourceDescriptions

    -- * InputProcessingConfigurationDescription
    , InputProcessingConfigurationDescription (..)
    , mkInputProcessingConfigurationDescription
    , ipcdInputLambdaProcessorDescription

    -- * LambdaOutputDescription
    , LambdaOutputDescription (..)
    , mkLambdaOutputDescription
    , lodResourceARN
    , lodRoleARN

    -- * ApplicationStatus
    , ApplicationStatus (..)

    -- * RecordEncoding
    , RecordEncoding (..)

    -- * KinesisFirehoseOutput
    , KinesisFirehoseOutput (..)
    , mkKinesisFirehoseOutput
    , kfoResourceARN
    , kfoRoleARN

    -- * InputSchemaUpdate
    , InputSchemaUpdate (..)
    , mkInputSchemaUpdate
    , isuRecordColumnUpdates
    , isuRecordEncodingUpdate
    , isuRecordFormatUpdate

    -- * JSONMappingParameters
    , JSONMappingParameters (..)
    , mkJSONMappingParameters
    , jsonmpRecordRowPath

    -- * OutputUpdate
    , OutputUpdate (..)
    , mkOutputUpdate
    , ouOutputId
    , ouDestinationSchemaUpdate
    , ouKinesisFirehoseOutputUpdate
    , ouKinesisStreamsOutputUpdate
    , ouLambdaOutputUpdate
    , ouNameUpdate

    -- * S3ReferenceDataSource
    , S3ReferenceDataSource (..)
    , mkS3ReferenceDataSource
    , srdsBucketARN
    , srdsFileKey
    , srdsReferenceRoleARN

    -- * KinesisFirehoseInputUpdate
    , KinesisFirehoseInputUpdate (..)
    , mkKinesisFirehoseInputUpdate
    , kfiuResourceARNUpdate
    , kfiuRoleARNUpdate

    -- * ApplicationSummary
    , ApplicationSummary (..)
    , mkApplicationSummary
    , asApplicationName
    , asApplicationARN
    , asApplicationStatus

    -- * MappingParameters
    , MappingParameters (..)
    , mkMappingParameters
    , mpCSVMappingParameters
    , mpJSONMappingParameters

    -- * InputConfiguration
    , InputConfiguration (..)
    , mkInputConfiguration
    , icId
    , icInputStartingPositionConfiguration

    -- * RecordFormatType
    , RecordFormatType (..)

    -- * FileKey
    , FileKey (..)

    -- * ProcessedInputRecord
    , ProcessedInputRecord (..)

    -- * LambdaOutputUpdate
    , LambdaOutputUpdate (..)
    , mkLambdaOutputUpdate
    , louResourceARNUpdate
    , louRoleARNUpdate

    -- * RoleARN
    , RoleARN (..)

    -- * InputLambdaProcessorDescription
    , InputLambdaProcessorDescription (..)
    , mkInputLambdaProcessorDescription
    , ilpdResourceARN
    , ilpdRoleARN

    -- * ReferenceId
    , ReferenceId (..)

    -- * InputId
    , InputId (..)

    -- * NamePrefixUpdate
    , NamePrefixUpdate (..)

    -- * TableNameUpdate
    , TableNameUpdate (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * CloudWatchLoggingOptionId
    , CloudWatchLoggingOptionId (..)

    -- * OutputId
    , OutputId (..)

    -- * ReferenceRoleARN
    , ReferenceRoleARN (..)

    -- * TableName
    , TableName (..)

    -- * ResourceARNUpdate
    , ResourceARNUpdate (..)

    -- * RoleARNUpdate
    , RoleARNUpdate (..)

    -- * NamePrefix
    , NamePrefix (..)

    -- * Name
    , Name (..)

    -- * BucketARNUpdate
    , BucketARNUpdate (..)

    -- * FileKeyUpdate
    , FileKeyUpdate (..)

    -- * ReferenceRoleARNUpdate
    , ReferenceRoleARNUpdate (..)

    -- * ExclusiveStartApplicationName
    , ExclusiveStartApplicationName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.KinesisAnalytics.Types.ApplicationDescription
  
import Network.AWS.KinesisAnalytics.Types.RecordColumnName
  
  
import Network.AWS.KinesisAnalytics.Types.InputStartingPosition
  
import Network.AWS.KinesisAnalytics.Types.InputUpdate
  
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
  
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
  
  
import Network.AWS.KinesisAnalytics.Types.RawInputRecord
  
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
  
import Network.AWS.KinesisAnalytics.Types.Tag
  
import Network.AWS.KinesisAnalytics.Types.InputParallelism
  
  
import Network.AWS.KinesisAnalytics.Types.LogStreamARN
  
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
  
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
  
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
  
import Network.AWS.KinesisAnalytics.Types.S3Configuration
  
import Network.AWS.KinesisAnalytics.Types.ParsedInputRecordField
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
  
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
  
import Network.AWS.KinesisAnalytics.Types.KinesisAnalyticsARN
  
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
  
import Network.AWS.KinesisAnalytics.Types.RecordFormat
  
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
  
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
  
import Network.AWS.KinesisAnalytics.Types.Input
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
  
import Network.AWS.KinesisAnalytics.Types.OutputDescription
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
  
  
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
  
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
  
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
  
import Network.AWS.KinesisAnalytics.Types.SourceSchema
  
import Network.AWS.KinesisAnalytics.Types.RecordRowDelimiter
  
import Network.AWS.KinesisAnalytics.Types.Output
  
import Network.AWS.KinesisAnalytics.Types.InAppTableName
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
  
import Network.AWS.KinesisAnalytics.Types.ResourceARN
  
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
  
import Network.AWS.KinesisAnalytics.Types.RecordColumnMapping
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
  
import Network.AWS.KinesisAnalytics.Types.ApplicationCode
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
  
import Network.AWS.KinesisAnalytics.Types.RecordRowPath
  
  
import Network.AWS.KinesisAnalytics.Types.ApplicationName
  
import Network.AWS.KinesisAnalytics.Types.Id
  
import Network.AWS.KinesisAnalytics.Types.InAppStreamName
  
import Network.AWS.KinesisAnalytics.Types.RecordColumnSqlType
  
import Network.AWS.KinesisAnalytics.Types.TagKey
  
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
  
import Network.AWS.KinesisAnalytics.Types.InputDescription
  
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
  
import Network.AWS.KinesisAnalytics.Types.RecordColumnDelimiter
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
  
  
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
  
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
  
  
import Network.AWS.KinesisAnalytics.Types.RecordColumn
  
import Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
  
import Network.AWS.KinesisAnalytics.Types.BucketARN
  
import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
  
import Network.AWS.KinesisAnalytics.Types.ApplicationDetail
  
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
  
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
  
import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
  
import Network.AWS.KinesisAnalytics.Types.RecordEncoding
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
  
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
  
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
  
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
  
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
  
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
  
import Network.AWS.KinesisAnalytics.Types.ApplicationSummary
  
import Network.AWS.KinesisAnalytics.Types.MappingParameters
  
import Network.AWS.KinesisAnalytics.Types.InputConfiguration
  
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
  
  
import Network.AWS.KinesisAnalytics.Types.FileKey
  
import Network.AWS.KinesisAnalytics.Types.ProcessedInputRecord
  
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
  
import Network.AWS.KinesisAnalytics.Types.RoleARN
  
  
  
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
  
import Network.AWS.KinesisAnalytics.Types.ReferenceId
  
import Network.AWS.KinesisAnalytics.Types.InputId
  
import Network.AWS.KinesisAnalytics.Types.NamePrefixUpdate
  
import Network.AWS.KinesisAnalytics.Types.TableNameUpdate
  
import Network.AWS.KinesisAnalytics.Types.Key
  
import Network.AWS.KinesisAnalytics.Types.Value
  
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionId
  
import Network.AWS.KinesisAnalytics.Types.OutputId
  
import Network.AWS.KinesisAnalytics.Types.ReferenceRoleARN
  
import Network.AWS.KinesisAnalytics.Types.TableName
  
import Network.AWS.KinesisAnalytics.Types.ResourceARNUpdate
  
import Network.AWS.KinesisAnalytics.Types.RoleARNUpdate
  
import Network.AWS.KinesisAnalytics.Types.NamePrefix
  
import Network.AWS.KinesisAnalytics.Types.Name
  
import Network.AWS.KinesisAnalytics.Types.BucketARNUpdate
  
import Network.AWS.KinesisAnalytics.Types.FileKeyUpdate
  
import Network.AWS.KinesisAnalytics.Types.ReferenceRoleARNUpdate
  
import Network.AWS.KinesisAnalytics.Types.ExclusiveStartApplicationName
  

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "KinesisAnalytics",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "kinesisanalytics",
                 Core._svcVersion = "2015-08-14", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "KinesisAnalytics",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | User-provided application configuration is not valid.
_InvalidApplicationConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidApplicationConfigurationException"
{-# INLINEABLE _InvalidApplicationConfigurationException #-}
{-# DEPRECATED _InvalidApplicationConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <https://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords> in the Amazon Kinesis Streams API Reference.
_ResourceProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceProvisionedThroughputExceededException
  = Core._MatchServiceError mkServiceConfig
      "ResourceProvisionedThroughputExceededException"
{-# INLINEABLE _ResourceProvisionedThroughputExceededException #-}
{-# DEPRECATED _ResourceProvisionedThroughputExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because a specified parameter is not supported or a specified resource is not valid for this operation. 
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedOperationException"
{-# INLINEABLE _UnsupportedOperationException #-}
{-# DEPRECATED _UnsupportedOperationException "Use generic-lens or generic-optics instead"  #-}

-- | Specified input parameter value is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidArgumentException"
{-# INLINEABLE _InvalidArgumentException #-}
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead"  #-}

-- | Application created with too many tags, or too many tags added to an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | User-provided application code (query) is invalid. This can be a simple syntax error.
_CodeValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeValidationException
  = Core._MatchServiceError mkServiceConfig "CodeValidationException"
{-# INLINEABLE _CodeValidationException #-}
{-# DEPRECATED _CodeValidationException "Use generic-lens or generic-optics instead"  #-}

-- | Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The service is unavailable. Back off and retry the operation. 
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | Data format is not valid. Amazon Kinesis Analytics is not able to detect schema for the given streaming source.
_UnableToDetectSchemaException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnableToDetectSchemaException
  = Core._MatchServiceError mkServiceConfig
      "UnableToDetectSchemaException"
{-# INLINEABLE _UnableToDetectSchemaException #-}
{-# DEPRECATED _UnableToDetectSchemaException "Use generic-lens or generic-optics instead"  #-}

-- | Specified application can't be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Exceeded the number of applications allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Application is not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
