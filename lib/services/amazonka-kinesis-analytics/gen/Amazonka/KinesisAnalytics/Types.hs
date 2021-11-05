{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidApplicationConfigurationException,
    _ResourceProvisionedThroughputExceededException,
    _UnsupportedOperationException,
    _InvalidArgumentException,
    _TooManyTagsException,
    _CodeValidationException,
    _ConcurrentModificationException,
    _ServiceUnavailableException,
    _UnableToDetectSchemaException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * InputStartingPosition
    InputStartingPosition (..),

    -- * RecordFormatType
    RecordFormatType (..),

    -- * ApplicationDetail
    ApplicationDetail (..),
    newApplicationDetail,
    applicationDetail_applicationDescription,
    applicationDetail_outputDescriptions,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_referenceDataSourceDescriptions,
    applicationDetail_inputDescriptions,
    applicationDetail_applicationCode,
    applicationDetail_createTimestamp,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_applicationName,
    applicationDetail_applicationARN,
    applicationDetail_applicationStatus,
    applicationDetail_applicationVersionId,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_applicationName,
    applicationSummary_applicationARN,
    applicationSummary_applicationStatus,

    -- * ApplicationUpdate
    ApplicationUpdate (..),
    newApplicationUpdate,
    applicationUpdate_referenceDataSourceUpdates,
    applicationUpdate_inputUpdates,
    applicationUpdate_cloudWatchLoggingOptionUpdates,
    applicationUpdate_outputUpdates,
    applicationUpdate_applicationCodeUpdate,

    -- * CSVMappingParameters
    CSVMappingParameters (..),
    newCSVMappingParameters,
    cSVMappingParameters_recordRowDelimiter,
    cSVMappingParameters_recordColumnDelimiter,

    -- * CloudWatchLoggingOption
    CloudWatchLoggingOption (..),
    newCloudWatchLoggingOption,
    cloudWatchLoggingOption_logStreamARN,
    cloudWatchLoggingOption_roleARN,

    -- * CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (..),
    newCloudWatchLoggingOptionDescription,
    cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId,
    cloudWatchLoggingOptionDescription_logStreamARN,
    cloudWatchLoggingOptionDescription_roleARN,

    -- * CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (..),
    newCloudWatchLoggingOptionUpdate,
    cloudWatchLoggingOptionUpdate_roleARNUpdate,
    cloudWatchLoggingOptionUpdate_logStreamARNUpdate,
    cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId,

    -- * DestinationSchema
    DestinationSchema (..),
    newDestinationSchema,
    destinationSchema_recordFormatType,

    -- * Input
    Input (..),
    newInput,
    input_inputParallelism,
    input_inputProcessingConfiguration,
    input_kinesisStreamsInput,
    input_kinesisFirehoseInput,
    input_namePrefix,
    input_inputSchema,

    -- * InputConfiguration
    InputConfiguration (..),
    newInputConfiguration,
    inputConfiguration_id,
    inputConfiguration_inputStartingPositionConfiguration,

    -- * InputDescription
    InputDescription (..),
    newInputDescription,
    inputDescription_inputStartingPositionConfiguration,
    inputDescription_inputParallelism,
    inputDescription_inputId,
    inputDescription_inAppStreamNames,
    inputDescription_kinesisFirehoseInputDescription,
    inputDescription_inputSchema,
    inputDescription_kinesisStreamsInputDescription,
    inputDescription_namePrefix,
    inputDescription_inputProcessingConfigurationDescription,

    -- * InputLambdaProcessor
    InputLambdaProcessor (..),
    newInputLambdaProcessor,
    inputLambdaProcessor_resourceARN,
    inputLambdaProcessor_roleARN,

    -- * InputLambdaProcessorDescription
    InputLambdaProcessorDescription (..),
    newInputLambdaProcessorDescription,
    inputLambdaProcessorDescription_resourceARN,
    inputLambdaProcessorDescription_roleARN,

    -- * InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (..),
    newInputLambdaProcessorUpdate,
    inputLambdaProcessorUpdate_roleARNUpdate,
    inputLambdaProcessorUpdate_resourceARNUpdate,

    -- * InputParallelism
    InputParallelism (..),
    newInputParallelism,
    inputParallelism_count,

    -- * InputParallelismUpdate
    InputParallelismUpdate (..),
    newInputParallelismUpdate,
    inputParallelismUpdate_countUpdate,

    -- * InputProcessingConfiguration
    InputProcessingConfiguration (..),
    newInputProcessingConfiguration,
    inputProcessingConfiguration_inputLambdaProcessor,

    -- * InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (..),
    newInputProcessingConfigurationDescription,
    inputProcessingConfigurationDescription_inputLambdaProcessorDescription,

    -- * InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (..),
    newInputProcessingConfigurationUpdate,
    inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate,

    -- * InputSchemaUpdate
    InputSchemaUpdate (..),
    newInputSchemaUpdate,
    inputSchemaUpdate_recordFormatUpdate,
    inputSchemaUpdate_recordEncodingUpdate,
    inputSchemaUpdate_recordColumnUpdates,

    -- * InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    newInputStartingPositionConfiguration,
    inputStartingPositionConfiguration_inputStartingPosition,

    -- * InputUpdate
    InputUpdate (..),
    newInputUpdate,
    inputUpdate_inputProcessingConfigurationUpdate,
    inputUpdate_kinesisStreamsInputUpdate,
    inputUpdate_inputParallelismUpdate,
    inputUpdate_namePrefixUpdate,
    inputUpdate_inputSchemaUpdate,
    inputUpdate_kinesisFirehoseInputUpdate,
    inputUpdate_inputId,

    -- * JSONMappingParameters
    JSONMappingParameters (..),
    newJSONMappingParameters,
    jSONMappingParameters_recordRowPath,

    -- * KinesisFirehoseInput
    KinesisFirehoseInput (..),
    newKinesisFirehoseInput,
    kinesisFirehoseInput_resourceARN,
    kinesisFirehoseInput_roleARN,

    -- * KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (..),
    newKinesisFirehoseInputDescription,
    kinesisFirehoseInputDescription_resourceARN,
    kinesisFirehoseInputDescription_roleARN,

    -- * KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (..),
    newKinesisFirehoseInputUpdate,
    kinesisFirehoseInputUpdate_roleARNUpdate,
    kinesisFirehoseInputUpdate_resourceARNUpdate,

    -- * KinesisFirehoseOutput
    KinesisFirehoseOutput (..),
    newKinesisFirehoseOutput,
    kinesisFirehoseOutput_resourceARN,
    kinesisFirehoseOutput_roleARN,

    -- * KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (..),
    newKinesisFirehoseOutputDescription,
    kinesisFirehoseOutputDescription_resourceARN,
    kinesisFirehoseOutputDescription_roleARN,

    -- * KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (..),
    newKinesisFirehoseOutputUpdate,
    kinesisFirehoseOutputUpdate_roleARNUpdate,
    kinesisFirehoseOutputUpdate_resourceARNUpdate,

    -- * KinesisStreamsInput
    KinesisStreamsInput (..),
    newKinesisStreamsInput,
    kinesisStreamsInput_resourceARN,
    kinesisStreamsInput_roleARN,

    -- * KinesisStreamsInputDescription
    KinesisStreamsInputDescription (..),
    newKinesisStreamsInputDescription,
    kinesisStreamsInputDescription_resourceARN,
    kinesisStreamsInputDescription_roleARN,

    -- * KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (..),
    newKinesisStreamsInputUpdate,
    kinesisStreamsInputUpdate_roleARNUpdate,
    kinesisStreamsInputUpdate_resourceARNUpdate,

    -- * KinesisStreamsOutput
    KinesisStreamsOutput (..),
    newKinesisStreamsOutput,
    kinesisStreamsOutput_resourceARN,
    kinesisStreamsOutput_roleARN,

    -- * KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (..),
    newKinesisStreamsOutputDescription,
    kinesisStreamsOutputDescription_resourceARN,
    kinesisStreamsOutputDescription_roleARN,

    -- * KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (..),
    newKinesisStreamsOutputUpdate,
    kinesisStreamsOutputUpdate_roleARNUpdate,
    kinesisStreamsOutputUpdate_resourceARNUpdate,

    -- * LambdaOutput
    LambdaOutput (..),
    newLambdaOutput,
    lambdaOutput_resourceARN,
    lambdaOutput_roleARN,

    -- * LambdaOutputDescription
    LambdaOutputDescription (..),
    newLambdaOutputDescription,
    lambdaOutputDescription_resourceARN,
    lambdaOutputDescription_roleARN,

    -- * LambdaOutputUpdate
    LambdaOutputUpdate (..),
    newLambdaOutputUpdate,
    lambdaOutputUpdate_roleARNUpdate,
    lambdaOutputUpdate_resourceARNUpdate,

    -- * MappingParameters
    MappingParameters (..),
    newMappingParameters,
    mappingParameters_cSVMappingParameters,
    mappingParameters_jSONMappingParameters,

    -- * Output
    Output (..),
    newOutput,
    output_lambdaOutput,
    output_kinesisStreamsOutput,
    output_kinesisFirehoseOutput,
    output_name,
    output_destinationSchema,

    -- * OutputDescription
    OutputDescription (..),
    newOutputDescription,
    outputDescription_outputId,
    outputDescription_destinationSchema,
    outputDescription_kinesisFirehoseOutputDescription,
    outputDescription_kinesisStreamsOutputDescription,
    outputDescription_name,
    outputDescription_lambdaOutputDescription,

    -- * OutputUpdate
    OutputUpdate (..),
    newOutputUpdate,
    outputUpdate_kinesisStreamsOutputUpdate,
    outputUpdate_destinationSchemaUpdate,
    outputUpdate_kinesisFirehoseOutputUpdate,
    outputUpdate_nameUpdate,
    outputUpdate_lambdaOutputUpdate,
    outputUpdate_outputId,

    -- * RecordColumn
    RecordColumn (..),
    newRecordColumn,
    recordColumn_mapping,
    recordColumn_name,
    recordColumn_sqlType,

    -- * RecordFormat
    RecordFormat (..),
    newRecordFormat,
    recordFormat_mappingParameters,
    recordFormat_recordFormatType,

    -- * ReferenceDataSource
    ReferenceDataSource (..),
    newReferenceDataSource,
    referenceDataSource_s3ReferenceDataSource,
    referenceDataSource_tableName,
    referenceDataSource_referenceSchema,

    -- * ReferenceDataSourceDescription
    ReferenceDataSourceDescription (..),
    newReferenceDataSourceDescription,
    referenceDataSourceDescription_referenceSchema,
    referenceDataSourceDescription_referenceId,
    referenceDataSourceDescription_tableName,
    referenceDataSourceDescription_s3ReferenceDataSourceDescription,

    -- * ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (..),
    newReferenceDataSourceUpdate,
    referenceDataSourceUpdate_tableNameUpdate,
    referenceDataSourceUpdate_s3ReferenceDataSourceUpdate,
    referenceDataSourceUpdate_referenceSchemaUpdate,
    referenceDataSourceUpdate_referenceId,

    -- * S3Configuration
    S3Configuration (..),
    newS3Configuration,
    s3Configuration_roleARN,
    s3Configuration_bucketARN,
    s3Configuration_fileKey,

    -- * S3ReferenceDataSource
    S3ReferenceDataSource (..),
    newS3ReferenceDataSource,
    s3ReferenceDataSource_bucketARN,
    s3ReferenceDataSource_fileKey,
    s3ReferenceDataSource_referenceRoleARN,

    -- * S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (..),
    newS3ReferenceDataSourceDescription,
    s3ReferenceDataSourceDescription_bucketARN,
    s3ReferenceDataSourceDescription_fileKey,
    s3ReferenceDataSourceDescription_referenceRoleARN,

    -- * S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (..),
    newS3ReferenceDataSourceUpdate,
    s3ReferenceDataSourceUpdate_bucketARNUpdate,
    s3ReferenceDataSourceUpdate_fileKeyUpdate,
    s3ReferenceDataSourceUpdate_referenceRoleARNUpdate,

    -- * SourceSchema
    SourceSchema (..),
    newSourceSchema,
    sourceSchema_recordEncoding,
    sourceSchema_recordFormat,
    sourceSchema_recordColumns,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalytics.Types.ApplicationDetail
import Amazonka.KinesisAnalytics.Types.ApplicationStatus
import Amazonka.KinesisAnalytics.Types.ApplicationSummary
import Amazonka.KinesisAnalytics.Types.ApplicationUpdate
import Amazonka.KinesisAnalytics.Types.CSVMappingParameters
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOption
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Amazonka.KinesisAnalytics.Types.DestinationSchema
import Amazonka.KinesisAnalytics.Types.Input
import Amazonka.KinesisAnalytics.Types.InputConfiguration
import Amazonka.KinesisAnalytics.Types.InputDescription
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessor
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Amazonka.KinesisAnalytics.Types.InputParallelism
import Amazonka.KinesisAnalytics.Types.InputParallelismUpdate
import Amazonka.KinesisAnalytics.Types.InputProcessingConfiguration
import Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Amazonka.KinesisAnalytics.Types.InputSchemaUpdate
import Amazonka.KinesisAnalytics.Types.InputStartingPosition
import Amazonka.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Amazonka.KinesisAnalytics.Types.InputUpdate
import Amazonka.KinesisAnalytics.Types.JSONMappingParameters
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInput
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutput
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInput
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutput
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Amazonka.KinesisAnalytics.Types.LambdaOutput
import Amazonka.KinesisAnalytics.Types.LambdaOutputDescription
import Amazonka.KinesisAnalytics.Types.LambdaOutputUpdate
import Amazonka.KinesisAnalytics.Types.MappingParameters
import Amazonka.KinesisAnalytics.Types.Output
import Amazonka.KinesisAnalytics.Types.OutputDescription
import Amazonka.KinesisAnalytics.Types.OutputUpdate
import Amazonka.KinesisAnalytics.Types.RecordColumn
import Amazonka.KinesisAnalytics.Types.RecordFormat
import Amazonka.KinesisAnalytics.Types.RecordFormatType
import Amazonka.KinesisAnalytics.Types.ReferenceDataSource
import Amazonka.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Amazonka.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Amazonka.KinesisAnalytics.Types.S3Configuration
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSource
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Amazonka.KinesisAnalytics.Types.SourceSchema
import Amazonka.KinesisAnalytics.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "KinesisAnalytics",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesisanalytics",
      Core._serviceSigningName = "kinesisanalytics",
      Core._serviceVersion = "2015-08-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "KinesisAnalytics",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | User-provided application configuration is not valid.
_InvalidApplicationConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidApplicationConfigurationException"

-- | Discovery failed to get a record from the streaming source because of
-- the Amazon Kinesis Streams ProvisionedThroughputExceededException. For
-- more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords>
-- in the Amazon Kinesis Streams API Reference.
_ResourceProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceProvisionedThroughputExceededException"

-- | The request was rejected because a specified parameter is not supported
-- or a specified resource is not valid for this operation.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | Specified input parameter value is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | Application created with too many tags, or too many tags added to an
-- application. Note that the maximum number of application tags includes
-- system tags. The maximum number of user-defined application tags is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | User-provided application code (query) is invalid. This can be a simple
-- syntax error.
_CodeValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeValidationException =
  Core._MatchServiceError
    defaultService
    "CodeValidationException"

-- | Exception thrown as a result of concurrent modification to an
-- application. For example, two individuals attempting to edit the same
-- application at the same time.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The service is unavailable. Back off and retry the operation.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | Data format is not valid. Amazon Kinesis Analytics is not able to detect
-- schema for the given streaming source.
_UnableToDetectSchemaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnableToDetectSchemaException =
  Core._MatchServiceError
    defaultService
    "UnableToDetectSchemaException"

-- | Specified application can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exceeded the number of applications allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Application is not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
