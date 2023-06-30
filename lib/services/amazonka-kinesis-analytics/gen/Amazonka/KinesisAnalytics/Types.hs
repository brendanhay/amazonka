{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CodeValidationException,
    _ConcurrentModificationException,
    _InvalidApplicationConfigurationException,
    _InvalidArgumentException,
    _LimitExceededException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceProvisionedThroughputExceededException,
    _ServiceUnavailableException,
    _TooManyTagsException,
    _UnableToDetectSchemaException,
    _UnsupportedOperationException,

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * InputStartingPosition
    InputStartingPosition (..),

    -- * RecordFormatType
    RecordFormatType (..),

    -- * ApplicationDetail
    ApplicationDetail (..),
    newApplicationDetail,
    applicationDetail_applicationCode,
    applicationDetail_applicationDescription,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_createTimestamp,
    applicationDetail_inputDescriptions,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_outputDescriptions,
    applicationDetail_referenceDataSourceDescriptions,
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
    applicationUpdate_applicationCodeUpdate,
    applicationUpdate_cloudWatchLoggingOptionUpdates,
    applicationUpdate_inputUpdates,
    applicationUpdate_outputUpdates,
    applicationUpdate_referenceDataSourceUpdates,

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
    cloudWatchLoggingOptionUpdate_logStreamARNUpdate,
    cloudWatchLoggingOptionUpdate_roleARNUpdate,
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
    input_kinesisFirehoseInput,
    input_kinesisStreamsInput,
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
    inputDescription_inAppStreamNames,
    inputDescription_inputId,
    inputDescription_inputParallelism,
    inputDescription_inputProcessingConfigurationDescription,
    inputDescription_inputSchema,
    inputDescription_inputStartingPositionConfiguration,
    inputDescription_kinesisFirehoseInputDescription,
    inputDescription_kinesisStreamsInputDescription,
    inputDescription_namePrefix,

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
    inputLambdaProcessorUpdate_resourceARNUpdate,
    inputLambdaProcessorUpdate_roleARNUpdate,

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
    inputSchemaUpdate_recordColumnUpdates,
    inputSchemaUpdate_recordEncodingUpdate,
    inputSchemaUpdate_recordFormatUpdate,

    -- * InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    newInputStartingPositionConfiguration,
    inputStartingPositionConfiguration_inputStartingPosition,

    -- * InputUpdate
    InputUpdate (..),
    newInputUpdate,
    inputUpdate_inputParallelismUpdate,
    inputUpdate_inputProcessingConfigurationUpdate,
    inputUpdate_inputSchemaUpdate,
    inputUpdate_kinesisFirehoseInputUpdate,
    inputUpdate_kinesisStreamsInputUpdate,
    inputUpdate_namePrefixUpdate,
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
    kinesisFirehoseInputUpdate_resourceARNUpdate,
    kinesisFirehoseInputUpdate_roleARNUpdate,

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
    kinesisFirehoseOutputUpdate_resourceARNUpdate,
    kinesisFirehoseOutputUpdate_roleARNUpdate,

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
    kinesisStreamsInputUpdate_resourceARNUpdate,
    kinesisStreamsInputUpdate_roleARNUpdate,

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
    kinesisStreamsOutputUpdate_resourceARNUpdate,
    kinesisStreamsOutputUpdate_roleARNUpdate,

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
    lambdaOutputUpdate_resourceARNUpdate,
    lambdaOutputUpdate_roleARNUpdate,

    -- * MappingParameters
    MappingParameters (..),
    newMappingParameters,
    mappingParameters_cSVMappingParameters,
    mappingParameters_jSONMappingParameters,

    -- * Output
    Output (..),
    newOutput,
    output_kinesisFirehoseOutput,
    output_kinesisStreamsOutput,
    output_lambdaOutput,
    output_name,
    output_destinationSchema,

    -- * OutputDescription
    OutputDescription (..),
    newOutputDescription,
    outputDescription_destinationSchema,
    outputDescription_kinesisFirehoseOutputDescription,
    outputDescription_kinesisStreamsOutputDescription,
    outputDescription_lambdaOutputDescription,
    outputDescription_name,
    outputDescription_outputId,

    -- * OutputUpdate
    OutputUpdate (..),
    newOutputUpdate,
    outputUpdate_destinationSchemaUpdate,
    outputUpdate_kinesisFirehoseOutputUpdate,
    outputUpdate_kinesisStreamsOutputUpdate,
    outputUpdate_lambdaOutputUpdate,
    outputUpdate_nameUpdate,
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
    referenceDataSourceUpdate_referenceSchemaUpdate,
    referenceDataSourceUpdate_s3ReferenceDataSourceUpdate,
    referenceDataSourceUpdate_tableNameUpdate,
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
import qualified Amazonka.Core.Lens.Internal as Lens
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "KinesisAnalytics",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kinesisanalytics",
      Core.signingName = "kinesisanalytics",
      Core.version = "2015-08-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "KinesisAnalytics",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | User-provided application code (query) is invalid. This can be a simple
-- syntax error.
_CodeValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CodeValidationException =
  Core._MatchServiceError
    defaultService
    "CodeValidationException"

-- | Exception thrown as a result of concurrent modification to an
-- application. For example, two individuals attempting to edit the same
-- application at the same time.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | User-provided application configuration is not valid.
_InvalidApplicationConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidApplicationConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidApplicationConfigurationException"

-- | Specified input parameter value is invalid.
_InvalidArgumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | Exceeded the number of applications allowed.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Application is not available for this operation.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | Specified application can\'t be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Discovery failed to get a record from the streaming source because of
-- the Amazon Kinesis Streams ProvisionedThroughputExceededException. For
-- more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords>
-- in the Amazon Kinesis Streams API Reference.
_ResourceProvisionedThroughputExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceProvisionedThroughputExceededException"

-- | The service is unavailable. Back off and retry the operation.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | Application created with too many tags, or too many tags added to an
-- application. Note that the maximum number of application tags includes
-- system tags. The maximum number of user-defined application tags is 50.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | Data format is not valid. Amazon Kinesis Analytics is not able to detect
-- schema for the given streaming source.
_UnableToDetectSchemaException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnableToDetectSchemaException =
  Core._MatchServiceError
    defaultService
    "UnableToDetectSchemaException"

-- | The request was rejected because a specified parameter is not supported
-- or a specified resource is not valid for this operation.
_UnsupportedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
