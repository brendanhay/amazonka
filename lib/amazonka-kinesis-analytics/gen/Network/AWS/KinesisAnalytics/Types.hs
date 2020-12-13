-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types
  ( -- * Service configuration
    kinesisAnalyticsService,

    -- * Errors

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * InputStartingPosition
    InputStartingPosition (..),

    -- * RecordFormatType
    RecordFormatType (..),

    -- * ApplicationDetail
    ApplicationDetail (..),
    mkApplicationDetail,
    adApplicationDescription,
    adApplicationARN,
    adOutputDescriptions,
    adApplicationVersionId,
    adCloudWatchLoggingOptionDescriptions,
    adReferenceDataSourceDescriptions,
    adInputDescriptions,
    adApplicationCode,
    adCreateTimestamp,
    adLastUpdateTimestamp,
    adApplicationName,
    adApplicationStatus,

    -- * ApplicationSummary
    ApplicationSummary (..),
    mkApplicationSummary,
    asApplicationARN,
    asApplicationName,
    asApplicationStatus,

    -- * ApplicationUpdate
    ApplicationUpdate (..),
    mkApplicationUpdate,
    auReferenceDataSourceUpdates,
    auInputUpdates,
    auCloudWatchLoggingOptionUpdates,
    auOutputUpdates,
    auApplicationCodeUpdate,

    -- * CSVMappingParameters
    CSVMappingParameters (..),
    mkCSVMappingParameters,
    cmpRecordRowDelimiter,
    cmpRecordColumnDelimiter,

    -- * CloudWatchLoggingOption
    CloudWatchLoggingOption (..),
    mkCloudWatchLoggingOption,
    cwloLogStreamARN,
    cwloRoleARN,

    -- * CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (..),
    mkCloudWatchLoggingOptionDescription,
    cwlodLogStreamARN,
    cwlodCloudWatchLoggingOptionId,
    cwlodRoleARN,

    -- * CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (..),
    mkCloudWatchLoggingOptionUpdate,
    cwlouCloudWatchLoggingOptionId,
    cwlouRoleARNUpdate,
    cwlouLogStreamARNUpdate,

    -- * DestinationSchema
    DestinationSchema (..),
    mkDestinationSchema,
    dsRecordFormatType,

    -- * Input
    Input (..),
    mkInput,
    iInputParallelism,
    iInputProcessingConfiguration,
    iKinesisStreamsInput,
    iKinesisFirehoseInput,
    iInputSchema,
    iNamePrefix,

    -- * InputConfiguration
    InputConfiguration (..),
    mkInputConfiguration,
    icInputStartingPositionConfiguration,
    icId,

    -- * InputDescription
    InputDescription (..),
    mkInputDescription,
    idInputStartingPositionConfiguration,
    idInputParallelism,
    idInputId,
    idInAppStreamNames,
    idKinesisFirehoseInputDescription,
    idInputSchema,
    idKinesisStreamsInputDescription,
    idNamePrefix,
    idInputProcessingConfigurationDescription,

    -- * InputLambdaProcessor
    InputLambdaProcessor (..),
    mkInputLambdaProcessor,
    ilpResourceARN,
    ilpRoleARN,

    -- * InputLambdaProcessorDescription
    InputLambdaProcessorDescription (..),
    mkInputLambdaProcessorDescription,
    ilpdResourceARN,
    ilpdRoleARN,

    -- * InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (..),
    mkInputLambdaProcessorUpdate,
    ilpuRoleARNUpdate,
    ilpuResourceARNUpdate,

    -- * InputParallelism
    InputParallelism (..),
    mkInputParallelism,
    ipCount,

    -- * InputParallelismUpdate
    InputParallelismUpdate (..),
    mkInputParallelismUpdate,
    ipuCountUpdate,

    -- * InputProcessingConfiguration
    InputProcessingConfiguration (..),
    mkInputProcessingConfiguration,
    ipcInputLambdaProcessor,

    -- * InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (..),
    mkInputProcessingConfigurationDescription,
    ipcdInputLambdaProcessorDescription,

    -- * InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (..),
    mkInputProcessingConfigurationUpdate,
    ipcuInputLambdaProcessorUpdate,

    -- * InputSchemaUpdate
    InputSchemaUpdate (..),
    mkInputSchemaUpdate,
    isuRecordFormatUpdate,
    isuRecordEncodingUpdate,
    isuRecordColumnUpdates,

    -- * InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    mkInputStartingPositionConfiguration,
    ispcInputStartingPosition,

    -- * InputUpdate
    InputUpdate (..),
    mkInputUpdate,
    iuInputId,
    iuInputProcessingConfigurationUpdate,
    iuKinesisStreamsInputUpdate,
    iuInputParallelismUpdate,
    iuNamePrefixUpdate,
    iuInputSchemaUpdate,
    iuKinesisFirehoseInputUpdate,

    -- * JSONMappingParameters
    JSONMappingParameters (..),
    mkJSONMappingParameters,
    jmpRecordRowPath,

    -- * KinesisFirehoseInput
    KinesisFirehoseInput (..),
    mkKinesisFirehoseInput,
    kfiResourceARN,
    kfiRoleARN,

    -- * KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (..),
    mkKinesisFirehoseInputDescription,
    kfidResourceARN,
    kfidRoleARN,

    -- * KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (..),
    mkKinesisFirehoseInputUpdate,
    kfiuRoleARNUpdate,
    kfiuResourceARNUpdate,

    -- * KinesisFirehoseOutput
    KinesisFirehoseOutput (..),
    mkKinesisFirehoseOutput,
    kfoResourceARN,
    kfoRoleARN,

    -- * KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (..),
    mkKinesisFirehoseOutputDescription,
    kfodResourceARN,
    kfodRoleARN,

    -- * KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (..),
    mkKinesisFirehoseOutputUpdate,
    kfouRoleARNUpdate,
    kfouResourceARNUpdate,

    -- * KinesisStreamsInput
    KinesisStreamsInput (..),
    mkKinesisStreamsInput,
    ksiResourceARN,
    ksiRoleARN,

    -- * KinesisStreamsInputDescription
    KinesisStreamsInputDescription (..),
    mkKinesisStreamsInputDescription,
    ksidResourceARN,
    ksidRoleARN,

    -- * KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (..),
    mkKinesisStreamsInputUpdate,
    ksiuRoleARNUpdate,
    ksiuResourceARNUpdate,

    -- * KinesisStreamsOutput
    KinesisStreamsOutput (..),
    mkKinesisStreamsOutput,
    ksoResourceARN,
    ksoRoleARN,

    -- * KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (..),
    mkKinesisStreamsOutputDescription,
    ksodResourceARN,
    ksodRoleARN,

    -- * KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (..),
    mkKinesisStreamsOutputUpdate,
    ksouRoleARNUpdate,
    ksouResourceARNUpdate,

    -- * LambdaOutput
    LambdaOutput (..),
    mkLambdaOutput,
    loResourceARN,
    loRoleARN,

    -- * LambdaOutputDescription
    LambdaOutputDescription (..),
    mkLambdaOutputDescription,
    lodResourceARN,
    lodRoleARN,

    -- * LambdaOutputUpdate
    LambdaOutputUpdate (..),
    mkLambdaOutputUpdate,
    louRoleARNUpdate,
    louResourceARNUpdate,

    -- * MappingParameters
    MappingParameters (..),
    mkMappingParameters,
    mpCSVMappingParameters,
    mpJSONMappingParameters,

    -- * Output
    Output (..),
    mkOutput,
    oDestinationSchema,
    oLambdaOutput,
    oName,
    oKinesisStreamsOutput,
    oKinesisFirehoseOutput,

    -- * OutputDescription
    OutputDescription (..),
    mkOutputDescription,
    odOutputId,
    odDestinationSchema,
    odKinesisFirehoseOutputDescription,
    odKinesisStreamsOutputDescription,
    odName,
    odLambdaOutputDescription,

    -- * OutputUpdate
    OutputUpdate (..),
    mkOutputUpdate,
    ouOutputId,
    ouKinesisStreamsOutputUpdate,
    ouDestinationSchemaUpdate,
    ouKinesisFirehoseOutputUpdate,
    ouNameUpdate,
    ouLambdaOutputUpdate,

    -- * RecordColumn
    RecordColumn (..),
    mkRecordColumn,
    rcSqlType,
    rcMapping,
    rcName,

    -- * RecordFormat
    RecordFormat (..),
    mkRecordFormat,
    rfMappingParameters,
    rfRecordFormatType,

    -- * ReferenceDataSource
    ReferenceDataSource (..),
    mkReferenceDataSource,
    rdsReferenceSchema,
    rdsS3ReferenceDataSource,
    rdsTableName,

    -- * ReferenceDataSourceDescription
    ReferenceDataSourceDescription (..),
    mkReferenceDataSourceDescription,
    rdsdReferenceSchema,
    rdsdS3ReferenceDataSourceDescription,
    rdsdReferenceId,
    rdsdTableName,

    -- * ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (..),
    mkReferenceDataSourceUpdate,
    rdsuTableNameUpdate,
    rdsuReferenceId,
    rdsuS3ReferenceDataSourceUpdate,
    rdsuReferenceSchemaUpdate,

    -- * S3Configuration
    S3Configuration (..),
    mkS3Configuration,
    scBucketARN,
    scFileKey,
    scRoleARN,

    -- * S3ReferenceDataSource
    S3ReferenceDataSource (..),
    mkS3ReferenceDataSource,
    srdsReferenceRoleARN,
    srdsBucketARN,
    srdsFileKey,

    -- * S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (..),
    mkS3ReferenceDataSourceDescription,
    srdsdReferenceRoleARN,
    srdsdBucketARN,
    srdsdFileKey,

    -- * S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (..),
    mkS3ReferenceDataSourceUpdate,
    srdsuBucketARNUpdate,
    srdsuFileKeyUpdate,
    srdsuReferenceRoleARNUpdate,

    -- * SourceSchema
    SourceSchema (..),
    mkSourceSchema,
    ssRecordFormat,
    ssRecordColumns,
    ssRecordEncoding,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

import Network.AWS.KinesisAnalytics.Types.ApplicationDetail
import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.KinesisAnalytics.Types.ApplicationSummary
import Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.Input
import Network.AWS.KinesisAnalytics.Types.InputConfiguration
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.InputStartingPosition
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.Output
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.S3Configuration
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.KinesisAnalytics.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
kinesisAnalyticsService :: Lude.Service
kinesisAnalyticsService =
  Lude.Service
    { Lude._svcAbbrev = "KinesisAnalytics",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "kinesisanalytics",
      Lude._svcVersion = "2015-08-14",
      Lude._svcEndpoint = Lude.defaultEndpoint kinesisAnalyticsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "KinesisAnalytics",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
