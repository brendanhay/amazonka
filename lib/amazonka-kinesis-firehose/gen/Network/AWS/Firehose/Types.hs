-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types
  ( -- * Service configuration
    firehoseService,

    -- * Errors

    -- * CompressionFormat
    CompressionFormat (..),

    -- * ContentEncoding
    ContentEncoding (..),

    -- * DeliveryStreamEncryptionStatus
    DeliveryStreamEncryptionStatus (..),

    -- * DeliveryStreamFailureType
    DeliveryStreamFailureType (..),

    -- * DeliveryStreamStatus
    DeliveryStreamStatus (..),

    -- * DeliveryStreamType
    DeliveryStreamType (..),

    -- * ElasticsearchIndexRotationPeriod
    ElasticsearchIndexRotationPeriod (..),

    -- * ElasticsearchS3BackupMode
    ElasticsearchS3BackupMode (..),

    -- * HECEndpointType
    HECEndpointType (..),

    -- * HTTPEndpointS3BackupMode
    HTTPEndpointS3BackupMode (..),

    -- * KeyType
    KeyType (..),

    -- * NoEncryptionConfig
    NoEncryptionConfig (..),

    -- * OrcCompression
    OrcCompression (..),

    -- * OrcFormatVersion
    OrcFormatVersion (..),

    -- * ParquetCompression
    ParquetCompression (..),

    -- * ParquetWriterVersion
    ParquetWriterVersion (..),

    -- * ProcessorParameterName
    ProcessorParameterName (..),

    -- * ProcessorType
    ProcessorType (..),

    -- * RedshiftS3BackupMode
    RedshiftS3BackupMode (..),

    -- * S3BackupMode
    S3BackupMode (..),

    -- * SplunkS3BackupMode
    SplunkS3BackupMode (..),

    -- * BufferingHints
    BufferingHints (..),
    mkBufferingHints,
    bhSizeInMBs,
    bhIntervalInSeconds,

    -- * CloudWatchLoggingOptions
    CloudWatchLoggingOptions (..),
    mkCloudWatchLoggingOptions,
    cwloEnabled,
    cwloLogGroupName,
    cwloLogStreamName,

    -- * CopyCommand
    CopyCommand (..),
    mkCopyCommand,
    ccCopyOptions,
    ccDataTableColumns,
    ccDataTableName,

    -- * DataFormatConversionConfiguration
    DataFormatConversionConfiguration (..),
    mkDataFormatConversionConfiguration,
    dfccOutputFormatConfiguration,
    dfccEnabled,
    dfccSchemaConfiguration,
    dfccInputFormatConfiguration,

    -- * DeliveryStreamDescription
    DeliveryStreamDescription (..),
    mkDeliveryStreamDescription,
    dsdDeliveryStreamStatus,
    dsdVersionId,
    dsdDeliveryStreamARN,
    dsdHasMoreDestinations,
    dsdFailureDescription,
    dsdDeliveryStreamEncryptionConfiguration,
    dsdDestinations,
    dsdDeliveryStreamName,
    dsdCreateTimestamp,
    dsdSource,
    dsdLastUpdateTimestamp,
    dsdDeliveryStreamType,

    -- * DeliveryStreamEncryptionConfiguration
    DeliveryStreamEncryptionConfiguration (..),
    mkDeliveryStreamEncryptionConfiguration,
    dsecStatus,
    dsecKeyType,
    dsecKeyARN,
    dsecFailureDescription,

    -- * DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput (..),
    mkDeliveryStreamEncryptionConfigurationInput,
    dseciKeyType,
    dseciKeyARN,

    -- * Deserializer
    Deserializer (..),
    mkDeserializer,
    dOpenXJSONSerDe,
    dHiveJSONSerDe,

    -- * DestinationDescription
    DestinationDescription (..),
    mkDestinationDescription,
    ddSplunkDestinationDescription,
    ddHTTPEndpointDestinationDescription,
    ddS3DestinationDescription,
    ddExtendedS3DestinationDescription,
    ddElasticsearchDestinationDescription,
    ddRedshiftDestinationDescription,
    ddDestinationId,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints (..),
    mkElasticsearchBufferingHints,
    ebhSizeInMBs,
    ebhIntervalInSeconds,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (..),
    mkElasticsearchDestinationConfiguration,
    edcIndexRotationPeriod,
    edcTypeName,
    edcS3BackupMode,
    edcDomainARN,
    edcS3Configuration,
    edcCloudWatchLoggingOptions,
    edcVPCConfiguration,
    edcBufferingHints,
    edcRetryOptions,
    edcProcessingConfiguration,
    edcRoleARN,
    edcClusterEndpoint,
    edcIndexName,

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (..),
    mkElasticsearchDestinationDescription,
    eddIndexRotationPeriod,
    eddTypeName,
    eddS3BackupMode,
    eddDomainARN,
    eddVPCConfigurationDescription,
    eddCloudWatchLoggingOptions,
    eddS3DestinationDescription,
    eddBufferingHints,
    eddRetryOptions,
    eddProcessingConfiguration,
    eddRoleARN,
    eddClusterEndpoint,
    eddIndexName,

    -- * ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (..),
    mkElasticsearchDestinationUpdate,
    eduIndexRotationPeriod,
    eduTypeName,
    eduDomainARN,
    eduCloudWatchLoggingOptions,
    eduS3Update,
    eduBufferingHints,
    eduRetryOptions,
    eduProcessingConfiguration,
    eduRoleARN,
    eduClusterEndpoint,
    eduIndexName,

    -- * ElasticsearchRetryOptions
    ElasticsearchRetryOptions (..),
    mkElasticsearchRetryOptions,
    eroDurationInSeconds,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecNoEncryptionConfig,
    ecKMSEncryptionConfig,

    -- * ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration (..),
    mkExtendedS3DestinationConfiguration,
    esdcS3BackupMode,
    esdcPrefix,
    esdcCloudWatchLoggingOptions,
    esdcS3BackupConfiguration,
    esdcErrorOutputPrefix,
    esdcEncryptionConfiguration,
    esdcCompressionFormat,
    esdcBufferingHints,
    esdcDataFormatConversionConfiguration,
    esdcBucketARN,
    esdcProcessingConfiguration,
    esdcRoleARN,

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (..),
    mkExtendedS3DestinationDescription,
    esddS3BackupMode,
    esddS3BackupDescription,
    esddPrefix,
    esddCloudWatchLoggingOptions,
    esddErrorOutputPrefix,
    esddEncryptionConfiguration,
    esddCompressionFormat,
    esddBufferingHints,
    esddDataFormatConversionConfiguration,
    esddBucketARN,
    esddProcessingConfiguration,
    esddRoleARN,

    -- * ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate (..),
    mkExtendedS3DestinationUpdate,
    esduS3BackupMode,
    esduPrefix,
    esduCloudWatchLoggingOptions,
    esduErrorOutputPrefix,
    esduS3BackupUpdate,
    esduEncryptionConfiguration,
    esduCompressionFormat,
    esduBufferingHints,
    esduDataFormatConversionConfiguration,
    esduBucketARN,
    esduProcessingConfiguration,
    esduRoleARN,

    -- * FailureDescription
    FailureDescription (..),
    mkFailureDescription,
    fdDetails,
    fdType,

    -- * HTTPEndpointBufferingHints
    HTTPEndpointBufferingHints (..),
    mkHTTPEndpointBufferingHints,
    httpebhSizeInMBs,
    httpebhIntervalInSeconds,

    -- * HTTPEndpointCommonAttribute
    HTTPEndpointCommonAttribute (..),
    mkHTTPEndpointCommonAttribute,
    httpecaAttributeValue,
    httpecaAttributeName,

    -- * HTTPEndpointConfiguration
    HTTPEndpointConfiguration (..),
    mkHTTPEndpointConfiguration,
    httpecURL,
    httpecName,
    httpecAccessKey,

    -- * HTTPEndpointDescription
    HTTPEndpointDescription (..),
    mkHTTPEndpointDescription,
    httpedURL,
    httpedName,

    -- * HTTPEndpointDestinationConfiguration
    HTTPEndpointDestinationConfiguration (..),
    mkHTTPEndpointDestinationConfiguration,
    httpedcS3BackupMode,
    httpedcS3Configuration,
    httpedcCloudWatchLoggingOptions,
    httpedcBufferingHints,
    httpedcRetryOptions,
    httpedcEndpointConfiguration,
    httpedcProcessingConfiguration,
    httpedcRequestConfiguration,
    httpedcRoleARN,

    -- * HTTPEndpointDestinationDescription
    HTTPEndpointDestinationDescription (..),
    mkHTTPEndpointDestinationDescription,
    httpeddS3BackupMode,
    httpeddCloudWatchLoggingOptions,
    httpeddS3DestinationDescription,
    httpeddBufferingHints,
    httpeddRetryOptions,
    httpeddEndpointConfiguration,
    httpeddProcessingConfiguration,
    httpeddRequestConfiguration,
    httpeddRoleARN,

    -- * HTTPEndpointDestinationUpdate
    HTTPEndpointDestinationUpdate (..),
    mkHTTPEndpointDestinationUpdate,
    httpeduS3BackupMode,
    httpeduCloudWatchLoggingOptions,
    httpeduS3Update,
    httpeduBufferingHints,
    httpeduRetryOptions,
    httpeduEndpointConfiguration,
    httpeduProcessingConfiguration,
    httpeduRequestConfiguration,
    httpeduRoleARN,

    -- * HTTPEndpointRequestConfiguration
    HTTPEndpointRequestConfiguration (..),
    mkHTTPEndpointRequestConfiguration,
    httpercCommonAttributes,
    httpercContentEncoding,

    -- * HTTPEndpointRetryOptions
    HTTPEndpointRetryOptions (..),
    mkHTTPEndpointRetryOptions,
    httperoDurationInSeconds,

    -- * HiveJSONSerDe
    HiveJSONSerDe (..),
    mkHiveJSONSerDe,
    hjsdTimestampFormats,

    -- * InputFormatConfiguration
    InputFormatConfiguration (..),
    mkInputFormatConfiguration,
    ifcDeserializer,

    -- * KMSEncryptionConfig
    KMSEncryptionConfig (..),
    mkKMSEncryptionConfig,
    kecAWSKMSKeyARN,

    -- * KinesisStreamSourceConfiguration
    KinesisStreamSourceConfiguration (..),
    mkKinesisStreamSourceConfiguration,
    ksscKinesisStreamARN,
    ksscRoleARN,

    -- * KinesisStreamSourceDescription
    KinesisStreamSourceDescription (..),
    mkKinesisStreamSourceDescription,
    kssdDeliveryStartTimestamp,
    kssdKinesisStreamARN,
    kssdRoleARN,

    -- * OpenXJSONSerDe
    OpenXJSONSerDe (..),
    mkOpenXJSONSerDe,
    oxjsdColumnToJSONKeyMappings,
    oxjsdCaseInsensitive,
    oxjsdConvertDotsInJSONKeysToUnderscores,

    -- * OrcSerDe
    OrcSerDe (..),
    mkOrcSerDe,
    osdBloomFilterFalsePositiveProbability,
    osdDictionaryKeyThreshold,
    osdEnablePadding,
    osdCompression,
    osdBloomFilterColumns,
    osdRowIndexStride,
    osdFormatVersion,
    osdBlockSizeBytes,
    osdStripeSizeBytes,
    osdPaddingTolerance,

    -- * OutputFormatConfiguration
    OutputFormatConfiguration (..),
    mkOutputFormatConfiguration,
    ofcSerializer,

    -- * ParquetSerDe
    ParquetSerDe (..),
    mkParquetSerDe,
    psdWriterVersion,
    psdCompression,
    psdMaxPaddingBytes,
    psdEnableDictionaryCompression,
    psdPageSizeBytes,
    psdBlockSizeBytes,

    -- * ProcessingConfiguration
    ProcessingConfiguration (..),
    mkProcessingConfiguration,
    pcEnabled,
    pcProcessors,

    -- * Processor
    Processor (..),
    mkProcessor,
    pParameters,
    pType,

    -- * ProcessorParameter
    ProcessorParameter (..),
    mkProcessorParameter,
    ppParameterValue,
    ppParameterName,

    -- * PutRecordBatchResponseEntry
    PutRecordBatchResponseEntry (..),
    mkPutRecordBatchResponseEntry,
    prbreRecordId,
    prbreErrorCode,
    prbreErrorMessage,

    -- * Record
    Record (..),
    mkRecord,
    rData,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (..),
    mkRedshiftDestinationConfiguration,
    rdcS3BackupMode,
    rdcS3Configuration,
    rdcCloudWatchLoggingOptions,
    rdcS3BackupConfiguration,
    rdcUsername,
    rdcPassword,
    rdcCopyCommand,
    rdcRetryOptions,
    rdcProcessingConfiguration,
    rdcClusterJDBCURL,
    rdcRoleARN,

    -- * RedshiftDestinationDescription
    RedshiftDestinationDescription (..),
    mkRedshiftDestinationDescription,
    rddS3BackupMode,
    rddS3BackupDescription,
    rddCloudWatchLoggingOptions,
    rddUsername,
    rddS3DestinationDescription,
    rddCopyCommand,
    rddRetryOptions,
    rddProcessingConfiguration,
    rddClusterJDBCURL,
    rddRoleARN,

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate (..),
    mkRedshiftDestinationUpdate,
    rduS3BackupMode,
    rduCloudWatchLoggingOptions,
    rduUsername,
    rduS3Update,
    rduPassword,
    rduS3BackupUpdate,
    rduCopyCommand,
    rduRetryOptions,
    rduProcessingConfiguration,
    rduClusterJDBCURL,
    rduRoleARN,

    -- * RedshiftRetryOptions
    RedshiftRetryOptions (..),
    mkRedshiftRetryOptions,
    rroDurationInSeconds,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    mkS3DestinationConfiguration,
    sdcPrefix,
    sdcCloudWatchLoggingOptions,
    sdcErrorOutputPrefix,
    sdcEncryptionConfiguration,
    sdcCompressionFormat,
    sdcBufferingHints,
    sdcBucketARN,
    sdcRoleARN,

    -- * S3DestinationDescription
    S3DestinationDescription (..),
    mkS3DestinationDescription,
    sddfPrefix,
    sddfCloudWatchLoggingOptions,
    sddfErrorOutputPrefix,
    sddfEncryptionConfiguration,
    sddfCompressionFormat,
    sddfBufferingHints,
    sddfBucketARN,
    sddfRoleARN,

    -- * S3DestinationUpdate
    S3DestinationUpdate (..),
    mkS3DestinationUpdate,
    sdufPrefix,
    sdufCloudWatchLoggingOptions,
    sdufErrorOutputPrefix,
    sdufEncryptionConfiguration,
    sdufCompressionFormat,
    sdufBufferingHints,
    sdufBucketARN,
    sdufRoleARN,

    -- * SchemaConfiguration
    SchemaConfiguration (..),
    mkSchemaConfiguration,
    scVersionId,
    scCatalogId,
    scDatabaseName,
    scRegion,
    scTableName,
    scRoleARN,

    -- * Serializer
    Serializer (..),
    mkSerializer,
    sOrcSerDe,
    sParquetSerDe,

    -- * SourceDescription
    SourceDescription (..),
    mkSourceDescription,
    sdKinesisStreamSourceDescription,

    -- * SplunkDestinationConfiguration
    SplunkDestinationConfiguration (..),
    mkSplunkDestinationConfiguration,
    sS3BackupMode,
    sHECToken,
    sHECEndpointType,
    sS3Configuration,
    sCloudWatchLoggingOptions,
    sHECAcknowledgmentTimeoutInSeconds,
    sHECEndpoint,
    sRetryOptions,
    sProcessingConfiguration,

    -- * SplunkDestinationDescription
    SplunkDestinationDescription (..),
    mkSplunkDestinationDescription,
    sddS3BackupMode,
    sddHECToken,
    sddHECEndpointType,
    sddCloudWatchLoggingOptions,
    sddHECAcknowledgmentTimeoutInSeconds,
    sddS3DestinationDescription,
    sddHECEndpoint,
    sddRetryOptions,
    sddProcessingConfiguration,

    -- * SplunkDestinationUpdate
    SplunkDestinationUpdate (..),
    mkSplunkDestinationUpdate,
    sduS3BackupMode,
    sduHECToken,
    sduHECEndpointType,
    sduCloudWatchLoggingOptions,
    sduHECAcknowledgmentTimeoutInSeconds,
    sduS3Update,
    sduHECEndpoint,
    sduRetryOptions,
    sduProcessingConfiguration,

    -- * SplunkRetryOptions
    SplunkRetryOptions (..),
    mkSplunkRetryOptions,
    sroDurationInSeconds,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * VPCConfiguration
    VPCConfiguration (..),
    mkVPCConfiguration,
    vcSecurityGroupIds,
    vcSubnetIds,
    vcRoleARN,

    -- * VPCConfigurationDescription
    VPCConfigurationDescription (..),
    mkVPCConfigurationDescription,
    vcdSecurityGroupIds,
    vcdSubnetIds,
    vcdVPCId,
    vcdRoleARN,
  )
where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.ContentEncoding
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamDescription
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
import Network.AWS.Firehose.Types.DeliveryStreamFailureType
import Network.AWS.Firehose.Types.DeliveryStreamStatus
import Network.AWS.Firehose.Types.DeliveryStreamType
import Network.AWS.Firehose.Types.Deserializer
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
import Network.AWS.Firehose.Types.HTTPEndpointConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointDescription
import Network.AWS.Firehose.Types.HTTPEndpointDestinationConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
import Network.AWS.Firehose.Types.HTTPEndpointDestinationUpdate
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.HiveJSONSerDe
import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.KeyType
import Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Firehose.Types.NoEncryptionConfig
import Network.AWS.Firehose.Types.OpenXJSONSerDe
import Network.AWS.Firehose.Types.OrcCompression
import Network.AWS.Firehose.Types.OrcFormatVersion
import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.OutputFormatConfiguration
import Network.AWS.Firehose.Types.ParquetCompression
import Network.AWS.Firehose.Types.ParquetSerDe
import Network.AWS.Firehose.Types.ParquetWriterVersion
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.Processor
import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorParameterName
import Network.AWS.Firehose.Types.ProcessorType
import Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
import Network.AWS.Firehose.Types.Record
import Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationUpdate
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Firehose.Types.SchemaConfiguration
import Network.AWS.Firehose.Types.Serializer
import Network.AWS.Firehose.Types.SourceDescription
import Network.AWS.Firehose.Types.SplunkDestinationConfiguration
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationUpdate
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import Network.AWS.Firehose.Types.Tag
import Network.AWS.Firehose.Types.VPCConfiguration
import Network.AWS.Firehose.Types.VPCConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-08-04@ of the Amazon Kinesis Firehose SDK configuration.
firehoseService :: Lude.Service
firehoseService =
  Lude.Service
    { Lude._svcAbbrev = "Firehose",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "firehose",
      Lude._svcVersion = "2015-08-04",
      Lude._svcEndpoint = Lude.defaultEndpoint firehoseService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Firehose",
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
