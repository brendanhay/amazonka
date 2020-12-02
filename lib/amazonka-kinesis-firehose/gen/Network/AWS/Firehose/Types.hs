{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types
  ( -- * Service Configuration
    firehose,

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
    BufferingHints,
    bufferingHints,
    bhSizeInMBs,
    bhIntervalInSeconds,

    -- * CloudWatchLoggingOptions
    CloudWatchLoggingOptions,
    cloudWatchLoggingOptions,
    cwloEnabled,
    cwloLogGroupName,
    cwloLogStreamName,

    -- * CopyCommand
    CopyCommand,
    copyCommand,
    ccCopyOptions,
    ccDataTableColumns,
    ccDataTableName,

    -- * DataFormatConversionConfiguration
    DataFormatConversionConfiguration,
    dataFormatConversionConfiguration,
    dfccOutputFormatConfiguration,
    dfccEnabled,
    dfccSchemaConfiguration,
    dfccInputFormatConfiguration,

    -- * DeliveryStreamDescription
    DeliveryStreamDescription,
    deliveryStreamDescription,
    dsdFailureDescription,
    dsdDeliveryStreamEncryptionConfiguration,
    dsdCreateTimestamp,
    dsdSource,
    dsdLastUpdateTimestamp,
    dsdDeliveryStreamName,
    dsdDeliveryStreamARN,
    dsdDeliveryStreamStatus,
    dsdDeliveryStreamType,
    dsdVersionId,
    dsdDestinations,
    dsdHasMoreDestinations,

    -- * DeliveryStreamEncryptionConfiguration
    DeliveryStreamEncryptionConfiguration,
    deliveryStreamEncryptionConfiguration,
    dsecStatus,
    dsecKeyType,
    dsecKeyARN,
    dsecFailureDescription,

    -- * DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput,
    deliveryStreamEncryptionConfigurationInput,
    dseciKeyARN,
    dseciKeyType,

    -- * Deserializer
    Deserializer,
    deserializer,
    dOpenXJSONSerDe,
    dHiveJSONSerDe,

    -- * DestinationDescription
    DestinationDescription,
    destinationDescription,
    ddSplunkDestinationDescription,
    ddHTTPEndpointDestinationDescription,
    ddS3DestinationDescription,
    ddExtendedS3DestinationDescription,
    ddElasticsearchDestinationDescription,
    ddRedshiftDestinationDescription,
    ddDestinationId,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints,
    elasticsearchBufferingHints,
    ebhSizeInMBs,
    ebhIntervalInSeconds,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration,
    elasticsearchDestinationConfiguration,
    edcIndexRotationPeriod,
    edcTypeName,
    edcS3BackupMode,
    edcDomainARN,
    edcCloudWatchLoggingOptions,
    edcVPCConfiguration,
    edcBufferingHints,
    edcRetryOptions,
    edcProcessingConfiguration,
    edcClusterEndpoint,
    edcRoleARN,
    edcIndexName,
    edcS3Configuration,

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription,
    elasticsearchDestinationDescription,
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
    ElasticsearchDestinationUpdate,
    elasticsearchDestinationUpdate,
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
    ElasticsearchRetryOptions,
    elasticsearchRetryOptions,
    eroDurationInSeconds,

    -- * EncryptionConfiguration
    EncryptionConfiguration,
    encryptionConfiguration,
    ecNoEncryptionConfig,
    ecKMSEncryptionConfig,

    -- * ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration,
    extendedS3DestinationConfiguration,
    esdcS3BackupMode,
    esdcPrefix,
    esdcCloudWatchLoggingOptions,
    esdcS3BackupConfiguration,
    esdcErrorOutputPrefix,
    esdcEncryptionConfiguration,
    esdcCompressionFormat,
    esdcBufferingHints,
    esdcDataFormatConversionConfiguration,
    esdcProcessingConfiguration,
    esdcRoleARN,
    esdcBucketARN,

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription,
    extendedS3DestinationDescription,
    esddS3BackupMode,
    esddS3BackupDescription,
    esddPrefix,
    esddCloudWatchLoggingOptions,
    esddErrorOutputPrefix,
    esddDataFormatConversionConfiguration,
    esddProcessingConfiguration,
    esddRoleARN,
    esddBucketARN,
    esddBufferingHints,
    esddCompressionFormat,
    esddEncryptionConfiguration,

    -- * ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate,
    extendedS3DestinationUpdate,
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
    FailureDescription,
    failureDescription,
    fdType,
    fdDetails,

    -- * HTTPEndpointBufferingHints
    HTTPEndpointBufferingHints,
    hTTPEndpointBufferingHints,
    httpebhSizeInMBs,
    httpebhIntervalInSeconds,

    -- * HTTPEndpointCommonAttribute
    HTTPEndpointCommonAttribute,
    hTTPEndpointCommonAttribute,
    httpecaAttributeName,
    httpecaAttributeValue,

    -- * HTTPEndpointConfiguration
    HTTPEndpointConfiguration,
    hTTPEndpointConfiguration,
    httpecName,
    httpecAccessKey,
    httpecURL,

    -- * HTTPEndpointDescription
    HTTPEndpointDescription,
    hTTPEndpointDescription,
    httpedURL,
    httpedName,

    -- * HTTPEndpointDestinationConfiguration
    HTTPEndpointDestinationConfiguration,
    hTTPEndpointDestinationConfiguration,
    httpedcS3BackupMode,
    httpedcCloudWatchLoggingOptions,
    httpedcBufferingHints,
    httpedcRetryOptions,
    httpedcProcessingConfiguration,
    httpedcRequestConfiguration,
    httpedcRoleARN,
    httpedcEndpointConfiguration,
    httpedcS3Configuration,

    -- * HTTPEndpointDestinationDescription
    HTTPEndpointDestinationDescription,
    hTTPEndpointDestinationDescription,
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
    HTTPEndpointDestinationUpdate,
    hTTPEndpointDestinationUpdate,
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
    HTTPEndpointRequestConfiguration,
    hTTPEndpointRequestConfiguration,
    httpercCommonAttributes,
    httpercContentEncoding,

    -- * HTTPEndpointRetryOptions
    HTTPEndpointRetryOptions,
    hTTPEndpointRetryOptions,
    httperoDurationInSeconds,

    -- * HiveJSONSerDe
    HiveJSONSerDe,
    hiveJSONSerDe,
    hjsdTimestampFormats,

    -- * InputFormatConfiguration
    InputFormatConfiguration,
    inputFormatConfiguration,
    ifcDeserializer,

    -- * KMSEncryptionConfig
    KMSEncryptionConfig,
    kmsEncryptionConfig,
    kecAWSKMSKeyARN,

    -- * KinesisStreamSourceConfiguration
    KinesisStreamSourceConfiguration,
    kinesisStreamSourceConfiguration,
    ksscKinesisStreamARN,
    ksscRoleARN,

    -- * KinesisStreamSourceDescription
    KinesisStreamSourceDescription,
    kinesisStreamSourceDescription,
    kssdDeliveryStartTimestamp,
    kssdKinesisStreamARN,
    kssdRoleARN,

    -- * OpenXJSONSerDe
    OpenXJSONSerDe,
    openXJSONSerDe,
    oxjsdColumnToJSONKeyMappings,
    oxjsdCaseInsensitive,
    oxjsdConvertDotsInJSONKeysToUnderscores,

    -- * OrcSerDe
    OrcSerDe,
    orcSerDe,
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
    OutputFormatConfiguration,
    outputFormatConfiguration,
    ofcSerializer,

    -- * ParquetSerDe
    ParquetSerDe,
    parquetSerDe,
    psdWriterVersion,
    psdCompression,
    psdMaxPaddingBytes,
    psdEnableDictionaryCompression,
    psdPageSizeBytes,
    psdBlockSizeBytes,

    -- * ProcessingConfiguration
    ProcessingConfiguration,
    processingConfiguration,
    pcEnabled,
    pcProcessors,

    -- * Processor
    Processor,
    processor,
    pParameters,
    pType,

    -- * ProcessorParameter
    ProcessorParameter,
    processorParameter,
    ppParameterName,
    ppParameterValue,

    -- * PutRecordBatchResponseEntry
    PutRecordBatchResponseEntry,
    putRecordBatchResponseEntry,
    prbreRecordId,
    prbreErrorCode,
    prbreErrorMessage,

    -- * Record
    Record,
    record,
    rData,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration,
    redshiftDestinationConfiguration,
    rdcS3BackupMode,
    rdcCloudWatchLoggingOptions,
    rdcS3BackupConfiguration,
    rdcRetryOptions,
    rdcProcessingConfiguration,
    rdcRoleARN,
    rdcClusterJDBCURL,
    rdcCopyCommand,
    rdcUsername,
    rdcPassword,
    rdcS3Configuration,

    -- * RedshiftDestinationDescription
    RedshiftDestinationDescription,
    redshiftDestinationDescription,
    rddS3BackupMode,
    rddS3BackupDescription,
    rddCloudWatchLoggingOptions,
    rddRetryOptions,
    rddProcessingConfiguration,
    rddRoleARN,
    rddClusterJDBCURL,
    rddCopyCommand,
    rddUsername,
    rddS3DestinationDescription,

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate,
    redshiftDestinationUpdate,
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
    RedshiftRetryOptions,
    redshiftRetryOptions,
    rroDurationInSeconds,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration,
    s3DestinationConfiguration,
    sdcPrefix,
    sdcCloudWatchLoggingOptions,
    sdcErrorOutputPrefix,
    sdcEncryptionConfiguration,
    sdcCompressionFormat,
    sdcBufferingHints,
    sdcRoleARN,
    sdcBucketARN,

    -- * S3DestinationDescription
    S3DestinationDescription,
    s3DestinationDescription,
    s3Prefix,
    s3CloudWatchLoggingOptions,
    s3ErrorOutputPrefix,
    s3RoleARN,
    s3BucketARN,
    s3BufferingHints,
    s3CompressionFormat,
    s3EncryptionConfiguration,

    -- * S3DestinationUpdate
    S3DestinationUpdate,
    s3DestinationUpdate,
    sPrefix,
    sCloudWatchLoggingOptions,
    sErrorOutputPrefix,
    sEncryptionConfiguration,
    sCompressionFormat,
    sBufferingHints,
    sBucketARN,
    sRoleARN,

    -- * SchemaConfiguration
    SchemaConfiguration,
    schemaConfiguration,
    scVersionId,
    scCatalogId,
    scDatabaseName,
    scRegion,
    scTableName,
    scRoleARN,

    -- * Serializer
    Serializer,
    serializer,
    sOrcSerDe,
    sParquetSerDe,

    -- * SourceDescription
    SourceDescription,
    sourceDescription,
    sdKinesisStreamSourceDescription,

    -- * SplunkDestinationConfiguration
    SplunkDestinationConfiguration,
    splunkDestinationConfiguration,
    splS3BackupMode,
    splCloudWatchLoggingOptions,
    splHECAcknowledgmentTimeoutInSeconds,
    splRetryOptions,
    splProcessingConfiguration,
    splHECEndpoint,
    splHECEndpointType,
    splHECToken,
    splS3Configuration,

    -- * SplunkDestinationDescription
    SplunkDestinationDescription,
    splunkDestinationDescription,
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
    SplunkDestinationUpdate,
    splunkDestinationUpdate,
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
    SplunkRetryOptions,
    splunkRetryOptions,
    sroDurationInSeconds,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * VPCConfiguration
    VPCConfiguration,
    vpcConfiguration,
    vcSubnetIds,
    vcRoleARN,
    vcSecurityGroupIds,

    -- * VPCConfigurationDescription
    VPCConfigurationDescription,
    vpcConfigurationDescription,
    vcdSubnetIds,
    vcdRoleARN,
    vcdSecurityGroupIds,
    vcdVPCId,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-08-04@ of the Amazon Kinesis Firehose SDK configuration.
firehose :: Service
firehose =
  Service
    { _svcAbbrev = "Firehose",
      _svcSigner = v4,
      _svcPrefix = "firehose",
      _svcVersion = "2015-08-04",
      _svcEndpoint = defaultEndpoint firehose,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Firehose",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
