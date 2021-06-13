{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceUnavailableException,
    _ConcurrentModificationException,
    _ResourceInUseException,
    _LimitExceededException,
    _InvalidKMSResourceException,
    _ResourceNotFoundException,
    _InvalidArgumentException,

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

    -- * HttpEndpointS3BackupMode
    HttpEndpointS3BackupMode (..),

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
    newBufferingHints,
    bufferingHints_sizeInMBs,
    bufferingHints_intervalInSeconds,

    -- * CloudWatchLoggingOptions
    CloudWatchLoggingOptions (..),
    newCloudWatchLoggingOptions,
    cloudWatchLoggingOptions_logStreamName,
    cloudWatchLoggingOptions_enabled,
    cloudWatchLoggingOptions_logGroupName,

    -- * CopyCommand
    CopyCommand (..),
    newCopyCommand,
    copyCommand_copyOptions,
    copyCommand_dataTableColumns,
    copyCommand_dataTableName,

    -- * DataFormatConversionConfiguration
    DataFormatConversionConfiguration (..),
    newDataFormatConversionConfiguration,
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_inputFormatConfiguration,
    dataFormatConversionConfiguration_outputFormatConfiguration,
    dataFormatConversionConfiguration_schemaConfiguration,

    -- * DeliveryStreamDescription
    DeliveryStreamDescription (..),
    newDeliveryStreamDescription,
    deliveryStreamDescription_deliveryStreamEncryptionConfiguration,
    deliveryStreamDescription_source,
    deliveryStreamDescription_createTimestamp,
    deliveryStreamDescription_failureDescription,
    deliveryStreamDescription_lastUpdateTimestamp,
    deliveryStreamDescription_deliveryStreamName,
    deliveryStreamDescription_deliveryStreamARN,
    deliveryStreamDescription_deliveryStreamStatus,
    deliveryStreamDescription_deliveryStreamType,
    deliveryStreamDescription_versionId,
    deliveryStreamDescription_destinations,
    deliveryStreamDescription_hasMoreDestinations,

    -- * DeliveryStreamEncryptionConfiguration
    DeliveryStreamEncryptionConfiguration (..),
    newDeliveryStreamEncryptionConfiguration,
    deliveryStreamEncryptionConfiguration_status,
    deliveryStreamEncryptionConfiguration_keyARN,
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_failureDescription,

    -- * DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput (..),
    newDeliveryStreamEncryptionConfigurationInput,
    deliveryStreamEncryptionConfigurationInput_keyARN,
    deliveryStreamEncryptionConfigurationInput_keyType,

    -- * Deserializer
    Deserializer (..),
    newDeserializer,
    deserializer_hiveJsonSerDe,
    deserializer_openXJsonSerDe,

    -- * DestinationDescription
    DestinationDescription (..),
    newDestinationDescription,
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_splunkDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_destinationId,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints (..),
    newElasticsearchBufferingHints,
    elasticsearchBufferingHints_sizeInMBs,
    elasticsearchBufferingHints_intervalInSeconds,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (..),
    newElasticsearchDestinationConfiguration,
    elasticsearchDestinationConfiguration_typeName,
    elasticsearchDestinationConfiguration_clusterEndpoint,
    elasticsearchDestinationConfiguration_processingConfiguration,
    elasticsearchDestinationConfiguration_cloudWatchLoggingOptions,
    elasticsearchDestinationConfiguration_domainARN,
    elasticsearchDestinationConfiguration_vpcConfiguration,
    elasticsearchDestinationConfiguration_indexRotationPeriod,
    elasticsearchDestinationConfiguration_bufferingHints,
    elasticsearchDestinationConfiguration_retryOptions,
    elasticsearchDestinationConfiguration_s3BackupMode,
    elasticsearchDestinationConfiguration_roleARN,
    elasticsearchDestinationConfiguration_indexName,
    elasticsearchDestinationConfiguration_s3Configuration,

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (..),
    newElasticsearchDestinationDescription,
    elasticsearchDestinationDescription_typeName,
    elasticsearchDestinationDescription_roleARN,
    elasticsearchDestinationDescription_clusterEndpoint,
    elasticsearchDestinationDescription_indexName,
    elasticsearchDestinationDescription_processingConfiguration,
    elasticsearchDestinationDescription_cloudWatchLoggingOptions,
    elasticsearchDestinationDescription_domainARN,
    elasticsearchDestinationDescription_indexRotationPeriod,
    elasticsearchDestinationDescription_vpcConfigurationDescription,
    elasticsearchDestinationDescription_bufferingHints,
    elasticsearchDestinationDescription_retryOptions,
    elasticsearchDestinationDescription_s3BackupMode,
    elasticsearchDestinationDescription_s3DestinationDescription,

    -- * ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (..),
    newElasticsearchDestinationUpdate,
    elasticsearchDestinationUpdate_typeName,
    elasticsearchDestinationUpdate_roleARN,
    elasticsearchDestinationUpdate_clusterEndpoint,
    elasticsearchDestinationUpdate_indexName,
    elasticsearchDestinationUpdate_s3Update,
    elasticsearchDestinationUpdate_processingConfiguration,
    elasticsearchDestinationUpdate_cloudWatchLoggingOptions,
    elasticsearchDestinationUpdate_domainARN,
    elasticsearchDestinationUpdate_indexRotationPeriod,
    elasticsearchDestinationUpdate_bufferingHints,
    elasticsearchDestinationUpdate_retryOptions,

    -- * ElasticsearchRetryOptions
    ElasticsearchRetryOptions (..),
    newElasticsearchRetryOptions,
    elasticsearchRetryOptions_durationInSeconds,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsEncryptionConfig,
    encryptionConfiguration_noEncryptionConfig,

    -- * ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration (..),
    newExtendedS3DestinationConfiguration,
    extendedS3DestinationConfiguration_errorOutputPrefix,
    extendedS3DestinationConfiguration_encryptionConfiguration,
    extendedS3DestinationConfiguration_s3BackupConfiguration,
    extendedS3DestinationConfiguration_processingConfiguration,
    extendedS3DestinationConfiguration_dataFormatConversionConfiguration,
    extendedS3DestinationConfiguration_cloudWatchLoggingOptions,
    extendedS3DestinationConfiguration_prefix,
    extendedS3DestinationConfiguration_bufferingHints,
    extendedS3DestinationConfiguration_s3BackupMode,
    extendedS3DestinationConfiguration_compressionFormat,
    extendedS3DestinationConfiguration_roleARN,
    extendedS3DestinationConfiguration_bucketARN,

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (..),
    newExtendedS3DestinationDescription,
    extendedS3DestinationDescription_errorOutputPrefix,
    extendedS3DestinationDescription_processingConfiguration,
    extendedS3DestinationDescription_dataFormatConversionConfiguration,
    extendedS3DestinationDescription_cloudWatchLoggingOptions,
    extendedS3DestinationDescription_prefix,
    extendedS3DestinationDescription_s3BackupDescription,
    extendedS3DestinationDescription_s3BackupMode,
    extendedS3DestinationDescription_roleARN,
    extendedS3DestinationDescription_bucketARN,
    extendedS3DestinationDescription_bufferingHints,
    extendedS3DestinationDescription_compressionFormat,
    extendedS3DestinationDescription_encryptionConfiguration,

    -- * ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate (..),
    newExtendedS3DestinationUpdate,
    extendedS3DestinationUpdate_errorOutputPrefix,
    extendedS3DestinationUpdate_encryptionConfiguration,
    extendedS3DestinationUpdate_roleARN,
    extendedS3DestinationUpdate_bucketARN,
    extendedS3DestinationUpdate_processingConfiguration,
    extendedS3DestinationUpdate_dataFormatConversionConfiguration,
    extendedS3DestinationUpdate_cloudWatchLoggingOptions,
    extendedS3DestinationUpdate_prefix,
    extendedS3DestinationUpdate_s3BackupUpdate,
    extendedS3DestinationUpdate_bufferingHints,
    extendedS3DestinationUpdate_s3BackupMode,
    extendedS3DestinationUpdate_compressionFormat,

    -- * FailureDescription
    FailureDescription (..),
    newFailureDescription,
    failureDescription_type,
    failureDescription_details,

    -- * HiveJsonSerDe
    HiveJsonSerDe (..),
    newHiveJsonSerDe,
    hiveJsonSerDe_timestampFormats,

    -- * HttpEndpointBufferingHints
    HttpEndpointBufferingHints (..),
    newHttpEndpointBufferingHints,
    httpEndpointBufferingHints_sizeInMBs,
    httpEndpointBufferingHints_intervalInSeconds,

    -- * HttpEndpointCommonAttribute
    HttpEndpointCommonAttribute (..),
    newHttpEndpointCommonAttribute,
    httpEndpointCommonAttribute_attributeName,
    httpEndpointCommonAttribute_attributeValue,

    -- * HttpEndpointConfiguration
    HttpEndpointConfiguration (..),
    newHttpEndpointConfiguration,
    httpEndpointConfiguration_accessKey,
    httpEndpointConfiguration_name,
    httpEndpointConfiguration_url,

    -- * HttpEndpointDescription
    HttpEndpointDescription (..),
    newHttpEndpointDescription,
    httpEndpointDescription_name,
    httpEndpointDescription_url,

    -- * HttpEndpointDestinationConfiguration
    HttpEndpointDestinationConfiguration (..),
    newHttpEndpointDestinationConfiguration,
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- * HttpEndpointDestinationDescription
    HttpEndpointDestinationDescription (..),
    newHttpEndpointDestinationDescription,
    httpEndpointDestinationDescription_roleARN,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_s3DestinationDescription,

    -- * HttpEndpointDestinationUpdate
    HttpEndpointDestinationUpdate (..),
    newHttpEndpointDestinationUpdate,
    httpEndpointDestinationUpdate_roleARN,
    httpEndpointDestinationUpdate_s3Update,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_retryOptions,
    httpEndpointDestinationUpdate_s3BackupMode,

    -- * HttpEndpointRequestConfiguration
    HttpEndpointRequestConfiguration (..),
    newHttpEndpointRequestConfiguration,
    httpEndpointRequestConfiguration_contentEncoding,
    httpEndpointRequestConfiguration_commonAttributes,

    -- * HttpEndpointRetryOptions
    HttpEndpointRetryOptions (..),
    newHttpEndpointRetryOptions,
    httpEndpointRetryOptions_durationInSeconds,

    -- * InputFormatConfiguration
    InputFormatConfiguration (..),
    newInputFormatConfiguration,
    inputFormatConfiguration_deserializer,

    -- * KMSEncryptionConfig
    KMSEncryptionConfig (..),
    newKMSEncryptionConfig,
    kmsEncryptionConfig_aWSKMSKeyARN,

    -- * KinesisStreamSourceConfiguration
    KinesisStreamSourceConfiguration (..),
    newKinesisStreamSourceConfiguration,
    kinesisStreamSourceConfiguration_kinesisStreamARN,
    kinesisStreamSourceConfiguration_roleARN,

    -- * KinesisStreamSourceDescription
    KinesisStreamSourceDescription (..),
    newKinesisStreamSourceDescription,
    kinesisStreamSourceDescription_roleARN,
    kinesisStreamSourceDescription_deliveryStartTimestamp,
    kinesisStreamSourceDescription_kinesisStreamARN,

    -- * OpenXJsonSerDe
    OpenXJsonSerDe (..),
    newOpenXJsonSerDe,
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_columnToJsonKeyMappings,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,

    -- * OrcSerDe
    OrcSerDe (..),
    newOrcSerDe,
    orcSerDe_rowIndexStride,
    orcSerDe_compression,
    orcSerDe_dictionaryKeyThreshold,
    orcSerDe_blockSizeBytes,
    orcSerDe_formatVersion,
    orcSerDe_bloomFilterColumns,
    orcSerDe_enablePadding,
    orcSerDe_bloomFilterFalsePositiveProbability,
    orcSerDe_paddingTolerance,
    orcSerDe_stripeSizeBytes,

    -- * OutputFormatConfiguration
    OutputFormatConfiguration (..),
    newOutputFormatConfiguration,
    outputFormatConfiguration_serializer,

    -- * ParquetSerDe
    ParquetSerDe (..),
    newParquetSerDe,
    parquetSerDe_pageSizeBytes,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_compression,
    parquetSerDe_writerVersion,
    parquetSerDe_blockSizeBytes,

    -- * ProcessingConfiguration
    ProcessingConfiguration (..),
    newProcessingConfiguration,
    processingConfiguration_enabled,
    processingConfiguration_processors,

    -- * Processor
    Processor (..),
    newProcessor,
    processor_parameters,
    processor_type,

    -- * ProcessorParameter
    ProcessorParameter (..),
    newProcessorParameter,
    processorParameter_parameterName,
    processorParameter_parameterValue,

    -- * PutRecordBatchResponseEntry
    PutRecordBatchResponseEntry (..),
    newPutRecordBatchResponseEntry,
    putRecordBatchResponseEntry_recordId,
    putRecordBatchResponseEntry_errorMessage,
    putRecordBatchResponseEntry_errorCode,

    -- * Record
    Record (..),
    newRecord,
    record_data,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (..),
    newRedshiftDestinationConfiguration,
    redshiftDestinationConfiguration_s3BackupConfiguration,
    redshiftDestinationConfiguration_processingConfiguration,
    redshiftDestinationConfiguration_cloudWatchLoggingOptions,
    redshiftDestinationConfiguration_retryOptions,
    redshiftDestinationConfiguration_s3BackupMode,
    redshiftDestinationConfiguration_roleARN,
    redshiftDestinationConfiguration_clusterJDBCURL,
    redshiftDestinationConfiguration_copyCommand,
    redshiftDestinationConfiguration_username,
    redshiftDestinationConfiguration_password,
    redshiftDestinationConfiguration_s3Configuration,

    -- * RedshiftDestinationDescription
    RedshiftDestinationDescription (..),
    newRedshiftDestinationDescription,
    redshiftDestinationDescription_processingConfiguration,
    redshiftDestinationDescription_cloudWatchLoggingOptions,
    redshiftDestinationDescription_s3BackupDescription,
    redshiftDestinationDescription_retryOptions,
    redshiftDestinationDescription_s3BackupMode,
    redshiftDestinationDescription_roleARN,
    redshiftDestinationDescription_clusterJDBCURL,
    redshiftDestinationDescription_copyCommand,
    redshiftDestinationDescription_username,
    redshiftDestinationDescription_s3DestinationDescription,

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate (..),
    newRedshiftDestinationUpdate,
    redshiftDestinationUpdate_roleARN,
    redshiftDestinationUpdate_s3Update,
    redshiftDestinationUpdate_clusterJDBCURL,
    redshiftDestinationUpdate_processingConfiguration,
    redshiftDestinationUpdate_cloudWatchLoggingOptions,
    redshiftDestinationUpdate_copyCommand,
    redshiftDestinationUpdate_s3BackupUpdate,
    redshiftDestinationUpdate_password,
    redshiftDestinationUpdate_username,
    redshiftDestinationUpdate_retryOptions,
    redshiftDestinationUpdate_s3BackupMode,

    -- * RedshiftRetryOptions
    RedshiftRetryOptions (..),
    newRedshiftRetryOptions,
    redshiftRetryOptions_durationInSeconds,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    newS3DestinationConfiguration,
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- * S3DestinationDescription
    S3DestinationDescription (..),
    newS3DestinationDescription,
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_prefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- * S3DestinationUpdate
    S3DestinationUpdate (..),
    newS3DestinationUpdate,
    s3DestinationUpdate_errorOutputPrefix,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_roleARN,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_compressionFormat,

    -- * SchemaConfiguration
    SchemaConfiguration (..),
    newSchemaConfiguration,
    schemaConfiguration_roleARN,
    schemaConfiguration_tableName,
    schemaConfiguration_catalogId,
    schemaConfiguration_versionId,
    schemaConfiguration_region,
    schemaConfiguration_databaseName,

    -- * Serializer
    Serializer (..),
    newSerializer,
    serializer_orcSerDe,
    serializer_parquetSerDe,

    -- * SourceDescription
    SourceDescription (..),
    newSourceDescription,
    sourceDescription_kinesisStreamSourceDescription,

    -- * SplunkDestinationConfiguration
    SplunkDestinationConfiguration (..),
    newSplunkDestinationConfiguration,
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- * SplunkDestinationDescription
    SplunkDestinationDescription (..),
    newSplunkDestinationDescription,
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_processingConfiguration,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_s3DestinationDescription,

    -- * SplunkDestinationUpdate
    SplunkDestinationUpdate (..),
    newSplunkDestinationUpdate,
    splunkDestinationUpdate_s3Update,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationUpdate_processingConfiguration,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_hECToken,

    -- * SplunkRetryOptions
    SplunkRetryOptions (..),
    newSplunkRetryOptions,
    splunkRetryOptions_durationInSeconds,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_subnetIds,
    vpcConfiguration_roleARN,
    vpcConfiguration_securityGroupIds,

    -- * VpcConfigurationDescription
    VpcConfigurationDescription (..),
    newVpcConfigurationDescription,
    vpcConfigurationDescription_subnetIds,
    vpcConfigurationDescription_roleARN,
    vpcConfigurationDescription_securityGroupIds,
    vpcConfigurationDescription_vpcId,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.Firehose.Types.HiveJsonSerDe
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
import Network.AWS.Firehose.Types.HttpEndpointConfiguration
import Network.AWS.Firehose.Types.HttpEndpointDescription
import Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration
import Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
import Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.KeyType
import Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Firehose.Types.NoEncryptionConfig
import Network.AWS.Firehose.Types.OpenXJsonSerDe
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
import Network.AWS.Firehose.Types.VpcConfiguration
import Network.AWS.Firehose.Types.VpcConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-08-04@ of the Amazon Kinesis Firehose SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Firehose",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "firehose",
      Core._serviceSigningName = "firehose",
      Core._serviceVersion = "2015-08-04",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Firehose",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The service is unavailable. Back off and retry the operation. If you
-- continue to see the exception, throughput limits for the delivery stream
-- may have been exceeded. For more information about limits and how to
-- request an increase, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits>.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | Another modification has already happened. Fetch @VersionId@ again and
-- use it to update the destination.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The resource is already in use and not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | You have already reached the limit for a requested resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Kinesis Data Firehose throws this exception when an attempt to put
-- records or to start or stop delivery stream encryption fails. This
-- happens when the KMS service throws one of the following exception
-- types: @AccessDeniedException@, @InvalidStateException@,
-- @DisabledException@, or @NotFoundException@.
_InvalidKMSResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidKMSResourceException"

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified input parameter has a value that is not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"
