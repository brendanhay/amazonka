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
    _InvalidArgumentException,
    _InvalidKMSResourceException,
    _ConcurrentModificationException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * AmazonopensearchserviceIndexRotationPeriod
    AmazonopensearchserviceIndexRotationPeriod (..),

    -- * AmazonopensearchserviceS3BackupMode
    AmazonopensearchserviceS3BackupMode (..),

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

    -- * AmazonopensearchserviceBufferingHints
    AmazonopensearchserviceBufferingHints (..),
    newAmazonopensearchserviceBufferingHints,
    amazonopensearchserviceBufferingHints_sizeInMBs,
    amazonopensearchserviceBufferingHints_intervalInSeconds,

    -- * AmazonopensearchserviceDestinationConfiguration
    AmazonopensearchserviceDestinationConfiguration (..),
    newAmazonopensearchserviceDestinationConfiguration,
    amazonopensearchserviceDestinationConfiguration_indexRotationPeriod,
    amazonopensearchserviceDestinationConfiguration_typeName,
    amazonopensearchserviceDestinationConfiguration_s3BackupMode,
    amazonopensearchserviceDestinationConfiguration_domainARN,
    amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationConfiguration_vpcConfiguration,
    amazonopensearchserviceDestinationConfiguration_bufferingHints,
    amazonopensearchserviceDestinationConfiguration_retryOptions,
    amazonopensearchserviceDestinationConfiguration_processingConfiguration,
    amazonopensearchserviceDestinationConfiguration_clusterEndpoint,
    amazonopensearchserviceDestinationConfiguration_roleARN,
    amazonopensearchserviceDestinationConfiguration_indexName,
    amazonopensearchserviceDestinationConfiguration_s3Configuration,

    -- * AmazonopensearchserviceDestinationDescription
    AmazonopensearchserviceDestinationDescription (..),
    newAmazonopensearchserviceDestinationDescription,
    amazonopensearchserviceDestinationDescription_indexRotationPeriod,
    amazonopensearchserviceDestinationDescription_typeName,
    amazonopensearchserviceDestinationDescription_s3BackupMode,
    amazonopensearchserviceDestinationDescription_domainARN,
    amazonopensearchserviceDestinationDescription_vpcConfigurationDescription,
    amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationDescription_s3DestinationDescription,
    amazonopensearchserviceDestinationDescription_bufferingHints,
    amazonopensearchserviceDestinationDescription_retryOptions,
    amazonopensearchserviceDestinationDescription_processingConfiguration,
    amazonopensearchserviceDestinationDescription_roleARN,
    amazonopensearchserviceDestinationDescription_clusterEndpoint,
    amazonopensearchserviceDestinationDescription_indexName,

    -- * AmazonopensearchserviceDestinationUpdate
    AmazonopensearchserviceDestinationUpdate (..),
    newAmazonopensearchserviceDestinationUpdate,
    amazonopensearchserviceDestinationUpdate_indexRotationPeriod,
    amazonopensearchserviceDestinationUpdate_typeName,
    amazonopensearchserviceDestinationUpdate_domainARN,
    amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationUpdate_s3Update,
    amazonopensearchserviceDestinationUpdate_bufferingHints,
    amazonopensearchserviceDestinationUpdate_retryOptions,
    amazonopensearchserviceDestinationUpdate_processingConfiguration,
    amazonopensearchserviceDestinationUpdate_roleARN,
    amazonopensearchserviceDestinationUpdate_clusterEndpoint,
    amazonopensearchserviceDestinationUpdate_indexName,

    -- * AmazonopensearchserviceRetryOptions
    AmazonopensearchserviceRetryOptions (..),
    newAmazonopensearchserviceRetryOptions,
    amazonopensearchserviceRetryOptions_durationInSeconds,

    -- * BufferingHints
    BufferingHints (..),
    newBufferingHints,
    bufferingHints_sizeInMBs,
    bufferingHints_intervalInSeconds,

    -- * CloudWatchLoggingOptions
    CloudWatchLoggingOptions (..),
    newCloudWatchLoggingOptions,
    cloudWatchLoggingOptions_enabled,
    cloudWatchLoggingOptions_logGroupName,
    cloudWatchLoggingOptions_logStreamName,

    -- * CopyCommand
    CopyCommand (..),
    newCopyCommand,
    copyCommand_copyOptions,
    copyCommand_dataTableColumns,
    copyCommand_dataTableName,

    -- * DataFormatConversionConfiguration
    DataFormatConversionConfiguration (..),
    newDataFormatConversionConfiguration,
    dataFormatConversionConfiguration_outputFormatConfiguration,
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_schemaConfiguration,
    dataFormatConversionConfiguration_inputFormatConfiguration,

    -- * DeliveryStreamDescription
    DeliveryStreamDescription (..),
    newDeliveryStreamDescription,
    deliveryStreamDescription_failureDescription,
    deliveryStreamDescription_deliveryStreamEncryptionConfiguration,
    deliveryStreamDescription_createTimestamp,
    deliveryStreamDescription_source,
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
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_keyARN,
    deliveryStreamEncryptionConfiguration_failureDescription,

    -- * DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput (..),
    newDeliveryStreamEncryptionConfigurationInput,
    deliveryStreamEncryptionConfigurationInput_keyARN,
    deliveryStreamEncryptionConfigurationInput_keyType,

    -- * Deserializer
    Deserializer (..),
    newDeserializer,
    deserializer_openXJsonSerDe,
    deserializer_hiveJsonSerDe,

    -- * DestinationDescription
    DestinationDescription (..),
    newDestinationDescription,
    destinationDescription_splunkDestinationDescription,
    destinationDescription_amazonopensearchserviceDestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_destinationId,

    -- * DynamicPartitioningConfiguration
    DynamicPartitioningConfiguration (..),
    newDynamicPartitioningConfiguration,
    dynamicPartitioningConfiguration_enabled,
    dynamicPartitioningConfiguration_retryOptions,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints (..),
    newElasticsearchBufferingHints,
    elasticsearchBufferingHints_sizeInMBs,
    elasticsearchBufferingHints_intervalInSeconds,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (..),
    newElasticsearchDestinationConfiguration,
    elasticsearchDestinationConfiguration_indexRotationPeriod,
    elasticsearchDestinationConfiguration_typeName,
    elasticsearchDestinationConfiguration_s3BackupMode,
    elasticsearchDestinationConfiguration_domainARN,
    elasticsearchDestinationConfiguration_cloudWatchLoggingOptions,
    elasticsearchDestinationConfiguration_vpcConfiguration,
    elasticsearchDestinationConfiguration_bufferingHints,
    elasticsearchDestinationConfiguration_retryOptions,
    elasticsearchDestinationConfiguration_processingConfiguration,
    elasticsearchDestinationConfiguration_clusterEndpoint,
    elasticsearchDestinationConfiguration_roleARN,
    elasticsearchDestinationConfiguration_indexName,
    elasticsearchDestinationConfiguration_s3Configuration,

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (..),
    newElasticsearchDestinationDescription,
    elasticsearchDestinationDescription_indexRotationPeriod,
    elasticsearchDestinationDescription_typeName,
    elasticsearchDestinationDescription_s3BackupMode,
    elasticsearchDestinationDescription_domainARN,
    elasticsearchDestinationDescription_vpcConfigurationDescription,
    elasticsearchDestinationDescription_cloudWatchLoggingOptions,
    elasticsearchDestinationDescription_s3DestinationDescription,
    elasticsearchDestinationDescription_bufferingHints,
    elasticsearchDestinationDescription_retryOptions,
    elasticsearchDestinationDescription_processingConfiguration,
    elasticsearchDestinationDescription_roleARN,
    elasticsearchDestinationDescription_clusterEndpoint,
    elasticsearchDestinationDescription_indexName,

    -- * ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (..),
    newElasticsearchDestinationUpdate,
    elasticsearchDestinationUpdate_indexRotationPeriod,
    elasticsearchDestinationUpdate_typeName,
    elasticsearchDestinationUpdate_domainARN,
    elasticsearchDestinationUpdate_cloudWatchLoggingOptions,
    elasticsearchDestinationUpdate_s3Update,
    elasticsearchDestinationUpdate_bufferingHints,
    elasticsearchDestinationUpdate_retryOptions,
    elasticsearchDestinationUpdate_processingConfiguration,
    elasticsearchDestinationUpdate_roleARN,
    elasticsearchDestinationUpdate_clusterEndpoint,
    elasticsearchDestinationUpdate_indexName,

    -- * ElasticsearchRetryOptions
    ElasticsearchRetryOptions (..),
    newElasticsearchRetryOptions,
    elasticsearchRetryOptions_durationInSeconds,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_noEncryptionConfig,
    encryptionConfiguration_kmsEncryptionConfig,

    -- * ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration (..),
    newExtendedS3DestinationConfiguration,
    extendedS3DestinationConfiguration_s3BackupMode,
    extendedS3DestinationConfiguration_prefix,
    extendedS3DestinationConfiguration_cloudWatchLoggingOptions,
    extendedS3DestinationConfiguration_s3BackupConfiguration,
    extendedS3DestinationConfiguration_errorOutputPrefix,
    extendedS3DestinationConfiguration_encryptionConfiguration,
    extendedS3DestinationConfiguration_dynamicPartitioningConfiguration,
    extendedS3DestinationConfiguration_compressionFormat,
    extendedS3DestinationConfiguration_bufferingHints,
    extendedS3DestinationConfiguration_dataFormatConversionConfiguration,
    extendedS3DestinationConfiguration_processingConfiguration,
    extendedS3DestinationConfiguration_roleARN,
    extendedS3DestinationConfiguration_bucketARN,

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (..),
    newExtendedS3DestinationDescription,
    extendedS3DestinationDescription_s3BackupMode,
    extendedS3DestinationDescription_s3BackupDescription,
    extendedS3DestinationDescription_prefix,
    extendedS3DestinationDescription_cloudWatchLoggingOptions,
    extendedS3DestinationDescription_errorOutputPrefix,
    extendedS3DestinationDescription_dynamicPartitioningConfiguration,
    extendedS3DestinationDescription_dataFormatConversionConfiguration,
    extendedS3DestinationDescription_processingConfiguration,
    extendedS3DestinationDescription_roleARN,
    extendedS3DestinationDescription_bucketARN,
    extendedS3DestinationDescription_bufferingHints,
    extendedS3DestinationDescription_compressionFormat,
    extendedS3DestinationDescription_encryptionConfiguration,

    -- * ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate (..),
    newExtendedS3DestinationUpdate,
    extendedS3DestinationUpdate_s3BackupMode,
    extendedS3DestinationUpdate_prefix,
    extendedS3DestinationUpdate_cloudWatchLoggingOptions,
    extendedS3DestinationUpdate_errorOutputPrefix,
    extendedS3DestinationUpdate_s3BackupUpdate,
    extendedS3DestinationUpdate_encryptionConfiguration,
    extendedS3DestinationUpdate_dynamicPartitioningConfiguration,
    extendedS3DestinationUpdate_compressionFormat,
    extendedS3DestinationUpdate_bufferingHints,
    extendedS3DestinationUpdate_dataFormatConversionConfiguration,
    extendedS3DestinationUpdate_bucketARN,
    extendedS3DestinationUpdate_processingConfiguration,
    extendedS3DestinationUpdate_roleARN,

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
    httpEndpointConfiguration_name,
    httpEndpointConfiguration_accessKey,
    httpEndpointConfiguration_url,

    -- * HttpEndpointDescription
    HttpEndpointDescription (..),
    newHttpEndpointDescription,
    httpEndpointDescription_url,
    httpEndpointDescription_name,

    -- * HttpEndpointDestinationConfiguration
    HttpEndpointDestinationConfiguration (..),
    newHttpEndpointDestinationConfiguration,
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- * HttpEndpointDestinationDescription
    HttpEndpointDestinationDescription (..),
    newHttpEndpointDestinationDescription,
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_s3DestinationDescription,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_roleARN,

    -- * HttpEndpointDestinationUpdate
    HttpEndpointDestinationUpdate (..),
    newHttpEndpointDestinationUpdate,
    httpEndpointDestinationUpdate_s3BackupMode,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_s3Update,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_retryOptions,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_roleARN,

    -- * HttpEndpointRequestConfiguration
    HttpEndpointRequestConfiguration (..),
    newHttpEndpointRequestConfiguration,
    httpEndpointRequestConfiguration_commonAttributes,
    httpEndpointRequestConfiguration_contentEncoding,

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
    kinesisStreamSourceDescription_deliveryStartTimestamp,
    kinesisStreamSourceDescription_kinesisStreamARN,
    kinesisStreamSourceDescription_roleARN,

    -- * OpenXJsonSerDe
    OpenXJsonSerDe (..),
    newOpenXJsonSerDe,
    openXJsonSerDe_columnToJsonKeyMappings,
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,

    -- * OrcSerDe
    OrcSerDe (..),
    newOrcSerDe,
    orcSerDe_bloomFilterFalsePositiveProbability,
    orcSerDe_dictionaryKeyThreshold,
    orcSerDe_enablePadding,
    orcSerDe_compression,
    orcSerDe_bloomFilterColumns,
    orcSerDe_rowIndexStride,
    orcSerDe_formatVersion,
    orcSerDe_blockSizeBytes,
    orcSerDe_stripeSizeBytes,
    orcSerDe_paddingTolerance,

    -- * OutputFormatConfiguration
    OutputFormatConfiguration (..),
    newOutputFormatConfiguration,
    outputFormatConfiguration_serializer,

    -- * ParquetSerDe
    ParquetSerDe (..),
    newParquetSerDe,
    parquetSerDe_writerVersion,
    parquetSerDe_compression,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_pageSizeBytes,
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
    putRecordBatchResponseEntry_errorCode,
    putRecordBatchResponseEntry_errorMessage,

    -- * Record
    Record (..),
    newRecord,
    record_data,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (..),
    newRedshiftDestinationConfiguration,
    redshiftDestinationConfiguration_s3BackupMode,
    redshiftDestinationConfiguration_cloudWatchLoggingOptions,
    redshiftDestinationConfiguration_s3BackupConfiguration,
    redshiftDestinationConfiguration_retryOptions,
    redshiftDestinationConfiguration_processingConfiguration,
    redshiftDestinationConfiguration_roleARN,
    redshiftDestinationConfiguration_clusterJDBCURL,
    redshiftDestinationConfiguration_copyCommand,
    redshiftDestinationConfiguration_username,
    redshiftDestinationConfiguration_password,
    redshiftDestinationConfiguration_s3Configuration,

    -- * RedshiftDestinationDescription
    RedshiftDestinationDescription (..),
    newRedshiftDestinationDescription,
    redshiftDestinationDescription_s3BackupMode,
    redshiftDestinationDescription_s3BackupDescription,
    redshiftDestinationDescription_cloudWatchLoggingOptions,
    redshiftDestinationDescription_retryOptions,
    redshiftDestinationDescription_processingConfiguration,
    redshiftDestinationDescription_roleARN,
    redshiftDestinationDescription_clusterJDBCURL,
    redshiftDestinationDescription_copyCommand,
    redshiftDestinationDescription_username,
    redshiftDestinationDescription_s3DestinationDescription,

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate (..),
    newRedshiftDestinationUpdate,
    redshiftDestinationUpdate_s3BackupMode,
    redshiftDestinationUpdate_cloudWatchLoggingOptions,
    redshiftDestinationUpdate_username,
    redshiftDestinationUpdate_s3Update,
    redshiftDestinationUpdate_password,
    redshiftDestinationUpdate_s3BackupUpdate,
    redshiftDestinationUpdate_copyCommand,
    redshiftDestinationUpdate_retryOptions,
    redshiftDestinationUpdate_processingConfiguration,
    redshiftDestinationUpdate_clusterJDBCURL,
    redshiftDestinationUpdate_roleARN,

    -- * RedshiftRetryOptions
    RedshiftRetryOptions (..),
    newRedshiftRetryOptions,
    redshiftRetryOptions_durationInSeconds,

    -- * RetryOptions
    RetryOptions (..),
    newRetryOptions,
    retryOptions_durationInSeconds,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    newS3DestinationConfiguration,
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- * S3DestinationDescription
    S3DestinationDescription (..),
    newS3DestinationDescription,
    s3DestinationDescription_prefix,
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- * S3DestinationUpdate
    S3DestinationUpdate (..),
    newS3DestinationUpdate,
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_errorOutputPrefix,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_compressionFormat,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_roleARN,

    -- * SchemaConfiguration
    SchemaConfiguration (..),
    newSchemaConfiguration,
    schemaConfiguration_versionId,
    schemaConfiguration_catalogId,
    schemaConfiguration_databaseName,
    schemaConfiguration_region,
    schemaConfiguration_tableName,
    schemaConfiguration_roleARN,

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
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- * SplunkDestinationDescription
    SplunkDestinationDescription (..),
    newSplunkDestinationDescription,
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_s3DestinationDescription,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_processingConfiguration,

    -- * SplunkDestinationUpdate
    SplunkDestinationUpdate (..),
    newSplunkDestinationUpdate,
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_hECToken,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationUpdate_s3Update,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_processingConfiguration,

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
import Network.AWS.Firehose.Types.AmazonopensearchserviceBufferingHints
import Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
import Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationDescription
import Network.AWS.Firehose.Types.AmazonopensearchserviceDestinationUpdate
import Network.AWS.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Network.AWS.Firehose.Types.AmazonopensearchserviceRetryOptions
import Network.AWS.Firehose.Types.AmazonopensearchserviceS3BackupMode
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
import Network.AWS.Firehose.Types.DynamicPartitioningConfiguration
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
import Network.AWS.Firehose.Types.RetryOptions
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

-- | The specified input parameter has a value that is not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

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

-- | Another modification has already happened. Fetch @VersionId@ again and
-- use it to update the destination.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

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

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You have already reached the limit for a requested resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource is already in use and not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
