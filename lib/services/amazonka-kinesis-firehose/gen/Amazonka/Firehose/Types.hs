{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Firehose.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _InvalidArgumentException,
    _InvalidKMSResourceException,
    _LimitExceededException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,

    -- * AmazonOpenSearchServerlessS3BackupMode
    AmazonOpenSearchServerlessS3BackupMode (..),

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

    -- * AmazonOpenSearchServerlessBufferingHints
    AmazonOpenSearchServerlessBufferingHints (..),
    newAmazonOpenSearchServerlessBufferingHints,
    amazonOpenSearchServerlessBufferingHints_intervalInSeconds,
    amazonOpenSearchServerlessBufferingHints_sizeInMBs,

    -- * AmazonOpenSearchServerlessDestinationConfiguration
    AmazonOpenSearchServerlessDestinationConfiguration (..),
    newAmazonOpenSearchServerlessDestinationConfiguration,
    amazonOpenSearchServerlessDestinationConfiguration_bufferingHints,
    amazonOpenSearchServerlessDestinationConfiguration_cloudWatchLoggingOptions,
    amazonOpenSearchServerlessDestinationConfiguration_collectionEndpoint,
    amazonOpenSearchServerlessDestinationConfiguration_processingConfiguration,
    amazonOpenSearchServerlessDestinationConfiguration_retryOptions,
    amazonOpenSearchServerlessDestinationConfiguration_s3BackupMode,
    amazonOpenSearchServerlessDestinationConfiguration_vpcConfiguration,
    amazonOpenSearchServerlessDestinationConfiguration_roleARN,
    amazonOpenSearchServerlessDestinationConfiguration_indexName,
    amazonOpenSearchServerlessDestinationConfiguration_s3Configuration,

    -- * AmazonOpenSearchServerlessDestinationDescription
    AmazonOpenSearchServerlessDestinationDescription (..),
    newAmazonOpenSearchServerlessDestinationDescription,
    amazonOpenSearchServerlessDestinationDescription_bufferingHints,
    amazonOpenSearchServerlessDestinationDescription_cloudWatchLoggingOptions,
    amazonOpenSearchServerlessDestinationDescription_collectionEndpoint,
    amazonOpenSearchServerlessDestinationDescription_indexName,
    amazonOpenSearchServerlessDestinationDescription_processingConfiguration,
    amazonOpenSearchServerlessDestinationDescription_retryOptions,
    amazonOpenSearchServerlessDestinationDescription_roleARN,
    amazonOpenSearchServerlessDestinationDescription_s3BackupMode,
    amazonOpenSearchServerlessDestinationDescription_s3DestinationDescription,
    amazonOpenSearchServerlessDestinationDescription_vpcConfigurationDescription,

    -- * AmazonOpenSearchServerlessDestinationUpdate
    AmazonOpenSearchServerlessDestinationUpdate (..),
    newAmazonOpenSearchServerlessDestinationUpdate,
    amazonOpenSearchServerlessDestinationUpdate_bufferingHints,
    amazonOpenSearchServerlessDestinationUpdate_cloudWatchLoggingOptions,
    amazonOpenSearchServerlessDestinationUpdate_collectionEndpoint,
    amazonOpenSearchServerlessDestinationUpdate_indexName,
    amazonOpenSearchServerlessDestinationUpdate_processingConfiguration,
    amazonOpenSearchServerlessDestinationUpdate_retryOptions,
    amazonOpenSearchServerlessDestinationUpdate_roleARN,
    amazonOpenSearchServerlessDestinationUpdate_s3Update,

    -- * AmazonOpenSearchServerlessRetryOptions
    AmazonOpenSearchServerlessRetryOptions (..),
    newAmazonOpenSearchServerlessRetryOptions,
    amazonOpenSearchServerlessRetryOptions_durationInSeconds,

    -- * AmazonopensearchserviceBufferingHints
    AmazonopensearchserviceBufferingHints (..),
    newAmazonopensearchserviceBufferingHints,
    amazonopensearchserviceBufferingHints_intervalInSeconds,
    amazonopensearchserviceBufferingHints_sizeInMBs,

    -- * AmazonopensearchserviceDestinationConfiguration
    AmazonopensearchserviceDestinationConfiguration (..),
    newAmazonopensearchserviceDestinationConfiguration,
    amazonopensearchserviceDestinationConfiguration_bufferingHints,
    amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationConfiguration_clusterEndpoint,
    amazonopensearchserviceDestinationConfiguration_domainARN,
    amazonopensearchserviceDestinationConfiguration_indexRotationPeriod,
    amazonopensearchserviceDestinationConfiguration_processingConfiguration,
    amazonopensearchserviceDestinationConfiguration_retryOptions,
    amazonopensearchserviceDestinationConfiguration_s3BackupMode,
    amazonopensearchserviceDestinationConfiguration_typeName,
    amazonopensearchserviceDestinationConfiguration_vpcConfiguration,
    amazonopensearchserviceDestinationConfiguration_roleARN,
    amazonopensearchserviceDestinationConfiguration_indexName,
    amazonopensearchserviceDestinationConfiguration_s3Configuration,

    -- * AmazonopensearchserviceDestinationDescription
    AmazonopensearchserviceDestinationDescription (..),
    newAmazonopensearchserviceDestinationDescription,
    amazonopensearchserviceDestinationDescription_bufferingHints,
    amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationDescription_clusterEndpoint,
    amazonopensearchserviceDestinationDescription_domainARN,
    amazonopensearchserviceDestinationDescription_indexName,
    amazonopensearchserviceDestinationDescription_indexRotationPeriod,
    amazonopensearchserviceDestinationDescription_processingConfiguration,
    amazonopensearchserviceDestinationDescription_retryOptions,
    amazonopensearchserviceDestinationDescription_roleARN,
    amazonopensearchserviceDestinationDescription_s3BackupMode,
    amazonopensearchserviceDestinationDescription_s3DestinationDescription,
    amazonopensearchserviceDestinationDescription_typeName,
    amazonopensearchserviceDestinationDescription_vpcConfigurationDescription,

    -- * AmazonopensearchserviceDestinationUpdate
    AmazonopensearchserviceDestinationUpdate (..),
    newAmazonopensearchserviceDestinationUpdate,
    amazonopensearchserviceDestinationUpdate_bufferingHints,
    amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationUpdate_clusterEndpoint,
    amazonopensearchserviceDestinationUpdate_domainARN,
    amazonopensearchserviceDestinationUpdate_indexName,
    amazonopensearchserviceDestinationUpdate_indexRotationPeriod,
    amazonopensearchserviceDestinationUpdate_processingConfiguration,
    amazonopensearchserviceDestinationUpdate_retryOptions,
    amazonopensearchserviceDestinationUpdate_roleARN,
    amazonopensearchserviceDestinationUpdate_s3Update,
    amazonopensearchserviceDestinationUpdate_typeName,

    -- * AmazonopensearchserviceRetryOptions
    AmazonopensearchserviceRetryOptions (..),
    newAmazonopensearchserviceRetryOptions,
    amazonopensearchserviceRetryOptions_durationInSeconds,

    -- * BufferingHints
    BufferingHints (..),
    newBufferingHints,
    bufferingHints_intervalInSeconds,
    bufferingHints_sizeInMBs,

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
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_inputFormatConfiguration,
    dataFormatConversionConfiguration_outputFormatConfiguration,
    dataFormatConversionConfiguration_schemaConfiguration,

    -- * DeliveryStreamDescription
    DeliveryStreamDescription (..),
    newDeliveryStreamDescription,
    deliveryStreamDescription_createTimestamp,
    deliveryStreamDescription_deliveryStreamEncryptionConfiguration,
    deliveryStreamDescription_failureDescription,
    deliveryStreamDescription_lastUpdateTimestamp,
    deliveryStreamDescription_source,
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
    deliveryStreamEncryptionConfiguration_failureDescription,
    deliveryStreamEncryptionConfiguration_keyARN,
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_status,

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
    destinationDescription_amazonOpenSearchServerlessDestinationDescription,
    destinationDescription_amazonopensearchserviceDestinationDescription,
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_splunkDestinationDescription,
    destinationDescription_destinationId,

    -- * DynamicPartitioningConfiguration
    DynamicPartitioningConfiguration (..),
    newDynamicPartitioningConfiguration,
    dynamicPartitioningConfiguration_enabled,
    dynamicPartitioningConfiguration_retryOptions,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints (..),
    newElasticsearchBufferingHints,
    elasticsearchBufferingHints_intervalInSeconds,
    elasticsearchBufferingHints_sizeInMBs,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (..),
    newElasticsearchDestinationConfiguration,
    elasticsearchDestinationConfiguration_bufferingHints,
    elasticsearchDestinationConfiguration_cloudWatchLoggingOptions,
    elasticsearchDestinationConfiguration_clusterEndpoint,
    elasticsearchDestinationConfiguration_domainARN,
    elasticsearchDestinationConfiguration_indexRotationPeriod,
    elasticsearchDestinationConfiguration_processingConfiguration,
    elasticsearchDestinationConfiguration_retryOptions,
    elasticsearchDestinationConfiguration_s3BackupMode,
    elasticsearchDestinationConfiguration_typeName,
    elasticsearchDestinationConfiguration_vpcConfiguration,
    elasticsearchDestinationConfiguration_roleARN,
    elasticsearchDestinationConfiguration_indexName,
    elasticsearchDestinationConfiguration_s3Configuration,

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (..),
    newElasticsearchDestinationDescription,
    elasticsearchDestinationDescription_bufferingHints,
    elasticsearchDestinationDescription_cloudWatchLoggingOptions,
    elasticsearchDestinationDescription_clusterEndpoint,
    elasticsearchDestinationDescription_domainARN,
    elasticsearchDestinationDescription_indexName,
    elasticsearchDestinationDescription_indexRotationPeriod,
    elasticsearchDestinationDescription_processingConfiguration,
    elasticsearchDestinationDescription_retryOptions,
    elasticsearchDestinationDescription_roleARN,
    elasticsearchDestinationDescription_s3BackupMode,
    elasticsearchDestinationDescription_s3DestinationDescription,
    elasticsearchDestinationDescription_typeName,
    elasticsearchDestinationDescription_vpcConfigurationDescription,

    -- * ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (..),
    newElasticsearchDestinationUpdate,
    elasticsearchDestinationUpdate_bufferingHints,
    elasticsearchDestinationUpdate_cloudWatchLoggingOptions,
    elasticsearchDestinationUpdate_clusterEndpoint,
    elasticsearchDestinationUpdate_domainARN,
    elasticsearchDestinationUpdate_indexName,
    elasticsearchDestinationUpdate_indexRotationPeriod,
    elasticsearchDestinationUpdate_processingConfiguration,
    elasticsearchDestinationUpdate_retryOptions,
    elasticsearchDestinationUpdate_roleARN,
    elasticsearchDestinationUpdate_s3Update,
    elasticsearchDestinationUpdate_typeName,

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
    extendedS3DestinationConfiguration_bufferingHints,
    extendedS3DestinationConfiguration_cloudWatchLoggingOptions,
    extendedS3DestinationConfiguration_compressionFormat,
    extendedS3DestinationConfiguration_dataFormatConversionConfiguration,
    extendedS3DestinationConfiguration_dynamicPartitioningConfiguration,
    extendedS3DestinationConfiguration_encryptionConfiguration,
    extendedS3DestinationConfiguration_errorOutputPrefix,
    extendedS3DestinationConfiguration_prefix,
    extendedS3DestinationConfiguration_processingConfiguration,
    extendedS3DestinationConfiguration_s3BackupConfiguration,
    extendedS3DestinationConfiguration_s3BackupMode,
    extendedS3DestinationConfiguration_roleARN,
    extendedS3DestinationConfiguration_bucketARN,

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (..),
    newExtendedS3DestinationDescription,
    extendedS3DestinationDescription_cloudWatchLoggingOptions,
    extendedS3DestinationDescription_dataFormatConversionConfiguration,
    extendedS3DestinationDescription_dynamicPartitioningConfiguration,
    extendedS3DestinationDescription_errorOutputPrefix,
    extendedS3DestinationDescription_prefix,
    extendedS3DestinationDescription_processingConfiguration,
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
    extendedS3DestinationUpdate_bucketARN,
    extendedS3DestinationUpdate_bufferingHints,
    extendedS3DestinationUpdate_cloudWatchLoggingOptions,
    extendedS3DestinationUpdate_compressionFormat,
    extendedS3DestinationUpdate_dataFormatConversionConfiguration,
    extendedS3DestinationUpdate_dynamicPartitioningConfiguration,
    extendedS3DestinationUpdate_encryptionConfiguration,
    extendedS3DestinationUpdate_errorOutputPrefix,
    extendedS3DestinationUpdate_prefix,
    extendedS3DestinationUpdate_processingConfiguration,
    extendedS3DestinationUpdate_roleARN,
    extendedS3DestinationUpdate_s3BackupMode,
    extendedS3DestinationUpdate_s3BackupUpdate,

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
    httpEndpointBufferingHints_intervalInSeconds,
    httpEndpointBufferingHints_sizeInMBs,

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
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- * HttpEndpointDestinationDescription
    HttpEndpointDestinationDescription (..),
    newHttpEndpointDestinationDescription,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_roleARN,
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_s3DestinationDescription,

    -- * HttpEndpointDestinationUpdate
    HttpEndpointDestinationUpdate (..),
    newHttpEndpointDestinationUpdate,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_retryOptions,
    httpEndpointDestinationUpdate_roleARN,
    httpEndpointDestinationUpdate_s3BackupMode,
    httpEndpointDestinationUpdate_s3Update,

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
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_columnToJsonKeyMappings,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,

    -- * OrcSerDe
    OrcSerDe (..),
    newOrcSerDe,
    orcSerDe_blockSizeBytes,
    orcSerDe_bloomFilterColumns,
    orcSerDe_bloomFilterFalsePositiveProbability,
    orcSerDe_compression,
    orcSerDe_dictionaryKeyThreshold,
    orcSerDe_enablePadding,
    orcSerDe_formatVersion,
    orcSerDe_paddingTolerance,
    orcSerDe_rowIndexStride,
    orcSerDe_stripeSizeBytes,

    -- * OutputFormatConfiguration
    OutputFormatConfiguration (..),
    newOutputFormatConfiguration,
    outputFormatConfiguration_serializer,

    -- * ParquetSerDe
    ParquetSerDe (..),
    newParquetSerDe,
    parquetSerDe_blockSizeBytes,
    parquetSerDe_compression,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_pageSizeBytes,
    parquetSerDe_writerVersion,

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
    putRecordBatchResponseEntry_errorCode,
    putRecordBatchResponseEntry_errorMessage,
    putRecordBatchResponseEntry_recordId,

    -- * Record
    Record (..),
    newRecord,
    record_data,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (..),
    newRedshiftDestinationConfiguration,
    redshiftDestinationConfiguration_cloudWatchLoggingOptions,
    redshiftDestinationConfiguration_processingConfiguration,
    redshiftDestinationConfiguration_retryOptions,
    redshiftDestinationConfiguration_s3BackupConfiguration,
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
    redshiftDestinationDescription_cloudWatchLoggingOptions,
    redshiftDestinationDescription_processingConfiguration,
    redshiftDestinationDescription_retryOptions,
    redshiftDestinationDescription_s3BackupDescription,
    redshiftDestinationDescription_s3BackupMode,
    redshiftDestinationDescription_roleARN,
    redshiftDestinationDescription_clusterJDBCURL,
    redshiftDestinationDescription_copyCommand,
    redshiftDestinationDescription_username,
    redshiftDestinationDescription_s3DestinationDescription,

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate (..),
    newRedshiftDestinationUpdate,
    redshiftDestinationUpdate_cloudWatchLoggingOptions,
    redshiftDestinationUpdate_clusterJDBCURL,
    redshiftDestinationUpdate_copyCommand,
    redshiftDestinationUpdate_password,
    redshiftDestinationUpdate_processingConfiguration,
    redshiftDestinationUpdate_retryOptions,
    redshiftDestinationUpdate_roleARN,
    redshiftDestinationUpdate_s3BackupMode,
    redshiftDestinationUpdate_s3BackupUpdate,
    redshiftDestinationUpdate_s3Update,
    redshiftDestinationUpdate_username,

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
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- * S3DestinationDescription
    S3DestinationDescription (..),
    newS3DestinationDescription,
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_prefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- * S3DestinationUpdate
    S3DestinationUpdate (..),
    newS3DestinationUpdate,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_compressionFormat,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_errorOutputPrefix,
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_roleARN,

    -- * SchemaConfiguration
    SchemaConfiguration (..),
    newSchemaConfiguration,
    schemaConfiguration_catalogId,
    schemaConfiguration_databaseName,
    schemaConfiguration_region,
    schemaConfiguration_roleARN,
    schemaConfiguration_tableName,
    schemaConfiguration_versionId,

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
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- * SplunkDestinationDescription
    SplunkDestinationDescription (..),
    newSplunkDestinationDescription,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_processingConfiguration,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_s3DestinationDescription,

    -- * SplunkDestinationUpdate
    SplunkDestinationUpdate (..),
    newSplunkDestinationUpdate,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_hECToken,
    splunkDestinationUpdate_processingConfiguration,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_s3Update,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessBufferingHints
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationConfiguration
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationDescription
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationUpdate
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessS3BackupMode
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationDescription
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationUpdate
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CompressionFormat
import Amazonka.Firehose.Types.ContentEncoding
import Amazonka.Firehose.Types.CopyCommand
import Amazonka.Firehose.Types.DataFormatConversionConfiguration
import Amazonka.Firehose.Types.DeliveryStreamDescription
import Amazonka.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Amazonka.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Amazonka.Firehose.Types.DeliveryStreamEncryptionStatus
import Amazonka.Firehose.Types.DeliveryStreamFailureType
import Amazonka.Firehose.Types.DeliveryStreamStatus
import Amazonka.Firehose.Types.DeliveryStreamType
import Amazonka.Firehose.Types.Deserializer
import Amazonka.Firehose.Types.DestinationDescription
import Amazonka.Firehose.Types.DynamicPartitioningConfiguration
import Amazonka.Firehose.Types.ElasticsearchBufferingHints
import Amazonka.Firehose.Types.ElasticsearchDestinationConfiguration
import Amazonka.Firehose.Types.ElasticsearchDestinationDescription
import Amazonka.Firehose.Types.ElasticsearchDestinationUpdate
import Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
import Amazonka.Firehose.Types.ElasticsearchRetryOptions
import Amazonka.Firehose.Types.ElasticsearchS3BackupMode
import Amazonka.Firehose.Types.EncryptionConfiguration
import Amazonka.Firehose.Types.ExtendedS3DestinationConfiguration
import Amazonka.Firehose.Types.ExtendedS3DestinationDescription
import Amazonka.Firehose.Types.ExtendedS3DestinationUpdate
import Amazonka.Firehose.Types.FailureDescription
import Amazonka.Firehose.Types.HECEndpointType
import Amazonka.Firehose.Types.HiveJsonSerDe
import Amazonka.Firehose.Types.HttpEndpointBufferingHints
import Amazonka.Firehose.Types.HttpEndpointCommonAttribute
import Amazonka.Firehose.Types.HttpEndpointConfiguration
import Amazonka.Firehose.Types.HttpEndpointDescription
import Amazonka.Firehose.Types.HttpEndpointDestinationConfiguration
import Amazonka.Firehose.Types.HttpEndpointDestinationDescription
import Amazonka.Firehose.Types.HttpEndpointDestinationUpdate
import Amazonka.Firehose.Types.HttpEndpointRequestConfiguration
import Amazonka.Firehose.Types.HttpEndpointRetryOptions
import Amazonka.Firehose.Types.HttpEndpointS3BackupMode
import Amazonka.Firehose.Types.InputFormatConfiguration
import Amazonka.Firehose.Types.KMSEncryptionConfig
import Amazonka.Firehose.Types.KeyType
import Amazonka.Firehose.Types.KinesisStreamSourceConfiguration
import Amazonka.Firehose.Types.KinesisStreamSourceDescription
import Amazonka.Firehose.Types.NoEncryptionConfig
import Amazonka.Firehose.Types.OpenXJsonSerDe
import Amazonka.Firehose.Types.OrcCompression
import Amazonka.Firehose.Types.OrcFormatVersion
import Amazonka.Firehose.Types.OrcSerDe
import Amazonka.Firehose.Types.OutputFormatConfiguration
import Amazonka.Firehose.Types.ParquetCompression
import Amazonka.Firehose.Types.ParquetSerDe
import Amazonka.Firehose.Types.ParquetWriterVersion
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.Processor
import Amazonka.Firehose.Types.ProcessorParameter
import Amazonka.Firehose.Types.ProcessorParameterName
import Amazonka.Firehose.Types.ProcessorType
import Amazonka.Firehose.Types.PutRecordBatchResponseEntry
import Amazonka.Firehose.Types.Record
import Amazonka.Firehose.Types.RedshiftDestinationConfiguration
import Amazonka.Firehose.Types.RedshiftDestinationDescription
import Amazonka.Firehose.Types.RedshiftDestinationUpdate
import Amazonka.Firehose.Types.RedshiftRetryOptions
import Amazonka.Firehose.Types.RedshiftS3BackupMode
import Amazonka.Firehose.Types.RetryOptions
import Amazonka.Firehose.Types.S3BackupMode
import Amazonka.Firehose.Types.S3DestinationConfiguration
import Amazonka.Firehose.Types.S3DestinationDescription
import Amazonka.Firehose.Types.S3DestinationUpdate
import Amazonka.Firehose.Types.SchemaConfiguration
import Amazonka.Firehose.Types.Serializer
import Amazonka.Firehose.Types.SourceDescription
import Amazonka.Firehose.Types.SplunkDestinationConfiguration
import Amazonka.Firehose.Types.SplunkDestinationDescription
import Amazonka.Firehose.Types.SplunkDestinationUpdate
import Amazonka.Firehose.Types.SplunkRetryOptions
import Amazonka.Firehose.Types.SplunkS3BackupMode
import Amazonka.Firehose.Types.Tag
import Amazonka.Firehose.Types.VpcConfiguration
import Amazonka.Firehose.Types.VpcConfigurationDescription
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-08-04@ of the Amazon Kinesis Firehose SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Firehose",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "firehose",
      Core.signingName = "firehose",
      Core.version = "2015-08-04",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Firehose",
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

-- | Another modification has already happened. Fetch @VersionId@ again and
-- use it to update the destination.
_ConcurrentModificationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified input parameter has a value that is not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | Kinesis Data Firehose throws this exception when an attempt to put
-- records or to start or stop delivery stream encryption fails. This
-- happens when the KMS service throws one of the following exception
-- types: @AccessDeniedException@, @InvalidStateException@,
-- @DisabledException@, or @NotFoundException@.
_InvalidKMSResourceException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidKMSResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidKMSResourceException"

-- | You have already reached the limit for a requested resource.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource is already in use and not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The service is unavailable. Back off and retry the operation. If you
-- continue to see the exception, throughput limits for the delivery stream
-- may have been exceeded. For more information about limits and how to
-- request an increase, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits>.
_ServiceUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
