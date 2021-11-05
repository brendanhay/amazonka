{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Firehose.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Lens
  ( -- * Operations

    -- ** PutRecord
    putRecord_deliveryStreamName,
    putRecord_record,
    putRecordResponse_encrypted,
    putRecordResponse_httpStatus,
    putRecordResponse_recordId,

    -- ** StopDeliveryStreamEncryption
    stopDeliveryStreamEncryption_deliveryStreamName,
    stopDeliveryStreamEncryptionResponse_httpStatus,

    -- ** TagDeliveryStream
    tagDeliveryStream_deliveryStreamName,
    tagDeliveryStream_tags,
    tagDeliveryStreamResponse_httpStatus,

    -- ** UpdateDestination
    updateDestination_amazonopensearchserviceDestinationUpdate,
    updateDestination_splunkDestinationUpdate,
    updateDestination_s3DestinationUpdate,
    updateDestination_redshiftDestinationUpdate,
    updateDestination_elasticsearchDestinationUpdate,
    updateDestination_extendedS3DestinationUpdate,
    updateDestination_httpEndpointDestinationUpdate,
    updateDestination_deliveryStreamName,
    updateDestination_currentDeliveryStreamVersionId,
    updateDestination_destinationId,
    updateDestinationResponse_httpStatus,

    -- ** PutRecordBatch
    putRecordBatch_deliveryStreamName,
    putRecordBatch_records,
    putRecordBatchResponse_encrypted,
    putRecordBatchResponse_httpStatus,
    putRecordBatchResponse_failedPutCount,
    putRecordBatchResponse_requestResponses,

    -- ** UntagDeliveryStream
    untagDeliveryStream_deliveryStreamName,
    untagDeliveryStream_tagKeys,
    untagDeliveryStreamResponse_httpStatus,

    -- ** CreateDeliveryStream
    createDeliveryStream_s3DestinationConfiguration,
    createDeliveryStream_redshiftDestinationConfiguration,
    createDeliveryStream_elasticsearchDestinationConfiguration,
    createDeliveryStream_extendedS3DestinationConfiguration,
    createDeliveryStream_kinesisStreamSourceConfiguration,
    createDeliveryStream_httpEndpointDestinationConfiguration,
    createDeliveryStream_amazonopensearchserviceDestinationConfiguration,
    createDeliveryStream_deliveryStreamType,
    createDeliveryStream_splunkDestinationConfiguration,
    createDeliveryStream_tags,
    createDeliveryStream_deliveryStreamEncryptionConfigurationInput,
    createDeliveryStream_deliveryStreamName,
    createDeliveryStreamResponse_deliveryStreamARN,
    createDeliveryStreamResponse_httpStatus,

    -- ** StartDeliveryStreamEncryption
    startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput,
    startDeliveryStreamEncryption_deliveryStreamName,
    startDeliveryStreamEncryptionResponse_httpStatus,

    -- ** DescribeDeliveryStream
    describeDeliveryStream_exclusiveStartDestinationId,
    describeDeliveryStream_limit,
    describeDeliveryStream_deliveryStreamName,
    describeDeliveryStreamResponse_httpStatus,
    describeDeliveryStreamResponse_deliveryStreamDescription,

    -- ** ListTagsForDeliveryStream
    listTagsForDeliveryStream_limit,
    listTagsForDeliveryStream_exclusiveStartTagKey,
    listTagsForDeliveryStream_deliveryStreamName,
    listTagsForDeliveryStreamResponse_httpStatus,
    listTagsForDeliveryStreamResponse_tags,
    listTagsForDeliveryStreamResponse_hasMoreTags,

    -- ** ListDeliveryStreams
    listDeliveryStreams_limit,
    listDeliveryStreams_deliveryStreamType,
    listDeliveryStreams_exclusiveStartDeliveryStreamName,
    listDeliveryStreamsResponse_httpStatus,
    listDeliveryStreamsResponse_deliveryStreamNames,
    listDeliveryStreamsResponse_hasMoreDeliveryStreams,

    -- ** DeleteDeliveryStream
    deleteDeliveryStream_allowForceDelete,
    deleteDeliveryStream_deliveryStreamName,
    deleteDeliveryStreamResponse_httpStatus,

    -- * Types

    -- ** AmazonopensearchserviceBufferingHints
    amazonopensearchserviceBufferingHints_sizeInMBs,
    amazonopensearchserviceBufferingHints_intervalInSeconds,

    -- ** AmazonopensearchserviceDestinationConfiguration
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

    -- ** AmazonopensearchserviceDestinationDescription
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

    -- ** AmazonopensearchserviceDestinationUpdate
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

    -- ** AmazonopensearchserviceRetryOptions
    amazonopensearchserviceRetryOptions_durationInSeconds,

    -- ** BufferingHints
    bufferingHints_sizeInMBs,
    bufferingHints_intervalInSeconds,

    -- ** CloudWatchLoggingOptions
    cloudWatchLoggingOptions_enabled,
    cloudWatchLoggingOptions_logGroupName,
    cloudWatchLoggingOptions_logStreamName,

    -- ** CopyCommand
    copyCommand_copyOptions,
    copyCommand_dataTableColumns,
    copyCommand_dataTableName,

    -- ** DataFormatConversionConfiguration
    dataFormatConversionConfiguration_outputFormatConfiguration,
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_schemaConfiguration,
    dataFormatConversionConfiguration_inputFormatConfiguration,

    -- ** DeliveryStreamDescription
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

    -- ** DeliveryStreamEncryptionConfiguration
    deliveryStreamEncryptionConfiguration_status,
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_keyARN,
    deliveryStreamEncryptionConfiguration_failureDescription,

    -- ** DeliveryStreamEncryptionConfigurationInput
    deliveryStreamEncryptionConfigurationInput_keyARN,
    deliveryStreamEncryptionConfigurationInput_keyType,

    -- ** Deserializer
    deserializer_openXJsonSerDe,
    deserializer_hiveJsonSerDe,

    -- ** DestinationDescription
    destinationDescription_splunkDestinationDescription,
    destinationDescription_amazonopensearchserviceDestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_destinationId,

    -- ** DynamicPartitioningConfiguration
    dynamicPartitioningConfiguration_enabled,
    dynamicPartitioningConfiguration_retryOptions,

    -- ** ElasticsearchBufferingHints
    elasticsearchBufferingHints_sizeInMBs,
    elasticsearchBufferingHints_intervalInSeconds,

    -- ** ElasticsearchDestinationConfiguration
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

    -- ** ElasticsearchDestinationDescription
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

    -- ** ElasticsearchDestinationUpdate
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

    -- ** ElasticsearchRetryOptions
    elasticsearchRetryOptions_durationInSeconds,

    -- ** EncryptionConfiguration
    encryptionConfiguration_noEncryptionConfig,
    encryptionConfiguration_kmsEncryptionConfig,

    -- ** ExtendedS3DestinationConfiguration
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

    -- ** ExtendedS3DestinationDescription
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

    -- ** ExtendedS3DestinationUpdate
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

    -- ** FailureDescription
    failureDescription_type,
    failureDescription_details,

    -- ** HiveJsonSerDe
    hiveJsonSerDe_timestampFormats,

    -- ** HttpEndpointBufferingHints
    httpEndpointBufferingHints_sizeInMBs,
    httpEndpointBufferingHints_intervalInSeconds,

    -- ** HttpEndpointCommonAttribute
    httpEndpointCommonAttribute_attributeName,
    httpEndpointCommonAttribute_attributeValue,

    -- ** HttpEndpointConfiguration
    httpEndpointConfiguration_name,
    httpEndpointConfiguration_accessKey,
    httpEndpointConfiguration_url,

    -- ** HttpEndpointDescription
    httpEndpointDescription_url,
    httpEndpointDescription_name,

    -- ** HttpEndpointDestinationConfiguration
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- ** HttpEndpointDestinationDescription
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_s3DestinationDescription,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_roleARN,

    -- ** HttpEndpointDestinationUpdate
    httpEndpointDestinationUpdate_s3BackupMode,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_s3Update,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_retryOptions,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_roleARN,

    -- ** HttpEndpointRequestConfiguration
    httpEndpointRequestConfiguration_commonAttributes,
    httpEndpointRequestConfiguration_contentEncoding,

    -- ** HttpEndpointRetryOptions
    httpEndpointRetryOptions_durationInSeconds,

    -- ** InputFormatConfiguration
    inputFormatConfiguration_deserializer,

    -- ** KMSEncryptionConfig
    kmsEncryptionConfig_aWSKMSKeyARN,

    -- ** KinesisStreamSourceConfiguration
    kinesisStreamSourceConfiguration_kinesisStreamARN,
    kinesisStreamSourceConfiguration_roleARN,

    -- ** KinesisStreamSourceDescription
    kinesisStreamSourceDescription_deliveryStartTimestamp,
    kinesisStreamSourceDescription_kinesisStreamARN,
    kinesisStreamSourceDescription_roleARN,

    -- ** OpenXJsonSerDe
    openXJsonSerDe_columnToJsonKeyMappings,
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,

    -- ** OrcSerDe
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

    -- ** OutputFormatConfiguration
    outputFormatConfiguration_serializer,

    -- ** ParquetSerDe
    parquetSerDe_writerVersion,
    parquetSerDe_compression,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_pageSizeBytes,
    parquetSerDe_blockSizeBytes,

    -- ** ProcessingConfiguration
    processingConfiguration_enabled,
    processingConfiguration_processors,

    -- ** Processor
    processor_parameters,
    processor_type,

    -- ** ProcessorParameter
    processorParameter_parameterName,
    processorParameter_parameterValue,

    -- ** PutRecordBatchResponseEntry
    putRecordBatchResponseEntry_recordId,
    putRecordBatchResponseEntry_errorCode,
    putRecordBatchResponseEntry_errorMessage,

    -- ** Record
    record_data,

    -- ** RedshiftDestinationConfiguration
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

    -- ** RedshiftDestinationDescription
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

    -- ** RedshiftDestinationUpdate
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

    -- ** RedshiftRetryOptions
    redshiftRetryOptions_durationInSeconds,

    -- ** RetryOptions
    retryOptions_durationInSeconds,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- ** S3DestinationDescription
    s3DestinationDescription_prefix,
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- ** S3DestinationUpdate
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_errorOutputPrefix,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_compressionFormat,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_roleARN,

    -- ** SchemaConfiguration
    schemaConfiguration_versionId,
    schemaConfiguration_catalogId,
    schemaConfiguration_databaseName,
    schemaConfiguration_region,
    schemaConfiguration_tableName,
    schemaConfiguration_roleARN,

    -- ** Serializer
    serializer_orcSerDe,
    serializer_parquetSerDe,

    -- ** SourceDescription
    sourceDescription_kinesisStreamSourceDescription,

    -- ** SplunkDestinationConfiguration
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- ** SplunkDestinationDescription
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_s3DestinationDescription,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_processingConfiguration,

    -- ** SplunkDestinationUpdate
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_hECToken,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationUpdate_s3Update,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_processingConfiguration,

    -- ** SplunkRetryOptions
    splunkRetryOptions_durationInSeconds,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VpcConfiguration
    vpcConfiguration_subnetIds,
    vpcConfiguration_roleARN,
    vpcConfiguration_securityGroupIds,

    -- ** VpcConfigurationDescription
    vpcConfigurationDescription_subnetIds,
    vpcConfigurationDescription_roleARN,
    vpcConfigurationDescription_securityGroupIds,
    vpcConfigurationDescription_vpcId,
  )
where

import Amazonka.Firehose.CreateDeliveryStream
import Amazonka.Firehose.DeleteDeliveryStream
import Amazonka.Firehose.DescribeDeliveryStream
import Amazonka.Firehose.ListDeliveryStreams
import Amazonka.Firehose.ListTagsForDeliveryStream
import Amazonka.Firehose.PutRecord
import Amazonka.Firehose.PutRecordBatch
import Amazonka.Firehose.StartDeliveryStreamEncryption
import Amazonka.Firehose.StopDeliveryStreamEncryption
import Amazonka.Firehose.TagDeliveryStream
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationDescription
import Amazonka.Firehose.Types.AmazonopensearchserviceDestinationUpdate
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.BufferingHints
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CopyCommand
import Amazonka.Firehose.Types.DataFormatConversionConfiguration
import Amazonka.Firehose.Types.DeliveryStreamDescription
import Amazonka.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Amazonka.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Amazonka.Firehose.Types.Deserializer
import Amazonka.Firehose.Types.DestinationDescription
import Amazonka.Firehose.Types.DynamicPartitioningConfiguration
import Amazonka.Firehose.Types.ElasticsearchBufferingHints
import Amazonka.Firehose.Types.ElasticsearchDestinationConfiguration
import Amazonka.Firehose.Types.ElasticsearchDestinationDescription
import Amazonka.Firehose.Types.ElasticsearchDestinationUpdate
import Amazonka.Firehose.Types.ElasticsearchRetryOptions
import Amazonka.Firehose.Types.EncryptionConfiguration
import Amazonka.Firehose.Types.ExtendedS3DestinationConfiguration
import Amazonka.Firehose.Types.ExtendedS3DestinationDescription
import Amazonka.Firehose.Types.ExtendedS3DestinationUpdate
import Amazonka.Firehose.Types.FailureDescription
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
import Amazonka.Firehose.Types.InputFormatConfiguration
import Amazonka.Firehose.Types.KMSEncryptionConfig
import Amazonka.Firehose.Types.KinesisStreamSourceConfiguration
import Amazonka.Firehose.Types.KinesisStreamSourceDescription
import Amazonka.Firehose.Types.OpenXJsonSerDe
import Amazonka.Firehose.Types.OrcSerDe
import Amazonka.Firehose.Types.OutputFormatConfiguration
import Amazonka.Firehose.Types.ParquetSerDe
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.Processor
import Amazonka.Firehose.Types.ProcessorParameter
import Amazonka.Firehose.Types.PutRecordBatchResponseEntry
import Amazonka.Firehose.Types.Record
import Amazonka.Firehose.Types.RedshiftDestinationConfiguration
import Amazonka.Firehose.Types.RedshiftDestinationDescription
import Amazonka.Firehose.Types.RedshiftDestinationUpdate
import Amazonka.Firehose.Types.RedshiftRetryOptions
import Amazonka.Firehose.Types.RetryOptions
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
import Amazonka.Firehose.Types.Tag
import Amazonka.Firehose.Types.VpcConfiguration
import Amazonka.Firehose.Types.VpcConfigurationDescription
import Amazonka.Firehose.UntagDeliveryStream
import Amazonka.Firehose.UpdateDestination
