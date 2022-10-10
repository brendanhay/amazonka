{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Firehose.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Lens
  ( -- * Operations

    -- ** CreateDeliveryStream
    createDeliveryStream_tags,
    createDeliveryStream_s3DestinationConfiguration,
    createDeliveryStream_httpEndpointDestinationConfiguration,
    createDeliveryStream_deliveryStreamType,
    createDeliveryStream_splunkDestinationConfiguration,
    createDeliveryStream_elasticsearchDestinationConfiguration,
    createDeliveryStream_redshiftDestinationConfiguration,
    createDeliveryStream_deliveryStreamEncryptionConfigurationInput,
    createDeliveryStream_extendedS3DestinationConfiguration,
    createDeliveryStream_kinesisStreamSourceConfiguration,
    createDeliveryStream_amazonopensearchserviceDestinationConfiguration,
    createDeliveryStream_deliveryStreamName,
    createDeliveryStreamResponse_deliveryStreamARN,
    createDeliveryStreamResponse_httpStatus,

    -- ** DeleteDeliveryStream
    deleteDeliveryStream_allowForceDelete,
    deleteDeliveryStream_deliveryStreamName,
    deleteDeliveryStreamResponse_httpStatus,

    -- ** DescribeDeliveryStream
    describeDeliveryStream_limit,
    describeDeliveryStream_exclusiveStartDestinationId,
    describeDeliveryStream_deliveryStreamName,
    describeDeliveryStreamResponse_httpStatus,
    describeDeliveryStreamResponse_deliveryStreamDescription,

    -- ** ListDeliveryStreams
    listDeliveryStreams_deliveryStreamType,
    listDeliveryStreams_limit,
    listDeliveryStreams_exclusiveStartDeliveryStreamName,
    listDeliveryStreamsResponse_httpStatus,
    listDeliveryStreamsResponse_deliveryStreamNames,
    listDeliveryStreamsResponse_hasMoreDeliveryStreams,

    -- ** ListTagsForDeliveryStream
    listTagsForDeliveryStream_limit,
    listTagsForDeliveryStream_exclusiveStartTagKey,
    listTagsForDeliveryStream_deliveryStreamName,
    listTagsForDeliveryStreamResponse_httpStatus,
    listTagsForDeliveryStreamResponse_tags,
    listTagsForDeliveryStreamResponse_hasMoreTags,

    -- ** PutRecord
    putRecord_deliveryStreamName,
    putRecord_record,
    putRecordResponse_encrypted,
    putRecordResponse_httpStatus,
    putRecordResponse_recordId,

    -- ** PutRecordBatch
    putRecordBatch_deliveryStreamName,
    putRecordBatch_records,
    putRecordBatchResponse_encrypted,
    putRecordBatchResponse_httpStatus,
    putRecordBatchResponse_failedPutCount,
    putRecordBatchResponse_requestResponses,

    -- ** StartDeliveryStreamEncryption
    startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput,
    startDeliveryStreamEncryption_deliveryStreamName,
    startDeliveryStreamEncryptionResponse_httpStatus,

    -- ** StopDeliveryStreamEncryption
    stopDeliveryStreamEncryption_deliveryStreamName,
    stopDeliveryStreamEncryptionResponse_httpStatus,

    -- ** TagDeliveryStream
    tagDeliveryStream_deliveryStreamName,
    tagDeliveryStream_tags,
    tagDeliveryStreamResponse_httpStatus,

    -- ** UntagDeliveryStream
    untagDeliveryStream_deliveryStreamName,
    untagDeliveryStream_tagKeys,
    untagDeliveryStreamResponse_httpStatus,

    -- ** UpdateDestination
    updateDestination_amazonopensearchserviceDestinationUpdate,
    updateDestination_splunkDestinationUpdate,
    updateDestination_extendedS3DestinationUpdate,
    updateDestination_s3DestinationUpdate,
    updateDestination_elasticsearchDestinationUpdate,
    updateDestination_httpEndpointDestinationUpdate,
    updateDestination_redshiftDestinationUpdate,
    updateDestination_deliveryStreamName,
    updateDestination_currentDeliveryStreamVersionId,
    updateDestination_destinationId,
    updateDestinationResponse_httpStatus,

    -- * Types

    -- ** AmazonopensearchserviceBufferingHints
    amazonopensearchserviceBufferingHints_sizeInMBs,
    amazonopensearchserviceBufferingHints_intervalInSeconds,

    -- ** AmazonopensearchserviceDestinationConfiguration
    amazonopensearchserviceDestinationConfiguration_vpcConfiguration,
    amazonopensearchserviceDestinationConfiguration_processingConfiguration,
    amazonopensearchserviceDestinationConfiguration_bufferingHints,
    amazonopensearchserviceDestinationConfiguration_clusterEndpoint,
    amazonopensearchserviceDestinationConfiguration_domainARN,
    amazonopensearchserviceDestinationConfiguration_typeName,
    amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationConfiguration_s3BackupMode,
    amazonopensearchserviceDestinationConfiguration_indexRotationPeriod,
    amazonopensearchserviceDestinationConfiguration_retryOptions,
    amazonopensearchserviceDestinationConfiguration_roleARN,
    amazonopensearchserviceDestinationConfiguration_indexName,
    amazonopensearchserviceDestinationConfiguration_s3Configuration,

    -- ** AmazonopensearchserviceDestinationDescription
    amazonopensearchserviceDestinationDescription_processingConfiguration,
    amazonopensearchserviceDestinationDescription_roleARN,
    amazonopensearchserviceDestinationDescription_bufferingHints,
    amazonopensearchserviceDestinationDescription_clusterEndpoint,
    amazonopensearchserviceDestinationDescription_domainARN,
    amazonopensearchserviceDestinationDescription_typeName,
    amazonopensearchserviceDestinationDescription_indexName,
    amazonopensearchserviceDestinationDescription_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationDescription_s3BackupMode,
    amazonopensearchserviceDestinationDescription_vpcConfigurationDescription,
    amazonopensearchserviceDestinationDescription_indexRotationPeriod,
    amazonopensearchserviceDestinationDescription_retryOptions,
    amazonopensearchserviceDestinationDescription_s3DestinationDescription,

    -- ** AmazonopensearchserviceDestinationUpdate
    amazonopensearchserviceDestinationUpdate_processingConfiguration,
    amazonopensearchserviceDestinationUpdate_roleARN,
    amazonopensearchserviceDestinationUpdate_s3Update,
    amazonopensearchserviceDestinationUpdate_bufferingHints,
    amazonopensearchserviceDestinationUpdate_clusterEndpoint,
    amazonopensearchserviceDestinationUpdate_domainARN,
    amazonopensearchserviceDestinationUpdate_typeName,
    amazonopensearchserviceDestinationUpdate_indexName,
    amazonopensearchserviceDestinationUpdate_cloudWatchLoggingOptions,
    amazonopensearchserviceDestinationUpdate_indexRotationPeriod,
    amazonopensearchserviceDestinationUpdate_retryOptions,

    -- ** AmazonopensearchserviceRetryOptions
    amazonopensearchserviceRetryOptions_durationInSeconds,

    -- ** BufferingHints
    bufferingHints_sizeInMBs,
    bufferingHints_intervalInSeconds,

    -- ** CloudWatchLoggingOptions
    cloudWatchLoggingOptions_enabled,
    cloudWatchLoggingOptions_logStreamName,
    cloudWatchLoggingOptions_logGroupName,

    -- ** CopyCommand
    copyCommand_dataTableColumns,
    copyCommand_copyOptions,
    copyCommand_dataTableName,

    -- ** DataFormatConversionConfiguration
    dataFormatConversionConfiguration_inputFormatConfiguration,
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_schemaConfiguration,
    dataFormatConversionConfiguration_outputFormatConfiguration,

    -- ** DeliveryStreamDescription
    deliveryStreamDescription_failureDescription,
    deliveryStreamDescription_lastUpdateTimestamp,
    deliveryStreamDescription_deliveryStreamEncryptionConfiguration,
    deliveryStreamDescription_createTimestamp,
    deliveryStreamDescription_source,
    deliveryStreamDescription_deliveryStreamName,
    deliveryStreamDescription_deliveryStreamARN,
    deliveryStreamDescription_deliveryStreamStatus,
    deliveryStreamDescription_deliveryStreamType,
    deliveryStreamDescription_versionId,
    deliveryStreamDescription_destinations,
    deliveryStreamDescription_hasMoreDestinations,

    -- ** DeliveryStreamEncryptionConfiguration
    deliveryStreamEncryptionConfiguration_failureDescription,
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_status,
    deliveryStreamEncryptionConfiguration_keyARN,

    -- ** DeliveryStreamEncryptionConfigurationInput
    deliveryStreamEncryptionConfigurationInput_keyARN,
    deliveryStreamEncryptionConfigurationInput_keyType,

    -- ** Deserializer
    deserializer_hiveJsonSerDe,
    deserializer_openXJsonSerDe,

    -- ** DestinationDescription
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_splunkDestinationDescription,
    destinationDescription_amazonopensearchserviceDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_destinationId,

    -- ** DynamicPartitioningConfiguration
    dynamicPartitioningConfiguration_enabled,
    dynamicPartitioningConfiguration_retryOptions,

    -- ** ElasticsearchBufferingHints
    elasticsearchBufferingHints_sizeInMBs,
    elasticsearchBufferingHints_intervalInSeconds,

    -- ** ElasticsearchDestinationConfiguration
    elasticsearchDestinationConfiguration_vpcConfiguration,
    elasticsearchDestinationConfiguration_processingConfiguration,
    elasticsearchDestinationConfiguration_bufferingHints,
    elasticsearchDestinationConfiguration_clusterEndpoint,
    elasticsearchDestinationConfiguration_domainARN,
    elasticsearchDestinationConfiguration_typeName,
    elasticsearchDestinationConfiguration_cloudWatchLoggingOptions,
    elasticsearchDestinationConfiguration_s3BackupMode,
    elasticsearchDestinationConfiguration_indexRotationPeriod,
    elasticsearchDestinationConfiguration_retryOptions,
    elasticsearchDestinationConfiguration_roleARN,
    elasticsearchDestinationConfiguration_indexName,
    elasticsearchDestinationConfiguration_s3Configuration,

    -- ** ElasticsearchDestinationDescription
    elasticsearchDestinationDescription_processingConfiguration,
    elasticsearchDestinationDescription_roleARN,
    elasticsearchDestinationDescription_bufferingHints,
    elasticsearchDestinationDescription_clusterEndpoint,
    elasticsearchDestinationDescription_domainARN,
    elasticsearchDestinationDescription_typeName,
    elasticsearchDestinationDescription_indexName,
    elasticsearchDestinationDescription_cloudWatchLoggingOptions,
    elasticsearchDestinationDescription_s3BackupMode,
    elasticsearchDestinationDescription_vpcConfigurationDescription,
    elasticsearchDestinationDescription_indexRotationPeriod,
    elasticsearchDestinationDescription_retryOptions,
    elasticsearchDestinationDescription_s3DestinationDescription,

    -- ** ElasticsearchDestinationUpdate
    elasticsearchDestinationUpdate_processingConfiguration,
    elasticsearchDestinationUpdate_roleARN,
    elasticsearchDestinationUpdate_s3Update,
    elasticsearchDestinationUpdate_bufferingHints,
    elasticsearchDestinationUpdate_clusterEndpoint,
    elasticsearchDestinationUpdate_domainARN,
    elasticsearchDestinationUpdate_typeName,
    elasticsearchDestinationUpdate_indexName,
    elasticsearchDestinationUpdate_cloudWatchLoggingOptions,
    elasticsearchDestinationUpdate_indexRotationPeriod,
    elasticsearchDestinationUpdate_retryOptions,

    -- ** ElasticsearchRetryOptions
    elasticsearchRetryOptions_durationInSeconds,

    -- ** EncryptionConfiguration
    encryptionConfiguration_noEncryptionConfig,
    encryptionConfiguration_kmsEncryptionConfig,

    -- ** ExtendedS3DestinationConfiguration
    extendedS3DestinationConfiguration_s3BackupConfiguration,
    extendedS3DestinationConfiguration_processingConfiguration,
    extendedS3DestinationConfiguration_bufferingHints,
    extendedS3DestinationConfiguration_dataFormatConversionConfiguration,
    extendedS3DestinationConfiguration_cloudWatchLoggingOptions,
    extendedS3DestinationConfiguration_s3BackupMode,
    extendedS3DestinationConfiguration_encryptionConfiguration,
    extendedS3DestinationConfiguration_prefix,
    extendedS3DestinationConfiguration_compressionFormat,
    extendedS3DestinationConfiguration_dynamicPartitioningConfiguration,
    extendedS3DestinationConfiguration_errorOutputPrefix,
    extendedS3DestinationConfiguration_roleARN,
    extendedS3DestinationConfiguration_bucketARN,

    -- ** ExtendedS3DestinationDescription
    extendedS3DestinationDescription_processingConfiguration,
    extendedS3DestinationDescription_dataFormatConversionConfiguration,
    extendedS3DestinationDescription_s3BackupDescription,
    extendedS3DestinationDescription_cloudWatchLoggingOptions,
    extendedS3DestinationDescription_s3BackupMode,
    extendedS3DestinationDescription_prefix,
    extendedS3DestinationDescription_dynamicPartitioningConfiguration,
    extendedS3DestinationDescription_errorOutputPrefix,
    extendedS3DestinationDescription_roleARN,
    extendedS3DestinationDescription_bucketARN,
    extendedS3DestinationDescription_bufferingHints,
    extendedS3DestinationDescription_compressionFormat,
    extendedS3DestinationDescription_encryptionConfiguration,

    -- ** ExtendedS3DestinationUpdate
    extendedS3DestinationUpdate_processingConfiguration,
    extendedS3DestinationUpdate_roleARN,
    extendedS3DestinationUpdate_bufferingHints,
    extendedS3DestinationUpdate_dataFormatConversionConfiguration,
    extendedS3DestinationUpdate_s3BackupUpdate,
    extendedS3DestinationUpdate_cloudWatchLoggingOptions,
    extendedS3DestinationUpdate_bucketARN,
    extendedS3DestinationUpdate_s3BackupMode,
    extendedS3DestinationUpdate_encryptionConfiguration,
    extendedS3DestinationUpdate_prefix,
    extendedS3DestinationUpdate_compressionFormat,
    extendedS3DestinationUpdate_dynamicPartitioningConfiguration,
    extendedS3DestinationUpdate_errorOutputPrefix,

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
    httpEndpointDescription_name,
    httpEndpointDescription_url,

    -- ** HttpEndpointDestinationConfiguration
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- ** HttpEndpointDestinationDescription
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_roleARN,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_s3DestinationDescription,

    -- ** HttpEndpointDestinationUpdate
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_roleARN,
    httpEndpointDestinationUpdate_s3Update,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_s3BackupMode,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_retryOptions,

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
    kinesisStreamSourceDescription_roleARN,
    kinesisStreamSourceDescription_kinesisStreamARN,
    kinesisStreamSourceDescription_deliveryStartTimestamp,

    -- ** OpenXJsonSerDe
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,
    openXJsonSerDe_columnToJsonKeyMappings,

    -- ** OrcSerDe
    orcSerDe_formatVersion,
    orcSerDe_compression,
    orcSerDe_blockSizeBytes,
    orcSerDe_bloomFilterFalsePositiveProbability,
    orcSerDe_rowIndexStride,
    orcSerDe_enablePadding,
    orcSerDe_bloomFilterColumns,
    orcSerDe_paddingTolerance,
    orcSerDe_dictionaryKeyThreshold,
    orcSerDe_stripeSizeBytes,

    -- ** OutputFormatConfiguration
    outputFormatConfiguration_serializer,

    -- ** ParquetSerDe
    parquetSerDe_compression,
    parquetSerDe_blockSizeBytes,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_writerVersion,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_pageSizeBytes,

    -- ** ProcessingConfiguration
    processingConfiguration_processors,
    processingConfiguration_enabled,

    -- ** Processor
    processor_parameters,
    processor_type,

    -- ** ProcessorParameter
    processorParameter_parameterName,
    processorParameter_parameterValue,

    -- ** PutRecordBatchResponseEntry
    putRecordBatchResponseEntry_errorMessage,
    putRecordBatchResponseEntry_recordId,
    putRecordBatchResponseEntry_errorCode,

    -- ** Record
    record_data,

    -- ** RedshiftDestinationConfiguration
    redshiftDestinationConfiguration_s3BackupConfiguration,
    redshiftDestinationConfiguration_processingConfiguration,
    redshiftDestinationConfiguration_cloudWatchLoggingOptions,
    redshiftDestinationConfiguration_s3BackupMode,
    redshiftDestinationConfiguration_retryOptions,
    redshiftDestinationConfiguration_roleARN,
    redshiftDestinationConfiguration_clusterJDBCURL,
    redshiftDestinationConfiguration_copyCommand,
    redshiftDestinationConfiguration_username,
    redshiftDestinationConfiguration_password,
    redshiftDestinationConfiguration_s3Configuration,

    -- ** RedshiftDestinationDescription
    redshiftDestinationDescription_processingConfiguration,
    redshiftDestinationDescription_s3BackupDescription,
    redshiftDestinationDescription_cloudWatchLoggingOptions,
    redshiftDestinationDescription_s3BackupMode,
    redshiftDestinationDescription_retryOptions,
    redshiftDestinationDescription_roleARN,
    redshiftDestinationDescription_clusterJDBCURL,
    redshiftDestinationDescription_copyCommand,
    redshiftDestinationDescription_username,
    redshiftDestinationDescription_s3DestinationDescription,

    -- ** RedshiftDestinationUpdate
    redshiftDestinationUpdate_processingConfiguration,
    redshiftDestinationUpdate_roleARN,
    redshiftDestinationUpdate_s3Update,
    redshiftDestinationUpdate_password,
    redshiftDestinationUpdate_username,
    redshiftDestinationUpdate_copyCommand,
    redshiftDestinationUpdate_s3BackupUpdate,
    redshiftDestinationUpdate_cloudWatchLoggingOptions,
    redshiftDestinationUpdate_s3BackupMode,
    redshiftDestinationUpdate_clusterJDBCURL,
    redshiftDestinationUpdate_retryOptions,

    -- ** RedshiftRetryOptions
    redshiftRetryOptions_durationInSeconds,

    -- ** RetryOptions
    retryOptions_durationInSeconds,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- ** S3DestinationDescription
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_prefix,
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- ** S3DestinationUpdate
    s3DestinationUpdate_roleARN,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_compressionFormat,
    s3DestinationUpdate_errorOutputPrefix,

    -- ** SchemaConfiguration
    schemaConfiguration_tableName,
    schemaConfiguration_roleARN,
    schemaConfiguration_databaseName,
    schemaConfiguration_region,
    schemaConfiguration_catalogId,
    schemaConfiguration_versionId,

    -- ** Serializer
    serializer_parquetSerDe,
    serializer_orcSerDe,

    -- ** SourceDescription
    sourceDescription_kinesisStreamSourceDescription,

    -- ** SplunkDestinationConfiguration
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- ** SplunkDestinationDescription
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_processingConfiguration,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_s3DestinationDescription,

    -- ** SplunkDestinationUpdate
    splunkDestinationUpdate_hECToken,
    splunkDestinationUpdate_processingConfiguration,
    splunkDestinationUpdate_s3Update,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,

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
