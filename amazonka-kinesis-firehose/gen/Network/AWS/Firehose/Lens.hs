{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Lens
  ( -- * Operations

    -- ** StartDeliveryStreamEncryption
    startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput,
    startDeliveryStreamEncryption_deliveryStreamName,
    startDeliveryStreamEncryptionResponse_httpStatus,

    -- ** StopDeliveryStreamEncryption
    stopDeliveryStreamEncryption_deliveryStreamName,
    stopDeliveryStreamEncryptionResponse_httpStatus,

    -- ** ListDeliveryStreams
    listDeliveryStreams_exclusiveStartDeliveryStreamName,
    listDeliveryStreams_deliveryStreamType,
    listDeliveryStreams_limit,
    listDeliveryStreamsResponse_httpStatus,
    listDeliveryStreamsResponse_deliveryStreamNames,
    listDeliveryStreamsResponse_hasMoreDeliveryStreams,

    -- ** PutRecordBatch
    putRecordBatch_deliveryStreamName,
    putRecordBatch_records,
    putRecordBatchResponse_encrypted,
    putRecordBatchResponse_httpStatus,
    putRecordBatchResponse_failedPutCount,
    putRecordBatchResponse_requestResponses,

    -- ** ListTagsForDeliveryStream
    listTagsForDeliveryStream_exclusiveStartTagKey,
    listTagsForDeliveryStream_limit,
    listTagsForDeliveryStream_deliveryStreamName,
    listTagsForDeliveryStreamResponse_httpStatus,
    listTagsForDeliveryStreamResponse_tags,
    listTagsForDeliveryStreamResponse_hasMoreTags,

    -- ** UpdateDestination
    updateDestination_redshiftDestinationUpdate,
    updateDestination_s3DestinationUpdate,
    updateDestination_extendedS3DestinationUpdate,
    updateDestination_httpEndpointDestinationUpdate,
    updateDestination_elasticsearchDestinationUpdate,
    updateDestination_splunkDestinationUpdate,
    updateDestination_deliveryStreamName,
    updateDestination_currentDeliveryStreamVersionId,
    updateDestination_destinationId,
    updateDestinationResponse_httpStatus,

    -- ** DescribeDeliveryStream
    describeDeliveryStream_exclusiveStartDestinationId,
    describeDeliveryStream_limit,
    describeDeliveryStream_deliveryStreamName,
    describeDeliveryStreamResponse_httpStatus,
    describeDeliveryStreamResponse_deliveryStreamDescription,

    -- ** CreateDeliveryStream
    createDeliveryStream_redshiftDestinationConfiguration,
    createDeliveryStream_s3DestinationConfiguration,
    createDeliveryStream_kinesisStreamSourceConfiguration,
    createDeliveryStream_extendedS3DestinationConfiguration,
    createDeliveryStream_httpEndpointDestinationConfiguration,
    createDeliveryStream_elasticsearchDestinationConfiguration,
    createDeliveryStream_tags,
    createDeliveryStream_deliveryStreamEncryptionConfigurationInput,
    createDeliveryStream_splunkDestinationConfiguration,
    createDeliveryStream_deliveryStreamType,
    createDeliveryStream_deliveryStreamName,
    createDeliveryStreamResponse_deliveryStreamARN,
    createDeliveryStreamResponse_httpStatus,

    -- ** PutRecord
    putRecord_deliveryStreamName,
    putRecord_record,
    putRecordResponse_encrypted,
    putRecordResponse_httpStatus,
    putRecordResponse_recordId,

    -- ** DeleteDeliveryStream
    deleteDeliveryStream_allowForceDelete,
    deleteDeliveryStream_deliveryStreamName,
    deleteDeliveryStreamResponse_httpStatus,

    -- ** UntagDeliveryStream
    untagDeliveryStream_deliveryStreamName,
    untagDeliveryStream_tagKeys,
    untagDeliveryStreamResponse_httpStatus,

    -- ** TagDeliveryStream
    tagDeliveryStream_deliveryStreamName,
    tagDeliveryStream_tags,
    tagDeliveryStreamResponse_httpStatus,

    -- * Types

    -- ** BufferingHints
    bufferingHints_sizeInMBs,
    bufferingHints_intervalInSeconds,

    -- ** CloudWatchLoggingOptions
    cloudWatchLoggingOptions_logStreamName,
    cloudWatchLoggingOptions_enabled,
    cloudWatchLoggingOptions_logGroupName,

    -- ** CopyCommand
    copyCommand_copyOptions,
    copyCommand_dataTableColumns,
    copyCommand_dataTableName,

    -- ** DataFormatConversionConfiguration
    dataFormatConversionConfiguration_enabled,
    dataFormatConversionConfiguration_inputFormatConfiguration,
    dataFormatConversionConfiguration_outputFormatConfiguration,
    dataFormatConversionConfiguration_schemaConfiguration,

    -- ** DeliveryStreamDescription
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

    -- ** DeliveryStreamEncryptionConfiguration
    deliveryStreamEncryptionConfiguration_status,
    deliveryStreamEncryptionConfiguration_keyARN,
    deliveryStreamEncryptionConfiguration_keyType,
    deliveryStreamEncryptionConfiguration_failureDescription,

    -- ** DeliveryStreamEncryptionConfigurationInput
    deliveryStreamEncryptionConfigurationInput_keyARN,
    deliveryStreamEncryptionConfigurationInput_keyType,

    -- ** Deserializer
    deserializer_hiveJsonSerDe,
    deserializer_openXJsonSerDe,

    -- ** DestinationDescription
    destinationDescription_elasticsearchDestinationDescription,
    destinationDescription_httpEndpointDestinationDescription,
    destinationDescription_extendedS3DestinationDescription,
    destinationDescription_redshiftDestinationDescription,
    destinationDescription_splunkDestinationDescription,
    destinationDescription_s3DestinationDescription,
    destinationDescription_destinationId,

    -- ** ElasticsearchBufferingHints
    elasticsearchBufferingHints_sizeInMBs,
    elasticsearchBufferingHints_intervalInSeconds,

    -- ** ElasticsearchDestinationConfiguration
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

    -- ** ElasticsearchDestinationDescription
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

    -- ** ElasticsearchDestinationUpdate
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

    -- ** ElasticsearchRetryOptions
    elasticsearchRetryOptions_durationInSeconds,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsEncryptionConfig,
    encryptionConfiguration_noEncryptionConfig,

    -- ** ExtendedS3DestinationConfiguration
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

    -- ** ExtendedS3DestinationDescription
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

    -- ** ExtendedS3DestinationUpdate
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
    httpEndpointConfiguration_accessKey,
    httpEndpointConfiguration_name,
    httpEndpointConfiguration_url,

    -- ** HttpEndpointDescription
    httpEndpointDescription_name,
    httpEndpointDescription_url,

    -- ** HttpEndpointDestinationConfiguration
    httpEndpointDestinationConfiguration_roleARN,
    httpEndpointDestinationConfiguration_processingConfiguration,
    httpEndpointDestinationConfiguration_cloudWatchLoggingOptions,
    httpEndpointDestinationConfiguration_requestConfiguration,
    httpEndpointDestinationConfiguration_bufferingHints,
    httpEndpointDestinationConfiguration_retryOptions,
    httpEndpointDestinationConfiguration_s3BackupMode,
    httpEndpointDestinationConfiguration_endpointConfiguration,
    httpEndpointDestinationConfiguration_s3Configuration,

    -- ** HttpEndpointDestinationDescription
    httpEndpointDestinationDescription_roleARN,
    httpEndpointDestinationDescription_processingConfiguration,
    httpEndpointDestinationDescription_endpointConfiguration,
    httpEndpointDestinationDescription_cloudWatchLoggingOptions,
    httpEndpointDestinationDescription_requestConfiguration,
    httpEndpointDestinationDescription_bufferingHints,
    httpEndpointDestinationDescription_retryOptions,
    httpEndpointDestinationDescription_s3BackupMode,
    httpEndpointDestinationDescription_s3DestinationDescription,

    -- ** HttpEndpointDestinationUpdate
    httpEndpointDestinationUpdate_roleARN,
    httpEndpointDestinationUpdate_s3Update,
    httpEndpointDestinationUpdate_processingConfiguration,
    httpEndpointDestinationUpdate_endpointConfiguration,
    httpEndpointDestinationUpdate_cloudWatchLoggingOptions,
    httpEndpointDestinationUpdate_requestConfiguration,
    httpEndpointDestinationUpdate_bufferingHints,
    httpEndpointDestinationUpdate_retryOptions,
    httpEndpointDestinationUpdate_s3BackupMode,

    -- ** HttpEndpointRequestConfiguration
    httpEndpointRequestConfiguration_contentEncoding,
    httpEndpointRequestConfiguration_commonAttributes,

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
    kinesisStreamSourceDescription_deliveryStartTimestamp,
    kinesisStreamSourceDescription_kinesisStreamARN,

    -- ** OpenXJsonSerDe
    openXJsonSerDe_caseInsensitive,
    openXJsonSerDe_columnToJsonKeyMappings,
    openXJsonSerDe_convertDotsInJsonKeysToUnderscores,

    -- ** OrcSerDe
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

    -- ** OutputFormatConfiguration
    outputFormatConfiguration_serializer,

    -- ** ParquetSerDe
    parquetSerDe_pageSizeBytes,
    parquetSerDe_enableDictionaryCompression,
    parquetSerDe_maxPaddingBytes,
    parquetSerDe_compression,
    parquetSerDe_writerVersion,
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
    putRecordBatchResponseEntry_errorMessage,
    putRecordBatchResponseEntry_errorCode,

    -- ** Record
    record_data,

    -- ** RedshiftDestinationConfiguration
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

    -- ** RedshiftDestinationDescription
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

    -- ** RedshiftDestinationUpdate
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

    -- ** RedshiftRetryOptions
    redshiftRetryOptions_durationInSeconds,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_errorOutputPrefix,
    s3DestinationConfiguration_encryptionConfiguration,
    s3DestinationConfiguration_cloudWatchLoggingOptions,
    s3DestinationConfiguration_prefix,
    s3DestinationConfiguration_bufferingHints,
    s3DestinationConfiguration_compressionFormat,
    s3DestinationConfiguration_roleARN,
    s3DestinationConfiguration_bucketARN,

    -- ** S3DestinationDescription
    s3DestinationDescription_errorOutputPrefix,
    s3DestinationDescription_cloudWatchLoggingOptions,
    s3DestinationDescription_prefix,
    s3DestinationDescription_roleARN,
    s3DestinationDescription_bucketARN,
    s3DestinationDescription_bufferingHints,
    s3DestinationDescription_compressionFormat,
    s3DestinationDescription_encryptionConfiguration,

    -- ** S3DestinationUpdate
    s3DestinationUpdate_errorOutputPrefix,
    s3DestinationUpdate_encryptionConfiguration,
    s3DestinationUpdate_roleARN,
    s3DestinationUpdate_bucketARN,
    s3DestinationUpdate_cloudWatchLoggingOptions,
    s3DestinationUpdate_prefix,
    s3DestinationUpdate_bufferingHints,
    s3DestinationUpdate_compressionFormat,

    -- ** SchemaConfiguration
    schemaConfiguration_roleARN,
    schemaConfiguration_tableName,
    schemaConfiguration_catalogId,
    schemaConfiguration_versionId,
    schemaConfiguration_region,
    schemaConfiguration_databaseName,

    -- ** Serializer
    serializer_orcSerDe,
    serializer_parquetSerDe,

    -- ** SourceDescription
    sourceDescription_kinesisStreamSourceDescription,

    -- ** SplunkDestinationConfiguration
    splunkDestinationConfiguration_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationConfiguration_processingConfiguration,
    splunkDestinationConfiguration_cloudWatchLoggingOptions,
    splunkDestinationConfiguration_retryOptions,
    splunkDestinationConfiguration_s3BackupMode,
    splunkDestinationConfiguration_hECEndpoint,
    splunkDestinationConfiguration_hECEndpointType,
    splunkDestinationConfiguration_hECToken,
    splunkDestinationConfiguration_s3Configuration,

    -- ** SplunkDestinationDescription
    splunkDestinationDescription_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationDescription_processingConfiguration,
    splunkDestinationDescription_cloudWatchLoggingOptions,
    splunkDestinationDescription_hECEndpointType,
    splunkDestinationDescription_retryOptions,
    splunkDestinationDescription_s3BackupMode,
    splunkDestinationDescription_hECEndpoint,
    splunkDestinationDescription_hECToken,
    splunkDestinationDescription_s3DestinationDescription,

    -- ** SplunkDestinationUpdate
    splunkDestinationUpdate_s3Update,
    splunkDestinationUpdate_hECAcknowledgmentTimeoutInSeconds,
    splunkDestinationUpdate_processingConfiguration,
    splunkDestinationUpdate_cloudWatchLoggingOptions,
    splunkDestinationUpdate_hECEndpointType,
    splunkDestinationUpdate_retryOptions,
    splunkDestinationUpdate_s3BackupMode,
    splunkDestinationUpdate_hECEndpoint,
    splunkDestinationUpdate_hECToken,

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

import Network.AWS.Firehose.CreateDeliveryStream
import Network.AWS.Firehose.DeleteDeliveryStream
import Network.AWS.Firehose.DescribeDeliveryStream
import Network.AWS.Firehose.ListDeliveryStreams
import Network.AWS.Firehose.ListTagsForDeliveryStream
import Network.AWS.Firehose.PutRecord
import Network.AWS.Firehose.PutRecordBatch
import Network.AWS.Firehose.StartDeliveryStreamEncryption
import Network.AWS.Firehose.StopDeliveryStreamEncryption
import Network.AWS.Firehose.TagDeliveryStream
import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamDescription
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Network.AWS.Firehose.Types.Deserializer
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
import Network.AWS.Firehose.Types.FailureDescription
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
import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Firehose.Types.OpenXJsonSerDe
import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.OutputFormatConfiguration
import Network.AWS.Firehose.Types.ParquetSerDe
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.Processor
import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
import Network.AWS.Firehose.Types.Record
import Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationUpdate
import Network.AWS.Firehose.Types.RedshiftRetryOptions
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
import Network.AWS.Firehose.Types.Tag
import Network.AWS.Firehose.Types.VpcConfiguration
import Network.AWS.Firehose.Types.VpcConfigurationDescription
import Network.AWS.Firehose.UntagDeliveryStream
import Network.AWS.Firehose.UpdateDestination
