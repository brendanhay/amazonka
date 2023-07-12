{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTAnalytics.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Lens
  ( -- * Operations

    -- ** BatchPutMessage
    batchPutMessage_channelName,
    batchPutMessage_messages,
    batchPutMessageResponse_batchPutMessageErrorEntries,
    batchPutMessageResponse_httpStatus,

    -- ** CancelPipelineReprocessing
    cancelPipelineReprocessing_pipelineName,
    cancelPipelineReprocessing_reprocessingId,
    cancelPipelineReprocessingResponse_httpStatus,

    -- ** CreateChannel
    createChannel_channelStorage,
    createChannel_retentionPeriod,
    createChannel_tags,
    createChannel_channelName,
    createChannelResponse_channelArn,
    createChannelResponse_channelName,
    createChannelResponse_retentionPeriod,
    createChannelResponse_httpStatus,

    -- ** CreateDataset
    createDataset_contentDeliveryRules,
    createDataset_lateDataRules,
    createDataset_retentionPeriod,
    createDataset_tags,
    createDataset_triggers,
    createDataset_versioningConfiguration,
    createDataset_datasetName,
    createDataset_actions,
    createDatasetResponse_datasetArn,
    createDatasetResponse_datasetName,
    createDatasetResponse_retentionPeriod,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetContent
    createDatasetContent_versionId,
    createDatasetContent_datasetName,
    createDatasetContentResponse_versionId,
    createDatasetContentResponse_httpStatus,

    -- ** CreateDatastore
    createDatastore_datastorePartitions,
    createDatastore_datastoreStorage,
    createDatastore_fileFormatConfiguration,
    createDatastore_retentionPeriod,
    createDatastore_tags,
    createDatastore_datastoreName,
    createDatastoreResponse_datastoreArn,
    createDatastoreResponse_datastoreName,
    createDatastoreResponse_retentionPeriod,
    createDatastoreResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_pipelineActivities,
    createPipelineResponse_pipelineArn,
    createPipelineResponse_pipelineName,
    createPipelineResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_channelName,

    -- ** DeleteDataset
    deleteDataset_datasetName,

    -- ** DeleteDatasetContent
    deleteDatasetContent_versionId,
    deleteDatasetContent_datasetName,

    -- ** DeleteDatastore
    deleteDatastore_datastoreName,

    -- ** DeletePipeline
    deletePipeline_pipelineName,

    -- ** DescribeChannel
    describeChannel_includeStatistics,
    describeChannel_channelName,
    describeChannelResponse_channel,
    describeChannelResponse_statistics,
    describeChannelResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetName,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDatastore
    describeDatastore_includeStatistics,
    describeDatastore_datastoreName,
    describeDatastoreResponse_datastore,
    describeDatastoreResponse_statistics,
    describeDatastoreResponse_httpStatus,

    -- ** DescribeLoggingOptions
    describeLoggingOptionsResponse_loggingOptions,
    describeLoggingOptionsResponse_httpStatus,

    -- ** DescribePipeline
    describePipeline_pipelineName,
    describePipelineResponse_pipeline,
    describePipelineResponse_httpStatus,

    -- ** GetDatasetContent
    getDatasetContent_versionId,
    getDatasetContent_datasetName,
    getDatasetContentResponse_entries,
    getDatasetContentResponse_status,
    getDatasetContentResponse_timestamp,
    getDatasetContentResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_channelSummaries,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListDatasetContents
    listDatasetContents_maxResults,
    listDatasetContents_nextToken,
    listDatasetContents_scheduledBefore,
    listDatasetContents_scheduledOnOrAfter,
    listDatasetContents_datasetName,
    listDatasetContentsResponse_datasetContentSummaries,
    listDatasetContentsResponse_nextToken,
    listDatasetContentsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_maxResults,
    listDatasets_nextToken,
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,

    -- ** ListDatastores
    listDatastores_maxResults,
    listDatastores_nextToken,
    listDatastoresResponse_datastoreSummaries,
    listDatastoresResponse_nextToken,
    listDatastoresResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_maxResults,
    listPipelines_nextToken,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutLoggingOptions
    putLoggingOptions_loggingOptions,

    -- ** RunPipelineActivity
    runPipelineActivity_pipelineActivity,
    runPipelineActivity_payloads,
    runPipelineActivityResponse_logResult,
    runPipelineActivityResponse_payloads,
    runPipelineActivityResponse_httpStatus,

    -- ** SampleChannelData
    sampleChannelData_endTime,
    sampleChannelData_maxMessages,
    sampleChannelData_startTime,
    sampleChannelData_channelName,
    sampleChannelDataResponse_payloads,
    sampleChannelDataResponse_httpStatus,

    -- ** StartPipelineReprocessing
    startPipelineReprocessing_channelMessages,
    startPipelineReprocessing_endTime,
    startPipelineReprocessing_startTime,
    startPipelineReprocessing_pipelineName,
    startPipelineReprocessingResponse_reprocessingId,
    startPipelineReprocessingResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_channelStorage,
    updateChannel_retentionPeriod,
    updateChannel_channelName,

    -- ** UpdateDataset
    updateDataset_contentDeliveryRules,
    updateDataset_lateDataRules,
    updateDataset_retentionPeriod,
    updateDataset_triggers,
    updateDataset_versioningConfiguration,
    updateDataset_datasetName,
    updateDataset_actions,

    -- ** UpdateDatastore
    updateDatastore_datastoreStorage,
    updateDatastore_fileFormatConfiguration,
    updateDatastore_retentionPeriod,
    updateDatastore_datastoreName,

    -- ** UpdatePipeline
    updatePipeline_pipelineName,
    updatePipeline_pipelineActivities,

    -- * Types

    -- ** AddAttributesActivity
    addAttributesActivity_next,
    addAttributesActivity_name,
    addAttributesActivity_attributes,

    -- ** BatchPutMessageErrorEntry
    batchPutMessageErrorEntry_errorCode,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,

    -- ** Channel
    channel_arn,
    channel_creationTime,
    channel_lastMessageArrivalTime,
    channel_lastUpdateTime,
    channel_name,
    channel_retentionPeriod,
    channel_status,
    channel_storage,

    -- ** ChannelActivity
    channelActivity_next,
    channelActivity_name,
    channelActivity_channelName,

    -- ** ChannelMessages
    channelMessages_s3Paths,

    -- ** ChannelStatistics
    channelStatistics_size,

    -- ** ChannelStorage
    channelStorage_customerManagedS3,
    channelStorage_serviceManagedS3,

    -- ** ChannelStorageSummary
    channelStorageSummary_customerManagedS3,
    channelStorageSummary_serviceManagedS3,

    -- ** ChannelSummary
    channelSummary_channelName,
    channelSummary_channelStorage,
    channelSummary_creationTime,
    channelSummary_lastMessageArrivalTime,
    channelSummary_lastUpdateTime,
    channelSummary_status,

    -- ** Column
    column_name,
    column_type,

    -- ** ContainerDatasetAction
    containerDatasetAction_variables,
    containerDatasetAction_image,
    containerDatasetAction_executionRoleArn,
    containerDatasetAction_resourceConfiguration,

    -- ** CustomerManagedChannelS3Storage
    customerManagedChannelS3Storage_keyPrefix,
    customerManagedChannelS3Storage_bucket,
    customerManagedChannelS3Storage_roleArn,

    -- ** CustomerManagedChannelS3StorageSummary
    customerManagedChannelS3StorageSummary_bucket,
    customerManagedChannelS3StorageSummary_keyPrefix,
    customerManagedChannelS3StorageSummary_roleArn,

    -- ** CustomerManagedDatastoreS3Storage
    customerManagedDatastoreS3Storage_keyPrefix,
    customerManagedDatastoreS3Storage_bucket,
    customerManagedDatastoreS3Storage_roleArn,

    -- ** CustomerManagedDatastoreS3StorageSummary
    customerManagedDatastoreS3StorageSummary_bucket,
    customerManagedDatastoreS3StorageSummary_keyPrefix,
    customerManagedDatastoreS3StorageSummary_roleArn,

    -- ** Dataset
    dataset_actions,
    dataset_arn,
    dataset_contentDeliveryRules,
    dataset_creationTime,
    dataset_lastUpdateTime,
    dataset_lateDataRules,
    dataset_name,
    dataset_retentionPeriod,
    dataset_status,
    dataset_triggers,
    dataset_versioningConfiguration,

    -- ** DatasetAction
    datasetAction_actionName,
    datasetAction_containerAction,
    datasetAction_queryAction,

    -- ** DatasetActionSummary
    datasetActionSummary_actionName,
    datasetActionSummary_actionType,

    -- ** DatasetContentDeliveryDestination
    datasetContentDeliveryDestination_iotEventsDestinationConfiguration,
    datasetContentDeliveryDestination_s3DestinationConfiguration,

    -- ** DatasetContentDeliveryRule
    datasetContentDeliveryRule_entryName,
    datasetContentDeliveryRule_destination,

    -- ** DatasetContentStatus
    datasetContentStatus_reason,
    datasetContentStatus_state,

    -- ** DatasetContentSummary
    datasetContentSummary_completionTime,
    datasetContentSummary_creationTime,
    datasetContentSummary_scheduleTime,
    datasetContentSummary_status,
    datasetContentSummary_version,

    -- ** DatasetContentVersionValue
    datasetContentVersionValue_datasetName,

    -- ** DatasetEntry
    datasetEntry_dataURI,
    datasetEntry_entryName,

    -- ** DatasetSummary
    datasetSummary_actions,
    datasetSummary_creationTime,
    datasetSummary_datasetName,
    datasetSummary_lastUpdateTime,
    datasetSummary_status,
    datasetSummary_triggers,

    -- ** DatasetTrigger
    datasetTrigger_dataset,
    datasetTrigger_schedule,

    -- ** Datastore
    datastore_arn,
    datastore_creationTime,
    datastore_datastorePartitions,
    datastore_fileFormatConfiguration,
    datastore_lastMessageArrivalTime,
    datastore_lastUpdateTime,
    datastore_name,
    datastore_retentionPeriod,
    datastore_status,
    datastore_storage,

    -- ** DatastoreActivity
    datastoreActivity_name,
    datastoreActivity_datastoreName,

    -- ** DatastoreIotSiteWiseMultiLayerStorage
    datastoreIotSiteWiseMultiLayerStorage_customerManagedS3Storage,

    -- ** DatastoreIotSiteWiseMultiLayerStorageSummary
    datastoreIotSiteWiseMultiLayerStorageSummary_customerManagedS3Storage,

    -- ** DatastorePartition
    datastorePartition_attributePartition,
    datastorePartition_timestampPartition,

    -- ** DatastorePartitions
    datastorePartitions_partitions,

    -- ** DatastoreStatistics
    datastoreStatistics_size,

    -- ** DatastoreStorage
    datastoreStorage_customerManagedS3,
    datastoreStorage_iotSiteWiseMultiLayerStorage,
    datastoreStorage_serviceManagedS3,

    -- ** DatastoreStorageSummary
    datastoreStorageSummary_customerManagedS3,
    datastoreStorageSummary_iotSiteWiseMultiLayerStorage,
    datastoreStorageSummary_serviceManagedS3,

    -- ** DatastoreSummary
    datastoreSummary_creationTime,
    datastoreSummary_datastoreName,
    datastoreSummary_datastorePartitions,
    datastoreSummary_datastoreStorage,
    datastoreSummary_fileFormatType,
    datastoreSummary_lastMessageArrivalTime,
    datastoreSummary_lastUpdateTime,
    datastoreSummary_status,

    -- ** DeltaTime
    deltaTime_offsetSeconds,
    deltaTime_timeExpression,

    -- ** DeltaTimeSessionWindowConfiguration
    deltaTimeSessionWindowConfiguration_timeoutInMinutes,

    -- ** DeviceRegistryEnrichActivity
    deviceRegistryEnrichActivity_next,
    deviceRegistryEnrichActivity_name,
    deviceRegistryEnrichActivity_attribute,
    deviceRegistryEnrichActivity_thingName,
    deviceRegistryEnrichActivity_roleArn,

    -- ** DeviceShadowEnrichActivity
    deviceShadowEnrichActivity_next,
    deviceShadowEnrichActivity_name,
    deviceShadowEnrichActivity_attribute,
    deviceShadowEnrichActivity_thingName,
    deviceShadowEnrichActivity_roleArn,

    -- ** EstimatedResourceSize
    estimatedResourceSize_estimatedOn,
    estimatedResourceSize_estimatedSizeInBytes,

    -- ** FileFormatConfiguration
    fileFormatConfiguration_jsonConfiguration,
    fileFormatConfiguration_parquetConfiguration,

    -- ** FilterActivity
    filterActivity_next,
    filterActivity_name,
    filterActivity_filter,

    -- ** GlueConfiguration
    glueConfiguration_tableName,
    glueConfiguration_databaseName,

    -- ** IotEventsDestinationConfiguration
    iotEventsDestinationConfiguration_inputName,
    iotEventsDestinationConfiguration_roleArn,

    -- ** IotSiteWiseCustomerManagedDatastoreS3Storage
    iotSiteWiseCustomerManagedDatastoreS3Storage_keyPrefix,
    iotSiteWiseCustomerManagedDatastoreS3Storage_bucket,

    -- ** IotSiteWiseCustomerManagedDatastoreS3StorageSummary
    iotSiteWiseCustomerManagedDatastoreS3StorageSummary_bucket,
    iotSiteWiseCustomerManagedDatastoreS3StorageSummary_keyPrefix,

    -- ** JsonConfiguration

    -- ** LambdaActivity
    lambdaActivity_next,
    lambdaActivity_name,
    lambdaActivity_lambdaName,
    lambdaActivity_batchSize,

    -- ** LateDataRule
    lateDataRule_ruleName,
    lateDataRule_ruleConfiguration,

    -- ** LateDataRuleConfiguration
    lateDataRuleConfiguration_deltaTimeSessionWindowConfiguration,

    -- ** LoggingOptions
    loggingOptions_roleArn,
    loggingOptions_level,
    loggingOptions_enabled,

    -- ** MathActivity
    mathActivity_next,
    mathActivity_name,
    mathActivity_attribute,
    mathActivity_math,

    -- ** Message
    message_messageId,
    message_payload,

    -- ** OutputFileUriValue
    outputFileUriValue_fileName,

    -- ** ParquetConfiguration
    parquetConfiguration_schemaDefinition,

    -- ** Partition
    partition_attributeName,

    -- ** Pipeline
    pipeline_activities,
    pipeline_arn,
    pipeline_creationTime,
    pipeline_lastUpdateTime,
    pipeline_name,
    pipeline_reprocessingSummaries,

    -- ** PipelineActivity
    pipelineActivity_addAttributes,
    pipelineActivity_channel,
    pipelineActivity_datastore,
    pipelineActivity_deviceRegistryEnrich,
    pipelineActivity_deviceShadowEnrich,
    pipelineActivity_filter,
    pipelineActivity_lambda,
    pipelineActivity_math,
    pipelineActivity_removeAttributes,
    pipelineActivity_selectAttributes,

    -- ** PipelineSummary
    pipelineSummary_creationTime,
    pipelineSummary_lastUpdateTime,
    pipelineSummary_pipelineName,
    pipelineSummary_reprocessingSummaries,

    -- ** QueryFilter
    queryFilter_deltaTime,

    -- ** RemoveAttributesActivity
    removeAttributesActivity_next,
    removeAttributesActivity_name,
    removeAttributesActivity_attributes,

    -- ** ReprocessingSummary
    reprocessingSummary_creationTime,
    reprocessingSummary_id,
    reprocessingSummary_status,

    -- ** ResourceConfiguration
    resourceConfiguration_computeType,
    resourceConfiguration_volumeSizeInGB,

    -- ** RetentionPeriod
    retentionPeriod_numberOfDays,
    retentionPeriod_unlimited,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_glueConfiguration,
    s3DestinationConfiguration_bucket,
    s3DestinationConfiguration_key,
    s3DestinationConfiguration_roleArn,

    -- ** Schedule
    schedule_expression,

    -- ** SchemaDefinition
    schemaDefinition_columns,

    -- ** SelectAttributesActivity
    selectAttributesActivity_next,
    selectAttributesActivity_name,
    selectAttributesActivity_attributes,

    -- ** ServiceManagedChannelS3Storage

    -- ** ServiceManagedChannelS3StorageSummary

    -- ** ServiceManagedDatastoreS3Storage

    -- ** ServiceManagedDatastoreS3StorageSummary

    -- ** SqlQueryDatasetAction
    sqlQueryDatasetAction_filters,
    sqlQueryDatasetAction_sqlQuery,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimestampPartition
    timestampPartition_timestampFormat,
    timestampPartition_attributeName,

    -- ** TriggeringDataset
    triggeringDataset_name,

    -- ** Variable
    variable_datasetContentVersionValue,
    variable_doubleValue,
    variable_outputFileUriValue,
    variable_stringValue,
    variable_name,

    -- ** VersioningConfiguration
    versioningConfiguration_maxVersions,
    versioningConfiguration_unlimited,
  )
where

import Amazonka.IoTAnalytics.BatchPutMessage
import Amazonka.IoTAnalytics.CancelPipelineReprocessing
import Amazonka.IoTAnalytics.CreateChannel
import Amazonka.IoTAnalytics.CreateDataset
import Amazonka.IoTAnalytics.CreateDatasetContent
import Amazonka.IoTAnalytics.CreateDatastore
import Amazonka.IoTAnalytics.CreatePipeline
import Amazonka.IoTAnalytics.DeleteChannel
import Amazonka.IoTAnalytics.DeleteDataset
import Amazonka.IoTAnalytics.DeleteDatasetContent
import Amazonka.IoTAnalytics.DeleteDatastore
import Amazonka.IoTAnalytics.DeletePipeline
import Amazonka.IoTAnalytics.DescribeChannel
import Amazonka.IoTAnalytics.DescribeDataset
import Amazonka.IoTAnalytics.DescribeDatastore
import Amazonka.IoTAnalytics.DescribeLoggingOptions
import Amazonka.IoTAnalytics.DescribePipeline
import Amazonka.IoTAnalytics.GetDatasetContent
import Amazonka.IoTAnalytics.ListChannels
import Amazonka.IoTAnalytics.ListDatasetContents
import Amazonka.IoTAnalytics.ListDatasets
import Amazonka.IoTAnalytics.ListDatastores
import Amazonka.IoTAnalytics.ListPipelines
import Amazonka.IoTAnalytics.ListTagsForResource
import Amazonka.IoTAnalytics.PutLoggingOptions
import Amazonka.IoTAnalytics.RunPipelineActivity
import Amazonka.IoTAnalytics.SampleChannelData
import Amazonka.IoTAnalytics.StartPipelineReprocessing
import Amazonka.IoTAnalytics.TagResource
import Amazonka.IoTAnalytics.Types.AddAttributesActivity
import Amazonka.IoTAnalytics.Types.BatchPutMessageErrorEntry
import Amazonka.IoTAnalytics.Types.Channel
import Amazonka.IoTAnalytics.Types.ChannelActivity
import Amazonka.IoTAnalytics.Types.ChannelMessages
import Amazonka.IoTAnalytics.Types.ChannelStatistics
import Amazonka.IoTAnalytics.Types.ChannelStorage
import Amazonka.IoTAnalytics.Types.ChannelStorageSummary
import Amazonka.IoTAnalytics.Types.ChannelSummary
import Amazonka.IoTAnalytics.Types.Column
import Amazonka.IoTAnalytics.Types.ContainerDatasetAction
import Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.Dataset
import Amazonka.IoTAnalytics.Types.DatasetAction
import Amazonka.IoTAnalytics.Types.DatasetActionSummary
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryDestination
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryRule
import Amazonka.IoTAnalytics.Types.DatasetContentStatus
import Amazonka.IoTAnalytics.Types.DatasetContentSummary
import Amazonka.IoTAnalytics.Types.DatasetContentVersionValue
import Amazonka.IoTAnalytics.Types.DatasetEntry
import Amazonka.IoTAnalytics.Types.DatasetSummary
import Amazonka.IoTAnalytics.Types.DatasetTrigger
import Amazonka.IoTAnalytics.Types.Datastore
import Amazonka.IoTAnalytics.Types.DatastoreActivity
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorageSummary
import Amazonka.IoTAnalytics.Types.DatastorePartition
import Amazonka.IoTAnalytics.Types.DatastorePartitions
import Amazonka.IoTAnalytics.Types.DatastoreStatistics
import Amazonka.IoTAnalytics.Types.DatastoreStorage
import Amazonka.IoTAnalytics.Types.DatastoreStorageSummary
import Amazonka.IoTAnalytics.Types.DatastoreSummary
import Amazonka.IoTAnalytics.Types.DeltaTime
import Amazonka.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import Amazonka.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Amazonka.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Amazonka.IoTAnalytics.Types.EstimatedResourceSize
import Amazonka.IoTAnalytics.Types.FileFormatConfiguration
import Amazonka.IoTAnalytics.Types.FilterActivity
import Amazonka.IoTAnalytics.Types.GlueConfiguration
import Amazonka.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.JsonConfiguration
import Amazonka.IoTAnalytics.Types.LambdaActivity
import Amazonka.IoTAnalytics.Types.LateDataRule
import Amazonka.IoTAnalytics.Types.LateDataRuleConfiguration
import Amazonka.IoTAnalytics.Types.LoggingOptions
import Amazonka.IoTAnalytics.Types.MathActivity
import Amazonka.IoTAnalytics.Types.Message
import Amazonka.IoTAnalytics.Types.OutputFileUriValue
import Amazonka.IoTAnalytics.Types.ParquetConfiguration
import Amazonka.IoTAnalytics.Types.Partition
import Amazonka.IoTAnalytics.Types.Pipeline
import Amazonka.IoTAnalytics.Types.PipelineActivity
import Amazonka.IoTAnalytics.Types.PipelineSummary
import Amazonka.IoTAnalytics.Types.QueryFilter
import Amazonka.IoTAnalytics.Types.RemoveAttributesActivity
import Amazonka.IoTAnalytics.Types.ReprocessingSummary
import Amazonka.IoTAnalytics.Types.ResourceConfiguration
import Amazonka.IoTAnalytics.Types.RetentionPeriod
import Amazonka.IoTAnalytics.Types.S3DestinationConfiguration
import Amazonka.IoTAnalytics.Types.Schedule
import Amazonka.IoTAnalytics.Types.SchemaDefinition
import Amazonka.IoTAnalytics.Types.SelectAttributesActivity
import Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.SqlQueryDatasetAction
import Amazonka.IoTAnalytics.Types.Tag
import Amazonka.IoTAnalytics.Types.TimestampPartition
import Amazonka.IoTAnalytics.Types.TriggeringDataset
import Amazonka.IoTAnalytics.Types.Variable
import Amazonka.IoTAnalytics.Types.VersioningConfiguration
import Amazonka.IoTAnalytics.UntagResource
import Amazonka.IoTAnalytics.UpdateChannel
import Amazonka.IoTAnalytics.UpdateDataset
import Amazonka.IoTAnalytics.UpdateDatastore
import Amazonka.IoTAnalytics.UpdatePipeline
