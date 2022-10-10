{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTAnalytics.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    createChannel_tags,
    createChannel_channelStorage,
    createChannel_retentionPeriod,
    createChannel_channelName,
    createChannelResponse_channelName,
    createChannelResponse_channelArn,
    createChannelResponse_retentionPeriod,
    createChannelResponse_httpStatus,

    -- ** CreateDataset
    createDataset_tags,
    createDataset_lateDataRules,
    createDataset_versioningConfiguration,
    createDataset_triggers,
    createDataset_contentDeliveryRules,
    createDataset_retentionPeriod,
    createDataset_datasetName,
    createDataset_actions,
    createDatasetResponse_datasetName,
    createDatasetResponse_datasetArn,
    createDatasetResponse_retentionPeriod,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetContent
    createDatasetContent_versionId,
    createDatasetContent_datasetName,
    createDatasetContentResponse_versionId,
    createDatasetContentResponse_httpStatus,

    -- ** CreateDatastore
    createDatastore_tags,
    createDatastore_fileFormatConfiguration,
    createDatastore_retentionPeriod,
    createDatastore_datastoreStorage,
    createDatastore_datastorePartitions,
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
    describeChannelResponse_statistics,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetName,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDatastore
    describeDatastore_includeStatistics,
    describeDatastore_datastoreName,
    describeDatastoreResponse_statistics,
    describeDatastoreResponse_datastore,
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
    getDatasetContentResponse_timestamp,
    getDatasetContentResponse_status,
    getDatasetContentResponse_entries,
    getDatasetContentResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_channelSummaries,
    listChannelsResponse_httpStatus,

    -- ** ListDatasetContents
    listDatasetContents_scheduledOnOrAfter,
    listDatasetContents_nextToken,
    listDatasetContents_maxResults,
    listDatasetContents_scheduledBefore,
    listDatasetContents_datasetName,
    listDatasetContentsResponse_nextToken,
    listDatasetContentsResponse_datasetContentSummaries,
    listDatasetContentsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_httpStatus,

    -- ** ListDatastores
    listDatastores_nextToken,
    listDatastores_maxResults,
    listDatastoresResponse_nextToken,
    listDatastoresResponse_datastoreSummaries,
    listDatastoresResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_nextToken,
    listPipelines_maxResults,
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
    runPipelineActivityResponse_payloads,
    runPipelineActivityResponse_logResult,
    runPipelineActivityResponse_httpStatus,

    -- ** SampleChannelData
    sampleChannelData_endTime,
    sampleChannelData_maxMessages,
    sampleChannelData_startTime,
    sampleChannelData_channelName,
    sampleChannelDataResponse_payloads,
    sampleChannelDataResponse_httpStatus,

    -- ** StartPipelineReprocessing
    startPipelineReprocessing_endTime,
    startPipelineReprocessing_channelMessages,
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
    updateDataset_lateDataRules,
    updateDataset_versioningConfiguration,
    updateDataset_triggers,
    updateDataset_contentDeliveryRules,
    updateDataset_retentionPeriod,
    updateDataset_datasetName,
    updateDataset_actions,

    -- ** UpdateDatastore
    updateDatastore_fileFormatConfiguration,
    updateDatastore_retentionPeriod,
    updateDatastore_datastoreStorage,
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
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,
    batchPutMessageErrorEntry_errorCode,

    -- ** Channel
    channel_name,
    channel_arn,
    channel_storage,
    channel_status,
    channel_retentionPeriod,
    channel_creationTime,
    channel_lastUpdateTime,
    channel_lastMessageArrivalTime,

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
    channelSummary_status,
    channelSummary_creationTime,
    channelSummary_lastUpdateTime,
    channelSummary_lastMessageArrivalTime,

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
    customerManagedChannelS3StorageSummary_roleArn,
    customerManagedChannelS3StorageSummary_bucket,
    customerManagedChannelS3StorageSummary_keyPrefix,

    -- ** CustomerManagedDatastoreS3Storage
    customerManagedDatastoreS3Storage_keyPrefix,
    customerManagedDatastoreS3Storage_bucket,
    customerManagedDatastoreS3Storage_roleArn,

    -- ** CustomerManagedDatastoreS3StorageSummary
    customerManagedDatastoreS3StorageSummary_roleArn,
    customerManagedDatastoreS3StorageSummary_bucket,
    customerManagedDatastoreS3StorageSummary_keyPrefix,

    -- ** Dataset
    dataset_name,
    dataset_lateDataRules,
    dataset_versioningConfiguration,
    dataset_arn,
    dataset_status,
    dataset_triggers,
    dataset_contentDeliveryRules,
    dataset_retentionPeriod,
    dataset_creationTime,
    dataset_lastUpdateTime,
    dataset_actions,

    -- ** DatasetAction
    datasetAction_queryAction,
    datasetAction_actionName,
    datasetAction_containerAction,

    -- ** DatasetActionSummary
    datasetActionSummary_actionName,
    datasetActionSummary_actionType,

    -- ** DatasetContentDeliveryDestination
    datasetContentDeliveryDestination_s3DestinationConfiguration,
    datasetContentDeliveryDestination_iotEventsDestinationConfiguration,

    -- ** DatasetContentDeliveryRule
    datasetContentDeliveryRule_entryName,
    datasetContentDeliveryRule_destination,

    -- ** DatasetContentStatus
    datasetContentStatus_state,
    datasetContentStatus_reason,

    -- ** DatasetContentSummary
    datasetContentSummary_status,
    datasetContentSummary_completionTime,
    datasetContentSummary_scheduleTime,
    datasetContentSummary_creationTime,
    datasetContentSummary_version,

    -- ** DatasetContentVersionValue
    datasetContentVersionValue_datasetName,

    -- ** DatasetEntry
    datasetEntry_entryName,
    datasetEntry_dataURI,

    -- ** DatasetSummary
    datasetSummary_datasetName,
    datasetSummary_status,
    datasetSummary_triggers,
    datasetSummary_creationTime,
    datasetSummary_lastUpdateTime,
    datasetSummary_actions,

    -- ** DatasetTrigger
    datasetTrigger_schedule,
    datasetTrigger_dataset,

    -- ** Datastore
    datastore_name,
    datastore_arn,
    datastore_storage,
    datastore_status,
    datastore_fileFormatConfiguration,
    datastore_retentionPeriod,
    datastore_creationTime,
    datastore_lastUpdateTime,
    datastore_lastMessageArrivalTime,
    datastore_datastorePartitions,

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
    datastoreSummary_fileFormatType,
    datastoreSummary_status,
    datastoreSummary_datastoreName,
    datastoreSummary_creationTime,
    datastoreSummary_lastUpdateTime,
    datastoreSummary_datastoreStorage,
    datastoreSummary_lastMessageArrivalTime,
    datastoreSummary_datastorePartitions,

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
    estimatedResourceSize_estimatedSizeInBytes,
    estimatedResourceSize_estimatedOn,

    -- ** FileFormatConfiguration
    fileFormatConfiguration_parquetConfiguration,
    fileFormatConfiguration_jsonConfiguration,

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
    pipeline_name,
    pipeline_activities,
    pipeline_reprocessingSummaries,
    pipeline_arn,
    pipeline_creationTime,
    pipeline_lastUpdateTime,

    -- ** PipelineActivity
    pipelineActivity_deviceRegistryEnrich,
    pipelineActivity_selectAttributes,
    pipelineActivity_datastore,
    pipelineActivity_addAttributes,
    pipelineActivity_channel,
    pipelineActivity_removeAttributes,
    pipelineActivity_deviceShadowEnrich,
    pipelineActivity_filter,
    pipelineActivity_math,
    pipelineActivity_lambda,

    -- ** PipelineSummary
    pipelineSummary_reprocessingSummaries,
    pipelineSummary_pipelineName,
    pipelineSummary_creationTime,
    pipelineSummary_lastUpdateTime,

    -- ** QueryFilter
    queryFilter_deltaTime,

    -- ** RemoveAttributesActivity
    removeAttributesActivity_next,
    removeAttributesActivity_name,
    removeAttributesActivity_attributes,

    -- ** ReprocessingSummary
    reprocessingSummary_status,
    reprocessingSummary_id,
    reprocessingSummary_creationTime,

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
    variable_doubleValue,
    variable_stringValue,
    variable_outputFileUriValue,
    variable_datasetContentVersionValue,
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
