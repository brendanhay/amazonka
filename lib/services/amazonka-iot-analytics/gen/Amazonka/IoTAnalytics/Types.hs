{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTAnalytics.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalFailureException,
    _InvalidRequestException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,

    -- * ChannelStatus
    ChannelStatus (..),

    -- * ComputeType
    ComputeType (..),

    -- * DatasetActionType
    DatasetActionType (..),

    -- * DatasetContentState
    DatasetContentState (..),

    -- * DatasetStatus
    DatasetStatus (..),

    -- * DatastoreStatus
    DatastoreStatus (..),

    -- * FileFormatType
    FileFormatType (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * ReprocessingStatus
    ReprocessingStatus (..),

    -- * AddAttributesActivity
    AddAttributesActivity (..),
    newAddAttributesActivity,
    addAttributesActivity_next,
    addAttributesActivity_name,
    addAttributesActivity_attributes,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    newBatchPutMessageErrorEntry,
    batchPutMessageErrorEntry_errorCode,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,

    -- * Channel
    Channel (..),
    newChannel,
    channel_arn,
    channel_creationTime,
    channel_lastMessageArrivalTime,
    channel_lastUpdateTime,
    channel_name,
    channel_retentionPeriod,
    channel_status,
    channel_storage,

    -- * ChannelActivity
    ChannelActivity (..),
    newChannelActivity,
    channelActivity_next,
    channelActivity_name,
    channelActivity_channelName,

    -- * ChannelMessages
    ChannelMessages (..),
    newChannelMessages,
    channelMessages_s3Paths,

    -- * ChannelStatistics
    ChannelStatistics (..),
    newChannelStatistics,
    channelStatistics_size,

    -- * ChannelStorage
    ChannelStorage (..),
    newChannelStorage,
    channelStorage_customerManagedS3,
    channelStorage_serviceManagedS3,

    -- * ChannelStorageSummary
    ChannelStorageSummary (..),
    newChannelStorageSummary,
    channelStorageSummary_customerManagedS3,
    channelStorageSummary_serviceManagedS3,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_channelName,
    channelSummary_channelStorage,
    channelSummary_creationTime,
    channelSummary_lastMessageArrivalTime,
    channelSummary_lastUpdateTime,
    channelSummary_status,

    -- * Column
    Column (..),
    newColumn,
    column_name,
    column_type,

    -- * ContainerDatasetAction
    ContainerDatasetAction (..),
    newContainerDatasetAction,
    containerDatasetAction_variables,
    containerDatasetAction_image,
    containerDatasetAction_executionRoleArn,
    containerDatasetAction_resourceConfiguration,

    -- * CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage (..),
    newCustomerManagedChannelS3Storage,
    customerManagedChannelS3Storage_keyPrefix,
    customerManagedChannelS3Storage_bucket,
    customerManagedChannelS3Storage_roleArn,

    -- * CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary (..),
    newCustomerManagedChannelS3StorageSummary,
    customerManagedChannelS3StorageSummary_bucket,
    customerManagedChannelS3StorageSummary_keyPrefix,
    customerManagedChannelS3StorageSummary_roleArn,

    -- * CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage (..),
    newCustomerManagedDatastoreS3Storage,
    customerManagedDatastoreS3Storage_keyPrefix,
    customerManagedDatastoreS3Storage_bucket,
    customerManagedDatastoreS3Storage_roleArn,

    -- * CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary (..),
    newCustomerManagedDatastoreS3StorageSummary,
    customerManagedDatastoreS3StorageSummary_bucket,
    customerManagedDatastoreS3StorageSummary_keyPrefix,
    customerManagedDatastoreS3StorageSummary_roleArn,

    -- * Dataset
    Dataset (..),
    newDataset,
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

    -- * DatasetAction
    DatasetAction (..),
    newDatasetAction,
    datasetAction_actionName,
    datasetAction_containerAction,
    datasetAction_queryAction,

    -- * DatasetActionSummary
    DatasetActionSummary (..),
    newDatasetActionSummary,
    datasetActionSummary_actionName,
    datasetActionSummary_actionType,

    -- * DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination (..),
    newDatasetContentDeliveryDestination,
    datasetContentDeliveryDestination_iotEventsDestinationConfiguration,
    datasetContentDeliveryDestination_s3DestinationConfiguration,

    -- * DatasetContentDeliveryRule
    DatasetContentDeliveryRule (..),
    newDatasetContentDeliveryRule,
    datasetContentDeliveryRule_entryName,
    datasetContentDeliveryRule_destination,

    -- * DatasetContentStatus
    DatasetContentStatus (..),
    newDatasetContentStatus,
    datasetContentStatus_reason,
    datasetContentStatus_state,

    -- * DatasetContentSummary
    DatasetContentSummary (..),
    newDatasetContentSummary,
    datasetContentSummary_completionTime,
    datasetContentSummary_creationTime,
    datasetContentSummary_scheduleTime,
    datasetContentSummary_status,
    datasetContentSummary_version,

    -- * DatasetContentVersionValue
    DatasetContentVersionValue (..),
    newDatasetContentVersionValue,
    datasetContentVersionValue_datasetName,

    -- * DatasetEntry
    DatasetEntry (..),
    newDatasetEntry,
    datasetEntry_dataURI,
    datasetEntry_entryName,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_actions,
    datasetSummary_creationTime,
    datasetSummary_datasetName,
    datasetSummary_lastUpdateTime,
    datasetSummary_status,
    datasetSummary_triggers,

    -- * DatasetTrigger
    DatasetTrigger (..),
    newDatasetTrigger,
    datasetTrigger_dataset,
    datasetTrigger_schedule,

    -- * Datastore
    Datastore (..),
    newDatastore,
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

    -- * DatastoreActivity
    DatastoreActivity (..),
    newDatastoreActivity,
    datastoreActivity_name,
    datastoreActivity_datastoreName,

    -- * DatastoreIotSiteWiseMultiLayerStorage
    DatastoreIotSiteWiseMultiLayerStorage (..),
    newDatastoreIotSiteWiseMultiLayerStorage,
    datastoreIotSiteWiseMultiLayerStorage_customerManagedS3Storage,

    -- * DatastoreIotSiteWiseMultiLayerStorageSummary
    DatastoreIotSiteWiseMultiLayerStorageSummary (..),
    newDatastoreIotSiteWiseMultiLayerStorageSummary,
    datastoreIotSiteWiseMultiLayerStorageSummary_customerManagedS3Storage,

    -- * DatastorePartition
    DatastorePartition (..),
    newDatastorePartition,
    datastorePartition_attributePartition,
    datastorePartition_timestampPartition,

    -- * DatastorePartitions
    DatastorePartitions (..),
    newDatastorePartitions,
    datastorePartitions_partitions,

    -- * DatastoreStatistics
    DatastoreStatistics (..),
    newDatastoreStatistics,
    datastoreStatistics_size,

    -- * DatastoreStorage
    DatastoreStorage (..),
    newDatastoreStorage,
    datastoreStorage_customerManagedS3,
    datastoreStorage_iotSiteWiseMultiLayerStorage,
    datastoreStorage_serviceManagedS3,

    -- * DatastoreStorageSummary
    DatastoreStorageSummary (..),
    newDatastoreStorageSummary,
    datastoreStorageSummary_customerManagedS3,
    datastoreStorageSummary_iotSiteWiseMultiLayerStorage,
    datastoreStorageSummary_serviceManagedS3,

    -- * DatastoreSummary
    DatastoreSummary (..),
    newDatastoreSummary,
    datastoreSummary_creationTime,
    datastoreSummary_datastoreName,
    datastoreSummary_datastorePartitions,
    datastoreSummary_datastoreStorage,
    datastoreSummary_fileFormatType,
    datastoreSummary_lastMessageArrivalTime,
    datastoreSummary_lastUpdateTime,
    datastoreSummary_status,

    -- * DeltaTime
    DeltaTime (..),
    newDeltaTime,
    deltaTime_offsetSeconds,
    deltaTime_timeExpression,

    -- * DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration (..),
    newDeltaTimeSessionWindowConfiguration,
    deltaTimeSessionWindowConfiguration_timeoutInMinutes,

    -- * DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity (..),
    newDeviceRegistryEnrichActivity,
    deviceRegistryEnrichActivity_next,
    deviceRegistryEnrichActivity_name,
    deviceRegistryEnrichActivity_attribute,
    deviceRegistryEnrichActivity_thingName,
    deviceRegistryEnrichActivity_roleArn,

    -- * DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity (..),
    newDeviceShadowEnrichActivity,
    deviceShadowEnrichActivity_next,
    deviceShadowEnrichActivity_name,
    deviceShadowEnrichActivity_attribute,
    deviceShadowEnrichActivity_thingName,
    deviceShadowEnrichActivity_roleArn,

    -- * EstimatedResourceSize
    EstimatedResourceSize (..),
    newEstimatedResourceSize,
    estimatedResourceSize_estimatedOn,
    estimatedResourceSize_estimatedSizeInBytes,

    -- * FileFormatConfiguration
    FileFormatConfiguration (..),
    newFileFormatConfiguration,
    fileFormatConfiguration_jsonConfiguration,
    fileFormatConfiguration_parquetConfiguration,

    -- * FilterActivity
    FilterActivity (..),
    newFilterActivity,
    filterActivity_next,
    filterActivity_name,
    filterActivity_filter,

    -- * GlueConfiguration
    GlueConfiguration (..),
    newGlueConfiguration,
    glueConfiguration_tableName,
    glueConfiguration_databaseName,

    -- * IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration (..),
    newIotEventsDestinationConfiguration,
    iotEventsDestinationConfiguration_inputName,
    iotEventsDestinationConfiguration_roleArn,

    -- * IotSiteWiseCustomerManagedDatastoreS3Storage
    IotSiteWiseCustomerManagedDatastoreS3Storage (..),
    newIotSiteWiseCustomerManagedDatastoreS3Storage,
    iotSiteWiseCustomerManagedDatastoreS3Storage_keyPrefix,
    iotSiteWiseCustomerManagedDatastoreS3Storage_bucket,

    -- * IotSiteWiseCustomerManagedDatastoreS3StorageSummary
    IotSiteWiseCustomerManagedDatastoreS3StorageSummary (..),
    newIotSiteWiseCustomerManagedDatastoreS3StorageSummary,
    iotSiteWiseCustomerManagedDatastoreS3StorageSummary_bucket,
    iotSiteWiseCustomerManagedDatastoreS3StorageSummary_keyPrefix,

    -- * JsonConfiguration
    JsonConfiguration (..),
    newJsonConfiguration,

    -- * LambdaActivity
    LambdaActivity (..),
    newLambdaActivity,
    lambdaActivity_next,
    lambdaActivity_name,
    lambdaActivity_lambdaName,
    lambdaActivity_batchSize,

    -- * LateDataRule
    LateDataRule (..),
    newLateDataRule,
    lateDataRule_ruleName,
    lateDataRule_ruleConfiguration,

    -- * LateDataRuleConfiguration
    LateDataRuleConfiguration (..),
    newLateDataRuleConfiguration,
    lateDataRuleConfiguration_deltaTimeSessionWindowConfiguration,

    -- * LoggingOptions
    LoggingOptions (..),
    newLoggingOptions,
    loggingOptions_roleArn,
    loggingOptions_level,
    loggingOptions_enabled,

    -- * MathActivity
    MathActivity (..),
    newMathActivity,
    mathActivity_next,
    mathActivity_name,
    mathActivity_attribute,
    mathActivity_math,

    -- * Message
    Message (..),
    newMessage,
    message_messageId,
    message_payload,

    -- * OutputFileUriValue
    OutputFileUriValue (..),
    newOutputFileUriValue,
    outputFileUriValue_fileName,

    -- * ParquetConfiguration
    ParquetConfiguration (..),
    newParquetConfiguration,
    parquetConfiguration_schemaDefinition,

    -- * Partition
    Partition (..),
    newPartition,
    partition_attributeName,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_activities,
    pipeline_arn,
    pipeline_creationTime,
    pipeline_lastUpdateTime,
    pipeline_name,
    pipeline_reprocessingSummaries,

    -- * PipelineActivity
    PipelineActivity (..),
    newPipelineActivity,
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

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_creationTime,
    pipelineSummary_lastUpdateTime,
    pipelineSummary_pipelineName,
    pipelineSummary_reprocessingSummaries,

    -- * QueryFilter
    QueryFilter (..),
    newQueryFilter,
    queryFilter_deltaTime,

    -- * RemoveAttributesActivity
    RemoveAttributesActivity (..),
    newRemoveAttributesActivity,
    removeAttributesActivity_next,
    removeAttributesActivity_name,
    removeAttributesActivity_attributes,

    -- * ReprocessingSummary
    ReprocessingSummary (..),
    newReprocessingSummary,
    reprocessingSummary_creationTime,
    reprocessingSummary_id,
    reprocessingSummary_status,

    -- * ResourceConfiguration
    ResourceConfiguration (..),
    newResourceConfiguration,
    resourceConfiguration_computeType,
    resourceConfiguration_volumeSizeInGB,

    -- * RetentionPeriod
    RetentionPeriod (..),
    newRetentionPeriod,
    retentionPeriod_numberOfDays,
    retentionPeriod_unlimited,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    newS3DestinationConfiguration,
    s3DestinationConfiguration_glueConfiguration,
    s3DestinationConfiguration_bucket,
    s3DestinationConfiguration_key,
    s3DestinationConfiguration_roleArn,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_expression,

    -- * SchemaDefinition
    SchemaDefinition (..),
    newSchemaDefinition,
    schemaDefinition_columns,

    -- * SelectAttributesActivity
    SelectAttributesActivity (..),
    newSelectAttributesActivity,
    selectAttributesActivity_next,
    selectAttributesActivity_name,
    selectAttributesActivity_attributes,

    -- * ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage (..),
    newServiceManagedChannelS3Storage,

    -- * ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary (..),
    newServiceManagedChannelS3StorageSummary,

    -- * ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage (..),
    newServiceManagedDatastoreS3Storage,

    -- * ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary (..),
    newServiceManagedDatastoreS3StorageSummary,

    -- * SqlQueryDatasetAction
    SqlQueryDatasetAction (..),
    newSqlQueryDatasetAction,
    sqlQueryDatasetAction_filters,
    sqlQueryDatasetAction_sqlQuery,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimestampPartition
    TimestampPartition (..),
    newTimestampPartition,
    timestampPartition_timestampFormat,
    timestampPartition_attributeName,

    -- * TriggeringDataset
    TriggeringDataset (..),
    newTriggeringDataset,
    triggeringDataset_name,

    -- * Variable
    Variable (..),
    newVariable,
    variable_datasetContentVersionValue,
    variable_doubleValue,
    variable_outputFileUriValue,
    variable_stringValue,
    variable_name,

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    newVersioningConfiguration,
    versioningConfiguration_maxVersions,
    versioningConfiguration_unlimited,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.AddAttributesActivity
import Amazonka.IoTAnalytics.Types.BatchPutMessageErrorEntry
import Amazonka.IoTAnalytics.Types.Channel
import Amazonka.IoTAnalytics.Types.ChannelActivity
import Amazonka.IoTAnalytics.Types.ChannelMessages
import Amazonka.IoTAnalytics.Types.ChannelStatistics
import Amazonka.IoTAnalytics.Types.ChannelStatus
import Amazonka.IoTAnalytics.Types.ChannelStorage
import Amazonka.IoTAnalytics.Types.ChannelStorageSummary
import Amazonka.IoTAnalytics.Types.ChannelSummary
import Amazonka.IoTAnalytics.Types.Column
import Amazonka.IoTAnalytics.Types.ComputeType
import Amazonka.IoTAnalytics.Types.ContainerDatasetAction
import Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.Dataset
import Amazonka.IoTAnalytics.Types.DatasetAction
import Amazonka.IoTAnalytics.Types.DatasetActionSummary
import Amazonka.IoTAnalytics.Types.DatasetActionType
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryDestination
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryRule
import Amazonka.IoTAnalytics.Types.DatasetContentState
import Amazonka.IoTAnalytics.Types.DatasetContentStatus
import Amazonka.IoTAnalytics.Types.DatasetContentSummary
import Amazonka.IoTAnalytics.Types.DatasetContentVersionValue
import Amazonka.IoTAnalytics.Types.DatasetEntry
import Amazonka.IoTAnalytics.Types.DatasetStatus
import Amazonka.IoTAnalytics.Types.DatasetSummary
import Amazonka.IoTAnalytics.Types.DatasetTrigger
import Amazonka.IoTAnalytics.Types.Datastore
import Amazonka.IoTAnalytics.Types.DatastoreActivity
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorageSummary
import Amazonka.IoTAnalytics.Types.DatastorePartition
import Amazonka.IoTAnalytics.Types.DatastorePartitions
import Amazonka.IoTAnalytics.Types.DatastoreStatistics
import Amazonka.IoTAnalytics.Types.DatastoreStatus
import Amazonka.IoTAnalytics.Types.DatastoreStorage
import Amazonka.IoTAnalytics.Types.DatastoreStorageSummary
import Amazonka.IoTAnalytics.Types.DatastoreSummary
import Amazonka.IoTAnalytics.Types.DeltaTime
import Amazonka.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import Amazonka.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Amazonka.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Amazonka.IoTAnalytics.Types.EstimatedResourceSize
import Amazonka.IoTAnalytics.Types.FileFormatConfiguration
import Amazonka.IoTAnalytics.Types.FileFormatType
import Amazonka.IoTAnalytics.Types.FilterActivity
import Amazonka.IoTAnalytics.Types.GlueConfiguration
import Amazonka.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.IotSiteWiseCustomerManagedDatastoreS3StorageSummary
import Amazonka.IoTAnalytics.Types.JsonConfiguration
import Amazonka.IoTAnalytics.Types.LambdaActivity
import Amazonka.IoTAnalytics.Types.LateDataRule
import Amazonka.IoTAnalytics.Types.LateDataRuleConfiguration
import Amazonka.IoTAnalytics.Types.LoggingLevel
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
import Amazonka.IoTAnalytics.Types.ReprocessingStatus
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon IoT Analytics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTAnalytics",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "iotanalytics",
      Core.signingName = "iotanalytics",
      Core.version = "2017-11-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTAnalytics",
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

-- | There was an internal failure.
_InternalFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The request was not valid.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The command caused an internal limit to be exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410

-- | A resource with the same name already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | A resource with the specified name could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429
