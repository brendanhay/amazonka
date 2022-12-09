{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTAnalytics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- IoT Analytics allows you to collect large amounts of device data,
-- process messages, and store them. You can then query the data and run
-- sophisticated analytics on it. IoT Analytics enables advanced data
-- exploration through integration with Jupyter Notebooks and data
-- visualization through integration with Amazon QuickSight.
--
-- Traditional analytics and business intelligence tools are designed to
-- process structured data. IoT data often comes from devices that record
-- noisy processes (such as temperature, motion, or sound). As a result the
-- data from these devices can have significant gaps, corrupted messages,
-- and false readings that must be cleaned up before analysis can occur.
-- Also, IoT data is often only meaningful in the context of other data
-- from external sources.
--
-- IoT Analytics automates the steps required to analyze data from IoT
-- devices. IoT Analytics filters, transforms, and enriches IoT data before
-- storing it in a time-series data store for analysis. You can set up the
-- service to collect only the data you need from your devices, apply
-- mathematical transforms to process the data, and enrich the data with
-- device-specific metadata such as device type and location before storing
-- it. Then, you can analyze your data by running queries using the
-- built-in SQL query engine, or perform more complex analytics and machine
-- learning inference. IoT Analytics includes pre-built models for common
-- IoT use cases so you can answer questions like which devices are about
-- to fail or which customers are at risk of abandoning their wearable
-- devices.
module Amazonka.IoTAnalytics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchPutMessage
    BatchPutMessage (BatchPutMessage'),
    newBatchPutMessage,
    BatchPutMessageResponse (BatchPutMessageResponse'),
    newBatchPutMessageResponse,

    -- ** CancelPipelineReprocessing
    CancelPipelineReprocessing (CancelPipelineReprocessing'),
    newCancelPipelineReprocessing,
    CancelPipelineReprocessingResponse (CancelPipelineReprocessingResponse'),
    newCancelPipelineReprocessingResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** CreateDatasetContent
    CreateDatasetContent (CreateDatasetContent'),
    newCreateDatasetContent,
    CreateDatasetContentResponse (CreateDatasetContentResponse'),
    newCreateDatasetContentResponse,

    -- ** CreateDatastore
    CreateDatastore (CreateDatastore'),
    newCreateDatastore,
    CreateDatastoreResponse (CreateDatastoreResponse'),
    newCreateDatastoreResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** DeleteDatasetContent
    DeleteDatasetContent (DeleteDatasetContent'),
    newDeleteDatasetContent,
    DeleteDatasetContentResponse (DeleteDatasetContentResponse'),
    newDeleteDatasetContentResponse,

    -- ** DeleteDatastore
    DeleteDatastore (DeleteDatastore'),
    newDeleteDatastore,
    DeleteDatastoreResponse (DeleteDatastoreResponse'),
    newDeleteDatastoreResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** DescribeDatastore
    DescribeDatastore (DescribeDatastore'),
    newDescribeDatastore,
    DescribeDatastoreResponse (DescribeDatastoreResponse'),
    newDescribeDatastoreResponse,

    -- ** DescribeLoggingOptions
    DescribeLoggingOptions (DescribeLoggingOptions'),
    newDescribeLoggingOptions,
    DescribeLoggingOptionsResponse (DescribeLoggingOptionsResponse'),
    newDescribeLoggingOptionsResponse,

    -- ** DescribePipeline
    DescribePipeline (DescribePipeline'),
    newDescribePipeline,
    DescribePipelineResponse (DescribePipelineResponse'),
    newDescribePipelineResponse,

    -- ** GetDatasetContent
    GetDatasetContent (GetDatasetContent'),
    newGetDatasetContent,
    GetDatasetContentResponse (GetDatasetContentResponse'),
    newGetDatasetContentResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListDatasetContents (Paginated)
    ListDatasetContents (ListDatasetContents'),
    newListDatasetContents,
    ListDatasetContentsResponse (ListDatasetContentsResponse'),
    newListDatasetContentsResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** ListDatastores (Paginated)
    ListDatastores (ListDatastores'),
    newListDatastores,
    ListDatastoresResponse (ListDatastoresResponse'),
    newListDatastoresResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** RunPipelineActivity
    RunPipelineActivity (RunPipelineActivity'),
    newRunPipelineActivity,
    RunPipelineActivityResponse (RunPipelineActivityResponse'),
    newRunPipelineActivityResponse,

    -- ** SampleChannelData
    SampleChannelData (SampleChannelData'),
    newSampleChannelData,
    SampleChannelDataResponse (SampleChannelDataResponse'),
    newSampleChannelDataResponse,

    -- ** StartPipelineReprocessing
    StartPipelineReprocessing (StartPipelineReprocessing'),
    newStartPipelineReprocessing,
    StartPipelineReprocessingResponse (StartPipelineReprocessingResponse'),
    newStartPipelineReprocessingResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** UpdateDataset
    UpdateDataset (UpdateDataset'),
    newUpdateDataset,
    UpdateDatasetResponse (UpdateDatasetResponse'),
    newUpdateDatasetResponse,

    -- ** UpdateDatastore
    UpdateDatastore (UpdateDatastore'),
    newUpdateDatastore,
    UpdateDatastoreResponse (UpdateDatastoreResponse'),
    newUpdateDatastoreResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- * Types

    -- ** ChannelStatus
    ChannelStatus (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** DatasetActionType
    DatasetActionType (..),

    -- ** DatasetContentState
    DatasetContentState (..),

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** DatastoreStatus
    DatastoreStatus (..),

    -- ** FileFormatType
    FileFormatType (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** ReprocessingStatus
    ReprocessingStatus (..),

    -- ** AddAttributesActivity
    AddAttributesActivity (AddAttributesActivity'),
    newAddAttributesActivity,

    -- ** BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (BatchPutMessageErrorEntry'),
    newBatchPutMessageErrorEntry,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelActivity
    ChannelActivity (ChannelActivity'),
    newChannelActivity,

    -- ** ChannelMessages
    ChannelMessages (ChannelMessages'),
    newChannelMessages,

    -- ** ChannelStatistics
    ChannelStatistics (ChannelStatistics'),
    newChannelStatistics,

    -- ** ChannelStorage
    ChannelStorage (ChannelStorage'),
    newChannelStorage,

    -- ** ChannelStorageSummary
    ChannelStorageSummary (ChannelStorageSummary'),
    newChannelStorageSummary,

    -- ** ChannelSummary
    ChannelSummary (ChannelSummary'),
    newChannelSummary,

    -- ** Column
    Column (Column'),
    newColumn,

    -- ** ContainerDatasetAction
    ContainerDatasetAction (ContainerDatasetAction'),
    newContainerDatasetAction,

    -- ** CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage (CustomerManagedChannelS3Storage'),
    newCustomerManagedChannelS3Storage,

    -- ** CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary (CustomerManagedChannelS3StorageSummary'),
    newCustomerManagedChannelS3StorageSummary,

    -- ** CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage (CustomerManagedDatastoreS3Storage'),
    newCustomerManagedDatastoreS3Storage,

    -- ** CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary (CustomerManagedDatastoreS3StorageSummary'),
    newCustomerManagedDatastoreS3StorageSummary,

    -- ** Dataset
    Dataset (Dataset'),
    newDataset,

    -- ** DatasetAction
    DatasetAction (DatasetAction'),
    newDatasetAction,

    -- ** DatasetActionSummary
    DatasetActionSummary (DatasetActionSummary'),
    newDatasetActionSummary,

    -- ** DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination (DatasetContentDeliveryDestination'),
    newDatasetContentDeliveryDestination,

    -- ** DatasetContentDeliveryRule
    DatasetContentDeliveryRule (DatasetContentDeliveryRule'),
    newDatasetContentDeliveryRule,

    -- ** DatasetContentStatus
    DatasetContentStatus (DatasetContentStatus'),
    newDatasetContentStatus,

    -- ** DatasetContentSummary
    DatasetContentSummary (DatasetContentSummary'),
    newDatasetContentSummary,

    -- ** DatasetContentVersionValue
    DatasetContentVersionValue (DatasetContentVersionValue'),
    newDatasetContentVersionValue,

    -- ** DatasetEntry
    DatasetEntry (DatasetEntry'),
    newDatasetEntry,

    -- ** DatasetSummary
    DatasetSummary (DatasetSummary'),
    newDatasetSummary,

    -- ** DatasetTrigger
    DatasetTrigger (DatasetTrigger'),
    newDatasetTrigger,

    -- ** Datastore
    Datastore (Datastore'),
    newDatastore,

    -- ** DatastoreActivity
    DatastoreActivity (DatastoreActivity'),
    newDatastoreActivity,

    -- ** DatastoreIotSiteWiseMultiLayerStorage
    DatastoreIotSiteWiseMultiLayerStorage (DatastoreIotSiteWiseMultiLayerStorage'),
    newDatastoreIotSiteWiseMultiLayerStorage,

    -- ** DatastoreIotSiteWiseMultiLayerStorageSummary
    DatastoreIotSiteWiseMultiLayerStorageSummary (DatastoreIotSiteWiseMultiLayerStorageSummary'),
    newDatastoreIotSiteWiseMultiLayerStorageSummary,

    -- ** DatastorePartition
    DatastorePartition (DatastorePartition'),
    newDatastorePartition,

    -- ** DatastorePartitions
    DatastorePartitions (DatastorePartitions'),
    newDatastorePartitions,

    -- ** DatastoreStatistics
    DatastoreStatistics (DatastoreStatistics'),
    newDatastoreStatistics,

    -- ** DatastoreStorage
    DatastoreStorage (DatastoreStorage'),
    newDatastoreStorage,

    -- ** DatastoreStorageSummary
    DatastoreStorageSummary (DatastoreStorageSummary'),
    newDatastoreStorageSummary,

    -- ** DatastoreSummary
    DatastoreSummary (DatastoreSummary'),
    newDatastoreSummary,

    -- ** DeltaTime
    DeltaTime (DeltaTime'),
    newDeltaTime,

    -- ** DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration (DeltaTimeSessionWindowConfiguration'),
    newDeltaTimeSessionWindowConfiguration,

    -- ** DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity (DeviceRegistryEnrichActivity'),
    newDeviceRegistryEnrichActivity,

    -- ** DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity (DeviceShadowEnrichActivity'),
    newDeviceShadowEnrichActivity,

    -- ** EstimatedResourceSize
    EstimatedResourceSize (EstimatedResourceSize'),
    newEstimatedResourceSize,

    -- ** FileFormatConfiguration
    FileFormatConfiguration (FileFormatConfiguration'),
    newFileFormatConfiguration,

    -- ** FilterActivity
    FilterActivity (FilterActivity'),
    newFilterActivity,

    -- ** GlueConfiguration
    GlueConfiguration (GlueConfiguration'),
    newGlueConfiguration,

    -- ** IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration (IotEventsDestinationConfiguration'),
    newIotEventsDestinationConfiguration,

    -- ** IotSiteWiseCustomerManagedDatastoreS3Storage
    IotSiteWiseCustomerManagedDatastoreS3Storage (IotSiteWiseCustomerManagedDatastoreS3Storage'),
    newIotSiteWiseCustomerManagedDatastoreS3Storage,

    -- ** IotSiteWiseCustomerManagedDatastoreS3StorageSummary
    IotSiteWiseCustomerManagedDatastoreS3StorageSummary (IotSiteWiseCustomerManagedDatastoreS3StorageSummary'),
    newIotSiteWiseCustomerManagedDatastoreS3StorageSummary,

    -- ** JsonConfiguration
    JsonConfiguration (JsonConfiguration'),
    newJsonConfiguration,

    -- ** LambdaActivity
    LambdaActivity (LambdaActivity'),
    newLambdaActivity,

    -- ** LateDataRule
    LateDataRule (LateDataRule'),
    newLateDataRule,

    -- ** LateDataRuleConfiguration
    LateDataRuleConfiguration (LateDataRuleConfiguration'),
    newLateDataRuleConfiguration,

    -- ** LoggingOptions
    LoggingOptions (LoggingOptions'),
    newLoggingOptions,

    -- ** MathActivity
    MathActivity (MathActivity'),
    newMathActivity,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** OutputFileUriValue
    OutputFileUriValue (OutputFileUriValue'),
    newOutputFileUriValue,

    -- ** ParquetConfiguration
    ParquetConfiguration (ParquetConfiguration'),
    newParquetConfiguration,

    -- ** Partition
    Partition (Partition'),
    newPartition,

    -- ** Pipeline
    Pipeline (Pipeline'),
    newPipeline,

    -- ** PipelineActivity
    PipelineActivity (PipelineActivity'),
    newPipelineActivity,

    -- ** PipelineSummary
    PipelineSummary (PipelineSummary'),
    newPipelineSummary,

    -- ** QueryFilter
    QueryFilter (QueryFilter'),
    newQueryFilter,

    -- ** RemoveAttributesActivity
    RemoveAttributesActivity (RemoveAttributesActivity'),
    newRemoveAttributesActivity,

    -- ** ReprocessingSummary
    ReprocessingSummary (ReprocessingSummary'),
    newReprocessingSummary,

    -- ** ResourceConfiguration
    ResourceConfiguration (ResourceConfiguration'),
    newResourceConfiguration,

    -- ** RetentionPeriod
    RetentionPeriod (RetentionPeriod'),
    newRetentionPeriod,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration (S3DestinationConfiguration'),
    newS3DestinationConfiguration,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** SchemaDefinition
    SchemaDefinition (SchemaDefinition'),
    newSchemaDefinition,

    -- ** SelectAttributesActivity
    SelectAttributesActivity (SelectAttributesActivity'),
    newSelectAttributesActivity,

    -- ** ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage (ServiceManagedChannelS3Storage'),
    newServiceManagedChannelS3Storage,

    -- ** ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary (ServiceManagedChannelS3StorageSummary'),
    newServiceManagedChannelS3StorageSummary,

    -- ** ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage (ServiceManagedDatastoreS3Storage'),
    newServiceManagedDatastoreS3Storage,

    -- ** ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary (ServiceManagedDatastoreS3StorageSummary'),
    newServiceManagedDatastoreS3StorageSummary,

    -- ** SqlQueryDatasetAction
    SqlQueryDatasetAction (SqlQueryDatasetAction'),
    newSqlQueryDatasetAction,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimestampPartition
    TimestampPartition (TimestampPartition'),
    newTimestampPartition,

    -- ** TriggeringDataset
    TriggeringDataset (TriggeringDataset'),
    newTriggeringDataset,

    -- ** Variable
    Variable (Variable'),
    newVariable,

    -- ** VersioningConfiguration
    VersioningConfiguration (VersioningConfiguration'),
    newVersioningConfiguration,
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
import Amazonka.IoTAnalytics.Lens
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
import Amazonka.IoTAnalytics.Types
import Amazonka.IoTAnalytics.UntagResource
import Amazonka.IoTAnalytics.UpdateChannel
import Amazonka.IoTAnalytics.UpdateDataset
import Amazonka.IoTAnalytics.UpdateDatastore
import Amazonka.IoTAnalytics.UpdatePipeline
import Amazonka.IoTAnalytics.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTAnalytics'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
