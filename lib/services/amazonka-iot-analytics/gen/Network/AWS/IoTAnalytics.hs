{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoTAnalytics
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IoTAnalytics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePipeline
    DescribePipeline (DescribePipeline'),
    newDescribePipeline,
    DescribePipelineResponse (DescribePipelineResponse'),
    newDescribePipelineResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

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

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** SampleChannelData
    SampleChannelData (SampleChannelData'),
    newSampleChannelData,
    SampleChannelDataResponse (SampleChannelDataResponse'),
    newSampleChannelDataResponse,

    -- ** CancelPipelineReprocessing
    CancelPipelineReprocessing (CancelPipelineReprocessing'),
    newCancelPipelineReprocessing,
    CancelPipelineReprocessingResponse (CancelPipelineReprocessingResponse'),
    newCancelPipelineReprocessingResponse,

    -- ** CreateDatastore
    CreateDatastore (CreateDatastore'),
    newCreateDatastore,
    CreateDatastoreResponse (CreateDatastoreResponse'),
    newCreateDatastoreResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** UpdateDataset
    UpdateDataset (UpdateDataset'),
    newUpdateDataset,
    UpdateDatasetResponse (UpdateDatasetResponse'),
    newUpdateDatasetResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** DeleteDatastore
    DeleteDatastore (DeleteDatastore'),
    newDeleteDatastore,
    DeleteDatastoreResponse (DeleteDatastoreResponse'),
    newDeleteDatastoreResponse,

    -- ** UpdateDatastore
    UpdateDatastore (UpdateDatastore'),
    newUpdateDatastore,
    UpdateDatastoreResponse (UpdateDatastoreResponse'),
    newUpdateDatastoreResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** BatchPutMessage
    BatchPutMessage (BatchPutMessage'),
    newBatchPutMessage,
    BatchPutMessageResponse (BatchPutMessageResponse'),
    newBatchPutMessageResponse,

    -- ** ListDatastores (Paginated)
    ListDatastores (ListDatastores'),
    newListDatastores,
    ListDatastoresResponse (ListDatastoresResponse'),
    newListDatastoresResponse,

    -- ** CreateDatasetContent
    CreateDatasetContent (CreateDatasetContent'),
    newCreateDatasetContent,
    CreateDatasetContentResponse (CreateDatasetContentResponse'),
    newCreateDatasetContentResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** DeleteDatasetContent
    DeleteDatasetContent (DeleteDatasetContent'),
    newDeleteDatasetContent,
    DeleteDatasetContentResponse (DeleteDatasetContentResponse'),
    newDeleteDatasetContentResponse,

    -- ** DescribeDatastore
    DescribeDatastore (DescribeDatastore'),
    newDescribeDatastore,
    DescribeDatastoreResponse (DescribeDatastoreResponse'),
    newDescribeDatastoreResponse,

    -- ** GetDatasetContent
    GetDatasetContent (GetDatasetContent'),
    newGetDatasetContent,
    GetDatasetContentResponse (GetDatasetContentResponse'),
    newGetDatasetContentResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** RunPipelineActivity
    RunPipelineActivity (RunPipelineActivity'),
    newRunPipelineActivity,
    RunPipelineActivityResponse (RunPipelineActivityResponse'),
    newRunPipelineActivityResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** StartPipelineReprocessing
    StartPipelineReprocessing (StartPipelineReprocessing'),
    newStartPipelineReprocessing,
    StartPipelineReprocessingResponse (StartPipelineReprocessingResponse'),
    newStartPipelineReprocessingResponse,

    -- ** DescribeLoggingOptions
    DescribeLoggingOptions (DescribeLoggingOptions'),
    newDescribeLoggingOptions,
    DescribeLoggingOptionsResponse (DescribeLoggingOptionsResponse'),
    newDescribeLoggingOptionsResponse,

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

import Network.AWS.IoTAnalytics.BatchPutMessage
import Network.AWS.IoTAnalytics.CancelPipelineReprocessing
import Network.AWS.IoTAnalytics.CreateChannel
import Network.AWS.IoTAnalytics.CreateDataset
import Network.AWS.IoTAnalytics.CreateDatasetContent
import Network.AWS.IoTAnalytics.CreateDatastore
import Network.AWS.IoTAnalytics.CreatePipeline
import Network.AWS.IoTAnalytics.DeleteChannel
import Network.AWS.IoTAnalytics.DeleteDataset
import Network.AWS.IoTAnalytics.DeleteDatasetContent
import Network.AWS.IoTAnalytics.DeleteDatastore
import Network.AWS.IoTAnalytics.DeletePipeline
import Network.AWS.IoTAnalytics.DescribeChannel
import Network.AWS.IoTAnalytics.DescribeDataset
import Network.AWS.IoTAnalytics.DescribeDatastore
import Network.AWS.IoTAnalytics.DescribeLoggingOptions
import Network.AWS.IoTAnalytics.DescribePipeline
import Network.AWS.IoTAnalytics.GetDatasetContent
import Network.AWS.IoTAnalytics.Lens
import Network.AWS.IoTAnalytics.ListChannels
import Network.AWS.IoTAnalytics.ListDatasetContents
import Network.AWS.IoTAnalytics.ListDatasets
import Network.AWS.IoTAnalytics.ListDatastores
import Network.AWS.IoTAnalytics.ListPipelines
import Network.AWS.IoTAnalytics.ListTagsForResource
import Network.AWS.IoTAnalytics.PutLoggingOptions
import Network.AWS.IoTAnalytics.RunPipelineActivity
import Network.AWS.IoTAnalytics.SampleChannelData
import Network.AWS.IoTAnalytics.StartPipelineReprocessing
import Network.AWS.IoTAnalytics.TagResource
import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.UntagResource
import Network.AWS.IoTAnalytics.UpdateChannel
import Network.AWS.IoTAnalytics.UpdateDataset
import Network.AWS.IoTAnalytics.UpdateDatastore
import Network.AWS.IoTAnalytics.UpdatePipeline
import Network.AWS.IoTAnalytics.Waiters

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
