{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT Analytics allows you to collect large amounts of device data, process messages, and store them. You can then query the data and run sophisticated analytics on it. AWS IoT Analytics enables advanced data exploration through integration with Jupyter Notebooks and data visualization through integration with Amazon QuickSight.
--
--
-- Traditional analytics and business intelligence tools are designed to process structured data. IoT data often comes from devices that record noisy processes (such as temperature, motion, or sound). As a result the data from these devices can have significant gaps, corrupted messages, and false readings that must be cleaned up before analysis can occur. Also, IoT data is often only meaningful in the context of other data from external sources.
--
-- AWS IoT Analytics automates the steps required to analyze data from IoT devices. AWS IoT Analytics filters, transforms, and enriches IoT data before storing it in a time-series data store for analysis. You can set up the service to collect only the data you need from your devices, apply mathematical transforms to process the data, and enrich the data with device-specific metadata such as device type and location before storing it. Then, you can analyze your data by running queries using the built-in SQL query engine, or perform more complex analytics and machine learning inference. AWS IoT Analytics includes pre-built models for common IoT use cases so you can answer questions like which devices are about to fail or which customers are at risk of abandoning their wearable devices.
module Network.AWS.IoTAnalytics
  ( -- * Service Configuration
    ioTAnalytics,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePipeline
    module Network.AWS.IoTAnalytics.DescribePipeline,

    -- ** DescribeDataset
    module Network.AWS.IoTAnalytics.DescribeDataset,

    -- ** ListChannels (Paginated)
    module Network.AWS.IoTAnalytics.ListChannels,

    -- ** ListDatasetContents (Paginated)
    module Network.AWS.IoTAnalytics.ListDatasetContents,

    -- ** PutLoggingOptions
    module Network.AWS.IoTAnalytics.PutLoggingOptions,

    -- ** ListTagsForResource
    module Network.AWS.IoTAnalytics.ListTagsForResource,

    -- ** DeleteChannel
    module Network.AWS.IoTAnalytics.DeleteChannel,

    -- ** UpdateChannel
    module Network.AWS.IoTAnalytics.UpdateChannel,

    -- ** SampleChannelData
    module Network.AWS.IoTAnalytics.SampleChannelData,

    -- ** CancelPipelineReprocessing
    module Network.AWS.IoTAnalytics.CancelPipelineReprocessing,

    -- ** CreateDatastore
    module Network.AWS.IoTAnalytics.CreateDatastore,

    -- ** UpdatePipeline
    module Network.AWS.IoTAnalytics.UpdatePipeline,

    -- ** DeletePipeline
    module Network.AWS.IoTAnalytics.DeletePipeline,

    -- ** DeleteDataset
    module Network.AWS.IoTAnalytics.DeleteDataset,

    -- ** UpdateDataset
    module Network.AWS.IoTAnalytics.UpdateDataset,

    -- ** ListPipelines (Paginated)
    module Network.AWS.IoTAnalytics.ListPipelines,

    -- ** DeleteDatastore
    module Network.AWS.IoTAnalytics.DeleteDatastore,

    -- ** UpdateDatastore
    module Network.AWS.IoTAnalytics.UpdateDatastore,

    -- ** CreateDataset
    module Network.AWS.IoTAnalytics.CreateDataset,

    -- ** BatchPutMessage
    module Network.AWS.IoTAnalytics.BatchPutMessage,

    -- ** ListDatastores (Paginated)
    module Network.AWS.IoTAnalytics.ListDatastores,

    -- ** CreateDatasetContent
    module Network.AWS.IoTAnalytics.CreateDatasetContent,

    -- ** CreateChannel
    module Network.AWS.IoTAnalytics.CreateChannel,

    -- ** DeleteDatasetContent
    module Network.AWS.IoTAnalytics.DeleteDatasetContent,

    -- ** DescribeDatastore
    module Network.AWS.IoTAnalytics.DescribeDatastore,

    -- ** GetDatasetContent
    module Network.AWS.IoTAnalytics.GetDatasetContent,

    -- ** TagResource
    module Network.AWS.IoTAnalytics.TagResource,

    -- ** ListDatasets (Paginated)
    module Network.AWS.IoTAnalytics.ListDatasets,

    -- ** UntagResource
    module Network.AWS.IoTAnalytics.UntagResource,

    -- ** RunPipelineActivity
    module Network.AWS.IoTAnalytics.RunPipelineActivity,

    -- ** DescribeChannel
    module Network.AWS.IoTAnalytics.DescribeChannel,

    -- ** CreatePipeline
    module Network.AWS.IoTAnalytics.CreatePipeline,

    -- ** StartPipelineReprocessing
    module Network.AWS.IoTAnalytics.StartPipelineReprocessing,

    -- ** DescribeLoggingOptions
    module Network.AWS.IoTAnalytics.DescribeLoggingOptions,

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

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** ReprocessingStatus
    ReprocessingStatus (..),

    -- ** AddAttributesActivity
    AddAttributesActivity,
    addAttributesActivity,
    aaaNext,
    aaaName,
    aaaAttributes,

    -- ** BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry,
    batchPutMessageErrorEntry,
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,

    -- ** Channel
    Channel,
    channel,
    cCreationTime,
    cStatus,
    cLastMessageArrivalTime,
    cArn,
    cStorage,
    cRetentionPeriod,
    cName,
    cLastUpdateTime,

    -- ** ChannelActivity
    ChannelActivity,
    channelActivity,
    caNext,
    caName,
    caChannelName,

    -- ** ChannelStatistics
    ChannelStatistics,
    channelStatistics,
    csSize,

    -- ** ChannelStorage
    ChannelStorage,
    channelStorage,
    csServiceManagedS3,
    csCustomerManagedS3,

    -- ** ChannelStorageSummary
    ChannelStorageSummary,
    channelStorageSummary,
    cssServiceManagedS3,
    cssCustomerManagedS3,

    -- ** ChannelSummary
    ChannelSummary,
    channelSummary,
    csCreationTime,
    csStatus,
    csLastMessageArrivalTime,
    csChannelName,
    csChannelStorage,
    csLastUpdateTime,

    -- ** ContainerDatasetAction
    ContainerDatasetAction,
    containerDatasetAction,
    cdaVariables,
    cdaImage,
    cdaExecutionRoleARN,
    cdaResourceConfiguration,

    -- ** CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage,
    customerManagedChannelS3Storage,
    cmcssKeyPrefix,
    cmcssBucket,
    cmcssRoleARN,

    -- ** CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary,
    customerManagedChannelS3StorageSummary,
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleARN,

    -- ** CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage,
    customerManagedDatastoreS3Storage,
    cmdssKeyPrefix,
    cmdssBucket,
    cmdssRoleARN,

    -- ** CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary,
    customerManagedDatastoreS3StorageSummary,
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleARN,

    -- ** Dataset
    Dataset,
    dataset,
    dCreationTime,
    dStatus,
    dVersioningConfiguration,
    dArn,
    dActions,
    dTriggers,
    dRetentionPeriod,
    dLateDataRules,
    dName,
    dContentDeliveryRules,
    dLastUpdateTime,

    -- ** DatasetAction
    DatasetAction,
    datasetAction,
    daQueryAction,
    daActionName,
    daContainerAction,

    -- ** DatasetActionSummary
    DatasetActionSummary,
    datasetActionSummary,
    dasActionName,
    dasActionType,

    -- ** DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination,
    datasetContentDeliveryDestination,
    dcddS3DestinationConfiguration,
    dcddIotEventsDestinationConfiguration,

    -- ** DatasetContentDeliveryRule
    DatasetContentDeliveryRule,
    datasetContentDeliveryRule,
    dcdrEntryName,
    dcdrDestination,

    -- ** DatasetContentStatus
    DatasetContentStatus,
    datasetContentStatus,
    dcsState,
    dcsReason,

    -- ** DatasetContentSummary
    DatasetContentSummary,
    datasetContentSummary,
    dcsCreationTime,
    dcsStatus,
    dcsScheduleTime,
    dcsCompletionTime,
    dcsVersion,

    -- ** DatasetContentVersionValue
    DatasetContentVersionValue,
    datasetContentVersionValue,
    dcvvDatasetName,

    -- ** DatasetEntry
    DatasetEntry,
    datasetEntry,
    deEntryName,
    deDataURI,

    -- ** DatasetSummary
    DatasetSummary,
    datasetSummary,
    dssCreationTime,
    dssStatus,
    dssActions,
    dssTriggers,
    dssDatasetName,
    dssLastUpdateTime,

    -- ** DatasetTrigger
    DatasetTrigger,
    datasetTrigger,
    dtDataset,
    dtSchedule,

    -- ** Datastore
    Datastore,
    datastore,
    datCreationTime,
    datStatus,
    datLastMessageArrivalTime,
    datArn,
    datStorage,
    datRetentionPeriod,
    datName,
    datLastUpdateTime,

    -- ** DatastoreActivity
    DatastoreActivity,
    datastoreActivity,
    daName,
    daDatastoreName,

    -- ** DatastoreStatistics
    DatastoreStatistics,
    datastoreStatistics,
    dsSize,

    -- ** DatastoreStorage
    DatastoreStorage,
    datastoreStorage,
    dsServiceManagedS3,
    dsCustomerManagedS3,

    -- ** DatastoreStorageSummary
    DatastoreStorageSummary,
    datastoreStorageSummary,
    dssServiceManagedS3,
    dssCustomerManagedS3,

    -- ** DatastoreSummary
    DatastoreSummary,
    datastoreSummary,
    dsCreationTime,
    dsStatus,
    dsLastMessageArrivalTime,
    dsDatastoreName,
    dsLastUpdateTime,
    dsDatastoreStorage,

    -- ** DeltaTime
    DeltaTime,
    deltaTime,
    dtOffsetSeconds,
    dtTimeExpression,

    -- ** DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration,
    deltaTimeSessionWindowConfiguration,
    dtswcTimeoutInMinutes,

    -- ** DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity,
    deviceRegistryEnrichActivity,
    dreaNext,
    dreaName,
    dreaAttribute,
    dreaThingName,
    dreaRoleARN,

    -- ** DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity,
    deviceShadowEnrichActivity,
    dseaNext,
    dseaName,
    dseaAttribute,
    dseaThingName,
    dseaRoleARN,

    -- ** EstimatedResourceSize
    EstimatedResourceSize,
    estimatedResourceSize,
    ersEstimatedOn,
    ersEstimatedSizeInBytes,

    -- ** FilterActivity
    FilterActivity,
    filterActivity,
    faNext,
    faName,
    faFilter,

    -- ** GlueConfiguration
    GlueConfiguration,
    glueConfiguration,
    gcTableName,
    gcDatabaseName,

    -- ** IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration,
    iotEventsDestinationConfiguration,
    iedcInputName,
    iedcRoleARN,

    -- ** LambdaActivity
    LambdaActivity,
    lambdaActivity,
    laNext,
    laName,
    laLambdaName,
    laBatchSize,

    -- ** LateDataRule
    LateDataRule,
    lateDataRule,
    ldrRuleName,
    ldrRuleConfiguration,

    -- ** LateDataRuleConfiguration
    LateDataRuleConfiguration,
    lateDataRuleConfiguration,
    ldrcDeltaTimeSessionWindowConfiguration,

    -- ** LoggingOptions
    LoggingOptions,
    loggingOptions,
    loRoleARN,
    loLevel,
    loEnabled,

    -- ** MathActivity
    MathActivity,
    mathActivity,
    maNext,
    maName,
    maAttribute,
    maMath,

    -- ** Message
    Message,
    message,
    mMessageId,
    mPayload,

    -- ** OutputFileURIValue
    OutputFileURIValue,
    outputFileURIValue,
    ofuvFileName,

    -- ** Pipeline
    Pipeline,
    pipeline,
    pCreationTime,
    pArn,
    pActivities,
    pName,
    pReprocessingSummaries,
    pLastUpdateTime,

    -- ** PipelineActivity
    PipelineActivity,
    pipelineActivity,
    paSelectAttributes,
    paChannel,
    paAddAttributes,
    paDeviceRegistryEnrich,
    paRemoveAttributes,
    paLambda,
    paDatastore,
    paDeviceShadowEnrich,
    paFilter,
    paMath,

    -- ** PipelineSummary
    PipelineSummary,
    pipelineSummary,
    psCreationTime,
    psPipelineName,
    psReprocessingSummaries,
    psLastUpdateTime,

    -- ** QueryFilter
    QueryFilter,
    queryFilter,
    qfDeltaTime,

    -- ** RemoveAttributesActivity
    RemoveAttributesActivity,
    removeAttributesActivity,
    raaNext,
    raaName,
    raaAttributes,

    -- ** ReprocessingSummary
    ReprocessingSummary,
    reprocessingSummary,
    rsCreationTime,
    rsStatus,
    rsId,

    -- ** ResourceConfiguration
    ResourceConfiguration,
    resourceConfiguration,
    rcComputeType,
    rcVolumeSizeInGB,

    -- ** RetentionPeriod
    RetentionPeriod,
    retentionPeriod,
    rpUnlimited,
    rpNumberOfDays,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration,
    s3DestinationConfiguration,
    sdcGlueConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleARN,

    -- ** Schedule
    Schedule,
    schedule,
    sExpression,

    -- ** SelectAttributesActivity
    SelectAttributesActivity,
    selectAttributesActivity,
    saaNext,
    saaName,
    saaAttributes,

    -- ** ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage,
    serviceManagedChannelS3Storage,

    -- ** ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary,
    serviceManagedChannelS3StorageSummary,

    -- ** ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage,
    serviceManagedDatastoreS3Storage,

    -- ** ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary,
    serviceManagedDatastoreS3StorageSummary,

    -- ** SqlQueryDatasetAction
    SqlQueryDatasetAction,
    sqlQueryDatasetAction,
    sqdaFilters,
    sqdaSqlQuery,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TriggeringDataset
    TriggeringDataset,
    triggeringDataset,
    tdName,

    -- ** Variable
    Variable,
    variable,
    vOutputFileURIValue,
    vDoubleValue,
    vStringValue,
    vDatasetContentVersionValue,
    vName,

    -- ** VersioningConfiguration
    VersioningConfiguration,
    versioningConfiguration,
    vcUnlimited,
    vcMaxVersions,
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
