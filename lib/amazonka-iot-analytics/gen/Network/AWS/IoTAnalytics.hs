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
-- Traditional analytics and business intelligence tools are designed to process structured data. IoT data often comes from devices that record noisy processes (such as temperature, motion, or sound). As a result the data from these devices can have significant gaps, corrupted messages, and false readings that must be cleaned up before analysis can occur. Also, IoT data is often only meaningful in the context of other data from external sources.
-- AWS IoT Analytics automates the steps required to analyze data from IoT devices. AWS IoT Analytics filters, transforms, and enriches IoT data before storing it in a time-series data store for analysis. You can set up the service to collect only the data you need from your devices, apply mathematical transforms to process the data, and enrich the data with device-specific metadata such as device type and location before storing it. Then, you can analyze your data by running queries using the built-in SQL query engine, or perform more complex analytics and machine learning inference. AWS IoT Analytics includes pre-built models for common IoT use cases so you can answer questions like which devices are about to fail or which customers are at risk of abandoning their wearable devices.
module Network.AWS.IoTAnalytics
  ( -- * Service configuration
    mkServiceConfig,

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

    -- ** DatasetActionType
    DatasetActionType (..),

    -- ** OutputFileUriValue
    OutputFileUriValue (..),
    mkOutputFileUriValue,
    ofuvFileName,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration (..),
    mkS3DestinationConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleArn,
    sdcGlueConfiguration,

    -- ** IotEventsInputName
    IotEventsInputName (..),

    -- ** DatasetContentStatus
    DatasetContentStatus (..),
    mkDatasetContentStatus,
    dcsReason,
    dcsState,

    -- ** CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage (..),
    mkCustomerManagedDatastoreS3Storage,
    cmdssBucket,
    cmdssRoleArn,
    cmdssKeyPrefix,

    -- ** PipelineActivity
    PipelineActivity (..),
    mkPipelineActivity,
    paAddAttributes,
    paChannel,
    paDatastore,
    paDeviceRegistryEnrich,
    paDeviceShadowEnrich,
    paFilter,
    paLambda,
    paMath,
    paRemoveAttributes,
    paSelectAttributes,

    -- ** CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary (..),
    mkCustomerManagedChannelS3StorageSummary,
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleArn,

    -- ** DatasetArn
    DatasetArn (..),

    -- ** Image
    Image (..),

    -- ** ChannelStatus
    ChannelStatus (..),

    -- ** S3KeyPrefix
    S3KeyPrefix (..),

    -- ** LambdaActivity
    LambdaActivity (..),
    mkLambdaActivity,
    laName,
    laLambdaName,
    laBatchSize,
    laNext,

    -- ** VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
    vcMaxVersions,
    vcUnlimited,

    -- ** ActivityName
    ActivityName (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    mkBatchPutMessageErrorEntry,
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,

    -- ** GlueTableName
    GlueTableName (..),

    -- ** LateDataRuleConfiguration
    LateDataRuleConfiguration (..),
    mkLateDataRuleConfiguration,
    ldrcDeltaTimeSessionWindowConfiguration,

    -- ** MathExpression
    MathExpression (..),

    -- ** DatastoreStatistics
    DatastoreStatistics (..),
    mkDatastoreStatistics,
    dsSize,

    -- ** FilterExpression
    FilterExpression (..),

    -- ** DatastoreActivity
    DatastoreActivity (..),
    mkDatastoreActivity,
    daName,
    daDatastoreName,

    -- ** QueryFilter
    QueryFilter (..),
    mkQueryFilter,
    qfDeltaTime,

    -- ** DatastoreStorageSummary
    DatastoreStorageSummary (..),
    mkDatastoreStorageSummary,
    dssCustomerManagedS3,
    dssServiceManagedS3,

    -- ** Dataset
    Dataset (..),
    mkDataset,
    dActions,
    dArn,
    dContentDeliveryRules,
    dCreationTime,
    dLastUpdateTime,
    dLateDataRules,
    dName,
    dRetentionPeriod,
    dStatus,
    dTriggers,
    dVersioningConfiguration,

    -- ** ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary (..),
    mkServiceManagedDatastoreS3StorageSummary,

    -- ** DatastoreArn
    DatastoreArn (..),

    -- ** ResourceConfiguration
    ResourceConfiguration (..),
    mkResourceConfiguration,
    rcComputeType,
    rcVolumeSizeInGB,

    -- ** FilterActivity
    FilterActivity (..),
    mkFilterActivity,
    faName,
    faFilter,
    faNext,

    -- ** EstimatedResourceSize
    EstimatedResourceSize (..),
    mkEstimatedResourceSize,
    ersEstimatedOn,
    ersEstimatedSizeInBytes,

    -- ** LateDataRuleName
    LateDataRuleName (..),

    -- ** LogResult
    LogResult (..),

    -- ** PipelineName
    PipelineName (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** MathActivity
    MathActivity (..),
    mkMathActivity,
    maName,
    maAttribute,
    maMath,
    maNext,

    -- ** Schedule
    Schedule (..),
    mkSchedule,
    sExpression,

    -- ** DeltaTime
    DeltaTime (..),
    mkDeltaTime,
    dtOffsetSeconds,
    dtTimeExpression,

    -- ** LambdaName
    LambdaName (..),

    -- ** DatastoreName
    DatastoreName (..),

    -- ** DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity (..),
    mkDeviceShadowEnrichActivity,
    dseaName,
    dseaAttribute,
    dseaThingName,
    dseaRoleArn,
    dseaNext,

    -- ** SqlQueryDatasetAction
    SqlQueryDatasetAction (..),
    mkSqlQueryDatasetAction,
    sqdaSqlQuery,
    sqdaFilters,

    -- ** PresignedURI
    PresignedURI (..),

    -- ** DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity (..),
    mkDeviceRegistryEnrichActivity,
    dreaName,
    dreaAttribute,
    dreaThingName,
    dreaRoleArn,
    dreaNext,

    -- ** DatastoreSummary
    DatastoreSummary (..),
    mkDatastoreSummary,
    dsCreationTime,
    dsDatastoreName,
    dsDatastoreStorage,
    dsLastMessageArrivalTime,
    dsLastUpdateTime,
    dsStatus,

    -- ** AddAttributesActivity
    AddAttributesActivity (..),
    mkAddAttributesActivity,
    aaaName,
    aaaAttributes,
    aaaNext,

    -- ** GlueConfiguration
    GlueConfiguration (..),
    mkGlueConfiguration,
    gcTableName,
    gcDatabaseName,

    -- ** Channel
    Channel (..),
    mkChannel,
    cArn,
    cCreationTime,
    cLastMessageArrivalTime,
    cLastUpdateTime,
    cName,
    cRetentionPeriod,
    cStatus,
    cStorage,

    -- ** DatasetActionName
    DatasetActionName (..),

    -- ** EntryName
    EntryName (..),

    -- ** ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary (..),
    mkServiceManagedChannelS3StorageSummary,

    -- ** ChannelActivity
    ChannelActivity (..),
    mkChannelActivity,
    caName,
    caChannelName,
    caNext,

    -- ** ChannelStatistics
    ChannelStatistics (..),
    mkChannelStatistics,
    csSize,

    -- ** Variable
    Variable (..),
    mkVariable,
    vName,
    vDatasetContentVersionValue,
    vDoubleValue,
    vOutputFileUriValue,
    vStringValue,

    -- ** ChannelArn
    ChannelArn (..),

    -- ** StringValue
    StringValue (..),

    -- ** PipelineSummary
    PipelineSummary (..),
    mkPipelineSummary,
    psCreationTime,
    psLastUpdateTime,
    psPipelineName,
    psReprocessingSummaries,

    -- ** ChannelStorageSummary
    ChannelStorageSummary (..),
    mkChannelStorageSummary,
    cssCustomerManagedS3,
    cssServiceManagedS3,

    -- ** DatasetEntry
    DatasetEntry (..),
    mkDatasetEntry,
    deDataURI,
    deEntryName,

    -- ** IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration (..),
    mkIotEventsDestinationConfiguration,
    iedcInputName,
    iedcRoleArn,

    -- ** DatasetContentVersion
    DatasetContentVersion (..),

    -- ** BucketName
    BucketName (..),

    -- ** RetentionPeriod
    RetentionPeriod (..),
    mkRetentionPeriod,
    rpNumberOfDays,
    rpUnlimited,

    -- ** ReprocessingId
    ReprocessingId (..),

    -- ** RemoveAttributesActivity
    RemoveAttributesActivity (..),
    mkRemoveAttributesActivity,
    raaName,
    raaAttributes,
    raaNext,

    -- ** SqlQuery
    SqlQuery (..),

    -- ** Reason
    Reason (..),

    -- ** NextToken
    NextToken (..),

    -- ** ChannelName
    ChannelName (..),

    -- ** DatasetContentState
    DatasetContentState (..),

    -- ** DatasetAction
    DatasetAction (..),
    mkDatasetAction,
    daActionName,
    daContainerAction,
    daQueryAction,

    -- ** Datastore
    Datastore (..),
    mkDatastore,
    dfArn,
    dfCreationTime,
    dfLastMessageArrivalTime,
    dfLastUpdateTime,
    dfName,
    dfRetentionPeriod,
    dfStatus,
    dfStorage,

    -- ** ResourceArn
    ResourceArn (..),

    -- ** DatasetContentSummary
    DatasetContentSummary (..),
    mkDatasetContentSummary,
    dcsCompletionTime,
    dcsCreationTime,
    dcsScheduleTime,
    dcsStatus,
    dcsVersion,

    -- ** DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration (..),
    mkDeltaTimeSessionWindowConfiguration,
    dtswcTimeoutInMinutes,

    -- ** TimeExpression
    TimeExpression (..),

    -- ** ChannelSummary
    ChannelSummary (..),
    mkChannelSummary,
    csChannelName,
    csChannelStorage,
    csCreationTime,
    csLastMessageArrivalTime,
    csLastUpdateTime,
    csStatus,

    -- ** PipelineArn
    PipelineArn (..),

    -- ** ChannelStorage
    ChannelStorage (..),
    mkChannelStorage,
    csCustomerManagedS3,
    csServiceManagedS3,

    -- ** DatasetTrigger
    DatasetTrigger (..),
    mkDatasetTrigger,
    dtDataset,
    dtSchedule,

    -- ** DatasetContentVersionValue
    DatasetContentVersionValue (..),
    mkDatasetContentVersionValue,
    dcvvDatasetName,

    -- ** Pipeline
    Pipeline (..),
    mkPipeline,
    pActivities,
    pArn,
    pCreationTime,
    pLastUpdateTime,
    pName,
    pReprocessingSummaries,

    -- ** SelectAttributesActivity
    SelectAttributesActivity (..),
    mkSelectAttributesActivity,
    saaName,
    saaAttributes,
    saaNext,

    -- ** LateDataRule
    LateDataRule (..),
    mkLateDataRule,
    ldrRuleConfiguration,
    ldrRuleName,

    -- ** TriggeringDataset
    TriggeringDataset (..),
    mkTriggeringDataset,
    tdName,

    -- ** ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage (..),
    mkServiceManagedChannelS3Storage,

    -- ** ErrorCode
    ErrorCode (..),

    -- ** DatasetName
    DatasetName (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** TagKey
    TagKey (..),

    -- ** ReprocessingStatus
    ReprocessingStatus (..),

    -- ** ReprocessingSummary
    ReprocessingSummary (..),
    mkReprocessingSummary,
    rsCreationTime,
    rsId,
    rsStatus,

    -- ** DatasetActionSummary
    DatasetActionSummary (..),
    mkDatasetActionSummary,
    dasActionName,
    dasActionType,

    -- ** LoggingOptions
    LoggingOptions (..),
    mkLoggingOptions,
    loRoleArn,
    loLevel,
    loEnabled,

    -- ** DatastoreStatus
    DatastoreStatus (..),

    -- ** ErrorMessage
    ErrorMessage (..),

    -- ** Message
    Message (..),
    mkMessage,
    mMessageId,
    mPayload,

    -- ** AttributeName
    AttributeName (..),

    -- ** CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage (..),
    mkCustomerManagedChannelS3Storage,
    cmcssBucket,
    cmcssRoleArn,
    cmcssKeyPrefix,

    -- ** DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination (..),
    mkDatasetContentDeliveryDestination,
    dcddIotEventsDestinationConfiguration,
    dcddS3DestinationConfiguration,

    -- ** ContainerDatasetAction
    ContainerDatasetAction (..),
    mkContainerDatasetAction,
    cdaImage,
    cdaExecutionRoleArn,
    cdaResourceConfiguration,
    cdaVariables,

    -- ** CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary (..),
    mkCustomerManagedDatastoreS3StorageSummary,
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleArn,

    -- ** ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage (..),
    mkServiceManagedDatastoreS3Storage,

    -- ** DatastoreStorage
    DatastoreStorage (..),
    mkDatastoreStorage,
    dsCustomerManagedS3,
    dsServiceManagedS3,

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** MessageId
    MessageId (..),

    -- ** DatasetSummary
    DatasetSummary (..),
    mkDatasetSummary,
    dsfActions,
    dsfCreationTime,
    dsfDatasetName,
    dsfLastUpdateTime,
    dsfStatus,
    dsfTriggers,

    -- ** DatasetContentDeliveryRule
    DatasetContentDeliveryRule (..),
    mkDatasetContentDeliveryRule,
    dcdrDestination,
    dcdrEntryName,

    -- ** RoleArn
    RoleArn (..),

    -- ** FileName
    FileName (..),

    -- ** Bucket
    Bucket (..),

    -- ** Key
    Key (..),

    -- ** KeyPrefix
    KeyPrefix (..),

    -- ** Name
    Name (..),

    -- ** Next
    Next (..),

    -- ** Value
    Value (..),

    -- ** Attribute
    Attribute (..),

    -- ** Expression
    Expression (..),

    -- ** ThingName
    ThingName (..),

    -- ** VersionId
    VersionId (..),

    -- ** DatabaseName
    DatabaseName (..),

    -- ** Arn
    Arn (..),

    -- ** ExecutionRoleArn
    ExecutionRoleArn (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
