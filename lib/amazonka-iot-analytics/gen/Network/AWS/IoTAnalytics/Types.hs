{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types
  ( -- * Service Configuration
    ioTAnalytics,

    -- * Errors

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

    -- * LoggingLevel
    LoggingLevel (..),

    -- * ReprocessingStatus
    ReprocessingStatus (..),

    -- * AddAttributesActivity
    AddAttributesActivity,
    addAttributesActivity,
    aaaNext,
    aaaName,
    aaaAttributes,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry,
    batchPutMessageErrorEntry,
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,

    -- * Channel
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

    -- * ChannelActivity
    ChannelActivity,
    channelActivity,
    caNext,
    caName,
    caChannelName,

    -- * ChannelStatistics
    ChannelStatistics,
    channelStatistics,
    csSize,

    -- * ChannelStorage
    ChannelStorage,
    channelStorage,
    csServiceManagedS3,
    csCustomerManagedS3,

    -- * ChannelStorageSummary
    ChannelStorageSummary,
    channelStorageSummary,
    cssServiceManagedS3,
    cssCustomerManagedS3,

    -- * ChannelSummary
    ChannelSummary,
    channelSummary,
    csCreationTime,
    csStatus,
    csLastMessageArrivalTime,
    csChannelName,
    csChannelStorage,
    csLastUpdateTime,

    -- * ContainerDatasetAction
    ContainerDatasetAction,
    containerDatasetAction,
    cdaVariables,
    cdaImage,
    cdaExecutionRoleARN,
    cdaResourceConfiguration,

    -- * CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage,
    customerManagedChannelS3Storage,
    cmcssKeyPrefix,
    cmcssBucket,
    cmcssRoleARN,

    -- * CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary,
    customerManagedChannelS3StorageSummary,
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleARN,

    -- * CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage,
    customerManagedDatastoreS3Storage,
    cmdssKeyPrefix,
    cmdssBucket,
    cmdssRoleARN,

    -- * CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary,
    customerManagedDatastoreS3StorageSummary,
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleARN,

    -- * Dataset
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

    -- * DatasetAction
    DatasetAction,
    datasetAction,
    daQueryAction,
    daActionName,
    daContainerAction,

    -- * DatasetActionSummary
    DatasetActionSummary,
    datasetActionSummary,
    dasActionName,
    dasActionType,

    -- * DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination,
    datasetContentDeliveryDestination,
    dcddS3DestinationConfiguration,
    dcddIotEventsDestinationConfiguration,

    -- * DatasetContentDeliveryRule
    DatasetContentDeliveryRule,
    datasetContentDeliveryRule,
    dcdrEntryName,
    dcdrDestination,

    -- * DatasetContentStatus
    DatasetContentStatus,
    datasetContentStatus,
    dcsState,
    dcsReason,

    -- * DatasetContentSummary
    DatasetContentSummary,
    datasetContentSummary,
    dcsCreationTime,
    dcsStatus,
    dcsScheduleTime,
    dcsCompletionTime,
    dcsVersion,

    -- * DatasetContentVersionValue
    DatasetContentVersionValue,
    datasetContentVersionValue,
    dcvvDatasetName,

    -- * DatasetEntry
    DatasetEntry,
    datasetEntry,
    deEntryName,
    deDataURI,

    -- * DatasetSummary
    DatasetSummary,
    datasetSummary,
    dssCreationTime,
    dssStatus,
    dssActions,
    dssTriggers,
    dssDatasetName,
    dssLastUpdateTime,

    -- * DatasetTrigger
    DatasetTrigger,
    datasetTrigger,
    dtDataset,
    dtSchedule,

    -- * Datastore
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

    -- * DatastoreActivity
    DatastoreActivity,
    datastoreActivity,
    daName,
    daDatastoreName,

    -- * DatastoreStatistics
    DatastoreStatistics,
    datastoreStatistics,
    dsSize,

    -- * DatastoreStorage
    DatastoreStorage,
    datastoreStorage,
    dsServiceManagedS3,
    dsCustomerManagedS3,

    -- * DatastoreStorageSummary
    DatastoreStorageSummary,
    datastoreStorageSummary,
    dssServiceManagedS3,
    dssCustomerManagedS3,

    -- * DatastoreSummary
    DatastoreSummary,
    datastoreSummary,
    dsCreationTime,
    dsStatus,
    dsLastMessageArrivalTime,
    dsDatastoreName,
    dsLastUpdateTime,
    dsDatastoreStorage,

    -- * DeltaTime
    DeltaTime,
    deltaTime,
    dtOffsetSeconds,
    dtTimeExpression,

    -- * DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration,
    deltaTimeSessionWindowConfiguration,
    dtswcTimeoutInMinutes,

    -- * DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity,
    deviceRegistryEnrichActivity,
    dreaNext,
    dreaName,
    dreaAttribute,
    dreaThingName,
    dreaRoleARN,

    -- * DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity,
    deviceShadowEnrichActivity,
    dseaNext,
    dseaName,
    dseaAttribute,
    dseaThingName,
    dseaRoleARN,

    -- * EstimatedResourceSize
    EstimatedResourceSize,
    estimatedResourceSize,
    ersEstimatedOn,
    ersEstimatedSizeInBytes,

    -- * FilterActivity
    FilterActivity,
    filterActivity,
    faNext,
    faName,
    faFilter,

    -- * GlueConfiguration
    GlueConfiguration,
    glueConfiguration,
    gcTableName,
    gcDatabaseName,

    -- * IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration,
    iotEventsDestinationConfiguration,
    iedcInputName,
    iedcRoleARN,

    -- * LambdaActivity
    LambdaActivity,
    lambdaActivity,
    laNext,
    laName,
    laLambdaName,
    laBatchSize,

    -- * LateDataRule
    LateDataRule,
    lateDataRule,
    ldrRuleName,
    ldrRuleConfiguration,

    -- * LateDataRuleConfiguration
    LateDataRuleConfiguration,
    lateDataRuleConfiguration,
    ldrcDeltaTimeSessionWindowConfiguration,

    -- * LoggingOptions
    LoggingOptions,
    loggingOptions,
    loRoleARN,
    loLevel,
    loEnabled,

    -- * MathActivity
    MathActivity,
    mathActivity,
    maNext,
    maName,
    maAttribute,
    maMath,

    -- * Message
    Message,
    message,
    mMessageId,
    mPayload,

    -- * OutputFileURIValue
    OutputFileURIValue,
    outputFileURIValue,
    ofuvFileName,

    -- * Pipeline
    Pipeline,
    pipeline,
    pCreationTime,
    pArn,
    pActivities,
    pName,
    pReprocessingSummaries,
    pLastUpdateTime,

    -- * PipelineActivity
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

    -- * PipelineSummary
    PipelineSummary,
    pipelineSummary,
    psCreationTime,
    psPipelineName,
    psReprocessingSummaries,
    psLastUpdateTime,

    -- * QueryFilter
    QueryFilter,
    queryFilter,
    qfDeltaTime,

    -- * RemoveAttributesActivity
    RemoveAttributesActivity,
    removeAttributesActivity,
    raaNext,
    raaName,
    raaAttributes,

    -- * ReprocessingSummary
    ReprocessingSummary,
    reprocessingSummary,
    rsCreationTime,
    rsStatus,
    rsId,

    -- * ResourceConfiguration
    ResourceConfiguration,
    resourceConfiguration,
    rcComputeType,
    rcVolumeSizeInGB,

    -- * RetentionPeriod
    RetentionPeriod,
    retentionPeriod,
    rpUnlimited,
    rpNumberOfDays,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration,
    s3DestinationConfiguration,
    sdcGlueConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleARN,

    -- * Schedule
    Schedule,
    schedule,
    sExpression,

    -- * SelectAttributesActivity
    SelectAttributesActivity,
    selectAttributesActivity,
    saaNext,
    saaName,
    saaAttributes,

    -- * ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage,
    serviceManagedChannelS3Storage,

    -- * ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary,
    serviceManagedChannelS3StorageSummary,

    -- * ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage,
    serviceManagedDatastoreS3Storage,

    -- * ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary,
    serviceManagedDatastoreS3StorageSummary,

    -- * SqlQueryDatasetAction
    SqlQueryDatasetAction,
    sqlQueryDatasetAction,
    sqdaFilters,
    sqdaSqlQuery,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TriggeringDataset
    TriggeringDataset,
    triggeringDataset,
    tdName,

    -- * Variable
    Variable,
    variable,
    vOutputFileURIValue,
    vDoubleValue,
    vStringValue,
    vDatasetContentVersionValue,
    vName,

    -- * VersioningConfiguration
    VersioningConfiguration,
    versioningConfiguration,
    vcUnlimited,
    vcMaxVersions,
  )
where

import Network.AWS.IoTAnalytics.Types.AddAttributesActivity
import Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
import Network.AWS.IoTAnalytics.Types.Channel
import Network.AWS.IoTAnalytics.Types.ChannelActivity
import Network.AWS.IoTAnalytics.Types.ChannelStatistics
import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorage
import Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
import Network.AWS.IoTAnalytics.Types.ChannelSummary
import Network.AWS.IoTAnalytics.Types.ComputeType
import Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.Dataset
import Network.AWS.IoTAnalytics.Types.DatasetAction
import Network.AWS.IoTAnalytics.Types.DatasetActionSummary
import Network.AWS.IoTAnalytics.Types.DatasetActionType
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
import Network.AWS.IoTAnalytics.Types.DatasetContentState
import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import Network.AWS.IoTAnalytics.Types.DatasetContentSummary
import Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
import Network.AWS.IoTAnalytics.Types.DatasetEntry
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetSummary
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.IoTAnalytics.Types.Datastore
import Network.AWS.IoTAnalytics.Types.DatastoreActivity
import Network.AWS.IoTAnalytics.Types.DatastoreStatistics
import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
import Network.AWS.IoTAnalytics.Types.DatastoreSummary
import Network.AWS.IoTAnalytics.Types.DeltaTime
import Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import Network.AWS.IoTAnalytics.Types.FilterActivity
import Network.AWS.IoTAnalytics.Types.GlueConfiguration
import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.LambdaActivity
import Network.AWS.IoTAnalytics.Types.LateDataRule
import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import Network.AWS.IoTAnalytics.Types.LoggingLevel
import Network.AWS.IoTAnalytics.Types.LoggingOptions
import Network.AWS.IoTAnalytics.Types.MathActivity
import Network.AWS.IoTAnalytics.Types.Message
import Network.AWS.IoTAnalytics.Types.OutputFileURIValue
import Network.AWS.IoTAnalytics.Types.Pipeline
import Network.AWS.IoTAnalytics.Types.PipelineActivity
import Network.AWS.IoTAnalytics.Types.PipelineSummary
import Network.AWS.IoTAnalytics.Types.QueryFilter
import Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import Network.AWS.IoTAnalytics.Types.ResourceConfiguration
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import Network.AWS.IoTAnalytics.Types.Schedule
import Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
import Network.AWS.IoTAnalytics.Types.Tag
import Network.AWS.IoTAnalytics.Types.TriggeringDataset
import Network.AWS.IoTAnalytics.Types.Variable
import Network.AWS.IoTAnalytics.Types.VersioningConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon IoT Analytics SDK configuration.
ioTAnalytics :: Service
ioTAnalytics =
  Service
    { _svcAbbrev = "IoTAnalytics",
      _svcSigner = v4,
      _svcPrefix = "iotanalytics",
      _svcVersion = "2017-11-27",
      _svcEndpoint = defaultEndpoint ioTAnalytics,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "IoTAnalytics",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
