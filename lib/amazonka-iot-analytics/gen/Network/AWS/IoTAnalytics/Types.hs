-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types
  ( -- * Service configuration
    ioTAnalyticsService,

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
    AddAttributesActivity (..),
    mkAddAttributesActivity,
    aaaNext,
    aaaName,
    aaaAttributes,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    mkBatchPutMessageErrorEntry,
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,

    -- * Channel
    Channel (..),
    mkChannel,
    cCreationTime,
    cStatus,
    cLastMessageArrivalTime,
    cArn,
    cStorage,
    cRetentionPeriod,
    cName,
    cLastUpdateTime,

    -- * ChannelActivity
    ChannelActivity (..),
    mkChannelActivity,
    caNext,
    caName,
    caChannelName,

    -- * ChannelStatistics
    ChannelStatistics (..),
    mkChannelStatistics,
    csSize,

    -- * ChannelStorage
    ChannelStorage (..),
    mkChannelStorage,
    csServiceManagedS3,
    csCustomerManagedS3,

    -- * ChannelStorageSummary
    ChannelStorageSummary (..),
    mkChannelStorageSummary,
    cssServiceManagedS3,
    cssCustomerManagedS3,

    -- * ChannelSummary
    ChannelSummary (..),
    mkChannelSummary,
    csCreationTime,
    csStatus,
    csLastMessageArrivalTime,
    csChannelName,
    csChannelStorage,
    csLastUpdateTime,

    -- * ContainerDatasetAction
    ContainerDatasetAction (..),
    mkContainerDatasetAction,
    cdaVariables,
    cdaImage,
    cdaExecutionRoleARN,
    cdaResourceConfiguration,

    -- * CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage (..),
    mkCustomerManagedChannelS3Storage,
    cmcssKeyPrefix,
    cmcssBucket,
    cmcssRoleARN,

    -- * CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary (..),
    mkCustomerManagedChannelS3StorageSummary,
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleARN,

    -- * CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage (..),
    mkCustomerManagedDatastoreS3Storage,
    cmdssKeyPrefix,
    cmdssBucket,
    cmdssRoleARN,

    -- * CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary (..),
    mkCustomerManagedDatastoreS3StorageSummary,
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleARN,

    -- * Dataset
    Dataset (..),
    mkDataset,
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
    DatasetAction (..),
    mkDatasetAction,
    daQueryAction,
    daActionName,
    daContainerAction,

    -- * DatasetActionSummary
    DatasetActionSummary (..),
    mkDatasetActionSummary,
    dasActionName,
    dasActionType,

    -- * DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination (..),
    mkDatasetContentDeliveryDestination,
    dcddS3DestinationConfiguration,
    dcddIotEventsDestinationConfiguration,

    -- * DatasetContentDeliveryRule
    DatasetContentDeliveryRule (..),
    mkDatasetContentDeliveryRule,
    dcdrEntryName,
    dcdrDestination,

    -- * DatasetContentStatus
    DatasetContentStatus (..),
    mkDatasetContentStatus,
    dcsState,
    dcsReason,

    -- * DatasetContentSummary
    DatasetContentSummary (..),
    mkDatasetContentSummary,
    dcsCreationTime,
    dcsStatus,
    dcsScheduleTime,
    dcsCompletionTime,
    dcsVersion,

    -- * DatasetContentVersionValue
    DatasetContentVersionValue (..),
    mkDatasetContentVersionValue,
    dcvvDatasetName,

    -- * DatasetEntry
    DatasetEntry (..),
    mkDatasetEntry,
    deEntryName,
    deDataURI,

    -- * DatasetSummary
    DatasetSummary (..),
    mkDatasetSummary,
    dssCreationTime,
    dssStatus,
    dssActions,
    dssTriggers,
    dssDatasetName,
    dssLastUpdateTime,

    -- * DatasetTrigger
    DatasetTrigger (..),
    mkDatasetTrigger,
    dtDataset,
    dtSchedule,

    -- * Datastore
    Datastore (..),
    mkDatastore,
    datCreationTime,
    datStatus,
    datLastMessageArrivalTime,
    datArn,
    datStorage,
    datRetentionPeriod,
    datName,
    datLastUpdateTime,

    -- * DatastoreActivity
    DatastoreActivity (..),
    mkDatastoreActivity,
    daName,
    daDatastoreName,

    -- * DatastoreStatistics
    DatastoreStatistics (..),
    mkDatastoreStatistics,
    dsSize,

    -- * DatastoreStorage
    DatastoreStorage (..),
    mkDatastoreStorage,
    dsServiceManagedS3,
    dsCustomerManagedS3,

    -- * DatastoreStorageSummary
    DatastoreStorageSummary (..),
    mkDatastoreStorageSummary,
    dssServiceManagedS3,
    dssCustomerManagedS3,

    -- * DatastoreSummary
    DatastoreSummary (..),
    mkDatastoreSummary,
    dsCreationTime,
    dsStatus,
    dsLastMessageArrivalTime,
    dsDatastoreName,
    dsLastUpdateTime,
    dsDatastoreStorage,

    -- * DeltaTime
    DeltaTime (..),
    mkDeltaTime,
    dtOffsetSeconds,
    dtTimeExpression,

    -- * DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration (..),
    mkDeltaTimeSessionWindowConfiguration,
    dtswcTimeoutInMinutes,

    -- * DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity (..),
    mkDeviceRegistryEnrichActivity,
    dreaNext,
    dreaName,
    dreaAttribute,
    dreaThingName,
    dreaRoleARN,

    -- * DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity (..),
    mkDeviceShadowEnrichActivity,
    dseaNext,
    dseaName,
    dseaAttribute,
    dseaThingName,
    dseaRoleARN,

    -- * EstimatedResourceSize
    EstimatedResourceSize (..),
    mkEstimatedResourceSize,
    ersEstimatedOn,
    ersEstimatedSizeInBytes,

    -- * FilterActivity
    FilterActivity (..),
    mkFilterActivity,
    faNext,
    faName,
    faFilter,

    -- * GlueConfiguration
    GlueConfiguration (..),
    mkGlueConfiguration,
    gcTableName,
    gcDatabaseName,

    -- * IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration (..),
    mkIotEventsDestinationConfiguration,
    iedcInputName,
    iedcRoleARN,

    -- * LambdaActivity
    LambdaActivity (..),
    mkLambdaActivity,
    laNext,
    laName,
    laLambdaName,
    laBatchSize,

    -- * LateDataRule
    LateDataRule (..),
    mkLateDataRule,
    ldrRuleName,
    ldrRuleConfiguration,

    -- * LateDataRuleConfiguration
    LateDataRuleConfiguration (..),
    mkLateDataRuleConfiguration,
    ldrcDeltaTimeSessionWindowConfiguration,

    -- * LoggingOptions
    LoggingOptions (..),
    mkLoggingOptions,
    loRoleARN,
    loLevel,
    loEnabled,

    -- * MathActivity
    MathActivity (..),
    mkMathActivity,
    maNext,
    maName,
    maAttribute,
    maMath,

    -- * Message
    Message (..),
    mkMessage,
    mMessageId,
    mPayload,

    -- * OutputFileURIValue
    OutputFileURIValue (..),
    mkOutputFileURIValue,
    ofuvFileName,

    -- * Pipeline
    Pipeline (..),
    mkPipeline,
    pCreationTime,
    pArn,
    pActivities,
    pName,
    pReprocessingSummaries,
    pLastUpdateTime,

    -- * PipelineActivity
    PipelineActivity (..),
    mkPipelineActivity,
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
    PipelineSummary (..),
    mkPipelineSummary,
    psCreationTime,
    psPipelineName,
    psReprocessingSummaries,
    psLastUpdateTime,

    -- * QueryFilter
    QueryFilter (..),
    mkQueryFilter,
    qfDeltaTime,

    -- * RemoveAttributesActivity
    RemoveAttributesActivity (..),
    mkRemoveAttributesActivity,
    raaNext,
    raaName,
    raaAttributes,

    -- * ReprocessingSummary
    ReprocessingSummary (..),
    mkReprocessingSummary,
    rsCreationTime,
    rsStatus,
    rsId,

    -- * ResourceConfiguration
    ResourceConfiguration (..),
    mkResourceConfiguration,
    rcComputeType,
    rcVolumeSizeInGB,

    -- * RetentionPeriod
    RetentionPeriod (..),
    mkRetentionPeriod,
    rpUnlimited,
    rpNumberOfDays,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    mkS3DestinationConfiguration,
    sdcGlueConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleARN,

    -- * Schedule
    Schedule (..),
    mkSchedule,
    sExpression,

    -- * SelectAttributesActivity
    SelectAttributesActivity (..),
    mkSelectAttributesActivity,
    saaNext,
    saaName,
    saaAttributes,

    -- * ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage (..),
    mkServiceManagedChannelS3Storage,

    -- * ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary (..),
    mkServiceManagedChannelS3StorageSummary,

    -- * ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage (..),
    mkServiceManagedDatastoreS3Storage,

    -- * ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary (..),
    mkServiceManagedDatastoreS3StorageSummary,

    -- * SqlQueryDatasetAction
    SqlQueryDatasetAction (..),
    mkSqlQueryDatasetAction,
    sqdaFilters,
    sqdaSqlQuery,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TriggeringDataset
    TriggeringDataset (..),
    mkTriggeringDataset,
    tdName,

    -- * Variable
    Variable (..),
    mkVariable,
    vOutputFileURIValue,
    vDoubleValue,
    vStringValue,
    vDatasetContentVersionValue,
    vName,

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon IoT Analytics SDK configuration.
ioTAnalyticsService :: Lude.Service
ioTAnalyticsService =
  Lude.Service
    { Lude._svcAbbrev = "IoTAnalytics",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "iotanalytics",
      Lude._svcVersion = "2017-11-27",
      Lude._svcEndpoint = Lude.defaultEndpoint ioTAnalyticsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "IoTAnalytics",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
