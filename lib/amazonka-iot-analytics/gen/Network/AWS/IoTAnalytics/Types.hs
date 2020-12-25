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
    mkServiceConfig,

    -- * Errors
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _ThrottlingException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * DatasetActionType
    DatasetActionType (..),

    -- * OutputFileUriValue
    OutputFileUriValue (..),
    mkOutputFileUriValue,
    ofuvFileName,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    mkS3DestinationConfiguration,
    sdcBucket,
    sdcKey,
    sdcRoleArn,
    sdcGlueConfiguration,

    -- * IotEventsInputName
    IotEventsInputName (..),

    -- * DatasetContentStatus
    DatasetContentStatus (..),
    mkDatasetContentStatus,
    dcsReason,
    dcsState,

    -- * CustomerManagedDatastoreS3Storage
    CustomerManagedDatastoreS3Storage (..),
    mkCustomerManagedDatastoreS3Storage,
    cmdssBucket,
    cmdssRoleArn,
    cmdssKeyPrefix,

    -- * PipelineActivity
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

    -- * CustomerManagedChannelS3StorageSummary
    CustomerManagedChannelS3StorageSummary (..),
    mkCustomerManagedChannelS3StorageSummary,
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleArn,

    -- * DatasetArn
    DatasetArn (..),

    -- * Image
    Image (..),

    -- * ChannelStatus
    ChannelStatus (..),

    -- * S3KeyPrefix
    S3KeyPrefix (..),

    -- * LambdaActivity
    LambdaActivity (..),
    mkLambdaActivity,
    laName,
    laLambdaName,
    laBatchSize,
    laNext,

    -- * VersioningConfiguration
    VersioningConfiguration (..),
    mkVersioningConfiguration,
    vcMaxVersions,
    vcUnlimited,

    -- * ActivityName
    ActivityName (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    mkBatchPutMessageErrorEntry,
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,

    -- * GlueTableName
    GlueTableName (..),

    -- * LateDataRuleConfiguration
    LateDataRuleConfiguration (..),
    mkLateDataRuleConfiguration,
    ldrcDeltaTimeSessionWindowConfiguration,

    -- * MathExpression
    MathExpression (..),

    -- * DatastoreStatistics
    DatastoreStatistics (..),
    mkDatastoreStatistics,
    dsSize,

    -- * FilterExpression
    FilterExpression (..),

    -- * DatastoreActivity
    DatastoreActivity (..),
    mkDatastoreActivity,
    daName,
    daDatastoreName,

    -- * QueryFilter
    QueryFilter (..),
    mkQueryFilter,
    qfDeltaTime,

    -- * DatastoreStorageSummary
    DatastoreStorageSummary (..),
    mkDatastoreStorageSummary,
    dssCustomerManagedS3,
    dssServiceManagedS3,

    -- * Dataset
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

    -- * ServiceManagedDatastoreS3StorageSummary
    ServiceManagedDatastoreS3StorageSummary (..),
    mkServiceManagedDatastoreS3StorageSummary,

    -- * DatastoreArn
    DatastoreArn (..),

    -- * ResourceConfiguration
    ResourceConfiguration (..),
    mkResourceConfiguration,
    rcComputeType,
    rcVolumeSizeInGB,

    -- * FilterActivity
    FilterActivity (..),
    mkFilterActivity,
    faName,
    faFilter,
    faNext,

    -- * EstimatedResourceSize
    EstimatedResourceSize (..),
    mkEstimatedResourceSize,
    ersEstimatedOn,
    ersEstimatedSizeInBytes,

    -- * LateDataRuleName
    LateDataRuleName (..),

    -- * LogResult
    LogResult (..),

    -- * PipelineName
    PipelineName (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * MathActivity
    MathActivity (..),
    mkMathActivity,
    maName,
    maAttribute,
    maMath,
    maNext,

    -- * Schedule
    Schedule (..),
    mkSchedule,
    sExpression,

    -- * DeltaTime
    DeltaTime (..),
    mkDeltaTime,
    dtOffsetSeconds,
    dtTimeExpression,

    -- * LambdaName
    LambdaName (..),

    -- * DatastoreName
    DatastoreName (..),

    -- * DeviceShadowEnrichActivity
    DeviceShadowEnrichActivity (..),
    mkDeviceShadowEnrichActivity,
    dseaName,
    dseaAttribute,
    dseaThingName,
    dseaRoleArn,
    dseaNext,

    -- * SqlQueryDatasetAction
    SqlQueryDatasetAction (..),
    mkSqlQueryDatasetAction,
    sqdaSqlQuery,
    sqdaFilters,

    -- * PresignedURI
    PresignedURI (..),

    -- * DeviceRegistryEnrichActivity
    DeviceRegistryEnrichActivity (..),
    mkDeviceRegistryEnrichActivity,
    dreaName,
    dreaAttribute,
    dreaThingName,
    dreaRoleArn,
    dreaNext,

    -- * DatastoreSummary
    DatastoreSummary (..),
    mkDatastoreSummary,
    dsCreationTime,
    dsDatastoreName,
    dsDatastoreStorage,
    dsLastMessageArrivalTime,
    dsLastUpdateTime,
    dsStatus,

    -- * AddAttributesActivity
    AddAttributesActivity (..),
    mkAddAttributesActivity,
    aaaName,
    aaaAttributes,
    aaaNext,

    -- * GlueConfiguration
    GlueConfiguration (..),
    mkGlueConfiguration,
    gcTableName,
    gcDatabaseName,

    -- * Channel
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

    -- * DatasetActionName
    DatasetActionName (..),

    -- * EntryName
    EntryName (..),

    -- * ServiceManagedChannelS3StorageSummary
    ServiceManagedChannelS3StorageSummary (..),
    mkServiceManagedChannelS3StorageSummary,

    -- * ChannelActivity
    ChannelActivity (..),
    mkChannelActivity,
    caName,
    caChannelName,
    caNext,

    -- * ChannelStatistics
    ChannelStatistics (..),
    mkChannelStatistics,
    csSize,

    -- * Variable
    Variable (..),
    mkVariable,
    vName,
    vDatasetContentVersionValue,
    vDoubleValue,
    vOutputFileUriValue,
    vStringValue,

    -- * ChannelArn
    ChannelArn (..),

    -- * StringValue
    StringValue (..),

    -- * PipelineSummary
    PipelineSummary (..),
    mkPipelineSummary,
    psCreationTime,
    psLastUpdateTime,
    psPipelineName,
    psReprocessingSummaries,

    -- * ChannelStorageSummary
    ChannelStorageSummary (..),
    mkChannelStorageSummary,
    cssCustomerManagedS3,
    cssServiceManagedS3,

    -- * DatasetEntry
    DatasetEntry (..),
    mkDatasetEntry,
    deDataURI,
    deEntryName,

    -- * IotEventsDestinationConfiguration
    IotEventsDestinationConfiguration (..),
    mkIotEventsDestinationConfiguration,
    iedcInputName,
    iedcRoleArn,

    -- * DatasetContentVersion
    DatasetContentVersion (..),

    -- * BucketName
    BucketName (..),

    -- * RetentionPeriod
    RetentionPeriod (..),
    mkRetentionPeriod,
    rpNumberOfDays,
    rpUnlimited,

    -- * ReprocessingId
    ReprocessingId (..),

    -- * RemoveAttributesActivity
    RemoveAttributesActivity (..),
    mkRemoveAttributesActivity,
    raaName,
    raaAttributes,
    raaNext,

    -- * SqlQuery
    SqlQuery (..),

    -- * Reason
    Reason (..),

    -- * NextToken
    NextToken (..),

    -- * ChannelName
    ChannelName (..),

    -- * DatasetContentState
    DatasetContentState (..),

    -- * DatasetAction
    DatasetAction (..),
    mkDatasetAction,
    daActionName,
    daContainerAction,
    daQueryAction,

    -- * Datastore
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

    -- * ResourceArn
    ResourceArn (..),

    -- * DatasetContentSummary
    DatasetContentSummary (..),
    mkDatasetContentSummary,
    dcsCompletionTime,
    dcsCreationTime,
    dcsScheduleTime,
    dcsStatus,
    dcsVersion,

    -- * DeltaTimeSessionWindowConfiguration
    DeltaTimeSessionWindowConfiguration (..),
    mkDeltaTimeSessionWindowConfiguration,
    dtswcTimeoutInMinutes,

    -- * TimeExpression
    TimeExpression (..),

    -- * ChannelSummary
    ChannelSummary (..),
    mkChannelSummary,
    csChannelName,
    csChannelStorage,
    csCreationTime,
    csLastMessageArrivalTime,
    csLastUpdateTime,
    csStatus,

    -- * PipelineArn
    PipelineArn (..),

    -- * ChannelStorage
    ChannelStorage (..),
    mkChannelStorage,
    csCustomerManagedS3,
    csServiceManagedS3,

    -- * DatasetTrigger
    DatasetTrigger (..),
    mkDatasetTrigger,
    dtDataset,
    dtSchedule,

    -- * DatasetContentVersionValue
    DatasetContentVersionValue (..),
    mkDatasetContentVersionValue,
    dcvvDatasetName,

    -- * Pipeline
    Pipeline (..),
    mkPipeline,
    pActivities,
    pArn,
    pCreationTime,
    pLastUpdateTime,
    pName,
    pReprocessingSummaries,

    -- * SelectAttributesActivity
    SelectAttributesActivity (..),
    mkSelectAttributesActivity,
    saaName,
    saaAttributes,
    saaNext,

    -- * LateDataRule
    LateDataRule (..),
    mkLateDataRule,
    ldrRuleConfiguration,
    ldrRuleName,

    -- * TriggeringDataset
    TriggeringDataset (..),
    mkTriggeringDataset,
    tdName,

    -- * ServiceManagedChannelS3Storage
    ServiceManagedChannelS3Storage (..),
    mkServiceManagedChannelS3Storage,

    -- * ErrorCode
    ErrorCode (..),

    -- * DatasetName
    DatasetName (..),

    -- * ComputeType
    ComputeType (..),

    -- * TagKey
    TagKey (..),

    -- * ReprocessingStatus
    ReprocessingStatus (..),

    -- * ReprocessingSummary
    ReprocessingSummary (..),
    mkReprocessingSummary,
    rsCreationTime,
    rsId,
    rsStatus,

    -- * DatasetActionSummary
    DatasetActionSummary (..),
    mkDatasetActionSummary,
    dasActionName,
    dasActionType,

    -- * LoggingOptions
    LoggingOptions (..),
    mkLoggingOptions,
    loRoleArn,
    loLevel,
    loEnabled,

    -- * DatastoreStatus
    DatastoreStatus (..),

    -- * ErrorMessage
    ErrorMessage (..),

    -- * Message
    Message (..),
    mkMessage,
    mMessageId,
    mPayload,

    -- * AttributeName
    AttributeName (..),

    -- * CustomerManagedChannelS3Storage
    CustomerManagedChannelS3Storage (..),
    mkCustomerManagedChannelS3Storage,
    cmcssBucket,
    cmcssRoleArn,
    cmcssKeyPrefix,

    -- * DatasetContentDeliveryDestination
    DatasetContentDeliveryDestination (..),
    mkDatasetContentDeliveryDestination,
    dcddIotEventsDestinationConfiguration,
    dcddS3DestinationConfiguration,

    -- * ContainerDatasetAction
    ContainerDatasetAction (..),
    mkContainerDatasetAction,
    cdaImage,
    cdaExecutionRoleArn,
    cdaResourceConfiguration,
    cdaVariables,

    -- * CustomerManagedDatastoreS3StorageSummary
    CustomerManagedDatastoreS3StorageSummary (..),
    mkCustomerManagedDatastoreS3StorageSummary,
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleArn,

    -- * ServiceManagedDatastoreS3Storage
    ServiceManagedDatastoreS3Storage (..),
    mkServiceManagedDatastoreS3Storage,

    -- * DatastoreStorage
    DatastoreStorage (..),
    mkDatastoreStorage,
    dsCustomerManagedS3,
    dsServiceManagedS3,

    -- * DatasetStatus
    DatasetStatus (..),

    -- * MessageId
    MessageId (..),

    -- * DatasetSummary
    DatasetSummary (..),
    mkDatasetSummary,
    dsfActions,
    dsfCreationTime,
    dsfDatasetName,
    dsfLastUpdateTime,
    dsfStatus,
    dsfTriggers,

    -- * DatasetContentDeliveryRule
    DatasetContentDeliveryRule (..),
    mkDatasetContentDeliveryRule,
    dcdrDestination,
    dcdrEntryName,

    -- * RoleArn
    RoleArn (..),

    -- * FileName
    FileName (..),

    -- * Bucket
    Bucket (..),

    -- * Key
    Key (..),

    -- * KeyPrefix
    KeyPrefix (..),

    -- * Name
    Name (..),

    -- * Next
    Next (..),

    -- * Value
    Value (..),

    -- * Attribute
    Attribute (..),

    -- * Expression
    Expression (..),

    -- * ThingName
    ThingName (..),

    -- * VersionId
    VersionId (..),

    -- * DatabaseName
    DatabaseName (..),

    -- * Arn
    Arn (..),

    -- * ExecutionRoleArn
    ExecutionRoleArn (..),
  )
where

import Network.AWS.IoTAnalytics.Types.ActivityName
import Network.AWS.IoTAnalytics.Types.AddAttributesActivity
import Network.AWS.IoTAnalytics.Types.Arn
import Network.AWS.IoTAnalytics.Types.Attribute
import Network.AWS.IoTAnalytics.Types.AttributeName
import Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
import Network.AWS.IoTAnalytics.Types.Bucket
import Network.AWS.IoTAnalytics.Types.BucketName
import Network.AWS.IoTAnalytics.Types.Channel
import Network.AWS.IoTAnalytics.Types.ChannelActivity
import Network.AWS.IoTAnalytics.Types.ChannelArn
import Network.AWS.IoTAnalytics.Types.ChannelName
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
import Network.AWS.IoTAnalytics.Types.DatabaseName
import Network.AWS.IoTAnalytics.Types.Dataset
import Network.AWS.IoTAnalytics.Types.DatasetAction
import Network.AWS.IoTAnalytics.Types.DatasetActionName
import Network.AWS.IoTAnalytics.Types.DatasetActionSummary
import Network.AWS.IoTAnalytics.Types.DatasetActionType
import Network.AWS.IoTAnalytics.Types.DatasetArn
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
import Network.AWS.IoTAnalytics.Types.DatasetContentState
import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import Network.AWS.IoTAnalytics.Types.DatasetContentSummary
import Network.AWS.IoTAnalytics.Types.DatasetContentVersion
import Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
import Network.AWS.IoTAnalytics.Types.DatasetEntry
import Network.AWS.IoTAnalytics.Types.DatasetName
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetSummary
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.IoTAnalytics.Types.Datastore
import Network.AWS.IoTAnalytics.Types.DatastoreActivity
import Network.AWS.IoTAnalytics.Types.DatastoreArn
import Network.AWS.IoTAnalytics.Types.DatastoreName
import Network.AWS.IoTAnalytics.Types.DatastoreStatistics
import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
import Network.AWS.IoTAnalytics.Types.DatastoreSummary
import Network.AWS.IoTAnalytics.Types.DeltaTime
import Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Network.AWS.IoTAnalytics.Types.EntryName
import Network.AWS.IoTAnalytics.Types.ErrorCode
import Network.AWS.IoTAnalytics.Types.ErrorMessage
import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import Network.AWS.IoTAnalytics.Types.ExecutionRoleArn
import Network.AWS.IoTAnalytics.Types.Expression
import Network.AWS.IoTAnalytics.Types.FileName
import Network.AWS.IoTAnalytics.Types.FilterActivity
import Network.AWS.IoTAnalytics.Types.FilterExpression
import Network.AWS.IoTAnalytics.Types.GlueConfiguration
import Network.AWS.IoTAnalytics.Types.GlueTableName
import Network.AWS.IoTAnalytics.Types.Image
import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.IotEventsInputName
import Network.AWS.IoTAnalytics.Types.Key
import Network.AWS.IoTAnalytics.Types.KeyPrefix
import Network.AWS.IoTAnalytics.Types.LambdaActivity
import Network.AWS.IoTAnalytics.Types.LambdaName
import Network.AWS.IoTAnalytics.Types.LateDataRule
import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import Network.AWS.IoTAnalytics.Types.LateDataRuleName
import Network.AWS.IoTAnalytics.Types.LogResult
import Network.AWS.IoTAnalytics.Types.LoggingLevel
import Network.AWS.IoTAnalytics.Types.LoggingOptions
import Network.AWS.IoTAnalytics.Types.MathActivity
import Network.AWS.IoTAnalytics.Types.MathExpression
import Network.AWS.IoTAnalytics.Types.Message
import Network.AWS.IoTAnalytics.Types.MessageId
import Network.AWS.IoTAnalytics.Types.Name
import Network.AWS.IoTAnalytics.Types.Next
import Network.AWS.IoTAnalytics.Types.NextToken
import Network.AWS.IoTAnalytics.Types.OutputFileUriValue
import Network.AWS.IoTAnalytics.Types.Pipeline
import Network.AWS.IoTAnalytics.Types.PipelineActivity
import Network.AWS.IoTAnalytics.Types.PipelineArn
import Network.AWS.IoTAnalytics.Types.PipelineName
import Network.AWS.IoTAnalytics.Types.PipelineSummary
import Network.AWS.IoTAnalytics.Types.PresignedURI
import Network.AWS.IoTAnalytics.Types.QueryFilter
import Network.AWS.IoTAnalytics.Types.Reason
import Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
import Network.AWS.IoTAnalytics.Types.ReprocessingId
import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import Network.AWS.IoTAnalytics.Types.ResourceArn
import Network.AWS.IoTAnalytics.Types.ResourceConfiguration
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.IoTAnalytics.Types.RoleArn
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import Network.AWS.IoTAnalytics.Types.S3KeyPrefix
import Network.AWS.IoTAnalytics.Types.Schedule
import Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.SqlQuery
import Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
import Network.AWS.IoTAnalytics.Types.StringValue
import Network.AWS.IoTAnalytics.Types.Tag
import Network.AWS.IoTAnalytics.Types.TagKey
import Network.AWS.IoTAnalytics.Types.ThingName
import Network.AWS.IoTAnalytics.Types.TimeExpression
import Network.AWS.IoTAnalytics.Types.TriggeringDataset
import Network.AWS.IoTAnalytics.Types.Value
import Network.AWS.IoTAnalytics.Types.Variable
import Network.AWS.IoTAnalytics.Types.VersionId
import Network.AWS.IoTAnalytics.Types.VersioningConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon IoT Analytics SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "IoTAnalytics",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "iotanalytics",
      Core._svcVersion = "2017-11-27",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "IoTAnalytics",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | A resource with the same name already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError mkServiceConfig "ThrottlingException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | There was an internal failure.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
    Core.. Core.hasStatues 503
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | A resource with the specified name could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The command caused an internal limit to be exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
    Core.. Core.hasStatues 410
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
