-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidParameterException,
    _InvalidSequenceTokenException,
    _UnrecognizedClientException,
    _ResourceAlreadyExistsException,
    _OperationAbortedException,
    _MalformedQueryException,
    _ServiceUnavailableException,
    _DataAlreadyAcceptedException,
    _InvalidOperationException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * MetricFilter
    MetricFilter (..),
    mkMetricFilter,
    mfCreationTime,
    mfFilterName,
    mfFilterPattern,
    mfLogGroupName,
    mfMetricTransformations,

    -- * SequenceToken
    SequenceToken (..),

    -- * ExportTaskId
    ExportTaskId (..),

    -- * TargetArn
    TargetArn (..),

    -- * Destination
    Destination (..),
    mkDestination,
    dAccessPolicy,
    dArn,
    dCreationTime,
    dDestinationName,
    dRoleArn,
    dTargetArn,

    -- * QueryId
    QueryId (..),

    -- * PolicyDocument
    PolicyDocument (..),

    -- * FilterName
    FilterName (..),

    -- * QueryStatistics
    QueryStatistics (..),
    mkQueryStatistics,
    qsBytesScanned,
    qsRecordsMatched,
    qsRecordsScanned,

    -- * SearchedLogStream
    SearchedLogStream (..),
    mkSearchedLogStream,
    slsLogStreamName,
    slsSearchedCompletely,

    -- * PolicyName
    PolicyName (..),

    -- * MetricFilterMatchRecord
    MetricFilterMatchRecord (..),
    mkMetricFilterMatchRecord,
    mfmrEventMessage,
    mfmrEventNumber,
    mfmrExtractedValues,

    -- * Field
    Field (..),

    -- * ResourcePolicy
    ResourcePolicy (..),
    mkResourcePolicy,
    rpLastUpdatedTime,
    rpPolicyDocument,
    rpPolicyName,

    -- * Distribution
    Distribution (..),

    -- * LogRecordPointer
    LogRecordPointer (..),

    -- * Arn
    Arn (..),

    -- * MetricTransformation
    MetricTransformation (..),
    mkMetricTransformation,
    mtMetricName,
    mtMetricNamespace,
    mtMetricValue,
    mtDefaultValue,

    -- * OrderBy
    OrderBy (..),

    -- * QueryDefinitionName
    QueryDefinitionName (..),

    -- * ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- * LogStream
    LogStream (..),
    mkLogStream,
    lsArn,
    lsCreationTime,
    lsFirstEventTimestamp,
    lsLastEventTimestamp,
    lsLastIngestionTime,
    lsLogStreamName,
    lsStoredBytes,
    lsUploadSequenceToken,

    -- * ExportDestinationPrefix
    ExportDestinationPrefix (..),

    -- * Token
    Token (..),

    -- * MetricName
    MetricName (..),

    -- * LogGroup
    LogGroup (..),
    mkLogGroup,
    lgArn,
    lgCreationTime,
    lgKmsKeyId,
    lgLogGroupName,
    lgMetricFilterCount,
    lgRetentionInDays,
    lgStoredBytes,

    -- * SubscriptionFilter
    SubscriptionFilter (..),
    mkSubscriptionFilter,
    sfCreationTime,
    sfDestinationArn,
    sfDistribution,
    sfFilterName,
    sfFilterPattern,
    sfLogGroupName,
    sfRoleArn,

    -- * RejectedLogEventsInfo
    RejectedLogEventsInfo (..),
    mkRejectedLogEventsInfo,
    rleiExpiredLogEventEndIndex,
    rleiTooNewLogEventStartIndex,
    rleiTooOldLogEventEndIndex,

    -- * Value
    Value (..),

    -- * ExportTaskStatusMessage
    ExportTaskStatusMessage (..),

    -- * ExportDestinationBucket
    ExportDestinationBucket (..),

    -- * QueryStatus
    QueryStatus (..),

    -- * InputLogEvent
    InputLogEvent (..),
    mkInputLogEvent,
    ileTimestamp,
    ileMessage,

    -- * TagValue
    TagValue (..),

    -- * FilteredLogEvent
    FilteredLogEvent (..),
    mkFilteredLogEvent,
    fleEventId,
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,

    -- * DestinationArn
    DestinationArn (..),

    -- * LogGroupName
    LogGroupName (..),

    -- * QueryInfo
    QueryInfo (..),
    mkQueryInfo,
    qiCreateTime,
    qiLogGroupName,
    qiQueryId,
    qiQueryString,
    qiStatus,

    -- * NextToken
    NextToken (..),

    -- * AccessPolicy
    AccessPolicy (..),

    -- * LogStreamName
    LogStreamName (..),

    -- * MetricNamespace
    MetricNamespace (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * MetricValue
    MetricValue (..),

    -- * EventMessage
    EventMessage (..),

    -- * QueryString
    QueryString (..),

    -- * OutputLogEvent
    OutputLogEvent (..),
    mkOutputLogEvent,
    oleIngestionTime,
    oleMessage,
    oleTimestamp,

    -- * QueryDefinition
    QueryDefinition (..),
    mkQueryDefinition,
    qdLastModified,
    qdLogGroupNames,
    qdName,
    qdQueryDefinitionId,
    qdQueryString,

    -- * QueryDefinitionString
    QueryDefinitionString (..),

    -- * ExportTaskStatus
    ExportTaskStatus (..),
    mkExportTaskStatus,
    etsCode,
    etsMessage,

    -- * ExportTaskExecutionInfo
    ExportTaskExecutionInfo (..),
    mkExportTaskExecutionInfo,
    eteiCompletionTime,
    eteiCreationTime,

    -- * TagKey
    TagKey (..),

    -- * FilterPattern
    FilterPattern (..),

    -- * DestinationName
    DestinationName (..),

    -- * Message
    Message (..),

    -- * ResultField
    ResultField (..),
    mkResultField,
    rfField,
    rfValue,

    -- * ExportTaskName
    ExportTaskName (..),

    -- * ExportTask
    ExportTask (..),
    mkExportTask,
    etDestination,
    etDestinationPrefix,
    etExecutionInfo,
    etFrom,
    etLogGroupName,
    etStatus,
    etTaskId,
    etTaskName,
    etTo,

    -- * EventId
    EventId (..),

    -- * LogGroupField
    LogGroupField (..),
    mkLogGroupField,
    lgfName,
    lgfPercent,

    -- * RoleArn
    RoleArn (..),

    -- * DestinationPrefix
    DestinationPrefix (..),

    -- * LogStreamNamePrefix
    LogStreamNamePrefix (..),

    -- * TaskName
    TaskName (..),

    -- * FilterNamePrefix
    FilterNamePrefix (..),

    -- * LogGroupNamePrefix
    LogGroupNamePrefix (..),

    -- * Name
    Name (..),
  )
where

import Network.AWS.CloudWatchLogs.Types.AccessPolicy
import Network.AWS.CloudWatchLogs.Types.Arn
import Network.AWS.CloudWatchLogs.Types.Destination
import Network.AWS.CloudWatchLogs.Types.DestinationArn
import Network.AWS.CloudWatchLogs.Types.DestinationName
import Network.AWS.CloudWatchLogs.Types.DestinationPrefix
import Network.AWS.CloudWatchLogs.Types.Distribution
import Network.AWS.CloudWatchLogs.Types.EventId
import Network.AWS.CloudWatchLogs.Types.EventMessage
import Network.AWS.CloudWatchLogs.Types.ExportDestinationBucket
import Network.AWS.CloudWatchLogs.Types.ExportDestinationPrefix
import Network.AWS.CloudWatchLogs.Types.ExportTask
import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskId
import Network.AWS.CloudWatchLogs.Types.ExportTaskName
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusMessage
import Network.AWS.CloudWatchLogs.Types.Field
import Network.AWS.CloudWatchLogs.Types.FilterName
import Network.AWS.CloudWatchLogs.Types.FilterNamePrefix
import Network.AWS.CloudWatchLogs.Types.FilterPattern
import Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
import Network.AWS.CloudWatchLogs.Types.InputLogEvent
import Network.AWS.CloudWatchLogs.Types.KmsKeyId
import Network.AWS.CloudWatchLogs.Types.LogGroup
import Network.AWS.CloudWatchLogs.Types.LogGroupField
import Network.AWS.CloudWatchLogs.Types.LogGroupName
import Network.AWS.CloudWatchLogs.Types.LogGroupNamePrefix
import Network.AWS.CloudWatchLogs.Types.LogRecordPointer
import Network.AWS.CloudWatchLogs.Types.LogStream
import Network.AWS.CloudWatchLogs.Types.LogStreamName
import Network.AWS.CloudWatchLogs.Types.LogStreamNamePrefix
import Network.AWS.CloudWatchLogs.Types.Message
import Network.AWS.CloudWatchLogs.Types.MetricFilter
import Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
import Network.AWS.CloudWatchLogs.Types.MetricName
import Network.AWS.CloudWatchLogs.Types.MetricNamespace
import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import Network.AWS.CloudWatchLogs.Types.MetricValue
import Network.AWS.CloudWatchLogs.Types.Name
import Network.AWS.CloudWatchLogs.Types.NextToken
import Network.AWS.CloudWatchLogs.Types.OrderBy
import Network.AWS.CloudWatchLogs.Types.OutputLogEvent
import Network.AWS.CloudWatchLogs.Types.PolicyDocument
import Network.AWS.CloudWatchLogs.Types.PolicyName
import Network.AWS.CloudWatchLogs.Types.QueryDefinition
import Network.AWS.CloudWatchLogs.Types.QueryDefinitionName
import Network.AWS.CloudWatchLogs.Types.QueryDefinitionString
import Network.AWS.CloudWatchLogs.Types.QueryId
import Network.AWS.CloudWatchLogs.Types.QueryInfo
import Network.AWS.CloudWatchLogs.Types.QueryStatistics
import Network.AWS.CloudWatchLogs.Types.QueryStatus
import Network.AWS.CloudWatchLogs.Types.QueryString
import Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
import Network.AWS.CloudWatchLogs.Types.ResourcePolicy
import Network.AWS.CloudWatchLogs.Types.ResultField
import Network.AWS.CloudWatchLogs.Types.RoleArn
import Network.AWS.CloudWatchLogs.Types.SearchedLogStream
import Network.AWS.CloudWatchLogs.Types.SequenceToken
import Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
import Network.AWS.CloudWatchLogs.Types.TagKey
import Network.AWS.CloudWatchLogs.Types.TagValue
import Network.AWS.CloudWatchLogs.Types.TargetArn
import Network.AWS.CloudWatchLogs.Types.TaskName
import Network.AWS.CloudWatchLogs.Types.Token
import Network.AWS.CloudWatchLogs.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudWatchLogs",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "logs",
      Core._svcVersion = "2014-03-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudWatchLogs",
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

-- | A parameter is specified incorrectly.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | The sequence token is not valid. You can get the correct sequence token in the @expectedSequenceToken@ field in the @InvalidSequenceTokenException@ message.
_InvalidSequenceTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSequenceTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSequenceTokenException"
{-# DEPRECATED _InvalidSequenceTokenException "Use generic-lens or generic-optics instead." #-}

-- | The most likely cause is an invalid AWS access key ID or secret key.
_UnrecognizedClientException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnrecognizedClientException =
  Core._MatchServiceError
    mkServiceConfig
    "UnrecognizedClientException"
{-# DEPRECATED _UnrecognizedClientException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Multiple requests to update the same resource were in conflict.
_OperationAbortedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationAbortedException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationAbortedException"
{-# DEPRECATED _OperationAbortedException "Use generic-lens or generic-optics instead." #-}

-- | The query string is not valid. Details about this error are displayed in a @QueryCompileError@ object. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_QueryCompileError.html QueryCompileError> .
--
-- For more information about valid query syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
_MalformedQueryException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedQueryException =
  Core._MatchServiceError mkServiceConfig "MalformedQueryException"
{-# DEPRECATED _MalformedQueryException "Use generic-lens or generic-optics instead." #-}

-- | The service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The event was already logged.
_DataAlreadyAcceptedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DataAlreadyAcceptedException =
  Core._MatchServiceError
    mkServiceConfig
    "DataAlreadyAcceptedException"
{-# DEPRECATED _DataAlreadyAcceptedException "Use generic-lens or generic-optics instead." #-}

-- | The operation is not valid on the specified resource.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidOperationException"
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | You have reached the maximum number of resources that can be created.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
