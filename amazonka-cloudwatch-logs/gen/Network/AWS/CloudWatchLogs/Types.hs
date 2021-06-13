{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidOperationException,
    _ResourceAlreadyExistsException,
    _ServiceUnavailableException,
    _UnrecognizedClientException,
    _MalformedQueryException,
    _InvalidParameterException,
    _LimitExceededException,
    _OperationAbortedException,
    _ResourceNotFoundException,
    _DataAlreadyAcceptedException,
    _InvalidSequenceTokenException,

    -- * Distribution
    Distribution (..),

    -- * ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- * OrderBy
    OrderBy (..),

    -- * QueryStatus
    QueryStatus (..),

    -- * Destination
    Destination (..),
    newDestination,
    destination_creationTime,
    destination_roleArn,
    destination_destinationName,
    destination_arn,
    destination_targetArn,
    destination_accessPolicy,

    -- * ExportTask
    ExportTask (..),
    newExportTask,
    exportTask_status,
    exportTask_executionInfo,
    exportTask_to,
    exportTask_taskId,
    exportTask_taskName,
    exportTask_logGroupName,
    exportTask_destination,
    exportTask_destinationPrefix,
    exportTask_from,

    -- * ExportTaskExecutionInfo
    ExportTaskExecutionInfo (..),
    newExportTaskExecutionInfo,
    exportTaskExecutionInfo_creationTime,
    exportTaskExecutionInfo_completionTime,

    -- * ExportTaskStatus
    ExportTaskStatus (..),
    newExportTaskStatus,
    exportTaskStatus_message,
    exportTaskStatus_code,

    -- * FilteredLogEvent
    FilteredLogEvent (..),
    newFilteredLogEvent,
    filteredLogEvent_logStreamName,
    filteredLogEvent_eventId,
    filteredLogEvent_message,
    filteredLogEvent_ingestionTime,
    filteredLogEvent_timestamp,

    -- * InputLogEvent
    InputLogEvent (..),
    newInputLogEvent,
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- * LogGroup
    LogGroup (..),
    newLogGroup,
    logGroup_retentionInDays,
    logGroup_creationTime,
    logGroup_arn,
    logGroup_storedBytes,
    logGroup_metricFilterCount,
    logGroup_kmsKeyId,
    logGroup_logGroupName,

    -- * LogGroupField
    LogGroupField (..),
    newLogGroupField,
    logGroupField_name,
    logGroupField_percent,

    -- * LogStream
    LogStream (..),
    newLogStream,
    logStream_logStreamName,
    logStream_creationTime,
    logStream_arn,
    logStream_storedBytes,
    logStream_uploadSequenceToken,
    logStream_firstEventTimestamp,
    logStream_lastEventTimestamp,
    logStream_lastIngestionTime,

    -- * MetricFilter
    MetricFilter (..),
    newMetricFilter,
    metricFilter_filterName,
    metricFilter_creationTime,
    metricFilter_filterPattern,
    metricFilter_logGroupName,
    metricFilter_metricTransformations,

    -- * MetricFilterMatchRecord
    MetricFilterMatchRecord (..),
    newMetricFilterMatchRecord,
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_eventMessage,
    metricFilterMatchRecord_extractedValues,

    -- * MetricTransformation
    MetricTransformation (..),
    newMetricTransformation,
    metricTransformation_defaultValue,
    metricTransformation_metricName,
    metricTransformation_metricNamespace,
    metricTransformation_metricValue,

    -- * OutputLogEvent
    OutputLogEvent (..),
    newOutputLogEvent,
    outputLogEvent_message,
    outputLogEvent_ingestionTime,
    outputLogEvent_timestamp,

    -- * QueryDefinition
    QueryDefinition (..),
    newQueryDefinition,
    queryDefinition_queryString,
    queryDefinition_name,
    queryDefinition_logGroupNames,
    queryDefinition_lastModified,
    queryDefinition_queryDefinitionId,

    -- * QueryInfo
    QueryInfo (..),
    newQueryInfo,
    queryInfo_queryString,
    queryInfo_status,
    queryInfo_queryId,
    queryInfo_logGroupName,
    queryInfo_createTime,

    -- * QueryStatistics
    QueryStatistics (..),
    newQueryStatistics,
    queryStatistics_bytesScanned,
    queryStatistics_recordsMatched,
    queryStatistics_recordsScanned,

    -- * RejectedLogEventsInfo
    RejectedLogEventsInfo (..),
    newRejectedLogEventsInfo,
    rejectedLogEventsInfo_tooOldLogEventEndIndex,
    rejectedLogEventsInfo_expiredLogEventEndIndex,
    rejectedLogEventsInfo_tooNewLogEventStartIndex,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_policyName,
    resourcePolicy_policyDocument,
    resourcePolicy_lastUpdatedTime,

    -- * ResultField
    ResultField (..),
    newResultField,
    resultField_value,
    resultField_field,

    -- * SearchedLogStream
    SearchedLogStream (..),
    newSearchedLogStream,
    searchedLogStream_logStreamName,
    searchedLogStream_searchedCompletely,

    -- * SubscriptionFilter
    SubscriptionFilter (..),
    newSubscriptionFilter,
    subscriptionFilter_filterName,
    subscriptionFilter_creationTime,
    subscriptionFilter_destinationArn,
    subscriptionFilter_roleArn,
    subscriptionFilter_filterPattern,
    subscriptionFilter_distribution,
    subscriptionFilter_logGroupName,
  )
where

import Network.AWS.CloudWatchLogs.Types.Destination
import Network.AWS.CloudWatchLogs.Types.Distribution
import Network.AWS.CloudWatchLogs.Types.ExportTask
import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
import Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
import Network.AWS.CloudWatchLogs.Types.InputLogEvent
import Network.AWS.CloudWatchLogs.Types.LogGroup
import Network.AWS.CloudWatchLogs.Types.LogGroupField
import Network.AWS.CloudWatchLogs.Types.LogStream
import Network.AWS.CloudWatchLogs.Types.MetricFilter
import Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import Network.AWS.CloudWatchLogs.Types.OrderBy
import Network.AWS.CloudWatchLogs.Types.OutputLogEvent
import Network.AWS.CloudWatchLogs.Types.QueryDefinition
import Network.AWS.CloudWatchLogs.Types.QueryInfo
import Network.AWS.CloudWatchLogs.Types.QueryStatistics
import Network.AWS.CloudWatchLogs.Types.QueryStatus
import Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
import Network.AWS.CloudWatchLogs.Types.ResourcePolicy
import Network.AWS.CloudWatchLogs.Types.ResultField
import Network.AWS.CloudWatchLogs.Types.SearchedLogStream
import Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CloudWatchLogs",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "logs",
      Core._serviceSigningName = "logs",
      Core._serviceVersion = "2014-03-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudWatchLogs",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation is not valid on the specified resource.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The most likely cause is an invalid AWS access key ID or secret key.
_UnrecognizedClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnrecognizedClientException =
  Core._MatchServiceError
    defaultService
    "UnrecognizedClientException"

-- | The query string is not valid. Details about this error are displayed in
-- a @QueryCompileError@ object. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_QueryCompileError.html QueryCompileError>.
--
-- For more information about valid query syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
_MalformedQueryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedQueryException =
  Core._MatchServiceError
    defaultService
    "MalformedQueryException"

-- | A parameter is specified incorrectly.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You have reached the maximum number of resources that can be created.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Multiple requests to update the same resource were in conflict.
_OperationAbortedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationAbortedException =
  Core._MatchServiceError
    defaultService
    "OperationAbortedException"

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The event was already logged.
_DataAlreadyAcceptedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataAlreadyAcceptedException =
  Core._MatchServiceError
    defaultService
    "DataAlreadyAcceptedException"

-- | The sequence token is not valid. You can get the correct sequence token
-- in the @expectedSequenceToken@ field in the
-- @InvalidSequenceTokenException@ message.
_InvalidSequenceTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSequenceTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidSequenceTokenException"
