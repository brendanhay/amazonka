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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CloudWatchLogs",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "logs",
      Prelude._svcSigningName = "logs",
      Prelude._svcVersion = "2014-03-28",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CloudWatchLogs",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation is not valid on the specified resource.
_InvalidOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOperationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The service cannot complete the request.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The most likely cause is an invalid AWS access key ID or secret key.
_UnrecognizedClientException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnrecognizedClientException =
  Prelude._MatchServiceError
    defaultService
    "UnrecognizedClientException"

-- | The query string is not valid. Details about this error are displayed in
-- a @QueryCompileError@ object. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_QueryCompileError.html QueryCompileError>.
--
-- For more information about valid query syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
_MalformedQueryException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedQueryException =
  Prelude._MatchServiceError
    defaultService
    "MalformedQueryException"

-- | A parameter is specified incorrectly.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You have reached the maximum number of resources that can be created.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Multiple requests to update the same resource were in conflict.
_OperationAbortedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationAbortedException =
  Prelude._MatchServiceError
    defaultService
    "OperationAbortedException"

-- | The specified resource does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The event was already logged.
_DataAlreadyAcceptedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DataAlreadyAcceptedException =
  Prelude._MatchServiceError
    defaultService
    "DataAlreadyAcceptedException"

-- | The sequence token is not valid. You can get the correct sequence token
-- in the @expectedSequenceToken@ field in the
-- @InvalidSequenceTokenException@ message.
_InvalidSequenceTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSequenceTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSequenceTokenException"
