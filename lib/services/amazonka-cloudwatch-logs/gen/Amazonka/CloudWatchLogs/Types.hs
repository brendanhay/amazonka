{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidOperationException,
    _InvalidSequenceTokenException,
    _TooManyTagsException,
    _OperationAbortedException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _UnrecognizedClientException,
    _DataAlreadyAcceptedException,
    _MalformedQueryException,
    _InvalidParameterException,

    -- * Distribution
    Distribution (..),

    -- * ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- * OrderBy
    OrderBy (..),

    -- * QueryStatus
    QueryStatus (..),

    -- * StandardUnit
    StandardUnit (..),

    -- * Destination
    Destination (..),
    newDestination,
    destination_roleArn,
    destination_destinationName,
    destination_targetArn,
    destination_arn,
    destination_accessPolicy,
    destination_creationTime,

    -- * ExportTask
    ExportTask (..),
    newExportTask,
    exportTask_destination,
    exportTask_destinationPrefix,
    exportTask_from,
    exportTask_taskName,
    exportTask_taskId,
    exportTask_to,
    exportTask_status,
    exportTask_executionInfo,
    exportTask_logGroupName,

    -- * ExportTaskExecutionInfo
    ExportTaskExecutionInfo (..),
    newExportTaskExecutionInfo,
    exportTaskExecutionInfo_completionTime,
    exportTaskExecutionInfo_creationTime,

    -- * ExportTaskStatus
    ExportTaskStatus (..),
    newExportTaskStatus,
    exportTaskStatus_message,
    exportTaskStatus_code,

    -- * FilteredLogEvent
    FilteredLogEvent (..),
    newFilteredLogEvent,
    filteredLogEvent_message,
    filteredLogEvent_timestamp,
    filteredLogEvent_eventId,
    filteredLogEvent_ingestionTime,
    filteredLogEvent_logStreamName,

    -- * InputLogEvent
    InputLogEvent (..),
    newInputLogEvent,
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- * LogGroup
    LogGroup (..),
    newLogGroup,
    logGroup_storedBytes,
    logGroup_arn,
    logGroup_retentionInDays,
    logGroup_kmsKeyId,
    logGroup_metricFilterCount,
    logGroup_creationTime,
    logGroup_logGroupName,

    -- * LogGroupField
    LogGroupField (..),
    newLogGroupField,
    logGroupField_name,
    logGroupField_percent,

    -- * LogStream
    LogStream (..),
    newLogStream,
    logStream_uploadSequenceToken,
    logStream_storedBytes,
    logStream_arn,
    logStream_firstEventTimestamp,
    logStream_lastEventTimestamp,
    logStream_creationTime,
    logStream_logStreamName,
    logStream_lastIngestionTime,

    -- * MetricFilter
    MetricFilter (..),
    newMetricFilter,
    metricFilter_filterName,
    metricFilter_creationTime,
    metricFilter_metricTransformations,
    metricFilter_filterPattern,
    metricFilter_logGroupName,

    -- * MetricFilterMatchRecord
    MetricFilterMatchRecord (..),
    newMetricFilterMatchRecord,
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_extractedValues,
    metricFilterMatchRecord_eventMessage,

    -- * MetricTransformation
    MetricTransformation (..),
    newMetricTransformation,
    metricTransformation_defaultValue,
    metricTransformation_dimensions,
    metricTransformation_unit,
    metricTransformation_metricName,
    metricTransformation_metricNamespace,
    metricTransformation_metricValue,

    -- * OutputLogEvent
    OutputLogEvent (..),
    newOutputLogEvent,
    outputLogEvent_message,
    outputLogEvent_timestamp,
    outputLogEvent_ingestionTime,

    -- * QueryDefinition
    QueryDefinition (..),
    newQueryDefinition,
    queryDefinition_name,
    queryDefinition_queryDefinitionId,
    queryDefinition_logGroupNames,
    queryDefinition_lastModified,
    queryDefinition_queryString,

    -- * QueryInfo
    QueryInfo (..),
    newQueryInfo,
    queryInfo_queryId,
    queryInfo_status,
    queryInfo_queryString,
    queryInfo_createTime,
    queryInfo_logGroupName,

    -- * QueryStatistics
    QueryStatistics (..),
    newQueryStatistics,
    queryStatistics_recordsMatched,
    queryStatistics_bytesScanned,
    queryStatistics_recordsScanned,

    -- * RejectedLogEventsInfo
    RejectedLogEventsInfo (..),
    newRejectedLogEventsInfo,
    rejectedLogEventsInfo_tooNewLogEventStartIndex,
    rejectedLogEventsInfo_expiredLogEventEndIndex,
    rejectedLogEventsInfo_tooOldLogEventEndIndex,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_policyName,
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyDocument,

    -- * ResultField
    ResultField (..),
    newResultField,
    resultField_field,
    resultField_value,

    -- * SearchedLogStream
    SearchedLogStream (..),
    newSearchedLogStream,
    searchedLogStream_searchedCompletely,
    searchedLogStream_logStreamName,

    -- * SubscriptionFilter
    SubscriptionFilter (..),
    newSubscriptionFilter,
    subscriptionFilter_roleArn,
    subscriptionFilter_filterName,
    subscriptionFilter_distribution,
    subscriptionFilter_creationTime,
    subscriptionFilter_filterPattern,
    subscriptionFilter_destinationArn,
    subscriptionFilter_logGroupName,
  )
where

import Amazonka.CloudWatchLogs.Types.Destination
import Amazonka.CloudWatchLogs.Types.Distribution
import Amazonka.CloudWatchLogs.Types.ExportTask
import Amazonka.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Amazonka.CloudWatchLogs.Types.ExportTaskStatus
import Amazonka.CloudWatchLogs.Types.ExportTaskStatusCode
import Amazonka.CloudWatchLogs.Types.FilteredLogEvent
import Amazonka.CloudWatchLogs.Types.InputLogEvent
import Amazonka.CloudWatchLogs.Types.LogGroup
import Amazonka.CloudWatchLogs.Types.LogGroupField
import Amazonka.CloudWatchLogs.Types.LogStream
import Amazonka.CloudWatchLogs.Types.MetricFilter
import Amazonka.CloudWatchLogs.Types.MetricFilterMatchRecord
import Amazonka.CloudWatchLogs.Types.MetricTransformation
import Amazonka.CloudWatchLogs.Types.OrderBy
import Amazonka.CloudWatchLogs.Types.OutputLogEvent
import Amazonka.CloudWatchLogs.Types.QueryDefinition
import Amazonka.CloudWatchLogs.Types.QueryInfo
import Amazonka.CloudWatchLogs.Types.QueryStatistics
import Amazonka.CloudWatchLogs.Types.QueryStatus
import Amazonka.CloudWatchLogs.Types.RejectedLogEventsInfo
import Amazonka.CloudWatchLogs.Types.ResourcePolicy
import Amazonka.CloudWatchLogs.Types.ResultField
import Amazonka.CloudWatchLogs.Types.SearchedLogStream
import Amazonka.CloudWatchLogs.Types.StandardUnit
import Amazonka.CloudWatchLogs.Types.SubscriptionFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudWatchLogs",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "logs",
      Core.signingName = "logs",
      Core.version = "2014-03-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudWatchLogs",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The operation is not valid on the specified resource.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The sequence token is not valid. You can get the correct sequence token
-- in the @expectedSequenceToken@ field in the
-- @InvalidSequenceTokenException@ message.
_InvalidSequenceTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSequenceTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidSequenceTokenException"

-- | A resource can have no more than 50 tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | Multiple concurrent requests to update the same resource were in
-- conflict.
_OperationAbortedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationAbortedException =
  Core._MatchServiceError
    defaultService
    "OperationAbortedException"

-- | The service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You have reached the maximum number of resources that can be created.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The most likely cause is an invalid Amazon Web Services access key ID or
-- secret key.
_UnrecognizedClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnrecognizedClientException =
  Core._MatchServiceError
    defaultService
    "UnrecognizedClientException"

-- | The event was already logged.
_DataAlreadyAcceptedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataAlreadyAcceptedException =
  Core._MatchServiceError
    defaultService
    "DataAlreadyAcceptedException"

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
