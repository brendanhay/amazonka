{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types
  ( -- * Service Configuration
    cloudWatchLogs,

    -- * Errors

    -- * Distribution
    Distribution (..),

    -- * ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- * OrderBy
    OrderBy (..),

    -- * QueryStatus
    QueryStatus (..),

    -- * Destination
    Destination,
    destination,
    dTargetARN,
    dCreationTime,
    dArn,
    dAccessPolicy,
    dDestinationName,
    dRoleARN,

    -- * ExportTask
    ExportTask,
    exportTask,
    etDestinationPrefix,
    etDestination,
    etStatus,
    etTaskName,
    etTaskId,
    etTo,
    etFrom,
    etLogGroupName,
    etExecutionInfo,

    -- * ExportTaskExecutionInfo
    ExportTaskExecutionInfo,
    exportTaskExecutionInfo,
    eteiCreationTime,
    eteiCompletionTime,

    -- * ExportTaskStatus
    ExportTaskStatus,
    exportTaskStatus,
    etsCode,
    etsMessage,

    -- * FilteredLogEvent
    FilteredLogEvent,
    filteredLogEvent,
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,
    fleEventId,

    -- * InputLogEvent
    InputLogEvent,
    inputLogEvent,
    ileTimestamp,
    ileMessage,

    -- * LogGroup
    LogGroup,
    logGroup,
    lgCreationTime,
    lgMetricFilterCount,
    lgArn,
    lgLogGroupName,
    lgRetentionInDays,
    lgKmsKeyId,
    lgStoredBytes,

    -- * LogGroupField
    LogGroupField,
    logGroupField,
    lgfPercent,
    lgfName,

    -- * LogStream
    LogStream,
    logStream,
    lsCreationTime,
    lsUploadSequenceToken,
    lsArn,
    lsFirstEventTimestamp,
    lsLogStreamName,
    lsStoredBytes,
    lsLastIngestionTime,
    lsLastEventTimestamp,

    -- * MetricFilter
    MetricFilter,
    metricFilter,
    mfCreationTime,
    mfFilterName,
    mfLogGroupName,
    mfFilterPattern,
    mfMetricTransformations,

    -- * MetricFilterMatchRecord
    MetricFilterMatchRecord,
    metricFilterMatchRecord,
    mfmrExtractedValues,
    mfmrEventNumber,
    mfmrEventMessage,

    -- * MetricTransformation
    MetricTransformation,
    metricTransformation,
    mtDefaultValue,
    mtMetricName,
    mtMetricNamespace,
    mtMetricValue,

    -- * OutputLogEvent
    OutputLogEvent,
    outputLogEvent,
    oleIngestionTime,
    oleMessage,
    oleTimestamp,

    -- * QueryDefinition
    QueryDefinition,
    queryDefinition,
    qdLogGroupNames,
    qdQueryDefinitionId,
    qdName,
    qdQueryString,
    qdLastModified,

    -- * QueryInfo
    QueryInfo,
    queryInfo,
    qiStatus,
    qiQueryId,
    qiLogGroupName,
    qiQueryString,
    qiCreateTime,

    -- * QueryStatistics
    QueryStatistics,
    queryStatistics,
    qsRecordsScanned,
    qsBytesScanned,
    qsRecordsMatched,

    -- * RejectedLogEventsInfo
    RejectedLogEventsInfo,
    rejectedLogEventsInfo,
    rleiTooOldLogEventEndIndex,
    rleiTooNewLogEventStartIndex,
    rleiExpiredLogEventEndIndex,

    -- * ResourcePolicy
    ResourcePolicy,
    resourcePolicy,
    rpPolicyName,
    rpPolicyDocument,
    rpLastUpdatedTime,

    -- * ResultField
    ResultField,
    resultField,
    rfField,
    rfValue,

    -- * SearchedLogStream
    SearchedLogStream,
    searchedLogStream,
    slsLogStreamName,
    slsSearchedCompletely,

    -- * SubscriptionFilter
    SubscriptionFilter,
    subscriptionFilter,
    sfCreationTime,
    sfFilterName,
    sfDistribution,
    sfDestinationARN,
    sfLogGroupName,
    sfFilterPattern,
    sfRoleARN,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
cloudWatchLogs :: Service
cloudWatchLogs =
  Service
    { _svcAbbrev = "CloudWatchLogs",
      _svcSigner = v4,
      _svcPrefix = "logs",
      _svcVersion = "2014-03-28",
      _svcEndpoint = defaultEndpoint cloudWatchLogs,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CloudWatchLogs",
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
