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
    cloudWatchLogsService,

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
    Destination (..),
    mkDestination,
    dTargetARN,
    dCreationTime,
    dArn,
    dAccessPolicy,
    dDestinationName,
    dRoleARN,

    -- * ExportTask
    ExportTask (..),
    mkExportTask,
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
    ExportTaskExecutionInfo (..),
    mkExportTaskExecutionInfo,
    eteiCreationTime,
    eteiCompletionTime,

    -- * ExportTaskStatus
    ExportTaskStatus (..),
    mkExportTaskStatus,
    etsCode,
    etsMessage,

    -- * FilteredLogEvent
    FilteredLogEvent (..),
    mkFilteredLogEvent,
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,
    fleEventId,

    -- * InputLogEvent
    InputLogEvent (..),
    mkInputLogEvent,
    ileMessage,
    ileTimestamp,

    -- * LogGroup
    LogGroup (..),
    mkLogGroup,
    lgCreationTime,
    lgMetricFilterCount,
    lgArn,
    lgLogGroupName,
    lgRetentionInDays,
    lgKmsKeyId,
    lgStoredBytes,

    -- * LogGroupField
    LogGroupField (..),
    mkLogGroupField,
    lgfPercent,
    lgfName,

    -- * LogStream
    LogStream (..),
    mkLogStream,
    lsCreationTime,
    lsUploadSequenceToken,
    lsArn,
    lsFirstEventTimestamp,
    lsLogStreamName,
    lsStoredBytes,
    lsLastIngestionTime,
    lsLastEventTimestamp,

    -- * MetricFilter
    MetricFilter (..),
    mkMetricFilter,
    mfCreationTime,
    mfFilterName,
    mfLogGroupName,
    mfFilterPattern,
    mfMetricTransformations,

    -- * MetricFilterMatchRecord
    MetricFilterMatchRecord (..),
    mkMetricFilterMatchRecord,
    mfmrExtractedValues,
    mfmrEventNumber,
    mfmrEventMessage,

    -- * MetricTransformation
    MetricTransformation (..),
    mkMetricTransformation,
    mtMetricName,
    mtMetricNamespace,
    mtMetricValue,
    mtDefaultValue,

    -- * OutputLogEvent
    OutputLogEvent (..),
    mkOutputLogEvent,
    oleIngestionTime,
    oleMessage,
    oleTimestamp,

    -- * QueryDefinition
    QueryDefinition (..),
    mkQueryDefinition,
    qdLogGroupNames,
    qdQueryDefinitionId,
    qdName,
    qdQueryString,
    qdLastModified,

    -- * QueryInfo
    QueryInfo (..),
    mkQueryInfo,
    qiStatus,
    qiQueryId,
    qiLogGroupName,
    qiQueryString,
    qiCreateTime,

    -- * QueryStatistics
    QueryStatistics (..),
    mkQueryStatistics,
    qsRecordsScanned,
    qsBytesScanned,
    qsRecordsMatched,

    -- * RejectedLogEventsInfo
    RejectedLogEventsInfo (..),
    mkRejectedLogEventsInfo,
    rleiTooOldLogEventEndIndex,
    rleiTooNewLogEventStartIndex,
    rleiExpiredLogEventEndIndex,

    -- * ResourcePolicy
    ResourcePolicy (..),
    mkResourcePolicy,
    rpPolicyName,
    rpPolicyDocument,
    rpLastUpdatedTime,

    -- * ResultField
    ResultField (..),
    mkResultField,
    rfField,
    rfValue,

    -- * SearchedLogStream
    SearchedLogStream (..),
    mkSearchedLogStream,
    slsLogStreamName,
    slsSearchedCompletely,

    -- * SubscriptionFilter
    SubscriptionFilter (..),
    mkSubscriptionFilter,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-03-28@ of the Amazon CloudWatch Logs SDK configuration.
cloudWatchLogsService :: Lude.Service
cloudWatchLogsService =
  Lude.Service
    { Lude._svcAbbrev = "CloudWatchLogs",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "logs",
      Lude._svcVersion = "2014-03-28",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudWatchLogsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudWatchLogs",
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
