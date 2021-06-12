{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DashboardNotFoundError,
    _LimitExceededFault,
    _InvalidParameterCombinationException,
    _MissingRequiredParameterException,
    _DashboardInvalidInputError,
    _InternalServiceFault,
    _ConcurrentModificationException,
    _InvalidParameterValueException,
    _LimitExceededException,
    _ResourceNotFound,
    _ResourceNotFoundException,
    _InvalidFormatFault,
    _InvalidNextToken,

    -- * AlarmType
    AlarmType (..),

    -- * AnomalyDetectorStateValue
    AnomalyDetectorStateValue (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * HistoryItemType
    HistoryItemType (..),

    -- * RecentlyActive
    RecentlyActive (..),

    -- * ScanBy
    ScanBy (..),

    -- * StandardUnit
    StandardUnit (..),

    -- * StateValue
    StateValue (..),

    -- * Statistic
    Statistic (..),

    -- * StatusCode
    StatusCode (..),

    -- * AlarmHistoryItem
    AlarmHistoryItem (..),
    newAlarmHistoryItem,
    alarmHistoryItem_historySummary,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_alarmName,
    alarmHistoryItem_timestamp,
    alarmHistoryItem_alarmType,
    alarmHistoryItem_historyData,

    -- * AnomalyDetector
    AnomalyDetector (..),
    newAnomalyDetector,
    anomalyDetector_metricName,
    anomalyDetector_configuration,
    anomalyDetector_stateValue,
    anomalyDetector_dimensions,
    anomalyDetector_namespace,
    anomalyDetector_stat,

    -- * AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (..),
    newAnomalyDetectorConfiguration,
    anomalyDetectorConfiguration_metricTimezone,
    anomalyDetectorConfiguration_excludedTimeRanges,

    -- * CompositeAlarm
    CompositeAlarm (..),
    newCompositeAlarm,
    compositeAlarm_alarmArn,
    compositeAlarm_alarmActions,
    compositeAlarm_stateReason,
    compositeAlarm_stateReasonData,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_alarmRule,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_stateValue,
    compositeAlarm_alarmName,
    compositeAlarm_oKActions,
    compositeAlarm_actionsEnabled,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_alarmDescription,

    -- * DashboardEntry
    DashboardEntry (..),
    newDashboardEntry,
    dashboardEntry_dashboardArn,
    dashboardEntry_lastModified,
    dashboardEntry_dashboardName,
    dashboardEntry_size,

    -- * DashboardValidationMessage
    DashboardValidationMessage (..),
    newDashboardValidationMessage,
    dashboardValidationMessage_message,
    dashboardValidationMessage_dataPath,

    -- * Datapoint
    Datapoint (..),
    newDatapoint,
    datapoint_unit,
    datapoint_minimum,
    datapoint_sum,
    datapoint_sampleCount,
    datapoint_timestamp,
    datapoint_average,
    datapoint_maximum,
    datapoint_extendedStatistics,

    -- * Dimension
    Dimension (..),
    newDimension,
    dimension_name,
    dimension_value,

    -- * DimensionFilter
    DimensionFilter (..),
    newDimensionFilter,
    dimensionFilter_value,
    dimensionFilter_name,

    -- * InsightRule
    InsightRule (..),
    newInsightRule,
    insightRule_name,
    insightRule_state,
    insightRule_schema,
    insightRule_definition,

    -- * InsightRuleContributor
    InsightRuleContributor (..),
    newInsightRuleContributor,
    insightRuleContributor_keys,
    insightRuleContributor_approximateAggregateValue,
    insightRuleContributor_datapoints,

    -- * InsightRuleContributorDatapoint
    InsightRuleContributorDatapoint (..),
    newInsightRuleContributorDatapoint,
    insightRuleContributorDatapoint_timestamp,
    insightRuleContributorDatapoint_approximateValue,

    -- * InsightRuleMetricDatapoint
    InsightRuleMetricDatapoint (..),
    newInsightRuleMetricDatapoint,
    insightRuleMetricDatapoint_minimum,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_maximum,
    insightRuleMetricDatapoint_timestamp,

    -- * LabelOptions
    LabelOptions (..),
    newLabelOptions,
    labelOptions_timezone,

    -- * MessageData
    MessageData (..),
    newMessageData,
    messageData_code,
    messageData_value,

    -- * Metric
    Metric (..),
    newMetric,
    metric_metricName,
    metric_dimensions,
    metric_namespace,

    -- * MetricAlarm
    MetricAlarm (..),
    newMetricAlarm,
    metricAlarm_threshold,
    metricAlarm_datapointsToAlarm,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_comparisonOperator,
    metricAlarm_extendedStatistic,
    metricAlarm_alarmArn,
    metricAlarm_alarmActions,
    metricAlarm_unit,
    metricAlarm_thresholdMetricId,
    metricAlarm_stateReason,
    metricAlarm_stateReasonData,
    metricAlarm_metricName,
    metricAlarm_insufficientDataActions,
    metricAlarm_treatMissingData,
    metricAlarm_metrics,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_stateValue,
    metricAlarm_alarmName,
    metricAlarm_oKActions,
    metricAlarm_statistic,
    metricAlarm_dimensions,
    metricAlarm_namespace,
    metricAlarm_evaluationPeriods,
    metricAlarm_actionsEnabled,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_alarmDescription,
    metricAlarm_period,

    -- * MetricDataQuery
    MetricDataQuery (..),
    newMetricDataQuery,
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
    metricDataQuery_label,
    metricDataQuery_period,
    metricDataQuery_expression,
    metricDataQuery_id,

    -- * MetricDataResult
    MetricDataResult (..),
    newMetricDataResult,
    metricDataResult_values,
    metricDataResult_id,
    metricDataResult_timestamps,
    metricDataResult_statusCode,
    metricDataResult_label,
    metricDataResult_messages,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_storageResolution,
    metricDatum_unit,
    metricDatum_values,
    metricDatum_counts,
    metricDatum_timestamp,
    metricDatum_statisticValues,
    metricDatum_value,
    metricDatum_dimensions,
    metricDatum_metricName,

    -- * MetricStat
    MetricStat (..),
    newMetricStat,
    metricStat_unit,
    metricStat_metric,
    metricStat_period,
    metricStat_stat,

    -- * PartialFailure
    PartialFailure (..),
    newPartialFailure,
    partialFailure_exceptionType,
    partialFailure_failureCode,
    partialFailure_failureDescription,
    partialFailure_failureResource,

    -- * Range
    Range (..),
    newRange,
    range_startTime,
    range_endTime,

    -- * StatisticSet
    StatisticSet (..),
    newStatisticSet,
    statisticSet_sampleCount,
    statisticSet_sum,
    statisticSet_minimum,
    statisticSet_maximum,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudWatch.Types.AlarmHistoryItem
import Network.AWS.CloudWatch.Types.AlarmType
import Network.AWS.CloudWatch.Types.AnomalyDetector
import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
import Network.AWS.CloudWatch.Types.ComparisonOperator
import Network.AWS.CloudWatch.Types.CompositeAlarm
import Network.AWS.CloudWatch.Types.DashboardEntry
import Network.AWS.CloudWatch.Types.DashboardValidationMessage
import Network.AWS.CloudWatch.Types.Datapoint
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.DimensionFilter
import Network.AWS.CloudWatch.Types.HistoryItemType
import Network.AWS.CloudWatch.Types.InsightRule
import Network.AWS.CloudWatch.Types.InsightRuleContributor
import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
import Network.AWS.CloudWatch.Types.LabelOptions
import Network.AWS.CloudWatch.Types.MessageData
import Network.AWS.CloudWatch.Types.Metric
import Network.AWS.CloudWatch.Types.MetricAlarm
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.MetricDataResult
import Network.AWS.CloudWatch.Types.MetricDatum
import Network.AWS.CloudWatch.Types.MetricStat
import Network.AWS.CloudWatch.Types.PartialFailure
import Network.AWS.CloudWatch.Types.Range
import Network.AWS.CloudWatch.Types.RecentlyActive
import Network.AWS.CloudWatch.Types.ScanBy
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StateValue
import Network.AWS.CloudWatch.Types.Statistic
import Network.AWS.CloudWatch.Types.StatisticSet
import Network.AWS.CloudWatch.Types.StatusCode
import Network.AWS.CloudWatch.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudWatch",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "monitoring",
      Core._serviceSigningName = "monitoring",
      Core._serviceVersion = "2010-08-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "CloudWatch",
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
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The specified dashboard does not exist.
_DashboardNotFoundError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DashboardNotFoundError =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Core.. Core.hasStatus 404

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Core.. Core.hasStatus 400

-- | Parameters were used together that cannot be used together.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Core.. Core.hasStatus 400

-- | An input parameter that is required is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingParameter"
    Core.. Core.hasStatus 400

-- | Some part of the dashboard data is invalid.
_DashboardInvalidInputError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DashboardInvalidInputError =
  Core._MatchServiceError
    defaultService
    "InvalidParameterInput"
    Core.. Core.hasStatus 400

-- | Request processing has failed due to some unknown error, exception, or
-- failure.
_InternalServiceFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceFault =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
    Core.. Core.hasStatus 500

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Core.. Core.hasStatus 429

-- | The value of an input parameter is bad or out-of-range.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Core.. Core.hasStatus 400

-- | The operation exceeded one or more limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Core.. Core.hasStatus 400

-- | The named resource does not exist.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Core.. Core.hasStatus 404

-- | The named resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Core.. Core.hasStatus 404

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFormatFault =
  Core._MatchServiceError
    defaultService
    "InvalidFormat"
    Core.. Core.hasStatus 400

-- | The next token specified is invalid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Core.. Core.hasStatus 400
