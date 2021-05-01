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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CloudWatch",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "monitoring",
      Prelude._svcSigningName = "monitoring",
      Prelude._svcVersion = "2010-08-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseXMLError "CloudWatch",
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

-- | The specified dashboard does not exist.
_DashboardNotFoundError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DashboardNotFoundError =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Parameters were used together that cannot be used together.
_InvalidParameterCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Prelude.hasStatus 400

-- | An input parameter that is required is missing.
_MissingRequiredParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingRequiredParameterException =
  Prelude._MatchServiceError
    defaultService
    "MissingParameter"
    Prelude.. Prelude.hasStatus 400

-- | Some part of the dashboard data is invalid.
_DashboardInvalidInputError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DashboardInvalidInputError =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterInput"
    Prelude.. Prelude.hasStatus 400

-- | Request processing has failed due to some unknown error, exception, or
-- failure.
_InternalServiceFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceFault =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceError"
    Prelude.. Prelude.hasStatus 500

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Prelude.hasStatus 429

-- | The value of an input parameter is bad or out-of-range.
_InvalidParameterValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Prelude.hasStatus 400

-- | The operation exceeded one or more limits.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The named resource does not exist.
_ResourceNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFound =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The named resource does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFormatFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidFormat"
    Prelude.. Prelude.hasStatus 400

-- | The next token specified is invalid.
_InvalidNextToken :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextToken =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Prelude.hasStatus 400
