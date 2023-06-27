{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _DashboardInvalidInputError,
    _DashboardNotFoundError,
    _InternalServiceFault,
    _InvalidFormatFault,
    _InvalidNextToken,
    _InvalidParameterCombinationException,
    _InvalidParameterValueException,
    _LimitExceededException,
    _LimitExceededFault,
    _MissingRequiredParameterException,
    _ResourceNotFound,
    _ResourceNotFoundException,

    -- * ActionsSuppressedBy
    ActionsSuppressedBy (..),

    -- * AlarmType
    AlarmType (..),

    -- * AnomalyDetectorStateValue
    AnomalyDetectorStateValue (..),

    -- * AnomalyDetectorType
    AnomalyDetectorType (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * EvaluationState
    EvaluationState (..),

    -- * HistoryItemType
    HistoryItemType (..),

    -- * MetricStreamOutputFormat
    MetricStreamOutputFormat (..),

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
    alarmHistoryItem_alarmName,
    alarmHistoryItem_alarmType,
    alarmHistoryItem_historyData,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_historySummary,
    alarmHistoryItem_timestamp,

    -- * AnomalyDetector
    AnomalyDetector (..),
    newAnomalyDetector,
    anomalyDetector_configuration,
    anomalyDetector_dimensions,
    anomalyDetector_metricMathAnomalyDetector,
    anomalyDetector_metricName,
    anomalyDetector_namespace,
    anomalyDetector_singleMetricAnomalyDetector,
    anomalyDetector_stat,
    anomalyDetector_stateValue,

    -- * AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (..),
    newAnomalyDetectorConfiguration,
    anomalyDetectorConfiguration_excludedTimeRanges,
    anomalyDetectorConfiguration_metricTimezone,

    -- * CompositeAlarm
    CompositeAlarm (..),
    newCompositeAlarm,
    compositeAlarm_actionsEnabled,
    compositeAlarm_actionsSuppressedBy,
    compositeAlarm_actionsSuppressedReason,
    compositeAlarm_actionsSuppressor,
    compositeAlarm_actionsSuppressorExtensionPeriod,
    compositeAlarm_actionsSuppressorWaitPeriod,
    compositeAlarm_alarmActions,
    compositeAlarm_alarmArn,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_alarmDescription,
    compositeAlarm_alarmName,
    compositeAlarm_alarmRule,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_oKActions,
    compositeAlarm_stateReason,
    compositeAlarm_stateReasonData,
    compositeAlarm_stateTransitionedTimestamp,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_stateValue,

    -- * DashboardEntry
    DashboardEntry (..),
    newDashboardEntry,
    dashboardEntry_dashboardArn,
    dashboardEntry_dashboardName,
    dashboardEntry_lastModified,
    dashboardEntry_size,

    -- * DashboardValidationMessage
    DashboardValidationMessage (..),
    newDashboardValidationMessage,
    dashboardValidationMessage_dataPath,
    dashboardValidationMessage_message,

    -- * Datapoint
    Datapoint (..),
    newDatapoint,
    datapoint_average,
    datapoint_extendedStatistics,
    datapoint_maximum,
    datapoint_minimum,
    datapoint_sampleCount,
    datapoint_sum,
    datapoint_timestamp,
    datapoint_unit,

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
    insightRule_managedRule,
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
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_maximum,
    insightRuleMetricDatapoint_minimum,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_timestamp,

    -- * LabelOptions
    LabelOptions (..),
    newLabelOptions,
    labelOptions_timezone,

    -- * ManagedRule
    ManagedRule (..),
    newManagedRule,
    managedRule_tags,
    managedRule_templateName,
    managedRule_resourceARN,

    -- * ManagedRuleDescription
    ManagedRuleDescription (..),
    newManagedRuleDescription,
    managedRuleDescription_resourceARN,
    managedRuleDescription_ruleState,
    managedRuleDescription_templateName,

    -- * ManagedRuleState
    ManagedRuleState (..),
    newManagedRuleState,
    managedRuleState_ruleName,
    managedRuleState_state,

    -- * MessageData
    MessageData (..),
    newMessageData,
    messageData_code,
    messageData_value,

    -- * Metric
    Metric (..),
    newMetric,
    metric_dimensions,
    metric_metricName,
    metric_namespace,

    -- * MetricAlarm
    MetricAlarm (..),
    newMetricAlarm,
    metricAlarm_actionsEnabled,
    metricAlarm_alarmActions,
    metricAlarm_alarmArn,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_alarmDescription,
    metricAlarm_alarmName,
    metricAlarm_comparisonOperator,
    metricAlarm_datapointsToAlarm,
    metricAlarm_dimensions,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_evaluationPeriods,
    metricAlarm_evaluationState,
    metricAlarm_extendedStatistic,
    metricAlarm_insufficientDataActions,
    metricAlarm_metricName,
    metricAlarm_metrics,
    metricAlarm_namespace,
    metricAlarm_oKActions,
    metricAlarm_period,
    metricAlarm_stateReason,
    metricAlarm_stateReasonData,
    metricAlarm_stateTransitionedTimestamp,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_stateValue,
    metricAlarm_statistic,
    metricAlarm_threshold,
    metricAlarm_thresholdMetricId,
    metricAlarm_treatMissingData,
    metricAlarm_unit,

    -- * MetricDataQuery
    MetricDataQuery (..),
    newMetricDataQuery,
    metricDataQuery_accountId,
    metricDataQuery_expression,
    metricDataQuery_label,
    metricDataQuery_metricStat,
    metricDataQuery_period,
    metricDataQuery_returnData,
    metricDataQuery_id,

    -- * MetricDataResult
    MetricDataResult (..),
    newMetricDataResult,
    metricDataResult_id,
    metricDataResult_label,
    metricDataResult_messages,
    metricDataResult_statusCode,
    metricDataResult_timestamps,
    metricDataResult_values,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_counts,
    metricDatum_dimensions,
    metricDatum_statisticValues,
    metricDatum_storageResolution,
    metricDatum_timestamp,
    metricDatum_unit,
    metricDatum_value,
    metricDatum_values,
    metricDatum_metricName,

    -- * MetricMathAnomalyDetector
    MetricMathAnomalyDetector (..),
    newMetricMathAnomalyDetector,
    metricMathAnomalyDetector_metricDataQueries,

    -- * MetricStat
    MetricStat (..),
    newMetricStat,
    metricStat_unit,
    metricStat_metric,
    metricStat_period,
    metricStat_stat,

    -- * MetricStreamEntry
    MetricStreamEntry (..),
    newMetricStreamEntry,
    metricStreamEntry_arn,
    metricStreamEntry_creationDate,
    metricStreamEntry_firehoseArn,
    metricStreamEntry_lastUpdateDate,
    metricStreamEntry_name,
    metricStreamEntry_outputFormat,
    metricStreamEntry_state,

    -- * MetricStreamFilter
    MetricStreamFilter (..),
    newMetricStreamFilter,
    metricStreamFilter_metricNames,
    metricStreamFilter_namespace,

    -- * MetricStreamStatisticsConfiguration
    MetricStreamStatisticsConfiguration (..),
    newMetricStreamStatisticsConfiguration,
    metricStreamStatisticsConfiguration_includeMetrics,
    metricStreamStatisticsConfiguration_additionalStatistics,

    -- * MetricStreamStatisticsMetric
    MetricStreamStatisticsMetric (..),
    newMetricStreamStatisticsMetric,
    metricStreamStatisticsMetric_namespace,
    metricStreamStatisticsMetric_metricName,

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

    -- * SingleMetricAnomalyDetector
    SingleMetricAnomalyDetector (..),
    newSingleMetricAnomalyDetector,
    singleMetricAnomalyDetector_dimensions,
    singleMetricAnomalyDetector_metricName,
    singleMetricAnomalyDetector_namespace,
    singleMetricAnomalyDetector_stat,

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

import Amazonka.CloudWatch.Types.ActionsSuppressedBy
import Amazonka.CloudWatch.Types.AlarmHistoryItem
import Amazonka.CloudWatch.Types.AlarmType
import Amazonka.CloudWatch.Types.AnomalyDetector
import Amazonka.CloudWatch.Types.AnomalyDetectorConfiguration
import Amazonka.CloudWatch.Types.AnomalyDetectorStateValue
import Amazonka.CloudWatch.Types.AnomalyDetectorType
import Amazonka.CloudWatch.Types.ComparisonOperator
import Amazonka.CloudWatch.Types.CompositeAlarm
import Amazonka.CloudWatch.Types.DashboardEntry
import Amazonka.CloudWatch.Types.DashboardValidationMessage
import Amazonka.CloudWatch.Types.Datapoint
import Amazonka.CloudWatch.Types.Dimension
import Amazonka.CloudWatch.Types.DimensionFilter
import Amazonka.CloudWatch.Types.EvaluationState
import Amazonka.CloudWatch.Types.HistoryItemType
import Amazonka.CloudWatch.Types.InsightRule
import Amazonka.CloudWatch.Types.InsightRuleContributor
import Amazonka.CloudWatch.Types.InsightRuleContributorDatapoint
import Amazonka.CloudWatch.Types.InsightRuleMetricDatapoint
import Amazonka.CloudWatch.Types.LabelOptions
import Amazonka.CloudWatch.Types.ManagedRule
import Amazonka.CloudWatch.Types.ManagedRuleDescription
import Amazonka.CloudWatch.Types.ManagedRuleState
import Amazonka.CloudWatch.Types.MessageData
import Amazonka.CloudWatch.Types.Metric
import Amazonka.CloudWatch.Types.MetricAlarm
import Amazonka.CloudWatch.Types.MetricDataQuery
import Amazonka.CloudWatch.Types.MetricDataResult
import Amazonka.CloudWatch.Types.MetricDatum
import Amazonka.CloudWatch.Types.MetricMathAnomalyDetector
import Amazonka.CloudWatch.Types.MetricStat
import Amazonka.CloudWatch.Types.MetricStreamEntry
import Amazonka.CloudWatch.Types.MetricStreamFilter
import Amazonka.CloudWatch.Types.MetricStreamOutputFormat
import Amazonka.CloudWatch.Types.MetricStreamStatisticsConfiguration
import Amazonka.CloudWatch.Types.MetricStreamStatisticsMetric
import Amazonka.CloudWatch.Types.PartialFailure
import Amazonka.CloudWatch.Types.Range
import Amazonka.CloudWatch.Types.RecentlyActive
import Amazonka.CloudWatch.Types.ScanBy
import Amazonka.CloudWatch.Types.SingleMetricAnomalyDetector
import Amazonka.CloudWatch.Types.StandardUnit
import Amazonka.CloudWatch.Types.StateValue
import Amazonka.CloudWatch.Types.Statistic
import Amazonka.CloudWatch.Types.StatisticSet
import Amazonka.CloudWatch.Types.StatusCode
import Amazonka.CloudWatch.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudWatch",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "monitoring",
      Core.signingName = "monitoring",
      Core.version = "2010-08-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "CloudWatch",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 429

-- | Some part of the dashboard data is invalid.
_DashboardInvalidInputError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DashboardInvalidInputError =
  Core._MatchServiceError
    defaultService
    "InvalidParameterInput"
    Prelude.. Core.hasStatus 400

-- | The specified dashboard does not exist.
_DashboardNotFoundError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DashboardNotFoundError =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | Request processing has failed due to some unknown error, exception, or
-- failure.
_InternalServiceFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceFault =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
    Prelude.. Core.hasStatus 500

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidFormatFault =
  Core._MatchServiceError
    defaultService
    "InvalidFormat"
    Prelude.. Core.hasStatus 400

-- | The next token specified is invalid.
_InvalidNextToken :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Core.hasStatus 400

-- | Parameters were used together that cannot be used together.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Core.hasStatus 400

-- | The value of an input parameter is bad or out-of-range.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Core.hasStatus 400

-- | The operation exceeded one or more limits.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 400

-- | An input parameter that is required is missing.
_MissingRequiredParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingParameter"
    Prelude.. Core.hasStatus 400

-- | The named resource does not exist.
_ResourceNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The named resource does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
