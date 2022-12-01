{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DashboardInvalidInputError,
    _ResourceNotFound,
    _ConcurrentModificationException,
    _InvalidNextToken,
    _LimitExceededFault,
    _InternalServiceFault,
    _MissingRequiredParameterException,
    _DashboardNotFoundError,
    _ResourceNotFoundException,
    _InvalidParameterCombinationException,
    _LimitExceededException,
    _InvalidFormatFault,
    _InvalidParameterValueException,

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
    alarmHistoryItem_alarmType,
    alarmHistoryItem_timestamp,
    alarmHistoryItem_historyData,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_historySummary,
    alarmHistoryItem_alarmName,

    -- * AnomalyDetector
    AnomalyDetector (..),
    newAnomalyDetector,
    anomalyDetector_singleMetricAnomalyDetector,
    anomalyDetector_configuration,
    anomalyDetector_dimensions,
    anomalyDetector_metricMathAnomalyDetector,
    anomalyDetector_stat,
    anomalyDetector_metricName,
    anomalyDetector_stateValue,
    anomalyDetector_namespace,

    -- * AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (..),
    newAnomalyDetectorConfiguration,
    anomalyDetectorConfiguration_metricTimezone,
    anomalyDetectorConfiguration_excludedTimeRanges,

    -- * CompositeAlarm
    CompositeAlarm (..),
    newCompositeAlarm,
    compositeAlarm_alarmActions,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_actionsSuppressorExtensionPeriod,
    compositeAlarm_alarmDescription,
    compositeAlarm_actionsEnabled,
    compositeAlarm_actionsSuppressedBy,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_stateTransitionedTimestamp,
    compositeAlarm_alarmArn,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_stateValue,
    compositeAlarm_stateReasonData,
    compositeAlarm_oKActions,
    compositeAlarm_actionsSuppressor,
    compositeAlarm_actionsSuppressorWaitPeriod,
    compositeAlarm_alarmName,
    compositeAlarm_actionsSuppressedReason,
    compositeAlarm_alarmRule,
    compositeAlarm_stateReason,

    -- * DashboardEntry
    DashboardEntry (..),
    newDashboardEntry,
    dashboardEntry_size,
    dashboardEntry_lastModified,
    dashboardEntry_dashboardName,
    dashboardEntry_dashboardArn,

    -- * DashboardValidationMessage
    DashboardValidationMessage (..),
    newDashboardValidationMessage,
    dashboardValidationMessage_message,
    dashboardValidationMessage_dataPath,

    -- * Datapoint
    Datapoint (..),
    newDatapoint,
    datapoint_minimum,
    datapoint_average,
    datapoint_timestamp,
    datapoint_sampleCount,
    datapoint_sum,
    datapoint_extendedStatistics,
    datapoint_maximum,
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
    insightRuleMetricDatapoint_minimum,
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_maximum,
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
    managedRuleDescription_templateName,
    managedRuleDescription_ruleState,
    managedRuleDescription_resourceARN,

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
    metricAlarm_alarmActions,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_alarmDescription,
    metricAlarm_extendedStatistic,
    metricAlarm_actionsEnabled,
    metricAlarm_period,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_dimensions,
    metricAlarm_thresholdMetricId,
    metricAlarm_treatMissingData,
    metricAlarm_metrics,
    metricAlarm_evaluationPeriods,
    metricAlarm_datapointsToAlarm,
    metricAlarm_insufficientDataActions,
    metricAlarm_alarmArn,
    metricAlarm_metricName,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_threshold,
    metricAlarm_stateValue,
    metricAlarm_stateReasonData,
    metricAlarm_oKActions,
    metricAlarm_alarmName,
    metricAlarm_comparisonOperator,
    metricAlarm_namespace,
    metricAlarm_statistic,
    metricAlarm_unit,
    metricAlarm_stateReason,

    -- * MetricDataQuery
    MetricDataQuery (..),
    newMetricDataQuery,
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
    metricDataQuery_label,
    metricDataQuery_period,
    metricDataQuery_expression,
    metricDataQuery_accountId,
    metricDataQuery_id,

    -- * MetricDataResult
    MetricDataResult (..),
    newMetricDataResult,
    metricDataResult_timestamps,
    metricDataResult_label,
    metricDataResult_id,
    metricDataResult_messages,
    metricDataResult_values,
    metricDataResult_statusCode,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_statisticValues,
    metricDatum_dimensions,
    metricDatum_timestamp,
    metricDatum_counts,
    metricDatum_values,
    metricDatum_unit,
    metricDatum_storageResolution,
    metricDatum_value,
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
    metricStreamEntry_name,
    metricStreamEntry_arn,
    metricStreamEntry_state,
    metricStreamEntry_creationDate,
    metricStreamEntry_lastUpdateDate,
    metricStreamEntry_outputFormat,
    metricStreamEntry_firehoseArn,

    -- * MetricStreamFilter
    MetricStreamFilter (..),
    newMetricStreamFilter,
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
    partialFailure_failureDescription,
    partialFailure_failureCode,
    partialFailure_exceptionType,
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
    singleMetricAnomalyDetector_stat,
    singleMetricAnomalyDetector_metricName,
    singleMetricAnomalyDetector_namespace,

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

-- | Some part of the dashboard data is invalid.
_DashboardInvalidInputError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DashboardInvalidInputError =
  Core._MatchServiceError
    defaultService
    "InvalidParameterInput"
    Prelude.. Core.hasStatus 400

-- | The named resource does not exist.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 429

-- | The next token specified is invalid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Core.hasStatus 400

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 400

-- | Request processing has failed due to some unknown error, exception, or
-- failure.
_InternalServiceFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceFault =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
    Prelude.. Core.hasStatus 500

-- | An input parameter that is required is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingParameter"
    Prelude.. Core.hasStatus 400

-- | The specified dashboard does not exist.
_DashboardNotFoundError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DashboardNotFoundError =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The named resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Parameters were used together that cannot be used together.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Core.hasStatus 400

-- | The operation exceeded one or more limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFormatFault =
  Core._MatchServiceError
    defaultService
    "InvalidFormat"
    Prelude.. Core.hasStatus 400

-- | The value of an input parameter is bad or out-of-range.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Core.hasStatus 400
