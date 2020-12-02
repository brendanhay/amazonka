{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types
  ( -- * Service Configuration
    cloudWatch,

    -- * Errors

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
    AlarmHistoryItem,
    alarmHistoryItem,
    ahiAlarmName,
    ahiHistoryItemType,
    ahiHistoryData,
    ahiAlarmType,
    ahiHistorySummary,
    ahiTimestamp,

    -- * AnomalyDetector
    AnomalyDetector,
    anomalyDetector,
    adMetricName,
    adNamespace,
    adStateValue,
    adStat,
    adConfiguration,
    adDimensions,

    -- * AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration,
    anomalyDetectorConfiguration,
    adcMetricTimezone,
    adcExcludedTimeRanges,

    -- * CompositeAlarm
    CompositeAlarm,
    compositeAlarm,
    caAlarmName,
    caStateUpdatedTimestamp,
    caAlarmDescription,
    caAlarmRule,
    caOKActions,
    caStateValue,
    caAlarmConfigurationUpdatedTimestamp,
    caActionsEnabled,
    caInsufficientDataActions,
    caStateReason,
    caStateReasonData,
    caAlarmARN,
    caAlarmActions,

    -- * DashboardEntry
    DashboardEntry,
    dashboardEntry,
    deSize,
    deDashboardName,
    deLastModified,
    deDashboardARN,

    -- * DashboardValidationMessage
    DashboardValidationMessage,
    dashboardValidationMessage,
    dvmDataPath,
    dvmMessage,

    -- * Datapoint
    Datapoint,
    datapoint,
    dSampleCount,
    dMaximum,
    dAverage,
    dMinimum,
    dExtendedStatistics,
    dSum,
    dUnit,
    dTimestamp,

    -- * Dimension
    Dimension,
    dimension,
    dName,
    dValue,

    -- * DimensionFilter
    DimensionFilter,
    dimensionFilter,
    dfValue,
    dfName,

    -- * InsightRule
    InsightRule,
    insightRule,
    irName,
    irState,
    irSchema,
    irDefinition,

    -- * InsightRuleContributor
    InsightRuleContributor,
    insightRuleContributor,
    ircKeys,
    ircApproximateAggregateValue,
    ircDatapoints,

    -- * InsightRuleContributorDatapoint
    InsightRuleContributorDatapoint,
    insightRuleContributorDatapoint,
    ircdTimestamp,
    ircdApproximateValue,

    -- * InsightRuleMetricDatapoint
    InsightRuleMetricDatapoint,
    insightRuleMetricDatapoint,
    irmdMaxContributorValue,
    irmdSampleCount,
    irmdMaximum,
    irmdAverage,
    irmdMinimum,
    irmdUniqueContributors,
    irmdSum,
    irmdTimestamp,

    -- * MessageData
    MessageData,
    messageData,
    mValue,
    mCode,

    -- * Metric
    Metric,
    metric,
    mMetricName,
    mNamespace,
    mDimensions,

    -- * MetricAlarm
    MetricAlarm,
    metricAlarm,
    maAlarmName,
    maStateUpdatedTimestamp,
    maMetrics,
    maTreatMissingData,
    maPeriod,
    maAlarmDescription,
    maEvaluationPeriods,
    maMetricName,
    maNamespace,
    maThresholdMetricId,
    maComparisonOperator,
    maOKActions,
    maEvaluateLowSampleCountPercentile,
    maStateValue,
    maDatapointsToAlarm,
    maThreshold,
    maAlarmConfigurationUpdatedTimestamp,
    maActionsEnabled,
    maInsufficientDataActions,
    maStateReason,
    maStateReasonData,
    maDimensions,
    maAlarmARN,
    maAlarmActions,
    maUnit,
    maStatistic,
    maExtendedStatistic,

    -- * MetricDataQuery
    MetricDataQuery,
    metricDataQuery,
    mdqReturnData,
    mdqPeriod,
    mdqExpression,
    mdqLabel,
    mdqMetricStat,
    mdqId,

    -- * MetricDataResult
    MetricDataResult,
    metricDataResult,
    mdrValues,
    mdrId,
    mdrTimestamps,
    mdrMessages,
    mdrLabel,
    mdrStatusCode,

    -- * MetricDatum
    MetricDatum,
    metricDatum,
    mdValues,
    mdCounts,
    mdValue,
    mdStorageResolution,
    mdDimensions,
    mdUnit,
    mdTimestamp,
    mdStatisticValues,
    mdMetricName,

    -- * MetricStat
    MetricStat,
    metricStat,
    msUnit,
    msMetric,
    msPeriod,
    msStat,

    -- * PartialFailure
    PartialFailure,
    partialFailure,
    pfFailureResource,
    pfFailureCode,
    pfFailureDescription,
    pfExceptionType,

    -- * Range
    Range,
    range,
    rStartTime,
    rEndTime,

    -- * StatisticSet
    StatisticSet,
    statisticSet,
    ssSampleCount,
    ssSum,
    ssMinimum,
    ssMaximum,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
cloudWatch :: Service
cloudWatch =
  Service
    { _svcAbbrev = "CloudWatch",
      _svcSigner = v4,
      _svcPrefix = "monitoring",
      _svcVersion = "2010-08-01",
      _svcEndpoint = defaultEndpoint cloudWatch,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "CloudWatch",
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
