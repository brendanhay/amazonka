-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types
  ( -- * Service configuration
    cloudWatchService,

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
    AlarmHistoryItem (..),
    mkAlarmHistoryItem,
    ahiAlarmName,
    ahiHistoryItemType,
    ahiHistoryData,
    ahiAlarmType,
    ahiHistorySummary,
    ahiTimestamp,

    -- * AnomalyDetector
    AnomalyDetector (..),
    mkAnomalyDetector,
    adMetricName,
    adNamespace,
    adStateValue,
    adStat,
    adConfiguration,
    adDimensions,

    -- * AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (..),
    mkAnomalyDetectorConfiguration,
    adcMetricTimezone,
    adcExcludedTimeRanges,

    -- * CompositeAlarm
    CompositeAlarm (..),
    mkCompositeAlarm,
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
    DashboardEntry (..),
    mkDashboardEntry,
    deSize,
    deDashboardName,
    deLastModified,
    deDashboardARN,

    -- * DashboardValidationMessage
    DashboardValidationMessage (..),
    mkDashboardValidationMessage,
    dvmDataPath,
    dvmMessage,

    -- * Datapoint
    Datapoint (..),
    mkDatapoint,
    dSampleCount,
    dMaximum,
    dAverage,
    dMinimum,
    dExtendedStatistics,
    dSum,
    dUnit,
    dTimestamp,

    -- * Dimension
    Dimension (..),
    mkDimension,
    dValue,
    dName,

    -- * DimensionFilter
    DimensionFilter (..),
    mkDimensionFilter,
    dfValue,
    dfName,

    -- * InsightRule
    InsightRule (..),
    mkInsightRule,
    irState,
    irDefinition,
    irSchema,
    irName,

    -- * InsightRuleContributor
    InsightRuleContributor (..),
    mkInsightRuleContributor,
    ircDatapoints,
    ircApproximateAggregateValue,
    ircKeys,

    -- * InsightRuleContributorDatapoint
    InsightRuleContributorDatapoint (..),
    mkInsightRuleContributorDatapoint,
    ircdApproximateValue,
    ircdTimestamp,

    -- * InsightRuleMetricDatapoint
    InsightRuleMetricDatapoint (..),
    mkInsightRuleMetricDatapoint,
    irmdMaxContributorValue,
    irmdSampleCount,
    irmdMaximum,
    irmdAverage,
    irmdMinimum,
    irmdUniqueContributors,
    irmdSum,
    irmdTimestamp,

    -- * MessageData
    MessageData (..),
    mkMessageData,
    mValue,
    mCode,

    -- * Metric
    Metric (..),
    mkMetric,
    mMetricName,
    mNamespace,
    mDimensions,

    -- * MetricAlarm
    MetricAlarm (..),
    mkMetricAlarm,
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
    MetricDataQuery (..),
    mkMetricDataQuery,
    mdqReturnData,
    mdqPeriod,
    mdqExpression,
    mdqId,
    mdqLabel,
    mdqMetricStat,

    -- * MetricDataResult
    MetricDataResult (..),
    mkMetricDataResult,
    mdrValues,
    mdrId,
    mdrTimestamps,
    mdrMessages,
    mdrLabel,
    mdrStatusCode,

    -- * MetricDatum
    MetricDatum (..),
    mkMetricDatum,
    mdValues,
    mdCounts,
    mdMetricName,
    mdValue,
    mdStorageResolution,
    mdDimensions,
    mdUnit,
    mdTimestamp,
    mdStatisticValues,

    -- * MetricStat
    MetricStat (..),
    mkMetricStat,
    msPeriod,
    msMetric,
    msStat,
    msUnit,

    -- * PartialFailure
    PartialFailure (..),
    mkPartialFailure,
    pfFailureResource,
    pfFailureCode,
    pfFailureDescription,
    pfExceptionType,

    -- * Range
    Range (..),
    mkRange,
    rStartTime,
    rEndTime,

    -- * StatisticSet
    StatisticSet (..),
    mkStatisticSet,
    ssSampleCount,
    ssMaximum,
    ssMinimum,
    ssSum,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
cloudWatchService :: Lude.Service
cloudWatchService =
  Lude.Service
    { Lude._svcAbbrev = "CloudWatch",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "monitoring",
      Lude._svcVersion = "2010-08-01",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudWatchService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "CloudWatch",
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
