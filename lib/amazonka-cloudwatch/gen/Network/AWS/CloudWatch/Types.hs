-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _LimitExceededFault
    , _DashboardNotFoundError
    , _InvalidNextToken
    , _InternalServiceFault
    , _DashboardInvalidInputError
    , _InvalidParameterValueException
    , _ConcurrentModificationException
    , _InvalidFormatFault
    , _MissingRequiredParameterException
    , _ResourceNotFoundException
    , _InvalidParameterCombinationException
    , _LimitExceededException
    , _ResourceNotFound

    -- * AlarmName
    , AlarmName (..)

    -- * DashboardValidationMessage
    , DashboardValidationMessage (..)
    , mkDashboardValidationMessage
    , dvmDataPath
    , dvmMessage

    -- * AlarmNamePrefix
    , AlarmNamePrefix (..)

    -- * StatisticSet
    , StatisticSet (..)
    , mkStatisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum

    -- * AnomalyDetectorMetricTimezone
    , AnomalyDetectorMetricTimezone (..)

    -- * MetricAlarm
    , MetricAlarm (..)
    , mkMetricAlarm
    , maActionsEnabled
    , maAlarmActions
    , maAlarmArn
    , maAlarmConfigurationUpdatedTimestamp
    , maAlarmDescription
    , maAlarmName
    , maComparisonOperator
    , maDatapointsToAlarm
    , maDimensions
    , maEvaluateLowSampleCountPercentile
    , maEvaluationPeriods
    , maExtendedStatistic
    , maInsufficientDataActions
    , maMetricName
    , maMetrics
    , maNamespace
    , maOKActions
    , maPeriod
    , maStateReason
    , maStateReasonData
    , maStateUpdatedTimestamp
    , maStateValue
    , maStatistic
    , maThreshold
    , maThresholdMetricId
    , maTreatMissingData
    , maUnit

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * InsightRuleState
    , InsightRuleState (..)

    -- * HistoryData
    , HistoryData (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * FailureResource
    , FailureResource (..)

    -- * TreatMissingData
    , TreatMissingData (..)

    -- * InsightRuleOrderBy
    , InsightRuleOrderBy (..)

    -- * ResourceName
    , ResourceName (..)

    -- * InsightRuleDefinition
    , InsightRuleDefinition (..)

    -- * MetricDatum
    , MetricDatum (..)
    , mkMetricDatum
    , mdMetricName
    , mdCounts
    , mdDimensions
    , mdStatisticValues
    , mdStorageResolution
    , mdTimestamp
    , mdUnit
    , mdValue
    , mdValues

    -- * AnomalyDetectorMetricStat
    , AnomalyDetectorMetricStat (..)

    -- * FailureCode
    , FailureCode (..)

    -- * DataPath
    , DataPath (..)

    -- * AlarmDescription
    , AlarmDescription (..)

    -- * StandardUnit
    , StandardUnit (..)

    -- * AlarmRule
    , AlarmRule (..)

    -- * InsightRule
    , InsightRule (..)
    , mkInsightRule
    , irName
    , irState
    , irSchema
    , irDefinition

    -- * InsightRuleMetricDatapoint
    , InsightRuleMetricDatapoint (..)
    , mkInsightRuleMetricDatapoint
    , irmdTimestamp
    , irmdAverage
    , irmdMaxContributorValue
    , irmdMaximum
    , irmdMinimum
    , irmdSampleCount
    , irmdSum
    , irmdUniqueContributors

    -- * MessageDataValue
    , MessageDataValue (..)

    -- * DashboardEntry
    , DashboardEntry (..)
    , mkDashboardEntry
    , deDashboardArn
    , deDashboardName
    , deLastModified
    , deSize

    -- * Dimension
    , Dimension (..)
    , mkDimension
    , dName
    , dValue

    -- * MetricName
    , MetricName (..)

    -- * AnomalyDetectorConfiguration
    , AnomalyDetectorConfiguration (..)
    , mkAnomalyDetectorConfiguration
    , adcExcludedTimeRanges
    , adcMetricTimezone

    -- * Namespace
    , Namespace (..)

    -- * DashboardName
    , DashboardName (..)

    -- * DashboardNamePrefix
    , DashboardNamePrefix (..)

    -- * AnomalyDetector
    , AnomalyDetector (..)
    , mkAnomalyDetector
    , adConfiguration
    , adDimensions
    , adMetricName
    , adNamespace
    , adStat
    , adStateValue

    -- * DimensionValue
    , DimensionValue (..)

    -- * InsightRuleMetricName
    , InsightRuleMetricName (..)

    -- * AlarmType
    , AlarmType (..)

    -- * ActionPrefix
    , ActionPrefix (..)

    -- * CompositeAlarm
    , CompositeAlarm (..)
    , mkCompositeAlarm
    , caActionsEnabled
    , caAlarmActions
    , caAlarmArn
    , caAlarmConfigurationUpdatedTimestamp
    , caAlarmDescription
    , caAlarmName
    , caAlarmRule
    , caInsufficientDataActions
    , caOKActions
    , caStateReason
    , caStateReasonData
    , caStateUpdatedTimestamp
    , caStateValue

    -- * FailureDescription
    , FailureDescription (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * MetricId
    , MetricId (..)

    -- * EvaluateLowSampleCountPercentile
    , EvaluateLowSampleCountPercentile (..)

    -- * InsightRuleContributorKey
    , InsightRuleContributorKey (..)

    -- * NextToken
    , NextToken (..)

    -- * ScanBy
    , ScanBy (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem (..)
    , mkAlarmHistoryItem
    , ahiAlarmName
    , ahiAlarmType
    , ahiHistoryData
    , ahiHistoryItemType
    , ahiHistorySummary
    , ahiTimestamp

    -- * Metric
    , Metric (..)
    , mkMetric
    , mDimensions
    , mMetricName
    , mNamespace

    -- * OutputFormat
    , OutputFormat (..)

    -- * StateValue
    , StateValue (..)

    -- * DashboardBody
    , DashboardBody (..)

    -- * Datapoint
    , Datapoint (..)
    , mkDatapoint
    , dAverage
    , dExtendedStatistics
    , dMaximum
    , dMinimum
    , dSampleCount
    , dSum
    , dTimestamp
    , dUnit

    -- * Range
    , Range (..)
    , mkRange
    , rStartTime
    , rEndTime

    -- * AnomalyDetectorStateValue
    , AnomalyDetectorStateValue (..)

    -- * InsightRuleName
    , InsightRuleName (..)

    -- * InsightRuleContributorDatapoint
    , InsightRuleContributorDatapoint (..)
    , mkInsightRuleContributorDatapoint
    , ircdTimestamp
    , ircdApproximateValue

    -- * ExceptionType
    , ExceptionType (..)

    -- * DimensionName
    , DimensionName (..)

    -- * MetricLabel
    , MetricLabel (..)

    -- * RecentlyActive
    , RecentlyActive (..)

    -- * Stat
    , Stat (..)

    -- * InsightRuleContributorKeyLabel
    , InsightRuleContributorKeyLabel (..)

    -- * MetricDataResult
    , MetricDataResult (..)
    , mkMetricDataResult
    , mdrId
    , mdrLabel
    , mdrMessages
    , mdrStatusCode
    , mdrTimestamps
    , mdrValues

    -- * TagKey
    , TagKey (..)

    -- * MetricWidget
    , MetricWidget (..)

    -- * DimensionFilter
    , DimensionFilter (..)
    , mkDimensionFilter
    , dfName
    , dfValue

    -- * StateReason
    , StateReason (..)

    -- * Message
    , Message (..)

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * InsightRuleContributor
    , InsightRuleContributor (..)
    , mkInsightRuleContributor
    , ircKeys
    , ircApproximateAggregateValue
    , ircDatapoints

    -- * StateReasonData
    , StateReasonData (..)

    -- * MessageData
    , MessageData (..)
    , mkMessageData
    , mCode
    , mValue

    -- * PartialFailure
    , PartialFailure (..)
    , mkPartialFailure
    , pfExceptionType
    , pfFailureCode
    , pfFailureDescription
    , pfFailureResource

    -- * MetricDataQuery
    , MetricDataQuery (..)
    , mkMetricDataQuery
    , mdqId
    , mdqExpression
    , mdqLabel
    , mdqMetricStat
    , mdqPeriod
    , mdqReturnData

    -- * AlarmArn
    , AlarmArn (..)

    -- * HistorySummary
    , HistorySummary (..)

    -- * MetricStat
    , MetricStat (..)
    , mkMetricStat
    , msMetric
    , msPeriod
    , msStat
    , msUnit

    -- * Statistic
    , Statistic (..)

    -- * ExtendedStatistic
    , ExtendedStatistic (..)

    -- * DashboardArn
    , DashboardArn (..)

    -- * StatusCode
    , StatusCode (..)

    -- * ThresholdMetricId
    , ThresholdMetricId (..)

    -- * RuleName
    , RuleName (..)

    -- * RuleDefinition
    , RuleDefinition (..)

    -- * RuleState
    , RuleState (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Name
    , Name (..)

    -- * Schema
    , Schema (..)

    -- * Label
    , Label (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * AggregationStatistic
    , AggregationStatistic (..)

    -- * Code
    , Code (..)

    -- * Expression
    , Expression (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CloudWatch.Types.AlarmName
  
import Network.AWS.CloudWatch.Types.DashboardValidationMessage
  
  
import Network.AWS.CloudWatch.Types.AlarmNamePrefix
  
import Network.AWS.CloudWatch.Types.StatisticSet
  
import Network.AWS.CloudWatch.Types.AnomalyDetectorMetricTimezone
  
import Network.AWS.CloudWatch.Types.MetricAlarm
  
import Network.AWS.CloudWatch.Types.HistoryItemType
  
import Network.AWS.CloudWatch.Types.InsightRuleState
  
  
import Network.AWS.CloudWatch.Types.HistoryData
  
import Network.AWS.CloudWatch.Types.Tag
  
import Network.AWS.CloudWatch.Types.FailureResource
  
import Network.AWS.CloudWatch.Types.TreatMissingData
  
import Network.AWS.CloudWatch.Types.InsightRuleOrderBy
  
import Network.AWS.CloudWatch.Types.ResourceName
  
import Network.AWS.CloudWatch.Types.InsightRuleDefinition
  
import Network.AWS.CloudWatch.Types.MetricDatum
  
import Network.AWS.CloudWatch.Types.AnomalyDetectorMetricStat
  
import Network.AWS.CloudWatch.Types.FailureCode
  
import Network.AWS.CloudWatch.Types.DataPath
  
import Network.AWS.CloudWatch.Types.AlarmDescription
  
  
import Network.AWS.CloudWatch.Types.StandardUnit
  
import Network.AWS.CloudWatch.Types.AlarmRule
  
import Network.AWS.CloudWatch.Types.InsightRule
  
import Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
  
import Network.AWS.CloudWatch.Types.MessageDataValue
  
import Network.AWS.CloudWatch.Types.DashboardEntry
  
import Network.AWS.CloudWatch.Types.Dimension
  
  
  
import Network.AWS.CloudWatch.Types.MetricName
  
import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
  
import Network.AWS.CloudWatch.Types.Namespace
  
import Network.AWS.CloudWatch.Types.DashboardName
  
import Network.AWS.CloudWatch.Types.DashboardNamePrefix
  
import Network.AWS.CloudWatch.Types.AnomalyDetector
  
import Network.AWS.CloudWatch.Types.DimensionValue
  
import Network.AWS.CloudWatch.Types.InsightRuleMetricName
  
import Network.AWS.CloudWatch.Types.AlarmType
  
import Network.AWS.CloudWatch.Types.ActionPrefix
  
import Network.AWS.CloudWatch.Types.CompositeAlarm
  
import Network.AWS.CloudWatch.Types.FailureDescription
  
import Network.AWS.CloudWatch.Types.ComparisonOperator
  
import Network.AWS.CloudWatch.Types.MetricId
  
import Network.AWS.CloudWatch.Types.EvaluateLowSampleCountPercentile
  
import Network.AWS.CloudWatch.Types.InsightRuleContributorKey
  
  
import Network.AWS.CloudWatch.Types.NextToken
  
import Network.AWS.CloudWatch.Types.ScanBy
  
import Network.AWS.CloudWatch.Types.AlarmHistoryItem
  
import Network.AWS.CloudWatch.Types.Metric
  
import Network.AWS.CloudWatch.Types.OutputFormat
  
import Network.AWS.CloudWatch.Types.StateValue
  
import Network.AWS.CloudWatch.Types.DashboardBody
  
import Network.AWS.CloudWatch.Types.Datapoint
  
import Network.AWS.CloudWatch.Types.Range
  
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
  
import Network.AWS.CloudWatch.Types.InsightRuleName
  
import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
  
import Network.AWS.CloudWatch.Types.ExceptionType
  
import Network.AWS.CloudWatch.Types.DimensionName
  
import Network.AWS.CloudWatch.Types.MetricLabel
  
import Network.AWS.CloudWatch.Types.RecentlyActive
  
  
import Network.AWS.CloudWatch.Types.Stat
  
import Network.AWS.CloudWatch.Types.InsightRuleContributorKeyLabel
  
import Network.AWS.CloudWatch.Types.MetricDataResult
  
import Network.AWS.CloudWatch.Types.TagKey
  
import Network.AWS.CloudWatch.Types.MetricWidget
  
import Network.AWS.CloudWatch.Types.DimensionFilter
  
  
import Network.AWS.CloudWatch.Types.StateReason
  
import Network.AWS.CloudWatch.Types.Message
  
import Network.AWS.CloudWatch.Types.AmazonResourceName
  
import Network.AWS.CloudWatch.Types.InsightRuleContributor
  
import Network.AWS.CloudWatch.Types.StateReasonData
  
  
import Network.AWS.CloudWatch.Types.MessageData
  
import Network.AWS.CloudWatch.Types.PartialFailure
  
import Network.AWS.CloudWatch.Types.MetricDataQuery
  
import Network.AWS.CloudWatch.Types.AlarmArn
  
import Network.AWS.CloudWatch.Types.HistorySummary
  
import Network.AWS.CloudWatch.Types.MetricStat
  
import Network.AWS.CloudWatch.Types.Statistic
  
  
  
import Network.AWS.CloudWatch.Types.ExtendedStatistic
  
import Network.AWS.CloudWatch.Types.DashboardArn
  
import Network.AWS.CloudWatch.Types.StatusCode
  
  
  
import Network.AWS.CloudWatch.Types.ThresholdMetricId
  
import Network.AWS.CloudWatch.Types.RuleName
  
import Network.AWS.CloudWatch.Types.RuleDefinition
  
import Network.AWS.CloudWatch.Types.RuleState
  
import Network.AWS.CloudWatch.Types.Key
  
import Network.AWS.CloudWatch.Types.Value
  
import Network.AWS.CloudWatch.Types.Name
  
import Network.AWS.CloudWatch.Types.Schema
  
import Network.AWS.CloudWatch.Types.Label
  
import Network.AWS.CloudWatch.Types.ResourceARN
  
import Network.AWS.CloudWatch.Types.AggregationStatistic
  
import Network.AWS.CloudWatch.Types.Code
  
import Network.AWS.CloudWatch.Types.Expression
  

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CloudWatch",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "monitoring",
                 Core._svcVersion = "2010-08-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "CloudWatch",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault
  = Core._MatchServiceError mkServiceConfig "LimitExceeded" Core..
      Core.hasStatues 400
{-# INLINEABLE _LimitExceededFault #-}
{-# DEPRECATED _LimitExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified dashboard does not exist.
_DashboardNotFoundError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DashboardNotFoundError
  = Core._MatchServiceError mkServiceConfig "ResourceNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _DashboardNotFoundError #-}
{-# DEPRECATED _DashboardNotFoundError "Use generic-lens or generic-optics instead"  #-}

-- | The next token specified is invalid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken
  = Core._MatchServiceError mkServiceConfig "InvalidNextToken" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidNextToken #-}
{-# DEPRECATED _InvalidNextToken "Use generic-lens or generic-optics instead"  #-}

-- | Request processing has failed due to some unknown error, exception, or failure.
_InternalServiceFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceFault
  = Core._MatchServiceError mkServiceConfig "InternalServiceError"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServiceFault #-}
{-# DEPRECATED _InternalServiceFault "Use generic-lens or generic-optics instead"  #-}

-- | Some part of the dashboard data is invalid.
_DashboardInvalidInputError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DashboardInvalidInputError
  = Core._MatchServiceError mkServiceConfig "InvalidParameterInput"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DashboardInvalidInputError #-}
{-# DEPRECATED _DashboardInvalidInputError "Use generic-lens or generic-optics instead"  #-}

-- | The value of an input parameter is bad or out-of-range.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException
  = Core._MatchServiceError mkServiceConfig "InvalidParameterValue"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterValueException #-}
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead"  #-}

-- | More than one process tried to modify a resource at the same time.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFormatFault
  = Core._MatchServiceError mkServiceConfig "InvalidFormat" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidFormatFault #-}
{-# DEPRECATED _InvalidFormatFault "Use generic-lens or generic-optics instead"  #-}

-- | An input parameter that is required is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException
  = Core._MatchServiceError mkServiceConfig "MissingParameter" Core..
      Core.hasStatues 400
{-# INLINEABLE _MissingRequiredParameterException #-}
{-# DEPRECATED _MissingRequiredParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The named resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Parameters were used together that cannot be used together.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterCombination"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterCombinationException #-}
{-# DEPRECATED _InvalidParameterCombinationException "Use generic-lens or generic-optics instead"  #-}

-- | The operation exceeded one or more limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The named resource does not exist.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound
  = Core._MatchServiceError mkServiceConfig "ResourceNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _ResourceNotFound #-}
{-# DEPRECATED _ResourceNotFound "Use generic-lens or generic-optics instead"  #-}
