{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types
    (
    -- * Service Configuration
      cloudWatch

    -- * Errors

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * ScanBy
    , ScanBy (..)

    -- * StandardUnit
    , StandardUnit (..)

    -- * StateValue
    , StateValue (..)

    -- * Statistic
    , Statistic (..)

    -- * StatusCode
    , StatusCode (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiHistorySummary
    , ahiTimestamp

    -- * AnomalyDetector
    , AnomalyDetector
    , anomalyDetector
    , adMetricName
    , adNamespace
    , adStat
    , adConfiguration
    , adDimensions

    -- * AnomalyDetectorConfiguration
    , AnomalyDetectorConfiguration
    , anomalyDetectorConfiguration
    , adcMetricTimezone
    , adcExcludedTimeRanges

    -- * DashboardEntry
    , DashboardEntry
    , dashboardEntry
    , deSize
    , deDashboardName
    , deLastModified
    , deDashboardARN

    -- * DashboardValidationMessage
    , DashboardValidationMessage
    , dashboardValidationMessage
    , dvmDataPath
    , dvmMessage

    -- * Datapoint
    , Datapoint
    , datapoint
    , dSampleCount
    , dMaximum
    , dAverage
    , dMinimum
    , dExtendedStatistics
    , dSum
    , dUnit
    , dTimestamp

    -- * Dimension
    , Dimension
    , dimension
    , dName
    , dValue

    -- * DimensionFilter
    , DimensionFilter
    , dimensionFilter
    , dfValue
    , dfName

    -- * MessageData
    , MessageData
    , messageData
    , mValue
    , mCode

    -- * Metric
    , Metric
    , metric
    , mMetricName
    , mNamespace
    , mDimensions

    -- * MetricAlarm
    , MetricAlarm
    , metricAlarm
    , maAlarmName
    , maStateUpdatedTimestamp
    , maMetrics
    , maTreatMissingData
    , maPeriod
    , maAlarmDescription
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maThresholdMetricId
    , maComparisonOperator
    , maOKActions
    , maEvaluateLowSampleCountPercentile
    , maStateValue
    , maDatapointsToAlarm
    , maThreshold
    , maAlarmConfigurationUpdatedTimestamp
    , maActionsEnabled
    , maInsufficientDataActions
    , maStateReason
    , maStateReasonData
    , maDimensions
    , maAlarmARN
    , maAlarmActions
    , maUnit
    , maStatistic
    , maExtendedStatistic

    -- * MetricDataQuery
    , MetricDataQuery
    , metricDataQuery
    , mdqReturnData
    , mdqExpression
    , mdqLabel
    , mdqMetricStat
    , mdqId

    -- * MetricDataResult
    , MetricDataResult
    , metricDataResult
    , mdrValues
    , mdrId
    , mdrTimestamps
    , mdrMessages
    , mdrLabel
    , mdrStatusCode

    -- * MetricDatum
    , MetricDatum
    , metricDatum
    , mdValues
    , mdCounts
    , mdValue
    , mdStorageResolution
    , mdDimensions
    , mdUnit
    , mdTimestamp
    , mdStatisticValues
    , mdMetricName

    -- * MetricStat
    , MetricStat
    , metricStat
    , msUnit
    , msMetric
    , msPeriod
    , msStat

    -- * Range
    , Range
    , range
    , rStartTime
    , rEndTime

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.CloudWatch.Types.Product
import Network.AWS.CloudWatch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
cloudWatch :: Service
cloudWatch =
  Service
    { _svcAbbrev = "CloudWatch"
    , _svcSigner = v4
    , _svcPrefix = "monitoring"
    , _svcVersion = "2010-08-01"
    , _svcEndpoint = defaultEndpoint cloudWatch
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "CloudWatch"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

