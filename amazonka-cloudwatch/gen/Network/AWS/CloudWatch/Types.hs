{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types
    (
    -- * Service
      CloudWatch

    -- * Errors
    , _LimitExceededFault
    , _InvalidNextToken
    , _InternalServiceFault
    , _InvalidParameterValueException
    , _InvalidFormatFault
    , _MissingRequiredParameterException
    , _InvalidParameterCombinationException
    , _ResourceNotFound

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * StandardUnit
    , StandardUnit (..)

    -- * StateValue
    , StateValue (..)

    -- * Statistic
    , Statistic (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiTimestamp
    , ahiHistorySummary

    -- * Datapoint
    , Datapoint
    , datapoint
    , dSampleCount
    , dMaximum
    , dAverage
    , dMinimum
    , dSum
    , dTimestamp
    , dUnit

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
    , maAlarmDescription
    , maPeriod
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maOKActions
    , maComparisonOperator
    , maStateValue
    , maThreshold
    , maActionsEnabled
    , maAlarmConfigurationUpdatedTimestamp
    , maInsufficientDataActions
    , maDimensions
    , maStateReasonData
    , maStateReason
    , maAlarmARN
    , maAlarmActions
    , maStatistic
    , maUnit

    -- * MetricDatum
    , MetricDatum
    , metricDatum
    , mdValue
    , mdDimensions
    , mdTimestamp
    , mdStatisticValues
    , mdUnit
    , mdMetricName

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum
    ) where

import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.CloudWatch.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-08-01@ of the Amazon CloudWatch SDK.
data CloudWatch

instance AWSService CloudWatch where
    type Sg CloudWatch = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudWatch"
            , _svcPrefix = "monitoring"
            , _svcVersion = "2010-08-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault = _ServiceError . hasStatus 400 . hasCode "LimitExceeded"

-- | The next token specified is invalid.
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | Indicates that the request processing has failed due to some unknown
-- error, exception, or failure.
_InternalServiceFault :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceFault =
    _ServiceError . hasStatus 500 . hasCode "InternalServiceError"

-- | Bad or out-of-range value was supplied for the input parameter.
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValue"

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFormatFault = _ServiceError . hasStatus 400 . hasCode "InvalidFormat"

-- | An input parameter that is mandatory for processing the request is not
-- supplied.
_MissingRequiredParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameterException =
    _ServiceError . hasStatus 400 . hasCode "MissingParameter"

-- | Parameters that must not be used together were used together.
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterCombination"

-- | The named resource does not exist.
_ResourceNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFound = _ServiceError . hasStatus 404 . hasCode "ResourceNotFound"
