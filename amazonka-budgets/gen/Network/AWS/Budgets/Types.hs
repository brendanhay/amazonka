{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types
    (
    -- * Service Configuration
      budgets

    -- * Errors
    , _InvalidParameterException
    , _InternalErrorException
    , _ExpiredNextTokenException
    , _NotFoundException
    , _InvalidNextTokenException
    , _DuplicateRecordException
    , _CreationLimitExceededException

    -- * BudgetType
    , BudgetType (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * NotificationType
    , NotificationType (..)

    -- * SubscriptionType
    , SubscriptionType (..)

    -- * ThresholdType
    , ThresholdType (..)

    -- * TimeUnit
    , TimeUnit (..)

    -- * Budget
    , Budget
    , budget
    , bCalculatedSpend
    , bBudgetLimit
    , bTimePeriod
    , bCostTypes
    , bCostFilters
    , bBudgetName
    , bTimeUnit
    , bBudgetType

    -- * CalculatedSpend
    , CalculatedSpend
    , calculatedSpend
    , csForecastedSpend
    , csActualSpend

    -- * CostTypes
    , CostTypes
    , costTypes
    , ctUseAmortized
    , ctIncludeRecurring
    , ctUseBlended
    , ctIncludeSupport
    , ctIncludeDiscount
    , ctIncludeSubscription
    , ctIncludeRefund
    , ctIncludeUpfront
    , ctIncludeOtherSubscription
    , ctIncludeTax
    , ctIncludeCredit

    -- * Notification
    , Notification
    , notification
    , nThresholdType
    , nNotificationType
    , nComparisonOperator
    , nThreshold

    -- * NotificationWithSubscribers
    , NotificationWithSubscribers
    , notificationWithSubscribers
    , nwsNotification
    , nwsSubscribers

    -- * Spend
    , Spend
    , spend
    , sAmount
    , sUnit

    -- * Subscriber
    , Subscriber
    , subscriber
    , sSubscriptionType
    , sAddress

    -- * TimePeriod
    , TimePeriod
    , timePeriod
    , tpStart
    , tpEnd
    ) where

import Network.AWS.Budgets.Types.Product
import Network.AWS.Budgets.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-10-20@ of the Amazon Budgets SDK configuration.
budgets :: Service
budgets =
  Service
    { _svcAbbrev = "Budgets"
    , _svcSigner = v4
    , _svcPrefix = "budgets"
    , _svcVersion = "2016-10-20"
    , _svcEndpoint = defaultEndpoint budgets
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Budgets"
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


-- | An error on the client occurred. Typically, the cause is an invalid input value.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError budgets "InvalidParameterException"


-- | An error on the server occurred during the processing of your request. Try again later.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _MatchServiceError budgets "InternalErrorException"


-- | The pagination token expired.
--
--
_ExpiredNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredNextTokenException =
  _MatchServiceError budgets "ExpiredNextTokenException"


-- | We can’t locate the resource that you specified.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError budgets "NotFoundException"


-- | The pagination token is invalid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError budgets "InvalidNextTokenException"


-- | The budget name already exists. Budget names must be unique within an account.
--
--
_DuplicateRecordException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRecordException =
  _MatchServiceError budgets "DuplicateRecordException"


-- | You've exceeded the notification or subscriber limit.
--
--
_CreationLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CreationLimitExceededException =
  _MatchServiceError budgets "CreationLimitExceededException"

