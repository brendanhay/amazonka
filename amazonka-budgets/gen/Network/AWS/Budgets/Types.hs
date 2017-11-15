{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types
-- Copyright   : (c) 2013-2017 Brendan Hay
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
    , bCostFilters
    , bBudgetName
    , bBudgetLimit
    , bCostTypes
    , bTimeUnit
    , bTimePeriod
    , bBudgetType

    -- * CalculatedSpend
    , CalculatedSpend
    , calculatedSpend
    , csForecastedSpend
    , csActualSpend

    -- * CostTypes
    , CostTypes
    , costTypes
    , ctIncludeTax
    , ctIncludeSubscription
    , ctUseBlended

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
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | This exception is thrown if any request is given an invalid parameter. E.g., if a required Date field is null.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError budgets "InvalidParameterException"


-- | This exception is thrown on an unknown internal failure.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _MatchServiceError budgets "InternalErrorException"


-- | This exception is thrown if the paging token is expired - past its TTL
_ExpiredNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredNextTokenException =
  _MatchServiceError budgets "ExpiredNextTokenException"


-- | This exception is thrown if a requested entity is not found. E.g., if a budget id doesn't exist for an account ID.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError budgets "NotFoundException"


-- | This exception is thrown if paging token signature didn't match the token, or the paging token isn't for this request
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError budgets "InvalidNextTokenException"


-- | The exception is thrown when customer tries to create a record (e.g. budget) that already exists.
_DuplicateRecordException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRecordException =
  _MatchServiceError budgets "DuplicateRecordException"


-- | The exception is thrown when customer tries to create a record (e.g. budget), but the number this record already exceeds the limitation.
_CreationLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CreationLimitExceededException =
  _MatchServiceError budgets "CreationLimitExceededException"

