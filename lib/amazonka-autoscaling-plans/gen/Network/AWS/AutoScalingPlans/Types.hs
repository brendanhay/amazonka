{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types
    (
    -- * Service Configuration
      autoScalingPlans

    -- * Errors
    , _ValidationException
    , _InvalidNextTokenException
    , _ConcurrentUpdateException
    , _InternalServiceException
    , _ObjectNotFoundException
    , _LimitExceededException

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * PolicyType
    , PolicyType (..)

    -- * ScalableDimension
    , ScalableDimension (..)

    -- * ScalingMetricType
    , ScalingMetricType (..)

    -- * ScalingPlanStatusCode
    , ScalingPlanStatusCode (..)

    -- * ScalingStatusCode
    , ScalingStatusCode (..)

    -- * ServiceNamespace
    , ServiceNamespace (..)

    -- * ApplicationSource
    , ApplicationSource
    , applicationSource
    , asTagFilters
    , asCloudFormationStackARN

    -- * CustomizedScalingMetricSpecification
    , CustomizedScalingMetricSpecification
    , customizedScalingMetricSpecification
    , csmsDimensions
    , csmsUnit
    , csmsMetricName
    , csmsNamespace
    , csmsStatistic

    -- * MetricDimension
    , MetricDimension
    , metricDimension
    , mdName
    , mdValue

    -- * PredefinedScalingMetricSpecification
    , PredefinedScalingMetricSpecification
    , predefinedScalingMetricSpecification
    , psmsResourceLabel
    , psmsPredefinedScalingMetricType

    -- * ScalingInstruction
    , ScalingInstruction
    , scalingInstruction
    , siServiceNamespace
    , siResourceId
    , siScalableDimension
    , siMinCapacity
    , siMaxCapacity
    , siTargetTrackingConfigurations

    -- * ScalingPlan
    , ScalingPlan
    , scalingPlan
    , spCreationTime
    , spStatusStartTime
    , spStatusMessage
    , spScalingPlanName
    , spScalingPlanVersion
    , spApplicationSource
    , spScalingInstructions
    , spStatusCode

    -- * ScalingPlanResource
    , ScalingPlanResource
    , scalingPlanResource
    , sprScalingStatusMessage
    , sprScalingPolicies
    , sprScalingPlanName
    , sprScalingPlanVersion
    , sprServiceNamespace
    , sprResourceId
    , sprScalableDimension
    , sprScalingStatusCode

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spTargetTrackingConfiguration
    , spPolicyName
    , spPolicyType

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfValues
    , tfKey

    -- * TargetTrackingConfiguration
    , TargetTrackingConfiguration
    , targetTrackingConfiguration
    , ttcEstimatedInstanceWarmup
    , ttcPredefinedScalingMetricSpecification
    , ttcScaleInCooldown
    , ttcDisableScaleIn
    , ttcCustomizedScalingMetricSpecification
    , ttcScaleOutCooldown
    , ttcTargetValue
    ) where

import Network.AWS.AutoScalingPlans.Types.Product
import Network.AWS.AutoScalingPlans.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
autoScalingPlans :: Service
autoScalingPlans =
  Service
    { _svcAbbrev = "AutoScalingPlans"
    , _svcSigner = v4
    , _svcPrefix = "autoscaling"
    , _svcVersion = "2018-01-06"
    , _svcEndpoint = defaultEndpoint autoScalingPlans
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AutoScalingPlans"
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


-- | An exception was thrown for a validation issue. Review the parameters provided.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _MatchServiceError autoScalingPlans "ValidationException"


-- | The token provided is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError autoScalingPlans "InvalidNextTokenException"


-- | Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.
--
--
_ConcurrentUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentUpdateException =
  _MatchServiceError autoScalingPlans "ConcurrentUpdateException"


-- | The service encountered an internal error.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError autoScalingPlans "InternalServiceException"


-- | The specified object could not be found.
--
--
_ObjectNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotFoundException =
  _MatchServiceError autoScalingPlans "ObjectNotFoundException"


-- | Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError autoScalingPlans "LimitExceededException"

