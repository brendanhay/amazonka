{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types
    (
    -- * Service Configuration
      applicationAutoScaling

    -- * Errors
    , _ValidationException
    , _FailedResourceAccessException
    , _InvalidNextTokenException
    , _ConcurrentUpdateException
    , _InternalServiceException
    , _ObjectNotFoundException
    , _LimitExceededException

    -- * AdjustmentType
    , AdjustmentType (..)

    -- * MetricAggregationType
    , MetricAggregationType (..)

    -- * PolicyType
    , PolicyType (..)

    -- * ScalableDimension
    , ScalableDimension (..)

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- * ServiceNamespace
    , ServiceNamespace (..)

    -- * Alarm
    , Alarm
    , alarm
    , aAlarmName
    , aAlarmARN

    -- * ScalableTarget
    , ScalableTarget
    , scalableTarget
    , stServiceNamespace
    , stResourceId
    , stScalableDimension
    , stMinCapacity
    , stMaxCapacity
    , stRoleARN
    , stCreationTime

    -- * ScalingActivity
    , ScalingActivity
    , scalingActivity
    , saStatusMessage
    , saEndTime
    , saDetails
    , saActivityId
    , saServiceNamespace
    , saResourceId
    , saScalableDimension
    , saDescription
    , saCause
    , saStartTime
    , saStatusCode

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spStepScalingPolicyConfiguration
    , spAlarms
    , spPolicyARN
    , spPolicyName
    , spServiceNamespace
    , spResourceId
    , spScalableDimension
    , spPolicyType
    , spCreationTime

    -- * StepAdjustment
    , StepAdjustment
    , stepAdjustment
    , saMetricIntervalLowerBound
    , saMetricIntervalUpperBound
    , saScalingAdjustment

    -- * StepScalingPolicyConfiguration
    , StepScalingPolicyConfiguration
    , stepScalingPolicyConfiguration
    , sspcStepAdjustments
    , sspcAdjustmentType
    , sspcCooldown
    , sspcMetricAggregationType
    , sspcMinAdjustmentMagnitude
    ) where

import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.ApplicationAutoScaling.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2016-02-06' of the Amazon Application Auto Scaling SDK configuration.
applicationAutoScaling :: Service
applicationAutoScaling =
    Service
    { _svcAbbrev = "ApplicationAutoScaling"
    , _svcSigner = v4
    , _svcPrefix = "autoscaling"
    , _svcVersion = "2016-02-06"
    , _svcEndpoint = defaultEndpoint applicationAutoScaling
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ApplicationAutoScaling"
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

-- | An exception was thrown for a validation issue. Review the available parameters for the API request.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | Failed access to resources caused an exception. This exception currently only applies to < DescribeScalingPolicies>. It is thrown when Application Auto Scaling is unable to retrieve the alarms associated with a scaling policy due to a client error, for example, if the role ARN specified for a scalable target does not have the proper permissions to call the CloudWatch <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html DescribeAlarms> API operation on behalf of your account.
_FailedResourceAccessException :: AsError a => Getting (First ServiceError) a ServiceError
_FailedResourceAccessException =
    _ServiceError . hasCode "FailedResourceAccessException"

-- | The next token supplied was invalid.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | Concurrent updates caused an exception, for example, if you request an update to an Application Auto Scaling resource that already has a pending update.
_ConcurrentUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentUpdateException =
    _ServiceError . hasCode "ConcurrentUpdateException"

-- | The service encountered an internal error.
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException = _ServiceError . hasCode "InternalServiceException"

-- | The specified object could not be found. For any 'Put' or 'Register' API operation, which depends on the existence of a scalable target, this exception is thrown if the scalable target with the specified service namespace, resource ID, and scalable dimension does not exist. For any 'Delete' or 'Deregister' API operation, this exception is thrown if the resource that is to be deleted or deregistered cannot be found.
_ObjectNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotFoundException = _ServiceError . hasCode "ObjectNotFoundException"

-- | Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded. Application Auto Scaling has a limit of 40 scalable targets per account for Amazon ECS services, 50 scaling policies per scalable target, and 20 step adjustments per step scaling policy.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
