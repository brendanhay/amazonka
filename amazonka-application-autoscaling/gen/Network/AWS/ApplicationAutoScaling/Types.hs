{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ObjectNotFoundException,
    _InternalServiceException,
    _InvalidNextTokenException,
    _FailedResourceAccessException,
    _ValidationException,
    _LimitExceededException,
    _ConcurrentUpdateException,

    -- * AdjustmentType
    AdjustmentType (..),

    -- * MetricAggregationType
    MetricAggregationType (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricType
    MetricType (..),

    -- * PolicyType
    PolicyType (..),

    -- * ScalableDimension
    ScalableDimension (..),

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * ServiceNamespace
    ServiceNamespace (..),

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_alarmName,
    alarm_alarmARN,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    newCustomizedMetricSpecification,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_name,
    metricDimension_value,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    newPredefinedMetricSpecification,
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- * ScalableTarget
    ScalableTarget (..),
    newScalableTarget,
    scalableTarget_suspendedState,
    scalableTarget_serviceNamespace,
    scalableTarget_resourceId,
    scalableTarget_scalableDimension,
    scalableTarget_minCapacity,
    scalableTarget_maxCapacity,
    scalableTarget_roleARN,
    scalableTarget_creationTime,

    -- * ScalableTargetAction
    ScalableTargetAction (..),
    newScalableTargetAction,
    scalableTargetAction_maxCapacity,
    scalableTargetAction_minCapacity,

    -- * ScalingActivity
    ScalingActivity (..),
    newScalingActivity,
    scalingActivity_statusMessage,
    scalingActivity_endTime,
    scalingActivity_details,
    scalingActivity_activityId,
    scalingActivity_serviceNamespace,
    scalingActivity_resourceId,
    scalingActivity_scalableDimension,
    scalingActivity_description,
    scalingActivity_cause,
    scalingActivity_startTime,
    scalingActivity_statusCode,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_targetTrackingScalingPolicyConfiguration,
    scalingPolicy_stepScalingPolicyConfiguration,
    scalingPolicy_alarms,
    scalingPolicy_policyARN,
    scalingPolicy_policyName,
    scalingPolicy_serviceNamespace,
    scalingPolicy_resourceId,
    scalingPolicy_scalableDimension,
    scalingPolicy_policyType,
    scalingPolicy_creationTime,

    -- * ScheduledAction
    ScheduledAction (..),
    newScheduledAction,
    scheduledAction_startTime,
    scheduledAction_endTime,
    scheduledAction_scalableDimension,
    scheduledAction_timezone,
    scheduledAction_scalableTargetAction,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionARN,
    scheduledAction_serviceNamespace,
    scheduledAction_schedule,
    scheduledAction_resourceId,
    scheduledAction_creationTime,

    -- * StepAdjustment
    StepAdjustment (..),
    newStepAdjustment,
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_scalingAdjustment,

    -- * StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    newStepScalingPolicyConfiguration,
    stepScalingPolicyConfiguration_stepAdjustments,
    stepScalingPolicyConfiguration_metricAggregationType,
    stepScalingPolicyConfiguration_cooldown,
    stepScalingPolicyConfiguration_adjustmentType,
    stepScalingPolicyConfiguration_minAdjustmentMagnitude,

    -- * SuspendedState
    SuspendedState (..),
    newSuspendedState,
    suspendedState_scheduledScalingSuspended,
    suspendedState_dynamicScalingInSuspended,
    suspendedState_dynamicScalingOutSuspended,

    -- * TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    newTargetTrackingScalingPolicyConfiguration,
    targetTrackingScalingPolicyConfiguration_disableScaleIn,
    targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_scaleOutCooldown,
    targetTrackingScalingPolicyConfiguration_customizedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_scaleInCooldown,
    targetTrackingScalingPolicyConfiguration_targetValue,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
import Network.AWS.ApplicationAutoScaling.Types.Alarm
import Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
import Network.AWS.ApplicationAutoScaling.Types.MetricDimension
import Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
import Network.AWS.ApplicationAutoScaling.Types.MetricType
import Network.AWS.ApplicationAutoScaling.Types.PolicyType
import Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ScalableTarget
import Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivity
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
import Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.SuspendedState
import Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-06@ of the Amazon Application Auto Scaling SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ApplicationAutoScaling",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "application-autoscaling",
      Core._serviceSigningName = "application-autoscaling",
      Core._serviceVersion = "2016-02-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ApplicationAutoScaling",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified object could not be found. For any operation that depends
-- on the existence of a scalable target, this exception is thrown if the
-- scalable target with the specified service namespace, resource ID, and
-- scalable dimension does not exist. For any operation that deletes or
-- deregisters a resource, this exception is thrown if the resource cannot
-- be found.
_ObjectNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectNotFoundException =
  Core._MatchServiceError
    defaultService
    "ObjectNotFoundException"

-- | The service encountered an internal error.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The next token supplied was invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | Failed access to resources caused an exception. This exception is thrown
-- when Application Auto Scaling is unable to retrieve the alarms
-- associated with a scaling policy due to a client error, for example, if
-- the role ARN specified for a scalable target does not have permission to
-- call the CloudWatch
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html DescribeAlarms>
-- on your behalf.
_FailedResourceAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FailedResourceAccessException =
  Core._MatchServiceError
    defaultService
    "FailedResourceAccessException"

-- | An exception was thrown for a validation issue. Review the available
-- parameters for the API request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | A per-account resource limit is exceeded. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-limits.html Application Auto Scaling service quotas>.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Concurrent updates caused an exception, for example, if you request an
-- update to an Application Auto Scaling resource that already has a
-- pending update.
_ConcurrentUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdateException"
