{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationAutoScaling.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentUpdateException,
    _FailedResourceAccessException,
    _InternalServiceException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _ObjectNotFoundException,
    _ValidationException,

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
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_name,
    metricDimension_value,

    -- * NotScaledReason
    NotScaledReason (..),
    newNotScaledReason,
    notScaledReason_currentCapacity,
    notScaledReason_maxCapacity,
    notScaledReason_minCapacity,
    notScaledReason_code,

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
    scalingActivity_details,
    scalingActivity_endTime,
    scalingActivity_notScaledReasons,
    scalingActivity_statusMessage,
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
    scalingPolicy_alarms,
    scalingPolicy_stepScalingPolicyConfiguration,
    scalingPolicy_targetTrackingScalingPolicyConfiguration,
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
    scheduledAction_endTime,
    scheduledAction_scalableDimension,
    scheduledAction_scalableTargetAction,
    scheduledAction_startTime,
    scheduledAction_timezone,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionARN,
    scheduledAction_serviceNamespace,
    scheduledAction_schedule,
    scheduledAction_resourceId,
    scheduledAction_creationTime,

    -- * StepAdjustment
    StepAdjustment (..),
    newStepAdjustment,
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_scalingAdjustment,

    -- * StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    newStepScalingPolicyConfiguration,
    stepScalingPolicyConfiguration_adjustmentType,
    stepScalingPolicyConfiguration_cooldown,
    stepScalingPolicyConfiguration_metricAggregationType,
    stepScalingPolicyConfiguration_minAdjustmentMagnitude,
    stepScalingPolicyConfiguration_stepAdjustments,

    -- * SuspendedState
    SuspendedState (..),
    newSuspendedState,
    suspendedState_dynamicScalingInSuspended,
    suspendedState_dynamicScalingOutSuspended,
    suspendedState_scheduledScalingSuspended,

    -- * TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    newTargetTrackingScalingPolicyConfiguration,
    targetTrackingScalingPolicyConfiguration_customizedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_disableScaleIn,
    targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_scaleInCooldown,
    targetTrackingScalingPolicyConfiguration_scaleOutCooldown,
    targetTrackingScalingPolicyConfiguration_targetValue,
  )
where

import Amazonka.ApplicationAutoScaling.Types.AdjustmentType
import Amazonka.ApplicationAutoScaling.Types.Alarm
import Amazonka.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Amazonka.ApplicationAutoScaling.Types.MetricAggregationType
import Amazonka.ApplicationAutoScaling.Types.MetricDimension
import Amazonka.ApplicationAutoScaling.Types.MetricStatistic
import Amazonka.ApplicationAutoScaling.Types.MetricType
import Amazonka.ApplicationAutoScaling.Types.NotScaledReason
import Amazonka.ApplicationAutoScaling.Types.PolicyType
import Amazonka.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Amazonka.ApplicationAutoScaling.Types.ScalableDimension
import Amazonka.ApplicationAutoScaling.Types.ScalableTarget
import Amazonka.ApplicationAutoScaling.Types.ScalableTargetAction
import Amazonka.ApplicationAutoScaling.Types.ScalingActivity
import Amazonka.ApplicationAutoScaling.Types.ScalingActivityStatusCode
import Amazonka.ApplicationAutoScaling.Types.ScalingPolicy
import Amazonka.ApplicationAutoScaling.Types.ScheduledAction
import Amazonka.ApplicationAutoScaling.Types.ServiceNamespace
import Amazonka.ApplicationAutoScaling.Types.StepAdjustment
import Amazonka.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Amazonka.ApplicationAutoScaling.Types.SuspendedState
import Amazonka.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-02-06@ of the Amazon Application Auto Scaling SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "ApplicationAutoScaling",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "application-autoscaling",
      Core.signingName = "application-autoscaling",
      Core.version = "2016-02-06",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ApplicationAutoScaling",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Concurrent updates caused an exception, for example, if you request an
-- update to an Application Auto Scaling resource that already has a
-- pending update.
_ConcurrentUpdateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConcurrentUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdateException"

-- | Failed access to resources caused an exception. This exception is thrown
-- when Application Auto Scaling is unable to retrieve the alarms
-- associated with a scaling policy due to a client error, for example, if
-- the role ARN specified for a scalable target does not have permission to
-- call the CloudWatch
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html DescribeAlarms>
-- on your behalf.
_FailedResourceAccessException :: Core.AsError a => Lens.Fold a Core.ServiceError
_FailedResourceAccessException =
  Core._MatchServiceError
    defaultService
    "FailedResourceAccessException"

-- | The service encountered an internal error.
_InternalServiceException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The next token supplied was invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | A per-account resource limit is exceeded. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-limits.html Application Auto Scaling service quotas>.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified object could not be found. For any operation that depends
-- on the existence of a scalable target, this exception is thrown if the
-- scalable target with the specified service namespace, resource ID, and
-- scalable dimension does not exist. For any operation that deletes or
-- deregisters a resource, this exception is thrown if the resource cannot
-- be found.
_ObjectNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ObjectNotFoundException =
  Core._MatchServiceError
    defaultService
    "ObjectNotFoundException"

-- | An exception was thrown for a validation issue. Review the available
-- parameters for the API request.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
