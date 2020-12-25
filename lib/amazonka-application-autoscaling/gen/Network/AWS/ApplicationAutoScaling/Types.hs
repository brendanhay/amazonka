-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ValidationException,
    _FailedResourceAccessException,
    _InvalidNextTokenException,
    _ConcurrentUpdateException,
    _InternalServiceException,
    _ObjectNotFoundException,
    _LimitExceededException,

    -- * XmlString
    XmlString (..),

    -- * MetricType
    MetricType (..),

    -- * ScalableDimension
    ScalableDimension (..),

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsPredefinedMetricType,
    pmsResourceLabel,

    -- * ResourceId
    ResourceId (..),

    -- * PolicyName
    PolicyName (..),

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spPolicyARN,
    spPolicyName,
    spServiceNamespace,
    spResourceId,
    spScalableDimension,
    spPolicyType,
    spCreationTime,
    spAlarms,
    spStepScalingPolicyConfiguration,
    spTargetTrackingScalingPolicyConfiguration,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,
    cmsDimensions,
    cmsUnit,

    -- * ScheduledActionName
    ScheduledActionName (..),

    -- * PolicyType
    PolicyType (..),

    -- * ScalingActivity
    ScalingActivity (..),
    mkScalingActivity,
    sActivityId,
    sServiceNamespace,
    sResourceId,
    sScalableDimension,
    sDescription,
    sCause,
    sStartTime,
    sStatusCode,
    sDetails,
    sEndTime,
    sStatusMessage,

    -- * MetricDimensionName
    MetricDimensionName (..),

    -- * MetricName
    MetricName (..),

    -- * ResourceIdMaxLen1600
    ResourceIdMaxLen1600 (..),

    -- * TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    mkTargetTrackingScalingPolicyConfiguration,
    ttspcTargetValue,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcScaleOutCooldown,

    -- * ServiceNamespace
    ServiceNamespace (..),

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- * SuspendedState
    SuspendedState (..),
    mkSuspendedState,
    ssDynamicScalingInSuspended,
    ssDynamicScalingOutSuspended,
    ssScheduledScalingSuspended,

    -- * ResourceLabel
    ResourceLabel (..),

    -- * ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saScheduledActionName,
    saScheduledActionARN,
    saServiceNamespace,
    saSchedule,
    saResourceId,
    saCreationTime,
    saEndTime,
    saScalableDimension,
    saScalableTargetAction,
    saStartTime,

    -- * StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    mkStepScalingPolicyConfiguration,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,
    sspcStepAdjustments,

    -- * AdjustmentType
    AdjustmentType (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saScalingAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,

    -- * MetricAggregationType
    MetricAggregationType (..),

    -- * ScalableTargetAction
    ScalableTargetAction (..),
    mkScalableTargetAction,
    staMaxCapacity,
    staMinCapacity,

    -- * Alarm
    Alarm (..),
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- * ScalableTarget
    ScalableTarget (..),
    mkScalableTarget,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,
    stSuspendedState,

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * Schedule
    Schedule (..),

    -- * PolicyARN
    PolicyARN (..),

    -- * Namespace
    Namespace (..),

    -- * Unit
    Unit (..),

    -- * Value
    Value (..),
  )
where

import Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
import Network.AWS.ApplicationAutoScaling.Types.Alarm
import Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
import Network.AWS.ApplicationAutoScaling.Types.MetricDimension
import Network.AWS.ApplicationAutoScaling.Types.MetricDimensionName
import Network.AWS.ApplicationAutoScaling.Types.MetricName
import Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
import Network.AWS.ApplicationAutoScaling.Types.MetricType
import Network.AWS.ApplicationAutoScaling.Types.Namespace
import Network.AWS.ApplicationAutoScaling.Types.PolicyARN
import Network.AWS.ApplicationAutoScaling.Types.PolicyName
import Network.AWS.ApplicationAutoScaling.Types.PolicyType
import Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.ResourceId
import Network.AWS.ApplicationAutoScaling.Types.ResourceIdMaxLen1600
import Network.AWS.ApplicationAutoScaling.Types.ResourceLabel
import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ScalableTarget
import Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivity
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
import Network.AWS.ApplicationAutoScaling.Types.Schedule
import Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
import Network.AWS.ApplicationAutoScaling.Types.ScheduledActionName
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.SuspendedState
import Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.Unit
import Network.AWS.ApplicationAutoScaling.Types.Value
import Network.AWS.ApplicationAutoScaling.Types.XmlString
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-06@ of the Amazon Application Auto Scaling SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "ApplicationAutoScaling",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "application-autoscaling",
      Core._svcVersion = "2016-02-06",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "ApplicationAutoScaling",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | An exception was thrown for a validation issue. Review the available parameters for the API request.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | Failed access to resources caused an exception. This exception is thrown when Application Auto Scaling is unable to retrieve the alarms associated with a scaling policy due to a client error, for example, if the role ARN specified for a scalable target does not have permission to call the CloudWatch <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html DescribeAlarms> on your behalf.
_FailedResourceAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FailedResourceAccessException =
  Core._MatchServiceError
    mkServiceConfig
    "FailedResourceAccessException"
{-# DEPRECATED _FailedResourceAccessException "Use generic-lens or generic-optics instead." #-}

-- | The next token supplied was invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | Concurrent updates caused an exception, for example, if you request an update to an Application Auto Scaling resource that already has a pending update.
_ConcurrentUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentUpdateException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentUpdateException"
{-# DEPRECATED _ConcurrentUpdateException "Use generic-lens or generic-optics instead." #-}

-- | The service encountered an internal error.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServiceException"
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead." #-}

-- | The specified object could not be found. For any operation that depends on the existence of a scalable target, this exception is thrown if the scalable target with the specified service namespace, resource ID, and scalable dimension does not exist. For any operation that deletes or deregisters a resource, this exception is thrown if the resource cannot be found.
_ObjectNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotFoundException =
  Core._MatchServiceError mkServiceConfig "ObjectNotFoundException"
{-# DEPRECATED _ObjectNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | A per-account resource limit is exceeded. For more information, see <https://docs.aws.amazon.com/ApplicationAutoScaling/latest/userguide/application-auto-scaling-limits.html Application Auto Scaling Limits> .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
