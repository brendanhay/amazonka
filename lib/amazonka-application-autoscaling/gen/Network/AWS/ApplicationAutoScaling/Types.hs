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
    applicationAutoScalingService,

    -- * Errors

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
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsResourceLabel,
    pmsPredefinedMetricType,

    -- * ScalableTarget
    ScalableTarget (..),
    mkScalableTarget,
    stSuspendedState,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,

    -- * ScalableTargetAction
    ScalableTargetAction (..),
    mkScalableTargetAction,
    staMaxCapacity,
    staMinCapacity,

    -- * ScalingActivity
    ScalingActivity (..),
    mkScalingActivity,
    sStatusMessage,
    sEndTime,
    sDetails,
    sActivityId,
    sServiceNamespace,
    sResourceId,
    sScalableDimension,
    sDescription,
    sCause,
    sStartTime,
    sStatusCode,

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spTargetTrackingScalingPolicyConfiguration,
    spStepScalingPolicyConfiguration,
    spAlarms,
    spPolicyARN,
    spPolicyName,
    spServiceNamespace,
    spResourceId,
    spScalableDimension,
    spPolicyType,
    spCreationTime,

    -- * ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saScalableDimension,
    saStartTime,
    saEndTime,
    saScalableTargetAction,
    saScheduledActionName,
    saScheduledActionARN,
    saServiceNamespace,
    saSchedule,
    saResourceId,
    saCreationTime,

    -- * StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- * StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    mkStepScalingPolicyConfiguration,
    sspcStepAdjustments,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,

    -- * SuspendedState
    SuspendedState (..),
    mkSuspendedState,
    ssDynamicScalingInSuspended,
    ssScheduledScalingSuspended,
    ssDynamicScalingOutSuspended,

    -- * TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    mkTargetTrackingScalingPolicyConfiguration,
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcScaleOutCooldown,
    ttspcTargetValue,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-02-06@ of the Amazon Application Auto Scaling SDK configuration.
applicationAutoScalingService :: Lude.Service
applicationAutoScalingService =
  Lude.Service
    { Lude._svcAbbrev = "ApplicationAutoScaling",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "application-autoscaling",
      Lude._svcVersion = "2016-02-06",
      Lude._svcEndpoint =
        Lude.defaultEndpoint applicationAutoScalingService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ApplicationAutoScaling",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
