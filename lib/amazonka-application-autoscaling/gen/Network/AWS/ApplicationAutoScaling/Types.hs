{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types
  ( -- * Service Configuration
    applicationAutoScaling,

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
    Alarm,
    alarm,
    aAlarmName,
    aAlarmARN,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification,
    customizedMetricSpecification,
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdName,
    mdValue,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification,
    predefinedMetricSpecification,
    pmsResourceLabel,
    pmsPredefinedMetricType,

    -- * ScalableTarget
    ScalableTarget,
    scalableTarget,
    stSuspendedState,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,

    -- * ScalableTargetAction
    ScalableTargetAction,
    scalableTargetAction,
    staMaxCapacity,
    staMinCapacity,

    -- * ScalingActivity
    ScalingActivity,
    scalingActivity,
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
    ScalingPolicy,
    scalingPolicy,
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
    ScheduledAction,
    scheduledAction,
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
    StepAdjustment,
    stepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- * StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration,
    stepScalingPolicyConfiguration,
    sspcStepAdjustments,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,

    -- * SuspendedState
    SuspendedState,
    suspendedState,
    ssDynamicScalingInSuspended,
    ssScheduledScalingSuspended,
    ssDynamicScalingOutSuspended,

    -- * TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration,
    targetTrackingScalingPolicyConfiguration,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-02-06@ of the Amazon Application Auto Scaling SDK configuration.
applicationAutoScaling :: Service
applicationAutoScaling =
  Service
    { _svcAbbrev = "ApplicationAutoScaling",
      _svcSigner = v4,
      _svcPrefix = "application-autoscaling",
      _svcVersion = "2016-02-06",
      _svcEndpoint = defaultEndpoint applicationAutoScaling,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ApplicationAutoScaling",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
