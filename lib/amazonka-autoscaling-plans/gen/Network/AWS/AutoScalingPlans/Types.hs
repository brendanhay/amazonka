{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types
  ( -- * Service Configuration
    autoScalingPlans,

    -- * Errors

    -- * ForecastDataType
    ForecastDataType (..),

    -- * LoadMetricType
    LoadMetricType (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * PolicyType
    PolicyType (..),

    -- * PredictiveScalingMaxCapacityBehavior
    PredictiveScalingMaxCapacityBehavior (..),

    -- * PredictiveScalingMode
    PredictiveScalingMode (..),

    -- * ScalableDimension
    ScalableDimension (..),

    -- * ScalingMetricType
    ScalingMetricType (..),

    -- * ScalingPlanStatusCode
    ScalingPlanStatusCode (..),

    -- * ScalingPolicyUpdateBehavior
    ScalingPolicyUpdateBehavior (..),

    -- * ScalingStatusCode
    ScalingStatusCode (..),

    -- * ServiceNamespace
    ServiceNamespace (..),

    -- * ApplicationSource
    ApplicationSource,
    applicationSource,
    asTagFilters,
    asCloudFormationStackARN,

    -- * CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification,
    customizedLoadMetricSpecification,
    clmsDimensions,
    clmsUnit,
    clmsMetricName,
    clmsNamespace,
    clmsStatistic,

    -- * CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification,
    customizedScalingMetricSpecification,
    csmsDimensions,
    csmsUnit,
    csmsMetricName,
    csmsNamespace,
    csmsStatistic,

    -- * Datapoint
    Datapoint,
    datapoint,
    dValue,
    dTimestamp,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdName,
    mdValue,

    -- * PredefinedLoadMetricSpecification
    PredefinedLoadMetricSpecification,
    predefinedLoadMetricSpecification,
    plmsResourceLabel,
    plmsPredefinedLoadMetricType,

    -- * PredefinedScalingMetricSpecification
    PredefinedScalingMetricSpecification,
    predefinedScalingMetricSpecification,
    psmsResourceLabel,
    psmsPredefinedScalingMetricType,

    -- * ScalingInstruction
    ScalingInstruction,
    scalingInstruction,
    siScheduledActionBufferTime,
    siPredictiveScalingMaxCapacityBuffer,
    siScalingPolicyUpdateBehavior,
    siCustomizedLoadMetricSpecification,
    siPredictiveScalingMode,
    siDisableDynamicScaling,
    siPredictiveScalingMaxCapacityBehavior,
    siPredefinedLoadMetricSpecification,
    siServiceNamespace,
    siResourceId,
    siScalableDimension,
    siMinCapacity,
    siMaxCapacity,
    siTargetTrackingConfigurations,

    -- * ScalingPlan
    ScalingPlan,
    scalingPlan,
    spCreationTime,
    spStatusStartTime,
    spStatusMessage,
    spScalingPlanName,
    spScalingPlanVersion,
    spApplicationSource,
    spScalingInstructions,
    spStatusCode,

    -- * ScalingPlanResource
    ScalingPlanResource,
    scalingPlanResource,
    sprScalingStatusMessage,
    sprScalingPolicies,
    sprScalingPlanName,
    sprScalingPlanVersion,
    sprServiceNamespace,
    sprResourceId,
    sprScalableDimension,
    sprScalingStatusCode,

    -- * ScalingPolicy
    ScalingPolicy,
    scalingPolicy,
    spTargetTrackingConfiguration,
    spPolicyName,
    spPolicyType,

    -- * TagFilter
    TagFilter,
    tagFilter,
    tfValues,
    tfKey,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration,
    targetTrackingConfiguration,
    ttcEstimatedInstanceWarmup,
    ttcPredefinedScalingMetricSpecification,
    ttcScaleInCooldown,
    ttcDisableScaleIn,
    ttcCustomizedScalingMetricSpecification,
    ttcScaleOutCooldown,
    ttcTargetValue,
  )
where

import Network.AWS.AutoScalingPlans.Types.ApplicationSource
import Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.Datapoint
import Network.AWS.AutoScalingPlans.Types.ForecastDataType
import Network.AWS.AutoScalingPlans.Types.LoadMetricType
import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
import Network.AWS.AutoScalingPlans.Types.PolicyType
import Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode
import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingInstruction
import Network.AWS.AutoScalingPlans.Types.ScalingMetricType
import Network.AWS.AutoScalingPlans.Types.ScalingPlan
import Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
import Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
import Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
import Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import Network.AWS.AutoScalingPlans.Types.TagFilter
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
autoScalingPlans :: Service
autoScalingPlans =
  Service
    { _svcAbbrev = "AutoScalingPlans",
      _svcSigner = v4,
      _svcPrefix = "autoscaling-plans",
      _svcVersion = "2018-01-06",
      _svcEndpoint = defaultEndpoint autoScalingPlans,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "AutoScalingPlans",
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
