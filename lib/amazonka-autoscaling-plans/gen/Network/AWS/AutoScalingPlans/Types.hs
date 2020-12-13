-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types
  ( -- * Service configuration
    autoScalingPlansService,

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
    ApplicationSource (..),
    mkApplicationSource,
    asTagFilters,
    asCloudFormationStackARN,

    -- * CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification (..),
    mkCustomizedLoadMetricSpecification,
    clmsMetricName,
    clmsNamespace,
    clmsDimensions,
    clmsUnit,
    clmsStatistic,

    -- * CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification (..),
    mkCustomizedScalingMetricSpecification,
    csmsMetricName,
    csmsNamespace,
    csmsDimensions,
    csmsUnit,
    csmsStatistic,

    -- * Datapoint
    Datapoint (..),
    mkDatapoint,
    dValue,
    dTimestamp,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdName,

    -- * PredefinedLoadMetricSpecification
    PredefinedLoadMetricSpecification (..),
    mkPredefinedLoadMetricSpecification,
    plmsResourceLabel,
    plmsPredefinedLoadMetricType,

    -- * PredefinedScalingMetricSpecification
    PredefinedScalingMetricSpecification (..),
    mkPredefinedScalingMetricSpecification,
    psmsResourceLabel,
    psmsPredefinedScalingMetricType,

    -- * ScalingInstruction
    ScalingInstruction (..),
    mkScalingInstruction,
    siScalableDimension,
    siResourceId,
    siScheduledActionBufferTime,
    siPredictiveScalingMaxCapacityBuffer,
    siTargetTrackingConfigurations,
    siScalingPolicyUpdateBehavior,
    siCustomizedLoadMetricSpecification,
    siServiceNamespace,
    siPredictiveScalingMode,
    siDisableDynamicScaling,
    siMaxCapacity,
    siMinCapacity,
    siPredictiveScalingMaxCapacityBehavior,
    siPredefinedLoadMetricSpecification,

    -- * ScalingPlan
    ScalingPlan (..),
    mkScalingPlan,
    spCreationTime,
    spScalingPlanVersion,
    spScalingInstructions,
    spStatusStartTime,
    spScalingPlanName,
    spApplicationSource,
    spStatusMessage,
    spStatusCode,

    -- * ScalingPlanResource
    ScalingPlanResource (..),
    mkScalingPlanResource,
    sprScalingStatusCode,
    sprScalingPlanVersion,
    sprScalableDimension,
    sprResourceId,
    sprServiceNamespace,
    sprScalingPlanName,
    sprScalingStatusMessage,
    sprScalingPolicies,

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spPolicyName,
    spPolicyType,
    spTargetTrackingConfiguration,

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfValues,
    tfKey,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcEstimatedInstanceWarmup,
    ttcPredefinedScalingMetricSpecification,
    ttcTargetValue,
    ttcScaleInCooldown,
    ttcDisableScaleIn,
    ttcCustomizedScalingMetricSpecification,
    ttcScaleOutCooldown,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
autoScalingPlansService :: Lude.Service
autoScalingPlansService =
  Lude.Service
    { Lude._svcAbbrev = "AutoScalingPlans",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "autoscaling-plans",
      Lude._svcVersion = "2018-01-06",
      Lude._svcEndpoint = Lude.defaultEndpoint autoScalingPlansService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "AutoScalingPlans",
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
