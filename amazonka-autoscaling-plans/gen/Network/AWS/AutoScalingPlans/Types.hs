{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ObjectNotFoundException,
    _InternalServiceException,
    _InvalidNextTokenException,
    _ValidationException,
    _LimitExceededException,
    _ConcurrentUpdateException,

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
    newApplicationSource,
    applicationSource_tagFilters,
    applicationSource_cloudFormationStackARN,

    -- * CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification (..),
    newCustomizedLoadMetricSpecification,
    customizedLoadMetricSpecification_unit,
    customizedLoadMetricSpecification_dimensions,
    customizedLoadMetricSpecification_metricName,
    customizedLoadMetricSpecification_namespace,
    customizedLoadMetricSpecification_statistic,

    -- * CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification (..),
    newCustomizedScalingMetricSpecification,
    customizedScalingMetricSpecification_unit,
    customizedScalingMetricSpecification_dimensions,
    customizedScalingMetricSpecification_metricName,
    customizedScalingMetricSpecification_namespace,
    customizedScalingMetricSpecification_statistic,

    -- * Datapoint
    Datapoint (..),
    newDatapoint,
    datapoint_timestamp,
    datapoint_value,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_name,
    metricDimension_value,

    -- * PredefinedLoadMetricSpecification
    PredefinedLoadMetricSpecification (..),
    newPredefinedLoadMetricSpecification,
    predefinedLoadMetricSpecification_resourceLabel,
    predefinedLoadMetricSpecification_predefinedLoadMetricType,

    -- * PredefinedScalingMetricSpecification
    PredefinedScalingMetricSpecification (..),
    newPredefinedScalingMetricSpecification,
    predefinedScalingMetricSpecification_resourceLabel,
    predefinedScalingMetricSpecification_predefinedScalingMetricType,

    -- * ScalingInstruction
    ScalingInstruction (..),
    newScalingInstruction,
    scalingInstruction_disableDynamicScaling,
    scalingInstruction_predefinedLoadMetricSpecification,
    scalingInstruction_predictiveScalingMaxCapacityBehavior,
    scalingInstruction_customizedLoadMetricSpecification,
    scalingInstruction_predictiveScalingMaxCapacityBuffer,
    scalingInstruction_predictiveScalingMode,
    scalingInstruction_scalingPolicyUpdateBehavior,
    scalingInstruction_scheduledActionBufferTime,
    scalingInstruction_serviceNamespace,
    scalingInstruction_resourceId,
    scalingInstruction_scalableDimension,
    scalingInstruction_minCapacity,
    scalingInstruction_maxCapacity,
    scalingInstruction_targetTrackingConfigurations,

    -- * ScalingPlan
    ScalingPlan (..),
    newScalingPlan,
    scalingPlan_statusMessage,
    scalingPlan_creationTime,
    scalingPlan_statusStartTime,
    scalingPlan_scalingPlanName,
    scalingPlan_scalingPlanVersion,
    scalingPlan_applicationSource,
    scalingPlan_scalingInstructions,
    scalingPlan_statusCode,

    -- * ScalingPlanResource
    ScalingPlanResource (..),
    newScalingPlanResource,
    scalingPlanResource_scalingPolicies,
    scalingPlanResource_scalingStatusMessage,
    scalingPlanResource_scalingPlanName,
    scalingPlanResource_scalingPlanVersion,
    scalingPlanResource_serviceNamespace,
    scalingPlanResource_resourceId,
    scalingPlanResource_scalableDimension,
    scalingPlanResource_scalingStatusCode,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_policyName,
    scalingPolicy_policyType,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_key,
    tagFilter_values,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    newTargetTrackingConfiguration,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_customizedScalingMetricSpecification,
    targetTrackingConfiguration_predefinedScalingMetricSpecification,
    targetTrackingConfiguration_estimatedInstanceWarmup,
    targetTrackingConfiguration_scaleOutCooldown,
    targetTrackingConfiguration_scaleInCooldown,
    targetTrackingConfiguration_targetValue,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "AutoScalingPlans",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "autoscaling-plans",
      Core._serviceSigningName = "autoscaling-plans",
      Core._serviceVersion = "2018-01-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AutoScalingPlans",
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

-- | The specified object could not be found.
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

-- | The token provided is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | An exception was thrown for a validation issue. Review the parameters
-- provided.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | Your account exceeded a limit. This exception is thrown when a
-- per-account resource limit is exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Concurrent updates caused an exception, for example, if you request an
-- update to a scaling plan that already has a pending update.
_ConcurrentUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdateException"
