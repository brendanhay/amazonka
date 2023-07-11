{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentUpdateException,
    _InternalServiceException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _ObjectNotFoundException,
    _ValidationException,

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
    applicationSource_cloudFormationStackARN,
    applicationSource_tagFilters,

    -- * CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification (..),
    newCustomizedLoadMetricSpecification,
    customizedLoadMetricSpecification_dimensions,
    customizedLoadMetricSpecification_unit,
    customizedLoadMetricSpecification_metricName,
    customizedLoadMetricSpecification_namespace,
    customizedLoadMetricSpecification_statistic,

    -- * CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification (..),
    newCustomizedScalingMetricSpecification,
    customizedScalingMetricSpecification_dimensions,
    customizedScalingMetricSpecification_unit,
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
    scalingInstruction_customizedLoadMetricSpecification,
    scalingInstruction_disableDynamicScaling,
    scalingInstruction_predefinedLoadMetricSpecification,
    scalingInstruction_predictiveScalingMaxCapacityBehavior,
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
    scalingPlan_creationTime,
    scalingPlan_statusMessage,
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
    targetTrackingConfiguration_customizedScalingMetricSpecification,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_estimatedInstanceWarmup,
    targetTrackingConfiguration_predefinedScalingMetricSpecification,
    targetTrackingConfiguration_scaleInCooldown,
    targetTrackingConfiguration_scaleOutCooldown,
    targetTrackingConfiguration_targetValue,
  )
where

import Amazonka.AutoScalingPlans.Types.ApplicationSource
import Amazonka.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Amazonka.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Amazonka.AutoScalingPlans.Types.Datapoint
import Amazonka.AutoScalingPlans.Types.ForecastDataType
import Amazonka.AutoScalingPlans.Types.LoadMetricType
import Amazonka.AutoScalingPlans.Types.MetricDimension
import Amazonka.AutoScalingPlans.Types.MetricStatistic
import Amazonka.AutoScalingPlans.Types.PolicyType
import Amazonka.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Amazonka.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import Amazonka.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
import Amazonka.AutoScalingPlans.Types.PredictiveScalingMode
import Amazonka.AutoScalingPlans.Types.ScalableDimension
import Amazonka.AutoScalingPlans.Types.ScalingInstruction
import Amazonka.AutoScalingPlans.Types.ScalingMetricType
import Amazonka.AutoScalingPlans.Types.ScalingPlan
import Amazonka.AutoScalingPlans.Types.ScalingPlanResource
import Amazonka.AutoScalingPlans.Types.ScalingPlanStatusCode
import Amazonka.AutoScalingPlans.Types.ScalingPolicy
import Amazonka.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
import Amazonka.AutoScalingPlans.Types.ScalingStatusCode
import Amazonka.AutoScalingPlans.Types.ServiceNamespace
import Amazonka.AutoScalingPlans.Types.TagFilter
import Amazonka.AutoScalingPlans.Types.TargetTrackingConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AutoScalingPlans",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "autoscaling-plans",
      Core.signingName = "autoscaling-plans",
      Core.version = "2018-01-06",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AutoScalingPlans",
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
-- update to a scaling plan that already has a pending update.
_ConcurrentUpdateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentUpdateException"

-- | The service encountered an internal error.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The token provided is not valid.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | Your account exceeded a limit. This exception is thrown when a
-- per-account resource limit is exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified object could not be found.
_ObjectNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ObjectNotFoundException =
  Core._MatchServiceError
    defaultService
    "ObjectNotFoundException"

-- | An exception was thrown for a validation issue. Review the parameters
-- provided.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
