-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ValidationException
    , _InvalidNextTokenException
    , _ConcurrentUpdateException
    , _InternalServiceException
    , _ObjectNotFoundException
    , _LimitExceededException

    -- * ScalingStatusCode
    , ScalingStatusCode (..)

    -- * ScalingPlanResource
    , ScalingPlanResource (..)
    , mkScalingPlanResource
    , sprScalingPlanName
    , sprScalingPlanVersion
    , sprServiceNamespace
    , sprResourceId
    , sprScalableDimension
    , sprScalingStatusCode
    , sprScalingPolicies
    , sprScalingStatusMessage

    -- * XmlString
    , XmlString (..)

    -- * XmlStringMaxLen128
    , XmlStringMaxLen128 (..)

    -- * ScalableDimension
    , ScalableDimension (..)

    -- * ScalingPlanStatusCode
    , ScalingPlanStatusCode (..)

    -- * PolicyName
    , PolicyName (..)

    -- * PredefinedScalingMetricSpecification
    , PredefinedScalingMetricSpecification (..)
    , mkPredefinedScalingMetricSpecification
    , psmsPredefinedScalingMetricType
    , psmsResourceLabel

    -- * ScalingPolicy
    , ScalingPolicy (..)
    , mkScalingPolicy
    , spPolicyName
    , spPolicyType
    , spTargetTrackingConfiguration

    -- * ScalingPolicyUpdateBehavior
    , ScalingPolicyUpdateBehavior (..)

    -- * PolicyType
    , PolicyType (..)

    -- * MetricDimensionName
    , MetricDimensionName (..)

    -- * MetricName
    , MetricName (..)

    -- * ResourceIdMaxLen1600
    , ResourceIdMaxLen1600 (..)

    -- * ScalingPlan
    , ScalingPlan (..)
    , mkScalingPlan
    , spScalingPlanName
    , spScalingPlanVersion
    , spApplicationSource
    , spScalingInstructions
    , spStatusCode
    , spCreationTime
    , spStatusMessage
    , spStatusStartTime

    -- * CustomizedLoadMetricSpecification
    , CustomizedLoadMetricSpecification (..)
    , mkCustomizedLoadMetricSpecification
    , clmsMetricName
    , clmsNamespace
    , clmsStatistic
    , clmsDimensions
    , clmsUnit

    -- * XmlStringMaxLen256
    , XmlStringMaxLen256 (..)

    -- * TargetTrackingConfiguration
    , TargetTrackingConfiguration (..)
    , mkTargetTrackingConfiguration
    , ttcTargetValue
    , ttcCustomizedScalingMetricSpecification
    , ttcDisableScaleIn
    , ttcEstimatedInstanceWarmup
    , ttcPredefinedScalingMetricSpecification
    , ttcScaleInCooldown
    , ttcScaleOutCooldown

    -- * ServiceNamespace
    , ServiceNamespace (..)

    -- * PredictiveScalingMode
    , PredictiveScalingMode (..)

    -- * ScalingPlanName
    , ScalingPlanName (..)

    -- * MetricDimension
    , MetricDimension (..)
    , mkMetricDimension
    , mdName
    , mdValue

    -- * NextToken
    , NextToken (..)

    -- * ResourceLabel
    , ResourceLabel (..)

    -- * Datapoint
    , Datapoint (..)
    , mkDatapoint
    , dTimestamp
    , dValue

    -- * MetricNamespace
    , MetricNamespace (..)

    -- * ApplicationSource
    , ApplicationSource (..)
    , mkApplicationSource
    , asCloudFormationStackARN
    , asTagFilters

    -- * MetricUnit
    , MetricUnit (..)

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * LoadMetricType
    , LoadMetricType (..)

    -- * TagFilter
    , TagFilter (..)
    , mkTagFilter
    , tfKey
    , tfValues

    -- * CustomizedScalingMetricSpecification
    , CustomizedScalingMetricSpecification (..)
    , mkCustomizedScalingMetricSpecification
    , csmsMetricName
    , csmsNamespace
    , csmsStatistic
    , csmsDimensions
    , csmsUnit

    -- * ScalingInstruction
    , ScalingInstruction (..)
    , mkScalingInstruction
    , siServiceNamespace
    , siResourceId
    , siScalableDimension
    , siMinCapacity
    , siMaxCapacity
    , siTargetTrackingConfigurations
    , siCustomizedLoadMetricSpecification
    , siDisableDynamicScaling
    , siPredefinedLoadMetricSpecification
    , siPredictiveScalingMaxCapacityBehavior
    , siPredictiveScalingMaxCapacityBuffer
    , siPredictiveScalingMode
    , siScalingPolicyUpdateBehavior
    , siScheduledActionBufferTime

    -- * ForecastDataType
    , ForecastDataType (..)

    -- * PredictiveScalingMaxCapacityBehavior
    , PredictiveScalingMaxCapacityBehavior (..)

    -- * PredefinedLoadMetricSpecification
    , PredefinedLoadMetricSpecification (..)
    , mkPredefinedLoadMetricSpecification
    , plmsPredefinedLoadMetricType
    , plmsResourceLabel

    -- * ScalingMetricType
    , ScalingMetricType (..)

    -- * ResourceId
    , ResourceId (..)

    -- * ScalingStatusMessage
    , ScalingStatusMessage (..)

    -- * Namespace
    , Namespace (..)

    -- * Unit
    , Unit (..)

    -- * Value
    , Value (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
  
import Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
  
import Network.AWS.AutoScalingPlans.Types.XmlString
  
import Network.AWS.AutoScalingPlans.Types.XmlStringMaxLen128
  
  
import Network.AWS.AutoScalingPlans.Types.ScalableDimension
  
import Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
  
import Network.AWS.AutoScalingPlans.Types.PolicyName
  
import Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
  
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
  
import Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
  
import Network.AWS.AutoScalingPlans.Types.PolicyType
  
import Network.AWS.AutoScalingPlans.Types.MetricDimensionName
  
import Network.AWS.AutoScalingPlans.Types.MetricName
  
import Network.AWS.AutoScalingPlans.Types.ResourceIdMaxLen1600
  
import Network.AWS.AutoScalingPlans.Types.ScalingPlan
  
import Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
  
import Network.AWS.AutoScalingPlans.Types.XmlStringMaxLen256
  
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
  
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
  
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode
  
import Network.AWS.AutoScalingPlans.Types.ScalingPlanName
  
import Network.AWS.AutoScalingPlans.Types.MetricDimension
  
import Network.AWS.AutoScalingPlans.Types.NextToken
  
import Network.AWS.AutoScalingPlans.Types.ResourceLabel
  
import Network.AWS.AutoScalingPlans.Types.Datapoint
  
import Network.AWS.AutoScalingPlans.Types.MetricNamespace
  
import Network.AWS.AutoScalingPlans.Types.ApplicationSource
  
import Network.AWS.AutoScalingPlans.Types.MetricUnit
  
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
  
import Network.AWS.AutoScalingPlans.Types.LoadMetricType
  
import Network.AWS.AutoScalingPlans.Types.TagFilter
  
  
import Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
  
  
import Network.AWS.AutoScalingPlans.Types.ScalingInstruction
  
  
import Network.AWS.AutoScalingPlans.Types.ForecastDataType
  
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
  
import Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
  
  
import Network.AWS.AutoScalingPlans.Types.ScalingMetricType
  
  
import Network.AWS.AutoScalingPlans.Types.ResourceId
  
import Network.AWS.AutoScalingPlans.Types.ScalingStatusMessage
  
import Network.AWS.AutoScalingPlans.Types.Namespace
  
import Network.AWS.AutoScalingPlans.Types.Unit
  
import Network.AWS.AutoScalingPlans.Types.Value
  

-- | API version @2018-01-06@ of the Amazon Auto Scaling Plans SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "AutoScalingPlans",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "autoscaling-plans",
                 Core._svcVersion = "2018-01-06", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "AutoScalingPlans",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | An exception was thrown for a validation issue. Review the parameters provided.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The token provided is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.
_ConcurrentUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentUpdateException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentUpdateException"
{-# INLINEABLE _ConcurrentUpdateException #-}
{-# DEPRECATED _ConcurrentUpdateException "Use generic-lens or generic-optics instead"  #-}

-- | The service encountered an internal error.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The specified object could not be found.
_ObjectNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotFoundException
  = Core._MatchServiceError mkServiceConfig "ObjectNotFoundException"
{-# INLINEABLE _ObjectNotFoundException #-}
{-# DEPRECATED _ObjectNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
