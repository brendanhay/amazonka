{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Auto Scaling__
--
-- Use AWS Auto Scaling to quickly discover all the scalable AWS resources for your application and configure dynamic scaling and predictive scaling for your resources using scaling plans. Use this service in conjunction with the Amazon EC2 Auto Scaling, Application Auto Scaling, Amazon CloudWatch, and AWS CloudFormation services.
-- Currently, predictive scaling is only available for Amazon EC2 Auto Scaling groups.
-- For more information about AWS Auto Scaling, including information about granting IAM users required permissions for AWS Auto Scaling actions, see the <https://docs.aws.amazon.com/autoscaling/plans/userguide/what-is-aws-auto-scaling.html AWS Auto Scaling User Guide> .
module Network.AWS.AutoScalingPlans
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ConcurrentUpdateException
    _ConcurrentUpdateException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** ObjectNotFoundException
    _ObjectNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeScalingPlanResources (Paginated)
    module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources,

    -- ** CreateScalingPlan
    module Network.AWS.AutoScalingPlans.CreateScalingPlan,

    -- ** DeleteScalingPlan
    module Network.AWS.AutoScalingPlans.DeleteScalingPlan,

    -- ** UpdateScalingPlan
    module Network.AWS.AutoScalingPlans.UpdateScalingPlan,

    -- ** DescribeScalingPlans (Paginated)
    module Network.AWS.AutoScalingPlans.DescribeScalingPlans,

    -- ** GetScalingPlanResourceForecastData
    module Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData,

    -- * Types

    -- ** ScalingStatusCode
    ScalingStatusCode (..),

    -- ** ScalingPlanResource
    ScalingPlanResource (..),
    mkScalingPlanResource,
    sprScalingPlanName,
    sprScalingPlanVersion,
    sprServiceNamespace,
    sprResourceId,
    sprScalableDimension,
    sprScalingStatusCode,
    sprScalingPolicies,
    sprScalingStatusMessage,

    -- ** XmlString
    XmlString (..),

    -- ** XmlStringMaxLen128
    XmlStringMaxLen128 (..),

    -- ** ScalableDimension
    ScalableDimension (..),

    -- ** ScalingPlanStatusCode
    ScalingPlanStatusCode (..),

    -- ** PolicyName
    PolicyName (..),

    -- ** PredefinedScalingMetricSpecification
    PredefinedScalingMetricSpecification (..),
    mkPredefinedScalingMetricSpecification,
    psmsPredefinedScalingMetricType,
    psmsResourceLabel,

    -- ** ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spPolicyName,
    spPolicyType,
    spTargetTrackingConfiguration,

    -- ** ScalingPolicyUpdateBehavior
    ScalingPolicyUpdateBehavior (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** MetricDimensionName
    MetricDimensionName (..),

    -- ** MetricName
    MetricName (..),

    -- ** ResourceIdMaxLen1600
    ResourceIdMaxLen1600 (..),

    -- ** ScalingPlan
    ScalingPlan (..),
    mkScalingPlan,
    spScalingPlanName,
    spScalingPlanVersion,
    spApplicationSource,
    spScalingInstructions,
    spStatusCode,
    spCreationTime,
    spStatusMessage,
    spStatusStartTime,

    -- ** CustomizedLoadMetricSpecification
    CustomizedLoadMetricSpecification (..),
    mkCustomizedLoadMetricSpecification,
    clmsMetricName,
    clmsNamespace,
    clmsStatistic,
    clmsDimensions,
    clmsUnit,

    -- ** XmlStringMaxLen256
    XmlStringMaxLen256 (..),

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcTargetValue,
    ttcCustomizedScalingMetricSpecification,
    ttcDisableScaleIn,
    ttcEstimatedInstanceWarmup,
    ttcPredefinedScalingMetricSpecification,
    ttcScaleInCooldown,
    ttcScaleOutCooldown,

    -- ** ServiceNamespace
    ServiceNamespace (..),

    -- ** PredictiveScalingMode
    PredictiveScalingMode (..),

    -- ** ScalingPlanName
    ScalingPlanName (..),

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- ** NextToken
    NextToken (..),

    -- ** ResourceLabel
    ResourceLabel (..),

    -- ** Datapoint
    Datapoint (..),
    mkDatapoint,
    dTimestamp,
    dValue,

    -- ** MetricNamespace
    MetricNamespace (..),

    -- ** ApplicationSource
    ApplicationSource (..),
    mkApplicationSource,
    asCloudFormationStackARN,
    asTagFilters,

    -- ** MetricUnit
    MetricUnit (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** LoadMetricType
    LoadMetricType (..),

    -- ** TagFilter
    TagFilter (..),
    mkTagFilter,
    tfKey,
    tfValues,

    -- ** CustomizedScalingMetricSpecification
    CustomizedScalingMetricSpecification (..),
    mkCustomizedScalingMetricSpecification,
    csmsMetricName,
    csmsNamespace,
    csmsStatistic,
    csmsDimensions,
    csmsUnit,

    -- ** ScalingInstruction
    ScalingInstruction (..),
    mkScalingInstruction,
    siServiceNamespace,
    siResourceId,
    siScalableDimension,
    siMinCapacity,
    siMaxCapacity,
    siTargetTrackingConfigurations,
    siCustomizedLoadMetricSpecification,
    siDisableDynamicScaling,
    siPredefinedLoadMetricSpecification,
    siPredictiveScalingMaxCapacityBehavior,
    siPredictiveScalingMaxCapacityBuffer,
    siPredictiveScalingMode,
    siScalingPolicyUpdateBehavior,
    siScheduledActionBufferTime,

    -- ** ForecastDataType
    ForecastDataType (..),

    -- ** PredictiveScalingMaxCapacityBehavior
    PredictiveScalingMaxCapacityBehavior (..),

    -- ** PredefinedLoadMetricSpecification
    PredefinedLoadMetricSpecification (..),
    mkPredefinedLoadMetricSpecification,
    plmsPredefinedLoadMetricType,
    plmsResourceLabel,

    -- ** ScalingMetricType
    ScalingMetricType (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** ScalingStatusMessage
    ScalingStatusMessage (..),

    -- ** Namespace
    Namespace (..),

    -- ** Unit
    Unit (..),

    -- ** Value
    Value (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.AutoScalingPlans.CreateScalingPlan
import Network.AWS.AutoScalingPlans.DeleteScalingPlan
import Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
import Network.AWS.AutoScalingPlans.DescribeScalingPlans
import Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.UpdateScalingPlan
import Network.AWS.AutoScalingPlans.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AutoScalingPlans'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
