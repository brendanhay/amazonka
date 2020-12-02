{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Auto Scaling__
--
-- Use AWS Auto Scaling to quickly discover all the scalable AWS resources for your application and configure dynamic scaling for your scalable resources.
--
-- To get started, create a scaling plan with a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies for the scalable resources in your scaling plan. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.
--
module Network.AWS.AutoScalingPlans
    (
    -- * Service Configuration
      autoScalingPlans

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ConcurrentUpdateException
    , _ConcurrentUpdateException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** ObjectNotFoundException
    , _ObjectNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeScalingPlanResources
    , module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources

    -- ** CreateScalingPlan
    , module Network.AWS.AutoScalingPlans.CreateScalingPlan

    -- ** DeleteScalingPlan
    , module Network.AWS.AutoScalingPlans.DeleteScalingPlan

    -- ** UpdateScalingPlan
    , module Network.AWS.AutoScalingPlans.UpdateScalingPlan

    -- ** DescribeScalingPlans
    , module Network.AWS.AutoScalingPlans.DescribeScalingPlans

    -- * Types

    -- ** MetricStatistic
    , MetricStatistic (..)

    -- ** PolicyType
    , PolicyType (..)

    -- ** ScalableDimension
    , ScalableDimension (..)

    -- ** ScalingMetricType
    , ScalingMetricType (..)

    -- ** ScalingPlanStatusCode
    , ScalingPlanStatusCode (..)

    -- ** ScalingStatusCode
    , ScalingStatusCode (..)

    -- ** ServiceNamespace
    , ServiceNamespace (..)

    -- ** ApplicationSource
    , ApplicationSource
    , applicationSource
    , asTagFilters
    , asCloudFormationStackARN

    -- ** CustomizedScalingMetricSpecification
    , CustomizedScalingMetricSpecification
    , customizedScalingMetricSpecification
    , csmsDimensions
    , csmsUnit
    , csmsMetricName
    , csmsNamespace
    , csmsStatistic

    -- ** MetricDimension
    , MetricDimension
    , metricDimension
    , mdName
    , mdValue

    -- ** PredefinedScalingMetricSpecification
    , PredefinedScalingMetricSpecification
    , predefinedScalingMetricSpecification
    , psmsResourceLabel
    , psmsPredefinedScalingMetricType

    -- ** ScalingInstruction
    , ScalingInstruction
    , scalingInstruction
    , siServiceNamespace
    , siResourceId
    , siScalableDimension
    , siMinCapacity
    , siMaxCapacity
    , siTargetTrackingConfigurations

    -- ** ScalingPlan
    , ScalingPlan
    , scalingPlan
    , spCreationTime
    , spStatusStartTime
    , spStatusMessage
    , spScalingPlanName
    , spScalingPlanVersion
    , spApplicationSource
    , spScalingInstructions
    , spStatusCode

    -- ** ScalingPlanResource
    , ScalingPlanResource
    , scalingPlanResource
    , sprScalingStatusMessage
    , sprScalingPolicies
    , sprScalingPlanName
    , sprScalingPlanVersion
    , sprServiceNamespace
    , sprResourceId
    , sprScalableDimension
    , sprScalingStatusCode

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spTargetTrackingConfiguration
    , spPolicyName
    , spPolicyType

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfValues
    , tfKey

    -- ** TargetTrackingConfiguration
    , TargetTrackingConfiguration
    , targetTrackingConfiguration
    , ttcEstimatedInstanceWarmup
    , ttcPredefinedScalingMetricSpecification
    , ttcScaleInCooldown
    , ttcDisableScaleIn
    , ttcCustomizedScalingMetricSpecification
    , ttcScaleOutCooldown
    , ttcTargetValue
    ) where

import Network.AWS.AutoScalingPlans.CreateScalingPlan
import Network.AWS.AutoScalingPlans.DeleteScalingPlan
import Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
import Network.AWS.AutoScalingPlans.DescribeScalingPlans
import Network.AWS.AutoScalingPlans.Types
import Network.AWS.AutoScalingPlans.UpdateScalingPlan
import Network.AWS.AutoScalingPlans.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AutoScalingPlans'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
