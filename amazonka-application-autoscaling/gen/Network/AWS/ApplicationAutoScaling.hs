{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Application Auto Scaling is a general purpose Auto Scaling service for supported elastic AWS resources. With Application Auto Scaling, you can automatically scale your AWS resources, with an experience similar to that of Auto Scaling.
--
-- Application Auto Scaling supports scaling the following AWS resources:
--
-- -   Amazon ECS services
--
-- -   Amazon EC2 Spot fleet instances
--
-- You can use Application Auto Scaling to accomplish the following tasks:
--
-- -   Define scaling policies for automatically adjusting your AWS resources
--
-- -   Scale your resources in response to CloudWatch alarms
--
-- -   View history of your scaling events
--
-- Application Auto Scaling is available in the following regions:
--
-- -   'us-east-1'
--
-- -   'us-west-1'
--
-- -   'us-west-2'
--
-- -   'ap-southeast-1'
--
-- -   'ap-southeast-2'
--
-- -   'ap-northeast-1'
--
-- -   'eu-central-1'
--
-- -   'eu-west-1'
--
module Network.AWS.ApplicationAutoScaling
    (
    -- * Service Configuration
      applicationAutoScaling

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** FailedResourceAccessException
    , _FailedResourceAccessException

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

    -- ** DeleteScalingPolicy
    , module Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy

    -- ** PutScalingPolicy
    , module Network.AWS.ApplicationAutoScaling.PutScalingPolicy

    -- ** RegisterScalableTarget
    , module Network.AWS.ApplicationAutoScaling.RegisterScalableTarget

    -- ** DescribeScalingPolicies
    , module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies

    -- ** DescribeScalableTargets
    , module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets

    -- ** DescribeScalingActivities
    , module Network.AWS.ApplicationAutoScaling.DescribeScalingActivities

    -- ** DeregisterScalableTarget
    , module Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget

    -- * Types

    -- ** AdjustmentType
    , AdjustmentType (..)

    -- ** MetricAggregationType
    , MetricAggregationType (..)

    -- ** PolicyType
    , PolicyType (..)

    -- ** ScalableDimension
    , ScalableDimension (..)

    -- ** ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- ** ServiceNamespace
    , ServiceNamespace (..)

    -- ** Alarm
    , Alarm
    , alarm
    , aAlarmName
    , aAlarmARN

    -- ** ScalableTarget
    , ScalableTarget
    , scalableTarget
    , stServiceNamespace
    , stResourceId
    , stScalableDimension
    , stMinCapacity
    , stMaxCapacity
    , stRoleARN
    , stCreationTime

    -- ** ScalingActivity
    , ScalingActivity
    , scalingActivity
    , saStatusMessage
    , saEndTime
    , saDetails
    , saActivityId
    , saServiceNamespace
    , saResourceId
    , saScalableDimension
    , saDescription
    , saCause
    , saStartTime
    , saStatusCode

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spStepScalingPolicyConfiguration
    , spAlarms
    , spPolicyARN
    , spPolicyName
    , spServiceNamespace
    , spResourceId
    , spScalableDimension
    , spPolicyType
    , spCreationTime

    -- ** StepAdjustment
    , StepAdjustment
    , stepAdjustment
    , saMetricIntervalLowerBound
    , saMetricIntervalUpperBound
    , saScalingAdjustment

    -- ** StepScalingPolicyConfiguration
    , StepScalingPolicyConfiguration
    , stepScalingPolicyConfiguration
    , sspcStepAdjustments
    , sspcAdjustmentType
    , sspcCooldown
    , sspcMetricAggregationType
    , sspcMinAdjustmentMagnitude
    ) where

import           Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy
import           Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
import           Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
import           Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
import           Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
import           Network.AWS.ApplicationAutoScaling.PutScalingPolicy
import           Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ApplicationAutoScaling'.
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
