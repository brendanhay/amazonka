{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- With Application Auto Scaling, you can configure automatic scaling for your scalable AWS resources. You can use Application Auto Scaling to accomplish the following tasks:
--
--
--     * Define scaling policies to automatically scale your AWS resources
--
--     * Scale your resources in response to CloudWatch alarms
--
--     * Schedule one-time or recurring scaling actions
--
--     * View the history of your scaling events
--
--
--
-- Application Auto Scaling can scale the following AWS resources:
--
--     * Amazon ECS services. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-auto-scaling.html Service Auto Scaling> in the /Amazon Elastic Container Service Developer Guide/ .
--
--     * Amazon EC2 Spot fleets. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/fleet-auto-scaling.html Automatic Scaling for Spot Fleet> in the /Amazon EC2 User Guide/ .
--
--     * Amazon EMR clusters. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/ManagementGuide/emr-automatic-scaling.html Using Automatic Scaling in Amazon EMR> in the /Amazon EMR Management Guide/ .
--
--     * AppStream 2.0 fleets. For more information, see <http://docs.aws.amazon.com/appstream2/latest/developerguide/autoscaling.html Fleet Auto Scaling for Amazon AppStream 2.0> in the /Amazon AppStream 2.0 Developer Guide/ .
--
--     * Provisioned read and write capacity for Amazon DynamoDB tables and global secondary indexes. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/AutoScaling.html Managing Throughput Capacity Automatically with DynamoDB Auto Scaling> in the /Amazon DynamoDB Developer Guide/ .
--
--     * Amazon Aurora Replicas. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Integrating.AutoScaling.html Using Amazon Aurora Auto Scaling with Aurora Replicas> .
--
--     * Amazon SageMaker endpoints. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/endpoint-auto-scaling.html Automatically Scaling Amazon SageMaker Models> .
--
--
--
-- To configure automatic scaling for multiple resources across multiple services, use AWS Auto Scaling to create a scaling plan for your application. For more information, see <http://aws.amazon.com/autoscaling AWS Auto Scaling> .
--
-- For a list of supported regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#as-app_region AWS Regions and Endpoints: Application Auto Scaling> in the /AWS General Reference/ .
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

    -- ** DescribeScalingPolicies (Paginated)
    , module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies

    -- ** PutScheduledAction
    , module Network.AWS.ApplicationAutoScaling.PutScheduledAction

    -- ** DeleteScheduledAction
    , module Network.AWS.ApplicationAutoScaling.DeleteScheduledAction

    -- ** DescribeScheduledActions
    , module Network.AWS.ApplicationAutoScaling.DescribeScheduledActions

    -- ** DescribeScalableTargets (Paginated)
    , module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets

    -- ** DescribeScalingActivities (Paginated)
    , module Network.AWS.ApplicationAutoScaling.DescribeScalingActivities

    -- ** DeregisterScalableTarget
    , module Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget

    -- * Types

    -- ** AdjustmentType
    , AdjustmentType (..)

    -- ** MetricAggregationType
    , MetricAggregationType (..)

    -- ** MetricStatistic
    , MetricStatistic (..)

    -- ** MetricType
    , MetricType (..)

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

    -- ** CustomizedMetricSpecification
    , CustomizedMetricSpecification
    , customizedMetricSpecification
    , cmsDimensions
    , cmsUnit
    , cmsMetricName
    , cmsNamespace
    , cmsStatistic

    -- ** MetricDimension
    , MetricDimension
    , metricDimension
    , mdName
    , mdValue

    -- ** PredefinedMetricSpecification
    , PredefinedMetricSpecification
    , predefinedMetricSpecification
    , pmsResourceLabel
    , pmsPredefinedMetricType

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

    -- ** ScalableTargetAction
    , ScalableTargetAction
    , scalableTargetAction
    , staMaxCapacity
    , staMinCapacity

    -- ** ScalingActivity
    , ScalingActivity
    , scalingActivity
    , sStatusMessage
    , sEndTime
    , sDetails
    , sActivityId
    , sServiceNamespace
    , sResourceId
    , sScalableDimension
    , sDescription
    , sCause
    , sStartTime
    , sStatusCode

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , spTargetTrackingScalingPolicyConfiguration
    , spStepScalingPolicyConfiguration
    , spAlarms
    , spPolicyARN
    , spPolicyName
    , spServiceNamespace
    , spResourceId
    , spScalableDimension
    , spPolicyType
    , spCreationTime

    -- ** ScheduledAction
    , ScheduledAction
    , scheduledAction
    , saScalableDimension
    , saStartTime
    , saEndTime
    , saScalableTargetAction
    , saScheduledActionName
    , saScheduledActionARN
    , saServiceNamespace
    , saSchedule
    , saResourceId
    , saCreationTime

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

    -- ** TargetTrackingScalingPolicyConfiguration
    , TargetTrackingScalingPolicyConfiguration
    , targetTrackingScalingPolicyConfiguration
    , ttspcPredefinedMetricSpecification
    , ttspcScaleInCooldown
    , ttspcCustomizedMetricSpecification
    , ttspcDisableScaleIn
    , ttspcScaleOutCooldown
    , ttspcTargetValue
    ) where

import Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy
import Network.AWS.ApplicationAutoScaling.DeleteScheduledAction
import Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
import Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
import Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
import Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
import Network.AWS.ApplicationAutoScaling.DescribeScheduledActions
import Network.AWS.ApplicationAutoScaling.PutScalingPolicy
import Network.AWS.ApplicationAutoScaling.PutScheduledAction
import Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Waiters

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
