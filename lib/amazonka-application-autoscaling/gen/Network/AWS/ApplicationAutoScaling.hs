{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- With Application Auto Scaling, you can configure automatic scaling for the following resources:
--
--
--     * Amazon ECS services
--
--
--     * Amazon EC2 Spot Fleet requests
--
--
--     * Amazon EMR clusters
--
--
--     * Amazon AppStream 2.0 fleets
--
--
--     * Amazon DynamoDB tables and global secondary indexes throughput capacity
--
--
--     * Amazon Aurora Replicas
--
--
--     * Amazon SageMaker endpoint variants
--
--
--     * Custom resources provided by your own applications or services
--
--
--     * Amazon Comprehend document classification and entity recognizer endpoints
--
--
--     * AWS Lambda function provisioned concurrency
--
--
--     * Amazon Keyspaces (for Apache Cassandra) tables
--
--
--     * Amazon Managed Streaming for Apache Kafka cluster storage
--
--
-- __API Summary__
-- The Application Auto Scaling service API includes three key sets of actions:
--
--     * Register and manage scalable targets - Register AWS or custom resources as scalable targets (a resource that Application Auto Scaling can scale), set minimum and maximum capacity limits, and retrieve information on existing scalable targets.
--
--
--     * Configure and manage automatic scaling - Define scaling policies to dynamically scale your resources in response to CloudWatch alarms, schedule one-time or recurring scaling actions, and retrieve your recent scaling activity history.
--
--
--     * Suspend and resume scaling - Temporarily suspend and later resume automatic scaling by calling the <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_RegisterScalableTarget.html RegisterScalableTarget> API action for any Application Auto Scaling scalable target. You can suspend and resume (individually or in combination) scale-out activities that are triggered by a scaling policy, scale-in activities that are triggered by a scaling policy, and scheduled scaling.
--
--
-- To learn more about Application Auto Scaling, including information about granting IAM users required permissions for Application Auto Scaling actions, see the <https://docs.aws.amazon.com/autoscaling/application/userguide/what-is-application-auto-scaling.html Application Auto Scaling User Guide> .
module Network.AWS.ApplicationAutoScaling
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** FailedResourceAccessException
    _FailedResourceAccessException,

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

    -- ** DeleteScalingPolicy
    module Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy,

    -- ** PutScalingPolicy
    module Network.AWS.ApplicationAutoScaling.PutScalingPolicy,

    -- ** RegisterScalableTarget
    module Network.AWS.ApplicationAutoScaling.RegisterScalableTarget,

    -- ** DescribeScalingPolicies (Paginated)
    module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies,

    -- ** PutScheduledAction
    module Network.AWS.ApplicationAutoScaling.PutScheduledAction,

    -- ** DeleteScheduledAction
    module Network.AWS.ApplicationAutoScaling.DeleteScheduledAction,

    -- ** DescribeScheduledActions (Paginated)
    module Network.AWS.ApplicationAutoScaling.DescribeScheduledActions,

    -- ** DescribeScalableTargets (Paginated)
    module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets,

    -- ** DescribeScalingActivities (Paginated)
    module Network.AWS.ApplicationAutoScaling.DescribeScalingActivities,

    -- ** DeregisterScalableTarget
    module Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget,

    -- * Types

    -- ** XmlString
    XmlString (..),

    -- ** MetricType
    MetricType (..),

    -- ** ScalableDimension
    ScalableDimension (..),

    -- ** PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsPredefinedMetricType,
    pmsResourceLabel,

    -- ** ResourceId
    ResourceId (..),

    -- ** PolicyName
    PolicyName (..),

    -- ** ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spPolicyARN,
    spPolicyName,
    spServiceNamespace,
    spResourceId,
    spScalableDimension,
    spPolicyType,
    spCreationTime,
    spAlarms,
    spStepScalingPolicyConfiguration,
    spTargetTrackingScalingPolicyConfiguration,

    -- ** CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,
    cmsDimensions,
    cmsUnit,

    -- ** ScheduledActionName
    ScheduledActionName (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** ScalingActivity
    ScalingActivity (..),
    mkScalingActivity,
    sActivityId,
    sServiceNamespace,
    sResourceId,
    sScalableDimension,
    sDescription,
    sCause,
    sStartTime,
    sStatusCode,
    sDetails,
    sEndTime,
    sStatusMessage,

    -- ** MetricDimensionName
    MetricDimensionName (..),

    -- ** MetricName
    MetricName (..),

    -- ** ResourceIdMaxLen1600
    ResourceIdMaxLen1600 (..),

    -- ** TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    mkTargetTrackingScalingPolicyConfiguration,
    ttspcTargetValue,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcScaleOutCooldown,

    -- ** ServiceNamespace
    ServiceNamespace (..),

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- ** SuspendedState
    SuspendedState (..),
    mkSuspendedState,
    ssDynamicScalingInSuspended,
    ssDynamicScalingOutSuspended,
    ssScheduledScalingSuspended,

    -- ** ResourceLabel
    ResourceLabel (..),

    -- ** ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saScheduledActionName,
    saScheduledActionARN,
    saServiceNamespace,
    saSchedule,
    saResourceId,
    saCreationTime,
    saEndTime,
    saScalableDimension,
    saScalableTargetAction,
    saStartTime,

    -- ** StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    mkStepScalingPolicyConfiguration,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,
    sspcStepAdjustments,

    -- ** AdjustmentType
    AdjustmentType (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saScalingAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,

    -- ** MetricAggregationType
    MetricAggregationType (..),

    -- ** ScalableTargetAction
    ScalableTargetAction (..),
    mkScalableTargetAction,
    staMaxCapacity,
    staMinCapacity,

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- ** ScalableTarget
    ScalableTarget (..),
    mkScalableTarget,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,
    stSuspendedState,

    -- ** ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- ** Schedule
    Schedule (..),

    -- ** PolicyARN
    PolicyARN (..),

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
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ApplicationAutoScaling'.

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
