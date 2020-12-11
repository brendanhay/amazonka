{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    applicationAutoScalingService,

    -- * Errors
    -- $errors

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

    -- ** AdjustmentType
    AdjustmentType (..),

    -- ** MetricAggregationType
    MetricAggregationType (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** MetricType
    MetricType (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** ScalableDimension
    ScalableDimension (..),

    -- ** ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- ** ServiceNamespace
    ServiceNamespace (..),

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- ** CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- ** PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsResourceLabel,
    pmsPredefinedMetricType,

    -- ** ScalableTarget
    ScalableTarget (..),
    mkScalableTarget,
    stSuspendedState,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,

    -- ** ScalableTargetAction
    ScalableTargetAction (..),
    mkScalableTargetAction,
    staMaxCapacity,
    staMinCapacity,

    -- ** ScalingActivity
    ScalingActivity (..),
    mkScalingActivity,
    sStatusMessage,
    sEndTime,
    sDetails,
    sActivityId,
    sServiceNamespace,
    sResourceId,
    sScalableDimension,
    sDescription,
    sCause,
    sStartTime,
    sStatusCode,

    -- ** ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spTargetTrackingScalingPolicyConfiguration,
    spStepScalingPolicyConfiguration,
    spAlarms,
    spPolicyARN,
    spPolicyName,
    spServiceNamespace,
    spResourceId,
    spScalableDimension,
    spPolicyType,
    spCreationTime,

    -- ** ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saScalableDimension,
    saStartTime,
    saEndTime,
    saScalableTargetAction,
    saScheduledActionName,
    saScheduledActionARN,
    saServiceNamespace,
    saSchedule,
    saResourceId,
    saCreationTime,

    -- ** StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- ** StepScalingPolicyConfiguration
    StepScalingPolicyConfiguration (..),
    mkStepScalingPolicyConfiguration,
    sspcStepAdjustments,
    sspcAdjustmentType,
    sspcCooldown,
    sspcMetricAggregationType,
    sspcMinAdjustmentMagnitude,

    -- ** SuspendedState
    SuspendedState (..),
    mkSuspendedState,
    ssDynamicScalingInSuspended,
    ssScheduledScalingSuspended,
    ssDynamicScalingOutSuspended,

    -- ** TargetTrackingScalingPolicyConfiguration
    TargetTrackingScalingPolicyConfiguration (..),
    mkTargetTrackingScalingPolicyConfiguration,
    ttspcPredefinedMetricSpecification,
    ttspcScaleInCooldown,
    ttspcCustomizedMetricSpecification,
    ttspcDisableScaleIn,
    ttspcScaleOutCooldown,
    ttspcTargetValue,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
