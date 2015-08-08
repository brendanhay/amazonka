{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Auto Scaling
--
-- Auto Scaling is designed to automatically launch or terminate EC2
-- instances based on user-defined policies, schedules, and health checks.
-- Use this service in conjunction with the Amazon CloudWatch and Elastic
-- Load Balancing services.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.AutoScaling
    (
    -- * Service Description
      AutoScaling

    -- * Error Matchers
    -- $errors
    , _LimitExceededFault
    , _AlreadyExistsFault
    , _ResourceInUseFault
    , _InvalidNextToken
    , _ScalingActivityInProgressFault
    , _ResourceContentionFault

    -- * Operations
    -- $operations

    -- ** DescribeMetricCollectionTypes
    , module Network.AWS.AutoScaling.DescribeMetricCollectionTypes

    -- ** PutScalingPolicy
    , module Network.AWS.AutoScaling.PutScalingPolicy

    -- ** DeleteNotificationConfiguration
    , module Network.AWS.AutoScaling.DeleteNotificationConfiguration

    -- ** DescribeTags (Paginated)
    , module Network.AWS.AutoScaling.DescribeTags
    -- $pager

    -- ** DeleteLaunchConfiguration
    , module Network.AWS.AutoScaling.DeleteLaunchConfiguration

    -- ** DescribeLoadBalancers
    , module Network.AWS.AutoScaling.DescribeLoadBalancers

    -- ** PutNotificationConfiguration
    , module Network.AWS.AutoScaling.PutNotificationConfiguration

    -- ** SetInstanceHealth
    , module Network.AWS.AutoScaling.SetInstanceHealth

    -- ** EnterStandby
    , module Network.AWS.AutoScaling.EnterStandby

    -- ** SuspendProcesses
    , module Network.AWS.AutoScaling.SuspendProcesses

    -- ** ExitStandby
    , module Network.AWS.AutoScaling.ExitStandby

    -- ** DescribeTerminationPolicyTypes
    , module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes

    -- ** DescribeAutoScalingInstances (Paginated)
    , module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    -- $pager

    -- ** DetachInstances
    , module Network.AWS.AutoScaling.DetachInstances

    -- ** DisableMetricsCollection
    , module Network.AWS.AutoScaling.DisableMetricsCollection

    -- ** RecordLifecycleActionHeartbeat
    , module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat

    -- ** DeleteTags
    , module Network.AWS.AutoScaling.DeleteTags

    -- ** DescribeScalingProcessTypes
    , module Network.AWS.AutoScaling.DescribeScalingProcessTypes

    -- ** EnableMetricsCollection
    , module Network.AWS.AutoScaling.EnableMetricsCollection

    -- ** DescribeLifecycleHooks
    , module Network.AWS.AutoScaling.DescribeLifecycleHooks

    -- ** DescribeAutoScalingGroups (Paginated)
    , module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    -- $pager

    -- ** SetDesiredCapacity
    , module Network.AWS.AutoScaling.SetDesiredCapacity

    -- ** DetachLoadBalancers
    , module Network.AWS.AutoScaling.DetachLoadBalancers

    -- ** DeleteScheduledAction
    , module Network.AWS.AutoScaling.DeleteScheduledAction

    -- ** CreateOrUpdateTags
    , module Network.AWS.AutoScaling.CreateOrUpdateTags

    -- ** DeletePolicy
    , module Network.AWS.AutoScaling.DeletePolicy

    -- ** DescribeAutoScalingNotificationTypes
    , module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes

    -- ** CompleteLifecycleAction
    , module Network.AWS.AutoScaling.CompleteLifecycleAction

    -- ** AttachInstances
    , module Network.AWS.AutoScaling.AttachInstances

    -- ** DescribeScheduledActions (Paginated)
    , module Network.AWS.AutoScaling.DescribeScheduledActions
    -- $pager

    -- ** DeleteAutoScalingGroup
    , module Network.AWS.AutoScaling.DeleteAutoScalingGroup

    -- ** PutLifecycleHook
    , module Network.AWS.AutoScaling.PutLifecycleHook

    -- ** UpdateAutoScalingGroup
    , module Network.AWS.AutoScaling.UpdateAutoScalingGroup

    -- ** DeleteLifecycleHook
    , module Network.AWS.AutoScaling.DeleteLifecycleHook

    -- ** ResumeProcesses
    , module Network.AWS.AutoScaling.ResumeProcesses

    -- ** ExecutePolicy
    , module Network.AWS.AutoScaling.ExecutePolicy

    -- ** TerminateInstanceInAutoScalingGroup
    , module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup

    -- ** DescribeAccountLimits
    , module Network.AWS.AutoScaling.DescribeAccountLimits

    -- ** AttachLoadBalancers
    , module Network.AWS.AutoScaling.AttachLoadBalancers

    -- ** PutScheduledUpdateGroupAction
    , module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction

    -- ** DescribePolicies (Paginated)
    , module Network.AWS.AutoScaling.DescribePolicies
    -- $pager

    -- ** DescribeNotificationConfigurations (Paginated)
    , module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    -- $pager

    -- ** DescribeLaunchConfigurations (Paginated)
    , module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    -- $pager

    -- ** DescribeLifecycleHookTypes
    , module Network.AWS.AutoScaling.DescribeLifecycleHookTypes

    -- ** DescribeScalingActivities (Paginated)
    , module Network.AWS.AutoScaling.DescribeScalingActivities
    -- $pager

    -- ** CreateAutoScalingGroup
    , module Network.AWS.AutoScaling.CreateAutoScalingGroup

    -- ** CreateLaunchConfiguration
    , module Network.AWS.AutoScaling.CreateLaunchConfiguration

    -- ** DescribeAdjustmentTypes
    , module Network.AWS.AutoScaling.DescribeAdjustmentTypes

    -- * Types

    -- ** LifecycleState
    , LifecycleState (..)

    -- ** ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- ** Activity
    , Activity
    , activity
    , aProgress
    , aStatusMessage
    , aDetails
    , aEndTime
    , aDescription
    , aActivityId
    , aAutoScalingGroupName
    , aCause
    , aStartTime
    , aStatusCode

    -- ** AdjustmentType
    , AdjustmentType
    , adjustmentType
    , atAdjustmentType

    -- ** Alarm
    , Alarm
    , alarm
    , aAlarmName
    , aAlarmARN

    -- ** AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgStatus
    , asgTerminationPolicies
    , asgHealthCheckGracePeriod
    , asgVPCZoneIdentifier
    , asgEnabledMetrics
    , asgInstances
    , asgAutoScalingGroupARN
    , asgSuspendedProcesses
    , asgPlacementGroup
    , asgLoadBalancerNames
    , asgTags
    , asgAutoScalingGroupName
    , asgLaunchConfigurationName
    , asgMinSize
    , asgMaxSize
    , asgDesiredCapacity
    , asgDefaultCooldown
    , asgAvailabilityZones
    , asgHealthCheckType
    , asgCreatedTime

    -- ** AutoScalingInstanceDetails
    , AutoScalingInstanceDetails
    , autoScalingInstanceDetails
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidLaunchConfigurationName

    -- ** BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- ** EBS
    , EBS
    , ebs
    , ebsDeleteOnTermination
    , ebsVolumeSize
    , ebsIOPS
    , ebsVolumeType
    , ebsSnapshotId

    -- ** EnabledMetric
    , EnabledMetric
    , enabledMetric
    , emGranularity
    , emMetric

    -- ** Filter
    , Filter
    , filter'
    , fValues
    , fName

    -- ** Instance
    , Instance
    , instance'
    , iInstanceId
    , iAvailabilityZone
    , iLifecycleState
    , iHealthStatus
    , iLaunchConfigurationName

    -- ** InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imEnabled

    -- ** LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcSecurityGroups
    , lcAssociatePublicIPAddress
    , lcInstanceMonitoring
    , lcSpotPrice
    , lcKeyName
    , lcClassicLinkVPCSecurityGroups
    , lcRAMDiskId
    , lcKernelId
    , lcEBSOptimized
    , lcUserData
    , lcClassicLinkVPCId
    , lcIAMInstanceProfile
    , lcLaunchConfigurationARN
    , lcPlacementTenancy
    , lcBlockDeviceMappings
    , lcLaunchConfigurationName
    , lcImageId
    , lcInstanceType
    , lcCreatedTime

    -- ** LifecycleHook
    , LifecycleHook
    , lifecycleHook
    , lhDefaultResult
    , lhLifecycleHookName
    , lhHeartbeatTimeout
    , lhAutoScalingGroupName
    , lhNotificationMetadata
    , lhGlobalTimeout
    , lhRoleARN
    , lhLifecycleTransition
    , lhNotificationTargetARN

    -- ** LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsState
    , lbsLoadBalancerName

    -- ** MetricCollectionType
    , MetricCollectionType
    , metricCollectionType
    , mctMetric

    -- ** MetricGranularityType
    , MetricGranularityType
    , metricGranularityType
    , mgtGranularity

    -- ** NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicARN
    , ncAutoScalingGroupName
    , ncNotificationType

    -- ** ProcessType
    , ProcessType
    , processType
    , ptProcessName

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , sEstimatedInstanceWarmup
    , sMinAdjustmentStep
    , sPolicyName
    , sPolicyType
    , sStepAdjustments
    , sAdjustmentType
    , sScalingAdjustment
    , sAutoScalingGroupName
    , sCooldown
    , sPolicyARN
    , sAlarms
    , sMetricAggregationType
    , sMinAdjustmentMagnitude

    -- ** ScalingProcessQuery
    , ScalingProcessQuery
    , scalingProcessQuery
    , spqScalingProcesses
    , spqAutoScalingGroupName

    -- ** ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction
    , scheduledUpdateGroupAction
    , sugaScheduledActionARN
    , sugaTime
    , sugaStartTime
    , sugaScheduledActionName
    , sugaMaxSize
    , sugaDesiredCapacity
    , sugaRecurrence
    , sugaMinSize
    , sugaEndTime
    , sugaAutoScalingGroupName

    -- ** StepAdjustment
    , StepAdjustment
    , stepAdjustment
    , saMetricIntervalLowerBound
    , saMetricIntervalUpperBound
    , saScalingAdjustment

    -- ** SuspendedProcess
    , SuspendedProcess
    , suspendedProcess
    , spProcessName
    , spSuspensionReason

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagResourceId
    , tagResourceType
    , tagPropagateAtLaunch
    , tagValue

    -- ** TagDescription
    , TagDescription
    , tagDescription
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdPropagateAtLaunch
    , tdValue
    ) where

import           Network.AWS.AutoScaling.AttachInstances
import           Network.AWS.AutoScaling.AttachLoadBalancers
import           Network.AWS.AutoScaling.CompleteLifecycleAction
import           Network.AWS.AutoScaling.CreateAutoScalingGroup
import           Network.AWS.AutoScaling.CreateLaunchConfiguration
import           Network.AWS.AutoScaling.CreateOrUpdateTags
import           Network.AWS.AutoScaling.DeleteAutoScalingGroup
import           Network.AWS.AutoScaling.DeleteLaunchConfiguration
import           Network.AWS.AutoScaling.DeleteLifecycleHook
import           Network.AWS.AutoScaling.DeleteNotificationConfiguration
import           Network.AWS.AutoScaling.DeletePolicy
import           Network.AWS.AutoScaling.DeleteScheduledAction
import           Network.AWS.AutoScaling.DeleteTags
import           Network.AWS.AutoScaling.DescribeAccountLimits
import           Network.AWS.AutoScaling.DescribeAdjustmentTypes
import           Network.AWS.AutoScaling.DescribeAutoScalingGroups
import           Network.AWS.AutoScaling.DescribeAutoScalingInstances
import           Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
import           Network.AWS.AutoScaling.DescribeLaunchConfigurations
import           Network.AWS.AutoScaling.DescribeLifecycleHooks
import           Network.AWS.AutoScaling.DescribeLifecycleHookTypes
import           Network.AWS.AutoScaling.DescribeLoadBalancers
import           Network.AWS.AutoScaling.DescribeMetricCollectionTypes
import           Network.AWS.AutoScaling.DescribeNotificationConfigurations
import           Network.AWS.AutoScaling.DescribePolicies
import           Network.AWS.AutoScaling.DescribeScalingActivities
import           Network.AWS.AutoScaling.DescribeScalingProcessTypes
import           Network.AWS.AutoScaling.DescribeScheduledActions
import           Network.AWS.AutoScaling.DescribeTags
import           Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
import           Network.AWS.AutoScaling.DetachInstances
import           Network.AWS.AutoScaling.DetachLoadBalancers
import           Network.AWS.AutoScaling.DisableMetricsCollection
import           Network.AWS.AutoScaling.EnableMetricsCollection
import           Network.AWS.AutoScaling.EnterStandby
import           Network.AWS.AutoScaling.ExecutePolicy
import           Network.AWS.AutoScaling.ExitStandby
import           Network.AWS.AutoScaling.PutLifecycleHook
import           Network.AWS.AutoScaling.PutNotificationConfiguration
import           Network.AWS.AutoScaling.PutScalingPolicy
import           Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
import           Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
import           Network.AWS.AutoScaling.ResumeProcesses
import           Network.AWS.AutoScaling.SetDesiredCapacity
import           Network.AWS.AutoScaling.SetInstanceHealth
import           Network.AWS.AutoScaling.SuspendProcesses
import           Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.UpdateAutoScalingGroup
import           Network.AWS.AutoScaling.Waiters

{- $errors
Error matchers are intended to be used with the <http://hackage.haskell.org/package/lens lens>
library functions provided by the "Control.Exception.Lens" module. This allows
the user to catch (and rethrow) service specific errors returned by 'AutoScaling'.
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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
