{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon EC2 Auto Scaling__
--
-- Amazon EC2 Auto Scaling is designed to automatically launch or terminate EC2 instances based on user-defined policies, schedules, and health checks. Use this service in conjunction with the AWS Auto Scaling, Amazon CloudWatch, and Elastic Load Balancing services.
--
module Network.AWS.AutoScaling
    (
    -- * Service Configuration
      autoScaling

    -- * Errors
    -- $errors

    -- ** AlreadyExistsFault
    , _AlreadyExistsFault

    -- ** LimitExceededFault
    , _LimitExceededFault

    -- ** ResourceInUseFault
    , _ResourceInUseFault

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** ScalingActivityInProgressFault
    , _ScalingActivityInProgressFault

    -- ** ResourceContentionFault
    , _ResourceContentionFault

    -- ** ServiceLinkedRoleFailure
    , _ServiceLinkedRoleFailure

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeMetricCollectionTypes
    , module Network.AWS.AutoScaling.DescribeMetricCollectionTypes

    -- ** DescribeLoadBalancers
    , module Network.AWS.AutoScaling.DescribeLoadBalancers

    -- ** PutNotificationConfiguration
    , module Network.AWS.AutoScaling.PutNotificationConfiguration

    -- ** DescribeTags (Paginated)
    , module Network.AWS.AutoScaling.DescribeTags

    -- ** DeleteNotificationConfiguration
    , module Network.AWS.AutoScaling.DeleteNotificationConfiguration

    -- ** PutScalingPolicy
    , module Network.AWS.AutoScaling.PutScalingPolicy

    -- ** AttachLoadBalancerTargetGroups
    , module Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups

    -- ** DeleteLaunchConfiguration
    , module Network.AWS.AutoScaling.DeleteLaunchConfiguration

    -- ** EnterStandby
    , module Network.AWS.AutoScaling.EnterStandby

    -- ** SuspendProcesses
    , module Network.AWS.AutoScaling.SuspendProcesses

    -- ** SetInstanceHealth
    , module Network.AWS.AutoScaling.SetInstanceHealth

    -- ** ExitStandby
    , module Network.AWS.AutoScaling.ExitStandby

    -- ** DescribeTerminationPolicyTypes
    , module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes

    -- ** DescribeAutoScalingInstances (Paginated)
    , module Network.AWS.AutoScaling.DescribeAutoScalingInstances

    -- ** RecordLifecycleActionHeartbeat
    , module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat

    -- ** DisableMetricsCollection
    , module Network.AWS.AutoScaling.DisableMetricsCollection

    -- ** DetachInstances
    , module Network.AWS.AutoScaling.DetachInstances

    -- ** EnableMetricsCollection
    , module Network.AWS.AutoScaling.EnableMetricsCollection

    -- ** DescribeScalingProcessTypes
    , module Network.AWS.AutoScaling.DescribeScalingProcessTypes

    -- ** DeleteTags
    , module Network.AWS.AutoScaling.DeleteTags

    -- ** DetachLoadBalancerTargetGroups
    , module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups

    -- ** DescribeLifecycleHooks
    , module Network.AWS.AutoScaling.DescribeLifecycleHooks

    -- ** DescribeAutoScalingGroups (Paginated)
    , module Network.AWS.AutoScaling.DescribeAutoScalingGroups

    -- ** DeleteScheduledAction
    , module Network.AWS.AutoScaling.DeleteScheduledAction

    -- ** SetDesiredCapacity
    , module Network.AWS.AutoScaling.SetDesiredCapacity

    -- ** DetachLoadBalancers
    , module Network.AWS.AutoScaling.DetachLoadBalancers

    -- ** DescribeAutoScalingNotificationTypes
    , module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes

    -- ** DescribeScheduledActions (Paginated)
    , module Network.AWS.AutoScaling.DescribeScheduledActions

    -- ** CreateOrUpdateTags
    , module Network.AWS.AutoScaling.CreateOrUpdateTags

    -- ** CompleteLifecycleAction
    , module Network.AWS.AutoScaling.CompleteLifecycleAction

    -- ** DeletePolicy
    , module Network.AWS.AutoScaling.DeletePolicy

    -- ** AttachInstances
    , module Network.AWS.AutoScaling.AttachInstances

    -- ** UpdateAutoScalingGroup
    , module Network.AWS.AutoScaling.UpdateAutoScalingGroup

    -- ** DeleteAutoScalingGroup
    , module Network.AWS.AutoScaling.DeleteAutoScalingGroup

    -- ** PutLifecycleHook
    , module Network.AWS.AutoScaling.PutLifecycleHook

    -- ** DeleteLifecycleHook
    , module Network.AWS.AutoScaling.DeleteLifecycleHook

    -- ** ResumeProcesses
    , module Network.AWS.AutoScaling.ResumeProcesses

    -- ** ExecutePolicy
    , module Network.AWS.AutoScaling.ExecutePolicy

    -- ** DescribeAccountLimits
    , module Network.AWS.AutoScaling.DescribeAccountLimits

    -- ** AttachLoadBalancers
    , module Network.AWS.AutoScaling.AttachLoadBalancers

    -- ** TerminateInstanceInAutoScalingGroup
    , module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup

    -- ** DescribeLoadBalancerTargetGroups
    , module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups

    -- ** PutScheduledUpdateGroupAction
    , module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction

    -- ** SetInstanceProtection
    , module Network.AWS.AutoScaling.SetInstanceProtection

    -- ** DescribePolicies (Paginated)
    , module Network.AWS.AutoScaling.DescribePolicies

    -- ** DescribeLaunchConfigurations (Paginated)
    , module Network.AWS.AutoScaling.DescribeLaunchConfigurations

    -- ** DescribeScalingActivities (Paginated)
    , module Network.AWS.AutoScaling.DescribeScalingActivities

    -- ** DescribeNotificationConfigurations (Paginated)
    , module Network.AWS.AutoScaling.DescribeNotificationConfigurations

    -- ** DescribeLifecycleHookTypes
    , module Network.AWS.AutoScaling.DescribeLifecycleHookTypes

    -- ** DescribeAdjustmentTypes
    , module Network.AWS.AutoScaling.DescribeAdjustmentTypes

    -- ** CreateAutoScalingGroup
    , module Network.AWS.AutoScaling.CreateAutoScalingGroup

    -- ** CreateLaunchConfiguration
    , module Network.AWS.AutoScaling.CreateLaunchConfiguration

    -- * Types

    -- ** LifecycleState
    , LifecycleState (..)

    -- ** MetricStatistic
    , MetricStatistic (..)

    -- ** MetricType
    , MetricType (..)

    -- ** ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- ** Activity
    , Activity
    , activity
    , aProgress
    , aStatusMessage
    , aEndTime
    , aDetails
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
    , asgServiceLinkedRoleARN
    , asgNewInstancesProtectedFromScaleIn
    , asgVPCZoneIdentifier
    , asgTargetGroupARNs
    , asgEnabledMetrics
    , asgLaunchConfigurationName
    , asgInstances
    , asgLaunchTemplate
    , asgAutoScalingGroupARN
    , asgPlacementGroup
    , asgSuspendedProcesses
    , asgLoadBalancerNames
    , asgTags
    , asgAutoScalingGroupName
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
    , asidLaunchConfigurationName
    , asidLaunchTemplate
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidProtectedFromScaleIn

    -- ** BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- ** CustomizedMetricSpecification
    , CustomizedMetricSpecification
    , customizedMetricSpecification
    , cmsDimensions
    , cmsUnit
    , cmsMetricName
    , cmsNamespace
    , cmsStatistic

    -- ** EBS
    , EBS
    , ebs
    , ebsDeleteOnTermination
    , ebsVolumeSize
    , ebsIOPS
    , ebsEncrypted
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
    , iLaunchConfigurationName
    , iLaunchTemplate
    , iInstanceId
    , iAvailabilityZone
    , iLifecycleState
    , iHealthStatus
    , iProtectedFromScaleIn

    -- ** InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imEnabled

    -- ** LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcAssociatePublicIPAddress
    , lcSecurityGroups
    , lcSpotPrice
    , lcInstanceMonitoring
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

    -- ** LaunchTemplateSpecification
    , LaunchTemplateSpecification
    , launchTemplateSpecification
    , ltsLaunchTemplateName
    , ltsLaunchTemplateId
    , ltsVersion

    -- ** LifecycleHook
    , LifecycleHook
    , lifecycleHook
    , lhDefaultResult
    , lhLifecycleHookName
    , lhHeartbeatTimeout
    , lhAutoScalingGroupName
    , lhNotificationMetadata
    , lhGlobalTimeout
    , lhNotificationTargetARN
    , lhLifecycleTransition
    , lhRoleARN

    -- ** LifecycleHookSpecification
    , LifecycleHookSpecification
    , lifecycleHookSpecification
    , lhsDefaultResult
    , lhsHeartbeatTimeout
    , lhsNotificationMetadata
    , lhsNotificationTargetARN
    , lhsRoleARN
    , lhsLifecycleHookName
    , lhsLifecycleTransition

    -- ** LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsState
    , lbsLoadBalancerName

    -- ** LoadBalancerTargetGroupState
    , LoadBalancerTargetGroupState
    , loadBalancerTargetGroupState
    , lbtgsState
    , lbtgsLoadBalancerTargetGroupARN

    -- ** MetricCollectionType
    , MetricCollectionType
    , metricCollectionType
    , mctMetric

    -- ** MetricDimension
    , MetricDimension
    , metricDimension
    , mdName
    , mdValue

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

    -- ** PredefinedMetricSpecification
    , PredefinedMetricSpecification
    , predefinedMetricSpecification
    , pmsResourceLabel
    , pmsPredefinedMetricType

    -- ** ProcessType
    , ProcessType
    , processType
    , ptProcessName

    -- ** ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , sMinAdjustmentStep
    , sEstimatedInstanceWarmup
    , sPolicyName
    , sPolicyType
    , sStepAdjustments
    , sTargetTrackingConfiguration
    , sAdjustmentType
    , sAutoScalingGroupName
    , sScalingAdjustment
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
    , sugaStartTime
    , sugaTime
    , sugaScheduledActionName
    , sugaMaxSize
    , sugaRecurrence
    , sugaDesiredCapacity
    , sugaMinSize
    , sugaAutoScalingGroupName
    , sugaEndTime

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

    -- ** TargetTrackingConfiguration
    , TargetTrackingConfiguration
    , targetTrackingConfiguration
    , ttcPredefinedMetricSpecification
    , ttcCustomizedMetricSpecification
    , ttcDisableScaleIn
    , ttcTargetValue
    ) where

import Network.AWS.AutoScaling.AttachInstances
import Network.AWS.AutoScaling.AttachLoadBalancers
import Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
import Network.AWS.AutoScaling.CompleteLifecycleAction
import Network.AWS.AutoScaling.CreateAutoScalingGroup
import Network.AWS.AutoScaling.CreateLaunchConfiguration
import Network.AWS.AutoScaling.CreateOrUpdateTags
import Network.AWS.AutoScaling.DeleteAutoScalingGroup
import Network.AWS.AutoScaling.DeleteLaunchConfiguration
import Network.AWS.AutoScaling.DeleteLifecycleHook
import Network.AWS.AutoScaling.DeleteNotificationConfiguration
import Network.AWS.AutoScaling.DeletePolicy
import Network.AWS.AutoScaling.DeleteScheduledAction
import Network.AWS.AutoScaling.DeleteTags
import Network.AWS.AutoScaling.DescribeAccountLimits
import Network.AWS.AutoScaling.DescribeAdjustmentTypes
import Network.AWS.AutoScaling.DescribeAutoScalingGroups
import Network.AWS.AutoScaling.DescribeAutoScalingInstances
import Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
import Network.AWS.AutoScaling.DescribeLaunchConfigurations
import Network.AWS.AutoScaling.DescribeLifecycleHooks
import Network.AWS.AutoScaling.DescribeLifecycleHookTypes
import Network.AWS.AutoScaling.DescribeLoadBalancers
import Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
import Network.AWS.AutoScaling.DescribeMetricCollectionTypes
import Network.AWS.AutoScaling.DescribeNotificationConfigurations
import Network.AWS.AutoScaling.DescribePolicies
import Network.AWS.AutoScaling.DescribeScalingActivities
import Network.AWS.AutoScaling.DescribeScalingProcessTypes
import Network.AWS.AutoScaling.DescribeScheduledActions
import Network.AWS.AutoScaling.DescribeTags
import Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
import Network.AWS.AutoScaling.DetachInstances
import Network.AWS.AutoScaling.DetachLoadBalancers
import Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
import Network.AWS.AutoScaling.DisableMetricsCollection
import Network.AWS.AutoScaling.EnableMetricsCollection
import Network.AWS.AutoScaling.EnterStandby
import Network.AWS.AutoScaling.ExecutePolicy
import Network.AWS.AutoScaling.ExitStandby
import Network.AWS.AutoScaling.PutLifecycleHook
import Network.AWS.AutoScaling.PutNotificationConfiguration
import Network.AWS.AutoScaling.PutScalingPolicy
import Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
import Network.AWS.AutoScaling.ResumeProcesses
import Network.AWS.AutoScaling.SetDesiredCapacity
import Network.AWS.AutoScaling.SetInstanceHealth
import Network.AWS.AutoScaling.SetInstanceProtection
import Network.AWS.AutoScaling.SuspendProcesses
import Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.UpdateAutoScalingGroup
import Network.AWS.AutoScaling.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AutoScaling'.
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
