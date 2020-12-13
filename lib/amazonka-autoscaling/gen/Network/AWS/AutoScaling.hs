{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon EC2 Auto Scaling__
--
-- Amazon EC2 Auto Scaling is designed to automatically launch or terminate EC2 instances based on user-defined scaling policies, scheduled actions, and health checks. Use this service with AWS Auto Scaling, Amazon CloudWatch, and Elastic Load Balancing.
-- For more information, including information about granting IAM users required permissions for Amazon EC2 Auto Scaling actions, see the <https://docs.aws.amazon.com/autoscaling/ec2/userguide/what-is-amazon-ec2-auto-scaling.html Amazon EC2 Auto Scaling User Guide> .
module Network.AWS.AutoScaling
  ( -- * Service configuration
    autoScalingService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeMetricCollectionTypes
    module Network.AWS.AutoScaling.DescribeMetricCollectionTypes,

    -- ** DescribeLoadBalancers (Paginated)
    module Network.AWS.AutoScaling.DescribeLoadBalancers,

    -- ** PutNotificationConfiguration
    module Network.AWS.AutoScaling.PutNotificationConfiguration,

    -- ** DescribeTags (Paginated)
    module Network.AWS.AutoScaling.DescribeTags,

    -- ** DeleteNotificationConfiguration
    module Network.AWS.AutoScaling.DeleteNotificationConfiguration,

    -- ** PutScalingPolicy
    module Network.AWS.AutoScaling.PutScalingPolicy,

    -- ** StartInstanceRefresh
    module Network.AWS.AutoScaling.StartInstanceRefresh,

    -- ** AttachLoadBalancerTargetGroups
    module Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups,

    -- ** DeleteLaunchConfiguration
    module Network.AWS.AutoScaling.DeleteLaunchConfiguration,

    -- ** EnterStandby
    module Network.AWS.AutoScaling.EnterStandby,

    -- ** SuspendProcesses
    module Network.AWS.AutoScaling.SuspendProcesses,

    -- ** SetInstanceHealth
    module Network.AWS.AutoScaling.SetInstanceHealth,

    -- ** ExitStandby
    module Network.AWS.AutoScaling.ExitStandby,

    -- ** DescribeTerminationPolicyTypes
    module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes,

    -- ** CancelInstanceRefresh
    module Network.AWS.AutoScaling.CancelInstanceRefresh,

    -- ** DescribeAutoScalingInstances (Paginated)
    module Network.AWS.AutoScaling.DescribeAutoScalingInstances,

    -- ** RecordLifecycleActionHeartbeat
    module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat,

    -- ** DisableMetricsCollection
    module Network.AWS.AutoScaling.DisableMetricsCollection,

    -- ** DetachInstances
    module Network.AWS.AutoScaling.DetachInstances,

    -- ** EnableMetricsCollection
    module Network.AWS.AutoScaling.EnableMetricsCollection,

    -- ** DescribeScalingProcessTypes
    module Network.AWS.AutoScaling.DescribeScalingProcessTypes,

    -- ** DeleteTags
    module Network.AWS.AutoScaling.DeleteTags,

    -- ** DetachLoadBalancerTargetGroups
    module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups,

    -- ** DescribeLifecycleHooks
    module Network.AWS.AutoScaling.DescribeLifecycleHooks,

    -- ** DescribeAutoScalingGroups (Paginated)
    module Network.AWS.AutoScaling.DescribeAutoScalingGroups,

    -- ** DeleteScheduledAction
    module Network.AWS.AutoScaling.DeleteScheduledAction,

    -- ** SetDesiredCapacity
    module Network.AWS.AutoScaling.SetDesiredCapacity,

    -- ** DetachLoadBalancers
    module Network.AWS.AutoScaling.DetachLoadBalancers,

    -- ** DescribeAutoScalingNotificationTypes
    module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes,

    -- ** DescribeScheduledActions (Paginated)
    module Network.AWS.AutoScaling.DescribeScheduledActions,

    -- ** CreateOrUpdateTags
    module Network.AWS.AutoScaling.CreateOrUpdateTags,

    -- ** CompleteLifecycleAction
    module Network.AWS.AutoScaling.CompleteLifecycleAction,

    -- ** DeletePolicy
    module Network.AWS.AutoScaling.DeletePolicy,

    -- ** AttachInstances
    module Network.AWS.AutoScaling.AttachInstances,

    -- ** UpdateAutoScalingGroup
    module Network.AWS.AutoScaling.UpdateAutoScalingGroup,

    -- ** DeleteAutoScalingGroup
    module Network.AWS.AutoScaling.DeleteAutoScalingGroup,

    -- ** PutLifecycleHook
    module Network.AWS.AutoScaling.PutLifecycleHook,

    -- ** BatchPutScheduledUpdateGroupAction
    module Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction,

    -- ** DeleteLifecycleHook
    module Network.AWS.AutoScaling.DeleteLifecycleHook,

    -- ** ResumeProcesses
    module Network.AWS.AutoScaling.ResumeProcesses,

    -- ** ExecutePolicy
    module Network.AWS.AutoScaling.ExecutePolicy,

    -- ** DescribeInstanceRefreshes
    module Network.AWS.AutoScaling.DescribeInstanceRefreshes,

    -- ** DescribeAccountLimits
    module Network.AWS.AutoScaling.DescribeAccountLimits,

    -- ** AttachLoadBalancers
    module Network.AWS.AutoScaling.AttachLoadBalancers,

    -- ** BatchDeleteScheduledAction
    module Network.AWS.AutoScaling.BatchDeleteScheduledAction,

    -- ** TerminateInstanceInAutoScalingGroup
    module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup,

    -- ** DescribeLoadBalancerTargetGroups (Paginated)
    module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups,

    -- ** PutScheduledUpdateGroupAction
    module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction,

    -- ** SetInstanceProtection
    module Network.AWS.AutoScaling.SetInstanceProtection,

    -- ** DescribePolicies (Paginated)
    module Network.AWS.AutoScaling.DescribePolicies,

    -- ** DescribeLaunchConfigurations (Paginated)
    module Network.AWS.AutoScaling.DescribeLaunchConfigurations,

    -- ** DescribeScalingActivities (Paginated)
    module Network.AWS.AutoScaling.DescribeScalingActivities,

    -- ** DescribeNotificationConfigurations (Paginated)
    module Network.AWS.AutoScaling.DescribeNotificationConfigurations,

    -- ** DescribeLifecycleHookTypes
    module Network.AWS.AutoScaling.DescribeLifecycleHookTypes,

    -- ** DescribeAdjustmentTypes
    module Network.AWS.AutoScaling.DescribeAdjustmentTypes,

    -- ** CreateAutoScalingGroup
    module Network.AWS.AutoScaling.CreateAutoScalingGroup,

    -- ** CreateLaunchConfiguration
    module Network.AWS.AutoScaling.CreateLaunchConfiguration,

    -- * Types

    -- ** InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- ** InstanceMetadataHTTPTokensState
    InstanceMetadataHTTPTokensState (..),

    -- ** InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- ** LifecycleState
    LifecycleState (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** MetricType
    MetricType (..),

    -- ** RefreshStrategy
    RefreshStrategy (..),

    -- ** ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- ** Activity
    Activity (..),
    mkActivity,
    aProgress,
    aStartTime,
    aActivityId,
    aCause,
    aStatusMessage,
    aAutoScalingGroupName,
    aEndTime,
    aDetails,
    aDescription,
    aStatusCode,

    -- ** AdjustmentType
    AdjustmentType (..),
    mkAdjustmentType,
    atAdjustmentType,

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- ** AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgStatus,
    asgTerminationPolicies,
    asgCreatedTime,
    asgHealthCheckGracePeriod,
    asgServiceLinkedRoleARN,
    asgNewInstancesProtectedFromScaleIn,
    asgVPCZoneIdentifier,
    asgTargetGroupARNs,
    asgMaxInstanceLifetime,
    asgDefaultCooldown,
    asgMaxSize,
    asgAvailabilityZones,
    asgDesiredCapacity,
    asgMixedInstancesPolicy,
    asgMinSize,
    asgEnabledMetrics,
    asgAutoScalingGroupName,
    asgLaunchConfigurationName,
    asgInstances,
    asgHealthCheckType,
    asgLaunchTemplate,
    asgCapacityRebalance,
    asgAutoScalingGroupARN,
    asgPlacementGroup,
    asgSuspendedProcesses,
    asgLoadBalancerNames,
    asgTags,

    -- ** AutoScalingInstanceDetails
    AutoScalingInstanceDetails (..),
    mkAutoScalingInstanceDetails,
    asidInstanceId,
    asidWeightedCapacity,
    asidProtectedFromScaleIn,
    asidInstanceType,
    asidAvailabilityZone,
    asidAutoScalingGroupName,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidHealthStatus,
    asidLifecycleState,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- ** CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsMetricName,
    cmsNamespace,
    cmsDimensions,
    cmsUnit,
    cmsStatistic,

    -- ** EBS
    EBS (..),
    mkEBS,
    ebsDeleteOnTermination,
    ebsVolumeSize,
    ebsIOPS,
    ebsEncrypted,
    ebsVolumeType,
    ebsSnapshotId,

    -- ** EnabledMetric
    EnabledMetric (..),
    mkEnabledMetric,
    emGranularity,
    emMetric,

    -- ** FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (..),
    mkFailedScheduledUpdateGroupActionRequest,
    fsugarScheduledActionName,
    fsugarErrorCode,
    fsugarErrorMessage,

    -- ** Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- ** Instance
    Instance (..),
    mkInstance,
    iInstanceId,
    iWeightedCapacity,
    iProtectedFromScaleIn,
    iInstanceType,
    iAvailabilityZone,
    iLaunchConfigurationName,
    iLaunchTemplate,
    iHealthStatus,
    iLifecycleState,

    -- ** InstanceMetadataOptions
    InstanceMetadataOptions (..),
    mkInstanceMetadataOptions,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,

    -- ** InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imEnabled,

    -- ** InstanceRefresh
    InstanceRefresh (..),
    mkInstanceRefresh,
    irStatus,
    irStartTime,
    irInstancesToUpdate,
    irPercentageComplete,
    irAutoScalingGroupName,
    irEndTime,
    irStatusReason,
    irInstanceRefreshId,

    -- ** InstancesDistribution
    InstancesDistribution (..),
    mkInstancesDistribution,
    idSpotAllocationStrategy,
    idSpotInstancePools,
    idSpotMaxPrice,
    idOnDemandBaseCapacity,
    idOnDemandAllocationStrategy,
    idOnDemandPercentageAboveBaseCapacity,

    -- ** LaunchConfiguration
    LaunchConfiguration (..),
    mkLaunchConfiguration,
    lcAssociatePublicIPAddress,
    lcSecurityGroups,
    lcSpotPrice,
    lcCreatedTime,
    lcInstanceMonitoring,
    lcKeyName,
    lcClassicLinkVPCSecurityGroups,
    lcRAMDiskId,
    lcKernelId,
    lcInstanceType,
    lcEBSOptimized,
    lcUserData,
    lcClassicLinkVPCId,
    lcIAMInstanceProfile,
    lcImageId,
    lcLaunchConfigurationName,
    lcMetadataOptions,
    lcLaunchConfigurationARN,
    lcPlacementTenancy,
    lcBlockDeviceMappings,

    -- ** LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltOverrides,
    ltLaunchTemplateSpecification,

    -- ** LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoWeightedCapacity,
    ltoInstanceType,
    ltoLaunchTemplateSpecification,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- ** LifecycleHook
    LifecycleHook (..),
    mkLifecycleHook,
    lhDefaultResult,
    lhLifecycleHookName,
    lhHeartbeatTimeout,
    lhAutoScalingGroupName,
    lhNotificationMetadata,
    lhGlobalTimeout,
    lhNotificationTargetARN,
    lhLifecycleTransition,
    lhRoleARN,

    -- ** LifecycleHookSpecification
    LifecycleHookSpecification (..),
    mkLifecycleHookSpecification,
    lhsDefaultResult,
    lhsLifecycleHookName,
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsLifecycleTransition,
    lhsRoleARN,

    -- ** LoadBalancerState
    LoadBalancerState (..),
    mkLoadBalancerState,
    lbsState,
    lbsLoadBalancerName,

    -- ** LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState (..),
    mkLoadBalancerTargetGroupState,
    lbtgsState,
    lbtgsLoadBalancerTargetGroupARN,

    -- ** MetricCollectionType
    MetricCollectionType (..),
    mkMetricCollectionType,
    mctMetric,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdName,

    -- ** MetricGranularityType
    MetricGranularityType (..),
    mkMetricGranularityType,
    mgtGranularity,

    -- ** MixedInstancesPolicy
    MixedInstancesPolicy (..),
    mkMixedInstancesPolicy,
    mipLaunchTemplate,
    mipInstancesDistribution,

    -- ** NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicARN,
    ncAutoScalingGroupName,
    ncNotificationType,

    -- ** PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsPredefinedMetricType,
    pmsResourceLabel,

    -- ** ProcessType
    ProcessType (..),
    mkProcessType,
    ptProcessName,

    -- ** RefreshPreferences
    RefreshPreferences (..),
    mkRefreshPreferences,
    rpMinHealthyPercentage,
    rpInstanceWarmup,

    -- ** ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spMinAdjustmentStep,
    spEstimatedInstanceWarmup,
    spPolicyName,
    spEnabled,
    spPolicyType,
    spStepAdjustments,
    spTargetTrackingConfiguration,
    spAdjustmentType,
    spAutoScalingGroupName,
    spScalingAdjustment,
    spCooldown,
    spPolicyARN,
    spAlarms,
    spMetricAggregationType,
    spMinAdjustmentMagnitude,

    -- ** ScalingProcessQuery
    ScalingProcessQuery (..),
    mkScalingProcessQuery,
    spqAutoScalingGroupName,
    spqScalingProcesses,

    -- ** ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (..),
    mkScheduledUpdateGroupAction,
    sugaScheduledActionARN,
    sugaStartTime,
    sugaTime,
    sugaScheduledActionName,
    sugaMaxSize,
    sugaRecurrence,
    sugaDesiredCapacity,
    sugaMinSize,
    sugaAutoScalingGroupName,
    sugaEndTime,

    -- ** ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (..),
    mkScheduledUpdateGroupActionRequest,
    sugarStartTime,
    sugarScheduledActionName,
    sugarMaxSize,
    sugarRecurrence,
    sugarDesiredCapacity,
    sugarMinSize,
    sugarEndTime,

    -- ** StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- ** SuspendedProcess
    SuspendedProcess (..),
    mkSuspendedProcess,
    spProcessName,
    spSuspensionReason,

    -- ** Tag
    Tag (..),
    mkTag,
    tResourceId,
    tResourceType,
    tValue,
    tKey,
    tPropagateAtLaunch,

    -- ** TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceId,
    tdResourceType,
    tdValue,
    tdKey,
    tdPropagateAtLaunch,

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcPredefinedMetricSpecification,
    ttcTargetValue,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.AutoScaling.AttachInstances
import Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
import Network.AWS.AutoScaling.AttachLoadBalancers
import Network.AWS.AutoScaling.BatchDeleteScheduledAction
import Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.CancelInstanceRefresh
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
import Network.AWS.AutoScaling.DescribeInstanceRefreshes
import Network.AWS.AutoScaling.DescribeLaunchConfigurations
import Network.AWS.AutoScaling.DescribeLifecycleHookTypes
import Network.AWS.AutoScaling.DescribeLifecycleHooks
import Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
import Network.AWS.AutoScaling.DescribeLoadBalancers
import Network.AWS.AutoScaling.DescribeMetricCollectionTypes
import Network.AWS.AutoScaling.DescribeNotificationConfigurations
import Network.AWS.AutoScaling.DescribePolicies
import Network.AWS.AutoScaling.DescribeScalingActivities
import Network.AWS.AutoScaling.DescribeScalingProcessTypes
import Network.AWS.AutoScaling.DescribeScheduledActions
import Network.AWS.AutoScaling.DescribeTags
import Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
import Network.AWS.AutoScaling.DetachInstances
import Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
import Network.AWS.AutoScaling.DetachLoadBalancers
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
import Network.AWS.AutoScaling.StartInstanceRefresh
import Network.AWS.AutoScaling.SuspendProcesses
import Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.UpdateAutoScalingGroup
import Network.AWS.AutoScaling.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AutoScaling'.

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
