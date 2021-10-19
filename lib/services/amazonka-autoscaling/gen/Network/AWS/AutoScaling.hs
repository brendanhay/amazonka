{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2011-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EC2 Auto Scaling
--
-- Amazon EC2 Auto Scaling is designed to automatically launch or terminate
-- EC2 instances based on user-defined scaling policies, scheduled actions,
-- and health checks.
--
-- For more information about Amazon EC2 Auto Scaling, see the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/what-is-amazon-ec2-auto-scaling.html Amazon EC2 Auto Scaling User Guide>.
-- For information about granting IAM users required permissions for calls
-- to Amazon EC2 Auto Scaling, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/ec2-auto-scaling-api-permissions.html Granting IAM users required permissions for Amazon EC2 Auto Scaling resources>
-- in the /Amazon EC2 Auto Scaling API Reference/.
module Network.AWS.AutoScaling
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InstanceRefreshInProgressFault
    _InstanceRefreshInProgressFault,

    -- ** AlreadyExistsFault
    _AlreadyExistsFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** ResourceInUseFault
    _ResourceInUseFault,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** ScalingActivityInProgressFault
    _ScalingActivityInProgressFault,

    -- ** ResourceContentionFault
    _ResourceContentionFault,

    -- ** ServiceLinkedRoleFailure
    _ServiceLinkedRoleFailure,

    -- ** ActiveInstanceRefreshNotFoundFault
    _ActiveInstanceRefreshNotFoundFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutWarmPool
    PutWarmPool (PutWarmPool'),
    newPutWarmPool,
    PutWarmPoolResponse (PutWarmPoolResponse'),
    newPutWarmPoolResponse,

    -- ** DescribeMetricCollectionTypes
    DescribeMetricCollectionTypes (DescribeMetricCollectionTypes'),
    newDescribeMetricCollectionTypes,
    DescribeMetricCollectionTypesResponse (DescribeMetricCollectionTypesResponse'),
    newDescribeMetricCollectionTypesResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** PutNotificationConfiguration
    PutNotificationConfiguration (PutNotificationConfiguration'),
    newPutNotificationConfiguration,
    PutNotificationConfigurationResponse (PutNotificationConfigurationResponse'),
    newPutNotificationConfigurationResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DeleteNotificationConfiguration
    DeleteNotificationConfiguration (DeleteNotificationConfiguration'),
    newDeleteNotificationConfiguration,
    DeleteNotificationConfigurationResponse (DeleteNotificationConfigurationResponse'),
    newDeleteNotificationConfigurationResponse,

    -- ** DeleteWarmPool
    DeleteWarmPool (DeleteWarmPool'),
    newDeleteWarmPool,
    DeleteWarmPoolResponse (DeleteWarmPoolResponse'),
    newDeleteWarmPoolResponse,

    -- ** PutScalingPolicy
    PutScalingPolicy (PutScalingPolicy'),
    newPutScalingPolicy,
    PutScalingPolicyResponse (PutScalingPolicyResponse'),
    newPutScalingPolicyResponse,

    -- ** StartInstanceRefresh
    StartInstanceRefresh (StartInstanceRefresh'),
    newStartInstanceRefresh,
    StartInstanceRefreshResponse (StartInstanceRefreshResponse'),
    newStartInstanceRefreshResponse,

    -- ** AttachLoadBalancerTargetGroups
    AttachLoadBalancerTargetGroups (AttachLoadBalancerTargetGroups'),
    newAttachLoadBalancerTargetGroups,
    AttachLoadBalancerTargetGroupsResponse (AttachLoadBalancerTargetGroupsResponse'),
    newAttachLoadBalancerTargetGroupsResponse,

    -- ** DeleteLaunchConfiguration
    DeleteLaunchConfiguration (DeleteLaunchConfiguration'),
    newDeleteLaunchConfiguration,
    DeleteLaunchConfigurationResponse (DeleteLaunchConfigurationResponse'),
    newDeleteLaunchConfigurationResponse,

    -- ** EnterStandby
    EnterStandby (EnterStandby'),
    newEnterStandby,
    EnterStandbyResponse (EnterStandbyResponse'),
    newEnterStandbyResponse,

    -- ** SuspendProcesses
    SuspendProcesses (SuspendProcesses'),
    newSuspendProcesses,
    SuspendProcessesResponse (SuspendProcessesResponse'),
    newSuspendProcessesResponse,

    -- ** SetInstanceHealth
    SetInstanceHealth (SetInstanceHealth'),
    newSetInstanceHealth,
    SetInstanceHealthResponse (SetInstanceHealthResponse'),
    newSetInstanceHealthResponse,

    -- ** ExitStandby
    ExitStandby (ExitStandby'),
    newExitStandby,
    ExitStandbyResponse (ExitStandbyResponse'),
    newExitStandbyResponse,

    -- ** DescribeTerminationPolicyTypes
    DescribeTerminationPolicyTypes (DescribeTerminationPolicyTypes'),
    newDescribeTerminationPolicyTypes,
    DescribeTerminationPolicyTypesResponse (DescribeTerminationPolicyTypesResponse'),
    newDescribeTerminationPolicyTypesResponse,

    -- ** CancelInstanceRefresh
    CancelInstanceRefresh (CancelInstanceRefresh'),
    newCancelInstanceRefresh,
    CancelInstanceRefreshResponse (CancelInstanceRefreshResponse'),
    newCancelInstanceRefreshResponse,

    -- ** DescribeAutoScalingInstances (Paginated)
    DescribeAutoScalingInstances (DescribeAutoScalingInstances'),
    newDescribeAutoScalingInstances,
    DescribeAutoScalingInstancesResponse (DescribeAutoScalingInstancesResponse'),
    newDescribeAutoScalingInstancesResponse,

    -- ** RecordLifecycleActionHeartbeat
    RecordLifecycleActionHeartbeat (RecordLifecycleActionHeartbeat'),
    newRecordLifecycleActionHeartbeat,
    RecordLifecycleActionHeartbeatResponse (RecordLifecycleActionHeartbeatResponse'),
    newRecordLifecycleActionHeartbeatResponse,

    -- ** DisableMetricsCollection
    DisableMetricsCollection (DisableMetricsCollection'),
    newDisableMetricsCollection,
    DisableMetricsCollectionResponse (DisableMetricsCollectionResponse'),
    newDisableMetricsCollectionResponse,

    -- ** DetachInstances
    DetachInstances (DetachInstances'),
    newDetachInstances,
    DetachInstancesResponse (DetachInstancesResponse'),
    newDetachInstancesResponse,

    -- ** EnableMetricsCollection
    EnableMetricsCollection (EnableMetricsCollection'),
    newEnableMetricsCollection,
    EnableMetricsCollectionResponse (EnableMetricsCollectionResponse'),
    newEnableMetricsCollectionResponse,

    -- ** DescribeScalingProcessTypes
    DescribeScalingProcessTypes (DescribeScalingProcessTypes'),
    newDescribeScalingProcessTypes,
    DescribeScalingProcessTypesResponse (DescribeScalingProcessTypesResponse'),
    newDescribeScalingProcessTypesResponse,

    -- ** DescribeWarmPool
    DescribeWarmPool (DescribeWarmPool'),
    newDescribeWarmPool,
    DescribeWarmPoolResponse (DescribeWarmPoolResponse'),
    newDescribeWarmPoolResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DetachLoadBalancerTargetGroups
    DetachLoadBalancerTargetGroups (DetachLoadBalancerTargetGroups'),
    newDetachLoadBalancerTargetGroups,
    DetachLoadBalancerTargetGroupsResponse (DetachLoadBalancerTargetGroupsResponse'),
    newDetachLoadBalancerTargetGroupsResponse,

    -- ** DescribeLifecycleHooks
    DescribeLifecycleHooks (DescribeLifecycleHooks'),
    newDescribeLifecycleHooks,
    DescribeLifecycleHooksResponse (DescribeLifecycleHooksResponse'),
    newDescribeLifecycleHooksResponse,

    -- ** DescribeAutoScalingGroups (Paginated)
    DescribeAutoScalingGroups (DescribeAutoScalingGroups'),
    newDescribeAutoScalingGroups,
    DescribeAutoScalingGroupsResponse (DescribeAutoScalingGroupsResponse'),
    newDescribeAutoScalingGroupsResponse,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** SetDesiredCapacity
    SetDesiredCapacity (SetDesiredCapacity'),
    newSetDesiredCapacity,
    SetDesiredCapacityResponse (SetDesiredCapacityResponse'),
    newSetDesiredCapacityResponse,

    -- ** DetachLoadBalancers
    DetachLoadBalancers (DetachLoadBalancers'),
    newDetachLoadBalancers,
    DetachLoadBalancersResponse (DetachLoadBalancersResponse'),
    newDetachLoadBalancersResponse,

    -- ** DescribeAutoScalingNotificationTypes
    DescribeAutoScalingNotificationTypes (DescribeAutoScalingNotificationTypes'),
    newDescribeAutoScalingNotificationTypes,
    DescribeAutoScalingNotificationTypesResponse (DescribeAutoScalingNotificationTypesResponse'),
    newDescribeAutoScalingNotificationTypesResponse,

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** CreateOrUpdateTags
    CreateOrUpdateTags (CreateOrUpdateTags'),
    newCreateOrUpdateTags,
    CreateOrUpdateTagsResponse (CreateOrUpdateTagsResponse'),
    newCreateOrUpdateTagsResponse,

    -- ** CompleteLifecycleAction
    CompleteLifecycleAction (CompleteLifecycleAction'),
    newCompleteLifecycleAction,
    CompleteLifecycleActionResponse (CompleteLifecycleActionResponse'),
    newCompleteLifecycleActionResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** AttachInstances
    AttachInstances (AttachInstances'),
    newAttachInstances,
    AttachInstancesResponse (AttachInstancesResponse'),
    newAttachInstancesResponse,

    -- ** UpdateAutoScalingGroup
    UpdateAutoScalingGroup (UpdateAutoScalingGroup'),
    newUpdateAutoScalingGroup,
    UpdateAutoScalingGroupResponse (UpdateAutoScalingGroupResponse'),
    newUpdateAutoScalingGroupResponse,

    -- ** DeleteAutoScalingGroup
    DeleteAutoScalingGroup (DeleteAutoScalingGroup'),
    newDeleteAutoScalingGroup,
    DeleteAutoScalingGroupResponse (DeleteAutoScalingGroupResponse'),
    newDeleteAutoScalingGroupResponse,

    -- ** PutLifecycleHook
    PutLifecycleHook (PutLifecycleHook'),
    newPutLifecycleHook,
    PutLifecycleHookResponse (PutLifecycleHookResponse'),
    newPutLifecycleHookResponse,

    -- ** BatchPutScheduledUpdateGroupAction
    BatchPutScheduledUpdateGroupAction (BatchPutScheduledUpdateGroupAction'),
    newBatchPutScheduledUpdateGroupAction,
    BatchPutScheduledUpdateGroupActionResponse (BatchPutScheduledUpdateGroupActionResponse'),
    newBatchPutScheduledUpdateGroupActionResponse,

    -- ** DeleteLifecycleHook
    DeleteLifecycleHook (DeleteLifecycleHook'),
    newDeleteLifecycleHook,
    DeleteLifecycleHookResponse (DeleteLifecycleHookResponse'),
    newDeleteLifecycleHookResponse,

    -- ** ResumeProcesses
    ResumeProcesses (ResumeProcesses'),
    newResumeProcesses,
    ResumeProcessesResponse (ResumeProcessesResponse'),
    newResumeProcessesResponse,

    -- ** ExecutePolicy
    ExecutePolicy (ExecutePolicy'),
    newExecutePolicy,
    ExecutePolicyResponse (ExecutePolicyResponse'),
    newExecutePolicyResponse,

    -- ** GetPredictiveScalingForecast
    GetPredictiveScalingForecast (GetPredictiveScalingForecast'),
    newGetPredictiveScalingForecast,
    GetPredictiveScalingForecastResponse (GetPredictiveScalingForecastResponse'),
    newGetPredictiveScalingForecastResponse,

    -- ** DescribeInstanceRefreshes
    DescribeInstanceRefreshes (DescribeInstanceRefreshes'),
    newDescribeInstanceRefreshes,
    DescribeInstanceRefreshesResponse (DescribeInstanceRefreshesResponse'),
    newDescribeInstanceRefreshesResponse,

    -- ** DescribeAccountLimits
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** AttachLoadBalancers
    AttachLoadBalancers (AttachLoadBalancers'),
    newAttachLoadBalancers,
    AttachLoadBalancersResponse (AttachLoadBalancersResponse'),
    newAttachLoadBalancersResponse,

    -- ** BatchDeleteScheduledAction
    BatchDeleteScheduledAction (BatchDeleteScheduledAction'),
    newBatchDeleteScheduledAction,
    BatchDeleteScheduledActionResponse (BatchDeleteScheduledActionResponse'),
    newBatchDeleteScheduledActionResponse,

    -- ** TerminateInstanceInAutoScalingGroup
    TerminateInstanceInAutoScalingGroup (TerminateInstanceInAutoScalingGroup'),
    newTerminateInstanceInAutoScalingGroup,
    TerminateInstanceInAutoScalingGroupResponse (TerminateInstanceInAutoScalingGroupResponse'),
    newTerminateInstanceInAutoScalingGroupResponse,

    -- ** DescribeLoadBalancerTargetGroups (Paginated)
    DescribeLoadBalancerTargetGroups (DescribeLoadBalancerTargetGroups'),
    newDescribeLoadBalancerTargetGroups,
    DescribeLoadBalancerTargetGroupsResponse (DescribeLoadBalancerTargetGroupsResponse'),
    newDescribeLoadBalancerTargetGroupsResponse,

    -- ** PutScheduledUpdateGroupAction
    PutScheduledUpdateGroupAction (PutScheduledUpdateGroupAction'),
    newPutScheduledUpdateGroupAction,
    PutScheduledUpdateGroupActionResponse (PutScheduledUpdateGroupActionResponse'),
    newPutScheduledUpdateGroupActionResponse,

    -- ** SetInstanceProtection
    SetInstanceProtection (SetInstanceProtection'),
    newSetInstanceProtection,
    SetInstanceProtectionResponse (SetInstanceProtectionResponse'),
    newSetInstanceProtectionResponse,

    -- ** DescribePolicies (Paginated)
    DescribePolicies (DescribePolicies'),
    newDescribePolicies,
    DescribePoliciesResponse (DescribePoliciesResponse'),
    newDescribePoliciesResponse,

    -- ** DescribeLaunchConfigurations (Paginated)
    DescribeLaunchConfigurations (DescribeLaunchConfigurations'),
    newDescribeLaunchConfigurations,
    DescribeLaunchConfigurationsResponse (DescribeLaunchConfigurationsResponse'),
    newDescribeLaunchConfigurationsResponse,

    -- ** DescribeScalingActivities (Paginated)
    DescribeScalingActivities (DescribeScalingActivities'),
    newDescribeScalingActivities,
    DescribeScalingActivitiesResponse (DescribeScalingActivitiesResponse'),
    newDescribeScalingActivitiesResponse,

    -- ** DescribeNotificationConfigurations (Paginated)
    DescribeNotificationConfigurations (DescribeNotificationConfigurations'),
    newDescribeNotificationConfigurations,
    DescribeNotificationConfigurationsResponse (DescribeNotificationConfigurationsResponse'),
    newDescribeNotificationConfigurationsResponse,

    -- ** DescribeLifecycleHookTypes
    DescribeLifecycleHookTypes (DescribeLifecycleHookTypes'),
    newDescribeLifecycleHookTypes,
    DescribeLifecycleHookTypesResponse (DescribeLifecycleHookTypesResponse'),
    newDescribeLifecycleHookTypesResponse,

    -- ** DescribeAdjustmentTypes
    DescribeAdjustmentTypes (DescribeAdjustmentTypes'),
    newDescribeAdjustmentTypes,
    DescribeAdjustmentTypesResponse (DescribeAdjustmentTypesResponse'),
    newDescribeAdjustmentTypesResponse,

    -- ** CreateAutoScalingGroup
    CreateAutoScalingGroup (CreateAutoScalingGroup'),
    newCreateAutoScalingGroup,
    CreateAutoScalingGroupResponse (CreateAutoScalingGroupResponse'),
    newCreateAutoScalingGroupResponse,

    -- ** CreateLaunchConfiguration
    CreateLaunchConfiguration (CreateLaunchConfiguration'),
    newCreateLaunchConfiguration,
    CreateLaunchConfigurationResponse (CreateLaunchConfigurationResponse'),
    newCreateLaunchConfigurationResponse,

    -- * Types

    -- ** InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- ** InstanceMetadataHttpTokensState
    InstanceMetadataHttpTokensState (..),

    -- ** InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- ** LifecycleState
    LifecycleState (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** MetricType
    MetricType (..),

    -- ** PredefinedLoadMetricType
    PredefinedLoadMetricType (..),

    -- ** PredefinedMetricPairType
    PredefinedMetricPairType (..),

    -- ** PredefinedScalingMetricType
    PredefinedScalingMetricType (..),

    -- ** PredictiveScalingMaxCapacityBreachBehavior
    PredictiveScalingMaxCapacityBreachBehavior (..),

    -- ** PredictiveScalingMode
    PredictiveScalingMode (..),

    -- ** RefreshStrategy
    RefreshStrategy (..),

    -- ** ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- ** WarmPoolState
    WarmPoolState (..),

    -- ** WarmPoolStatus
    WarmPoolStatus (..),

    -- ** Activity
    Activity (Activity'),
    newActivity,

    -- ** AdjustmentType
    AdjustmentType (AdjustmentType'),
    newAdjustmentType,

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AutoScalingGroup
    AutoScalingGroup (AutoScalingGroup'),
    newAutoScalingGroup,

    -- ** AutoScalingInstanceDetails
    AutoScalingInstanceDetails (AutoScalingInstanceDetails'),
    newAutoScalingInstanceDetails,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (BlockDeviceMapping'),
    newBlockDeviceMapping,

    -- ** CapacityForecast
    CapacityForecast (CapacityForecast'),
    newCapacityForecast,

    -- ** CustomizedMetricSpecification
    CustomizedMetricSpecification (CustomizedMetricSpecification'),
    newCustomizedMetricSpecification,

    -- ** DesiredConfiguration
    DesiredConfiguration (DesiredConfiguration'),
    newDesiredConfiguration,

    -- ** Ebs
    Ebs (Ebs'),
    newEbs,

    -- ** EnabledMetric
    EnabledMetric (EnabledMetric'),
    newEnabledMetric,

    -- ** FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (FailedScheduledUpdateGroupActionRequest'),
    newFailedScheduledUpdateGroupActionRequest,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceMetadataOptions
    InstanceMetadataOptions (InstanceMetadataOptions'),
    newInstanceMetadataOptions,

    -- ** InstanceMonitoring
    InstanceMonitoring (InstanceMonitoring'),
    newInstanceMonitoring,

    -- ** InstanceRefresh
    InstanceRefresh (InstanceRefresh'),
    newInstanceRefresh,

    -- ** InstanceRefreshLivePoolProgress
    InstanceRefreshLivePoolProgress (InstanceRefreshLivePoolProgress'),
    newInstanceRefreshLivePoolProgress,

    -- ** InstanceRefreshProgressDetails
    InstanceRefreshProgressDetails (InstanceRefreshProgressDetails'),
    newInstanceRefreshProgressDetails,

    -- ** InstanceRefreshWarmPoolProgress
    InstanceRefreshWarmPoolProgress (InstanceRefreshWarmPoolProgress'),
    newInstanceRefreshWarmPoolProgress,

    -- ** InstancesDistribution
    InstancesDistribution (InstancesDistribution'),
    newInstancesDistribution,

    -- ** LaunchConfiguration
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** LaunchTemplate
    LaunchTemplate (LaunchTemplate'),
    newLaunchTemplate,

    -- ** LaunchTemplateOverrides
    LaunchTemplateOverrides (LaunchTemplateOverrides'),
    newLaunchTemplateOverrides,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (LaunchTemplateSpecification'),
    newLaunchTemplateSpecification,

    -- ** LifecycleHook
    LifecycleHook (LifecycleHook'),
    newLifecycleHook,

    -- ** LifecycleHookSpecification
    LifecycleHookSpecification (LifecycleHookSpecification'),
    newLifecycleHookSpecification,

    -- ** LoadBalancerState
    LoadBalancerState (LoadBalancerState'),
    newLoadBalancerState,

    -- ** LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState (LoadBalancerTargetGroupState'),
    newLoadBalancerTargetGroupState,

    -- ** LoadForecast
    LoadForecast (LoadForecast'),
    newLoadForecast,

    -- ** MetricCollectionType
    MetricCollectionType (MetricCollectionType'),
    newMetricCollectionType,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** MetricGranularityType
    MetricGranularityType (MetricGranularityType'),
    newMetricGranularityType,

    -- ** MixedInstancesPolicy
    MixedInstancesPolicy (MixedInstancesPolicy'),
    newMixedInstancesPolicy,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** PredefinedMetricSpecification
    PredefinedMetricSpecification (PredefinedMetricSpecification'),
    newPredefinedMetricSpecification,

    -- ** PredictiveScalingConfiguration
    PredictiveScalingConfiguration (PredictiveScalingConfiguration'),
    newPredictiveScalingConfiguration,

    -- ** PredictiveScalingMetricSpecification
    PredictiveScalingMetricSpecification (PredictiveScalingMetricSpecification'),
    newPredictiveScalingMetricSpecification,

    -- ** PredictiveScalingPredefinedLoadMetric
    PredictiveScalingPredefinedLoadMetric (PredictiveScalingPredefinedLoadMetric'),
    newPredictiveScalingPredefinedLoadMetric,

    -- ** PredictiveScalingPredefinedMetricPair
    PredictiveScalingPredefinedMetricPair (PredictiveScalingPredefinedMetricPair'),
    newPredictiveScalingPredefinedMetricPair,

    -- ** PredictiveScalingPredefinedScalingMetric
    PredictiveScalingPredefinedScalingMetric (PredictiveScalingPredefinedScalingMetric'),
    newPredictiveScalingPredefinedScalingMetric,

    -- ** ProcessType
    ProcessType (ProcessType'),
    newProcessType,

    -- ** RefreshPreferences
    RefreshPreferences (RefreshPreferences'),
    newRefreshPreferences,

    -- ** ScalingPolicy
    ScalingPolicy (ScalingPolicy'),
    newScalingPolicy,

    -- ** ScalingProcessQuery
    ScalingProcessQuery (ScalingProcessQuery'),
    newScalingProcessQuery,

    -- ** ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (ScheduledUpdateGroupAction'),
    newScheduledUpdateGroupAction,

    -- ** ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (ScheduledUpdateGroupActionRequest'),
    newScheduledUpdateGroupActionRequest,

    -- ** StepAdjustment
    StepAdjustment (StepAdjustment'),
    newStepAdjustment,

    -- ** SuspendedProcess
    SuspendedProcess (SuspendedProcess'),
    newSuspendedProcess,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagDescription
    TagDescription (TagDescription'),
    newTagDescription,

    -- ** TargetTrackingConfiguration
    TargetTrackingConfiguration (TargetTrackingConfiguration'),
    newTargetTrackingConfiguration,

    -- ** WarmPoolConfiguration
    WarmPoolConfiguration (WarmPoolConfiguration'),
    newWarmPoolConfiguration,
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
import Network.AWS.AutoScaling.DeleteWarmPool
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
import Network.AWS.AutoScaling.DescribeWarmPool
import Network.AWS.AutoScaling.DetachInstances
import Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
import Network.AWS.AutoScaling.DetachLoadBalancers
import Network.AWS.AutoScaling.DisableMetricsCollection
import Network.AWS.AutoScaling.EnableMetricsCollection
import Network.AWS.AutoScaling.EnterStandby
import Network.AWS.AutoScaling.ExecutePolicy
import Network.AWS.AutoScaling.ExitStandby
import Network.AWS.AutoScaling.GetPredictiveScalingForecast
import Network.AWS.AutoScaling.Lens
import Network.AWS.AutoScaling.PutLifecycleHook
import Network.AWS.AutoScaling.PutNotificationConfiguration
import Network.AWS.AutoScaling.PutScalingPolicy
import Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.PutWarmPool
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
