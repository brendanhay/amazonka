{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AutoScaling
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2011-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EC2 Auto Scaling
--
-- Amazon EC2 Auto Scaling is designed to automatically launch and
-- terminate EC2 instances based on user-defined scaling policies,
-- scheduled actions, and health checks.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ Amazon EC2 Auto Scaling User Guide>
-- and the
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/Welcome.html Amazon EC2 Auto Scaling API Reference>.
module Amazonka.AutoScaling
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ActiveInstanceRefreshNotFoundFault
    _ActiveInstanceRefreshNotFoundFault,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** ResourceInUseFault
    _ResourceInUseFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** ServiceLinkedRoleFailure
    _ServiceLinkedRoleFailure,

    -- ** ScalingActivityInProgressFault
    _ScalingActivityInProgressFault,

    -- ** InstanceRefreshInProgressFault
    _InstanceRefreshInProgressFault,

    -- ** AlreadyExistsFault
    _AlreadyExistsFault,

    -- ** ResourceContentionFault
    _ResourceContentionFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AttachInstances
    AttachInstances (AttachInstances'),
    newAttachInstances,
    AttachInstancesResponse (AttachInstancesResponse'),
    newAttachInstancesResponse,

    -- ** AttachLoadBalancerTargetGroups
    AttachLoadBalancerTargetGroups (AttachLoadBalancerTargetGroups'),
    newAttachLoadBalancerTargetGroups,
    AttachLoadBalancerTargetGroupsResponse (AttachLoadBalancerTargetGroupsResponse'),
    newAttachLoadBalancerTargetGroupsResponse,

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

    -- ** BatchPutScheduledUpdateGroupAction
    BatchPutScheduledUpdateGroupAction (BatchPutScheduledUpdateGroupAction'),
    newBatchPutScheduledUpdateGroupAction,
    BatchPutScheduledUpdateGroupActionResponse (BatchPutScheduledUpdateGroupActionResponse'),
    newBatchPutScheduledUpdateGroupActionResponse,

    -- ** CancelInstanceRefresh
    CancelInstanceRefresh (CancelInstanceRefresh'),
    newCancelInstanceRefresh,
    CancelInstanceRefreshResponse (CancelInstanceRefreshResponse'),
    newCancelInstanceRefreshResponse,

    -- ** CompleteLifecycleAction
    CompleteLifecycleAction (CompleteLifecycleAction'),
    newCompleteLifecycleAction,
    CompleteLifecycleActionResponse (CompleteLifecycleActionResponse'),
    newCompleteLifecycleActionResponse,

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

    -- ** CreateOrUpdateTags
    CreateOrUpdateTags (CreateOrUpdateTags'),
    newCreateOrUpdateTags,
    CreateOrUpdateTagsResponse (CreateOrUpdateTagsResponse'),
    newCreateOrUpdateTagsResponse,

    -- ** DeleteAutoScalingGroup
    DeleteAutoScalingGroup (DeleteAutoScalingGroup'),
    newDeleteAutoScalingGroup,
    DeleteAutoScalingGroupResponse (DeleteAutoScalingGroupResponse'),
    newDeleteAutoScalingGroupResponse,

    -- ** DeleteLaunchConfiguration
    DeleteLaunchConfiguration (DeleteLaunchConfiguration'),
    newDeleteLaunchConfiguration,
    DeleteLaunchConfigurationResponse (DeleteLaunchConfigurationResponse'),
    newDeleteLaunchConfigurationResponse,

    -- ** DeleteLifecycleHook
    DeleteLifecycleHook (DeleteLifecycleHook'),
    newDeleteLifecycleHook,
    DeleteLifecycleHookResponse (DeleteLifecycleHookResponse'),
    newDeleteLifecycleHookResponse,

    -- ** DeleteNotificationConfiguration
    DeleteNotificationConfiguration (DeleteNotificationConfiguration'),
    newDeleteNotificationConfiguration,
    DeleteNotificationConfigurationResponse (DeleteNotificationConfigurationResponse'),
    newDeleteNotificationConfigurationResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteWarmPool
    DeleteWarmPool (DeleteWarmPool'),
    newDeleteWarmPool,
    DeleteWarmPoolResponse (DeleteWarmPoolResponse'),
    newDeleteWarmPoolResponse,

    -- ** DescribeAccountLimits
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** DescribeAdjustmentTypes
    DescribeAdjustmentTypes (DescribeAdjustmentTypes'),
    newDescribeAdjustmentTypes,
    DescribeAdjustmentTypesResponse (DescribeAdjustmentTypesResponse'),
    newDescribeAdjustmentTypesResponse,

    -- ** DescribeAutoScalingGroups (Paginated)
    DescribeAutoScalingGroups (DescribeAutoScalingGroups'),
    newDescribeAutoScalingGroups,
    DescribeAutoScalingGroupsResponse (DescribeAutoScalingGroupsResponse'),
    newDescribeAutoScalingGroupsResponse,

    -- ** DescribeAutoScalingInstances (Paginated)
    DescribeAutoScalingInstances (DescribeAutoScalingInstances'),
    newDescribeAutoScalingInstances,
    DescribeAutoScalingInstancesResponse (DescribeAutoScalingInstancesResponse'),
    newDescribeAutoScalingInstancesResponse,

    -- ** DescribeAutoScalingNotificationTypes
    DescribeAutoScalingNotificationTypes (DescribeAutoScalingNotificationTypes'),
    newDescribeAutoScalingNotificationTypes,
    DescribeAutoScalingNotificationTypesResponse (DescribeAutoScalingNotificationTypesResponse'),
    newDescribeAutoScalingNotificationTypesResponse,

    -- ** DescribeInstanceRefreshes
    DescribeInstanceRefreshes (DescribeInstanceRefreshes'),
    newDescribeInstanceRefreshes,
    DescribeInstanceRefreshesResponse (DescribeInstanceRefreshesResponse'),
    newDescribeInstanceRefreshesResponse,

    -- ** DescribeLaunchConfigurations (Paginated)
    DescribeLaunchConfigurations (DescribeLaunchConfigurations'),
    newDescribeLaunchConfigurations,
    DescribeLaunchConfigurationsResponse (DescribeLaunchConfigurationsResponse'),
    newDescribeLaunchConfigurationsResponse,

    -- ** DescribeLifecycleHookTypes
    DescribeLifecycleHookTypes (DescribeLifecycleHookTypes'),
    newDescribeLifecycleHookTypes,
    DescribeLifecycleHookTypesResponse (DescribeLifecycleHookTypesResponse'),
    newDescribeLifecycleHookTypesResponse,

    -- ** DescribeLifecycleHooks
    DescribeLifecycleHooks (DescribeLifecycleHooks'),
    newDescribeLifecycleHooks,
    DescribeLifecycleHooksResponse (DescribeLifecycleHooksResponse'),
    newDescribeLifecycleHooksResponse,

    -- ** DescribeLoadBalancerTargetGroups (Paginated)
    DescribeLoadBalancerTargetGroups (DescribeLoadBalancerTargetGroups'),
    newDescribeLoadBalancerTargetGroups,
    DescribeLoadBalancerTargetGroupsResponse (DescribeLoadBalancerTargetGroupsResponse'),
    newDescribeLoadBalancerTargetGroupsResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** DescribeMetricCollectionTypes
    DescribeMetricCollectionTypes (DescribeMetricCollectionTypes'),
    newDescribeMetricCollectionTypes,
    DescribeMetricCollectionTypesResponse (DescribeMetricCollectionTypesResponse'),
    newDescribeMetricCollectionTypesResponse,

    -- ** DescribeNotificationConfigurations (Paginated)
    DescribeNotificationConfigurations (DescribeNotificationConfigurations'),
    newDescribeNotificationConfigurations,
    DescribeNotificationConfigurationsResponse (DescribeNotificationConfigurationsResponse'),
    newDescribeNotificationConfigurationsResponse,

    -- ** DescribePolicies (Paginated)
    DescribePolicies (DescribePolicies'),
    newDescribePolicies,
    DescribePoliciesResponse (DescribePoliciesResponse'),
    newDescribePoliciesResponse,

    -- ** DescribeScalingActivities (Paginated)
    DescribeScalingActivities (DescribeScalingActivities'),
    newDescribeScalingActivities,
    DescribeScalingActivitiesResponse (DescribeScalingActivitiesResponse'),
    newDescribeScalingActivitiesResponse,

    -- ** DescribeScalingProcessTypes
    DescribeScalingProcessTypes (DescribeScalingProcessTypes'),
    newDescribeScalingProcessTypes,
    DescribeScalingProcessTypesResponse (DescribeScalingProcessTypesResponse'),
    newDescribeScalingProcessTypesResponse,

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeTerminationPolicyTypes
    DescribeTerminationPolicyTypes (DescribeTerminationPolicyTypes'),
    newDescribeTerminationPolicyTypes,
    DescribeTerminationPolicyTypesResponse (DescribeTerminationPolicyTypesResponse'),
    newDescribeTerminationPolicyTypesResponse,

    -- ** DescribeWarmPool
    DescribeWarmPool (DescribeWarmPool'),
    newDescribeWarmPool,
    DescribeWarmPoolResponse (DescribeWarmPoolResponse'),
    newDescribeWarmPoolResponse,

    -- ** DetachInstances
    DetachInstances (DetachInstances'),
    newDetachInstances,
    DetachInstancesResponse (DetachInstancesResponse'),
    newDetachInstancesResponse,

    -- ** DetachLoadBalancerTargetGroups
    DetachLoadBalancerTargetGroups (DetachLoadBalancerTargetGroups'),
    newDetachLoadBalancerTargetGroups,
    DetachLoadBalancerTargetGroupsResponse (DetachLoadBalancerTargetGroupsResponse'),
    newDetachLoadBalancerTargetGroupsResponse,

    -- ** DetachLoadBalancers
    DetachLoadBalancers (DetachLoadBalancers'),
    newDetachLoadBalancers,
    DetachLoadBalancersResponse (DetachLoadBalancersResponse'),
    newDetachLoadBalancersResponse,

    -- ** DisableMetricsCollection
    DisableMetricsCollection (DisableMetricsCollection'),
    newDisableMetricsCollection,
    DisableMetricsCollectionResponse (DisableMetricsCollectionResponse'),
    newDisableMetricsCollectionResponse,

    -- ** EnableMetricsCollection
    EnableMetricsCollection (EnableMetricsCollection'),
    newEnableMetricsCollection,
    EnableMetricsCollectionResponse (EnableMetricsCollectionResponse'),
    newEnableMetricsCollectionResponse,

    -- ** EnterStandby
    EnterStandby (EnterStandby'),
    newEnterStandby,
    EnterStandbyResponse (EnterStandbyResponse'),
    newEnterStandbyResponse,

    -- ** ExecutePolicy
    ExecutePolicy (ExecutePolicy'),
    newExecutePolicy,
    ExecutePolicyResponse (ExecutePolicyResponse'),
    newExecutePolicyResponse,

    -- ** ExitStandby
    ExitStandby (ExitStandby'),
    newExitStandby,
    ExitStandbyResponse (ExitStandbyResponse'),
    newExitStandbyResponse,

    -- ** GetPredictiveScalingForecast
    GetPredictiveScalingForecast (GetPredictiveScalingForecast'),
    newGetPredictiveScalingForecast,
    GetPredictiveScalingForecastResponse (GetPredictiveScalingForecastResponse'),
    newGetPredictiveScalingForecastResponse,

    -- ** PutLifecycleHook
    PutLifecycleHook (PutLifecycleHook'),
    newPutLifecycleHook,
    PutLifecycleHookResponse (PutLifecycleHookResponse'),
    newPutLifecycleHookResponse,

    -- ** PutNotificationConfiguration
    PutNotificationConfiguration (PutNotificationConfiguration'),
    newPutNotificationConfiguration,
    PutNotificationConfigurationResponse (PutNotificationConfigurationResponse'),
    newPutNotificationConfigurationResponse,

    -- ** PutScalingPolicy
    PutScalingPolicy (PutScalingPolicy'),
    newPutScalingPolicy,
    PutScalingPolicyResponse (PutScalingPolicyResponse'),
    newPutScalingPolicyResponse,

    -- ** PutScheduledUpdateGroupAction
    PutScheduledUpdateGroupAction (PutScheduledUpdateGroupAction'),
    newPutScheduledUpdateGroupAction,
    PutScheduledUpdateGroupActionResponse (PutScheduledUpdateGroupActionResponse'),
    newPutScheduledUpdateGroupActionResponse,

    -- ** PutWarmPool
    PutWarmPool (PutWarmPool'),
    newPutWarmPool,
    PutWarmPoolResponse (PutWarmPoolResponse'),
    newPutWarmPoolResponse,

    -- ** RecordLifecycleActionHeartbeat
    RecordLifecycleActionHeartbeat (RecordLifecycleActionHeartbeat'),
    newRecordLifecycleActionHeartbeat,
    RecordLifecycleActionHeartbeatResponse (RecordLifecycleActionHeartbeatResponse'),
    newRecordLifecycleActionHeartbeatResponse,

    -- ** ResumeProcesses
    ResumeProcesses (ResumeProcesses'),
    newResumeProcesses,
    ResumeProcessesResponse (ResumeProcessesResponse'),
    newResumeProcessesResponse,

    -- ** SetDesiredCapacity
    SetDesiredCapacity (SetDesiredCapacity'),
    newSetDesiredCapacity,
    SetDesiredCapacityResponse (SetDesiredCapacityResponse'),
    newSetDesiredCapacityResponse,

    -- ** SetInstanceHealth
    SetInstanceHealth (SetInstanceHealth'),
    newSetInstanceHealth,
    SetInstanceHealthResponse (SetInstanceHealthResponse'),
    newSetInstanceHealthResponse,

    -- ** SetInstanceProtection
    SetInstanceProtection (SetInstanceProtection'),
    newSetInstanceProtection,
    SetInstanceProtectionResponse (SetInstanceProtectionResponse'),
    newSetInstanceProtectionResponse,

    -- ** StartInstanceRefresh
    StartInstanceRefresh (StartInstanceRefresh'),
    newStartInstanceRefresh,
    StartInstanceRefreshResponse (StartInstanceRefreshResponse'),
    newStartInstanceRefreshResponse,

    -- ** SuspendProcesses
    SuspendProcesses (SuspendProcesses'),
    newSuspendProcesses,
    SuspendProcessesResponse (SuspendProcessesResponse'),
    newSuspendProcessesResponse,

    -- ** TerminateInstanceInAutoScalingGroup
    TerminateInstanceInAutoScalingGroup (TerminateInstanceInAutoScalingGroup'),
    newTerminateInstanceInAutoScalingGroup,
    TerminateInstanceInAutoScalingGroupResponse (TerminateInstanceInAutoScalingGroupResponse'),
    newTerminateInstanceInAutoScalingGroupResponse,

    -- ** UpdateAutoScalingGroup
    UpdateAutoScalingGroup (UpdateAutoScalingGroup'),
    newUpdateAutoScalingGroup,
    UpdateAutoScalingGroupResponse (UpdateAutoScalingGroupResponse'),
    newUpdateAutoScalingGroupResponse,

    -- * Types

    -- ** AcceleratorManufacturer
    AcceleratorManufacturer (..),

    -- ** AcceleratorName
    AcceleratorName (..),

    -- ** AcceleratorType
    AcceleratorType (..),

    -- ** BareMetal
    BareMetal (..),

    -- ** BurstablePerformance
    BurstablePerformance (..),

    -- ** CpuManufacturer
    CpuManufacturer (..),

    -- ** InstanceGeneration
    InstanceGeneration (..),

    -- ** InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- ** InstanceMetadataHttpTokensState
    InstanceMetadataHttpTokensState (..),

    -- ** InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- ** LifecycleState
    LifecycleState (..),

    -- ** LocalStorage
    LocalStorage (..),

    -- ** LocalStorageType
    LocalStorageType (..),

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

    -- ** AcceleratorCountRequest
    AcceleratorCountRequest (AcceleratorCountRequest'),
    newAcceleratorCountRequest,

    -- ** AcceleratorTotalMemoryMiBRequest
    AcceleratorTotalMemoryMiBRequest (AcceleratorTotalMemoryMiBRequest'),
    newAcceleratorTotalMemoryMiBRequest,

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

    -- ** BaselineEbsBandwidthMbpsRequest
    BaselineEbsBandwidthMbpsRequest (BaselineEbsBandwidthMbpsRequest'),
    newBaselineEbsBandwidthMbpsRequest,

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

    -- ** InstanceRequirements
    InstanceRequirements (InstanceRequirements'),
    newInstanceRequirements,

    -- ** InstanceReusePolicy
    InstanceReusePolicy (InstanceReusePolicy'),
    newInstanceReusePolicy,

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

    -- ** MemoryGiBPerVCpuRequest
    MemoryGiBPerVCpuRequest (MemoryGiBPerVCpuRequest'),
    newMemoryGiBPerVCpuRequest,

    -- ** MemoryMiBRequest
    MemoryMiBRequest (MemoryMiBRequest'),
    newMemoryMiBRequest,

    -- ** Metric
    Metric (Metric'),
    newMetric,

    -- ** MetricCollectionType
    MetricCollectionType (MetricCollectionType'),
    newMetricCollectionType,

    -- ** MetricDataQuery
    MetricDataQuery (MetricDataQuery'),
    newMetricDataQuery,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** MetricGranularityType
    MetricGranularityType (MetricGranularityType'),
    newMetricGranularityType,

    -- ** MetricStat
    MetricStat (MetricStat'),
    newMetricStat,

    -- ** MixedInstancesPolicy
    MixedInstancesPolicy (MixedInstancesPolicy'),
    newMixedInstancesPolicy,

    -- ** NetworkBandwidthGbpsRequest
    NetworkBandwidthGbpsRequest (NetworkBandwidthGbpsRequest'),
    newNetworkBandwidthGbpsRequest,

    -- ** NetworkInterfaceCountRequest
    NetworkInterfaceCountRequest (NetworkInterfaceCountRequest'),
    newNetworkInterfaceCountRequest,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** PredefinedMetricSpecification
    PredefinedMetricSpecification (PredefinedMetricSpecification'),
    newPredefinedMetricSpecification,

    -- ** PredictiveScalingConfiguration
    PredictiveScalingConfiguration (PredictiveScalingConfiguration'),
    newPredictiveScalingConfiguration,

    -- ** PredictiveScalingCustomizedCapacityMetric
    PredictiveScalingCustomizedCapacityMetric (PredictiveScalingCustomizedCapacityMetric'),
    newPredictiveScalingCustomizedCapacityMetric,

    -- ** PredictiveScalingCustomizedLoadMetric
    PredictiveScalingCustomizedLoadMetric (PredictiveScalingCustomizedLoadMetric'),
    newPredictiveScalingCustomizedLoadMetric,

    -- ** PredictiveScalingCustomizedScalingMetric
    PredictiveScalingCustomizedScalingMetric (PredictiveScalingCustomizedScalingMetric'),
    newPredictiveScalingCustomizedScalingMetric,

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

    -- ** TotalLocalStorageGBRequest
    TotalLocalStorageGBRequest (TotalLocalStorageGBRequest'),
    newTotalLocalStorageGBRequest,

    -- ** VCpuCountRequest
    VCpuCountRequest (VCpuCountRequest'),
    newVCpuCountRequest,

    -- ** WarmPoolConfiguration
    WarmPoolConfiguration (WarmPoolConfiguration'),
    newWarmPoolConfiguration,
  )
where

import Amazonka.AutoScaling.AttachInstances
import Amazonka.AutoScaling.AttachLoadBalancerTargetGroups
import Amazonka.AutoScaling.AttachLoadBalancers
import Amazonka.AutoScaling.BatchDeleteScheduledAction
import Amazonka.AutoScaling.BatchPutScheduledUpdateGroupAction
import Amazonka.AutoScaling.CancelInstanceRefresh
import Amazonka.AutoScaling.CompleteLifecycleAction
import Amazonka.AutoScaling.CreateAutoScalingGroup
import Amazonka.AutoScaling.CreateLaunchConfiguration
import Amazonka.AutoScaling.CreateOrUpdateTags
import Amazonka.AutoScaling.DeleteAutoScalingGroup
import Amazonka.AutoScaling.DeleteLaunchConfiguration
import Amazonka.AutoScaling.DeleteLifecycleHook
import Amazonka.AutoScaling.DeleteNotificationConfiguration
import Amazonka.AutoScaling.DeletePolicy
import Amazonka.AutoScaling.DeleteScheduledAction
import Amazonka.AutoScaling.DeleteTags
import Amazonka.AutoScaling.DeleteWarmPool
import Amazonka.AutoScaling.DescribeAccountLimits
import Amazonka.AutoScaling.DescribeAdjustmentTypes
import Amazonka.AutoScaling.DescribeAutoScalingGroups
import Amazonka.AutoScaling.DescribeAutoScalingInstances
import Amazonka.AutoScaling.DescribeAutoScalingNotificationTypes
import Amazonka.AutoScaling.DescribeInstanceRefreshes
import Amazonka.AutoScaling.DescribeLaunchConfigurations
import Amazonka.AutoScaling.DescribeLifecycleHookTypes
import Amazonka.AutoScaling.DescribeLifecycleHooks
import Amazonka.AutoScaling.DescribeLoadBalancerTargetGroups
import Amazonka.AutoScaling.DescribeLoadBalancers
import Amazonka.AutoScaling.DescribeMetricCollectionTypes
import Amazonka.AutoScaling.DescribeNotificationConfigurations
import Amazonka.AutoScaling.DescribePolicies
import Amazonka.AutoScaling.DescribeScalingActivities
import Amazonka.AutoScaling.DescribeScalingProcessTypes
import Amazonka.AutoScaling.DescribeScheduledActions
import Amazonka.AutoScaling.DescribeTags
import Amazonka.AutoScaling.DescribeTerminationPolicyTypes
import Amazonka.AutoScaling.DescribeWarmPool
import Amazonka.AutoScaling.DetachInstances
import Amazonka.AutoScaling.DetachLoadBalancerTargetGroups
import Amazonka.AutoScaling.DetachLoadBalancers
import Amazonka.AutoScaling.DisableMetricsCollection
import Amazonka.AutoScaling.EnableMetricsCollection
import Amazonka.AutoScaling.EnterStandby
import Amazonka.AutoScaling.ExecutePolicy
import Amazonka.AutoScaling.ExitStandby
import Amazonka.AutoScaling.GetPredictiveScalingForecast
import Amazonka.AutoScaling.Lens
import Amazonka.AutoScaling.PutLifecycleHook
import Amazonka.AutoScaling.PutNotificationConfiguration
import Amazonka.AutoScaling.PutScalingPolicy
import Amazonka.AutoScaling.PutScheduledUpdateGroupAction
import Amazonka.AutoScaling.PutWarmPool
import Amazonka.AutoScaling.RecordLifecycleActionHeartbeat
import Amazonka.AutoScaling.ResumeProcesses
import Amazonka.AutoScaling.SetDesiredCapacity
import Amazonka.AutoScaling.SetInstanceHealth
import Amazonka.AutoScaling.SetInstanceProtection
import Amazonka.AutoScaling.StartInstanceRefresh
import Amazonka.AutoScaling.SuspendProcesses
import Amazonka.AutoScaling.TerminateInstanceInAutoScalingGroup
import Amazonka.AutoScaling.Types
import Amazonka.AutoScaling.UpdateAutoScalingGroup
import Amazonka.AutoScaling.Waiters

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
