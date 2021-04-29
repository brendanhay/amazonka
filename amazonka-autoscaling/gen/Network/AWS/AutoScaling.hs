{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EC2 Auto Scaling
--
-- Amazon EC2 Auto Scaling is designed to automatically launch or terminate
-- EC2 instances based on user-defined scaling policies, scheduled actions,
-- and health checks. Use this service with AWS Auto Scaling, Amazon
-- CloudWatch, and Elastic Load Balancing.
--
-- For more information, including information about granting IAM users
-- required permissions for Amazon EC2 Auto Scaling actions, see the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/what-is-amazon-ec2-auto-scaling.html Amazon EC2 Auto Scaling User Guide>.
module Network.AWS.AutoScaling
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AlreadyExistsFault
    _AlreadyExistsFault,

    -- ** ResourceInUseFault
    _ResourceInUseFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** InstanceRefreshInProgressFault
    _InstanceRefreshInProgressFault,

    -- ** ScalingActivityInProgressFault
    _ScalingActivityInProgressFault,

    -- ** ActiveInstanceRefreshNotFoundFault
    _ActiveInstanceRefreshNotFoundFault,

    -- ** ResourceContentionFault
    _ResourceContentionFault,

    -- ** ServiceLinkedRoleFailure
    _ServiceLinkedRoleFailure,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SuspendProcesses
    SuspendProcesses (SuspendProcesses'),
    newSuspendProcesses,
    SuspendProcessesResponse (SuspendProcessesResponse'),
    newSuspendProcessesResponse,

    -- ** DescribeInstanceRefreshes
    DescribeInstanceRefreshes (DescribeInstanceRefreshes'),
    newDescribeInstanceRefreshes,
    DescribeInstanceRefreshesResponse (DescribeInstanceRefreshesResponse'),
    newDescribeInstanceRefreshesResponse,

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

    -- ** PutNotificationConfiguration
    PutNotificationConfiguration (PutNotificationConfiguration'),
    newPutNotificationConfiguration,
    PutNotificationConfigurationResponse (PutNotificationConfigurationResponse'),
    newPutNotificationConfigurationResponse,

    -- ** AttachLoadBalancerTargetGroups
    AttachLoadBalancerTargetGroups (AttachLoadBalancerTargetGroups'),
    newAttachLoadBalancerTargetGroups,
    AttachLoadBalancerTargetGroupsResponse (AttachLoadBalancerTargetGroupsResponse'),
    newAttachLoadBalancerTargetGroupsResponse,

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

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

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

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** DescribeAutoScalingNotificationTypes
    DescribeAutoScalingNotificationTypes (DescribeAutoScalingNotificationTypes'),
    newDescribeAutoScalingNotificationTypes,
    DescribeAutoScalingNotificationTypesResponse (DescribeAutoScalingNotificationTypesResponse'),
    newDescribeAutoScalingNotificationTypesResponse,

    -- ** DescribeAdjustmentTypes
    DescribeAdjustmentTypes (DescribeAdjustmentTypes'),
    newDescribeAdjustmentTypes,
    DescribeAdjustmentTypesResponse (DescribeAdjustmentTypesResponse'),
    newDescribeAdjustmentTypesResponse,

    -- ** DetachLoadBalancers
    DetachLoadBalancers (DetachLoadBalancers'),
    newDetachLoadBalancers,
    DetachLoadBalancersResponse (DetachLoadBalancersResponse'),
    newDetachLoadBalancersResponse,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** DescribeScalingActivities (Paginated)
    DescribeScalingActivities (DescribeScalingActivities'),
    newDescribeScalingActivities,
    DescribeScalingActivitiesResponse (DescribeScalingActivitiesResponse'),
    newDescribeScalingActivitiesResponse,

    -- ** DescribeLifecycleHooks
    DescribeLifecycleHooks (DescribeLifecycleHooks'),
    newDescribeLifecycleHooks,
    DescribeLifecycleHooksResponse (DescribeLifecycleHooksResponse'),
    newDescribeLifecycleHooksResponse,

    -- ** DetachLoadBalancerTargetGroups
    DetachLoadBalancerTargetGroups (DetachLoadBalancerTargetGroups'),
    newDetachLoadBalancerTargetGroups,
    DetachLoadBalancerTargetGroupsResponse (DetachLoadBalancerTargetGroupsResponse'),
    newDetachLoadBalancerTargetGroupsResponse,

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

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DetachInstances
    DetachInstances (DetachInstances'),
    newDetachInstances,
    DetachInstancesResponse (DetachInstancesResponse'),
    newDetachInstancesResponse,

    -- ** AttachLoadBalancers
    AttachLoadBalancers (AttachLoadBalancers'),
    newAttachLoadBalancers,
    AttachLoadBalancersResponse (AttachLoadBalancersResponse'),
    newAttachLoadBalancersResponse,

    -- ** DescribeAccountLimits
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** TerminateInstanceInAutoScalingGroup
    TerminateInstanceInAutoScalingGroup (TerminateInstanceInAutoScalingGroup'),
    newTerminateInstanceInAutoScalingGroup,
    TerminateInstanceInAutoScalingGroupResponse (TerminateInstanceInAutoScalingGroupResponse'),
    newTerminateInstanceInAutoScalingGroupResponse,

    -- ** DescribeTerminationPolicyTypes
    DescribeTerminationPolicyTypes (DescribeTerminationPolicyTypes'),
    newDescribeTerminationPolicyTypes,
    DescribeTerminationPolicyTypesResponse (DescribeTerminationPolicyTypesResponse'),
    newDescribeTerminationPolicyTypesResponse,

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

    -- ** DeleteLaunchConfiguration
    DeleteLaunchConfiguration (DeleteLaunchConfiguration'),
    newDeleteLaunchConfiguration,
    DeleteLaunchConfigurationResponse (DeleteLaunchConfigurationResponse'),
    newDeleteLaunchConfigurationResponse,

    -- ** DeleteNotificationConfiguration
    DeleteNotificationConfiguration (DeleteNotificationConfiguration'),
    newDeleteNotificationConfiguration,
    DeleteNotificationConfigurationResponse (DeleteNotificationConfigurationResponse'),
    newDeleteNotificationConfigurationResponse,

    -- ** UpdateAutoScalingGroup
    UpdateAutoScalingGroup (UpdateAutoScalingGroup'),
    newUpdateAutoScalingGroup,
    UpdateAutoScalingGroupResponse (UpdateAutoScalingGroupResponse'),
    newUpdateAutoScalingGroupResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** DeleteAutoScalingGroup
    DeleteAutoScalingGroup (DeleteAutoScalingGroup'),
    newDeleteAutoScalingGroup,
    DeleteAutoScalingGroupResponse (DeleteAutoScalingGroupResponse'),
    newDeleteAutoScalingGroupResponse,

    -- ** DescribeMetricCollectionTypes
    DescribeMetricCollectionTypes (DescribeMetricCollectionTypes'),
    newDescribeMetricCollectionTypes,
    DescribeMetricCollectionTypesResponse (DescribeMetricCollectionTypesResponse'),
    newDescribeMetricCollectionTypesResponse,

    -- ** CreateAutoScalingGroup
    CreateAutoScalingGroup (CreateAutoScalingGroup'),
    newCreateAutoScalingGroup,
    CreateAutoScalingGroupResponse (CreateAutoScalingGroupResponse'),
    newCreateAutoScalingGroupResponse,

    -- ** CompleteLifecycleAction
    CompleteLifecycleAction (CompleteLifecycleAction'),
    newCompleteLifecycleAction,
    CompleteLifecycleActionResponse (CompleteLifecycleActionResponse'),
    newCompleteLifecycleActionResponse,

    -- ** AttachInstances
    AttachInstances (AttachInstances'),
    newAttachInstances,
    AttachInstancesResponse (AttachInstancesResponse'),
    newAttachInstancesResponse,

    -- ** SetDesiredCapacity
    SetDesiredCapacity (SetDesiredCapacity'),
    newSetDesiredCapacity,
    SetDesiredCapacityResponse (SetDesiredCapacityResponse'),
    newSetDesiredCapacityResponse,

    -- ** DescribePolicies (Paginated)
    DescribePolicies (DescribePolicies'),
    newDescribePolicies,
    DescribePoliciesResponse (DescribePoliciesResponse'),
    newDescribePoliciesResponse,

    -- ** DescribeAutoScalingGroups (Paginated)
    DescribeAutoScalingGroups (DescribeAutoScalingGroups'),
    newDescribeAutoScalingGroups,
    DescribeAutoScalingGroupsResponse (DescribeAutoScalingGroupsResponse'),
    newDescribeAutoScalingGroupsResponse,

    -- ** DescribeLaunchConfigurations (Paginated)
    DescribeLaunchConfigurations (DescribeLaunchConfigurations'),
    newDescribeLaunchConfigurations,
    DescribeLaunchConfigurationsResponse (DescribeLaunchConfigurationsResponse'),
    newDescribeLaunchConfigurationsResponse,

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

    -- ** DescribeAutoScalingInstances (Paginated)
    DescribeAutoScalingInstances (DescribeAutoScalingInstances'),
    newDescribeAutoScalingInstances,
    DescribeAutoScalingInstancesResponse (DescribeAutoScalingInstancesResponse'),
    newDescribeAutoScalingInstancesResponse,

    -- ** DisableMetricsCollection
    DisableMetricsCollection (DisableMetricsCollection'),
    newDisableMetricsCollection,
    DisableMetricsCollectionResponse (DisableMetricsCollectionResponse'),
    newDisableMetricsCollectionResponse,

    -- ** RecordLifecycleActionHeartbeat
    RecordLifecycleActionHeartbeat (RecordLifecycleActionHeartbeat'),
    newRecordLifecycleActionHeartbeat,
    RecordLifecycleActionHeartbeatResponse (RecordLifecycleActionHeartbeatResponse'),
    newRecordLifecycleActionHeartbeatResponse,

    -- ** BatchDeleteScheduledAction
    BatchDeleteScheduledAction (BatchDeleteScheduledAction'),
    newBatchDeleteScheduledAction,
    BatchDeleteScheduledActionResponse (BatchDeleteScheduledActionResponse'),
    newBatchDeleteScheduledActionResponse,

    -- ** DescribeLoadBalancerTargetGroups (Paginated)
    DescribeLoadBalancerTargetGroups (DescribeLoadBalancerTargetGroups'),
    newDescribeLoadBalancerTargetGroups,
    DescribeLoadBalancerTargetGroupsResponse (DescribeLoadBalancerTargetGroupsResponse'),
    newDescribeLoadBalancerTargetGroupsResponse,

    -- ** CancelInstanceRefresh
    CancelInstanceRefresh (CancelInstanceRefresh'),
    newCancelInstanceRefresh,
    CancelInstanceRefreshResponse (CancelInstanceRefreshResponse'),
    newCancelInstanceRefreshResponse,

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

    -- ** RefreshStrategy
    RefreshStrategy (..),

    -- ** ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

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

    -- ** CustomizedMetricSpecification
    CustomizedMetricSpecification (CustomizedMetricSpecification'),
    newCustomizedMetricSpecification,

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
import Network.AWS.AutoScaling.Lens
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
