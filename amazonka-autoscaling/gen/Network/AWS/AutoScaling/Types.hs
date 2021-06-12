{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsFault,
    _ResourceInUseFault,
    _LimitExceededFault,
    _InstanceRefreshInProgressFault,
    _ScalingActivityInProgressFault,
    _ActiveInstanceRefreshNotFoundFault,
    _ResourceContentionFault,
    _ServiceLinkedRoleFailure,
    _InvalidNextToken,

    -- * InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- * InstanceMetadataHttpTokensState
    InstanceMetadataHttpTokensState (..),

    -- * InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- * LifecycleState
    LifecycleState (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricType
    MetricType (..),

    -- * RefreshStrategy
    RefreshStrategy (..),

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * Activity
    Activity (..),
    newActivity,
    activity_statusMessage,
    activity_autoScalingGroupARN,
    activity_details,
    activity_endTime,
    activity_autoScalingGroupState,
    activity_description,
    activity_progress,
    activity_activityId,
    activity_autoScalingGroupName,
    activity_cause,
    activity_startTime,
    activity_statusCode,

    -- * AdjustmentType
    AdjustmentType (..),
    newAdjustmentType,
    adjustmentType_adjustmentType,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_alarmARN,
    alarm_alarmName,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_status,
    autoScalingGroup_placementGroup,
    autoScalingGroup_suspendedProcesses,
    autoScalingGroup_maxInstanceLifetime,
    autoScalingGroup_autoScalingGroupARN,
    autoScalingGroup_launchTemplate,
    autoScalingGroup_instances,
    autoScalingGroup_launchConfigurationName,
    autoScalingGroup_mixedInstancesPolicy,
    autoScalingGroup_tags,
    autoScalingGroup_loadBalancerNames,
    autoScalingGroup_vPCZoneIdentifier,
    autoScalingGroup_targetGroupARNs,
    autoScalingGroup_capacityRebalance,
    autoScalingGroup_newInstancesProtectedFromScaleIn,
    autoScalingGroup_serviceLinkedRoleARN,
    autoScalingGroup_healthCheckGracePeriod,
    autoScalingGroup_enabledMetrics,
    autoScalingGroup_terminationPolicies,
    autoScalingGroup_autoScalingGroupName,
    autoScalingGroup_minSize,
    autoScalingGroup_maxSize,
    autoScalingGroup_desiredCapacity,
    autoScalingGroup_defaultCooldown,
    autoScalingGroup_availabilityZones,
    autoScalingGroup_healthCheckType,
    autoScalingGroup_createdTime,

    -- * AutoScalingInstanceDetails
    AutoScalingInstanceDetails (..),
    newAutoScalingInstanceDetails,
    autoScalingInstanceDetails_instanceType,
    autoScalingInstanceDetails_launchTemplate,
    autoScalingInstanceDetails_launchConfigurationName,
    autoScalingInstanceDetails_weightedCapacity,
    autoScalingInstanceDetails_instanceId,
    autoScalingInstanceDetails_autoScalingGroupName,
    autoScalingInstanceDetails_availabilityZone,
    autoScalingInstanceDetails_lifecycleState,
    autoScalingInstanceDetails_healthStatus,
    autoScalingInstanceDetails_protectedFromScaleIn,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    newCustomizedMetricSpecification,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- * Ebs
    Ebs (..),
    newEbs,
    ebs_encrypted,
    ebs_deleteOnTermination,
    ebs_snapshotId,
    ebs_volumeType,
    ebs_volumeSize,
    ebs_iops,

    -- * EnabledMetric
    EnabledMetric (..),
    newEnabledMetric,
    enabledMetric_granularity,
    enabledMetric_metric,

    -- * FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (..),
    newFailedScheduledUpdateGroupActionRequest,
    failedScheduledUpdateGroupActionRequest_errorMessage,
    failedScheduledUpdateGroupActionRequest_errorCode,
    failedScheduledUpdateGroupActionRequest_scheduledActionName,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * Instance
    Instance (..),
    newInstance,
    instance_instanceType,
    instance_launchTemplate,
    instance_launchConfigurationName,
    instance_weightedCapacity,
    instance_instanceId,
    instance_availabilityZone,
    instance_lifecycleState,
    instance_healthStatus,
    instance_protectedFromScaleIn,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    newInstanceMetadataOptions,
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    newInstanceMonitoring,
    instanceMonitoring_enabled,

    -- * InstanceRefresh
    InstanceRefresh (..),
    newInstanceRefresh,
    instanceRefresh_status,
    instanceRefresh_instanceRefreshId,
    instanceRefresh_percentageComplete,
    instanceRefresh_startTime,
    instanceRefresh_endTime,
    instanceRefresh_instancesToUpdate,
    instanceRefresh_statusReason,
    instanceRefresh_autoScalingGroupName,

    -- * InstancesDistribution
    InstancesDistribution (..),
    newInstancesDistribution,
    instancesDistribution_spotMaxPrice,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotAllocationStrategy,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_onDemandBaseCapacity,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_ebsOptimized,
    launchConfiguration_userData,
    launchConfiguration_ramdiskId,
    launchConfiguration_classicLinkVPCSecurityGroups,
    launchConfiguration_spotPrice,
    launchConfiguration_associatePublicIpAddress,
    launchConfiguration_securityGroups,
    launchConfiguration_iamInstanceProfile,
    launchConfiguration_classicLinkVPCId,
    launchConfiguration_blockDeviceMappings,
    launchConfiguration_kernelId,
    launchConfiguration_placementTenancy,
    launchConfiguration_keyName,
    launchConfiguration_launchConfigurationARN,
    launchConfiguration_instanceMonitoring,
    launchConfiguration_metadataOptions,
    launchConfiguration_launchConfigurationName,
    launchConfiguration_imageId,
    launchConfiguration_instanceType,
    launchConfiguration_createdTime,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_launchTemplateSpecification,
    launchTemplate_overrides,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    newLaunchTemplateOverrides,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_launchTemplateSpecification,
    launchTemplateOverrides_weightedCapacity,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- * LifecycleHook
    LifecycleHook (..),
    newLifecycleHook,
    lifecycleHook_roleARN,
    lifecycleHook_notificationTargetARN,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_heartbeatTimeout,
    lifecycleHook_globalTimeout,
    lifecycleHook_notificationMetadata,
    lifecycleHook_defaultResult,
    lifecycleHook_lifecycleHookName,
    lifecycleHook_autoScalingGroupName,

    -- * LifecycleHookSpecification
    LifecycleHookSpecification (..),
    newLifecycleHookSpecification,
    lifecycleHookSpecification_roleARN,
    lifecycleHookSpecification_notificationTargetARN,
    lifecycleHookSpecification_heartbeatTimeout,
    lifecycleHookSpecification_notificationMetadata,
    lifecycleHookSpecification_defaultResult,
    lifecycleHookSpecification_lifecycleHookName,
    lifecycleHookSpecification_lifecycleTransition,

    -- * LoadBalancerState
    LoadBalancerState (..),
    newLoadBalancerState,
    loadBalancerState_state,
    loadBalancerState_loadBalancerName,

    -- * LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState (..),
    newLoadBalancerTargetGroupState,
    loadBalancerTargetGroupState_state,
    loadBalancerTargetGroupState_loadBalancerTargetGroupARN,

    -- * MetricCollectionType
    MetricCollectionType (..),
    newMetricCollectionType,
    metricCollectionType_metric,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_name,
    metricDimension_value,

    -- * MetricGranularityType
    MetricGranularityType (..),
    newMetricGranularityType,
    metricGranularityType_granularity,

    -- * MixedInstancesPolicy
    MixedInstancesPolicy (..),
    newMixedInstancesPolicy,
    mixedInstancesPolicy_instancesDistribution,
    mixedInstancesPolicy_launchTemplate,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_notificationType,
    notificationConfiguration_topicARN,
    notificationConfiguration_autoScalingGroupName,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    newPredefinedMetricSpecification,
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- * ProcessType
    ProcessType (..),
    newProcessType,
    processType_processName,

    -- * RefreshPreferences
    RefreshPreferences (..),
    newRefreshPreferences,
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_instanceWarmup,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_policyName,
    scalingPolicy_stepAdjustments,
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_metricAggregationType,
    scalingPolicy_policyType,
    scalingPolicy_cooldown,
    scalingPolicy_enabled,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_adjustmentType,
    scalingPolicy_minAdjustmentStep,
    scalingPolicy_estimatedInstanceWarmup,
    scalingPolicy_minAdjustmentMagnitude,
    scalingPolicy_alarms,
    scalingPolicy_policyARN,
    scalingPolicy_autoScalingGroupName,

    -- * ScalingProcessQuery
    ScalingProcessQuery (..),
    newScalingProcessQuery,
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- * ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (..),
    newScheduledUpdateGroupAction,
    scheduledUpdateGroupAction_minSize,
    scheduledUpdateGroupAction_desiredCapacity,
    scheduledUpdateGroupAction_startTime,
    scheduledUpdateGroupAction_scheduledActionARN,
    scheduledUpdateGroupAction_endTime,
    scheduledUpdateGroupAction_recurrence,
    scheduledUpdateGroupAction_maxSize,
    scheduledUpdateGroupAction_scheduledActionName,
    scheduledUpdateGroupAction_time,
    scheduledUpdateGroupAction_autoScalingGroupName,

    -- * ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (..),
    newScheduledUpdateGroupActionRequest,
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_startTime,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_scheduledActionName,

    -- * StepAdjustment
    StepAdjustment (..),
    newStepAdjustment,
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_scalingAdjustment,

    -- * SuspendedProcess
    SuspendedProcess (..),
    newSuspendedProcess,
    suspendedProcess_processName,
    suspendedProcess_suspensionReason,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_resourceId,
    tag_resourceType,
    tag_propagateAtLaunch,
    tag_value,

    -- * TagDescription
    TagDescription (..),
    newTagDescription,
    tagDescription_resourceId,
    tagDescription_resourceType,
    tagDescription_key,
    tagDescription_propagateAtLaunch,
    tagDescription_value,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    newTargetTrackingConfiguration,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_targetValue,
  )
where

import Network.AWS.AutoScaling.Types.Activity
import Network.AWS.AutoScaling.Types.AdjustmentType
import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.AutoScalingGroup
import Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.Ebs
import Network.AWS.AutoScaling.Types.EnabledMetric
import Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.Filter
import Network.AWS.AutoScaling.Types.Instance
import Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
import Network.AWS.AutoScaling.Types.InstanceMetadataHttpTokensState
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.AutoScaling.Types.InstanceRefresh
import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchConfiguration
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleHook
import Network.AWS.AutoScaling.Types.LifecycleHookSpecification
import Network.AWS.AutoScaling.Types.LifecycleState
import Network.AWS.AutoScaling.Types.LoadBalancerState
import Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MetricStatistic
import Network.AWS.AutoScaling.Types.MetricType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.ProcessType
import Network.AWS.AutoScaling.Types.RefreshPreferences
import Network.AWS.AutoScaling.Types.RefreshStrategy
import Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.AutoScaling.Types.ScalingPolicy
import Network.AWS.AutoScaling.Types.ScalingProcessQuery
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.SuspendedProcess
import Network.AWS.AutoScaling.Types.Tag
import Network.AWS.AutoScaling.Types.TagDescription
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2011-01-01@ of the Amazon Auto Scaling SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AutoScaling",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "autoscaling",
      Core._serviceSigningName = "autoscaling",
      Core._serviceVersion = "2011-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseXMLError "AutoScaling",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | You already have an Auto Scaling group or launch configuration with this
-- name.
_AlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AlreadyExists"
    Core.. Core.hasStatus 400

-- | The operation can\'t be performed because the resource is in use.
_ResourceInUseFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseFault =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Core.. Core.hasStatus 400

-- | You have already reached a limit for your Amazon EC2 Auto Scaling
-- resources (for example, Auto Scaling groups, launch configurations, or
-- lifecycle hooks). For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_DescribeAccountLimits.html DescribeAccountLimits>
-- in the /Amazon EC2 Auto Scaling API Reference/.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Core.. Core.hasStatus 400

-- | The request failed because an active instance refresh operation already
-- exists for the specified Auto Scaling group.
_InstanceRefreshInProgressFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceRefreshInProgressFault =
  Core._MatchServiceError
    defaultService
    "InstanceRefreshInProgress"
    Core.. Core.hasStatus 400

-- | The operation can\'t be performed because there are scaling activities
-- in progress.
_ScalingActivityInProgressFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScalingActivityInProgressFault =
  Core._MatchServiceError
    defaultService
    "ScalingActivityInProgress"
    Core.. Core.hasStatus 400

-- | The request failed because an active instance refresh for the specified
-- Auto Scaling group was not found.
_ActiveInstanceRefreshNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActiveInstanceRefreshNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ActiveInstanceRefreshNotFound"
    Core.. Core.hasStatus 400

-- | You already have a pending update to an Amazon EC2 Auto Scaling resource
-- (for example, an Auto Scaling group, instance, or load balancer).
_ResourceContentionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceContentionFault =
  Core._MatchServiceError
    defaultService
    "ResourceContention"
    Core.. Core.hasStatus 500

-- | The service-linked role is not yet ready for use.
_ServiceLinkedRoleFailure :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleFailure =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleFailure"
    Core.. Core.hasStatus 500

-- | The @NextToken@ value is not valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Core.. Core.hasStatus 400
