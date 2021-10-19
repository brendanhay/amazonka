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
    _InstanceRefreshInProgressFault,
    _AlreadyExistsFault,
    _LimitExceededFault,
    _ResourceInUseFault,
    _InvalidNextToken,
    _ScalingActivityInProgressFault,
    _ResourceContentionFault,
    _ServiceLinkedRoleFailure,
    _ActiveInstanceRefreshNotFoundFault,

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

    -- * PredefinedLoadMetricType
    PredefinedLoadMetricType (..),

    -- * PredefinedMetricPairType
    PredefinedMetricPairType (..),

    -- * PredefinedScalingMetricType
    PredefinedScalingMetricType (..),

    -- * PredictiveScalingMaxCapacityBreachBehavior
    PredictiveScalingMaxCapacityBreachBehavior (..),

    -- * PredictiveScalingMode
    PredictiveScalingMode (..),

    -- * RefreshStrategy
    RefreshStrategy (..),

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * WarmPoolState
    WarmPoolState (..),

    -- * WarmPoolStatus
    WarmPoolStatus (..),

    -- * Activity
    Activity (..),
    newActivity,
    activity_progress,
    activity_statusMessage,
    activity_autoScalingGroupState,
    activity_endTime,
    activity_details,
    activity_autoScalingGroupARN,
    activity_description,
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
    alarm_alarmName,
    alarm_alarmARN,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_status,
    autoScalingGroup_context,
    autoScalingGroup_terminationPolicies,
    autoScalingGroup_healthCheckGracePeriod,
    autoScalingGroup_serviceLinkedRoleARN,
    autoScalingGroup_newInstancesProtectedFromScaleIn,
    autoScalingGroup_vPCZoneIdentifier,
    autoScalingGroup_targetGroupARNs,
    autoScalingGroup_maxInstanceLifetime,
    autoScalingGroup_mixedInstancesPolicy,
    autoScalingGroup_enabledMetrics,
    autoScalingGroup_launchConfigurationName,
    autoScalingGroup_instances,
    autoScalingGroup_launchTemplate,
    autoScalingGroup_warmPoolConfiguration,
    autoScalingGroup_capacityRebalance,
    autoScalingGroup_autoScalingGroupARN,
    autoScalingGroup_predictedCapacity,
    autoScalingGroup_warmPoolSize,
    autoScalingGroup_placementGroup,
    autoScalingGroup_suspendedProcesses,
    autoScalingGroup_loadBalancerNames,
    autoScalingGroup_tags,
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
    autoScalingInstanceDetails_weightedCapacity,
    autoScalingInstanceDetails_instanceType,
    autoScalingInstanceDetails_launchConfigurationName,
    autoScalingInstanceDetails_launchTemplate,
    autoScalingInstanceDetails_instanceId,
    autoScalingInstanceDetails_autoScalingGroupName,
    autoScalingInstanceDetails_availabilityZone,
    autoScalingInstanceDetails_lifecycleState,
    autoScalingInstanceDetails_healthStatus,
    autoScalingInstanceDetails_protectedFromScaleIn,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    newBlockDeviceMapping,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,

    -- * CapacityForecast
    CapacityForecast (..),
    newCapacityForecast,
    capacityForecast_timestamps,
    capacityForecast_values,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    newCustomizedMetricSpecification,
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- * DesiredConfiguration
    DesiredConfiguration (..),
    newDesiredConfiguration,
    desiredConfiguration_mixedInstancesPolicy,
    desiredConfiguration_launchTemplate,

    -- * Ebs
    Ebs (..),
    newEbs,
    ebs_deleteOnTermination,
    ebs_throughput,
    ebs_volumeSize,
    ebs_iops,
    ebs_encrypted,
    ebs_volumeType,
    ebs_snapshotId,

    -- * EnabledMetric
    EnabledMetric (..),
    newEnabledMetric,
    enabledMetric_granularity,
    enabledMetric_metric,

    -- * FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (..),
    newFailedScheduledUpdateGroupActionRequest,
    failedScheduledUpdateGroupActionRequest_errorCode,
    failedScheduledUpdateGroupActionRequest_errorMessage,
    failedScheduledUpdateGroupActionRequest_scheduledActionName,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * Instance
    Instance (..),
    newInstance,
    instance_weightedCapacity,
    instance_instanceType,
    instance_launchConfigurationName,
    instance_launchTemplate,
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
    instanceRefresh_progressDetails,
    instanceRefresh_startTime,
    instanceRefresh_preferences,
    instanceRefresh_instancesToUpdate,
    instanceRefresh_percentageComplete,
    instanceRefresh_autoScalingGroupName,
    instanceRefresh_endTime,
    instanceRefresh_statusReason,
    instanceRefresh_desiredConfiguration,
    instanceRefresh_instanceRefreshId,

    -- * InstanceRefreshLivePoolProgress
    InstanceRefreshLivePoolProgress (..),
    newInstanceRefreshLivePoolProgress,
    instanceRefreshLivePoolProgress_instancesToUpdate,
    instanceRefreshLivePoolProgress_percentageComplete,

    -- * InstanceRefreshProgressDetails
    InstanceRefreshProgressDetails (..),
    newInstanceRefreshProgressDetails,
    instanceRefreshProgressDetails_livePoolProgress,
    instanceRefreshProgressDetails_warmPoolProgress,

    -- * InstanceRefreshWarmPoolProgress
    InstanceRefreshWarmPoolProgress (..),
    newInstanceRefreshWarmPoolProgress,
    instanceRefreshWarmPoolProgress_instancesToUpdate,
    instanceRefreshWarmPoolProgress_percentageComplete,

    -- * InstancesDistribution
    InstancesDistribution (..),
    newInstancesDistribution,
    instancesDistribution_spotAllocationStrategy,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotMaxPrice,
    instancesDistribution_onDemandBaseCapacity,
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_associatePublicIpAddress,
    launchConfiguration_securityGroups,
    launchConfiguration_spotPrice,
    launchConfiguration_instanceMonitoring,
    launchConfiguration_keyName,
    launchConfiguration_classicLinkVPCSecurityGroups,
    launchConfiguration_ramdiskId,
    launchConfiguration_kernelId,
    launchConfiguration_ebsOptimized,
    launchConfiguration_userData,
    launchConfiguration_classicLinkVPCId,
    launchConfiguration_iamInstanceProfile,
    launchConfiguration_metadataOptions,
    launchConfiguration_launchConfigurationARN,
    launchConfiguration_placementTenancy,
    launchConfiguration_blockDeviceMappings,
    launchConfiguration_launchConfigurationName,
    launchConfiguration_imageId,
    launchConfiguration_instanceType,
    launchConfiguration_createdTime,

    -- * LaunchTemplate
    LaunchTemplate (..),
    newLaunchTemplate,
    launchTemplate_overrides,
    launchTemplate_launchTemplateSpecification,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    newLaunchTemplateOverrides,
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_launchTemplateSpecification,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- * LifecycleHook
    LifecycleHook (..),
    newLifecycleHook,
    lifecycleHook_defaultResult,
    lifecycleHook_lifecycleHookName,
    lifecycleHook_heartbeatTimeout,
    lifecycleHook_autoScalingGroupName,
    lifecycleHook_notificationMetadata,
    lifecycleHook_globalTimeout,
    lifecycleHook_notificationTargetARN,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_roleARN,

    -- * LifecycleHookSpecification
    LifecycleHookSpecification (..),
    newLifecycleHookSpecification,
    lifecycleHookSpecification_defaultResult,
    lifecycleHookSpecification_heartbeatTimeout,
    lifecycleHookSpecification_notificationMetadata,
    lifecycleHookSpecification_notificationTargetARN,
    lifecycleHookSpecification_roleARN,
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

    -- * LoadForecast
    LoadForecast (..),
    newLoadForecast,
    loadForecast_timestamps,
    loadForecast_values,
    loadForecast_metricSpecification,

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
    mixedInstancesPolicy_launchTemplate,
    mixedInstancesPolicy_instancesDistribution,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicARN,
    notificationConfiguration_autoScalingGroupName,
    notificationConfiguration_notificationType,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    newPredefinedMetricSpecification,
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- * PredictiveScalingConfiguration
    PredictiveScalingConfiguration (..),
    newPredictiveScalingConfiguration,
    predictiveScalingConfiguration_schedulingBufferTime,
    predictiveScalingConfiguration_maxCapacityBuffer,
    predictiveScalingConfiguration_mode,
    predictiveScalingConfiguration_maxCapacityBreachBehavior,
    predictiveScalingConfiguration_metricSpecifications,

    -- * PredictiveScalingMetricSpecification
    PredictiveScalingMetricSpecification (..),
    newPredictiveScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedMetricPairSpecification,
    predictiveScalingMetricSpecification_predefinedLoadMetricSpecification,
    predictiveScalingMetricSpecification_targetValue,

    -- * PredictiveScalingPredefinedLoadMetric
    PredictiveScalingPredefinedLoadMetric (..),
    newPredictiveScalingPredefinedLoadMetric,
    predictiveScalingPredefinedLoadMetric_resourceLabel,
    predictiveScalingPredefinedLoadMetric_predefinedMetricType,

    -- * PredictiveScalingPredefinedMetricPair
    PredictiveScalingPredefinedMetricPair (..),
    newPredictiveScalingPredefinedMetricPair,
    predictiveScalingPredefinedMetricPair_resourceLabel,
    predictiveScalingPredefinedMetricPair_predefinedMetricType,

    -- * PredictiveScalingPredefinedScalingMetric
    PredictiveScalingPredefinedScalingMetric (..),
    newPredictiveScalingPredefinedScalingMetric,
    predictiveScalingPredefinedScalingMetric_resourceLabel,
    predictiveScalingPredefinedScalingMetric_predefinedMetricType,

    -- * ProcessType
    ProcessType (..),
    newProcessType,
    processType_processName,

    -- * RefreshPreferences
    RefreshPreferences (..),
    newRefreshPreferences,
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_skipMatching,
    refreshPreferences_checkpointPercentages,
    refreshPreferences_checkpointDelay,
    refreshPreferences_instanceWarmup,

    -- * ScalingPolicy
    ScalingPolicy (..),
    newScalingPolicy,
    scalingPolicy_minAdjustmentStep,
    scalingPolicy_estimatedInstanceWarmup,
    scalingPolicy_policyName,
    scalingPolicy_enabled,
    scalingPolicy_policyType,
    scalingPolicy_stepAdjustments,
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_adjustmentType,
    scalingPolicy_autoScalingGroupName,
    scalingPolicy_predictiveScalingConfiguration,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_cooldown,
    scalingPolicy_policyARN,
    scalingPolicy_alarms,
    scalingPolicy_metricAggregationType,
    scalingPolicy_minAdjustmentMagnitude,

    -- * ScalingProcessQuery
    ScalingProcessQuery (..),
    newScalingProcessQuery,
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- * ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (..),
    newScheduledUpdateGroupAction,
    scheduledUpdateGroupAction_scheduledActionARN,
    scheduledUpdateGroupAction_startTime,
    scheduledUpdateGroupAction_time,
    scheduledUpdateGroupAction_scheduledActionName,
    scheduledUpdateGroupAction_maxSize,
    scheduledUpdateGroupAction_recurrence,
    scheduledUpdateGroupAction_desiredCapacity,
    scheduledUpdateGroupAction_minSize,
    scheduledUpdateGroupAction_autoScalingGroupName,
    scheduledUpdateGroupAction_endTime,
    scheduledUpdateGroupAction_timeZone,

    -- * ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (..),
    newScheduledUpdateGroupActionRequest,
    scheduledUpdateGroupActionRequest_startTime,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_timeZone,
    scheduledUpdateGroupActionRequest_scheduledActionName,

    -- * StepAdjustment
    StepAdjustment (..),
    newStepAdjustment,
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_metricIntervalUpperBound,
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
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_targetValue,

    -- * WarmPoolConfiguration
    WarmPoolConfiguration (..),
    newWarmPoolConfiguration,
    warmPoolConfiguration_status,
    warmPoolConfiguration_minSize,
    warmPoolConfiguration_maxGroupPreparedCapacity,
    warmPoolConfiguration_poolState,
  )
where

import Network.AWS.AutoScaling.Types.Activity
import Network.AWS.AutoScaling.Types.AdjustmentType
import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.AutoScalingGroup
import Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.CapacityForecast
import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.DesiredConfiguration
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
import Network.AWS.AutoScaling.Types.InstanceRefreshLivePoolProgress
import Network.AWS.AutoScaling.Types.InstanceRefreshProgressDetails
import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.AutoScaling.Types.InstanceRefreshWarmPoolProgress
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
import Network.AWS.AutoScaling.Types.LoadForecast
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MetricStatistic
import Network.AWS.AutoScaling.Types.MetricType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.PredefinedLoadMetricType
import Network.AWS.AutoScaling.Types.PredefinedMetricPairType
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.PredefinedScalingMetricType
import Network.AWS.AutoScaling.Types.PredictiveScalingConfiguration
import Network.AWS.AutoScaling.Types.PredictiveScalingMaxCapacityBreachBehavior
import Network.AWS.AutoScaling.Types.PredictiveScalingMetricSpecification
import Network.AWS.AutoScaling.Types.PredictiveScalingMode
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedScalingMetric
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
import Network.AWS.AutoScaling.Types.WarmPoolConfiguration
import Network.AWS.AutoScaling.Types.WarmPoolState
import Network.AWS.AutoScaling.Types.WarmPoolStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
      Core._serviceTimeout = Prelude.Just 70,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request failed because an active instance refresh operation already
-- exists for the specified Auto Scaling group.
_InstanceRefreshInProgressFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceRefreshInProgressFault =
  Core._MatchServiceError
    defaultService
    "InstanceRefreshInProgress"
    Prelude.. Core.hasStatus 400

-- | You already have an Auto Scaling group or launch configuration with this
-- name.
_AlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You have already reached a limit for your Amazon EC2 Auto Scaling
-- resources (for example, Auto Scaling groups, launch configurations, or
-- lifecycle hooks). For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_DescribeAccountLimits.html DescribeAccountLimits>
-- in the /Amazon EC2 Auto Scaling API Reference/.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The operation can\'t be performed because the resource is in use.
_ResourceInUseFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseFault =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Core.hasStatus 400

-- | The @NextToken@ value is not valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Core.hasStatus 400

-- | The operation can\'t be performed because there are scaling activities
-- in progress.
_ScalingActivityInProgressFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScalingActivityInProgressFault =
  Core._MatchServiceError
    defaultService
    "ScalingActivityInProgress"
    Prelude.. Core.hasStatus 400

-- | You already have a pending update to an Amazon EC2 Auto Scaling resource
-- (for example, an Auto Scaling group, instance, or load balancer).
_ResourceContentionFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceContentionFault =
  Core._MatchServiceError
    defaultService
    "ResourceContention"
    Prelude.. Core.hasStatus 500

-- | The service-linked role is not yet ready for use.
_ServiceLinkedRoleFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleFailure =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleFailure"
    Prelude.. Core.hasStatus 500

-- | The request failed because an active instance refresh for the specified
-- Auto Scaling group was not found.
_ActiveInstanceRefreshNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActiveInstanceRefreshNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ActiveInstanceRefreshNotFound"
    Prelude.. Core.hasStatus 400
