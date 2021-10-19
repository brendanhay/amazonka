{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Lens
  ( -- * Operations

    -- ** PutWarmPool
    putWarmPool_minSize,
    putWarmPool_maxGroupPreparedCapacity,
    putWarmPool_poolState,
    putWarmPool_autoScalingGroupName,
    putWarmPoolResponse_httpStatus,

    -- ** DescribeMetricCollectionTypes
    describeMetricCollectionTypesResponse_metrics,
    describeMetricCollectionTypesResponse_granularities,
    describeMetricCollectionTypesResponse_httpStatus,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_nextToken,
    describeLoadBalancers_maxRecords,
    describeLoadBalancers_autoScalingGroupName,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_nextToken,
    describeLoadBalancersResponse_httpStatus,

    -- ** PutNotificationConfiguration
    putNotificationConfiguration_autoScalingGroupName,
    putNotificationConfiguration_topicARN,
    putNotificationConfiguration_notificationTypes,

    -- ** DescribeTags
    describeTags_filters,
    describeTags_nextToken,
    describeTags_maxRecords,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DeleteNotificationConfiguration
    deleteNotificationConfiguration_autoScalingGroupName,
    deleteNotificationConfiguration_topicARN,

    -- ** DeleteWarmPool
    deleteWarmPool_forceDelete,
    deleteWarmPool_autoScalingGroupName,
    deleteWarmPoolResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_minAdjustmentStep,
    putScalingPolicy_estimatedInstanceWarmup,
    putScalingPolicy_enabled,
    putScalingPolicy_policyType,
    putScalingPolicy_stepAdjustments,
    putScalingPolicy_targetTrackingConfiguration,
    putScalingPolicy_adjustmentType,
    putScalingPolicy_predictiveScalingConfiguration,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_cooldown,
    putScalingPolicy_metricAggregationType,
    putScalingPolicy_minAdjustmentMagnitude,
    putScalingPolicy_autoScalingGroupName,
    putScalingPolicy_policyName,
    putScalingPolicyResponse_policyARN,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_httpStatus,

    -- ** StartInstanceRefresh
    startInstanceRefresh_preferences,
    startInstanceRefresh_strategy,
    startInstanceRefresh_desiredConfiguration,
    startInstanceRefresh_autoScalingGroupName,
    startInstanceRefreshResponse_instanceRefreshId,
    startInstanceRefreshResponse_httpStatus,

    -- ** AttachLoadBalancerTargetGroups
    attachLoadBalancerTargetGroups_autoScalingGroupName,
    attachLoadBalancerTargetGroups_targetGroupARNs,
    attachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DeleteLaunchConfiguration
    deleteLaunchConfiguration_launchConfigurationName,

    -- ** EnterStandby
    enterStandby_instanceIds,
    enterStandby_autoScalingGroupName,
    enterStandby_shouldDecrementDesiredCapacity,
    enterStandbyResponse_activities,
    enterStandbyResponse_httpStatus,

    -- ** SuspendProcesses
    suspendProcesses_scalingProcesses,
    suspendProcesses_autoScalingGroupName,

    -- ** SetInstanceHealth
    setInstanceHealth_shouldRespectGracePeriod,
    setInstanceHealth_instanceId,
    setInstanceHealth_healthStatus,

    -- ** ExitStandby
    exitStandby_instanceIds,
    exitStandby_autoScalingGroupName,
    exitStandbyResponse_activities,
    exitStandbyResponse_httpStatus,

    -- ** DescribeTerminationPolicyTypes
    describeTerminationPolicyTypesResponse_terminationPolicyTypes,
    describeTerminationPolicyTypesResponse_httpStatus,

    -- ** CancelInstanceRefresh
    cancelInstanceRefresh_autoScalingGroupName,
    cancelInstanceRefreshResponse_instanceRefreshId,
    cancelInstanceRefreshResponse_httpStatus,

    -- ** DescribeAutoScalingInstances
    describeAutoScalingInstances_nextToken,
    describeAutoScalingInstances_instanceIds,
    describeAutoScalingInstances_maxRecords,
    describeAutoScalingInstancesResponse_nextToken,
    describeAutoScalingInstancesResponse_autoScalingInstances,
    describeAutoScalingInstancesResponse_httpStatus,

    -- ** RecordLifecycleActionHeartbeat
    recordLifecycleActionHeartbeat_instanceId,
    recordLifecycleActionHeartbeat_lifecycleActionToken,
    recordLifecycleActionHeartbeat_lifecycleHookName,
    recordLifecycleActionHeartbeat_autoScalingGroupName,
    recordLifecycleActionHeartbeatResponse_httpStatus,

    -- ** DisableMetricsCollection
    disableMetricsCollection_metrics,
    disableMetricsCollection_autoScalingGroupName,

    -- ** DetachInstances
    detachInstances_instanceIds,
    detachInstances_autoScalingGroupName,
    detachInstances_shouldDecrementDesiredCapacity,
    detachInstancesResponse_activities,
    detachInstancesResponse_httpStatus,

    -- ** EnableMetricsCollection
    enableMetricsCollection_metrics,
    enableMetricsCollection_autoScalingGroupName,
    enableMetricsCollection_granularity,

    -- ** DescribeScalingProcessTypes
    describeScalingProcessTypesResponse_processes,
    describeScalingProcessTypesResponse_httpStatus,

    -- ** DescribeWarmPool
    describeWarmPool_nextToken,
    describeWarmPool_maxRecords,
    describeWarmPool_autoScalingGroupName,
    describeWarmPoolResponse_nextToken,
    describeWarmPoolResponse_instances,
    describeWarmPoolResponse_warmPoolConfiguration,
    describeWarmPoolResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tags,

    -- ** DetachLoadBalancerTargetGroups
    detachLoadBalancerTargetGroups_autoScalingGroupName,
    detachLoadBalancerTargetGroups_targetGroupARNs,
    detachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DescribeLifecycleHooks
    describeLifecycleHooks_lifecycleHookNames,
    describeLifecycleHooks_autoScalingGroupName,
    describeLifecycleHooksResponse_lifecycleHooks,
    describeLifecycleHooksResponse_httpStatus,

    -- ** DescribeAutoScalingGroups
    describeAutoScalingGroups_filters,
    describeAutoScalingGroups_autoScalingGroupNames,
    describeAutoScalingGroups_nextToken,
    describeAutoScalingGroups_maxRecords,
    describeAutoScalingGroupsResponse_nextToken,
    describeAutoScalingGroupsResponse_httpStatus,
    describeAutoScalingGroupsResponse_autoScalingGroups,

    -- ** DeleteScheduledAction
    deleteScheduledAction_autoScalingGroupName,
    deleteScheduledAction_scheduledActionName,

    -- ** SetDesiredCapacity
    setDesiredCapacity_honorCooldown,
    setDesiredCapacity_autoScalingGroupName,
    setDesiredCapacity_desiredCapacity,

    -- ** DetachLoadBalancers
    detachLoadBalancers_autoScalingGroupName,
    detachLoadBalancers_loadBalancerNames,
    detachLoadBalancersResponse_httpStatus,

    -- ** DescribeAutoScalingNotificationTypes
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,

    -- ** DescribeScheduledActions
    describeScheduledActions_startTime,
    describeScheduledActions_nextToken,
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActions_maxRecords,
    describeScheduledActions_endTime,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_httpStatus,

    -- ** CreateOrUpdateTags
    createOrUpdateTags_tags,

    -- ** CompleteLifecycleAction
    completeLifecycleAction_instanceId,
    completeLifecycleAction_lifecycleActionToken,
    completeLifecycleAction_lifecycleHookName,
    completeLifecycleAction_autoScalingGroupName,
    completeLifecycleAction_lifecycleActionResult,
    completeLifecycleActionResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_autoScalingGroupName,
    deletePolicy_policyName,

    -- ** AttachInstances
    attachInstances_instanceIds,
    attachInstances_autoScalingGroupName,

    -- ** UpdateAutoScalingGroup
    updateAutoScalingGroup_context,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_availabilityZones,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_autoScalingGroupName,

    -- ** DeleteAutoScalingGroup
    deleteAutoScalingGroup_forceDelete,
    deleteAutoScalingGroup_autoScalingGroupName,

    -- ** PutLifecycleHook
    putLifecycleHook_defaultResult,
    putLifecycleHook_heartbeatTimeout,
    putLifecycleHook_notificationMetadata,
    putLifecycleHook_notificationTargetARN,
    putLifecycleHook_lifecycleTransition,
    putLifecycleHook_roleARN,
    putLifecycleHook_lifecycleHookName,
    putLifecycleHook_autoScalingGroupName,
    putLifecycleHookResponse_httpStatus,

    -- ** BatchPutScheduledUpdateGroupAction
    batchPutScheduledUpdateGroupAction_autoScalingGroupName,
    batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_httpStatus,

    -- ** DeleteLifecycleHook
    deleteLifecycleHook_lifecycleHookName,
    deleteLifecycleHook_autoScalingGroupName,
    deleteLifecycleHookResponse_httpStatus,

    -- ** ResumeProcesses
    resumeProcesses_scalingProcesses,
    resumeProcesses_autoScalingGroupName,

    -- ** ExecutePolicy
    executePolicy_honorCooldown,
    executePolicy_metricValue,
    executePolicy_autoScalingGroupName,
    executePolicy_breachThreshold,
    executePolicy_policyName,

    -- ** GetPredictiveScalingForecast
    getPredictiveScalingForecast_autoScalingGroupName,
    getPredictiveScalingForecast_policyName,
    getPredictiveScalingForecast_startTime,
    getPredictiveScalingForecast_endTime,
    getPredictiveScalingForecastResponse_httpStatus,
    getPredictiveScalingForecastResponse_loadForecast,
    getPredictiveScalingForecastResponse_capacityForecast,
    getPredictiveScalingForecastResponse_updateTime,

    -- ** DescribeInstanceRefreshes
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_autoScalingGroupName,
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_httpStatus,

    -- ** AttachLoadBalancers
    attachLoadBalancers_autoScalingGroupName,
    attachLoadBalancers_loadBalancerNames,
    attachLoadBalancersResponse_httpStatus,

    -- ** BatchDeleteScheduledAction
    batchDeleteScheduledAction_autoScalingGroupName,
    batchDeleteScheduledAction_scheduledActionNames,
    batchDeleteScheduledActionResponse_failedScheduledActions,
    batchDeleteScheduledActionResponse_httpStatus,

    -- ** TerminateInstanceInAutoScalingGroup
    terminateInstanceInAutoScalingGroup_instanceId,
    terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity,
    terminateInstanceInAutoScalingGroupResponse_activity,
    terminateInstanceInAutoScalingGroupResponse_httpStatus,

    -- ** DescribeLoadBalancerTargetGroups
    describeLoadBalancerTargetGroups_nextToken,
    describeLoadBalancerTargetGroups_maxRecords,
    describeLoadBalancerTargetGroups_autoScalingGroupName,
    describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups,
    describeLoadBalancerTargetGroupsResponse_nextToken,
    describeLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** PutScheduledUpdateGroupAction
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_timeZone,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- ** SetInstanceProtection
    setInstanceProtection_instanceIds,
    setInstanceProtection_autoScalingGroupName,
    setInstanceProtection_protectedFromScaleIn,
    setInstanceProtectionResponse_httpStatus,

    -- ** DescribePolicies
    describePolicies_policyNames,
    describePolicies_nextToken,
    describePolicies_autoScalingGroupName,
    describePolicies_maxRecords,
    describePolicies_policyTypes,
    describePoliciesResponse_nextToken,
    describePoliciesResponse_scalingPolicies,
    describePoliciesResponse_httpStatus,

    -- ** DescribeLaunchConfigurations
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurations_nextToken,
    describeLaunchConfigurations_maxRecords,
    describeLaunchConfigurationsResponse_nextToken,
    describeLaunchConfigurationsResponse_httpStatus,
    describeLaunchConfigurationsResponse_launchConfigurations,

    -- ** DescribeScalingActivities
    describeScalingActivities_nextToken,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivities_maxRecords,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_activityIds,
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,

    -- ** DescribeNotificationConfigurations
    describeNotificationConfigurations_autoScalingGroupNames,
    describeNotificationConfigurations_nextToken,
    describeNotificationConfigurations_maxRecords,
    describeNotificationConfigurationsResponse_nextToken,
    describeNotificationConfigurationsResponse_httpStatus,
    describeNotificationConfigurationsResponse_notificationConfigurations,

    -- ** DescribeLifecycleHookTypes
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,

    -- ** DescribeAdjustmentTypes
    describeAdjustmentTypesResponse_adjustmentTypes,
    describeAdjustmentTypesResponse_httpStatus,

    -- ** CreateAutoScalingGroup
    createAutoScalingGroup_instanceId,
    createAutoScalingGroup_context,
    createAutoScalingGroup_terminationPolicies,
    createAutoScalingGroup_healthCheckGracePeriod,
    createAutoScalingGroup_serviceLinkedRoleARN,
    createAutoScalingGroup_newInstancesProtectedFromScaleIn,
    createAutoScalingGroup_vPCZoneIdentifier,
    createAutoScalingGroup_targetGroupARNs,
    createAutoScalingGroup_maxInstanceLifetime,
    createAutoScalingGroup_defaultCooldown,
    createAutoScalingGroup_availabilityZones,
    createAutoScalingGroup_desiredCapacity,
    createAutoScalingGroup_mixedInstancesPolicy,
    createAutoScalingGroup_launchConfigurationName,
    createAutoScalingGroup_lifecycleHookSpecificationList,
    createAutoScalingGroup_healthCheckType,
    createAutoScalingGroup_launchTemplate,
    createAutoScalingGroup_capacityRebalance,
    createAutoScalingGroup_placementGroup,
    createAutoScalingGroup_loadBalancerNames,
    createAutoScalingGroup_tags,
    createAutoScalingGroup_autoScalingGroupName,
    createAutoScalingGroup_minSize,
    createAutoScalingGroup_maxSize,

    -- ** CreateLaunchConfiguration
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_userData,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_launchConfigurationName,

    -- * Types

    -- ** Activity
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

    -- ** AdjustmentType
    adjustmentType_adjustmentType,

    -- ** Alarm
    alarm_alarmName,
    alarm_alarmARN,

    -- ** AutoScalingGroup
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

    -- ** AutoScalingInstanceDetails
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

    -- ** BlockDeviceMapping
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,

    -- ** CapacityForecast
    capacityForecast_timestamps,
    capacityForecast_values,

    -- ** CustomizedMetricSpecification
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- ** DesiredConfiguration
    desiredConfiguration_mixedInstancesPolicy,
    desiredConfiguration_launchTemplate,

    -- ** Ebs
    ebs_deleteOnTermination,
    ebs_throughput,
    ebs_volumeSize,
    ebs_iops,
    ebs_encrypted,
    ebs_volumeType,
    ebs_snapshotId,

    -- ** EnabledMetric
    enabledMetric_granularity,
    enabledMetric_metric,

    -- ** FailedScheduledUpdateGroupActionRequest
    failedScheduledUpdateGroupActionRequest_errorCode,
    failedScheduledUpdateGroupActionRequest_errorMessage,
    failedScheduledUpdateGroupActionRequest_scheduledActionName,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** Instance
    instance_weightedCapacity,
    instance_instanceType,
    instance_launchConfigurationName,
    instance_launchTemplate,
    instance_instanceId,
    instance_availabilityZone,
    instance_lifecycleState,
    instance_healthStatus,
    instance_protectedFromScaleIn,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,

    -- ** InstanceMonitoring
    instanceMonitoring_enabled,

    -- ** InstanceRefresh
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

    -- ** InstanceRefreshLivePoolProgress
    instanceRefreshLivePoolProgress_instancesToUpdate,
    instanceRefreshLivePoolProgress_percentageComplete,

    -- ** InstanceRefreshProgressDetails
    instanceRefreshProgressDetails_livePoolProgress,
    instanceRefreshProgressDetails_warmPoolProgress,

    -- ** InstanceRefreshWarmPoolProgress
    instanceRefreshWarmPoolProgress_instancesToUpdate,
    instanceRefreshWarmPoolProgress_percentageComplete,

    -- ** InstancesDistribution
    instancesDistribution_spotAllocationStrategy,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotMaxPrice,
    instancesDistribution_onDemandBaseCapacity,
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,

    -- ** LaunchConfiguration
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

    -- ** LaunchTemplate
    launchTemplate_overrides,
    launchTemplate_launchTemplateSpecification,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_launchTemplateSpecification,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,

    -- ** LifecycleHook
    lifecycleHook_defaultResult,
    lifecycleHook_lifecycleHookName,
    lifecycleHook_heartbeatTimeout,
    lifecycleHook_autoScalingGroupName,
    lifecycleHook_notificationMetadata,
    lifecycleHook_globalTimeout,
    lifecycleHook_notificationTargetARN,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_roleARN,

    -- ** LifecycleHookSpecification
    lifecycleHookSpecification_defaultResult,
    lifecycleHookSpecification_heartbeatTimeout,
    lifecycleHookSpecification_notificationMetadata,
    lifecycleHookSpecification_notificationTargetARN,
    lifecycleHookSpecification_roleARN,
    lifecycleHookSpecification_lifecycleHookName,
    lifecycleHookSpecification_lifecycleTransition,

    -- ** LoadBalancerState
    loadBalancerState_state,
    loadBalancerState_loadBalancerName,

    -- ** LoadBalancerTargetGroupState
    loadBalancerTargetGroupState_state,
    loadBalancerTargetGroupState_loadBalancerTargetGroupARN,

    -- ** LoadForecast
    loadForecast_timestamps,
    loadForecast_values,
    loadForecast_metricSpecification,

    -- ** MetricCollectionType
    metricCollectionType_metric,

    -- ** MetricDimension
    metricDimension_name,
    metricDimension_value,

    -- ** MetricGranularityType
    metricGranularityType_granularity,

    -- ** MixedInstancesPolicy
    mixedInstancesPolicy_launchTemplate,
    mixedInstancesPolicy_instancesDistribution,

    -- ** NotificationConfiguration
    notificationConfiguration_topicARN,
    notificationConfiguration_autoScalingGroupName,
    notificationConfiguration_notificationType,

    -- ** PredefinedMetricSpecification
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- ** PredictiveScalingConfiguration
    predictiveScalingConfiguration_schedulingBufferTime,
    predictiveScalingConfiguration_maxCapacityBuffer,
    predictiveScalingConfiguration_mode,
    predictiveScalingConfiguration_maxCapacityBreachBehavior,
    predictiveScalingConfiguration_metricSpecifications,

    -- ** PredictiveScalingMetricSpecification
    predictiveScalingMetricSpecification_predefinedScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedMetricPairSpecification,
    predictiveScalingMetricSpecification_predefinedLoadMetricSpecification,
    predictiveScalingMetricSpecification_targetValue,

    -- ** PredictiveScalingPredefinedLoadMetric
    predictiveScalingPredefinedLoadMetric_resourceLabel,
    predictiveScalingPredefinedLoadMetric_predefinedMetricType,

    -- ** PredictiveScalingPredefinedMetricPair
    predictiveScalingPredefinedMetricPair_resourceLabel,
    predictiveScalingPredefinedMetricPair_predefinedMetricType,

    -- ** PredictiveScalingPredefinedScalingMetric
    predictiveScalingPredefinedScalingMetric_resourceLabel,
    predictiveScalingPredefinedScalingMetric_predefinedMetricType,

    -- ** ProcessType
    processType_processName,

    -- ** RefreshPreferences
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_skipMatching,
    refreshPreferences_checkpointPercentages,
    refreshPreferences_checkpointDelay,
    refreshPreferences_instanceWarmup,

    -- ** ScalingPolicy
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

    -- ** ScalingProcessQuery
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- ** ScheduledUpdateGroupAction
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

    -- ** ScheduledUpdateGroupActionRequest
    scheduledUpdateGroupActionRequest_startTime,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_timeZone,
    scheduledUpdateGroupActionRequest_scheduledActionName,

    -- ** StepAdjustment
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_scalingAdjustment,

    -- ** SuspendedProcess
    suspendedProcess_processName,
    suspendedProcess_suspensionReason,

    -- ** Tag
    tag_key,
    tag_resourceId,
    tag_resourceType,
    tag_propagateAtLaunch,
    tag_value,

    -- ** TagDescription
    tagDescription_resourceId,
    tagDescription_resourceType,
    tagDescription_key,
    tagDescription_propagateAtLaunch,
    tagDescription_value,

    -- ** TargetTrackingConfiguration
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_targetValue,

    -- ** WarmPoolConfiguration
    warmPoolConfiguration_status,
    warmPoolConfiguration_minSize,
    warmPoolConfiguration_maxGroupPreparedCapacity,
    warmPoolConfiguration_poolState,
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
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.AutoScaling.Types.InstanceRefresh
import Network.AWS.AutoScaling.Types.InstanceRefreshLivePoolProgress
import Network.AWS.AutoScaling.Types.InstanceRefreshProgressDetails
import Network.AWS.AutoScaling.Types.InstanceRefreshWarmPoolProgress
import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchConfiguration
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleHook
import Network.AWS.AutoScaling.Types.LifecycleHookSpecification
import Network.AWS.AutoScaling.Types.LoadBalancerState
import Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
import Network.AWS.AutoScaling.Types.LoadForecast
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.PredictiveScalingConfiguration
import Network.AWS.AutoScaling.Types.PredictiveScalingMetricSpecification
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
import Network.AWS.AutoScaling.Types.PredictiveScalingPredefinedScalingMetric
import Network.AWS.AutoScaling.Types.ProcessType
import Network.AWS.AutoScaling.Types.RefreshPreferences
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
import Network.AWS.AutoScaling.UpdateAutoScalingGroup
