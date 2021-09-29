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

    -- ** ExecutePolicy
    executePolicy_metricValue,
    executePolicy_breachThreshold,
    executePolicy_honorCooldown,
    executePolicy_autoScalingGroupName,
    executePolicy_policyName,

    -- ** SuspendProcesses
    suspendProcesses_scalingProcesses,
    suspendProcesses_autoScalingGroupName,

    -- ** DescribeInstanceRefreshes
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_autoScalingGroupName,
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_httpStatus,

    -- ** EnterStandby
    enterStandby_instanceIds,
    enterStandby_autoScalingGroupName,
    enterStandby_shouldDecrementDesiredCapacity,
    enterStandbyResponse_activities,
    enterStandbyResponse_httpStatus,

    -- ** DeleteLifecycleHook
    deleteLifecycleHook_lifecycleHookName,
    deleteLifecycleHook_autoScalingGroupName,
    deleteLifecycleHookResponse_httpStatus,

    -- ** AttachLoadBalancerTargetGroups
    attachLoadBalancerTargetGroups_autoScalingGroupName,
    attachLoadBalancerTargetGroups_targetGroupARNs,
    attachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** StartInstanceRefresh
    startInstanceRefresh_strategy,
    startInstanceRefresh_desiredConfiguration,
    startInstanceRefresh_preferences,
    startInstanceRefresh_autoScalingGroupName,
    startInstanceRefreshResponse_instanceRefreshId,
    startInstanceRefreshResponse_httpStatus,

    -- ** ResumeProcesses
    resumeProcesses_scalingProcesses,
    resumeProcesses_autoScalingGroupName,

    -- ** PutNotificationConfiguration
    putNotificationConfiguration_autoScalingGroupName,
    putNotificationConfiguration_topicARN,
    putNotificationConfiguration_notificationTypes,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_filters,
    describeTags_maxRecords,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DeleteWarmPool
    deleteWarmPool_forceDelete,
    deleteWarmPool_autoScalingGroupName,
    deleteWarmPoolResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_targetTrackingConfiguration,
    putScalingPolicy_stepAdjustments,
    putScalingPolicy_metricAggregationType,
    putScalingPolicy_policyType,
    putScalingPolicy_enabled,
    putScalingPolicy_cooldown,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_adjustmentType,
    putScalingPolicy_minAdjustmentStep,
    putScalingPolicy_estimatedInstanceWarmup,
    putScalingPolicy_minAdjustmentMagnitude,
    putScalingPolicy_predictiveScalingConfiguration,
    putScalingPolicy_autoScalingGroupName,
    putScalingPolicy_policyName,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_policyARN,
    putScalingPolicyResponse_httpStatus,

    -- ** CreateOrUpdateTags
    createOrUpdateTags_tags,

    -- ** DeletePolicy
    deletePolicy_autoScalingGroupName,
    deletePolicy_policyName,

    -- ** CreateLaunchConfiguration
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_userData,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_launchConfigurationName,

    -- ** DescribeAutoScalingNotificationTypes
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,

    -- ** DescribeScheduledActions
    describeScheduledActions_nextToken,
    describeScheduledActions_startTime,
    describeScheduledActions_endTime,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActions_maxRecords,
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** DescribeAdjustmentTypes
    describeAdjustmentTypesResponse_adjustmentTypes,
    describeAdjustmentTypesResponse_httpStatus,

    -- ** DetachLoadBalancers
    detachLoadBalancers_autoScalingGroupName,
    detachLoadBalancers_loadBalancerNames,
    detachLoadBalancersResponse_httpStatus,

    -- ** DeleteScheduledAction
    deleteScheduledAction_autoScalingGroupName,
    deleteScheduledAction_scheduledActionName,

    -- ** DescribeScalingActivities
    describeScalingActivities_nextToken,
    describeScalingActivities_activityIds,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_maxRecords,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,

    -- ** DescribeLifecycleHooks
    describeLifecycleHooks_lifecycleHookNames,
    describeLifecycleHooks_autoScalingGroupName,
    describeLifecycleHooksResponse_lifecycleHooks,
    describeLifecycleHooksResponse_httpStatus,

    -- ** PutScheduledUpdateGroupAction
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_timeZone,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- ** DeleteTags
    deleteTags_tags,

    -- ** DetachLoadBalancerTargetGroups
    detachLoadBalancerTargetGroups_autoScalingGroupName,
    detachLoadBalancerTargetGroups_targetGroupARNs,
    detachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DescribeWarmPool
    describeWarmPool_nextToken,
    describeWarmPool_maxRecords,
    describeWarmPool_autoScalingGroupName,
    describeWarmPoolResponse_nextToken,
    describeWarmPoolResponse_instances,
    describeWarmPoolResponse_warmPoolConfiguration,
    describeWarmPoolResponse_httpStatus,

    -- ** SetInstanceProtection
    setInstanceProtection_instanceIds,
    setInstanceProtection_autoScalingGroupName,
    setInstanceProtection_protectedFromScaleIn,
    setInstanceProtectionResponse_httpStatus,

    -- ** DetachInstances
    detachInstances_instanceIds,
    detachInstances_autoScalingGroupName,
    detachInstances_shouldDecrementDesiredCapacity,
    detachInstancesResponse_activities,
    detachInstancesResponse_httpStatus,

    -- ** TerminateInstanceInAutoScalingGroup
    terminateInstanceInAutoScalingGroup_instanceId,
    terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity,
    terminateInstanceInAutoScalingGroupResponse_activity,
    terminateInstanceInAutoScalingGroupResponse_httpStatus,

    -- ** AttachLoadBalancers
    attachLoadBalancers_autoScalingGroupName,
    attachLoadBalancers_loadBalancerNames,
    attachLoadBalancersResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeTerminationPolicyTypes
    describeTerminationPolicyTypesResponse_terminationPolicyTypes,
    describeTerminationPolicyTypesResponse_httpStatus,

    -- ** SetInstanceHealth
    setInstanceHealth_shouldRespectGracePeriod,
    setInstanceHealth_instanceId,
    setInstanceHealth_healthStatus,

    -- ** ExitStandby
    exitStandby_instanceIds,
    exitStandby_autoScalingGroupName,
    exitStandbyResponse_activities,
    exitStandbyResponse_httpStatus,

    -- ** GetPredictiveScalingForecast
    getPredictiveScalingForecast_autoScalingGroupName,
    getPredictiveScalingForecast_policyName,
    getPredictiveScalingForecast_startTime,
    getPredictiveScalingForecast_endTime,
    getPredictiveScalingForecastResponse_httpStatus,
    getPredictiveScalingForecastResponse_loadForecast,
    getPredictiveScalingForecastResponse_capacityForecast,
    getPredictiveScalingForecastResponse_updateTime,

    -- ** PutWarmPool
    putWarmPool_minSize,
    putWarmPool_poolState,
    putWarmPool_maxGroupPreparedCapacity,
    putWarmPool_autoScalingGroupName,
    putWarmPoolResponse_httpStatus,

    -- ** DeleteNotificationConfiguration
    deleteNotificationConfiguration_autoScalingGroupName,
    deleteNotificationConfiguration_topicARN,

    -- ** PutLifecycleHook
    putLifecycleHook_roleARN,
    putLifecycleHook_lifecycleTransition,
    putLifecycleHook_notificationTargetARN,
    putLifecycleHook_heartbeatTimeout,
    putLifecycleHook_notificationMetadata,
    putLifecycleHook_defaultResult,
    putLifecycleHook_lifecycleHookName,
    putLifecycleHook_autoScalingGroupName,
    putLifecycleHookResponse_httpStatus,

    -- ** BatchPutScheduledUpdateGroupAction
    batchPutScheduledUpdateGroupAction_autoScalingGroupName,
    batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_httpStatus,

    -- ** DescribeMetricCollectionTypes
    describeMetricCollectionTypesResponse_metrics,
    describeMetricCollectionTypesResponse_granularities,
    describeMetricCollectionTypesResponse_httpStatus,

    -- ** DeleteLaunchConfiguration
    deleteLaunchConfiguration_launchConfigurationName,

    -- ** UpdateAutoScalingGroup
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_availabilityZones,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_context,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_autoScalingGroupName,

    -- ** DeleteAutoScalingGroup
    deleteAutoScalingGroup_forceDelete,
    deleteAutoScalingGroup_autoScalingGroupName,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_nextToken,
    describeLoadBalancers_maxRecords,
    describeLoadBalancers_autoScalingGroupName,
    describeLoadBalancersResponse_nextToken,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_httpStatus,

    -- ** CreateAutoScalingGroup
    createAutoScalingGroup_availabilityZones,
    createAutoScalingGroup_instanceId,
    createAutoScalingGroup_desiredCapacity,
    createAutoScalingGroup_defaultCooldown,
    createAutoScalingGroup_maxInstanceLifetime,
    createAutoScalingGroup_placementGroup,
    createAutoScalingGroup_launchTemplate,
    createAutoScalingGroup_launchConfigurationName,
    createAutoScalingGroup_healthCheckType,
    createAutoScalingGroup_context,
    createAutoScalingGroup_mixedInstancesPolicy,
    createAutoScalingGroup_tags,
    createAutoScalingGroup_loadBalancerNames,
    createAutoScalingGroup_targetGroupARNs,
    createAutoScalingGroup_vPCZoneIdentifier,
    createAutoScalingGroup_capacityRebalance,
    createAutoScalingGroup_serviceLinkedRoleARN,
    createAutoScalingGroup_healthCheckGracePeriod,
    createAutoScalingGroup_newInstancesProtectedFromScaleIn,
    createAutoScalingGroup_lifecycleHookSpecificationList,
    createAutoScalingGroup_terminationPolicies,
    createAutoScalingGroup_autoScalingGroupName,
    createAutoScalingGroup_minSize,
    createAutoScalingGroup_maxSize,

    -- ** AttachInstances
    attachInstances_instanceIds,
    attachInstances_autoScalingGroupName,

    -- ** CompleteLifecycleAction
    completeLifecycleAction_instanceId,
    completeLifecycleAction_lifecycleActionToken,
    completeLifecycleAction_lifecycleHookName,
    completeLifecycleAction_autoScalingGroupName,
    completeLifecycleAction_lifecycleActionResult,
    completeLifecycleActionResponse_httpStatus,

    -- ** SetDesiredCapacity
    setDesiredCapacity_honorCooldown,
    setDesiredCapacity_autoScalingGroupName,
    setDesiredCapacity_desiredCapacity,

    -- ** DescribeLifecycleHookTypes
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,

    -- ** DescribeNotificationConfigurations
    describeNotificationConfigurations_nextToken,
    describeNotificationConfigurations_autoScalingGroupNames,
    describeNotificationConfigurations_maxRecords,
    describeNotificationConfigurationsResponse_nextToken,
    describeNotificationConfigurationsResponse_httpStatus,
    describeNotificationConfigurationsResponse_notificationConfigurations,

    -- ** DescribePolicies
    describePolicies_nextToken,
    describePolicies_policyTypes,
    describePolicies_policyNames,
    describePolicies_maxRecords,
    describePolicies_autoScalingGroupName,
    describePoliciesResponse_nextToken,
    describePoliciesResponse_scalingPolicies,
    describePoliciesResponse_httpStatus,

    -- ** DescribeLaunchConfigurations
    describeLaunchConfigurations_nextToken,
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurations_maxRecords,
    describeLaunchConfigurationsResponse_nextToken,
    describeLaunchConfigurationsResponse_httpStatus,
    describeLaunchConfigurationsResponse_launchConfigurations,

    -- ** DescribeAutoScalingGroups
    describeAutoScalingGroups_nextToken,
    describeAutoScalingGroups_autoScalingGroupNames,
    describeAutoScalingGroups_maxRecords,
    describeAutoScalingGroupsResponse_nextToken,
    describeAutoScalingGroupsResponse_httpStatus,
    describeAutoScalingGroupsResponse_autoScalingGroups,

    -- ** EnableMetricsCollection
    enableMetricsCollection_metrics,
    enableMetricsCollection_autoScalingGroupName,
    enableMetricsCollection_granularity,

    -- ** DescribeScalingProcessTypes
    describeScalingProcessTypesResponse_processes,
    describeScalingProcessTypesResponse_httpStatus,

    -- ** RecordLifecycleActionHeartbeat
    recordLifecycleActionHeartbeat_instanceId,
    recordLifecycleActionHeartbeat_lifecycleActionToken,
    recordLifecycleActionHeartbeat_lifecycleHookName,
    recordLifecycleActionHeartbeat_autoScalingGroupName,
    recordLifecycleActionHeartbeatResponse_httpStatus,

    -- ** DisableMetricsCollection
    disableMetricsCollection_metrics,
    disableMetricsCollection_autoScalingGroupName,

    -- ** DescribeAutoScalingInstances
    describeAutoScalingInstances_instanceIds,
    describeAutoScalingInstances_nextToken,
    describeAutoScalingInstances_maxRecords,
    describeAutoScalingInstancesResponse_nextToken,
    describeAutoScalingInstancesResponse_autoScalingInstances,
    describeAutoScalingInstancesResponse_httpStatus,

    -- ** CancelInstanceRefresh
    cancelInstanceRefresh_autoScalingGroupName,
    cancelInstanceRefreshResponse_instanceRefreshId,
    cancelInstanceRefreshResponse_httpStatus,

    -- ** BatchDeleteScheduledAction
    batchDeleteScheduledAction_autoScalingGroupName,
    batchDeleteScheduledAction_scheduledActionNames,
    batchDeleteScheduledActionResponse_failedScheduledActions,
    batchDeleteScheduledActionResponse_httpStatus,

    -- ** DescribeLoadBalancerTargetGroups
    describeLoadBalancerTargetGroups_nextToken,
    describeLoadBalancerTargetGroups_maxRecords,
    describeLoadBalancerTargetGroups_autoScalingGroupName,
    describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups,
    describeLoadBalancerTargetGroupsResponse_nextToken,
    describeLoadBalancerTargetGroupsResponse_httpStatus,

    -- * Types

    -- ** Activity
    activity_statusMessage,
    activity_autoScalingGroupARN,
    activity_endTime,
    activity_autoScalingGroupState,
    activity_details,
    activity_description,
    activity_progress,
    activity_activityId,
    activity_autoScalingGroupName,
    activity_cause,
    activity_startTime,
    activity_statusCode,

    -- ** AdjustmentType
    adjustmentType_adjustmentType,

    -- ** Alarm
    alarm_alarmARN,
    alarm_alarmName,

    -- ** AutoScalingGroup
    autoScalingGroup_status,
    autoScalingGroup_maxInstanceLifetime,
    autoScalingGroup_placementGroup,
    autoScalingGroup_suspendedProcesses,
    autoScalingGroup_predictedCapacity,
    autoScalingGroup_autoScalingGroupARN,
    autoScalingGroup_launchTemplate,
    autoScalingGroup_launchConfigurationName,
    autoScalingGroup_instances,
    autoScalingGroup_context,
    autoScalingGroup_mixedInstancesPolicy,
    autoScalingGroup_tags,
    autoScalingGroup_loadBalancerNames,
    autoScalingGroup_targetGroupARNs,
    autoScalingGroup_vPCZoneIdentifier,
    autoScalingGroup_warmPoolSize,
    autoScalingGroup_warmPoolConfiguration,
    autoScalingGroup_capacityRebalance,
    autoScalingGroup_serviceLinkedRoleARN,
    autoScalingGroup_healthCheckGracePeriod,
    autoScalingGroup_newInstancesProtectedFromScaleIn,
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

    -- ** AutoScalingInstanceDetails
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

    -- ** BlockDeviceMapping
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- ** CapacityForecast
    capacityForecast_timestamps,
    capacityForecast_values,

    -- ** CustomizedMetricSpecification
    customizedMetricSpecification_unit,
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- ** DesiredConfiguration
    desiredConfiguration_launchTemplate,
    desiredConfiguration_mixedInstancesPolicy,

    -- ** Ebs
    ebs_encrypted,
    ebs_throughput,
    ebs_deleteOnTermination,
    ebs_snapshotId,
    ebs_volumeType,
    ebs_iops,
    ebs_volumeSize,

    -- ** EnabledMetric
    enabledMetric_granularity,
    enabledMetric_metric,

    -- ** FailedScheduledUpdateGroupActionRequest
    failedScheduledUpdateGroupActionRequest_errorMessage,
    failedScheduledUpdateGroupActionRequest_errorCode,
    failedScheduledUpdateGroupActionRequest_scheduledActionName,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** Instance
    instance_instanceType,
    instance_launchTemplate,
    instance_launchConfigurationName,
    instance_weightedCapacity,
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
    instanceRefresh_instanceRefreshId,
    instanceRefresh_percentageComplete,
    instanceRefresh_startTime,
    instanceRefresh_endTime,
    instanceRefresh_progressDetails,
    instanceRefresh_desiredConfiguration,
    instanceRefresh_preferences,
    instanceRefresh_statusReason,
    instanceRefresh_instancesToUpdate,
    instanceRefresh_autoScalingGroupName,

    -- ** InstanceRefreshLivePoolProgress
    instanceRefreshLivePoolProgress_percentageComplete,
    instanceRefreshLivePoolProgress_instancesToUpdate,

    -- ** InstanceRefreshProgressDetails
    instanceRefreshProgressDetails_livePoolProgress,
    instanceRefreshProgressDetails_warmPoolProgress,

    -- ** InstanceRefreshWarmPoolProgress
    instanceRefreshWarmPoolProgress_percentageComplete,
    instanceRefreshWarmPoolProgress_instancesToUpdate,

    -- ** InstancesDistribution
    instancesDistribution_spotMaxPrice,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotAllocationStrategy,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_onDemandBaseCapacity,

    -- ** LaunchConfiguration
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
    launchConfiguration_launchConfigurationARN,
    launchConfiguration_keyName,
    launchConfiguration_instanceMonitoring,
    launchConfiguration_metadataOptions,
    launchConfiguration_launchConfigurationName,
    launchConfiguration_imageId,
    launchConfiguration_instanceType,
    launchConfiguration_createdTime,

    -- ** LaunchTemplate
    launchTemplate_launchTemplateSpecification,
    launchTemplate_overrides,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_launchTemplateSpecification,
    launchTemplateOverrides_weightedCapacity,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- ** LifecycleHook
    lifecycleHook_roleARN,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_notificationTargetARN,
    lifecycleHook_heartbeatTimeout,
    lifecycleHook_globalTimeout,
    lifecycleHook_notificationMetadata,
    lifecycleHook_lifecycleHookName,
    lifecycleHook_defaultResult,
    lifecycleHook_autoScalingGroupName,

    -- ** LifecycleHookSpecification
    lifecycleHookSpecification_roleARN,
    lifecycleHookSpecification_notificationTargetARN,
    lifecycleHookSpecification_heartbeatTimeout,
    lifecycleHookSpecification_notificationMetadata,
    lifecycleHookSpecification_defaultResult,
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
    mixedInstancesPolicy_instancesDistribution,
    mixedInstancesPolicy_launchTemplate,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationType,
    notificationConfiguration_topicARN,
    notificationConfiguration_autoScalingGroupName,

    -- ** PredefinedMetricSpecification
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- ** PredictiveScalingConfiguration
    predictiveScalingConfiguration_maxCapacityBreachBehavior,
    predictiveScalingConfiguration_mode,
    predictiveScalingConfiguration_maxCapacityBuffer,
    predictiveScalingConfiguration_schedulingBufferTime,
    predictiveScalingConfiguration_metricSpecifications,

    -- ** PredictiveScalingMetricSpecification
    predictiveScalingMetricSpecification_predefinedLoadMetricSpecification,
    predictiveScalingMetricSpecification_predefinedScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedMetricPairSpecification,
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
    refreshPreferences_checkpointDelay,
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_instanceWarmup,
    refreshPreferences_skipMatching,
    refreshPreferences_checkpointPercentages,

    -- ** ScalingPolicy
    scalingPolicy_policyName,
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_stepAdjustments,
    scalingPolicy_metricAggregationType,
    scalingPolicy_policyType,
    scalingPolicy_enabled,
    scalingPolicy_cooldown,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_adjustmentType,
    scalingPolicy_minAdjustmentStep,
    scalingPolicy_estimatedInstanceWarmup,
    scalingPolicy_minAdjustmentMagnitude,
    scalingPolicy_alarms,
    scalingPolicy_policyARN,
    scalingPolicy_predictiveScalingConfiguration,
    scalingPolicy_autoScalingGroupName,

    -- ** ScalingProcessQuery
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- ** ScheduledUpdateGroupAction
    scheduledUpdateGroupAction_minSize,
    scheduledUpdateGroupAction_desiredCapacity,
    scheduledUpdateGroupAction_startTime,
    scheduledUpdateGroupAction_endTime,
    scheduledUpdateGroupAction_scheduledActionARN,
    scheduledUpdateGroupAction_recurrence,
    scheduledUpdateGroupAction_maxSize,
    scheduledUpdateGroupAction_timeZone,
    scheduledUpdateGroupAction_scheduledActionName,
    scheduledUpdateGroupAction_time,
    scheduledUpdateGroupAction_autoScalingGroupName,

    -- ** ScheduledUpdateGroupActionRequest
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_startTime,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_timeZone,
    scheduledUpdateGroupActionRequest_scheduledActionName,

    -- ** StepAdjustment
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_metricIntervalLowerBound,
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
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_targetValue,

    -- ** WarmPoolConfiguration
    warmPoolConfiguration_minSize,
    warmPoolConfiguration_status,
    warmPoolConfiguration_poolState,
    warmPoolConfiguration_maxGroupPreparedCapacity,
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
