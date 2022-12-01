{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Lens
  ( -- * Operations

    -- ** AttachInstances
    attachInstances_instanceIds,
    attachInstances_autoScalingGroupName,

    -- ** AttachLoadBalancerTargetGroups
    attachLoadBalancerTargetGroups_autoScalingGroupName,
    attachLoadBalancerTargetGroups_targetGroupARNs,
    attachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** AttachLoadBalancers
    attachLoadBalancers_autoScalingGroupName,
    attachLoadBalancers_loadBalancerNames,
    attachLoadBalancersResponse_httpStatus,

    -- ** BatchDeleteScheduledAction
    batchDeleteScheduledAction_autoScalingGroupName,
    batchDeleteScheduledAction_scheduledActionNames,
    batchDeleteScheduledActionResponse_failedScheduledActions,
    batchDeleteScheduledActionResponse_httpStatus,

    -- ** BatchPutScheduledUpdateGroupAction
    batchPutScheduledUpdateGroupAction_autoScalingGroupName,
    batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_httpStatus,

    -- ** CancelInstanceRefresh
    cancelInstanceRefresh_autoScalingGroupName,
    cancelInstanceRefreshResponse_instanceRefreshId,
    cancelInstanceRefreshResponse_httpStatus,

    -- ** CompleteLifecycleAction
    completeLifecycleAction_lifecycleActionToken,
    completeLifecycleAction_instanceId,
    completeLifecycleAction_lifecycleHookName,
    completeLifecycleAction_autoScalingGroupName,
    completeLifecycleAction_lifecycleActionResult,
    completeLifecycleActionResponse_httpStatus,

    -- ** CreateAutoScalingGroup
    createAutoScalingGroup_tags,
    createAutoScalingGroup_loadBalancerNames,
    createAutoScalingGroup_availabilityZones,
    createAutoScalingGroup_healthCheckGracePeriod,
    createAutoScalingGroup_launchTemplate,
    createAutoScalingGroup_lifecycleHookSpecificationList,
    createAutoScalingGroup_serviceLinkedRoleARN,
    createAutoScalingGroup_context,
    createAutoScalingGroup_maxInstanceLifetime,
    createAutoScalingGroup_vPCZoneIdentifier,
    createAutoScalingGroup_launchConfigurationName,
    createAutoScalingGroup_targetGroupARNs,
    createAutoScalingGroup_defaultInstanceWarmup,
    createAutoScalingGroup_instanceId,
    createAutoScalingGroup_mixedInstancesPolicy,
    createAutoScalingGroup_healthCheckType,
    createAutoScalingGroup_placementGroup,
    createAutoScalingGroup_desiredCapacityType,
    createAutoScalingGroup_newInstancesProtectedFromScaleIn,
    createAutoScalingGroup_defaultCooldown,
    createAutoScalingGroup_terminationPolicies,
    createAutoScalingGroup_desiredCapacity,
    createAutoScalingGroup_capacityRebalance,
    createAutoScalingGroup_autoScalingGroupName,
    createAutoScalingGroup_minSize,
    createAutoScalingGroup_maxSize,

    -- ** CreateLaunchConfiguration
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_userData,
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_launchConfigurationName,

    -- ** CreateOrUpdateTags
    createOrUpdateTags_tags,

    -- ** DeleteAutoScalingGroup
    deleteAutoScalingGroup_forceDelete,
    deleteAutoScalingGroup_autoScalingGroupName,

    -- ** DeleteLaunchConfiguration
    deleteLaunchConfiguration_launchConfigurationName,

    -- ** DeleteLifecycleHook
    deleteLifecycleHook_lifecycleHookName,
    deleteLifecycleHook_autoScalingGroupName,
    deleteLifecycleHookResponse_httpStatus,

    -- ** DeleteNotificationConfiguration
    deleteNotificationConfiguration_autoScalingGroupName,
    deleteNotificationConfiguration_topicARN,

    -- ** DeletePolicy
    deletePolicy_autoScalingGroupName,
    deletePolicy_policyName,

    -- ** DeleteScheduledAction
    deleteScheduledAction_autoScalingGroupName,
    deleteScheduledAction_scheduledActionName,

    -- ** DeleteTags
    deleteTags_tags,

    -- ** DeleteWarmPool
    deleteWarmPool_forceDelete,
    deleteWarmPool_autoScalingGroupName,
    deleteWarmPoolResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeAdjustmentTypes
    describeAdjustmentTypesResponse_adjustmentTypes,
    describeAdjustmentTypesResponse_httpStatus,

    -- ** DescribeAutoScalingGroups
    describeAutoScalingGroups_nextToken,
    describeAutoScalingGroups_filters,
    describeAutoScalingGroups_autoScalingGroupNames,
    describeAutoScalingGroups_maxRecords,
    describeAutoScalingGroupsResponse_nextToken,
    describeAutoScalingGroupsResponse_httpStatus,
    describeAutoScalingGroupsResponse_autoScalingGroups,

    -- ** DescribeAutoScalingInstances
    describeAutoScalingInstances_nextToken,
    describeAutoScalingInstances_maxRecords,
    describeAutoScalingInstances_instanceIds,
    describeAutoScalingInstancesResponse_nextToken,
    describeAutoScalingInstancesResponse_autoScalingInstances,
    describeAutoScalingInstancesResponse_httpStatus,

    -- ** DescribeAutoScalingNotificationTypes
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,

    -- ** DescribeInstanceRefreshes
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_autoScalingGroupName,
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_httpStatus,

    -- ** DescribeLaunchConfigurations
    describeLaunchConfigurations_nextToken,
    describeLaunchConfigurations_maxRecords,
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurationsResponse_nextToken,
    describeLaunchConfigurationsResponse_httpStatus,
    describeLaunchConfigurationsResponse_launchConfigurations,

    -- ** DescribeLifecycleHookTypes
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,

    -- ** DescribeLifecycleHooks
    describeLifecycleHooks_lifecycleHookNames,
    describeLifecycleHooks_autoScalingGroupName,
    describeLifecycleHooksResponse_lifecycleHooks,
    describeLifecycleHooksResponse_httpStatus,

    -- ** DescribeLoadBalancerTargetGroups
    describeLoadBalancerTargetGroups_nextToken,
    describeLoadBalancerTargetGroups_maxRecords,
    describeLoadBalancerTargetGroups_autoScalingGroupName,
    describeLoadBalancerTargetGroupsResponse_nextToken,
    describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups,
    describeLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_nextToken,
    describeLoadBalancers_maxRecords,
    describeLoadBalancers_autoScalingGroupName,
    describeLoadBalancersResponse_nextToken,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_httpStatus,

    -- ** DescribeMetricCollectionTypes
    describeMetricCollectionTypesResponse_metrics,
    describeMetricCollectionTypesResponse_granularities,
    describeMetricCollectionTypesResponse_httpStatus,

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
    describePolicies_maxRecords,
    describePolicies_policyNames,
    describePolicies_autoScalingGroupName,
    describePoliciesResponse_nextToken,
    describePoliciesResponse_scalingPolicies,
    describePoliciesResponse_httpStatus,

    -- ** DescribeScalingActivities
    describeScalingActivities_nextToken,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_maxRecords,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivities_activityIds,
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,

    -- ** DescribeScalingProcessTypes
    describeScalingProcessTypesResponse_processes,
    describeScalingProcessTypesResponse_httpStatus,

    -- ** DescribeScheduledActions
    describeScheduledActions_nextToken,
    describeScheduledActions_endTime,
    describeScheduledActions_maxRecords,
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActions_startTime,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_filters,
    describeTags_maxRecords,
    describeTagsResponse_tags,
    describeTagsResponse_nextToken,
    describeTagsResponse_httpStatus,

    -- ** DescribeTerminationPolicyTypes
    describeTerminationPolicyTypesResponse_terminationPolicyTypes,
    describeTerminationPolicyTypesResponse_httpStatus,

    -- ** DescribeWarmPool
    describeWarmPool_nextToken,
    describeWarmPool_maxRecords,
    describeWarmPool_autoScalingGroupName,
    describeWarmPoolResponse_instances,
    describeWarmPoolResponse_warmPoolConfiguration,
    describeWarmPoolResponse_nextToken,
    describeWarmPoolResponse_httpStatus,

    -- ** DetachInstances
    detachInstances_instanceIds,
    detachInstances_autoScalingGroupName,
    detachInstances_shouldDecrementDesiredCapacity,
    detachInstancesResponse_activities,
    detachInstancesResponse_httpStatus,

    -- ** DetachLoadBalancerTargetGroups
    detachLoadBalancerTargetGroups_autoScalingGroupName,
    detachLoadBalancerTargetGroups_targetGroupARNs,
    detachLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DetachLoadBalancers
    detachLoadBalancers_autoScalingGroupName,
    detachLoadBalancers_loadBalancerNames,
    detachLoadBalancersResponse_httpStatus,

    -- ** DisableMetricsCollection
    disableMetricsCollection_metrics,
    disableMetricsCollection_autoScalingGroupName,

    -- ** EnableMetricsCollection
    enableMetricsCollection_metrics,
    enableMetricsCollection_autoScalingGroupName,
    enableMetricsCollection_granularity,

    -- ** EnterStandby
    enterStandby_instanceIds,
    enterStandby_autoScalingGroupName,
    enterStandby_shouldDecrementDesiredCapacity,
    enterStandbyResponse_activities,
    enterStandbyResponse_httpStatus,

    -- ** ExecutePolicy
    executePolicy_honorCooldown,
    executePolicy_breachThreshold,
    executePolicy_metricValue,
    executePolicy_autoScalingGroupName,
    executePolicy_policyName,

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

    -- ** PutLifecycleHook
    putLifecycleHook_roleARN,
    putLifecycleHook_notificationMetadata,
    putLifecycleHook_defaultResult,
    putLifecycleHook_notificationTargetARN,
    putLifecycleHook_lifecycleTransition,
    putLifecycleHook_heartbeatTimeout,
    putLifecycleHook_lifecycleHookName,
    putLifecycleHook_autoScalingGroupName,
    putLifecycleHookResponse_httpStatus,

    -- ** PutNotificationConfiguration
    putNotificationConfiguration_autoScalingGroupName,
    putNotificationConfiguration_topicARN,
    putNotificationConfiguration_notificationTypes,

    -- ** PutScalingPolicy
    putScalingPolicy_metricAggregationType,
    putScalingPolicy_policyType,
    putScalingPolicy_cooldown,
    putScalingPolicy_adjustmentType,
    putScalingPolicy_estimatedInstanceWarmup,
    putScalingPolicy_enabled,
    putScalingPolicy_targetTrackingConfiguration,
    putScalingPolicy_minAdjustmentStep,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_stepAdjustments,
    putScalingPolicy_predictiveScalingConfiguration,
    putScalingPolicy_minAdjustmentMagnitude,
    putScalingPolicy_autoScalingGroupName,
    putScalingPolicy_policyName,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_policyARN,
    putScalingPolicyResponse_httpStatus,

    -- ** PutScheduledUpdateGroupAction
    putScheduledUpdateGroupAction_timeZone,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- ** PutWarmPool
    putWarmPool_poolState,
    putWarmPool_minSize,
    putWarmPool_instanceReusePolicy,
    putWarmPool_maxGroupPreparedCapacity,
    putWarmPool_autoScalingGroupName,
    putWarmPoolResponse_httpStatus,

    -- ** RecordLifecycleActionHeartbeat
    recordLifecycleActionHeartbeat_lifecycleActionToken,
    recordLifecycleActionHeartbeat_instanceId,
    recordLifecycleActionHeartbeat_lifecycleHookName,
    recordLifecycleActionHeartbeat_autoScalingGroupName,
    recordLifecycleActionHeartbeatResponse_httpStatus,

    -- ** ResumeProcesses
    resumeProcesses_scalingProcesses,
    resumeProcesses_autoScalingGroupName,

    -- ** SetDesiredCapacity
    setDesiredCapacity_honorCooldown,
    setDesiredCapacity_autoScalingGroupName,
    setDesiredCapacity_desiredCapacity,

    -- ** SetInstanceHealth
    setInstanceHealth_shouldRespectGracePeriod,
    setInstanceHealth_instanceId,
    setInstanceHealth_healthStatus,

    -- ** SetInstanceProtection
    setInstanceProtection_instanceIds,
    setInstanceProtection_autoScalingGroupName,
    setInstanceProtection_protectedFromScaleIn,
    setInstanceProtectionResponse_httpStatus,

    -- ** StartInstanceRefresh
    startInstanceRefresh_preferences,
    startInstanceRefresh_strategy,
    startInstanceRefresh_desiredConfiguration,
    startInstanceRefresh_autoScalingGroupName,
    startInstanceRefreshResponse_instanceRefreshId,
    startInstanceRefreshResponse_httpStatus,

    -- ** SuspendProcesses
    suspendProcesses_scalingProcesses,
    suspendProcesses_autoScalingGroupName,

    -- ** TerminateInstanceInAutoScalingGroup
    terminateInstanceInAutoScalingGroup_instanceId,
    terminateInstanceInAutoScalingGroup_shouldDecrementDesiredCapacity,
    terminateInstanceInAutoScalingGroupResponse_activity,
    terminateInstanceInAutoScalingGroupResponse_httpStatus,

    -- ** UpdateAutoScalingGroup
    updateAutoScalingGroup_availabilityZones,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_context,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_defaultInstanceWarmup,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_desiredCapacityType,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_autoScalingGroupName,

    -- * Types

    -- ** AcceleratorCountRequest
    acceleratorCountRequest_max,
    acceleratorCountRequest_min,

    -- ** AcceleratorTotalMemoryMiBRequest
    acceleratorTotalMemoryMiBRequest_max,
    acceleratorTotalMemoryMiBRequest_min,

    -- ** Activity
    activity_progress,
    activity_autoScalingGroupARN,
    activity_autoScalingGroupState,
    activity_endTime,
    activity_description,
    activity_details,
    activity_statusMessage,
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
    autoScalingGroup_tags,
    autoScalingGroup_instances,
    autoScalingGroup_warmPoolConfiguration,
    autoScalingGroup_autoScalingGroupARN,
    autoScalingGroup_loadBalancerNames,
    autoScalingGroup_predictedCapacity,
    autoScalingGroup_healthCheckGracePeriod,
    autoScalingGroup_launchTemplate,
    autoScalingGroup_warmPoolSize,
    autoScalingGroup_serviceLinkedRoleARN,
    autoScalingGroup_context,
    autoScalingGroup_status,
    autoScalingGroup_maxInstanceLifetime,
    autoScalingGroup_vPCZoneIdentifier,
    autoScalingGroup_launchConfigurationName,
    autoScalingGroup_targetGroupARNs,
    autoScalingGroup_defaultInstanceWarmup,
    autoScalingGroup_mixedInstancesPolicy,
    autoScalingGroup_placementGroup,
    autoScalingGroup_desiredCapacityType,
    autoScalingGroup_newInstancesProtectedFromScaleIn,
    autoScalingGroup_terminationPolicies,
    autoScalingGroup_suspendedProcesses,
    autoScalingGroup_enabledMetrics,
    autoScalingGroup_capacityRebalance,
    autoScalingGroup_autoScalingGroupName,
    autoScalingGroup_minSize,
    autoScalingGroup_maxSize,
    autoScalingGroup_desiredCapacity,
    autoScalingGroup_defaultCooldown,
    autoScalingGroup_availabilityZones,
    autoScalingGroup_healthCheckType,
    autoScalingGroup_createdTime,

    -- ** AutoScalingInstanceDetails
    autoScalingInstanceDetails_launchTemplate,
    autoScalingInstanceDetails_launchConfigurationName,
    autoScalingInstanceDetails_instanceType,
    autoScalingInstanceDetails_weightedCapacity,
    autoScalingInstanceDetails_instanceId,
    autoScalingInstanceDetails_autoScalingGroupName,
    autoScalingInstanceDetails_availabilityZone,
    autoScalingInstanceDetails_lifecycleState,
    autoScalingInstanceDetails_healthStatus,
    autoScalingInstanceDetails_protectedFromScaleIn,

    -- ** BaselineEbsBandwidthMbpsRequest
    baselineEbsBandwidthMbpsRequest_max,
    baselineEbsBandwidthMbpsRequest_min,

    -- ** BlockDeviceMapping
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
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
    desiredConfiguration_launchTemplate,
    desiredConfiguration_mixedInstancesPolicy,

    -- ** Ebs
    ebs_deleteOnTermination,
    ebs_snapshotId,
    ebs_volumeType,
    ebs_volumeSize,
    ebs_encrypted,
    ebs_throughput,
    ebs_iops,

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
    instance_launchTemplate,
    instance_launchConfigurationName,
    instance_instanceType,
    instance_weightedCapacity,
    instance_instanceId,
    instance_availabilityZone,
    instance_lifecycleState,
    instance_healthStatus,
    instance_protectedFromScaleIn,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,
    instanceMetadataOptions_httpEndpoint,

    -- ** InstanceMonitoring
    instanceMonitoring_enabled,

    -- ** InstanceRefresh
    instanceRefresh_preferences,
    instanceRefresh_progressDetails,
    instanceRefresh_instanceRefreshId,
    instanceRefresh_statusReason,
    instanceRefresh_status,
    instanceRefresh_endTime,
    instanceRefresh_autoScalingGroupName,
    instanceRefresh_desiredConfiguration,
    instanceRefresh_percentageComplete,
    instanceRefresh_startTime,
    instanceRefresh_instancesToUpdate,

    -- ** InstanceRefreshLivePoolProgress
    instanceRefreshLivePoolProgress_percentageComplete,
    instanceRefreshLivePoolProgress_instancesToUpdate,

    -- ** InstanceRefreshProgressDetails
    instanceRefreshProgressDetails_warmPoolProgress,
    instanceRefreshProgressDetails_livePoolProgress,

    -- ** InstanceRefreshWarmPoolProgress
    instanceRefreshWarmPoolProgress_percentageComplete,
    instanceRefreshWarmPoolProgress_instancesToUpdate,

    -- ** InstanceRequirements
    instanceRequirements_instanceGenerations,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_bareMetal,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_localStorageTypes,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_acceleratorNames,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_acceleratorCount,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_localStorage,
    instanceRequirements_vCpuCount,
    instanceRequirements_memoryMiB,

    -- ** InstanceReusePolicy
    instanceReusePolicy_reuseOnScaleIn,

    -- ** InstancesDistribution
    instancesDistribution_onDemandBaseCapacity,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotMaxPrice,
    instancesDistribution_spotAllocationStrategy,

    -- ** LaunchConfiguration
    launchConfiguration_ebsOptimized,
    launchConfiguration_iamInstanceProfile,
    launchConfiguration_classicLinkVPCId,
    launchConfiguration_userData,
    launchConfiguration_associatePublicIpAddress,
    launchConfiguration_blockDeviceMappings,
    launchConfiguration_launchConfigurationARN,
    launchConfiguration_placementTenancy,
    launchConfiguration_securityGroups,
    launchConfiguration_ramdiskId,
    launchConfiguration_instanceMonitoring,
    launchConfiguration_keyName,
    launchConfiguration_kernelId,
    launchConfiguration_spotPrice,
    launchConfiguration_classicLinkVPCSecurityGroups,
    launchConfiguration_metadataOptions,
    launchConfiguration_launchConfigurationName,
    launchConfiguration_imageId,
    launchConfiguration_instanceType,
    launchConfiguration_createdTime,

    -- ** LaunchTemplate
    launchTemplate_launchTemplateSpecification,
    launchTemplate_overrides,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_launchTemplateSpecification,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_weightedCapacity,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- ** LifecycleHook
    lifecycleHook_lifecycleHookName,
    lifecycleHook_roleARN,
    lifecycleHook_notificationMetadata,
    lifecycleHook_defaultResult,
    lifecycleHook_notificationTargetARN,
    lifecycleHook_globalTimeout,
    lifecycleHook_autoScalingGroupName,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_heartbeatTimeout,

    -- ** LifecycleHookSpecification
    lifecycleHookSpecification_roleARN,
    lifecycleHookSpecification_notificationMetadata,
    lifecycleHookSpecification_defaultResult,
    lifecycleHookSpecification_notificationTargetARN,
    lifecycleHookSpecification_heartbeatTimeout,
    lifecycleHookSpecification_lifecycleHookName,
    lifecycleHookSpecification_lifecycleTransition,

    -- ** LoadBalancerState
    loadBalancerState_loadBalancerName,
    loadBalancerState_state,

    -- ** LoadBalancerTargetGroupState
    loadBalancerTargetGroupState_state,
    loadBalancerTargetGroupState_loadBalancerTargetGroupARN,

    -- ** LoadForecast
    loadForecast_timestamps,
    loadForecast_values,
    loadForecast_metricSpecification,

    -- ** MemoryGiBPerVCpuRequest
    memoryGiBPerVCpuRequest_max,
    memoryGiBPerVCpuRequest_min,

    -- ** MemoryMiBRequest
    memoryMiBRequest_max,
    memoryMiBRequest_min,

    -- ** Metric
    metric_dimensions,
    metric_namespace,
    metric_metricName,

    -- ** MetricCollectionType
    metricCollectionType_metric,

    -- ** MetricDataQuery
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
    metricDataQuery_label,
    metricDataQuery_expression,
    metricDataQuery_id,

    -- ** MetricDimension
    metricDimension_name,
    metricDimension_value,

    -- ** MetricGranularityType
    metricGranularityType_granularity,

    -- ** MetricStat
    metricStat_unit,
    metricStat_metric,
    metricStat_stat,

    -- ** MixedInstancesPolicy
    mixedInstancesPolicy_instancesDistribution,
    mixedInstancesPolicy_launchTemplate,

    -- ** NetworkBandwidthGbpsRequest
    networkBandwidthGbpsRequest_max,
    networkBandwidthGbpsRequest_min,

    -- ** NetworkInterfaceCountRequest
    networkInterfaceCountRequest_max,
    networkInterfaceCountRequest_min,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationType,
    notificationConfiguration_topicARN,
    notificationConfiguration_autoScalingGroupName,

    -- ** PredefinedMetricSpecification
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- ** PredictiveScalingConfiguration
    predictiveScalingConfiguration_maxCapacityBuffer,
    predictiveScalingConfiguration_maxCapacityBreachBehavior,
    predictiveScalingConfiguration_mode,
    predictiveScalingConfiguration_schedulingBufferTime,
    predictiveScalingConfiguration_metricSpecifications,

    -- ** PredictiveScalingCustomizedCapacityMetric
    predictiveScalingCustomizedCapacityMetric_metricDataQueries,

    -- ** PredictiveScalingCustomizedLoadMetric
    predictiveScalingCustomizedLoadMetric_metricDataQueries,

    -- ** PredictiveScalingCustomizedScalingMetric
    predictiveScalingCustomizedScalingMetric_metricDataQueries,

    -- ** PredictiveScalingMetricSpecification
    predictiveScalingMetricSpecification_customizedScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedLoadMetricSpecification,
    predictiveScalingMetricSpecification_customizedLoadMetricSpecification,
    predictiveScalingMetricSpecification_customizedCapacityMetricSpecification,
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
    refreshPreferences_checkpointPercentages,
    refreshPreferences_skipMatching,
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_instanceWarmup,
    refreshPreferences_checkpointDelay,

    -- ** ScalingPolicy
    scalingPolicy_policyName,
    scalingPolicy_alarms,
    scalingPolicy_metricAggregationType,
    scalingPolicy_policyType,
    scalingPolicy_cooldown,
    scalingPolicy_adjustmentType,
    scalingPolicy_estimatedInstanceWarmup,
    scalingPolicy_enabled,
    scalingPolicy_autoScalingGroupName,
    scalingPolicy_policyARN,
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_minAdjustmentStep,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_stepAdjustments,
    scalingPolicy_predictiveScalingConfiguration,
    scalingPolicy_minAdjustmentMagnitude,

    -- ** ScalingProcessQuery
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- ** ScheduledUpdateGroupAction
    scheduledUpdateGroupAction_timeZone,
    scheduledUpdateGroupAction_time,
    scheduledUpdateGroupAction_endTime,
    scheduledUpdateGroupAction_scheduledActionARN,
    scheduledUpdateGroupAction_autoScalingGroupName,
    scheduledUpdateGroupAction_minSize,
    scheduledUpdateGroupAction_recurrence,
    scheduledUpdateGroupAction_maxSize,
    scheduledUpdateGroupAction_scheduledActionName,
    scheduledUpdateGroupAction_desiredCapacity,
    scheduledUpdateGroupAction_startTime,

    -- ** ScheduledUpdateGroupActionRequest
    scheduledUpdateGroupActionRequest_timeZone,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_startTime,
    scheduledUpdateGroupActionRequest_scheduledActionName,

    -- ** StepAdjustment
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_scalingAdjustment,

    -- ** SuspendedProcess
    suspendedProcess_suspensionReason,
    suspendedProcess_processName,

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
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_targetValue,

    -- ** TotalLocalStorageGBRequest
    totalLocalStorageGBRequest_max,
    totalLocalStorageGBRequest_min,

    -- ** VCpuCountRequest
    vCpuCountRequest_max,
    vCpuCountRequest_min,

    -- ** WarmPoolConfiguration
    warmPoolConfiguration_poolState,
    warmPoolConfiguration_status,
    warmPoolConfiguration_minSize,
    warmPoolConfiguration_instanceReusePolicy,
    warmPoolConfiguration_maxGroupPreparedCapacity,
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
import Amazonka.AutoScaling.Types.AcceleratorCountRequest
import Amazonka.AutoScaling.Types.AcceleratorTotalMemoryMiBRequest
import Amazonka.AutoScaling.Types.Activity
import Amazonka.AutoScaling.Types.AdjustmentType
import Amazonka.AutoScaling.Types.Alarm
import Amazonka.AutoScaling.Types.AutoScalingGroup
import Amazonka.AutoScaling.Types.AutoScalingInstanceDetails
import Amazonka.AutoScaling.Types.BaselineEbsBandwidthMbpsRequest
import Amazonka.AutoScaling.Types.BlockDeviceMapping
import Amazonka.AutoScaling.Types.CapacityForecast
import Amazonka.AutoScaling.Types.CustomizedMetricSpecification
import Amazonka.AutoScaling.Types.DesiredConfiguration
import Amazonka.AutoScaling.Types.Ebs
import Amazonka.AutoScaling.Types.EnabledMetric
import Amazonka.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
import Amazonka.AutoScaling.Types.Filter
import Amazonka.AutoScaling.Types.Instance
import Amazonka.AutoScaling.Types.InstanceMetadataOptions
import Amazonka.AutoScaling.Types.InstanceMonitoring
import Amazonka.AutoScaling.Types.InstanceRefresh
import Amazonka.AutoScaling.Types.InstanceRefreshLivePoolProgress
import Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails
import Amazonka.AutoScaling.Types.InstanceRefreshWarmPoolProgress
import Amazonka.AutoScaling.Types.InstanceRequirements
import Amazonka.AutoScaling.Types.InstanceReusePolicy
import Amazonka.AutoScaling.Types.InstancesDistribution
import Amazonka.AutoScaling.Types.LaunchConfiguration
import Amazonka.AutoScaling.Types.LaunchTemplate
import Amazonka.AutoScaling.Types.LaunchTemplateOverrides
import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import Amazonka.AutoScaling.Types.LifecycleHook
import Amazonka.AutoScaling.Types.LifecycleHookSpecification
import Amazonka.AutoScaling.Types.LoadBalancerState
import Amazonka.AutoScaling.Types.LoadBalancerTargetGroupState
import Amazonka.AutoScaling.Types.LoadForecast
import Amazonka.AutoScaling.Types.MemoryGiBPerVCpuRequest
import Amazonka.AutoScaling.Types.MemoryMiBRequest
import Amazonka.AutoScaling.Types.Metric
import Amazonka.AutoScaling.Types.MetricCollectionType
import Amazonka.AutoScaling.Types.MetricDataQuery
import Amazonka.AutoScaling.Types.MetricDimension
import Amazonka.AutoScaling.Types.MetricGranularityType
import Amazonka.AutoScaling.Types.MetricStat
import Amazonka.AutoScaling.Types.MixedInstancesPolicy
import Amazonka.AutoScaling.Types.NetworkBandwidthGbpsRequest
import Amazonka.AutoScaling.Types.NetworkInterfaceCountRequest
import Amazonka.AutoScaling.Types.NotificationConfiguration
import Amazonka.AutoScaling.Types.PredefinedMetricSpecification
import Amazonka.AutoScaling.Types.PredictiveScalingConfiguration
import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedCapacityMetric
import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedLoadMetric
import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedScalingMetric
import Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedScalingMetric
import Amazonka.AutoScaling.Types.ProcessType
import Amazonka.AutoScaling.Types.RefreshPreferences
import Amazonka.AutoScaling.Types.ScalingPolicy
import Amazonka.AutoScaling.Types.ScalingProcessQuery
import Amazonka.AutoScaling.Types.ScheduledUpdateGroupAction
import Amazonka.AutoScaling.Types.ScheduledUpdateGroupActionRequest
import Amazonka.AutoScaling.Types.StepAdjustment
import Amazonka.AutoScaling.Types.SuspendedProcess
import Amazonka.AutoScaling.Types.Tag
import Amazonka.AutoScaling.Types.TagDescription
import Amazonka.AutoScaling.Types.TargetTrackingConfiguration
import Amazonka.AutoScaling.Types.TotalLocalStorageGBRequest
import Amazonka.AutoScaling.Types.VCpuCountRequest
import Amazonka.AutoScaling.Types.WarmPoolConfiguration
import Amazonka.AutoScaling.UpdateAutoScalingGroup
