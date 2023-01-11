{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** AttachTrafficSources
    attachTrafficSources_autoScalingGroupName,
    attachTrafficSources_trafficSources,
    attachTrafficSourcesResponse_httpStatus,

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
    completeLifecycleAction_instanceId,
    completeLifecycleAction_lifecycleActionToken,
    completeLifecycleAction_lifecycleHookName,
    completeLifecycleAction_autoScalingGroupName,
    completeLifecycleAction_lifecycleActionResult,
    completeLifecycleActionResponse_httpStatus,

    -- ** CreateAutoScalingGroup
    createAutoScalingGroup_availabilityZones,
    createAutoScalingGroup_capacityRebalance,
    createAutoScalingGroup_context,
    createAutoScalingGroup_defaultCooldown,
    createAutoScalingGroup_defaultInstanceWarmup,
    createAutoScalingGroup_desiredCapacity,
    createAutoScalingGroup_desiredCapacityType,
    createAutoScalingGroup_healthCheckGracePeriod,
    createAutoScalingGroup_healthCheckType,
    createAutoScalingGroup_instanceId,
    createAutoScalingGroup_launchConfigurationName,
    createAutoScalingGroup_launchTemplate,
    createAutoScalingGroup_lifecycleHookSpecificationList,
    createAutoScalingGroup_loadBalancerNames,
    createAutoScalingGroup_maxInstanceLifetime,
    createAutoScalingGroup_mixedInstancesPolicy,
    createAutoScalingGroup_newInstancesProtectedFromScaleIn,
    createAutoScalingGroup_placementGroup,
    createAutoScalingGroup_serviceLinkedRoleARN,
    createAutoScalingGroup_tags,
    createAutoScalingGroup_targetGroupARNs,
    createAutoScalingGroup_terminationPolicies,
    createAutoScalingGroup_trafficSources,
    createAutoScalingGroup_vPCZoneIdentifier,
    createAutoScalingGroup_autoScalingGroupName,
    createAutoScalingGroup_minSize,
    createAutoScalingGroup_maxSize,

    -- ** CreateLaunchConfiguration
    createLaunchConfiguration_associatePublicIpAddress,
    createLaunchConfiguration_blockDeviceMappings,
    createLaunchConfiguration_classicLinkVPCId,
    createLaunchConfiguration_classicLinkVPCSecurityGroups,
    createLaunchConfiguration_ebsOptimized,
    createLaunchConfiguration_iamInstanceProfile,
    createLaunchConfiguration_imageId,
    createLaunchConfiguration_instanceId,
    createLaunchConfiguration_instanceMonitoring,
    createLaunchConfiguration_instanceType,
    createLaunchConfiguration_kernelId,
    createLaunchConfiguration_keyName,
    createLaunchConfiguration_metadataOptions,
    createLaunchConfiguration_placementTenancy,
    createLaunchConfiguration_ramdiskId,
    createLaunchConfiguration_securityGroups,
    createLaunchConfiguration_spotPrice,
    createLaunchConfiguration_userData,
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
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeAdjustmentTypes
    describeAdjustmentTypesResponse_adjustmentTypes,
    describeAdjustmentTypesResponse_httpStatus,

    -- ** DescribeAutoScalingGroups
    describeAutoScalingGroups_autoScalingGroupNames,
    describeAutoScalingGroups_filters,
    describeAutoScalingGroups_maxRecords,
    describeAutoScalingGroups_nextToken,
    describeAutoScalingGroupsResponse_nextToken,
    describeAutoScalingGroupsResponse_httpStatus,
    describeAutoScalingGroupsResponse_autoScalingGroups,

    -- ** DescribeAutoScalingInstances
    describeAutoScalingInstances_instanceIds,
    describeAutoScalingInstances_maxRecords,
    describeAutoScalingInstances_nextToken,
    describeAutoScalingInstancesResponse_autoScalingInstances,
    describeAutoScalingInstancesResponse_nextToken,
    describeAutoScalingInstancesResponse_httpStatus,

    -- ** DescribeAutoScalingNotificationTypes
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,

    -- ** DescribeInstanceRefreshes
    describeInstanceRefreshes_instanceRefreshIds,
    describeInstanceRefreshes_maxRecords,
    describeInstanceRefreshes_nextToken,
    describeInstanceRefreshes_autoScalingGroupName,
    describeInstanceRefreshesResponse_instanceRefreshes,
    describeInstanceRefreshesResponse_nextToken,
    describeInstanceRefreshesResponse_httpStatus,

    -- ** DescribeLaunchConfigurations
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurations_maxRecords,
    describeLaunchConfigurations_nextToken,
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
    describeLoadBalancerTargetGroups_maxRecords,
    describeLoadBalancerTargetGroups_nextToken,
    describeLoadBalancerTargetGroups_autoScalingGroupName,
    describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups,
    describeLoadBalancerTargetGroupsResponse_nextToken,
    describeLoadBalancerTargetGroupsResponse_httpStatus,

    -- ** DescribeLoadBalancers
    describeLoadBalancers_maxRecords,
    describeLoadBalancers_nextToken,
    describeLoadBalancers_autoScalingGroupName,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_nextToken,
    describeLoadBalancersResponse_httpStatus,

    -- ** DescribeMetricCollectionTypes
    describeMetricCollectionTypesResponse_granularities,
    describeMetricCollectionTypesResponse_metrics,
    describeMetricCollectionTypesResponse_httpStatus,

    -- ** DescribeNotificationConfigurations
    describeNotificationConfigurations_autoScalingGroupNames,
    describeNotificationConfigurations_maxRecords,
    describeNotificationConfigurations_nextToken,
    describeNotificationConfigurationsResponse_nextToken,
    describeNotificationConfigurationsResponse_httpStatus,
    describeNotificationConfigurationsResponse_notificationConfigurations,

    -- ** DescribePolicies
    describePolicies_autoScalingGroupName,
    describePolicies_maxRecords,
    describePolicies_nextToken,
    describePolicies_policyNames,
    describePolicies_policyTypes,
    describePoliciesResponse_nextToken,
    describePoliciesResponse_scalingPolicies,
    describePoliciesResponse_httpStatus,

    -- ** DescribeScalingActivities
    describeScalingActivities_activityIds,
    describeScalingActivities_autoScalingGroupName,
    describeScalingActivities_includeDeletedGroups,
    describeScalingActivities_maxRecords,
    describeScalingActivities_nextToken,
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_httpStatus,
    describeScalingActivitiesResponse_activities,

    -- ** DescribeScalingProcessTypes
    describeScalingProcessTypesResponse_processes,
    describeScalingProcessTypesResponse_httpStatus,

    -- ** DescribeScheduledActions
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActions_endTime,
    describeScheduledActions_maxRecords,
    describeScheduledActions_nextToken,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActions_startTime,
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_filters,
    describeTags_maxRecords,
    describeTags_nextToken,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DescribeTerminationPolicyTypes
    describeTerminationPolicyTypesResponse_terminationPolicyTypes,
    describeTerminationPolicyTypesResponse_httpStatus,

    -- ** DescribeTrafficSources
    describeTrafficSources_maxRecords,
    describeTrafficSources_nextToken,
    describeTrafficSources_autoScalingGroupName,
    describeTrafficSources_trafficSourceType,
    describeTrafficSourcesResponse_nextToken,
    describeTrafficSourcesResponse_trafficSources,
    describeTrafficSourcesResponse_httpStatus,

    -- ** DescribeWarmPool
    describeWarmPool_maxRecords,
    describeWarmPool_nextToken,
    describeWarmPool_autoScalingGroupName,
    describeWarmPoolResponse_instances,
    describeWarmPoolResponse_nextToken,
    describeWarmPoolResponse_warmPoolConfiguration,
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

    -- ** DetachTrafficSources
    detachTrafficSources_autoScalingGroupName,
    detachTrafficSources_trafficSources,
    detachTrafficSourcesResponse_httpStatus,

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
    executePolicy_autoScalingGroupName,
    executePolicy_breachThreshold,
    executePolicy_honorCooldown,
    executePolicy_metricValue,
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
    putLifecycleHook_defaultResult,
    putLifecycleHook_heartbeatTimeout,
    putLifecycleHook_lifecycleTransition,
    putLifecycleHook_notificationMetadata,
    putLifecycleHook_notificationTargetARN,
    putLifecycleHook_roleARN,
    putLifecycleHook_lifecycleHookName,
    putLifecycleHook_autoScalingGroupName,
    putLifecycleHookResponse_httpStatus,

    -- ** PutNotificationConfiguration
    putNotificationConfiguration_autoScalingGroupName,
    putNotificationConfiguration_topicARN,
    putNotificationConfiguration_notificationTypes,

    -- ** PutScalingPolicy
    putScalingPolicy_adjustmentType,
    putScalingPolicy_cooldown,
    putScalingPolicy_enabled,
    putScalingPolicy_estimatedInstanceWarmup,
    putScalingPolicy_metricAggregationType,
    putScalingPolicy_minAdjustmentMagnitude,
    putScalingPolicy_minAdjustmentStep,
    putScalingPolicy_policyType,
    putScalingPolicy_predictiveScalingConfiguration,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_stepAdjustments,
    putScalingPolicy_targetTrackingConfiguration,
    putScalingPolicy_autoScalingGroupName,
    putScalingPolicy_policyName,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_policyARN,
    putScalingPolicyResponse_httpStatus,

    -- ** PutScheduledUpdateGroupAction
    putScheduledUpdateGroupAction_desiredCapacity,
    putScheduledUpdateGroupAction_endTime,
    putScheduledUpdateGroupAction_maxSize,
    putScheduledUpdateGroupAction_minSize,
    putScheduledUpdateGroupAction_recurrence,
    putScheduledUpdateGroupAction_startTime,
    putScheduledUpdateGroupAction_time,
    putScheduledUpdateGroupAction_timeZone,
    putScheduledUpdateGroupAction_autoScalingGroupName,
    putScheduledUpdateGroupAction_scheduledActionName,

    -- ** PutWarmPool
    putWarmPool_instanceReusePolicy,
    putWarmPool_maxGroupPreparedCapacity,
    putWarmPool_minSize,
    putWarmPool_poolState,
    putWarmPool_autoScalingGroupName,
    putWarmPoolResponse_httpStatus,

    -- ** RecordLifecycleActionHeartbeat
    recordLifecycleActionHeartbeat_instanceId,
    recordLifecycleActionHeartbeat_lifecycleActionToken,
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
    startInstanceRefresh_desiredConfiguration,
    startInstanceRefresh_preferences,
    startInstanceRefresh_strategy,
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
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_context,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_defaultInstanceWarmup,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_desiredCapacityType,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_autoScalingGroupName,

    -- * Types

    -- ** AcceleratorCountRequest
    acceleratorCountRequest_max,
    acceleratorCountRequest_min,

    -- ** AcceleratorTotalMemoryMiBRequest
    acceleratorTotalMemoryMiBRequest_max,
    acceleratorTotalMemoryMiBRequest_min,

    -- ** Activity
    activity_autoScalingGroupARN,
    activity_autoScalingGroupState,
    activity_description,
    activity_details,
    activity_endTime,
    activity_progress,
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
    autoScalingGroup_autoScalingGroupARN,
    autoScalingGroup_capacityRebalance,
    autoScalingGroup_context,
    autoScalingGroup_defaultInstanceWarmup,
    autoScalingGroup_desiredCapacityType,
    autoScalingGroup_enabledMetrics,
    autoScalingGroup_healthCheckGracePeriod,
    autoScalingGroup_instances,
    autoScalingGroup_launchConfigurationName,
    autoScalingGroup_launchTemplate,
    autoScalingGroup_loadBalancerNames,
    autoScalingGroup_maxInstanceLifetime,
    autoScalingGroup_mixedInstancesPolicy,
    autoScalingGroup_newInstancesProtectedFromScaleIn,
    autoScalingGroup_placementGroup,
    autoScalingGroup_predictedCapacity,
    autoScalingGroup_serviceLinkedRoleARN,
    autoScalingGroup_status,
    autoScalingGroup_suspendedProcesses,
    autoScalingGroup_tags,
    autoScalingGroup_targetGroupARNs,
    autoScalingGroup_terminationPolicies,
    autoScalingGroup_trafficSources,
    autoScalingGroup_vPCZoneIdentifier,
    autoScalingGroup_warmPoolConfiguration,
    autoScalingGroup_warmPoolSize,
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
    autoScalingInstanceDetails_launchConfigurationName,
    autoScalingInstanceDetails_launchTemplate,
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
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_metrics,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,
    customizedMetricSpecification_unit,

    -- ** DesiredConfiguration
    desiredConfiguration_launchTemplate,
    desiredConfiguration_mixedInstancesPolicy,

    -- ** Ebs
    ebs_deleteOnTermination,
    ebs_encrypted,
    ebs_iops,
    ebs_snapshotId,
    ebs_throughput,
    ebs_volumeSize,
    ebs_volumeType,

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
    instance_instanceType,
    instance_launchConfigurationName,
    instance_launchTemplate,
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
    instanceRefresh_autoScalingGroupName,
    instanceRefresh_desiredConfiguration,
    instanceRefresh_endTime,
    instanceRefresh_instanceRefreshId,
    instanceRefresh_instancesToUpdate,
    instanceRefresh_percentageComplete,
    instanceRefresh_preferences,
    instanceRefresh_progressDetails,
    instanceRefresh_startTime,
    instanceRefresh_status,
    instanceRefresh_statusReason,

    -- ** InstanceRefreshLivePoolProgress
    instanceRefreshLivePoolProgress_instancesToUpdate,
    instanceRefreshLivePoolProgress_percentageComplete,

    -- ** InstanceRefreshProgressDetails
    instanceRefreshProgressDetails_livePoolProgress,
    instanceRefreshProgressDetails_warmPoolProgress,

    -- ** InstanceRefreshWarmPoolProgress
    instanceRefreshWarmPoolProgress_instancesToUpdate,
    instanceRefreshWarmPoolProgress_percentageComplete,

    -- ** InstanceRequirements
    instanceRequirements_acceleratorCount,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_acceleratorNames,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_bareMetal,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_instanceGenerations,
    instanceRequirements_localStorage,
    instanceRequirements_localStorageTypes,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_vCpuCount,
    instanceRequirements_memoryMiB,

    -- ** InstanceReusePolicy
    instanceReusePolicy_reuseOnScaleIn,

    -- ** InstancesDistribution
    instancesDistribution_onDemandAllocationStrategy,
    instancesDistribution_onDemandBaseCapacity,
    instancesDistribution_onDemandPercentageAboveBaseCapacity,
    instancesDistribution_spotAllocationStrategy,
    instancesDistribution_spotInstancePools,
    instancesDistribution_spotMaxPrice,

    -- ** LaunchConfiguration
    launchConfiguration_associatePublicIpAddress,
    launchConfiguration_blockDeviceMappings,
    launchConfiguration_classicLinkVPCId,
    launchConfiguration_classicLinkVPCSecurityGroups,
    launchConfiguration_ebsOptimized,
    launchConfiguration_iamInstanceProfile,
    launchConfiguration_instanceMonitoring,
    launchConfiguration_kernelId,
    launchConfiguration_keyName,
    launchConfiguration_launchConfigurationARN,
    launchConfiguration_metadataOptions,
    launchConfiguration_placementTenancy,
    launchConfiguration_ramdiskId,
    launchConfiguration_securityGroups,
    launchConfiguration_spotPrice,
    launchConfiguration_userData,
    launchConfiguration_launchConfigurationName,
    launchConfiguration_imageId,
    launchConfiguration_instanceType,
    launchConfiguration_createdTime,

    -- ** LaunchTemplate
    launchTemplate_launchTemplateSpecification,
    launchTemplate_overrides,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_launchTemplateSpecification,
    launchTemplateOverrides_weightedCapacity,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LifecycleHook
    lifecycleHook_autoScalingGroupName,
    lifecycleHook_defaultResult,
    lifecycleHook_globalTimeout,
    lifecycleHook_heartbeatTimeout,
    lifecycleHook_lifecycleHookName,
    lifecycleHook_lifecycleTransition,
    lifecycleHook_notificationMetadata,
    lifecycleHook_notificationTargetARN,
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
    loadBalancerState_loadBalancerName,
    loadBalancerState_state,

    -- ** LoadBalancerTargetGroupState
    loadBalancerTargetGroupState_loadBalancerTargetGroupARN,
    loadBalancerTargetGroupState_state,

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
    metricDataQuery_expression,
    metricDataQuery_label,
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
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
    notificationConfiguration_autoScalingGroupName,
    notificationConfiguration_notificationType,
    notificationConfiguration_topicARN,

    -- ** PredefinedMetricSpecification
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- ** PredictiveScalingConfiguration
    predictiveScalingConfiguration_maxCapacityBreachBehavior,
    predictiveScalingConfiguration_maxCapacityBuffer,
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
    predictiveScalingMetricSpecification_customizedCapacityMetricSpecification,
    predictiveScalingMetricSpecification_customizedLoadMetricSpecification,
    predictiveScalingMetricSpecification_customizedScalingMetricSpecification,
    predictiveScalingMetricSpecification_predefinedLoadMetricSpecification,
    predictiveScalingMetricSpecification_predefinedMetricPairSpecification,
    predictiveScalingMetricSpecification_predefinedScalingMetricSpecification,
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
    refreshPreferences_checkpointPercentages,
    refreshPreferences_instanceWarmup,
    refreshPreferences_minHealthyPercentage,
    refreshPreferences_skipMatching,

    -- ** ScalingPolicy
    scalingPolicy_adjustmentType,
    scalingPolicy_alarms,
    scalingPolicy_autoScalingGroupName,
    scalingPolicy_cooldown,
    scalingPolicy_enabled,
    scalingPolicy_estimatedInstanceWarmup,
    scalingPolicy_metricAggregationType,
    scalingPolicy_minAdjustmentMagnitude,
    scalingPolicy_minAdjustmentStep,
    scalingPolicy_policyARN,
    scalingPolicy_policyName,
    scalingPolicy_policyType,
    scalingPolicy_predictiveScalingConfiguration,
    scalingPolicy_scalingAdjustment,
    scalingPolicy_stepAdjustments,
    scalingPolicy_targetTrackingConfiguration,

    -- ** ScalingProcessQuery
    scalingProcessQuery_scalingProcesses,
    scalingProcessQuery_autoScalingGroupName,

    -- ** ScheduledUpdateGroupAction
    scheduledUpdateGroupAction_autoScalingGroupName,
    scheduledUpdateGroupAction_desiredCapacity,
    scheduledUpdateGroupAction_endTime,
    scheduledUpdateGroupAction_maxSize,
    scheduledUpdateGroupAction_minSize,
    scheduledUpdateGroupAction_recurrence,
    scheduledUpdateGroupAction_scheduledActionARN,
    scheduledUpdateGroupAction_scheduledActionName,
    scheduledUpdateGroupAction_startTime,
    scheduledUpdateGroupAction_time,
    scheduledUpdateGroupAction_timeZone,

    -- ** ScheduledUpdateGroupActionRequest
    scheduledUpdateGroupActionRequest_desiredCapacity,
    scheduledUpdateGroupActionRequest_endTime,
    scheduledUpdateGroupActionRequest_maxSize,
    scheduledUpdateGroupActionRequest_minSize,
    scheduledUpdateGroupActionRequest_recurrence,
    scheduledUpdateGroupActionRequest_startTime,
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
    targetTrackingConfiguration_customizedMetricSpecification,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_predefinedMetricSpecification,
    targetTrackingConfiguration_targetValue,

    -- ** TargetTrackingMetricDataQuery
    targetTrackingMetricDataQuery_expression,
    targetTrackingMetricDataQuery_label,
    targetTrackingMetricDataQuery_metricStat,
    targetTrackingMetricDataQuery_returnData,
    targetTrackingMetricDataQuery_id,

    -- ** TargetTrackingMetricStat
    targetTrackingMetricStat_unit,
    targetTrackingMetricStat_metric,
    targetTrackingMetricStat_stat,

    -- ** TotalLocalStorageGBRequest
    totalLocalStorageGBRequest_max,
    totalLocalStorageGBRequest_min,

    -- ** TrafficSourceIdentifier
    trafficSourceIdentifier_identifier,

    -- ** TrafficSourceState
    trafficSourceState_state,
    trafficSourceState_trafficSource,

    -- ** VCpuCountRequest
    vCpuCountRequest_max,
    vCpuCountRequest_min,

    -- ** WarmPoolConfiguration
    warmPoolConfiguration_instanceReusePolicy,
    warmPoolConfiguration_maxGroupPreparedCapacity,
    warmPoolConfiguration_minSize,
    warmPoolConfiguration_poolState,
    warmPoolConfiguration_status,
  )
where

import Amazonka.AutoScaling.AttachInstances
import Amazonka.AutoScaling.AttachLoadBalancerTargetGroups
import Amazonka.AutoScaling.AttachLoadBalancers
import Amazonka.AutoScaling.AttachTrafficSources
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
import Amazonka.AutoScaling.DescribeTrafficSources
import Amazonka.AutoScaling.DescribeWarmPool
import Amazonka.AutoScaling.DetachInstances
import Amazonka.AutoScaling.DetachLoadBalancerTargetGroups
import Amazonka.AutoScaling.DetachLoadBalancers
import Amazonka.AutoScaling.DetachTrafficSources
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
import Amazonka.AutoScaling.Types.TargetTrackingMetricDataQuery
import Amazonka.AutoScaling.Types.TargetTrackingMetricStat
import Amazonka.AutoScaling.Types.TotalLocalStorageGBRequest
import Amazonka.AutoScaling.Types.TrafficSourceIdentifier
import Amazonka.AutoScaling.Types.TrafficSourceState
import Amazonka.AutoScaling.Types.VCpuCountRequest
import Amazonka.AutoScaling.Types.WarmPoolConfiguration
import Amazonka.AutoScaling.UpdateAutoScalingGroup
