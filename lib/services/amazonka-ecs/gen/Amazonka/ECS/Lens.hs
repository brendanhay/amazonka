{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Lens
  ( -- * Operations

    -- ** CreateCapacityProvider
    createCapacityProvider_tags,
    createCapacityProvider_name,
    createCapacityProvider_autoScalingGroupProvider,
    createCapacityProviderResponse_capacityProvider,
    createCapacityProviderResponse_httpStatus,

    -- ** CreateCluster
    createCluster_capacityProviders,
    createCluster_clusterName,
    createCluster_configuration,
    createCluster_defaultCapacityProviderStrategy,
    createCluster_serviceConnectDefaults,
    createCluster_settings,
    createCluster_tags,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateService
    createService_capacityProviderStrategy,
    createService_clientToken,
    createService_cluster,
    createService_deploymentConfiguration,
    createService_deploymentController,
    createService_desiredCount,
    createService_enableECSManagedTags,
    createService_enableExecuteCommand,
    createService_healthCheckGracePeriodSeconds,
    createService_launchType,
    createService_loadBalancers,
    createService_networkConfiguration,
    createService_placementConstraints,
    createService_placementStrategy,
    createService_platformVersion,
    createService_propagateTags,
    createService_role,
    createService_schedulingStrategy,
    createService_serviceConnectConfiguration,
    createService_serviceRegistries,
    createService_tags,
    createService_taskDefinition,
    createService_serviceName,
    createServiceResponse_service,
    createServiceResponse_httpStatus,

    -- ** CreateTaskSet
    createTaskSet_capacityProviderStrategy,
    createTaskSet_clientToken,
    createTaskSet_externalId,
    createTaskSet_launchType,
    createTaskSet_loadBalancers,
    createTaskSet_networkConfiguration,
    createTaskSet_platformVersion,
    createTaskSet_scale,
    createTaskSet_serviceRegistries,
    createTaskSet_tags,
    createTaskSet_service,
    createTaskSet_cluster,
    createTaskSet_taskDefinition,
    createTaskSetResponse_taskSet,
    createTaskSetResponse_httpStatus,

    -- ** DeleteAccountSetting
    deleteAccountSetting_principalArn,
    deleteAccountSetting_name,
    deleteAccountSettingResponse_setting,
    deleteAccountSettingResponse_httpStatus,

    -- ** DeleteAttributes
    deleteAttributes_cluster,
    deleteAttributes_attributes,
    deleteAttributesResponse_attributes,
    deleteAttributesResponse_httpStatus,

    -- ** DeleteCapacityProvider
    deleteCapacityProvider_capacityProvider,
    deleteCapacityProviderResponse_capacityProvider,
    deleteCapacityProviderResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_cluster,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteService
    deleteService_cluster,
    deleteService_force,
    deleteService_service,
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,

    -- ** DeleteTaskSet
    deleteTaskSet_force,
    deleteTaskSet_cluster,
    deleteTaskSet_service,
    deleteTaskSet_taskSet,
    deleteTaskSetResponse_taskSet,
    deleteTaskSetResponse_httpStatus,

    -- ** DeregisterContainerInstance
    deregisterContainerInstance_cluster,
    deregisterContainerInstance_force,
    deregisterContainerInstance_containerInstance,
    deregisterContainerInstanceResponse_containerInstance,
    deregisterContainerInstanceResponse_httpStatus,

    -- ** DeregisterTaskDefinition
    deregisterTaskDefinition_taskDefinition,
    deregisterTaskDefinitionResponse_taskDefinition,
    deregisterTaskDefinitionResponse_httpStatus,

    -- ** DescribeCapacityProviders
    describeCapacityProviders_capacityProviders,
    describeCapacityProviders_include,
    describeCapacityProviders_maxResults,
    describeCapacityProviders_nextToken,
    describeCapacityProvidersResponse_capacityProviders,
    describeCapacityProvidersResponse_failures,
    describeCapacityProvidersResponse_nextToken,
    describeCapacityProvidersResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_clusters,
    describeClusters_include,
    describeClustersResponse_clusters,
    describeClustersResponse_failures,
    describeClustersResponse_httpStatus,

    -- ** DescribeContainerInstances
    describeContainerInstances_cluster,
    describeContainerInstances_include,
    describeContainerInstances_containerInstances,
    describeContainerInstancesResponse_containerInstances,
    describeContainerInstancesResponse_failures,
    describeContainerInstancesResponse_httpStatus,

    -- ** DescribeServices
    describeServices_cluster,
    describeServices_include,
    describeServices_services,
    describeServicesResponse_failures,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** DescribeTaskDefinition
    describeTaskDefinition_include,
    describeTaskDefinition_taskDefinition,
    describeTaskDefinitionResponse_tags,
    describeTaskDefinitionResponse_taskDefinition,
    describeTaskDefinitionResponse_httpStatus,

    -- ** DescribeTaskSets
    describeTaskSets_include,
    describeTaskSets_taskSets,
    describeTaskSets_cluster,
    describeTaskSets_service,
    describeTaskSetsResponse_failures,
    describeTaskSetsResponse_taskSets,
    describeTaskSetsResponse_httpStatus,

    -- ** DescribeTasks
    describeTasks_cluster,
    describeTasks_include,
    describeTasks_tasks,
    describeTasksResponse_failures,
    describeTasksResponse_tasks,
    describeTasksResponse_httpStatus,

    -- ** DiscoverPollEndpoint
    discoverPollEndpoint_cluster,
    discoverPollEndpoint_containerInstance,
    discoverPollEndpointResponse_endpoint,
    discoverPollEndpointResponse_serviceConnectEndpoint,
    discoverPollEndpointResponse_telemetryEndpoint,
    discoverPollEndpointResponse_httpStatus,

    -- ** ExecuteCommand
    executeCommand_cluster,
    executeCommand_container,
    executeCommand_command,
    executeCommand_interactive,
    executeCommand_task,
    executeCommandResponse_clusterArn,
    executeCommandResponse_containerArn,
    executeCommandResponse_containerName,
    executeCommandResponse_interactive,
    executeCommandResponse_session,
    executeCommandResponse_taskArn,
    executeCommandResponse_httpStatus,

    -- ** GetTaskProtection
    getTaskProtection_tasks,
    getTaskProtection_cluster,
    getTaskProtectionResponse_failures,
    getTaskProtectionResponse_protectedTasks,
    getTaskProtectionResponse_httpStatus,

    -- ** ListAccountSettings
    listAccountSettings_effectiveSettings,
    listAccountSettings_maxResults,
    listAccountSettings_name,
    listAccountSettings_nextToken,
    listAccountSettings_principalArn,
    listAccountSettings_value,
    listAccountSettingsResponse_nextToken,
    listAccountSettingsResponse_settings,
    listAccountSettingsResponse_httpStatus,

    -- ** ListAttributes
    listAttributes_attributeName,
    listAttributes_attributeValue,
    listAttributes_cluster,
    listAttributes_maxResults,
    listAttributes_nextToken,
    listAttributes_targetType,
    listAttributesResponse_attributes,
    listAttributesResponse_nextToken,
    listAttributesResponse_httpStatus,

    -- ** ListClusters
    listClusters_maxResults,
    listClusters_nextToken,
    listClustersResponse_clusterArns,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListContainerInstances
    listContainerInstances_cluster,
    listContainerInstances_filter,
    listContainerInstances_maxResults,
    listContainerInstances_nextToken,
    listContainerInstances_status,
    listContainerInstancesResponse_containerInstanceArns,
    listContainerInstancesResponse_nextToken,
    listContainerInstancesResponse_httpStatus,

    -- ** ListServices
    listServices_cluster,
    listServices_launchType,
    listServices_maxResults,
    listServices_nextToken,
    listServices_schedulingStrategy,
    listServicesResponse_nextToken,
    listServicesResponse_serviceArns,
    listServicesResponse_httpStatus,

    -- ** ListServicesByNamespace
    listServicesByNamespace_maxResults,
    listServicesByNamespace_nextToken,
    listServicesByNamespace_namespace,
    listServicesByNamespaceResponse_nextToken,
    listServicesByNamespaceResponse_serviceArns,
    listServicesByNamespaceResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTaskDefinitionFamilies
    listTaskDefinitionFamilies_familyPrefix,
    listTaskDefinitionFamilies_maxResults,
    listTaskDefinitionFamilies_nextToken,
    listTaskDefinitionFamilies_status,
    listTaskDefinitionFamiliesResponse_families,
    listTaskDefinitionFamiliesResponse_nextToken,
    listTaskDefinitionFamiliesResponse_httpStatus,

    -- ** ListTaskDefinitions
    listTaskDefinitions_familyPrefix,
    listTaskDefinitions_maxResults,
    listTaskDefinitions_nextToken,
    listTaskDefinitions_sort,
    listTaskDefinitions_status,
    listTaskDefinitionsResponse_nextToken,
    listTaskDefinitionsResponse_taskDefinitionArns,
    listTaskDefinitionsResponse_httpStatus,

    -- ** ListTasks
    listTasks_cluster,
    listTasks_containerInstance,
    listTasks_desiredStatus,
    listTasks_family,
    listTasks_launchType,
    listTasks_maxResults,
    listTasks_nextToken,
    listTasks_serviceName,
    listTasks_startedBy,
    listTasksResponse_nextToken,
    listTasksResponse_taskArns,
    listTasksResponse_httpStatus,

    -- ** PutAccountSetting
    putAccountSetting_principalArn,
    putAccountSetting_name,
    putAccountSetting_value,
    putAccountSettingResponse_setting,
    putAccountSettingResponse_httpStatus,

    -- ** PutAccountSettingDefault
    putAccountSettingDefault_name,
    putAccountSettingDefault_value,
    putAccountSettingDefaultResponse_setting,
    putAccountSettingDefaultResponse_httpStatus,

    -- ** PutAttributes
    putAttributes_cluster,
    putAttributes_attributes,
    putAttributesResponse_attributes,
    putAttributesResponse_httpStatus,

    -- ** PutClusterCapacityProviders
    putClusterCapacityProviders_cluster,
    putClusterCapacityProviders_capacityProviders,
    putClusterCapacityProviders_defaultCapacityProviderStrategy,
    putClusterCapacityProvidersResponse_cluster,
    putClusterCapacityProvidersResponse_httpStatus,

    -- ** RegisterContainerInstance
    registerContainerInstance_attributes,
    registerContainerInstance_cluster,
    registerContainerInstance_containerInstanceArn,
    registerContainerInstance_instanceIdentityDocument,
    registerContainerInstance_instanceIdentityDocumentSignature,
    registerContainerInstance_platformDevices,
    registerContainerInstance_tags,
    registerContainerInstance_totalResources,
    registerContainerInstance_versionInfo,
    registerContainerInstanceResponse_containerInstance,
    registerContainerInstanceResponse_httpStatus,

    -- ** RegisterTaskDefinition
    registerTaskDefinition_cpu,
    registerTaskDefinition_ephemeralStorage,
    registerTaskDefinition_executionRoleArn,
    registerTaskDefinition_inferenceAccelerators,
    registerTaskDefinition_ipcMode,
    registerTaskDefinition_memory,
    registerTaskDefinition_networkMode,
    registerTaskDefinition_pidMode,
    registerTaskDefinition_placementConstraints,
    registerTaskDefinition_proxyConfiguration,
    registerTaskDefinition_requiresCompatibilities,
    registerTaskDefinition_runtimePlatform,
    registerTaskDefinition_tags,
    registerTaskDefinition_taskRoleArn,
    registerTaskDefinition_volumes,
    registerTaskDefinition_family,
    registerTaskDefinition_containerDefinitions,
    registerTaskDefinitionResponse_tags,
    registerTaskDefinitionResponse_taskDefinition,
    registerTaskDefinitionResponse_httpStatus,

    -- ** RunTask
    runTask_capacityProviderStrategy,
    runTask_cluster,
    runTask_count,
    runTask_enableECSManagedTags,
    runTask_enableExecuteCommand,
    runTask_group,
    runTask_launchType,
    runTask_networkConfiguration,
    runTask_overrides,
    runTask_placementConstraints,
    runTask_placementStrategy,
    runTask_platformVersion,
    runTask_propagateTags,
    runTask_referenceId,
    runTask_startedBy,
    runTask_tags,
    runTask_taskDefinition,
    runTaskResponse_failures,
    runTaskResponse_tasks,
    runTaskResponse_httpStatus,

    -- ** StartTask
    startTask_cluster,
    startTask_enableECSManagedTags,
    startTask_enableExecuteCommand,
    startTask_group,
    startTask_networkConfiguration,
    startTask_overrides,
    startTask_propagateTags,
    startTask_referenceId,
    startTask_startedBy,
    startTask_tags,
    startTask_containerInstances,
    startTask_taskDefinition,
    startTaskResponse_failures,
    startTaskResponse_tasks,
    startTaskResponse_httpStatus,

    -- ** StopTask
    stopTask_cluster,
    stopTask_reason,
    stopTask_task,
    stopTaskResponse_task,
    stopTaskResponse_httpStatus,

    -- ** SubmitAttachmentStateChanges
    submitAttachmentStateChanges_cluster,
    submitAttachmentStateChanges_attachments,
    submitAttachmentStateChangesResponse_acknowledgment,
    submitAttachmentStateChangesResponse_httpStatus,

    -- ** SubmitContainerStateChange
    submitContainerStateChange_cluster,
    submitContainerStateChange_containerName,
    submitContainerStateChange_exitCode,
    submitContainerStateChange_networkBindings,
    submitContainerStateChange_reason,
    submitContainerStateChange_runtimeId,
    submitContainerStateChange_status,
    submitContainerStateChange_task,
    submitContainerStateChangeResponse_acknowledgment,
    submitContainerStateChangeResponse_httpStatus,

    -- ** SubmitTaskStateChange
    submitTaskStateChange_attachments,
    submitTaskStateChange_cluster,
    submitTaskStateChange_containers,
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_managedAgents,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_reason,
    submitTaskStateChange_status,
    submitTaskStateChange_task,
    submitTaskStateChangeResponse_acknowledgment,
    submitTaskStateChangeResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCapacityProvider
    updateCapacityProvider_name,
    updateCapacityProvider_autoScalingGroupProvider,
    updateCapacityProviderResponse_capacityProvider,
    updateCapacityProviderResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_configuration,
    updateCluster_serviceConnectDefaults,
    updateCluster_settings,
    updateCluster_cluster,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** UpdateClusterSettings
    updateClusterSettings_cluster,
    updateClusterSettings_settings,
    updateClusterSettingsResponse_cluster,
    updateClusterSettingsResponse_httpStatus,

    -- ** UpdateContainerAgent
    updateContainerAgent_cluster,
    updateContainerAgent_containerInstance,
    updateContainerAgentResponse_containerInstance,
    updateContainerAgentResponse_httpStatus,

    -- ** UpdateContainerInstancesState
    updateContainerInstancesState_cluster,
    updateContainerInstancesState_containerInstances,
    updateContainerInstancesState_status,
    updateContainerInstancesStateResponse_containerInstances,
    updateContainerInstancesStateResponse_failures,
    updateContainerInstancesStateResponse_httpStatus,

    -- ** UpdateService
    updateService_capacityProviderStrategy,
    updateService_cluster,
    updateService_deploymentConfiguration,
    updateService_desiredCount,
    updateService_enableECSManagedTags,
    updateService_enableExecuteCommand,
    updateService_forceNewDeployment,
    updateService_healthCheckGracePeriodSeconds,
    updateService_loadBalancers,
    updateService_networkConfiguration,
    updateService_placementConstraints,
    updateService_placementStrategy,
    updateService_platformVersion,
    updateService_propagateTags,
    updateService_serviceConnectConfiguration,
    updateService_serviceRegistries,
    updateService_taskDefinition,
    updateService_service,
    updateServiceResponse_service,
    updateServiceResponse_httpStatus,

    -- ** UpdateServicePrimaryTaskSet
    updateServicePrimaryTaskSet_cluster,
    updateServicePrimaryTaskSet_service,
    updateServicePrimaryTaskSet_primaryTaskSet,
    updateServicePrimaryTaskSetResponse_taskSet,
    updateServicePrimaryTaskSetResponse_httpStatus,

    -- ** UpdateTaskProtection
    updateTaskProtection_expiresInMinutes,
    updateTaskProtection_cluster,
    updateTaskProtection_tasks,
    updateTaskProtection_protectionEnabled,
    updateTaskProtectionResponse_failures,
    updateTaskProtectionResponse_protectedTasks,
    updateTaskProtectionResponse_httpStatus,

    -- ** UpdateTaskSet
    updateTaskSet_cluster,
    updateTaskSet_service,
    updateTaskSet_taskSet,
    updateTaskSet_scale,
    updateTaskSetResponse_taskSet,
    updateTaskSetResponse_httpStatus,

    -- * Types

    -- ** Attachment
    attachment_details,
    attachment_id,
    attachment_status,
    attachment_type,

    -- ** AttachmentStateChange
    attachmentStateChange_attachmentArn,
    attachmentStateChange_status,

    -- ** Attribute
    attribute_targetId,
    attribute_targetType,
    attribute_value,
    attribute_name,

    -- ** AutoScalingGroupProvider
    autoScalingGroupProvider_managedScaling,
    autoScalingGroupProvider_managedTerminationProtection,
    autoScalingGroupProvider_autoScalingGroupArn,

    -- ** AutoScalingGroupProviderUpdate
    autoScalingGroupProviderUpdate_managedScaling,
    autoScalingGroupProviderUpdate_managedTerminationProtection,

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- ** CapacityProvider
    capacityProvider_autoScalingGroupProvider,
    capacityProvider_capacityProviderArn,
    capacityProvider_name,
    capacityProvider_status,
    capacityProvider_tags,
    capacityProvider_updateStatus,
    capacityProvider_updateStatusReason,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** Cluster
    cluster_activeServicesCount,
    cluster_attachments,
    cluster_attachmentsStatus,
    cluster_capacityProviders,
    cluster_clusterArn,
    cluster_clusterName,
    cluster_configuration,
    cluster_defaultCapacityProviderStrategy,
    cluster_pendingTasksCount,
    cluster_registeredContainerInstancesCount,
    cluster_runningTasksCount,
    cluster_serviceConnectDefaults,
    cluster_settings,
    cluster_statistics,
    cluster_status,
    cluster_tags,

    -- ** ClusterConfiguration
    clusterConfiguration_executeCommandConfiguration,

    -- ** ClusterServiceConnectDefaults
    clusterServiceConnectDefaults_namespace,

    -- ** ClusterServiceConnectDefaultsRequest
    clusterServiceConnectDefaultsRequest_namespace,

    -- ** ClusterSetting
    clusterSetting_name,
    clusterSetting_value,

    -- ** Container
    container_containerArn,
    container_cpu,
    container_exitCode,
    container_gpuIds,
    container_healthStatus,
    container_image,
    container_imageDigest,
    container_lastStatus,
    container_managedAgents,
    container_memory,
    container_memoryReservation,
    container_name,
    container_networkBindings,
    container_networkInterfaces,
    container_reason,
    container_runtimeId,
    container_taskArn,

    -- ** ContainerDefinition
    containerDefinition_command,
    containerDefinition_cpu,
    containerDefinition_dependsOn,
    containerDefinition_disableNetworking,
    containerDefinition_dnsSearchDomains,
    containerDefinition_dnsServers,
    containerDefinition_dockerLabels,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_entryPoint,
    containerDefinition_environment,
    containerDefinition_environmentFiles,
    containerDefinition_essential,
    containerDefinition_extraHosts,
    containerDefinition_firelensConfiguration,
    containerDefinition_healthCheck,
    containerDefinition_hostname,
    containerDefinition_image,
    containerDefinition_interactive,
    containerDefinition_links,
    containerDefinition_linuxParameters,
    containerDefinition_logConfiguration,
    containerDefinition_memory,
    containerDefinition_memoryReservation,
    containerDefinition_mountPoints,
    containerDefinition_name,
    containerDefinition_portMappings,
    containerDefinition_privileged,
    containerDefinition_pseudoTerminal,
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_repositoryCredentials,
    containerDefinition_resourceRequirements,
    containerDefinition_secrets,
    containerDefinition_startTimeout,
    containerDefinition_stopTimeout,
    containerDefinition_systemControls,
    containerDefinition_ulimits,
    containerDefinition_user,
    containerDefinition_volumesFrom,
    containerDefinition_workingDirectory,

    -- ** ContainerDependency
    containerDependency_containerName,
    containerDependency_condition,

    -- ** ContainerInstance
    containerInstance_agentConnected,
    containerInstance_agentUpdateStatus,
    containerInstance_attachments,
    containerInstance_attributes,
    containerInstance_capacityProviderName,
    containerInstance_containerInstanceArn,
    containerInstance_ec2InstanceId,
    containerInstance_healthStatus,
    containerInstance_pendingTasksCount,
    containerInstance_registeredAt,
    containerInstance_registeredResources,
    containerInstance_remainingResources,
    containerInstance_runningTasksCount,
    containerInstance_status,
    containerInstance_statusReason,
    containerInstance_tags,
    containerInstance_version,
    containerInstance_versionInfo,

    -- ** ContainerInstanceHealthStatus
    containerInstanceHealthStatus_details,
    containerInstanceHealthStatus_overallStatus,

    -- ** ContainerOverride
    containerOverride_command,
    containerOverride_cpu,
    containerOverride_environment,
    containerOverride_environmentFiles,
    containerOverride_memory,
    containerOverride_memoryReservation,
    containerOverride_name,
    containerOverride_resourceRequirements,

    -- ** ContainerService
    containerService_capacityProviderStrategy,
    containerService_clusterArn,
    containerService_createdAt,
    containerService_createdBy,
    containerService_deploymentConfiguration,
    containerService_deploymentController,
    containerService_deployments,
    containerService_desiredCount,
    containerService_enableECSManagedTags,
    containerService_enableExecuteCommand,
    containerService_events,
    containerService_healthCheckGracePeriodSeconds,
    containerService_launchType,
    containerService_loadBalancers,
    containerService_networkConfiguration,
    containerService_pendingCount,
    containerService_placementConstraints,
    containerService_placementStrategy,
    containerService_platformFamily,
    containerService_platformVersion,
    containerService_propagateTags,
    containerService_roleArn,
    containerService_runningCount,
    containerService_schedulingStrategy,
    containerService_serviceArn,
    containerService_serviceName,
    containerService_serviceRegistries,
    containerService_status,
    containerService_tags,
    containerService_taskDefinition,
    containerService_taskSets,

    -- ** ContainerStateChange
    containerStateChange_containerName,
    containerStateChange_exitCode,
    containerStateChange_imageDigest,
    containerStateChange_networkBindings,
    containerStateChange_reason,
    containerStateChange_runtimeId,
    containerStateChange_status,

    -- ** Deployment
    deployment_capacityProviderStrategy,
    deployment_createdAt,
    deployment_desiredCount,
    deployment_failedTasks,
    deployment_id,
    deployment_launchType,
    deployment_networkConfiguration,
    deployment_pendingCount,
    deployment_platformFamily,
    deployment_platformVersion,
    deployment_rolloutState,
    deployment_rolloutStateReason,
    deployment_runningCount,
    deployment_serviceConnectConfiguration,
    deployment_serviceConnectResources,
    deployment_status,
    deployment_taskDefinition,
    deployment_updatedAt,

    -- ** DeploymentAlarms
    deploymentAlarms_alarmNames,
    deploymentAlarms_enable,
    deploymentAlarms_rollback,

    -- ** DeploymentCircuitBreaker
    deploymentCircuitBreaker_enable,
    deploymentCircuitBreaker_rollback,

    -- ** DeploymentConfiguration
    deploymentConfiguration_alarms,
    deploymentConfiguration_deploymentCircuitBreaker,
    deploymentConfiguration_maximumPercent,
    deploymentConfiguration_minimumHealthyPercent,

    -- ** DeploymentController
    deploymentController_type,

    -- ** Device
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- ** DockerVolumeConfiguration
    dockerVolumeConfiguration_autoprovision,
    dockerVolumeConfiguration_driver,
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_labels,
    dockerVolumeConfiguration_scope,

    -- ** EFSAuthorizationConfig
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- ** EFSVolumeConfiguration
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_fileSystemId,

    -- ** EnvironmentFile
    environmentFile_value,
    environmentFile_type,

    -- ** EphemeralStorage
    ephemeralStorage_sizeInGiB,

    -- ** ExecuteCommandConfiguration
    executeCommandConfiguration_kmsKeyId,
    executeCommandConfiguration_logConfiguration,
    executeCommandConfiguration_logging,

    -- ** ExecuteCommandLogConfiguration
    executeCommandLogConfiguration_cloudWatchEncryptionEnabled,
    executeCommandLogConfiguration_cloudWatchLogGroupName,
    executeCommandLogConfiguration_s3BucketName,
    executeCommandLogConfiguration_s3EncryptionEnabled,
    executeCommandLogConfiguration_s3KeyPrefix,

    -- ** FSxWindowsFileServerAuthorizationConfig
    fSxWindowsFileServerAuthorizationConfig_credentialsParameter,
    fSxWindowsFileServerAuthorizationConfig_domain,

    -- ** FSxWindowsFileServerVolumeConfiguration
    fSxWindowsFileServerVolumeConfiguration_fileSystemId,
    fSxWindowsFileServerVolumeConfiguration_rootDirectory,
    fSxWindowsFileServerVolumeConfiguration_authorizationConfig,

    -- ** Failure
    failure_arn,
    failure_detail,
    failure_reason,

    -- ** FirelensConfiguration
    firelensConfiguration_options,
    firelensConfiguration_type,

    -- ** HealthCheck
    healthCheck_interval,
    healthCheck_retries,
    healthCheck_startPeriod,
    healthCheck_timeout,
    healthCheck_command,

    -- ** HostEntry
    hostEntry_hostname,
    hostEntry_ipAddress,

    -- ** HostVolumeProperties
    hostVolumeProperties_sourcePath,

    -- ** InferenceAccelerator
    inferenceAccelerator_deviceName,
    inferenceAccelerator_deviceType,

    -- ** InferenceAcceleratorOverride
    inferenceAcceleratorOverride_deviceName,
    inferenceAcceleratorOverride_deviceType,

    -- ** InstanceHealthCheckResult
    instanceHealthCheckResult_lastStatusChange,
    instanceHealthCheckResult_lastUpdated,
    instanceHealthCheckResult_status,
    instanceHealthCheckResult_type,

    -- ** KernelCapabilities
    kernelCapabilities_add,
    kernelCapabilities_drop,

    -- ** KeyValuePair
    keyValuePair_name,
    keyValuePair_value,

    -- ** LinuxParameters
    linuxParameters_capabilities,
    linuxParameters_devices,
    linuxParameters_initProcessEnabled,
    linuxParameters_maxSwap,
    linuxParameters_sharedMemorySize,
    linuxParameters_swappiness,
    linuxParameters_tmpfs,

    -- ** LoadBalancer
    loadBalancer_containerName,
    loadBalancer_containerPort,
    loadBalancer_loadBalancerName,
    loadBalancer_targetGroupArn,

    -- ** LogConfiguration
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- ** ManagedAgent
    managedAgent_lastStartedAt,
    managedAgent_lastStatus,
    managedAgent_name,
    managedAgent_reason,

    -- ** ManagedAgentStateChange
    managedAgentStateChange_reason,
    managedAgentStateChange_containerName,
    managedAgentStateChange_managedAgentName,
    managedAgentStateChange_status,

    -- ** ManagedScaling
    managedScaling_instanceWarmupPeriod,
    managedScaling_maximumScalingStepSize,
    managedScaling_minimumScalingStepSize,
    managedScaling_status,
    managedScaling_targetCapacity,

    -- ** MountPoint
    mountPoint_containerPath,
    mountPoint_readOnly,
    mountPoint_sourceVolume,

    -- ** NetworkBinding
    networkBinding_bindIP,
    networkBinding_containerPort,
    networkBinding_containerPortRange,
    networkBinding_hostPort,
    networkBinding_hostPortRange,
    networkBinding_protocol,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** NetworkInterface
    networkInterface_attachmentId,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,

    -- ** PlacementConstraint
    placementConstraint_expression,
    placementConstraint_type,

    -- ** PlacementStrategy
    placementStrategy_field,
    placementStrategy_type,

    -- ** PlatformDevice
    platformDevice_id,
    platformDevice_type,

    -- ** PortMapping
    portMapping_appProtocol,
    portMapping_containerPort,
    portMapping_containerPortRange,
    portMapping_hostPort,
    portMapping_name,
    portMapping_protocol,

    -- ** ProtectedTask
    protectedTask_expirationDate,
    protectedTask_protectionEnabled,
    protectedTask_taskArn,

    -- ** ProxyConfiguration
    proxyConfiguration_properties,
    proxyConfiguration_type,
    proxyConfiguration_containerName,

    -- ** RepositoryCredentials
    repositoryCredentials_credentialsParameter,

    -- ** Resource
    resource_doubleValue,
    resource_integerValue,
    resource_longValue,
    resource_name,
    resource_stringSetValue,
    resource_type,

    -- ** ResourceRequirement
    resourceRequirement_value,
    resourceRequirement_type,

    -- ** RuntimePlatform
    runtimePlatform_cpuArchitecture,
    runtimePlatform_operatingSystemFamily,

    -- ** Scale
    scale_unit,
    scale_value,

    -- ** Secret
    secret_name,
    secret_valueFrom,

    -- ** ServiceConnectClientAlias
    serviceConnectClientAlias_dnsName,
    serviceConnectClientAlias_port,

    -- ** ServiceConnectConfiguration
    serviceConnectConfiguration_logConfiguration,
    serviceConnectConfiguration_namespace,
    serviceConnectConfiguration_services,
    serviceConnectConfiguration_enabled,

    -- ** ServiceConnectService
    serviceConnectService_clientAliases,
    serviceConnectService_discoveryName,
    serviceConnectService_ingressPortOverride,
    serviceConnectService_portName,

    -- ** ServiceConnectServiceResource
    serviceConnectServiceResource_discoveryArn,
    serviceConnectServiceResource_discoveryName,

    -- ** ServiceEvent
    serviceEvent_createdAt,
    serviceEvent_id,
    serviceEvent_message,

    -- ** ServiceRegistry
    serviceRegistry_containerName,
    serviceRegistry_containerPort,
    serviceRegistry_port,
    serviceRegistry_registryArn,

    -- ** Session
    session_sessionId,
    session_streamUrl,
    session_tokenValue,

    -- ** Setting
    setting_name,
    setting_principalArn,
    setting_value,

    -- ** SystemControl
    systemControl_namespace,
    systemControl_value,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Task
    task_attachments,
    task_attributes,
    task_availabilityZone,
    task_capacityProviderName,
    task_clusterArn,
    task_connectivity,
    task_connectivityAt,
    task_containerInstanceArn,
    task_containers,
    task_cpu,
    task_createdAt,
    task_desiredStatus,
    task_enableExecuteCommand,
    task_ephemeralStorage,
    task_executionStoppedAt,
    task_group,
    task_healthStatus,
    task_inferenceAccelerators,
    task_lastStatus,
    task_launchType,
    task_memory,
    task_overrides,
    task_platformFamily,
    task_platformVersion,
    task_pullStartedAt,
    task_pullStoppedAt,
    task_startedAt,
    task_startedBy,
    task_stopCode,
    task_stoppedAt,
    task_stoppedReason,
    task_stoppingAt,
    task_tags,
    task_taskArn,
    task_taskDefinitionArn,
    task_version,

    -- ** TaskDefinition
    taskDefinition_compatibilities,
    taskDefinition_containerDefinitions,
    taskDefinition_cpu,
    taskDefinition_deregisteredAt,
    taskDefinition_ephemeralStorage,
    taskDefinition_executionRoleArn,
    taskDefinition_family,
    taskDefinition_inferenceAccelerators,
    taskDefinition_ipcMode,
    taskDefinition_memory,
    taskDefinition_networkMode,
    taskDefinition_pidMode,
    taskDefinition_placementConstraints,
    taskDefinition_proxyConfiguration,
    taskDefinition_registeredAt,
    taskDefinition_registeredBy,
    taskDefinition_requiresAttributes,
    taskDefinition_requiresCompatibilities,
    taskDefinition_revision,
    taskDefinition_runtimePlatform,
    taskDefinition_status,
    taskDefinition_taskDefinitionArn,
    taskDefinition_taskRoleArn,
    taskDefinition_volumes,

    -- ** TaskDefinitionPlacementConstraint
    taskDefinitionPlacementConstraint_expression,
    taskDefinitionPlacementConstraint_type,

    -- ** TaskOverride
    taskOverride_containerOverrides,
    taskOverride_cpu,
    taskOverride_ephemeralStorage,
    taskOverride_executionRoleArn,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_memory,
    taskOverride_taskRoleArn,

    -- ** TaskSet
    taskSet_capacityProviderStrategy,
    taskSet_clusterArn,
    taskSet_computedDesiredCount,
    taskSet_createdAt,
    taskSet_externalId,
    taskSet_id,
    taskSet_launchType,
    taskSet_loadBalancers,
    taskSet_networkConfiguration,
    taskSet_pendingCount,
    taskSet_platformFamily,
    taskSet_platformVersion,
    taskSet_runningCount,
    taskSet_scale,
    taskSet_serviceArn,
    taskSet_serviceRegistries,
    taskSet_stabilityStatus,
    taskSet_stabilityStatusAt,
    taskSet_startedBy,
    taskSet_status,
    taskSet_tags,
    taskSet_taskDefinition,
    taskSet_taskSetArn,
    taskSet_updatedAt,

    -- ** Tmpfs
    tmpfs_mountOptions,
    tmpfs_containerPath,
    tmpfs_size,

    -- ** Ulimit
    ulimit_name,
    ulimit_softLimit,
    ulimit_hardLimit,

    -- ** VersionInfo
    versionInfo_agentHash,
    versionInfo_agentVersion,
    versionInfo_dockerVersion,

    -- ** Volume
    volume_dockerVolumeConfiguration,
    volume_efsVolumeConfiguration,
    volume_fsxWindowsFileServerVolumeConfiguration,
    volume_host,
    volume_name,

    -- ** VolumeFrom
    volumeFrom_readOnly,
    volumeFrom_sourceContainer,
  )
where

import Amazonka.ECS.CreateCapacityProvider
import Amazonka.ECS.CreateCluster
import Amazonka.ECS.CreateService
import Amazonka.ECS.CreateTaskSet
import Amazonka.ECS.DeleteAccountSetting
import Amazonka.ECS.DeleteAttributes
import Amazonka.ECS.DeleteCapacityProvider
import Amazonka.ECS.DeleteCluster
import Amazonka.ECS.DeleteService
import Amazonka.ECS.DeleteTaskSet
import Amazonka.ECS.DeregisterContainerInstance
import Amazonka.ECS.DeregisterTaskDefinition
import Amazonka.ECS.DescribeCapacityProviders
import Amazonka.ECS.DescribeClusters
import Amazonka.ECS.DescribeContainerInstances
import Amazonka.ECS.DescribeServices
import Amazonka.ECS.DescribeTaskDefinition
import Amazonka.ECS.DescribeTaskSets
import Amazonka.ECS.DescribeTasks
import Amazonka.ECS.DiscoverPollEndpoint
import Amazonka.ECS.ExecuteCommand
import Amazonka.ECS.GetTaskProtection
import Amazonka.ECS.ListAccountSettings
import Amazonka.ECS.ListAttributes
import Amazonka.ECS.ListClusters
import Amazonka.ECS.ListContainerInstances
import Amazonka.ECS.ListServices
import Amazonka.ECS.ListServicesByNamespace
import Amazonka.ECS.ListTagsForResource
import Amazonka.ECS.ListTaskDefinitionFamilies
import Amazonka.ECS.ListTaskDefinitions
import Amazonka.ECS.ListTasks
import Amazonka.ECS.PutAccountSetting
import Amazonka.ECS.PutAccountSettingDefault
import Amazonka.ECS.PutAttributes
import Amazonka.ECS.PutClusterCapacityProviders
import Amazonka.ECS.RegisterContainerInstance
import Amazonka.ECS.RegisterTaskDefinition
import Amazonka.ECS.RunTask
import Amazonka.ECS.StartTask
import Amazonka.ECS.StopTask
import Amazonka.ECS.SubmitAttachmentStateChanges
import Amazonka.ECS.SubmitContainerStateChange
import Amazonka.ECS.SubmitTaskStateChange
import Amazonka.ECS.TagResource
import Amazonka.ECS.Types.Attachment
import Amazonka.ECS.Types.AttachmentStateChange
import Amazonka.ECS.Types.Attribute
import Amazonka.ECS.Types.AutoScalingGroupProvider
import Amazonka.ECS.Types.AutoScalingGroupProviderUpdate
import Amazonka.ECS.Types.AwsVpcConfiguration
import Amazonka.ECS.Types.CapacityProvider
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.Cluster
import Amazonka.ECS.Types.ClusterConfiguration
import Amazonka.ECS.Types.ClusterServiceConnectDefaults
import Amazonka.ECS.Types.ClusterServiceConnectDefaultsRequest
import Amazonka.ECS.Types.ClusterSetting
import Amazonka.ECS.Types.Container
import Amazonka.ECS.Types.ContainerDefinition
import Amazonka.ECS.Types.ContainerDependency
import Amazonka.ECS.Types.ContainerInstance
import Amazonka.ECS.Types.ContainerInstanceHealthStatus
import Amazonka.ECS.Types.ContainerOverride
import Amazonka.ECS.Types.ContainerService
import Amazonka.ECS.Types.ContainerStateChange
import Amazonka.ECS.Types.Deployment
import Amazonka.ECS.Types.DeploymentAlarms
import Amazonka.ECS.Types.DeploymentCircuitBreaker
import Amazonka.ECS.Types.DeploymentConfiguration
import Amazonka.ECS.Types.DeploymentController
import Amazonka.ECS.Types.Device
import Amazonka.ECS.Types.DockerVolumeConfiguration
import Amazonka.ECS.Types.EFSAuthorizationConfig
import Amazonka.ECS.Types.EFSVolumeConfiguration
import Amazonka.ECS.Types.EnvironmentFile
import Amazonka.ECS.Types.EphemeralStorage
import Amazonka.ECS.Types.ExecuteCommandConfiguration
import Amazonka.ECS.Types.ExecuteCommandLogConfiguration
import Amazonka.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Amazonka.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Amazonka.ECS.Types.Failure
import Amazonka.ECS.Types.FirelensConfiguration
import Amazonka.ECS.Types.HealthCheck
import Amazonka.ECS.Types.HostEntry
import Amazonka.ECS.Types.HostVolumeProperties
import Amazonka.ECS.Types.InferenceAccelerator
import Amazonka.ECS.Types.InferenceAcceleratorOverride
import Amazonka.ECS.Types.InstanceHealthCheckResult
import Amazonka.ECS.Types.KernelCapabilities
import Amazonka.ECS.Types.KeyValuePair
import Amazonka.ECS.Types.LinuxParameters
import Amazonka.ECS.Types.LoadBalancer
import Amazonka.ECS.Types.LogConfiguration
import Amazonka.ECS.Types.ManagedAgent
import Amazonka.ECS.Types.ManagedAgentStateChange
import Amazonka.ECS.Types.ManagedScaling
import Amazonka.ECS.Types.MountPoint
import Amazonka.ECS.Types.NetworkBinding
import Amazonka.ECS.Types.NetworkConfiguration
import Amazonka.ECS.Types.NetworkInterface
import Amazonka.ECS.Types.PlacementConstraint
import Amazonka.ECS.Types.PlacementStrategy
import Amazonka.ECS.Types.PlatformDevice
import Amazonka.ECS.Types.PortMapping
import Amazonka.ECS.Types.ProtectedTask
import Amazonka.ECS.Types.ProxyConfiguration
import Amazonka.ECS.Types.RepositoryCredentials
import Amazonka.ECS.Types.Resource
import Amazonka.ECS.Types.ResourceRequirement
import Amazonka.ECS.Types.RuntimePlatform
import Amazonka.ECS.Types.Scale
import Amazonka.ECS.Types.Secret
import Amazonka.ECS.Types.ServiceConnectClientAlias
import Amazonka.ECS.Types.ServiceConnectConfiguration
import Amazonka.ECS.Types.ServiceConnectService
import Amazonka.ECS.Types.ServiceConnectServiceResource
import Amazonka.ECS.Types.ServiceEvent
import Amazonka.ECS.Types.ServiceRegistry
import Amazonka.ECS.Types.Session
import Amazonka.ECS.Types.Setting
import Amazonka.ECS.Types.SystemControl
import Amazonka.ECS.Types.Tag
import Amazonka.ECS.Types.Task
import Amazonka.ECS.Types.TaskDefinition
import Amazonka.ECS.Types.TaskDefinitionPlacementConstraint
import Amazonka.ECS.Types.TaskOverride
import Amazonka.ECS.Types.TaskSet
import Amazonka.ECS.Types.Tmpfs
import Amazonka.ECS.Types.Ulimit
import Amazonka.ECS.Types.VersionInfo
import Amazonka.ECS.Types.Volume
import Amazonka.ECS.Types.VolumeFrom
import Amazonka.ECS.UntagResource
import Amazonka.ECS.UpdateCapacityProvider
import Amazonka.ECS.UpdateCluster
import Amazonka.ECS.UpdateClusterSettings
import Amazonka.ECS.UpdateContainerAgent
import Amazonka.ECS.UpdateContainerInstancesState
import Amazonka.ECS.UpdateService
import Amazonka.ECS.UpdateServicePrimaryTaskSet
import Amazonka.ECS.UpdateTaskProtection
import Amazonka.ECS.UpdateTaskSet
