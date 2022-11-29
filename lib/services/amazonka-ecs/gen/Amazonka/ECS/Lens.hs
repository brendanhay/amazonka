{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createCluster_tags,
    createCluster_configuration,
    createCluster_settings,
    createCluster_capacityProviders,
    createCluster_defaultCapacityProviderStrategy,
    createCluster_clusterName,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateService
    createService_healthCheckGracePeriodSeconds,
    createService_tags,
    createService_clientToken,
    createService_deploymentConfiguration,
    createService_serviceRegistries,
    createService_schedulingStrategy,
    createService_cluster,
    createService_placementStrategy,
    createService_taskDefinition,
    createService_networkConfiguration,
    createService_desiredCount,
    createService_enableExecuteCommand,
    createService_capacityProviderStrategy,
    createService_placementConstraints,
    createService_propagateTags,
    createService_deploymentController,
    createService_loadBalancers,
    createService_launchType,
    createService_role,
    createService_platformVersion,
    createService_enableECSManagedTags,
    createService_serviceName,
    createServiceResponse_service,
    createServiceResponse_httpStatus,

    -- ** CreateTaskSet
    createTaskSet_tags,
    createTaskSet_clientToken,
    createTaskSet_serviceRegistries,
    createTaskSet_externalId,
    createTaskSet_networkConfiguration,
    createTaskSet_capacityProviderStrategy,
    createTaskSet_loadBalancers,
    createTaskSet_launchType,
    createTaskSet_platformVersion,
    createTaskSet_scale,
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
    describeCapacityProviders_nextToken,
    describeCapacityProviders_maxResults,
    describeCapacityProviders_capacityProviders,
    describeCapacityProviders_include,
    describeCapacityProvidersResponse_nextToken,
    describeCapacityProvidersResponse_failures,
    describeCapacityProvidersResponse_capacityProviders,
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
    describeServicesResponse_services,
    describeServicesResponse_failures,
    describeServicesResponse_httpStatus,

    -- ** DescribeTaskDefinition
    describeTaskDefinition_include,
    describeTaskDefinition_taskDefinition,
    describeTaskDefinitionResponse_tags,
    describeTaskDefinitionResponse_taskDefinition,
    describeTaskDefinitionResponse_httpStatus,

    -- ** DescribeTaskSets
    describeTaskSets_taskSets,
    describeTaskSets_include,
    describeTaskSets_cluster,
    describeTaskSets_service,
    describeTaskSetsResponse_failures,
    describeTaskSetsResponse_taskSets,
    describeTaskSetsResponse_httpStatus,

    -- ** DescribeTasks
    describeTasks_cluster,
    describeTasks_include,
    describeTasks_tasks,
    describeTasksResponse_tasks,
    describeTasksResponse_failures,
    describeTasksResponse_httpStatus,

    -- ** DiscoverPollEndpoint
    discoverPollEndpoint_containerInstance,
    discoverPollEndpoint_cluster,
    discoverPollEndpointResponse_telemetryEndpoint,
    discoverPollEndpointResponse_endpoint,
    discoverPollEndpointResponse_httpStatus,

    -- ** ExecuteCommand
    executeCommand_cluster,
    executeCommand_container,
    executeCommand_command,
    executeCommand_interactive,
    executeCommand_task,
    executeCommandResponse_clusterArn,
    executeCommandResponse_containerName,
    executeCommandResponse_taskArn,
    executeCommandResponse_session,
    executeCommandResponse_containerArn,
    executeCommandResponse_interactive,
    executeCommandResponse_httpStatus,

    -- ** GetTaskProtection
    getTaskProtection_tasks,
    getTaskProtection_cluster,
    getTaskProtectionResponse_failures,
    getTaskProtectionResponse_protectedTasks,
    getTaskProtectionResponse_httpStatus,

    -- ** ListAccountSettings
    listAccountSettings_name,
    listAccountSettings_nextToken,
    listAccountSettings_effectiveSettings,
    listAccountSettings_principalArn,
    listAccountSettings_maxResults,
    listAccountSettings_value,
    listAccountSettingsResponse_nextToken,
    listAccountSettingsResponse_settings,
    listAccountSettingsResponse_httpStatus,

    -- ** ListAttributes
    listAttributes_nextToken,
    listAttributes_attributeValue,
    listAttributes_cluster,
    listAttributes_maxResults,
    listAttributes_attributeName,
    listAttributes_targetType,
    listAttributesResponse_nextToken,
    listAttributesResponse_attributes,
    listAttributesResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_clusterArns,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListContainerInstances
    listContainerInstances_nextToken,
    listContainerInstances_cluster,
    listContainerInstances_status,
    listContainerInstances_filter,
    listContainerInstances_maxResults,
    listContainerInstancesResponse_nextToken,
    listContainerInstancesResponse_containerInstanceArns,
    listContainerInstancesResponse_httpStatus,

    -- ** ListServices
    listServices_nextToken,
    listServices_schedulingStrategy,
    listServices_cluster,
    listServices_maxResults,
    listServices_launchType,
    listServicesResponse_nextToken,
    listServicesResponse_serviceArns,
    listServicesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTaskDefinitionFamilies
    listTaskDefinitionFamilies_nextToken,
    listTaskDefinitionFamilies_status,
    listTaskDefinitionFamilies_maxResults,
    listTaskDefinitionFamilies_familyPrefix,
    listTaskDefinitionFamiliesResponse_nextToken,
    listTaskDefinitionFamiliesResponse_families,
    listTaskDefinitionFamiliesResponse_httpStatus,

    -- ** ListTaskDefinitions
    listTaskDefinitions_nextToken,
    listTaskDefinitions_status,
    listTaskDefinitions_sort,
    listTaskDefinitions_maxResults,
    listTaskDefinitions_familyPrefix,
    listTaskDefinitionsResponse_nextToken,
    listTaskDefinitionsResponse_taskDefinitionArns,
    listTaskDefinitionsResponse_httpStatus,

    -- ** ListTasks
    listTasks_nextToken,
    listTasks_containerInstance,
    listTasks_cluster,
    listTasks_desiredStatus,
    listTasks_startedBy,
    listTasks_maxResults,
    listTasks_launchType,
    listTasks_family,
    listTasks_serviceName,
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
    registerContainerInstance_tags,
    registerContainerInstance_cluster,
    registerContainerInstance_versionInfo,
    registerContainerInstance_instanceIdentityDocumentSignature,
    registerContainerInstance_platformDevices,
    registerContainerInstance_containerInstanceArn,
    registerContainerInstance_instanceIdentityDocument,
    registerContainerInstance_attributes,
    registerContainerInstance_totalResources,
    registerContainerInstanceResponse_containerInstance,
    registerContainerInstanceResponse_httpStatus,

    -- ** RegisterTaskDefinition
    registerTaskDefinition_tags,
    registerTaskDefinition_ephemeralStorage,
    registerTaskDefinition_runtimePlatform,
    registerTaskDefinition_proxyConfiguration,
    registerTaskDefinition_pidMode,
    registerTaskDefinition_memory,
    registerTaskDefinition_cpu,
    registerTaskDefinition_taskRoleArn,
    registerTaskDefinition_inferenceAccelerators,
    registerTaskDefinition_volumes,
    registerTaskDefinition_requiresCompatibilities,
    registerTaskDefinition_placementConstraints,
    registerTaskDefinition_executionRoleArn,
    registerTaskDefinition_ipcMode,
    registerTaskDefinition_networkMode,
    registerTaskDefinition_family,
    registerTaskDefinition_containerDefinitions,
    registerTaskDefinitionResponse_tags,
    registerTaskDefinitionResponse_taskDefinition,
    registerTaskDefinitionResponse_httpStatus,

    -- ** RunTask
    runTask_tags,
    runTask_cluster,
    runTask_placementStrategy,
    runTask_networkConfiguration,
    runTask_count,
    runTask_startedBy,
    runTask_enableExecuteCommand,
    runTask_capacityProviderStrategy,
    runTask_placementConstraints,
    runTask_propagateTags,
    runTask_referenceId,
    runTask_launchType,
    runTask_platformVersion,
    runTask_enableECSManagedTags,
    runTask_group,
    runTask_overrides,
    runTask_taskDefinition,
    runTaskResponse_tasks,
    runTaskResponse_failures,
    runTaskResponse_httpStatus,

    -- ** StartTask
    startTask_tags,
    startTask_cluster,
    startTask_networkConfiguration,
    startTask_startedBy,
    startTask_enableExecuteCommand,
    startTask_propagateTags,
    startTask_referenceId,
    startTask_enableECSManagedTags,
    startTask_group,
    startTask_overrides,
    startTask_containerInstances,
    startTask_taskDefinition,
    startTaskResponse_tasks,
    startTaskResponse_failures,
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
    submitContainerStateChange_containerName,
    submitContainerStateChange_task,
    submitContainerStateChange_cluster,
    submitContainerStateChange_status,
    submitContainerStateChange_runtimeId,
    submitContainerStateChange_networkBindings,
    submitContainerStateChange_reason,
    submitContainerStateChange_exitCode,
    submitContainerStateChangeResponse_acknowledgment,
    submitContainerStateChangeResponse_httpStatus,

    -- ** SubmitTaskStateChange
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_task,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_cluster,
    submitTaskStateChange_containers,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChange_status,
    submitTaskStateChange_attachments,
    submitTaskStateChange_managedAgents,
    submitTaskStateChange_reason,
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
    updateService_healthCheckGracePeriodSeconds,
    updateService_deploymentConfiguration,
    updateService_serviceRegistries,
    updateService_cluster,
    updateService_placementStrategy,
    updateService_taskDefinition,
    updateService_networkConfiguration,
    updateService_desiredCount,
    updateService_enableExecuteCommand,
    updateService_capacityProviderStrategy,
    updateService_placementConstraints,
    updateService_propagateTags,
    updateService_loadBalancers,
    updateService_forceNewDeployment,
    updateService_platformVersion,
    updateService_enableECSManagedTags,
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
    attachment_type,
    attachment_status,
    attachment_details,
    attachment_id,

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
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_subnets,

    -- ** CapacityProvider
    capacityProvider_tags,
    capacityProvider_name,
    capacityProvider_capacityProviderArn,
    capacityProvider_updateStatusReason,
    capacityProvider_updateStatus,
    capacityProvider_status,
    capacityProvider_autoScalingGroupProvider,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** Cluster
    cluster_tags,
    cluster_clusterArn,
    cluster_attachmentsStatus,
    cluster_statistics,
    cluster_pendingTasksCount,
    cluster_configuration,
    cluster_registeredContainerInstancesCount,
    cluster_status,
    cluster_settings,
    cluster_runningTasksCount,
    cluster_attachments,
    cluster_capacityProviders,
    cluster_activeServicesCount,
    cluster_defaultCapacityProviderStrategy,
    cluster_clusterName,

    -- ** ClusterConfiguration
    clusterConfiguration_executeCommandConfiguration,

    -- ** ClusterSetting
    clusterSetting_name,
    clusterSetting_value,

    -- ** Container
    container_name,
    container_memory,
    container_cpu,
    container_taskArn,
    container_memoryReservation,
    container_healthStatus,
    container_lastStatus,
    container_gpuIds,
    container_runtimeId,
    container_containerArn,
    container_managedAgents,
    container_networkBindings,
    container_reason,
    container_exitCode,
    container_imageDigest,
    container_image,
    container_networkInterfaces,

    -- ** ContainerDefinition
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_name,
    containerDefinition_dependsOn,
    containerDefinition_extraHosts,
    containerDefinition_healthCheck,
    containerDefinition_environment,
    containerDefinition_logConfiguration,
    containerDefinition_resourceRequirements,
    containerDefinition_startTimeout,
    containerDefinition_memory,
    containerDefinition_cpu,
    containerDefinition_user,
    containerDefinition_dnsServers,
    containerDefinition_ulimits,
    containerDefinition_memoryReservation,
    containerDefinition_repositoryCredentials,
    containerDefinition_command,
    containerDefinition_portMappings,
    containerDefinition_hostname,
    containerDefinition_environmentFiles,
    containerDefinition_secrets,
    containerDefinition_disableNetworking,
    containerDefinition_dockerLabels,
    containerDefinition_volumesFrom,
    containerDefinition_dnsSearchDomains,
    containerDefinition_privileged,
    containerDefinition_stopTimeout,
    containerDefinition_entryPoint,
    containerDefinition_links,
    containerDefinition_mountPoints,
    containerDefinition_image,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_interactive,
    containerDefinition_linuxParameters,
    containerDefinition_essential,
    containerDefinition_pseudoTerminal,
    containerDefinition_firelensConfiguration,
    containerDefinition_systemControls,
    containerDefinition_workingDirectory,

    -- ** ContainerDependency
    containerDependency_containerName,
    containerDependency_condition,

    -- ** ContainerInstance
    containerInstance_tags,
    containerInstance_capacityProviderName,
    containerInstance_ec2InstanceId,
    containerInstance_remainingResources,
    containerInstance_registeredResources,
    containerInstance_pendingTasksCount,
    containerInstance_versionInfo,
    containerInstance_agentUpdateStatus,
    containerInstance_healthStatus,
    containerInstance_statusReason,
    containerInstance_status,
    containerInstance_agentConnected,
    containerInstance_runningTasksCount,
    containerInstance_attachments,
    containerInstance_containerInstanceArn,
    containerInstance_attributes,
    containerInstance_registeredAt,
    containerInstance_version,

    -- ** ContainerInstanceHealthStatus
    containerInstanceHealthStatus_details,
    containerInstanceHealthStatus_overallStatus,

    -- ** ContainerOverride
    containerOverride_name,
    containerOverride_environment,
    containerOverride_resourceRequirements,
    containerOverride_memory,
    containerOverride_cpu,
    containerOverride_memoryReservation,
    containerOverride_command,
    containerOverride_environmentFiles,

    -- ** ContainerService
    containerService_healthCheckGracePeriodSeconds,
    containerService_tags,
    containerService_clusterArn,
    containerService_deploymentConfiguration,
    containerService_roleArn,
    containerService_serviceRegistries,
    containerService_schedulingStrategy,
    containerService_platformFamily,
    containerService_placementStrategy,
    containerService_taskDefinition,
    containerService_networkConfiguration,
    containerService_status,
    containerService_desiredCount,
    containerService_pendingCount,
    containerService_enableExecuteCommand,
    containerService_capacityProviderStrategy,
    containerService_placementConstraints,
    containerService_taskSets,
    containerService_propagateTags,
    containerService_deploymentController,
    containerService_events,
    containerService_loadBalancers,
    containerService_launchType,
    containerService_runningCount,
    containerService_platformVersion,
    containerService_serviceName,
    containerService_deployments,
    containerService_enableECSManagedTags,
    containerService_createdBy,
    containerService_serviceArn,
    containerService_createdAt,

    -- ** ContainerStateChange
    containerStateChange_containerName,
    containerStateChange_status,
    containerStateChange_runtimeId,
    containerStateChange_networkBindings,
    containerStateChange_reason,
    containerStateChange_exitCode,
    containerStateChange_imageDigest,

    -- ** Deployment
    deployment_platformFamily,
    deployment_taskDefinition,
    deployment_failedTasks,
    deployment_networkConfiguration,
    deployment_status,
    deployment_id,
    deployment_desiredCount,
    deployment_pendingCount,
    deployment_capacityProviderStrategy,
    deployment_rolloutStateReason,
    deployment_launchType,
    deployment_runningCount,
    deployment_platformVersion,
    deployment_rolloutState,
    deployment_createdAt,
    deployment_updatedAt,

    -- ** DeploymentCircuitBreaker
    deploymentCircuitBreaker_enable,
    deploymentCircuitBreaker_rollback,

    -- ** DeploymentConfiguration
    deploymentConfiguration_minimumHealthyPercent,
    deploymentConfiguration_maximumPercent,
    deploymentConfiguration_deploymentCircuitBreaker,

    -- ** DeploymentController
    deploymentController_type,

    -- ** Device
    device_permissions,
    device_containerPath,
    device_hostPath,

    -- ** DockerVolumeConfiguration
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_autoprovision,
    dockerVolumeConfiguration_labels,
    dockerVolumeConfiguration_scope,
    dockerVolumeConfiguration_driver,

    -- ** EFSAuthorizationConfig
    eFSAuthorizationConfig_iam,
    eFSAuthorizationConfig_accessPointId,

    -- ** EFSVolumeConfiguration
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_fileSystemId,

    -- ** EnvironmentFile
    environmentFile_value,
    environmentFile_type,

    -- ** EphemeralStorage
    ephemeralStorage_sizeInGiB,

    -- ** ExecuteCommandConfiguration
    executeCommandConfiguration_logConfiguration,
    executeCommandConfiguration_logging,
    executeCommandConfiguration_kmsKeyId,

    -- ** ExecuteCommandLogConfiguration
    executeCommandLogConfiguration_s3KeyPrefix,
    executeCommandLogConfiguration_s3BucketName,
    executeCommandLogConfiguration_s3EncryptionEnabled,
    executeCommandLogConfiguration_cloudWatchEncryptionEnabled,
    executeCommandLogConfiguration_cloudWatchLogGroupName,

    -- ** FSxWindowsFileServerAuthorizationConfig
    fSxWindowsFileServerAuthorizationConfig_credentialsParameter,
    fSxWindowsFileServerAuthorizationConfig_domain,

    -- ** FSxWindowsFileServerVolumeConfiguration
    fSxWindowsFileServerVolumeConfiguration_fileSystemId,
    fSxWindowsFileServerVolumeConfiguration_rootDirectory,
    fSxWindowsFileServerVolumeConfiguration_authorizationConfig,

    -- ** Failure
    failure_arn,
    failure_reason,
    failure_detail,

    -- ** FirelensConfiguration
    firelensConfiguration_options,
    firelensConfiguration_type,

    -- ** HealthCheck
    healthCheck_timeout,
    healthCheck_startPeriod,
    healthCheck_interval,
    healthCheck_retries,
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
    instanceHealthCheckResult_type,
    instanceHealthCheckResult_status,
    instanceHealthCheckResult_lastStatusChange,
    instanceHealthCheckResult_lastUpdated,

    -- ** KernelCapabilities
    kernelCapabilities_drop,
    kernelCapabilities_add,

    -- ** KeyValuePair
    keyValuePair_name,
    keyValuePair_value,

    -- ** LinuxParameters
    linuxParameters_devices,
    linuxParameters_swappiness,
    linuxParameters_tmpfs,
    linuxParameters_initProcessEnabled,
    linuxParameters_maxSwap,
    linuxParameters_capabilities,
    linuxParameters_sharedMemorySize,

    -- ** LoadBalancer
    loadBalancer_containerPort,
    loadBalancer_containerName,
    loadBalancer_loadBalancerName,
    loadBalancer_targetGroupArn,

    -- ** LogConfiguration
    logConfiguration_secretOptions,
    logConfiguration_options,
    logConfiguration_logDriver,

    -- ** ManagedAgent
    managedAgent_lastStartedAt,
    managedAgent_name,
    managedAgent_lastStatus,
    managedAgent_reason,

    -- ** ManagedAgentStateChange
    managedAgentStateChange_reason,
    managedAgentStateChange_containerName,
    managedAgentStateChange_managedAgentName,
    managedAgentStateChange_status,

    -- ** ManagedScaling
    managedScaling_targetCapacity,
    managedScaling_instanceWarmupPeriod,
    managedScaling_status,
    managedScaling_minimumScalingStepSize,
    managedScaling_maximumScalingStepSize,

    -- ** MountPoint
    mountPoint_sourceVolume,
    mountPoint_containerPath,
    mountPoint_readOnly,

    -- ** NetworkBinding
    networkBinding_containerPort,
    networkBinding_bindIP,
    networkBinding_hostPort,
    networkBinding_protocol,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** NetworkInterface
    networkInterface_attachmentId,
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,

    -- ** PlacementConstraint
    placementConstraint_type,
    placementConstraint_expression,

    -- ** PlacementStrategy
    placementStrategy_type,
    placementStrategy_field,

    -- ** PlatformDevice
    platformDevice_id,
    platformDevice_type,

    -- ** PortMapping
    portMapping_containerPort,
    portMapping_hostPort,
    portMapping_protocol,

    -- ** ProtectedTask
    protectedTask_taskArn,
    protectedTask_protectionEnabled,
    protectedTask_expirationDate,

    -- ** ProxyConfiguration
    proxyConfiguration_type,
    proxyConfiguration_properties,
    proxyConfiguration_containerName,

    -- ** RepositoryCredentials
    repositoryCredentials_credentialsParameter,

    -- ** Resource
    resource_name,
    resource_type,
    resource_integerValue,
    resource_stringSetValue,
    resource_doubleValue,
    resource_longValue,

    -- ** ResourceRequirement
    resourceRequirement_value,
    resourceRequirement_type,

    -- ** RuntimePlatform
    runtimePlatform_operatingSystemFamily,
    runtimePlatform_cpuArchitecture,

    -- ** Scale
    scale_unit,
    scale_value,

    -- ** Secret
    secret_name,
    secret_valueFrom,

    -- ** ServiceEvent
    serviceEvent_message,
    serviceEvent_id,
    serviceEvent_createdAt,

    -- ** ServiceRegistry
    serviceRegistry_port,
    serviceRegistry_containerPort,
    serviceRegistry_containerName,
    serviceRegistry_registryArn,

    -- ** Session
    session_tokenValue,
    session_streamUrl,
    session_sessionId,

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
    task_tags,
    task_clusterArn,
    task_capacityProviderName,
    task_ephemeralStorage,
    task_executionStoppedAt,
    task_pullStoppedAt,
    task_platformFamily,
    task_memory,
    task_cpu,
    task_taskArn,
    task_connectivityAt,
    task_containers,
    task_healthStatus,
    task_inferenceAccelerators,
    task_lastStatus,
    task_taskDefinitionArn,
    task_pullStartedAt,
    task_desiredStatus,
    task_availabilityZone,
    task_startedBy,
    task_enableExecuteCommand,
    task_stopCode,
    task_startedAt,
    task_attachments,
    task_stoppingAt,
    task_containerInstanceArn,
    task_launchType,
    task_platformVersion,
    task_attributes,
    task_stoppedReason,
    task_stoppedAt,
    task_group,
    task_connectivity,
    task_createdAt,
    task_version,
    task_overrides,

    -- ** TaskDefinition
    taskDefinition_ephemeralStorage,
    taskDefinition_runtimePlatform,
    taskDefinition_proxyConfiguration,
    taskDefinition_requiresAttributes,
    taskDefinition_pidMode,
    taskDefinition_registeredBy,
    taskDefinition_memory,
    taskDefinition_cpu,
    taskDefinition_taskRoleArn,
    taskDefinition_revision,
    taskDefinition_inferenceAccelerators,
    taskDefinition_taskDefinitionArn,
    taskDefinition_status,
    taskDefinition_volumes,
    taskDefinition_requiresCompatibilities,
    taskDefinition_placementConstraints,
    taskDefinition_family,
    taskDefinition_compatibilities,
    taskDefinition_containerDefinitions,
    taskDefinition_executionRoleArn,
    taskDefinition_ipcMode,
    taskDefinition_networkMode,
    taskDefinition_registeredAt,
    taskDefinition_deregisteredAt,

    -- ** TaskDefinitionPlacementConstraint
    taskDefinitionPlacementConstraint_type,
    taskDefinitionPlacementConstraint_expression,

    -- ** TaskOverride
    taskOverride_ephemeralStorage,
    taskOverride_memory,
    taskOverride_cpu,
    taskOverride_taskRoleArn,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_executionRoleArn,
    taskOverride_containerOverrides,

    -- ** TaskSet
    taskSet_tags,
    taskSet_clusterArn,
    taskSet_serviceRegistries,
    taskSet_platformFamily,
    taskSet_taskDefinition,
    taskSet_stabilityStatus,
    taskSet_externalId,
    taskSet_networkConfiguration,
    taskSet_status,
    taskSet_id,
    taskSet_taskSetArn,
    taskSet_pendingCount,
    taskSet_startedBy,
    taskSet_capacityProviderStrategy,
    taskSet_loadBalancers,
    taskSet_launchType,
    taskSet_runningCount,
    taskSet_platformVersion,
    taskSet_computedDesiredCount,
    taskSet_stabilityStatusAt,
    taskSet_scale,
    taskSet_serviceArn,
    taskSet_createdAt,
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
    versionInfo_dockerVersion,
    versionInfo_agentHash,
    versionInfo_agentVersion,

    -- ** Volume
    volume_efsVolumeConfiguration,
    volume_name,
    volume_host,
    volume_dockerVolumeConfiguration,
    volume_fsxWindowsFileServerVolumeConfiguration,

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
