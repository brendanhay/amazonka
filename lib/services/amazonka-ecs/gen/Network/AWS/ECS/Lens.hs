{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Lens
  ( -- * Operations

    -- ** ListServices
    listServices_cluster,
    listServices_nextToken,
    listServices_launchType,
    listServices_schedulingStrategy,
    listServices_maxResults,
    listServicesResponse_serviceArns,
    listServicesResponse_nextToken,
    listServicesResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_include,
    describeClusters_clusters,
    describeClustersResponse_failures,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DeleteService
    deleteService_cluster,
    deleteService_force,
    deleteService_service,
    deleteServiceResponse_service,
    deleteServiceResponse_httpStatus,

    -- ** UpdateService
    updateService_cluster,
    updateService_platformVersion,
    updateService_desiredCount,
    updateService_placementConstraints,
    updateService_placementStrategy,
    updateService_forceNewDeployment,
    updateService_taskDefinition,
    updateService_healthCheckGracePeriodSeconds,
    updateService_networkConfiguration,
    updateService_capacityProviderStrategy,
    updateService_enableExecuteCommand,
    updateService_deploymentConfiguration,
    updateService_service,
    updateServiceResponse_service,
    updateServiceResponse_httpStatus,

    -- ** DiscoverPollEndpoint
    discoverPollEndpoint_cluster,
    discoverPollEndpoint_containerInstance,
    discoverPollEndpointResponse_telemetryEndpoint,
    discoverPollEndpointResponse_endpoint,
    discoverPollEndpointResponse_httpStatus,

    -- ** SubmitAttachmentStateChanges
    submitAttachmentStateChanges_cluster,
    submitAttachmentStateChanges_attachments,
    submitAttachmentStateChangesResponse_acknowledgment,
    submitAttachmentStateChangesResponse_httpStatus,

    -- ** SubmitContainerStateChange
    submitContainerStateChange_networkBindings,
    submitContainerStateChange_status,
    submitContainerStateChange_cluster,
    submitContainerStateChange_containerName,
    submitContainerStateChange_reason,
    submitContainerStateChange_exitCode,
    submitContainerStateChange_task,
    submitContainerStateChange_runtimeId,
    submitContainerStateChangeResponse_acknowledgment,
    submitContainerStateChangeResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StopTask
    stopTask_cluster,
    stopTask_reason,
    stopTask_task,
    stopTaskResponse_task,
    stopTaskResponse_httpStatus,

    -- ** DescribeTaskDefinition
    describeTaskDefinition_include,
    describeTaskDefinition_taskDefinition,
    describeTaskDefinitionResponse_taskDefinition,
    describeTaskDefinitionResponse_tags,
    describeTaskDefinitionResponse_httpStatus,

    -- ** SubmitTaskStateChange
    submitTaskStateChange_status,
    submitTaskStateChange_managedAgents,
    submitTaskStateChange_cluster,
    submitTaskStateChange_attachments,
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_containers,
    submitTaskStateChange_reason,
    submitTaskStateChange_task,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChangeResponse_acknowledgment,
    submitTaskStateChangeResponse_httpStatus,

    -- ** DescribeContainerInstances
    describeContainerInstances_include,
    describeContainerInstances_cluster,
    describeContainerInstances_containerInstances,
    describeContainerInstancesResponse_failures,
    describeContainerInstancesResponse_containerInstances,
    describeContainerInstancesResponse_httpStatus,

    -- ** DescribeCapacityProviders
    describeCapacityProviders_include,
    describeCapacityProviders_nextToken,
    describeCapacityProviders_capacityProviders,
    describeCapacityProviders_maxResults,
    describeCapacityProvidersResponse_failures,
    describeCapacityProvidersResponse_nextToken,
    describeCapacityProvidersResponse_capacityProviders,
    describeCapacityProvidersResponse_httpStatus,

    -- ** UpdateContainerInstancesState
    updateContainerInstancesState_cluster,
    updateContainerInstancesState_containerInstances,
    updateContainerInstancesState_status,
    updateContainerInstancesStateResponse_failures,
    updateContainerInstancesStateResponse_containerInstances,
    updateContainerInstancesStateResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_cluster,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_settings,
    updateCluster_configuration,
    updateCluster_cluster,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** CreateCluster
    createCluster_defaultCapacityProviderStrategy,
    createCluster_settings,
    createCluster_clusterName,
    createCluster_configuration,
    createCluster_capacityProviders,
    createCluster_tags,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** PutAccountSetting
    putAccountSetting_principalArn,
    putAccountSetting_name,
    putAccountSetting_value,
    putAccountSettingResponse_setting,
    putAccountSettingResponse_httpStatus,

    -- ** DeleteAccountSetting
    deleteAccountSetting_principalArn,
    deleteAccountSetting_name,
    deleteAccountSettingResponse_setting,
    deleteAccountSettingResponse_httpStatus,

    -- ** ListTaskDefinitions
    listTaskDefinitions_status,
    listTaskDefinitions_familyPrefix,
    listTaskDefinitions_nextToken,
    listTaskDefinitions_sort,
    listTaskDefinitions_maxResults,
    listTaskDefinitionsResponse_taskDefinitionArns,
    listTaskDefinitionsResponse_nextToken,
    listTaskDefinitionsResponse_httpStatus,

    -- ** RunTask
    runTask_overrides,
    runTask_group,
    runTask_cluster,
    runTask_propagateTags,
    runTask_platformVersion,
    runTask_enableECSManagedTags,
    runTask_count,
    runTask_referenceId,
    runTask_placementConstraints,
    runTask_placementStrategy,
    runTask_startedBy,
    runTask_launchType,
    runTask_networkConfiguration,
    runTask_capacityProviderStrategy,
    runTask_enableExecuteCommand,
    runTask_tags,
    runTask_taskDefinition,
    runTaskResponse_failures,
    runTaskResponse_tasks,
    runTaskResponse_httpStatus,

    -- ** DeleteCapacityProvider
    deleteCapacityProvider_capacityProvider,
    deleteCapacityProviderResponse_capacityProvider,
    deleteCapacityProviderResponse_httpStatus,

    -- ** ListTasks
    listTasks_desiredStatus,
    listTasks_cluster,
    listTasks_family,
    listTasks_nextToken,
    listTasks_startedBy,
    listTasks_serviceName,
    listTasks_launchType,
    listTasks_containerInstance,
    listTasks_maxResults,
    listTasksResponse_nextToken,
    listTasksResponse_taskArns,
    listTasksResponse_httpStatus,

    -- ** UpdateCapacityProvider
    updateCapacityProvider_name,
    updateCapacityProvider_autoScalingGroupProvider,
    updateCapacityProviderResponse_capacityProvider,
    updateCapacityProviderResponse_httpStatus,

    -- ** RegisterContainerInstance
    registerContainerInstance_platformDevices,
    registerContainerInstance_instanceIdentityDocumentSignature,
    registerContainerInstance_cluster,
    registerContainerInstance_instanceIdentityDocument,
    registerContainerInstance_containerInstanceArn,
    registerContainerInstance_versionInfo,
    registerContainerInstance_attributes,
    registerContainerInstance_totalResources,
    registerContainerInstance_tags,
    registerContainerInstanceResponse_containerInstance,
    registerContainerInstanceResponse_httpStatus,

    -- ** UpdateContainerAgent
    updateContainerAgent_cluster,
    updateContainerAgent_containerInstance,
    updateContainerAgentResponse_containerInstance,
    updateContainerAgentResponse_httpStatus,

    -- ** ListContainerInstances
    listContainerInstances_status,
    listContainerInstances_cluster,
    listContainerInstances_nextToken,
    listContainerInstances_filter,
    listContainerInstances_maxResults,
    listContainerInstancesResponse_containerInstanceArns,
    listContainerInstancesResponse_nextToken,
    listContainerInstancesResponse_httpStatus,

    -- ** UpdateServicePrimaryTaskSet
    updateServicePrimaryTaskSet_cluster,
    updateServicePrimaryTaskSet_service,
    updateServicePrimaryTaskSet_primaryTaskSet,
    updateServicePrimaryTaskSetResponse_taskSet,
    updateServicePrimaryTaskSetResponse_httpStatus,

    -- ** ListTaskDefinitionFamilies
    listTaskDefinitionFamilies_status,
    listTaskDefinitionFamilies_familyPrefix,
    listTaskDefinitionFamilies_nextToken,
    listTaskDefinitionFamilies_maxResults,
    listTaskDefinitionFamiliesResponse_families,
    listTaskDefinitionFamiliesResponse_nextToken,
    listTaskDefinitionFamiliesResponse_httpStatus,

    -- ** StartTask
    startTask_overrides,
    startTask_group,
    startTask_cluster,
    startTask_propagateTags,
    startTask_enableECSManagedTags,
    startTask_referenceId,
    startTask_startedBy,
    startTask_networkConfiguration,
    startTask_enableExecuteCommand,
    startTask_tags,
    startTask_containerInstances,
    startTask_taskDefinition,
    startTaskResponse_failures,
    startTaskResponse_tasks,
    startTaskResponse_httpStatus,

    -- ** PutClusterCapacityProviders
    putClusterCapacityProviders_cluster,
    putClusterCapacityProviders_capacityProviders,
    putClusterCapacityProviders_defaultCapacityProviderStrategy,
    putClusterCapacityProvidersResponse_cluster,
    putClusterCapacityProvidersResponse_httpStatus,

    -- ** PutAccountSettingDefault
    putAccountSettingDefault_name,
    putAccountSettingDefault_value,
    putAccountSettingDefaultResponse_setting,
    putAccountSettingDefaultResponse_httpStatus,

    -- ** ListAttributes
    listAttributes_attributeValue,
    listAttributes_cluster,
    listAttributes_nextToken,
    listAttributes_attributeName,
    listAttributes_maxResults,
    listAttributes_targetType,
    listAttributesResponse_nextToken,
    listAttributesResponse_attributes,
    listAttributesResponse_httpStatus,

    -- ** ExecuteCommand
    executeCommand_cluster,
    executeCommand_container,
    executeCommand_command,
    executeCommand_interactive,
    executeCommand_task,
    executeCommandResponse_clusterArn,
    executeCommandResponse_containerArn,
    executeCommandResponse_taskArn,
    executeCommandResponse_containerName,
    executeCommandResponse_interactive,
    executeCommandResponse_session,
    executeCommandResponse_httpStatus,

    -- ** DeregisterTaskDefinition
    deregisterTaskDefinition_taskDefinition,
    deregisterTaskDefinitionResponse_taskDefinition,
    deregisterTaskDefinitionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateTaskSet
    createTaskSet_clientToken,
    createTaskSet_platformVersion,
    createTaskSet_scale,
    createTaskSet_loadBalancers,
    createTaskSet_launchType,
    createTaskSet_externalId,
    createTaskSet_networkConfiguration,
    createTaskSet_serviceRegistries,
    createTaskSet_capacityProviderStrategy,
    createTaskSet_tags,
    createTaskSet_service,
    createTaskSet_cluster,
    createTaskSet_taskDefinition,
    createTaskSetResponse_taskSet,
    createTaskSetResponse_httpStatus,

    -- ** DescribeTasks
    describeTasks_include,
    describeTasks_cluster,
    describeTasks_tasks,
    describeTasksResponse_failures,
    describeTasksResponse_tasks,
    describeTasksResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_clusterArns,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeServices
    describeServices_include,
    describeServices_cluster,
    describeServices_services,
    describeServicesResponse_failures,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** DeregisterContainerInstance
    deregisterContainerInstance_cluster,
    deregisterContainerInstance_force,
    deregisterContainerInstance_containerInstance,
    deregisterContainerInstanceResponse_containerInstance,
    deregisterContainerInstanceResponse_httpStatus,

    -- ** UpdateClusterSettings
    updateClusterSettings_cluster,
    updateClusterSettings_settings,
    updateClusterSettingsResponse_cluster,
    updateClusterSettingsResponse_httpStatus,

    -- ** DeleteAttributes
    deleteAttributes_cluster,
    deleteAttributes_attributes,
    deleteAttributesResponse_attributes,
    deleteAttributesResponse_httpStatus,

    -- ** PutAttributes
    putAttributes_cluster,
    putAttributes_attributes,
    putAttributesResponse_attributes,
    putAttributesResponse_httpStatus,

    -- ** ListAccountSettings
    listAccountSettings_value,
    listAccountSettings_nextToken,
    listAccountSettings_name,
    listAccountSettings_principalArn,
    listAccountSettings_effectiveSettings,
    listAccountSettings_maxResults,
    listAccountSettingsResponse_settings,
    listAccountSettingsResponse_nextToken,
    listAccountSettingsResponse_httpStatus,

    -- ** DeleteTaskSet
    deleteTaskSet_force,
    deleteTaskSet_cluster,
    deleteTaskSet_service,
    deleteTaskSet_taskSet,
    deleteTaskSetResponse_taskSet,
    deleteTaskSetResponse_httpStatus,

    -- ** UpdateTaskSet
    updateTaskSet_cluster,
    updateTaskSet_service,
    updateTaskSet_taskSet,
    updateTaskSet_scale,
    updateTaskSetResponse_taskSet,
    updateTaskSetResponse_httpStatus,

    -- ** CreateCapacityProvider
    createCapacityProvider_tags,
    createCapacityProvider_name,
    createCapacityProvider_autoScalingGroupProvider,
    createCapacityProviderResponse_capacityProvider,
    createCapacityProviderResponse_httpStatus,

    -- ** DescribeTaskSets
    describeTaskSets_taskSets,
    describeTaskSets_include,
    describeTaskSets_cluster,
    describeTaskSets_service,
    describeTaskSetsResponse_taskSets,
    describeTaskSetsResponse_failures,
    describeTaskSetsResponse_httpStatus,

    -- ** RegisterTaskDefinition
    registerTaskDefinition_inferenceAccelerators,
    registerTaskDefinition_executionRoleArn,
    registerTaskDefinition_requiresCompatibilities,
    registerTaskDefinition_ephemeralStorage,
    registerTaskDefinition_pidMode,
    registerTaskDefinition_ipcMode,
    registerTaskDefinition_memory,
    registerTaskDefinition_proxyConfiguration,
    registerTaskDefinition_taskRoleArn,
    registerTaskDefinition_placementConstraints,
    registerTaskDefinition_networkMode,
    registerTaskDefinition_volumes,
    registerTaskDefinition_cpu,
    registerTaskDefinition_tags,
    registerTaskDefinition_family,
    registerTaskDefinition_containerDefinitions,
    registerTaskDefinitionResponse_taskDefinition,
    registerTaskDefinitionResponse_tags,
    registerTaskDefinitionResponse_httpStatus,

    -- ** CreateService
    createService_cluster,
    createService_clientToken,
    createService_propagateTags,
    createService_platformVersion,
    createService_enableECSManagedTags,
    createService_desiredCount,
    createService_loadBalancers,
    createService_role,
    createService_placementConstraints,
    createService_placementStrategy,
    createService_deploymentController,
    createService_launchType,
    createService_taskDefinition,
    createService_schedulingStrategy,
    createService_healthCheckGracePeriodSeconds,
    createService_networkConfiguration,
    createService_serviceRegistries,
    createService_capacityProviderStrategy,
    createService_enableExecuteCommand,
    createService_tags,
    createService_deploymentConfiguration,
    createService_serviceName,
    createServiceResponse_service,
    createServiceResponse_httpStatus,

    -- * Types

    -- ** Attachment
    attachment_status,
    attachment_details,
    attachment_id,
    attachment_type,

    -- ** AttachmentStateChange
    attachmentStateChange_attachmentArn,
    attachmentStateChange_status,

    -- ** Attribute
    attribute_targetId,
    attribute_value,
    attribute_targetType,
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
    capacityProvider_status,
    capacityProvider_updateStatusReason,
    capacityProvider_autoScalingGroupProvider,
    capacityProvider_name,
    capacityProvider_updateStatus,
    capacityProvider_capacityProviderArn,
    capacityProvider_tags,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** Cluster
    cluster_status,
    cluster_clusterArn,
    cluster_attachments,
    cluster_runningTasksCount,
    cluster_defaultCapacityProviderStrategy,
    cluster_settings,
    cluster_registeredContainerInstancesCount,
    cluster_pendingTasksCount,
    cluster_clusterName,
    cluster_statistics,
    cluster_configuration,
    cluster_attachmentsStatus,
    cluster_capacityProviders,
    cluster_activeServicesCount,
    cluster_tags,

    -- ** ClusterConfiguration
    clusterConfiguration_executeCommandConfiguration,

    -- ** ClusterSetting
    clusterSetting_value,
    clusterSetting_name,

    -- ** Container
    container_gpuIds,
    container_networkBindings,
    container_managedAgents,
    container_image,
    container_containerArn,
    container_networkInterfaces,
    container_taskArn,
    container_lastStatus,
    container_memory,
    container_reason,
    container_name,
    container_imageDigest,
    container_exitCode,
    container_healthStatus,
    container_cpu,
    container_runtimeId,
    container_memoryReservation,

    -- ** ContainerDefinition
    containerDefinition_image,
    containerDefinition_command,
    containerDefinition_hostname,
    containerDefinition_repositoryCredentials,
    containerDefinition_dockerSecurityOptions,
    containerDefinition_healthCheck,
    containerDefinition_disableNetworking,
    containerDefinition_secrets,
    containerDefinition_volumesFrom,
    containerDefinition_environment,
    containerDefinition_environmentFiles,
    containerDefinition_entryPoint,
    containerDefinition_workingDirectory,
    containerDefinition_ulimits,
    containerDefinition_stopTimeout,
    containerDefinition_privileged,
    containerDefinition_portMappings,
    containerDefinition_resourceRequirements,
    containerDefinition_dockerLabels,
    containerDefinition_extraHosts,
    containerDefinition_memory,
    containerDefinition_systemControls,
    containerDefinition_user,
    containerDefinition_firelensConfiguration,
    containerDefinition_dnsSearchDomains,
    containerDefinition_logConfiguration,
    containerDefinition_linuxParameters,
    containerDefinition_pseudoTerminal,
    containerDefinition_dependsOn,
    containerDefinition_name,
    containerDefinition_dnsServers,
    containerDefinition_mountPoints,
    containerDefinition_interactive,
    containerDefinition_startTimeout,
    containerDefinition_links,
    containerDefinition_readonlyRootFilesystem,
    containerDefinition_essential,
    containerDefinition_cpu,
    containerDefinition_memoryReservation,

    -- ** ContainerDependency
    containerDependency_containerName,
    containerDependency_condition,

    -- ** ContainerInstance
    containerInstance_status,
    containerInstance_attachments,
    containerInstance_runningTasksCount,
    containerInstance_remainingResources,
    containerInstance_ec2InstanceId,
    containerInstance_containerInstanceArn,
    containerInstance_agentConnected,
    containerInstance_versionInfo,
    containerInstance_agentUpdateStatus,
    containerInstance_attributes,
    containerInstance_version,
    containerInstance_pendingTasksCount,
    containerInstance_capacityProviderName,
    containerInstance_registeredAt,
    containerInstance_statusReason,
    containerInstance_tags,
    containerInstance_registeredResources,

    -- ** ContainerOverride
    containerOverride_command,
    containerOverride_environment,
    containerOverride_environmentFiles,
    containerOverride_resourceRequirements,
    containerOverride_memory,
    containerOverride_name,
    containerOverride_cpu,
    containerOverride_memoryReservation,

    -- ** ContainerService
    containerService_taskSets,
    containerService_runningCount,
    containerService_status,
    containerService_clusterArn,
    containerService_propagateTags,
    containerService_createdAt,
    containerService_platformVersion,
    containerService_enableECSManagedTags,
    containerService_createdBy,
    containerService_desiredCount,
    containerService_loadBalancers,
    containerService_pendingCount,
    containerService_placementConstraints,
    containerService_events,
    containerService_placementStrategy,
    containerService_deployments,
    containerService_serviceName,
    containerService_deploymentController,
    containerService_launchType,
    containerService_serviceArn,
    containerService_taskDefinition,
    containerService_schedulingStrategy,
    containerService_healthCheckGracePeriodSeconds,
    containerService_networkConfiguration,
    containerService_serviceRegistries,
    containerService_capacityProviderStrategy,
    containerService_enableExecuteCommand,
    containerService_tags,
    containerService_roleArn,
    containerService_deploymentConfiguration,

    -- ** ContainerStateChange
    containerStateChange_networkBindings,
    containerStateChange_status,
    containerStateChange_containerName,
    containerStateChange_reason,
    containerStateChange_imageDigest,
    containerStateChange_exitCode,
    containerStateChange_runtimeId,

    -- ** Deployment
    deployment_rolloutState,
    deployment_runningCount,
    deployment_status,
    deployment_createdAt,
    deployment_platformVersion,
    deployment_desiredCount,
    deployment_pendingCount,
    deployment_id,
    deployment_failedTasks,
    deployment_launchType,
    deployment_updatedAt,
    deployment_taskDefinition,
    deployment_rolloutStateReason,
    deployment_networkConfiguration,
    deployment_capacityProviderStrategy,

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
    device_containerPath,
    device_permissions,
    device_hostPath,

    -- ** DockerVolumeConfiguration
    dockerVolumeConfiguration_driverOpts,
    dockerVolumeConfiguration_driver,
    dockerVolumeConfiguration_scope,
    dockerVolumeConfiguration_labels,
    dockerVolumeConfiguration_autoprovision,

    -- ** EFSAuthorizationConfig
    eFSAuthorizationConfig_accessPointId,
    eFSAuthorizationConfig_iam,

    -- ** EFSVolumeConfiguration
    eFSVolumeConfiguration_rootDirectory,
    eFSVolumeConfiguration_transitEncryption,
    eFSVolumeConfiguration_authorizationConfig,
    eFSVolumeConfiguration_transitEncryptionPort,
    eFSVolumeConfiguration_fileSystemId,

    -- ** EnvironmentFile
    environmentFile_value,
    environmentFile_type,

    -- ** EphemeralStorage
    ephemeralStorage_sizeInGiB,

    -- ** ExecuteCommandConfiguration
    executeCommandConfiguration_logConfiguration,
    executeCommandConfiguration_kmsKeyId,
    executeCommandConfiguration_logging,

    -- ** ExecuteCommandLogConfiguration
    executeCommandLogConfiguration_cloudWatchLogGroupName,
    executeCommandLogConfiguration_s3KeyPrefix,
    executeCommandLogConfiguration_cloudWatchEncryptionEnabled,
    executeCommandLogConfiguration_s3EncryptionEnabled,
    executeCommandLogConfiguration_s3BucketName,

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
    healthCheck_startPeriod,
    healthCheck_retries,
    healthCheck_interval,
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

    -- ** KernelCapabilities
    kernelCapabilities_drop,
    kernelCapabilities_add,

    -- ** KeyValuePair
    keyValuePair_value,
    keyValuePair_name,

    -- ** LinuxParameters
    linuxParameters_sharedMemorySize,
    linuxParameters_initProcessEnabled,
    linuxParameters_tmpfs,
    linuxParameters_swappiness,
    linuxParameters_devices,
    linuxParameters_capabilities,
    linuxParameters_maxSwap,

    -- ** LoadBalancer
    loadBalancer_loadBalancerName,
    loadBalancer_containerName,
    loadBalancer_targetGroupArn,
    loadBalancer_containerPort,

    -- ** LogConfiguration
    logConfiguration_options,
    logConfiguration_secretOptions,
    logConfiguration_logDriver,

    -- ** ManagedAgent
    managedAgent_lastStatus,
    managedAgent_reason,
    managedAgent_name,
    managedAgent_lastStartedAt,

    -- ** ManagedAgentStateChange
    managedAgentStateChange_reason,
    managedAgentStateChange_containerName,
    managedAgentStateChange_managedAgentName,
    managedAgentStateChange_status,

    -- ** ManagedScaling
    managedScaling_status,
    managedScaling_maximumScalingStepSize,
    managedScaling_targetCapacity,
    managedScaling_minimumScalingStepSize,
    managedScaling_instanceWarmupPeriod,

    -- ** MountPoint
    mountPoint_containerPath,
    mountPoint_sourceVolume,
    mountPoint_readOnly,

    -- ** NetworkBinding
    networkBinding_bindIP,
    networkBinding_protocol,
    networkBinding_hostPort,
    networkBinding_containerPort,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** NetworkInterface
    networkInterface_ipv6Address,
    networkInterface_privateIpv4Address,
    networkInterface_attachmentId,

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
    portMapping_protocol,
    portMapping_hostPort,
    portMapping_containerPort,

    -- ** ProxyConfiguration
    proxyConfiguration_type,
    proxyConfiguration_properties,
    proxyConfiguration_containerName,

    -- ** RepositoryCredentials
    repositoryCredentials_credentialsParameter,

    -- ** Resource
    resource_stringSetValue,
    resource_integerValue,
    resource_doubleValue,
    resource_longValue,
    resource_name,
    resource_type,

    -- ** ResourceRequirement
    resourceRequirement_value,
    resourceRequirement_type,

    -- ** Scale
    scale_value,
    scale_unit,

    -- ** Secret
    secret_name,
    secret_valueFrom,

    -- ** ServiceEvent
    serviceEvent_createdAt,
    serviceEvent_id,
    serviceEvent_message,

    -- ** ServiceRegistry
    serviceRegistry_registryArn,
    serviceRegistry_containerName,
    serviceRegistry_containerPort,
    serviceRegistry_port,

    -- ** Session
    session_streamUrl,
    session_tokenValue,
    session_sessionId,

    -- ** Setting
    setting_value,
    setting_name,
    setting_principalArn,

    -- ** SystemControl
    systemControl_value,
    systemControl_namespace,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Task
    task_stoppedAt,
    task_desiredStatus,
    task_overrides,
    task_inferenceAccelerators,
    task_clusterArn,
    task_group,
    task_attachments,
    task_createdAt,
    task_stopCode,
    task_platformVersion,
    task_taskArn,
    task_containerInstanceArn,
    task_executionStoppedAt,
    task_ephemeralStorage,
    task_lastStatus,
    task_memory,
    task_pullStoppedAt,
    task_containers,
    task_startedAt,
    task_availabilityZone,
    task_attributes,
    task_version,
    task_capacityProviderName,
    task_startedBy,
    task_stoppedReason,
    task_connectivity,
    task_stoppingAt,
    task_launchType,
    task_taskDefinitionArn,
    task_healthStatus,
    task_connectivityAt,
    task_cpu,
    task_enableExecuteCommand,
    task_pullStartedAt,
    task_tags,

    -- ** TaskDefinition
    taskDefinition_status,
    taskDefinition_inferenceAccelerators,
    taskDefinition_executionRoleArn,
    taskDefinition_requiresCompatibilities,
    taskDefinition_ephemeralStorage,
    taskDefinition_pidMode,
    taskDefinition_family,
    taskDefinition_ipcMode,
    taskDefinition_containerDefinitions,
    taskDefinition_memory,
    taskDefinition_proxyConfiguration,
    taskDefinition_taskRoleArn,
    taskDefinition_deregisteredAt,
    taskDefinition_placementConstraints,
    taskDefinition_registeredAt,
    taskDefinition_networkMode,
    taskDefinition_taskDefinitionArn,
    taskDefinition_compatibilities,
    taskDefinition_registeredBy,
    taskDefinition_revision,
    taskDefinition_volumes,
    taskDefinition_cpu,
    taskDefinition_requiresAttributes,

    -- ** TaskDefinitionPlacementConstraint
    taskDefinitionPlacementConstraint_expression,
    taskDefinitionPlacementConstraint_type,

    -- ** TaskOverride
    taskOverride_containerOverrides,
    taskOverride_executionRoleArn,
    taskOverride_ephemeralStorage,
    taskOverride_memory,
    taskOverride_taskRoleArn,
    taskOverride_inferenceAcceleratorOverrides,
    taskOverride_cpu,

    -- ** TaskSet
    taskSet_runningCount,
    taskSet_status,
    taskSet_clusterArn,
    taskSet_computedDesiredCount,
    taskSet_createdAt,
    taskSet_platformVersion,
    taskSet_scale,
    taskSet_loadBalancers,
    taskSet_stabilityStatusAt,
    taskSet_pendingCount,
    taskSet_taskSetArn,
    taskSet_startedBy,
    taskSet_id,
    taskSet_launchType,
    taskSet_updatedAt,
    taskSet_serviceArn,
    taskSet_taskDefinition,
    taskSet_externalId,
    taskSet_networkConfiguration,
    taskSet_serviceRegistries,
    taskSet_capacityProviderStrategy,
    taskSet_stabilityStatus,
    taskSet_tags,

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
    volume_fsxWindowsFileServerVolumeConfiguration,
    volume_name,
    volume_efsVolumeConfiguration,
    volume_host,

    -- ** VolumeFrom
    volumeFrom_sourceContainer,
    volumeFrom_readOnly,
  )
where

import Network.AWS.ECS.CreateCapacityProvider
import Network.AWS.ECS.CreateCluster
import Network.AWS.ECS.CreateService
import Network.AWS.ECS.CreateTaskSet
import Network.AWS.ECS.DeleteAccountSetting
import Network.AWS.ECS.DeleteAttributes
import Network.AWS.ECS.DeleteCapacityProvider
import Network.AWS.ECS.DeleteCluster
import Network.AWS.ECS.DeleteService
import Network.AWS.ECS.DeleteTaskSet
import Network.AWS.ECS.DeregisterContainerInstance
import Network.AWS.ECS.DeregisterTaskDefinition
import Network.AWS.ECS.DescribeCapacityProviders
import Network.AWS.ECS.DescribeClusters
import Network.AWS.ECS.DescribeContainerInstances
import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTaskDefinition
import Network.AWS.ECS.DescribeTaskSets
import Network.AWS.ECS.DescribeTasks
import Network.AWS.ECS.DiscoverPollEndpoint
import Network.AWS.ECS.ExecuteCommand
import Network.AWS.ECS.ListAccountSettings
import Network.AWS.ECS.ListAttributes
import Network.AWS.ECS.ListClusters
import Network.AWS.ECS.ListContainerInstances
import Network.AWS.ECS.ListServices
import Network.AWS.ECS.ListTagsForResource
import Network.AWS.ECS.ListTaskDefinitionFamilies
import Network.AWS.ECS.ListTaskDefinitions
import Network.AWS.ECS.ListTasks
import Network.AWS.ECS.PutAccountSetting
import Network.AWS.ECS.PutAccountSettingDefault
import Network.AWS.ECS.PutAttributes
import Network.AWS.ECS.PutClusterCapacityProviders
import Network.AWS.ECS.RegisterContainerInstance
import Network.AWS.ECS.RegisterTaskDefinition
import Network.AWS.ECS.RunTask
import Network.AWS.ECS.StartTask
import Network.AWS.ECS.StopTask
import Network.AWS.ECS.SubmitAttachmentStateChanges
import Network.AWS.ECS.SubmitContainerStateChange
import Network.AWS.ECS.SubmitTaskStateChange
import Network.AWS.ECS.TagResource
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.AttachmentStateChange
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
import Network.AWS.ECS.Types.AwsVpcConfiguration
import Network.AWS.ECS.Types.CapacityProvider
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.Cluster
import Network.AWS.ECS.Types.ClusterConfiguration
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.ContainerInstance
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.ContainerService
import Network.AWS.ECS.Types.ContainerStateChange
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentCircuitBreaker
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.EphemeralStorage
import Network.AWS.ECS.Types.ExecuteCommandConfiguration
import Network.AWS.ECS.Types.ExecuteCommandLogConfiguration
import Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import Network.AWS.ECS.Types.FSxWindowsFileServerVolumeConfiguration
import Network.AWS.ECS.Types.Failure
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.ManagedAgent
import Network.AWS.ECS.Types.ManagedAgentStateChange
import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.NetworkInterface
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PlatformDevice
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Session
import Network.AWS.ECS.Types.Setting
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.Task
import Network.AWS.ECS.Types.TaskDefinition
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.ECS.Types.Volume
import Network.AWS.ECS.Types.VolumeFrom
import Network.AWS.ECS.UntagResource
import Network.AWS.ECS.UpdateCapacityProvider
import Network.AWS.ECS.UpdateCluster
import Network.AWS.ECS.UpdateClusterSettings
import Network.AWS.ECS.UpdateContainerAgent
import Network.AWS.ECS.UpdateContainerInstancesState
import Network.AWS.ECS.UpdateService
import Network.AWS.ECS.UpdateServicePrimaryTaskSet
import Network.AWS.ECS.UpdateTaskSet
