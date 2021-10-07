{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Lens
  ( -- * Operations

    -- ** DescribeDeployments
    describeDeployments_deploymentIds,
    describeDeployments_appId,
    describeDeployments_stackId,
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,

    -- ** UpdateMyUserProfile
    updateMyUserProfile_sshPublicKey,

    -- ** DescribeInstances
    describeInstances_instanceIds,
    describeInstances_stackId,
    describeInstances_layerId,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** SetTimeBasedAutoScaling
    setTimeBasedAutoScaling_autoScalingSchedule,
    setTimeBasedAutoScaling_instanceId,

    -- ** AttachElasticLoadBalancer
    attachElasticLoadBalancer_elasticLoadBalancerName,
    attachElasticLoadBalancer_layerId,

    -- ** DescribeRdsDbInstances
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,

    -- ** DeregisterElasticIp
    deregisterElasticIp_elasticIp,

    -- ** SetPermission
    setPermission_allowSudo,
    setPermission_allowSsh,
    setPermission_level,
    setPermission_stackId,
    setPermission_iamUserArn,

    -- ** RegisterVolume
    registerVolume_ec2VolumeId,
    registerVolume_stackId,
    registerVolumeResponse_volumeId,
    registerVolumeResponse_httpStatus,

    -- ** StartInstance
    startInstance_instanceId,

    -- ** DescribeEcsClusters
    describeEcsClusters_nextToken,
    describeEcsClusters_maxResults,
    describeEcsClusters_stackId,
    describeEcsClusters_ecsClusterArns,
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceId,

    -- ** DisassociateElasticIp
    disassociateElasticIp_elasticIp,

    -- ** DescribeOperatingSystems
    describeOperatingSystemsResponse_operatingSystems,
    describeOperatingSystemsResponse_httpStatus,

    -- ** StopStack
    stopStack_stackId,

    -- ** DescribeVolumes
    describeVolumes_instanceId,
    describeVolumes_volumeIds,
    describeVolumes_stackId,
    describeVolumes_raidArrayId,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** StartStack
    startStack_stackId,

    -- ** UpdateUserProfile
    updateUserProfile_sshUsername,
    updateUserProfile_allowSelfManagement,
    updateUserProfile_sshPublicKey,
    updateUserProfile_iamUserArn,

    -- ** DescribeTimeBasedAutoScaling
    describeTimeBasedAutoScaling_instanceIds,
    describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations,
    describeTimeBasedAutoScalingResponse_httpStatus,

    -- ** DescribeServiceErrors
    describeServiceErrors_instanceId,
    describeServiceErrors_stackId,
    describeServiceErrors_serviceErrorIds,
    describeServiceErrorsResponse_serviceErrors,
    describeServiceErrorsResponse_httpStatus,

    -- ** RegisterRdsDbInstance
    registerRdsDbInstance_stackId,
    registerRdsDbInstance_rdsDbInstanceArn,
    registerRdsDbInstance_dbUser,
    registerRdsDbInstance_dbPassword,

    -- ** DeleteUserProfile
    deleteUserProfile_iamUserArn,

    -- ** DescribeMyUserProfile
    describeMyUserProfileResponse_userProfile,
    describeMyUserProfileResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateInstance
    updateInstance_hostname,
    updateInstance_installUpdatesOnBoot,
    updateInstance_ebsOptimized,
    updateInstance_instanceType,
    updateInstance_agentVersion,
    updateInstance_amiId,
    updateInstance_sshKeyName,
    updateInstance_architecture,
    updateInstance_layerIds,
    updateInstance_autoScalingType,
    updateInstance_os,
    updateInstance_instanceId,

    -- ** AssignInstance
    assignInstance_instanceId,
    assignInstance_layerIds,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** RebootInstance
    rebootInstance_instanceId,

    -- ** DeregisterVolume
    deregisterVolume_volumeId,

    -- ** DescribeStackProvisioningParameters
    describeStackProvisioningParameters_stackId,
    describeStackProvisioningParametersResponse_agentInstallerUrl,
    describeStackProvisioningParametersResponse_parameters,
    describeStackProvisioningParametersResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_deleteVolumes,
    deleteInstance_deleteElasticIp,
    deleteInstance_instanceId,

    -- ** DescribeStacks
    describeStacks_stackIds,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DetachElasticLoadBalancer
    detachElasticLoadBalancer_elasticLoadBalancerName,
    detachElasticLoadBalancer_layerId,

    -- ** RegisterElasticIp
    registerElasticIp_elasticIp,
    registerElasticIp_stackId,
    registerElasticIpResponse_elasticIp,
    registerElasticIpResponse_httpStatus,

    -- ** CloneStack
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_defaultOs,
    cloneStack_customCookbooksSource,
    cloneStack_defaultAvailabilityZone,
    cloneStack_customJson,
    cloneStack_agentVersion,
    cloneStack_clonePermissions,
    cloneStack_defaultRootDeviceType,
    cloneStack_name,
    cloneStack_attributes,
    cloneStack_defaultInstanceProfileArn,
    cloneStack_cloneAppIds,
    cloneStack_hostnameTheme,
    cloneStack_configurationManager,
    cloneStack_defaultSshKeyName,
    cloneStack_chefConfiguration,
    cloneStack_region,
    cloneStack_vpcId,
    cloneStack_defaultSubnetId,
    cloneStack_useCustomCookbooks,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,

    -- ** DescribeAgentVersions
    describeAgentVersions_stackId,
    describeAgentVersions_configurationManager,
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,

    -- ** UpdateLayer
    updateLayer_installUpdatesOnBoot,
    updateLayer_customSecurityGroupIds,
    updateLayer_customInstanceProfileArn,
    updateLayer_packages,
    updateLayer_enableAutoHealing,
    updateLayer_customJson,
    updateLayer_volumeConfigurations,
    updateLayer_shortname,
    updateLayer_name,
    updateLayer_attributes,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_autoAssignElasticIps,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_customRecipes,
    updateLayer_autoAssignPublicIps,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_layerId,

    -- ** CreateStack
    createStack_useOpsworksSecurityGroups,
    createStack_defaultOs,
    createStack_customCookbooksSource,
    createStack_defaultAvailabilityZone,
    createStack_customJson,
    createStack_agentVersion,
    createStack_defaultRootDeviceType,
    createStack_attributes,
    createStack_hostnameTheme,
    createStack_configurationManager,
    createStack_defaultSshKeyName,
    createStack_chefConfiguration,
    createStack_vpcId,
    createStack_defaultSubnetId,
    createStack_useCustomCookbooks,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** DeleteLayer
    deleteLayer_layerId,

    -- ** UnassignVolume
    unassignVolume_volumeId,

    -- ** GrantAccess
    grantAccess_validForInMinutes,
    grantAccess_instanceId,
    grantAccessResponse_temporaryCredential,
    grantAccessResponse_httpStatus,

    -- ** CreateLayer
    createLayer_installUpdatesOnBoot,
    createLayer_customSecurityGroupIds,
    createLayer_customInstanceProfileArn,
    createLayer_packages,
    createLayer_enableAutoHealing,
    createLayer_customJson,
    createLayer_volumeConfigurations,
    createLayer_attributes,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_autoAssignElasticIps,
    createLayer_useEbsOptimizedInstances,
    createLayer_customRecipes,
    createLayer_autoAssignPublicIps,
    createLayer_lifecycleEventConfiguration,
    createLayer_stackId,
    createLayer_type,
    createLayer_name,
    createLayer_shortname,
    createLayerResponse_layerId,
    createLayerResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_stackId,

    -- ** UpdateStack
    updateStack_useOpsworksSecurityGroups,
    updateStack_defaultOs,
    updateStack_customCookbooksSource,
    updateStack_serviceRoleArn,
    updateStack_defaultAvailabilityZone,
    updateStack_customJson,
    updateStack_agentVersion,
    updateStack_defaultRootDeviceType,
    updateStack_name,
    updateStack_attributes,
    updateStack_defaultInstanceProfileArn,
    updateStack_hostnameTheme,
    updateStack_configurationManager,
    updateStack_defaultSshKeyName,
    updateStack_chefConfiguration,
    updateStack_defaultSubnetId,
    updateStack_useCustomCookbooks,
    updateStack_stackId,

    -- ** SetLoadBasedAutoScaling
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_layerId,

    -- ** DescribeStackSummary
    describeStackSummary_stackId,
    describeStackSummaryResponse_stackSummary,
    describeStackSummaryResponse_httpStatus,

    -- ** DescribeApps
    describeApps_appIds,
    describeApps_stackId,
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,

    -- ** DeregisterEcsCluster
    deregisterEcsCluster_ecsClusterArn,

    -- ** DescribeUserProfiles
    describeUserProfiles_iamUserArns,
    describeUserProfilesResponse_userProfiles,
    describeUserProfilesResponse_httpStatus,

    -- ** DescribeElasticLoadBalancers
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,

    -- ** DescribeRaidArrays
    describeRaidArrays_instanceId,
    describeRaidArrays_raidArrayIds,
    describeRaidArrays_stackId,
    describeRaidArraysResponse_raidArrays,
    describeRaidArraysResponse_httpStatus,

    -- ** DescribeCommands
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommands_commandIds,
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,

    -- ** DeregisterInstance
    deregisterInstance_instanceId,

    -- ** UpdateVolume
    updateVolume_name,
    updateVolume_mountPoint,
    updateVolume_volumeId,

    -- ** AssignVolume
    assignVolume_instanceId,
    assignVolume_volumeId,

    -- ** RegisterEcsCluster
    registerEcsCluster_ecsClusterArn,
    registerEcsCluster_stackId,
    registerEcsClusterResponse_ecsClusterArn,
    registerEcsClusterResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_sshUsername,
    createUserProfile_allowSelfManagement,
    createUserProfile_sshPublicKey,
    createUserProfile_iamUserArn,
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** UnassignInstance
    unassignInstance_instanceId,

    -- ** UpdateRdsDbInstance
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- ** RegisterInstance
    registerInstance_hostname,
    registerInstance_rsaPublicKey,
    registerInstance_instanceIdentity,
    registerInstance_privateIp,
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_publicIp,
    registerInstance_stackId,
    registerInstanceResponse_instanceId,
    registerInstanceResponse_httpStatus,

    -- ** DescribeLoadBasedAutoScaling
    describeLoadBasedAutoScaling_layerIds,
    describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations,
    describeLoadBasedAutoScalingResponse_httpStatus,

    -- ** AssociateElasticIp
    associateElasticIp_instanceId,
    associateElasticIp_elasticIp,

    -- ** UpdateApp
    updateApp_sslConfiguration,
    updateApp_appSource,
    updateApp_dataSources,
    updateApp_domains,
    updateApp_enableSsl,
    updateApp_environment,
    updateApp_name,
    updateApp_attributes,
    updateApp_description,
    updateApp_type,
    updateApp_appId,

    -- ** DeleteApp
    deleteApp_appId,

    -- ** GetHostnameSuggestion
    getHostnameSuggestion_layerId,
    getHostnameSuggestionResponse_hostname,
    getHostnameSuggestionResponse_layerId,
    getHostnameSuggestionResponse_httpStatus,

    -- ** CreateInstance
    createInstance_virtualizationType,
    createInstance_hostname,
    createInstance_installUpdatesOnBoot,
    createInstance_ebsOptimized,
    createInstance_rootDeviceType,
    createInstance_agentVersion,
    createInstance_amiId,
    createInstance_sshKeyName,
    createInstance_architecture,
    createInstance_autoScalingType,
    createInstance_tenancy,
    createInstance_availabilityZone,
    createInstance_os,
    createInstance_blockDeviceMappings,
    createInstance_subnetId,
    createInstance_stackId,
    createInstance_layerIds,
    createInstance_instanceType,
    createInstanceResponse_instanceId,
    createInstanceResponse_httpStatus,

    -- ** DescribeLayers
    describeLayers_stackId,
    describeLayers_layerIds,
    describeLayersResponse_layers,
    describeLayersResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_instanceIds,
    createDeployment_appId,
    createDeployment_customJson,
    createDeployment_comment,
    createDeployment_layerIds,
    createDeployment_stackId,
    createDeployment_command,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateApp
    createApp_sslConfiguration,
    createApp_appSource,
    createApp_dataSources,
    createApp_domains,
    createApp_enableSsl,
    createApp_environment,
    createApp_shortname,
    createApp_attributes,
    createApp_description,
    createApp_stackId,
    createApp_name,
    createApp_type,
    createAppResponse_appId,
    createAppResponse_httpStatus,

    -- ** DescribePermissions
    describePermissions_iamUserArn,
    describePermissions_stackId,
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,

    -- ** UpdateElasticIp
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- ** DescribeElasticIps
    describeElasticIps_instanceId,
    describeElasticIps_stackId,
    describeElasticIps_ips,
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,

    -- ** DeregisterRdsDbInstance
    deregisterRdsDbInstance_rdsDbInstanceArn,

    -- * Types

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_configurationManager,

    -- ** App
    app_appId,
    app_sslConfiguration,
    app_appSource,
    app_dataSources,
    app_domains,
    app_stackId,
    app_createdAt,
    app_enableSsl,
    app_environment,
    app_shortname,
    app_name,
    app_attributes,
    app_description,
    app_type,

    -- ** AutoScalingThresholds
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_cpuThreshold,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_alarms,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_instanceCount,

    -- ** BlockDeviceMapping
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- ** ChefConfiguration
    chefConfiguration_manageBerkshelf,
    chefConfiguration_berkshelfVersion,

    -- ** CloudWatchLogsConfiguration
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- ** CloudWatchLogsLogStream
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_datetimeFormat,

    -- ** Command
    command_logUrl,
    command_status,
    command_deploymentId,
    command_instanceId,
    command_completedAt,
    command_createdAt,
    command_exitCode,
    command_commandId,
    command_acknowledgedAt,
    command_type,

    -- ** DataSource
    dataSource_arn,
    dataSource_type,
    dataSource_databaseName,

    -- ** Deployment
    deployment_instanceIds,
    deployment_appId,
    deployment_status,
    deployment_deploymentId,
    deployment_iamUserArn,
    deployment_duration,
    deployment_customJson,
    deployment_stackId,
    deployment_comment,
    deployment_completedAt,
    deployment_createdAt,
    deployment_command,

    -- ** DeploymentCommand
    deploymentCommand_args,
    deploymentCommand_name,

    -- ** EbsBlockDevice
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_iops,
    ebsBlockDevice_volumeSize,

    -- ** EcsCluster
    ecsCluster_stackId,
    ecsCluster_ecsClusterName,
    ecsCluster_registeredAt,
    ecsCluster_ecsClusterArn,

    -- ** ElasticIp
    elasticIp_instanceId,
    elasticIp_ip,
    elasticIp_domain,
    elasticIp_name,
    elasticIp_region,

    -- ** ElasticLoadBalancer
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_subnetIds,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_region,
    elasticLoadBalancer_vpcId,

    -- ** EnvironmentVariable
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- ** Instance
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_platform,
    instance_hostname,
    instance_sshHostRsaKeyFingerprint,
    instance_securityGroupIds,
    instance_installUpdatesOnBoot,
    instance_status,
    instance_instanceId,
    instance_privateDns,
    instance_reportedAgentVersion,
    instance_elasticIp,
    instance_ebsOptimized,
    instance_sshHostDsaKeyFingerprint,
    instance_instanceType,
    instance_rootDeviceType,
    instance_agentVersion,
    instance_stackId,
    instance_rootDeviceVolumeId,
    instance_publicDns,
    instance_amiId,
    instance_sshKeyName,
    instance_architecture,
    instance_createdAt,
    instance_layerIds,
    instance_arn,
    instance_autoScalingType,
    instance_tenancy,
    instance_availabilityZone,
    instance_os,
    instance_privateIp,
    instance_infrastructureClass,
    instance_ecsContainerInstanceArn,
    instance_blockDeviceMappings,
    instance_subnetId,
    instance_registeredBy,
    instance_reportedOs,
    instance_ec2InstanceId,
    instance_publicIp,
    instance_lastServiceErrorId,
    instance_ecsClusterArn,

    -- ** InstanceIdentity
    instanceIdentity_document,
    instanceIdentity_signature,

    -- ** InstancesCount
    instancesCount_online,
    instancesCount_setupFailed,
    instancesCount_registering,
    instancesCount_booting,
    instancesCount_startFailed,
    instancesCount_stopFailed,
    instancesCount_runningSetup,
    instancesCount_terminated,
    instancesCount_pending,
    instancesCount_terminating,
    instancesCount_shuttingDown,
    instancesCount_assigning,
    instancesCount_stopped,
    instancesCount_rebooting,
    instancesCount_stopping,
    instancesCount_requested,
    instancesCount_registered,
    instancesCount_deregistering,
    instancesCount_connectionLost,
    instancesCount_unassigning,

    -- ** Layer
    layer_installUpdatesOnBoot,
    layer_customSecurityGroupIds,
    layer_customInstanceProfileArn,
    layer_packages,
    layer_enableAutoHealing,
    layer_customJson,
    layer_stackId,
    layer_volumeConfigurations,
    layer_defaultRecipes,
    layer_createdAt,
    layer_arn,
    layer_shortname,
    layer_name,
    layer_attributes,
    layer_cloudWatchLogsConfiguration,
    layer_autoAssignElasticIps,
    layer_defaultSecurityGroupNames,
    layer_layerId,
    layer_useEbsOptimizedInstances,
    layer_customRecipes,
    layer_type,
    layer_autoAssignPublicIps,
    layer_lifecycleEventConfiguration,

    -- ** LifecycleEventConfiguration
    lifecycleEventConfiguration_shutdown,

    -- ** LoadBasedAutoScalingConfiguration
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_layerId,
    loadBasedAutoScalingConfiguration_upScaling,

    -- ** OperatingSystem
    operatingSystem_supported,
    operatingSystem_configurationManagers,
    operatingSystem_reportedVersion,
    operatingSystem_id,
    operatingSystem_name,
    operatingSystem_reportedName,
    operatingSystem_type,

    -- ** OperatingSystemConfigurationManager
    operatingSystemConfigurationManager_version,
    operatingSystemConfigurationManager_name,

    -- ** Permission
    permission_allowSudo,
    permission_iamUserArn,
    permission_stackId,
    permission_allowSsh,
    permission_level,

    -- ** RaidArray
    raidArray_instanceId,
    raidArray_numberOfDisks,
    raidArray_stackId,
    raidArray_device,
    raidArray_createdAt,
    raidArray_raidArrayId,
    raidArray_name,
    raidArray_availabilityZone,
    raidArray_mountPoint,
    raidArray_volumeType,
    raidArray_raidLevel,
    raidArray_iops,
    raidArray_size,

    -- ** RdsDbInstance
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_dbUser,
    rdsDbInstance_address,
    rdsDbInstance_stackId,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_engine,
    rdsDbInstance_region,

    -- ** Recipes
    recipes_configure,
    recipes_undeploy,
    recipes_shutdown,
    recipes_setup,
    recipes_deploy,

    -- ** ReportedOs
    reportedOs_version,
    reportedOs_name,
    reportedOs_family,

    -- ** SelfUserProfile
    selfUserProfile_iamUserArn,
    selfUserProfile_sshUsername,
    selfUserProfile_name,
    selfUserProfile_sshPublicKey,

    -- ** ServiceError'
    serviceError'_instanceId,
    serviceError'_stackId,
    serviceError'_message,
    serviceError'_serviceErrorId,
    serviceError'_createdAt,
    serviceError'_type,

    -- ** ShutdownEventConfiguration
    shutdownEventConfiguration_executionTimeout,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,

    -- ** Source
    source_sshKey,
    source_password,
    source_username,
    source_url,
    source_revision,
    source_type,

    -- ** SslConfiguration
    sslConfiguration_privateKey,
    sslConfiguration_certificate,
    sslConfiguration_chain,

    -- ** Stack
    stack_useOpsworksSecurityGroups,
    stack_defaultOs,
    stack_customCookbooksSource,
    stack_serviceRoleArn,
    stack_defaultAvailabilityZone,
    stack_customJson,
    stack_agentVersion,
    stack_stackId,
    stack_createdAt,
    stack_arn,
    stack_defaultRootDeviceType,
    stack_name,
    stack_attributes,
    stack_defaultInstanceProfileArn,
    stack_hostnameTheme,
    stack_configurationManager,
    stack_defaultSshKeyName,
    stack_chefConfiguration,
    stack_region,
    stack_vpcId,
    stack_defaultSubnetId,
    stack_useCustomCookbooks,

    -- ** StackConfigurationManager
    stackConfigurationManager_version,
    stackConfigurationManager_name,

    -- ** StackSummary
    stackSummary_stackId,
    stackSummary_layersCount,
    stackSummary_arn,
    stackSummary_name,
    stackSummary_appsCount,
    stackSummary_instancesCount,

    -- ** TemporaryCredential
    temporaryCredential_validForInMinutes,
    temporaryCredential_instanceId,
    temporaryCredential_password,
    temporaryCredential_username,

    -- ** TimeBasedAutoScalingConfiguration
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- ** UserProfile
    userProfile_iamUserArn,
    userProfile_sshUsername,
    userProfile_allowSelfManagement,
    userProfile_name,
    userProfile_sshPublicKey,

    -- ** Volume
    volume_status,
    volume_instanceId,
    volume_ec2VolumeId,
    volume_encrypted,
    volume_device,
    volume_volumeId,
    volume_raidArrayId,
    volume_name,
    volume_availabilityZone,
    volume_mountPoint,
    volume_volumeType,
    volume_region,
    volume_iops,
    volume_size,

    -- ** VolumeConfiguration
    volumeConfiguration_encrypted,
    volumeConfiguration_volumeType,
    volumeConfiguration_raidLevel,
    volumeConfiguration_iops,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- ** WeeklyAutoScalingSchedule
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_wednesday,
  )
where

import Network.AWS.OpsWorks.AssignInstance
import Network.AWS.OpsWorks.AssignVolume
import Network.AWS.OpsWorks.AssociateElasticIp
import Network.AWS.OpsWorks.AttachElasticLoadBalancer
import Network.AWS.OpsWorks.CloneStack
import Network.AWS.OpsWorks.CreateApp
import Network.AWS.OpsWorks.CreateDeployment
import Network.AWS.OpsWorks.CreateInstance
import Network.AWS.OpsWorks.CreateLayer
import Network.AWS.OpsWorks.CreateStack
import Network.AWS.OpsWorks.CreateUserProfile
import Network.AWS.OpsWorks.DeleteApp
import Network.AWS.OpsWorks.DeleteInstance
import Network.AWS.OpsWorks.DeleteLayer
import Network.AWS.OpsWorks.DeleteStack
import Network.AWS.OpsWorks.DeleteUserProfile
import Network.AWS.OpsWorks.DeregisterEcsCluster
import Network.AWS.OpsWorks.DeregisterElasticIp
import Network.AWS.OpsWorks.DeregisterInstance
import Network.AWS.OpsWorks.DeregisterRdsDbInstance
import Network.AWS.OpsWorks.DeregisterVolume
import Network.AWS.OpsWorks.DescribeAgentVersions
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeCommands
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeEcsClusters
import Network.AWS.OpsWorks.DescribeElasticIps
import Network.AWS.OpsWorks.DescribeElasticLoadBalancers
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeLayers
import Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
import Network.AWS.OpsWorks.DescribeMyUserProfile
import Network.AWS.OpsWorks.DescribeOperatingSystems
import Network.AWS.OpsWorks.DescribePermissions
import Network.AWS.OpsWorks.DescribeRaidArrays
import Network.AWS.OpsWorks.DescribeRdsDbInstances
import Network.AWS.OpsWorks.DescribeServiceErrors
import Network.AWS.OpsWorks.DescribeStackProvisioningParameters
import Network.AWS.OpsWorks.DescribeStackSummary
import Network.AWS.OpsWorks.DescribeStacks
import Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
import Network.AWS.OpsWorks.DescribeUserProfiles
import Network.AWS.OpsWorks.DescribeVolumes
import Network.AWS.OpsWorks.DetachElasticLoadBalancer
import Network.AWS.OpsWorks.DisassociateElasticIp
import Network.AWS.OpsWorks.GetHostnameSuggestion
import Network.AWS.OpsWorks.GrantAccess
import Network.AWS.OpsWorks.ListTags
import Network.AWS.OpsWorks.RebootInstance
import Network.AWS.OpsWorks.RegisterEcsCluster
import Network.AWS.OpsWorks.RegisterElasticIp
import Network.AWS.OpsWorks.RegisterInstance
import Network.AWS.OpsWorks.RegisterRdsDbInstance
import Network.AWS.OpsWorks.RegisterVolume
import Network.AWS.OpsWorks.SetLoadBasedAutoScaling
import Network.AWS.OpsWorks.SetPermission
import Network.AWS.OpsWorks.SetTimeBasedAutoScaling
import Network.AWS.OpsWorks.StartInstance
import Network.AWS.OpsWorks.StartStack
import Network.AWS.OpsWorks.StopInstance
import Network.AWS.OpsWorks.StopStack
import Network.AWS.OpsWorks.TagResource
import Network.AWS.OpsWorks.Types.AgentVersion
import Network.AWS.OpsWorks.Types.App
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.OpsWorks.Types.Command
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.Deployment
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.OpsWorks.Types.EbsBlockDevice
import Network.AWS.OpsWorks.Types.EcsCluster
import Network.AWS.OpsWorks.Types.ElasticIp
import Network.AWS.OpsWorks.Types.ElasticLoadBalancer
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Instance
import Network.AWS.OpsWorks.Types.InstanceIdentity
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.OpsWorks.Types.Layer
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.OperatingSystem
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import Network.AWS.OpsWorks.Types.Permission
import Network.AWS.OpsWorks.Types.RaidArray
import Network.AWS.OpsWorks.Types.RdsDbInstance
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.ReportedOs
import Network.AWS.OpsWorks.Types.SelfUserProfile
import Network.AWS.OpsWorks.Types.ServiceError'
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.SslConfiguration
import Network.AWS.OpsWorks.Types.Stack
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.OpsWorks.Types.StackSummary
import Network.AWS.OpsWorks.Types.TemporaryCredential
import Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.UserProfile
import Network.AWS.OpsWorks.Types.Volume
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import Network.AWS.OpsWorks.UnassignInstance
import Network.AWS.OpsWorks.UnassignVolume
import Network.AWS.OpsWorks.UntagResource
import Network.AWS.OpsWorks.UpdateApp
import Network.AWS.OpsWorks.UpdateElasticIp
import Network.AWS.OpsWorks.UpdateInstance
import Network.AWS.OpsWorks.UpdateLayer
import Network.AWS.OpsWorks.UpdateMyUserProfile
import Network.AWS.OpsWorks.UpdateRdsDbInstance
import Network.AWS.OpsWorks.UpdateStack
import Network.AWS.OpsWorks.UpdateUserProfile
import Network.AWS.OpsWorks.UpdateVolume
