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

    -- ** DescribeInstances
    describeInstances_instanceIds,
    describeInstances_stackId,
    describeInstances_layerId,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DescribeDeployments
    describeDeployments_deploymentIds,
    describeDeployments_appId,
    describeDeployments_stackId,
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,

    -- ** UpdateMyUserProfile
    updateMyUserProfile_sshPublicKey,

    -- ** DeregisterElasticIp
    deregisterElasticIp_elasticIp,

    -- ** SetTimeBasedAutoScaling
    setTimeBasedAutoScaling_autoScalingSchedule,
    setTimeBasedAutoScaling_instanceId,

    -- ** DescribeRdsDbInstances
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,

    -- ** AttachElasticLoadBalancer
    attachElasticLoadBalancer_elasticLoadBalancerName,
    attachElasticLoadBalancer_layerId,

    -- ** StartInstance
    startInstance_instanceId,

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

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceId,

    -- ** DescribeEcsClusters
    describeEcsClusters_nextToken,
    describeEcsClusters_maxResults,
    describeEcsClusters_stackId,
    describeEcsClusters_ecsClusterArns,
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_instanceId,
    describeVolumes_volumeIds,
    describeVolumes_stackId,
    describeVolumes_raidArrayId,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DescribeOperatingSystems
    describeOperatingSystemsResponse_operatingSystems,
    describeOperatingSystemsResponse_httpStatus,

    -- ** DisassociateElasticIp
    disassociateElasticIp_elasticIp,

    -- ** StartStack
    startStack_stackId,

    -- ** StopStack
    stopStack_stackId,

    -- ** RegisterRdsDbInstance
    registerRdsDbInstance_stackId,
    registerRdsDbInstance_rdsDbInstanceArn,
    registerRdsDbInstance_dbUser,
    registerRdsDbInstance_dbPassword,

    -- ** DescribeServiceErrors
    describeServiceErrors_instanceId,
    describeServiceErrors_stackId,
    describeServiceErrors_serviceErrorIds,
    describeServiceErrorsResponse_serviceErrors,
    describeServiceErrorsResponse_httpStatus,

    -- ** DescribeTimeBasedAutoScaling
    describeTimeBasedAutoScaling_instanceIds,
    describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations,
    describeTimeBasedAutoScalingResponse_httpStatus,

    -- ** UpdateUserProfile
    updateUserProfile_allowSelfManagement,
    updateUserProfile_sshUsername,
    updateUserProfile_sshPublicKey,
    updateUserProfile_iamUserArn,

    -- ** DescribeMyUserProfile
    describeMyUserProfileResponse_userProfile,
    describeMyUserProfileResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DeleteUserProfile
    deleteUserProfile_iamUserArn,

    -- ** AssignInstance
    assignInstance_instanceId,
    assignInstance_layerIds,

    -- ** DetachElasticLoadBalancer
    detachElasticLoadBalancer_elasticLoadBalancerName,
    detachElasticLoadBalancer_layerId,

    -- ** DescribeStackProvisioningParameters
    describeStackProvisioningParameters_stackId,
    describeStackProvisioningParametersResponse_agentInstallerUrl,
    describeStackProvisioningParametersResponse_parameters,
    describeStackProvisioningParametersResponse_httpStatus,

    -- ** DeregisterVolume
    deregisterVolume_volumeId,

    -- ** DescribeStacks
    describeStacks_stackIds,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_deleteVolumes,
    deleteInstance_deleteElasticIp,
    deleteInstance_instanceId,

    -- ** RebootInstance
    rebootInstance_instanceId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UpdateInstance
    updateInstance_hostname,
    updateInstance_installUpdatesOnBoot,
    updateInstance_instanceType,
    updateInstance_ebsOptimized,
    updateInstance_agentVersion,
    updateInstance_sshKeyName,
    updateInstance_amiId,
    updateInstance_layerIds,
    updateInstance_architecture,
    updateInstance_autoScalingType,
    updateInstance_os,
    updateInstance_instanceId,

    -- ** CloneStack
    cloneStack_defaultOs,
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_customCookbooksSource,
    cloneStack_defaultAvailabilityZone,
    cloneStack_agentVersion,
    cloneStack_clonePermissions,
    cloneStack_customJson,
    cloneStack_defaultRootDeviceType,
    cloneStack_attributes,
    cloneStack_name,
    cloneStack_cloneAppIds,
    cloneStack_defaultInstanceProfileArn,
    cloneStack_hostnameTheme,
    cloneStack_defaultSshKeyName,
    cloneStack_configurationManager,
    cloneStack_region,
    cloneStack_vpcId,
    cloneStack_chefConfiguration,
    cloneStack_defaultSubnetId,
    cloneStack_useCustomCookbooks,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,

    -- ** RegisterElasticIp
    registerElasticIp_elasticIp,
    registerElasticIp_stackId,
    registerElasticIpResponse_elasticIp,
    registerElasticIpResponse_httpStatus,

    -- ** DescribeAgentVersions
    describeAgentVersions_stackId,
    describeAgentVersions_configurationManager,
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,

    -- ** UpdateLayer
    updateLayer_installUpdatesOnBoot,
    updateLayer_customInstanceProfileArn,
    updateLayer_customSecurityGroupIds,
    updateLayer_packages,
    updateLayer_enableAutoHealing,
    updateLayer_volumeConfigurations,
    updateLayer_customJson,
    updateLayer_shortname,
    updateLayer_attributes,
    updateLayer_name,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_autoAssignElasticIps,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_customRecipes,
    updateLayer_autoAssignPublicIps,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_layerId,

    -- ** CreateStack
    createStack_defaultOs,
    createStack_useOpsworksSecurityGroups,
    createStack_customCookbooksSource,
    createStack_defaultAvailabilityZone,
    createStack_agentVersion,
    createStack_customJson,
    createStack_defaultRootDeviceType,
    createStack_attributes,
    createStack_hostnameTheme,
    createStack_defaultSshKeyName,
    createStack_configurationManager,
    createStack_vpcId,
    createStack_chefConfiguration,
    createStack_defaultSubnetId,
    createStack_useCustomCookbooks,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** UnassignVolume
    unassignVolume_volumeId,

    -- ** GrantAccess
    grantAccess_validForInMinutes,
    grantAccess_instanceId,
    grantAccessResponse_temporaryCredential,
    grantAccessResponse_httpStatus,

    -- ** DeleteLayer
    deleteLayer_layerId,

    -- ** DescribeApps
    describeApps_appIds,
    describeApps_stackId,
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,

    -- ** DeregisterEcsCluster
    deregisterEcsCluster_ecsClusterArn,

    -- ** DescribeStackSummary
    describeStackSummary_stackId,
    describeStackSummaryResponse_stackSummary,
    describeStackSummaryResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_stackId,

    -- ** SetLoadBasedAutoScaling
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_layerId,

    -- ** CreateLayer
    createLayer_installUpdatesOnBoot,
    createLayer_customInstanceProfileArn,
    createLayer_customSecurityGroupIds,
    createLayer_packages,
    createLayer_enableAutoHealing,
    createLayer_volumeConfigurations,
    createLayer_customJson,
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

    -- ** UpdateStack
    updateStack_defaultOs,
    updateStack_useOpsworksSecurityGroups,
    updateStack_customCookbooksSource,
    updateStack_serviceRoleArn,
    updateStack_defaultAvailabilityZone,
    updateStack_agentVersion,
    updateStack_customJson,
    updateStack_defaultRootDeviceType,
    updateStack_attributes,
    updateStack_name,
    updateStack_defaultInstanceProfileArn,
    updateStack_hostnameTheme,
    updateStack_defaultSshKeyName,
    updateStack_configurationManager,
    updateStack_chefConfiguration,
    updateStack_defaultSubnetId,
    updateStack_useCustomCookbooks,
    updateStack_stackId,

    -- ** DescribeUserProfiles
    describeUserProfiles_iamUserArns,
    describeUserProfilesResponse_userProfiles,
    describeUserProfilesResponse_httpStatus,

    -- ** DescribeElasticLoadBalancers
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,

    -- ** DescribeCommands
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommands_commandIds,
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,

    -- ** UpdateVolume
    updateVolume_name,
    updateVolume_mountPoint,
    updateVolume_volumeId,

    -- ** AssignVolume
    assignVolume_instanceId,
    assignVolume_volumeId,

    -- ** DescribeRaidArrays
    describeRaidArrays_instanceId,
    describeRaidArrays_raidArrayIds,
    describeRaidArrays_stackId,
    describeRaidArraysResponse_raidArrays,
    describeRaidArraysResponse_httpStatus,

    -- ** DeregisterInstance
    deregisterInstance_instanceId,

    -- ** RegisterEcsCluster
    registerEcsCluster_ecsClusterArn,
    registerEcsCluster_stackId,
    registerEcsClusterResponse_ecsClusterArn,
    registerEcsClusterResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_allowSelfManagement,
    createUserProfile_sshUsername,
    createUserProfile_sshPublicKey,
    createUserProfile_iamUserArn,
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,

    -- ** UpdateRdsDbInstance
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- ** UnassignInstance
    unassignInstance_instanceId,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DescribeLoadBasedAutoScaling
    describeLoadBasedAutoScaling_layerIds,
    describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations,
    describeLoadBasedAutoScalingResponse_httpStatus,

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

    -- ** DeleteApp
    deleteApp_appId,

    -- ** UpdateApp
    updateApp_sslConfiguration,
    updateApp_appSource,
    updateApp_dataSources,
    updateApp_domains,
    updateApp_enableSsl,
    updateApp_environment,
    updateApp_attributes,
    updateApp_name,
    updateApp_description,
    updateApp_type,
    updateApp_appId,

    -- ** AssociateElasticIp
    associateElasticIp_instanceId,
    associateElasticIp_elasticIp,

    -- ** UpdateElasticIp
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- ** DescribePermissions
    describePermissions_iamUserArn,
    describePermissions_stackId,
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,

    -- ** GetHostnameSuggestion
    getHostnameSuggestion_layerId,
    getHostnameSuggestionResponse_hostname,
    getHostnameSuggestionResponse_layerId,
    getHostnameSuggestionResponse_httpStatus,

    -- ** CreateInstance
    createInstance_hostname,
    createInstance_virtualizationType,
    createInstance_installUpdatesOnBoot,
    createInstance_ebsOptimized,
    createInstance_rootDeviceType,
    createInstance_agentVersion,
    createInstance_sshKeyName,
    createInstance_amiId,
    createInstance_architecture,
    createInstance_tenancy,
    createInstance_autoScalingType,
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

    -- ** CreateApp
    createApp_sslConfiguration,
    createApp_appSource,
    createApp_dataSources,
    createApp_domains,
    createApp_enableSsl,
    createApp_shortname,
    createApp_environment,
    createApp_attributes,
    createApp_description,
    createApp_stackId,
    createApp_name,
    createApp_type,
    createAppResponse_appId,
    createAppResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_instanceIds,
    createDeployment_appId,
    createDeployment_comment,
    createDeployment_customJson,
    createDeployment_layerIds,
    createDeployment_stackId,
    createDeployment_command,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** DeregisterRdsDbInstance
    deregisterRdsDbInstance_rdsDbInstanceArn,

    -- ** DescribeElasticIps
    describeElasticIps_instanceId,
    describeElasticIps_stackId,
    describeElasticIps_ips,
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,

    -- * Types

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_configurationManager,

    -- ** App
    app_sslConfiguration,
    app_appSource,
    app_appId,
    app_dataSources,
    app_stackId,
    app_domains,
    app_enableSsl,
    app_shortname,
    app_createdAt,
    app_environment,
    app_attributes,
    app_name,
    app_description,
    app_type,

    -- ** AutoScalingThresholds
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_cpuThreshold,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_alarms,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_thresholdsWaitTime,
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
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_batchCount,
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
    deployment_status,
    deployment_deploymentId,
    deployment_appId,
    deployment_iamUserArn,
    deployment_duration,
    deployment_stackId,
    deployment_comment,
    deployment_customJson,
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
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,

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
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_region,
    elasticLoadBalancer_vpcId,

    -- ** EnvironmentVariable
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- ** Instance
    instance_hostname,
    instance_platform,
    instance_securityGroupIds,
    instance_sshHostRsaKeyFingerprint,
    instance_instanceProfileArn,
    instance_virtualizationType,
    instance_privateDns,
    instance_elasticIp,
    instance_status,
    instance_installUpdatesOnBoot,
    instance_instanceId,
    instance_reportedAgentVersion,
    instance_instanceType,
    instance_sshHostDsaKeyFingerprint,
    instance_ebsOptimized,
    instance_rootDeviceType,
    instance_stackId,
    instance_agentVersion,
    instance_rootDeviceVolumeId,
    instance_sshKeyName,
    instance_publicDns,
    instance_amiId,
    instance_arn,
    instance_createdAt,
    instance_layerIds,
    instance_architecture,
    instance_tenancy,
    instance_autoScalingType,
    instance_availabilityZone,
    instance_os,
    instance_privateIp,
    instance_infrastructureClass,
    instance_blockDeviceMappings,
    instance_subnetId,
    instance_ecsContainerInstanceArn,
    instance_registeredBy,
    instance_reportedOs,
    instance_publicIp,
    instance_ec2InstanceId,
    instance_ecsClusterArn,
    instance_lastServiceErrorId,

    -- ** InstanceIdentity
    instanceIdentity_document,
    instanceIdentity_signature,

    -- ** InstancesCount
    instancesCount_online,
    instancesCount_setupFailed,
    instancesCount_registering,
    instancesCount_booting,
    instancesCount_stopFailed,
    instancesCount_startFailed,
    instancesCount_runningSetup,
    instancesCount_terminated,
    instancesCount_pending,
    instancesCount_terminating,
    instancesCount_shuttingDown,
    instancesCount_assigning,
    instancesCount_stopped,
    instancesCount_rebooting,
    instancesCount_registered,
    instancesCount_requested,
    instancesCount_deregistering,
    instancesCount_stopping,
    instancesCount_unassigning,
    instancesCount_connectionLost,

    -- ** Layer
    layer_installUpdatesOnBoot,
    layer_customInstanceProfileArn,
    layer_customSecurityGroupIds,
    layer_packages,
    layer_enableAutoHealing,
    layer_volumeConfigurations,
    layer_stackId,
    layer_customJson,
    layer_defaultRecipes,
    layer_arn,
    layer_shortname,
    layer_createdAt,
    layer_attributes,
    layer_name,
    layer_cloudWatchLogsConfiguration,
    layer_autoAssignElasticIps,
    layer_layerId,
    layer_defaultSecurityGroupNames,
    layer_type,
    layer_useEbsOptimizedInstances,
    layer_customRecipes,
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
    operatingSystem_id,
    operatingSystem_reportedVersion,
    operatingSystem_name,
    operatingSystem_type,
    operatingSystem_reportedName,

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
    raidArray_numberOfDisks,
    raidArray_instanceId,
    raidArray_stackId,
    raidArray_device,
    raidArray_createdAt,
    raidArray_raidArrayId,
    raidArray_availabilityZone,
    raidArray_name,
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
    recipes_shutdown,
    recipes_configure,
    recipes_undeploy,
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
    stack_defaultOs,
    stack_useOpsworksSecurityGroups,
    stack_customCookbooksSource,
    stack_serviceRoleArn,
    stack_defaultAvailabilityZone,
    stack_stackId,
    stack_agentVersion,
    stack_customJson,
    stack_arn,
    stack_createdAt,
    stack_defaultRootDeviceType,
    stack_attributes,
    stack_name,
    stack_defaultInstanceProfileArn,
    stack_hostnameTheme,
    stack_defaultSshKeyName,
    stack_configurationManager,
    stack_region,
    stack_vpcId,
    stack_chefConfiguration,
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
    stackSummary_instancesCount,
    stackSummary_appsCount,

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
    userProfile_allowSelfManagement,
    userProfile_sshUsername,
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
    volume_availabilityZone,
    volume_name,
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
