{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Lens
  ( -- * Operations

    -- ** AssignInstance
    assignInstance_instanceId,
    assignInstance_layerIds,

    -- ** AssignVolume
    assignVolume_instanceId,
    assignVolume_volumeId,

    -- ** AssociateElasticIp
    associateElasticIp_instanceId,
    associateElasticIp_elasticIp,

    -- ** AttachElasticLoadBalancer
    attachElasticLoadBalancer_elasticLoadBalancerName,
    attachElasticLoadBalancer_layerId,

    -- ** CloneStack
    cloneStack_agentVersion,
    cloneStack_attributes,
    cloneStack_chefConfiguration,
    cloneStack_cloneAppIds,
    cloneStack_clonePermissions,
    cloneStack_configurationManager,
    cloneStack_customCookbooksSource,
    cloneStack_customJson,
    cloneStack_defaultAvailabilityZone,
    cloneStack_defaultInstanceProfileArn,
    cloneStack_defaultOs,
    cloneStack_defaultRootDeviceType,
    cloneStack_defaultSshKeyName,
    cloneStack_defaultSubnetId,
    cloneStack_hostnameTheme,
    cloneStack_name,
    cloneStack_region,
    cloneStack_useCustomCookbooks,
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_vpcId,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,

    -- ** CreateApp
    createApp_appSource,
    createApp_attributes,
    createApp_dataSources,
    createApp_description,
    createApp_domains,
    createApp_enableSsl,
    createApp_environment,
    createApp_shortname,
    createApp_sslConfiguration,
    createApp_stackId,
    createApp_name,
    createApp_type,
    createAppResponse_appId,
    createAppResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_appId,
    createDeployment_comment,
    createDeployment_customJson,
    createDeployment_instanceIds,
    createDeployment_layerIds,
    createDeployment_stackId,
    createDeployment_command,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateInstance
    createInstance_agentVersion,
    createInstance_amiId,
    createInstance_architecture,
    createInstance_autoScalingType,
    createInstance_availabilityZone,
    createInstance_blockDeviceMappings,
    createInstance_ebsOptimized,
    createInstance_hostname,
    createInstance_installUpdatesOnBoot,
    createInstance_os,
    createInstance_rootDeviceType,
    createInstance_sshKeyName,
    createInstance_subnetId,
    createInstance_tenancy,
    createInstance_virtualizationType,
    createInstance_stackId,
    createInstance_layerIds,
    createInstance_instanceType,
    createInstanceResponse_instanceId,
    createInstanceResponse_httpStatus,

    -- ** CreateLayer
    createLayer_attributes,
    createLayer_autoAssignElasticIps,
    createLayer_autoAssignPublicIps,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_customInstanceProfileArn,
    createLayer_customJson,
    createLayer_customRecipes,
    createLayer_customSecurityGroupIds,
    createLayer_enableAutoHealing,
    createLayer_installUpdatesOnBoot,
    createLayer_lifecycleEventConfiguration,
    createLayer_packages,
    createLayer_useEbsOptimizedInstances,
    createLayer_volumeConfigurations,
    createLayer_stackId,
    createLayer_type,
    createLayer_name,
    createLayer_shortname,
    createLayerResponse_layerId,
    createLayerResponse_httpStatus,

    -- ** CreateStack
    createStack_agentVersion,
    createStack_attributes,
    createStack_chefConfiguration,
    createStack_configurationManager,
    createStack_customCookbooksSource,
    createStack_customJson,
    createStack_defaultAvailabilityZone,
    createStack_defaultOs,
    createStack_defaultRootDeviceType,
    createStack_defaultSshKeyName,
    createStack_defaultSubnetId,
    createStack_hostnameTheme,
    createStack_useCustomCookbooks,
    createStack_useOpsworksSecurityGroups,
    createStack_vpcId,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_allowSelfManagement,
    createUserProfile_sshPublicKey,
    createUserProfile_sshUsername,
    createUserProfile_iamUserArn,
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_appId,

    -- ** DeleteInstance
    deleteInstance_deleteElasticIp,
    deleteInstance_deleteVolumes,
    deleteInstance_instanceId,

    -- ** DeleteLayer
    deleteLayer_layerId,

    -- ** DeleteStack
    deleteStack_stackId,

    -- ** DeleteUserProfile
    deleteUserProfile_iamUserArn,

    -- ** DeregisterEcsCluster
    deregisterEcsCluster_ecsClusterArn,

    -- ** DeregisterElasticIp
    deregisterElasticIp_elasticIp,

    -- ** DeregisterInstance
    deregisterInstance_instanceId,

    -- ** DeregisterRdsDbInstance
    deregisterRdsDbInstance_rdsDbInstanceArn,

    -- ** DeregisterVolume
    deregisterVolume_volumeId,

    -- ** DescribeAgentVersions
    describeAgentVersions_configurationManager,
    describeAgentVersions_stackId,
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,

    -- ** DescribeApps
    describeApps_appIds,
    describeApps_stackId,
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,

    -- ** DescribeCommands
    describeCommands_commandIds,
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,

    -- ** DescribeDeployments
    describeDeployments_appId,
    describeDeployments_deploymentIds,
    describeDeployments_stackId,
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,

    -- ** DescribeEcsClusters
    describeEcsClusters_ecsClusterArns,
    describeEcsClusters_maxResults,
    describeEcsClusters_nextToken,
    describeEcsClusters_stackId,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_httpStatus,

    -- ** DescribeElasticIps
    describeElasticIps_instanceId,
    describeElasticIps_ips,
    describeElasticIps_stackId,
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,

    -- ** DescribeElasticLoadBalancers
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_instanceIds,
    describeInstances_layerId,
    describeInstances_stackId,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DescribeLayers
    describeLayers_layerIds,
    describeLayers_stackId,
    describeLayersResponse_layers,
    describeLayersResponse_httpStatus,

    -- ** DescribeLoadBasedAutoScaling
    describeLoadBasedAutoScaling_layerIds,
    describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations,
    describeLoadBasedAutoScalingResponse_httpStatus,

    -- ** DescribeMyUserProfile
    describeMyUserProfileResponse_userProfile,
    describeMyUserProfileResponse_httpStatus,

    -- ** DescribeOperatingSystems
    describeOperatingSystemsResponse_operatingSystems,
    describeOperatingSystemsResponse_httpStatus,

    -- ** DescribePermissions
    describePermissions_iamUserArn,
    describePermissions_stackId,
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,

    -- ** DescribeRaidArrays
    describeRaidArrays_instanceId,
    describeRaidArrays_raidArrayIds,
    describeRaidArrays_stackId,
    describeRaidArraysResponse_raidArrays,
    describeRaidArraysResponse_httpStatus,

    -- ** DescribeRdsDbInstances
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,

    -- ** DescribeServiceErrors
    describeServiceErrors_instanceId,
    describeServiceErrors_serviceErrorIds,
    describeServiceErrors_stackId,
    describeServiceErrorsResponse_serviceErrors,
    describeServiceErrorsResponse_httpStatus,

    -- ** DescribeStackProvisioningParameters
    describeStackProvisioningParameters_stackId,
    describeStackProvisioningParametersResponse_agentInstallerUrl,
    describeStackProvisioningParametersResponse_parameters,
    describeStackProvisioningParametersResponse_httpStatus,

    -- ** DescribeStackSummary
    describeStackSummary_stackId,
    describeStackSummaryResponse_stackSummary,
    describeStackSummaryResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_stackIds,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DescribeTimeBasedAutoScaling
    describeTimeBasedAutoScaling_instanceIds,
    describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations,
    describeTimeBasedAutoScalingResponse_httpStatus,

    -- ** DescribeUserProfiles
    describeUserProfiles_iamUserArns,
    describeUserProfilesResponse_userProfiles,
    describeUserProfilesResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_instanceId,
    describeVolumes_raidArrayId,
    describeVolumes_stackId,
    describeVolumes_volumeIds,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DetachElasticLoadBalancer
    detachElasticLoadBalancer_elasticLoadBalancerName,
    detachElasticLoadBalancer_layerId,

    -- ** DisassociateElasticIp
    disassociateElasticIp_elasticIp,

    -- ** GetHostnameSuggestion
    getHostnameSuggestion_layerId,
    getHostnameSuggestionResponse_hostname,
    getHostnameSuggestionResponse_layerId,
    getHostnameSuggestionResponse_httpStatus,

    -- ** GrantAccess
    grantAccess_validForInMinutes,
    grantAccess_instanceId,
    grantAccessResponse_temporaryCredential,
    grantAccessResponse_httpStatus,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** RebootInstance
    rebootInstance_instanceId,

    -- ** RegisterEcsCluster
    registerEcsCluster_ecsClusterArn,
    registerEcsCluster_stackId,
    registerEcsClusterResponse_ecsClusterArn,
    registerEcsClusterResponse_httpStatus,

    -- ** RegisterElasticIp
    registerElasticIp_elasticIp,
    registerElasticIp_stackId,
    registerElasticIpResponse_elasticIp,
    registerElasticIpResponse_httpStatus,

    -- ** RegisterInstance
    registerInstance_hostname,
    registerInstance_instanceIdentity,
    registerInstance_privateIp,
    registerInstance_publicIp,
    registerInstance_rsaPublicKey,
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_stackId,
    registerInstanceResponse_instanceId,
    registerInstanceResponse_httpStatus,

    -- ** RegisterRdsDbInstance
    registerRdsDbInstance_stackId,
    registerRdsDbInstance_rdsDbInstanceArn,
    registerRdsDbInstance_dbUser,
    registerRdsDbInstance_dbPassword,

    -- ** RegisterVolume
    registerVolume_ec2VolumeId,
    registerVolume_stackId,
    registerVolumeResponse_volumeId,
    registerVolumeResponse_httpStatus,

    -- ** SetLoadBasedAutoScaling
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_layerId,

    -- ** SetPermission
    setPermission_allowSsh,
    setPermission_allowSudo,
    setPermission_level,
    setPermission_stackId,
    setPermission_iamUserArn,

    -- ** SetTimeBasedAutoScaling
    setTimeBasedAutoScaling_autoScalingSchedule,
    setTimeBasedAutoScaling_instanceId,

    -- ** StartInstance
    startInstance_instanceId,

    -- ** StartStack
    startStack_stackId,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceId,

    -- ** StopStack
    stopStack_stackId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UnassignInstance
    unassignInstance_instanceId,

    -- ** UnassignVolume
    unassignVolume_volumeId,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApp
    updateApp_appSource,
    updateApp_attributes,
    updateApp_dataSources,
    updateApp_description,
    updateApp_domains,
    updateApp_enableSsl,
    updateApp_environment,
    updateApp_name,
    updateApp_sslConfiguration,
    updateApp_type,
    updateApp_appId,

    -- ** UpdateElasticIp
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- ** UpdateInstance
    updateInstance_agentVersion,
    updateInstance_amiId,
    updateInstance_architecture,
    updateInstance_autoScalingType,
    updateInstance_ebsOptimized,
    updateInstance_hostname,
    updateInstance_installUpdatesOnBoot,
    updateInstance_instanceType,
    updateInstance_layerIds,
    updateInstance_os,
    updateInstance_sshKeyName,
    updateInstance_instanceId,

    -- ** UpdateLayer
    updateLayer_attributes,
    updateLayer_autoAssignElasticIps,
    updateLayer_autoAssignPublicIps,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_customInstanceProfileArn,
    updateLayer_customJson,
    updateLayer_customRecipes,
    updateLayer_customSecurityGroupIds,
    updateLayer_enableAutoHealing,
    updateLayer_installUpdatesOnBoot,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_name,
    updateLayer_packages,
    updateLayer_shortname,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_volumeConfigurations,
    updateLayer_layerId,

    -- ** UpdateMyUserProfile
    updateMyUserProfile_sshPublicKey,

    -- ** UpdateRdsDbInstance
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- ** UpdateStack
    updateStack_agentVersion,
    updateStack_attributes,
    updateStack_chefConfiguration,
    updateStack_configurationManager,
    updateStack_customCookbooksSource,
    updateStack_customJson,
    updateStack_defaultAvailabilityZone,
    updateStack_defaultInstanceProfileArn,
    updateStack_defaultOs,
    updateStack_defaultRootDeviceType,
    updateStack_defaultSshKeyName,
    updateStack_defaultSubnetId,
    updateStack_hostnameTheme,
    updateStack_name,
    updateStack_serviceRoleArn,
    updateStack_useCustomCookbooks,
    updateStack_useOpsworksSecurityGroups,
    updateStack_stackId,

    -- ** UpdateUserProfile
    updateUserProfile_allowSelfManagement,
    updateUserProfile_sshPublicKey,
    updateUserProfile_sshUsername,
    updateUserProfile_iamUserArn,

    -- ** UpdateVolume
    updateVolume_mountPoint,
    updateVolume_name,
    updateVolume_volumeId,

    -- * Types

    -- ** AgentVersion
    agentVersion_configurationManager,
    agentVersion_version,

    -- ** App
    app_appId,
    app_appSource,
    app_attributes,
    app_createdAt,
    app_dataSources,
    app_description,
    app_domains,
    app_enableSsl,
    app_environment,
    app_name,
    app_shortname,
    app_sslConfiguration,
    app_stackId,
    app_type,

    -- ** AutoScalingThresholds
    autoScalingThresholds_alarms,
    autoScalingThresholds_cpuThreshold,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_thresholdsWaitTime,

    -- ** BlockDeviceMapping
    blockDeviceMapping_deviceName,
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,

    -- ** ChefConfiguration
    chefConfiguration_berkshelfVersion,
    chefConfiguration_manageBerkshelf,

    -- ** CloudWatchLogsConfiguration
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- ** CloudWatchLogsLogStream
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_timeZone,

    -- ** Command
    command_acknowledgedAt,
    command_commandId,
    command_completedAt,
    command_createdAt,
    command_deploymentId,
    command_exitCode,
    command_instanceId,
    command_logUrl,
    command_status,
    command_type,

    -- ** DataSource
    dataSource_arn,
    dataSource_databaseName,
    dataSource_type,

    -- ** Deployment
    deployment_appId,
    deployment_command,
    deployment_comment,
    deployment_completedAt,
    deployment_createdAt,
    deployment_customJson,
    deployment_deploymentId,
    deployment_duration,
    deployment_iamUserArn,
    deployment_instanceIds,
    deployment_stackId,
    deployment_status,

    -- ** DeploymentCommand
    deploymentCommand_args,
    deploymentCommand_name,

    -- ** EbsBlockDevice
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_iops,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_volumeType,

    -- ** EcsCluster
    ecsCluster_ecsClusterArn,
    ecsCluster_ecsClusterName,
    ecsCluster_registeredAt,
    ecsCluster_stackId,

    -- ** ElasticIp
    elasticIp_domain,
    elasticIp_instanceId,
    elasticIp_ip,
    elasticIp_name,
    elasticIp_region,

    -- ** ElasticLoadBalancer
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_region,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_subnetIds,
    elasticLoadBalancer_vpcId,

    -- ** EnvironmentVariable
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- ** Instance
    instance_agentVersion,
    instance_amiId,
    instance_architecture,
    instance_arn,
    instance_autoScalingType,
    instance_availabilityZone,
    instance_blockDeviceMappings,
    instance_createdAt,
    instance_ebsOptimized,
    instance_ec2InstanceId,
    instance_ecsClusterArn,
    instance_ecsContainerInstanceArn,
    instance_elasticIp,
    instance_hostname,
    instance_infrastructureClass,
    instance_installUpdatesOnBoot,
    instance_instanceId,
    instance_instanceProfileArn,
    instance_instanceType,
    instance_lastServiceErrorId,
    instance_layerIds,
    instance_os,
    instance_platform,
    instance_privateDns,
    instance_privateIp,
    instance_publicDns,
    instance_publicIp,
    instance_registeredBy,
    instance_reportedAgentVersion,
    instance_reportedOs,
    instance_rootDeviceType,
    instance_rootDeviceVolumeId,
    instance_securityGroupIds,
    instance_sshHostDsaKeyFingerprint,
    instance_sshHostRsaKeyFingerprint,
    instance_sshKeyName,
    instance_stackId,
    instance_status,
    instance_subnetId,
    instance_tenancy,
    instance_virtualizationType,

    -- ** InstanceIdentity
    instanceIdentity_document,
    instanceIdentity_signature,

    -- ** InstancesCount
    instancesCount_assigning,
    instancesCount_booting,
    instancesCount_connectionLost,
    instancesCount_deregistering,
    instancesCount_online,
    instancesCount_pending,
    instancesCount_rebooting,
    instancesCount_registered,
    instancesCount_registering,
    instancesCount_requested,
    instancesCount_runningSetup,
    instancesCount_setupFailed,
    instancesCount_shuttingDown,
    instancesCount_startFailed,
    instancesCount_stopFailed,
    instancesCount_stopped,
    instancesCount_stopping,
    instancesCount_terminated,
    instancesCount_terminating,
    instancesCount_unassigning,

    -- ** Layer
    layer_arn,
    layer_attributes,
    layer_autoAssignElasticIps,
    layer_autoAssignPublicIps,
    layer_cloudWatchLogsConfiguration,
    layer_createdAt,
    layer_customInstanceProfileArn,
    layer_customJson,
    layer_customRecipes,
    layer_customSecurityGroupIds,
    layer_defaultRecipes,
    layer_defaultSecurityGroupNames,
    layer_enableAutoHealing,
    layer_installUpdatesOnBoot,
    layer_layerId,
    layer_lifecycleEventConfiguration,
    layer_name,
    layer_packages,
    layer_shortname,
    layer_stackId,
    layer_type,
    layer_useEbsOptimizedInstances,
    layer_volumeConfigurations,

    -- ** LifecycleEventConfiguration
    lifecycleEventConfiguration_shutdown,

    -- ** LoadBasedAutoScalingConfiguration
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_layerId,
    loadBasedAutoScalingConfiguration_upScaling,

    -- ** OperatingSystem
    operatingSystem_configurationManagers,
    operatingSystem_id,
    operatingSystem_name,
    operatingSystem_reportedName,
    operatingSystem_reportedVersion,
    operatingSystem_supported,
    operatingSystem_type,

    -- ** OperatingSystemConfigurationManager
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- ** Permission
    permission_allowSsh,
    permission_allowSudo,
    permission_iamUserArn,
    permission_level,
    permission_stackId,

    -- ** RaidArray
    raidArray_availabilityZone,
    raidArray_createdAt,
    raidArray_device,
    raidArray_instanceId,
    raidArray_iops,
    raidArray_mountPoint,
    raidArray_name,
    raidArray_numberOfDisks,
    raidArray_raidArrayId,
    raidArray_raidLevel,
    raidArray_size,
    raidArray_stackId,
    raidArray_volumeType,

    -- ** RdsDbInstance
    rdsDbInstance_address,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_dbUser,
    rdsDbInstance_engine,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_region,
    rdsDbInstance_stackId,

    -- ** Recipes
    recipes_configure,
    recipes_deploy,
    recipes_setup,
    recipes_shutdown,
    recipes_undeploy,

    -- ** ReportedOs
    reportedOs_family,
    reportedOs_name,
    reportedOs_version,

    -- ** SelfUserProfile
    selfUserProfile_iamUserArn,
    selfUserProfile_name,
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,

    -- ** ServiceError
    serviceError_createdAt,
    serviceError_instanceId,
    serviceError_message,
    serviceError_serviceErrorId,
    serviceError_stackId,
    serviceError_type,

    -- ** ShutdownEventConfiguration
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,
    shutdownEventConfiguration_executionTimeout,

    -- ** Source
    source_password,
    source_revision,
    source_sshKey,
    source_type,
    source_url,
    source_username,

    -- ** SslConfiguration
    sslConfiguration_certificate,
    sslConfiguration_chain,
    sslConfiguration_privateKey,

    -- ** Stack
    stack_agentVersion,
    stack_arn,
    stack_attributes,
    stack_chefConfiguration,
    stack_configurationManager,
    stack_createdAt,
    stack_customCookbooksSource,
    stack_customJson,
    stack_defaultAvailabilityZone,
    stack_defaultInstanceProfileArn,
    stack_defaultOs,
    stack_defaultRootDeviceType,
    stack_defaultSshKeyName,
    stack_defaultSubnetId,
    stack_hostnameTheme,
    stack_name,
    stack_region,
    stack_serviceRoleArn,
    stack_stackId,
    stack_useCustomCookbooks,
    stack_useOpsworksSecurityGroups,
    stack_vpcId,

    -- ** StackConfigurationManager
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- ** StackSummary
    stackSummary_appsCount,
    stackSummary_arn,
    stackSummary_instancesCount,
    stackSummary_layersCount,
    stackSummary_name,
    stackSummary_stackId,

    -- ** TemporaryCredential
    temporaryCredential_instanceId,
    temporaryCredential_password,
    temporaryCredential_username,
    temporaryCredential_validForInMinutes,

    -- ** TimeBasedAutoScalingConfiguration
    timeBasedAutoScalingConfiguration_autoScalingSchedule,
    timeBasedAutoScalingConfiguration_instanceId,

    -- ** UserProfile
    userProfile_allowSelfManagement,
    userProfile_iamUserArn,
    userProfile_name,
    userProfile_sshPublicKey,
    userProfile_sshUsername,

    -- ** Volume
    volume_availabilityZone,
    volume_device,
    volume_ec2VolumeId,
    volume_encrypted,
    volume_instanceId,
    volume_iops,
    volume_mountPoint,
    volume_name,
    volume_raidArrayId,
    volume_region,
    volume_size,
    volume_status,
    volume_volumeId,
    volume_volumeType,

    -- ** VolumeConfiguration
    volumeConfiguration_encrypted,
    volumeConfiguration_iops,
    volumeConfiguration_raidLevel,
    volumeConfiguration_volumeType,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- ** WeeklyAutoScalingSchedule
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_wednesday,
  )
where

import Amazonka.OpsWorks.AssignInstance
import Amazonka.OpsWorks.AssignVolume
import Amazonka.OpsWorks.AssociateElasticIp
import Amazonka.OpsWorks.AttachElasticLoadBalancer
import Amazonka.OpsWorks.CloneStack
import Amazonka.OpsWorks.CreateApp
import Amazonka.OpsWorks.CreateDeployment
import Amazonka.OpsWorks.CreateInstance
import Amazonka.OpsWorks.CreateLayer
import Amazonka.OpsWorks.CreateStack
import Amazonka.OpsWorks.CreateUserProfile
import Amazonka.OpsWorks.DeleteApp
import Amazonka.OpsWorks.DeleteInstance
import Amazonka.OpsWorks.DeleteLayer
import Amazonka.OpsWorks.DeleteStack
import Amazonka.OpsWorks.DeleteUserProfile
import Amazonka.OpsWorks.DeregisterEcsCluster
import Amazonka.OpsWorks.DeregisterElasticIp
import Amazonka.OpsWorks.DeregisterInstance
import Amazonka.OpsWorks.DeregisterRdsDbInstance
import Amazonka.OpsWorks.DeregisterVolume
import Amazonka.OpsWorks.DescribeAgentVersions
import Amazonka.OpsWorks.DescribeApps
import Amazonka.OpsWorks.DescribeCommands
import Amazonka.OpsWorks.DescribeDeployments
import Amazonka.OpsWorks.DescribeEcsClusters
import Amazonka.OpsWorks.DescribeElasticIps
import Amazonka.OpsWorks.DescribeElasticLoadBalancers
import Amazonka.OpsWorks.DescribeInstances
import Amazonka.OpsWorks.DescribeLayers
import Amazonka.OpsWorks.DescribeLoadBasedAutoScaling
import Amazonka.OpsWorks.DescribeMyUserProfile
import Amazonka.OpsWorks.DescribeOperatingSystems
import Amazonka.OpsWorks.DescribePermissions
import Amazonka.OpsWorks.DescribeRaidArrays
import Amazonka.OpsWorks.DescribeRdsDbInstances
import Amazonka.OpsWorks.DescribeServiceErrors
import Amazonka.OpsWorks.DescribeStackProvisioningParameters
import Amazonka.OpsWorks.DescribeStackSummary
import Amazonka.OpsWorks.DescribeStacks
import Amazonka.OpsWorks.DescribeTimeBasedAutoScaling
import Amazonka.OpsWorks.DescribeUserProfiles
import Amazonka.OpsWorks.DescribeVolumes
import Amazonka.OpsWorks.DetachElasticLoadBalancer
import Amazonka.OpsWorks.DisassociateElasticIp
import Amazonka.OpsWorks.GetHostnameSuggestion
import Amazonka.OpsWorks.GrantAccess
import Amazonka.OpsWorks.ListTags
import Amazonka.OpsWorks.RebootInstance
import Amazonka.OpsWorks.RegisterEcsCluster
import Amazonka.OpsWorks.RegisterElasticIp
import Amazonka.OpsWorks.RegisterInstance
import Amazonka.OpsWorks.RegisterRdsDbInstance
import Amazonka.OpsWorks.RegisterVolume
import Amazonka.OpsWorks.SetLoadBasedAutoScaling
import Amazonka.OpsWorks.SetPermission
import Amazonka.OpsWorks.SetTimeBasedAutoScaling
import Amazonka.OpsWorks.StartInstance
import Amazonka.OpsWorks.StartStack
import Amazonka.OpsWorks.StopInstance
import Amazonka.OpsWorks.StopStack
import Amazonka.OpsWorks.TagResource
import Amazonka.OpsWorks.Types.AgentVersion
import Amazonka.OpsWorks.Types.App
import Amazonka.OpsWorks.Types.AutoScalingThresholds
import Amazonka.OpsWorks.Types.BlockDeviceMapping
import Amazonka.OpsWorks.Types.ChefConfiguration
import Amazonka.OpsWorks.Types.CloudWatchLogsConfiguration
import Amazonka.OpsWorks.Types.CloudWatchLogsLogStream
import Amazonka.OpsWorks.Types.Command
import Amazonka.OpsWorks.Types.DataSource
import Amazonka.OpsWorks.Types.Deployment
import Amazonka.OpsWorks.Types.DeploymentCommand
import Amazonka.OpsWorks.Types.EbsBlockDevice
import Amazonka.OpsWorks.Types.EcsCluster
import Amazonka.OpsWorks.Types.ElasticIp
import Amazonka.OpsWorks.Types.ElasticLoadBalancer
import Amazonka.OpsWorks.Types.EnvironmentVariable
import Amazonka.OpsWorks.Types.Instance
import Amazonka.OpsWorks.Types.InstanceIdentity
import Amazonka.OpsWorks.Types.InstancesCount
import Amazonka.OpsWorks.Types.Layer
import Amazonka.OpsWorks.Types.LifecycleEventConfiguration
import Amazonka.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Amazonka.OpsWorks.Types.OperatingSystem
import Amazonka.OpsWorks.Types.OperatingSystemConfigurationManager
import Amazonka.OpsWorks.Types.Permission
import Amazonka.OpsWorks.Types.RaidArray
import Amazonka.OpsWorks.Types.RdsDbInstance
import Amazonka.OpsWorks.Types.Recipes
import Amazonka.OpsWorks.Types.ReportedOs
import Amazonka.OpsWorks.Types.SelfUserProfile
import Amazonka.OpsWorks.Types.ServiceError
import Amazonka.OpsWorks.Types.ShutdownEventConfiguration
import Amazonka.OpsWorks.Types.Source
import Amazonka.OpsWorks.Types.SslConfiguration
import Amazonka.OpsWorks.Types.Stack
import Amazonka.OpsWorks.Types.StackConfigurationManager
import Amazonka.OpsWorks.Types.StackSummary
import Amazonka.OpsWorks.Types.TemporaryCredential
import Amazonka.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Amazonka.OpsWorks.Types.UserProfile
import Amazonka.OpsWorks.Types.Volume
import Amazonka.OpsWorks.Types.VolumeConfiguration
import Amazonka.OpsWorks.Types.WeeklyAutoScalingSchedule
import Amazonka.OpsWorks.UnassignInstance
import Amazonka.OpsWorks.UnassignVolume
import Amazonka.OpsWorks.UntagResource
import Amazonka.OpsWorks.UpdateApp
import Amazonka.OpsWorks.UpdateElasticIp
import Amazonka.OpsWorks.UpdateInstance
import Amazonka.OpsWorks.UpdateLayer
import Amazonka.OpsWorks.UpdateMyUserProfile
import Amazonka.OpsWorks.UpdateRdsDbInstance
import Amazonka.OpsWorks.UpdateStack
import Amazonka.OpsWorks.UpdateUserProfile
import Amazonka.OpsWorks.UpdateVolume
