{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Lens
  ( -- * Operations

    -- ** DescribeRdsDbInstances
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_stackId,

    -- ** UpdateStack
    updateStack_defaultInstanceProfileArn,
    updateStack_serviceRoleArn,
    updateStack_defaultRootDeviceType,
    updateStack_chefConfiguration,
    updateStack_agentVersion,
    updateStack_defaultSshKeyName,
    updateStack_customJson,
    updateStack_customCookbooksSource,
    updateStack_defaultAvailabilityZone,
    updateStack_attributes,
    updateStack_name,
    updateStack_defaultOs,
    updateStack_useOpsworksSecurityGroups,
    updateStack_useCustomCookbooks,
    updateStack_defaultSubnetId,
    updateStack_configurationManager,
    updateStack_hostnameTheme,
    updateStack_stackId,

    -- ** CreateLayer
    createLayer_customInstanceProfileArn,
    createLayer_customSecurityGroupIds,
    createLayer_installUpdatesOnBoot,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_lifecycleEventConfiguration,
    createLayer_customRecipes,
    createLayer_customJson,
    createLayer_volumeConfigurations,
    createLayer_enableAutoHealing,
    createLayer_packages,
    createLayer_attributes,
    createLayer_autoAssignPublicIps,
    createLayer_useEbsOptimizedInstances,
    createLayer_autoAssignElasticIps,
    createLayer_stackId,
    createLayer_type,
    createLayer_name,
    createLayer_shortname,
    createLayerResponse_layerId,
    createLayerResponse_httpStatus,

    -- ** SetLoadBasedAutoScaling
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_layerId,

    -- ** DeregisterRdsDbInstance
    deregisterRdsDbInstance_rdsDbInstanceArn,

    -- ** UnassignVolume
    unassignVolume_volumeId,

    -- ** CreateInstance
    createInstance_installUpdatesOnBoot,
    createInstance_virtualizationType,
    createInstance_hostname,
    createInstance_sshKeyName,
    createInstance_agentVersion,
    createInstance_subnetId,
    createInstance_ebsOptimized,
    createInstance_os,
    createInstance_availabilityZone,
    createInstance_tenancy,
    createInstance_autoScalingType,
    createInstance_architecture,
    createInstance_amiId,
    createInstance_rootDeviceType,
    createInstance_blockDeviceMappings,
    createInstance_stackId,
    createInstance_layerIds,
    createInstance_instanceType,
    createInstanceResponse_instanceId,
    createInstanceResponse_httpStatus,

    -- ** DescribeLayers
    describeLayers_layerIds,
    describeLayers_stackId,
    describeLayersResponse_layers,
    describeLayersResponse_httpStatus,

    -- ** RegisterElasticIp
    registerElasticIp_elasticIp,
    registerElasticIp_stackId,
    registerElasticIpResponse_elasticIp,
    registerElasticIpResponse_httpStatus,

    -- ** DescribeAgentVersions
    describeAgentVersions_configurationManager,
    describeAgentVersions_stackId,
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_customJson,
    createDeployment_appId,
    createDeployment_instanceIds,
    createDeployment_layerIds,
    createDeployment_comment,
    createDeployment_stackId,
    createDeployment_command,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** AssignInstance
    assignInstance_instanceId,
    assignInstance_layerIds,

    -- ** DescribeStacks
    describeStacks_stackIds,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_deleteVolumes,
    deleteInstance_deleteElasticIp,
    deleteInstance_instanceId,

    -- ** UpdateInstance
    updateInstance_installUpdatesOnBoot,
    updateInstance_hostname,
    updateInstance_sshKeyName,
    updateInstance_agentVersion,
    updateInstance_instanceType,
    updateInstance_ebsOptimized,
    updateInstance_os,
    updateInstance_autoScalingType,
    updateInstance_layerIds,
    updateInstance_architecture,
    updateInstance_amiId,
    updateInstance_instanceId,

    -- ** DeregisterVolume
    deregisterVolume_volumeId,

    -- ** RebootInstance
    rebootInstance_instanceId,

    -- ** DeleteApp
    deleteApp_appId,

    -- ** UpdateApp
    updateApp_sslConfiguration,
    updateApp_environment,
    updateApp_enableSsl,
    updateApp_dataSources,
    updateApp_appSource,
    updateApp_attributes,
    updateApp_name,
    updateApp_type,
    updateApp_domains,
    updateApp_description,
    updateApp_appId,

    -- ** UpdateRdsDbInstance
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- ** DescribeTimeBasedAutoScaling
    describeTimeBasedAutoScaling_instanceIds,
    describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations,
    describeTimeBasedAutoScalingResponse_httpStatus,

    -- ** StopStack
    stopStack_stackId,

    -- ** DescribeVolumes
    describeVolumes_instanceId,
    describeVolumes_volumeIds,
    describeVolumes_raidArrayId,
    describeVolumes_stackId,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DisassociateElasticIp
    disassociateElasticIp_elasticIp,

    -- ** RegisterEcsCluster
    registerEcsCluster_ecsClusterArn,
    registerEcsCluster_stackId,
    registerEcsClusterResponse_ecsClusterArn,
    registerEcsClusterResponse_httpStatus,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceId,

    -- ** RegisterVolume
    registerVolume_ec2VolumeId,
    registerVolume_stackId,
    registerVolumeResponse_volumeId,
    registerVolumeResponse_httpStatus,

    -- ** SetTimeBasedAutoScaling
    setTimeBasedAutoScaling_autoScalingSchedule,
    setTimeBasedAutoScaling_instanceId,

    -- ** DescribeUserProfiles
    describeUserProfiles_iamUserArns,
    describeUserProfilesResponse_userProfiles,
    describeUserProfilesResponse_httpStatus,

    -- ** AttachElasticLoadBalancer
    attachElasticLoadBalancer_elasticLoadBalancerName,
    attachElasticLoadBalancer_layerId,

    -- ** DeregisterElasticIp
    deregisterElasticIp_elasticIp,

    -- ** DeregisterEcsCluster
    deregisterEcsCluster_ecsClusterArn,

    -- ** DescribeApps
    describeApps_appIds,
    describeApps_stackId,
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,

    -- ** UpdateMyUserProfile
    updateMyUserProfile_sshPublicKey,

    -- ** DescribeStackSummary
    describeStackSummary_stackId,
    describeStackSummaryResponse_stackSummary,
    describeStackSummaryResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_instanceIds,
    describeInstances_stackId,
    describeInstances_layerId,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DescribeDeployments
    describeDeployments_appId,
    describeDeployments_deploymentIds,
    describeDeployments_stackId,
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,

    -- ** DescribeElasticIps
    describeElasticIps_instanceId,
    describeElasticIps_ips,
    describeElasticIps_stackId,
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,

    -- ** GrantAccess
    grantAccess_validForInMinutes,
    grantAccess_instanceId,
    grantAccessResponse_temporaryCredential,
    grantAccessResponse_httpStatus,

    -- ** DeleteLayer
    deleteLayer_layerId,

    -- ** UpdateLayer
    updateLayer_customInstanceProfileArn,
    updateLayer_customSecurityGroupIds,
    updateLayer_installUpdatesOnBoot,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_shortname,
    updateLayer_customRecipes,
    updateLayer_customJson,
    updateLayer_volumeConfigurations,
    updateLayer_enableAutoHealing,
    updateLayer_packages,
    updateLayer_attributes,
    updateLayer_name,
    updateLayer_autoAssignPublicIps,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_autoAssignElasticIps,
    updateLayer_layerId,

    -- ** CreateStack
    createStack_defaultRootDeviceType,
    createStack_vpcId,
    createStack_chefConfiguration,
    createStack_agentVersion,
    createStack_defaultSshKeyName,
    createStack_customJson,
    createStack_customCookbooksSource,
    createStack_defaultAvailabilityZone,
    createStack_attributes,
    createStack_defaultOs,
    createStack_useOpsworksSecurityGroups,
    createStack_useCustomCookbooks,
    createStack_defaultSubnetId,
    createStack_configurationManager,
    createStack_hostnameTheme,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** UpdateElasticIp
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- ** CreateApp
    createApp_sslConfiguration,
    createApp_environment,
    createApp_enableSsl,
    createApp_shortname,
    createApp_dataSources,
    createApp_appSource,
    createApp_attributes,
    createApp_domains,
    createApp_description,
    createApp_stackId,
    createApp_name,
    createApp_type,
    createAppResponse_appId,
    createAppResponse_httpStatus,

    -- ** GetHostnameSuggestion
    getHostnameSuggestion_layerId,
    getHostnameSuggestionResponse_hostname,
    getHostnameSuggestionResponse_layerId,
    getHostnameSuggestionResponse_httpStatus,

    -- ** CloneStack
    cloneStack_defaultInstanceProfileArn,
    cloneStack_cloneAppIds,
    cloneStack_defaultRootDeviceType,
    cloneStack_vpcId,
    cloneStack_chefConfiguration,
    cloneStack_agentVersion,
    cloneStack_defaultSshKeyName,
    cloneStack_customJson,
    cloneStack_clonePermissions,
    cloneStack_customCookbooksSource,
    cloneStack_defaultAvailabilityZone,
    cloneStack_attributes,
    cloneStack_name,
    cloneStack_defaultOs,
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_useCustomCookbooks,
    cloneStack_defaultSubnetId,
    cloneStack_region,
    cloneStack_configurationManager,
    cloneStack_hostnameTheme,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,

    -- ** DescribePermissions
    describePermissions_iamUserArn,
    describePermissions_stackId,
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,

    -- ** DetachElasticLoadBalancer
    detachElasticLoadBalancer_elasticLoadBalancerName,
    detachElasticLoadBalancer_layerId,

    -- ** RegisterInstance
    registerInstance_privateIp,
    registerInstance_hostname,
    registerInstance_instanceIdentity,
    registerInstance_publicIp,
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_rsaPublicKey,
    registerInstance_stackId,
    registerInstanceResponse_instanceId,
    registerInstanceResponse_httpStatus,

    -- ** AssociateElasticIp
    associateElasticIp_instanceId,
    associateElasticIp_elasticIp,

    -- ** DescribeLoadBasedAutoScaling
    describeLoadBasedAutoScaling_layerIds,
    describeLoadBasedAutoScalingResponse_loadBasedAutoScalingConfigurations,
    describeLoadBasedAutoScalingResponse_httpStatus,

    -- ** DescribeStackProvisioningParameters
    describeStackProvisioningParameters_stackId,
    describeStackProvisioningParametersResponse_agentInstallerUrl,
    describeStackProvisioningParametersResponse_parameters,
    describeStackProvisioningParametersResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** UnassignInstance
    unassignInstance_instanceId,

    -- ** DescribeMyUserProfile
    describeMyUserProfileResponse_userProfile,
    describeMyUserProfileResponse_httpStatus,

    -- ** DeleteUserProfile
    deleteUserProfile_iamUserArn,

    -- ** UpdateUserProfile
    updateUserProfile_allowSelfManagement,
    updateUserProfile_sshPublicKey,
    updateUserProfile_sshUsername,
    updateUserProfile_iamUserArn,

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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** StartStack
    startStack_stackId,

    -- ** CreateUserProfile
    createUserProfile_allowSelfManagement,
    createUserProfile_sshPublicKey,
    createUserProfile_sshUsername,
    createUserProfile_iamUserArn,
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,

    -- ** DescribeOperatingSystems
    describeOperatingSystemsResponse_operatingSystems,
    describeOperatingSystemsResponse_httpStatus,

    -- ** DescribeCommands
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommands_commandIds,
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,

    -- ** AssignVolume
    assignVolume_instanceId,
    assignVolume_volumeId,

    -- ** DescribeElasticLoadBalancers
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,

    -- ** SetPermission
    setPermission_allowSudo,
    setPermission_level,
    setPermission_allowSsh,
    setPermission_stackId,
    setPermission_iamUserArn,

    -- ** DeregisterInstance
    deregisterInstance_instanceId,

    -- ** DescribeEcsClusters
    describeEcsClusters_nextToken,
    describeEcsClusters_stackId,
    describeEcsClusters_maxResults,
    describeEcsClusters_ecsClusterArns,
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,

    -- ** DescribeRaidArrays
    describeRaidArrays_instanceId,
    describeRaidArrays_raidArrayIds,
    describeRaidArrays_stackId,
    describeRaidArraysResponse_raidArrays,
    describeRaidArraysResponse_httpStatus,

    -- ** UpdateVolume
    updateVolume_name,
    updateVolume_mountPoint,
    updateVolume_volumeId,

    -- ** StartInstance
    startInstance_instanceId,

    -- * Types

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_configurationManager,

    -- ** App
    app_sslConfiguration,
    app_environment,
    app_enableSsl,
    app_createdAt,
    app_shortname,
    app_dataSources,
    app_appSource,
    app_appId,
    app_attributes,
    app_name,
    app_type,
    app_stackId,
    app_domains,
    app_description,

    -- ** AutoScalingThresholds
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_alarms,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_cpuThreshold,

    -- ** BlockDeviceMapping
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,

    -- ** ChefConfiguration
    chefConfiguration_berkshelfVersion,
    chefConfiguration_manageBerkshelf,

    -- ** CloudWatchLogsConfiguration
    cloudWatchLogsConfiguration_enabled,
    cloudWatchLogsConfiguration_logStreams,

    -- ** CloudWatchLogsLogStream
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_logGroupName,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_file,

    -- ** Command
    command_deploymentId,
    command_instanceId,
    command_status,
    command_logUrl,
    command_createdAt,
    command_commandId,
    command_exitCode,
    command_type,
    command_completedAt,
    command_acknowledgedAt,

    -- ** DataSource
    dataSource_arn,
    dataSource_databaseName,
    dataSource_type,

    -- ** Deployment
    deployment_deploymentId,
    deployment_status,
    deployment_command,
    deployment_createdAt,
    deployment_customJson,
    deployment_iamUserArn,
    deployment_appId,
    deployment_instanceIds,
    deployment_completedAt,
    deployment_stackId,
    deployment_comment,
    deployment_duration,

    -- ** DeploymentCommand
    deploymentCommand_args,
    deploymentCommand_name,

    -- ** EbsBlockDevice
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_snapshotId,

    -- ** EcsCluster
    ecsCluster_ecsClusterArn,
    ecsCluster_ecsClusterName,
    ecsCluster_registeredAt,
    ecsCluster_stackId,

    -- ** ElasticIp
    elasticIp_instanceId,
    elasticIp_domain,
    elasticIp_ip,
    elasticIp_name,
    elasticIp_region,

    -- ** ElasticLoadBalancer
    elasticLoadBalancer_subnetIds,
    elasticLoadBalancer_vpcId,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_region,
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,

    -- ** EnvironmentVariable
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- ** Instance
    instance_privateDns,
    instance_reportedAgentVersion,
    instance_instanceId,
    instance_status,
    instance_privateIp,
    instance_installUpdatesOnBoot,
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_platform,
    instance_hostname,
    instance_sshHostRsaKeyFingerprint,
    instance_securityGroupIds,
    instance_ecsClusterArn,
    instance_arn,
    instance_createdAt,
    instance_ec2InstanceId,
    instance_sshKeyName,
    instance_agentVersion,
    instance_rootDeviceVolumeId,
    instance_subnetId,
    instance_infrastructureClass,
    instance_sshHostDsaKeyFingerprint,
    instance_instanceType,
    instance_ebsOptimized,
    instance_elasticIp,
    instance_os,
    instance_availabilityZone,
    instance_lastServiceErrorId,
    instance_tenancy,
    instance_autoScalingType,
    instance_layerIds,
    instance_architecture,
    instance_publicDns,
    instance_amiId,
    instance_publicIp,
    instance_reportedOs,
    instance_registeredBy,
    instance_stackId,
    instance_rootDeviceType,
    instance_ecsContainerInstanceArn,
    instance_blockDeviceMappings,

    -- ** InstanceIdentity
    instanceIdentity_signature,
    instanceIdentity_document,

    -- ** InstancesCount
    instancesCount_terminating,
    instancesCount_pending,
    instancesCount_online,
    instancesCount_unassigning,
    instancesCount_deregistering,
    instancesCount_runningSetup,
    instancesCount_requested,
    instancesCount_stopFailed,
    instancesCount_booting,
    instancesCount_stopped,
    instancesCount_rebooting,
    instancesCount_assigning,
    instancesCount_shuttingDown,
    instancesCount_setupFailed,
    instancesCount_connectionLost,
    instancesCount_terminated,
    instancesCount_stopping,
    instancesCount_registered,
    instancesCount_startFailed,
    instancesCount_registering,

    -- ** Layer
    layer_customInstanceProfileArn,
    layer_customSecurityGroupIds,
    layer_installUpdatesOnBoot,
    layer_cloudWatchLogsConfiguration,
    layer_lifecycleEventConfiguration,
    layer_arn,
    layer_createdAt,
    layer_shortname,
    layer_defaultRecipes,
    layer_customRecipes,
    layer_customJson,
    layer_volumeConfigurations,
    layer_enableAutoHealing,
    layer_packages,
    layer_attributes,
    layer_name,
    layer_autoAssignPublicIps,
    layer_type,
    layer_useEbsOptimizedInstances,
    layer_stackId,
    layer_layerId,
    layer_defaultSecurityGroupNames,
    layer_autoAssignElasticIps,

    -- ** LifecycleEventConfiguration
    lifecycleEventConfiguration_shutdown,

    -- ** LoadBasedAutoScalingConfiguration
    loadBasedAutoScalingConfiguration_upScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_layerId,

    -- ** OperatingSystem
    operatingSystem_reportedVersion,
    operatingSystem_supported,
    operatingSystem_name,
    operatingSystem_id,
    operatingSystem_configurationManagers,
    operatingSystem_type,
    operatingSystem_reportedName,

    -- ** OperatingSystemConfigurationManager
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- ** Permission
    permission_iamUserArn,
    permission_allowSudo,
    permission_stackId,
    permission_level,
    permission_allowSsh,

    -- ** RaidArray
    raidArray_instanceId,
    raidArray_size,
    raidArray_iops,
    raidArray_createdAt,
    raidArray_raidLevel,
    raidArray_device,
    raidArray_numberOfDisks,
    raidArray_availabilityZone,
    raidArray_name,
    raidArray_raidArrayId,
    raidArray_volumeType,
    raidArray_stackId,
    raidArray_mountPoint,

    -- ** RdsDbInstance
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_dbUser,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_engine,
    rdsDbInstance_address,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_region,
    rdsDbInstance_stackId,
    rdsDbInstance_dbPassword,

    -- ** Recipes
    recipes_setup,
    recipes_shutdown,
    recipes_undeploy,
    recipes_configure,
    recipes_deploy,

    -- ** ReportedOs
    reportedOs_family,
    reportedOs_name,
    reportedOs_version,

    -- ** SelfUserProfile
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,
    selfUserProfile_iamUserArn,
    selfUserProfile_name,

    -- ** ServiceError
    serviceError_instanceId,
    serviceError_createdAt,
    serviceError_serviceErrorId,
    serviceError_type,
    serviceError_stackId,
    serviceError_message,

    -- ** ShutdownEventConfiguration
    shutdownEventConfiguration_executionTimeout,
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,

    -- ** Source
    source_url,
    source_username,
    source_sshKey,
    source_password,
    source_type,
    source_revision,

    -- ** SslConfiguration
    sslConfiguration_privateKey,
    sslConfiguration_certificate,
    sslConfiguration_chain,

    -- ** Stack
    stack_defaultInstanceProfileArn,
    stack_serviceRoleArn,
    stack_defaultRootDeviceType,
    stack_arn,
    stack_createdAt,
    stack_vpcId,
    stack_chefConfiguration,
    stack_agentVersion,
    stack_defaultSshKeyName,
    stack_customJson,
    stack_customCookbooksSource,
    stack_defaultAvailabilityZone,
    stack_attributes,
    stack_name,
    stack_defaultOs,
    stack_useOpsworksSecurityGroups,
    stack_useCustomCookbooks,
    stack_defaultSubnetId,
    stack_region,
    stack_configurationManager,
    stack_stackId,
    stack_hostnameTheme,

    -- ** StackConfigurationManager
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- ** StackSummary
    stackSummary_arn,
    stackSummary_appsCount,
    stackSummary_name,
    stackSummary_stackId,
    stackSummary_layersCount,
    stackSummary_instancesCount,

    -- ** TemporaryCredential
    temporaryCredential_instanceId,
    temporaryCredential_username,
    temporaryCredential_password,
    temporaryCredential_validForInMinutes,

    -- ** TimeBasedAutoScalingConfiguration
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- ** UserProfile
    userProfile_allowSelfManagement,
    userProfile_sshPublicKey,
    userProfile_sshUsername,
    userProfile_iamUserArn,
    userProfile_name,

    -- ** Volume
    volume_instanceId,
    volume_status,
    volume_size,
    volume_iops,
    volume_device,
    volume_encrypted,
    volume_availabilityZone,
    volume_name,
    volume_raidArrayId,
    volume_volumeId,
    volume_region,
    volume_volumeType,
    volume_ec2VolumeId,
    volume_mountPoint,

    -- ** VolumeConfiguration
    volumeConfiguration_iops,
    volumeConfiguration_raidLevel,
    volumeConfiguration_encrypted,
    volumeConfiguration_volumeType,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- ** WeeklyAutoScalingSchedule
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_wednesday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_monday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_tuesday,
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
