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
    cloneStack_cloneAppIds,
    cloneStack_hostnameTheme,
    cloneStack_name,
    cloneStack_defaultSshKeyName,
    cloneStack_customJson,
    cloneStack_defaultAvailabilityZone,
    cloneStack_defaultRootDeviceType,
    cloneStack_defaultInstanceProfileArn,
    cloneStack_clonePermissions,
    cloneStack_configurationManager,
    cloneStack_defaultSubnetId,
    cloneStack_region,
    cloneStack_useCustomCookbooks,
    cloneStack_defaultOs,
    cloneStack_useOpsworksSecurityGroups,
    cloneStack_attributes,
    cloneStack_vpcId,
    cloneStack_chefConfiguration,
    cloneStack_customCookbooksSource,
    cloneStack_agentVersion,
    cloneStack_sourceStackId,
    cloneStack_serviceRoleArn,
    cloneStackResponse_stackId,
    cloneStackResponse_httpStatus,

    -- ** CreateApp
    createApp_domains,
    createApp_appSource,
    createApp_environment,
    createApp_dataSources,
    createApp_sslConfiguration,
    createApp_description,
    createApp_attributes,
    createApp_enableSsl,
    createApp_shortname,
    createApp_stackId,
    createApp_name,
    createApp_type,
    createAppResponse_appId,
    createAppResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_customJson,
    createDeployment_comment,
    createDeployment_layerIds,
    createDeployment_instanceIds,
    createDeployment_appId,
    createDeployment_stackId,
    createDeployment_command,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,

    -- ** CreateInstance
    createInstance_ebsOptimized,
    createInstance_os,
    createInstance_amiId,
    createInstance_blockDeviceMappings,
    createInstance_autoScalingType,
    createInstance_rootDeviceType,
    createInstance_virtualizationType,
    createInstance_subnetId,
    createInstance_availabilityZone,
    createInstance_hostname,
    createInstance_sshKeyName,
    createInstance_architecture,
    createInstance_tenancy,
    createInstance_agentVersion,
    createInstance_installUpdatesOnBoot,
    createInstance_stackId,
    createInstance_layerIds,
    createInstance_instanceType,
    createInstanceResponse_instanceId,
    createInstanceResponse_httpStatus,

    -- ** CreateLayer
    createLayer_customRecipes,
    createLayer_autoAssignPublicIps,
    createLayer_customJson,
    createLayer_packages,
    createLayer_volumeConfigurations,
    createLayer_enableAutoHealing,
    createLayer_customSecurityGroupIds,
    createLayer_autoAssignElasticIps,
    createLayer_cloudWatchLogsConfiguration,
    createLayer_lifecycleEventConfiguration,
    createLayer_customInstanceProfileArn,
    createLayer_attributes,
    createLayer_useEbsOptimizedInstances,
    createLayer_installUpdatesOnBoot,
    createLayer_stackId,
    createLayer_type,
    createLayer_name,
    createLayer_shortname,
    createLayerResponse_layerId,
    createLayerResponse_httpStatus,

    -- ** CreateStack
    createStack_hostnameTheme,
    createStack_defaultSshKeyName,
    createStack_customJson,
    createStack_defaultAvailabilityZone,
    createStack_defaultRootDeviceType,
    createStack_configurationManager,
    createStack_defaultSubnetId,
    createStack_useCustomCookbooks,
    createStack_defaultOs,
    createStack_useOpsworksSecurityGroups,
    createStack_attributes,
    createStack_vpcId,
    createStack_chefConfiguration,
    createStack_customCookbooksSource,
    createStack_agentVersion,
    createStack_name,
    createStack_region,
    createStack_serviceRoleArn,
    createStack_defaultInstanceProfileArn,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_sshPublicKey,
    createUserProfile_sshUsername,
    createUserProfile_allowSelfManagement,
    createUserProfile_iamUserArn,
    createUserProfileResponse_iamUserArn,
    createUserProfileResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_appId,

    -- ** DeleteInstance
    deleteInstance_deleteVolumes,
    deleteInstance_deleteElasticIp,
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
    describeAgentVersions_stackId,
    describeAgentVersions_configurationManager,
    describeAgentVersionsResponse_agentVersions,
    describeAgentVersionsResponse_httpStatus,

    -- ** DescribeApps
    describeApps_stackId,
    describeApps_appIds,
    describeAppsResponse_apps,
    describeAppsResponse_httpStatus,

    -- ** DescribeCommands
    describeCommands_commandIds,
    describeCommands_deploymentId,
    describeCommands_instanceId,
    describeCommandsResponse_commands,
    describeCommandsResponse_httpStatus,

    -- ** DescribeDeployments
    describeDeployments_stackId,
    describeDeployments_deploymentIds,
    describeDeployments_appId,
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,

    -- ** DescribeEcsClusters
    describeEcsClusters_stackId,
    describeEcsClusters_nextToken,
    describeEcsClusters_ecsClusterArns,
    describeEcsClusters_maxResults,
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,

    -- ** DescribeElasticIps
    describeElasticIps_stackId,
    describeElasticIps_ips,
    describeElasticIps_instanceId,
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,

    -- ** DescribeElasticLoadBalancers
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_stackId,
    describeInstances_instanceIds,
    describeInstances_layerId,
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,

    -- ** DescribeLayers
    describeLayers_stackId,
    describeLayers_layerIds,
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
    describePermissions_stackId,
    describePermissions_iamUserArn,
    describePermissionsResponse_permissions,
    describePermissionsResponse_httpStatus,

    -- ** DescribeRaidArrays
    describeRaidArrays_stackId,
    describeRaidArrays_raidArrayIds,
    describeRaidArrays_instanceId,
    describeRaidArraysResponse_raidArrays,
    describeRaidArraysResponse_httpStatus,

    -- ** DescribeRdsDbInstances
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,

    -- ** DescribeServiceErrors
    describeServiceErrors_stackId,
    describeServiceErrors_instanceId,
    describeServiceErrors_serviceErrorIds,
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
    describeVolumes_stackId,
    describeVolumes_volumeIds,
    describeVolumes_raidArrayId,
    describeVolumes_instanceId,
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
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_nextToken,
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
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_hostname,
    registerInstance_instanceIdentity,
    registerInstance_publicIp,
    registerInstance_rsaPublicKey,
    registerInstance_privateIp,
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
    setLoadBasedAutoScaling_upScaling,
    setLoadBasedAutoScaling_enable,
    setLoadBasedAutoScaling_downScaling,
    setLoadBasedAutoScaling_layerId,

    -- ** SetPermission
    setPermission_allowSudo,
    setPermission_level,
    setPermission_allowSsh,
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
    updateApp_domains,
    updateApp_appSource,
    updateApp_name,
    updateApp_type,
    updateApp_environment,
    updateApp_dataSources,
    updateApp_sslConfiguration,
    updateApp_description,
    updateApp_attributes,
    updateApp_enableSsl,
    updateApp_appId,

    -- ** UpdateElasticIp
    updateElasticIp_name,
    updateElasticIp_elasticIp,

    -- ** UpdateInstance
    updateInstance_ebsOptimized,
    updateInstance_os,
    updateInstance_amiId,
    updateInstance_autoScalingType,
    updateInstance_hostname,
    updateInstance_instanceType,
    updateInstance_sshKeyName,
    updateInstance_layerIds,
    updateInstance_architecture,
    updateInstance_agentVersion,
    updateInstance_installUpdatesOnBoot,
    updateInstance_instanceId,

    -- ** UpdateLayer
    updateLayer_name,
    updateLayer_customRecipes,
    updateLayer_autoAssignPublicIps,
    updateLayer_customJson,
    updateLayer_packages,
    updateLayer_volumeConfigurations,
    updateLayer_enableAutoHealing,
    updateLayer_customSecurityGroupIds,
    updateLayer_autoAssignElasticIps,
    updateLayer_cloudWatchLogsConfiguration,
    updateLayer_lifecycleEventConfiguration,
    updateLayer_customInstanceProfileArn,
    updateLayer_attributes,
    updateLayer_shortname,
    updateLayer_useEbsOptimizedInstances,
    updateLayer_installUpdatesOnBoot,
    updateLayer_layerId,

    -- ** UpdateMyUserProfile
    updateMyUserProfile_sshPublicKey,

    -- ** UpdateRdsDbInstance
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- ** UpdateStack
    updateStack_hostnameTheme,
    updateStack_name,
    updateStack_defaultSshKeyName,
    updateStack_customJson,
    updateStack_defaultAvailabilityZone,
    updateStack_serviceRoleArn,
    updateStack_defaultRootDeviceType,
    updateStack_defaultInstanceProfileArn,
    updateStack_configurationManager,
    updateStack_defaultSubnetId,
    updateStack_useCustomCookbooks,
    updateStack_defaultOs,
    updateStack_useOpsworksSecurityGroups,
    updateStack_attributes,
    updateStack_chefConfiguration,
    updateStack_customCookbooksSource,
    updateStack_agentVersion,
    updateStack_stackId,

    -- ** UpdateUserProfile
    updateUserProfile_sshPublicKey,
    updateUserProfile_sshUsername,
    updateUserProfile_allowSelfManagement,
    updateUserProfile_iamUserArn,

    -- ** UpdateVolume
    updateVolume_name,
    updateVolume_mountPoint,
    updateVolume_volumeId,

    -- * Types

    -- ** AgentVersion
    agentVersion_configurationManager,
    agentVersion_version,

    -- ** App
    app_domains,
    app_stackId,
    app_appSource,
    app_name,
    app_type,
    app_environment,
    app_dataSources,
    app_sslConfiguration,
    app_description,
    app_attributes,
    app_enableSsl,
    app_shortname,
    app_createdAt,
    app_appId,

    -- ** AutoScalingThresholds
    autoScalingThresholds_ignoreMetricsTime,
    autoScalingThresholds_alarms,
    autoScalingThresholds_memoryThreshold,
    autoScalingThresholds_instanceCount,
    autoScalingThresholds_loadThreshold,
    autoScalingThresholds_thresholdsWaitTime,
    autoScalingThresholds_cpuThreshold,

    -- ** BlockDeviceMapping
    blockDeviceMapping_ebs,
    blockDeviceMapping_deviceName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,

    -- ** ChefConfiguration
    chefConfiguration_berkshelfVersion,
    chefConfiguration_manageBerkshelf,

    -- ** CloudWatchLogsConfiguration
    cloudWatchLogsConfiguration_logStreams,
    cloudWatchLogsConfiguration_enabled,

    -- ** CloudWatchLogsLogStream
    cloudWatchLogsLogStream_batchCount,
    cloudWatchLogsLogStream_multiLineStartPattern,
    cloudWatchLogsLogStream_fileFingerprintLines,
    cloudWatchLogsLogStream_encoding,
    cloudWatchLogsLogStream_timeZone,
    cloudWatchLogsLogStream_datetimeFormat,
    cloudWatchLogsLogStream_initialPosition,
    cloudWatchLogsLogStream_file,
    cloudWatchLogsLogStream_bufferDuration,
    cloudWatchLogsLogStream_batchSize,
    cloudWatchLogsLogStream_logGroupName,

    -- ** Command
    command_type,
    command_deploymentId,
    command_logUrl,
    command_status,
    command_commandId,
    command_instanceId,
    command_acknowledgedAt,
    command_exitCode,
    command_completedAt,
    command_createdAt,

    -- ** DataSource
    dataSource_type,
    dataSource_databaseName,
    dataSource_arn,

    -- ** Deployment
    deployment_stackId,
    deployment_iamUserArn,
    deployment_customJson,
    deployment_deploymentId,
    deployment_command,
    deployment_status,
    deployment_duration,
    deployment_comment,
    deployment_instanceIds,
    deployment_completedAt,
    deployment_createdAt,
    deployment_appId,

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
    ecsCluster_ecsClusterName,
    ecsCluster_stackId,
    ecsCluster_ecsClusterArn,
    ecsCluster_registeredAt,

    -- ** ElasticIp
    elasticIp_name,
    elasticIp_domain,
    elasticIp_ip,
    elasticIp_region,
    elasticIp_instanceId,

    -- ** ElasticLoadBalancer
    elasticLoadBalancer_elasticLoadBalancerName,
    elasticLoadBalancer_stackId,
    elasticLoadBalancer_availabilityZones,
    elasticLoadBalancer_region,
    elasticLoadBalancer_ec2InstanceIds,
    elasticLoadBalancer_vpcId,
    elasticLoadBalancer_layerId,
    elasticLoadBalancer_dnsName,
    elasticLoadBalancer_subnetIds,

    -- ** EnvironmentVariable
    environmentVariable_secure,
    environmentVariable_key,
    environmentVariable_value,

    -- ** Instance
    instance_ebsOptimized,
    instance_os,
    instance_privateDns,
    instance_stackId,
    instance_amiId,
    instance_ec2InstanceId,
    instance_elasticIp,
    instance_ecsClusterArn,
    instance_blockDeviceMappings,
    instance_securityGroupIds,
    instance_autoScalingType,
    instance_registeredBy,
    instance_rootDeviceType,
    instance_virtualizationType,
    instance_instanceProfileArn,
    instance_subnetId,
    instance_ecsContainerInstanceArn,
    instance_arn,
    instance_rootDeviceVolumeId,
    instance_status,
    instance_platform,
    instance_availabilityZone,
    instance_hostname,
    instance_publicIp,
    instance_infrastructureClass,
    instance_reportedAgentVersion,
    instance_instanceType,
    instance_instanceId,
    instance_sshHostDsaKeyFingerprint,
    instance_sshKeyName,
    instance_lastServiceErrorId,
    instance_layerIds,
    instance_privateIp,
    instance_reportedOs,
    instance_architecture,
    instance_createdAt,
    instance_publicDns,
    instance_tenancy,
    instance_agentVersion,
    instance_sshHostRsaKeyFingerprint,
    instance_installUpdatesOnBoot,

    -- ** InstanceIdentity
    instanceIdentity_document,
    instanceIdentity_signature,

    -- ** InstancesCount
    instancesCount_booting,
    instancesCount_deregistering,
    instancesCount_runningSetup,
    instancesCount_stopFailed,
    instancesCount_terminated,
    instancesCount_requested,
    instancesCount_connectionLost,
    instancesCount_online,
    instancesCount_startFailed,
    instancesCount_terminating,
    instancesCount_unassigning,
    instancesCount_setupFailed,
    instancesCount_rebooting,
    instancesCount_assigning,
    instancesCount_stopped,
    instancesCount_registered,
    instancesCount_pending,
    instancesCount_stopping,
    instancesCount_shuttingDown,
    instancesCount_registering,

    -- ** Layer
    layer_stackId,
    layer_name,
    layer_customRecipes,
    layer_type,
    layer_autoAssignPublicIps,
    layer_customJson,
    layer_packages,
    layer_volumeConfigurations,
    layer_arn,
    layer_defaultSecurityGroupNames,
    layer_enableAutoHealing,
    layer_customSecurityGroupIds,
    layer_autoAssignElasticIps,
    layer_defaultRecipes,
    layer_cloudWatchLogsConfiguration,
    layer_lifecycleEventConfiguration,
    layer_customInstanceProfileArn,
    layer_attributes,
    layer_layerId,
    layer_shortname,
    layer_createdAt,
    layer_useEbsOptimizedInstances,
    layer_installUpdatesOnBoot,

    -- ** LifecycleEventConfiguration
    lifecycleEventConfiguration_shutdown,

    -- ** LoadBasedAutoScalingConfiguration
    loadBasedAutoScalingConfiguration_upScaling,
    loadBasedAutoScalingConfiguration_enable,
    loadBasedAutoScalingConfiguration_downScaling,
    loadBasedAutoScalingConfiguration_layerId,

    -- ** OperatingSystem
    operatingSystem_configurationManagers,
    operatingSystem_name,
    operatingSystem_type,
    operatingSystem_supported,
    operatingSystem_reportedVersion,
    operatingSystem_id,
    operatingSystem_reportedName,

    -- ** OperatingSystemConfigurationManager
    operatingSystemConfigurationManager_name,
    operatingSystemConfigurationManager_version,

    -- ** Permission
    permission_stackId,
    permission_iamUserArn,
    permission_allowSudo,
    permission_level,
    permission_allowSsh,

    -- ** RaidArray
    raidArray_stackId,
    raidArray_name,
    raidArray_device,
    raidArray_raidArrayId,
    raidArray_mountPoint,
    raidArray_size,
    raidArray_volumeType,
    raidArray_availabilityZone,
    raidArray_instanceId,
    raidArray_numberOfDisks,
    raidArray_raidLevel,
    raidArray_iops,
    raidArray_createdAt,

    -- ** RdsDbInstance
    rdsDbInstance_stackId,
    rdsDbInstance_missingOnRds,
    rdsDbInstance_dbInstanceIdentifier,
    rdsDbInstance_dbPassword,
    rdsDbInstance_region,
    rdsDbInstance_address,
    rdsDbInstance_rdsDbInstanceArn,
    rdsDbInstance_engine,
    rdsDbInstance_dbUser,

    -- ** Recipes
    recipes_deploy,
    recipes_configure,
    recipes_undeploy,
    recipes_shutdown,
    recipes_setup,

    -- ** ReportedOs
    reportedOs_name,
    reportedOs_family,
    reportedOs_version,

    -- ** SelfUserProfile
    selfUserProfile_name,
    selfUserProfile_iamUserArn,
    selfUserProfile_sshPublicKey,
    selfUserProfile_sshUsername,

    -- ** ServiceError
    serviceError_stackId,
    serviceError_message,
    serviceError_type,
    serviceError_serviceErrorId,
    serviceError_instanceId,
    serviceError_createdAt,

    -- ** ShutdownEventConfiguration
    shutdownEventConfiguration_delayUntilElbConnectionsDrained,
    shutdownEventConfiguration_executionTimeout,

    -- ** Source
    source_type,
    source_password,
    source_username,
    source_revision,
    source_url,
    source_sshKey,

    -- ** SslConfiguration
    sslConfiguration_chain,
    sslConfiguration_privateKey,
    sslConfiguration_certificate,

    -- ** Stack
    stack_hostnameTheme,
    stack_stackId,
    stack_name,
    stack_defaultSshKeyName,
    stack_customJson,
    stack_defaultAvailabilityZone,
    stack_serviceRoleArn,
    stack_defaultRootDeviceType,
    stack_defaultInstanceProfileArn,
    stack_arn,
    stack_configurationManager,
    stack_defaultSubnetId,
    stack_region,
    stack_useCustomCookbooks,
    stack_defaultOs,
    stack_useOpsworksSecurityGroups,
    stack_attributes,
    stack_vpcId,
    stack_createdAt,
    stack_chefConfiguration,
    stack_customCookbooksSource,
    stack_agentVersion,

    -- ** StackConfigurationManager
    stackConfigurationManager_name,
    stackConfigurationManager_version,

    -- ** StackSummary
    stackSummary_stackId,
    stackSummary_name,
    stackSummary_arn,
    stackSummary_instancesCount,
    stackSummary_appsCount,
    stackSummary_layersCount,

    -- ** TemporaryCredential
    temporaryCredential_password,
    temporaryCredential_username,
    temporaryCredential_validForInMinutes,
    temporaryCredential_instanceId,

    -- ** TimeBasedAutoScalingConfiguration
    timeBasedAutoScalingConfiguration_instanceId,
    timeBasedAutoScalingConfiguration_autoScalingSchedule,

    -- ** UserProfile
    userProfile_name,
    userProfile_iamUserArn,
    userProfile_sshPublicKey,
    userProfile_sshUsername,
    userProfile_allowSelfManagement,

    -- ** Volume
    volume_name,
    volume_device,
    volume_raidArrayId,
    volume_mountPoint,
    volume_size,
    volume_volumeType,
    volume_status,
    volume_availabilityZone,
    volume_ec2VolumeId,
    volume_region,
    volume_instanceId,
    volume_encrypted,
    volume_volumeId,
    volume_iops,

    -- ** VolumeConfiguration
    volumeConfiguration_volumeType,
    volumeConfiguration_encrypted,
    volumeConfiguration_raidLevel,
    volumeConfiguration_iops,
    volumeConfiguration_mountPoint,
    volumeConfiguration_numberOfDisks,
    volumeConfiguration_size,

    -- ** WeeklyAutoScalingSchedule
    weeklyAutoScalingSchedule_tuesday,
    weeklyAutoScalingSchedule_friday,
    weeklyAutoScalingSchedule_saturday,
    weeklyAutoScalingSchedule_thursday,
    weeklyAutoScalingSchedule_sunday,
    weeklyAutoScalingSchedule_wednesday,
    weeklyAutoScalingSchedule_monday,
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
