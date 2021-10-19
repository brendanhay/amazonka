{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Lens
  ( -- * Operations

    -- ** RunJobFlow
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_amiVersion,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_additionalInfo,
    runJobFlow_autoTerminationPolicy,
    runJobFlow_configurations,
    runJobFlow_customAmiId,
    runJobFlow_autoScalingRole,
    runJobFlow_securityConfiguration,
    runJobFlow_scaleDownBehavior,
    runJobFlow_steps,
    runJobFlow_jobFlowRole,
    runJobFlow_bootstrapActions,
    runJobFlow_releaseLabel,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_placementGroupConfigs,
    runJobFlow_logUri,
    runJobFlow_kerberosAttributes,
    runJobFlow_newSupportedProducts,
    runJobFlow_managedScalingPolicy,
    runJobFlow_visibleToAllUsers,
    runJobFlow_supportedProducts,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_applications,
    runJobFlow_tags,
    runJobFlow_serviceRole,
    runJobFlow_name,
    runJobFlow_instances,
    runJobFlowResponse_clusterArn,
    runJobFlowResponse_jobFlowId,
    runJobFlowResponse_httpStatus,

    -- ** RemoveAutoScalingPolicy
    removeAutoScalingPolicy_clusterId,
    removeAutoScalingPolicy_instanceGroupId,
    removeAutoScalingPolicyResponse_httpStatus,

    -- ** CreateStudio
    createStudio_idpAuthUrl,
    createStudio_idpRelayStateParameterName,
    createStudio_userRole,
    createStudio_description,
    createStudio_tags,
    createStudio_name,
    createStudio_authMode,
    createStudio_vpcId,
    createStudio_subnetIds,
    createStudio_serviceRole,
    createStudio_workspaceSecurityGroupId,
    createStudio_engineSecurityGroupId,
    createStudio_defaultS3Location,
    createStudioResponse_studioId,
    createStudioResponse_url,
    createStudioResponse_httpStatus,

    -- ** SetVisibleToAllUsers
    setVisibleToAllUsers_jobFlowIds,
    setVisibleToAllUsers_visibleToAllUsers,

    -- ** TerminateJobFlows
    terminateJobFlows_jobFlowIds,

    -- ** DescribeStep
    describeStep_clusterId,
    describeStep_stepId,
    describeStepResponse_step,
    describeStepResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceId,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_httpStatus,
    describeClusterResponse_cluster,

    -- ** ListSecurityConfigurations
    listSecurityConfigurations_marker,
    listSecurityConfigurationsResponse_securityConfigurations,
    listSecurityConfigurationsResponse_marker,
    listSecurityConfigurationsResponse_httpStatus,

    -- ** CancelSteps
    cancelSteps_stepCancellationOption,
    cancelSteps_clusterId,
    cancelSteps_stepIds,
    cancelStepsResponse_cancelStepsInfoList,
    cancelStepsResponse_httpStatus,

    -- ** ListNotebookExecutions
    listNotebookExecutions_status,
    listNotebookExecutions_editorId,
    listNotebookExecutions_to,
    listNotebookExecutions_from,
    listNotebookExecutions_marker,
    listNotebookExecutionsResponse_notebookExecutions,
    listNotebookExecutionsResponse_marker,
    listNotebookExecutionsResponse_httpStatus,

    -- ** PutAutoTerminationPolicy
    putAutoTerminationPolicy_autoTerminationPolicy,
    putAutoTerminationPolicy_clusterId,
    putAutoTerminationPolicyResponse_httpStatus,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_securityConfiguration,
    createSecurityConfigurationResponse_httpStatus,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_creationDateTime,

    -- ** DescribeReleaseLabel
    describeReleaseLabel_nextToken,
    describeReleaseLabel_releaseLabel,
    describeReleaseLabel_maxResults,
    describeReleaseLabelResponse_nextToken,
    describeReleaseLabelResponse_releaseLabel,
    describeReleaseLabelResponse_applications,
    describeReleaseLabelResponse_httpStatus,

    -- ** SetTerminationProtection
    setTerminationProtection_jobFlowIds,
    setTerminationProtection_terminationProtected,

    -- ** AddJobFlowSteps
    addJobFlowSteps_jobFlowId,
    addJobFlowSteps_steps,
    addJobFlowStepsResponse_stepIds,
    addJobFlowStepsResponse_httpStatus,

    -- ** DescribeStudio
    describeStudio_studioId,
    describeStudioResponse_studio,
    describeStudioResponse_httpStatus,

    -- ** ModifyInstanceGroups
    modifyInstanceGroups_clusterId,
    modifyInstanceGroups_instanceGroups,

    -- ** StartNotebookExecution
    startNotebookExecution_notebookInstanceSecurityGroupId,
    startNotebookExecution_notebookExecutionName,
    startNotebookExecution_notebookParams,
    startNotebookExecution_tags,
    startNotebookExecution_editorId,
    startNotebookExecution_relativePath,
    startNotebookExecution_executionEngine,
    startNotebookExecution_serviceRole,
    startNotebookExecutionResponse_notebookExecutionId,
    startNotebookExecutionResponse_httpStatus,

    -- ** ListSteps
    listSteps_stepIds,
    listSteps_stepStates,
    listSteps_marker,
    listSteps_clusterId,
    listStepsResponse_steps,
    listStepsResponse_marker,
    listStepsResponse_httpStatus,

    -- ** ListReleaseLabels
    listReleaseLabels_filters,
    listReleaseLabels_nextToken,
    listReleaseLabels_maxResults,
    listReleaseLabelsResponse_releaseLabels,
    listReleaseLabelsResponse_nextToken,
    listReleaseLabelsResponse_httpStatus,

    -- ** CreateStudioSessionMapping
    createStudioSessionMapping_identityId,
    createStudioSessionMapping_identityName,
    createStudioSessionMapping_studioId,
    createStudioSessionMapping_identityType,
    createStudioSessionMapping_sessionPolicyArn,

    -- ** AddInstanceFleet
    addInstanceFleet_clusterId,
    addInstanceFleet_instanceFleet,
    addInstanceFleetResponse_clusterArn,
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_studioId,

    -- ** UpdateStudio
    updateStudio_subnetIds,
    updateStudio_defaultS3Location,
    updateStudio_name,
    updateStudio_description,
    updateStudio_studioId,

    -- ** ListStudios
    listStudios_marker,
    listStudiosResponse_studios,
    listStudiosResponse_marker,
    listStudiosResponse_httpStatus,

    -- ** PutManagedScalingPolicy
    putManagedScalingPolicy_clusterId,
    putManagedScalingPolicy_managedScalingPolicy,
    putManagedScalingPolicyResponse_httpStatus,

    -- ** AddInstanceGroups
    addInstanceGroups_instanceGroups,
    addInstanceGroups_jobFlowId,
    addInstanceGroupsResponse_clusterArn,
    addInstanceGroupsResponse_jobFlowId,
    addInstanceGroupsResponse_instanceGroupIds,
    addInstanceGroupsResponse_httpStatus,

    -- ** GetStudioSessionMapping
    getStudioSessionMapping_identityId,
    getStudioSessionMapping_identityName,
    getStudioSessionMapping_studioId,
    getStudioSessionMapping_identityType,
    getStudioSessionMappingResponse_sessionMapping,
    getStudioSessionMappingResponse_httpStatus,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** ModifyInstanceFleet
    modifyInstanceFleet_clusterId,
    modifyInstanceFleet_instanceFleet,

    -- ** ListInstanceGroups
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_httpStatus,

    -- ** GetBlockPublicAccessConfiguration
    getBlockPublicAccessConfigurationResponse_httpStatus,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata,

    -- ** ModifyCluster
    modifyCluster_stepConcurrencyLevel,
    modifyCluster_clusterId,
    modifyClusterResponse_stepConcurrencyLevel,
    modifyClusterResponse_httpStatus,

    -- ** GetAutoTerminationPolicy
    getAutoTerminationPolicy_clusterId,
    getAutoTerminationPolicyResponse_autoTerminationPolicy,
    getAutoTerminationPolicyResponse_httpStatus,

    -- ** PutBlockPublicAccessConfiguration
    putBlockPublicAccessConfiguration_blockPublicAccessConfiguration,
    putBlockPublicAccessConfigurationResponse_httpStatus,

    -- ** ListBootstrapActions
    listBootstrapActions_marker,
    listBootstrapActions_clusterId,
    listBootstrapActionsResponse_bootstrapActions,
    listBootstrapActionsResponse_marker,
    listBootstrapActionsResponse_httpStatus,

    -- ** RemoveAutoTerminationPolicy
    removeAutoTerminationPolicy_clusterId,
    removeAutoTerminationPolicyResponse_httpStatus,

    -- ** AddTags
    addTags_resourceId,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** ListInstances
    listInstances_instanceGroupTypes,
    listInstances_instanceFleetType,
    listInstances_marker,
    listInstances_instanceFleetId,
    listInstances_instanceStates,
    listInstances_instanceGroupId,
    listInstances_clusterId,
    listInstancesResponse_marker,
    listInstancesResponse_instances,
    listInstancesResponse_httpStatus,

    -- ** PutAutoScalingPolicy
    putAutoScalingPolicy_clusterId,
    putAutoScalingPolicy_instanceGroupId,
    putAutoScalingPolicy_autoScalingPolicy,
    putAutoScalingPolicyResponse_clusterArn,
    putAutoScalingPolicyResponse_clusterId,
    putAutoScalingPolicyResponse_autoScalingPolicy,
    putAutoScalingPolicyResponse_instanceGroupId,
    putAutoScalingPolicyResponse_httpStatus,

    -- ** DeleteStudioSessionMapping
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_identityName,
    deleteStudioSessionMapping_studioId,
    deleteStudioSessionMapping_identityType,

    -- ** UpdateStudioSessionMapping
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- ** ListClusters
    listClusters_createdAfter,
    listClusters_marker,
    listClusters_clusterStates,
    listClusters_createdBefore,
    listClustersResponse_marker,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,

    -- ** DescribeSecurityConfiguration
    describeSecurityConfiguration_name,
    describeSecurityConfigurationResponse_securityConfiguration,
    describeSecurityConfigurationResponse_name,
    describeSecurityConfigurationResponse_creationDateTime,
    describeSecurityConfigurationResponse_httpStatus,

    -- ** StopNotebookExecution
    stopNotebookExecution_notebookExecutionId,

    -- ** ListStudioSessionMappings
    listStudioSessionMappings_studioId,
    listStudioSessionMappings_identityType,
    listStudioSessionMappings_marker,
    listStudioSessionMappingsResponse_sessionMappings,
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_httpStatus,

    -- ** GetManagedScalingPolicy
    getManagedScalingPolicy_clusterId,
    getManagedScalingPolicyResponse_managedScalingPolicy,
    getManagedScalingPolicyResponse_httpStatus,

    -- ** ListInstanceFleets
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_httpStatus,

    -- ** RemoveManagedScalingPolicy
    removeManagedScalingPolicy_clusterId,
    removeManagedScalingPolicyResponse_httpStatus,

    -- ** DescribeNotebookExecution
    describeNotebookExecution_notebookExecutionId,
    describeNotebookExecutionResponse_notebookExecution,
    describeNotebookExecutionResponse_httpStatus,

    -- * Types

    -- ** Application
    application_args,
    application_additionalInfo,
    application_name,
    application_version,

    -- ** AutoScalingPolicy
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_status,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_constraints,

    -- ** AutoScalingPolicyStateChangeReason
    autoScalingPolicyStateChangeReason_code,
    autoScalingPolicyStateChangeReason_message,

    -- ** AutoScalingPolicyStatus
    autoScalingPolicyStatus_state,
    autoScalingPolicyStatus_stateChangeReason,

    -- ** AutoTerminationPolicy
    autoTerminationPolicy_idleTimeout,

    -- ** BlockPublicAccessConfiguration
    blockPublicAccessConfiguration_permittedPublicSecurityGroupRuleRanges,
    blockPublicAccessConfiguration_blockPublicSecurityGroupRules,

    -- ** BlockPublicAccessConfigurationMetadata
    blockPublicAccessConfigurationMetadata_creationDateTime,
    blockPublicAccessConfigurationMetadata_createdByArn,

    -- ** BootstrapActionConfig
    bootstrapActionConfig_name,
    bootstrapActionConfig_scriptBootstrapAction,

    -- ** CancelStepsInfo
    cancelStepsInfo_status,
    cancelStepsInfo_stepId,
    cancelStepsInfo_reason,

    -- ** CloudWatchAlarmDefinition
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- ** Cluster
    cluster_logEncryptionKmsKeyId,
    cluster_clusterArn,
    cluster_requestedAmiVersion,
    cluster_ebsRootVolumeSize,
    cluster_ec2InstanceAttributes,
    cluster_outpostArn,
    cluster_normalizedInstanceHours,
    cluster_configurations,
    cluster_customAmiId,
    cluster_autoScalingRole,
    cluster_securityConfiguration,
    cluster_scaleDownBehavior,
    cluster_instanceCollectionType,
    cluster_releaseLabel,
    cluster_repoUpgradeOnBoot,
    cluster_logUri,
    cluster_kerberosAttributes,
    cluster_placementGroups,
    cluster_runningAmiVersion,
    cluster_masterPublicDnsName,
    cluster_terminationProtected,
    cluster_visibleToAllUsers,
    cluster_autoTerminate,
    cluster_stepConcurrencyLevel,
    cluster_applications,
    cluster_tags,
    cluster_serviceRole,
    cluster_id,
    cluster_name,
    cluster_status,

    -- ** ClusterStateChangeReason
    clusterStateChangeReason_code,
    clusterStateChangeReason_message,

    -- ** ClusterStatus
    clusterStatus_state,
    clusterStatus_stateChangeReason,
    clusterStatus_timeline,

    -- ** ClusterSummary
    clusterSummary_status,
    clusterSummary_clusterArn,
    clusterSummary_outpostArn,
    clusterSummary_normalizedInstanceHours,
    clusterSummary_name,
    clusterSummary_id,

    -- ** ClusterTimeline
    clusterTimeline_readyDateTime,
    clusterTimeline_creationDateTime,
    clusterTimeline_endDateTime,

    -- ** Command
    command_args,
    command_scriptPath,
    command_name,

    -- ** ComputeLimits
    computeLimits_maximumOnDemandCapacityUnits,
    computeLimits_maximumCoreCapacityUnits,
    computeLimits_unitType,
    computeLimits_minimumCapacityUnits,
    computeLimits_maximumCapacityUnits,

    -- ** Configuration
    configuration_configurations,
    configuration_classification,
    configuration_properties,

    -- ** EbsBlockDevice
    ebsBlockDevice_device,
    ebsBlockDevice_volumeSpecification,

    -- ** EbsBlockDeviceConfig
    ebsBlockDeviceConfig_volumesPerInstance,
    ebsBlockDeviceConfig_volumeSpecification,

    -- ** EbsConfiguration
    ebsConfiguration_ebsOptimized,
    ebsConfiguration_ebsBlockDeviceConfigs,

    -- ** EbsVolume
    ebsVolume_device,
    ebsVolume_volumeId,

    -- ** Ec2InstanceAttributes
    ec2InstanceAttributes_ec2KeyName,
    ec2InstanceAttributes_emrManagedSlaveSecurityGroup,
    ec2InstanceAttributes_additionalSlaveSecurityGroups,
    ec2InstanceAttributes_requestedEc2SubnetIds,
    ec2InstanceAttributes_additionalMasterSecurityGroups,
    ec2InstanceAttributes_iamInstanceProfile,
    ec2InstanceAttributes_emrManagedMasterSecurityGroup,
    ec2InstanceAttributes_ec2SubnetId,
    ec2InstanceAttributes_requestedEc2AvailabilityZones,
    ec2InstanceAttributes_serviceAccessSecurityGroup,
    ec2InstanceAttributes_ec2AvailabilityZone,

    -- ** ExecutionEngineConfig
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_type,
    executionEngineConfig_id,

    -- ** FailureDetails
    failureDetails_logFile,
    failureDetails_reason,
    failureDetails_message,

    -- ** HadoopJarStepConfig
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_jar,

    -- ** HadoopStepConfig
    hadoopStepConfig_args,
    hadoopStepConfig_jar,
    hadoopStepConfig_mainClass,
    hadoopStepConfig_properties,

    -- ** Instance
    instance_status,
    instance_publicDnsName,
    instance_ebsVolumes,
    instance_ec2InstanceId,
    instance_instanceType,
    instance_market,
    instance_privateIpAddress,
    instance_instanceFleetId,
    instance_id,
    instance_instanceGroupId,
    instance_privateDnsName,
    instance_publicIpAddress,

    -- ** InstanceFleet
    instanceFleet_provisionedSpotCapacity,
    instanceFleet_status,
    instanceFleet_targetOnDemandCapacity,
    instanceFleet_instanceFleetType,
    instanceFleet_instanceTypeSpecifications,
    instanceFleet_name,
    instanceFleet_provisionedOnDemandCapacity,
    instanceFleet_targetSpotCapacity,
    instanceFleet_id,
    instanceFleet_launchSpecifications,

    -- ** InstanceFleetConfig
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_name,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_instanceFleetType,

    -- ** InstanceFleetModifyConfig
    instanceFleetModifyConfig_targetOnDemandCapacity,
    instanceFleetModifyConfig_targetSpotCapacity,
    instanceFleetModifyConfig_instanceFleetId,

    -- ** InstanceFleetProvisioningSpecifications
    instanceFleetProvisioningSpecifications_spotSpecification,
    instanceFleetProvisioningSpecifications_onDemandSpecification,

    -- ** InstanceFleetStateChangeReason
    instanceFleetStateChangeReason_code,
    instanceFleetStateChangeReason_message,

    -- ** InstanceFleetStatus
    instanceFleetStatus_state,
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_timeline,

    -- ** InstanceFleetTimeline
    instanceFleetTimeline_readyDateTime,
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_endDateTime,

    -- ** InstanceGroup
    instanceGroup_status,
    instanceGroup_lastSuccessfullyAppliedConfigurationsVersion,
    instanceGroup_bidPrice,
    instanceGroup_requestedInstanceCount,
    instanceGroup_runningInstanceCount,
    instanceGroup_lastSuccessfullyAppliedConfigurations,
    instanceGroup_configurations,
    instanceGroup_customAmiId,
    instanceGroup_instanceGroupType,
    instanceGroup_ebsBlockDevices,
    instanceGroup_instanceType,
    instanceGroup_configurationsVersion,
    instanceGroup_ebsOptimized,
    instanceGroup_market,
    instanceGroup_name,
    instanceGroup_autoScalingPolicy,
    instanceGroup_shrinkPolicy,
    instanceGroup_id,

    -- ** InstanceGroupConfig
    instanceGroupConfig_ebsConfiguration,
    instanceGroupConfig_bidPrice,
    instanceGroupConfig_configurations,
    instanceGroupConfig_customAmiId,
    instanceGroupConfig_market,
    instanceGroupConfig_name,
    instanceGroupConfig_autoScalingPolicy,
    instanceGroupConfig_instanceRole,
    instanceGroupConfig_instanceType,
    instanceGroupConfig_instanceCount,

    -- ** InstanceGroupModifyConfig
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_instanceGroupId,

    -- ** InstanceGroupStateChangeReason
    instanceGroupStateChangeReason_code,
    instanceGroupStateChangeReason_message,

    -- ** InstanceGroupStatus
    instanceGroupStatus_state,
    instanceGroupStatus_stateChangeReason,
    instanceGroupStatus_timeline,

    -- ** InstanceGroupTimeline
    instanceGroupTimeline_readyDateTime,
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_endDateTime,

    -- ** InstanceResizePolicy
    instanceResizePolicy_instancesToProtect,
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instanceTerminationTimeout,

    -- ** InstanceStateChangeReason
    instanceStateChangeReason_code,
    instanceStateChangeReason_message,

    -- ** InstanceStatus
    instanceStatus_state,
    instanceStatus_stateChangeReason,
    instanceStatus_timeline,

    -- ** InstanceTimeline
    instanceTimeline_readyDateTime,
    instanceTimeline_creationDateTime,
    instanceTimeline_endDateTime,

    -- ** InstanceTypeConfig
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_configurations,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_instanceType,

    -- ** InstanceTypeSpecification
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_weightedCapacity,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,

    -- ** JobFlowInstancesConfig
    jobFlowInstancesConfig_instanceFleets,
    jobFlowInstancesConfig_ec2KeyName,
    jobFlowInstancesConfig_slaveInstanceType,
    jobFlowInstancesConfig_instanceCount,
    jobFlowInstancesConfig_emrManagedSlaveSecurityGroup,
    jobFlowInstancesConfig_additionalSlaveSecurityGroups,
    jobFlowInstancesConfig_ec2SubnetIds,
    jobFlowInstancesConfig_hadoopVersion,
    jobFlowInstancesConfig_additionalMasterSecurityGroups,
    jobFlowInstancesConfig_emrManagedMasterSecurityGroup,
    jobFlowInstancesConfig_ec2SubnetId,
    jobFlowInstancesConfig_masterInstanceType,
    jobFlowInstancesConfig_instanceGroups,
    jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps,
    jobFlowInstancesConfig_serviceAccessSecurityGroup,
    jobFlowInstancesConfig_terminationProtected,
    jobFlowInstancesConfig_placement,

    -- ** KerberosAttributes
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_realm,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_crossRealmTrustPrincipalPassword,
    kerberosAttributes_aDDomainJoinUser,

    -- ** KeyValue
    keyValue_value,
    keyValue_key,

    -- ** ManagedScalingPolicy
    managedScalingPolicy_computeLimits,

    -- ** MetricDimension
    metricDimension_value,
    metricDimension_key,

    -- ** NotebookExecution
    notebookExecution_status,
    notebookExecution_executionEngine,
    notebookExecution_notebookInstanceSecurityGroupId,
    notebookExecution_editorId,
    notebookExecution_startTime,
    notebookExecution_arn,
    notebookExecution_outputNotebookURI,
    notebookExecution_notebookExecutionId,
    notebookExecution_notebookExecutionName,
    notebookExecution_lastStateChangeReason,
    notebookExecution_endTime,
    notebookExecution_notebookParams,
    notebookExecution_tags,

    -- ** NotebookExecutionSummary
    notebookExecutionSummary_status,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_endTime,

    -- ** OnDemandCapacityReservationOptions
    onDemandCapacityReservationOptions_usageStrategy,
    onDemandCapacityReservationOptions_capacityReservationResourceGroupArn,
    onDemandCapacityReservationOptions_capacityReservationPreference,

    -- ** OnDemandProvisioningSpecification
    onDemandProvisioningSpecification_capacityReservationOptions,
    onDemandProvisioningSpecification_allocationStrategy,

    -- ** PlacementGroupConfig
    placementGroupConfig_placementStrategy,
    placementGroupConfig_instanceRole,

    -- ** PlacementType
    placementType_availabilityZones,
    placementType_availabilityZone,

    -- ** PortRange
    portRange_maxRange,
    portRange_minRange,

    -- ** ReleaseLabelFilter
    releaseLabelFilter_application,
    releaseLabelFilter_prefix,

    -- ** ScalingAction
    scalingAction_market,
    scalingAction_simpleScalingPolicyConfiguration,

    -- ** ScalingConstraints
    scalingConstraints_minCapacity,
    scalingConstraints_maxCapacity,

    -- ** ScalingRule
    scalingRule_description,
    scalingRule_name,
    scalingRule_action,
    scalingRule_trigger,

    -- ** ScalingTrigger
    scalingTrigger_cloudWatchAlarmDefinition,

    -- ** ScriptBootstrapActionConfig
    scriptBootstrapActionConfig_args,
    scriptBootstrapActionConfig_path,

    -- ** SecurityConfigurationSummary
    securityConfigurationSummary_name,
    securityConfigurationSummary_creationDateTime,

    -- ** SessionMappingDetail
    sessionMappingDetail_creationTime,
    sessionMappingDetail_studioId,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_identityType,
    sessionMappingDetail_identityId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_identityName,

    -- ** SessionMappingSummary
    sessionMappingSummary_creationTime,
    sessionMappingSummary_studioId,
    sessionMappingSummary_identityType,
    sessionMappingSummary_identityId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_identityName,

    -- ** ShrinkPolicy
    shrinkPolicy_decommissionTimeout,
    shrinkPolicy_instanceResizePolicy,

    -- ** SimpleScalingPolicyConfiguration
    simpleScalingPolicyConfiguration_adjustmentType,
    simpleScalingPolicyConfiguration_coolDown,
    simpleScalingPolicyConfiguration_scalingAdjustment,

    -- ** SimplifiedApplication
    simplifiedApplication_name,
    simplifiedApplication_version,

    -- ** SpotProvisioningSpecification
    spotProvisioningSpecification_blockDurationMinutes,
    spotProvisioningSpecification_allocationStrategy,
    spotProvisioningSpecification_timeoutDurationMinutes,
    spotProvisioningSpecification_timeoutAction,

    -- ** Step
    step_status,
    step_actionOnFailure,
    step_config,
    step_name,
    step_id,

    -- ** StepConfig
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- ** StepStateChangeReason
    stepStateChangeReason_code,
    stepStateChangeReason_message,

    -- ** StepStatus
    stepStatus_state,
    stepStatus_failureDetails,
    stepStatus_stateChangeReason,
    stepStatus_timeline,

    -- ** StepSummary
    stepSummary_status,
    stepSummary_actionOnFailure,
    stepSummary_config,
    stepSummary_name,
    stepSummary_id,

    -- ** StepTimeline
    stepTimeline_creationDateTime,
    stepTimeline_endDateTime,
    stepTimeline_startDateTime,

    -- ** Studio
    studio_creationTime,
    studio_engineSecurityGroupId,
    studio_subnetIds,
    studio_studioId,
    studio_vpcId,
    studio_url,
    studio_authMode,
    studio_defaultS3Location,
    studio_idpAuthUrl,
    studio_workspaceSecurityGroupId,
    studio_name,
    studio_idpRelayStateParameterName,
    studio_studioArn,
    studio_userRole,
    studio_description,
    studio_tags,
    studio_serviceRole,

    -- ** StudioSummary
    studioSummary_creationTime,
    studioSummary_studioId,
    studioSummary_vpcId,
    studioSummary_url,
    studioSummary_authMode,
    studioSummary_name,
    studioSummary_description,

    -- ** SupportedProductConfig
    supportedProductConfig_args,
    supportedProductConfig_name,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VolumeSpecification
    volumeSpecification_iops,
    volumeSpecification_volumeType,
    volumeSpecification_sizeInGB,
  )
where

import Network.AWS.EMR.AddInstanceFleet
import Network.AWS.EMR.AddInstanceGroups
import Network.AWS.EMR.AddJobFlowSteps
import Network.AWS.EMR.AddTags
import Network.AWS.EMR.CancelSteps
import Network.AWS.EMR.CreateSecurityConfiguration
import Network.AWS.EMR.CreateStudio
import Network.AWS.EMR.CreateStudioSessionMapping
import Network.AWS.EMR.DeleteSecurityConfiguration
import Network.AWS.EMR.DeleteStudio
import Network.AWS.EMR.DeleteStudioSessionMapping
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeNotebookExecution
import Network.AWS.EMR.DescribeReleaseLabel
import Network.AWS.EMR.DescribeSecurityConfiguration
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.DescribeStudio
import Network.AWS.EMR.GetAutoTerminationPolicy
import Network.AWS.EMR.GetBlockPublicAccessConfiguration
import Network.AWS.EMR.GetManagedScalingPolicy
import Network.AWS.EMR.GetStudioSessionMapping
import Network.AWS.EMR.ListBootstrapActions
import Network.AWS.EMR.ListClusters
import Network.AWS.EMR.ListInstanceFleets
import Network.AWS.EMR.ListInstanceGroups
import Network.AWS.EMR.ListInstances
import Network.AWS.EMR.ListNotebookExecutions
import Network.AWS.EMR.ListReleaseLabels
import Network.AWS.EMR.ListSecurityConfigurations
import Network.AWS.EMR.ListSteps
import Network.AWS.EMR.ListStudioSessionMappings
import Network.AWS.EMR.ListStudios
import Network.AWS.EMR.ModifyCluster
import Network.AWS.EMR.ModifyInstanceFleet
import Network.AWS.EMR.ModifyInstanceGroups
import Network.AWS.EMR.PutAutoScalingPolicy
import Network.AWS.EMR.PutAutoTerminationPolicy
import Network.AWS.EMR.PutBlockPublicAccessConfiguration
import Network.AWS.EMR.PutManagedScalingPolicy
import Network.AWS.EMR.RemoveAutoScalingPolicy
import Network.AWS.EMR.RemoveAutoTerminationPolicy
import Network.AWS.EMR.RemoveManagedScalingPolicy
import Network.AWS.EMR.RemoveTags
import Network.AWS.EMR.RunJobFlow
import Network.AWS.EMR.SetTerminationProtection
import Network.AWS.EMR.SetVisibleToAllUsers
import Network.AWS.EMR.StartNotebookExecution
import Network.AWS.EMR.StopNotebookExecution
import Network.AWS.EMR.TerminateJobFlows
import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.AutoTerminationPolicy
import Network.AWS.EMR.Types.BlockPublicAccessConfiguration
import Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
import Network.AWS.EMR.Types.BootstrapActionConfig
import Network.AWS.EMR.Types.CancelStepsInfo
import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
import Network.AWS.EMR.Types.Cluster
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.ClusterSummary
import Network.AWS.EMR.Types.ClusterTimeline
import Network.AWS.EMR.Types.Command
import Network.AWS.EMR.Types.ComputeLimits
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsBlockDevice
import Network.AWS.EMR.Types.EbsBlockDeviceConfig
import Network.AWS.EMR.Types.EbsConfiguration
import Network.AWS.EMR.Types.EbsVolume
import Network.AWS.EMR.Types.Ec2InstanceAttributes
import Network.AWS.EMR.Types.ExecutionEngineConfig
import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.HadoopJarStepConfig
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.Instance
import Network.AWS.EMR.Types.InstanceFleet
import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceFleetModifyConfig
import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
import Network.AWS.EMR.Types.InstanceFleetStatus
import Network.AWS.EMR.Types.InstanceFleetTimeline
import Network.AWS.EMR.Types.InstanceGroup
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.InstanceGroupModifyConfig
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupTimeline
import Network.AWS.EMR.Types.InstanceResizePolicy
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.InstanceTimeline
import Network.AWS.EMR.Types.InstanceTypeConfig
import Network.AWS.EMR.Types.InstanceTypeSpecification
import Network.AWS.EMR.Types.JobFlowInstancesConfig
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.KeyValue
import Network.AWS.EMR.Types.ManagedScalingPolicy
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.NotebookExecution
import Network.AWS.EMR.Types.NotebookExecutionSummary
import Network.AWS.EMR.Types.OnDemandCapacityReservationOptions
import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.PlacementType
import Network.AWS.EMR.Types.PortRange
import Network.AWS.EMR.Types.ReleaseLabelFilter
import Network.AWS.EMR.Types.ScalingAction
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import Network.AWS.EMR.Types.ScalingTrigger
import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
import Network.AWS.EMR.Types.SecurityConfigurationSummary
import Network.AWS.EMR.Types.SessionMappingDetail
import Network.AWS.EMR.Types.SessionMappingSummary
import Network.AWS.EMR.Types.ShrinkPolicy
import Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
import Network.AWS.EMR.Types.SimplifiedApplication
import Network.AWS.EMR.Types.SpotProvisioningSpecification
import Network.AWS.EMR.Types.Step
import Network.AWS.EMR.Types.StepConfig
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepStatus
import Network.AWS.EMR.Types.StepSummary
import Network.AWS.EMR.Types.StepTimeline
import Network.AWS.EMR.Types.Studio
import Network.AWS.EMR.Types.StudioSummary
import Network.AWS.EMR.Types.SupportedProductConfig
import Network.AWS.EMR.Types.Tag
import Network.AWS.EMR.Types.VolumeSpecification
import Network.AWS.EMR.UpdateStudio
import Network.AWS.EMR.UpdateStudioSessionMapping
