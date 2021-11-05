{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Lens
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

import Amazonka.EMR.AddInstanceFleet
import Amazonka.EMR.AddInstanceGroups
import Amazonka.EMR.AddJobFlowSteps
import Amazonka.EMR.AddTags
import Amazonka.EMR.CancelSteps
import Amazonka.EMR.CreateSecurityConfiguration
import Amazonka.EMR.CreateStudio
import Amazonka.EMR.CreateStudioSessionMapping
import Amazonka.EMR.DeleteSecurityConfiguration
import Amazonka.EMR.DeleteStudio
import Amazonka.EMR.DeleteStudioSessionMapping
import Amazonka.EMR.DescribeCluster
import Amazonka.EMR.DescribeNotebookExecution
import Amazonka.EMR.DescribeReleaseLabel
import Amazonka.EMR.DescribeSecurityConfiguration
import Amazonka.EMR.DescribeStep
import Amazonka.EMR.DescribeStudio
import Amazonka.EMR.GetAutoTerminationPolicy
import Amazonka.EMR.GetBlockPublicAccessConfiguration
import Amazonka.EMR.GetManagedScalingPolicy
import Amazonka.EMR.GetStudioSessionMapping
import Amazonka.EMR.ListBootstrapActions
import Amazonka.EMR.ListClusters
import Amazonka.EMR.ListInstanceFleets
import Amazonka.EMR.ListInstanceGroups
import Amazonka.EMR.ListInstances
import Amazonka.EMR.ListNotebookExecutions
import Amazonka.EMR.ListReleaseLabels
import Amazonka.EMR.ListSecurityConfigurations
import Amazonka.EMR.ListSteps
import Amazonka.EMR.ListStudioSessionMappings
import Amazonka.EMR.ListStudios
import Amazonka.EMR.ModifyCluster
import Amazonka.EMR.ModifyInstanceFleet
import Amazonka.EMR.ModifyInstanceGroups
import Amazonka.EMR.PutAutoScalingPolicy
import Amazonka.EMR.PutAutoTerminationPolicy
import Amazonka.EMR.PutBlockPublicAccessConfiguration
import Amazonka.EMR.PutManagedScalingPolicy
import Amazonka.EMR.RemoveAutoScalingPolicy
import Amazonka.EMR.RemoveAutoTerminationPolicy
import Amazonka.EMR.RemoveManagedScalingPolicy
import Amazonka.EMR.RemoveTags
import Amazonka.EMR.RunJobFlow
import Amazonka.EMR.SetTerminationProtection
import Amazonka.EMR.SetVisibleToAllUsers
import Amazonka.EMR.StartNotebookExecution
import Amazonka.EMR.StopNotebookExecution
import Amazonka.EMR.TerminateJobFlows
import Amazonka.EMR.Types.Application
import Amazonka.EMR.Types.AutoScalingPolicy
import Amazonka.EMR.Types.AutoScalingPolicyDescription
import Amazonka.EMR.Types.AutoScalingPolicyStateChangeReason
import Amazonka.EMR.Types.AutoScalingPolicyStatus
import Amazonka.EMR.Types.AutoTerminationPolicy
import Amazonka.EMR.Types.BlockPublicAccessConfiguration
import Amazonka.EMR.Types.BlockPublicAccessConfigurationMetadata
import Amazonka.EMR.Types.BootstrapActionConfig
import Amazonka.EMR.Types.CancelStepsInfo
import Amazonka.EMR.Types.CloudWatchAlarmDefinition
import Amazonka.EMR.Types.Cluster
import Amazonka.EMR.Types.ClusterStateChangeReason
import Amazonka.EMR.Types.ClusterStatus
import Amazonka.EMR.Types.ClusterSummary
import Amazonka.EMR.Types.ClusterTimeline
import Amazonka.EMR.Types.Command
import Amazonka.EMR.Types.ComputeLimits
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.EbsBlockDevice
import Amazonka.EMR.Types.EbsBlockDeviceConfig
import Amazonka.EMR.Types.EbsConfiguration
import Amazonka.EMR.Types.EbsVolume
import Amazonka.EMR.Types.Ec2InstanceAttributes
import Amazonka.EMR.Types.ExecutionEngineConfig
import Amazonka.EMR.Types.FailureDetails
import Amazonka.EMR.Types.HadoopJarStepConfig
import Amazonka.EMR.Types.HadoopStepConfig
import Amazonka.EMR.Types.Instance
import Amazonka.EMR.Types.InstanceFleet
import Amazonka.EMR.Types.InstanceFleetConfig
import Amazonka.EMR.Types.InstanceFleetModifyConfig
import Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications
import Amazonka.EMR.Types.InstanceFleetStateChangeReason
import Amazonka.EMR.Types.InstanceFleetStatus
import Amazonka.EMR.Types.InstanceFleetTimeline
import Amazonka.EMR.Types.InstanceGroup
import Amazonka.EMR.Types.InstanceGroupConfig
import Amazonka.EMR.Types.InstanceGroupModifyConfig
import Amazonka.EMR.Types.InstanceGroupStateChangeReason
import Amazonka.EMR.Types.InstanceGroupStatus
import Amazonka.EMR.Types.InstanceGroupTimeline
import Amazonka.EMR.Types.InstanceResizePolicy
import Amazonka.EMR.Types.InstanceStateChangeReason
import Amazonka.EMR.Types.InstanceStatus
import Amazonka.EMR.Types.InstanceTimeline
import Amazonka.EMR.Types.InstanceTypeConfig
import Amazonka.EMR.Types.InstanceTypeSpecification
import Amazonka.EMR.Types.JobFlowInstancesConfig
import Amazonka.EMR.Types.KerberosAttributes
import Amazonka.EMR.Types.KeyValue
import Amazonka.EMR.Types.ManagedScalingPolicy
import Amazonka.EMR.Types.MetricDimension
import Amazonka.EMR.Types.NotebookExecution
import Amazonka.EMR.Types.NotebookExecutionSummary
import Amazonka.EMR.Types.OnDemandCapacityReservationOptions
import Amazonka.EMR.Types.OnDemandProvisioningSpecification
import Amazonka.EMR.Types.PlacementGroupConfig
import Amazonka.EMR.Types.PlacementType
import Amazonka.EMR.Types.PortRange
import Amazonka.EMR.Types.ReleaseLabelFilter
import Amazonka.EMR.Types.ScalingAction
import Amazonka.EMR.Types.ScalingConstraints
import Amazonka.EMR.Types.ScalingRule
import Amazonka.EMR.Types.ScalingTrigger
import Amazonka.EMR.Types.ScriptBootstrapActionConfig
import Amazonka.EMR.Types.SecurityConfigurationSummary
import Amazonka.EMR.Types.SessionMappingDetail
import Amazonka.EMR.Types.SessionMappingSummary
import Amazonka.EMR.Types.ShrinkPolicy
import Amazonka.EMR.Types.SimpleScalingPolicyConfiguration
import Amazonka.EMR.Types.SimplifiedApplication
import Amazonka.EMR.Types.SpotProvisioningSpecification
import Amazonka.EMR.Types.Step
import Amazonka.EMR.Types.StepConfig
import Amazonka.EMR.Types.StepStateChangeReason
import Amazonka.EMR.Types.StepStatus
import Amazonka.EMR.Types.StepSummary
import Amazonka.EMR.Types.StepTimeline
import Amazonka.EMR.Types.Studio
import Amazonka.EMR.Types.StudioSummary
import Amazonka.EMR.Types.SupportedProductConfig
import Amazonka.EMR.Types.Tag
import Amazonka.EMR.Types.VolumeSpecification
import Amazonka.EMR.UpdateStudio
import Amazonka.EMR.UpdateStudioSessionMapping
