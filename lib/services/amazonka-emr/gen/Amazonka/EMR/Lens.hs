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

    -- ** AddInstanceFleet
    addInstanceFleet_clusterId,
    addInstanceFleet_instanceFleet,
    addInstanceFleetResponse_clusterArn,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_httpStatus,

    -- ** AddInstanceGroups
    addInstanceGroups_instanceGroups,
    addInstanceGroups_jobFlowId,
    addInstanceGroupsResponse_clusterArn,
    addInstanceGroupsResponse_jobFlowId,
    addInstanceGroupsResponse_instanceGroupIds,
    addInstanceGroupsResponse_httpStatus,

    -- ** AddJobFlowSteps
    addJobFlowSteps_jobFlowId,
    addJobFlowSteps_steps,
    addJobFlowStepsResponse_stepIds,
    addJobFlowStepsResponse_httpStatus,

    -- ** AddTags
    addTags_resourceId,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** CancelSteps
    cancelSteps_stepCancellationOption,
    cancelSteps_clusterId,
    cancelSteps_stepIds,
    cancelStepsResponse_cancelStepsInfoList,
    cancelStepsResponse_httpStatus,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_securityConfiguration,
    createSecurityConfigurationResponse_httpStatus,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_creationDateTime,

    -- ** CreateStudio
    createStudio_tags,
    createStudio_idpRelayStateParameterName,
    createStudio_idpAuthUrl,
    createStudio_description,
    createStudio_userRole,
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

    -- ** CreateStudioSessionMapping
    createStudioSessionMapping_identityName,
    createStudioSessionMapping_identityId,
    createStudioSessionMapping_studioId,
    createStudioSessionMapping_identityType,
    createStudioSessionMapping_sessionPolicyArn,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_studioId,

    -- ** DeleteStudioSessionMapping
    deleteStudioSessionMapping_identityName,
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_studioId,
    deleteStudioSessionMapping_identityType,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_httpStatus,
    describeClusterResponse_cluster,

    -- ** DescribeNotebookExecution
    describeNotebookExecution_notebookExecutionId,
    describeNotebookExecutionResponse_notebookExecution,
    describeNotebookExecutionResponse_httpStatus,

    -- ** DescribeReleaseLabel
    describeReleaseLabel_nextToken,
    describeReleaseLabel_releaseLabel,
    describeReleaseLabel_maxResults,
    describeReleaseLabelResponse_nextToken,
    describeReleaseLabelResponse_applications,
    describeReleaseLabelResponse_releaseLabel,
    describeReleaseLabelResponse_httpStatus,

    -- ** DescribeSecurityConfiguration
    describeSecurityConfiguration_name,
    describeSecurityConfigurationResponse_securityConfiguration,
    describeSecurityConfigurationResponse_name,
    describeSecurityConfigurationResponse_creationDateTime,
    describeSecurityConfigurationResponse_httpStatus,

    -- ** DescribeStep
    describeStep_clusterId,
    describeStep_stepId,
    describeStepResponse_step,
    describeStepResponse_httpStatus,

    -- ** DescribeStudio
    describeStudio_studioId,
    describeStudioResponse_studio,
    describeStudioResponse_httpStatus,

    -- ** GetAutoTerminationPolicy
    getAutoTerminationPolicy_clusterId,
    getAutoTerminationPolicyResponse_autoTerminationPolicy,
    getAutoTerminationPolicyResponse_httpStatus,

    -- ** GetBlockPublicAccessConfiguration
    getBlockPublicAccessConfigurationResponse_httpStatus,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata,

    -- ** GetManagedScalingPolicy
    getManagedScalingPolicy_clusterId,
    getManagedScalingPolicyResponse_managedScalingPolicy,
    getManagedScalingPolicyResponse_httpStatus,

    -- ** GetStudioSessionMapping
    getStudioSessionMapping_identityName,
    getStudioSessionMapping_identityId,
    getStudioSessionMapping_studioId,
    getStudioSessionMapping_identityType,
    getStudioSessionMappingResponse_sessionMapping,
    getStudioSessionMappingResponse_httpStatus,

    -- ** ListBootstrapActions
    listBootstrapActions_marker,
    listBootstrapActions_clusterId,
    listBootstrapActionsResponse_marker,
    listBootstrapActionsResponse_bootstrapActions,
    listBootstrapActionsResponse_httpStatus,

    -- ** ListClusters
    listClusters_clusterStates,
    listClusters_marker,
    listClusters_createdBefore,
    listClusters_createdAfter,
    listClustersResponse_marker,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,

    -- ** ListInstanceFleets
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_httpStatus,

    -- ** ListInstanceGroups
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_httpStatus,

    -- ** ListInstances
    listInstances_marker,
    listInstances_instanceFleetId,
    listInstances_instanceFleetType,
    listInstances_instanceGroupTypes,
    listInstances_instanceGroupId,
    listInstances_instanceStates,
    listInstances_clusterId,
    listInstancesResponse_instances,
    listInstancesResponse_marker,
    listInstancesResponse_httpStatus,

    -- ** ListNotebookExecutions
    listNotebookExecutions_marker,
    listNotebookExecutions_from,
    listNotebookExecutions_to,
    listNotebookExecutions_status,
    listNotebookExecutions_editorId,
    listNotebookExecutionsResponse_marker,
    listNotebookExecutionsResponse_notebookExecutions,
    listNotebookExecutionsResponse_httpStatus,

    -- ** ListReleaseLabels
    listReleaseLabels_nextToken,
    listReleaseLabels_filters,
    listReleaseLabels_maxResults,
    listReleaseLabelsResponse_nextToken,
    listReleaseLabelsResponse_releaseLabels,
    listReleaseLabelsResponse_httpStatus,

    -- ** ListSecurityConfigurations
    listSecurityConfigurations_marker,
    listSecurityConfigurationsResponse_marker,
    listSecurityConfigurationsResponse_securityConfigurations,
    listSecurityConfigurationsResponse_httpStatus,

    -- ** ListSteps
    listSteps_marker,
    listSteps_stepStates,
    listSteps_stepIds,
    listSteps_clusterId,
    listStepsResponse_marker,
    listStepsResponse_steps,
    listStepsResponse_httpStatus,

    -- ** ListStudioSessionMappings
    listStudioSessionMappings_studioId,
    listStudioSessionMappings_marker,
    listStudioSessionMappings_identityType,
    listStudioSessionMappingsResponse_sessionMappings,
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_httpStatus,

    -- ** ListStudios
    listStudios_marker,
    listStudiosResponse_marker,
    listStudiosResponse_studios,
    listStudiosResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_stepConcurrencyLevel,
    modifyCluster_clusterId,
    modifyClusterResponse_stepConcurrencyLevel,
    modifyClusterResponse_httpStatus,

    -- ** ModifyInstanceFleet
    modifyInstanceFleet_clusterId,
    modifyInstanceFleet_instanceFleet,

    -- ** ModifyInstanceGroups
    modifyInstanceGroups_clusterId,
    modifyInstanceGroups_instanceGroups,

    -- ** PutAutoScalingPolicy
    putAutoScalingPolicy_clusterId,
    putAutoScalingPolicy_instanceGroupId,
    putAutoScalingPolicy_autoScalingPolicy,
    putAutoScalingPolicyResponse_clusterArn,
    putAutoScalingPolicyResponse_clusterId,
    putAutoScalingPolicyResponse_autoScalingPolicy,
    putAutoScalingPolicyResponse_instanceGroupId,
    putAutoScalingPolicyResponse_httpStatus,

    -- ** PutAutoTerminationPolicy
    putAutoTerminationPolicy_autoTerminationPolicy,
    putAutoTerminationPolicy_clusterId,
    putAutoTerminationPolicyResponse_httpStatus,

    -- ** PutBlockPublicAccessConfiguration
    putBlockPublicAccessConfiguration_blockPublicAccessConfiguration,
    putBlockPublicAccessConfigurationResponse_httpStatus,

    -- ** PutManagedScalingPolicy
    putManagedScalingPolicy_clusterId,
    putManagedScalingPolicy_managedScalingPolicy,
    putManagedScalingPolicyResponse_httpStatus,

    -- ** RemoveAutoScalingPolicy
    removeAutoScalingPolicy_clusterId,
    removeAutoScalingPolicy_instanceGroupId,
    removeAutoScalingPolicyResponse_httpStatus,

    -- ** RemoveAutoTerminationPolicy
    removeAutoTerminationPolicy_clusterId,
    removeAutoTerminationPolicyResponse_httpStatus,

    -- ** RemoveManagedScalingPolicy
    removeManagedScalingPolicy_clusterId,
    removeManagedScalingPolicyResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceId,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** RunJobFlow
    runJobFlow_securityConfiguration,
    runJobFlow_tags,
    runJobFlow_amiVersion,
    runJobFlow_placementGroupConfigs,
    runJobFlow_managedScalingPolicy,
    runJobFlow_additionalInfo,
    runJobFlow_supportedProducts,
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_jobFlowRole,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_applications,
    runJobFlow_releaseLabel,
    runJobFlow_autoScalingRole,
    runJobFlow_scaleDownBehavior,
    runJobFlow_steps,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_autoTerminationPolicy,
    runJobFlow_serviceRole,
    runJobFlow_configurations,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_newSupportedProducts,
    runJobFlow_logUri,
    runJobFlow_visibleToAllUsers,
    runJobFlow_customAmiId,
    runJobFlow_kerberosAttributes,
    runJobFlow_bootstrapActions,
    runJobFlow_name,
    runJobFlow_instances,
    runJobFlowResponse_clusterArn,
    runJobFlowResponse_jobFlowId,
    runJobFlowResponse_httpStatus,

    -- ** SetTerminationProtection
    setTerminationProtection_jobFlowIds,
    setTerminationProtection_terminationProtected,

    -- ** SetVisibleToAllUsers
    setVisibleToAllUsers_jobFlowIds,
    setVisibleToAllUsers_visibleToAllUsers,

    -- ** StartNotebookExecution
    startNotebookExecution_tags,
    startNotebookExecution_notebookInstanceSecurityGroupId,
    startNotebookExecution_notebookExecutionName,
    startNotebookExecution_notebookParams,
    startNotebookExecution_editorId,
    startNotebookExecution_relativePath,
    startNotebookExecution_executionEngine,
    startNotebookExecution_serviceRole,
    startNotebookExecutionResponse_notebookExecutionId,
    startNotebookExecutionResponse_httpStatus,

    -- ** StopNotebookExecution
    stopNotebookExecution_notebookExecutionId,

    -- ** TerminateJobFlows
    terminateJobFlows_jobFlowIds,

    -- ** UpdateStudio
    updateStudio_name,
    updateStudio_description,
    updateStudio_defaultS3Location,
    updateStudio_subnetIds,
    updateStudio_studioId,

    -- ** UpdateStudioSessionMapping
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- * Types

    -- ** Application
    application_name,
    application_additionalInfo,
    application_args,
    application_version,

    -- ** AutoScalingPolicy
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_status,

    -- ** AutoScalingPolicyStateChangeReason
    autoScalingPolicyStateChangeReason_message,
    autoScalingPolicyStateChangeReason_code,

    -- ** AutoScalingPolicyStatus
    autoScalingPolicyStatus_stateChangeReason,
    autoScalingPolicyStatus_state,

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
    cancelStepsInfo_reason,
    cancelStepsInfo_stepId,

    -- ** CloudWatchAlarmDefinition
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- ** Cluster
    cluster_clusterArn,
    cluster_securityConfiguration,
    cluster_tags,
    cluster_outpostArn,
    cluster_instanceCollectionType,
    cluster_runningAmiVersion,
    cluster_logEncryptionKmsKeyId,
    cluster_ebsRootVolumeSize,
    cluster_applications,
    cluster_requestedAmiVersion,
    cluster_releaseLabel,
    cluster_autoScalingRole,
    cluster_scaleDownBehavior,
    cluster_terminationProtected,
    cluster_repoUpgradeOnBoot,
    cluster_serviceRole,
    cluster_configurations,
    cluster_autoTerminate,
    cluster_stepConcurrencyLevel,
    cluster_logUri,
    cluster_visibleToAllUsers,
    cluster_masterPublicDnsName,
    cluster_customAmiId,
    cluster_kerberosAttributes,
    cluster_normalizedInstanceHours,
    cluster_ec2InstanceAttributes,
    cluster_placementGroups,
    cluster_id,
    cluster_name,
    cluster_status,

    -- ** ClusterStateChangeReason
    clusterStateChangeReason_message,
    clusterStateChangeReason_code,

    -- ** ClusterStatus
    clusterStatus_stateChangeReason,
    clusterStatus_timeline,
    clusterStatus_state,

    -- ** ClusterSummary
    clusterSummary_clusterArn,
    clusterSummary_name,
    clusterSummary_outpostArn,
    clusterSummary_status,
    clusterSummary_id,
    clusterSummary_normalizedInstanceHours,

    -- ** ClusterTimeline
    clusterTimeline_creationDateTime,
    clusterTimeline_readyDateTime,
    clusterTimeline_endDateTime,

    -- ** Command
    command_name,
    command_scriptPath,
    command_args,

    -- ** ComputeLimits
    computeLimits_maximumOnDemandCapacityUnits,
    computeLimits_maximumCoreCapacityUnits,
    computeLimits_unitType,
    computeLimits_minimumCapacityUnits,
    computeLimits_maximumCapacityUnits,

    -- ** Configuration
    configuration_properties,
    configuration_configurations,
    configuration_classification,

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
    ec2InstanceAttributes_iamInstanceProfile,
    ec2InstanceAttributes_ec2KeyName,
    ec2InstanceAttributes_ec2AvailabilityZone,
    ec2InstanceAttributes_ec2SubnetId,
    ec2InstanceAttributes_requestedEc2AvailabilityZones,
    ec2InstanceAttributes_emrManagedMasterSecurityGroup,
    ec2InstanceAttributes_additionalMasterSecurityGroups,
    ec2InstanceAttributes_serviceAccessSecurityGroup,
    ec2InstanceAttributes_additionalSlaveSecurityGroups,
    ec2InstanceAttributes_requestedEc2SubnetIds,
    ec2InstanceAttributes_emrManagedSlaveSecurityGroup,

    -- ** ExecutionEngineConfig
    executionEngineConfig_type,
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_id,

    -- ** FailureDetails
    failureDetails_message,
    failureDetails_reason,
    failureDetails_logFile,

    -- ** HadoopJarStepConfig
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_jar,

    -- ** HadoopStepConfig
    hadoopStepConfig_mainClass,
    hadoopStepConfig_properties,
    hadoopStepConfig_jar,
    hadoopStepConfig_args,

    -- ** Instance
    instance_ebsVolumes,
    instance_ec2InstanceId,
    instance_instanceFleetId,
    instance_status,
    instance_id,
    instance_instanceType,
    instance_publicIpAddress,
    instance_publicDnsName,
    instance_market,
    instance_privateIpAddress,
    instance_privateDnsName,
    instance_instanceGroupId,

    -- ** InstanceFleet
    instanceFleet_name,
    instanceFleet_targetOnDemandCapacity,
    instanceFleet_provisionedSpotCapacity,
    instanceFleet_status,
    instanceFleet_launchSpecifications,
    instanceFleet_id,
    instanceFleet_instanceFleetType,
    instanceFleet_instanceTypeSpecifications,
    instanceFleet_provisionedOnDemandCapacity,
    instanceFleet_targetSpotCapacity,

    -- ** InstanceFleetConfig
    instanceFleetConfig_name,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_instanceFleetType,

    -- ** InstanceFleetModifyConfig
    instanceFleetModifyConfig_targetOnDemandCapacity,
    instanceFleetModifyConfig_targetSpotCapacity,
    instanceFleetModifyConfig_instanceFleetId,

    -- ** InstanceFleetProvisioningSpecifications
    instanceFleetProvisioningSpecifications_spotSpecification,
    instanceFleetProvisioningSpecifications_onDemandSpecification,

    -- ** InstanceFleetStateChangeReason
    instanceFleetStateChangeReason_message,
    instanceFleetStateChangeReason_code,

    -- ** InstanceFleetStatus
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_timeline,
    instanceFleetStatus_state,

    -- ** InstanceFleetTimeline
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_readyDateTime,
    instanceFleetTimeline_endDateTime,

    -- ** InstanceGroup
    instanceGroup_ebsOptimized,
    instanceGroup_lastSuccessfullyAppliedConfigurationsVersion,
    instanceGroup_name,
    instanceGroup_requestedInstanceCount,
    instanceGroup_instanceGroupType,
    instanceGroup_shrinkPolicy,
    instanceGroup_runningInstanceCount,
    instanceGroup_status,
    instanceGroup_id,
    instanceGroup_configurations,
    instanceGroup_instanceType,
    instanceGroup_bidPrice,
    instanceGroup_market,
    instanceGroup_customAmiId,
    instanceGroup_autoScalingPolicy,
    instanceGroup_lastSuccessfullyAppliedConfigurations,
    instanceGroup_ebsBlockDevices,
    instanceGroup_configurationsVersion,

    -- ** InstanceGroupConfig
    instanceGroupConfig_name,
    instanceGroupConfig_ebsConfiguration,
    instanceGroupConfig_configurations,
    instanceGroupConfig_bidPrice,
    instanceGroupConfig_market,
    instanceGroupConfig_customAmiId,
    instanceGroupConfig_autoScalingPolicy,
    instanceGroupConfig_instanceRole,
    instanceGroupConfig_instanceType,
    instanceGroupConfig_instanceCount,

    -- ** InstanceGroupModifyConfig
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_instanceGroupId,

    -- ** InstanceGroupStateChangeReason
    instanceGroupStateChangeReason_message,
    instanceGroupStateChangeReason_code,

    -- ** InstanceGroupStatus
    instanceGroupStatus_stateChangeReason,
    instanceGroupStatus_timeline,
    instanceGroupStatus_state,

    -- ** InstanceGroupTimeline
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_readyDateTime,
    instanceGroupTimeline_endDateTime,

    -- ** InstanceResizePolicy
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instanceTerminationTimeout,
    instanceResizePolicy_instancesToProtect,

    -- ** InstanceStateChangeReason
    instanceStateChangeReason_message,
    instanceStateChangeReason_code,

    -- ** InstanceStatus
    instanceStatus_stateChangeReason,
    instanceStatus_timeline,
    instanceStatus_state,

    -- ** InstanceTimeline
    instanceTimeline_creationDateTime,
    instanceTimeline_readyDateTime,
    instanceTimeline_endDateTime,

    -- ** InstanceTypeConfig
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_configurations,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- ** InstanceTypeSpecification
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_weightedCapacity,
    instanceTypeSpecification_ebsBlockDevices,

    -- ** JobFlowInstancesConfig
    jobFlowInstancesConfig_ec2SubnetIds,
    jobFlowInstancesConfig_ec2KeyName,
    jobFlowInstancesConfig_placement,
    jobFlowInstancesConfig_ec2SubnetId,
    jobFlowInstancesConfig_hadoopVersion,
    jobFlowInstancesConfig_emrManagedMasterSecurityGroup,
    jobFlowInstancesConfig_additionalMasterSecurityGroups,
    jobFlowInstancesConfig_serviceAccessSecurityGroup,
    jobFlowInstancesConfig_terminationProtected,
    jobFlowInstancesConfig_slaveInstanceType,
    jobFlowInstancesConfig_instanceCount,
    jobFlowInstancesConfig_instanceFleets,
    jobFlowInstancesConfig_additionalSlaveSecurityGroups,
    jobFlowInstancesConfig_instanceGroups,
    jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps,
    jobFlowInstancesConfig_emrManagedSlaveSecurityGroup,
    jobFlowInstancesConfig_masterInstanceType,

    -- ** KerberosAttributes
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_realm,
    kerberosAttributes_crossRealmTrustPrincipalPassword,

    -- ** KeyValue
    keyValue_key,
    keyValue_value,

    -- ** ManagedScalingPolicy
    managedScalingPolicy_computeLimits,

    -- ** MetricDimension
    metricDimension_key,
    metricDimension_value,

    -- ** NotebookExecution
    notebookExecution_tags,
    notebookExecution_lastStateChangeReason,
    notebookExecution_executionEngine,
    notebookExecution_notebookInstanceSecurityGroupId,
    notebookExecution_arn,
    notebookExecution_status,
    notebookExecution_outputNotebookURI,
    notebookExecution_notebookExecutionName,
    notebookExecution_endTime,
    notebookExecution_editorId,
    notebookExecution_notebookParams,
    notebookExecution_startTime,
    notebookExecution_notebookExecutionId,

    -- ** NotebookExecutionSummary
    notebookExecutionSummary_status,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_endTime,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_notebookExecutionId,

    -- ** OnDemandCapacityReservationOptions
    onDemandCapacityReservationOptions_capacityReservationPreference,
    onDemandCapacityReservationOptions_capacityReservationResourceGroupArn,
    onDemandCapacityReservationOptions_usageStrategy,

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
    sessionMappingDetail_studioId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_identityName,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_creationTime,
    sessionMappingDetail_identityId,
    sessionMappingDetail_identityType,

    -- ** SessionMappingSummary
    sessionMappingSummary_studioId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_identityName,
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityId,
    sessionMappingSummary_identityType,

    -- ** ShrinkPolicy
    shrinkPolicy_instanceResizePolicy,
    shrinkPolicy_decommissionTimeout,

    -- ** SimpleScalingPolicyConfiguration
    simpleScalingPolicyConfiguration_coolDown,
    simpleScalingPolicyConfiguration_adjustmentType,
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
    step_name,
    step_status,
    step_id,
    step_actionOnFailure,
    step_config,

    -- ** StepConfig
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- ** StepStateChangeReason
    stepStateChangeReason_message,
    stepStateChangeReason_code,

    -- ** StepStatus
    stepStatus_stateChangeReason,
    stepStatus_timeline,
    stepStatus_state,
    stepStatus_failureDetails,

    -- ** StepSummary
    stepSummary_name,
    stepSummary_status,
    stepSummary_id,
    stepSummary_actionOnFailure,
    stepSummary_config,

    -- ** StepTimeline
    stepTimeline_creationDateTime,
    stepTimeline_startDateTime,
    stepTimeline_endDateTime,

    -- ** Studio
    studio_tags,
    studio_studioId,
    studio_name,
    studio_workspaceSecurityGroupId,
    studio_idpRelayStateParameterName,
    studio_idpAuthUrl,
    studio_url,
    studio_description,
    studio_serviceRole,
    studio_authMode,
    studio_studioArn,
    studio_creationTime,
    studio_userRole,
    studio_vpcId,
    studio_engineSecurityGroupId,
    studio_defaultS3Location,
    studio_subnetIds,

    -- ** StudioSummary
    studioSummary_studioId,
    studioSummary_name,
    studioSummary_url,
    studioSummary_description,
    studioSummary_authMode,
    studioSummary_creationTime,
    studioSummary_vpcId,

    -- ** SupportedProductConfig
    supportedProductConfig_name,
    supportedProductConfig_args,

    -- ** Tag
    tag_key,
    tag_value,

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
