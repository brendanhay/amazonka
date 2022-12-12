{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_httpStatus,

    -- ** AddInstanceGroups
    addInstanceGroups_instanceGroups,
    addInstanceGroups_jobFlowId,
    addInstanceGroupsResponse_clusterArn,
    addInstanceGroupsResponse_instanceGroupIds,
    addInstanceGroupsResponse_jobFlowId,
    addInstanceGroupsResponse_httpStatus,

    -- ** AddJobFlowSteps
    addJobFlowSteps_executionRoleArn,
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
    createStudio_description,
    createStudio_idpAuthUrl,
    createStudio_idpRelayStateParameterName,
    createStudio_tags,
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
    createStudioSessionMapping_identityId,
    createStudioSessionMapping_identityName,
    createStudioSessionMapping_studioId,
    createStudioSessionMapping_identityType,
    createStudioSessionMapping_sessionPolicyArn,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_studioId,

    -- ** DeleteStudioSessionMapping
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_identityName,
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
    describeReleaseLabel_maxResults,
    describeReleaseLabel_nextToken,
    describeReleaseLabel_releaseLabel,
    describeReleaseLabelResponse_applications,
    describeReleaseLabelResponse_availableOSReleases,
    describeReleaseLabelResponse_nextToken,
    describeReleaseLabelResponse_releaseLabel,
    describeReleaseLabelResponse_httpStatus,

    -- ** DescribeSecurityConfiguration
    describeSecurityConfiguration_name,
    describeSecurityConfigurationResponse_creationDateTime,
    describeSecurityConfigurationResponse_name,
    describeSecurityConfigurationResponse_securityConfiguration,
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
    getStudioSessionMapping_identityId,
    getStudioSessionMapping_identityName,
    getStudioSessionMapping_studioId,
    getStudioSessionMapping_identityType,
    getStudioSessionMappingResponse_sessionMapping,
    getStudioSessionMappingResponse_httpStatus,

    -- ** ListBootstrapActions
    listBootstrapActions_marker,
    listBootstrapActions_clusterId,
    listBootstrapActionsResponse_bootstrapActions,
    listBootstrapActionsResponse_marker,
    listBootstrapActionsResponse_httpStatus,

    -- ** ListClusters
    listClusters_clusterStates,
    listClusters_createdAfter,
    listClusters_createdBefore,
    listClusters_marker,
    listClustersResponse_clusters,
    listClustersResponse_marker,
    listClustersResponse_httpStatus,

    -- ** ListInstanceFleets
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_httpStatus,

    -- ** ListInstanceGroups
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_httpStatus,

    -- ** ListInstances
    listInstances_instanceFleetId,
    listInstances_instanceFleetType,
    listInstances_instanceGroupId,
    listInstances_instanceGroupTypes,
    listInstances_instanceStates,
    listInstances_marker,
    listInstances_clusterId,
    listInstancesResponse_instances,
    listInstancesResponse_marker,
    listInstancesResponse_httpStatus,

    -- ** ListNotebookExecutions
    listNotebookExecutions_editorId,
    listNotebookExecutions_from,
    listNotebookExecutions_marker,
    listNotebookExecutions_status,
    listNotebookExecutions_to,
    listNotebookExecutionsResponse_marker,
    listNotebookExecutionsResponse_notebookExecutions,
    listNotebookExecutionsResponse_httpStatus,

    -- ** ListReleaseLabels
    listReleaseLabels_filters,
    listReleaseLabels_maxResults,
    listReleaseLabels_nextToken,
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
    listSteps_stepIds,
    listSteps_stepStates,
    listSteps_clusterId,
    listStepsResponse_marker,
    listStepsResponse_steps,
    listStepsResponse_httpStatus,

    -- ** ListStudioSessionMappings
    listStudioSessionMappings_identityType,
    listStudioSessionMappings_marker,
    listStudioSessionMappings_studioId,
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_sessionMappings,
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
    putAutoScalingPolicyResponse_autoScalingPolicy,
    putAutoScalingPolicyResponse_clusterArn,
    putAutoScalingPolicyResponse_clusterId,
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
    runJobFlow_additionalInfo,
    runJobFlow_amiVersion,
    runJobFlow_applications,
    runJobFlow_autoScalingRole,
    runJobFlow_autoTerminationPolicy,
    runJobFlow_bootstrapActions,
    runJobFlow_configurations,
    runJobFlow_customAmiId,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_jobFlowRole,
    runJobFlow_kerberosAttributes,
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_logUri,
    runJobFlow_managedScalingPolicy,
    runJobFlow_newSupportedProducts,
    runJobFlow_oSReleaseLabel,
    runJobFlow_placementGroupConfigs,
    runJobFlow_releaseLabel,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_scaleDownBehavior,
    runJobFlow_securityConfiguration,
    runJobFlow_serviceRole,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_steps,
    runJobFlow_supportedProducts,
    runJobFlow_tags,
    runJobFlow_visibleToAllUsers,
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
    startNotebookExecution_notebookExecutionName,
    startNotebookExecution_notebookInstanceSecurityGroupId,
    startNotebookExecution_notebookParams,
    startNotebookExecution_tags,
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
    updateStudio_defaultS3Location,
    updateStudio_description,
    updateStudio_name,
    updateStudio_subnetIds,
    updateStudio_studioId,

    -- ** UpdateStudioSessionMapping
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- * Types

    -- ** Application
    application_additionalInfo,
    application_args,
    application_name,
    application_version,

    -- ** AutoScalingPolicy
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_status,

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
    cancelStepsInfo_reason,
    cancelStepsInfo_status,
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
    cluster_applications,
    cluster_autoScalingRole,
    cluster_autoTerminate,
    cluster_clusterArn,
    cluster_configurations,
    cluster_customAmiId,
    cluster_ebsRootVolumeSize,
    cluster_ec2InstanceAttributes,
    cluster_instanceCollectionType,
    cluster_kerberosAttributes,
    cluster_logEncryptionKmsKeyId,
    cluster_logUri,
    cluster_masterPublicDnsName,
    cluster_normalizedInstanceHours,
    cluster_oSReleaseLabel,
    cluster_outpostArn,
    cluster_placementGroups,
    cluster_releaseLabel,
    cluster_repoUpgradeOnBoot,
    cluster_requestedAmiVersion,
    cluster_runningAmiVersion,
    cluster_scaleDownBehavior,
    cluster_securityConfiguration,
    cluster_serviceRole,
    cluster_stepConcurrencyLevel,
    cluster_tags,
    cluster_terminationProtected,
    cluster_visibleToAllUsers,
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
    clusterSummary_clusterArn,
    clusterSummary_id,
    clusterSummary_name,
    clusterSummary_normalizedInstanceHours,
    clusterSummary_outpostArn,
    clusterSummary_status,

    -- ** ClusterTimeline
    clusterTimeline_creationDateTime,
    clusterTimeline_endDateTime,
    clusterTimeline_readyDateTime,

    -- ** Command
    command_args,
    command_name,
    command_scriptPath,

    -- ** ComputeLimits
    computeLimits_maximumCoreCapacityUnits,
    computeLimits_maximumOnDemandCapacityUnits,
    computeLimits_unitType,
    computeLimits_minimumCapacityUnits,
    computeLimits_maximumCapacityUnits,

    -- ** Configuration
    configuration_classification,
    configuration_configurations,
    configuration_properties,

    -- ** EbsBlockDevice
    ebsBlockDevice_device,
    ebsBlockDevice_volumeSpecification,

    -- ** EbsBlockDeviceConfig
    ebsBlockDeviceConfig_volumesPerInstance,
    ebsBlockDeviceConfig_volumeSpecification,

    -- ** EbsConfiguration
    ebsConfiguration_ebsBlockDeviceConfigs,
    ebsConfiguration_ebsOptimized,

    -- ** EbsVolume
    ebsVolume_device,
    ebsVolume_volumeId,

    -- ** Ec2InstanceAttributes
    ec2InstanceAttributes_additionalMasterSecurityGroups,
    ec2InstanceAttributes_additionalSlaveSecurityGroups,
    ec2InstanceAttributes_ec2AvailabilityZone,
    ec2InstanceAttributes_ec2KeyName,
    ec2InstanceAttributes_ec2SubnetId,
    ec2InstanceAttributes_emrManagedMasterSecurityGroup,
    ec2InstanceAttributes_emrManagedSlaveSecurityGroup,
    ec2InstanceAttributes_iamInstanceProfile,
    ec2InstanceAttributes_requestedEc2AvailabilityZones,
    ec2InstanceAttributes_requestedEc2SubnetIds,
    ec2InstanceAttributes_serviceAccessSecurityGroup,

    -- ** ExecutionEngineConfig
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_type,
    executionEngineConfig_id,

    -- ** FailureDetails
    failureDetails_logFile,
    failureDetails_message,
    failureDetails_reason,

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
    instance_ebsVolumes,
    instance_ec2InstanceId,
    instance_id,
    instance_instanceFleetId,
    instance_instanceGroupId,
    instance_instanceType,
    instance_market,
    instance_privateDnsName,
    instance_privateIpAddress,
    instance_publicDnsName,
    instance_publicIpAddress,
    instance_status,

    -- ** InstanceFleet
    instanceFleet_id,
    instanceFleet_instanceFleetType,
    instanceFleet_instanceTypeSpecifications,
    instanceFleet_launchSpecifications,
    instanceFleet_name,
    instanceFleet_provisionedOnDemandCapacity,
    instanceFleet_provisionedSpotCapacity,
    instanceFleet_status,
    instanceFleet_targetOnDemandCapacity,
    instanceFleet_targetSpotCapacity,

    -- ** InstanceFleetConfig
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_name,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_instanceFleetType,

    -- ** InstanceFleetModifyConfig
    instanceFleetModifyConfig_targetOnDemandCapacity,
    instanceFleetModifyConfig_targetSpotCapacity,
    instanceFleetModifyConfig_instanceFleetId,

    -- ** InstanceFleetProvisioningSpecifications
    instanceFleetProvisioningSpecifications_onDemandSpecification,
    instanceFleetProvisioningSpecifications_spotSpecification,

    -- ** InstanceFleetStateChangeReason
    instanceFleetStateChangeReason_code,
    instanceFleetStateChangeReason_message,

    -- ** InstanceFleetStatus
    instanceFleetStatus_state,
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_timeline,

    -- ** InstanceFleetTimeline
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_endDateTime,
    instanceFleetTimeline_readyDateTime,

    -- ** InstanceGroup
    instanceGroup_autoScalingPolicy,
    instanceGroup_bidPrice,
    instanceGroup_configurations,
    instanceGroup_configurationsVersion,
    instanceGroup_customAmiId,
    instanceGroup_ebsBlockDevices,
    instanceGroup_ebsOptimized,
    instanceGroup_id,
    instanceGroup_instanceGroupType,
    instanceGroup_instanceType,
    instanceGroup_lastSuccessfullyAppliedConfigurations,
    instanceGroup_lastSuccessfullyAppliedConfigurationsVersion,
    instanceGroup_market,
    instanceGroup_name,
    instanceGroup_requestedInstanceCount,
    instanceGroup_runningInstanceCount,
    instanceGroup_shrinkPolicy,
    instanceGroup_status,

    -- ** InstanceGroupConfig
    instanceGroupConfig_autoScalingPolicy,
    instanceGroupConfig_bidPrice,
    instanceGroupConfig_configurations,
    instanceGroupConfig_customAmiId,
    instanceGroupConfig_ebsConfiguration,
    instanceGroupConfig_market,
    instanceGroupConfig_name,
    instanceGroupConfig_instanceRole,
    instanceGroupConfig_instanceType,
    instanceGroupConfig_instanceCount,

    -- ** InstanceGroupModifyConfig
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_reconfigurationType,
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
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_endDateTime,
    instanceGroupTimeline_readyDateTime,

    -- ** InstanceResizePolicy
    instanceResizePolicy_instanceTerminationTimeout,
    instanceResizePolicy_instancesToProtect,
    instanceResizePolicy_instancesToTerminate,

    -- ** InstanceStateChangeReason
    instanceStateChangeReason_code,
    instanceStateChangeReason_message,

    -- ** InstanceStatus
    instanceStatus_state,
    instanceStatus_stateChangeReason,
    instanceStatus_timeline,

    -- ** InstanceTimeline
    instanceTimeline_creationDateTime,
    instanceTimeline_endDateTime,
    instanceTimeline_readyDateTime,

    -- ** InstanceTypeConfig
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_configurations,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- ** InstanceTypeSpecification
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_weightedCapacity,

    -- ** JobFlowInstancesConfig
    jobFlowInstancesConfig_additionalMasterSecurityGroups,
    jobFlowInstancesConfig_additionalSlaveSecurityGroups,
    jobFlowInstancesConfig_ec2KeyName,
    jobFlowInstancesConfig_ec2SubnetId,
    jobFlowInstancesConfig_ec2SubnetIds,
    jobFlowInstancesConfig_emrManagedMasterSecurityGroup,
    jobFlowInstancesConfig_emrManagedSlaveSecurityGroup,
    jobFlowInstancesConfig_hadoopVersion,
    jobFlowInstancesConfig_instanceCount,
    jobFlowInstancesConfig_instanceFleets,
    jobFlowInstancesConfig_instanceGroups,
    jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps,
    jobFlowInstancesConfig_masterInstanceType,
    jobFlowInstancesConfig_placement,
    jobFlowInstancesConfig_serviceAccessSecurityGroup,
    jobFlowInstancesConfig_slaveInstanceType,
    jobFlowInstancesConfig_terminationProtected,

    -- ** KerberosAttributes
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_crossRealmTrustPrincipalPassword,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_realm,

    -- ** KeyValue
    keyValue_key,
    keyValue_value,

    -- ** ManagedScalingPolicy
    managedScalingPolicy_computeLimits,

    -- ** MetricDimension
    metricDimension_key,
    metricDimension_value,

    -- ** NotebookExecution
    notebookExecution_arn,
    notebookExecution_editorId,
    notebookExecution_endTime,
    notebookExecution_executionEngine,
    notebookExecution_lastStateChangeReason,
    notebookExecution_notebookExecutionId,
    notebookExecution_notebookExecutionName,
    notebookExecution_notebookInstanceSecurityGroupId,
    notebookExecution_notebookParams,
    notebookExecution_outputNotebookURI,
    notebookExecution_startTime,
    notebookExecution_status,
    notebookExecution_tags,

    -- ** NotebookExecutionSummary
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_endTime,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_status,

    -- ** OSRelease
    oSRelease_label,

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
    placementType_availabilityZone,
    placementType_availabilityZones,

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
    securityConfigurationSummary_creationDateTime,
    securityConfigurationSummary_name,

    -- ** SessionMappingDetail
    sessionMappingDetail_creationTime,
    sessionMappingDetail_identityId,
    sessionMappingDetail_identityName,
    sessionMappingDetail_identityType,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_studioId,

    -- ** SessionMappingSummary
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityId,
    sessionMappingSummary_identityName,
    sessionMappingSummary_identityType,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_studioId,

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
    spotProvisioningSpecification_allocationStrategy,
    spotProvisioningSpecification_blockDurationMinutes,
    spotProvisioningSpecification_timeoutDurationMinutes,
    spotProvisioningSpecification_timeoutAction,

    -- ** Step
    step_actionOnFailure,
    step_config,
    step_executionRoleArn,
    step_id,
    step_name,
    step_status,

    -- ** StepConfig
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- ** StepStateChangeReason
    stepStateChangeReason_code,
    stepStateChangeReason_message,

    -- ** StepStatus
    stepStatus_failureDetails,
    stepStatus_state,
    stepStatus_stateChangeReason,
    stepStatus_timeline,

    -- ** StepSummary
    stepSummary_actionOnFailure,
    stepSummary_config,
    stepSummary_id,
    stepSummary_name,
    stepSummary_status,

    -- ** StepTimeline
    stepTimeline_creationDateTime,
    stepTimeline_endDateTime,
    stepTimeline_startDateTime,

    -- ** Studio
    studio_authMode,
    studio_creationTime,
    studio_defaultS3Location,
    studio_description,
    studio_engineSecurityGroupId,
    studio_idpAuthUrl,
    studio_idpRelayStateParameterName,
    studio_name,
    studio_serviceRole,
    studio_studioArn,
    studio_studioId,
    studio_subnetIds,
    studio_tags,
    studio_url,
    studio_userRole,
    studio_vpcId,
    studio_workspaceSecurityGroupId,

    -- ** StudioSummary
    studioSummary_authMode,
    studioSummary_creationTime,
    studioSummary_description,
    studioSummary_name,
    studioSummary_studioId,
    studioSummary_url,
    studioSummary_vpcId,

    -- ** SupportedProductConfig
    supportedProductConfig_args,
    supportedProductConfig_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VolumeSpecification
    volumeSpecification_iops,
    volumeSpecification_throughput,
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
import Amazonka.EMR.Types.OSRelease
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
