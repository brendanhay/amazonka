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

    -- ** DescribeStep
    describeStep_clusterId,
    describeStep_stepId,
    describeStepResponse_step,
    describeStepResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceId,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** DeleteSecurityConfiguration
    deleteSecurityConfiguration_name,
    deleteSecurityConfigurationResponse_httpStatus,

    -- ** ListSecurityConfigurations
    listSecurityConfigurations_marker,
    listSecurityConfigurationsResponse_securityConfigurations,
    listSecurityConfigurationsResponse_marker,
    listSecurityConfigurationsResponse_httpStatus,

    -- ** ModifyInstanceFleet
    modifyInstanceFleet_clusterId,
    modifyInstanceFleet_instanceFleet,

    -- ** RunJobFlow
    runJobFlow_amiVersion,
    runJobFlow_additionalInfo,
    runJobFlow_placementGroupConfigs,
    runJobFlow_repoUpgradeOnBoot,
    runJobFlow_serviceRole,
    runJobFlow_securityConfiguration,
    runJobFlow_scaleDownBehavior,
    runJobFlow_autoScalingRole,
    runJobFlow_configurations,
    runJobFlow_releaseLabel,
    runJobFlow_ebsRootVolumeSize,
    runJobFlow_bootstrapActions,
    runJobFlow_logEncryptionKmsKeyId,
    runJobFlow_tags,
    runJobFlow_applications,
    runJobFlow_stepConcurrencyLevel,
    runJobFlow_jobFlowRole,
    runJobFlow_steps,
    runJobFlow_supportedProducts,
    runJobFlow_visibleToAllUsers,
    runJobFlow_customAmiId,
    runJobFlow_managedScalingPolicy,
    runJobFlow_kerberosAttributes,
    runJobFlow_logUri,
    runJobFlow_newSupportedProducts,
    runJobFlow_name,
    runJobFlow_instances,
    runJobFlowResponse_clusterArn,
    runJobFlowResponse_jobFlowId,
    runJobFlowResponse_httpStatus,

    -- ** GetStudioSessionMapping
    getStudioSessionMapping_identityName,
    getStudioSessionMapping_identityId,
    getStudioSessionMapping_studioId,
    getStudioSessionMapping_identityType,
    getStudioSessionMappingResponse_sessionMapping,
    getStudioSessionMappingResponse_httpStatus,

    -- ** SetVisibleToAllUsers
    setVisibleToAllUsers_jobFlowIds,
    setVisibleToAllUsers_visibleToAllUsers,

    -- ** AddInstanceGroups
    addInstanceGroups_instanceGroups,
    addInstanceGroups_jobFlowId,
    addInstanceGroupsResponse_clusterArn,
    addInstanceGroupsResponse_instanceGroupIds,
    addInstanceGroupsResponse_jobFlowId,
    addInstanceGroupsResponse_httpStatus,

    -- ** CreateStudio
    createStudio_tags,
    createStudio_description,
    createStudio_name,
    createStudio_authMode,
    createStudio_vpcId,
    createStudio_subnetIds,
    createStudio_serviceRole,
    createStudio_userRole,
    createStudio_workspaceSecurityGroupId,
    createStudio_engineSecurityGroupId,
    createStudio_defaultS3Location,
    createStudioResponse_url,
    createStudioResponse_studioId,
    createStudioResponse_httpStatus,

    -- ** DeleteStudio
    deleteStudio_studioId,

    -- ** UpdateStudio
    updateStudio_defaultS3Location,
    updateStudio_subnetIds,
    updateStudio_name,
    updateStudio_description,
    updateStudio_studioId,

    -- ** ListInstanceFleets
    listInstanceFleets_marker,
    listInstanceFleets_clusterId,
    listInstanceFleetsResponse_instanceFleets,
    listInstanceFleetsResponse_marker,
    listInstanceFleetsResponse_httpStatus,

    -- ** RemoveManagedScalingPolicy
    removeManagedScalingPolicy_clusterId,
    removeManagedScalingPolicyResponse_httpStatus,

    -- ** DescribeSecurityConfiguration
    describeSecurityConfiguration_name,
    describeSecurityConfigurationResponse_securityConfiguration,
    describeSecurityConfigurationResponse_name,
    describeSecurityConfigurationResponse_creationDateTime,
    describeSecurityConfigurationResponse_httpStatus,

    -- ** StartNotebookExecution
    startNotebookExecution_notebookExecutionName,
    startNotebookExecution_notebookParams,
    startNotebookExecution_notebookInstanceSecurityGroupId,
    startNotebookExecution_tags,
    startNotebookExecution_editorId,
    startNotebookExecution_relativePath,
    startNotebookExecution_executionEngine,
    startNotebookExecution_serviceRole,
    startNotebookExecutionResponse_notebookExecutionId,
    startNotebookExecutionResponse_httpStatus,

    -- ** ListStudioSessionMappings
    listStudioSessionMappings_identityType,
    listStudioSessionMappings_studioId,
    listStudioSessionMappings_marker,
    listStudioSessionMappingsResponse_sessionMappings,
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_httpStatus,

    -- ** StopNotebookExecution
    stopNotebookExecution_notebookExecutionId,

    -- ** ListInstances
    listInstances_instanceFleetType,
    listInstances_instanceGroupId,
    listInstances_instanceStates,
    listInstances_instanceFleetId,
    listInstances_instanceGroupTypes,
    listInstances_marker,
    listInstances_clusterId,
    listInstancesResponse_instances,
    listInstancesResponse_marker,
    listInstancesResponse_httpStatus,

    -- ** AddTags
    addTags_resourceId,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** AddJobFlowSteps
    addJobFlowSteps_jobFlowId,
    addJobFlowSteps_steps,
    addJobFlowStepsResponse_stepIds,
    addJobFlowStepsResponse_httpStatus,

    -- ** ListBootstrapActions
    listBootstrapActions_marker,
    listBootstrapActions_clusterId,
    listBootstrapActionsResponse_bootstrapActions,
    listBootstrapActionsResponse_marker,
    listBootstrapActionsResponse_httpStatus,

    -- ** ListNotebookExecutions
    listNotebookExecutions_status,
    listNotebookExecutions_editorId,
    listNotebookExecutions_to,
    listNotebookExecutions_from,
    listNotebookExecutions_marker,
    listNotebookExecutionsResponse_notebookExecutions,
    listNotebookExecutionsResponse_marker,
    listNotebookExecutionsResponse_httpStatus,

    -- ** GetBlockPublicAccessConfiguration
    getBlockPublicAccessConfigurationResponse_httpStatus,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfiguration,
    getBlockPublicAccessConfigurationResponse_blockPublicAccessConfigurationMetadata,

    -- ** ModifyCluster
    modifyCluster_stepConcurrencyLevel,
    modifyCluster_clusterId,
    modifyClusterResponse_stepConcurrencyLevel,
    modifyClusterResponse_httpStatus,

    -- ** TerminateJobFlows
    terminateJobFlows_jobFlowIds,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_httpStatus,
    describeClusterResponse_cluster,

    -- ** CancelSteps
    cancelSteps_stepCancellationOption,
    cancelSteps_clusterId,
    cancelSteps_stepIds,
    cancelStepsResponse_cancelStepsInfoList,
    cancelStepsResponse_httpStatus,

    -- ** ListInstanceGroups
    listInstanceGroups_marker,
    listInstanceGroups_clusterId,
    listInstanceGroupsResponse_instanceGroups,
    listInstanceGroupsResponse_marker,
    listInstanceGroupsResponse_httpStatus,

    -- ** RemoveAutoScalingPolicy
    removeAutoScalingPolicy_clusterId,
    removeAutoScalingPolicy_instanceGroupId,
    removeAutoScalingPolicyResponse_httpStatus,

    -- ** PutManagedScalingPolicy
    putManagedScalingPolicy_clusterId,
    putManagedScalingPolicy_managedScalingPolicy,
    putManagedScalingPolicyResponse_httpStatus,

    -- ** ListStudios
    listStudios_marker,
    listStudiosResponse_studios,
    listStudiosResponse_marker,
    listStudiosResponse_httpStatus,

    -- ** AddInstanceFleet
    addInstanceFleet_clusterId,
    addInstanceFleet_instanceFleet,
    addInstanceFleetResponse_clusterArn,
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_httpStatus,

    -- ** CreateStudioSessionMapping
    createStudioSessionMapping_identityName,
    createStudioSessionMapping_identityId,
    createStudioSessionMapping_studioId,
    createStudioSessionMapping_identityType,
    createStudioSessionMapping_sessionPolicyArn,

    -- ** GetManagedScalingPolicy
    getManagedScalingPolicy_clusterId,
    getManagedScalingPolicyResponse_managedScalingPolicy,
    getManagedScalingPolicyResponse_httpStatus,

    -- ** DescribeNotebookExecution
    describeNotebookExecution_notebookExecutionId,
    describeNotebookExecutionResponse_notebookExecution,
    describeNotebookExecutionResponse_httpStatus,

    -- ** UpdateStudioSessionMapping
    updateStudioSessionMapping_identityName,
    updateStudioSessionMapping_identityId,
    updateStudioSessionMapping_studioId,
    updateStudioSessionMapping_identityType,
    updateStudioSessionMapping_sessionPolicyArn,

    -- ** DeleteStudioSessionMapping
    deleteStudioSessionMapping_identityName,
    deleteStudioSessionMapping_identityId,
    deleteStudioSessionMapping_studioId,
    deleteStudioSessionMapping_identityType,

    -- ** ListSteps
    listSteps_stepIds,
    listSteps_stepStates,
    listSteps_marker,
    listSteps_clusterId,
    listStepsResponse_steps,
    listStepsResponse_marker,
    listStepsResponse_httpStatus,

    -- ** ListClusters
    listClusters_createdAfter,
    listClusters_createdBefore,
    listClusters_clusterStates,
    listClusters_marker,
    listClustersResponse_clusters,
    listClustersResponse_marker,
    listClustersResponse_httpStatus,

    -- ** PutAutoScalingPolicy
    putAutoScalingPolicy_clusterId,
    putAutoScalingPolicy_instanceGroupId,
    putAutoScalingPolicy_autoScalingPolicy,
    putAutoScalingPolicyResponse_clusterArn,
    putAutoScalingPolicyResponse_clusterId,
    putAutoScalingPolicyResponse_instanceGroupId,
    putAutoScalingPolicyResponse_autoScalingPolicy,
    putAutoScalingPolicyResponse_httpStatus,

    -- ** SetTerminationProtection
    setTerminationProtection_jobFlowIds,
    setTerminationProtection_terminationProtected,

    -- ** PutBlockPublicAccessConfiguration
    putBlockPublicAccessConfiguration_blockPublicAccessConfiguration,
    putBlockPublicAccessConfigurationResponse_httpStatus,

    -- ** DescribeStudio
    describeStudio_studioId,
    describeStudioResponse_studio,
    describeStudioResponse_httpStatus,

    -- ** ModifyInstanceGroups
    modifyInstanceGroups_clusterId,
    modifyInstanceGroups_instanceGroups,

    -- ** CreateSecurityConfiguration
    createSecurityConfiguration_name,
    createSecurityConfiguration_securityConfiguration,
    createSecurityConfigurationResponse_httpStatus,
    createSecurityConfigurationResponse_name,
    createSecurityConfigurationResponse_creationDateTime,

    -- * Types

    -- ** Application
    application_args,
    application_additionalInfo,
    application_version,
    application_name,

    -- ** AutoScalingPolicy
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- ** AutoScalingPolicyDescription
    autoScalingPolicyDescription_status,
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,

    -- ** AutoScalingPolicyStateChangeReason
    autoScalingPolicyStateChangeReason_message,
    autoScalingPolicyStateChangeReason_code,

    -- ** AutoScalingPolicyStatus
    autoScalingPolicyStatus_stateChangeReason,
    autoScalingPolicyStatus_state,

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
    cancelStepsInfo_stepId,
    cancelStepsInfo_status,
    cancelStepsInfo_reason,

    -- ** CloudWatchAlarmDefinition
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- ** Cluster
    cluster_clusterArn,
    cluster_repoUpgradeOnBoot,
    cluster_serviceRole,
    cluster_securityConfiguration,
    cluster_scaleDownBehavior,
    cluster_autoScalingRole,
    cluster_terminationProtected,
    cluster_configurations,
    cluster_outpostArn,
    cluster_masterPublicDnsName,
    cluster_runningAmiVersion,
    cluster_requestedAmiVersion,
    cluster_releaseLabel,
    cluster_ebsRootVolumeSize,
    cluster_instanceCollectionType,
    cluster_logEncryptionKmsKeyId,
    cluster_tags,
    cluster_applications,
    cluster_stepConcurrencyLevel,
    cluster_visibleToAllUsers,
    cluster_autoTerminate,
    cluster_normalizedInstanceHours,
    cluster_customAmiId,
    cluster_placementGroups,
    cluster_ec2InstanceAttributes,
    cluster_kerberosAttributes,
    cluster_logUri,
    cluster_id,
    cluster_name,
    cluster_status,

    -- ** ClusterStateChangeReason
    clusterStateChangeReason_message,
    clusterStateChangeReason_code,

    -- ** ClusterStatus
    clusterStatus_stateChangeReason,
    clusterStatus_state,
    clusterStatus_timeline,

    -- ** ClusterSummary
    clusterSummary_clusterArn,
    clusterSummary_status,
    clusterSummary_outpostArn,
    clusterSummary_id,
    clusterSummary_name,
    clusterSummary_normalizedInstanceHours,

    -- ** ClusterTimeline
    clusterTimeline_endDateTime,
    clusterTimeline_creationDateTime,
    clusterTimeline_readyDateTime,

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
    configuration_properties,
    configuration_classification,

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
    ec2InstanceAttributes_ec2KeyName,
    ec2InstanceAttributes_additionalSlaveSecurityGroups,
    ec2InstanceAttributes_ec2AvailabilityZone,
    ec2InstanceAttributes_emrManagedSlaveSecurityGroup,
    ec2InstanceAttributes_requestedEc2AvailabilityZones,
    ec2InstanceAttributes_ec2SubnetId,
    ec2InstanceAttributes_emrManagedMasterSecurityGroup,
    ec2InstanceAttributes_iamInstanceProfile,
    ec2InstanceAttributes_additionalMasterSecurityGroups,
    ec2InstanceAttributes_requestedEc2SubnetIds,
    ec2InstanceAttributes_serviceAccessSecurityGroup,

    -- ** ExecutionEngineConfig
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_type,
    executionEngineConfig_id,

    -- ** FailureDetails
    failureDetails_message,
    failureDetails_reason,
    failureDetails_logFile,

    -- ** HadoopJarStepConfig
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_jar,

    -- ** HadoopStepConfig
    hadoopStepConfig_args,
    hadoopStepConfig_jar,
    hadoopStepConfig_properties,
    hadoopStepConfig_mainClass,

    -- ** Instance
    instance_ebsVolumes,
    instance_status,
    instance_instanceType,
    instance_instanceGroupId,
    instance_id,
    instance_instanceFleetId,
    instance_publicDnsName,
    instance_market,
    instance_publicIpAddress,
    instance_privateDnsName,
    instance_ec2InstanceId,
    instance_privateIpAddress,

    -- ** InstanceFleet
    instanceFleet_instanceFleetType,
    instanceFleet_status,
    instanceFleet_targetOnDemandCapacity,
    instanceFleet_id,
    instanceFleet_targetSpotCapacity,
    instanceFleet_provisionedOnDemandCapacity,
    instanceFleet_instanceTypeSpecifications,
    instanceFleet_name,
    instanceFleet_provisionedSpotCapacity,
    instanceFleet_launchSpecifications,

    -- ** InstanceFleetConfig
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_name,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_instanceFleetType,

    -- ** InstanceFleetModifyConfig
    instanceFleetModifyConfig_targetOnDemandCapacity,
    instanceFleetModifyConfig_targetSpotCapacity,
    instanceFleetModifyConfig_instanceFleetId,

    -- ** InstanceFleetProvisioningSpecifications
    instanceFleetProvisioningSpecifications_onDemandSpecification,
    instanceFleetProvisioningSpecifications_spotSpecification,

    -- ** InstanceFleetStateChangeReason
    instanceFleetStateChangeReason_message,
    instanceFleetStateChangeReason_code,

    -- ** InstanceFleetStatus
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_state,
    instanceFleetStatus_timeline,

    -- ** InstanceFleetTimeline
    instanceFleetTimeline_endDateTime,
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_readyDateTime,

    -- ** InstanceGroup
    instanceGroup_lastSuccessfullyAppliedConfigurationsVersion,
    instanceGroup_status,
    instanceGroup_instanceType,
    instanceGroup_ebsOptimized,
    instanceGroup_ebsBlockDevices,
    instanceGroup_instanceGroupType,
    instanceGroup_configurations,
    instanceGroup_shrinkPolicy,
    instanceGroup_id,
    instanceGroup_lastSuccessfullyAppliedConfigurations,
    instanceGroup_requestedInstanceCount,
    instanceGroup_autoScalingPolicy,
    instanceGroup_bidPrice,
    instanceGroup_name,
    instanceGroup_market,
    instanceGroup_configurationsVersion,
    instanceGroup_runningInstanceCount,

    -- ** InstanceGroupConfig
    instanceGroupConfig_ebsConfiguration,
    instanceGroupConfig_configurations,
    instanceGroupConfig_autoScalingPolicy,
    instanceGroupConfig_bidPrice,
    instanceGroupConfig_name,
    instanceGroupConfig_market,
    instanceGroupConfig_instanceRole,
    instanceGroupConfig_instanceType,
    instanceGroupConfig_instanceCount,

    -- ** InstanceGroupModifyConfig
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_instanceGroupId,

    -- ** InstanceGroupStateChangeReason
    instanceGroupStateChangeReason_message,
    instanceGroupStateChangeReason_code,

    -- ** InstanceGroupStatus
    instanceGroupStatus_stateChangeReason,
    instanceGroupStatus_state,
    instanceGroupStatus_timeline,

    -- ** InstanceGroupTimeline
    instanceGroupTimeline_endDateTime,
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_readyDateTime,

    -- ** InstanceResizePolicy
    instanceResizePolicy_instanceTerminationTimeout,
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instancesToProtect,

    -- ** InstanceStateChangeReason
    instanceStateChangeReason_message,
    instanceStateChangeReason_code,

    -- ** InstanceStatus
    instanceStatus_stateChangeReason,
    instanceStatus_state,
    instanceStatus_timeline,

    -- ** InstanceTimeline
    instanceTimeline_endDateTime,
    instanceTimeline_creationDateTime,
    instanceTimeline_readyDateTime,

    -- ** InstanceTypeConfig
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_configurations,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- ** InstanceTypeSpecification
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_weightedCapacity,

    -- ** JobFlowInstancesConfig
    jobFlowInstancesConfig_hadoopVersion,
    jobFlowInstancesConfig_ec2KeyName,
    jobFlowInstancesConfig_instanceFleets,
    jobFlowInstancesConfig_ec2SubnetIds,
    jobFlowInstancesConfig_placement,
    jobFlowInstancesConfig_additionalSlaveSecurityGroups,
    jobFlowInstancesConfig_terminationProtected,
    jobFlowInstancesConfig_emrManagedSlaveSecurityGroup,
    jobFlowInstancesConfig_instanceGroups,
    jobFlowInstancesConfig_masterInstanceType,
    jobFlowInstancesConfig_ec2SubnetId,
    jobFlowInstancesConfig_emrManagedMasterSecurityGroup,
    jobFlowInstancesConfig_additionalMasterSecurityGroups,
    jobFlowInstancesConfig_slaveInstanceType,
    jobFlowInstancesConfig_serviceAccessSecurityGroup,
    jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps,
    jobFlowInstancesConfig_instanceCount,

    -- ** KerberosAttributes
    kerberosAttributes_realm,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_aDDomainJoinPassword,
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
    notebookExecution_lastStateChangeReason,
    notebookExecution_status,
    notebookExecution_notebookExecutionName,
    notebookExecution_editorId,
    notebookExecution_notebookExecutionId,
    notebookExecution_startTime,
    notebookExecution_arn,
    notebookExecution_notebookParams,
    notebookExecution_endTime,
    notebookExecution_notebookInstanceSecurityGroupId,
    notebookExecution_executionEngine,
    notebookExecution_tags,
    notebookExecution_outputNotebookURI,

    -- ** NotebookExecutionSummary
    notebookExecutionSummary_status,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_endTime,

    -- ** OnDemandProvisioningSpecification
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
    sessionMappingDetail_identityName,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_identityType,
    sessionMappingDetail_identityId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_studioId,

    -- ** SessionMappingSummary
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityName,
    sessionMappingSummary_identityType,
    sessionMappingSummary_identityId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_studioId,

    -- ** ShrinkPolicy
    shrinkPolicy_instanceResizePolicy,
    shrinkPolicy_decommissionTimeout,

    -- ** SimpleScalingPolicyConfiguration
    simpleScalingPolicyConfiguration_coolDown,
    simpleScalingPolicyConfiguration_adjustmentType,
    simpleScalingPolicyConfiguration_scalingAdjustment,

    -- ** SpotProvisioningSpecification
    spotProvisioningSpecification_blockDurationMinutes,
    spotProvisioningSpecification_allocationStrategy,
    spotProvisioningSpecification_timeoutDurationMinutes,
    spotProvisioningSpecification_timeoutAction,

    -- ** Step
    step_status,
    step_id,
    step_config,
    step_actionOnFailure,
    step_name,

    -- ** StepConfig
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- ** StepStateChangeReason
    stepStateChangeReason_message,
    stepStateChangeReason_code,

    -- ** StepStatus
    stepStatus_stateChangeReason,
    stepStatus_failureDetails,
    stepStatus_state,
    stepStatus_timeline,

    -- ** StepSummary
    stepSummary_status,
    stepSummary_id,
    stepSummary_config,
    stepSummary_actionOnFailure,
    stepSummary_name,

    -- ** StepTimeline
    stepTimeline_startDateTime,
    stepTimeline_endDateTime,
    stepTimeline_creationDateTime,

    -- ** Studio
    studio_creationTime,
    studio_serviceRole,
    studio_workspaceSecurityGroupId,
    studio_defaultS3Location,
    studio_authMode,
    studio_subnetIds,
    studio_userRole,
    studio_name,
    studio_tags,
    studio_description,
    studio_url,
    studio_vpcId,
    studio_studioArn,
    studio_studioId,
    studio_engineSecurityGroupId,

    -- ** StudioSummary
    studioSummary_creationTime,
    studioSummary_name,
    studioSummary_description,
    studioSummary_url,
    studioSummary_vpcId,
    studioSummary_studioId,

    -- ** SupportedProductConfig
    supportedProductConfig_args,
    supportedProductConfig_name,

    -- ** Tag
    tag_key,
    tag_value,

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
import Network.AWS.EMR.DescribeSecurityConfiguration
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.DescribeStudio
import Network.AWS.EMR.GetBlockPublicAccessConfiguration
import Network.AWS.EMR.GetManagedScalingPolicy
import Network.AWS.EMR.GetStudioSessionMapping
import Network.AWS.EMR.ListBootstrapActions
import Network.AWS.EMR.ListClusters
import Network.AWS.EMR.ListInstanceFleets
import Network.AWS.EMR.ListInstanceGroups
import Network.AWS.EMR.ListInstances
import Network.AWS.EMR.ListNotebookExecutions
import Network.AWS.EMR.ListSecurityConfigurations
import Network.AWS.EMR.ListSteps
import Network.AWS.EMR.ListStudioSessionMappings
import Network.AWS.EMR.ListStudios
import Network.AWS.EMR.ModifyCluster
import Network.AWS.EMR.ModifyInstanceFleet
import Network.AWS.EMR.ModifyInstanceGroups
import Network.AWS.EMR.PutAutoScalingPolicy
import Network.AWS.EMR.PutBlockPublicAccessConfiguration
import Network.AWS.EMR.PutManagedScalingPolicy
import Network.AWS.EMR.RemoveAutoScalingPolicy
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
import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.PlacementType
import Network.AWS.EMR.Types.PortRange
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
