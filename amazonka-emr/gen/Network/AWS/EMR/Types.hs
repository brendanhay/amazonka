{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerError,
    _InvalidRequestException,
    _InternalServerException,

    -- * ActionOnFailure
    ActionOnFailure (..),

    -- * AdjustmentType
    AdjustmentType (..),

    -- * AuthMode
    AuthMode (..),

    -- * AutoScalingPolicyState
    AutoScalingPolicyState (..),

    -- * AutoScalingPolicyStateChangeReasonCode
    AutoScalingPolicyStateChangeReasonCode (..),

    -- * CancelStepsRequestStatus
    CancelStepsRequestStatus (..),

    -- * ClusterState
    ClusterState (..),

    -- * ClusterStateChangeReasonCode
    ClusterStateChangeReasonCode (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * ComputeLimitsUnitType
    ComputeLimitsUnitType (..),

    -- * ExecutionEngineType
    ExecutionEngineType (..),

    -- * IdentityType
    IdentityType (..),

    -- * InstanceCollectionType
    InstanceCollectionType (..),

    -- * InstanceFleetState
    InstanceFleetState (..),

    -- * InstanceFleetStateChangeReasonCode
    InstanceFleetStateChangeReasonCode (..),

    -- * InstanceFleetType
    InstanceFleetType (..),

    -- * InstanceGroupState
    InstanceGroupState (..),

    -- * InstanceGroupStateChangeReasonCode
    InstanceGroupStateChangeReasonCode (..),

    -- * InstanceGroupType
    InstanceGroupType (..),

    -- * InstanceRoleType
    InstanceRoleType (..),

    -- * InstanceState
    InstanceState (..),

    -- * InstanceStateChangeReasonCode
    InstanceStateChangeReasonCode (..),

    -- * MarketType
    MarketType (..),

    -- * NotebookExecutionStatus
    NotebookExecutionStatus (..),

    -- * OnDemandProvisioningAllocationStrategy
    OnDemandProvisioningAllocationStrategy (..),

    -- * PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- * RepoUpgradeOnBoot
    RepoUpgradeOnBoot (..),

    -- * ScaleDownBehavior
    ScaleDownBehavior (..),

    -- * SpotProvisioningAllocationStrategy
    SpotProvisioningAllocationStrategy (..),

    -- * SpotProvisioningTimeoutAction
    SpotProvisioningTimeoutAction (..),

    -- * Statistic
    Statistic (..),

    -- * StepCancellationOption
    StepCancellationOption (..),

    -- * StepState
    StepState (..),

    -- * StepStateChangeReasonCode
    StepStateChangeReasonCode (..),

    -- * Unit
    Unit (..),

    -- * Application
    Application (..),
    newApplication,
    application_args,
    application_additionalInfo,
    application_version,
    application_name,

    -- * AutoScalingPolicy
    AutoScalingPolicy (..),
    newAutoScalingPolicy,
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    newAutoScalingPolicyDescription,
    autoScalingPolicyDescription_status,
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,

    -- * AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (..),
    newAutoScalingPolicyStateChangeReason,
    autoScalingPolicyStateChangeReason_message,
    autoScalingPolicyStateChangeReason_code,

    -- * AutoScalingPolicyStatus
    AutoScalingPolicyStatus (..),
    newAutoScalingPolicyStatus,
    autoScalingPolicyStatus_stateChangeReason,
    autoScalingPolicyStatus_state,

    -- * BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration (..),
    newBlockPublicAccessConfiguration,
    blockPublicAccessConfiguration_permittedPublicSecurityGroupRuleRanges,
    blockPublicAccessConfiguration_blockPublicSecurityGroupRules,

    -- * BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata (..),
    newBlockPublicAccessConfigurationMetadata,
    blockPublicAccessConfigurationMetadata_creationDateTime,
    blockPublicAccessConfigurationMetadata_createdByArn,

    -- * BootstrapActionConfig
    BootstrapActionConfig (..),
    newBootstrapActionConfig,
    bootstrapActionConfig_name,
    bootstrapActionConfig_scriptBootstrapAction,

    -- * CancelStepsInfo
    CancelStepsInfo (..),
    newCancelStepsInfo,
    cancelStepsInfo_stepId,
    cancelStepsInfo_status,
    cancelStepsInfo_reason,

    -- * CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (..),
    newCloudWatchAlarmDefinition,
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- * Cluster
    Cluster (..),
    newCluster,
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

    -- * ClusterStateChangeReason
    ClusterStateChangeReason (..),
    newClusterStateChangeReason,
    clusterStateChangeReason_message,
    clusterStateChangeReason_code,

    -- * ClusterStatus
    ClusterStatus (..),
    newClusterStatus,
    clusterStatus_stateChangeReason,
    clusterStatus_state,
    clusterStatus_timeline,

    -- * ClusterSummary
    ClusterSummary (..),
    newClusterSummary,
    clusterSummary_clusterArn,
    clusterSummary_status,
    clusterSummary_outpostArn,
    clusterSummary_id,
    clusterSummary_name,
    clusterSummary_normalizedInstanceHours,

    -- * ClusterTimeline
    ClusterTimeline (..),
    newClusterTimeline,
    clusterTimeline_endDateTime,
    clusterTimeline_creationDateTime,
    clusterTimeline_readyDateTime,

    -- * Command
    Command (..),
    newCommand,
    command_args,
    command_scriptPath,
    command_name,

    -- * ComputeLimits
    ComputeLimits (..),
    newComputeLimits,
    computeLimits_maximumOnDemandCapacityUnits,
    computeLimits_maximumCoreCapacityUnits,
    computeLimits_unitType,
    computeLimits_minimumCapacityUnits,
    computeLimits_maximumCapacityUnits,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_configurations,
    configuration_properties,
    configuration_classification,

    -- * EbsBlockDevice
    EbsBlockDevice (..),
    newEbsBlockDevice,
    ebsBlockDevice_device,
    ebsBlockDevice_volumeSpecification,

    -- * EbsBlockDeviceConfig
    EbsBlockDeviceConfig (..),
    newEbsBlockDeviceConfig,
    ebsBlockDeviceConfig_volumesPerInstance,
    ebsBlockDeviceConfig_volumeSpecification,

    -- * EbsConfiguration
    EbsConfiguration (..),
    newEbsConfiguration,
    ebsConfiguration_ebsBlockDeviceConfigs,
    ebsConfiguration_ebsOptimized,

    -- * EbsVolume
    EbsVolume (..),
    newEbsVolume,
    ebsVolume_device,
    ebsVolume_volumeId,

    -- * Ec2InstanceAttributes
    Ec2InstanceAttributes (..),
    newEc2InstanceAttributes,
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

    -- * ExecutionEngineConfig
    ExecutionEngineConfig (..),
    newExecutionEngineConfig,
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_type,
    executionEngineConfig_id,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_message,
    failureDetails_reason,
    failureDetails_logFile,

    -- * HadoopJarStepConfig
    HadoopJarStepConfig (..),
    newHadoopJarStepConfig,
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_jar,

    -- * HadoopStepConfig
    HadoopStepConfig (..),
    newHadoopStepConfig,
    hadoopStepConfig_args,
    hadoopStepConfig_jar,
    hadoopStepConfig_properties,
    hadoopStepConfig_mainClass,

    -- * Instance
    Instance (..),
    newInstance,
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

    -- * InstanceFleet
    InstanceFleet (..),
    newInstanceFleet,
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

    -- * InstanceFleetConfig
    InstanceFleetConfig (..),
    newInstanceFleetConfig,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_name,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_instanceFleetType,

    -- * InstanceFleetModifyConfig
    InstanceFleetModifyConfig (..),
    newInstanceFleetModifyConfig,
    instanceFleetModifyConfig_targetOnDemandCapacity,
    instanceFleetModifyConfig_targetSpotCapacity,
    instanceFleetModifyConfig_instanceFleetId,

    -- * InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications (..),
    newInstanceFleetProvisioningSpecifications,
    instanceFleetProvisioningSpecifications_onDemandSpecification,
    instanceFleetProvisioningSpecifications_spotSpecification,

    -- * InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (..),
    newInstanceFleetStateChangeReason,
    instanceFleetStateChangeReason_message,
    instanceFleetStateChangeReason_code,

    -- * InstanceFleetStatus
    InstanceFleetStatus (..),
    newInstanceFleetStatus,
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_state,
    instanceFleetStatus_timeline,

    -- * InstanceFleetTimeline
    InstanceFleetTimeline (..),
    newInstanceFleetTimeline,
    instanceFleetTimeline_endDateTime,
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_readyDateTime,

    -- * InstanceGroup
    InstanceGroup (..),
    newInstanceGroup,
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

    -- * InstanceGroupConfig
    InstanceGroupConfig (..),
    newInstanceGroupConfig,
    instanceGroupConfig_ebsConfiguration,
    instanceGroupConfig_configurations,
    instanceGroupConfig_autoScalingPolicy,
    instanceGroupConfig_bidPrice,
    instanceGroupConfig_name,
    instanceGroupConfig_market,
    instanceGroupConfig_instanceRole,
    instanceGroupConfig_instanceType,
    instanceGroupConfig_instanceCount,

    -- * InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    newInstanceGroupModifyConfig,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_instanceGroupId,

    -- * InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (..),
    newInstanceGroupStateChangeReason,
    instanceGroupStateChangeReason_message,
    instanceGroupStateChangeReason_code,

    -- * InstanceGroupStatus
    InstanceGroupStatus (..),
    newInstanceGroupStatus,
    instanceGroupStatus_stateChangeReason,
    instanceGroupStatus_state,
    instanceGroupStatus_timeline,

    -- * InstanceGroupTimeline
    InstanceGroupTimeline (..),
    newInstanceGroupTimeline,
    instanceGroupTimeline_endDateTime,
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_readyDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy (..),
    newInstanceResizePolicy,
    instanceResizePolicy_instanceTerminationTimeout,
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instancesToProtect,

    -- * InstanceStateChangeReason
    InstanceStateChangeReason (..),
    newInstanceStateChangeReason,
    instanceStateChangeReason_message,
    instanceStateChangeReason_code,

    -- * InstanceStatus
    InstanceStatus (..),
    newInstanceStatus,
    instanceStatus_stateChangeReason,
    instanceStatus_state,
    instanceStatus_timeline,

    -- * InstanceTimeline
    InstanceTimeline (..),
    newInstanceTimeline,
    instanceTimeline_endDateTime,
    instanceTimeline_creationDateTime,
    instanceTimeline_readyDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig (..),
    newInstanceTypeConfig,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_configurations,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification (..),
    newInstanceTypeSpecification,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_weightedCapacity,

    -- * JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    newJobFlowInstancesConfig,
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

    -- * KerberosAttributes
    KerberosAttributes (..),
    newKerberosAttributes,
    kerberosAttributes_realm,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_crossRealmTrustPrincipalPassword,

    -- * KeyValue
    KeyValue (..),
    newKeyValue,
    keyValue_key,
    keyValue_value,

    -- * ManagedScalingPolicy
    ManagedScalingPolicy (..),
    newManagedScalingPolicy,
    managedScalingPolicy_computeLimits,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_key,
    metricDimension_value,

    -- * NotebookExecution
    NotebookExecution (..),
    newNotebookExecution,
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

    -- * NotebookExecutionSummary
    NotebookExecutionSummary (..),
    newNotebookExecutionSummary,
    notebookExecutionSummary_status,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_endTime,

    -- * OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (..),
    newOnDemandProvisioningSpecification,
    onDemandProvisioningSpecification_allocationStrategy,

    -- * PlacementGroupConfig
    PlacementGroupConfig (..),
    newPlacementGroupConfig,
    placementGroupConfig_placementStrategy,
    placementGroupConfig_instanceRole,

    -- * PlacementType
    PlacementType (..),
    newPlacementType,
    placementType_availabilityZones,
    placementType_availabilityZone,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_maxRange,
    portRange_minRange,

    -- * ScalingAction
    ScalingAction (..),
    newScalingAction,
    scalingAction_market,
    scalingAction_simpleScalingPolicyConfiguration,

    -- * ScalingConstraints
    ScalingConstraints (..),
    newScalingConstraints,
    scalingConstraints_minCapacity,
    scalingConstraints_maxCapacity,

    -- * ScalingRule
    ScalingRule (..),
    newScalingRule,
    scalingRule_description,
    scalingRule_name,
    scalingRule_action,
    scalingRule_trigger,

    -- * ScalingTrigger
    ScalingTrigger (..),
    newScalingTrigger,
    scalingTrigger_cloudWatchAlarmDefinition,

    -- * ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig (..),
    newScriptBootstrapActionConfig,
    scriptBootstrapActionConfig_args,
    scriptBootstrapActionConfig_path,

    -- * SecurityConfigurationSummary
    SecurityConfigurationSummary (..),
    newSecurityConfigurationSummary,
    securityConfigurationSummary_name,
    securityConfigurationSummary_creationDateTime,

    -- * SessionMappingDetail
    SessionMappingDetail (..),
    newSessionMappingDetail,
    sessionMappingDetail_creationTime,
    sessionMappingDetail_identityName,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_identityType,
    sessionMappingDetail_identityId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_studioId,

    -- * SessionMappingSummary
    SessionMappingSummary (..),
    newSessionMappingSummary,
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityName,
    sessionMappingSummary_identityType,
    sessionMappingSummary_identityId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_studioId,

    -- * ShrinkPolicy
    ShrinkPolicy (..),
    newShrinkPolicy,
    shrinkPolicy_instanceResizePolicy,
    shrinkPolicy_decommissionTimeout,

    -- * SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (..),
    newSimpleScalingPolicyConfiguration,
    simpleScalingPolicyConfiguration_coolDown,
    simpleScalingPolicyConfiguration_adjustmentType,
    simpleScalingPolicyConfiguration_scalingAdjustment,

    -- * SpotProvisioningSpecification
    SpotProvisioningSpecification (..),
    newSpotProvisioningSpecification,
    spotProvisioningSpecification_blockDurationMinutes,
    spotProvisioningSpecification_allocationStrategy,
    spotProvisioningSpecification_timeoutDurationMinutes,
    spotProvisioningSpecification_timeoutAction,

    -- * Step
    Step (..),
    newStep,
    step_status,
    step_id,
    step_config,
    step_actionOnFailure,
    step_name,

    -- * StepConfig
    StepConfig (..),
    newStepConfig,
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- * StepStateChangeReason
    StepStateChangeReason (..),
    newStepStateChangeReason,
    stepStateChangeReason_message,
    stepStateChangeReason_code,

    -- * StepStatus
    StepStatus (..),
    newStepStatus,
    stepStatus_stateChangeReason,
    stepStatus_failureDetails,
    stepStatus_state,
    stepStatus_timeline,

    -- * StepSummary
    StepSummary (..),
    newStepSummary,
    stepSummary_status,
    stepSummary_id,
    stepSummary_config,
    stepSummary_actionOnFailure,
    stepSummary_name,

    -- * StepTimeline
    StepTimeline (..),
    newStepTimeline,
    stepTimeline_startDateTime,
    stepTimeline_endDateTime,
    stepTimeline_creationDateTime,

    -- * Studio
    Studio (..),
    newStudio,
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

    -- * StudioSummary
    StudioSummary (..),
    newStudioSummary,
    studioSummary_creationTime,
    studioSummary_name,
    studioSummary_description,
    studioSummary_url,
    studioSummary_vpcId,
    studioSummary_studioId,

    -- * SupportedProductConfig
    SupportedProductConfig (..),
    newSupportedProductConfig,
    supportedProductConfig_args,
    supportedProductConfig_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VolumeSpecification
    VolumeSpecification (..),
    newVolumeSpecification,
    volumeSpecification_iops,
    volumeSpecification_volumeType,
    volumeSpecification_sizeInGB,
  )
where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.AdjustmentType
import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.AuthMode
import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.AutoScalingPolicyState
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.BlockPublicAccessConfiguration
import Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
import Network.AWS.EMR.Types.BootstrapActionConfig
import Network.AWS.EMR.Types.CancelStepsInfo
import Network.AWS.EMR.Types.CancelStepsRequestStatus
import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
import Network.AWS.EMR.Types.Cluster
import Network.AWS.EMR.Types.ClusterState
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterStateChangeReasonCode
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.ClusterSummary
import Network.AWS.EMR.Types.ClusterTimeline
import Network.AWS.EMR.Types.Command
import Network.AWS.EMR.Types.ComparisonOperator
import Network.AWS.EMR.Types.ComputeLimits
import Network.AWS.EMR.Types.ComputeLimitsUnitType
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EbsBlockDevice
import Network.AWS.EMR.Types.EbsBlockDeviceConfig
import Network.AWS.EMR.Types.EbsConfiguration
import Network.AWS.EMR.Types.EbsVolume
import Network.AWS.EMR.Types.Ec2InstanceAttributes
import Network.AWS.EMR.Types.ExecutionEngineConfig
import Network.AWS.EMR.Types.ExecutionEngineType
import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.HadoopJarStepConfig
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.IdentityType
import Network.AWS.EMR.Types.Instance
import Network.AWS.EMR.Types.InstanceCollectionType
import Network.AWS.EMR.Types.InstanceFleet
import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceFleetModifyConfig
import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetState
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
import Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
import Network.AWS.EMR.Types.InstanceFleetStatus
import Network.AWS.EMR.Types.InstanceFleetTimeline
import Network.AWS.EMR.Types.InstanceFleetType
import Network.AWS.EMR.Types.InstanceGroup
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.InstanceGroupModifyConfig
import Network.AWS.EMR.Types.InstanceGroupState
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupTimeline
import Network.AWS.EMR.Types.InstanceGroupType
import Network.AWS.EMR.Types.InstanceResizePolicy
import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.InstanceState
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.InstanceTimeline
import Network.AWS.EMR.Types.InstanceTypeConfig
import Network.AWS.EMR.Types.InstanceTypeSpecification
import Network.AWS.EMR.Types.JobFlowInstancesConfig
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.KeyValue
import Network.AWS.EMR.Types.ManagedScalingPolicy
import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.NotebookExecution
import Network.AWS.EMR.Types.NotebookExecutionStatus
import Network.AWS.EMR.Types.NotebookExecutionSummary
import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.PlacementGroupStrategy
import Network.AWS.EMR.Types.PlacementType
import Network.AWS.EMR.Types.PortRange
import Network.AWS.EMR.Types.RepoUpgradeOnBoot
import Network.AWS.EMR.Types.ScaleDownBehavior
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
import Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
import Network.AWS.EMR.Types.SpotProvisioningSpecification
import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
import Network.AWS.EMR.Types.Statistic
import Network.AWS.EMR.Types.Step
import Network.AWS.EMR.Types.StepCancellationOption
import Network.AWS.EMR.Types.StepConfig
import Network.AWS.EMR.Types.StepState
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepStateChangeReasonCode
import Network.AWS.EMR.Types.StepStatus
import Network.AWS.EMR.Types.StepSummary
import Network.AWS.EMR.Types.StepTimeline
import Network.AWS.EMR.Types.Studio
import Network.AWS.EMR.Types.StudioSummary
import Network.AWS.EMR.Types.SupportedProductConfig
import Network.AWS.EMR.Types.Tag
import Network.AWS.EMR.Types.Unit
import Network.AWS.EMR.Types.VolumeSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2009-03-31@ of the Amazon Elastic MapReduce SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "EMR",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "elasticmapreduce",
      Prelude._svcVersion = "2009-03-31",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "EMR",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that an error occurred while processing the request and that
-- the request was not completed.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | This exception occurs when there is an internal failure in the Amazon
-- EMR service.
_InternalServerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerException"
