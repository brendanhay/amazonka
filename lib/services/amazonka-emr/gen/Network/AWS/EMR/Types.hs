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
    _InvalidRequestException,
    _InternalServerError,
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

    -- * OnDemandCapacityReservationPreference
    OnDemandCapacityReservationPreference (..),

    -- * OnDemandCapacityReservationUsageStrategy
    OnDemandCapacityReservationUsageStrategy (..),

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
    application_name,
    application_version,

    -- * AutoScalingPolicy
    AutoScalingPolicy (..),
    newAutoScalingPolicy,
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    newAutoScalingPolicyDescription,
    autoScalingPolicyDescription_status,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_constraints,

    -- * AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (..),
    newAutoScalingPolicyStateChangeReason,
    autoScalingPolicyStateChangeReason_code,
    autoScalingPolicyStateChangeReason_message,

    -- * AutoScalingPolicyStatus
    AutoScalingPolicyStatus (..),
    newAutoScalingPolicyStatus,
    autoScalingPolicyStatus_state,
    autoScalingPolicyStatus_stateChangeReason,

    -- * AutoTerminationPolicy
    AutoTerminationPolicy (..),
    newAutoTerminationPolicy,
    autoTerminationPolicy_idleTimeout,

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
    cancelStepsInfo_status,
    cancelStepsInfo_stepId,
    cancelStepsInfo_reason,

    -- * CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (..),
    newCloudWatchAlarmDefinition,
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- * Cluster
    Cluster (..),
    newCluster,
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

    -- * ClusterStateChangeReason
    ClusterStateChangeReason (..),
    newClusterStateChangeReason,
    clusterStateChangeReason_code,
    clusterStateChangeReason_message,

    -- * ClusterStatus
    ClusterStatus (..),
    newClusterStatus,
    clusterStatus_state,
    clusterStatus_stateChangeReason,
    clusterStatus_timeline,

    -- * ClusterSummary
    ClusterSummary (..),
    newClusterSummary,
    clusterSummary_status,
    clusterSummary_clusterArn,
    clusterSummary_outpostArn,
    clusterSummary_normalizedInstanceHours,
    clusterSummary_name,
    clusterSummary_id,

    -- * ClusterTimeline
    ClusterTimeline (..),
    newClusterTimeline,
    clusterTimeline_readyDateTime,
    clusterTimeline_creationDateTime,
    clusterTimeline_endDateTime,

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
    configuration_classification,
    configuration_properties,

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
    ebsConfiguration_ebsOptimized,
    ebsConfiguration_ebsBlockDeviceConfigs,

    -- * EbsVolume
    EbsVolume (..),
    newEbsVolume,
    ebsVolume_device,
    ebsVolume_volumeId,

    -- * Ec2InstanceAttributes
    Ec2InstanceAttributes (..),
    newEc2InstanceAttributes,
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

    -- * ExecutionEngineConfig
    ExecutionEngineConfig (..),
    newExecutionEngineConfig,
    executionEngineConfig_masterInstanceSecurityGroupId,
    executionEngineConfig_type,
    executionEngineConfig_id,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_logFile,
    failureDetails_reason,
    failureDetails_message,

    -- * HadoopJarStepConfig
    HadoopJarStepConfig (..),
    newHadoopJarStepConfig,
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_jar,

    -- * HadoopStepConfig
    HadoopStepConfig (..),
    newHadoopStepConfig,
    hadoopStepConfig_args,
    hadoopStepConfig_jar,
    hadoopStepConfig_mainClass,
    hadoopStepConfig_properties,

    -- * Instance
    Instance (..),
    newInstance,
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

    -- * InstanceFleet
    InstanceFleet (..),
    newInstanceFleet,
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

    -- * InstanceFleetConfig
    InstanceFleetConfig (..),
    newInstanceFleetConfig,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_name,
    instanceFleetConfig_targetSpotCapacity,
    instanceFleetConfig_launchSpecifications,
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
    instanceFleetProvisioningSpecifications_spotSpecification,
    instanceFleetProvisioningSpecifications_onDemandSpecification,

    -- * InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (..),
    newInstanceFleetStateChangeReason,
    instanceFleetStateChangeReason_code,
    instanceFleetStateChangeReason_message,

    -- * InstanceFleetStatus
    InstanceFleetStatus (..),
    newInstanceFleetStatus,
    instanceFleetStatus_state,
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_timeline,

    -- * InstanceFleetTimeline
    InstanceFleetTimeline (..),
    newInstanceFleetTimeline,
    instanceFleetTimeline_readyDateTime,
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_endDateTime,

    -- * InstanceGroup
    InstanceGroup (..),
    newInstanceGroup,
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

    -- * InstanceGroupConfig
    InstanceGroupConfig (..),
    newInstanceGroupConfig,
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

    -- * InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    newInstanceGroupModifyConfig,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_instanceGroupId,

    -- * InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (..),
    newInstanceGroupStateChangeReason,
    instanceGroupStateChangeReason_code,
    instanceGroupStateChangeReason_message,

    -- * InstanceGroupStatus
    InstanceGroupStatus (..),
    newInstanceGroupStatus,
    instanceGroupStatus_state,
    instanceGroupStatus_stateChangeReason,
    instanceGroupStatus_timeline,

    -- * InstanceGroupTimeline
    InstanceGroupTimeline (..),
    newInstanceGroupTimeline,
    instanceGroupTimeline_readyDateTime,
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_endDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy (..),
    newInstanceResizePolicy,
    instanceResizePolicy_instancesToProtect,
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instanceTerminationTimeout,

    -- * InstanceStateChangeReason
    InstanceStateChangeReason (..),
    newInstanceStateChangeReason,
    instanceStateChangeReason_code,
    instanceStateChangeReason_message,

    -- * InstanceStatus
    InstanceStatus (..),
    newInstanceStatus,
    instanceStatus_state,
    instanceStatus_stateChangeReason,
    instanceStatus_timeline,

    -- * InstanceTimeline
    InstanceTimeline (..),
    newInstanceTimeline,
    instanceTimeline_readyDateTime,
    instanceTimeline_creationDateTime,
    instanceTimeline_endDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig (..),
    newInstanceTypeConfig,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_configurations,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_instanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification (..),
    newInstanceTypeSpecification,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_weightedCapacity,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,

    -- * JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    newJobFlowInstancesConfig,
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

    -- * KerberosAttributes
    KerberosAttributes (..),
    newKerberosAttributes,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_realm,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_crossRealmTrustPrincipalPassword,
    kerberosAttributes_aDDomainJoinUser,

    -- * KeyValue
    KeyValue (..),
    newKeyValue,
    keyValue_value,
    keyValue_key,

    -- * ManagedScalingPolicy
    ManagedScalingPolicy (..),
    newManagedScalingPolicy,
    managedScalingPolicy_computeLimits,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_value,
    metricDimension_key,

    -- * NotebookExecution
    NotebookExecution (..),
    newNotebookExecution,
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

    -- * NotebookExecutionSummary
    NotebookExecutionSummary (..),
    newNotebookExecutionSummary,
    notebookExecutionSummary_status,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_endTime,

    -- * OnDemandCapacityReservationOptions
    OnDemandCapacityReservationOptions (..),
    newOnDemandCapacityReservationOptions,
    onDemandCapacityReservationOptions_usageStrategy,
    onDemandCapacityReservationOptions_capacityReservationResourceGroupArn,
    onDemandCapacityReservationOptions_capacityReservationPreference,

    -- * OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (..),
    newOnDemandProvisioningSpecification,
    onDemandProvisioningSpecification_capacityReservationOptions,
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

    -- * ReleaseLabelFilter
    ReleaseLabelFilter (..),
    newReleaseLabelFilter,
    releaseLabelFilter_application,
    releaseLabelFilter_prefix,

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
    sessionMappingDetail_studioId,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_identityType,
    sessionMappingDetail_identityId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_identityName,

    -- * SessionMappingSummary
    SessionMappingSummary (..),
    newSessionMappingSummary,
    sessionMappingSummary_creationTime,
    sessionMappingSummary_studioId,
    sessionMappingSummary_identityType,
    sessionMappingSummary_identityId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_identityName,

    -- * ShrinkPolicy
    ShrinkPolicy (..),
    newShrinkPolicy,
    shrinkPolicy_decommissionTimeout,
    shrinkPolicy_instanceResizePolicy,

    -- * SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (..),
    newSimpleScalingPolicyConfiguration,
    simpleScalingPolicyConfiguration_adjustmentType,
    simpleScalingPolicyConfiguration_coolDown,
    simpleScalingPolicyConfiguration_scalingAdjustment,

    -- * SimplifiedApplication
    SimplifiedApplication (..),
    newSimplifiedApplication,
    simplifiedApplication_name,
    simplifiedApplication_version,

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
    step_actionOnFailure,
    step_config,
    step_name,
    step_id,

    -- * StepConfig
    StepConfig (..),
    newStepConfig,
    stepConfig_actionOnFailure,
    stepConfig_name,
    stepConfig_hadoopJarStep,

    -- * StepStateChangeReason
    StepStateChangeReason (..),
    newStepStateChangeReason,
    stepStateChangeReason_code,
    stepStateChangeReason_message,

    -- * StepStatus
    StepStatus (..),
    newStepStatus,
    stepStatus_state,
    stepStatus_failureDetails,
    stepStatus_stateChangeReason,
    stepStatus_timeline,

    -- * StepSummary
    StepSummary (..),
    newStepSummary,
    stepSummary_status,
    stepSummary_actionOnFailure,
    stepSummary_config,
    stepSummary_name,
    stepSummary_id,

    -- * StepTimeline
    StepTimeline (..),
    newStepTimeline,
    stepTimeline_creationDateTime,
    stepTimeline_endDateTime,
    stepTimeline_startDateTime,

    -- * Studio
    Studio (..),
    newStudio,
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

    -- * StudioSummary
    StudioSummary (..),
    newStudioSummary,
    studioSummary_creationTime,
    studioSummary_studioId,
    studioSummary_vpcId,
    studioSummary_url,
    studioSummary_authMode,
    studioSummary_name,
    studioSummary_description,

    -- * SupportedProductConfig
    SupportedProductConfig (..),
    newSupportedProductConfig,
    supportedProductConfig_args,
    supportedProductConfig_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * VolumeSpecification
    VolumeSpecification (..),
    newVolumeSpecification,
    volumeSpecification_iops,
    volumeSpecification_volumeType,
    volumeSpecification_sizeInGB,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.EMR.Types.AutoTerminationPolicy
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
import Network.AWS.EMR.Types.OnDemandCapacityReservationOptions
import Network.AWS.EMR.Types.OnDemandCapacityReservationPreference
import Network.AWS.EMR.Types.OnDemandCapacityReservationUsageStrategy
import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.PlacementGroupConfig
import Network.AWS.EMR.Types.PlacementGroupStrategy
import Network.AWS.EMR.Types.PlacementType
import Network.AWS.EMR.Types.PortRange
import Network.AWS.EMR.Types.ReleaseLabelFilter
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
import Network.AWS.EMR.Types.SimplifiedApplication
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

-- | API version @2009-03-31@ of the Amazon EMR SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "EMR",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticmapreduce",
      Core._serviceSigningName = "elasticmapreduce",
      Core._serviceVersion = "2009-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "EMR",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Indicates that an error occurred while processing the request and that
-- the request was not completed.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | This exception occurs when there is an internal failure in the Amazon
-- EMR service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
