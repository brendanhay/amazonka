{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _InternalServerError,
    _InvalidRequestException,

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

    -- * ReconfigurationType
    ReconfigurationType (..),

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
    application_name,
    application_additionalInfo,
    application_args,
    application_version,

    -- * AutoScalingPolicy
    AutoScalingPolicy (..),
    newAutoScalingPolicy,
    autoScalingPolicy_constraints,
    autoScalingPolicy_rules,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    newAutoScalingPolicyDescription,
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_status,

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
    cancelStepsInfo_reason,
    cancelStepsInfo_stepId,

    -- * CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (..),
    newCloudWatchAlarmDefinition,
    cloudWatchAlarmDefinition_dimensions,
    cloudWatchAlarmDefinition_evaluationPeriods,
    cloudWatchAlarmDefinition_namespace,
    cloudWatchAlarmDefinition_statistic,
    cloudWatchAlarmDefinition_unit,
    cloudWatchAlarmDefinition_comparisonOperator,
    cloudWatchAlarmDefinition_metricName,
    cloudWatchAlarmDefinition_period,
    cloudWatchAlarmDefinition_threshold,

    -- * Cluster
    Cluster (..),
    newCluster,
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
    cluster_oSReleaseLabel,
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

    -- * ClusterStateChangeReason
    ClusterStateChangeReason (..),
    newClusterStateChangeReason,
    clusterStateChangeReason_message,
    clusterStateChangeReason_code,

    -- * ClusterStatus
    ClusterStatus (..),
    newClusterStatus,
    clusterStatus_stateChangeReason,
    clusterStatus_timeline,
    clusterStatus_state,

    -- * ClusterSummary
    ClusterSummary (..),
    newClusterSummary,
    clusterSummary_clusterArn,
    clusterSummary_name,
    clusterSummary_outpostArn,
    clusterSummary_status,
    clusterSummary_id,
    clusterSummary_normalizedInstanceHours,

    -- * ClusterTimeline
    ClusterTimeline (..),
    newClusterTimeline,
    clusterTimeline_creationDateTime,
    clusterTimeline_readyDateTime,
    clusterTimeline_endDateTime,

    -- * Command
    Command (..),
    newCommand,
    command_name,
    command_scriptPath,
    command_args,

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
    configuration_properties,
    configuration_configurations,
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

    -- * ExecutionEngineConfig
    ExecutionEngineConfig (..),
    newExecutionEngineConfig,
    executionEngineConfig_type,
    executionEngineConfig_masterInstanceSecurityGroupId,
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
    hadoopJarStepConfig_mainClass,
    hadoopJarStepConfig_properties,
    hadoopJarStepConfig_args,
    hadoopJarStepConfig_jar,

    -- * HadoopStepConfig
    HadoopStepConfig (..),
    newHadoopStepConfig,
    hadoopStepConfig_mainClass,
    hadoopStepConfig_properties,
    hadoopStepConfig_jar,
    hadoopStepConfig_args,

    -- * Instance
    Instance (..),
    newInstance,
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

    -- * InstanceFleet
    InstanceFleet (..),
    newInstanceFleet,
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

    -- * InstanceFleetConfig
    InstanceFleetConfig (..),
    newInstanceFleetConfig,
    instanceFleetConfig_name,
    instanceFleetConfig_targetOnDemandCapacity,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_targetSpotCapacity,
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
    instanceFleetStateChangeReason_message,
    instanceFleetStateChangeReason_code,

    -- * InstanceFleetStatus
    InstanceFleetStatus (..),
    newInstanceFleetStatus,
    instanceFleetStatus_stateChangeReason,
    instanceFleetStatus_timeline,
    instanceFleetStatus_state,

    -- * InstanceFleetTimeline
    InstanceFleetTimeline (..),
    newInstanceFleetTimeline,
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_readyDateTime,
    instanceFleetTimeline_endDateTime,

    -- * InstanceGroup
    InstanceGroup (..),
    newInstanceGroup,
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

    -- * InstanceGroupConfig
    InstanceGroupConfig (..),
    newInstanceGroupConfig,
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

    -- * InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    newInstanceGroupModifyConfig,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_shrinkPolicy,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_reconfigurationType,
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
    instanceGroupStatus_timeline,
    instanceGroupStatus_state,

    -- * InstanceGroupTimeline
    InstanceGroupTimeline (..),
    newInstanceGroupTimeline,
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_readyDateTime,
    instanceGroupTimeline_endDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy (..),
    newInstanceResizePolicy,
    instanceResizePolicy_instancesToTerminate,
    instanceResizePolicy_instanceTerminationTimeout,
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
    instanceStatus_timeline,
    instanceStatus_state,

    -- * InstanceTimeline
    InstanceTimeline (..),
    newInstanceTimeline,
    instanceTimeline_creationDateTime,
    instanceTimeline_readyDateTime,
    instanceTimeline_endDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig (..),
    newInstanceTypeConfig,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_configurations,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification (..),
    newInstanceTypeSpecification,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_weightedCapacity,
    instanceTypeSpecification_ebsBlockDevices,

    -- * JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    newJobFlowInstancesConfig,
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

    -- * KerberosAttributes
    KerberosAttributes (..),
    newKerberosAttributes,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_realm,
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

    -- * NotebookExecutionSummary
    NotebookExecutionSummary (..),
    newNotebookExecutionSummary,
    notebookExecutionSummary_status,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_endTime,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_notebookExecutionId,

    -- * OSRelease
    OSRelease (..),
    newOSRelease,
    oSRelease_label,

    -- * OnDemandCapacityReservationOptions
    OnDemandCapacityReservationOptions (..),
    newOnDemandCapacityReservationOptions,
    onDemandCapacityReservationOptions_capacityReservationPreference,
    onDemandCapacityReservationOptions_capacityReservationResourceGroupArn,
    onDemandCapacityReservationOptions_usageStrategy,

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
    sessionMappingDetail_studioId,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_identityName,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_creationTime,
    sessionMappingDetail_identityId,
    sessionMappingDetail_identityType,

    -- * SessionMappingSummary
    SessionMappingSummary (..),
    newSessionMappingSummary,
    sessionMappingSummary_studioId,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_identityName,
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityId,
    sessionMappingSummary_identityType,

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
    step_name,
    step_status,
    step_id,
    step_actionOnFailure,
    step_executionRoleArn,
    step_config,

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
    stepStatus_timeline,
    stepStatus_state,
    stepStatus_failureDetails,

    -- * StepSummary
    StepSummary (..),
    newStepSummary,
    stepSummary_name,
    stepSummary_status,
    stepSummary_id,
    stepSummary_actionOnFailure,
    stepSummary_config,

    -- * StepTimeline
    StepTimeline (..),
    newStepTimeline,
    stepTimeline_creationDateTime,
    stepTimeline_startDateTime,
    stepTimeline_endDateTime,

    -- * Studio
    Studio (..),
    newStudio,
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

    -- * StudioSummary
    StudioSummary (..),
    newStudioSummary,
    studioSummary_studioId,
    studioSummary_name,
    studioSummary_url,
    studioSummary_description,
    studioSummary_authMode,
    studioSummary_creationTime,
    studioSummary_vpcId,

    -- * SupportedProductConfig
    SupportedProductConfig (..),
    newSupportedProductConfig,
    supportedProductConfig_name,
    supportedProductConfig_args,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VolumeSpecification
    VolumeSpecification (..),
    newVolumeSpecification,
    volumeSpecification_throughput,
    volumeSpecification_iops,
    volumeSpecification_volumeType,
    volumeSpecification_sizeInGB,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.ActionOnFailure
import Amazonka.EMR.Types.AdjustmentType
import Amazonka.EMR.Types.Application
import Amazonka.EMR.Types.AuthMode
import Amazonka.EMR.Types.AutoScalingPolicy
import Amazonka.EMR.Types.AutoScalingPolicyDescription
import Amazonka.EMR.Types.AutoScalingPolicyState
import Amazonka.EMR.Types.AutoScalingPolicyStateChangeReason
import Amazonka.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import Amazonka.EMR.Types.AutoScalingPolicyStatus
import Amazonka.EMR.Types.AutoTerminationPolicy
import Amazonka.EMR.Types.BlockPublicAccessConfiguration
import Amazonka.EMR.Types.BlockPublicAccessConfigurationMetadata
import Amazonka.EMR.Types.BootstrapActionConfig
import Amazonka.EMR.Types.CancelStepsInfo
import Amazonka.EMR.Types.CancelStepsRequestStatus
import Amazonka.EMR.Types.CloudWatchAlarmDefinition
import Amazonka.EMR.Types.Cluster
import Amazonka.EMR.Types.ClusterState
import Amazonka.EMR.Types.ClusterStateChangeReason
import Amazonka.EMR.Types.ClusterStateChangeReasonCode
import Amazonka.EMR.Types.ClusterStatus
import Amazonka.EMR.Types.ClusterSummary
import Amazonka.EMR.Types.ClusterTimeline
import Amazonka.EMR.Types.Command
import Amazonka.EMR.Types.ComparisonOperator
import Amazonka.EMR.Types.ComputeLimits
import Amazonka.EMR.Types.ComputeLimitsUnitType
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.EbsBlockDevice
import Amazonka.EMR.Types.EbsBlockDeviceConfig
import Amazonka.EMR.Types.EbsConfiguration
import Amazonka.EMR.Types.EbsVolume
import Amazonka.EMR.Types.Ec2InstanceAttributes
import Amazonka.EMR.Types.ExecutionEngineConfig
import Amazonka.EMR.Types.ExecutionEngineType
import Amazonka.EMR.Types.FailureDetails
import Amazonka.EMR.Types.HadoopJarStepConfig
import Amazonka.EMR.Types.HadoopStepConfig
import Amazonka.EMR.Types.IdentityType
import Amazonka.EMR.Types.Instance
import Amazonka.EMR.Types.InstanceCollectionType
import Amazonka.EMR.Types.InstanceFleet
import Amazonka.EMR.Types.InstanceFleetConfig
import Amazonka.EMR.Types.InstanceFleetModifyConfig
import Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications
import Amazonka.EMR.Types.InstanceFleetState
import Amazonka.EMR.Types.InstanceFleetStateChangeReason
import Amazonka.EMR.Types.InstanceFleetStateChangeReasonCode
import Amazonka.EMR.Types.InstanceFleetStatus
import Amazonka.EMR.Types.InstanceFleetTimeline
import Amazonka.EMR.Types.InstanceFleetType
import Amazonka.EMR.Types.InstanceGroup
import Amazonka.EMR.Types.InstanceGroupConfig
import Amazonka.EMR.Types.InstanceGroupModifyConfig
import Amazonka.EMR.Types.InstanceGroupState
import Amazonka.EMR.Types.InstanceGroupStateChangeReason
import Amazonka.EMR.Types.InstanceGroupStateChangeReasonCode
import Amazonka.EMR.Types.InstanceGroupStatus
import Amazonka.EMR.Types.InstanceGroupTimeline
import Amazonka.EMR.Types.InstanceGroupType
import Amazonka.EMR.Types.InstanceResizePolicy
import Amazonka.EMR.Types.InstanceRoleType
import Amazonka.EMR.Types.InstanceState
import Amazonka.EMR.Types.InstanceStateChangeReason
import Amazonka.EMR.Types.InstanceStateChangeReasonCode
import Amazonka.EMR.Types.InstanceStatus
import Amazonka.EMR.Types.InstanceTimeline
import Amazonka.EMR.Types.InstanceTypeConfig
import Amazonka.EMR.Types.InstanceTypeSpecification
import Amazonka.EMR.Types.JobFlowInstancesConfig
import Amazonka.EMR.Types.KerberosAttributes
import Amazonka.EMR.Types.KeyValue
import Amazonka.EMR.Types.ManagedScalingPolicy
import Amazonka.EMR.Types.MarketType
import Amazonka.EMR.Types.MetricDimension
import Amazonka.EMR.Types.NotebookExecution
import Amazonka.EMR.Types.NotebookExecutionStatus
import Amazonka.EMR.Types.NotebookExecutionSummary
import Amazonka.EMR.Types.OSRelease
import Amazonka.EMR.Types.OnDemandCapacityReservationOptions
import Amazonka.EMR.Types.OnDemandCapacityReservationPreference
import Amazonka.EMR.Types.OnDemandCapacityReservationUsageStrategy
import Amazonka.EMR.Types.OnDemandProvisioningAllocationStrategy
import Amazonka.EMR.Types.OnDemandProvisioningSpecification
import Amazonka.EMR.Types.PlacementGroupConfig
import Amazonka.EMR.Types.PlacementGroupStrategy
import Amazonka.EMR.Types.PlacementType
import Amazonka.EMR.Types.PortRange
import Amazonka.EMR.Types.ReconfigurationType
import Amazonka.EMR.Types.ReleaseLabelFilter
import Amazonka.EMR.Types.RepoUpgradeOnBoot
import Amazonka.EMR.Types.ScaleDownBehavior
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
import Amazonka.EMR.Types.SpotProvisioningAllocationStrategy
import Amazonka.EMR.Types.SpotProvisioningSpecification
import Amazonka.EMR.Types.SpotProvisioningTimeoutAction
import Amazonka.EMR.Types.Statistic
import Amazonka.EMR.Types.Step
import Amazonka.EMR.Types.StepCancellationOption
import Amazonka.EMR.Types.StepConfig
import Amazonka.EMR.Types.StepState
import Amazonka.EMR.Types.StepStateChangeReason
import Amazonka.EMR.Types.StepStateChangeReasonCode
import Amazonka.EMR.Types.StepStatus
import Amazonka.EMR.Types.StepSummary
import Amazonka.EMR.Types.StepTimeline
import Amazonka.EMR.Types.Studio
import Amazonka.EMR.Types.StudioSummary
import Amazonka.EMR.Types.SupportedProductConfig
import Amazonka.EMR.Types.Tag
import Amazonka.EMR.Types.Unit
import Amazonka.EMR.Types.VolumeSpecification
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2009-03-31@ of the Amazon EMR SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "EMR",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elasticmapreduce",
      Core.signingName = "elasticmapreduce",
      Core.version = "2009-03-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "EMR",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception occurs when there is an internal failure in the Amazon
-- EMR service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | Indicates that an error occurred while processing the request and that
-- the request was not completed.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
