{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerError,
    _InternalServerException,
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
    application_additionalInfo,
    application_args,
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
    autoScalingPolicyDescription_constraints,
    autoScalingPolicyDescription_rules,
    autoScalingPolicyDescription_status,

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
    cancelStepsInfo_reason,
    cancelStepsInfo_status,
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
    clusterSummary_clusterArn,
    clusterSummary_id,
    clusterSummary_name,
    clusterSummary_normalizedInstanceHours,
    clusterSummary_outpostArn,
    clusterSummary_status,

    -- * ClusterTimeline
    ClusterTimeline (..),
    newClusterTimeline,
    clusterTimeline_creationDateTime,
    clusterTimeline_endDateTime,
    clusterTimeline_readyDateTime,

    -- * Command
    Command (..),
    newCommand,
    command_args,
    command_name,
    command_scriptPath,

    -- * ComputeLimits
    ComputeLimits (..),
    newComputeLimits,
    computeLimits_maximumCoreCapacityUnits,
    computeLimits_maximumOnDemandCapacityUnits,
    computeLimits_unitType,
    computeLimits_minimumCapacityUnits,
    computeLimits_maximumCapacityUnits,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_classification,
    configuration_configurations,
    configuration_properties,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_usernamePassword,

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
    failureDetails_message,
    failureDetails_reason,

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

    -- * InstanceFleet
    InstanceFleet (..),
    newInstanceFleet,
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

    -- * InstanceFleetConfig
    InstanceFleetConfig (..),
    newInstanceFleetConfig,
    instanceFleetConfig_instanceTypeConfigs,
    instanceFleetConfig_launchSpecifications,
    instanceFleetConfig_name,
    instanceFleetConfig_targetOnDemandCapacity,
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
    instanceFleetProvisioningSpecifications_onDemandSpecification,
    instanceFleetProvisioningSpecifications_spotSpecification,

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
    instanceFleetTimeline_creationDateTime,
    instanceFleetTimeline_endDateTime,
    instanceFleetTimeline_readyDateTime,

    -- * InstanceGroup
    InstanceGroup (..),
    newInstanceGroup,
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

    -- * InstanceGroupConfig
    InstanceGroupConfig (..),
    newInstanceGroupConfig,
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

    -- * InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    newInstanceGroupModifyConfig,
    instanceGroupModifyConfig_configurations,
    instanceGroupModifyConfig_eC2InstanceIdsToTerminate,
    instanceGroupModifyConfig_instanceCount,
    instanceGroupModifyConfig_reconfigurationType,
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
    instanceGroupTimeline_creationDateTime,
    instanceGroupTimeline_endDateTime,
    instanceGroupTimeline_readyDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy (..),
    newInstanceResizePolicy,
    instanceResizePolicy_instanceTerminationTimeout,
    instanceResizePolicy_instancesToProtect,
    instanceResizePolicy_instancesToTerminate,

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
    instanceTimeline_creationDateTime,
    instanceTimeline_endDateTime,
    instanceTimeline_readyDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig (..),
    newInstanceTypeConfig,
    instanceTypeConfig_bidPrice,
    instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeConfig_configurations,
    instanceTypeConfig_customAmiId,
    instanceTypeConfig_ebsConfiguration,
    instanceTypeConfig_weightedCapacity,
    instanceTypeConfig_instanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification (..),
    newInstanceTypeSpecification,
    instanceTypeSpecification_bidPrice,
    instanceTypeSpecification_bidPriceAsPercentageOfOnDemandPrice,
    instanceTypeSpecification_configurations,
    instanceTypeSpecification_customAmiId,
    instanceTypeSpecification_ebsBlockDevices,
    instanceTypeSpecification_ebsOptimized,
    instanceTypeSpecification_instanceType,
    instanceTypeSpecification_weightedCapacity,

    -- * JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    newJobFlowInstancesConfig,
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

    -- * KerberosAttributes
    KerberosAttributes (..),
    newKerberosAttributes,
    kerberosAttributes_aDDomainJoinPassword,
    kerberosAttributes_aDDomainJoinUser,
    kerberosAttributes_crossRealmTrustPrincipalPassword,
    kerberosAttributes_kdcAdminPassword,
    kerberosAttributes_realm,

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

    -- * NotebookExecutionSummary
    NotebookExecutionSummary (..),
    newNotebookExecutionSummary,
    notebookExecutionSummary_editorId,
    notebookExecutionSummary_endTime,
    notebookExecutionSummary_notebookExecutionId,
    notebookExecutionSummary_notebookExecutionName,
    notebookExecutionSummary_startTime,
    notebookExecutionSummary_status,

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
    placementType_availabilityZone,
    placementType_availabilityZones,

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
    securityConfigurationSummary_creationDateTime,
    securityConfigurationSummary_name,

    -- * SessionMappingDetail
    SessionMappingDetail (..),
    newSessionMappingDetail,
    sessionMappingDetail_creationTime,
    sessionMappingDetail_identityId,
    sessionMappingDetail_identityName,
    sessionMappingDetail_identityType,
    sessionMappingDetail_lastModifiedTime,
    sessionMappingDetail_sessionPolicyArn,
    sessionMappingDetail_studioId,

    -- * SessionMappingSummary
    SessionMappingSummary (..),
    newSessionMappingSummary,
    sessionMappingSummary_creationTime,
    sessionMappingSummary_identityId,
    sessionMappingSummary_identityName,
    sessionMappingSummary_identityType,
    sessionMappingSummary_sessionPolicyArn,
    sessionMappingSummary_studioId,

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
    spotProvisioningSpecification_allocationStrategy,
    spotProvisioningSpecification_blockDurationMinutes,
    spotProvisioningSpecification_timeoutDurationMinutes,
    spotProvisioningSpecification_timeoutAction,

    -- * Step
    Step (..),
    newStep,
    step_actionOnFailure,
    step_config,
    step_executionRoleArn,
    step_id,
    step_name,
    step_status,

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
    stepStatus_failureDetails,
    stepStatus_state,
    stepStatus_stateChangeReason,
    stepStatus_timeline,

    -- * StepSummary
    StepSummary (..),
    newStepSummary,
    stepSummary_actionOnFailure,
    stepSummary_config,
    stepSummary_id,
    stepSummary_name,
    stepSummary_status,

    -- * StepTimeline
    StepTimeline (..),
    newStepTimeline,
    stepTimeline_creationDateTime,
    stepTimeline_endDateTime,
    stepTimeline_startDateTime,

    -- * Studio
    Studio (..),
    newStudio,
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

    -- * StudioSummary
    StudioSummary (..),
    newStudioSummary,
    studioSummary_authMode,
    studioSummary_creationTime,
    studioSummary_description,
    studioSummary_name,
    studioSummary_studioId,
    studioSummary_url,
    studioSummary_vpcId,

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

    -- * UsernamePassword
    UsernamePassword (..),
    newUsernamePassword,
    usernamePassword_password,
    usernamePassword_username,

    -- * VolumeSpecification
    VolumeSpecification (..),
    newVolumeSpecification,
    volumeSpecification_iops,
    volumeSpecification_throughput,
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
import Amazonka.EMR.Types.Credentials
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
import Amazonka.EMR.Types.UsernamePassword
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that an error occurred while processing the request and that
-- the request was not completed.
_InternalServerError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | This exception occurs when there is an internal failure in the Amazon
-- EMR service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | This exception occurs when there is something wrong with user input.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
