{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EMR is a web service that makes it easier to process large amounts of data efficiently. Amazon EMR uses Hadoop processing combined with several AWS services to do tasks such as web indexing, data mining, log file analysis, machine learning, scientific simulation, and data warehouse management.
module Network.AWS.EMR
  ( -- * Service configuration
    emrService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** StepComplete
    mkStepComplete,

    -- ** ClusterTerminated
    mkClusterTerminated,

    -- ** ClusterRunning
    mkClusterRunning,

    -- * Operations
    -- $operations

    -- ** RunJobFlow
    module Network.AWS.EMR.RunJobFlow,

    -- ** RemoveAutoScalingPolicy
    module Network.AWS.EMR.RemoveAutoScalingPolicy,

    -- ** CreateStudio
    module Network.AWS.EMR.CreateStudio,

    -- ** SetVisibleToAllUsers
    module Network.AWS.EMR.SetVisibleToAllUsers,

    -- ** TerminateJobFlows
    module Network.AWS.EMR.TerminateJobFlows,

    -- ** DescribeStep
    module Network.AWS.EMR.DescribeStep,

    -- ** RemoveTags
    module Network.AWS.EMR.RemoveTags,

    -- ** DescribeCluster
    module Network.AWS.EMR.DescribeCluster,

    -- ** ListSecurityConfigurations (Paginated)
    module Network.AWS.EMR.ListSecurityConfigurations,

    -- ** CancelSteps
    module Network.AWS.EMR.CancelSteps,

    -- ** ListNotebookExecutions (Paginated)
    module Network.AWS.EMR.ListNotebookExecutions,

    -- ** CreateSecurityConfiguration
    module Network.AWS.EMR.CreateSecurityConfiguration,

    -- ** SetTerminationProtection
    module Network.AWS.EMR.SetTerminationProtection,

    -- ** AddJobFlowSteps
    module Network.AWS.EMR.AddJobFlowSteps,

    -- ** DescribeStudio
    module Network.AWS.EMR.DescribeStudio,

    -- ** ModifyInstanceGroups
    module Network.AWS.EMR.ModifyInstanceGroups,

    -- ** StartNotebookExecution
    module Network.AWS.EMR.StartNotebookExecution,

    -- ** ListSteps (Paginated)
    module Network.AWS.EMR.ListSteps,

    -- ** CreateStudioSessionMapping
    module Network.AWS.EMR.CreateStudioSessionMapping,

    -- ** AddInstanceFleet
    module Network.AWS.EMR.AddInstanceFleet,

    -- ** DeleteStudio
    module Network.AWS.EMR.DeleteStudio,

    -- ** ListStudios (Paginated)
    module Network.AWS.EMR.ListStudios,

    -- ** PutManagedScalingPolicy
    module Network.AWS.EMR.PutManagedScalingPolicy,

    -- ** AddInstanceGroups
    module Network.AWS.EMR.AddInstanceGroups,

    -- ** GetStudioSessionMapping
    module Network.AWS.EMR.GetStudioSessionMapping,

    -- ** DeleteSecurityConfiguration
    module Network.AWS.EMR.DeleteSecurityConfiguration,

    -- ** ModifyInstanceFleet
    module Network.AWS.EMR.ModifyInstanceFleet,

    -- ** ListInstanceGroups (Paginated)
    module Network.AWS.EMR.ListInstanceGroups,

    -- ** GetBlockPublicAccessConfiguration
    module Network.AWS.EMR.GetBlockPublicAccessConfiguration,

    -- ** ModifyCluster
    module Network.AWS.EMR.ModifyCluster,

    -- ** PutBlockPublicAccessConfiguration
    module Network.AWS.EMR.PutBlockPublicAccessConfiguration,

    -- ** ListBootstrapActions (Paginated)
    module Network.AWS.EMR.ListBootstrapActions,

    -- ** AddTags
    module Network.AWS.EMR.AddTags,

    -- ** ListInstances (Paginated)
    module Network.AWS.EMR.ListInstances,

    -- ** PutAutoScalingPolicy
    module Network.AWS.EMR.PutAutoScalingPolicy,

    -- ** DeleteStudioSessionMapping
    module Network.AWS.EMR.DeleteStudioSessionMapping,

    -- ** UpdateStudioSessionMapping
    module Network.AWS.EMR.UpdateStudioSessionMapping,

    -- ** ListClusters (Paginated)
    module Network.AWS.EMR.ListClusters,

    -- ** DescribeSecurityConfiguration
    module Network.AWS.EMR.DescribeSecurityConfiguration,

    -- ** StopNotebookExecution
    module Network.AWS.EMR.StopNotebookExecution,

    -- ** ListStudioSessionMappings (Paginated)
    module Network.AWS.EMR.ListStudioSessionMappings,

    -- ** GetManagedScalingPolicy
    module Network.AWS.EMR.GetManagedScalingPolicy,

    -- ** ListInstanceFleets (Paginated)
    module Network.AWS.EMR.ListInstanceFleets,

    -- ** RemoveManagedScalingPolicy
    module Network.AWS.EMR.RemoveManagedScalingPolicy,

    -- ** DescribeNotebookExecution
    module Network.AWS.EMR.DescribeNotebookExecution,

    -- * Types

    -- ** ActionOnFailure
    ActionOnFailure (..),

    -- ** AdjustmentType
    AdjustmentType (..),

    -- ** AuthMode
    AuthMode (..),

    -- ** AutoScalingPolicyState
    AutoScalingPolicyState (..),

    -- ** AutoScalingPolicyStateChangeReasonCode
    AutoScalingPolicyStateChangeReasonCode (..),

    -- ** CancelStepsRequestStatus
    CancelStepsRequestStatus (..),

    -- ** ClusterState
    ClusterState (..),

    -- ** ClusterStateChangeReasonCode
    ClusterStateChangeReasonCode (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** ComputeLimitsUnitType
    ComputeLimitsUnitType (..),

    -- ** ExecutionEngineType
    ExecutionEngineType (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** InstanceCollectionType
    InstanceCollectionType (..),

    -- ** InstanceFleetState
    InstanceFleetState (..),

    -- ** InstanceFleetStateChangeReasonCode
    InstanceFleetStateChangeReasonCode (..),

    -- ** InstanceFleetType
    InstanceFleetType (..),

    -- ** InstanceGroupState
    InstanceGroupState (..),

    -- ** InstanceGroupStateChangeReasonCode
    InstanceGroupStateChangeReasonCode (..),

    -- ** InstanceGroupType
    InstanceGroupType (..),

    -- ** InstanceRoleType
    InstanceRoleType (..),

    -- ** InstanceState
    InstanceState (..),

    -- ** InstanceStateChangeReasonCode
    InstanceStateChangeReasonCode (..),

    -- ** MarketType
    MarketType (..),

    -- ** NotebookExecutionStatus
    NotebookExecutionStatus (..),

    -- ** OnDemandProvisioningAllocationStrategy
    OnDemandProvisioningAllocationStrategy (..),

    -- ** PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- ** RepoUpgradeOnBoot
    RepoUpgradeOnBoot (..),

    -- ** ScaleDownBehavior
    ScaleDownBehavior (..),

    -- ** SpotProvisioningAllocationStrategy
    SpotProvisioningAllocationStrategy (..),

    -- ** SpotProvisioningTimeoutAction
    SpotProvisioningTimeoutAction (..),

    -- ** Statistic
    Statistic (..),

    -- ** StepCancellationOption
    StepCancellationOption (..),

    -- ** StepState
    StepState (..),

    -- ** StepStateChangeReasonCode
    StepStateChangeReasonCode (..),

    -- ** Unit
    Unit (..),

    -- ** Application
    Application (..),
    mkApplication,
    aArgs,
    aAdditionalInfo,
    aName,
    aVersion,

    -- ** AutoScalingPolicy
    AutoScalingPolicy (..),
    mkAutoScalingPolicy,
    aspConstraints,
    aspRules,

    -- ** AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdStatus,
    aspdRules,
    aspdConstraints,

    -- ** AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (..),
    mkAutoScalingPolicyStateChangeReason,
    aspscrCode,
    aspscrMessage,

    -- ** AutoScalingPolicyStatus
    AutoScalingPolicyStatus (..),
    mkAutoScalingPolicyStatus,
    aspsState,
    aspsStateChangeReason,

    -- ** BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration (..),
    mkBlockPublicAccessConfiguration,
    bpacPermittedPublicSecurityGroupRuleRanges,
    bpacBlockPublicSecurityGroupRules,

    -- ** BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata (..),
    mkBlockPublicAccessConfigurationMetadata,
    bpacmCreationDateTime,
    bpacmCreatedByARN,

    -- ** BootstrapActionConfig
    BootstrapActionConfig (..),
    mkBootstrapActionConfig,
    bacName,
    bacScriptBootstrapAction,

    -- ** CancelStepsInfo
    CancelStepsInfo (..),
    mkCancelStepsInfo,
    csiStatus,
    csiStepId,
    csiReason,

    -- ** CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (..),
    mkCloudWatchAlarmDefinition,
    cwadEvaluationPeriods,
    cwadNamespace,
    cwadDimensions,
    cwadUnit,
    cwadStatistic,
    cwadComparisonOperator,
    cwadMetricName,
    cwadPeriod,
    cwadThreshold,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cluLogEncryptionKMSKeyId,
    cluClusterARN,
    cluRequestedAMIVersion,
    cluEBSRootVolumeSize,
    cluEC2InstanceAttributes,
    cluOutpostARN,
    cluNormalizedInstanceHours,
    cluConfigurations,
    cluCustomAMIId,
    cluAutoScalingRole,
    cluSecurityConfiguration,
    cluScaleDownBehavior,
    cluInstanceCollectionType,
    cluReleaseLabel,
    cluRepoUpgradeOnBoot,
    cluLogURI,
    cluKerberosAttributes,
    cluPlacementGroups,
    cluRunningAMIVersion,
    cluMasterPublicDNSName,
    cluTerminationProtected,
    cluVisibleToAllUsers,
    cluAutoTerminate,
    cluStepConcurrencyLevel,
    cluApplications,
    cluTags,
    cluServiceRole,
    cluId,
    cluName,
    cluStatus,

    -- ** ClusterStateChangeReason
    ClusterStateChangeReason (..),
    mkClusterStateChangeReason,
    cscrCode,
    cscrMessage,

    -- ** ClusterStatus
    ClusterStatus (..),
    mkClusterStatus,
    csState,
    csStateChangeReason,
    csTimeline,

    -- ** ClusterSummary
    ClusterSummary (..),
    mkClusterSummary,
    cStatus,
    cClusterARN,
    cOutpostARN,
    cNormalizedInstanceHours,
    cName,
    cId,

    -- ** ClusterTimeline
    ClusterTimeline (..),
    mkClusterTimeline,
    ctReadyDateTime,
    ctCreationDateTime,
    ctEndDateTime,

    -- ** Command
    Command (..),
    mkCommand,
    comArgs,
    comScriptPath,
    comName,

    -- ** ComputeLimits
    ComputeLimits (..),
    mkComputeLimits,
    clMaximumOnDemandCapacityUnits,
    clMaximumCoreCapacityUnits,
    clUnitType,
    clMinimumCapacityUnits,
    clMaximumCapacityUnits,

    -- ** Configuration
    Configuration (..),
    mkConfiguration,
    cConfigurations,
    cClassification,
    cProperties,

    -- ** EBSBlockDevice
    EBSBlockDevice (..),
    mkEBSBlockDevice,
    ebdDevice,
    ebdVolumeSpecification,

    -- ** EBSBlockDeviceConfig
    EBSBlockDeviceConfig (..),
    mkEBSBlockDeviceConfig,
    ebdcVolumesPerInstance,
    ebdcVolumeSpecification,

    -- ** EBSConfiguration
    EBSConfiguration (..),
    mkEBSConfiguration,
    ecEBSOptimized,
    ecEBSBlockDeviceConfigs,

    -- ** EBSVolume
    EBSVolume (..),
    mkEBSVolume,
    evDevice,
    evVolumeId,

    -- ** EC2InstanceAttributes
    EC2InstanceAttributes (..),
    mkEC2InstanceAttributes,
    eiaEC2KeyName,
    eiaEmrManagedSlaveSecurityGroup,
    eiaAdditionalSlaveSecurityGroups,
    eiaRequestedEC2SubnetIds,
    eiaAdditionalMasterSecurityGroups,
    eiaIAMInstanceProfile,
    eiaEmrManagedMasterSecurityGroup,
    eiaEC2SubnetId,
    eiaRequestedEC2AvailabilityZones,
    eiaServiceAccessSecurityGroup,
    eiaEC2AvailabilityZone,

    -- ** ExecutionEngineConfig
    ExecutionEngineConfig (..),
    mkExecutionEngineConfig,
    eecMasterInstanceSecurityGroupId,
    eecType,
    eecId,

    -- ** FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdLogFile,
    fdReason,
    fdMessage,

    -- ** HadoopJARStepConfig
    HadoopJARStepConfig (..),
    mkHadoopJARStepConfig,
    hjscArgs,
    hjscMainClass,
    hjscProperties,
    hjscJAR,

    -- ** HadoopStepConfig
    HadoopStepConfig (..),
    mkHadoopStepConfig,
    hscArgs,
    hscJAR,
    hscMainClass,
    hscProperties,

    -- ** Instance
    Instance (..),
    mkInstance,
    iStatus,
    iPublicDNSName,
    iEBSVolumes,
    iEC2InstanceId,
    iInstanceType,
    iMarket,
    iPrivateIPAddress,
    iInstanceFleetId,
    iId,
    iInstanceGroupId,
    iPrivateDNSName,
    iPublicIPAddress,

    -- ** InstanceFleet
    InstanceFleet (..),
    mkInstanceFleet,
    ifProvisionedSpotCapacity,
    ifStatus,
    ifTargetOnDemandCapacity,
    ifInstanceFleetType,
    ifInstanceTypeSpecifications,
    ifName,
    ifProvisionedOnDemandCapacity,
    ifTargetSpotCapacity,
    ifId,
    ifLaunchSpecifications,

    -- ** InstanceFleetConfig
    InstanceFleetConfig (..),
    mkInstanceFleetConfig,
    ifcInstanceTypeConfigs,
    ifcTargetOnDemandCapacity,
    ifcName,
    ifcTargetSpotCapacity,
    ifcLaunchSpecifications,
    ifcInstanceFleetType,

    -- ** InstanceFleetModifyConfig
    InstanceFleetModifyConfig (..),
    mkInstanceFleetModifyConfig,
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,
    ifmcInstanceFleetId,

    -- ** InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications (..),
    mkInstanceFleetProvisioningSpecifications,
    ifpsSpotSpecification,
    ifpsOnDemandSpecification,

    -- ** InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (..),
    mkInstanceFleetStateChangeReason,
    ifscrCode,
    ifscrMessage,

    -- ** InstanceFleetStatus
    InstanceFleetStatus (..),
    mkInstanceFleetStatus,
    ifsState,
    ifsStateChangeReason,
    ifsTimeline,

    -- ** InstanceFleetTimeline
    InstanceFleetTimeline (..),
    mkInstanceFleetTimeline,
    iftReadyDateTime,
    iftCreationDateTime,
    iftEndDateTime,

    -- ** InstanceGroup
    InstanceGroup (..),
    mkInstanceGroup,
    igStatus,
    igLastSuccessfullyAppliedConfigurationsVersion,
    igBidPrice,
    igRequestedInstanceCount,
    igRunningInstanceCount,
    igLastSuccessfullyAppliedConfigurations,
    igConfigurations,
    igInstanceGroupType,
    igEBSBlockDevices,
    igInstanceType,
    igConfigurationsVersion,
    igEBSOptimized,
    igMarket,
    igName,
    igAutoScalingPolicy,
    igShrinkPolicy,
    igId,

    -- ** InstanceGroupConfig
    InstanceGroupConfig (..),
    mkInstanceGroupConfig,
    igcEBSConfiguration,
    igcBidPrice,
    igcConfigurations,
    igcMarket,
    igcName,
    igcAutoScalingPolicy,
    igcInstanceRole,
    igcInstanceType,
    igcInstanceCount,

    -- ** InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    mkInstanceGroupModifyConfig,
    igmcInstanceCount,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcShrinkPolicy,
    igmcInstanceGroupId,

    -- ** InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (..),
    mkInstanceGroupStateChangeReason,
    igscrCode,
    igscrMessage,

    -- ** InstanceGroupStatus
    InstanceGroupStatus (..),
    mkInstanceGroupStatus,
    igsState,
    igsStateChangeReason,
    igsTimeline,

    -- ** InstanceGroupTimeline
    InstanceGroupTimeline (..),
    mkInstanceGroupTimeline,
    igtReadyDateTime,
    igtCreationDateTime,
    igtEndDateTime,

    -- ** InstanceResizePolicy
    InstanceResizePolicy (..),
    mkInstanceResizePolicy,
    irpInstancesToProtect,
    irpInstancesToTerminate,
    irpInstanceTerminationTimeout,

    -- ** InstanceStateChangeReason
    InstanceStateChangeReason (..),
    mkInstanceStateChangeReason,
    iscrCode,
    iscrMessage,

    -- ** InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    isState,
    isStateChangeReason,
    isTimeline,

    -- ** InstanceTimeline
    InstanceTimeline (..),
    mkInstanceTimeline,
    itReadyDateTime,
    itCreationDateTime,
    itEndDateTime,

    -- ** InstanceTypeConfig
    InstanceTypeConfig (..),
    mkInstanceTypeConfig,
    itcEBSConfiguration,
    itcBidPrice,
    itcWeightedCapacity,
    itcConfigurations,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcInstanceType,

    -- ** InstanceTypeSpecification
    InstanceTypeSpecification (..),
    mkInstanceTypeSpecification,
    itsBidPrice,
    itsWeightedCapacity,
    itsConfigurations,
    itsEBSBlockDevices,
    itsInstanceType,
    itsEBSOptimized,
    itsBidPriceAsPercentageOfOnDemandPrice,

    -- ** JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    mkJobFlowInstancesConfig,
    jficInstanceFleets,
    jficEC2KeyName,
    jficSlaveInstanceType,
    jficInstanceCount,
    jficEmrManagedSlaveSecurityGroup,
    jficAdditionalSlaveSecurityGroups,
    jficEC2SubnetIds,
    jficHadoopVersion,
    jficAdditionalMasterSecurityGroups,
    jficEmrManagedMasterSecurityGroup,
    jficEC2SubnetId,
    jficMasterInstanceType,
    jficInstanceGroups,
    jficKeepJobFlowAliveWhenNoSteps,
    jficServiceAccessSecurityGroup,
    jficTerminationProtected,
    jficPlacement,

    -- ** KerberosAttributes
    KerberosAttributes (..),
    mkKerberosAttributes,
    kaKdcAdminPassword,
    kaRealm,
    kaADDomainJoinPassword,
    kaCrossRealmTrustPrincipalPassword,
    kaADDomainJoinUser,

    -- ** KeyValue
    KeyValue (..),
    mkKeyValue,
    kvValue,
    kvKey,

    -- ** ManagedScalingPolicy
    ManagedScalingPolicy (..),
    mkManagedScalingPolicy,
    mspComputeLimits,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdKey,

    -- ** NotebookExecution
    NotebookExecution (..),
    mkNotebookExecution,
    neStatus,
    neExecutionEngine,
    neNotebookInstanceSecurityGroupId,
    neEditorId,
    neStartTime,
    neARN,
    neOutputNotebookURI,
    neNotebookExecutionId,
    neNotebookExecutionName,
    neLastStateChangeReason,
    neEndTime,
    neNotebookParams,
    neTags,

    -- ** NotebookExecutionSummary
    NotebookExecutionSummary (..),
    mkNotebookExecutionSummary,
    nesStatus,
    nesEditorId,
    nesStartTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesEndTime,

    -- ** OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (..),
    mkOnDemandProvisioningSpecification,
    odpsAllocationStrategy,

    -- ** PlacementGroupConfig
    PlacementGroupConfig (..),
    mkPlacementGroupConfig,
    pgcPlacementStrategy,
    pgcInstanceRole,

    -- ** PlacementType
    PlacementType (..),
    mkPlacementType,
    ptAvailabilityZones,
    ptAvailabilityZone,

    -- ** PortRange
    PortRange (..),
    mkPortRange,
    prMaxRange,
    prMinRange,

    -- ** ScalingAction
    ScalingAction (..),
    mkScalingAction,
    saMarket,
    saSimpleScalingPolicyConfiguration,

    -- ** ScalingConstraints
    ScalingConstraints (..),
    mkScalingConstraints,
    scMinCapacity,
    scMaxCapacity,

    -- ** ScalingRule
    ScalingRule (..),
    mkScalingRule,
    srDescription,
    srName,
    srAction,
    srTrigger,

    -- ** ScalingTrigger
    ScalingTrigger (..),
    mkScalingTrigger,
    stCloudWatchAlarmDefinition,

    -- ** ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig (..),
    mkScriptBootstrapActionConfig,
    sbacArgs,
    sbacPath,

    -- ** SecurityConfigurationSummary
    SecurityConfigurationSummary (..),
    mkSecurityConfigurationSummary,
    scsName,
    scsCreationDateTime,

    -- ** SessionMappingDetail
    SessionMappingDetail (..),
    mkSessionMappingDetail,
    smdCreationTime,
    smdStudioId,
    smdLastModifiedTime,
    smdIdentityType,
    smdIdentityId,
    smdSessionPolicyARN,
    smdIdentityName,

    -- ** SessionMappingSummary
    SessionMappingSummary (..),
    mkSessionMappingSummary,
    smsCreationTime,
    smsStudioId,
    smsIdentityType,
    smsIdentityId,
    smsSessionPolicyARN,
    smsIdentityName,

    -- ** ShrinkPolicy
    ShrinkPolicy (..),
    mkShrinkPolicy,
    spDecommissionTimeout,
    spInstanceResizePolicy,

    -- ** SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (..),
    mkSimpleScalingPolicyConfiguration,
    sspcAdjustmentType,
    sspcCoolDown,
    sspcScalingAdjustment,

    -- ** SpotProvisioningSpecification
    SpotProvisioningSpecification (..),
    mkSpotProvisioningSpecification,
    spsBlockDurationMinutes,
    spsAllocationStrategy,
    spsTimeoutDurationMinutes,
    spsTimeoutAction,

    -- ** Step
    Step (..),
    mkStep,
    sStatus,
    sActionOnFailure,
    sConfig,
    sName,
    sId,

    -- ** StepConfig
    StepConfig (..),
    mkStepConfig,
    scActionOnFailure,
    scName,
    scHadoopJARStep,

    -- ** StepStateChangeReason
    StepStateChangeReason (..),
    mkStepStateChangeReason,
    sscrCode,
    sscrMessage,

    -- ** StepStatus
    StepStatus (..),
    mkStepStatus,
    ssState,
    ssFailureDetails,
    ssStateChangeReason,
    ssTimeline,

    -- ** StepSummary
    StepSummary (..),
    mkStepSummary,
    steStatus,
    steActionOnFailure,
    steConfig,
    steName,
    steId,

    -- ** StepTimeline
    StepTimeline (..),
    mkStepTimeline,
    stCreationDateTime,
    stEndDateTime,
    stStartDateTime,

    -- ** Studio
    Studio (..),
    mkStudio,
    stuCreationTime,
    stuEngineSecurityGroupId,
    stuSubnetIds,
    stuStudioId,
    stuVPCId,
    stuURL,
    stuAuthMode,
    stuDefaultS3Location,
    stuWorkspaceSecurityGroupId,
    stuName,
    stuStudioARN,
    stuUserRole,
    stuDescription,
    stuTags,
    stuServiceRole,

    -- ** StudioSummary
    StudioSummary (..),
    mkStudioSummary,
    ssCreationTime,
    ssStudioId,
    ssVPCId,
    ssURL,
    ssName,
    ssDescription,

    -- ** SupportedProductConfig
    SupportedProductConfig (..),
    mkSupportedProductConfig,
    spcArgs,
    spcName,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** VolumeSpecification
    VolumeSpecification (..),
    mkVolumeSpecification,
    vsIOPS,
    vsVolumeType,
    vsSizeInGB,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.EMR.Types
import Network.AWS.EMR.UpdateStudioSessionMapping
import Network.AWS.EMR.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EMR'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
