-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types
  ( -- * Service configuration
    emrService,

    -- * Errors

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
    mkApplication,
    aArgs,
    aAdditionalInfo,
    aName,
    aVersion,

    -- * AutoScalingPolicy
    AutoScalingPolicy (..),
    mkAutoScalingPolicy,
    aspConstraints,
    aspRules,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdStatus,
    aspdRules,
    aspdConstraints,

    -- * AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (..),
    mkAutoScalingPolicyStateChangeReason,
    aspscrCode,
    aspscrMessage,

    -- * AutoScalingPolicyStatus
    AutoScalingPolicyStatus (..),
    mkAutoScalingPolicyStatus,
    aspsState,
    aspsStateChangeReason,

    -- * BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration (..),
    mkBlockPublicAccessConfiguration,
    bpacPermittedPublicSecurityGroupRuleRanges,
    bpacBlockPublicSecurityGroupRules,

    -- * BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata (..),
    mkBlockPublicAccessConfigurationMetadata,
    bpacmCreationDateTime,
    bpacmCreatedByARN,

    -- * BootstrapActionConfig
    BootstrapActionConfig (..),
    mkBootstrapActionConfig,
    bacName,
    bacScriptBootstrapAction,

    -- * CancelStepsInfo
    CancelStepsInfo (..),
    mkCancelStepsInfo,
    csiStatus,
    csiStepId,
    csiReason,

    -- * CloudWatchAlarmDefinition
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

    -- * Cluster
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

    -- * ClusterStateChangeReason
    ClusterStateChangeReason (..),
    mkClusterStateChangeReason,
    cscrCode,
    cscrMessage,

    -- * ClusterStatus
    ClusterStatus (..),
    mkClusterStatus,
    csState,
    csStateChangeReason,
    csTimeline,

    -- * ClusterSummary
    ClusterSummary (..),
    mkClusterSummary,
    cStatus,
    cClusterARN,
    cOutpostARN,
    cNormalizedInstanceHours,
    cName,
    cId,

    -- * ClusterTimeline
    ClusterTimeline (..),
    mkClusterTimeline,
    ctReadyDateTime,
    ctCreationDateTime,
    ctEndDateTime,

    -- * Command
    Command (..),
    mkCommand,
    comArgs,
    comScriptPath,
    comName,

    -- * ComputeLimits
    ComputeLimits (..),
    mkComputeLimits,
    clMaximumOnDemandCapacityUnits,
    clMaximumCoreCapacityUnits,
    clUnitType,
    clMinimumCapacityUnits,
    clMaximumCapacityUnits,

    -- * Configuration
    Configuration (..),
    mkConfiguration,
    cConfigurations,
    cClassification,
    cProperties,

    -- * EBSBlockDevice
    EBSBlockDevice (..),
    mkEBSBlockDevice,
    ebdDevice,
    ebdVolumeSpecification,

    -- * EBSBlockDeviceConfig
    EBSBlockDeviceConfig (..),
    mkEBSBlockDeviceConfig,
    ebdcVolumesPerInstance,
    ebdcVolumeSpecification,

    -- * EBSConfiguration
    EBSConfiguration (..),
    mkEBSConfiguration,
    ecEBSOptimized,
    ecEBSBlockDeviceConfigs,

    -- * EBSVolume
    EBSVolume (..),
    mkEBSVolume,
    evDevice,
    evVolumeId,

    -- * EC2InstanceAttributes
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

    -- * ExecutionEngineConfig
    ExecutionEngineConfig (..),
    mkExecutionEngineConfig,
    eecMasterInstanceSecurityGroupId,
    eecType,
    eecId,

    -- * FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdLogFile,
    fdReason,
    fdMessage,

    -- * HadoopJARStepConfig
    HadoopJARStepConfig (..),
    mkHadoopJARStepConfig,
    hjscArgs,
    hjscMainClass,
    hjscProperties,
    hjscJAR,

    -- * HadoopStepConfig
    HadoopStepConfig (..),
    mkHadoopStepConfig,
    hscArgs,
    hscJAR,
    hscMainClass,
    hscProperties,

    -- * Instance
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

    -- * InstanceFleet
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

    -- * InstanceFleetConfig
    InstanceFleetConfig (..),
    mkInstanceFleetConfig,
    ifcInstanceTypeConfigs,
    ifcTargetOnDemandCapacity,
    ifcName,
    ifcTargetSpotCapacity,
    ifcLaunchSpecifications,
    ifcInstanceFleetType,

    -- * InstanceFleetModifyConfig
    InstanceFleetModifyConfig (..),
    mkInstanceFleetModifyConfig,
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,
    ifmcInstanceFleetId,

    -- * InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications (..),
    mkInstanceFleetProvisioningSpecifications,
    ifpsSpotSpecification,
    ifpsOnDemandSpecification,

    -- * InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (..),
    mkInstanceFleetStateChangeReason,
    ifscrCode,
    ifscrMessage,

    -- * InstanceFleetStatus
    InstanceFleetStatus (..),
    mkInstanceFleetStatus,
    ifsState,
    ifsStateChangeReason,
    ifsTimeline,

    -- * InstanceFleetTimeline
    InstanceFleetTimeline (..),
    mkInstanceFleetTimeline,
    iftReadyDateTime,
    iftCreationDateTime,
    iftEndDateTime,

    -- * InstanceGroup
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

    -- * InstanceGroupConfig
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

    -- * InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    mkInstanceGroupModifyConfig,
    igmcInstanceCount,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcShrinkPolicy,
    igmcInstanceGroupId,

    -- * InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (..),
    mkInstanceGroupStateChangeReason,
    igscrCode,
    igscrMessage,

    -- * InstanceGroupStatus
    InstanceGroupStatus (..),
    mkInstanceGroupStatus,
    igsState,
    igsStateChangeReason,
    igsTimeline,

    -- * InstanceGroupTimeline
    InstanceGroupTimeline (..),
    mkInstanceGroupTimeline,
    igtReadyDateTime,
    igtCreationDateTime,
    igtEndDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy (..),
    mkInstanceResizePolicy,
    irpInstancesToProtect,
    irpInstancesToTerminate,
    irpInstanceTerminationTimeout,

    -- * InstanceStateChangeReason
    InstanceStateChangeReason (..),
    mkInstanceStateChangeReason,
    iscrCode,
    iscrMessage,

    -- * InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    isState,
    isStateChangeReason,
    isTimeline,

    -- * InstanceTimeline
    InstanceTimeline (..),
    mkInstanceTimeline,
    itReadyDateTime,
    itCreationDateTime,
    itEndDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig (..),
    mkInstanceTypeConfig,
    itcEBSConfiguration,
    itcBidPrice,
    itcWeightedCapacity,
    itcConfigurations,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcInstanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification (..),
    mkInstanceTypeSpecification,
    itsBidPrice,
    itsWeightedCapacity,
    itsConfigurations,
    itsEBSBlockDevices,
    itsInstanceType,
    itsEBSOptimized,
    itsBidPriceAsPercentageOfOnDemandPrice,

    -- * JobFlowInstancesConfig
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

    -- * KerberosAttributes
    KerberosAttributes (..),
    mkKerberosAttributes,
    kaKdcAdminPassword,
    kaRealm,
    kaADDomainJoinPassword,
    kaCrossRealmTrustPrincipalPassword,
    kaADDomainJoinUser,

    -- * KeyValue
    KeyValue (..),
    mkKeyValue,
    kvValue,
    kvKey,

    -- * ManagedScalingPolicy
    ManagedScalingPolicy (..),
    mkManagedScalingPolicy,
    mspComputeLimits,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdKey,

    -- * NotebookExecution
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

    -- * NotebookExecutionSummary
    NotebookExecutionSummary (..),
    mkNotebookExecutionSummary,
    nesStatus,
    nesEditorId,
    nesStartTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesEndTime,

    -- * OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (..),
    mkOnDemandProvisioningSpecification,
    odpsAllocationStrategy,

    -- * PlacementGroupConfig
    PlacementGroupConfig (..),
    mkPlacementGroupConfig,
    pgcPlacementStrategy,
    pgcInstanceRole,

    -- * PlacementType
    PlacementType (..),
    mkPlacementType,
    ptAvailabilityZones,
    ptAvailabilityZone,

    -- * PortRange
    PortRange (..),
    mkPortRange,
    prMaxRange,
    prMinRange,

    -- * ScalingAction
    ScalingAction (..),
    mkScalingAction,
    saMarket,
    saSimpleScalingPolicyConfiguration,

    -- * ScalingConstraints
    ScalingConstraints (..),
    mkScalingConstraints,
    scMinCapacity,
    scMaxCapacity,

    -- * ScalingRule
    ScalingRule (..),
    mkScalingRule,
    srDescription,
    srName,
    srAction,
    srTrigger,

    -- * ScalingTrigger
    ScalingTrigger (..),
    mkScalingTrigger,
    stCloudWatchAlarmDefinition,

    -- * ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig (..),
    mkScriptBootstrapActionConfig,
    sbacArgs,
    sbacPath,

    -- * SecurityConfigurationSummary
    SecurityConfigurationSummary (..),
    mkSecurityConfigurationSummary,
    scsName,
    scsCreationDateTime,

    -- * SessionMappingDetail
    SessionMappingDetail (..),
    mkSessionMappingDetail,
    smdCreationTime,
    smdStudioId,
    smdLastModifiedTime,
    smdIdentityType,
    smdIdentityId,
    smdSessionPolicyARN,
    smdIdentityName,

    -- * SessionMappingSummary
    SessionMappingSummary (..),
    mkSessionMappingSummary,
    smsCreationTime,
    smsStudioId,
    smsIdentityType,
    smsIdentityId,
    smsSessionPolicyARN,
    smsIdentityName,

    -- * ShrinkPolicy
    ShrinkPolicy (..),
    mkShrinkPolicy,
    spDecommissionTimeout,
    spInstanceResizePolicy,

    -- * SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (..),
    mkSimpleScalingPolicyConfiguration,
    sspcAdjustmentType,
    sspcCoolDown,
    sspcScalingAdjustment,

    -- * SpotProvisioningSpecification
    SpotProvisioningSpecification (..),
    mkSpotProvisioningSpecification,
    spsBlockDurationMinutes,
    spsAllocationStrategy,
    spsTimeoutDurationMinutes,
    spsTimeoutAction,

    -- * Step
    Step (..),
    mkStep,
    sStatus,
    sActionOnFailure,
    sConfig,
    sName,
    sId,

    -- * StepConfig
    StepConfig (..),
    mkStepConfig,
    scActionOnFailure,
    scName,
    scHadoopJARStep,

    -- * StepStateChangeReason
    StepStateChangeReason (..),
    mkStepStateChangeReason,
    sscrCode,
    sscrMessage,

    -- * StepStatus
    StepStatus (..),
    mkStepStatus,
    ssState,
    ssFailureDetails,
    ssStateChangeReason,
    ssTimeline,

    -- * StepSummary
    StepSummary (..),
    mkStepSummary,
    steStatus,
    steActionOnFailure,
    steConfig,
    steName,
    steId,

    -- * StepTimeline
    StepTimeline (..),
    mkStepTimeline,
    stCreationDateTime,
    stEndDateTime,
    stStartDateTime,

    -- * Studio
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

    -- * StudioSummary
    StudioSummary (..),
    mkStudioSummary,
    ssCreationTime,
    ssStudioId,
    ssVPCId,
    ssURL,
    ssName,
    ssDescription,

    -- * SupportedProductConfig
    SupportedProductConfig (..),
    mkSupportedProductConfig,
    spcArgs,
    spcName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * VolumeSpecification
    VolumeSpecification (..),
    mkVolumeSpecification,
    vsIOPS,
    vsVolumeType,
    vsSizeInGB,
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
import Network.AWS.EMR.Types.EBSBlockDevice
import Network.AWS.EMR.Types.EBSBlockDeviceConfig
import Network.AWS.EMR.Types.EBSConfiguration
import Network.AWS.EMR.Types.EBSVolume
import Network.AWS.EMR.Types.EC2InstanceAttributes
import Network.AWS.EMR.Types.ExecutionEngineConfig
import Network.AWS.EMR.Types.ExecutionEngineType
import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.HadoopJARStepConfig
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2009-03-31@ of the Amazon Elastic MapReduce SDK configuration.
emrService :: Lude.Service
emrService =
  Lude.Service
    { Lude._svcAbbrev = "EMR",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticmapreduce",
      Lude._svcVersion = "2009-03-31",
      Lude._svcEndpoint = Lude.defaultEndpoint emrService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "EMR",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
