{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types
  ( -- * Service Configuration
    emr,

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
    Application,
    application,
    aArgs,
    aAdditionalInfo,
    aName,
    aVersion,

    -- * AutoScalingPolicy
    AutoScalingPolicy,
    autoScalingPolicy,
    aspConstraints,
    aspRules,

    -- * AutoScalingPolicyDescription
    AutoScalingPolicyDescription,
    autoScalingPolicyDescription,
    aspdStatus,
    aspdRules,
    aspdConstraints,

    -- * AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason,
    autoScalingPolicyStateChangeReason,
    aspscrCode,
    aspscrMessage,

    -- * AutoScalingPolicyStatus
    AutoScalingPolicyStatus,
    autoScalingPolicyStatus,
    aspsState,
    aspsStateChangeReason,

    -- * BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration,
    blockPublicAccessConfiguration,
    bpacPermittedPublicSecurityGroupRuleRanges,
    bpacBlockPublicSecurityGroupRules,

    -- * BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata,
    blockPublicAccessConfigurationMetadata,
    bpacmCreationDateTime,
    bpacmCreatedByARN,

    -- * BootstrapActionConfig
    BootstrapActionConfig,
    bootstrapActionConfig,
    bacName,
    bacScriptBootstrapAction,

    -- * CancelStepsInfo
    CancelStepsInfo,
    cancelStepsInfo,
    csiStatus,
    csiStepId,
    csiReason,

    -- * CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition,
    cloudWatchAlarmDefinition,
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
    Cluster,
    cluster,
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
    ClusterStateChangeReason,
    clusterStateChangeReason,
    cscrCode,
    cscrMessage,

    -- * ClusterStatus
    ClusterStatus,
    clusterStatus,
    csState,
    csStateChangeReason,
    csTimeline,

    -- * ClusterSummary
    ClusterSummary,
    clusterSummary,
    cStatus,
    cClusterARN,
    cOutpostARN,
    cNormalizedInstanceHours,
    cName,
    cId,

    -- * ClusterTimeline
    ClusterTimeline,
    clusterTimeline,
    ctReadyDateTime,
    ctCreationDateTime,
    ctEndDateTime,

    -- * Command
    Command,
    command,
    comArgs,
    comScriptPath,
    comName,

    -- * ComputeLimits
    ComputeLimits,
    computeLimits,
    clMaximumOnDemandCapacityUnits,
    clMaximumCoreCapacityUnits,
    clUnitType,
    clMinimumCapacityUnits,
    clMaximumCapacityUnits,

    -- * Configuration
    Configuration,
    configuration,
    cConfigurations,
    cClassification,
    cProperties,

    -- * EBSBlockDevice
    EBSBlockDevice,
    ebsBlockDevice,
    ebdDevice,
    ebdVolumeSpecification,

    -- * EBSBlockDeviceConfig
    EBSBlockDeviceConfig,
    ebsBlockDeviceConfig,
    ebdcVolumesPerInstance,
    ebdcVolumeSpecification,

    -- * EBSConfiguration
    EBSConfiguration,
    ebsConfiguration,
    ecEBSOptimized,
    ecEBSBlockDeviceConfigs,

    -- * EBSVolume
    EBSVolume,
    ebsVolume,
    evDevice,
    evVolumeId,

    -- * EC2InstanceAttributes
    EC2InstanceAttributes,
    ec2InstanceAttributes,
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
    ExecutionEngineConfig,
    executionEngineConfig,
    eecMasterInstanceSecurityGroupId,
    eecType,
    eecId,

    -- * FailureDetails
    FailureDetails,
    failureDetails,
    fdLogFile,
    fdReason,
    fdMessage,

    -- * HadoopJARStepConfig
    HadoopJARStepConfig,
    hadoopJARStepConfig,
    hjscArgs,
    hjscMainClass,
    hjscProperties,
    hjscJAR,

    -- * HadoopStepConfig
    HadoopStepConfig,
    hadoopStepConfig,
    hscArgs,
    hscJAR,
    hscMainClass,
    hscProperties,

    -- * Instance
    Instance,
    instance',
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
    InstanceFleet,
    instanceFleet,
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
    InstanceFleetConfig,
    instanceFleetConfig,
    ifcInstanceTypeConfigs,
    ifcTargetOnDemandCapacity,
    ifcName,
    ifcTargetSpotCapacity,
    ifcLaunchSpecifications,
    ifcInstanceFleetType,

    -- * InstanceFleetModifyConfig
    InstanceFleetModifyConfig,
    instanceFleetModifyConfig,
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,
    ifmcInstanceFleetId,

    -- * InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications,
    instanceFleetProvisioningSpecifications,
    ifpsSpotSpecification,
    ifpsOnDemandSpecification,

    -- * InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason,
    instanceFleetStateChangeReason,
    ifscrCode,
    ifscrMessage,

    -- * InstanceFleetStatus
    InstanceFleetStatus,
    instanceFleetStatus,
    ifsState,
    ifsStateChangeReason,
    ifsTimeline,

    -- * InstanceFleetTimeline
    InstanceFleetTimeline,
    instanceFleetTimeline,
    iftReadyDateTime,
    iftCreationDateTime,
    iftEndDateTime,

    -- * InstanceGroup
    InstanceGroup,
    instanceGroup,
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
    InstanceGroupConfig,
    instanceGroupConfig,
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
    InstanceGroupModifyConfig,
    instanceGroupModifyConfig,
    igmcInstanceCount,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcShrinkPolicy,
    igmcInstanceGroupId,

    -- * InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason,
    instanceGroupStateChangeReason,
    igscrCode,
    igscrMessage,

    -- * InstanceGroupStatus
    InstanceGroupStatus,
    instanceGroupStatus,
    igsState,
    igsStateChangeReason,
    igsTimeline,

    -- * InstanceGroupTimeline
    InstanceGroupTimeline,
    instanceGroupTimeline,
    igtReadyDateTime,
    igtCreationDateTime,
    igtEndDateTime,

    -- * InstanceResizePolicy
    InstanceResizePolicy,
    instanceResizePolicy,
    irpInstancesToProtect,
    irpInstancesToTerminate,
    irpInstanceTerminationTimeout,

    -- * InstanceStateChangeReason
    InstanceStateChangeReason,
    instanceStateChangeReason,
    iscrCode,
    iscrMessage,

    -- * InstanceStatus
    InstanceStatus,
    instanceStatus,
    isState,
    isStateChangeReason,
    isTimeline,

    -- * InstanceTimeline
    InstanceTimeline,
    instanceTimeline,
    itReadyDateTime,
    itCreationDateTime,
    itEndDateTime,

    -- * InstanceTypeConfig
    InstanceTypeConfig,
    instanceTypeConfig,
    itcEBSConfiguration,
    itcBidPrice,
    itcWeightedCapacity,
    itcConfigurations,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcInstanceType,

    -- * InstanceTypeSpecification
    InstanceTypeSpecification,
    instanceTypeSpecification,
    itsBidPrice,
    itsWeightedCapacity,
    itsConfigurations,
    itsEBSBlockDevices,
    itsInstanceType,
    itsEBSOptimized,
    itsBidPriceAsPercentageOfOnDemandPrice,

    -- * JobFlowInstancesConfig
    JobFlowInstancesConfig,
    jobFlowInstancesConfig,
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
    KerberosAttributes,
    kerberosAttributes,
    kaKdcAdminPassword,
    kaRealm,
    kaADDomainJoinPassword,
    kaCrossRealmTrustPrincipalPassword,
    kaADDomainJoinUser,

    -- * KeyValue
    KeyValue,
    keyValue,
    kvValue,
    kvKey,

    -- * ManagedScalingPolicy
    ManagedScalingPolicy,
    managedScalingPolicy,
    mspComputeLimits,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdValue,
    mdKey,

    -- * NotebookExecution
    NotebookExecution,
    notebookExecution,
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
    NotebookExecutionSummary,
    notebookExecutionSummary,
    nesStatus,
    nesEditorId,
    nesStartTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesEndTime,

    -- * OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification,
    onDemandProvisioningSpecification,
    odpsAllocationStrategy,

    -- * PlacementGroupConfig
    PlacementGroupConfig,
    placementGroupConfig,
    pgcPlacementStrategy,
    pgcInstanceRole,

    -- * PlacementType
    PlacementType,
    placementType,
    ptAvailabilityZones,
    ptAvailabilityZone,

    -- * PortRange
    PortRange,
    portRange,
    prMaxRange,
    prMinRange,

    -- * ScalingAction
    ScalingAction,
    scalingAction,
    saMarket,
    saSimpleScalingPolicyConfiguration,

    -- * ScalingConstraints
    ScalingConstraints,
    scalingConstraints,
    scMinCapacity,
    scMaxCapacity,

    -- * ScalingRule
    ScalingRule,
    scalingRule,
    srDescription,
    srName,
    srAction,
    srTrigger,

    -- * ScalingTrigger
    ScalingTrigger,
    scalingTrigger,
    stCloudWatchAlarmDefinition,

    -- * ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig,
    scriptBootstrapActionConfig,
    sbacArgs,
    sbacPath,

    -- * SecurityConfigurationSummary
    SecurityConfigurationSummary,
    securityConfigurationSummary,
    scsName,
    scsCreationDateTime,

    -- * SessionMappingDetail
    SessionMappingDetail,
    sessionMappingDetail,
    smdCreationTime,
    smdStudioId,
    smdLastModifiedTime,
    smdIdentityType,
    smdIdentityId,
    smdSessionPolicyARN,
    smdIdentityName,

    -- * SessionMappingSummary
    SessionMappingSummary,
    sessionMappingSummary,
    smsCreationTime,
    smsStudioId,
    smsIdentityType,
    smsIdentityId,
    smsSessionPolicyARN,
    smsIdentityName,

    -- * ShrinkPolicy
    ShrinkPolicy,
    shrinkPolicy,
    spDecommissionTimeout,
    spInstanceResizePolicy,

    -- * SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration,
    simpleScalingPolicyConfiguration,
    sspcAdjustmentType,
    sspcCoolDown,
    sspcScalingAdjustment,

    -- * SpotProvisioningSpecification
    SpotProvisioningSpecification,
    spotProvisioningSpecification,
    spsBlockDurationMinutes,
    spsAllocationStrategy,
    spsTimeoutDurationMinutes,
    spsTimeoutAction,

    -- * Step
    Step,
    step,
    sStatus,
    sActionOnFailure,
    sConfig,
    sName,
    sId,

    -- * StepConfig
    StepConfig,
    stepConfig,
    scActionOnFailure,
    scName,
    scHadoopJARStep,

    -- * StepStateChangeReason
    StepStateChangeReason,
    stepStateChangeReason,
    sscrCode,
    sscrMessage,

    -- * StepStatus
    StepStatus,
    stepStatus,
    ssState,
    ssFailureDetails,
    ssStateChangeReason,
    ssTimeline,

    -- * StepSummary
    StepSummary,
    stepSummary,
    steStatus,
    steActionOnFailure,
    steConfig,
    steName,
    steId,

    -- * StepTimeline
    StepTimeline,
    stepTimeline,
    stCreationDateTime,
    stEndDateTime,
    stStartDateTime,

    -- * Studio
    Studio,
    studio,
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
    StudioSummary,
    studioSummary,
    ssCreationTime,
    ssStudioId,
    ssVPCId,
    ssURL,
    ssName,
    ssDescription,

    -- * SupportedProductConfig
    SupportedProductConfig,
    supportedProductConfig,
    spcArgs,
    spcName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * VolumeSpecification
    VolumeSpecification,
    volumeSpecification,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2009-03-31@ of the Amazon Elastic MapReduce SDK configuration.
emr :: Service
emr =
  Service
    { _svcAbbrev = "EMR",
      _svcSigner = v4,
      _svcPrefix = "elasticmapreduce",
      _svcVersion = "2009-03-31",
      _svcEndpoint = defaultEndpoint emr,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "EMR",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
