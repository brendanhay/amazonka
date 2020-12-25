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
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InternalServerException
    _InternalServerException,

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

    -- ** BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration (..),
    mkBlockPublicAccessConfiguration,
    bpacBlockPublicSecurityGroupRules,
    bpacPermittedPublicSecurityGroupRuleRanges,

    -- ** XmlString
    XmlString (..),

    -- ** MarketType
    MarketType (..),

    -- ** InstanceGroupConfig
    InstanceGroupConfig (..),
    mkInstanceGroupConfig,
    igcInstanceRole,
    igcInstanceType,
    igcInstanceCount,
    igcAutoScalingPolicy,
    igcBidPrice,
    igcConfigurations,
    igcEbsConfiguration,
    igcMarket,
    igcName,

    -- ** InstanceId
    InstanceId (..),

    -- ** SpotProvisioningTimeoutAction
    SpotProvisioningTimeoutAction (..),

    -- ** EbsConfiguration
    EbsConfiguration (..),
    mkEbsConfiguration,
    ecEbsBlockDeviceConfigs,
    ecEbsOptimized,

    -- ** InstanceStateChangeReason
    InstanceStateChangeReason (..),
    mkInstanceStateChangeReason,
    iscrCode,
    iscrMessage,

    -- ** PlacementGroupConfig
    PlacementGroupConfig (..),
    mkPlacementGroupConfig,
    pgcInstanceRole,
    pgcPlacementStrategy,

    -- ** KeyValue
    KeyValue (..),
    mkKeyValue,
    kvKey,
    kvValue,

    -- ** InstanceTypeSpecification
    InstanceTypeSpecification (..),
    mkInstanceTypeSpecification,
    itsBidPrice,
    itsBidPriceAsPercentageOfOnDemandPrice,
    itsConfigurations,
    itsEbsBlockDevices,
    itsEbsOptimized,
    itsInstanceType,
    itsWeightedCapacity,

    -- ** NotebookExecutionStatus
    NotebookExecutionStatus (..),

    -- ** SupportedProductConfig
    SupportedProductConfig (..),
    mkSupportedProductConfig,
    spcArgs,
    spcName,

    -- ** Command
    Command (..),
    mkCommand,
    cArgs,
    cName,
    cScriptPath,

    -- ** StepId
    StepId (..),

    -- ** ActionOnFailure
    ActionOnFailure (..),

    -- ** InstanceFleet
    InstanceFleet (..),
    mkInstanceFleet,
    ifId,
    ifInstanceFleetType,
    ifInstanceTypeSpecifications,
    ifLaunchSpecifications,
    ifName,
    ifProvisionedOnDemandCapacity,
    ifProvisionedSpotCapacity,
    ifStatus,
    ifTargetOnDemandCapacity,
    ifTargetSpotCapacity,

    -- ** ClusterStateChangeReason
    ClusterStateChangeReason (..),
    mkClusterStateChangeReason,
    cscrCode,
    cscrMessage,

    -- ** InstanceFleetStateChangeReasonCode
    InstanceFleetStateChangeReasonCode (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** Application
    Application (..),
    mkApplication,
    aAdditionalInfo,
    aArgs,
    aName,
    aVersion,

    -- ** AutoScalingPolicyStateChangeReasonCode
    AutoScalingPolicyStateChangeReasonCode (..),

    -- ** SpotProvisioningAllocationStrategy
    SpotProvisioningAllocationStrategy (..),

    -- ** InstanceGroupStatus
    InstanceGroupStatus (..),
    mkInstanceGroupStatus,
    igsState,
    igsStateChangeReason,
    igsTimeline,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cfApplications,
    cfAutoScalingRole,
    cfAutoTerminate,
    cfClusterArn,
    cfConfigurations,
    cfCustomAmiId,
    cfEbsRootVolumeSize,
    cfEc2InstanceAttributes,
    cfId,
    cfInstanceCollectionType,
    cfKerberosAttributes,
    cfLogEncryptionKmsKeyId,
    cfLogUri,
    cfMasterPublicDnsName,
    cfName,
    cfNormalizedInstanceHours,
    cfOutpostArn,
    cfPlacementGroups,
    cfReleaseLabel,
    cfRepoUpgradeOnBoot,
    cfRequestedAmiVersion,
    cfRunningAmiVersion,
    cfScaleDownBehavior,
    cfSecurityConfiguration,
    cfServiceRole,
    cfStatus,
    cfStepConcurrencyLevel,
    cfTags,
    cfTerminationProtected,
    cfVisibleToAllUsers,

    -- ** CancelStepsRequestStatus
    CancelStepsRequestStatus (..),

    -- ** InstanceTimeline
    InstanceTimeline (..),
    mkInstanceTimeline,
    itCreationDateTime,
    itEndDateTime,
    itReadyDateTime,

    -- ** Ec2InstanceAttributes
    Ec2InstanceAttributes (..),
    mkEc2InstanceAttributes,
    eiaAdditionalMasterSecurityGroups,
    eiaAdditionalSlaveSecurityGroups,
    eiaEc2AvailabilityZone,
    eiaEc2KeyName,
    eiaEc2SubnetId,
    eiaEmrManagedMasterSecurityGroup,
    eiaEmrManagedSlaveSecurityGroup,
    eiaIamInstanceProfile,
    eiaRequestedEc2AvailabilityZones,
    eiaRequestedEc2SubnetIds,
    eiaServiceAccessSecurityGroup,

    -- ** OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (..),
    mkOnDemandProvisioningSpecification,
    odpsAllocationStrategy,

    -- ** StepStateChangeReasonCode
    StepStateChangeReasonCode (..),

    -- ** FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdLogFile,
    fdMessage,
    fdReason,

    -- ** ClusterState
    ClusterState (..),

    -- ** InstanceFleetConfig
    InstanceFleetConfig (..),
    mkInstanceFleetConfig,
    ifcInstanceFleetType,
    ifcInstanceTypeConfigs,
    ifcLaunchSpecifications,
    ifcName,
    ifcTargetOnDemandCapacity,
    ifcTargetSpotCapacity,

    -- ** ComputeLimitsUnitType
    ComputeLimitsUnitType (..),

    -- ** String
    String (..),

    -- ** PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- ** HadoopStepConfig
    HadoopStepConfig (..),
    mkHadoopStepConfig,
    hscArgs,
    hscJar,
    hscMainClass,
    hscProperties,

    -- ** SessionMappingDetail
    SessionMappingDetail (..),
    mkSessionMappingDetail,
    smdCreationTime,
    smdIdentityId,
    smdIdentityName,
    smdIdentityType,
    smdLastModifiedTime,
    smdSessionPolicyArn,
    smdStudioId,

    -- ** ScalingRule
    ScalingRule (..),
    mkScalingRule,
    srName,
    srAction,
    srTrigger,
    srDescription,

    -- ** ScalingAction
    ScalingAction (..),
    mkScalingAction,
    saSimpleScalingPolicyConfiguration,
    saMarket,

    -- ** InstanceTypeConfig
    InstanceTypeConfig (..),
    mkInstanceTypeConfig,
    itcInstanceType,
    itcBidPrice,
    itcBidPriceAsPercentageOfOnDemandPrice,
    itcConfigurations,
    itcEbsConfiguration,
    itcWeightedCapacity,

    -- ** NotebookExecution
    NotebookExecution (..),
    mkNotebookExecution,
    neArn,
    neEditorId,
    neEndTime,
    neExecutionEngine,
    neLastStateChangeReason,
    neNotebookExecutionId,
    neNotebookExecutionName,
    neNotebookInstanceSecurityGroupId,
    neNotebookParams,
    neOutputNotebookURI,
    neStartTime,
    neStatus,
    neTags,

    -- ** AuthMode
    AuthMode (..),

    -- ** InstanceResizePolicy
    InstanceResizePolicy (..),
    mkInstanceResizePolicy,
    irpInstanceTerminationTimeout,
    irpInstancesToProtect,
    irpInstancesToTerminate,

    -- ** InstanceFleetStatus
    InstanceFleetStatus (..),
    mkInstanceFleetStatus,
    ifsState,
    ifsStateChangeReason,
    ifsTimeline,

    -- ** InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (..),
    mkInstanceGroupStateChangeReason,
    igscrCode,
    igscrMessage,

    -- ** SessionMappingSummary
    SessionMappingSummary (..),
    mkSessionMappingSummary,
    smsCreationTime,
    smsIdentityId,
    smsIdentityName,
    smsIdentityType,
    smsSessionPolicyArn,
    smsStudioId,

    -- ** InstanceGroupType
    InstanceGroupType (..),

    -- ** AutoScalingPolicyStatus
    AutoScalingPolicyStatus (..),
    mkAutoScalingPolicyStatus,
    aspsState,
    aspsStateChangeReason,

    -- ** XmlStringMaxLen256
    XmlStringMaxLen256 (..),

    -- ** StepCancellationOption
    StepCancellationOption (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** ScaleDownBehavior
    ScaleDownBehavior (..),

    -- ** PortRange
    PortRange (..),
    mkPortRange,
    prMinRange,
    prMaxRange,

    -- ** InstanceGroupStateChangeReasonCode
    InstanceGroupStateChangeReasonCode (..),

    -- ** StepStatus
    StepStatus (..),
    mkStepStatus,
    ssFailureDetails,
    ssState,
    ssStateChangeReason,
    ssTimeline,

    -- ** StepSummary
    StepSummary (..),
    mkStepSummary,
    sActionOnFailure,
    sConfig,
    sId,
    sName,
    sStatus,

    -- ** InstanceGroupState
    InstanceGroupState (..),

    -- ** StepTimeline
    StepTimeline (..),
    mkStepTimeline,
    stCreationDateTime,
    stEndDateTime,
    stStartDateTime,

    -- ** PlacementType
    PlacementType (..),
    mkPlacementType,
    ptAvailabilityZone,
    ptAvailabilityZones,

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** CancelStepsInfo
    CancelStepsInfo (..),
    mkCancelStepsInfo,
    csiReason,
    csiStatus,
    csiStepId,

    -- ** InstanceType
    InstanceType (..),

    -- ** HadoopJarStepConfig
    HadoopJarStepConfig (..),
    mkHadoopJarStepConfig,
    hjscJar,
    hjscArgs,
    hjscMainClass,
    hjscProperties,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdKey,
    mdValue,

    -- ** ExecutionEngineType
    ExecutionEngineType (..),

    -- ** InstanceGroupModifyConfig
    InstanceGroupModifyConfig (..),
    mkInstanceGroupModifyConfig,
    igmcInstanceGroupId,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcInstanceCount,
    igmcShrinkPolicy,

    -- ** StudioSummary
    StudioSummary (..),
    mkStudioSummary,
    ssCreationTime,
    ssDescription,
    ssName,
    ssStudioId,
    ssUrl,
    ssVpcId,

    -- ** SpotProvisioningSpecification
    SpotProvisioningSpecification (..),
    mkSpotProvisioningSpecification,
    spsTimeoutDurationMinutes,
    spsTimeoutAction,
    spsAllocationStrategy,
    spsBlockDurationMinutes,

    -- ** AutoScalingPolicyDescription
    AutoScalingPolicyDescription (..),
    mkAutoScalingPolicyDescription,
    aspdConstraints,
    aspdRules,
    aspdStatus,

    -- ** OnDemandProvisioningAllocationStrategy
    OnDemandProvisioningAllocationStrategy (..),

    -- ** InstanceFleetTimeline
    InstanceFleetTimeline (..),
    mkInstanceFleetTimeline,
    iftCreationDateTime,
    iftEndDateTime,
    iftReadyDateTime,

    -- ** EbsVolume
    EbsVolume (..),
    mkEbsVolume,
    evDevice,
    evVolumeId,

    -- ** EbsBlockDeviceConfig
    EbsBlockDeviceConfig (..),
    mkEbsBlockDeviceConfig,
    ebdcVolumeSpecification,
    ebdcVolumesPerInstance,

    -- ** InstanceCollectionType
    InstanceCollectionType (..),

    -- ** InstanceFleetType
    InstanceFleetType (..),

    -- ** AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (..),
    mkAutoScalingPolicyStateChangeReason,
    aspscrCode,
    aspscrMessage,

    -- ** AdjustmentType
    AdjustmentType (..),

    -- ** NotebookExecutionSummary
    NotebookExecutionSummary (..),
    mkNotebookExecutionSummary,
    nesEditorId,
    nesEndTime,
    nesNotebookExecutionId,
    nesNotebookExecutionName,
    nesStartTime,
    nesStatus,

    -- ** StepStateChangeReason
    StepStateChangeReason (..),
    mkStepStateChangeReason,
    sscrCode,
    sscrMessage,

    -- ** RepoUpgradeOnBoot
    RepoUpgradeOnBoot (..),

    -- ** ClusterId
    ClusterId (..),

    -- ** SecurityConfigurationSummary
    SecurityConfigurationSummary (..),
    mkSecurityConfigurationSummary,
    scsCreationDateTime,
    scsName,

    -- ** KerberosAttributes
    KerberosAttributes (..),
    mkKerberosAttributes,
    kaRealm,
    kaKdcAdminPassword,
    kaADDomainJoinPassword,
    kaADDomainJoinUser,
    kaCrossRealmTrustPrincipalPassword,

    -- ** Marker
    Marker (..),

    -- ** InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (..),
    mkInstanceFleetStateChangeReason,
    ifscrCode,
    ifscrMessage,

    -- ** ClusterStateChangeReasonCode
    ClusterStateChangeReasonCode (..),

    -- ** AutoScalingPolicy
    AutoScalingPolicy (..),
    mkAutoScalingPolicy,
    aspConstraints,
    aspRules,

    -- ** InstanceFleetId
    InstanceFleetId (..),

    -- ** Step
    Step (..),
    mkStep,
    sfActionOnFailure,
    sfConfig,
    sfId,
    sfName,
    sfStatus,

    -- ** StepState
    StepState (..),

    -- ** ShrinkPolicy
    ShrinkPolicy (..),
    mkShrinkPolicy,
    spDecommissionTimeout,
    spInstanceResizePolicy,

    -- ** VolumeSpecification
    VolumeSpecification (..),
    mkVolumeSpecification,
    vsVolumeType,
    vsSizeInGB,
    vsIops,

    -- ** ScalingTrigger
    ScalingTrigger (..),
    mkScalingTrigger,
    stCloudWatchAlarmDefinition,

    -- ** InstanceGroupTimeline
    InstanceGroupTimeline (..),
    mkInstanceGroupTimeline,
    igtCreationDateTime,
    igtEndDateTime,
    igtReadyDateTime,

    -- ** InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    isState,
    isStateChangeReason,
    isTimeline,

    -- ** AutoScalingPolicyState
    AutoScalingPolicyState (..),

    -- ** InstanceRoleType
    InstanceRoleType (..),

    -- ** EbsBlockDevice
    EbsBlockDevice (..),
    mkEbsBlockDevice,
    ebdDevice,
    ebdVolumeSpecification,

    -- ** JobFlowInstancesConfig
    JobFlowInstancesConfig (..),
    mkJobFlowInstancesConfig,
    jficAdditionalMasterSecurityGroups,
    jficAdditionalSlaveSecurityGroups,
    jficEc2KeyName,
    jficEc2SubnetId,
    jficEc2SubnetIds,
    jficEmrManagedMasterSecurityGroup,
    jficEmrManagedSlaveSecurityGroup,
    jficHadoopVersion,
    jficInstanceCount,
    jficInstanceFleets,
    jficInstanceGroups,
    jficKeepJobFlowAliveWhenNoSteps,
    jficMasterInstanceType,
    jficPlacement,
    jficServiceAccessSecurityGroup,
    jficSlaveInstanceType,
    jficTerminationProtected,

    -- ** InstanceFleetModifyConfig
    InstanceFleetModifyConfig (..),
    mkInstanceFleetModifyConfig,
    ifmcInstanceFleetId,
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,

    -- ** ArnType
    ArnType (..),

    -- ** InstanceFleetState
    InstanceFleetState (..),

    -- ** Configuration
    Configuration (..),
    mkConfiguration,
    cClassification,
    cConfigurations,
    cProperties,

    -- ** InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications (..),
    mkInstanceFleetProvisioningSpecifications,
    ifpsOnDemandSpecification,
    ifpsSpotSpecification,

    -- ** Studio
    Studio (..),
    mkStudio,
    sgAuthMode,
    sgCreationTime,
    sgDefaultS3Location,
    sgDescription,
    sgEngineSecurityGroupId,
    sgName,
    sgServiceRole,
    sgStudioArn,
    sgStudioId,
    sgSubnetIds,
    sgTags,
    sgUrl,
    sgUserRole,
    sgVpcId,
    sgWorkspaceSecurityGroupId,

    -- ** ManagedScalingPolicy
    ManagedScalingPolicy (..),
    mkManagedScalingPolicy,
    mspComputeLimits,

    -- ** StepConfig
    StepConfig (..),
    mkStepConfig,
    scName,
    scHadoopJarStep,
    scActionOnFailure,

    -- ** InstanceGroupId
    InstanceGroupId (..),

    -- ** OptionalArnType
    OptionalArnType (..),

    -- ** ScalingConstraints
    ScalingConstraints (..),
    mkScalingConstraints,
    scMinCapacity,
    scMaxCapacity,

    -- ** SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (..),
    mkSimpleScalingPolicyConfiguration,
    sspcScalingAdjustment,
    sspcAdjustmentType,
    sspcCoolDown,

    -- ** ComputeLimits
    ComputeLimits (..),
    mkComputeLimits,
    clUnitType,
    clMinimumCapacityUnits,
    clMaximumCapacityUnits,
    clMaximumCoreCapacityUnits,
    clMaximumOnDemandCapacityUnits,

    -- ** InstanceGroup
    InstanceGroup (..),
    mkInstanceGroup,
    igAutoScalingPolicy,
    igBidPrice,
    igConfigurations,
    igConfigurationsVersion,
    igEbsBlockDevices,
    igEbsOptimized,
    igId,
    igInstanceGroupType,
    igInstanceType,
    igLastSuccessfullyAppliedConfigurations,
    igLastSuccessfullyAppliedConfigurationsVersion,
    igMarket,
    igName,
    igRequestedInstanceCount,
    igRunningInstanceCount,
    igShrinkPolicy,
    igStatus,

    -- ** BootstrapActionConfig
    BootstrapActionConfig (..),
    mkBootstrapActionConfig,
    bacName,
    bacScriptBootstrapAction,

    -- ** ClusterSummary
    ClusterSummary (..),
    mkClusterSummary,
    csClusterArn,
    csId,
    csName,
    csNormalizedInstanceHours,
    csOutpostArn,
    csStatus,

    -- ** ExecutionEngineConfig
    ExecutionEngineConfig (..),
    mkExecutionEngineConfig,
    eecId,
    eecMasterInstanceSecurityGroupId,
    eecType,

    -- ** Unit
    Unit (..),

    -- ** CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (..),
    mkCloudWatchAlarmDefinition,
    cwadComparisonOperator,
    cwadMetricName,
    cwadPeriod,
    cwadThreshold,
    cwadDimensions,
    cwadEvaluationPeriods,
    cwadNamespace,
    cwadStatistic,
    cwadUnit,

    -- ** ClusterStatus
    ClusterStatus (..),
    mkClusterStatus,
    csState,
    csStateChangeReason,
    csTimeline,

    -- ** InstanceState
    InstanceState (..),

    -- ** Statistic
    Statistic (..),

    -- ** BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata (..),
    mkBlockPublicAccessConfigurationMetadata,
    bpacmCreationDateTime,
    bpacmCreatedByArn,

    -- ** ClusterTimeline
    ClusterTimeline (..),
    mkClusterTimeline,
    ctCreationDateTime,
    ctEndDateTime,
    ctReadyDateTime,

    -- ** InstanceStateChangeReasonCode
    InstanceStateChangeReasonCode (..),

    -- ** Instance
    Instance (..),
    mkInstance,
    iEbsVolumes,
    iEc2InstanceId,
    iId,
    iInstanceFleetId,
    iInstanceGroupId,
    iInstanceType,
    iMarket,
    iPrivateDnsName,
    iPrivateIpAddress,
    iPublicDnsName,
    iPublicIpAddress,
    iStatus,

    -- ** ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig (..),
    mkScriptBootstrapActionConfig,
    sbacPath,
    sbacArgs,

    -- ** SecurityConfiguration
    SecurityConfiguration (..),

    -- ** BidPrice
    BidPrice (..),

    -- ** Name
    Name (..),

    -- ** Message
    Message (..),

    -- ** JobFlowId
    JobFlowId (..),

    -- ** ClusterArn
    ClusterArn (..),

    -- ** ScriptPath
    ScriptPath (..),

    -- ** Id
    Id (..),

    -- ** StudioId
    StudioId (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** Version
    Version (..),

    -- ** CustomAmiId
    CustomAmiId (..),

    -- ** LogEncryptionKmsKeyId
    LogEncryptionKmsKeyId (..),

    -- ** LogUri
    LogUri (..),

    -- ** MasterPublicDnsName
    MasterPublicDnsName (..),

    -- ** OutpostArn
    OutpostArn (..),

    -- ** ReleaseLabel
    ReleaseLabel (..),

    -- ** RequestedAmiVersion
    RequestedAmiVersion (..),

    -- ** RunningAmiVersion
    RunningAmiVersion (..),

    -- ** ServiceRole
    ServiceRole (..),

    -- ** IdentityId
    IdentityId (..),

    -- ** IdentityName
    IdentityName (..),

    -- ** Ec2AvailabilityZone
    Ec2AvailabilityZone (..),

    -- ** Ec2KeyName
    Ec2KeyName (..),

    -- ** Ec2SubnetId
    Ec2SubnetId (..),

    -- ** EmrManagedMasterSecurityGroup
    EmrManagedMasterSecurityGroup (..),

    -- ** EmrManagedSlaveSecurityGroup
    EmrManagedSlaveSecurityGroup (..),

    -- ** IamInstanceProfile
    IamInstanceProfile (..),

    -- ** ServiceAccessSecurityGroup
    ServiceAccessSecurityGroup (..),

    -- ** LogFile
    LogFile (..),

    -- ** Reason
    Reason (..),

    -- ** EditorId
    EditorId (..),

    -- ** NotebookExecutionName
    NotebookExecutionName (..),

    -- ** NotebookInstanceSecurityGroupId
    NotebookInstanceSecurityGroupId (..),

    -- ** SessionPolicyArn
    SessionPolicyArn (..),

    -- ** Arn
    Arn (..),

    -- ** NotebookExecutionId
    NotebookExecutionId (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
