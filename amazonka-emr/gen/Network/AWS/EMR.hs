{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon EMR is a web service that makes it easier to process large
-- amounts of data efficiently. Amazon EMR uses Hadoop processing combined
-- with several AWS services to do tasks such as web indexing, data mining,
-- log file analysis, machine learning, scientific simulation, and data
-- warehouse management.
module Network.AWS.EMR
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InternalServerException
    _InternalServerException,

    -- * Waiters
    -- $waiters

    -- ** ClusterTerminated
    newClusterTerminated,

    -- ** StepComplete
    newStepComplete,

    -- ** ClusterRunning
    newClusterRunning,

    -- * Operations
    -- $operations

    -- ** DescribeStep
    DescribeStep (DescribeStep'),
    newDescribeStep,
    DescribeStepResponse (DescribeStepResponse'),
    newDescribeStepResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** DeleteSecurityConfiguration
    DeleteSecurityConfiguration (DeleteSecurityConfiguration'),
    newDeleteSecurityConfiguration,
    DeleteSecurityConfigurationResponse (DeleteSecurityConfigurationResponse'),
    newDeleteSecurityConfigurationResponse,

    -- ** ListSecurityConfigurations (Paginated)
    ListSecurityConfigurations (ListSecurityConfigurations'),
    newListSecurityConfigurations,
    ListSecurityConfigurationsResponse (ListSecurityConfigurationsResponse'),
    newListSecurityConfigurationsResponse,

    -- ** ModifyInstanceFleet
    ModifyInstanceFleet (ModifyInstanceFleet'),
    newModifyInstanceFleet,
    ModifyInstanceFleetResponse (ModifyInstanceFleetResponse'),
    newModifyInstanceFleetResponse,

    -- ** RunJobFlow
    RunJobFlow (RunJobFlow'),
    newRunJobFlow,
    RunJobFlowResponse (RunJobFlowResponse'),
    newRunJobFlowResponse,

    -- ** GetStudioSessionMapping
    GetStudioSessionMapping (GetStudioSessionMapping'),
    newGetStudioSessionMapping,
    GetStudioSessionMappingResponse (GetStudioSessionMappingResponse'),
    newGetStudioSessionMappingResponse,

    -- ** SetVisibleToAllUsers
    SetVisibleToAllUsers (SetVisibleToAllUsers'),
    newSetVisibleToAllUsers,
    SetVisibleToAllUsersResponse (SetVisibleToAllUsersResponse'),
    newSetVisibleToAllUsersResponse,

    -- ** AddInstanceGroups
    AddInstanceGroups (AddInstanceGroups'),
    newAddInstanceGroups,
    AddInstanceGroupsResponse (AddInstanceGroupsResponse'),
    newAddInstanceGroupsResponse,

    -- ** CreateStudio
    CreateStudio (CreateStudio'),
    newCreateStudio,
    CreateStudioResponse (CreateStudioResponse'),
    newCreateStudioResponse,

    -- ** DeleteStudio
    DeleteStudio (DeleteStudio'),
    newDeleteStudio,
    DeleteStudioResponse (DeleteStudioResponse'),
    newDeleteStudioResponse,

    -- ** UpdateStudio
    UpdateStudio (UpdateStudio'),
    newUpdateStudio,
    UpdateStudioResponse (UpdateStudioResponse'),
    newUpdateStudioResponse,

    -- ** ListInstanceFleets (Paginated)
    ListInstanceFleets (ListInstanceFleets'),
    newListInstanceFleets,
    ListInstanceFleetsResponse (ListInstanceFleetsResponse'),
    newListInstanceFleetsResponse,

    -- ** RemoveManagedScalingPolicy
    RemoveManagedScalingPolicy (RemoveManagedScalingPolicy'),
    newRemoveManagedScalingPolicy,
    RemoveManagedScalingPolicyResponse (RemoveManagedScalingPolicyResponse'),
    newRemoveManagedScalingPolicyResponse,

    -- ** DescribeSecurityConfiguration
    DescribeSecurityConfiguration (DescribeSecurityConfiguration'),
    newDescribeSecurityConfiguration,
    DescribeSecurityConfigurationResponse (DescribeSecurityConfigurationResponse'),
    newDescribeSecurityConfigurationResponse,

    -- ** StartNotebookExecution
    StartNotebookExecution (StartNotebookExecution'),
    newStartNotebookExecution,
    StartNotebookExecutionResponse (StartNotebookExecutionResponse'),
    newStartNotebookExecutionResponse,

    -- ** ListStudioSessionMappings (Paginated)
    ListStudioSessionMappings (ListStudioSessionMappings'),
    newListStudioSessionMappings,
    ListStudioSessionMappingsResponse (ListStudioSessionMappingsResponse'),
    newListStudioSessionMappingsResponse,

    -- ** StopNotebookExecution
    StopNotebookExecution (StopNotebookExecution'),
    newStopNotebookExecution,
    StopNotebookExecutionResponse (StopNotebookExecutionResponse'),
    newStopNotebookExecutionResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** AddJobFlowSteps
    AddJobFlowSteps (AddJobFlowSteps'),
    newAddJobFlowSteps,
    AddJobFlowStepsResponse (AddJobFlowStepsResponse'),
    newAddJobFlowStepsResponse,

    -- ** ListBootstrapActions (Paginated)
    ListBootstrapActions (ListBootstrapActions'),
    newListBootstrapActions,
    ListBootstrapActionsResponse (ListBootstrapActionsResponse'),
    newListBootstrapActionsResponse,

    -- ** ListNotebookExecutions (Paginated)
    ListNotebookExecutions (ListNotebookExecutions'),
    newListNotebookExecutions,
    ListNotebookExecutionsResponse (ListNotebookExecutionsResponse'),
    newListNotebookExecutionsResponse,

    -- ** GetBlockPublicAccessConfiguration
    GetBlockPublicAccessConfiguration (GetBlockPublicAccessConfiguration'),
    newGetBlockPublicAccessConfiguration,
    GetBlockPublicAccessConfigurationResponse (GetBlockPublicAccessConfigurationResponse'),
    newGetBlockPublicAccessConfigurationResponse,

    -- ** ModifyCluster
    ModifyCluster (ModifyCluster'),
    newModifyCluster,
    ModifyClusterResponse (ModifyClusterResponse'),
    newModifyClusterResponse,

    -- ** TerminateJobFlows
    TerminateJobFlows (TerminateJobFlows'),
    newTerminateJobFlows,
    TerminateJobFlowsResponse (TerminateJobFlowsResponse'),
    newTerminateJobFlowsResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** CancelSteps
    CancelSteps (CancelSteps'),
    newCancelSteps,
    CancelStepsResponse (CancelStepsResponse'),
    newCancelStepsResponse,

    -- ** ListInstanceGroups (Paginated)
    ListInstanceGroups (ListInstanceGroups'),
    newListInstanceGroups,
    ListInstanceGroupsResponse (ListInstanceGroupsResponse'),
    newListInstanceGroupsResponse,

    -- ** RemoveAutoScalingPolicy
    RemoveAutoScalingPolicy (RemoveAutoScalingPolicy'),
    newRemoveAutoScalingPolicy,
    RemoveAutoScalingPolicyResponse (RemoveAutoScalingPolicyResponse'),
    newRemoveAutoScalingPolicyResponse,

    -- ** PutManagedScalingPolicy
    PutManagedScalingPolicy (PutManagedScalingPolicy'),
    newPutManagedScalingPolicy,
    PutManagedScalingPolicyResponse (PutManagedScalingPolicyResponse'),
    newPutManagedScalingPolicyResponse,

    -- ** ListStudios (Paginated)
    ListStudios (ListStudios'),
    newListStudios,
    ListStudiosResponse (ListStudiosResponse'),
    newListStudiosResponse,

    -- ** AddInstanceFleet
    AddInstanceFleet (AddInstanceFleet'),
    newAddInstanceFleet,
    AddInstanceFleetResponse (AddInstanceFleetResponse'),
    newAddInstanceFleetResponse,

    -- ** CreateStudioSessionMapping
    CreateStudioSessionMapping (CreateStudioSessionMapping'),
    newCreateStudioSessionMapping,
    CreateStudioSessionMappingResponse (CreateStudioSessionMappingResponse'),
    newCreateStudioSessionMappingResponse,

    -- ** GetManagedScalingPolicy
    GetManagedScalingPolicy (GetManagedScalingPolicy'),
    newGetManagedScalingPolicy,
    GetManagedScalingPolicyResponse (GetManagedScalingPolicyResponse'),
    newGetManagedScalingPolicyResponse,

    -- ** DescribeNotebookExecution
    DescribeNotebookExecution (DescribeNotebookExecution'),
    newDescribeNotebookExecution,
    DescribeNotebookExecutionResponse (DescribeNotebookExecutionResponse'),
    newDescribeNotebookExecutionResponse,

    -- ** UpdateStudioSessionMapping
    UpdateStudioSessionMapping (UpdateStudioSessionMapping'),
    newUpdateStudioSessionMapping,
    UpdateStudioSessionMappingResponse (UpdateStudioSessionMappingResponse'),
    newUpdateStudioSessionMappingResponse,

    -- ** DeleteStudioSessionMapping
    DeleteStudioSessionMapping (DeleteStudioSessionMapping'),
    newDeleteStudioSessionMapping,
    DeleteStudioSessionMappingResponse (DeleteStudioSessionMappingResponse'),
    newDeleteStudioSessionMappingResponse,

    -- ** ListSteps (Paginated)
    ListSteps (ListSteps'),
    newListSteps,
    ListStepsResponse (ListStepsResponse'),
    newListStepsResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** PutAutoScalingPolicy
    PutAutoScalingPolicy (PutAutoScalingPolicy'),
    newPutAutoScalingPolicy,
    PutAutoScalingPolicyResponse (PutAutoScalingPolicyResponse'),
    newPutAutoScalingPolicyResponse,

    -- ** SetTerminationProtection
    SetTerminationProtection (SetTerminationProtection'),
    newSetTerminationProtection,
    SetTerminationProtectionResponse (SetTerminationProtectionResponse'),
    newSetTerminationProtectionResponse,

    -- ** PutBlockPublicAccessConfiguration
    PutBlockPublicAccessConfiguration (PutBlockPublicAccessConfiguration'),
    newPutBlockPublicAccessConfiguration,
    PutBlockPublicAccessConfigurationResponse (PutBlockPublicAccessConfigurationResponse'),
    newPutBlockPublicAccessConfigurationResponse,

    -- ** DescribeStudio
    DescribeStudio (DescribeStudio'),
    newDescribeStudio,
    DescribeStudioResponse (DescribeStudioResponse'),
    newDescribeStudioResponse,

    -- ** ModifyInstanceGroups
    ModifyInstanceGroups (ModifyInstanceGroups'),
    newModifyInstanceGroups,
    ModifyInstanceGroupsResponse (ModifyInstanceGroupsResponse'),
    newModifyInstanceGroupsResponse,

    -- ** CreateSecurityConfiguration
    CreateSecurityConfiguration (CreateSecurityConfiguration'),
    newCreateSecurityConfiguration,
    CreateSecurityConfigurationResponse (CreateSecurityConfigurationResponse'),
    newCreateSecurityConfigurationResponse,

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
    Application (Application'),
    newApplication,

    -- ** AutoScalingPolicy
    AutoScalingPolicy (AutoScalingPolicy'),
    newAutoScalingPolicy,

    -- ** AutoScalingPolicyDescription
    AutoScalingPolicyDescription (AutoScalingPolicyDescription'),
    newAutoScalingPolicyDescription,

    -- ** AutoScalingPolicyStateChangeReason
    AutoScalingPolicyStateChangeReason (AutoScalingPolicyStateChangeReason'),
    newAutoScalingPolicyStateChangeReason,

    -- ** AutoScalingPolicyStatus
    AutoScalingPolicyStatus (AutoScalingPolicyStatus'),
    newAutoScalingPolicyStatus,

    -- ** BlockPublicAccessConfiguration
    BlockPublicAccessConfiguration (BlockPublicAccessConfiguration'),
    newBlockPublicAccessConfiguration,

    -- ** BlockPublicAccessConfigurationMetadata
    BlockPublicAccessConfigurationMetadata (BlockPublicAccessConfigurationMetadata'),
    newBlockPublicAccessConfigurationMetadata,

    -- ** BootstrapActionConfig
    BootstrapActionConfig (BootstrapActionConfig'),
    newBootstrapActionConfig,

    -- ** CancelStepsInfo
    CancelStepsInfo (CancelStepsInfo'),
    newCancelStepsInfo,

    -- ** CloudWatchAlarmDefinition
    CloudWatchAlarmDefinition (CloudWatchAlarmDefinition'),
    newCloudWatchAlarmDefinition,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterStateChangeReason
    ClusterStateChangeReason (ClusterStateChangeReason'),
    newClusterStateChangeReason,

    -- ** ClusterStatus
    ClusterStatus (ClusterStatus'),
    newClusterStatus,

    -- ** ClusterSummary
    ClusterSummary (ClusterSummary'),
    newClusterSummary,

    -- ** ClusterTimeline
    ClusterTimeline (ClusterTimeline'),
    newClusterTimeline,

    -- ** Command
    Command (Command'),
    newCommand,

    -- ** ComputeLimits
    ComputeLimits (ComputeLimits'),
    newComputeLimits,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** EbsBlockDevice
    EbsBlockDevice (EbsBlockDevice'),
    newEbsBlockDevice,

    -- ** EbsBlockDeviceConfig
    EbsBlockDeviceConfig (EbsBlockDeviceConfig'),
    newEbsBlockDeviceConfig,

    -- ** EbsConfiguration
    EbsConfiguration (EbsConfiguration'),
    newEbsConfiguration,

    -- ** EbsVolume
    EbsVolume (EbsVolume'),
    newEbsVolume,

    -- ** Ec2InstanceAttributes
    Ec2InstanceAttributes (Ec2InstanceAttributes'),
    newEc2InstanceAttributes,

    -- ** ExecutionEngineConfig
    ExecutionEngineConfig (ExecutionEngineConfig'),
    newExecutionEngineConfig,

    -- ** FailureDetails
    FailureDetails (FailureDetails'),
    newFailureDetails,

    -- ** HadoopJarStepConfig
    HadoopJarStepConfig (HadoopJarStepConfig'),
    newHadoopJarStepConfig,

    -- ** HadoopStepConfig
    HadoopStepConfig (HadoopStepConfig'),
    newHadoopStepConfig,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceFleet
    InstanceFleet (InstanceFleet'),
    newInstanceFleet,

    -- ** InstanceFleetConfig
    InstanceFleetConfig (InstanceFleetConfig'),
    newInstanceFleetConfig,

    -- ** InstanceFleetModifyConfig
    InstanceFleetModifyConfig (InstanceFleetModifyConfig'),
    newInstanceFleetModifyConfig,

    -- ** InstanceFleetProvisioningSpecifications
    InstanceFleetProvisioningSpecifications (InstanceFleetProvisioningSpecifications'),
    newInstanceFleetProvisioningSpecifications,

    -- ** InstanceFleetStateChangeReason
    InstanceFleetStateChangeReason (InstanceFleetStateChangeReason'),
    newInstanceFleetStateChangeReason,

    -- ** InstanceFleetStatus
    InstanceFleetStatus (InstanceFleetStatus'),
    newInstanceFleetStatus,

    -- ** InstanceFleetTimeline
    InstanceFleetTimeline (InstanceFleetTimeline'),
    newInstanceFleetTimeline,

    -- ** InstanceGroup
    InstanceGroup (InstanceGroup'),
    newInstanceGroup,

    -- ** InstanceGroupConfig
    InstanceGroupConfig (InstanceGroupConfig'),
    newInstanceGroupConfig,

    -- ** InstanceGroupModifyConfig
    InstanceGroupModifyConfig (InstanceGroupModifyConfig'),
    newInstanceGroupModifyConfig,

    -- ** InstanceGroupStateChangeReason
    InstanceGroupStateChangeReason (InstanceGroupStateChangeReason'),
    newInstanceGroupStateChangeReason,

    -- ** InstanceGroupStatus
    InstanceGroupStatus (InstanceGroupStatus'),
    newInstanceGroupStatus,

    -- ** InstanceGroupTimeline
    InstanceGroupTimeline (InstanceGroupTimeline'),
    newInstanceGroupTimeline,

    -- ** InstanceResizePolicy
    InstanceResizePolicy (InstanceResizePolicy'),
    newInstanceResizePolicy,

    -- ** InstanceStateChangeReason
    InstanceStateChangeReason (InstanceStateChangeReason'),
    newInstanceStateChangeReason,

    -- ** InstanceStatus
    InstanceStatus (InstanceStatus'),
    newInstanceStatus,

    -- ** InstanceTimeline
    InstanceTimeline (InstanceTimeline'),
    newInstanceTimeline,

    -- ** InstanceTypeConfig
    InstanceTypeConfig (InstanceTypeConfig'),
    newInstanceTypeConfig,

    -- ** InstanceTypeSpecification
    InstanceTypeSpecification (InstanceTypeSpecification'),
    newInstanceTypeSpecification,

    -- ** JobFlowInstancesConfig
    JobFlowInstancesConfig (JobFlowInstancesConfig'),
    newJobFlowInstancesConfig,

    -- ** KerberosAttributes
    KerberosAttributes (KerberosAttributes'),
    newKerberosAttributes,

    -- ** KeyValue
    KeyValue (KeyValue'),
    newKeyValue,

    -- ** ManagedScalingPolicy
    ManagedScalingPolicy (ManagedScalingPolicy'),
    newManagedScalingPolicy,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** NotebookExecution
    NotebookExecution (NotebookExecution'),
    newNotebookExecution,

    -- ** NotebookExecutionSummary
    NotebookExecutionSummary (NotebookExecutionSummary'),
    newNotebookExecutionSummary,

    -- ** OnDemandProvisioningSpecification
    OnDemandProvisioningSpecification (OnDemandProvisioningSpecification'),
    newOnDemandProvisioningSpecification,

    -- ** PlacementGroupConfig
    PlacementGroupConfig (PlacementGroupConfig'),
    newPlacementGroupConfig,

    -- ** PlacementType
    PlacementType (PlacementType'),
    newPlacementType,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** ScalingAction
    ScalingAction (ScalingAction'),
    newScalingAction,

    -- ** ScalingConstraints
    ScalingConstraints (ScalingConstraints'),
    newScalingConstraints,

    -- ** ScalingRule
    ScalingRule (ScalingRule'),
    newScalingRule,

    -- ** ScalingTrigger
    ScalingTrigger (ScalingTrigger'),
    newScalingTrigger,

    -- ** ScriptBootstrapActionConfig
    ScriptBootstrapActionConfig (ScriptBootstrapActionConfig'),
    newScriptBootstrapActionConfig,

    -- ** SecurityConfigurationSummary
    SecurityConfigurationSummary (SecurityConfigurationSummary'),
    newSecurityConfigurationSummary,

    -- ** SessionMappingDetail
    SessionMappingDetail (SessionMappingDetail'),
    newSessionMappingDetail,

    -- ** SessionMappingSummary
    SessionMappingSummary (SessionMappingSummary'),
    newSessionMappingSummary,

    -- ** ShrinkPolicy
    ShrinkPolicy (ShrinkPolicy'),
    newShrinkPolicy,

    -- ** SimpleScalingPolicyConfiguration
    SimpleScalingPolicyConfiguration (SimpleScalingPolicyConfiguration'),
    newSimpleScalingPolicyConfiguration,

    -- ** SpotProvisioningSpecification
    SpotProvisioningSpecification (SpotProvisioningSpecification'),
    newSpotProvisioningSpecification,

    -- ** Step
    Step (Step'),
    newStep,

    -- ** StepConfig
    StepConfig (StepConfig'),
    newStepConfig,

    -- ** StepStateChangeReason
    StepStateChangeReason (StepStateChangeReason'),
    newStepStateChangeReason,

    -- ** StepStatus
    StepStatus (StepStatus'),
    newStepStatus,

    -- ** StepSummary
    StepSummary (StepSummary'),
    newStepSummary,

    -- ** StepTimeline
    StepTimeline (StepTimeline'),
    newStepTimeline,

    -- ** Studio
    Studio (Studio'),
    newStudio,

    -- ** StudioSummary
    StudioSummary (StudioSummary'),
    newStudioSummary,

    -- ** SupportedProductConfig
    SupportedProductConfig (SupportedProductConfig'),
    newSupportedProductConfig,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VolumeSpecification
    VolumeSpecification (VolumeSpecification'),
    newVolumeSpecification,
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
import Network.AWS.EMR.Lens
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
import Network.AWS.EMR.UpdateStudio
import Network.AWS.EMR.UpdateStudioSessionMapping
import Network.AWS.EMR.Waiters

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
