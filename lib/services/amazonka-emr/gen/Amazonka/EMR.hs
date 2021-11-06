{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EMR
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2009-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EMR is a web service that makes it easier to process large
-- amounts of data efficiently. Amazon EMR uses Hadoop processing combined
-- with several Amazon Web Services services to do tasks such as web
-- indexing, data mining, log file analysis, machine learning, scientific
-- simulation, and data warehouse management.
module Amazonka.EMR
  ( -- * Service Configuration
    defaultService,

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
    newStepComplete,

    -- ** ClusterTerminated
    newClusterTerminated,

    -- ** ClusterRunning
    newClusterRunning,

    -- * Operations
    -- $operations

    -- ** RunJobFlow
    RunJobFlow (RunJobFlow'),
    newRunJobFlow,
    RunJobFlowResponse (RunJobFlowResponse'),
    newRunJobFlowResponse,

    -- ** RemoveAutoScalingPolicy
    RemoveAutoScalingPolicy (RemoveAutoScalingPolicy'),
    newRemoveAutoScalingPolicy,
    RemoveAutoScalingPolicyResponse (RemoveAutoScalingPolicyResponse'),
    newRemoveAutoScalingPolicyResponse,

    -- ** CreateStudio
    CreateStudio (CreateStudio'),
    newCreateStudio,
    CreateStudioResponse (CreateStudioResponse'),
    newCreateStudioResponse,

    -- ** SetVisibleToAllUsers
    SetVisibleToAllUsers (SetVisibleToAllUsers'),
    newSetVisibleToAllUsers,
    SetVisibleToAllUsersResponse (SetVisibleToAllUsersResponse'),
    newSetVisibleToAllUsersResponse,

    -- ** TerminateJobFlows
    TerminateJobFlows (TerminateJobFlows'),
    newTerminateJobFlows,
    TerminateJobFlowsResponse (TerminateJobFlowsResponse'),
    newTerminateJobFlowsResponse,

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

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** ListSecurityConfigurations (Paginated)
    ListSecurityConfigurations (ListSecurityConfigurations'),
    newListSecurityConfigurations,
    ListSecurityConfigurationsResponse (ListSecurityConfigurationsResponse'),
    newListSecurityConfigurationsResponse,

    -- ** CancelSteps
    CancelSteps (CancelSteps'),
    newCancelSteps,
    CancelStepsResponse (CancelStepsResponse'),
    newCancelStepsResponse,

    -- ** ListNotebookExecutions (Paginated)
    ListNotebookExecutions (ListNotebookExecutions'),
    newListNotebookExecutions,
    ListNotebookExecutionsResponse (ListNotebookExecutionsResponse'),
    newListNotebookExecutionsResponse,

    -- ** PutAutoTerminationPolicy
    PutAutoTerminationPolicy (PutAutoTerminationPolicy'),
    newPutAutoTerminationPolicy,
    PutAutoTerminationPolicyResponse (PutAutoTerminationPolicyResponse'),
    newPutAutoTerminationPolicyResponse,

    -- ** CreateSecurityConfiguration
    CreateSecurityConfiguration (CreateSecurityConfiguration'),
    newCreateSecurityConfiguration,
    CreateSecurityConfigurationResponse (CreateSecurityConfigurationResponse'),
    newCreateSecurityConfigurationResponse,

    -- ** DescribeReleaseLabel
    DescribeReleaseLabel (DescribeReleaseLabel'),
    newDescribeReleaseLabel,
    DescribeReleaseLabelResponse (DescribeReleaseLabelResponse'),
    newDescribeReleaseLabelResponse,

    -- ** SetTerminationProtection
    SetTerminationProtection (SetTerminationProtection'),
    newSetTerminationProtection,
    SetTerminationProtectionResponse (SetTerminationProtectionResponse'),
    newSetTerminationProtectionResponse,

    -- ** AddJobFlowSteps
    AddJobFlowSteps (AddJobFlowSteps'),
    newAddJobFlowSteps,
    AddJobFlowStepsResponse (AddJobFlowStepsResponse'),
    newAddJobFlowStepsResponse,

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

    -- ** StartNotebookExecution
    StartNotebookExecution (StartNotebookExecution'),
    newStartNotebookExecution,
    StartNotebookExecutionResponse (StartNotebookExecutionResponse'),
    newStartNotebookExecutionResponse,

    -- ** ListSteps (Paginated)
    ListSteps (ListSteps'),
    newListSteps,
    ListStepsResponse (ListStepsResponse'),
    newListStepsResponse,

    -- ** ListReleaseLabels
    ListReleaseLabels (ListReleaseLabels'),
    newListReleaseLabels,
    ListReleaseLabelsResponse (ListReleaseLabelsResponse'),
    newListReleaseLabelsResponse,

    -- ** CreateStudioSessionMapping
    CreateStudioSessionMapping (CreateStudioSessionMapping'),
    newCreateStudioSessionMapping,
    CreateStudioSessionMappingResponse (CreateStudioSessionMappingResponse'),
    newCreateStudioSessionMappingResponse,

    -- ** AddInstanceFleet
    AddInstanceFleet (AddInstanceFleet'),
    newAddInstanceFleet,
    AddInstanceFleetResponse (AddInstanceFleetResponse'),
    newAddInstanceFleetResponse,

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

    -- ** ListStudios (Paginated)
    ListStudios (ListStudios'),
    newListStudios,
    ListStudiosResponse (ListStudiosResponse'),
    newListStudiosResponse,

    -- ** PutManagedScalingPolicy
    PutManagedScalingPolicy (PutManagedScalingPolicy'),
    newPutManagedScalingPolicy,
    PutManagedScalingPolicyResponse (PutManagedScalingPolicyResponse'),
    newPutManagedScalingPolicyResponse,

    -- ** AddInstanceGroups
    AddInstanceGroups (AddInstanceGroups'),
    newAddInstanceGroups,
    AddInstanceGroupsResponse (AddInstanceGroupsResponse'),
    newAddInstanceGroupsResponse,

    -- ** GetStudioSessionMapping
    GetStudioSessionMapping (GetStudioSessionMapping'),
    newGetStudioSessionMapping,
    GetStudioSessionMappingResponse (GetStudioSessionMappingResponse'),
    newGetStudioSessionMappingResponse,

    -- ** DeleteSecurityConfiguration
    DeleteSecurityConfiguration (DeleteSecurityConfiguration'),
    newDeleteSecurityConfiguration,
    DeleteSecurityConfigurationResponse (DeleteSecurityConfigurationResponse'),
    newDeleteSecurityConfigurationResponse,

    -- ** ModifyInstanceFleet
    ModifyInstanceFleet (ModifyInstanceFleet'),
    newModifyInstanceFleet,
    ModifyInstanceFleetResponse (ModifyInstanceFleetResponse'),
    newModifyInstanceFleetResponse,

    -- ** ListInstanceGroups (Paginated)
    ListInstanceGroups (ListInstanceGroups'),
    newListInstanceGroups,
    ListInstanceGroupsResponse (ListInstanceGroupsResponse'),
    newListInstanceGroupsResponse,

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

    -- ** GetAutoTerminationPolicy
    GetAutoTerminationPolicy (GetAutoTerminationPolicy'),
    newGetAutoTerminationPolicy,
    GetAutoTerminationPolicyResponse (GetAutoTerminationPolicyResponse'),
    newGetAutoTerminationPolicyResponse,

    -- ** PutBlockPublicAccessConfiguration
    PutBlockPublicAccessConfiguration (PutBlockPublicAccessConfiguration'),
    newPutBlockPublicAccessConfiguration,
    PutBlockPublicAccessConfigurationResponse (PutBlockPublicAccessConfigurationResponse'),
    newPutBlockPublicAccessConfigurationResponse,

    -- ** ListBootstrapActions (Paginated)
    ListBootstrapActions (ListBootstrapActions'),
    newListBootstrapActions,
    ListBootstrapActionsResponse (ListBootstrapActionsResponse'),
    newListBootstrapActionsResponse,

    -- ** RemoveAutoTerminationPolicy
    RemoveAutoTerminationPolicy (RemoveAutoTerminationPolicy'),
    newRemoveAutoTerminationPolicy,
    RemoveAutoTerminationPolicyResponse (RemoveAutoTerminationPolicyResponse'),
    newRemoveAutoTerminationPolicyResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** PutAutoScalingPolicy
    PutAutoScalingPolicy (PutAutoScalingPolicy'),
    newPutAutoScalingPolicy,
    PutAutoScalingPolicyResponse (PutAutoScalingPolicyResponse'),
    newPutAutoScalingPolicyResponse,

    -- ** DeleteStudioSessionMapping
    DeleteStudioSessionMapping (DeleteStudioSessionMapping'),
    newDeleteStudioSessionMapping,
    DeleteStudioSessionMappingResponse (DeleteStudioSessionMappingResponse'),
    newDeleteStudioSessionMappingResponse,

    -- ** UpdateStudioSessionMapping
    UpdateStudioSessionMapping (UpdateStudioSessionMapping'),
    newUpdateStudioSessionMapping,
    UpdateStudioSessionMappingResponse (UpdateStudioSessionMappingResponse'),
    newUpdateStudioSessionMappingResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** DescribeSecurityConfiguration
    DescribeSecurityConfiguration (DescribeSecurityConfiguration'),
    newDescribeSecurityConfiguration,
    DescribeSecurityConfigurationResponse (DescribeSecurityConfigurationResponse'),
    newDescribeSecurityConfigurationResponse,

    -- ** StopNotebookExecution
    StopNotebookExecution (StopNotebookExecution'),
    newStopNotebookExecution,
    StopNotebookExecutionResponse (StopNotebookExecutionResponse'),
    newStopNotebookExecutionResponse,

    -- ** ListStudioSessionMappings (Paginated)
    ListStudioSessionMappings (ListStudioSessionMappings'),
    newListStudioSessionMappings,
    ListStudioSessionMappingsResponse (ListStudioSessionMappingsResponse'),
    newListStudioSessionMappingsResponse,

    -- ** GetManagedScalingPolicy
    GetManagedScalingPolicy (GetManagedScalingPolicy'),
    newGetManagedScalingPolicy,
    GetManagedScalingPolicyResponse (GetManagedScalingPolicyResponse'),
    newGetManagedScalingPolicyResponse,

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

    -- ** DescribeNotebookExecution
    DescribeNotebookExecution (DescribeNotebookExecution'),
    newDescribeNotebookExecution,
    DescribeNotebookExecutionResponse (DescribeNotebookExecutionResponse'),
    newDescribeNotebookExecutionResponse,

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

    -- ** OnDemandCapacityReservationPreference
    OnDemandCapacityReservationPreference (..),

    -- ** OnDemandCapacityReservationUsageStrategy
    OnDemandCapacityReservationUsageStrategy (..),

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

    -- ** AutoTerminationPolicy
    AutoTerminationPolicy (AutoTerminationPolicy'),
    newAutoTerminationPolicy,

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

    -- ** OnDemandCapacityReservationOptions
    OnDemandCapacityReservationOptions (OnDemandCapacityReservationOptions'),
    newOnDemandCapacityReservationOptions,

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

    -- ** ReleaseLabelFilter
    ReleaseLabelFilter (ReleaseLabelFilter'),
    newReleaseLabelFilter,

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

    -- ** SimplifiedApplication
    SimplifiedApplication (SimplifiedApplication'),
    newSimplifiedApplication,

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

import Amazonka.EMR.AddInstanceFleet
import Amazonka.EMR.AddInstanceGroups
import Amazonka.EMR.AddJobFlowSteps
import Amazonka.EMR.AddTags
import Amazonka.EMR.CancelSteps
import Amazonka.EMR.CreateSecurityConfiguration
import Amazonka.EMR.CreateStudio
import Amazonka.EMR.CreateStudioSessionMapping
import Amazonka.EMR.DeleteSecurityConfiguration
import Amazonka.EMR.DeleteStudio
import Amazonka.EMR.DeleteStudioSessionMapping
import Amazonka.EMR.DescribeCluster
import Amazonka.EMR.DescribeNotebookExecution
import Amazonka.EMR.DescribeReleaseLabel
import Amazonka.EMR.DescribeSecurityConfiguration
import Amazonka.EMR.DescribeStep
import Amazonka.EMR.DescribeStudio
import Amazonka.EMR.GetAutoTerminationPolicy
import Amazonka.EMR.GetBlockPublicAccessConfiguration
import Amazonka.EMR.GetManagedScalingPolicy
import Amazonka.EMR.GetStudioSessionMapping
import Amazonka.EMR.Lens
import Amazonka.EMR.ListBootstrapActions
import Amazonka.EMR.ListClusters
import Amazonka.EMR.ListInstanceFleets
import Amazonka.EMR.ListInstanceGroups
import Amazonka.EMR.ListInstances
import Amazonka.EMR.ListNotebookExecutions
import Amazonka.EMR.ListReleaseLabels
import Amazonka.EMR.ListSecurityConfigurations
import Amazonka.EMR.ListSteps
import Amazonka.EMR.ListStudioSessionMappings
import Amazonka.EMR.ListStudios
import Amazonka.EMR.ModifyCluster
import Amazonka.EMR.ModifyInstanceFleet
import Amazonka.EMR.ModifyInstanceGroups
import Amazonka.EMR.PutAutoScalingPolicy
import Amazonka.EMR.PutAutoTerminationPolicy
import Amazonka.EMR.PutBlockPublicAccessConfiguration
import Amazonka.EMR.PutManagedScalingPolicy
import Amazonka.EMR.RemoveAutoScalingPolicy
import Amazonka.EMR.RemoveAutoTerminationPolicy
import Amazonka.EMR.RemoveManagedScalingPolicy
import Amazonka.EMR.RemoveTags
import Amazonka.EMR.RunJobFlow
import Amazonka.EMR.SetTerminationProtection
import Amazonka.EMR.SetVisibleToAllUsers
import Amazonka.EMR.StartNotebookExecution
import Amazonka.EMR.StopNotebookExecution
import Amazonka.EMR.TerminateJobFlows
import Amazonka.EMR.Types
import Amazonka.EMR.UpdateStudio
import Amazonka.EMR.UpdateStudioSessionMapping
import Amazonka.EMR.Waiters

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
