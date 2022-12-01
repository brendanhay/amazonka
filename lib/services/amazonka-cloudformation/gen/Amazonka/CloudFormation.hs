{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudFormation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- CloudFormation
--
-- CloudFormation allows you to create and manage Amazon Web Services
-- infrastructure deployments predictably and repeatedly. You can use
-- CloudFormation to leverage Amazon Web Services products, such as Amazon
-- Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple
-- Notification Service, Elastic Load Balancing, and Auto Scaling to build
-- highly reliable, highly scalable, cost-effective applications without
-- creating or configuring the underlying Amazon Web Services
-- infrastructure.
--
-- With CloudFormation, you declare all your resources and dependencies in
-- a template file. The template defines a collection of resources as a
-- single unit called a stack. CloudFormation creates and deletes all
-- member resources of the stack together and manages all dependencies
-- between the resources for you.
--
-- For more information about CloudFormation, see the
-- <http://aws.amazon.com/cloudformation/ CloudFormation product page>.
--
-- CloudFormation makes use of other Amazon Web Services products. If you
-- need additional technical information about a specific Amazon Web
-- Services product, you can find the product\'s technical documentation at
-- <https://docs.aws.amazon.com/ docs.aws.amazon.com> .
module Amazonka.CloudFormation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** OperationNotFoundException
    _OperationNotFoundException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** StackSetNotEmptyException
    _StackSetNotEmptyException,

    -- ** StaleRequestException
    _StaleRequestException,

    -- ** ChangeSetNotFoundException
    _ChangeSetNotFoundException,

    -- ** OperationStatusCheckFailedException
    _OperationStatusCheckFailedException,

    -- ** StackInstanceNotFoundException
    _StackInstanceNotFoundException,

    -- ** TokenAlreadyExistsException
    _TokenAlreadyExistsException,

    -- ** InsufficientCapabilitiesException
    _InsufficientCapabilitiesException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** CFNRegistryException
    _CFNRegistryException,

    -- ** TypeConfigurationNotFoundException
    _TypeConfigurationNotFoundException,

    -- ** OperationIdAlreadyExistsException
    _OperationIdAlreadyExistsException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** CreatedButModifiedException
    _CreatedButModifiedException,

    -- ** StackNotFoundException
    _StackNotFoundException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** NameAlreadyExistsException
    _NameAlreadyExistsException,

    -- ** TypeNotFoundException
    _TypeNotFoundException,

    -- ** InvalidChangeSetStatusException
    _InvalidChangeSetStatusException,

    -- ** StackSetNotFoundException
    _StackSetNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** StackUpdateComplete
    newStackUpdateComplete,

    -- ** StackImportComplete
    newStackImportComplete,

    -- ** StackCreateComplete
    newStackCreateComplete,

    -- ** StackRollbackComplete
    newStackRollbackComplete,

    -- ** StackExists
    newStackExists,

    -- ** TypeRegistrationComplete
    newTypeRegistrationComplete,

    -- ** ChangeSetCreateComplete
    newChangeSetCreateComplete,

    -- ** StackDeleteComplete
    newStackDeleteComplete,

    -- * Operations
    -- $operations

    -- ** ActivateType
    ActivateType (ActivateType'),
    newActivateType,
    ActivateTypeResponse (ActivateTypeResponse'),
    newActivateTypeResponse,

    -- ** BatchDescribeTypeConfigurations
    BatchDescribeTypeConfigurations (BatchDescribeTypeConfigurations'),
    newBatchDescribeTypeConfigurations,
    BatchDescribeTypeConfigurationsResponse (BatchDescribeTypeConfigurationsResponse'),
    newBatchDescribeTypeConfigurationsResponse,

    -- ** CancelUpdateStack
    CancelUpdateStack (CancelUpdateStack'),
    newCancelUpdateStack,
    CancelUpdateStackResponse (CancelUpdateStackResponse'),
    newCancelUpdateStackResponse,

    -- ** ContinueUpdateRollback
    ContinueUpdateRollback (ContinueUpdateRollback'),
    newContinueUpdateRollback,
    ContinueUpdateRollbackResponse (ContinueUpdateRollbackResponse'),
    newContinueUpdateRollbackResponse,

    -- ** CreateChangeSet
    CreateChangeSet (CreateChangeSet'),
    newCreateChangeSet,
    CreateChangeSetResponse (CreateChangeSetResponse'),
    newCreateChangeSetResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** CreateStackInstances
    CreateStackInstances (CreateStackInstances'),
    newCreateStackInstances,
    CreateStackInstancesResponse (CreateStackInstancesResponse'),
    newCreateStackInstancesResponse,

    -- ** CreateStackSet
    CreateStackSet (CreateStackSet'),
    newCreateStackSet,
    CreateStackSetResponse (CreateStackSetResponse'),
    newCreateStackSetResponse,

    -- ** DeactivateType
    DeactivateType (DeactivateType'),
    newDeactivateType,
    DeactivateTypeResponse (DeactivateTypeResponse'),
    newDeactivateTypeResponse,

    -- ** DeleteChangeSet
    DeleteChangeSet (DeleteChangeSet'),
    newDeleteChangeSet,
    DeleteChangeSetResponse (DeleteChangeSetResponse'),
    newDeleteChangeSetResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** DeleteStackInstances
    DeleteStackInstances (DeleteStackInstances'),
    newDeleteStackInstances,
    DeleteStackInstancesResponse (DeleteStackInstancesResponse'),
    newDeleteStackInstancesResponse,

    -- ** DeleteStackSet
    DeleteStackSet (DeleteStackSet'),
    newDeleteStackSet,
    DeleteStackSetResponse (DeleteStackSetResponse'),
    newDeleteStackSetResponse,

    -- ** DeregisterType
    DeregisterType (DeregisterType'),
    newDeregisterType,
    DeregisterTypeResponse (DeregisterTypeResponse'),
    newDeregisterTypeResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** DescribeChangeSet (Paginated)
    DescribeChangeSet (DescribeChangeSet'),
    newDescribeChangeSet,
    DescribeChangeSetResponse (DescribeChangeSetResponse'),
    newDescribeChangeSetResponse,

    -- ** DescribeChangeSetHooks
    DescribeChangeSetHooks (DescribeChangeSetHooks'),
    newDescribeChangeSetHooks,
    DescribeChangeSetHooksResponse (DescribeChangeSetHooksResponse'),
    newDescribeChangeSetHooksResponse,

    -- ** DescribePublisher
    DescribePublisher (DescribePublisher'),
    newDescribePublisher,
    DescribePublisherResponse (DescribePublisherResponse'),
    newDescribePublisherResponse,

    -- ** DescribeStackDriftDetectionStatus
    DescribeStackDriftDetectionStatus (DescribeStackDriftDetectionStatus'),
    newDescribeStackDriftDetectionStatus,
    DescribeStackDriftDetectionStatusResponse (DescribeStackDriftDetectionStatusResponse'),
    newDescribeStackDriftDetectionStatusResponse,

    -- ** DescribeStackEvents (Paginated)
    DescribeStackEvents (DescribeStackEvents'),
    newDescribeStackEvents,
    DescribeStackEventsResponse (DescribeStackEventsResponse'),
    newDescribeStackEventsResponse,

    -- ** DescribeStackInstance
    DescribeStackInstance (DescribeStackInstance'),
    newDescribeStackInstance,
    DescribeStackInstanceResponse (DescribeStackInstanceResponse'),
    newDescribeStackInstanceResponse,

    -- ** DescribeStackResource
    DescribeStackResource (DescribeStackResource'),
    newDescribeStackResource,
    DescribeStackResourceResponse (DescribeStackResourceResponse'),
    newDescribeStackResourceResponse,

    -- ** DescribeStackResourceDrifts
    DescribeStackResourceDrifts (DescribeStackResourceDrifts'),
    newDescribeStackResourceDrifts,
    DescribeStackResourceDriftsResponse (DescribeStackResourceDriftsResponse'),
    newDescribeStackResourceDriftsResponse,

    -- ** DescribeStackResources
    DescribeStackResources (DescribeStackResources'),
    newDescribeStackResources,
    DescribeStackResourcesResponse (DescribeStackResourcesResponse'),
    newDescribeStackResourcesResponse,

    -- ** DescribeStackSet
    DescribeStackSet (DescribeStackSet'),
    newDescribeStackSet,
    DescribeStackSetResponse (DescribeStackSetResponse'),
    newDescribeStackSetResponse,

    -- ** DescribeStackSetOperation
    DescribeStackSetOperation (DescribeStackSetOperation'),
    newDescribeStackSetOperation,
    DescribeStackSetOperationResponse (DescribeStackSetOperationResponse'),
    newDescribeStackSetOperationResponse,

    -- ** DescribeStacks (Paginated)
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** DescribeType
    DescribeType (DescribeType'),
    newDescribeType,
    DescribeTypeResponse (DescribeTypeResponse'),
    newDescribeTypeResponse,

    -- ** DescribeTypeRegistration
    DescribeTypeRegistration (DescribeTypeRegistration'),
    newDescribeTypeRegistration,
    DescribeTypeRegistrationResponse (DescribeTypeRegistrationResponse'),
    newDescribeTypeRegistrationResponse,

    -- ** DetectStackDrift
    DetectStackDrift (DetectStackDrift'),
    newDetectStackDrift,
    DetectStackDriftResponse (DetectStackDriftResponse'),
    newDetectStackDriftResponse,

    -- ** DetectStackResourceDrift
    DetectStackResourceDrift (DetectStackResourceDrift'),
    newDetectStackResourceDrift,
    DetectStackResourceDriftResponse (DetectStackResourceDriftResponse'),
    newDetectStackResourceDriftResponse,

    -- ** DetectStackSetDrift
    DetectStackSetDrift (DetectStackSetDrift'),
    newDetectStackSetDrift,
    DetectStackSetDriftResponse (DetectStackSetDriftResponse'),
    newDetectStackSetDriftResponse,

    -- ** EstimateTemplateCost
    EstimateTemplateCost (EstimateTemplateCost'),
    newEstimateTemplateCost,
    EstimateTemplateCostResponse (EstimateTemplateCostResponse'),
    newEstimateTemplateCostResponse,

    -- ** ExecuteChangeSet
    ExecuteChangeSet (ExecuteChangeSet'),
    newExecuteChangeSet,
    ExecuteChangeSetResponse (ExecuteChangeSetResponse'),
    newExecuteChangeSetResponse,

    -- ** GetStackPolicy
    GetStackPolicy (GetStackPolicy'),
    newGetStackPolicy,
    GetStackPolicyResponse (GetStackPolicyResponse'),
    newGetStackPolicyResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** GetTemplateSummary
    GetTemplateSummary (GetTemplateSummary'),
    newGetTemplateSummary,
    GetTemplateSummaryResponse (GetTemplateSummaryResponse'),
    newGetTemplateSummaryResponse,

    -- ** ImportStacksToStackSet
    ImportStacksToStackSet (ImportStacksToStackSet'),
    newImportStacksToStackSet,
    ImportStacksToStackSetResponse (ImportStacksToStackSetResponse'),
    newImportStacksToStackSetResponse,

    -- ** ListChangeSets (Paginated)
    ListChangeSets (ListChangeSets'),
    newListChangeSets,
    ListChangeSetsResponse (ListChangeSetsResponse'),
    newListChangeSetsResponse,

    -- ** ListExports (Paginated)
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** ListImports (Paginated)
    ListImports (ListImports'),
    newListImports,
    ListImportsResponse (ListImportsResponse'),
    newListImportsResponse,

    -- ** ListStackInstances (Paginated)
    ListStackInstances (ListStackInstances'),
    newListStackInstances,
    ListStackInstancesResponse (ListStackInstancesResponse'),
    newListStackInstancesResponse,

    -- ** ListStackResources (Paginated)
    ListStackResources (ListStackResources'),
    newListStackResources,
    ListStackResourcesResponse (ListStackResourcesResponse'),
    newListStackResourcesResponse,

    -- ** ListStackSetOperationResults (Paginated)
    ListStackSetOperationResults (ListStackSetOperationResults'),
    newListStackSetOperationResults,
    ListStackSetOperationResultsResponse (ListStackSetOperationResultsResponse'),
    newListStackSetOperationResultsResponse,

    -- ** ListStackSetOperations (Paginated)
    ListStackSetOperations (ListStackSetOperations'),
    newListStackSetOperations,
    ListStackSetOperationsResponse (ListStackSetOperationsResponse'),
    newListStackSetOperationsResponse,

    -- ** ListStackSets (Paginated)
    ListStackSets (ListStackSets'),
    newListStackSets,
    ListStackSetsResponse (ListStackSetsResponse'),
    newListStackSetsResponse,

    -- ** ListStacks (Paginated)
    ListStacks (ListStacks'),
    newListStacks,
    ListStacksResponse (ListStacksResponse'),
    newListStacksResponse,

    -- ** ListTypeRegistrations
    ListTypeRegistrations (ListTypeRegistrations'),
    newListTypeRegistrations,
    ListTypeRegistrationsResponse (ListTypeRegistrationsResponse'),
    newListTypeRegistrationsResponse,

    -- ** ListTypeVersions
    ListTypeVersions (ListTypeVersions'),
    newListTypeVersions,
    ListTypeVersionsResponse (ListTypeVersionsResponse'),
    newListTypeVersionsResponse,

    -- ** ListTypes (Paginated)
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** PublishType
    PublishType (PublishType'),
    newPublishType,
    PublishTypeResponse (PublishTypeResponse'),
    newPublishTypeResponse,

    -- ** RecordHandlerProgress
    RecordHandlerProgress (RecordHandlerProgress'),
    newRecordHandlerProgress,
    RecordHandlerProgressResponse (RecordHandlerProgressResponse'),
    newRecordHandlerProgressResponse,

    -- ** RegisterPublisher
    RegisterPublisher (RegisterPublisher'),
    newRegisterPublisher,
    RegisterPublisherResponse (RegisterPublisherResponse'),
    newRegisterPublisherResponse,

    -- ** RegisterType
    RegisterType (RegisterType'),
    newRegisterType,
    RegisterTypeResponse (RegisterTypeResponse'),
    newRegisterTypeResponse,

    -- ** RollbackStack
    RollbackStack (RollbackStack'),
    newRollbackStack,
    RollbackStackResponse (RollbackStackResponse'),
    newRollbackStackResponse,

    -- ** SetStackPolicy
    SetStackPolicy (SetStackPolicy'),
    newSetStackPolicy,
    SetStackPolicyResponse (SetStackPolicyResponse'),
    newSetStackPolicyResponse,

    -- ** SetTypeConfiguration
    SetTypeConfiguration (SetTypeConfiguration'),
    newSetTypeConfiguration,
    SetTypeConfigurationResponse (SetTypeConfigurationResponse'),
    newSetTypeConfigurationResponse,

    -- ** SetTypeDefaultVersion
    SetTypeDefaultVersion (SetTypeDefaultVersion'),
    newSetTypeDefaultVersion,
    SetTypeDefaultVersionResponse (SetTypeDefaultVersionResponse'),
    newSetTypeDefaultVersionResponse,

    -- ** SignalResource
    SignalResource (SignalResource'),
    newSignalResource,
    SignalResourceResponse (SignalResourceResponse'),
    newSignalResourceResponse,

    -- ** StopStackSetOperation
    StopStackSetOperation (StopStackSetOperation'),
    newStopStackSetOperation,
    StopStackSetOperationResponse (StopStackSetOperationResponse'),
    newStopStackSetOperationResponse,

    -- ** TestType
    TestType (TestType'),
    newTestType,
    TestTypeResponse (TestTypeResponse'),
    newTestTypeResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- ** UpdateStackInstances
    UpdateStackInstances (UpdateStackInstances'),
    newUpdateStackInstances,
    UpdateStackInstancesResponse (UpdateStackInstancesResponse'),
    newUpdateStackInstancesResponse,

    -- ** UpdateStackSet
    UpdateStackSet (UpdateStackSet'),
    newUpdateStackSet,
    UpdateStackSetResponse (UpdateStackSetResponse'),
    newUpdateStackSetResponse,

    -- ** UpdateTerminationProtection
    UpdateTerminationProtection (UpdateTerminationProtection'),
    newUpdateTerminationProtection,
    UpdateTerminationProtectionResponse (UpdateTerminationProtectionResponse'),
    newUpdateTerminationProtectionResponse,

    -- ** ValidateTemplate
    ValidateTemplate (ValidateTemplate'),
    newValidateTemplate,
    ValidateTemplateResponse (ValidateTemplateResponse'),
    newValidateTemplateResponse,

    -- * Types

    -- ** AccountFilterType
    AccountFilterType (..),

    -- ** AccountGateStatus
    AccountGateStatus (..),

    -- ** CallAs
    CallAs (..),

    -- ** Capability
    Capability (..),

    -- ** Category
    Category (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeSetHooksStatus
    ChangeSetHooksStatus (..),

    -- ** ChangeSetStatus
    ChangeSetStatus (..),

    -- ** ChangeSetType
    ChangeSetType (..),

    -- ** ChangeSource
    ChangeSource (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** DeprecatedStatus
    DeprecatedStatus (..),

    -- ** DifferenceType
    DifferenceType (..),

    -- ** EvaluationType
    EvaluationType (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** HandlerErrorCode
    HandlerErrorCode (..),

    -- ** HookFailureMode
    HookFailureMode (..),

    -- ** HookInvocationPoint
    HookInvocationPoint (..),

    -- ** HookStatus
    HookStatus (..),

    -- ** HookTargetType
    HookTargetType (..),

    -- ** IdentityProvider
    IdentityProvider (..),

    -- ** OnFailure
    OnFailure (..),

    -- ** OperationResultFilterName
    OperationResultFilterName (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** PermissionModels
    PermissionModels (..),

    -- ** ProvisioningType
    ProvisioningType (..),

    -- ** PublisherStatus
    PublisherStatus (..),

    -- ** RegionConcurrencyType
    RegionConcurrencyType (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** RegistryType
    RegistryType (..),

    -- ** Replacement
    Replacement (..),

    -- ** RequiresRecreation
    RequiresRecreation (..),

    -- ** ResourceAttribute
    ResourceAttribute (..),

    -- ** ResourceSignalStatus
    ResourceSignalStatus (..),

    -- ** ResourceStatus
    ResourceStatus (..),

    -- ** StackDriftDetectionStatus
    StackDriftDetectionStatus (..),

    -- ** StackDriftStatus
    StackDriftStatus (..),

    -- ** StackInstanceDetailedStatus
    StackInstanceDetailedStatus (..),

    -- ** StackInstanceFilterName
    StackInstanceFilterName (..),

    -- ** StackInstanceStatus
    StackInstanceStatus (..),

    -- ** StackResourceDriftStatus
    StackResourceDriftStatus (..),

    -- ** StackSetDriftDetectionStatus
    StackSetDriftDetectionStatus (..),

    -- ** StackSetDriftStatus
    StackSetDriftStatus (..),

    -- ** StackSetOperationAction
    StackSetOperationAction (..),

    -- ** StackSetOperationResultStatus
    StackSetOperationResultStatus (..),

    -- ** StackSetOperationStatus
    StackSetOperationStatus (..),

    -- ** StackSetStatus
    StackSetStatus (..),

    -- ** StackStatus
    StackStatus (..),

    -- ** TemplateStage
    TemplateStage (..),

    -- ** ThirdPartyType
    ThirdPartyType (..),

    -- ** TypeTestsStatus
    TypeTestsStatus (..),

    -- ** VersionBump
    VersionBump (..),

    -- ** Visibility
    Visibility (..),

    -- ** AccountGateResult
    AccountGateResult (AccountGateResult'),
    newAccountGateResult,

    -- ** AccountLimit
    AccountLimit (AccountLimit'),
    newAccountLimit,

    -- ** AutoDeployment
    AutoDeployment (AutoDeployment'),
    newAutoDeployment,

    -- ** BatchDescribeTypeConfigurationsError
    BatchDescribeTypeConfigurationsError (BatchDescribeTypeConfigurationsError'),
    newBatchDescribeTypeConfigurationsError,

    -- ** Change
    Change (Change'),
    newChange,

    -- ** ChangeSetHook
    ChangeSetHook (ChangeSetHook'),
    newChangeSetHook,

    -- ** ChangeSetHookResourceTargetDetails
    ChangeSetHookResourceTargetDetails (ChangeSetHookResourceTargetDetails'),
    newChangeSetHookResourceTargetDetails,

    -- ** ChangeSetHookTargetDetails
    ChangeSetHookTargetDetails (ChangeSetHookTargetDetails'),
    newChangeSetHookTargetDetails,

    -- ** ChangeSetSummary
    ChangeSetSummary (ChangeSetSummary'),
    newChangeSetSummary,

    -- ** DeploymentTargets
    DeploymentTargets (DeploymentTargets'),
    newDeploymentTargets,

    -- ** Export
    Export (Export'),
    newExport,

    -- ** LoggingConfig
    LoggingConfig (LoggingConfig'),
    newLoggingConfig,

    -- ** ManagedExecution
    ManagedExecution (ManagedExecution'),
    newManagedExecution,

    -- ** ModuleInfo
    ModuleInfo (ModuleInfo'),
    newModuleInfo,

    -- ** OperationResultFilter
    OperationResultFilter (OperationResultFilter'),
    newOperationResultFilter,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterConstraints
    ParameterConstraints (ParameterConstraints'),
    newParameterConstraints,

    -- ** ParameterDeclaration
    ParameterDeclaration (ParameterDeclaration'),
    newParameterDeclaration,

    -- ** PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair (PhysicalResourceIdContextKeyValuePair'),
    newPhysicalResourceIdContextKeyValuePair,

    -- ** PropertyDifference
    PropertyDifference (PropertyDifference'),
    newPropertyDifference,

    -- ** RequiredActivatedType
    RequiredActivatedType (RequiredActivatedType'),
    newRequiredActivatedType,

    -- ** ResourceChange
    ResourceChange (ResourceChange'),
    newResourceChange,

    -- ** ResourceChangeDetail
    ResourceChangeDetail (ResourceChangeDetail'),
    newResourceChangeDetail,

    -- ** ResourceIdentifierSummary
    ResourceIdentifierSummary (ResourceIdentifierSummary'),
    newResourceIdentifierSummary,

    -- ** ResourceTargetDefinition
    ResourceTargetDefinition (ResourceTargetDefinition'),
    newResourceTargetDefinition,

    -- ** ResourceToImport
    ResourceToImport (ResourceToImport'),
    newResourceToImport,

    -- ** RollbackConfiguration
    RollbackConfiguration (RollbackConfiguration'),
    newRollbackConfiguration,

    -- ** RollbackTrigger
    RollbackTrigger (RollbackTrigger'),
    newRollbackTrigger,

    -- ** Stack
    Stack (Stack'),
    newStack,

    -- ** StackDriftInformation
    StackDriftInformation (StackDriftInformation'),
    newStackDriftInformation,

    -- ** StackDriftInformationSummary
    StackDriftInformationSummary (StackDriftInformationSummary'),
    newStackDriftInformationSummary,

    -- ** StackEvent
    StackEvent (StackEvent'),
    newStackEvent,

    -- ** StackInstance
    StackInstance (StackInstance'),
    newStackInstance,

    -- ** StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus (StackInstanceComprehensiveStatus'),
    newStackInstanceComprehensiveStatus,

    -- ** StackInstanceFilter
    StackInstanceFilter (StackInstanceFilter'),
    newStackInstanceFilter,

    -- ** StackInstanceSummary
    StackInstanceSummary (StackInstanceSummary'),
    newStackInstanceSummary,

    -- ** StackResource
    StackResource (StackResource'),
    newStackResource,

    -- ** StackResourceDetail
    StackResourceDetail (StackResourceDetail'),
    newStackResourceDetail,

    -- ** StackResourceDrift
    StackResourceDrift (StackResourceDrift'),
    newStackResourceDrift,

    -- ** StackResourceDriftInformation
    StackResourceDriftInformation (StackResourceDriftInformation'),
    newStackResourceDriftInformation,

    -- ** StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary (StackResourceDriftInformationSummary'),
    newStackResourceDriftInformationSummary,

    -- ** StackResourceSummary
    StackResourceSummary (StackResourceSummary'),
    newStackResourceSummary,

    -- ** StackSet
    StackSet (StackSet'),
    newStackSet,

    -- ** StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails (StackSetDriftDetectionDetails'),
    newStackSetDriftDetectionDetails,

    -- ** StackSetOperation
    StackSetOperation (StackSetOperation'),
    newStackSetOperation,

    -- ** StackSetOperationPreferences
    StackSetOperationPreferences (StackSetOperationPreferences'),
    newStackSetOperationPreferences,

    -- ** StackSetOperationResultSummary
    StackSetOperationResultSummary (StackSetOperationResultSummary'),
    newStackSetOperationResultSummary,

    -- ** StackSetOperationStatusDetails
    StackSetOperationStatusDetails (StackSetOperationStatusDetails'),
    newStackSetOperationStatusDetails,

    -- ** StackSetOperationSummary
    StackSetOperationSummary (StackSetOperationSummary'),
    newStackSetOperationSummary,

    -- ** StackSetSummary
    StackSetSummary (StackSetSummary'),
    newStackSetSummary,

    -- ** StackSummary
    StackSummary (StackSummary'),
    newStackSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TemplateParameter
    TemplateParameter (TemplateParameter'),
    newTemplateParameter,

    -- ** TypeConfigurationDetails
    TypeConfigurationDetails (TypeConfigurationDetails'),
    newTypeConfigurationDetails,

    -- ** TypeConfigurationIdentifier
    TypeConfigurationIdentifier (TypeConfigurationIdentifier'),
    newTypeConfigurationIdentifier,

    -- ** TypeFilters
    TypeFilters (TypeFilters'),
    newTypeFilters,

    -- ** TypeSummary
    TypeSummary (TypeSummary'),
    newTypeSummary,

    -- ** TypeVersionSummary
    TypeVersionSummary (TypeVersionSummary'),
    newTypeVersionSummary,
  )
where

import Amazonka.CloudFormation.ActivateType
import Amazonka.CloudFormation.BatchDescribeTypeConfigurations
import Amazonka.CloudFormation.CancelUpdateStack
import Amazonka.CloudFormation.ContinueUpdateRollback
import Amazonka.CloudFormation.CreateChangeSet
import Amazonka.CloudFormation.CreateStack
import Amazonka.CloudFormation.CreateStackInstances
import Amazonka.CloudFormation.CreateStackSet
import Amazonka.CloudFormation.DeactivateType
import Amazonka.CloudFormation.DeleteChangeSet
import Amazonka.CloudFormation.DeleteStack
import Amazonka.CloudFormation.DeleteStackInstances
import Amazonka.CloudFormation.DeleteStackSet
import Amazonka.CloudFormation.DeregisterType
import Amazonka.CloudFormation.DescribeAccountLimits
import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.DescribeChangeSetHooks
import Amazonka.CloudFormation.DescribePublisher
import Amazonka.CloudFormation.DescribeStackDriftDetectionStatus
import Amazonka.CloudFormation.DescribeStackEvents
import Amazonka.CloudFormation.DescribeStackInstance
import Amazonka.CloudFormation.DescribeStackResource
import Amazonka.CloudFormation.DescribeStackResourceDrifts
import Amazonka.CloudFormation.DescribeStackResources
import Amazonka.CloudFormation.DescribeStackSet
import Amazonka.CloudFormation.DescribeStackSetOperation
import Amazonka.CloudFormation.DescribeStacks
import Amazonka.CloudFormation.DescribeType
import Amazonka.CloudFormation.DescribeTypeRegistration
import Amazonka.CloudFormation.DetectStackDrift
import Amazonka.CloudFormation.DetectStackResourceDrift
import Amazonka.CloudFormation.DetectStackSetDrift
import Amazonka.CloudFormation.EstimateTemplateCost
import Amazonka.CloudFormation.ExecuteChangeSet
import Amazonka.CloudFormation.GetStackPolicy
import Amazonka.CloudFormation.GetTemplate
import Amazonka.CloudFormation.GetTemplateSummary
import Amazonka.CloudFormation.ImportStacksToStackSet
import Amazonka.CloudFormation.Lens
import Amazonka.CloudFormation.ListChangeSets
import Amazonka.CloudFormation.ListExports
import Amazonka.CloudFormation.ListImports
import Amazonka.CloudFormation.ListStackInstances
import Amazonka.CloudFormation.ListStackResources
import Amazonka.CloudFormation.ListStackSetOperationResults
import Amazonka.CloudFormation.ListStackSetOperations
import Amazonka.CloudFormation.ListStackSets
import Amazonka.CloudFormation.ListStacks
import Amazonka.CloudFormation.ListTypeRegistrations
import Amazonka.CloudFormation.ListTypeVersions
import Amazonka.CloudFormation.ListTypes
import Amazonka.CloudFormation.PublishType
import Amazonka.CloudFormation.RecordHandlerProgress
import Amazonka.CloudFormation.RegisterPublisher
import Amazonka.CloudFormation.RegisterType
import Amazonka.CloudFormation.RollbackStack
import Amazonka.CloudFormation.SetStackPolicy
import Amazonka.CloudFormation.SetTypeConfiguration
import Amazonka.CloudFormation.SetTypeDefaultVersion
import Amazonka.CloudFormation.SignalResource
import Amazonka.CloudFormation.StopStackSetOperation
import Amazonka.CloudFormation.TestType
import Amazonka.CloudFormation.Types
import Amazonka.CloudFormation.UpdateStack
import Amazonka.CloudFormation.UpdateStackInstances
import Amazonka.CloudFormation.UpdateStackSet
import Amazonka.CloudFormation.UpdateTerminationProtection
import Amazonka.CloudFormation.ValidateTemplate
import Amazonka.CloudFormation.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudFormation'.

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
