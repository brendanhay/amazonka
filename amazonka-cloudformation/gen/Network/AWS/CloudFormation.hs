{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS CloudFormation
--
-- CloudFormation allows you to create and manage Amazon Web Services
-- infrastructure deployments predictably and repeatedly. You can use
-- CloudFormation to leverage Amazon Web Services products, such as Amazon
-- Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple
-- Notification Service, Elastic Load Balancing, and Auto Scaling to build
-- highly-reliable, highly scalable, cost-effective applications without
-- creating or configuring the underlying Amazon Web Services
-- infrastructure.
--
-- With CloudFormation, you declare all of your resources and dependencies
-- in a template file. The template defines a collection of resources as a
-- single unit called a stack. CloudFormation creates and deletes all
-- member resources of the stack together and manages all dependencies
-- between the resources for you.
--
-- For more information about CloudFormation, see the
-- <http://aws.amazon.com/cloudformation/ CloudFormation Product Page>.
--
-- CloudFormation makes use of other Amazon Web Services products. If you
-- need additional technical information about a specific Amazon Web
-- Services product, you can find the product\'s technical documentation at
-- <https://docs.aws.amazon.com/ docs.aws.amazon.com> .
module Network.AWS.CloudFormation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InsufficientCapabilitiesException
    _InsufficientCapabilitiesException,

    -- ** StaleRequestException
    _StaleRequestException,

    -- ** OperationNotFoundException
    _OperationNotFoundException,

    -- ** NameAlreadyExistsException
    _NameAlreadyExistsException,

    -- ** InvalidChangeSetStatusException
    _InvalidChangeSetStatusException,

    -- ** OperationStatusCheckFailedException
    _OperationStatusCheckFailedException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** StackSetNotEmptyException
    _StackSetNotEmptyException,

    -- ** ChangeSetNotFoundException
    _ChangeSetNotFoundException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** StackSetNotFoundException
    _StackSetNotFoundException,

    -- ** CreatedButModifiedException
    _CreatedButModifiedException,

    -- ** TokenAlreadyExistsException
    _TokenAlreadyExistsException,

    -- ** TypeConfigurationNotFoundException
    _TypeConfigurationNotFoundException,

    -- ** TypeNotFoundException
    _TypeNotFoundException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** OperationIdAlreadyExistsException
    _OperationIdAlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** CFNRegistryException
    _CFNRegistryException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** StackInstanceNotFoundException
    _StackInstanceNotFoundException,

    -- ** StackNotFoundException
    _StackNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** ChangeSetCreateComplete
    newChangeSetCreateComplete,

    -- ** StackRollbackComplete
    newStackRollbackComplete,

    -- ** StackExists
    newStackExists,

    -- ** StackUpdateComplete
    newStackUpdateComplete,

    -- ** StackDeleteComplete
    newStackDeleteComplete,

    -- ** StackCreateComplete
    newStackCreateComplete,

    -- ** TypeRegistrationComplete
    newTypeRegistrationComplete,

    -- ** StackImportComplete
    newStackImportComplete,

    -- * Operations
    -- $operations

    -- ** RegisterPublisher
    RegisterPublisher (RegisterPublisher'),
    newRegisterPublisher,
    RegisterPublisherResponse (RegisterPublisherResponse'),
    newRegisterPublisherResponse,

    -- ** ImportStacksToStackSet
    ImportStacksToStackSet (ImportStacksToStackSet'),
    newImportStacksToStackSet,
    ImportStacksToStackSetResponse (ImportStacksToStackSetResponse'),
    newImportStacksToStackSetResponse,

    -- ** DescribeStackResourceDrifts
    DescribeStackResourceDrifts (DescribeStackResourceDrifts'),
    newDescribeStackResourceDrifts,
    DescribeStackResourceDriftsResponse (DescribeStackResourceDriftsResponse'),
    newDescribeStackResourceDriftsResponse,

    -- ** DescribeStackEvents (Paginated)
    DescribeStackEvents (DescribeStackEvents'),
    newDescribeStackEvents,
    DescribeStackEventsResponse (DescribeStackEventsResponse'),
    newDescribeStackEventsResponse,

    -- ** ListImports (Paginated)
    ListImports (ListImports'),
    newListImports,
    ListImportsResponse (ListImportsResponse'),
    newListImportsResponse,

    -- ** DescribeChangeSet (Paginated)
    DescribeChangeSet (DescribeChangeSet'),
    newDescribeChangeSet,
    DescribeChangeSetResponse (DescribeChangeSetResponse'),
    newDescribeChangeSetResponse,

    -- ** StopStackSetOperation
    StopStackSetOperation (StopStackSetOperation'),
    newStopStackSetOperation,
    StopStackSetOperationResponse (StopStackSetOperationResponse'),
    newStopStackSetOperationResponse,

    -- ** DescribeStackResource
    DescribeStackResource (DescribeStackResource'),
    newDescribeStackResource,
    DescribeStackResourceResponse (DescribeStackResourceResponse'),
    newDescribeStackResourceResponse,

    -- ** TestType
    TestType (TestType'),
    newTestType,
    TestTypeResponse (TestTypeResponse'),
    newTestTypeResponse,

    -- ** DetectStackResourceDrift
    DetectStackResourceDrift (DetectStackResourceDrift'),
    newDetectStackResourceDrift,
    DetectStackResourceDriftResponse (DetectStackResourceDriftResponse'),
    newDetectStackResourceDriftResponse,

    -- ** SetTypeDefaultVersion
    SetTypeDefaultVersion (SetTypeDefaultVersion'),
    newSetTypeDefaultVersion,
    SetTypeDefaultVersionResponse (SetTypeDefaultVersionResponse'),
    newSetTypeDefaultVersionResponse,

    -- ** ExecuteChangeSet
    ExecuteChangeSet (ExecuteChangeSet'),
    newExecuteChangeSet,
    ExecuteChangeSetResponse (ExecuteChangeSetResponse'),
    newExecuteChangeSetResponse,

    -- ** RollbackStack
    RollbackStack (RollbackStack'),
    newRollbackStack,
    RollbackStackResponse (RollbackStackResponse'),
    newRollbackStackResponse,

    -- ** GetStackPolicy
    GetStackPolicy (GetStackPolicy'),
    newGetStackPolicy,
    GetStackPolicyResponse (GetStackPolicyResponse'),
    newGetStackPolicyResponse,

    -- ** CreateStackInstances
    CreateStackInstances (CreateStackInstances'),
    newCreateStackInstances,
    CreateStackInstancesResponse (CreateStackInstancesResponse'),
    newCreateStackInstancesResponse,

    -- ** DescribeStacks (Paginated)
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** RecordHandlerProgress
    RecordHandlerProgress (RecordHandlerProgress'),
    newRecordHandlerProgress,
    RecordHandlerProgressResponse (RecordHandlerProgressResponse'),
    newRecordHandlerProgressResponse,

    -- ** ListStackSetOperations (Paginated)
    ListStackSetOperations (ListStackSetOperations'),
    newListStackSetOperations,
    ListStackSetOperationsResponse (ListStackSetOperationsResponse'),
    newListStackSetOperationsResponse,

    -- ** UpdateStackSet
    UpdateStackSet (UpdateStackSet'),
    newUpdateStackSet,
    UpdateStackSetResponse (UpdateStackSetResponse'),
    newUpdateStackSetResponse,

    -- ** EstimateTemplateCost
    EstimateTemplateCost (EstimateTemplateCost'),
    newEstimateTemplateCost,
    EstimateTemplateCostResponse (EstimateTemplateCostResponse'),
    newEstimateTemplateCostResponse,

    -- ** DeleteStackSet
    DeleteStackSet (DeleteStackSet'),
    newDeleteStackSet,
    DeleteStackSetResponse (DeleteStackSetResponse'),
    newDeleteStackSetResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** GetTemplateSummary
    GetTemplateSummary (GetTemplateSummary'),
    newGetTemplateSummary,
    GetTemplateSummaryResponse (GetTemplateSummaryResponse'),
    newGetTemplateSummaryResponse,

    -- ** SetTypeConfiguration
    SetTypeConfiguration (SetTypeConfiguration'),
    newSetTypeConfiguration,
    SetTypeConfigurationResponse (SetTypeConfigurationResponse'),
    newSetTypeConfigurationResponse,

    -- ** DescribeStackInstance
    DescribeStackInstance (DescribeStackInstance'),
    newDescribeStackInstance,
    DescribeStackInstanceResponse (DescribeStackInstanceResponse'),
    newDescribeStackInstanceResponse,

    -- ** DeleteStack
    DeleteStack (DeleteStack'),
    newDeleteStack,
    DeleteStackResponse (DeleteStackResponse'),
    newDeleteStackResponse,

    -- ** UpdateStack
    UpdateStack (UpdateStack'),
    newUpdateStack,
    UpdateStackResponse (UpdateStackResponse'),
    newUpdateStackResponse,

    -- ** ListStacks (Paginated)
    ListStacks (ListStacks'),
    newListStacks,
    ListStacksResponse (ListStacksResponse'),
    newListStacksResponse,

    -- ** SignalResource
    SignalResource (SignalResource'),
    newSignalResource,
    SignalResourceResponse (SignalResourceResponse'),
    newSignalResourceResponse,

    -- ** DetectStackDrift
    DetectStackDrift (DetectStackDrift'),
    newDetectStackDrift,
    DetectStackDriftResponse (DetectStackDriftResponse'),
    newDetectStackDriftResponse,

    -- ** SetStackPolicy
    SetStackPolicy (SetStackPolicy'),
    newSetStackPolicy,
    SetStackPolicyResponse (SetStackPolicyResponse'),
    newSetStackPolicyResponse,

    -- ** DescribeStackSetOperation
    DescribeStackSetOperation (DescribeStackSetOperation'),
    newDescribeStackSetOperation,
    DescribeStackSetOperationResponse (DescribeStackSetOperationResponse'),
    newDescribeStackSetOperationResponse,

    -- ** DescribeTypeRegistration
    DescribeTypeRegistration (DescribeTypeRegistration'),
    newDescribeTypeRegistration,
    DescribeTypeRegistrationResponse (DescribeTypeRegistrationResponse'),
    newDescribeTypeRegistrationResponse,

    -- ** ListTypes (Paginated)
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** DescribeStackSet
    DescribeStackSet (DescribeStackSet'),
    newDescribeStackSet,
    DescribeStackSetResponse (DescribeStackSetResponse'),
    newDescribeStackSetResponse,

    -- ** ListStackSetOperationResults (Paginated)
    ListStackSetOperationResults (ListStackSetOperationResults'),
    newListStackSetOperationResults,
    ListStackSetOperationResultsResponse (ListStackSetOperationResultsResponse'),
    newListStackSetOperationResultsResponse,

    -- ** RegisterType
    RegisterType (RegisterType'),
    newRegisterType,
    RegisterTypeResponse (RegisterTypeResponse'),
    newRegisterTypeResponse,

    -- ** PublishType
    PublishType (PublishType'),
    newPublishType,
    PublishTypeResponse (PublishTypeResponse'),
    newPublishTypeResponse,

    -- ** CancelUpdateStack
    CancelUpdateStack (CancelUpdateStack'),
    newCancelUpdateStack,
    CancelUpdateStackResponse (CancelUpdateStackResponse'),
    newCancelUpdateStackResponse,

    -- ** ActivateType
    ActivateType (ActivateType'),
    newActivateType,
    ActivateTypeResponse (ActivateTypeResponse'),
    newActivateTypeResponse,

    -- ** ValidateTemplate
    ValidateTemplate (ValidateTemplate'),
    newValidateTemplate,
    ValidateTemplateResponse (ValidateTemplateResponse'),
    newValidateTemplateResponse,

    -- ** DetectStackSetDrift
    DetectStackSetDrift (DetectStackSetDrift'),
    newDetectStackSetDrift,
    DetectStackSetDriftResponse (DetectStackSetDriftResponse'),
    newDetectStackSetDriftResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** ListStackInstances (Paginated)
    ListStackInstances (ListStackInstances'),
    newListStackInstances,
    ListStackInstancesResponse (ListStackInstancesResponse'),
    newListStackInstancesResponse,

    -- ** ContinueUpdateRollback
    ContinueUpdateRollback (ContinueUpdateRollback'),
    newContinueUpdateRollback,
    ContinueUpdateRollbackResponse (ContinueUpdateRollbackResponse'),
    newContinueUpdateRollbackResponse,

    -- ** UpdateTerminationProtection
    UpdateTerminationProtection (UpdateTerminationProtection'),
    newUpdateTerminationProtection,
    UpdateTerminationProtectionResponse (UpdateTerminationProtectionResponse'),
    newUpdateTerminationProtectionResponse,

    -- ** ListTypeVersions
    ListTypeVersions (ListTypeVersions'),
    newListTypeVersions,
    ListTypeVersionsResponse (ListTypeVersionsResponse'),
    newListTypeVersionsResponse,

    -- ** DescribePublisher
    DescribePublisher (DescribePublisher'),
    newDescribePublisher,
    DescribePublisherResponse (DescribePublisherResponse'),
    newDescribePublisherResponse,

    -- ** DeactivateType
    DeactivateType (DeactivateType'),
    newDeactivateType,
    DeactivateTypeResponse (DeactivateTypeResponse'),
    newDeactivateTypeResponse,

    -- ** ListTypeRegistrations
    ListTypeRegistrations (ListTypeRegistrations'),
    newListTypeRegistrations,
    ListTypeRegistrationsResponse (ListTypeRegistrationsResponse'),
    newListTypeRegistrationsResponse,

    -- ** CreateStackSet
    CreateStackSet (CreateStackSet'),
    newCreateStackSet,
    CreateStackSetResponse (CreateStackSetResponse'),
    newCreateStackSetResponse,

    -- ** CreateChangeSet
    CreateChangeSet (CreateChangeSet'),
    newCreateChangeSet,
    CreateChangeSetResponse (CreateChangeSetResponse'),
    newCreateChangeSetResponse,

    -- ** DeregisterType
    DeregisterType (DeregisterType'),
    newDeregisterType,
    DeregisterTypeResponse (DeregisterTypeResponse'),
    newDeregisterTypeResponse,

    -- ** DescribeType
    DescribeType (DescribeType'),
    newDescribeType,
    DescribeTypeResponse (DescribeTypeResponse'),
    newDescribeTypeResponse,

    -- ** ListChangeSets (Paginated)
    ListChangeSets (ListChangeSets'),
    newListChangeSets,
    ListChangeSetsResponse (ListChangeSetsResponse'),
    newListChangeSetsResponse,

    -- ** DeleteChangeSet
    DeleteChangeSet (DeleteChangeSet'),
    newDeleteChangeSet,
    DeleteChangeSetResponse (DeleteChangeSetResponse'),
    newDeleteChangeSetResponse,

    -- ** DeleteStackInstances
    DeleteStackInstances (DeleteStackInstances'),
    newDeleteStackInstances,
    DeleteStackInstancesResponse (DeleteStackInstancesResponse'),
    newDeleteStackInstancesResponse,

    -- ** ListStackResources (Paginated)
    ListStackResources (ListStackResources'),
    newListStackResources,
    ListStackResourcesResponse (ListStackResourcesResponse'),
    newListStackResourcesResponse,

    -- ** UpdateStackInstances
    UpdateStackInstances (UpdateStackInstances'),
    newUpdateStackInstances,
    UpdateStackInstancesResponse (UpdateStackInstancesResponse'),
    newUpdateStackInstancesResponse,

    -- ** DescribeStackDriftDetectionStatus
    DescribeStackDriftDetectionStatus (DescribeStackDriftDetectionStatus'),
    newDescribeStackDriftDetectionStatus,
    DescribeStackDriftDetectionStatusResponse (DescribeStackDriftDetectionStatusResponse'),
    newDescribeStackDriftDetectionStatusResponse,

    -- ** ListStackSets (Paginated)
    ListStackSets (ListStackSets'),
    newListStackSets,
    ListStackSetsResponse (ListStackSetsResponse'),
    newListStackSetsResponse,

    -- ** ListExports (Paginated)
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** DescribeStackResources
    DescribeStackResources (DescribeStackResources'),
    newDescribeStackResources,
    DescribeStackResourcesResponse (DescribeStackResourcesResponse'),
    newDescribeStackResourcesResponse,

    -- ** BatchDescribeTypeConfigurations
    BatchDescribeTypeConfigurations (BatchDescribeTypeConfigurations'),
    newBatchDescribeTypeConfigurations,
    BatchDescribeTypeConfigurationsResponse (BatchDescribeTypeConfigurationsResponse'),
    newBatchDescribeTypeConfigurationsResponse,

    -- * Types

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

    -- ** IdentityProvider
    IdentityProvider (..),

    -- ** OnFailure
    OnFailure (..),

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

    -- ** ModuleInfo
    ModuleInfo (ModuleInfo'),
    newModuleInfo,

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

import Network.AWS.CloudFormation.ActivateType
import Network.AWS.CloudFormation.BatchDescribeTypeConfigurations
import Network.AWS.CloudFormation.CancelUpdateStack
import Network.AWS.CloudFormation.ContinueUpdateRollback
import Network.AWS.CloudFormation.CreateChangeSet
import Network.AWS.CloudFormation.CreateStack
import Network.AWS.CloudFormation.CreateStackInstances
import Network.AWS.CloudFormation.CreateStackSet
import Network.AWS.CloudFormation.DeactivateType
import Network.AWS.CloudFormation.DeleteChangeSet
import Network.AWS.CloudFormation.DeleteStack
import Network.AWS.CloudFormation.DeleteStackInstances
import Network.AWS.CloudFormation.DeleteStackSet
import Network.AWS.CloudFormation.DeregisterType
import Network.AWS.CloudFormation.DescribeAccountLimits
import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribePublisher
import Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus
import Network.AWS.CloudFormation.DescribeStackEvents
import Network.AWS.CloudFormation.DescribeStackInstance
import Network.AWS.CloudFormation.DescribeStackResource
import Network.AWS.CloudFormation.DescribeStackResourceDrifts
import Network.AWS.CloudFormation.DescribeStackResources
import Network.AWS.CloudFormation.DescribeStackSet
import Network.AWS.CloudFormation.DescribeStackSetOperation
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeType
import Network.AWS.CloudFormation.DescribeTypeRegistration
import Network.AWS.CloudFormation.DetectStackDrift
import Network.AWS.CloudFormation.DetectStackResourceDrift
import Network.AWS.CloudFormation.DetectStackSetDrift
import Network.AWS.CloudFormation.EstimateTemplateCost
import Network.AWS.CloudFormation.ExecuteChangeSet
import Network.AWS.CloudFormation.GetStackPolicy
import Network.AWS.CloudFormation.GetTemplate
import Network.AWS.CloudFormation.GetTemplateSummary
import Network.AWS.CloudFormation.ImportStacksToStackSet
import Network.AWS.CloudFormation.Lens
import Network.AWS.CloudFormation.ListChangeSets
import Network.AWS.CloudFormation.ListExports
import Network.AWS.CloudFormation.ListImports
import Network.AWS.CloudFormation.ListStackInstances
import Network.AWS.CloudFormation.ListStackResources
import Network.AWS.CloudFormation.ListStackSetOperationResults
import Network.AWS.CloudFormation.ListStackSetOperations
import Network.AWS.CloudFormation.ListStackSets
import Network.AWS.CloudFormation.ListStacks
import Network.AWS.CloudFormation.ListTypeRegistrations
import Network.AWS.CloudFormation.ListTypeVersions
import Network.AWS.CloudFormation.ListTypes
import Network.AWS.CloudFormation.PublishType
import Network.AWS.CloudFormation.RecordHandlerProgress
import Network.AWS.CloudFormation.RegisterPublisher
import Network.AWS.CloudFormation.RegisterType
import Network.AWS.CloudFormation.RollbackStack
import Network.AWS.CloudFormation.SetStackPolicy
import Network.AWS.CloudFormation.SetTypeConfiguration
import Network.AWS.CloudFormation.SetTypeDefaultVersion
import Network.AWS.CloudFormation.SignalResource
import Network.AWS.CloudFormation.StopStackSetOperation
import Network.AWS.CloudFormation.TestType
import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.UpdateStack
import Network.AWS.CloudFormation.UpdateStackInstances
import Network.AWS.CloudFormation.UpdateStackSet
import Network.AWS.CloudFormation.UpdateTerminationProtection
import Network.AWS.CloudFormation.ValidateTemplate
import Network.AWS.CloudFormation.Waiters

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
