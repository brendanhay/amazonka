{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CloudFormation
--
-- AWS CloudFormation allows you to create and manage AWS infrastructure
-- deployments predictably and repeatedly. You can use AWS CloudFormation
-- to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon
-- Elastic Block Store, Amazon Simple Notification Service, Elastic Load
-- Balancing, and Auto Scaling to build highly-reliable, highly scalable,
-- cost-effective applications without creating or configuring the
-- underlying AWS infrastructure.
--
-- With AWS CloudFormation, you declare all of your resources and
-- dependencies in a template file. The template defines a collection of
-- resources as a single unit called a stack. AWS CloudFormation creates
-- and deletes all member resources of the stack together and manages all
-- dependencies between the resources for you.
--
-- For more information about AWS CloudFormation, see the
-- <http://aws.amazon.com/cloudformation/ AWS CloudFormation Product Page>.
--
-- Amazon CloudFormation makes use of other AWS products. If you need
-- additional technical information about a specific AWS product, you can
-- find the product\'s technical documentation at
-- <https://docs.aws.amazon.com/ docs.aws.amazon.com>.
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

    -- ** InvalidChangeSetStatusException
    _InvalidChangeSetStatusException,

    -- ** NameAlreadyExistsException
    _NameAlreadyExistsException,

    -- ** StackSetNotEmptyException
    _StackSetNotEmptyException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** OperationStatusCheckFailedException
    _OperationStatusCheckFailedException,

    -- ** ChangeSetNotFoundException
    _ChangeSetNotFoundException,

    -- ** StackSetNotFoundException
    _StackSetNotFoundException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** CreatedButModifiedException
    _CreatedButModifiedException,

    -- ** TokenAlreadyExistsException
    _TokenAlreadyExistsException,

    -- ** TypeNotFoundException
    _TypeNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** CFNRegistryException
    _CFNRegistryException,

    -- ** OperationIdAlreadyExistsException
    _OperationIdAlreadyExistsException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** StackInstanceNotFoundException
    _StackInstanceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** ChangeSetCreateComplete
    newChangeSetCreateComplete,

    -- ** StackRollbackComplete
    newStackRollbackComplete,

    -- ** StackDeleteComplete
    newStackDeleteComplete,

    -- ** StackExists
    newStackExists,

    -- ** StackUpdateComplete
    newStackUpdateComplete,

    -- ** StackCreateComplete
    newStackCreateComplete,

    -- ** TypeRegistrationComplete
    newTypeRegistrationComplete,

    -- ** StackImportComplete
    newStackImportComplete,

    -- * Operations
    -- $operations

    -- ** DescribeStackResourceDrifts
    DescribeStackResourceDrifts (DescribeStackResourceDrifts'),
    newDescribeStackResourceDrifts,
    DescribeStackResourceDriftsResponse (DescribeStackResourceDriftsResponse'),
    newDescribeStackResourceDriftsResponse,

    -- ** ListImports (Paginated)
    ListImports (ListImports'),
    newListImports,
    ListImportsResponse (ListImportsResponse'),
    newListImportsResponse,

    -- ** DescribeStackEvents (Paginated)
    DescribeStackEvents (DescribeStackEvents'),
    newDescribeStackEvents,
    DescribeStackEventsResponse (DescribeStackEventsResponse'),
    newDescribeStackEventsResponse,

    -- ** StopStackSetOperation
    StopStackSetOperation (StopStackSetOperation'),
    newStopStackSetOperation,
    StopStackSetOperationResponse (StopStackSetOperationResponse'),
    newStopStackSetOperationResponse,

    -- ** DescribeChangeSet (Paginated)
    DescribeChangeSet (DescribeChangeSet'),
    newDescribeChangeSet,
    DescribeChangeSetResponse (DescribeChangeSetResponse'),
    newDescribeChangeSetResponse,

    -- ** DescribeStackResource
    DescribeStackResource (DescribeStackResource'),
    newDescribeStackResource,
    DescribeStackResourceResponse (DescribeStackResourceResponse'),
    newDescribeStackResourceResponse,

    -- ** DetectStackResourceDrift
    DetectStackResourceDrift (DetectStackResourceDrift'),
    newDetectStackResourceDrift,
    DetectStackResourceDriftResponse (DetectStackResourceDriftResponse'),
    newDetectStackResourceDriftResponse,

    -- ** ExecuteChangeSet
    ExecuteChangeSet (ExecuteChangeSet'),
    newExecuteChangeSet,
    ExecuteChangeSetResponse (ExecuteChangeSetResponse'),
    newExecuteChangeSetResponse,

    -- ** SetTypeDefaultVersion
    SetTypeDefaultVersion (SetTypeDefaultVersion'),
    newSetTypeDefaultVersion,
    SetTypeDefaultVersionResponse (SetTypeDefaultVersionResponse'),
    newSetTypeDefaultVersionResponse,

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

    -- ** CreateStackInstances
    CreateStackInstances (CreateStackInstances'),
    newCreateStackInstances,
    CreateStackInstancesResponse (CreateStackInstancesResponse'),
    newCreateStackInstancesResponse,

    -- ** GetStackPolicy
    GetStackPolicy (GetStackPolicy'),
    newGetStackPolicy,
    GetStackPolicyResponse (GetStackPolicyResponse'),
    newGetStackPolicyResponse,

    -- ** DescribeStacks (Paginated)
    DescribeStacks (DescribeStacks'),
    newDescribeStacks,
    DescribeStacksResponse (DescribeStacksResponse'),
    newDescribeStacksResponse,

    -- ** DeleteStackSet
    DeleteStackSet (DeleteStackSet'),
    newDeleteStackSet,
    DeleteStackSetResponse (DeleteStackSetResponse'),
    newDeleteStackSetResponse,

    -- ** EstimateTemplateCost
    EstimateTemplateCost (EstimateTemplateCost'),
    newEstimateTemplateCost,
    EstimateTemplateCostResponse (EstimateTemplateCostResponse'),
    newEstimateTemplateCostResponse,

    -- ** UpdateStackSet
    UpdateStackSet (UpdateStackSet'),
    newUpdateStackSet,
    UpdateStackSetResponse (UpdateStackSetResponse'),
    newUpdateStackSetResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** CreateStack
    CreateStack (CreateStack'),
    newCreateStack,
    CreateStackResponse (CreateStackResponse'),
    newCreateStackResponse,

    -- ** DescribeStackInstance
    DescribeStackInstance (DescribeStackInstance'),
    newDescribeStackInstance,
    DescribeStackInstanceResponse (DescribeStackInstanceResponse'),
    newDescribeStackInstanceResponse,

    -- ** GetTemplateSummary
    GetTemplateSummary (GetTemplateSummary'),
    newGetTemplateSummary,
    GetTemplateSummaryResponse (GetTemplateSummaryResponse'),
    newGetTemplateSummaryResponse,

    -- ** ListStacks (Paginated)
    ListStacks (ListStacks'),
    newListStacks,
    ListStacksResponse (ListStacksResponse'),
    newListStacksResponse,

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

    -- ** DetectStackDrift
    DetectStackDrift (DetectStackDrift'),
    newDetectStackDrift,
    DetectStackDriftResponse (DetectStackDriftResponse'),
    newDetectStackDriftResponse,

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

    -- ** SetStackPolicy
    SetStackPolicy (SetStackPolicy'),
    newSetStackPolicy,
    SetStackPolicyResponse (SetStackPolicyResponse'),
    newSetStackPolicyResponse,

    -- ** SignalResource
    SignalResource (SignalResource'),
    newSignalResource,
    SignalResourceResponse (SignalResourceResponse'),
    newSignalResourceResponse,

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

    -- ** CancelUpdateStack
    CancelUpdateStack (CancelUpdateStack'),
    newCancelUpdateStack,
    CancelUpdateStackResponse (CancelUpdateStackResponse'),
    newCancelUpdateStackResponse,

    -- ** ListTypes
    ListTypes (ListTypes'),
    newListTypes,
    ListTypesResponse (ListTypesResponse'),
    newListTypesResponse,

    -- ** RegisterType
    RegisterType (RegisterType'),
    newRegisterType,
    RegisterTypeResponse (RegisterTypeResponse'),
    newRegisterTypeResponse,

    -- ** ListStackInstances (Paginated)
    ListStackInstances (ListStackInstances'),
    newListStackInstances,
    ListStackInstancesResponse (ListStackInstancesResponse'),
    newListStackInstancesResponse,

    -- ** DetectStackSetDrift
    DetectStackSetDrift (DetectStackSetDrift'),
    newDetectStackSetDrift,
    DetectStackSetDriftResponse (DetectStackSetDriftResponse'),
    newDetectStackSetDriftResponse,

    -- ** ContinueUpdateRollback
    ContinueUpdateRollback (ContinueUpdateRollback'),
    newContinueUpdateRollback,
    ContinueUpdateRollbackResponse (ContinueUpdateRollbackResponse'),
    newContinueUpdateRollbackResponse,

    -- ** ValidateTemplate
    ValidateTemplate (ValidateTemplate'),
    newValidateTemplate,
    ValidateTemplateResponse (ValidateTemplateResponse'),
    newValidateTemplateResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

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

    -- ** ListTypeRegistrations
    ListTypeRegistrations (ListTypeRegistrations'),
    newListTypeRegistrations,
    ListTypeRegistrationsResponse (ListTypeRegistrationsResponse'),
    newListTypeRegistrationsResponse,

    -- ** DeregisterType
    DeregisterType (DeregisterType'),
    newDeregisterType,
    DeregisterTypeResponse (DeregisterTypeResponse'),
    newDeregisterTypeResponse,

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

    -- ** ListExports (Paginated)
    ListExports (ListExports'),
    newListExports,
    ListExportsResponse (ListExportsResponse'),
    newListExportsResponse,

    -- ** DescribeType
    DescribeType (DescribeType'),
    newDescribeType,
    DescribeTypeResponse (DescribeTypeResponse'),
    newDescribeTypeResponse,

    -- ** ListStackResources (Paginated)
    ListStackResources (ListStackResources'),
    newListStackResources,
    ListStackResourcesResponse (ListStackResourcesResponse'),
    newListStackResourcesResponse,

    -- ** DeleteStackInstances
    DeleteStackInstances (DeleteStackInstances'),
    newDeleteStackInstances,
    DeleteStackInstancesResponse (DeleteStackInstancesResponse'),
    newDeleteStackInstancesResponse,

    -- ** ListStackSets (Paginated)
    ListStackSets (ListStackSets'),
    newListStackSets,
    ListStackSetsResponse (ListStackSetsResponse'),
    newListStackSetsResponse,

    -- ** DescribeStackDriftDetectionStatus
    DescribeStackDriftDetectionStatus (DescribeStackDriftDetectionStatus'),
    newDescribeStackDriftDetectionStatus,
    DescribeStackDriftDetectionStatusResponse (DescribeStackDriftDetectionStatusResponse'),
    newDescribeStackDriftDetectionStatusResponse,

    -- ** UpdateStackInstances
    UpdateStackInstances (UpdateStackInstances'),
    newUpdateStackInstances,
    UpdateStackInstancesResponse (UpdateStackInstancesResponse'),
    newUpdateStackInstancesResponse,

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

    -- ** DescribeStackResources
    DescribeStackResources (DescribeStackResources'),
    newDescribeStackResources,
    DescribeStackResourcesResponse (DescribeStackResourcesResponse'),
    newDescribeStackResourcesResponse,

    -- * Types

    -- ** AccountGateStatus
    AccountGateStatus (..),

    -- ** CallAs
    CallAs (..),

    -- ** Capability
    Capability (..),

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

    -- ** OnFailure
    OnFailure (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** PermissionModels
    PermissionModels (..),

    -- ** ProvisioningType
    ProvisioningType (..),

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

    -- ** TypeSummary
    TypeSummary (TypeSummary'),
    newTypeSummary,

    -- ** TypeVersionSummary
    TypeVersionSummary (TypeVersionSummary'),
    newTypeVersionSummary,
  )
where

import Network.AWS.CloudFormation.CancelUpdateStack
import Network.AWS.CloudFormation.ContinueUpdateRollback
import Network.AWS.CloudFormation.CreateChangeSet
import Network.AWS.CloudFormation.CreateStack
import Network.AWS.CloudFormation.CreateStackInstances
import Network.AWS.CloudFormation.CreateStackSet
import Network.AWS.CloudFormation.DeleteChangeSet
import Network.AWS.CloudFormation.DeleteStack
import Network.AWS.CloudFormation.DeleteStackInstances
import Network.AWS.CloudFormation.DeleteStackSet
import Network.AWS.CloudFormation.DeregisterType
import Network.AWS.CloudFormation.DescribeAccountLimits
import Network.AWS.CloudFormation.DescribeChangeSet
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
import Network.AWS.CloudFormation.RecordHandlerProgress
import Network.AWS.CloudFormation.RegisterType
import Network.AWS.CloudFormation.SetStackPolicy
import Network.AWS.CloudFormation.SetTypeDefaultVersion
import Network.AWS.CloudFormation.SignalResource
import Network.AWS.CloudFormation.StopStackSetOperation
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
