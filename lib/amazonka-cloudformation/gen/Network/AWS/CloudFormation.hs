{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudFormation__
--
-- AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.
--
-- With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.
--
-- For more information about AWS CloudFormation, see the <http://aws.amazon.com/cloudformation/ AWS CloudFormation Product Page> .
--
-- Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <https://docs.aws.amazon.com/ docs.aws.amazon.com> .
module Network.AWS.CloudFormation
  ( -- * Service Configuration
    cloudFormation,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** StackImportComplete
    stackImportComplete,

    -- ** StackCreateComplete
    stackCreateComplete,

    -- ** StackRollbackComplete
    stackRollbackComplete,

    -- ** TypeRegistrationComplete
    typeRegistrationComplete,

    -- ** StackUpdateComplete
    stackUpdateComplete,

    -- ** StackExists
    stackExists,

    -- ** StackDeleteComplete
    stackDeleteComplete,

    -- ** ChangeSetCreateComplete
    changeSetCreateComplete,

    -- * Operations
    -- $operations

    -- ** DescribeStackSetOperation
    module Network.AWS.CloudFormation.DescribeStackSetOperation,

    -- ** DeleteStack
    module Network.AWS.CloudFormation.DeleteStack,

    -- ** UpdateStack
    module Network.AWS.CloudFormation.UpdateStack,

    -- ** GetTemplateSummary
    module Network.AWS.CloudFormation.GetTemplateSummary,

    -- ** ListChangeSets (Paginated)
    module Network.AWS.CloudFormation.ListChangeSets,

    -- ** ListStackResources (Paginated)
    module Network.AWS.CloudFormation.ListStackResources,

    -- ** UpdateStackInstances
    module Network.AWS.CloudFormation.UpdateStackInstances,

    -- ** DeleteStackInstances
    module Network.AWS.CloudFormation.DeleteStackInstances,

    -- ** DescribeType
    module Network.AWS.CloudFormation.DescribeType,

    -- ** CreateStackInstances
    module Network.AWS.CloudFormation.CreateStackInstances,

    -- ** ListTypeRegistrations
    module Network.AWS.CloudFormation.ListTypeRegistrations,

    -- ** GetStackPolicy
    module Network.AWS.CloudFormation.GetStackPolicy,

    -- ** DescribeStacks (Paginated)
    module Network.AWS.CloudFormation.DescribeStacks,

    -- ** CreateChangeSet
    module Network.AWS.CloudFormation.CreateChangeSet,

    -- ** ListStackSetOperations (Paginated)
    module Network.AWS.CloudFormation.ListStackSetOperations,

    -- ** ExecuteChangeSet
    module Network.AWS.CloudFormation.ExecuteChangeSet,

    -- ** ListStackInstances (Paginated)
    module Network.AWS.CloudFormation.ListStackInstances,

    -- ** ContinueUpdateRollback
    module Network.AWS.CloudFormation.ContinueUpdateRollback,

    -- ** ValidateTemplate
    module Network.AWS.CloudFormation.ValidateTemplate,

    -- ** CancelUpdateStack
    module Network.AWS.CloudFormation.CancelUpdateStack,

    -- ** ListTypes
    module Network.AWS.CloudFormation.ListTypes,

    -- ** DescribeTypeRegistration
    module Network.AWS.CloudFormation.DescribeTypeRegistration,

    -- ** DetectStackDrift
    module Network.AWS.CloudFormation.DetectStackDrift,

    -- ** DescribeStackEvents (Paginated)
    module Network.AWS.CloudFormation.DescribeStackEvents,

    -- ** SignalResource
    module Network.AWS.CloudFormation.SignalResource,

    -- ** SetStackPolicy
    module Network.AWS.CloudFormation.SetStackPolicy,

    -- ** ListImports (Paginated)
    module Network.AWS.CloudFormation.ListImports,

    -- ** DescribeStackResourceDrifts
    module Network.AWS.CloudFormation.DescribeStackResourceDrifts,

    -- ** ListStacks (Paginated)
    module Network.AWS.CloudFormation.ListStacks,

    -- ** DescribeAccountLimits (Paginated)
    module Network.AWS.CloudFormation.DescribeAccountLimits,

    -- ** DescribeStackResources
    module Network.AWS.CloudFormation.DescribeStackResources,

    -- ** DescribeStackInstance
    module Network.AWS.CloudFormation.DescribeStackInstance,

    -- ** CreateStack
    module Network.AWS.CloudFormation.CreateStack,

    -- ** UpdateStackSet
    module Network.AWS.CloudFormation.UpdateStackSet,

    -- ** DeleteStackSet
    module Network.AWS.CloudFormation.DeleteStackSet,

    -- ** EstimateTemplateCost
    module Network.AWS.CloudFormation.EstimateTemplateCost,

    -- ** DeleteChangeSet
    module Network.AWS.CloudFormation.DeleteChangeSet,

    -- ** ListStackSets (Paginated)
    module Network.AWS.CloudFormation.ListStackSets,

    -- ** ListExports (Paginated)
    module Network.AWS.CloudFormation.ListExports,

    -- ** DescribeStackDriftDetectionStatus
    module Network.AWS.CloudFormation.DescribeStackDriftDetectionStatus,

    -- ** CreateStackSet
    module Network.AWS.CloudFormation.CreateStackSet,

    -- ** DeregisterType
    module Network.AWS.CloudFormation.DeregisterType,

    -- ** RecordHandlerProgress
    module Network.AWS.CloudFormation.RecordHandlerProgress,

    -- ** ListTypeVersions
    module Network.AWS.CloudFormation.ListTypeVersions,

    -- ** SetTypeDefaultVersion
    module Network.AWS.CloudFormation.SetTypeDefaultVersion,

    -- ** UpdateTerminationProtection
    module Network.AWS.CloudFormation.UpdateTerminationProtection,

    -- ** GetTemplate
    module Network.AWS.CloudFormation.GetTemplate,

    -- ** DetectStackSetDrift
    module Network.AWS.CloudFormation.DetectStackSetDrift,

    -- ** DetectStackResourceDrift
    module Network.AWS.CloudFormation.DetectStackResourceDrift,

    -- ** DescribeChangeSet (Paginated)
    module Network.AWS.CloudFormation.DescribeChangeSet,

    -- ** DescribeStackSet
    module Network.AWS.CloudFormation.DescribeStackSet,

    -- ** ListStackSetOperationResults (Paginated)
    module Network.AWS.CloudFormation.ListStackSetOperationResults,

    -- ** RegisterType
    module Network.AWS.CloudFormation.RegisterType,

    -- ** StopStackSetOperation
    module Network.AWS.CloudFormation.StopStackSetOperation,

    -- ** DescribeStackResource
    module Network.AWS.CloudFormation.DescribeStackResource,

    -- * Types

    -- ** AccountGateStatus
    AccountGateStatus (..),

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
    AccountGateResult,
    accountGateResult,
    agrStatus,
    agrStatusReason,

    -- ** AccountLimit
    AccountLimit,
    accountLimit,
    alValue,
    alName,

    -- ** AutoDeployment
    AutoDeployment,
    autoDeployment,
    adEnabled,
    adRetainStacksOnAccountRemoval,

    -- ** Change
    Change,
    change,
    cResourceChange,
    cType,

    -- ** ChangeSetSummary
    ChangeSetSummary,
    changeSetSummary,
    cCreationTime,
    cStatus,
    cParentChangeSetId,
    cChangeSetName,
    cExecutionStatus,
    cChangeSetId,
    cIncludeNestedStacks,
    cRootChangeSetId,
    cStatusReason,
    cStackId,
    cDescription,
    cStackName,

    -- ** DeploymentTargets
    DeploymentTargets,
    deploymentTargets,
    dtAccounts,
    dtOrganizationalUnitIds,

    -- ** Export
    Export,
    export',
    eValue,
    eExportingStackId,
    eName,

    -- ** LoggingConfig
    LoggingConfig,
    loggingConfig,
    lcLogRoleARN,
    lcLogGroupName,

    -- ** ModuleInfo
    ModuleInfo,
    moduleInfo,
    miTypeHierarchy,
    miLogicalIdHierarchy,

    -- ** Output
    Output,
    output,
    oOutputValue,
    oOutputKey,
    oExportName,
    oDescription,

    -- ** Parameter
    Parameter,
    parameter,
    pParameterValue,
    pResolvedValue,
    pParameterKey,
    pUsePreviousValue,

    -- ** ParameterConstraints
    ParameterConstraints,
    parameterConstraints,
    pcAllowedValues,

    -- ** ParameterDeclaration
    ParameterDeclaration,
    parameterDeclaration,
    pdParameterKey,
    pdParameterType,
    pdParameterConstraints,
    pdDefaultValue,
    pdNoEcho,
    pdDescription,

    -- ** PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair,
    physicalResourceIdContextKeyValuePair,
    prickvpKey,
    prickvpValue,

    -- ** PropertyDifference
    PropertyDifference,
    propertyDifference,
    pdPropertyPath,
    pdExpectedValue,
    pdActualValue,
    pdDifferenceType,

    -- ** ResourceChange
    ResourceChange,
    resourceChange,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcChangeSetId,
    rcModuleInfo,
    rcScope,
    rcDetails,
    rcReplacement,

    -- ** ResourceChangeDetail
    ResourceChangeDetail,
    resourceChangeDetail,
    rcdCausingEntity,
    rcdChangeSource,
    rcdEvaluation,
    rcdTarget,

    -- ** ResourceIdentifierSummary
    ResourceIdentifierSummary,
    resourceIdentifierSummary,
    risResourceType,
    risLogicalResourceIds,
    risResourceIdentifiers,

    -- ** ResourceTargetDefinition
    ResourceTargetDefinition,
    resourceTargetDefinition,
    rtdAttribute,
    rtdRequiresRecreation,
    rtdName,

    -- ** ResourceToImport
    ResourceToImport,
    resourceToImport,
    rtiResourceType,
    rtiLogicalResourceId,
    rtiResourceIdentifier,

    -- ** RollbackConfiguration
    RollbackConfiguration,
    rollbackConfiguration,
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,

    -- ** RollbackTrigger
    RollbackTrigger,
    rollbackTrigger,
    rtARN,
    rtType,

    -- ** Stack
    Stack,
    stack,
    staDisableRollback,
    staLastUpdatedTime,
    staRootId,
    staNotificationARNs,
    staStackStatusReason,
    staEnableTerminationProtection,
    staDriftInformation,
    staChangeSetId,
    staDeletionTime,
    staOutputs,
    staParameters,
    staStackId,
    staDescription,
    staCapabilities,
    staRollbackConfiguration,
    staTags,
    staTimeoutInMinutes,
    staParentId,
    staRoleARN,
    staStackName,
    staCreationTime,
    staStackStatus,

    -- ** StackDriftInformation
    StackDriftInformation,
    stackDriftInformation,
    sdiLastCheckTimestamp,
    sdiStackDriftStatus,

    -- ** StackDriftInformationSummary
    StackDriftInformationSummary,
    stackDriftInformationSummary,
    sdisLastCheckTimestamp,
    sdisStackDriftStatus,

    -- ** StackEvent
    StackEvent,
    stackEvent,
    seLogicalResourceId,
    sePhysicalResourceId,
    seResourceType,
    seResourceStatusReason,
    seResourceProperties,
    seResourceStatus,
    seClientRequestToken,
    seStackId,
    seEventId,
    seStackName,
    seTimestamp,

    -- ** StackInstance
    StackInstance,
    stackInstance,
    siStatus,
    siLastDriftCheckTimestamp,
    siAccount,
    siDriftStatus,
    siOrganizationalUnitId,
    siRegion,
    siStatusReason,
    siStackId,
    siStackInstanceStatus,
    siParameterOverrides,
    siStackSetId,

    -- ** StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus,
    stackInstanceComprehensiveStatus,
    sicsDetailedStatus,

    -- ** StackInstanceFilter
    StackInstanceFilter,
    stackInstanceFilter,
    sifValues,
    sifName,

    -- ** StackInstanceSummary
    StackInstanceSummary,
    stackInstanceSummary,
    sisStatus,
    sisLastDriftCheckTimestamp,
    sisAccount,
    sisDriftStatus,
    sisOrganizationalUnitId,
    sisRegion,
    sisStatusReason,
    sisStackId,
    sisStackInstanceStatus,
    sisStackSetId,

    -- ** StackResource
    StackResource,
    stackResource,
    srPhysicalResourceId,
    srResourceStatusReason,
    srDriftInformation,
    srModuleInfo,
    srStackId,
    srDescription,
    srStackName,
    srLogicalResourceId,
    srResourceType,
    srTimestamp,
    srResourceStatus,

    -- ** StackResourceDetail
    StackResourceDetail,
    stackResourceDetail,
    sPhysicalResourceId,
    sResourceStatusReason,
    sDriftInformation,
    sModuleInfo,
    sMetadata,
    sStackId,
    sDescription,
    sStackName,
    sLogicalResourceId,
    sResourceType,
    sLastUpdatedTimestamp,
    sResourceStatus,

    -- ** StackResourceDrift
    StackResourceDrift,
    stackResourceDrift,
    srdActualProperties,
    srdPhysicalResourceId,
    srdPhysicalResourceIdContext,
    srdPropertyDifferences,
    srdModuleInfo,
    srdExpectedProperties,
    srdStackId,
    srdLogicalResourceId,
    srdResourceType,
    srdStackResourceDriftStatus,
    srdTimestamp,

    -- ** StackResourceDriftInformation
    StackResourceDriftInformation,
    stackResourceDriftInformation,
    srdiLastCheckTimestamp,
    srdiStackResourceDriftStatus,

    -- ** StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary,
    stackResourceDriftInformationSummary,
    srdisLastCheckTimestamp,
    srdisStackResourceDriftStatus,

    -- ** StackResourceSummary
    StackResourceSummary,
    stackResourceSummary,
    srsPhysicalResourceId,
    srsResourceStatusReason,
    srsDriftInformation,
    srsModuleInfo,
    srsLogicalResourceId,
    srsResourceType,
    srsLastUpdatedTimestamp,
    srsResourceStatus,

    -- ** StackSet
    StackSet,
    stackSet,
    ssStackSetDriftDetectionDetails,
    ssStatus,
    ssAdministrationRoleARN,
    ssAutoDeployment,
    ssOrganizationalUnitIds,
    ssStackSetARN,
    ssPermissionModel,
    ssParameters,
    ssTemplateBody,
    ssStackSetName,
    ssDescription,
    ssCapabilities,
    ssTags,
    ssStackSetId,
    ssExecutionRoleName,

    -- ** StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails,
    stackSetDriftDetectionDetails,
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddDriftedStackInstancesCount,
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddFailedStackInstancesCount,
    ssdddInSyncStackInstancesCount,

    -- ** StackSetOperation
    StackSetOperation,
    stackSetOperation,
    ssoStackSetDriftDetectionDetails,
    ssoStatus,
    ssoAdministrationRoleARN,
    ssoAction,
    ssoEndTimestamp,
    ssoCreationTimestamp,
    ssoOperationPreferences,
    ssoOperationId,
    ssoRetainStacks,
    ssoDeploymentTargets,
    ssoStackSetId,
    ssoExecutionRoleName,

    -- ** StackSetOperationPreferences
    StackSetOperationPreferences,
    stackSetOperationPreferences,
    ssopRegionOrder,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,

    -- ** StackSetOperationResultSummary
    StackSetOperationResultSummary,
    stackSetOperationResultSummary,
    ssorsStatus,
    ssorsAccount,
    ssorsAccountGateResult,
    ssorsOrganizationalUnitId,
    ssorsRegion,
    ssorsStatusReason,

    -- ** StackSetOperationSummary
    StackSetOperationSummary,
    stackSetOperationSummary,
    ssosStatus,
    ssosAction,
    ssosEndTimestamp,
    ssosCreationTimestamp,
    ssosOperationId,

    -- ** StackSetSummary
    StackSetSummary,
    stackSetSummary,
    sssStatus,
    sssLastDriftCheckTimestamp,
    sssAutoDeployment,
    sssDriftStatus,
    sssPermissionModel,
    sssStackSetName,
    sssDescription,
    sssStackSetId,

    -- ** StackSummary
    StackSummary,
    stackSummary,
    ssLastUpdatedTime,
    ssRootId,
    ssStackStatusReason,
    ssTemplateDescription,
    ssDriftInformation,
    ssDeletionTime,
    ssStackId,
    ssParentId,
    ssStackName,
    ssCreationTime,
    ssStackStatus,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TemplateParameter
    TemplateParameter,
    templateParameter,
    tpParameterKey,
    tpDefaultValue,
    tpNoEcho,
    tpDescription,

    -- ** TypeSummary
    TypeSummary,
    typeSummary,
    tsLastUpdated,
    tsTypeName,
    tsDefaultVersionId,
    tsTypeARN,
    tsType,
    tsDescription,

    -- ** TypeVersionSummary
    TypeVersionSummary,
    typeVersionSummary,
    tvsVersionId,
    tvsTypeName,
    tvsARN,
    tvsTimeCreated,
    tvsType,
    tvsIsDefaultVersion,
    tvsDescription,
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
