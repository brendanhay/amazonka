-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _TypeNotFoundException,
    _CreatedButModifiedException,
    _ChangeSetNotFoundException,
    _OperationInProgressException,
    _InvalidChangeSetStatusException,
    _OperationNotFoundException,
    _OperationIdAlreadyExistsException,
    _InsufficientCapabilitiesException,
    _TokenAlreadyExistsException,
    _StackSetNotFoundException,
    _StackInstanceNotFoundException,
    _OperationStatusCheckFailedException,
    _StackSetNotEmptyException,
    _InvalidOperationException,
    _InvalidStateTransitionException,
    _NameAlreadyExistsException,
    _CFNRegistryException,
    _StaleRequestException,
    _AlreadyExistsException,
    _LimitExceededException,

    -- * ResourceChange
    ResourceChange (..),
    mkResourceChange,
    rcAction,
    rcChangeSetId,
    rcDetails,
    rcLogicalResourceId,
    rcModuleInfo,
    rcPhysicalResourceId,
    rcReplacement,
    rcResourceType,
    rcScope,

    -- * OutputValue
    OutputValue (..),

    -- * AccountLimit
    AccountLimit (..),
    mkAccountLimit,
    alName,
    alValue,

    -- * StackSetOperationResultStatus
    StackSetOperationResultStatus (..),

    -- * StackSetOperationResultSummary
    StackSetOperationResultSummary (..),
    mkStackSetOperationResultSummary,
    ssorsAccount,
    ssorsAccountGateResult,
    ssorsOrganizationalUnitId,
    ssorsRegion,
    ssorsStatus,
    ssorsStatusReason,

    -- * ChangeSetType
    ChangeSetType (..),

    -- * Export
    Export (..),
    mkExport,
    eExportingStackId,
    eName,
    eValue,

    -- * StackSet
    StackSet (..),
    mkStackSet,
    ssAdministrationRoleARN,
    ssAutoDeployment,
    ssCapabilities,
    ssDescription,
    ssExecutionRoleName,
    ssOrganizationalUnitIds,
    ssParameters,
    ssPermissionModel,
    ssStackSetARN,
    ssStackSetDriftDetectionDetails,
    ssStackSetId,
    ssStackSetName,
    ssStatus,
    ssTags,
    ssTemplateBody,

    -- * StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails (..),
    mkStackSetDriftDetectionDetails,
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddDriftedStackInstancesCount,
    ssdddFailedStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddInSyncStackInstancesCount,
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,

    -- * ResourceToImport
    ResourceToImport (..),
    mkResourceToImport,
    rtiResourceType,
    rtiLogicalResourceId,
    rtiResourceIdentifier,

    -- * LogicalResourceId
    LogicalResourceId (..),

    -- * ChangeSetStatusReason
    ChangeSetStatusReason (..),

    -- * StackDriftDetectionStatusReason
    StackDriftDetectionStatusReason (..),

    -- * TypeHierarchy
    TypeHierarchy (..),

    -- * PermissionModels
    PermissionModels (..),

    -- * StackDriftInformation
    StackDriftInformation (..),
    mkStackDriftInformation,
    sdiStackDriftStatus,
    sdiLastCheckTimestamp,

    -- * DifferenceType
    DifferenceType (..),

    -- * StackDriftStatus
    StackDriftStatus (..),

    -- * StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus (..),
    mkStackInstanceComprehensiveStatus,
    sicsDetailedStatus,

    -- * StackInstanceFilter
    StackInstanceFilter (..),
    mkStackInstanceFilter,
    sifName,
    sifValues,

    -- * CausingEntity
    CausingEntity (..),

    -- * TypeName
    TypeName (..),

    -- * RequestToken
    RequestToken (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * StackStatus
    StackStatus (..),

    -- * PhysicalResourceId
    PhysicalResourceId (..),

    -- * RegistrationToken
    RegistrationToken (..),

    -- * StackEvent
    StackEvent (..),
    mkStackEvent,
    seStackId,
    seEventId,
    seStackName,
    seTimestamp,
    seClientRequestToken,
    seLogicalResourceId,
    sePhysicalResourceId,
    seResourceProperties,
    seResourceStatus,
    seResourceStatusReason,
    seResourceType,

    -- * ResourceType
    ResourceType (..),

    -- * StackSummary
    StackSummary (..),
    mkStackSummary,
    ssStackName,
    ssCreationTime,
    ssStackStatus,
    ssDeletionTime,
    ssDriftInformation,
    ssLastUpdatedTime,
    ssParentId,
    ssRootId,
    ssStackId,
    ssStackStatusReason,
    ssTemplateDescription,

    -- * ClientToken
    ClientToken (..),

    -- * ParameterValue
    ParameterValue (..),

    -- * PropertyPath
    PropertyPath (..),

    -- * ResourceStatusReason
    ResourceStatusReason (..),

    -- * ResourceIdentifierSummary
    ResourceIdentifierSummary (..),
    mkResourceIdentifierSummary,
    risLogicalResourceIds,
    risResourceIdentifiers,
    risResourceType,

    -- * ResourceModel
    ResourceModel (..),

    -- * Arn
    Arn (..),

    -- * RegistryType
    RegistryType (..),

    -- * HandlerErrorCode
    HandlerErrorCode (..),

    -- * StackSetOperationStatus
    StackSetOperationStatus (..),

    -- * StackSetDriftStatus
    StackSetDriftStatus (..),

    -- * StackResourceDetail
    StackResourceDetail (..),
    mkStackResourceDetail,
    srdLogicalResourceId,
    srdResourceType,
    srdLastUpdatedTimestamp,
    srdResourceStatus,
    srdDescription,
    srdDriftInformation,
    srdMetadata,
    srdModuleInfo,
    srdPhysicalResourceId,
    srdResourceStatusReason,
    srdStackId,
    srdStackName,

    -- * StackSetStatus
    StackSetStatus (..),

    -- * StackSetSummary
    StackSetSummary (..),
    mkStackSetSummary,
    sssAutoDeployment,
    sssDescription,
    sssDriftStatus,
    sssLastDriftCheckTimestamp,
    sssPermissionModel,
    sssStackSetId,
    sssStackSetName,
    sssStatus,

    -- * ChangeAction
    ChangeAction (..),

    -- * PrivateTypeArn
    PrivateTypeArn (..),

    -- * RequiresRecreation
    RequiresRecreation (..),

    -- * StackInstanceSummary
    StackInstanceSummary (..),
    mkStackInstanceSummary,
    sisAccount,
    sisDriftStatus,
    sisLastDriftCheckTimestamp,
    sisOrganizationalUnitId,
    sisRegion,
    sisStackId,
    sisStackInstanceStatus,
    sisStackSetId,
    sisStatus,
    sisStatusReason,

    -- * ResourceProperties
    ResourceProperties (..),

    -- * Url
    Url (..),

    -- * Value
    Value (..),

    -- * ChangeSetNameOrId
    ChangeSetNameOrId (..),

    -- * ChangeSource
    ChangeSource (..),

    -- * ChangeSetName
    ChangeSetName (..),

    -- * AutoDeployment
    AutoDeployment (..),
    mkAutoDeployment,
    adEnabled,
    adRetainStacksOnAccountRemoval,

    -- * StackStatusReason
    StackStatusReason (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * ParameterKey
    ParameterKey (..),

    -- * ResourceAttribute
    ResourceAttribute (..),

    -- * StackPolicyBody
    StackPolicyBody (..),

    -- * TemplateDescription
    TemplateDescription (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * Change
    Change (..),
    mkChange,
    cResourceChange,
    cType,

    -- * EvaluationType
    EvaluationType (..),

    -- * Visibility
    Visibility (..),

    -- * LogicalIdHierarchy
    LogicalIdHierarchy (..),

    -- * TemplateParameter
    TemplateParameter (..),
    mkTemplateParameter,
    tpDefaultValue,
    tpDescription,
    tpNoEcho,
    tpParameterKey,

    -- * OperationStatus
    OperationStatus (..),

    -- * ParameterType
    ParameterType (..),

    -- * ParameterDeclaration
    ParameterDeclaration (..),
    mkParameterDeclaration,
    pdDefaultValue,
    pdDescription,
    pdNoEcho,
    pdParameterConstraints,
    pdParameterKey,
    pdParameterType,

    -- * ChangeSetId
    ChangeSetId (..),

    -- * StackSetOperation
    StackSetOperation (..),
    mkStackSetOperation,
    ssoAction,
    ssoAdministrationRoleARN,
    ssoCreationTimestamp,
    ssoDeploymentTargets,
    ssoEndTimestamp,
    ssoExecutionRoleName,
    ssoOperationId,
    ssoOperationPreferences,
    ssoRetainStacks,
    ssoStackSetDriftDetectionDetails,
    ssoStackSetId,
    ssoStatus,

    -- * ResourceIdentifierPropertyKey
    ResourceIdentifierPropertyKey (..),

    -- * StackSetARN
    StackSetARN (..),

    -- * StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary (..),
    mkStackResourceDriftInformationSummary,
    srdisStackResourceDriftStatus,
    srdisLastCheckTimestamp,

    -- * StackDriftDetectionId
    StackDriftDetectionId (..),

    -- * Account
    Account (..),

    -- * LogGroupName
    LogGroupName (..),

    -- * PropertyValue
    PropertyValue (..),

    -- * TypeSummary
    TypeSummary (..),
    mkTypeSummary,
    tsDefaultVersionId,
    tsDescription,
    tsLastUpdated,
    tsType,
    tsTypeArn,
    tsTypeName,

    -- * Reason
    Reason (..),

    -- * ModuleInfo
    ModuleInfo (..),
    mkModuleInfo,
    miLogicalIdHierarchy,
    miTypeHierarchy,

    -- * NextToken
    NextToken (..),

    -- * StackPolicyDuringUpdateBody
    StackPolicyDuringUpdateBody (..),

    -- * TemplateStage
    TemplateStage (..),

    -- * StackResource
    StackResource (..),
    mkStackResource,
    srLogicalResourceId,
    srResourceType,
    srTimestamp,
    srResourceStatus,
    srDescription,
    srDriftInformation,
    srModuleInfo,
    srPhysicalResourceId,
    srResourceStatusReason,
    srStackId,
    srStackName,

    -- * Output
    Output (..),
    mkOutput,
    oDescription,
    oExportName,
    oOutputKey,
    oOutputValue,

    -- * ParameterConstraints
    ParameterConstraints (..),
    mkParameterConstraints,
    pcAllowedValues,

    -- * StackSetOperationAction
    StackSetOperationAction (..),

    -- * Key
    Key (..),

    -- * StackInstance
    StackInstance (..),
    mkStackInstance,
    siAccount,
    siDriftStatus,
    siLastDriftCheckTimestamp,
    siOrganizationalUnitId,
    siParameterOverrides,
    siRegion,
    siStackId,
    siStackInstanceStatus,
    siStackSetId,
    siStatus,
    siStatusReason,

    -- * StatusMessage
    StatusMessage (..),

    -- * TypeVersionId
    TypeVersionId (..),

    -- * StackPolicyDuringUpdateURL
    StackPolicyDuringUpdateURL (..),

    -- * AccountGateResult
    AccountGateResult (..),
    mkAccountGateResult,
    agrStatus,
    agrStatusReason,

    -- * AccountGateStatus
    AccountGateStatus (..),

    -- * Version
    Version (..),

    -- * StackSetDriftDetectionStatus
    StackSetDriftDetectionStatus (..),

    -- * CapabilitiesReason
    CapabilitiesReason (..),

    -- * ResourceIdentifierPropertyValue
    ResourceIdentifierPropertyValue (..),

    -- * DeprecatedStatus
    DeprecatedStatus (..),

    -- * TypeArn
    TypeArn (..),

    -- * StackResourceDriftInformation
    StackResourceDriftInformation (..),
    mkStackResourceDriftInformation,
    srdiStackResourceDriftStatus,
    srdiLastCheckTimestamp,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    mkResourceChangeDetail,
    rcdCausingEntity,
    rcdChangeSource,
    rcdEvaluation,
    rcdTarget,

    -- * OrganizationalUnitId
    OrganizationalUnitId (..),

    -- * Metadata
    Metadata (..),

    -- * StackSetOperationSummary
    StackSetOperationSummary (..),
    mkStackSetOperationSummary,
    ssosAction,
    ssosCreationTimestamp,
    ssosEndTimestamp,
    ssosOperationId,
    ssosStatus,

    -- * StackResourceDriftStatus
    StackResourceDriftStatus (..),

    -- * StackPolicyURL
    StackPolicyURL (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * StackDriftDetectionStatus
    StackDriftDetectionStatus (..),

    -- * StackResourceSummary
    StackResourceSummary (..),
    mkStackResourceSummary,
    srsLogicalResourceId,
    srsResourceType,
    srsLastUpdatedTimestamp,
    srsResourceStatus,
    srsDriftInformation,
    srsModuleInfo,
    srsPhysicalResourceId,
    srsResourceStatusReason,

    -- * Region
    Region (..),

    -- * NotificationARN
    NotificationARN (..),

    -- * Capability
    Capability (..),

    -- * PropertyDifference
    PropertyDifference (..),
    mkPropertyDifference,
    pdPropertyPath,
    pdExpectedValue,
    pdActualValue,
    pdDifferenceType,

    -- * ChangeSetSummary
    ChangeSetSummary (..),
    mkChangeSetSummary,
    cssChangeSetId,
    cssChangeSetName,
    cssCreationTime,
    cssDescription,
    cssExecutionStatus,
    cssIncludeNestedStacks,
    cssParentChangeSetId,
    cssRootChangeSetId,
    cssStackId,
    cssStackName,
    cssStatus,
    cssStatusReason,

    -- * Type
    Type (..),

    -- * TemplateBody
    TemplateBody (..),

    -- * PropertyName
    PropertyName (..),

    -- * OutputKey
    OutputKey (..),

    -- * StackInstanceFilterName
    StackInstanceFilterName (..),

    -- * ChangeSetStatus
    ChangeSetStatus (..),

    -- * ResourceSignalStatus
    ResourceSignalStatus (..),

    -- * ResourceToSkip
    ResourceToSkip (..),

    -- * TemplateURL
    TemplateURL (..),

    -- * TransformName
    TransformName (..),

    -- * StackInstanceDetailedStatus
    StackInstanceDetailedStatus (..),

    -- * DeploymentTargets
    DeploymentTargets (..),
    mkDeploymentTargets,
    dtAccounts,
    dtOrganizationalUnitIds,

    -- * StackId
    StackId (..),

    -- * StackSetName
    StackSetName (..),

    -- * ExportName
    ExportName (..),

    -- * PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair (..),
    mkPhysicalResourceIdContextKeyValuePair,
    prickvpKey,
    prickvpValue,

    -- * StackInstanceStatus
    StackInstanceStatus (..),

    -- * StackDriftInformationSummary
    StackDriftInformationSummary (..),
    mkStackDriftInformationSummary,
    sdisStackDriftStatus,
    sdisLastCheckTimestamp,

    -- * AllowedValue
    AllowedValue (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * Replacement
    Replacement (..),

    -- * TypeVersionSummary
    TypeVersionSummary (..),
    mkTypeVersionSummary,
    tvsArn,
    tvsDescription,
    tvsIsDefaultVersion,
    tvsTimeCreated,
    tvsType,
    tvsTypeName,
    tvsVersionId,

    -- * Description
    Description (..),

    -- * Stack
    Stack (..),
    mkStack,
    sfStackName,
    sfCreationTime,
    sfStackStatus,
    sfCapabilities,
    sfChangeSetId,
    sfDeletionTime,
    sfDescription,
    sfDisableRollback,
    sfDriftInformation,
    sfEnableTerminationProtection,
    sfLastUpdatedTime,
    sfNotificationARNs,
    sfOutputs,
    sfParameters,
    sfParentId,
    sfRoleARN,
    sfRollbackConfiguration,
    sfRootId,
    sfStackId,
    sfStackStatusReason,
    sfTags,
    sfTimeoutInMinutes,

    -- * ChangeType
    ChangeType (..),

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    mkRollbackConfiguration,
    rcMonitoringTimeInMinutes,
    rcRollbackTriggers,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    mkResourceTargetDefinition,
    rtdAttribute,
    rtdName,
    rtdRequiresRecreation,

    -- * RollbackTrigger
    RollbackTrigger (..),
    mkRollbackTrigger,
    rtArn,
    rtType,

    -- * OnFailure
    OnFailure (..),

    -- * StackResourceDrift
    StackResourceDrift (..),
    mkStackResourceDrift,
    sStackId,
    sLogicalResourceId,
    sResourceType,
    sStackResourceDriftStatus,
    sTimestamp,
    sActualProperties,
    sExpectedProperties,
    sModuleInfo,
    sPhysicalResourceId,
    sPhysicalResourceIdContext,
    sPropertyDifferences,

    -- * StackSetOperationPreferences
    StackSetOperationPreferences (..),
    mkStackSetOperationPreferences,
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopRegionOrder,

    -- * Parameter
    Parameter (..),
    mkParameter,
    pParameterKey,
    pParameterValue,
    pResolvedValue,
    pUsePreviousValue,

    -- * ProvisioningType
    ProvisioningType (..),

    -- * StackSetId
    StackSetId (..),

    -- * RoleARN
    RoleARN (..),

    -- * LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcLogRoleArn,
    lcLogGroupName,

    -- * ExecutionRoleName
    ExecutionRoleName (..),

    -- * EventId
    EventId (..),

    -- * StackName
    StackName (..),

    -- * Name
    Name (..),

    -- * StatusReason
    StatusReason (..),

    -- * ExportingStackId
    ExportingStackId (..),

    -- * AdministrationRoleARN
    AdministrationRoleARN (..),

    -- * StackInstanceAccount
    StackInstanceAccount (..),

    -- * StackInstanceRegion
    StackInstanceRegion (..),

    -- * Values
    Values (..),

    -- * UniqueId
    UniqueId (..),

    -- * ParentId
    ParentId (..),

    -- * RootId
    RootId (..),

    -- * OperationId
    OperationId (..),

    -- * TypeVersionArn
    TypeVersionArn (..),

    -- * VersionId
    VersionId (..),

    -- * DefaultVersionId
    DefaultVersionId (..),

    -- * SchemaHandlerPackage
    SchemaHandlerPackage (..),

    -- * ExecutionRoleArn
    ExecutionRoleArn (..),

    -- * DocumentationUrl
    DocumentationUrl (..),

    -- * Schema
    Schema (..),

    -- * SourceUrl
    SourceUrl (..),

    -- * ActualProperties
    ActualProperties (..),

    -- * ExpectedProperties
    ExpectedProperties (..),

    -- * LogRoleArn
    LogRoleArn (..),
  )
where

import Network.AWS.CloudFormation.Types.Account
import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.AccountGateStatus
import Network.AWS.CloudFormation.Types.AccountLimit
import Network.AWS.CloudFormation.Types.ActualProperties
import Network.AWS.CloudFormation.Types.AdministrationRoleARN
import Network.AWS.CloudFormation.Types.AllowedValue
import Network.AWS.CloudFormation.Types.Arn
import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.CapabilitiesReason
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.CausingEntity
import Network.AWS.CloudFormation.Types.Change
import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ChangeSetId
import Network.AWS.CloudFormation.Types.ChangeSetName
import Network.AWS.CloudFormation.Types.ChangeSetNameOrId
import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ChangeSetStatusReason
import Network.AWS.CloudFormation.Types.ChangeSetSummary
import Network.AWS.CloudFormation.Types.ChangeSetType
import Network.AWS.CloudFormation.Types.ChangeSource
import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.ClientRequestToken
import Network.AWS.CloudFormation.Types.ClientToken
import Network.AWS.CloudFormation.Types.DefaultVersionId
import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.DeprecatedStatus
import Network.AWS.CloudFormation.Types.Description
import Network.AWS.CloudFormation.Types.DifferenceType
import Network.AWS.CloudFormation.Types.DocumentationUrl
import Network.AWS.CloudFormation.Types.EvaluationType
import Network.AWS.CloudFormation.Types.EventId
import Network.AWS.CloudFormation.Types.ExecutionRoleArn
import Network.AWS.CloudFormation.Types.ExecutionRoleName
import Network.AWS.CloudFormation.Types.ExecutionStatus
import Network.AWS.CloudFormation.Types.ExpectedProperties
import Network.AWS.CloudFormation.Types.Export
import Network.AWS.CloudFormation.Types.ExportName
import Network.AWS.CloudFormation.Types.ExportingStackId
import Network.AWS.CloudFormation.Types.HandlerErrorCode
import Network.AWS.CloudFormation.Types.Key
import Network.AWS.CloudFormation.Types.LogGroupName
import Network.AWS.CloudFormation.Types.LogRoleArn
import Network.AWS.CloudFormation.Types.LoggingConfig
import Network.AWS.CloudFormation.Types.LogicalIdHierarchy
import Network.AWS.CloudFormation.Types.LogicalResourceId
import Network.AWS.CloudFormation.Types.Metadata
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.Name
import Network.AWS.CloudFormation.Types.NextToken
import Network.AWS.CloudFormation.Types.NotificationARN
import Network.AWS.CloudFormation.Types.OnFailure
import Network.AWS.CloudFormation.Types.OperationId
import Network.AWS.CloudFormation.Types.OperationStatus
import Network.AWS.CloudFormation.Types.OrganizationalUnitId
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.OutputKey
import Network.AWS.CloudFormation.Types.OutputValue
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.ParameterConstraints
import Network.AWS.CloudFormation.Types.ParameterDeclaration
import Network.AWS.CloudFormation.Types.ParameterKey
import Network.AWS.CloudFormation.Types.ParameterType
import Network.AWS.CloudFormation.Types.ParameterValue
import Network.AWS.CloudFormation.Types.ParentId
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.PhysicalResourceId
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PrivateTypeArn
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.PropertyName
import Network.AWS.CloudFormation.Types.PropertyPath
import Network.AWS.CloudFormation.Types.PropertyValue
import Network.AWS.CloudFormation.Types.ProvisioningType
import Network.AWS.CloudFormation.Types.Reason
import Network.AWS.CloudFormation.Types.Region
import Network.AWS.CloudFormation.Types.RegistrationStatus
import Network.AWS.CloudFormation.Types.RegistrationToken
import Network.AWS.CloudFormation.Types.RegistryType
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.RequestToken
import Network.AWS.CloudFormation.Types.RequiresRecreation
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChange
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import Network.AWS.CloudFormation.Types.ResourceIdentifierPropertyKey
import Network.AWS.CloudFormation.Types.ResourceIdentifierPropertyValue
import Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
import Network.AWS.CloudFormation.Types.ResourceModel
import Network.AWS.CloudFormation.Types.ResourceProperties
import Network.AWS.CloudFormation.Types.ResourceSignalStatus
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.ResourceStatusReason
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import Network.AWS.CloudFormation.Types.ResourceToImport
import Network.AWS.CloudFormation.Types.ResourceToSkip
import Network.AWS.CloudFormation.Types.ResourceType
import Network.AWS.CloudFormation.Types.RoleARN
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.RollbackTrigger
import Network.AWS.CloudFormation.Types.RootId
import Network.AWS.CloudFormation.Types.Schema
import Network.AWS.CloudFormation.Types.SchemaHandlerPackage
import Network.AWS.CloudFormation.Types.SourceUrl
import Network.AWS.CloudFormation.Types.Stack
import Network.AWS.CloudFormation.Types.StackDriftDetectionId
import Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackDriftDetectionStatusReason
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackEvent
import Network.AWS.CloudFormation.Types.StackId
import Network.AWS.CloudFormation.Types.StackInstance
import Network.AWS.CloudFormation.Types.StackInstanceAccount
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import Network.AWS.CloudFormation.Types.StackInstanceFilter
import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import Network.AWS.CloudFormation.Types.StackInstanceRegion
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.CloudFormation.Types.StackInstanceSummary
import Network.AWS.CloudFormation.Types.StackName
import Network.AWS.CloudFormation.Types.StackPolicyBody
import Network.AWS.CloudFormation.Types.StackPolicyDuringUpdateBody
import Network.AWS.CloudFormation.Types.StackPolicyDuringUpdateURL
import Network.AWS.CloudFormation.Types.StackPolicyURL
import Network.AWS.CloudFormation.Types.StackResource
import Network.AWS.CloudFormation.Types.StackResourceDetail
import Network.AWS.CloudFormation.Types.StackResourceDrift
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.CloudFormation.Types.StackResourceSummary
import Network.AWS.CloudFormation.Types.StackSet
import Network.AWS.CloudFormation.Types.StackSetARN
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
import Network.AWS.CloudFormation.Types.StackSetId
import Network.AWS.CloudFormation.Types.StackSetName
import Network.AWS.CloudFormation.Types.StackSetOperation
import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationPreferences
import Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
import Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import Network.AWS.CloudFormation.Types.StackSetOperationSummary
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.CloudFormation.Types.StackSetSummary
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.CloudFormation.Types.StackStatusReason
import Network.AWS.CloudFormation.Types.StackSummary
import Network.AWS.CloudFormation.Types.StatusMessage
import Network.AWS.CloudFormation.Types.StatusReason
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.CloudFormation.Types.TemplateBody
import Network.AWS.CloudFormation.Types.TemplateDescription
import Network.AWS.CloudFormation.Types.TemplateParameter
import Network.AWS.CloudFormation.Types.TemplateStage
import Network.AWS.CloudFormation.Types.TemplateURL
import Network.AWS.CloudFormation.Types.TransformName
import Network.AWS.CloudFormation.Types.Type
import Network.AWS.CloudFormation.Types.TypeArn
import Network.AWS.CloudFormation.Types.TypeHierarchy
import Network.AWS.CloudFormation.Types.TypeName
import Network.AWS.CloudFormation.Types.TypeSummary
import Network.AWS.CloudFormation.Types.TypeVersionArn
import Network.AWS.CloudFormation.Types.TypeVersionId
import Network.AWS.CloudFormation.Types.TypeVersionSummary
import Network.AWS.CloudFormation.Types.UniqueId
import Network.AWS.CloudFormation.Types.Url
import Network.AWS.CloudFormation.Types.Value
import Network.AWS.CloudFormation.Types.Values
import Network.AWS.CloudFormation.Types.Version
import Network.AWS.CloudFormation.Types.VersionId
import Network.AWS.CloudFormation.Types.Visibility
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudFormation",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloudformation",
      Core._svcVersion = "2010-05-15",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "CloudFormation",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The specified type does not exist in the CloudFormation registry.
_TypeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TypeNotFoundException =
  Core._MatchServiceError mkServiceConfig "TypeNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _TypeNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource exists, but has been changed.
_CreatedButModifiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CreatedButModifiedException =
  Core._MatchServiceError
    mkServiceConfig
    "CreatedButModifiedException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _CreatedButModifiedException "Use generic-lens or generic-optics instead." #-}

-- | The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the @ListChangeSets@ action.
_ChangeSetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ChangeSetNotFoundException =
  Core._MatchServiceError mkServiceConfig "ChangeSetNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ChangeSetNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationInProgressException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _OperationInProgressException "Use generic-lens or generic-optics instead." #-}

-- | The specified change set can't be used to update the stack. For example, the change set status might be @CREATE_IN_PROGRESS@ , or the stack status might be @UPDATE_IN_PROGRESS@ .
_InvalidChangeSetStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidChangeSetStatusException =
  Core._MatchServiceError mkServiceConfig "InvalidChangeSetStatus"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidChangeSetStatusException "Use generic-lens or generic-optics instead." #-}

-- | The specified ID refers to an operation that doesn't exist.
_OperationNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _OperationNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified operation ID already exists.
_OperationIdAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationIdAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationIdAlreadyExistsException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _OperationIdAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The template contains resources with capabilities that weren't specified in the Capabilities parameter.
_InsufficientCapabilitiesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientCapabilitiesException =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientCapabilitiesException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InsufficientCapabilitiesException "Use generic-lens or generic-optics instead." #-}

-- | A client request token already exists.
_TokenAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TokenAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "TokenAlreadyExistsException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TokenAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The specified stack set doesn't exist.
_StackSetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StackSetNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "StackSetNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _StackSetNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified stack instance doesn't exist.
_StackInstanceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StackInstanceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "StackInstanceNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _StackInstanceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Error reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> . CloudFormation does not return this error to users.
_OperationStatusCheckFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationStatusCheckFailedException =
  Core._MatchServiceError mkServiceConfig "ConditionalCheckFailed"
    Core.. Core.hasStatues 400
{-# DEPRECATED _OperationStatusCheckFailedException "Use generic-lens or generic-optics instead." #-}

-- | You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.
_StackSetNotEmptyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StackSetNotEmptyException =
  Core._MatchServiceError
    mkServiceConfig
    "StackSetNotEmptyException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _StackSetNotEmptyException "Use generic-lens or generic-optics instead." #-}

-- | The specified operation isn't valid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidOperationException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead." #-}

-- | Error reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> . CloudFormation does not return this error to users.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError mkServiceConfig "InvalidStateTransition"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidStateTransitionException "Use generic-lens or generic-optics instead." #-}

-- | The specified name is already in use.
_NameAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NameAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "NameAlreadyExistsException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _NameAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | An error occurred during a CloudFormation registry operation.
_CFNRegistryException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CFNRegistryException =
  Core._MatchServiceError mkServiceConfig "CFNRegistryException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _CFNRegistryException "Use generic-lens or generic-optics instead." #-}

-- | Another operation has been performed on this stack set since the specified operation was performed.
_StaleRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StaleRequestException =
  Core._MatchServiceError mkServiceConfig "StaleRequestException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _StaleRequestException "Use generic-lens or generic-optics instead." #-}

-- | The resource with the name requested already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError mkServiceConfig "AlreadyExistsException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The quota for the resource has already been reached.
--
-- For information on resource and stack limitations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html Limits> in the /AWS CloudFormation User Guide/ .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
