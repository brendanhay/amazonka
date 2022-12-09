{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFormation.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _CFNRegistryException,
    _ChangeSetNotFoundException,
    _CreatedButModifiedException,
    _InsufficientCapabilitiesException,
    _InvalidChangeSetStatusException,
    _InvalidOperationException,
    _InvalidStateTransitionException,
    _LimitExceededException,
    _NameAlreadyExistsException,
    _OperationIdAlreadyExistsException,
    _OperationInProgressException,
    _OperationNotFoundException,
    _OperationStatusCheckFailedException,
    _StackInstanceNotFoundException,
    _StackNotFoundException,
    _StackSetNotEmptyException,
    _StackSetNotFoundException,
    _StaleRequestException,
    _TokenAlreadyExistsException,
    _TypeConfigurationNotFoundException,
    _TypeNotFoundException,

    -- * AccountFilterType
    AccountFilterType (..),

    -- * AccountGateStatus
    AccountGateStatus (..),

    -- * CallAs
    CallAs (..),

    -- * Capability
    Capability (..),

    -- * Category
    Category (..),

    -- * ChangeAction
    ChangeAction (..),

    -- * ChangeSetHooksStatus
    ChangeSetHooksStatus (..),

    -- * ChangeSetStatus
    ChangeSetStatus (..),

    -- * ChangeSetType
    ChangeSetType (..),

    -- * ChangeSource
    ChangeSource (..),

    -- * ChangeType
    ChangeType (..),

    -- * DeprecatedStatus
    DeprecatedStatus (..),

    -- * DifferenceType
    DifferenceType (..),

    -- * EvaluationType
    EvaluationType (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * HandlerErrorCode
    HandlerErrorCode (..),

    -- * HookFailureMode
    HookFailureMode (..),

    -- * HookInvocationPoint
    HookInvocationPoint (..),

    -- * HookStatus
    HookStatus (..),

    -- * HookTargetType
    HookTargetType (..),

    -- * IdentityProvider
    IdentityProvider (..),

    -- * OnFailure
    OnFailure (..),

    -- * OperationResultFilterName
    OperationResultFilterName (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * PermissionModels
    PermissionModels (..),

    -- * ProvisioningType
    ProvisioningType (..),

    -- * PublisherStatus
    PublisherStatus (..),

    -- * RegionConcurrencyType
    RegionConcurrencyType (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * RegistryType
    RegistryType (..),

    -- * Replacement
    Replacement (..),

    -- * RequiresRecreation
    RequiresRecreation (..),

    -- * ResourceAttribute
    ResourceAttribute (..),

    -- * ResourceSignalStatus
    ResourceSignalStatus (..),

    -- * ResourceStatus
    ResourceStatus (..),

    -- * StackDriftDetectionStatus
    StackDriftDetectionStatus (..),

    -- * StackDriftStatus
    StackDriftStatus (..),

    -- * StackInstanceDetailedStatus
    StackInstanceDetailedStatus (..),

    -- * StackInstanceFilterName
    StackInstanceFilterName (..),

    -- * StackInstanceStatus
    StackInstanceStatus (..),

    -- * StackResourceDriftStatus
    StackResourceDriftStatus (..),

    -- * StackSetDriftDetectionStatus
    StackSetDriftDetectionStatus (..),

    -- * StackSetDriftStatus
    StackSetDriftStatus (..),

    -- * StackSetOperationAction
    StackSetOperationAction (..),

    -- * StackSetOperationResultStatus
    StackSetOperationResultStatus (..),

    -- * StackSetOperationStatus
    StackSetOperationStatus (..),

    -- * StackSetStatus
    StackSetStatus (..),

    -- * StackStatus
    StackStatus (..),

    -- * TemplateStage
    TemplateStage (..),

    -- * ThirdPartyType
    ThirdPartyType (..),

    -- * TypeTestsStatus
    TypeTestsStatus (..),

    -- * VersionBump
    VersionBump (..),

    -- * Visibility
    Visibility (..),

    -- * AccountGateResult
    AccountGateResult (..),
    newAccountGateResult,
    accountGateResult_status,
    accountGateResult_statusReason,

    -- * AccountLimit
    AccountLimit (..),
    newAccountLimit,
    accountLimit_name,
    accountLimit_value,

    -- * AutoDeployment
    AutoDeployment (..),
    newAutoDeployment,
    autoDeployment_enabled,
    autoDeployment_retainStacksOnAccountRemoval,

    -- * BatchDescribeTypeConfigurationsError
    BatchDescribeTypeConfigurationsError (..),
    newBatchDescribeTypeConfigurationsError,
    batchDescribeTypeConfigurationsError_errorCode,
    batchDescribeTypeConfigurationsError_errorMessage,
    batchDescribeTypeConfigurationsError_typeConfigurationIdentifier,

    -- * Change
    Change (..),
    newChange,
    change_hookInvocationCount,
    change_resourceChange,
    change_type,

    -- * ChangeSetHook
    ChangeSetHook (..),
    newChangeSetHook,
    changeSetHook_failureMode,
    changeSetHook_invocationPoint,
    changeSetHook_targetDetails,
    changeSetHook_typeConfigurationVersionId,
    changeSetHook_typeName,
    changeSetHook_typeVersionId,

    -- * ChangeSetHookResourceTargetDetails
    ChangeSetHookResourceTargetDetails (..),
    newChangeSetHookResourceTargetDetails,
    changeSetHookResourceTargetDetails_logicalResourceId,
    changeSetHookResourceTargetDetails_resourceAction,
    changeSetHookResourceTargetDetails_resourceType,

    -- * ChangeSetHookTargetDetails
    ChangeSetHookTargetDetails (..),
    newChangeSetHookTargetDetails,
    changeSetHookTargetDetails_resourceTargetDetails,
    changeSetHookTargetDetails_targetType,

    -- * ChangeSetSummary
    ChangeSetSummary (..),
    newChangeSetSummary,
    changeSetSummary_changeSetId,
    changeSetSummary_changeSetName,
    changeSetSummary_creationTime,
    changeSetSummary_description,
    changeSetSummary_executionStatus,
    changeSetSummary_includeNestedStacks,
    changeSetSummary_parentChangeSetId,
    changeSetSummary_rootChangeSetId,
    changeSetSummary_stackId,
    changeSetSummary_stackName,
    changeSetSummary_status,
    changeSetSummary_statusReason,

    -- * DeploymentTargets
    DeploymentTargets (..),
    newDeploymentTargets,
    deploymentTargets_accountFilterType,
    deploymentTargets_accounts,
    deploymentTargets_accountsUrl,
    deploymentTargets_organizationalUnitIds,

    -- * Export
    Export (..),
    newExport,
    export_exportingStackId,
    export_name,
    export_value,

    -- * LoggingConfig
    LoggingConfig (..),
    newLoggingConfig,
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- * ManagedExecution
    ManagedExecution (..),
    newManagedExecution,
    managedExecution_active,

    -- * ModuleInfo
    ModuleInfo (..),
    newModuleInfo,
    moduleInfo_logicalIdHierarchy,
    moduleInfo_typeHierarchy,

    -- * OperationResultFilter
    OperationResultFilter (..),
    newOperationResultFilter,
    operationResultFilter_name,
    operationResultFilter_values,

    -- * Output
    Output (..),
    newOutput,
    output_description,
    output_exportName,
    output_outputKey,
    output_outputValue,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_parameterKey,
    parameter_parameterValue,
    parameter_resolvedValue,
    parameter_usePreviousValue,

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_allowedValues,

    -- * ParameterDeclaration
    ParameterDeclaration (..),
    newParameterDeclaration,
    parameterDeclaration_defaultValue,
    parameterDeclaration_description,
    parameterDeclaration_noEcho,
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_parameterKey,
    parameterDeclaration_parameterType,

    -- * PhysicalResourceIdContextKeyValuePair
    PhysicalResourceIdContextKeyValuePair (..),
    newPhysicalResourceIdContextKeyValuePair,
    physicalResourceIdContextKeyValuePair_key,
    physicalResourceIdContextKeyValuePair_value,

    -- * PropertyDifference
    PropertyDifference (..),
    newPropertyDifference,
    propertyDifference_propertyPath,
    propertyDifference_expectedValue,
    propertyDifference_actualValue,
    propertyDifference_differenceType,

    -- * RequiredActivatedType
    RequiredActivatedType (..),
    newRequiredActivatedType,
    requiredActivatedType_originalTypeName,
    requiredActivatedType_publisherId,
    requiredActivatedType_supportedMajorVersions,
    requiredActivatedType_typeNameAlias,

    -- * ResourceChange
    ResourceChange (..),
    newResourceChange,
    resourceChange_action,
    resourceChange_changeSetId,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_moduleInfo,
    resourceChange_physicalResourceId,
    resourceChange_replacement,
    resourceChange_resourceType,
    resourceChange_scope,

    -- * ResourceChangeDetail
    ResourceChangeDetail (..),
    newResourceChangeDetail,
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_changeSource,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- * ResourceIdentifierSummary
    ResourceIdentifierSummary (..),
    newResourceIdentifierSummary,
    resourceIdentifierSummary_logicalResourceIds,
    resourceIdentifierSummary_resourceIdentifiers,
    resourceIdentifierSummary_resourceType,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    newResourceTargetDefinition,
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_name,
    resourceTargetDefinition_requiresRecreation,

    -- * ResourceToImport
    ResourceToImport (..),
    newResourceToImport,
    resourceToImport_resourceType,
    resourceToImport_logicalResourceId,
    resourceToImport_resourceIdentifier,

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    newRollbackConfiguration,
    rollbackConfiguration_monitoringTimeInMinutes,
    rollbackConfiguration_rollbackTriggers,

    -- * RollbackTrigger
    RollbackTrigger (..),
    newRollbackTrigger,
    rollbackTrigger_arn,
    rollbackTrigger_type,

    -- * Stack
    Stack (..),
    newStack,
    stack_capabilities,
    stack_changeSetId,
    stack_deletionTime,
    stack_description,
    stack_disableRollback,
    stack_driftInformation,
    stack_enableTerminationProtection,
    stack_lastUpdatedTime,
    stack_notificationARNs,
    stack_outputs,
    stack_parameters,
    stack_parentId,
    stack_roleARN,
    stack_rollbackConfiguration,
    stack_rootId,
    stack_stackId,
    stack_stackStatusReason,
    stack_tags,
    stack_timeoutInMinutes,
    stack_stackName,
    stack_creationTime,
    stack_stackStatus,

    -- * StackDriftInformation
    StackDriftInformation (..),
    newStackDriftInformation,
    stackDriftInformation_lastCheckTimestamp,
    stackDriftInformation_stackDriftStatus,

    -- * StackDriftInformationSummary
    StackDriftInformationSummary (..),
    newStackDriftInformationSummary,
    stackDriftInformationSummary_lastCheckTimestamp,
    stackDriftInformationSummary_stackDriftStatus,

    -- * StackEvent
    StackEvent (..),
    newStackEvent,
    stackEvent_clientRequestToken,
    stackEvent_hookFailureMode,
    stackEvent_hookInvocationPoint,
    stackEvent_hookStatus,
    stackEvent_hookStatusReason,
    stackEvent_hookType,
    stackEvent_logicalResourceId,
    stackEvent_physicalResourceId,
    stackEvent_resourceProperties,
    stackEvent_resourceStatus,
    stackEvent_resourceStatusReason,
    stackEvent_resourceType,
    stackEvent_stackId,
    stackEvent_eventId,
    stackEvent_stackName,
    stackEvent_timestamp,

    -- * StackInstance
    StackInstance (..),
    newStackInstance,
    stackInstance_account,
    stackInstance_driftStatus,
    stackInstance_lastDriftCheckTimestamp,
    stackInstance_lastOperationId,
    stackInstance_organizationalUnitId,
    stackInstance_parameterOverrides,
    stackInstance_region,
    stackInstance_stackId,
    stackInstance_stackInstanceStatus,
    stackInstance_stackSetId,
    stackInstance_status,
    stackInstance_statusReason,

    -- * StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus (..),
    newStackInstanceComprehensiveStatus,
    stackInstanceComprehensiveStatus_detailedStatus,

    -- * StackInstanceFilter
    StackInstanceFilter (..),
    newStackInstanceFilter,
    stackInstanceFilter_name,
    stackInstanceFilter_values,

    -- * StackInstanceSummary
    StackInstanceSummary (..),
    newStackInstanceSummary,
    stackInstanceSummary_account,
    stackInstanceSummary_driftStatus,
    stackInstanceSummary_lastDriftCheckTimestamp,
    stackInstanceSummary_lastOperationId,
    stackInstanceSummary_organizationalUnitId,
    stackInstanceSummary_region,
    stackInstanceSummary_stackId,
    stackInstanceSummary_stackInstanceStatus,
    stackInstanceSummary_stackSetId,
    stackInstanceSummary_status,
    stackInstanceSummary_statusReason,

    -- * StackResource
    StackResource (..),
    newStackResource,
    stackResource_description,
    stackResource_driftInformation,
    stackResource_moduleInfo,
    stackResource_physicalResourceId,
    stackResource_resourceStatusReason,
    stackResource_stackId,
    stackResource_stackName,
    stackResource_logicalResourceId,
    stackResource_resourceType,
    stackResource_timestamp,
    stackResource_resourceStatus,

    -- * StackResourceDetail
    StackResourceDetail (..),
    newStackResourceDetail,
    stackResourceDetail_description,
    stackResourceDetail_driftInformation,
    stackResourceDetail_metadata,
    stackResourceDetail_moduleInfo,
    stackResourceDetail_physicalResourceId,
    stackResourceDetail_resourceStatusReason,
    stackResourceDetail_stackId,
    stackResourceDetail_stackName,
    stackResourceDetail_logicalResourceId,
    stackResourceDetail_resourceType,
    stackResourceDetail_lastUpdatedTimestamp,
    stackResourceDetail_resourceStatus,

    -- * StackResourceDrift
    StackResourceDrift (..),
    newStackResourceDrift,
    stackResourceDrift_actualProperties,
    stackResourceDrift_expectedProperties,
    stackResourceDrift_moduleInfo,
    stackResourceDrift_physicalResourceId,
    stackResourceDrift_physicalResourceIdContext,
    stackResourceDrift_propertyDifferences,
    stackResourceDrift_stackId,
    stackResourceDrift_logicalResourceId,
    stackResourceDrift_resourceType,
    stackResourceDrift_stackResourceDriftStatus,
    stackResourceDrift_timestamp,

    -- * StackResourceDriftInformation
    StackResourceDriftInformation (..),
    newStackResourceDriftInformation,
    stackResourceDriftInformation_lastCheckTimestamp,
    stackResourceDriftInformation_stackResourceDriftStatus,

    -- * StackResourceDriftInformationSummary
    StackResourceDriftInformationSummary (..),
    newStackResourceDriftInformationSummary,
    stackResourceDriftInformationSummary_lastCheckTimestamp,
    stackResourceDriftInformationSummary_stackResourceDriftStatus,

    -- * StackResourceSummary
    StackResourceSummary (..),
    newStackResourceSummary,
    stackResourceSummary_driftInformation,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- * StackSet
    StackSet (..),
    newStackSet,
    stackSet_administrationRoleARN,
    stackSet_autoDeployment,
    stackSet_capabilities,
    stackSet_description,
    stackSet_executionRoleName,
    stackSet_managedExecution,
    stackSet_organizationalUnitIds,
    stackSet_parameters,
    stackSet_permissionModel,
    stackSet_stackSetARN,
    stackSet_stackSetDriftDetectionDetails,
    stackSet_stackSetId,
    stackSet_stackSetName,
    stackSet_status,
    stackSet_tags,
    stackSet_templateBody,

    -- * StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails (..),
    newStackSetDriftDetectionDetails,
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,
    stackSetDriftDetectionDetails_totalStackInstancesCount,

    -- * StackSetOperation
    StackSetOperation (..),
    newStackSetOperation,
    stackSetOperation_action,
    stackSetOperation_administrationRoleARN,
    stackSetOperation_creationTimestamp,
    stackSetOperation_deploymentTargets,
    stackSetOperation_endTimestamp,
    stackSetOperation_executionRoleName,
    stackSetOperation_operationId,
    stackSetOperation_operationPreferences,
    stackSetOperation_retainStacks,
    stackSetOperation_stackSetDriftDetectionDetails,
    stackSetOperation_stackSetId,
    stackSetOperation_status,
    stackSetOperation_statusDetails,
    stackSetOperation_statusReason,

    -- * StackSetOperationPreferences
    StackSetOperationPreferences (..),
    newStackSetOperationPreferences,
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_failureTolerancePercentage,
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_maxConcurrentPercentage,
    stackSetOperationPreferences_regionConcurrencyType,
    stackSetOperationPreferences_regionOrder,

    -- * StackSetOperationResultSummary
    StackSetOperationResultSummary (..),
    newStackSetOperationResultSummary,
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_accountGateResult,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_statusReason,

    -- * StackSetOperationStatusDetails
    StackSetOperationStatusDetails (..),
    newStackSetOperationStatusDetails,
    stackSetOperationStatusDetails_failedStackInstancesCount,

    -- * StackSetOperationSummary
    StackSetOperationSummary (..),
    newStackSetOperationSummary,
    stackSetOperationSummary_action,
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_operationId,
    stackSetOperationSummary_operationPreferences,
    stackSetOperationSummary_status,
    stackSetOperationSummary_statusDetails,
    stackSetOperationSummary_statusReason,

    -- * StackSetSummary
    StackSetSummary (..),
    newStackSetSummary,
    stackSetSummary_autoDeployment,
    stackSetSummary_description,
    stackSetSummary_driftStatus,
    stackSetSummary_lastDriftCheckTimestamp,
    stackSetSummary_managedExecution,
    stackSetSummary_permissionModel,
    stackSetSummary_stackSetId,
    stackSetSummary_stackSetName,
    stackSetSummary_status,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_deletionTime,
    stackSummary_driftInformation,
    stackSummary_lastUpdatedTime,
    stackSummary_parentId,
    stackSummary_rootId,
    stackSummary_stackId,
    stackSummary_stackStatusReason,
    stackSummary_templateDescription,
    stackSummary_stackName,
    stackSummary_creationTime,
    stackSummary_stackStatus,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TemplateParameter
    TemplateParameter (..),
    newTemplateParameter,
    templateParameter_defaultValue,
    templateParameter_description,
    templateParameter_noEcho,
    templateParameter_parameterKey,

    -- * TypeConfigurationDetails
    TypeConfigurationDetails (..),
    newTypeConfigurationDetails,
    typeConfigurationDetails_alias,
    typeConfigurationDetails_arn,
    typeConfigurationDetails_configuration,
    typeConfigurationDetails_isDefaultConfiguration,
    typeConfigurationDetails_lastUpdated,
    typeConfigurationDetails_typeArn,
    typeConfigurationDetails_typeName,

    -- * TypeConfigurationIdentifier
    TypeConfigurationIdentifier (..),
    newTypeConfigurationIdentifier,
    typeConfigurationIdentifier_type,
    typeConfigurationIdentifier_typeArn,
    typeConfigurationIdentifier_typeConfigurationAlias,
    typeConfigurationIdentifier_typeConfigurationArn,
    typeConfigurationIdentifier_typeName,

    -- * TypeFilters
    TypeFilters (..),
    newTypeFilters,
    typeFilters_category,
    typeFilters_publisherId,
    typeFilters_typeNamePrefix,

    -- * TypeSummary
    TypeSummary (..),
    newTypeSummary,
    typeSummary_defaultVersionId,
    typeSummary_description,
    typeSummary_isActivated,
    typeSummary_lastUpdated,
    typeSummary_latestPublicVersion,
    typeSummary_originalTypeName,
    typeSummary_publicVersionNumber,
    typeSummary_publisherId,
    typeSummary_publisherIdentity,
    typeSummary_publisherName,
    typeSummary_type,
    typeSummary_typeArn,
    typeSummary_typeName,

    -- * TypeVersionSummary
    TypeVersionSummary (..),
    newTypeVersionSummary,
    typeVersionSummary_arn,
    typeVersionSummary_description,
    typeVersionSummary_isDefaultVersion,
    typeVersionSummary_publicVersionNumber,
    typeVersionSummary_timeCreated,
    typeVersionSummary_type,
    typeVersionSummary_typeName,
    typeVersionSummary_versionId,
  )
where

import Amazonka.CloudFormation.Types.AccountFilterType
import Amazonka.CloudFormation.Types.AccountGateResult
import Amazonka.CloudFormation.Types.AccountGateStatus
import Amazonka.CloudFormation.Types.AccountLimit
import Amazonka.CloudFormation.Types.AutoDeployment
import Amazonka.CloudFormation.Types.BatchDescribeTypeConfigurationsError
import Amazonka.CloudFormation.Types.CallAs
import Amazonka.CloudFormation.Types.Capability
import Amazonka.CloudFormation.Types.Category
import Amazonka.CloudFormation.Types.Change
import Amazonka.CloudFormation.Types.ChangeAction
import Amazonka.CloudFormation.Types.ChangeSetHook
import Amazonka.CloudFormation.Types.ChangeSetHookResourceTargetDetails
import Amazonka.CloudFormation.Types.ChangeSetHookTargetDetails
import Amazonka.CloudFormation.Types.ChangeSetHooksStatus
import Amazonka.CloudFormation.Types.ChangeSetStatus
import Amazonka.CloudFormation.Types.ChangeSetSummary
import Amazonka.CloudFormation.Types.ChangeSetType
import Amazonka.CloudFormation.Types.ChangeSource
import Amazonka.CloudFormation.Types.ChangeType
import Amazonka.CloudFormation.Types.DeploymentTargets
import Amazonka.CloudFormation.Types.DeprecatedStatus
import Amazonka.CloudFormation.Types.DifferenceType
import Amazonka.CloudFormation.Types.EvaluationType
import Amazonka.CloudFormation.Types.ExecutionStatus
import Amazonka.CloudFormation.Types.Export
import Amazonka.CloudFormation.Types.HandlerErrorCode
import Amazonka.CloudFormation.Types.HookFailureMode
import Amazonka.CloudFormation.Types.HookInvocationPoint
import Amazonka.CloudFormation.Types.HookStatus
import Amazonka.CloudFormation.Types.HookTargetType
import Amazonka.CloudFormation.Types.IdentityProvider
import Amazonka.CloudFormation.Types.LoggingConfig
import Amazonka.CloudFormation.Types.ManagedExecution
import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.OnFailure
import Amazonka.CloudFormation.Types.OperationResultFilter
import Amazonka.CloudFormation.Types.OperationResultFilterName
import Amazonka.CloudFormation.Types.OperationStatus
import Amazonka.CloudFormation.Types.Output
import Amazonka.CloudFormation.Types.Parameter
import Amazonka.CloudFormation.Types.ParameterConstraints
import Amazonka.CloudFormation.Types.ParameterDeclaration
import Amazonka.CloudFormation.Types.PermissionModels
import Amazonka.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Amazonka.CloudFormation.Types.PropertyDifference
import Amazonka.CloudFormation.Types.ProvisioningType
import Amazonka.CloudFormation.Types.PublisherStatus
import Amazonka.CloudFormation.Types.RegionConcurrencyType
import Amazonka.CloudFormation.Types.RegistrationStatus
import Amazonka.CloudFormation.Types.RegistryType
import Amazonka.CloudFormation.Types.Replacement
import Amazonka.CloudFormation.Types.RequiredActivatedType
import Amazonka.CloudFormation.Types.RequiresRecreation
import Amazonka.CloudFormation.Types.ResourceAttribute
import Amazonka.CloudFormation.Types.ResourceChange
import Amazonka.CloudFormation.Types.ResourceChangeDetail
import Amazonka.CloudFormation.Types.ResourceIdentifierSummary
import Amazonka.CloudFormation.Types.ResourceSignalStatus
import Amazonka.CloudFormation.Types.ResourceStatus
import Amazonka.CloudFormation.Types.ResourceTargetDefinition
import Amazonka.CloudFormation.Types.ResourceToImport
import Amazonka.CloudFormation.Types.RollbackConfiguration
import Amazonka.CloudFormation.Types.RollbackTrigger
import Amazonka.CloudFormation.Types.Stack
import Amazonka.CloudFormation.Types.StackDriftDetectionStatus
import Amazonka.CloudFormation.Types.StackDriftInformation
import Amazonka.CloudFormation.Types.StackDriftInformationSummary
import Amazonka.CloudFormation.Types.StackDriftStatus
import Amazonka.CloudFormation.Types.StackEvent
import Amazonka.CloudFormation.Types.StackInstance
import Amazonka.CloudFormation.Types.StackInstanceComprehensiveStatus
import Amazonka.CloudFormation.Types.StackInstanceDetailedStatus
import Amazonka.CloudFormation.Types.StackInstanceFilter
import Amazonka.CloudFormation.Types.StackInstanceFilterName
import Amazonka.CloudFormation.Types.StackInstanceStatus
import Amazonka.CloudFormation.Types.StackInstanceSummary
import Amazonka.CloudFormation.Types.StackResource
import Amazonka.CloudFormation.Types.StackResourceDetail
import Amazonka.CloudFormation.Types.StackResourceDrift
import Amazonka.CloudFormation.Types.StackResourceDriftInformation
import Amazonka.CloudFormation.Types.StackResourceDriftInformationSummary
import Amazonka.CloudFormation.Types.StackResourceDriftStatus
import Amazonka.CloudFormation.Types.StackResourceSummary
import Amazonka.CloudFormation.Types.StackSet
import Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
import Amazonka.CloudFormation.Types.StackSetDriftDetectionStatus
import Amazonka.CloudFormation.Types.StackSetDriftStatus
import Amazonka.CloudFormation.Types.StackSetOperation
import Amazonka.CloudFormation.Types.StackSetOperationAction
import Amazonka.CloudFormation.Types.StackSetOperationPreferences
import Amazonka.CloudFormation.Types.StackSetOperationResultStatus
import Amazonka.CloudFormation.Types.StackSetOperationResultSummary
import Amazonka.CloudFormation.Types.StackSetOperationStatus
import Amazonka.CloudFormation.Types.StackSetOperationStatusDetails
import Amazonka.CloudFormation.Types.StackSetOperationSummary
import Amazonka.CloudFormation.Types.StackSetStatus
import Amazonka.CloudFormation.Types.StackSetSummary
import Amazonka.CloudFormation.Types.StackStatus
import Amazonka.CloudFormation.Types.StackSummary
import Amazonka.CloudFormation.Types.Tag
import Amazonka.CloudFormation.Types.TemplateParameter
import Amazonka.CloudFormation.Types.TemplateStage
import Amazonka.CloudFormation.Types.ThirdPartyType
import Amazonka.CloudFormation.Types.TypeConfigurationDetails
import Amazonka.CloudFormation.Types.TypeConfigurationIdentifier
import Amazonka.CloudFormation.Types.TypeFilters
import Amazonka.CloudFormation.Types.TypeSummary
import Amazonka.CloudFormation.Types.TypeTestsStatus
import Amazonka.CloudFormation.Types.TypeVersionSummary
import Amazonka.CloudFormation.Types.VersionBump
import Amazonka.CloudFormation.Types.Visibility
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudFormation",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudformation",
      Core.signingName = "cloudformation",
      Core.version = "2010-05-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "CloudFormation",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource with the name requested already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | An error occurred during a CloudFormation registry operation.
_CFNRegistryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CFNRegistryException =
  Core._MatchServiceError
    defaultService
    "CFNRegistryException"
    Prelude.. Core.hasStatus 400

-- | The specified change set name or ID doesn\'t exit. To view valid change
-- sets for a stack, use the @ListChangeSets@ operation.
_ChangeSetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ChangeSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "ChangeSetNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified resource exists, but has been changed.
_CreatedButModifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CreatedButModifiedException =
  Core._MatchServiceError
    defaultService
    "CreatedButModifiedException"
    Prelude.. Core.hasStatus 409

-- | The template contains resources with capabilities that weren\'t
-- specified in the Capabilities parameter.
_InsufficientCapabilitiesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientCapabilitiesException =
  Core._MatchServiceError
    defaultService
    "InsufficientCapabilitiesException"
    Prelude.. Core.hasStatus 400

-- | The specified change set can\'t be used to update the stack. For
-- example, the change set status might be @CREATE_IN_PROGRESS@, or the
-- stack status might be @UPDATE_IN_PROGRESS@.
_InvalidChangeSetStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidChangeSetStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidChangeSetStatus"
    Prelude.. Core.hasStatus 400

-- | The specified operation isn\'t valid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"
    Prelude.. Core.hasStatus 400

-- | Error reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- CloudFormation doesn\'t return this error to users.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransition"
    Prelude.. Core.hasStatus 400

-- | The quota for the resource has already been reached.
--
-- For information about resource and stack limitations, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html CloudFormation quotas>
-- in the /CloudFormation User Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified name is already in use.
_NameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "NameAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The specified operation ID already exists.
_OperationIdAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationIdAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OperationIdAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | Another operation is currently in progress for this stack set. Only one
-- operation can be performed for a stack set at a given time.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressException"
    Prelude.. Core.hasStatus 409

-- | The specified ID refers to an operation that doesn\'t exist.
_OperationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OperationNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Error reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- CloudFormation doesn\'t return this error to users.
_OperationStatusCheckFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationStatusCheckFailedException =
  Core._MatchServiceError
    defaultService
    "ConditionalCheckFailed"
    Prelude.. Core.hasStatus 400

-- | The specified stack instance doesn\'t exist.
_StackInstanceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackInstanceNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackInstanceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified stack ARN doesn\'t exist or stack doesn\'t exist
-- corresponding to the ARN in input.
_StackNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You can\'t yet delete this stack set, because it still contains one or
-- more stack instances. Delete all stack instances from the stack set
-- before deleting the stack set.
_StackSetNotEmptyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackSetNotEmptyException =
  Core._MatchServiceError
    defaultService
    "StackSetNotEmptyException"
    Prelude.. Core.hasStatus 409

-- | The specified stack set doesn\'t exist.
_StackSetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackSetNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Another operation has been performed on this stack set since the
-- specified operation was performed.
_StaleRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StaleRequestException =
  Core._MatchServiceError
    defaultService
    "StaleRequestException"
    Prelude.. Core.hasStatus 409

-- | A client request token already exists.
_TokenAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TokenAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TokenAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The specified extension configuration can\'t be found.
_TypeConfigurationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeConfigurationNotFoundException =
  Core._MatchServiceError
    defaultService
    "TypeConfigurationNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified extension doesn\'t exist in the CloudFormation registry.
_TypeNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "TypeNotFoundException"
    Prelude.. Core.hasStatus 404
