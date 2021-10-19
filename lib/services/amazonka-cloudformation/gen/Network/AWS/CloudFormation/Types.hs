{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TypeNotFoundException,
    _CreatedButModifiedException,
    _ChangeSetNotFoundException,
    _OperationInProgressException,
    _InvalidChangeSetStatusException,
    _OperationNotFoundException,
    _OperationIdAlreadyExistsException,
    _TypeConfigurationNotFoundException,
    _InsufficientCapabilitiesException,
    _TokenAlreadyExistsException,
    _StackNotFoundException,
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

    -- * IdentityProvider
    IdentityProvider (..),

    -- * OnFailure
    OnFailure (..),

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
    accountLimit_value,
    accountLimit_name,

    -- * AutoDeployment
    AutoDeployment (..),
    newAutoDeployment,
    autoDeployment_enabled,
    autoDeployment_retainStacksOnAccountRemoval,

    -- * BatchDescribeTypeConfigurationsError
    BatchDescribeTypeConfigurationsError (..),
    newBatchDescribeTypeConfigurationsError,
    batchDescribeTypeConfigurationsError_typeConfigurationIdentifier,
    batchDescribeTypeConfigurationsError_errorCode,
    batchDescribeTypeConfigurationsError_errorMessage,

    -- * Change
    Change (..),
    newChange,
    change_resourceChange,
    change_type,

    -- * ChangeSetSummary
    ChangeSetSummary (..),
    newChangeSetSummary,
    changeSetSummary_creationTime,
    changeSetSummary_status,
    changeSetSummary_parentChangeSetId,
    changeSetSummary_changeSetName,
    changeSetSummary_executionStatus,
    changeSetSummary_changeSetId,
    changeSetSummary_includeNestedStacks,
    changeSetSummary_rootChangeSetId,
    changeSetSummary_statusReason,
    changeSetSummary_stackId,
    changeSetSummary_description,
    changeSetSummary_stackName,

    -- * DeploymentTargets
    DeploymentTargets (..),
    newDeploymentTargets,
    deploymentTargets_accounts,
    deploymentTargets_organizationalUnitIds,
    deploymentTargets_accountsUrl,

    -- * Export
    Export (..),
    newExport,
    export_value,
    export_exportingStackId,
    export_name,

    -- * LoggingConfig
    LoggingConfig (..),
    newLoggingConfig,
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- * ModuleInfo
    ModuleInfo (..),
    newModuleInfo,
    moduleInfo_typeHierarchy,
    moduleInfo_logicalIdHierarchy,

    -- * Output
    Output (..),
    newOutput,
    output_outputValue,
    output_outputKey,
    output_exportName,
    output_description,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_parameterValue,
    parameter_resolvedValue,
    parameter_parameterKey,
    parameter_usePreviousValue,

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_allowedValues,

    -- * ParameterDeclaration
    ParameterDeclaration (..),
    newParameterDeclaration,
    parameterDeclaration_parameterKey,
    parameterDeclaration_parameterType,
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_defaultValue,
    parameterDeclaration_noEcho,
    parameterDeclaration_description,

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
    requiredActivatedType_typeNameAlias,
    requiredActivatedType_supportedMajorVersions,
    requiredActivatedType_publisherId,

    -- * ResourceChange
    ResourceChange (..),
    newResourceChange,
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_action,
    resourceChange_changeSetId,
    resourceChange_moduleInfo,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_replacement,

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
    resourceIdentifierSummary_resourceType,
    resourceIdentifierSummary_logicalResourceIds,
    resourceIdentifierSummary_resourceIdentifiers,

    -- * ResourceTargetDefinition
    ResourceTargetDefinition (..),
    newResourceTargetDefinition,
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,

    -- * ResourceToImport
    ResourceToImport (..),
    newResourceToImport,
    resourceToImport_resourceType,
    resourceToImport_logicalResourceId,
    resourceToImport_resourceIdentifier,

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    newRollbackConfiguration,
    rollbackConfiguration_rollbackTriggers,
    rollbackConfiguration_monitoringTimeInMinutes,

    -- * RollbackTrigger
    RollbackTrigger (..),
    newRollbackTrigger,
    rollbackTrigger_arn,
    rollbackTrigger_type,

    -- * Stack
    Stack (..),
    newStack,
    stack_disableRollback,
    stack_lastUpdatedTime,
    stack_rootId,
    stack_notificationARNs,
    stack_stackStatusReason,
    stack_enableTerminationProtection,
    stack_driftInformation,
    stack_changeSetId,
    stack_deletionTime,
    stack_outputs,
    stack_parameters,
    stack_stackId,
    stack_description,
    stack_capabilities,
    stack_rollbackConfiguration,
    stack_tags,
    stack_timeoutInMinutes,
    stack_parentId,
    stack_roleARN,
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
    stackEvent_logicalResourceId,
    stackEvent_physicalResourceId,
    stackEvent_resourceType,
    stackEvent_resourceStatusReason,
    stackEvent_resourceProperties,
    stackEvent_resourceStatus,
    stackEvent_clientRequestToken,
    stackEvent_stackId,
    stackEvent_eventId,
    stackEvent_stackName,
    stackEvent_timestamp,

    -- * StackInstance
    StackInstance (..),
    newStackInstance,
    stackInstance_status,
    stackInstance_lastDriftCheckTimestamp,
    stackInstance_account,
    stackInstance_driftStatus,
    stackInstance_organizationalUnitId,
    stackInstance_region,
    stackInstance_statusReason,
    stackInstance_stackId,
    stackInstance_stackInstanceStatus,
    stackInstance_parameterOverrides,
    stackInstance_stackSetId,

    -- * StackInstanceComprehensiveStatus
    StackInstanceComprehensiveStatus (..),
    newStackInstanceComprehensiveStatus,
    stackInstanceComprehensiveStatus_detailedStatus,

    -- * StackInstanceFilter
    StackInstanceFilter (..),
    newStackInstanceFilter,
    stackInstanceFilter_values,
    stackInstanceFilter_name,

    -- * StackInstanceSummary
    StackInstanceSummary (..),
    newStackInstanceSummary,
    stackInstanceSummary_status,
    stackInstanceSummary_lastDriftCheckTimestamp,
    stackInstanceSummary_account,
    stackInstanceSummary_driftStatus,
    stackInstanceSummary_organizationalUnitId,
    stackInstanceSummary_region,
    stackInstanceSummary_statusReason,
    stackInstanceSummary_stackId,
    stackInstanceSummary_stackInstanceStatus,
    stackInstanceSummary_stackSetId,

    -- * StackResource
    StackResource (..),
    newStackResource,
    stackResource_physicalResourceId,
    stackResource_resourceStatusReason,
    stackResource_driftInformation,
    stackResource_moduleInfo,
    stackResource_stackId,
    stackResource_description,
    stackResource_stackName,
    stackResource_logicalResourceId,
    stackResource_resourceType,
    stackResource_timestamp,
    stackResource_resourceStatus,

    -- * StackResourceDetail
    StackResourceDetail (..),
    newStackResourceDetail,
    stackResourceDetail_physicalResourceId,
    stackResourceDetail_resourceStatusReason,
    stackResourceDetail_driftInformation,
    stackResourceDetail_moduleInfo,
    stackResourceDetail_metadata,
    stackResourceDetail_stackId,
    stackResourceDetail_description,
    stackResourceDetail_stackName,
    stackResourceDetail_logicalResourceId,
    stackResourceDetail_resourceType,
    stackResourceDetail_lastUpdatedTimestamp,
    stackResourceDetail_resourceStatus,

    -- * StackResourceDrift
    StackResourceDrift (..),
    newStackResourceDrift,
    stackResourceDrift_actualProperties,
    stackResourceDrift_physicalResourceId,
    stackResourceDrift_physicalResourceIdContext,
    stackResourceDrift_propertyDifferences,
    stackResourceDrift_moduleInfo,
    stackResourceDrift_expectedProperties,
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
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_driftInformation,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- * StackSet
    StackSet (..),
    newStackSet,
    stackSet_stackSetDriftDetectionDetails,
    stackSet_status,
    stackSet_administrationRoleARN,
    stackSet_autoDeployment,
    stackSet_organizationalUnitIds,
    stackSet_stackSetARN,
    stackSet_permissionModel,
    stackSet_parameters,
    stackSet_templateBody,
    stackSet_stackSetName,
    stackSet_description,
    stackSet_capabilities,
    stackSet_tags,
    stackSet_stackSetId,
    stackSet_executionRoleName,

    -- * StackSetDriftDetectionDetails
    StackSetDriftDetectionDetails (..),
    newStackSetDriftDetectionDetails,
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,
    stackSetDriftDetectionDetails_totalStackInstancesCount,
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,

    -- * StackSetOperation
    StackSetOperation (..),
    newStackSetOperation,
    stackSetOperation_stackSetDriftDetectionDetails,
    stackSetOperation_status,
    stackSetOperation_administrationRoleARN,
    stackSetOperation_action,
    stackSetOperation_endTimestamp,
    stackSetOperation_creationTimestamp,
    stackSetOperation_operationPreferences,
    stackSetOperation_operationId,
    stackSetOperation_retainStacks,
    stackSetOperation_deploymentTargets,
    stackSetOperation_stackSetId,
    stackSetOperation_executionRoleName,

    -- * StackSetOperationPreferences
    StackSetOperationPreferences (..),
    newStackSetOperationPreferences,
    stackSetOperationPreferences_regionOrder,
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_maxConcurrentPercentage,
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_regionConcurrencyType,
    stackSetOperationPreferences_failureTolerancePercentage,

    -- * StackSetOperationResultSummary
    StackSetOperationResultSummary (..),
    newStackSetOperationResultSummary,
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_accountGateResult,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_statusReason,

    -- * StackSetOperationSummary
    StackSetOperationSummary (..),
    newStackSetOperationSummary,
    stackSetOperationSummary_status,
    stackSetOperationSummary_action,
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_operationId,

    -- * StackSetSummary
    StackSetSummary (..),
    newStackSetSummary,
    stackSetSummary_status,
    stackSetSummary_lastDriftCheckTimestamp,
    stackSetSummary_autoDeployment,
    stackSetSummary_driftStatus,
    stackSetSummary_permissionModel,
    stackSetSummary_stackSetName,
    stackSetSummary_description,
    stackSetSummary_stackSetId,

    -- * StackSummary
    StackSummary (..),
    newStackSummary,
    stackSummary_lastUpdatedTime,
    stackSummary_rootId,
    stackSummary_stackStatusReason,
    stackSummary_templateDescription,
    stackSummary_driftInformation,
    stackSummary_deletionTime,
    stackSummary_stackId,
    stackSummary_parentId,
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
    templateParameter_parameterKey,
    templateParameter_defaultValue,
    templateParameter_noEcho,
    templateParameter_description,

    -- * TypeConfigurationDetails
    TypeConfigurationDetails (..),
    newTypeConfigurationDetails,
    typeConfigurationDetails_lastUpdated,
    typeConfigurationDetails_typeName,
    typeConfigurationDetails_arn,
    typeConfigurationDetails_alias,
    typeConfigurationDetails_typeArn,
    typeConfigurationDetails_configuration,
    typeConfigurationDetails_isDefaultConfiguration,

    -- * TypeConfigurationIdentifier
    TypeConfigurationIdentifier (..),
    newTypeConfigurationIdentifier,
    typeConfigurationIdentifier_typeName,
    typeConfigurationIdentifier_typeConfigurationArn,
    typeConfigurationIdentifier_typeConfigurationAlias,
    typeConfigurationIdentifier_typeArn,
    typeConfigurationIdentifier_type,

    -- * TypeFilters
    TypeFilters (..),
    newTypeFilters,
    typeFilters_typeNamePrefix,
    typeFilters_category,
    typeFilters_publisherId,

    -- * TypeSummary
    TypeSummary (..),
    newTypeSummary,
    typeSummary_lastUpdated,
    typeSummary_typeName,
    typeSummary_publicVersionNumber,
    typeSummary_publisherIdentity,
    typeSummary_originalTypeName,
    typeSummary_defaultVersionId,
    typeSummary_isActivated,
    typeSummary_typeArn,
    typeSummary_type,
    typeSummary_latestPublicVersion,
    typeSummary_publisherId,
    typeSummary_description,
    typeSummary_publisherName,

    -- * TypeVersionSummary
    TypeVersionSummary (..),
    newTypeVersionSummary,
    typeVersionSummary_versionId,
    typeVersionSummary_typeName,
    typeVersionSummary_arn,
    typeVersionSummary_publicVersionNumber,
    typeVersionSummary_timeCreated,
    typeVersionSummary_type,
    typeVersionSummary_isDefaultVersion,
    typeVersionSummary_description,
  )
where

import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.AccountGateStatus
import Network.AWS.CloudFormation.Types.AccountLimit
import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.BatchDescribeTypeConfigurationsError
import Network.AWS.CloudFormation.Types.CallAs
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Category
import Network.AWS.CloudFormation.Types.Change
import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ChangeSetSummary
import Network.AWS.CloudFormation.Types.ChangeSetType
import Network.AWS.CloudFormation.Types.ChangeSource
import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.DeprecatedStatus
import Network.AWS.CloudFormation.Types.DifferenceType
import Network.AWS.CloudFormation.Types.EvaluationType
import Network.AWS.CloudFormation.Types.ExecutionStatus
import Network.AWS.CloudFormation.Types.Export
import Network.AWS.CloudFormation.Types.HandlerErrorCode
import Network.AWS.CloudFormation.Types.IdentityProvider
import Network.AWS.CloudFormation.Types.LoggingConfig
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.OnFailure
import Network.AWS.CloudFormation.Types.OperationStatus
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.ParameterConstraints
import Network.AWS.CloudFormation.Types.ParameterDeclaration
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.ProvisioningType
import Network.AWS.CloudFormation.Types.PublisherStatus
import Network.AWS.CloudFormation.Types.RegionConcurrencyType
import Network.AWS.CloudFormation.Types.RegistrationStatus
import Network.AWS.CloudFormation.Types.RegistryType
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.RequiredActivatedType
import Network.AWS.CloudFormation.Types.RequiresRecreation
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChange
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
import Network.AWS.CloudFormation.Types.ResourceSignalStatus
import Network.AWS.CloudFormation.Types.ResourceStatus
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import Network.AWS.CloudFormation.Types.ResourceToImport
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.RollbackTrigger
import Network.AWS.CloudFormation.Types.Stack
import Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackEvent
import Network.AWS.CloudFormation.Types.StackInstance
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import Network.AWS.CloudFormation.Types.StackInstanceFilter
import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.CloudFormation.Types.StackInstanceSummary
import Network.AWS.CloudFormation.Types.StackResource
import Network.AWS.CloudFormation.Types.StackResourceDetail
import Network.AWS.CloudFormation.Types.StackResourceDrift
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.CloudFormation.Types.StackResourceSummary
import Network.AWS.CloudFormation.Types.StackSet
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
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
import Network.AWS.CloudFormation.Types.StackSummary
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.CloudFormation.Types.TemplateParameter
import Network.AWS.CloudFormation.Types.TemplateStage
import Network.AWS.CloudFormation.Types.ThirdPartyType
import Network.AWS.CloudFormation.Types.TypeConfigurationDetails
import Network.AWS.CloudFormation.Types.TypeConfigurationIdentifier
import Network.AWS.CloudFormation.Types.TypeFilters
import Network.AWS.CloudFormation.Types.TypeSummary
import Network.AWS.CloudFormation.Types.TypeTestsStatus
import Network.AWS.CloudFormation.Types.TypeVersionSummary
import Network.AWS.CloudFormation.Types.VersionBump
import Network.AWS.CloudFormation.Types.Visibility
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-05-15@ of the Amazon CloudFormation SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CloudFormation",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudformation",
      Core._serviceSigningName = "cloudformation",
      Core._serviceVersion = "2010-05-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseXMLError "CloudFormation",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified extension does not exist in the CloudFormation registry.
_TypeNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "TypeNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified resource exists, but has been changed.
_CreatedButModifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CreatedButModifiedException =
  Core._MatchServiceError
    defaultService
    "CreatedButModifiedException"
    Prelude.. Core.hasStatus 409

-- | The specified change set name or ID doesn\'t exit. To view valid change
-- sets for a stack, use the @ListChangeSets@ action.
_ChangeSetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ChangeSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "ChangeSetNotFound"
    Prelude.. Core.hasStatus 404

-- | Another operation is currently in progress for this stack set. Only one
-- operation can be performed for a stack set at a given time.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressException"
    Prelude.. Core.hasStatus 409

-- | The specified change set can\'t be used to update the stack. For
-- example, the change set status might be @CREATE_IN_PROGRESS@, or the
-- stack status might be @UPDATE_IN_PROGRESS@.
_InvalidChangeSetStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidChangeSetStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidChangeSetStatus"
    Prelude.. Core.hasStatus 400

-- | The specified ID refers to an operation that doesn\'t exist.
_OperationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotFoundException =
  Core._MatchServiceError
    defaultService
    "OperationNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified operation ID already exists.
_OperationIdAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationIdAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "OperationIdAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The specified extension configuration cannot be found.
_TypeConfigurationNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeConfigurationNotFoundException =
  Core._MatchServiceError
    defaultService
    "TypeConfigurationNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The template contains resources with capabilities that weren\'t
-- specified in the Capabilities parameter.
_InsufficientCapabilitiesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientCapabilitiesException =
  Core._MatchServiceError
    defaultService
    "InsufficientCapabilitiesException"
    Prelude.. Core.hasStatus 400

-- | A client request token already exists.
_TokenAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TokenAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TokenAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The specified stack ARN doesn’t exist or stack doesn’t exist
-- corresponding to the ARN in input.
_StackNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified stack set doesn\'t exist.
_StackSetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackSetNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackSetNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified stack instance doesn\'t exist.
_StackInstanceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackInstanceNotFoundException =
  Core._MatchServiceError
    defaultService
    "StackInstanceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Error reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- CloudFormation does not return this error to users.
_OperationStatusCheckFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationStatusCheckFailedException =
  Core._MatchServiceError
    defaultService
    "ConditionalCheckFailed"
    Prelude.. Core.hasStatus 400

-- | You can\'t yet delete this stack set, because it still contains one or
-- more stack instances. Delete all stack instances from the stack set
-- before deleting the stack set.
_StackSetNotEmptyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StackSetNotEmptyException =
  Core._MatchServiceError
    defaultService
    "StackSetNotEmptyException"
    Prelude.. Core.hasStatus 409

-- | The specified operation isn\'t valid.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"
    Prelude.. Core.hasStatus 400

-- | Error reserved for use by the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI>.
-- CloudFormation does not return this error to users.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransition"
    Prelude.. Core.hasStatus 400

-- | The specified name is already in use.
_NameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "NameAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | An error occurred during a CloudFormation registry operation.
_CFNRegistryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CFNRegistryException =
  Core._MatchServiceError
    defaultService
    "CFNRegistryException"
    Prelude.. Core.hasStatus 400

-- | Another operation has been performed on this stack set since the
-- specified operation was performed.
_StaleRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StaleRequestException =
  Core._MatchServiceError
    defaultService
    "StaleRequestException"
    Prelude.. Core.hasStatus 409

-- | The resource with the name requested already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The quota for the resource has already been reached.
--
-- For information on resource and stack limitations, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html Limits>
-- in the /CloudFormation User Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400
