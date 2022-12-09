{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFormation.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Lens
  ( -- * Operations

    -- ** ActivateType
    activateType_autoUpdate,
    activateType_executionRoleArn,
    activateType_loggingConfig,
    activateType_majorVersion,
    activateType_publicTypeArn,
    activateType_publisherId,
    activateType_type,
    activateType_typeName,
    activateType_typeNameAlias,
    activateType_versionBump,
    activateTypeResponse_arn,
    activateTypeResponse_httpStatus,

    -- ** BatchDescribeTypeConfigurations
    batchDescribeTypeConfigurations_typeConfigurationIdentifiers,
    batchDescribeTypeConfigurationsResponse_errors,
    batchDescribeTypeConfigurationsResponse_typeConfigurations,
    batchDescribeTypeConfigurationsResponse_unprocessedTypeConfigurations,
    batchDescribeTypeConfigurationsResponse_httpStatus,

    -- ** CancelUpdateStack
    cancelUpdateStack_clientRequestToken,
    cancelUpdateStack_stackName,

    -- ** ContinueUpdateRollback
    continueUpdateRollback_clientRequestToken,
    continueUpdateRollback_resourcesToSkip,
    continueUpdateRollback_roleARN,
    continueUpdateRollback_stackName,
    continueUpdateRollbackResponse_httpStatus,

    -- ** CreateChangeSet
    createChangeSet_capabilities,
    createChangeSet_changeSetType,
    createChangeSet_clientToken,
    createChangeSet_description,
    createChangeSet_includeNestedStacks,
    createChangeSet_notificationARNs,
    createChangeSet_parameters,
    createChangeSet_resourceTypes,
    createChangeSet_resourcesToImport,
    createChangeSet_roleARN,
    createChangeSet_rollbackConfiguration,
    createChangeSet_tags,
    createChangeSet_templateBody,
    createChangeSet_templateURL,
    createChangeSet_usePreviousTemplate,
    createChangeSet_stackName,
    createChangeSet_changeSetName,
    createChangeSetResponse_id,
    createChangeSetResponse_stackId,
    createChangeSetResponse_httpStatus,

    -- ** CreateStack
    createStack_capabilities,
    createStack_clientRequestToken,
    createStack_disableRollback,
    createStack_enableTerminationProtection,
    createStack_notificationARNs,
    createStack_onFailure,
    createStack_parameters,
    createStack_resourceTypes,
    createStack_roleARN,
    createStack_rollbackConfiguration,
    createStack_stackPolicyBody,
    createStack_stackPolicyURL,
    createStack_tags,
    createStack_templateBody,
    createStack_templateURL,
    createStack_timeoutInMinutes,
    createStack_stackName,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** CreateStackInstances
    createStackInstances_accounts,
    createStackInstances_callAs,
    createStackInstances_deploymentTargets,
    createStackInstances_operationId,
    createStackInstances_operationPreferences,
    createStackInstances_parameterOverrides,
    createStackInstances_stackSetName,
    createStackInstances_regions,
    createStackInstancesResponse_operationId,
    createStackInstancesResponse_httpStatus,

    -- ** CreateStackSet
    createStackSet_administrationRoleARN,
    createStackSet_autoDeployment,
    createStackSet_callAs,
    createStackSet_capabilities,
    createStackSet_clientRequestToken,
    createStackSet_description,
    createStackSet_executionRoleName,
    createStackSet_managedExecution,
    createStackSet_parameters,
    createStackSet_permissionModel,
    createStackSet_stackId,
    createStackSet_tags,
    createStackSet_templateBody,
    createStackSet_templateURL,
    createStackSet_stackSetName,
    createStackSetResponse_stackSetId,
    createStackSetResponse_httpStatus,

    -- ** DeactivateType
    deactivateType_arn,
    deactivateType_type,
    deactivateType_typeName,
    deactivateTypeResponse_httpStatus,

    -- ** DeleteChangeSet
    deleteChangeSet_stackName,
    deleteChangeSet_changeSetName,
    deleteChangeSetResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_clientRequestToken,
    deleteStack_retainResources,
    deleteStack_roleARN,
    deleteStack_stackName,

    -- ** DeleteStackInstances
    deleteStackInstances_accounts,
    deleteStackInstances_callAs,
    deleteStackInstances_deploymentTargets,
    deleteStackInstances_operationId,
    deleteStackInstances_operationPreferences,
    deleteStackInstances_stackSetName,
    deleteStackInstances_regions,
    deleteStackInstances_retainStacks,
    deleteStackInstancesResponse_operationId,
    deleteStackInstancesResponse_httpStatus,

    -- ** DeleteStackSet
    deleteStackSet_callAs,
    deleteStackSet_stackSetName,
    deleteStackSetResponse_httpStatus,

    -- ** DeregisterType
    deregisterType_arn,
    deregisterType_type,
    deregisterType_typeName,
    deregisterType_versionId,
    deregisterTypeResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_nextToken,
    describeChangeSet_stackName,
    describeChangeSet_changeSetName,
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_description,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,

    -- ** DescribeChangeSetHooks
    describeChangeSetHooks_logicalResourceId,
    describeChangeSetHooks_nextToken,
    describeChangeSetHooks_stackName,
    describeChangeSetHooks_changeSetName,
    describeChangeSetHooksResponse_changeSetId,
    describeChangeSetHooksResponse_changeSetName,
    describeChangeSetHooksResponse_hooks,
    describeChangeSetHooksResponse_nextToken,
    describeChangeSetHooksResponse_stackId,
    describeChangeSetHooksResponse_stackName,
    describeChangeSetHooksResponse_status,
    describeChangeSetHooksResponse_httpStatus,

    -- ** DescribePublisher
    describePublisher_publisherId,
    describePublisherResponse_identityProvider,
    describePublisherResponse_publisherId,
    describePublisherResponse_publisherProfile,
    describePublisherResponse_publisherStatus,
    describePublisherResponse_httpStatus,

    -- ** DescribeStackDriftDetectionStatus
    describeStackDriftDetectionStatus_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_detectionStatusReason,
    describeStackDriftDetectionStatusResponse_driftedStackResourceCount,
    describeStackDriftDetectionStatusResponse_stackDriftStatus,
    describeStackDriftDetectionStatusResponse_httpStatus,
    describeStackDriftDetectionStatusResponse_stackId,
    describeStackDriftDetectionStatusResponse_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_detectionStatus,
    describeStackDriftDetectionStatusResponse_timestamp,

    -- ** DescribeStackEvents
    describeStackEvents_nextToken,
    describeStackEvents_stackName,
    describeStackEventsResponse_nextToken,
    describeStackEventsResponse_stackEvents,
    describeStackEventsResponse_httpStatus,

    -- ** DescribeStackInstance
    describeStackInstance_callAs,
    describeStackInstance_stackSetName,
    describeStackInstance_stackInstanceAccount,
    describeStackInstance_stackInstanceRegion,
    describeStackInstanceResponse_stackInstance,
    describeStackInstanceResponse_httpStatus,

    -- ** DescribeStackResource
    describeStackResource_stackName,
    describeStackResource_logicalResourceId,
    describeStackResourceResponse_stackResourceDetail,
    describeStackResourceResponse_httpStatus,

    -- ** DescribeStackResourceDrifts
    describeStackResourceDrifts_maxResults,
    describeStackResourceDrifts_nextToken,
    describeStackResourceDrifts_stackResourceDriftStatusFilters,
    describeStackResourceDrifts_stackName,
    describeStackResourceDriftsResponse_nextToken,
    describeStackResourceDriftsResponse_httpStatus,
    describeStackResourceDriftsResponse_stackResourceDrifts,

    -- ** DescribeStackResources
    describeStackResources_logicalResourceId,
    describeStackResources_physicalResourceId,
    describeStackResources_stackName,
    describeStackResourcesResponse_stackResources,
    describeStackResourcesResponse_httpStatus,

    -- ** DescribeStackSet
    describeStackSet_callAs,
    describeStackSet_stackSetName,
    describeStackSetResponse_stackSet,
    describeStackSetResponse_httpStatus,

    -- ** DescribeStackSetOperation
    describeStackSetOperation_callAs,
    describeStackSetOperation_stackSetName,
    describeStackSetOperation_operationId,
    describeStackSetOperationResponse_stackSetOperation,
    describeStackSetOperationResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_nextToken,
    describeStacks_stackName,
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DescribeType
    describeType_arn,
    describeType_publicVersionNumber,
    describeType_publisherId,
    describeType_type,
    describeType_typeName,
    describeType_versionId,
    describeTypeResponse_arn,
    describeTypeResponse_autoUpdate,
    describeTypeResponse_configurationSchema,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_description,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_isActivated,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_latestPublicVersion,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_originalTypeArn,
    describeTypeResponse_originalTypeName,
    describeTypeResponse_provisioningType,
    describeTypeResponse_publicVersionNumber,
    describeTypeResponse_publisherId,
    describeTypeResponse_requiredActivatedTypes,
    describeTypeResponse_schema,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_timeCreated,
    describeTypeResponse_type,
    describeTypeResponse_typeName,
    describeTypeResponse_typeTestsStatus,
    describeTypeResponse_typeTestsStatusDescription,
    describeTypeResponse_visibility,
    describeTypeResponse_httpStatus,

    -- ** DescribeTypeRegistration
    describeTypeRegistration_registrationToken,
    describeTypeRegistrationResponse_description,
    describeTypeRegistrationResponse_progressStatus,
    describeTypeRegistrationResponse_typeArn,
    describeTypeRegistrationResponse_typeVersionArn,
    describeTypeRegistrationResponse_httpStatus,

    -- ** DetectStackDrift
    detectStackDrift_logicalResourceIds,
    detectStackDrift_stackName,
    detectStackDriftResponse_httpStatus,
    detectStackDriftResponse_stackDriftDetectionId,

    -- ** DetectStackResourceDrift
    detectStackResourceDrift_stackName,
    detectStackResourceDrift_logicalResourceId,
    detectStackResourceDriftResponse_httpStatus,
    detectStackResourceDriftResponse_stackResourceDrift,

    -- ** DetectStackSetDrift
    detectStackSetDrift_callAs,
    detectStackSetDrift_operationId,
    detectStackSetDrift_operationPreferences,
    detectStackSetDrift_stackSetName,
    detectStackSetDriftResponse_operationId,
    detectStackSetDriftResponse_httpStatus,

    -- ** EstimateTemplateCost
    estimateTemplateCost_parameters,
    estimateTemplateCost_templateBody,
    estimateTemplateCost_templateURL,
    estimateTemplateCostResponse_url,
    estimateTemplateCostResponse_httpStatus,

    -- ** ExecuteChangeSet
    executeChangeSet_clientRequestToken,
    executeChangeSet_disableRollback,
    executeChangeSet_stackName,
    executeChangeSet_changeSetName,
    executeChangeSetResponse_httpStatus,

    -- ** GetStackPolicy
    getStackPolicy_stackName,
    getStackPolicyResponse_stackPolicyBody,
    getStackPolicyResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_changeSetName,
    getTemplate_stackName,
    getTemplate_templateStage,
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_templateBody,
    getTemplateResponse_httpStatus,

    -- ** GetTemplateSummary
    getTemplateSummary_callAs,
    getTemplateSummary_stackName,
    getTemplateSummary_stackSetName,
    getTemplateSummary_templateBody,
    getTemplateSummary_templateURL,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_httpStatus,

    -- ** ImportStacksToStackSet
    importStacksToStackSet_callAs,
    importStacksToStackSet_operationId,
    importStacksToStackSet_operationPreferences,
    importStacksToStackSet_organizationalUnitIds,
    importStacksToStackSet_stackIds,
    importStacksToStackSet_stackIdsUrl,
    importStacksToStackSet_stackSetName,
    importStacksToStackSetResponse_operationId,
    importStacksToStackSetResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_nextToken,
    listChangeSets_stackName,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_summaries,
    listChangeSetsResponse_httpStatus,

    -- ** ListExports
    listExports_nextToken,
    listExportsResponse_exports,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** ListImports
    listImports_nextToken,
    listImports_exportName,
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListStackInstances
    listStackInstances_callAs,
    listStackInstances_filters,
    listStackInstances_maxResults,
    listStackInstances_nextToken,
    listStackInstances_stackInstanceAccount,
    listStackInstances_stackInstanceRegion,
    listStackInstances_stackSetName,
    listStackInstancesResponse_nextToken,
    listStackInstancesResponse_summaries,
    listStackInstancesResponse_httpStatus,

    -- ** ListStackResources
    listStackResources_nextToken,
    listStackResources_stackName,
    listStackResourcesResponse_nextToken,
    listStackResourcesResponse_stackResourceSummaries,
    listStackResourcesResponse_httpStatus,

    -- ** ListStackSetOperationResults
    listStackSetOperationResults_callAs,
    listStackSetOperationResults_filters,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_stackSetName,
    listStackSetOperationResults_operationId,
    listStackSetOperationResultsResponse_nextToken,
    listStackSetOperationResultsResponse_summaries,
    listStackSetOperationResultsResponse_httpStatus,

    -- ** ListStackSetOperations
    listStackSetOperations_callAs,
    listStackSetOperations_maxResults,
    listStackSetOperations_nextToken,
    listStackSetOperations_stackSetName,
    listStackSetOperationsResponse_nextToken,
    listStackSetOperationsResponse_summaries,
    listStackSetOperationsResponse_httpStatus,

    -- ** ListStackSets
    listStackSets_callAs,
    listStackSets_maxResults,
    listStackSets_nextToken,
    listStackSets_status,
    listStackSetsResponse_nextToken,
    listStackSetsResponse_summaries,
    listStackSetsResponse_httpStatus,

    -- ** ListStacks
    listStacks_nextToken,
    listStacks_stackStatusFilter,
    listStacksResponse_nextToken,
    listStacksResponse_stackSummaries,
    listStacksResponse_httpStatus,

    -- ** ListTypeRegistrations
    listTypeRegistrations_maxResults,
    listTypeRegistrations_nextToken,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_type,
    listTypeRegistrations_typeArn,
    listTypeRegistrations_typeName,
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_httpStatus,

    -- ** ListTypeVersions
    listTypeVersions_arn,
    listTypeVersions_deprecatedStatus,
    listTypeVersions_maxResults,
    listTypeVersions_nextToken,
    listTypeVersions_publisherId,
    listTypeVersions_type,
    listTypeVersions_typeName,
    listTypeVersionsResponse_nextToken,
    listTypeVersionsResponse_typeVersionSummaries,
    listTypeVersionsResponse_httpStatus,

    -- ** ListTypes
    listTypes_deprecatedStatus,
    listTypes_filters,
    listTypes_maxResults,
    listTypes_nextToken,
    listTypes_provisioningType,
    listTypes_type,
    listTypes_visibility,
    listTypesResponse_nextToken,
    listTypesResponse_typeSummaries,
    listTypesResponse_httpStatus,

    -- ** PublishType
    publishType_arn,
    publishType_publicVersionNumber,
    publishType_type,
    publishType_typeName,
    publishTypeResponse_publicTypeArn,
    publishTypeResponse_httpStatus,

    -- ** RecordHandlerProgress
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_errorCode,
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_statusMessage,
    recordHandlerProgress_bearerToken,
    recordHandlerProgress_operationStatus,
    recordHandlerProgressResponse_httpStatus,

    -- ** RegisterPublisher
    registerPublisher_acceptTermsAndConditions,
    registerPublisher_connectionArn,
    registerPublisherResponse_publisherId,
    registerPublisherResponse_httpStatus,

    -- ** RegisterType
    registerType_clientRequestToken,
    registerType_executionRoleArn,
    registerType_loggingConfig,
    registerType_type,
    registerType_typeName,
    registerType_schemaHandlerPackage,
    registerTypeResponse_registrationToken,
    registerTypeResponse_httpStatus,

    -- ** RollbackStack
    rollbackStack_clientRequestToken,
    rollbackStack_roleARN,
    rollbackStack_stackName,
    rollbackStackResponse_stackId,
    rollbackStackResponse_httpStatus,

    -- ** SetStackPolicy
    setStackPolicy_stackPolicyBody,
    setStackPolicy_stackPolicyURL,
    setStackPolicy_stackName,

    -- ** SetTypeConfiguration
    setTypeConfiguration_configurationAlias,
    setTypeConfiguration_type,
    setTypeConfiguration_typeArn,
    setTypeConfiguration_typeName,
    setTypeConfiguration_configuration,
    setTypeConfigurationResponse_configurationArn,
    setTypeConfigurationResponse_httpStatus,

    -- ** SetTypeDefaultVersion
    setTypeDefaultVersion_arn,
    setTypeDefaultVersion_type,
    setTypeDefaultVersion_typeName,
    setTypeDefaultVersion_versionId,
    setTypeDefaultVersionResponse_httpStatus,

    -- ** SignalResource
    signalResource_stackName,
    signalResource_logicalResourceId,
    signalResource_uniqueId,
    signalResource_status,

    -- ** StopStackSetOperation
    stopStackSetOperation_callAs,
    stopStackSetOperation_stackSetName,
    stopStackSetOperation_operationId,
    stopStackSetOperationResponse_httpStatus,

    -- ** TestType
    testType_arn,
    testType_logDeliveryBucket,
    testType_type,
    testType_typeName,
    testType_versionId,
    testTypeResponse_typeVersionArn,
    testTypeResponse_httpStatus,

    -- ** UpdateStack
    updateStack_capabilities,
    updateStack_clientRequestToken,
    updateStack_disableRollback,
    updateStack_notificationARNs,
    updateStack_parameters,
    updateStack_resourceTypes,
    updateStack_roleARN,
    updateStack_rollbackConfiguration,
    updateStack_stackPolicyBody,
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_stackPolicyURL,
    updateStack_tags,
    updateStack_templateBody,
    updateStack_templateURL,
    updateStack_usePreviousTemplate,
    updateStack_stackName,
    updateStackResponse_stackId,
    updateStackResponse_httpStatus,

    -- ** UpdateStackInstances
    updateStackInstances_accounts,
    updateStackInstances_callAs,
    updateStackInstances_deploymentTargets,
    updateStackInstances_operationId,
    updateStackInstances_operationPreferences,
    updateStackInstances_parameterOverrides,
    updateStackInstances_stackSetName,
    updateStackInstances_regions,
    updateStackInstancesResponse_operationId,
    updateStackInstancesResponse_httpStatus,

    -- ** UpdateStackSet
    updateStackSet_accounts,
    updateStackSet_administrationRoleARN,
    updateStackSet_autoDeployment,
    updateStackSet_callAs,
    updateStackSet_capabilities,
    updateStackSet_deploymentTargets,
    updateStackSet_description,
    updateStackSet_executionRoleName,
    updateStackSet_managedExecution,
    updateStackSet_operationId,
    updateStackSet_operationPreferences,
    updateStackSet_parameters,
    updateStackSet_permissionModel,
    updateStackSet_regions,
    updateStackSet_tags,
    updateStackSet_templateBody,
    updateStackSet_templateURL,
    updateStackSet_usePreviousTemplate,
    updateStackSet_stackSetName,
    updateStackSetResponse_operationId,
    updateStackSetResponse_httpStatus,

    -- ** UpdateTerminationProtection
    updateTerminationProtection_enableTerminationProtection,
    updateTerminationProtection_stackName,
    updateTerminationProtectionResponse_stackId,
    updateTerminationProtectionResponse_httpStatus,

    -- ** ValidateTemplate
    validateTemplate_templateBody,
    validateTemplate_templateURL,
    validateTemplateResponse_capabilities,
    validateTemplateResponse_capabilitiesReason,
    validateTemplateResponse_declaredTransforms,
    validateTemplateResponse_description,
    validateTemplateResponse_parameters,
    validateTemplateResponse_httpStatus,

    -- * Types

    -- ** AccountGateResult
    accountGateResult_status,
    accountGateResult_statusReason,

    -- ** AccountLimit
    accountLimit_name,
    accountLimit_value,

    -- ** AutoDeployment
    autoDeployment_enabled,
    autoDeployment_retainStacksOnAccountRemoval,

    -- ** BatchDescribeTypeConfigurationsError
    batchDescribeTypeConfigurationsError_errorCode,
    batchDescribeTypeConfigurationsError_errorMessage,
    batchDescribeTypeConfigurationsError_typeConfigurationIdentifier,

    -- ** Change
    change_hookInvocationCount,
    change_resourceChange,
    change_type,

    -- ** ChangeSetHook
    changeSetHook_failureMode,
    changeSetHook_invocationPoint,
    changeSetHook_targetDetails,
    changeSetHook_typeConfigurationVersionId,
    changeSetHook_typeName,
    changeSetHook_typeVersionId,

    -- ** ChangeSetHookResourceTargetDetails
    changeSetHookResourceTargetDetails_logicalResourceId,
    changeSetHookResourceTargetDetails_resourceAction,
    changeSetHookResourceTargetDetails_resourceType,

    -- ** ChangeSetHookTargetDetails
    changeSetHookTargetDetails_resourceTargetDetails,
    changeSetHookTargetDetails_targetType,

    -- ** ChangeSetSummary
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

    -- ** DeploymentTargets
    deploymentTargets_accountFilterType,
    deploymentTargets_accounts,
    deploymentTargets_accountsUrl,
    deploymentTargets_organizationalUnitIds,

    -- ** Export
    export_exportingStackId,
    export_name,
    export_value,

    -- ** LoggingConfig
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- ** ManagedExecution
    managedExecution_active,

    -- ** ModuleInfo
    moduleInfo_logicalIdHierarchy,
    moduleInfo_typeHierarchy,

    -- ** OperationResultFilter
    operationResultFilter_name,
    operationResultFilter_values,

    -- ** Output
    output_description,
    output_exportName,
    output_outputKey,
    output_outputValue,

    -- ** Parameter
    parameter_parameterKey,
    parameter_parameterValue,
    parameter_resolvedValue,
    parameter_usePreviousValue,

    -- ** ParameterConstraints
    parameterConstraints_allowedValues,

    -- ** ParameterDeclaration
    parameterDeclaration_defaultValue,
    parameterDeclaration_description,
    parameterDeclaration_noEcho,
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_parameterKey,
    parameterDeclaration_parameterType,

    -- ** PhysicalResourceIdContextKeyValuePair
    physicalResourceIdContextKeyValuePair_key,
    physicalResourceIdContextKeyValuePair_value,

    -- ** PropertyDifference
    propertyDifference_propertyPath,
    propertyDifference_expectedValue,
    propertyDifference_actualValue,
    propertyDifference_differenceType,

    -- ** RequiredActivatedType
    requiredActivatedType_originalTypeName,
    requiredActivatedType_publisherId,
    requiredActivatedType_supportedMajorVersions,
    requiredActivatedType_typeNameAlias,

    -- ** ResourceChange
    resourceChange_action,
    resourceChange_changeSetId,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_moduleInfo,
    resourceChange_physicalResourceId,
    resourceChange_replacement,
    resourceChange_resourceType,
    resourceChange_scope,

    -- ** ResourceChangeDetail
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_changeSource,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- ** ResourceIdentifierSummary
    resourceIdentifierSummary_logicalResourceIds,
    resourceIdentifierSummary_resourceIdentifiers,
    resourceIdentifierSummary_resourceType,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_name,
    resourceTargetDefinition_requiresRecreation,

    -- ** ResourceToImport
    resourceToImport_resourceType,
    resourceToImport_logicalResourceId,
    resourceToImport_resourceIdentifier,

    -- ** RollbackConfiguration
    rollbackConfiguration_monitoringTimeInMinutes,
    rollbackConfiguration_rollbackTriggers,

    -- ** RollbackTrigger
    rollbackTrigger_arn,
    rollbackTrigger_type,

    -- ** Stack
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

    -- ** StackDriftInformation
    stackDriftInformation_lastCheckTimestamp,
    stackDriftInformation_stackDriftStatus,

    -- ** StackDriftInformationSummary
    stackDriftInformationSummary_lastCheckTimestamp,
    stackDriftInformationSummary_stackDriftStatus,

    -- ** StackEvent
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

    -- ** StackInstance
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

    -- ** StackInstanceComprehensiveStatus
    stackInstanceComprehensiveStatus_detailedStatus,

    -- ** StackInstanceFilter
    stackInstanceFilter_name,
    stackInstanceFilter_values,

    -- ** StackInstanceSummary
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

    -- ** StackResource
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

    -- ** StackResourceDetail
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

    -- ** StackResourceDrift
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

    -- ** StackResourceDriftInformation
    stackResourceDriftInformation_lastCheckTimestamp,
    stackResourceDriftInformation_stackResourceDriftStatus,

    -- ** StackResourceDriftInformationSummary
    stackResourceDriftInformationSummary_lastCheckTimestamp,
    stackResourceDriftInformationSummary_stackResourceDriftStatus,

    -- ** StackResourceSummary
    stackResourceSummary_driftInformation,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- ** StackSet
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

    -- ** StackSetDriftDetectionDetails
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,
    stackSetDriftDetectionDetails_totalStackInstancesCount,

    -- ** StackSetOperation
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

    -- ** StackSetOperationPreferences
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_failureTolerancePercentage,
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_maxConcurrentPercentage,
    stackSetOperationPreferences_regionConcurrencyType,
    stackSetOperationPreferences_regionOrder,

    -- ** StackSetOperationResultSummary
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_accountGateResult,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_statusReason,

    -- ** StackSetOperationStatusDetails
    stackSetOperationStatusDetails_failedStackInstancesCount,

    -- ** StackSetOperationSummary
    stackSetOperationSummary_action,
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_operationId,
    stackSetOperationSummary_operationPreferences,
    stackSetOperationSummary_status,
    stackSetOperationSummary_statusDetails,
    stackSetOperationSummary_statusReason,

    -- ** StackSetSummary
    stackSetSummary_autoDeployment,
    stackSetSummary_description,
    stackSetSummary_driftStatus,
    stackSetSummary_lastDriftCheckTimestamp,
    stackSetSummary_managedExecution,
    stackSetSummary_permissionModel,
    stackSetSummary_stackSetId,
    stackSetSummary_stackSetName,
    stackSetSummary_status,

    -- ** StackSummary
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateParameter
    templateParameter_defaultValue,
    templateParameter_description,
    templateParameter_noEcho,
    templateParameter_parameterKey,

    -- ** TypeConfigurationDetails
    typeConfigurationDetails_alias,
    typeConfigurationDetails_arn,
    typeConfigurationDetails_configuration,
    typeConfigurationDetails_isDefaultConfiguration,
    typeConfigurationDetails_lastUpdated,
    typeConfigurationDetails_typeArn,
    typeConfigurationDetails_typeName,

    -- ** TypeConfigurationIdentifier
    typeConfigurationIdentifier_type,
    typeConfigurationIdentifier_typeArn,
    typeConfigurationIdentifier_typeConfigurationAlias,
    typeConfigurationIdentifier_typeConfigurationArn,
    typeConfigurationIdentifier_typeName,

    -- ** TypeFilters
    typeFilters_category,
    typeFilters_publisherId,
    typeFilters_typeNamePrefix,

    -- ** TypeSummary
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

    -- ** TypeVersionSummary
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
import Amazonka.CloudFormation.Types.AccountGateResult
import Amazonka.CloudFormation.Types.AccountLimit
import Amazonka.CloudFormation.Types.AutoDeployment
import Amazonka.CloudFormation.Types.BatchDescribeTypeConfigurationsError
import Amazonka.CloudFormation.Types.Change
import Amazonka.CloudFormation.Types.ChangeSetHook
import Amazonka.CloudFormation.Types.ChangeSetHookResourceTargetDetails
import Amazonka.CloudFormation.Types.ChangeSetHookTargetDetails
import Amazonka.CloudFormation.Types.ChangeSetSummary
import Amazonka.CloudFormation.Types.DeploymentTargets
import Amazonka.CloudFormation.Types.Export
import Amazonka.CloudFormation.Types.LoggingConfig
import Amazonka.CloudFormation.Types.ManagedExecution
import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.OperationResultFilter
import Amazonka.CloudFormation.Types.Output
import Amazonka.CloudFormation.Types.Parameter
import Amazonka.CloudFormation.Types.ParameterConstraints
import Amazonka.CloudFormation.Types.ParameterDeclaration
import Amazonka.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Amazonka.CloudFormation.Types.PropertyDifference
import Amazonka.CloudFormation.Types.RequiredActivatedType
import Amazonka.CloudFormation.Types.ResourceChange
import Amazonka.CloudFormation.Types.ResourceChangeDetail
import Amazonka.CloudFormation.Types.ResourceIdentifierSummary
import Amazonka.CloudFormation.Types.ResourceTargetDefinition
import Amazonka.CloudFormation.Types.ResourceToImport
import Amazonka.CloudFormation.Types.RollbackConfiguration
import Amazonka.CloudFormation.Types.RollbackTrigger
import Amazonka.CloudFormation.Types.Stack
import Amazonka.CloudFormation.Types.StackDriftInformation
import Amazonka.CloudFormation.Types.StackDriftInformationSummary
import Amazonka.CloudFormation.Types.StackEvent
import Amazonka.CloudFormation.Types.StackInstance
import Amazonka.CloudFormation.Types.StackInstanceComprehensiveStatus
import Amazonka.CloudFormation.Types.StackInstanceFilter
import Amazonka.CloudFormation.Types.StackInstanceSummary
import Amazonka.CloudFormation.Types.StackResource
import Amazonka.CloudFormation.Types.StackResourceDetail
import Amazonka.CloudFormation.Types.StackResourceDrift
import Amazonka.CloudFormation.Types.StackResourceDriftInformation
import Amazonka.CloudFormation.Types.StackResourceDriftInformationSummary
import Amazonka.CloudFormation.Types.StackResourceSummary
import Amazonka.CloudFormation.Types.StackSet
import Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
import Amazonka.CloudFormation.Types.StackSetOperation
import Amazonka.CloudFormation.Types.StackSetOperationPreferences
import Amazonka.CloudFormation.Types.StackSetOperationResultSummary
import Amazonka.CloudFormation.Types.StackSetOperationStatusDetails
import Amazonka.CloudFormation.Types.StackSetOperationSummary
import Amazonka.CloudFormation.Types.StackSetSummary
import Amazonka.CloudFormation.Types.StackSummary
import Amazonka.CloudFormation.Types.Tag
import Amazonka.CloudFormation.Types.TemplateParameter
import Amazonka.CloudFormation.Types.TypeConfigurationDetails
import Amazonka.CloudFormation.Types.TypeConfigurationIdentifier
import Amazonka.CloudFormation.Types.TypeFilters
import Amazonka.CloudFormation.Types.TypeSummary
import Amazonka.CloudFormation.Types.TypeVersionSummary
import Amazonka.CloudFormation.UpdateStack
import Amazonka.CloudFormation.UpdateStackInstances
import Amazonka.CloudFormation.UpdateStackSet
import Amazonka.CloudFormation.UpdateTerminationProtection
import Amazonka.CloudFormation.ValidateTemplate
