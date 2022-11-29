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
    activateType_majorVersion,
    activateType_type,
    activateType_publicTypeArn,
    activateType_autoUpdate,
    activateType_publisherId,
    activateType_typeName,
    activateType_versionBump,
    activateType_typeNameAlias,
    activateType_executionRoleArn,
    activateType_loggingConfig,
    activateTypeResponse_arn,
    activateTypeResponse_httpStatus,

    -- ** BatchDescribeTypeConfigurations
    batchDescribeTypeConfigurations_typeConfigurationIdentifiers,
    batchDescribeTypeConfigurationsResponse_errors,
    batchDescribeTypeConfigurationsResponse_unprocessedTypeConfigurations,
    batchDescribeTypeConfigurationsResponse_typeConfigurations,
    batchDescribeTypeConfigurationsResponse_httpStatus,

    -- ** CancelUpdateStack
    cancelUpdateStack_clientRequestToken,
    cancelUpdateStack_stackName,

    -- ** ContinueUpdateRollback
    continueUpdateRollback_resourcesToSkip,
    continueUpdateRollback_roleARN,
    continueUpdateRollback_clientRequestToken,
    continueUpdateRollback_stackName,
    continueUpdateRollbackResponse_httpStatus,

    -- ** CreateChangeSet
    createChangeSet_tags,
    createChangeSet_clientToken,
    createChangeSet_roleARN,
    createChangeSet_notificationARNs,
    createChangeSet_usePreviousTemplate,
    createChangeSet_templateBody,
    createChangeSet_resourceTypes,
    createChangeSet_changeSetType,
    createChangeSet_description,
    createChangeSet_resourcesToImport,
    createChangeSet_includeNestedStacks,
    createChangeSet_capabilities,
    createChangeSet_rollbackConfiguration,
    createChangeSet_templateURL,
    createChangeSet_parameters,
    createChangeSet_stackName,
    createChangeSet_changeSetName,
    createChangeSetResponse_stackId,
    createChangeSetResponse_id,
    createChangeSetResponse_httpStatus,

    -- ** CreateStack
    createStack_tags,
    createStack_roleARN,
    createStack_timeoutInMinutes,
    createStack_clientRequestToken,
    createStack_enableTerminationProtection,
    createStack_notificationARNs,
    createStack_stackPolicyBody,
    createStack_stackPolicyURL,
    createStack_templateBody,
    createStack_resourceTypes,
    createStack_disableRollback,
    createStack_onFailure,
    createStack_capabilities,
    createStack_rollbackConfiguration,
    createStack_templateURL,
    createStack_parameters,
    createStack_stackName,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** CreateStackInstances
    createStackInstances_operationPreferences,
    createStackInstances_callAs,
    createStackInstances_parameterOverrides,
    createStackInstances_operationId,
    createStackInstances_accounts,
    createStackInstances_deploymentTargets,
    createStackInstances_stackSetName,
    createStackInstances_regions,
    createStackInstancesResponse_operationId,
    createStackInstancesResponse_httpStatus,

    -- ** CreateStackSet
    createStackSet_tags,
    createStackSet_stackId,
    createStackSet_clientRequestToken,
    createStackSet_callAs,
    createStackSet_templateBody,
    createStackSet_description,
    createStackSet_autoDeployment,
    createStackSet_capabilities,
    createStackSet_managedExecution,
    createStackSet_executionRoleName,
    createStackSet_administrationRoleARN,
    createStackSet_permissionModel,
    createStackSet_templateURL,
    createStackSet_parameters,
    createStackSet_stackSetName,
    createStackSetResponse_stackSetId,
    createStackSetResponse_httpStatus,

    -- ** DeactivateType
    deactivateType_type,
    deactivateType_arn,
    deactivateType_typeName,
    deactivateTypeResponse_httpStatus,

    -- ** DeleteChangeSet
    deleteChangeSet_stackName,
    deleteChangeSet_changeSetName,
    deleteChangeSetResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_roleARN,
    deleteStack_clientRequestToken,
    deleteStack_retainResources,
    deleteStack_stackName,

    -- ** DeleteStackInstances
    deleteStackInstances_operationPreferences,
    deleteStackInstances_callAs,
    deleteStackInstances_operationId,
    deleteStackInstances_accounts,
    deleteStackInstances_deploymentTargets,
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
    deregisterType_type,
    deregisterType_arn,
    deregisterType_typeName,
    deregisterType_versionId,
    deregisterTypeResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_nextToken,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_nextToken,
    describeChangeSet_stackName,
    describeChangeSet_changeSetName,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_description,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,

    -- ** DescribeChangeSetHooks
    describeChangeSetHooks_nextToken,
    describeChangeSetHooks_stackName,
    describeChangeSetHooks_logicalResourceId,
    describeChangeSetHooks_changeSetName,
    describeChangeSetHooksResponse_stackId,
    describeChangeSetHooksResponse_nextToken,
    describeChangeSetHooksResponse_changeSetId,
    describeChangeSetHooksResponse_changeSetName,
    describeChangeSetHooksResponse_hooks,
    describeChangeSetHooksResponse_status,
    describeChangeSetHooksResponse_stackName,
    describeChangeSetHooksResponse_httpStatus,

    -- ** DescribePublisher
    describePublisher_publisherId,
    describePublisherResponse_publisherId,
    describePublisherResponse_publisherStatus,
    describePublisherResponse_publisherProfile,
    describePublisherResponse_identityProvider,
    describePublisherResponse_httpStatus,

    -- ** DescribeStackDriftDetectionStatus
    describeStackDriftDetectionStatus_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_stackDriftStatus,
    describeStackDriftDetectionStatusResponse_detectionStatusReason,
    describeStackDriftDetectionStatusResponse_driftedStackResourceCount,
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
    describeStackResourceDrifts_stackResourceDriftStatusFilters,
    describeStackResourceDrifts_nextToken,
    describeStackResourceDrifts_maxResults,
    describeStackResourceDrifts_stackName,
    describeStackResourceDriftsResponse_nextToken,
    describeStackResourceDriftsResponse_httpStatus,
    describeStackResourceDriftsResponse_stackResourceDrifts,

    -- ** DescribeStackResources
    describeStackResources_stackName,
    describeStackResources_logicalResourceId,
    describeStackResources_physicalResourceId,
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
    describeStacksResponse_stacks,
    describeStacksResponse_nextToken,
    describeStacksResponse_httpStatus,

    -- ** DescribeType
    describeType_type,
    describeType_arn,
    describeType_publicVersionNumber,
    describeType_publisherId,
    describeType_typeName,
    describeType_versionId,
    describeTypeResponse_typeTestsStatusDescription,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_type,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_requiredActivatedTypes,
    describeTypeResponse_configurationSchema,
    describeTypeResponse_visibility,
    describeTypeResponse_autoUpdate,
    describeTypeResponse_arn,
    describeTypeResponse_timeCreated,
    describeTypeResponse_publicVersionNumber,
    describeTypeResponse_publisherId,
    describeTypeResponse_typeName,
    describeTypeResponse_description,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_originalTypeName,
    describeTypeResponse_provisioningType,
    describeTypeResponse_latestPublicVersion,
    describeTypeResponse_schema,
    describeTypeResponse_isActivated,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_typeTestsStatus,
    describeTypeResponse_originalTypeArn,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_httpStatus,

    -- ** DescribeTypeRegistration
    describeTypeRegistration_registrationToken,
    describeTypeRegistrationResponse_typeArn,
    describeTypeRegistrationResponse_progressStatus,
    describeTypeRegistrationResponse_description,
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
    detectStackSetDrift_operationPreferences,
    detectStackSetDrift_callAs,
    detectStackSetDrift_operationId,
    detectStackSetDrift_stackSetName,
    detectStackSetDriftResponse_operationId,
    detectStackSetDriftResponse_httpStatus,

    -- ** EstimateTemplateCost
    estimateTemplateCost_templateBody,
    estimateTemplateCost_templateURL,
    estimateTemplateCost_parameters,
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
    getTemplate_templateStage,
    getTemplate_changeSetName,
    getTemplate_stackName,
    getTemplateResponse_templateBody,
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_httpStatus,

    -- ** GetTemplateSummary
    getTemplateSummary_stackSetName,
    getTemplateSummary_callAs,
    getTemplateSummary_templateBody,
    getTemplateSummary_stackName,
    getTemplateSummary_templateURL,
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_httpStatus,

    -- ** ImportStacksToStackSet
    importStacksToStackSet_operationPreferences,
    importStacksToStackSet_callAs,
    importStacksToStackSet_operationId,
    importStacksToStackSet_stackIdsUrl,
    importStacksToStackSet_organizationalUnitIds,
    importStacksToStackSet_stackIds,
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
    listExportsResponse_nextToken,
    listExportsResponse_exports,
    listExportsResponse_httpStatus,

    -- ** ListImports
    listImports_nextToken,
    listImports_exportName,
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListStackInstances
    listStackInstances_nextToken,
    listStackInstances_callAs,
    listStackInstances_filters,
    listStackInstances_maxResults,
    listStackInstances_stackInstanceRegion,
    listStackInstances_stackInstanceAccount,
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
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_callAs,
    listStackSetOperationResults_filters,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_stackSetName,
    listStackSetOperationResults_operationId,
    listStackSetOperationResultsResponse_nextToken,
    listStackSetOperationResultsResponse_summaries,
    listStackSetOperationResultsResponse_httpStatus,

    -- ** ListStackSetOperations
    listStackSetOperations_nextToken,
    listStackSetOperations_callAs,
    listStackSetOperations_maxResults,
    listStackSetOperations_stackSetName,
    listStackSetOperationsResponse_nextToken,
    listStackSetOperationsResponse_summaries,
    listStackSetOperationsResponse_httpStatus,

    -- ** ListStackSets
    listStackSets_nextToken,
    listStackSets_callAs,
    listStackSets_status,
    listStackSets_maxResults,
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
    listTypeRegistrations_nextToken,
    listTypeRegistrations_type,
    listTypeRegistrations_typeArn,
    listTypeRegistrations_typeName,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_maxResults,
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_httpStatus,

    -- ** ListTypeVersions
    listTypeVersions_deprecatedStatus,
    listTypeVersions_nextToken,
    listTypeVersions_type,
    listTypeVersions_arn,
    listTypeVersions_publisherId,
    listTypeVersions_typeName,
    listTypeVersions_maxResults,
    listTypeVersionsResponse_nextToken,
    listTypeVersionsResponse_typeVersionSummaries,
    listTypeVersionsResponse_httpStatus,

    -- ** ListTypes
    listTypes_deprecatedStatus,
    listTypes_nextToken,
    listTypes_type,
    listTypes_filters,
    listTypes_visibility,
    listTypes_maxResults,
    listTypes_provisioningType,
    listTypesResponse_nextToken,
    listTypesResponse_typeSummaries,
    listTypesResponse_httpStatus,

    -- ** PublishType
    publishType_type,
    publishType_arn,
    publishType_publicVersionNumber,
    publishType_typeName,
    publishTypeResponse_publicTypeArn,
    publishTypeResponse_httpStatus,

    -- ** RecordHandlerProgress
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_errorCode,
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
    registerType_type,
    registerType_clientRequestToken,
    registerType_executionRoleArn,
    registerType_loggingConfig,
    registerType_typeName,
    registerType_schemaHandlerPackage,
    registerTypeResponse_registrationToken,
    registerTypeResponse_httpStatus,

    -- ** RollbackStack
    rollbackStack_roleARN,
    rollbackStack_clientRequestToken,
    rollbackStack_stackName,
    rollbackStackResponse_stackId,
    rollbackStackResponse_httpStatus,

    -- ** SetStackPolicy
    setStackPolicy_stackPolicyBody,
    setStackPolicy_stackPolicyURL,
    setStackPolicy_stackName,

    -- ** SetTypeConfiguration
    setTypeConfiguration_type,
    setTypeConfiguration_typeArn,
    setTypeConfiguration_configurationAlias,
    setTypeConfiguration_typeName,
    setTypeConfiguration_configuration,
    setTypeConfigurationResponse_configurationArn,
    setTypeConfigurationResponse_httpStatus,

    -- ** SetTypeDefaultVersion
    setTypeDefaultVersion_type,
    setTypeDefaultVersion_arn,
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
    testType_type,
    testType_arn,
    testType_logDeliveryBucket,
    testType_typeName,
    testType_versionId,
    testTypeResponse_typeVersionArn,
    testTypeResponse_httpStatus,

    -- ** UpdateStack
    updateStack_tags,
    updateStack_roleARN,
    updateStack_clientRequestToken,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_notificationARNs,
    updateStack_usePreviousTemplate,
    updateStack_stackPolicyBody,
    updateStack_stackPolicyURL,
    updateStack_templateBody,
    updateStack_resourceTypes,
    updateStack_disableRollback,
    updateStack_capabilities,
    updateStack_rollbackConfiguration,
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_templateURL,
    updateStack_parameters,
    updateStack_stackName,
    updateStackResponse_stackId,
    updateStackResponse_httpStatus,

    -- ** UpdateStackInstances
    updateStackInstances_operationPreferences,
    updateStackInstances_callAs,
    updateStackInstances_parameterOverrides,
    updateStackInstances_operationId,
    updateStackInstances_accounts,
    updateStackInstances_deploymentTargets,
    updateStackInstances_stackSetName,
    updateStackInstances_regions,
    updateStackInstancesResponse_operationId,
    updateStackInstancesResponse_httpStatus,

    -- ** UpdateStackSet
    updateStackSet_tags,
    updateStackSet_operationPreferences,
    updateStackSet_callAs,
    updateStackSet_regions,
    updateStackSet_usePreviousTemplate,
    updateStackSet_templateBody,
    updateStackSet_operationId,
    updateStackSet_description,
    updateStackSet_autoDeployment,
    updateStackSet_accounts,
    updateStackSet_capabilities,
    updateStackSet_managedExecution,
    updateStackSet_executionRoleName,
    updateStackSet_administrationRoleARN,
    updateStackSet_deploymentTargets,
    updateStackSet_permissionModel,
    updateStackSet_templateURL,
    updateStackSet_parameters,
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
    validateTemplateResponse_capabilitiesReason,
    validateTemplateResponse_description,
    validateTemplateResponse_capabilities,
    validateTemplateResponse_declaredTransforms,
    validateTemplateResponse_parameters,
    validateTemplateResponse_httpStatus,

    -- * Types

    -- ** AccountGateResult
    accountGateResult_statusReason,
    accountGateResult_status,

    -- ** AccountLimit
    accountLimit_name,
    accountLimit_value,

    -- ** AutoDeployment
    autoDeployment_enabled,
    autoDeployment_retainStacksOnAccountRemoval,

    -- ** BatchDescribeTypeConfigurationsError
    batchDescribeTypeConfigurationsError_errorMessage,
    batchDescribeTypeConfigurationsError_typeConfigurationIdentifier,
    batchDescribeTypeConfigurationsError_errorCode,

    -- ** Change
    change_type,
    change_hookInvocationCount,
    change_resourceChange,

    -- ** ChangeSetHook
    changeSetHook_typeConfigurationVersionId,
    changeSetHook_invocationPoint,
    changeSetHook_failureMode,
    changeSetHook_typeName,
    changeSetHook_typeVersionId,
    changeSetHook_targetDetails,

    -- ** ChangeSetHookResourceTargetDetails
    changeSetHookResourceTargetDetails_resourceType,
    changeSetHookResourceTargetDetails_resourceAction,
    changeSetHookResourceTargetDetails_logicalResourceId,

    -- ** ChangeSetHookTargetDetails
    changeSetHookTargetDetails_targetType,
    changeSetHookTargetDetails_resourceTargetDetails,

    -- ** ChangeSetSummary
    changeSetSummary_stackId,
    changeSetSummary_changeSetId,
    changeSetSummary_changeSetName,
    changeSetSummary_statusReason,
    changeSetSummary_rootChangeSetId,
    changeSetSummary_status,
    changeSetSummary_description,
    changeSetSummary_includeNestedStacks,
    changeSetSummary_stackName,
    changeSetSummary_parentChangeSetId,
    changeSetSummary_creationTime,
    changeSetSummary_executionStatus,

    -- ** DeploymentTargets
    deploymentTargets_accountFilterType,
    deploymentTargets_organizationalUnitIds,
    deploymentTargets_accounts,
    deploymentTargets_accountsUrl,

    -- ** Export
    export_name,
    export_exportingStackId,
    export_value,

    -- ** LoggingConfig
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- ** ManagedExecution
    managedExecution_active,

    -- ** ModuleInfo
    moduleInfo_typeHierarchy,
    moduleInfo_logicalIdHierarchy,

    -- ** OperationResultFilter
    operationResultFilter_name,
    operationResultFilter_values,

    -- ** Output
    output_outputKey,
    output_description,
    output_outputValue,
    output_exportName,

    -- ** Parameter
    parameter_parameterValue,
    parameter_resolvedValue,
    parameter_usePreviousValue,
    parameter_parameterKey,

    -- ** ParameterConstraints
    parameterConstraints_allowedValues,

    -- ** ParameterDeclaration
    parameterDeclaration_noEcho,
    parameterDeclaration_defaultValue,
    parameterDeclaration_description,
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_parameterType,
    parameterDeclaration_parameterKey,

    -- ** PhysicalResourceIdContextKeyValuePair
    physicalResourceIdContextKeyValuePair_key,
    physicalResourceIdContextKeyValuePair_value,

    -- ** PropertyDifference
    propertyDifference_propertyPath,
    propertyDifference_expectedValue,
    propertyDifference_actualValue,
    propertyDifference_differenceType,

    -- ** RequiredActivatedType
    requiredActivatedType_publisherId,
    requiredActivatedType_originalTypeName,
    requiredActivatedType_typeNameAlias,
    requiredActivatedType_supportedMajorVersions,

    -- ** ResourceChange
    resourceChange_resourceType,
    resourceChange_changeSetId,
    resourceChange_replacement,
    resourceChange_details,
    resourceChange_logicalResourceId,
    resourceChange_scope,
    resourceChange_action,
    resourceChange_moduleInfo,
    resourceChange_physicalResourceId,

    -- ** ResourceChangeDetail
    resourceChangeDetail_target,
    resourceChangeDetail_changeSource,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_causingEntity,

    -- ** ResourceIdentifierSummary
    resourceIdentifierSummary_resourceType,
    resourceIdentifierSummary_resourceIdentifiers,
    resourceIdentifierSummary_logicalResourceIds,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_name,
    resourceTargetDefinition_attribute,
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
    stack_deletionTime,
    stack_tags,
    stack_stackId,
    stack_roleARN,
    stack_timeoutInMinutes,
    stack_enableTerminationProtection,
    stack_changeSetId,
    stack_notificationARNs,
    stack_stackStatusReason,
    stack_parentId,
    stack_disableRollback,
    stack_lastUpdatedTime,
    stack_description,
    stack_outputs,
    stack_capabilities,
    stack_rootId,
    stack_rollbackConfiguration,
    stack_driftInformation,
    stack_parameters,
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
    stackEvent_resourceType,
    stackEvent_hookInvocationPoint,
    stackEvent_clientRequestToken,
    stackEvent_resourceStatusReason,
    stackEvent_hookFailureMode,
    stackEvent_hookStatus,
    stackEvent_hookStatusReason,
    stackEvent_logicalResourceId,
    stackEvent_hookType,
    stackEvent_resourceProperties,
    stackEvent_physicalResourceId,
    stackEvent_resourceStatus,
    stackEvent_stackId,
    stackEvent_eventId,
    stackEvent_stackName,
    stackEvent_timestamp,

    -- ** StackInstance
    stackInstance_stackId,
    stackInstance_stackInstanceStatus,
    stackInstance_driftStatus,
    stackInstance_stackSetId,
    stackInstance_parameterOverrides,
    stackInstance_account,
    stackInstance_statusReason,
    stackInstance_status,
    stackInstance_lastOperationId,
    stackInstance_region,
    stackInstance_organizationalUnitId,
    stackInstance_lastDriftCheckTimestamp,

    -- ** StackInstanceComprehensiveStatus
    stackInstanceComprehensiveStatus_detailedStatus,

    -- ** StackInstanceFilter
    stackInstanceFilter_name,
    stackInstanceFilter_values,

    -- ** StackInstanceSummary
    stackInstanceSummary_stackId,
    stackInstanceSummary_stackInstanceStatus,
    stackInstanceSummary_driftStatus,
    stackInstanceSummary_stackSetId,
    stackInstanceSummary_account,
    stackInstanceSummary_statusReason,
    stackInstanceSummary_status,
    stackInstanceSummary_lastOperationId,
    stackInstanceSummary_region,
    stackInstanceSummary_organizationalUnitId,
    stackInstanceSummary_lastDriftCheckTimestamp,

    -- ** StackResource
    stackResource_stackId,
    stackResource_resourceStatusReason,
    stackResource_description,
    stackResource_stackName,
    stackResource_moduleInfo,
    stackResource_driftInformation,
    stackResource_physicalResourceId,
    stackResource_logicalResourceId,
    stackResource_resourceType,
    stackResource_timestamp,
    stackResource_resourceStatus,

    -- ** StackResourceDetail
    stackResourceDetail_stackId,
    stackResourceDetail_metadata,
    stackResourceDetail_resourceStatusReason,
    stackResourceDetail_description,
    stackResourceDetail_stackName,
    stackResourceDetail_moduleInfo,
    stackResourceDetail_driftInformation,
    stackResourceDetail_physicalResourceId,
    stackResourceDetail_logicalResourceId,
    stackResourceDetail_resourceType,
    stackResourceDetail_lastUpdatedTimestamp,
    stackResourceDetail_resourceStatus,

    -- ** StackResourceDrift
    stackResourceDrift_expectedProperties,
    stackResourceDrift_actualProperties,
    stackResourceDrift_moduleInfo,
    stackResourceDrift_propertyDifferences,
    stackResourceDrift_physicalResourceIdContext,
    stackResourceDrift_physicalResourceId,
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
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_driftInformation,
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- ** StackSet
    stackSet_tags,
    stackSet_stackSetName,
    stackSet_stackSetARN,
    stackSet_stackSetId,
    stackSet_templateBody,
    stackSet_status,
    stackSet_description,
    stackSet_autoDeployment,
    stackSet_organizationalUnitIds,
    stackSet_capabilities,
    stackSet_managedExecution,
    stackSet_executionRoleName,
    stackSet_administrationRoleARN,
    stackSet_permissionModel,
    stackSet_stackSetDriftDetectionDetails,
    stackSet_parameters,

    -- ** StackSetDriftDetectionDetails
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_totalStackInstancesCount,
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,

    -- ** StackSetOperation
    stackSetOperation_endTimestamp,
    stackSetOperation_operationPreferences,
    stackSetOperation_statusDetails,
    stackSetOperation_stackSetId,
    stackSetOperation_statusReason,
    stackSetOperation_operationId,
    stackSetOperation_status,
    stackSetOperation_creationTimestamp,
    stackSetOperation_action,
    stackSetOperation_executionRoleName,
    stackSetOperation_administrationRoleARN,
    stackSetOperation_deploymentTargets,
    stackSetOperation_retainStacks,
    stackSetOperation_stackSetDriftDetectionDetails,

    -- ** StackSetOperationPreferences
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_regionOrder,
    stackSetOperationPreferences_failureTolerancePercentage,
    stackSetOperationPreferences_regionConcurrencyType,
    stackSetOperationPreferences_maxConcurrentPercentage,

    -- ** StackSetOperationResultSummary
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_statusReason,
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_accountGateResult,

    -- ** StackSetOperationStatusDetails
    stackSetOperationStatusDetails_failedStackInstancesCount,

    -- ** StackSetOperationSummary
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_operationPreferences,
    stackSetOperationSummary_statusDetails,
    stackSetOperationSummary_statusReason,
    stackSetOperationSummary_operationId,
    stackSetOperationSummary_status,
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_action,

    -- ** StackSetSummary
    stackSetSummary_stackSetName,
    stackSetSummary_driftStatus,
    stackSetSummary_stackSetId,
    stackSetSummary_status,
    stackSetSummary_description,
    stackSetSummary_autoDeployment,
    stackSetSummary_managedExecution,
    stackSetSummary_permissionModel,
    stackSetSummary_lastDriftCheckTimestamp,

    -- ** StackSummary
    stackSummary_deletionTime,
    stackSummary_stackId,
    stackSummary_stackStatusReason,
    stackSummary_parentId,
    stackSummary_lastUpdatedTime,
    stackSummary_rootId,
    stackSummary_driftInformation,
    stackSummary_templateDescription,
    stackSummary_stackName,
    stackSummary_creationTime,
    stackSummary_stackStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateParameter
    templateParameter_noEcho,
    templateParameter_defaultValue,
    templateParameter_description,
    templateParameter_parameterKey,

    -- ** TypeConfigurationDetails
    typeConfigurationDetails_alias,
    typeConfigurationDetails_typeArn,
    typeConfigurationDetails_configuration,
    typeConfigurationDetails_arn,
    typeConfigurationDetails_typeName,
    typeConfigurationDetails_isDefaultConfiguration,
    typeConfigurationDetails_lastUpdated,

    -- ** TypeConfigurationIdentifier
    typeConfigurationIdentifier_type,
    typeConfigurationIdentifier_typeArn,
    typeConfigurationIdentifier_typeName,
    typeConfigurationIdentifier_typeConfigurationAlias,
    typeConfigurationIdentifier_typeConfigurationArn,

    -- ** TypeFilters
    typeFilters_publisherId,
    typeFilters_typeNamePrefix,
    typeFilters_category,

    -- ** TypeSummary
    typeSummary_defaultVersionId,
    typeSummary_type,
    typeSummary_typeArn,
    typeSummary_publicVersionNumber,
    typeSummary_publisherId,
    typeSummary_typeName,
    typeSummary_publisherIdentity,
    typeSummary_description,
    typeSummary_lastUpdated,
    typeSummary_originalTypeName,
    typeSummary_latestPublicVersion,
    typeSummary_isActivated,
    typeSummary_publisherName,

    -- ** TypeVersionSummary
    typeVersionSummary_isDefaultVersion,
    typeVersionSummary_type,
    typeVersionSummary_arn,
    typeVersionSummary_timeCreated,
    typeVersionSummary_publicVersionNumber,
    typeVersionSummary_typeName,
    typeVersionSummary_description,
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
