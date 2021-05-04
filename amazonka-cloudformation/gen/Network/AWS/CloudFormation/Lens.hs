{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Lens
  ( -- * Operations

    -- ** DescribeStackResourceDrifts
    describeStackResourceDrifts_nextToken,
    describeStackResourceDrifts_maxResults,
    describeStackResourceDrifts_stackResourceDriftStatusFilters,
    describeStackResourceDrifts_stackName,
    describeStackResourceDriftsResponse_nextToken,
    describeStackResourceDriftsResponse_httpStatus,
    describeStackResourceDriftsResponse_stackResourceDrifts,

    -- ** ListImports
    listImports_nextToken,
    listImports_exportName,
    listImportsResponse_nextToken,
    listImportsResponse_imports,
    listImportsResponse_httpStatus,

    -- ** DescribeStackEvents
    describeStackEvents_nextToken,
    describeStackEvents_stackName,
    describeStackEventsResponse_nextToken,
    describeStackEventsResponse_stackEvents,
    describeStackEventsResponse_httpStatus,

    -- ** StopStackSetOperation
    stopStackSetOperation_callAs,
    stopStackSetOperation_stackSetName,
    stopStackSetOperation_operationId,
    stopStackSetOperationResponse_httpStatus,

    -- ** DescribeChangeSet
    describeChangeSet_nextToken,
    describeChangeSet_stackName,
    describeChangeSet_changeSetName,
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_description,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,

    -- ** DescribeStackResource
    describeStackResource_stackName,
    describeStackResource_logicalResourceId,
    describeStackResourceResponse_stackResourceDetail,
    describeStackResourceResponse_httpStatus,

    -- ** DetectStackResourceDrift
    detectStackResourceDrift_stackName,
    detectStackResourceDrift_logicalResourceId,
    detectStackResourceDriftResponse_httpStatus,
    detectStackResourceDriftResponse_stackResourceDrift,

    -- ** ExecuteChangeSet
    executeChangeSet_stackName,
    executeChangeSet_clientRequestToken,
    executeChangeSet_changeSetName,
    executeChangeSetResponse_httpStatus,

    -- ** SetTypeDefaultVersion
    setTypeDefaultVersion_typeName,
    setTypeDefaultVersion_arn,
    setTypeDefaultVersion_versionId,
    setTypeDefaultVersion_type,
    setTypeDefaultVersionResponse_httpStatus,

    -- ** RecordHandlerProgress
    recordHandlerProgress_statusMessage,
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_errorCode,
    recordHandlerProgress_bearerToken,
    recordHandlerProgress_operationStatus,
    recordHandlerProgressResponse_httpStatus,

    -- ** ListStackSetOperations
    listStackSetOperations_nextToken,
    listStackSetOperations_maxResults,
    listStackSetOperations_callAs,
    listStackSetOperations_stackSetName,
    listStackSetOperationsResponse_nextToken,
    listStackSetOperationsResponse_summaries,
    listStackSetOperationsResponse_httpStatus,

    -- ** CreateStackInstances
    createStackInstances_parameterOverrides,
    createStackInstances_deploymentTargets,
    createStackInstances_operationId,
    createStackInstances_callAs,
    createStackInstances_operationPreferences,
    createStackInstances_accounts,
    createStackInstances_stackSetName,
    createStackInstances_regions,
    createStackInstancesResponse_operationId,
    createStackInstancesResponse_httpStatus,

    -- ** GetStackPolicy
    getStackPolicy_stackName,
    getStackPolicyResponse_stackPolicyBody,
    getStackPolicyResponse_httpStatus,

    -- ** DescribeStacks
    describeStacks_nextToken,
    describeStacks_stackName,
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,

    -- ** DeleteStackSet
    deleteStackSet_callAs,
    deleteStackSet_stackSetName,
    deleteStackSetResponse_httpStatus,

    -- ** EstimateTemplateCost
    estimateTemplateCost_templateURL,
    estimateTemplateCost_templateBody,
    estimateTemplateCost_parameters,
    estimateTemplateCostResponse_url,
    estimateTemplateCostResponse_httpStatus,

    -- ** UpdateStackSet
    updateStackSet_permissionModel,
    updateStackSet_executionRoleName,
    updateStackSet_capabilities,
    updateStackSet_templateURL,
    updateStackSet_deploymentTargets,
    updateStackSet_operationId,
    updateStackSet_callAs,
    updateStackSet_operationPreferences,
    updateStackSet_accounts,
    updateStackSet_administrationRoleARN,
    updateStackSet_tags,
    updateStackSet_autoDeployment,
    updateStackSet_description,
    updateStackSet_regions,
    updateStackSet_templateBody,
    updateStackSet_parameters,
    updateStackSet_usePreviousTemplate,
    updateStackSet_stackSetName,
    updateStackSetResponse_operationId,
    updateStackSetResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_httpStatus,

    -- ** CreateStack
    createStack_onFailure,
    createStack_roleARN,
    createStack_resourceTypes,
    createStack_capabilities,
    createStack_stackPolicyBody,
    createStack_enableTerminationProtection,
    createStack_templateURL,
    createStack_notificationARNs,
    createStack_stackPolicyURL,
    createStack_tags,
    createStack_timeoutInMinutes,
    createStack_rollbackConfiguration,
    createStack_clientRequestToken,
    createStack_templateBody,
    createStack_disableRollback,
    createStack_parameters,
    createStack_stackName,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** DescribeStackInstance
    describeStackInstance_callAs,
    describeStackInstance_stackSetName,
    describeStackInstance_stackInstanceAccount,
    describeStackInstance_stackInstanceRegion,
    describeStackInstanceResponse_stackInstance,
    describeStackInstanceResponse_httpStatus,

    -- ** GetTemplateSummary
    getTemplateSummary_stackName,
    getTemplateSummary_templateURL,
    getTemplateSummary_stackSetName,
    getTemplateSummary_templateBody,
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_httpStatus,

    -- ** ListStacks
    listStacks_nextToken,
    listStacks_stackStatusFilter,
    listStacksResponse_nextToken,
    listStacksResponse_stackSummaries,
    listStacksResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_retainResources,
    deleteStack_roleARN,
    deleteStack_clientRequestToken,
    deleteStack_stackName,

    -- ** UpdateStack
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_roleARN,
    updateStack_resourceTypes,
    updateStack_capabilities,
    updateStack_stackPolicyBody,
    updateStack_templateURL,
    updateStack_notificationARNs,
    updateStack_stackPolicyURL,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_tags,
    updateStack_rollbackConfiguration,
    updateStack_clientRequestToken,
    updateStack_templateBody,
    updateStack_parameters,
    updateStack_usePreviousTemplate,
    updateStack_stackName,
    updateStackResponse_stackId,
    updateStackResponse_httpStatus,

    -- ** DetectStackDrift
    detectStackDrift_logicalResourceIds,
    detectStackDrift_stackName,
    detectStackDriftResponse_httpStatus,
    detectStackDriftResponse_stackDriftDetectionId,

    -- ** DescribeStackSetOperation
    describeStackSetOperation_callAs,
    describeStackSetOperation_stackSetName,
    describeStackSetOperation_operationId,
    describeStackSetOperationResponse_stackSetOperation,
    describeStackSetOperationResponse_httpStatus,

    -- ** DescribeTypeRegistration
    describeTypeRegistration_registrationToken,
    describeTypeRegistrationResponse_typeVersionArn,
    describeTypeRegistrationResponse_progressStatus,
    describeTypeRegistrationResponse_description,
    describeTypeRegistrationResponse_typeArn,
    describeTypeRegistrationResponse_httpStatus,

    -- ** SetStackPolicy
    setStackPolicy_stackPolicyBody,
    setStackPolicy_stackPolicyURL,
    setStackPolicy_stackName,

    -- ** SignalResource
    signalResource_stackName,
    signalResource_logicalResourceId,
    signalResource_uniqueId,
    signalResource_status,

    -- ** DescribeStackSet
    describeStackSet_callAs,
    describeStackSet_stackSetName,
    describeStackSetResponse_stackSet,
    describeStackSetResponse_httpStatus,

    -- ** ListStackSetOperationResults
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_callAs,
    listStackSetOperationResults_stackSetName,
    listStackSetOperationResults_operationId,
    listStackSetOperationResultsResponse_nextToken,
    listStackSetOperationResultsResponse_summaries,
    listStackSetOperationResultsResponse_httpStatus,

    -- ** CancelUpdateStack
    cancelUpdateStack_clientRequestToken,
    cancelUpdateStack_stackName,

    -- ** ListTypes
    listTypes_nextToken,
    listTypes_maxResults,
    listTypes_deprecatedStatus,
    listTypes_provisioningType,
    listTypes_visibility,
    listTypes_type,
    listTypesResponse_nextToken,
    listTypesResponse_typeSummaries,
    listTypesResponse_httpStatus,

    -- ** RegisterType
    registerType_loggingConfig,
    registerType_executionRoleArn,
    registerType_clientRequestToken,
    registerType_type,
    registerType_typeName,
    registerType_schemaHandlerPackage,
    registerTypeResponse_registrationToken,
    registerTypeResponse_httpStatus,

    -- ** ListStackInstances
    listStackInstances_nextToken,
    listStackInstances_stackInstanceAccount,
    listStackInstances_maxResults,
    listStackInstances_callAs,
    listStackInstances_stackInstanceRegion,
    listStackInstances_filters,
    listStackInstances_stackSetName,
    listStackInstancesResponse_nextToken,
    listStackInstancesResponse_summaries,
    listStackInstancesResponse_httpStatus,

    -- ** DetectStackSetDrift
    detectStackSetDrift_operationId,
    detectStackSetDrift_callAs,
    detectStackSetDrift_operationPreferences,
    detectStackSetDrift_stackSetName,
    detectStackSetDriftResponse_operationId,
    detectStackSetDriftResponse_httpStatus,

    -- ** ContinueUpdateRollback
    continueUpdateRollback_roleARN,
    continueUpdateRollback_resourcesToSkip,
    continueUpdateRollback_clientRequestToken,
    continueUpdateRollback_stackName,
    continueUpdateRollbackResponse_httpStatus,

    -- ** ValidateTemplate
    validateTemplate_templateURL,
    validateTemplate_templateBody,
    validateTemplateResponse_capabilities,
    validateTemplateResponse_declaredTransforms,
    validateTemplateResponse_description,
    validateTemplateResponse_parameters,
    validateTemplateResponse_capabilitiesReason,
    validateTemplateResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_templateStage,
    getTemplate_stackName,
    getTemplate_changeSetName,
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_templateBody,
    getTemplateResponse_httpStatus,

    -- ** UpdateTerminationProtection
    updateTerminationProtection_enableTerminationProtection,
    updateTerminationProtection_stackName,
    updateTerminationProtectionResponse_stackId,
    updateTerminationProtectionResponse_httpStatus,

    -- ** ListTypeVersions
    listTypeVersions_typeName,
    listTypeVersions_nextToken,
    listTypeVersions_maxResults,
    listTypeVersions_arn,
    listTypeVersions_deprecatedStatus,
    listTypeVersions_type,
    listTypeVersionsResponse_nextToken,
    listTypeVersionsResponse_typeVersionSummaries,
    listTypeVersionsResponse_httpStatus,

    -- ** ListTypeRegistrations
    listTypeRegistrations_typeName,
    listTypeRegistrations_nextToken,
    listTypeRegistrations_maxResults,
    listTypeRegistrations_type,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_typeArn,
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_httpStatus,

    -- ** DeregisterType
    deregisterType_typeName,
    deregisterType_arn,
    deregisterType_versionId,
    deregisterType_type,
    deregisterTypeResponse_httpStatus,

    -- ** CreateStackSet
    createStackSet_permissionModel,
    createStackSet_executionRoleName,
    createStackSet_capabilities,
    createStackSet_templateURL,
    createStackSet_callAs,
    createStackSet_administrationRoleARN,
    createStackSet_tags,
    createStackSet_autoDeployment,
    createStackSet_description,
    createStackSet_clientRequestToken,
    createStackSet_templateBody,
    createStackSet_parameters,
    createStackSet_stackSetName,
    createStackSetResponse_stackSetId,
    createStackSetResponse_httpStatus,

    -- ** CreateChangeSet
    createChangeSet_resourcesToImport,
    createChangeSet_includeNestedStacks,
    createChangeSet_roleARN,
    createChangeSet_resourceTypes,
    createChangeSet_capabilities,
    createChangeSet_templateURL,
    createChangeSet_notificationARNs,
    createChangeSet_changeSetType,
    createChangeSet_tags,
    createChangeSet_rollbackConfiguration,
    createChangeSet_description,
    createChangeSet_templateBody,
    createChangeSet_parameters,
    createChangeSet_clientToken,
    createChangeSet_usePreviousTemplate,
    createChangeSet_stackName,
    createChangeSet_changeSetName,
    createChangeSetResponse_stackId,
    createChangeSetResponse_id,
    createChangeSetResponse_httpStatus,

    -- ** ListExports
    listExports_nextToken,
    listExportsResponse_exports,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** DescribeType
    describeType_typeName,
    describeType_arn,
    describeType_versionId,
    describeType_type,
    describeTypeResponse_typeName,
    describeTypeResponse_schema,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_arn,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_provisioningType,
    describeTypeResponse_visibility,
    describeTypeResponse_description,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_type,
    describeTypeResponse_timeCreated,
    describeTypeResponse_httpStatus,

    -- ** ListStackResources
    listStackResources_nextToken,
    listStackResources_stackName,
    listStackResourcesResponse_nextToken,
    listStackResourcesResponse_stackResourceSummaries,
    listStackResourcesResponse_httpStatus,

    -- ** DeleteStackInstances
    deleteStackInstances_deploymentTargets,
    deleteStackInstances_operationId,
    deleteStackInstances_callAs,
    deleteStackInstances_operationPreferences,
    deleteStackInstances_accounts,
    deleteStackInstances_stackSetName,
    deleteStackInstances_regions,
    deleteStackInstances_retainStacks,
    deleteStackInstancesResponse_operationId,
    deleteStackInstancesResponse_httpStatus,

    -- ** ListStackSets
    listStackSets_status,
    listStackSets_nextToken,
    listStackSets_maxResults,
    listStackSets_callAs,
    listStackSetsResponse_nextToken,
    listStackSetsResponse_summaries,
    listStackSetsResponse_httpStatus,

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

    -- ** UpdateStackInstances
    updateStackInstances_parameterOverrides,
    updateStackInstances_deploymentTargets,
    updateStackInstances_operationId,
    updateStackInstances_callAs,
    updateStackInstances_operationPreferences,
    updateStackInstances_accounts,
    updateStackInstances_stackSetName,
    updateStackInstances_regions,
    updateStackInstancesResponse_operationId,
    updateStackInstancesResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_nextToken,
    listChangeSets_stackName,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_summaries,
    listChangeSetsResponse_httpStatus,

    -- ** DeleteChangeSet
    deleteChangeSet_stackName,
    deleteChangeSet_changeSetName,
    deleteChangeSetResponse_httpStatus,

    -- ** DescribeStackResources
    describeStackResources_stackName,
    describeStackResources_physicalResourceId,
    describeStackResources_logicalResourceId,
    describeStackResourcesResponse_stackResources,
    describeStackResourcesResponse_httpStatus,

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

    -- ** Change
    change_resourceChange,
    change_type,

    -- ** ChangeSetSummary
    changeSetSummary_rootChangeSetId,
    changeSetSummary_status,
    changeSetSummary_creationTime,
    changeSetSummary_includeNestedStacks,
    changeSetSummary_stackName,
    changeSetSummary_executionStatus,
    changeSetSummary_stackId,
    changeSetSummary_parentChangeSetId,
    changeSetSummary_changeSetId,
    changeSetSummary_description,
    changeSetSummary_changeSetName,
    changeSetSummary_statusReason,

    -- ** DeploymentTargets
    deploymentTargets_organizationalUnitIds,
    deploymentTargets_accounts,

    -- ** Export
    export_exportingStackId,
    export_name,
    export_value,

    -- ** LoggingConfig
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- ** ModuleInfo
    moduleInfo_logicalIdHierarchy,
    moduleInfo_typeHierarchy,

    -- ** Output
    output_outputKey,
    output_outputValue,
    output_description,
    output_exportName,

    -- ** Parameter
    parameter_parameterValue,
    parameter_usePreviousValue,
    parameter_parameterKey,
    parameter_resolvedValue,

    -- ** ParameterConstraints
    parameterConstraints_allowedValues,

    -- ** ParameterDeclaration
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_parameterType,
    parameterDeclaration_parameterKey,
    parameterDeclaration_description,
    parameterDeclaration_noEcho,
    parameterDeclaration_defaultValue,

    -- ** PhysicalResourceIdContextKeyValuePair
    physicalResourceIdContextKeyValuePair_key,
    physicalResourceIdContextKeyValuePair_value,

    -- ** PropertyDifference
    propertyDifference_propertyPath,
    propertyDifference_expectedValue,
    propertyDifference_actualValue,
    propertyDifference_differenceType,

    -- ** ResourceChange
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_moduleInfo,
    resourceChange_logicalResourceId,
    resourceChange_changeSetId,
    resourceChange_action,
    resourceChange_replacement,

    -- ** ResourceChangeDetail
    resourceChangeDetail_evaluation,
    resourceChangeDetail_changeSource,
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_target,

    -- ** ResourceIdentifierSummary
    resourceIdentifierSummary_resourceIdentifiers,
    resourceIdentifierSummary_resourceType,
    resourceIdentifierSummary_logicalResourceIds,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,
    resourceTargetDefinition_attribute,

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
    stack_outputs,
    stack_driftInformation,
    stack_roleARN,
    stack_deletionTime,
    stack_capabilities,
    stack_stackStatusReason,
    stack_enableTerminationProtection,
    stack_stackId,
    stack_notificationARNs,
    stack_rootId,
    stack_tags,
    stack_changeSetId,
    stack_timeoutInMinutes,
    stack_parentId,
    stack_rollbackConfiguration,
    stack_description,
    stack_disableRollback,
    stack_parameters,
    stack_lastUpdatedTime,
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
    stackEvent_resourceProperties,
    stackEvent_physicalResourceId,
    stackEvent_resourceType,
    stackEvent_resourceStatusReason,
    stackEvent_logicalResourceId,
    stackEvent_resourceStatus,
    stackEvent_clientRequestToken,
    stackEvent_stackId,
    stackEvent_eventId,
    stackEvent_stackName,
    stackEvent_timestamp,

    -- ** StackInstance
    stackInstance_status,
    stackInstance_parameterOverrides,
    stackInstance_stackId,
    stackInstance_stackInstanceStatus,
    stackInstance_organizationalUnitId,
    stackInstance_lastDriftCheckTimestamp,
    stackInstance_driftStatus,
    stackInstance_account,
    stackInstance_stackSetId,
    stackInstance_region,
    stackInstance_statusReason,

    -- ** StackInstanceComprehensiveStatus
    stackInstanceComprehensiveStatus_detailedStatus,

    -- ** StackInstanceFilter
    stackInstanceFilter_values,
    stackInstanceFilter_name,

    -- ** StackInstanceSummary
    stackInstanceSummary_status,
    stackInstanceSummary_stackId,
    stackInstanceSummary_stackInstanceStatus,
    stackInstanceSummary_organizationalUnitId,
    stackInstanceSummary_lastDriftCheckTimestamp,
    stackInstanceSummary_driftStatus,
    stackInstanceSummary_account,
    stackInstanceSummary_stackSetId,
    stackInstanceSummary_region,
    stackInstanceSummary_statusReason,

    -- ** StackResource
    stackResource_driftInformation,
    stackResource_stackName,
    stackResource_stackId,
    stackResource_physicalResourceId,
    stackResource_resourceStatusReason,
    stackResource_moduleInfo,
    stackResource_description,
    stackResource_logicalResourceId,
    stackResource_resourceType,
    stackResource_timestamp,
    stackResource_resourceStatus,

    -- ** StackResourceDetail
    stackResourceDetail_driftInformation,
    stackResourceDetail_stackName,
    stackResourceDetail_stackId,
    stackResourceDetail_metadata,
    stackResourceDetail_physicalResourceId,
    stackResourceDetail_resourceStatusReason,
    stackResourceDetail_moduleInfo,
    stackResourceDetail_description,
    stackResourceDetail_logicalResourceId,
    stackResourceDetail_resourceType,
    stackResourceDetail_lastUpdatedTimestamp,
    stackResourceDetail_resourceStatus,

    -- ** StackResourceDrift
    stackResourceDrift_actualProperties,
    stackResourceDrift_physicalResourceIdContext,
    stackResourceDrift_physicalResourceId,
    stackResourceDrift_expectedProperties,
    stackResourceDrift_moduleInfo,
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
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- ** StackSet
    stackSet_status,
    stackSet_permissionModel,
    stackSet_executionRoleName,
    stackSet_capabilities,
    stackSet_organizationalUnitIds,
    stackSet_administrationRoleARN,
    stackSet_stackSetDriftDetectionDetails,
    stackSet_stackSetId,
    stackSet_tags,
    stackSet_stackSetARN,
    stackSet_autoDeployment,
    stackSet_description,
    stackSet_stackSetName,
    stackSet_templateBody,
    stackSet_parameters,

    -- ** StackSetDriftDetectionDetails
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_totalStackInstancesCount,

    -- ** StackSetOperation
    stackSetOperation_creationTimestamp,
    stackSetOperation_status,
    stackSetOperation_executionRoleName,
    stackSetOperation_endTimestamp,
    stackSetOperation_deploymentTargets,
    stackSetOperation_operationId,
    stackSetOperation_operationPreferences,
    stackSetOperation_administrationRoleARN,
    stackSetOperation_stackSetDriftDetectionDetails,
    stackSetOperation_stackSetId,
    stackSetOperation_action,
    stackSetOperation_retainStacks,

    -- ** StackSetOperationPreferences
    stackSetOperationPreferences_maxConcurrentPercentage,
    stackSetOperationPreferences_regionOrder,
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_failureTolerancePercentage,

    -- ** StackSetOperationResultSummary
    stackSetOperationResultSummary_accountGateResult,
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_statusReason,

    -- ** StackSetOperationSummary
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_status,
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_operationId,
    stackSetOperationSummary_action,

    -- ** StackSetSummary
    stackSetSummary_status,
    stackSetSummary_permissionModel,
    stackSetSummary_lastDriftCheckTimestamp,
    stackSetSummary_driftStatus,
    stackSetSummary_stackSetId,
    stackSetSummary_autoDeployment,
    stackSetSummary_description,
    stackSetSummary_stackSetName,

    -- ** StackSummary
    stackSummary_driftInformation,
    stackSummary_deletionTime,
    stackSummary_templateDescription,
    stackSummary_stackStatusReason,
    stackSummary_stackId,
    stackSummary_rootId,
    stackSummary_parentId,
    stackSummary_lastUpdatedTime,
    stackSummary_stackName,
    stackSummary_creationTime,
    stackSummary_stackStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateParameter
    templateParameter_parameterKey,
    templateParameter_description,
    templateParameter_noEcho,
    templateParameter_defaultValue,

    -- ** TypeSummary
    typeSummary_typeName,
    typeSummary_lastUpdated,
    typeSummary_defaultVersionId,
    typeSummary_description,
    typeSummary_type,
    typeSummary_typeArn,

    -- ** TypeVersionSummary
    typeVersionSummary_typeName,
    typeVersionSummary_arn,
    typeVersionSummary_versionId,
    typeVersionSummary_description,
    typeVersionSummary_isDefaultVersion,
    typeVersionSummary_type,
    typeVersionSummary_timeCreated,
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
import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.AccountLimit
import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.Change
import Network.AWS.CloudFormation.Types.ChangeSetSummary
import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.Export
import Network.AWS.CloudFormation.Types.LoggingConfig
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.ParameterConstraints
import Network.AWS.CloudFormation.Types.ParameterDeclaration
import Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Network.AWS.CloudFormation.Types.PropertyDifference
import Network.AWS.CloudFormation.Types.ResourceChange
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import Network.AWS.CloudFormation.Types.ResourceToImport
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.RollbackTrigger
import Network.AWS.CloudFormation.Types.Stack
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackEvent
import Network.AWS.CloudFormation.Types.StackInstance
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceFilter
import Network.AWS.CloudFormation.Types.StackInstanceSummary
import Network.AWS.CloudFormation.Types.StackResource
import Network.AWS.CloudFormation.Types.StackResourceDetail
import Network.AWS.CloudFormation.Types.StackResourceDrift
import Network.AWS.CloudFormation.Types.StackResourceDriftInformation
import Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
import Network.AWS.CloudFormation.Types.StackResourceSummary
import Network.AWS.CloudFormation.Types.StackSet
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetOperation
import Network.AWS.CloudFormation.Types.StackSetOperationPreferences
import Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
import Network.AWS.CloudFormation.Types.StackSetOperationSummary
import Network.AWS.CloudFormation.Types.StackSetSummary
import Network.AWS.CloudFormation.Types.StackSummary
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.CloudFormation.Types.TemplateParameter
import Network.AWS.CloudFormation.Types.TypeSummary
import Network.AWS.CloudFormation.Types.TypeVersionSummary
import Network.AWS.CloudFormation.UpdateStack
import Network.AWS.CloudFormation.UpdateStackInstances
import Network.AWS.CloudFormation.UpdateStackSet
import Network.AWS.CloudFormation.UpdateTerminationProtection
import Network.AWS.CloudFormation.ValidateTemplate
