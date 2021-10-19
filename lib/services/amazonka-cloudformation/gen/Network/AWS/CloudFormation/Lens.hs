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

    -- ** DescribeStackSetOperation
    describeStackSetOperation_callAs,
    describeStackSetOperation_stackSetName,
    describeStackSetOperation_operationId,
    describeStackSetOperationResponse_stackSetOperation,
    describeStackSetOperationResponse_httpStatus,

    -- ** ImportStacksToStackSet
    importStacksToStackSet_callAs,
    importStacksToStackSet_operationPreferences,
    importStacksToStackSet_operationId,
    importStacksToStackSet_stackSetName,
    importStacksToStackSet_stackIds,
    importStacksToStackSetResponse_operationId,
    importStacksToStackSetResponse_httpStatus,

    -- ** DeleteStack
    deleteStack_retainResources,
    deleteStack_clientRequestToken,
    deleteStack_roleARN,
    deleteStack_stackName,

    -- ** UpdateStack
    updateStack_disableRollback,
    updateStack_usePreviousTemplate,
    updateStack_notificationARNs,
    updateStack_stackPolicyBody,
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_parameters,
    updateStack_stackPolicyURL,
    updateStack_templateBody,
    updateStack_templateURL,
    updateStack_clientRequestToken,
    updateStack_capabilities,
    updateStack_rollbackConfiguration,
    updateStack_resourceTypes,
    updateStack_tags,
    updateStack_roleARN,
    updateStack_stackName,
    updateStackResponse_stackId,
    updateStackResponse_httpStatus,

    -- ** BatchDescribeTypeConfigurations
    batchDescribeTypeConfigurations_typeConfigurationIdentifiers,
    batchDescribeTypeConfigurationsResponse_unprocessedTypeConfigurations,
    batchDescribeTypeConfigurationsResponse_typeConfigurations,
    batchDescribeTypeConfigurationsResponse_errors,
    batchDescribeTypeConfigurationsResponse_httpStatus,

    -- ** SetTypeConfiguration
    setTypeConfiguration_typeName,
    setTypeConfiguration_typeArn,
    setTypeConfiguration_type,
    setTypeConfiguration_configurationAlias,
    setTypeConfiguration_configuration,
    setTypeConfigurationResponse_configurationArn,
    setTypeConfigurationResponse_httpStatus,

    -- ** GetTemplateSummary
    getTemplateSummary_callAs,
    getTemplateSummary_templateBody,
    getTemplateSummary_templateURL,
    getTemplateSummary_stackSetName,
    getTemplateSummary_stackName,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_httpStatus,

    -- ** ListChangeSets
    listChangeSets_nextToken,
    listChangeSets_stackName,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_summaries,
    listChangeSetsResponse_httpStatus,

    -- ** ListStackResources
    listStackResources_nextToken,
    listStackResources_stackName,
    listStackResourcesResponse_nextToken,
    listStackResourcesResponse_stackResourceSummaries,
    listStackResourcesResponse_httpStatus,

    -- ** UpdateStackInstances
    updateStackInstances_accounts,
    updateStackInstances_callAs,
    updateStackInstances_operationPreferences,
    updateStackInstances_operationId,
    updateStackInstances_deploymentTargets,
    updateStackInstances_parameterOverrides,
    updateStackInstances_stackSetName,
    updateStackInstances_regions,
    updateStackInstancesResponse_operationId,
    updateStackInstancesResponse_httpStatus,

    -- ** DeleteStackInstances
    deleteStackInstances_accounts,
    deleteStackInstances_callAs,
    deleteStackInstances_operationPreferences,
    deleteStackInstances_operationId,
    deleteStackInstances_deploymentTargets,
    deleteStackInstances_stackSetName,
    deleteStackInstances_regions,
    deleteStackInstances_retainStacks,
    deleteStackInstancesResponse_operationId,
    deleteStackInstancesResponse_httpStatus,

    -- ** DescribeType
    describeType_versionId,
    describeType_typeName,
    describeType_arn,
    describeType_publicVersionNumber,
    describeType_type,
    describeType_publisherId,
    describeTypeResponse_typeTestsStatusDescription,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_typeName,
    describeTypeResponse_arn,
    describeTypeResponse_publicVersionNumber,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_autoUpdate,
    describeTypeResponse_originalTypeName,
    describeTypeResponse_visibility,
    describeTypeResponse_schema,
    describeTypeResponse_originalTypeArn,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_isActivated,
    describeTypeResponse_requiredActivatedTypes,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_timeCreated,
    describeTypeResponse_type,
    describeTypeResponse_latestPublicVersion,
    describeTypeResponse_publisherId,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_description,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_configurationSchema,
    describeTypeResponse_provisioningType,
    describeTypeResponse_typeTestsStatus,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_httpStatus,

    -- ** CreateStackInstances
    createStackInstances_accounts,
    createStackInstances_callAs,
    createStackInstances_operationPreferences,
    createStackInstances_operationId,
    createStackInstances_deploymentTargets,
    createStackInstances_parameterOverrides,
    createStackInstances_stackSetName,
    createStackInstances_regions,
    createStackInstancesResponse_operationId,
    createStackInstancesResponse_httpStatus,

    -- ** ListTypeRegistrations
    listTypeRegistrations_typeName,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_nextToken,
    listTypeRegistrations_typeArn,
    listTypeRegistrations_type,
    listTypeRegistrations_maxResults,
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_httpStatus,

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

    -- ** CreateChangeSet
    createChangeSet_changeSetType,
    createChangeSet_usePreviousTemplate,
    createChangeSet_clientToken,
    createChangeSet_notificationARNs,
    createChangeSet_includeNestedStacks,
    createChangeSet_resourcesToImport,
    createChangeSet_parameters,
    createChangeSet_templateBody,
    createChangeSet_templateURL,
    createChangeSet_description,
    createChangeSet_capabilities,
    createChangeSet_rollbackConfiguration,
    createChangeSet_resourceTypes,
    createChangeSet_tags,
    createChangeSet_roleARN,
    createChangeSet_stackName,
    createChangeSet_changeSetName,
    createChangeSetResponse_id,
    createChangeSetResponse_stackId,
    createChangeSetResponse_httpStatus,

    -- ** ListStackSetOperations
    listStackSetOperations_callAs,
    listStackSetOperations_nextToken,
    listStackSetOperations_maxResults,
    listStackSetOperations_stackSetName,
    listStackSetOperationsResponse_nextToken,
    listStackSetOperationsResponse_summaries,
    listStackSetOperationsResponse_httpStatus,

    -- ** ExecuteChangeSet
    executeChangeSet_disableRollback,
    executeChangeSet_clientRequestToken,
    executeChangeSet_stackName,
    executeChangeSet_changeSetName,
    executeChangeSetResponse_httpStatus,

    -- ** DescribePublisher
    describePublisher_publisherId,
    describePublisherResponse_publisherStatus,
    describePublisherResponse_publisherProfile,
    describePublisherResponse_identityProvider,
    describePublisherResponse_publisherId,
    describePublisherResponse_httpStatus,

    -- ** ListStackInstances
    listStackInstances_stackInstanceRegion,
    listStackInstances_callAs,
    listStackInstances_filters,
    listStackInstances_nextToken,
    listStackInstances_stackInstanceAccount,
    listStackInstances_maxResults,
    listStackInstances_stackSetName,
    listStackInstancesResponse_nextToken,
    listStackInstancesResponse_summaries,
    listStackInstancesResponse_httpStatus,

    -- ** ContinueUpdateRollback
    continueUpdateRollback_resourcesToSkip,
    continueUpdateRollback_clientRequestToken,
    continueUpdateRollback_roleARN,
    continueUpdateRollback_stackName,
    continueUpdateRollbackResponse_httpStatus,

    -- ** ValidateTemplate
    validateTemplate_templateBody,
    validateTemplate_templateURL,
    validateTemplateResponse_declaredTransforms,
    validateTemplateResponse_capabilitiesReason,
    validateTemplateResponse_parameters,
    validateTemplateResponse_description,
    validateTemplateResponse_capabilities,
    validateTemplateResponse_httpStatus,

    -- ** CancelUpdateStack
    cancelUpdateStack_clientRequestToken,
    cancelUpdateStack_stackName,

    -- ** PublishType
    publishType_typeName,
    publishType_arn,
    publishType_publicVersionNumber,
    publishType_type,
    publishTypeResponse_publicTypeArn,
    publishTypeResponse_httpStatus,

    -- ** ListTypes
    listTypes_filters,
    listTypes_visibility,
    listTypes_nextToken,
    listTypes_deprecatedStatus,
    listTypes_type,
    listTypes_maxResults,
    listTypes_provisioningType,
    listTypesResponse_typeSummaries,
    listTypesResponse_nextToken,
    listTypesResponse_httpStatus,

    -- ** DescribeTypeRegistration
    describeTypeRegistration_registrationToken,
    describeTypeRegistrationResponse_typeVersionArn,
    describeTypeRegistrationResponse_progressStatus,
    describeTypeRegistrationResponse_typeArn,
    describeTypeRegistrationResponse_description,
    describeTypeRegistrationResponse_httpStatus,

    -- ** DetectStackDrift
    detectStackDrift_logicalResourceIds,
    detectStackDrift_stackName,
    detectStackDriftResponse_httpStatus,
    detectStackDriftResponse_stackDriftDetectionId,

    -- ** DescribeStackEvents
    describeStackEvents_nextToken,
    describeStackEvents_stackName,
    describeStackEventsResponse_nextToken,
    describeStackEventsResponse_stackEvents,
    describeStackEventsResponse_httpStatus,

    -- ** SignalResource
    signalResource_stackName,
    signalResource_logicalResourceId,
    signalResource_uniqueId,
    signalResource_status,

    -- ** SetStackPolicy
    setStackPolicy_stackPolicyBody,
    setStackPolicy_stackPolicyURL,
    setStackPolicy_stackName,

    -- ** ListImports
    listImports_nextToken,
    listImports_exportName,
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** DescribeStackResourceDrifts
    describeStackResourceDrifts_nextToken,
    describeStackResourceDrifts_maxResults,
    describeStackResourceDrifts_stackResourceDriftStatusFilters,
    describeStackResourceDrifts_stackName,
    describeStackResourceDriftsResponse_nextToken,
    describeStackResourceDriftsResponse_httpStatus,
    describeStackResourceDriftsResponse_stackResourceDrifts,

    -- ** ListStacks
    listStacks_nextToken,
    listStacks_stackStatusFilter,
    listStacksResponse_nextToken,
    listStacksResponse_stackSummaries,
    listStacksResponse_httpStatus,

    -- ** RegisterPublisher
    registerPublisher_connectionArn,
    registerPublisher_acceptTermsAndConditions,
    registerPublisherResponse_publisherId,
    registerPublisherResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_nextToken,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeStackResources
    describeStackResources_logicalResourceId,
    describeStackResources_physicalResourceId,
    describeStackResources_stackName,
    describeStackResourcesResponse_stackResources,
    describeStackResourcesResponse_httpStatus,

    -- ** DescribeStackInstance
    describeStackInstance_callAs,
    describeStackInstance_stackSetName,
    describeStackInstance_stackInstanceAccount,
    describeStackInstance_stackInstanceRegion,
    describeStackInstanceResponse_stackInstance,
    describeStackInstanceResponse_httpStatus,

    -- ** CreateStack
    createStack_disableRollback,
    createStack_notificationARNs,
    createStack_enableTerminationProtection,
    createStack_stackPolicyBody,
    createStack_parameters,
    createStack_stackPolicyURL,
    createStack_templateBody,
    createStack_templateURL,
    createStack_clientRequestToken,
    createStack_capabilities,
    createStack_rollbackConfiguration,
    createStack_onFailure,
    createStack_resourceTypes,
    createStack_tags,
    createStack_timeoutInMinutes,
    createStack_roleARN,
    createStack_stackName,
    createStackResponse_stackId,
    createStackResponse_httpStatus,

    -- ** UpdateStackSet
    updateStackSet_administrationRoleARN,
    updateStackSet_usePreviousTemplate,
    updateStackSet_accounts,
    updateStackSet_callAs,
    updateStackSet_regions,
    updateStackSet_autoDeployment,
    updateStackSet_permissionModel,
    updateStackSet_parameters,
    updateStackSet_operationPreferences,
    updateStackSet_operationId,
    updateStackSet_templateBody,
    updateStackSet_templateURL,
    updateStackSet_deploymentTargets,
    updateStackSet_description,
    updateStackSet_capabilities,
    updateStackSet_tags,
    updateStackSet_executionRoleName,
    updateStackSet_stackSetName,
    updateStackSetResponse_operationId,
    updateStackSetResponse_httpStatus,

    -- ** DeleteStackSet
    deleteStackSet_callAs,
    deleteStackSet_stackSetName,
    deleteStackSetResponse_httpStatus,

    -- ** EstimateTemplateCost
    estimateTemplateCost_parameters,
    estimateTemplateCost_templateBody,
    estimateTemplateCost_templateURL,
    estimateTemplateCostResponse_url,
    estimateTemplateCostResponse_httpStatus,

    -- ** DeleteChangeSet
    deleteChangeSet_stackName,
    deleteChangeSet_changeSetName,
    deleteChangeSetResponse_httpStatus,

    -- ** ListStackSets
    listStackSets_status,
    listStackSets_callAs,
    listStackSets_nextToken,
    listStackSets_maxResults,
    listStackSetsResponse_nextToken,
    listStackSetsResponse_summaries,
    listStackSetsResponse_httpStatus,

    -- ** ListExports
    listExports_nextToken,
    listExportsResponse_nextToken,
    listExportsResponse_exports,
    listExportsResponse_httpStatus,

    -- ** DescribeStackDriftDetectionStatus
    describeStackDriftDetectionStatus_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_stackDriftStatus,
    describeStackDriftDetectionStatusResponse_driftedStackResourceCount,
    describeStackDriftDetectionStatusResponse_detectionStatusReason,
    describeStackDriftDetectionStatusResponse_httpStatus,
    describeStackDriftDetectionStatusResponse_stackId,
    describeStackDriftDetectionStatusResponse_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_detectionStatus,
    describeStackDriftDetectionStatusResponse_timestamp,

    -- ** RollbackStack
    rollbackStack_clientRequestToken,
    rollbackStack_roleARN,
    rollbackStack_stackName,
    rollbackStackResponse_stackId,
    rollbackStackResponse_httpStatus,

    -- ** CreateStackSet
    createStackSet_administrationRoleARN,
    createStackSet_callAs,
    createStackSet_autoDeployment,
    createStackSet_permissionModel,
    createStackSet_parameters,
    createStackSet_templateBody,
    createStackSet_templateURL,
    createStackSet_stackId,
    createStackSet_clientRequestToken,
    createStackSet_description,
    createStackSet_capabilities,
    createStackSet_tags,
    createStackSet_executionRoleName,
    createStackSet_stackSetName,
    createStackSetResponse_stackSetId,
    createStackSetResponse_httpStatus,

    -- ** DeregisterType
    deregisterType_versionId,
    deregisterType_typeName,
    deregisterType_arn,
    deregisterType_type,
    deregisterTypeResponse_httpStatus,

    -- ** DeactivateType
    deactivateType_typeName,
    deactivateType_arn,
    deactivateType_type,
    deactivateTypeResponse_httpStatus,

    -- ** RecordHandlerProgress
    recordHandlerProgress_resourceModel,
    recordHandlerProgress_statusMessage,
    recordHandlerProgress_errorCode,
    recordHandlerProgress_currentOperationStatus,
    recordHandlerProgress_clientRequestToken,
    recordHandlerProgress_bearerToken,
    recordHandlerProgress_operationStatus,
    recordHandlerProgressResponse_httpStatus,

    -- ** ListTypeVersions
    listTypeVersions_typeName,
    listTypeVersions_arn,
    listTypeVersions_nextToken,
    listTypeVersions_deprecatedStatus,
    listTypeVersions_type,
    listTypeVersions_publisherId,
    listTypeVersions_maxResults,
    listTypeVersionsResponse_nextToken,
    listTypeVersionsResponse_typeVersionSummaries,
    listTypeVersionsResponse_httpStatus,

    -- ** SetTypeDefaultVersion
    setTypeDefaultVersion_versionId,
    setTypeDefaultVersion_typeName,
    setTypeDefaultVersion_arn,
    setTypeDefaultVersion_type,
    setTypeDefaultVersionResponse_httpStatus,

    -- ** UpdateTerminationProtection
    updateTerminationProtection_enableTerminationProtection,
    updateTerminationProtection_stackName,
    updateTerminationProtectionResponse_stackId,
    updateTerminationProtectionResponse_httpStatus,

    -- ** TestType
    testType_versionId,
    testType_typeName,
    testType_arn,
    testType_logDeliveryBucket,
    testType_type,
    testTypeResponse_typeVersionArn,
    testTypeResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_changeSetName,
    getTemplate_templateStage,
    getTemplate_stackName,
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_templateBody,
    getTemplateResponse_httpStatus,

    -- ** DetectStackSetDrift
    detectStackSetDrift_callAs,
    detectStackSetDrift_operationPreferences,
    detectStackSetDrift_operationId,
    detectStackSetDrift_stackSetName,
    detectStackSetDriftResponse_operationId,
    detectStackSetDriftResponse_httpStatus,

    -- ** DetectStackResourceDrift
    detectStackResourceDrift_stackName,
    detectStackResourceDrift_logicalResourceId,
    detectStackResourceDriftResponse_httpStatus,
    detectStackResourceDriftResponse_stackResourceDrift,

    -- ** DescribeChangeSet
    describeChangeSet_nextToken,
    describeChangeSet_stackName,
    describeChangeSet_changeSetName,
    describeChangeSetResponse_creationTime,
    describeChangeSetResponse_parentChangeSetId,
    describeChangeSetResponse_changes,
    describeChangeSetResponse_notificationARNs,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_executionStatus,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_includeNestedStacks,
    describeChangeSetResponse_nextToken,
    describeChangeSetResponse_rootChangeSetId,
    describeChangeSetResponse_parameters,
    describeChangeSetResponse_statusReason,
    describeChangeSetResponse_stackId,
    describeChangeSetResponse_description,
    describeChangeSetResponse_capabilities,
    describeChangeSetResponse_rollbackConfiguration,
    describeChangeSetResponse_tags,
    describeChangeSetResponse_stackName,
    describeChangeSetResponse_httpStatus,
    describeChangeSetResponse_status,

    -- ** DescribeStackSet
    describeStackSet_callAs,
    describeStackSet_stackSetName,
    describeStackSetResponse_stackSet,
    describeStackSetResponse_httpStatus,

    -- ** ListStackSetOperationResults
    listStackSetOperationResults_callAs,
    listStackSetOperationResults_nextToken,
    listStackSetOperationResults_maxResults,
    listStackSetOperationResults_stackSetName,
    listStackSetOperationResults_operationId,
    listStackSetOperationResultsResponse_nextToken,
    listStackSetOperationResultsResponse_summaries,
    listStackSetOperationResultsResponse_httpStatus,

    -- ** RegisterType
    registerType_executionRoleArn,
    registerType_type,
    registerType_clientRequestToken,
    registerType_loggingConfig,
    registerType_typeName,
    registerType_schemaHandlerPackage,
    registerTypeResponse_registrationToken,
    registerTypeResponse_httpStatus,

    -- ** ActivateType
    activateType_typeName,
    activateType_versionBump,
    activateType_executionRoleArn,
    activateType_autoUpdate,
    activateType_typeNameAlias,
    activateType_majorVersion,
    activateType_publicTypeArn,
    activateType_type,
    activateType_publisherId,
    activateType_loggingConfig,
    activateTypeResponse_arn,
    activateTypeResponse_httpStatus,

    -- ** StopStackSetOperation
    stopStackSetOperation_callAs,
    stopStackSetOperation_stackSetName,
    stopStackSetOperation_operationId,
    stopStackSetOperationResponse_httpStatus,

    -- ** DescribeStackResource
    describeStackResource_stackName,
    describeStackResource_logicalResourceId,
    describeStackResourceResponse_stackResourceDetail,
    describeStackResourceResponse_httpStatus,

    -- * Types

    -- ** AccountGateResult
    accountGateResult_status,
    accountGateResult_statusReason,

    -- ** AccountLimit
    accountLimit_value,
    accountLimit_name,

    -- ** AutoDeployment
    autoDeployment_enabled,
    autoDeployment_retainStacksOnAccountRemoval,

    -- ** BatchDescribeTypeConfigurationsError
    batchDescribeTypeConfigurationsError_typeConfigurationIdentifier,
    batchDescribeTypeConfigurationsError_errorCode,
    batchDescribeTypeConfigurationsError_errorMessage,

    -- ** Change
    change_resourceChange,
    change_type,

    -- ** ChangeSetSummary
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

    -- ** DeploymentTargets
    deploymentTargets_accounts,
    deploymentTargets_organizationalUnitIds,
    deploymentTargets_accountsUrl,

    -- ** Export
    export_value,
    export_exportingStackId,
    export_name,

    -- ** LoggingConfig
    loggingConfig_logRoleArn,
    loggingConfig_logGroupName,

    -- ** ModuleInfo
    moduleInfo_typeHierarchy,
    moduleInfo_logicalIdHierarchy,

    -- ** Output
    output_outputValue,
    output_outputKey,
    output_exportName,
    output_description,

    -- ** Parameter
    parameter_parameterValue,
    parameter_resolvedValue,
    parameter_parameterKey,
    parameter_usePreviousValue,

    -- ** ParameterConstraints
    parameterConstraints_allowedValues,

    -- ** ParameterDeclaration
    parameterDeclaration_parameterKey,
    parameterDeclaration_parameterType,
    parameterDeclaration_parameterConstraints,
    parameterDeclaration_defaultValue,
    parameterDeclaration_noEcho,
    parameterDeclaration_description,

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
    requiredActivatedType_typeNameAlias,
    requiredActivatedType_supportedMajorVersions,
    requiredActivatedType_publisherId,

    -- ** ResourceChange
    resourceChange_logicalResourceId,
    resourceChange_physicalResourceId,
    resourceChange_resourceType,
    resourceChange_action,
    resourceChange_changeSetId,
    resourceChange_moduleInfo,
    resourceChange_scope,
    resourceChange_details,
    resourceChange_replacement,

    -- ** ResourceChangeDetail
    resourceChangeDetail_causingEntity,
    resourceChangeDetail_changeSource,
    resourceChangeDetail_evaluation,
    resourceChangeDetail_target,

    -- ** ResourceIdentifierSummary
    resourceIdentifierSummary_resourceType,
    resourceIdentifierSummary_logicalResourceIds,
    resourceIdentifierSummary_resourceIdentifiers,

    -- ** ResourceTargetDefinition
    resourceTargetDefinition_attribute,
    resourceTargetDefinition_requiresRecreation,
    resourceTargetDefinition_name,

    -- ** ResourceToImport
    resourceToImport_resourceType,
    resourceToImport_logicalResourceId,
    resourceToImport_resourceIdentifier,

    -- ** RollbackConfiguration
    rollbackConfiguration_rollbackTriggers,
    rollbackConfiguration_monitoringTimeInMinutes,

    -- ** RollbackTrigger
    rollbackTrigger_arn,
    rollbackTrigger_type,

    -- ** Stack
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

    -- ** StackDriftInformation
    stackDriftInformation_lastCheckTimestamp,
    stackDriftInformation_stackDriftStatus,

    -- ** StackDriftInformationSummary
    stackDriftInformationSummary_lastCheckTimestamp,
    stackDriftInformationSummary_stackDriftStatus,

    -- ** StackEvent
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

    -- ** StackInstance
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

    -- ** StackInstanceComprehensiveStatus
    stackInstanceComprehensiveStatus_detailedStatus,

    -- ** StackInstanceFilter
    stackInstanceFilter_values,
    stackInstanceFilter_name,

    -- ** StackInstanceSummary
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

    -- ** StackResource
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

    -- ** StackResourceDetail
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

    -- ** StackResourceDrift
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

    -- ** StackResourceDriftInformation
    stackResourceDriftInformation_lastCheckTimestamp,
    stackResourceDriftInformation_stackResourceDriftStatus,

    -- ** StackResourceDriftInformationSummary
    stackResourceDriftInformationSummary_lastCheckTimestamp,
    stackResourceDriftInformationSummary_stackResourceDriftStatus,

    -- ** StackResourceSummary
    stackResourceSummary_physicalResourceId,
    stackResourceSummary_resourceStatusReason,
    stackResourceSummary_driftInformation,
    stackResourceSummary_moduleInfo,
    stackResourceSummary_logicalResourceId,
    stackResourceSummary_resourceType,
    stackResourceSummary_lastUpdatedTimestamp,
    stackResourceSummary_resourceStatus,

    -- ** StackSet
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

    -- ** StackSetDriftDetectionDetails
    stackSetDriftDetectionDetails_lastDriftCheckTimestamp,
    stackSetDriftDetectionDetails_totalStackInstancesCount,
    stackSetDriftDetectionDetails_inProgressStackInstancesCount,
    stackSetDriftDetectionDetails_driftedStackInstancesCount,
    stackSetDriftDetectionDetails_driftDetectionStatus,
    stackSetDriftDetectionDetails_driftStatus,
    stackSetDriftDetectionDetails_failedStackInstancesCount,
    stackSetDriftDetectionDetails_inSyncStackInstancesCount,

    -- ** StackSetOperation
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

    -- ** StackSetOperationPreferences
    stackSetOperationPreferences_regionOrder,
    stackSetOperationPreferences_maxConcurrentCount,
    stackSetOperationPreferences_maxConcurrentPercentage,
    stackSetOperationPreferences_failureToleranceCount,
    stackSetOperationPreferences_regionConcurrencyType,
    stackSetOperationPreferences_failureTolerancePercentage,

    -- ** StackSetOperationResultSummary
    stackSetOperationResultSummary_status,
    stackSetOperationResultSummary_account,
    stackSetOperationResultSummary_accountGateResult,
    stackSetOperationResultSummary_organizationalUnitId,
    stackSetOperationResultSummary_region,
    stackSetOperationResultSummary_statusReason,

    -- ** StackSetOperationSummary
    stackSetOperationSummary_status,
    stackSetOperationSummary_action,
    stackSetOperationSummary_endTimestamp,
    stackSetOperationSummary_creationTimestamp,
    stackSetOperationSummary_operationId,

    -- ** StackSetSummary
    stackSetSummary_status,
    stackSetSummary_lastDriftCheckTimestamp,
    stackSetSummary_autoDeployment,
    stackSetSummary_driftStatus,
    stackSetSummary_permissionModel,
    stackSetSummary_stackSetName,
    stackSetSummary_description,
    stackSetSummary_stackSetId,

    -- ** StackSummary
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateParameter
    templateParameter_parameterKey,
    templateParameter_defaultValue,
    templateParameter_noEcho,
    templateParameter_description,

    -- ** TypeConfigurationDetails
    typeConfigurationDetails_lastUpdated,
    typeConfigurationDetails_typeName,
    typeConfigurationDetails_arn,
    typeConfigurationDetails_alias,
    typeConfigurationDetails_typeArn,
    typeConfigurationDetails_configuration,
    typeConfigurationDetails_isDefaultConfiguration,

    -- ** TypeConfigurationIdentifier
    typeConfigurationIdentifier_typeName,
    typeConfigurationIdentifier_typeConfigurationArn,
    typeConfigurationIdentifier_typeConfigurationAlias,
    typeConfigurationIdentifier_typeArn,
    typeConfigurationIdentifier_type,

    -- ** TypeFilters
    typeFilters_typeNamePrefix,
    typeFilters_category,
    typeFilters_publisherId,

    -- ** TypeSummary
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

    -- ** TypeVersionSummary
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
import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.AccountLimit
import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.BatchDescribeTypeConfigurationsError
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
import Network.AWS.CloudFormation.Types.RequiredActivatedType
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
import Network.AWS.CloudFormation.Types.TypeConfigurationDetails
import Network.AWS.CloudFormation.Types.TypeConfigurationIdentifier
import Network.AWS.CloudFormation.Types.TypeFilters
import Network.AWS.CloudFormation.Types.TypeSummary
import Network.AWS.CloudFormation.Types.TypeVersionSummary
import Network.AWS.CloudFormation.UpdateStack
import Network.AWS.CloudFormation.UpdateStackInstances
import Network.AWS.CloudFormation.UpdateStackSet
import Network.AWS.CloudFormation.UpdateTerminationProtection
import Network.AWS.CloudFormation.ValidateTemplate
