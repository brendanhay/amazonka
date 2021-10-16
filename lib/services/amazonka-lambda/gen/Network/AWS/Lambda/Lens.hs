{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Lens
  ( -- * Operations

    -- ** DeleteAlias
    deleteAlias_functionName,
    deleteAlias_name,

    -- ** GetLayerVersionPolicy
    getLayerVersionPolicy_layerName,
    getLayerVersionPolicy_versionNumber,
    getLayerVersionPolicyResponse_revisionId,
    getLayerVersionPolicyResponse_policy,
    getLayerVersionPolicyResponse_httpStatus,

    -- ** PutFunctionCodeSigningConfig
    putFunctionCodeSigningConfig_codeSigningConfigArn,
    putFunctionCodeSigningConfig_functionName,
    putFunctionCodeSigningConfigResponse_httpStatus,
    putFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    putFunctionCodeSigningConfigResponse_functionName,

    -- ** UpdateAlias
    updateAlias_revisionId,
    updateAlias_routingConfig,
    updateAlias_functionVersion,
    updateAlias_description,
    updateAlias_functionName,
    updateAlias_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,

    -- ** UpdateEventSourceMapping
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** GetFunction
    getFunction_qualifier,
    getFunction_functionName,
    getFunctionResponse_configuration,
    getFunctionResponse_code,
    getFunctionResponse_tags,
    getFunctionResponse_concurrency,
    getFunctionResponse_httpStatus,

    -- ** ListEventSourceMappings
    listEventSourceMappings_eventSourceArn,
    listEventSourceMappings_functionName,
    listEventSourceMappings_maxItems,
    listEventSourceMappings_marker,
    listEventSourceMappingsResponse_eventSourceMappings,
    listEventSourceMappingsResponse_nextMarker,
    listEventSourceMappingsResponse_httpStatus,

    -- ** GetFunctionConfiguration
    getFunctionConfiguration_qualifier,
    getFunctionConfiguration_functionName,
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** DeleteEventSourceMapping
    deleteEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** GetLayerVersionByArn
    getLayerVersionByArn_arn,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_version,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_description,
    getLayerVersionResponse_licenseInfo,

    -- ** CreateEventSourceMapping
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_enabled,
    createEventSourceMapping_queues,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_topics,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_functionName,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** GetFunctionConcurrency
    getFunctionConcurrency_functionName,
    getFunctionConcurrencyResponse_reservedConcurrentExecutions,
    getFunctionConcurrencyResponse_httpStatus,

    -- ** UpdateFunctionCode
    updateFunctionCode_imageUri,
    updateFunctionCode_publish,
    updateFunctionCode_revisionId,
    updateFunctionCode_s3Bucket,
    updateFunctionCode_dryRun,
    updateFunctionCode_zipFile,
    updateFunctionCode_s3Key,
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_functionName,
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** DeleteFunctionConcurrency
    deleteFunctionConcurrency_functionName,

    -- ** ListProvisionedConcurrencyConfigs
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_functionName,
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_httpStatus,

    -- ** DeleteProvisionedConcurrencyConfig
    deleteProvisionedConcurrencyConfig_functionName,
    deleteProvisionedConcurrencyConfig_qualifier,

    -- ** ListFunctions
    listFunctions_masterRegion,
    listFunctions_functionVersion,
    listFunctions_maxItems,
    listFunctions_marker,
    listFunctionsResponse_functions,
    listFunctionsResponse_nextMarker,
    listFunctionsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,

    -- ** UpdateFunctionConfiguration
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_functionName,
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,

    -- ** Invoke
    invoke_logType,
    invoke_invocationType,
    invoke_qualifier,
    invoke_clientContext,
    invoke_functionName,
    invoke_payload,
    invokeResponse_payload,
    invokeResponse_logResult,
    invokeResponse_executedVersion,
    invokeResponse_functionError,
    invokeResponse_statusCode,

    -- ** ListLayerVersions
    listLayerVersions_maxItems,
    listLayerVersions_marker,
    listLayerVersions_compatibleRuntime,
    listLayerVersions_layerName,
    listLayerVersionsResponse_nextMarker,
    listLayerVersionsResponse_layerVersions,
    listLayerVersionsResponse_httpStatus,

    -- ** CreateCodeSigningConfig
    createCodeSigningConfig_description,
    createCodeSigningConfig_codeSigningPolicies,
    createCodeSigningConfig_allowedPublishers,
    createCodeSigningConfigResponse_httpStatus,
    createCodeSigningConfigResponse_codeSigningConfig,

    -- ** DeleteLayerVersion
    deleteLayerVersion_layerName,
    deleteLayerVersion_versionNumber,

    -- ** GetAlias
    getAlias_functionName,
    getAlias_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,

    -- ** ListFunctionEventInvokeConfigs
    listFunctionEventInvokeConfigs_maxItems,
    listFunctionEventInvokeConfigs_marker,
    listFunctionEventInvokeConfigs_functionName,
    listFunctionEventInvokeConfigsResponse_nextMarker,
    listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs,
    listFunctionEventInvokeConfigsResponse_httpStatus,

    -- ** DeleteFunctionEventInvokeConfig
    deleteFunctionEventInvokeConfig_qualifier,
    deleteFunctionEventInvokeConfig_functionName,

    -- ** UpdateFunctionEventInvokeConfig
    updateFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    updateFunctionEventInvokeConfig_qualifier,
    updateFunctionEventInvokeConfig_destinationConfig,
    updateFunctionEventInvokeConfig_maximumRetryAttempts,
    updateFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** DeleteCodeSigningConfig
    deleteCodeSigningConfig_codeSigningConfigArn,
    deleteCodeSigningConfigResponse_httpStatus,

    -- ** ListCodeSigningConfigs
    listCodeSigningConfigs_maxItems,
    listCodeSigningConfigs_marker,
    listCodeSigningConfigsResponse_nextMarker,
    listCodeSigningConfigsResponse_codeSigningConfigs,
    listCodeSigningConfigsResponse_httpStatus,

    -- ** UpdateCodeSigningConfig
    updateCodeSigningConfig_allowedPublishers,
    updateCodeSigningConfig_description,
    updateCodeSigningConfig_codeSigningPolicies,
    updateCodeSigningConfig_codeSigningConfigArn,
    updateCodeSigningConfigResponse_httpStatus,
    updateCodeSigningConfigResponse_codeSigningConfig,

    -- ** ListVersionsByFunction
    listVersionsByFunction_maxItems,
    listVersionsByFunction_marker,
    listVersionsByFunction_functionName,
    listVersionsByFunctionResponse_versions,
    listVersionsByFunctionResponse_nextMarker,
    listVersionsByFunctionResponse_httpStatus,

    -- ** AddPermission
    addPermission_revisionId,
    addPermission_qualifier,
    addPermission_eventSourceToken,
    addPermission_sourceAccount,
    addPermission_sourceArn,
    addPermission_functionName,
    addPermission_statementId,
    addPermission_action,
    addPermission_principal,
    addPermissionResponse_statement,
    addPermissionResponse_httpStatus,

    -- ** GetLayerVersion
    getLayerVersion_layerName,
    getLayerVersion_versionNumber,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_version,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_description,
    getLayerVersionResponse_licenseInfo,

    -- ** DeleteFunctionCodeSigningConfig
    deleteFunctionCodeSigningConfig_functionName,

    -- ** RemoveLayerVersionPermission
    removeLayerVersionPermission_revisionId,
    removeLayerVersionPermission_layerName,
    removeLayerVersionPermission_versionNumber,
    removeLayerVersionPermission_statementId,

    -- ** ListFunctionsByCodeSigningConfig
    listFunctionsByCodeSigningConfig_maxItems,
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_codeSigningConfigArn,
    listFunctionsByCodeSigningConfigResponse_functionArns,
    listFunctionsByCodeSigningConfigResponse_nextMarker,
    listFunctionsByCodeSigningConfigResponse_httpStatus,

    -- ** GetProvisionedConcurrencyConfig
    getProvisionedConcurrencyConfig_functionName,
    getProvisionedConcurrencyConfig_qualifier,
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_qualifier,
    getPolicy_functionName,
    getPolicyResponse_revisionId,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** CreateFunction
    createFunction_vpcConfig,
    createFunction_memorySize,
    createFunction_publish,
    createFunction_codeSigningConfigArn,
    createFunction_timeout,
    createFunction_handler,
    createFunction_deadLetterConfig,
    createFunction_imageConfig,
    createFunction_environment,
    createFunction_kmsKeyArn,
    createFunction_runtime,
    createFunction_tags,
    createFunction_description,
    createFunction_tracingConfig,
    createFunction_layers,
    createFunction_fileSystemConfigs,
    createFunction_packageType,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** PutProvisionedConcurrencyConfig
    putProvisionedConcurrencyConfig_functionName,
    putProvisionedConcurrencyConfig_qualifier,
    putProvisionedConcurrencyConfig_provisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_status,
    putProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_lastModified,
    putProvisionedConcurrencyConfigResponse_statusReason,
    putProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** PutFunctionConcurrency
    putFunctionConcurrency_functionName,
    putFunctionConcurrency_reservedConcurrentExecutions,
    concurrency_reservedConcurrentExecutions,

    -- ** ListTags
    listTags_resource,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeleteFunction
    deleteFunction_qualifier,
    deleteFunction_functionName,

    -- ** GetEventSourceMapping
    getEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** PublishVersion
    publishVersion_revisionId,
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_functionName,
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** AddLayerVersionPermission
    addLayerVersionPermission_revisionId,
    addLayerVersionPermission_organizationId,
    addLayerVersionPermission_layerName,
    addLayerVersionPermission_versionNumber,
    addLayerVersionPermission_statementId,
    addLayerVersionPermission_action,
    addLayerVersionPermission_principal,
    addLayerVersionPermissionResponse_revisionId,
    addLayerVersionPermissionResponse_statement,
    addLayerVersionPermissionResponse_httpStatus,

    -- ** GetFunctionCodeSigningConfig
    getFunctionCodeSigningConfig_functionName,
    getFunctionCodeSigningConfigResponse_httpStatus,
    getFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    getFunctionCodeSigningConfigResponse_functionName,

    -- ** PublishLayerVersion
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_description,
    publishLayerVersion_licenseInfo,
    publishLayerVersion_layerName,
    publishLayerVersion_content,
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_httpStatus,

    -- ** PutFunctionEventInvokeConfig
    putFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    putFunctionEventInvokeConfig_qualifier,
    putFunctionEventInvokeConfig_destinationConfig,
    putFunctionEventInvokeConfig_maximumRetryAttempts,
    putFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** RemovePermission
    removePermission_revisionId,
    removePermission_qualifier,
    removePermission_functionName,
    removePermission_statementId,

    -- ** ListAliases
    listAliases_functionVersion,
    listAliases_maxItems,
    listAliases_marker,
    listAliases_functionName,
    listAliasesResponse_nextMarker,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,

    -- ** CreateAlias
    createAlias_routingConfig,
    createAlias_description,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,

    -- ** GetFunctionEventInvokeConfig
    getFunctionEventInvokeConfig_qualifier,
    getFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountLimit,
    getAccountSettingsResponse_accountUsage,
    getAccountSettingsResponse_httpStatus,

    -- ** GetCodeSigningConfig
    getCodeSigningConfig_codeSigningConfigArn,
    getCodeSigningConfigResponse_httpStatus,
    getCodeSigningConfigResponse_codeSigningConfig,

    -- ** ListLayers
    listLayers_maxItems,
    listLayers_marker,
    listLayers_compatibleRuntime,
    listLayersResponse_nextMarker,
    listLayersResponse_layers,
    listLayersResponse_httpStatus,

    -- * Types

    -- ** AccountLimit
    accountLimit_codeSizeUnzipped,
    accountLimit_concurrentExecutions,
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_codeSizeZipped,

    -- ** AccountUsage
    accountUsage_functionCount,
    accountUsage_totalCodeSize,

    -- ** AliasConfiguration
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_description,
    aliasConfiguration_aliasArn,

    -- ** AliasRoutingConfiguration
    aliasRoutingConfiguration_additionalVersionWeights,

    -- ** AllowedPublishers
    allowedPublishers_signingProfileVersionArns,

    -- ** CodeSigningConfig
    codeSigningConfig_description,
    codeSigningConfig_codeSigningConfigId,
    codeSigningConfig_codeSigningConfigArn,
    codeSigningConfig_allowedPublishers,
    codeSigningConfig_codeSigningPolicies,
    codeSigningConfig_lastModified,

    -- ** CodeSigningPolicies
    codeSigningPolicies_untrustedArtifactOnDeployment,

    -- ** Concurrency
    concurrency_reservedConcurrentExecutions,

    -- ** DeadLetterConfig
    deadLetterConfig_targetArn,

    -- ** DestinationConfig
    destinationConfig_onFailure,
    destinationConfig_onSuccess,

    -- ** Environment
    environment_variables,

    -- ** EnvironmentError
    environmentError_message,
    environmentError_errorCode,

    -- ** EnvironmentResponse
    environmentResponse_variables,
    environmentResponse_error,

    -- ** EventSourceMappingConfiguration
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** FileSystemConfig
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- ** FunctionCode
    functionCode_imageUri,
    functionCode_s3Bucket,
    functionCode_zipFile,
    functionCode_s3Key,
    functionCode_s3ObjectVersion,

    -- ** FunctionCodeLocation
    functionCodeLocation_imageUri,
    functionCodeLocation_resolvedImageUri,
    functionCodeLocation_location,
    functionCodeLocation_repositoryType,

    -- ** FunctionConfiguration
    functionConfiguration_vpcConfig,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_memorySize,
    functionConfiguration_masterArn,
    functionConfiguration_revisionId,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_codeSha256,
    functionConfiguration_stateReason,
    functionConfiguration_timeout,
    functionConfiguration_handler,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_environment,
    functionConfiguration_functionName,
    functionConfiguration_version,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_state,
    functionConfiguration_functionArn,
    functionConfiguration_runtime,
    functionConfiguration_role,
    functionConfiguration_signingJobArn,
    functionConfiguration_stateReasonCode,
    functionConfiguration_description,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_tracingConfig,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSize,
    functionConfiguration_layers,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_packageType,

    -- ** FunctionEventInvokeConfig
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** GetLayerVersionResponse
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_version,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_description,
    getLayerVersionResponse_licenseInfo,

    -- ** ImageConfig
    imageConfig_entryPoint,
    imageConfig_workingDirectory,
    imageConfig_command,

    -- ** ImageConfigError
    imageConfigError_message,
    imageConfigError_errorCode,

    -- ** ImageConfigResponse
    imageConfigResponse_imageConfig,
    imageConfigResponse_error,

    -- ** Layer
    layer_signingProfileVersionArn,
    layer_arn,
    layer_signingJobArn,
    layer_codeSize,

    -- ** LayerVersionContentInput
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_s3ObjectVersion,

    -- ** LayerVersionContentOutput
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_location,

    -- ** LayerVersionsListItem
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_description,
    layerVersionsListItem_licenseInfo,

    -- ** LayersListItem
    layersListItem_layerArn,
    layersListItem_layerName,
    layersListItem_latestMatchingVersion,

    -- ** OnFailure
    onFailure_destination,

    -- ** OnSuccess
    onSuccess_destination,

    -- ** ProvisionedConcurrencyConfigListItem
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_statusReason,

    -- ** SelfManagedEventSource
    selfManagedEventSource_endpoints,

    -- ** SourceAccessConfiguration
    sourceAccessConfiguration_uri,
    sourceAccessConfiguration_type,

    -- ** TracingConfig
    tracingConfig_mode,

    -- ** TracingConfigResponse
    tracingConfigResponse_mode,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,

    -- ** VpcConfigResponse
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_vpcId,
  )
where

import Network.AWS.Lambda.AddLayerVersionPermission
import Network.AWS.Lambda.AddPermission
import Network.AWS.Lambda.CreateAlias
import Network.AWS.Lambda.CreateCodeSigningConfig
import Network.AWS.Lambda.CreateEventSourceMapping
import Network.AWS.Lambda.CreateFunction
import Network.AWS.Lambda.DeleteAlias
import Network.AWS.Lambda.DeleteCodeSigningConfig
import Network.AWS.Lambda.DeleteEventSourceMapping
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
import Network.AWS.Lambda.DeleteFunctionConcurrency
import Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
import Network.AWS.Lambda.DeleteLayerVersion
import Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
import Network.AWS.Lambda.GetAccountSettings
import Network.AWS.Lambda.GetAlias
import Network.AWS.Lambda.GetCodeSigningConfig
import Network.AWS.Lambda.GetEventSourceMapping
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionCodeSigningConfig
import Network.AWS.Lambda.GetFunctionConcurrency
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.GetFunctionEventInvokeConfig
import Network.AWS.Lambda.GetLayerVersion
import Network.AWS.Lambda.GetLayerVersionByArn
import Network.AWS.Lambda.GetLayerVersionPolicy
import Network.AWS.Lambda.GetPolicy
import Network.AWS.Lambda.GetProvisionedConcurrencyConfig
import Network.AWS.Lambda.Invoke
import Network.AWS.Lambda.ListAliases
import Network.AWS.Lambda.ListCodeSigningConfigs
import Network.AWS.Lambda.ListEventSourceMappings
import Network.AWS.Lambda.ListFunctionEventInvokeConfigs
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
import Network.AWS.Lambda.ListLayerVersions
import Network.AWS.Lambda.ListLayers
import Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
import Network.AWS.Lambda.ListTags
import Network.AWS.Lambda.ListVersionsByFunction
import Network.AWS.Lambda.PublishLayerVersion
import Network.AWS.Lambda.PublishVersion
import Network.AWS.Lambda.PutFunctionCodeSigningConfig
import Network.AWS.Lambda.PutFunctionConcurrency
import Network.AWS.Lambda.PutFunctionEventInvokeConfig
import Network.AWS.Lambda.PutProvisionedConcurrencyConfig
import Network.AWS.Lambda.RemoveLayerVersionPermission
import Network.AWS.Lambda.RemovePermission
import Network.AWS.Lambda.TagResource
import Network.AWS.Lambda.Types.AccountLimit
import Network.AWS.Lambda.Types.AccountUsage
import Network.AWS.Lambda.Types.AliasConfiguration
import Network.AWS.Lambda.Types.AliasRoutingConfiguration
import Network.AWS.Lambda.Types.AllowedPublishers
import Network.AWS.Lambda.Types.CodeSigningConfig
import Network.AWS.Lambda.Types.CodeSigningPolicies
import Network.AWS.Lambda.Types.Concurrency
import Network.AWS.Lambda.Types.DeadLetterConfig
import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.Environment
import Network.AWS.Lambda.Types.EnvironmentError
import Network.AWS.Lambda.Types.EnvironmentResponse
import Network.AWS.Lambda.Types.EventSourceMappingConfiguration
import Network.AWS.Lambda.Types.FileSystemConfig
import Network.AWS.Lambda.Types.FunctionCode
import Network.AWS.Lambda.Types.FunctionCodeLocation
import Network.AWS.Lambda.Types.FunctionConfiguration
import Network.AWS.Lambda.Types.FunctionEventInvokeConfig
import Network.AWS.Lambda.Types.GetLayerVersionResponse
import Network.AWS.Lambda.Types.ImageConfig
import Network.AWS.Lambda.Types.ImageConfigError
import Network.AWS.Lambda.Types.ImageConfigResponse
import Network.AWS.Lambda.Types.Layer
import Network.AWS.Lambda.Types.LayerVersionContentInput
import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.LayerVersionsListItem
import Network.AWS.Lambda.Types.LayersListItem
import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Network.AWS.Lambda.Types.SelfManagedEventSource
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import Network.AWS.Lambda.Types.TracingConfig
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.VpcConfig
import Network.AWS.Lambda.Types.VpcConfigResponse
import Network.AWS.Lambda.UntagResource
import Network.AWS.Lambda.UpdateAlias
import Network.AWS.Lambda.UpdateCodeSigningConfig
import Network.AWS.Lambda.UpdateEventSourceMapping
import Network.AWS.Lambda.UpdateFunctionCode
import Network.AWS.Lambda.UpdateFunctionConfiguration
import Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
