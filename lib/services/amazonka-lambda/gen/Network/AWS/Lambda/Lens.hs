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

    -- ** GetFunctionConfiguration
    getFunctionConfiguration_qualifier,
    getFunctionConfiguration_functionName,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** DeleteEventSourceMapping
    deleteEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- ** UpdateEventSourceMapping
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- ** GetLayerVersion
    getLayerVersion_layerName,
    getLayerVersion_versionNumber,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_version,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,

    -- ** DeleteFunctionCodeSigningConfig
    deleteFunctionCodeSigningConfig_functionName,

    -- ** PutFunctionCodeSigningConfig
    putFunctionCodeSigningConfig_codeSigningConfigArn,
    putFunctionCodeSigningConfig_functionName,
    putFunctionCodeSigningConfigResponse_httpStatus,
    putFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    putFunctionCodeSigningConfigResponse_functionName,

    -- ** CreateAlias
    createAlias_routingConfig,
    createAlias_description,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,
    aliasConfiguration_routingConfig,
    aliasConfiguration_name,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** ListVersionsByFunction
    listVersionsByFunction_marker,
    listVersionsByFunction_maxItems,
    listVersionsByFunction_functionName,
    listVersionsByFunctionResponse_versions,
    listVersionsByFunctionResponse_nextMarker,
    listVersionsByFunctionResponse_httpStatus,

    -- ** ListAliases
    listAliases_marker,
    listAliases_maxItems,
    listAliases_functionVersion,
    listAliases_functionName,
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_httpStatus,

    -- ** DeleteCodeSigningConfig
    deleteCodeSigningConfig_codeSigningConfigArn,
    deleteCodeSigningConfigResponse_httpStatus,

    -- ** UpdateCodeSigningConfig
    updateCodeSigningConfig_allowedPublishers,
    updateCodeSigningConfig_codeSigningPolicies,
    updateCodeSigningConfig_description,
    updateCodeSigningConfig_codeSigningConfigArn,
    updateCodeSigningConfigResponse_httpStatus,
    updateCodeSigningConfigResponse_codeSigningConfig,

    -- ** RemovePermission
    removePermission_qualifier,
    removePermission_revisionId,
    removePermission_functionName,
    removePermission_statementId,

    -- ** DeleteFunctionEventInvokeConfig
    deleteFunctionEventInvokeConfig_qualifier,
    deleteFunctionEventInvokeConfig_functionName,

    -- ** UpdateFunctionEventInvokeConfig
    updateFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    updateFunctionEventInvokeConfig_maximumRetryAttempts,
    updateFunctionEventInvokeConfig_qualifier,
    updateFunctionEventInvokeConfig_destinationConfig,
    updateFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_destinationConfig,

    -- ** PutFunctionEventInvokeConfig
    putFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    putFunctionEventInvokeConfig_maximumRetryAttempts,
    putFunctionEventInvokeConfig_qualifier,
    putFunctionEventInvokeConfig_destinationConfig,
    putFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_destinationConfig,

    -- ** Invoke
    invoke_invocationType,
    invoke_logType,
    invoke_qualifier,
    invoke_clientContext,
    invoke_functionName,
    invoke_payload,
    invokeResponse_functionError,
    invokeResponse_logResult,
    invokeResponse_payload,
    invokeResponse_executedVersion,
    invokeResponse_statusCode,

    -- ** DeleteLayerVersion
    deleteLayerVersion_layerName,
    deleteLayerVersion_versionNumber,

    -- ** GetAlias
    getAlias_functionName,
    getAlias_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_name,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** PublishLayerVersion
    publishLayerVersion_licenseInfo,
    publishLayerVersion_compatibleArchitectures,
    publishLayerVersion_description,
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_layerName,
    publishLayerVersion_content,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_compatibleArchitectures,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_httpStatus,

    -- ** GetEventSourceMapping
    getEventSourceMapping_uuid,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- ** AddLayerVersionPermission
    addLayerVersionPermission_revisionId,
    addLayerVersionPermission_organizationId,
    addLayerVersionPermission_layerName,
    addLayerVersionPermission_versionNumber,
    addLayerVersionPermission_statementId,
    addLayerVersionPermission_action,
    addLayerVersionPermission_principal,
    addLayerVersionPermissionResponse_statement,
    addLayerVersionPermissionResponse_revisionId,
    addLayerVersionPermissionResponse_httpStatus,

    -- ** ListProvisionedConcurrencyConfigs
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_functionName,
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_httpStatus,

    -- ** PutFunctionConcurrency
    putFunctionConcurrency_functionName,
    putFunctionConcurrency_reservedConcurrentExecutions,
    concurrency_reservedConcurrentExecutions,

    -- ** CreateFunction
    createFunction_memorySize,
    createFunction_runtime,
    createFunction_kmsKeyArn,
    createFunction_packageType,
    createFunction_fileSystemConfigs,
    createFunction_environment,
    createFunction_imageConfig,
    createFunction_deadLetterConfig,
    createFunction_architectures,
    createFunction_codeSigningConfigArn,
    createFunction_vpcConfig,
    createFunction_layers,
    createFunction_handler,
    createFunction_timeout,
    createFunction_tracingConfig,
    createFunction_description,
    createFunction_tags,
    createFunction_publish,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** DeleteFunctionConcurrency
    deleteFunctionConcurrency_functionName,

    -- ** GetLayerVersionByArn
    getLayerVersionByArn_arn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_version,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,

    -- ** GetFunctionConcurrency
    getFunctionConcurrency_functionName,
    getFunctionConcurrencyResponse_reservedConcurrentExecutions,
    getFunctionConcurrencyResponse_httpStatus,

    -- ** CreateEventSourceMapping
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_topics,
    createEventSourceMapping_queues,
    createEventSourceMapping_enabled,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_functionName,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- ** GetProvisionedConcurrencyConfig
    getProvisionedConcurrencyConfig_functionName,
    getProvisionedConcurrencyConfig_qualifier,
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** RemoveLayerVersionPermission
    removeLayerVersionPermission_revisionId,
    removeLayerVersionPermission_layerName,
    removeLayerVersionPermission_versionNumber,
    removeLayerVersionPermission_statementId,

    -- ** ListFunctionsByCodeSigningConfig
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_maxItems,
    listFunctionsByCodeSigningConfig_codeSigningConfigArn,
    listFunctionsByCodeSigningConfigResponse_functionArns,
    listFunctionsByCodeSigningConfigResponse_nextMarker,
    listFunctionsByCodeSigningConfigResponse_httpStatus,

    -- ** GetFunction
    getFunction_qualifier,
    getFunction_functionName,
    getFunctionResponse_concurrency,
    getFunctionResponse_code,
    getFunctionResponse_configuration,
    getFunctionResponse_tags,
    getFunctionResponse_httpStatus,

    -- ** ListEventSourceMappings
    listEventSourceMappings_eventSourceArn,
    listEventSourceMappings_marker,
    listEventSourceMappings_maxItems,
    listEventSourceMappings_functionName,
    listEventSourceMappingsResponse_eventSourceMappings,
    listEventSourceMappingsResponse_nextMarker,
    listEventSourceMappingsResponse_httpStatus,

    -- ** GetLayerVersionPolicy
    getLayerVersionPolicy_layerName,
    getLayerVersionPolicy_versionNumber,
    getLayerVersionPolicyResponse_policy,
    getLayerVersionPolicyResponse_revisionId,
    getLayerVersionPolicyResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_functionName,
    deleteAlias_name,

    -- ** UpdateAlias
    updateAlias_routingConfig,
    updateAlias_functionVersion,
    updateAlias_description,
    updateAlias_revisionId,
    updateAlias_functionName,
    updateAlias_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_name,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountLimit,
    getAccountSettingsResponse_accountUsage,
    getAccountSettingsResponse_httpStatus,

    -- ** GetFunctionEventInvokeConfig
    getFunctionEventInvokeConfig_qualifier,
    getFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_destinationConfig,

    -- ** GetCodeSigningConfig
    getCodeSigningConfig_codeSigningConfigArn,
    getCodeSigningConfigResponse_httpStatus,
    getCodeSigningConfigResponse_codeSigningConfig,

    -- ** AddPermission
    addPermission_sourceAccount,
    addPermission_eventSourceToken,
    addPermission_sourceArn,
    addPermission_qualifier,
    addPermission_revisionId,
    addPermission_functionName,
    addPermission_statementId,
    addPermission_action,
    addPermission_principal,
    addPermissionResponse_statement,
    addPermissionResponse_httpStatus,

    -- ** ListLayers
    listLayers_compatibleRuntime,
    listLayers_marker,
    listLayers_maxItems,
    listLayers_compatibleArchitecture,
    listLayersResponse_nextMarker,
    listLayersResponse_layers,
    listLayersResponse_httpStatus,

    -- ** ListFunctionEventInvokeConfigs
    listFunctionEventInvokeConfigs_marker,
    listFunctionEventInvokeConfigs_maxItems,
    listFunctionEventInvokeConfigs_functionName,
    listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs,
    listFunctionEventInvokeConfigsResponse_nextMarker,
    listFunctionEventInvokeConfigsResponse_httpStatus,

    -- ** ListCodeSigningConfigs
    listCodeSigningConfigs_marker,
    listCodeSigningConfigs_maxItems,
    listCodeSigningConfigsResponse_codeSigningConfigs,
    listCodeSigningConfigsResponse_nextMarker,
    listCodeSigningConfigsResponse_httpStatus,

    -- ** GetFunctionCodeSigningConfig
    getFunctionCodeSigningConfig_functionName,
    getFunctionCodeSigningConfigResponse_httpStatus,
    getFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    getFunctionCodeSigningConfigResponse_functionName,

    -- ** CreateCodeSigningConfig
    createCodeSigningConfig_codeSigningPolicies,
    createCodeSigningConfig_description,
    createCodeSigningConfig_allowedPublishers,
    createCodeSigningConfigResponse_httpStatus,
    createCodeSigningConfigResponse_codeSigningConfig,

    -- ** ListLayerVersions
    listLayerVersions_compatibleRuntime,
    listLayerVersions_marker,
    listLayerVersions_maxItems,
    listLayerVersions_compatibleArchitecture,
    listLayerVersions_layerName,
    listLayerVersionsResponse_layerVersions,
    listLayerVersionsResponse_nextMarker,
    listLayerVersionsResponse_httpStatus,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,

    -- ** PublishVersion
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_revisionId,
    publishVersion_functionName,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** ListTags
    listTags_resource,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeleteFunction
    deleteFunction_qualifier,
    deleteFunction_functionName,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,

    -- ** UpdateFunctionConfiguration
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_functionName,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** ListFunctions
    listFunctions_masterRegion,
    listFunctions_marker,
    listFunctions_maxItems,
    listFunctions_functionVersion,
    listFunctionsResponse_nextMarker,
    listFunctionsResponse_functions,
    listFunctionsResponse_httpStatus,

    -- ** UpdateFunctionCode
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_s3Key,
    updateFunctionCode_zipFile,
    updateFunctionCode_architectures,
    updateFunctionCode_imageUri,
    updateFunctionCode_s3Bucket,
    updateFunctionCode_dryRun,
    updateFunctionCode_revisionId,
    updateFunctionCode_publish,
    updateFunctionCode_functionName,
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** DeleteProvisionedConcurrencyConfig
    deleteProvisionedConcurrencyConfig_functionName,
    deleteProvisionedConcurrencyConfig_qualifier,

    -- ** GetPolicy
    getPolicy_qualifier,
    getPolicy_functionName,
    getPolicyResponse_policy,
    getPolicyResponse_revisionId,
    getPolicyResponse_httpStatus,

    -- ** PutProvisionedConcurrencyConfig
    putProvisionedConcurrencyConfig_functionName,
    putProvisionedConcurrencyConfig_qualifier,
    putProvisionedConcurrencyConfig_provisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_status,
    putProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_statusReason,
    putProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_lastModified,
    putProvisionedConcurrencyConfigResponse_httpStatus,

    -- * Types

    -- ** AccountLimit
    accountLimit_concurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_codeSizeUnzipped,
    accountLimit_codeSizeZipped,

    -- ** AccountUsage
    accountUsage_totalCodeSize,
    accountUsage_functionCount,

    -- ** AliasConfiguration
    aliasConfiguration_routingConfig,
    aliasConfiguration_name,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

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
    destinationConfig_onSuccess,
    destinationConfig_onFailure,

    -- ** Environment
    environment_variables,

    -- ** EnvironmentError
    environmentError_errorCode,
    environmentError_message,

    -- ** EnvironmentResponse
    environmentResponse_variables,
    environmentResponse_error,

    -- ** EventSourceMappingConfiguration
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,

    -- ** FileSystemConfig
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- ** FunctionCode
    functionCode_s3ObjectVersion,
    functionCode_s3Key,
    functionCode_zipFile,
    functionCode_imageUri,
    functionCode_s3Bucket,

    -- ** FunctionCodeLocation
    functionCodeLocation_location,
    functionCodeLocation_resolvedImageUri,
    functionCodeLocation_imageUri,
    functionCodeLocation_repositoryType,

    -- ** FunctionConfiguration
    functionConfiguration_memorySize,
    functionConfiguration_runtime,
    functionConfiguration_state,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_functionArn,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_packageType,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_environment,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_architectures,
    functionConfiguration_signingJobArn,
    functionConfiguration_role,
    functionConfiguration_vpcConfig,
    functionConfiguration_version,
    functionConfiguration_functionName,
    functionConfiguration_layers,
    functionConfiguration_codeSize,
    functionConfiguration_handler,
    functionConfiguration_timeout,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_stateReason,
    functionConfiguration_lastModified,
    functionConfiguration_codeSha256,
    functionConfiguration_tracingConfig,
    functionConfiguration_stateReasonCode,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_description,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_masterArn,

    -- ** FunctionEventInvokeConfig
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_destinationConfig,

    -- ** GetLayerVersionResponse
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_version,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,

    -- ** ImageConfig
    imageConfig_command,
    imageConfig_entryPoint,
    imageConfig_workingDirectory,

    -- ** ImageConfigError
    imageConfigError_errorCode,
    imageConfigError_message,

    -- ** ImageConfigResponse
    imageConfigResponse_imageConfig,
    imageConfigResponse_error,

    -- ** Layer
    layer_signingProfileVersionArn,
    layer_arn,
    layer_signingJobArn,
    layer_codeSize,

    -- ** LayerVersionContentInput
    layerVersionContentInput_s3ObjectVersion,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3Bucket,

    -- ** LayerVersionContentOutput
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_codeSha256,

    -- ** LayerVersionsListItem
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_description,
    layerVersionsListItem_compatibleRuntimes,

    -- ** LayersListItem
    layersListItem_layerName,
    layersListItem_latestMatchingVersion,
    layersListItem_layerArn,

    -- ** OnFailure
    onFailure_destination,

    -- ** OnSuccess
    onSuccess_destination,

    -- ** ProvisionedConcurrencyConfigListItem
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_statusReason,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,

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
