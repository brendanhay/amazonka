{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Lens
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

import Amazonka.Lambda.AddLayerVersionPermission
import Amazonka.Lambda.AddPermission
import Amazonka.Lambda.CreateAlias
import Amazonka.Lambda.CreateCodeSigningConfig
import Amazonka.Lambda.CreateEventSourceMapping
import Amazonka.Lambda.CreateFunction
import Amazonka.Lambda.DeleteAlias
import Amazonka.Lambda.DeleteCodeSigningConfig
import Amazonka.Lambda.DeleteEventSourceMapping
import Amazonka.Lambda.DeleteFunction
import Amazonka.Lambda.DeleteFunctionCodeSigningConfig
import Amazonka.Lambda.DeleteFunctionConcurrency
import Amazonka.Lambda.DeleteFunctionEventInvokeConfig
import Amazonka.Lambda.DeleteLayerVersion
import Amazonka.Lambda.DeleteProvisionedConcurrencyConfig
import Amazonka.Lambda.GetAccountSettings
import Amazonka.Lambda.GetAlias
import Amazonka.Lambda.GetCodeSigningConfig
import Amazonka.Lambda.GetEventSourceMapping
import Amazonka.Lambda.GetFunction
import Amazonka.Lambda.GetFunctionCodeSigningConfig
import Amazonka.Lambda.GetFunctionConcurrency
import Amazonka.Lambda.GetFunctionConfiguration
import Amazonka.Lambda.GetFunctionEventInvokeConfig
import Amazonka.Lambda.GetLayerVersion
import Amazonka.Lambda.GetLayerVersionByArn
import Amazonka.Lambda.GetLayerVersionPolicy
import Amazonka.Lambda.GetPolicy
import Amazonka.Lambda.GetProvisionedConcurrencyConfig
import Amazonka.Lambda.Invoke
import Amazonka.Lambda.ListAliases
import Amazonka.Lambda.ListCodeSigningConfigs
import Amazonka.Lambda.ListEventSourceMappings
import Amazonka.Lambda.ListFunctionEventInvokeConfigs
import Amazonka.Lambda.ListFunctions
import Amazonka.Lambda.ListFunctionsByCodeSigningConfig
import Amazonka.Lambda.ListLayerVersions
import Amazonka.Lambda.ListLayers
import Amazonka.Lambda.ListProvisionedConcurrencyConfigs
import Amazonka.Lambda.ListTags
import Amazonka.Lambda.ListVersionsByFunction
import Amazonka.Lambda.PublishLayerVersion
import Amazonka.Lambda.PublishVersion
import Amazonka.Lambda.PutFunctionCodeSigningConfig
import Amazonka.Lambda.PutFunctionConcurrency
import Amazonka.Lambda.PutFunctionEventInvokeConfig
import Amazonka.Lambda.PutProvisionedConcurrencyConfig
import Amazonka.Lambda.RemoveLayerVersionPermission
import Amazonka.Lambda.RemovePermission
import Amazonka.Lambda.TagResource
import Amazonka.Lambda.Types.AccountLimit
import Amazonka.Lambda.Types.AccountUsage
import Amazonka.Lambda.Types.AliasConfiguration
import Amazonka.Lambda.Types.AliasRoutingConfiguration
import Amazonka.Lambda.Types.AllowedPublishers
import Amazonka.Lambda.Types.CodeSigningConfig
import Amazonka.Lambda.Types.CodeSigningPolicies
import Amazonka.Lambda.Types.Concurrency
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.DestinationConfig
import Amazonka.Lambda.Types.Environment
import Amazonka.Lambda.Types.EnvironmentError
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.EventSourceMappingConfiguration
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.FunctionCode
import Amazonka.Lambda.Types.FunctionCodeLocation
import Amazonka.Lambda.Types.FunctionConfiguration
import Amazonka.Lambda.Types.FunctionEventInvokeConfig
import Amazonka.Lambda.Types.GetLayerVersionResponse
import Amazonka.Lambda.Types.ImageConfig
import Amazonka.Lambda.Types.ImageConfigError
import Amazonka.Lambda.Types.ImageConfigResponse
import Amazonka.Lambda.Types.Layer
import Amazonka.Lambda.Types.LayerVersionContentInput
import Amazonka.Lambda.Types.LayerVersionContentOutput
import Amazonka.Lambda.Types.LayerVersionsListItem
import Amazonka.Lambda.Types.LayersListItem
import Amazonka.Lambda.Types.OnFailure
import Amazonka.Lambda.Types.OnSuccess
import Amazonka.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Amazonka.Lambda.Types.SelfManagedEventSource
import Amazonka.Lambda.Types.SourceAccessConfiguration
import Amazonka.Lambda.Types.TracingConfig
import Amazonka.Lambda.Types.TracingConfigResponse
import Amazonka.Lambda.Types.VpcConfig
import Amazonka.Lambda.Types.VpcConfigResponse
import Amazonka.Lambda.UntagResource
import Amazonka.Lambda.UpdateAlias
import Amazonka.Lambda.UpdateCodeSigningConfig
import Amazonka.Lambda.UpdateEventSourceMapping
import Amazonka.Lambda.UpdateFunctionCode
import Amazonka.Lambda.UpdateFunctionConfiguration
import Amazonka.Lambda.UpdateFunctionEventInvokeConfig
