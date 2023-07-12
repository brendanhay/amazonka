{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Lens
  ( -- * Operations

    -- ** AddLayerVersionPermission
    addLayerVersionPermission_organizationId,
    addLayerVersionPermission_revisionId,
    addLayerVersionPermission_layerName,
    addLayerVersionPermission_versionNumber,
    addLayerVersionPermission_statementId,
    addLayerVersionPermission_action,
    addLayerVersionPermission_principal,
    addLayerVersionPermissionResponse_revisionId,
    addLayerVersionPermissionResponse_statement,
    addLayerVersionPermissionResponse_httpStatus,

    -- ** AddPermission
    addPermission_eventSourceToken,
    addPermission_functionUrlAuthType,
    addPermission_principalOrgID,
    addPermission_qualifier,
    addPermission_revisionId,
    addPermission_sourceAccount,
    addPermission_sourceArn,
    addPermission_functionName,
    addPermission_statementId,
    addPermission_action,
    addPermission_principal,
    addPermissionResponse_statement,
    addPermissionResponse_httpStatus,

    -- ** CreateAlias
    createAlias_description,
    createAlias_routingConfig,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,

    -- ** CreateCodeSigningConfig
    createCodeSigningConfig_codeSigningPolicies,
    createCodeSigningConfig_description,
    createCodeSigningConfig_allowedPublishers,
    createCodeSigningConfigResponse_httpStatus,
    createCodeSigningConfigResponse_codeSigningConfig,

    -- ** CreateEventSourceMapping
    createEventSourceMapping_amazonManagedKafkaEventSourceConfig,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_enabled,
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_filterCriteria,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_queues,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_selfManagedKafkaEventSourceConfig,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_topics,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_functionName,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

    -- ** CreateFunction
    createFunction_architectures,
    createFunction_codeSigningConfigArn,
    createFunction_deadLetterConfig,
    createFunction_description,
    createFunction_environment,
    createFunction_ephemeralStorage,
    createFunction_fileSystemConfigs,
    createFunction_handler,
    createFunction_imageConfig,
    createFunction_kmsKeyArn,
    createFunction_layers,
    createFunction_memorySize,
    createFunction_packageType,
    createFunction_publish,
    createFunction_runtime,
    createFunction_snapStart,
    createFunction_tags,
    createFunction_timeout,
    createFunction_tracingConfig,
    createFunction_vpcConfig,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** CreateFunctionUrlConfig
    createFunctionUrlConfig_cors,
    createFunctionUrlConfig_qualifier,
    createFunctionUrlConfig_functionName,
    createFunctionUrlConfig_authType,
    createFunctionUrlConfigResponse_cors,
    createFunctionUrlConfigResponse_httpStatus,
    createFunctionUrlConfigResponse_functionUrl,
    createFunctionUrlConfigResponse_functionArn,
    createFunctionUrlConfigResponse_authType,
    createFunctionUrlConfigResponse_creationTime,

    -- ** DeleteAlias
    deleteAlias_functionName,
    deleteAlias_name,

    -- ** DeleteCodeSigningConfig
    deleteCodeSigningConfig_codeSigningConfigArn,
    deleteCodeSigningConfigResponse_httpStatus,

    -- ** DeleteEventSourceMapping
    deleteEventSourceMapping_uuid,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

    -- ** DeleteFunction
    deleteFunction_qualifier,
    deleteFunction_functionName,

    -- ** DeleteFunctionCodeSigningConfig
    deleteFunctionCodeSigningConfig_functionName,

    -- ** DeleteFunctionConcurrency
    deleteFunctionConcurrency_functionName,

    -- ** DeleteFunctionEventInvokeConfig
    deleteFunctionEventInvokeConfig_qualifier,
    deleteFunctionEventInvokeConfig_functionName,

    -- ** DeleteFunctionUrlConfig
    deleteFunctionUrlConfig_qualifier,
    deleteFunctionUrlConfig_functionName,

    -- ** DeleteLayerVersion
    deleteLayerVersion_layerName,
    deleteLayerVersion_versionNumber,

    -- ** DeleteProvisionedConcurrencyConfig
    deleteProvisionedConcurrencyConfig_functionName,
    deleteProvisionedConcurrencyConfig_qualifier,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountLimit,
    getAccountSettingsResponse_accountUsage,
    getAccountSettingsResponse_httpStatus,

    -- ** GetAlias
    getAlias_functionName,
    getAlias_name,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,

    -- ** GetCodeSigningConfig
    getCodeSigningConfig_codeSigningConfigArn,
    getCodeSigningConfigResponse_httpStatus,
    getCodeSigningConfigResponse_codeSigningConfig,

    -- ** GetEventSourceMapping
    getEventSourceMapping_uuid,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

    -- ** GetFunction
    getFunction_qualifier,
    getFunction_functionName,
    getFunctionResponse_code,
    getFunctionResponse_concurrency,
    getFunctionResponse_configuration,
    getFunctionResponse_tags,
    getFunctionResponse_httpStatus,

    -- ** GetFunctionCodeSigningConfig
    getFunctionCodeSigningConfig_functionName,
    getFunctionCodeSigningConfigResponse_httpStatus,
    getFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    getFunctionCodeSigningConfigResponse_functionName,

    -- ** GetFunctionConcurrency
    getFunctionConcurrency_functionName,
    getFunctionConcurrencyResponse_reservedConcurrentExecutions,
    getFunctionConcurrencyResponse_httpStatus,

    -- ** GetFunctionConfiguration
    getFunctionConfiguration_qualifier,
    getFunctionConfiguration_functionName,
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** GetFunctionEventInvokeConfig
    getFunctionEventInvokeConfig_qualifier,
    getFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,

    -- ** GetFunctionUrlConfig
    getFunctionUrlConfig_qualifier,
    getFunctionUrlConfig_functionName,
    getFunctionUrlConfigResponse_cors,
    getFunctionUrlConfigResponse_httpStatus,
    getFunctionUrlConfigResponse_functionUrl,
    getFunctionUrlConfigResponse_functionArn,
    getFunctionUrlConfigResponse_authType,
    getFunctionUrlConfigResponse_creationTime,
    getFunctionUrlConfigResponse_lastModifiedTime,

    -- ** GetLayerVersion
    getLayerVersion_layerName,
    getLayerVersion_versionNumber,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_description,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_version,

    -- ** GetLayerVersionByArn
    getLayerVersionByArn_arn,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_description,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_version,

    -- ** GetLayerVersionPolicy
    getLayerVersionPolicy_layerName,
    getLayerVersionPolicy_versionNumber,
    getLayerVersionPolicyResponse_policy,
    getLayerVersionPolicyResponse_revisionId,
    getLayerVersionPolicyResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_qualifier,
    getPolicy_functionName,
    getPolicyResponse_policy,
    getPolicyResponse_revisionId,
    getPolicyResponse_httpStatus,

    -- ** GetProvisionedConcurrencyConfig
    getProvisionedConcurrencyConfig_functionName,
    getProvisionedConcurrencyConfig_qualifier,
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** Invoke
    invoke_clientContext,
    invoke_invocationType,
    invoke_logType,
    invoke_qualifier,
    invoke_functionName,
    invoke_payload,
    invokeResponse_executedVersion,
    invokeResponse_functionError,
    invokeResponse_logResult,
    invokeResponse_payload,
    invokeResponse_statusCode,

    -- ** ListAliases
    listAliases_functionVersion,
    listAliases_marker,
    listAliases_maxItems,
    listAliases_functionName,
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_httpStatus,

    -- ** ListCodeSigningConfigs
    listCodeSigningConfigs_marker,
    listCodeSigningConfigs_maxItems,
    listCodeSigningConfigsResponse_codeSigningConfigs,
    listCodeSigningConfigsResponse_nextMarker,
    listCodeSigningConfigsResponse_httpStatus,

    -- ** ListEventSourceMappings
    listEventSourceMappings_eventSourceArn,
    listEventSourceMappings_functionName,
    listEventSourceMappings_marker,
    listEventSourceMappings_maxItems,
    listEventSourceMappingsResponse_eventSourceMappings,
    listEventSourceMappingsResponse_nextMarker,
    listEventSourceMappingsResponse_httpStatus,

    -- ** ListFunctionEventInvokeConfigs
    listFunctionEventInvokeConfigs_marker,
    listFunctionEventInvokeConfigs_maxItems,
    listFunctionEventInvokeConfigs_functionName,
    listFunctionEventInvokeConfigsResponse_functionEventInvokeConfigs,
    listFunctionEventInvokeConfigsResponse_nextMarker,
    listFunctionEventInvokeConfigsResponse_httpStatus,

    -- ** ListFunctionUrlConfigs
    listFunctionUrlConfigs_marker,
    listFunctionUrlConfigs_maxItems,
    listFunctionUrlConfigs_functionName,
    listFunctionUrlConfigsResponse_nextMarker,
    listFunctionUrlConfigsResponse_httpStatus,
    listFunctionUrlConfigsResponse_functionUrlConfigs,

    -- ** ListFunctions
    listFunctions_functionVersion,
    listFunctions_marker,
    listFunctions_masterRegion,
    listFunctions_maxItems,
    listFunctionsResponse_functions,
    listFunctionsResponse_nextMarker,
    listFunctionsResponse_httpStatus,

    -- ** ListFunctionsByCodeSigningConfig
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_maxItems,
    listFunctionsByCodeSigningConfig_codeSigningConfigArn,
    listFunctionsByCodeSigningConfigResponse_functionArns,
    listFunctionsByCodeSigningConfigResponse_nextMarker,
    listFunctionsByCodeSigningConfigResponse_httpStatus,

    -- ** ListLayerVersions
    listLayerVersions_compatibleArchitecture,
    listLayerVersions_compatibleRuntime,
    listLayerVersions_marker,
    listLayerVersions_maxItems,
    listLayerVersions_layerName,
    listLayerVersionsResponse_layerVersions,
    listLayerVersionsResponse_nextMarker,
    listLayerVersionsResponse_httpStatus,

    -- ** ListLayers
    listLayers_compatibleArchitecture,
    listLayers_compatibleRuntime,
    listLayers_marker,
    listLayers_maxItems,
    listLayersResponse_layers,
    listLayersResponse_nextMarker,
    listLayersResponse_httpStatus,

    -- ** ListProvisionedConcurrencyConfigs
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_functionName,
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_httpStatus,

    -- ** ListTags
    listTags_resource,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** ListVersionsByFunction
    listVersionsByFunction_marker,
    listVersionsByFunction_maxItems,
    listVersionsByFunction_functionName,
    listVersionsByFunctionResponse_nextMarker,
    listVersionsByFunctionResponse_versions,
    listVersionsByFunctionResponse_httpStatus,

    -- ** PublishLayerVersion
    publishLayerVersion_compatibleArchitectures,
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_description,
    publishLayerVersion_licenseInfo,
    publishLayerVersion_layerName,
    publishLayerVersion_content,
    publishLayerVersionResponse_compatibleArchitectures,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_httpStatus,

    -- ** PublishVersion
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_revisionId,
    publishVersion_functionName,
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** PutFunctionCodeSigningConfig
    putFunctionCodeSigningConfig_codeSigningConfigArn,
    putFunctionCodeSigningConfig_functionName,
    putFunctionCodeSigningConfigResponse_httpStatus,
    putFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    putFunctionCodeSigningConfigResponse_functionName,

    -- ** PutFunctionConcurrency
    putFunctionConcurrency_functionName,
    putFunctionConcurrency_reservedConcurrentExecutions,
    concurrency_reservedConcurrentExecutions,

    -- ** PutFunctionEventInvokeConfig
    putFunctionEventInvokeConfig_destinationConfig,
    putFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    putFunctionEventInvokeConfig_maximumRetryAttempts,
    putFunctionEventInvokeConfig_qualifier,
    putFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,

    -- ** PutProvisionedConcurrencyConfig
    putProvisionedConcurrencyConfig_functionName,
    putProvisionedConcurrencyConfig_qualifier,
    putProvisionedConcurrencyConfig_provisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_lastModified,
    putProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_status,
    putProvisionedConcurrencyConfigResponse_statusReason,
    putProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** RemoveLayerVersionPermission
    removeLayerVersionPermission_revisionId,
    removeLayerVersionPermission_layerName,
    removeLayerVersionPermission_versionNumber,
    removeLayerVersionPermission_statementId,

    -- ** RemovePermission
    removePermission_qualifier,
    removePermission_revisionId,
    removePermission_functionName,
    removePermission_statementId,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,

    -- ** UpdateAlias
    updateAlias_description,
    updateAlias_functionVersion,
    updateAlias_revisionId,
    updateAlias_routingConfig,
    updateAlias_functionName,
    updateAlias_name,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,

    -- ** UpdateCodeSigningConfig
    updateCodeSigningConfig_allowedPublishers,
    updateCodeSigningConfig_codeSigningPolicies,
    updateCodeSigningConfig_description,
    updateCodeSigningConfig_codeSigningConfigArn,
    updateCodeSigningConfigResponse_httpStatus,
    updateCodeSigningConfigResponse_codeSigningConfig,

    -- ** UpdateEventSourceMapping
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_filterCriteria,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_uuid,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

    -- ** UpdateFunctionCode
    updateFunctionCode_architectures,
    updateFunctionCode_dryRun,
    updateFunctionCode_imageUri,
    updateFunctionCode_publish,
    updateFunctionCode_revisionId,
    updateFunctionCode_s3Bucket,
    updateFunctionCode_s3Key,
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_zipFile,
    updateFunctionCode_functionName,
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** UpdateFunctionConfiguration
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_ephemeralStorage,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_snapStart,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_functionName,
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** UpdateFunctionEventInvokeConfig
    updateFunctionEventInvokeConfig_destinationConfig,
    updateFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    updateFunctionEventInvokeConfig_maximumRetryAttempts,
    updateFunctionEventInvokeConfig_qualifier,
    updateFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,

    -- ** UpdateFunctionUrlConfig
    updateFunctionUrlConfig_authType,
    updateFunctionUrlConfig_cors,
    updateFunctionUrlConfig_qualifier,
    updateFunctionUrlConfig_functionName,
    updateFunctionUrlConfigResponse_cors,
    updateFunctionUrlConfigResponse_httpStatus,
    updateFunctionUrlConfigResponse_functionUrl,
    updateFunctionUrlConfigResponse_functionArn,
    updateFunctionUrlConfigResponse_authType,
    updateFunctionUrlConfigResponse_creationTime,
    updateFunctionUrlConfigResponse_lastModifiedTime,

    -- * Types

    -- ** AccountLimit
    accountLimit_codeSizeUnzipped,
    accountLimit_codeSizeZipped,
    accountLimit_concurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_unreservedConcurrentExecutions,

    -- ** AccountUsage
    accountUsage_functionCount,
    accountUsage_totalCodeSize,

    -- ** AliasConfiguration
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_revisionId,
    aliasConfiguration_routingConfig,

    -- ** AliasRoutingConfiguration
    aliasRoutingConfiguration_additionalVersionWeights,

    -- ** AllowedPublishers
    allowedPublishers_signingProfileVersionArns,

    -- ** AmazonManagedKafkaEventSourceConfig
    amazonManagedKafkaEventSourceConfig_consumerGroupId,

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

    -- ** Cors
    cors_allowCredentials,
    cors_allowHeaders,
    cors_allowMethods,
    cors_allowOrigins,
    cors_exposeHeaders,
    cors_maxAge,

    -- ** DeadLetterConfig
    deadLetterConfig_targetArn,

    -- ** DestinationConfig
    destinationConfig_onFailure,
    destinationConfig_onSuccess,

    -- ** Environment
    environment_variables,

    -- ** EnvironmentError
    environmentError_errorCode,
    environmentError_message,

    -- ** EnvironmentResponse
    environmentResponse_error,
    environmentResponse_variables,

    -- ** EphemeralStorage
    ephemeralStorage_size,

    -- ** EventSourceMappingConfiguration
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,

    -- ** FileSystemConfig
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- ** Filter
    filter_pattern,

    -- ** FilterCriteria
    filterCriteria_filters,

    -- ** FunctionCode
    functionCode_imageUri,
    functionCode_s3Bucket,
    functionCode_s3Key,
    functionCode_s3ObjectVersion,
    functionCode_zipFile,

    -- ** FunctionCodeLocation
    functionCodeLocation_imageUri,
    functionCodeLocation_location,
    functionCodeLocation_repositoryType,
    functionCodeLocation_resolvedImageUri,

    -- ** FunctionConfiguration
    functionConfiguration_architectures,
    functionConfiguration_codeSha256,
    functionConfiguration_codeSize,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_description,
    functionConfiguration_environment,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_functionArn,
    functionConfiguration_functionName,
    functionConfiguration_handler,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_lastModified,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_layers,
    functionConfiguration_masterArn,
    functionConfiguration_memorySize,
    functionConfiguration_packageType,
    functionConfiguration_revisionId,
    functionConfiguration_role,
    functionConfiguration_runtime,
    functionConfiguration_signingJobArn,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_snapStart,
    functionConfiguration_state,
    functionConfiguration_stateReason,
    functionConfiguration_stateReasonCode,
    functionConfiguration_timeout,
    functionConfiguration_tracingConfig,
    functionConfiguration_version,
    functionConfiguration_vpcConfig,

    -- ** FunctionEventInvokeConfig
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,

    -- ** FunctionUrlConfig
    functionUrlConfig_cors,
    functionUrlConfig_functionUrl,
    functionUrlConfig_functionArn,
    functionUrlConfig_creationTime,
    functionUrlConfig_lastModifiedTime,
    functionUrlConfig_authType,

    -- ** GetLayerVersionResponse
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_description,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_version,

    -- ** ImageConfig
    imageConfig_command,
    imageConfig_entryPoint,
    imageConfig_workingDirectory,

    -- ** ImageConfigError
    imageConfigError_errorCode,
    imageConfigError_message,

    -- ** ImageConfigResponse
    imageConfigResponse_error,
    imageConfigResponse_imageConfig,

    -- ** Layer
    layer_arn,
    layer_codeSize,
    layer_signingJobArn,
    layer_signingProfileVersionArn,

    -- ** LayerVersionContentInput
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_s3ObjectVersion,
    layerVersionContentInput_zipFile,

    -- ** LayerVersionContentOutput
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_codeSize,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_signingProfileVersionArn,

    -- ** LayerVersionsListItem
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_description,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_version,

    -- ** LayersListItem
    layersListItem_latestMatchingVersion,
    layersListItem_layerArn,
    layersListItem_layerName,

    -- ** OnFailure
    onFailure_destination,

    -- ** OnSuccess
    onSuccess_destination,

    -- ** ProvisionedConcurrencyConfigListItem
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_statusReason,

    -- ** SelfManagedEventSource
    selfManagedEventSource_endpoints,

    -- ** SelfManagedKafkaEventSourceConfig
    selfManagedKafkaEventSourceConfig_consumerGroupId,

    -- ** SnapStart
    snapStart_applyOn,

    -- ** SnapStartResponse
    snapStartResponse_applyOn,
    snapStartResponse_optimizationStatus,

    -- ** SourceAccessConfiguration
    sourceAccessConfiguration_type,
    sourceAccessConfiguration_uri,

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
import Amazonka.Lambda.CreateFunctionUrlConfig
import Amazonka.Lambda.DeleteAlias
import Amazonka.Lambda.DeleteCodeSigningConfig
import Amazonka.Lambda.DeleteEventSourceMapping
import Amazonka.Lambda.DeleteFunction
import Amazonka.Lambda.DeleteFunctionCodeSigningConfig
import Amazonka.Lambda.DeleteFunctionConcurrency
import Amazonka.Lambda.DeleteFunctionEventInvokeConfig
import Amazonka.Lambda.DeleteFunctionUrlConfig
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
import Amazonka.Lambda.GetFunctionUrlConfig
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
import Amazonka.Lambda.ListFunctionUrlConfigs
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
import Amazonka.Lambda.Types.AmazonManagedKafkaEventSourceConfig
import Amazonka.Lambda.Types.CodeSigningConfig
import Amazonka.Lambda.Types.CodeSigningPolicies
import Amazonka.Lambda.Types.Concurrency
import Amazonka.Lambda.Types.Cors
import Amazonka.Lambda.Types.DeadLetterConfig
import Amazonka.Lambda.Types.DestinationConfig
import Amazonka.Lambda.Types.Environment
import Amazonka.Lambda.Types.EnvironmentError
import Amazonka.Lambda.Types.EnvironmentResponse
import Amazonka.Lambda.Types.EphemeralStorage
import Amazonka.Lambda.Types.EventSourceMappingConfiguration
import Amazonka.Lambda.Types.FileSystemConfig
import Amazonka.Lambda.Types.Filter
import Amazonka.Lambda.Types.FilterCriteria
import Amazonka.Lambda.Types.FunctionCode
import Amazonka.Lambda.Types.FunctionCodeLocation
import Amazonka.Lambda.Types.FunctionConfiguration
import Amazonka.Lambda.Types.FunctionEventInvokeConfig
import Amazonka.Lambda.Types.FunctionUrlConfig
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
import Amazonka.Lambda.Types.SelfManagedKafkaEventSourceConfig
import Amazonka.Lambda.Types.SnapStart
import Amazonka.Lambda.Types.SnapStartResponse
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
import Amazonka.Lambda.UpdateFunctionUrlConfig
