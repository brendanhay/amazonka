{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Lens
  ( -- * Operations

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

    -- ** AddPermission
    addPermission_sourceArn,
    addPermission_functionUrlAuthType,
    addPermission_eventSourceToken,
    addPermission_principalOrgID,
    addPermission_revisionId,
    addPermission_qualifier,
    addPermission_sourceAccount,
    addPermission_functionName,
    addPermission_statementId,
    addPermission_action,
    addPermission_principal,
    addPermissionResponse_statement,
    addPermissionResponse_httpStatus,

    -- ** CreateAlias
    createAlias_routingConfig,
    createAlias_description,
    createAlias_functionName,
    createAlias_name,
    createAlias_functionVersion,
    aliasConfiguration_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** CreateCodeSigningConfig
    createCodeSigningConfig_description,
    createCodeSigningConfig_codeSigningPolicies,
    createCodeSigningConfig_allowedPublishers,
    createCodeSigningConfigResponse_httpStatus,
    createCodeSigningConfigResponse_codeSigningConfig,

    -- ** CreateEventSourceMapping
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_amazonManagedKafkaEventSourceConfig,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_enabled,
    createEventSourceMapping_filterCriteria,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_selfManagedKafkaEventSourceConfig,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_topics,
    createEventSourceMapping_queues,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_functionName,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** CreateFunction
    createFunction_tracingConfig,
    createFunction_tags,
    createFunction_fileSystemConfigs,
    createFunction_timeout,
    createFunction_ephemeralStorage,
    createFunction_memorySize,
    createFunction_publish,
    createFunction_imageConfig,
    createFunction_environment,
    createFunction_vpcConfig,
    createFunction_codeSigningConfigArn,
    createFunction_runtime,
    createFunction_description,
    createFunction_kmsKeyArn,
    createFunction_handler,
    createFunction_layers,
    createFunction_packageType,
    createFunction_architectures,
    createFunction_deadLetterConfig,
    createFunction_functionName,
    createFunction_role,
    createFunction_code,
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

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
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

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
    aliasConfiguration_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** GetCodeSigningConfig
    getCodeSigningConfig_codeSigningConfigArn,
    getCodeSigningConfigResponse_httpStatus,
    getCodeSigningConfigResponse_codeSigningConfig,

    -- ** GetEventSourceMapping
    getEventSourceMapping_uuid,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** GetFunction
    getFunction_qualifier,
    getFunction_functionName,
    getFunctionResponse_tags,
    getFunctionResponse_code,
    getFunctionResponse_configuration,
    getFunctionResponse_concurrency,
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
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

    -- ** GetFunctionEventInvokeConfig
    getFunctionEventInvokeConfig_qualifier,
    getFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

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
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_content,
    getLayerVersionResponse_version,

    -- ** GetLayerVersionByArn
    getLayerVersionByArn_arn,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_content,
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
    getProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_statusReason,
    getProvisionedConcurrencyConfigResponse_status,
    getProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_lastModified,
    getProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    getProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** Invoke
    invoke_logType,
    invoke_clientContext,
    invoke_invocationType,
    invoke_qualifier,
    invoke_functionName,
    invoke_payload,
    invokeResponse_executedVersion,
    invokeResponse_functionError,
    invokeResponse_payload,
    invokeResponse_logResult,
    invokeResponse_statusCode,

    -- ** ListAliases
    listAliases_marker,
    listAliases_functionVersion,
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
    listEventSourceMappings_marker,
    listEventSourceMappings_functionName,
    listEventSourceMappings_maxItems,
    listEventSourceMappings_eventSourceArn,
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
    listFunctions_marker,
    listFunctions_masterRegion,
    listFunctions_functionVersion,
    listFunctions_maxItems,
    listFunctionsResponse_functions,
    listFunctionsResponse_nextMarker,
    listFunctionsResponse_httpStatus,

    -- ** ListFunctionsByCodeSigningConfig
    listFunctionsByCodeSigningConfig_marker,
    listFunctionsByCodeSigningConfig_maxItems,
    listFunctionsByCodeSigningConfig_codeSigningConfigArn,
    listFunctionsByCodeSigningConfigResponse_nextMarker,
    listFunctionsByCodeSigningConfigResponse_functionArns,
    listFunctionsByCodeSigningConfigResponse_httpStatus,

    -- ** ListLayerVersions
    listLayerVersions_compatibleArchitecture,
    listLayerVersions_marker,
    listLayerVersions_maxItems,
    listLayerVersions_compatibleRuntime,
    listLayerVersions_layerName,
    listLayerVersionsResponse_nextMarker,
    listLayerVersionsResponse_layerVersions,
    listLayerVersionsResponse_httpStatus,

    -- ** ListLayers
    listLayers_compatibleArchitecture,
    listLayers_marker,
    listLayers_maxItems,
    listLayers_compatibleRuntime,
    listLayersResponse_layers,
    listLayersResponse_nextMarker,
    listLayersResponse_httpStatus,

    -- ** ListProvisionedConcurrencyConfigs
    listProvisionedConcurrencyConfigs_marker,
    listProvisionedConcurrencyConfigs_maxItems,
    listProvisionedConcurrencyConfigs_functionName,
    listProvisionedConcurrencyConfigsResponse_provisionedConcurrencyConfigs,
    listProvisionedConcurrencyConfigsResponse_nextMarker,
    listProvisionedConcurrencyConfigsResponse_httpStatus,

    -- ** ListTags
    listTags_resource,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** ListVersionsByFunction
    listVersionsByFunction_marker,
    listVersionsByFunction_maxItems,
    listVersionsByFunction_functionName,
    listVersionsByFunctionResponse_versions,
    listVersionsByFunctionResponse_nextMarker,
    listVersionsByFunctionResponse_httpStatus,

    -- ** PublishLayerVersion
    publishLayerVersion_compatibleArchitectures,
    publishLayerVersion_licenseInfo,
    publishLayerVersion_description,
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_layerName,
    publishLayerVersion_content,
    publishLayerVersionResponse_compatibleArchitectures,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_httpStatus,

    -- ** PublishVersion
    publishVersion_codeSha256,
    publishVersion_description,
    publishVersion_revisionId,
    publishVersion_functionName,
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

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
    putFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    putFunctionEventInvokeConfig_destinationConfig,
    putFunctionEventInvokeConfig_maximumRetryAttempts,
    putFunctionEventInvokeConfig_qualifier,
    putFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** PutProvisionedConcurrencyConfig
    putProvisionedConcurrencyConfig_functionName,
    putProvisionedConcurrencyConfig_qualifier,
    putProvisionedConcurrencyConfig_provisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_availableProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_statusReason,
    putProvisionedConcurrencyConfigResponse_status,
    putProvisionedConcurrencyConfigResponse_requestedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_lastModified,
    putProvisionedConcurrencyConfigResponse_allocatedProvisionedConcurrentExecutions,
    putProvisionedConcurrencyConfigResponse_httpStatus,

    -- ** RemoveLayerVersionPermission
    removeLayerVersionPermission_revisionId,
    removeLayerVersionPermission_layerName,
    removeLayerVersionPermission_versionNumber,
    removeLayerVersionPermission_statementId,

    -- ** RemovePermission
    removePermission_revisionId,
    removePermission_qualifier,
    removePermission_functionName,
    removePermission_statementId,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,

    -- ** UpdateAlias
    updateAlias_routingConfig,
    updateAlias_functionVersion,
    updateAlias_description,
    updateAlias_revisionId,
    updateAlias_functionName,
    updateAlias_name,
    aliasConfiguration_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

    -- ** UpdateCodeSigningConfig
    updateCodeSigningConfig_description,
    updateCodeSigningConfig_allowedPublishers,
    updateCodeSigningConfig_codeSigningPolicies,
    updateCodeSigningConfig_codeSigningConfigArn,
    updateCodeSigningConfigResponse_httpStatus,
    updateCodeSigningConfigResponse_codeSigningConfig,

    -- ** UpdateEventSourceMapping
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_filterCriteria,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_uuid,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** UpdateFunctionCode
    updateFunctionCode_s3Bucket,
    updateFunctionCode_publish,
    updateFunctionCode_imageUri,
    updateFunctionCode_s3Key,
    updateFunctionCode_zipFile,
    updateFunctionCode_dryRun,
    updateFunctionCode_revisionId,
    updateFunctionCode_s3ObjectVersion,
    updateFunctionCode_architectures,
    updateFunctionCode_functionName,
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

    -- ** UpdateFunctionConfiguration
    updateFunctionConfiguration_tracingConfig,
    updateFunctionConfiguration_fileSystemConfigs,
    updateFunctionConfiguration_timeout,
    updateFunctionConfiguration_ephemeralStorage,
    updateFunctionConfiguration_memorySize,
    updateFunctionConfiguration_imageConfig,
    updateFunctionConfiguration_environment,
    updateFunctionConfiguration_vpcConfig,
    updateFunctionConfiguration_runtime,
    updateFunctionConfiguration_description,
    updateFunctionConfiguration_kmsKeyArn,
    updateFunctionConfiguration_handler,
    updateFunctionConfiguration_layers,
    updateFunctionConfiguration_revisionId,
    updateFunctionConfiguration_role,
    updateFunctionConfiguration_deadLetterConfig,
    updateFunctionConfiguration_functionName,
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

    -- ** UpdateFunctionEventInvokeConfig
    updateFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    updateFunctionEventInvokeConfig_destinationConfig,
    updateFunctionEventInvokeConfig_maximumRetryAttempts,
    updateFunctionEventInvokeConfig_qualifier,
    updateFunctionEventInvokeConfig_functionName,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** UpdateFunctionUrlConfig
    updateFunctionUrlConfig_cors,
    updateFunctionUrlConfig_qualifier,
    updateFunctionUrlConfig_authType,
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
    accountLimit_unreservedConcurrentExecutions,
    accountLimit_totalCodeSize,
    accountLimit_concurrentExecutions,
    accountLimit_codeSizeZipped,
    accountLimit_codeSizeUnzipped,

    -- ** AccountUsage
    accountUsage_totalCodeSize,
    accountUsage_functionCount,

    -- ** AliasConfiguration
    aliasConfiguration_name,
    aliasConfiguration_routingConfig,
    aliasConfiguration_functionVersion,
    aliasConfiguration_aliasArn,
    aliasConfiguration_description,
    aliasConfiguration_revisionId,

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
    cors_allowHeaders,
    cors_exposeHeaders,
    cors_allowCredentials,
    cors_allowMethods,
    cors_allowOrigins,
    cors_maxAge,

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
    environmentResponse_error,
    environmentResponse_variables,

    -- ** EphemeralStorage
    ephemeralStorage_size,

    -- ** EventSourceMappingConfiguration
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_sourceAccessConfigurations,

    -- ** FileSystemConfig
    fileSystemConfig_arn,
    fileSystemConfig_localMountPath,

    -- ** Filter
    filter_pattern,

    -- ** FilterCriteria
    filterCriteria_filters,

    -- ** FunctionCode
    functionCode_s3Bucket,
    functionCode_imageUri,
    functionCode_s3Key,
    functionCode_zipFile,
    functionCode_s3ObjectVersion,

    -- ** FunctionCodeLocation
    functionCodeLocation_imageUri,
    functionCodeLocation_location,
    functionCodeLocation_repositoryType,
    functionCodeLocation_resolvedImageUri,

    -- ** FunctionConfiguration
    functionConfiguration_tracingConfig,
    functionConfiguration_fileSystemConfigs,
    functionConfiguration_lastUpdateStatusReason,
    functionConfiguration_masterArn,
    functionConfiguration_functionArn,
    functionConfiguration_timeout,
    functionConfiguration_ephemeralStorage,
    functionConfiguration_memorySize,
    functionConfiguration_codeSha256,
    functionConfiguration_environment,
    functionConfiguration_vpcConfig,
    functionConfiguration_state,
    functionConfiguration_functionName,
    functionConfiguration_runtime,
    functionConfiguration_signingProfileVersionArn,
    functionConfiguration_description,
    functionConfiguration_kmsKeyArn,
    functionConfiguration_handler,
    functionConfiguration_layers,
    functionConfiguration_stateReasonCode,
    functionConfiguration_packageType,
    functionConfiguration_lastUpdateStatusReasonCode,
    functionConfiguration_revisionId,
    functionConfiguration_signingJobArn,
    functionConfiguration_imageConfigResponse,
    functionConfiguration_lastUpdateStatus,
    functionConfiguration_lastModified,
    functionConfiguration_role,
    functionConfiguration_architectures,
    functionConfiguration_stateReason,
    functionConfiguration_version,
    functionConfiguration_deadLetterConfig,
    functionConfiguration_codeSize,

    -- ** FunctionEventInvokeConfig
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,

    -- ** FunctionUrlConfig
    functionUrlConfig_cors,
    functionUrlConfig_functionUrl,
    functionUrlConfig_functionArn,
    functionUrlConfig_creationTime,
    functionUrlConfig_lastModifiedTime,
    functionUrlConfig_authType,

    -- ** GetLayerVersionResponse
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_content,
    getLayerVersionResponse_version,

    -- ** ImageConfig
    imageConfig_command,
    imageConfig_entryPoint,
    imageConfig_workingDirectory,

    -- ** ImageConfigError
    imageConfigError_message,
    imageConfigError_errorCode,

    -- ** ImageConfigResponse
    imageConfigResponse_imageConfig,
    imageConfigResponse_error,

    -- ** Layer
    layer_arn,
    layer_signingProfileVersionArn,
    layer_signingJobArn,
    layer_codeSize,

    -- ** LayerVersionContentInput
    layerVersionContentInput_s3Bucket,
    layerVersionContentInput_s3Key,
    layerVersionContentInput_zipFile,
    layerVersionContentInput_s3ObjectVersion,

    -- ** LayerVersionContentOutput
    layerVersionContentOutput_codeSha256,
    layerVersionContentOutput_signingProfileVersionArn,
    layerVersionContentOutput_location,
    layerVersionContentOutput_signingJobArn,
    layerVersionContentOutput_codeSize,

    -- ** LayerVersionsListItem
    layerVersionsListItem_compatibleArchitectures,
    layerVersionsListItem_layerVersionArn,
    layerVersionsListItem_licenseInfo,
    layerVersionsListItem_description,
    layerVersionsListItem_compatibleRuntimes,
    layerVersionsListItem_createdDate,
    layerVersionsListItem_version,

    -- ** LayersListItem
    layersListItem_layerArn,
    layersListItem_layerName,
    layersListItem_latestMatchingVersion,

    -- ** OnFailure
    onFailure_destination,

    -- ** OnSuccess
    onSuccess_destination,

    -- ** ProvisionedConcurrencyConfigListItem
    provisionedConcurrencyConfigListItem_functionArn,
    provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_statusReason,
    provisionedConcurrencyConfigListItem_status,
    provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions,
    provisionedConcurrencyConfigListItem_lastModified,
    provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions,

    -- ** SelfManagedEventSource
    selfManagedEventSource_endpoints,

    -- ** SelfManagedKafkaEventSourceConfig
    selfManagedKafkaEventSourceConfig_consumerGroupId,

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
    vpcConfigResponse_vpcId,
    vpcConfigResponse_subnetIds,
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
