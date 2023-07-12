{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Lens
  ( -- * Operations

    -- ** CreateApiKey
    createApiKey_customerId,
    createApiKey_description,
    createApiKey_enabled,
    createApiKey_generateDistinctId,
    createApiKey_name,
    createApiKey_stageKeys,
    createApiKey_tags,
    createApiKey_value,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
    apiKey_value,

    -- ** CreateAuthorizer
    createAuthorizer_authType,
    createAuthorizer_authorizerCredentials,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_authorizerUri,
    createAuthorizer_identitySource,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_providerARNs,
    createAuthorizer_restApiId,
    createAuthorizer_name,
    createAuthorizer_type,
    authorizer_authType,
    authorizer_authorizerCredentials,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerUri,
    authorizer_id,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_type,

    -- ** CreateBasePathMapping
    createBasePathMapping_basePath,
    createBasePathMapping_stage,
    createBasePathMapping_domainName,
    createBasePathMapping_restApiId,
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,

    -- ** CreateDeployment
    createDeployment_cacheClusterEnabled,
    createDeployment_cacheClusterSize,
    createDeployment_canarySettings,
    createDeployment_description,
    createDeployment_stageDescription,
    createDeployment_stageName,
    createDeployment_tracingEnabled,
    createDeployment_variables,
    createDeployment_restApiId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,

    -- ** CreateDocumentationPart
    createDocumentationPart_restApiId,
    createDocumentationPart_location,
    createDocumentationPart_properties,
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,

    -- ** CreateDocumentationVersion
    createDocumentationVersion_description,
    createDocumentationVersion_stageName,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,

    -- ** CreateDomainName
    createDomainName_certificateArn,
    createDomainName_certificateBody,
    createDomainName_certificateChain,
    createDomainName_certificateName,
    createDomainName_certificatePrivateKey,
    createDomainName_endpointConfiguration,
    createDomainName_mutualTlsAuthentication,
    createDomainName_ownershipVerificationCertificateArn,
    createDomainName_regionalCertificateArn,
    createDomainName_regionalCertificateName,
    createDomainName_securityPolicy,
    createDomainName_tags,
    createDomainName_domainName,
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
    domainName_tags,

    -- ** CreateModel
    createModel_description,
    createModel_schema,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,

    -- ** CreateRequestValidator
    createRequestValidator_name,
    createRequestValidator_validateRequestBody,
    createRequestValidator_validateRequestParameters,
    createRequestValidator_restApiId,
    requestValidator_id,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_validateRequestParameters,

    -- ** CreateResource
    createResource_restApiId,
    createResource_parentId,
    createResource_pathPart,
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,

    -- ** CreateRestApi
    createRestApi_apiKeySource,
    createRestApi_binaryMediaTypes,
    createRestApi_cloneFrom,
    createRestApi_description,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_endpointConfiguration,
    createRestApi_minimumCompressionSize,
    createRestApi_policy,
    createRestApi_tags,
    createRestApi_version,
    createRestApi_name,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** CreateStage
    createStage_cacheClusterEnabled,
    createStage_cacheClusterSize,
    createStage_canarySettings,
    createStage_description,
    createStage_documentationVersion,
    createStage_tags,
    createStage_tracingEnabled,
    createStage_variables,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,

    -- ** CreateUsagePlan
    createUsagePlan_apiStages,
    createUsagePlan_description,
    createUsagePlan_quota,
    createUsagePlan_tags,
    createUsagePlan_throttle,
    createUsagePlan_name,
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,

    -- ** CreateUsagePlanKey
    createUsagePlanKey_usagePlanId,
    createUsagePlanKey_keyId,
    createUsagePlanKey_keyType,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_value,

    -- ** CreateVpcLink
    createVpcLink_description,
    createVpcLink_tags,
    createVpcLink_name,
    createVpcLink_targetArns,
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,

    -- ** DeleteApiKey
    deleteApiKey_apiKey,

    -- ** DeleteAuthorizer
    deleteAuthorizer_restApiId,
    deleteAuthorizer_authorizerId,

    -- ** DeleteBasePathMapping
    deleteBasePathMapping_domainName,
    deleteBasePathMapping_basePath,

    -- ** DeleteClientCertificate
    deleteClientCertificate_clientCertificateId,

    -- ** DeleteDeployment
    deleteDeployment_restApiId,
    deleteDeployment_deploymentId,

    -- ** DeleteDocumentationPart
    deleteDocumentationPart_restApiId,
    deleteDocumentationPart_documentationPartId,

    -- ** DeleteDocumentationVersion
    deleteDocumentationVersion_restApiId,
    deleteDocumentationVersion_documentationVersion,

    -- ** DeleteDomainName
    deleteDomainName_domainName,

    -- ** DeleteGatewayResponse
    deleteGatewayResponse_restApiId,
    deleteGatewayResponse_responseType,

    -- ** DeleteIntegration
    deleteIntegration_restApiId,
    deleteIntegration_resourceId,
    deleteIntegration_httpMethod,

    -- ** DeleteIntegrationResponse
    deleteIntegrationResponse_restApiId,
    deleteIntegrationResponse_resourceId,
    deleteIntegrationResponse_httpMethod,
    deleteIntegrationResponse_statusCode,

    -- ** DeleteMethod
    deleteMethod_restApiId,
    deleteMethod_resourceId,
    deleteMethod_httpMethod,

    -- ** DeleteMethodResponse
    deleteMethodResponse_restApiId,
    deleteMethodResponse_resourceId,
    deleteMethodResponse_httpMethod,
    deleteMethodResponse_statusCode,

    -- ** DeleteModel
    deleteModel_restApiId,
    deleteModel_modelName,

    -- ** DeleteRequestValidator
    deleteRequestValidator_restApiId,
    deleteRequestValidator_requestValidatorId,

    -- ** DeleteResource
    deleteResource_restApiId,
    deleteResource_resourceId,

    -- ** DeleteRestApi
    deleteRestApi_restApiId,

    -- ** DeleteStage
    deleteStage_restApiId,
    deleteStage_stageName,

    -- ** DeleteUsagePlan
    deleteUsagePlan_usagePlanId,

    -- ** DeleteUsagePlanKey
    deleteUsagePlanKey_usagePlanId,
    deleteUsagePlanKey_keyId,

    -- ** DeleteVpcLink
    deleteVpcLink_vpcLinkId,

    -- ** FlushStageAuthorizersCache
    flushStageAuthorizersCache_restApiId,
    flushStageAuthorizersCache_stageName,

    -- ** FlushStageCache
    flushStageCache_restApiId,
    flushStageCache_stageName,

    -- ** GenerateClientCertificate
    generateClientCertificate_description,
    generateClientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,

    -- ** GetAccount
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** GetApiKey
    getApiKey_includeValue,
    getApiKey_apiKey,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
    apiKey_value,

    -- ** GetApiKeys
    getApiKeys_customerId,
    getApiKeys_includeValues,
    getApiKeys_limit,
    getApiKeys_nameQuery,
    getApiKeys_position,
    getApiKeysResponse_items,
    getApiKeysResponse_position,
    getApiKeysResponse_warnings,
    getApiKeysResponse_httpStatus,

    -- ** GetAuthorizer
    getAuthorizer_restApiId,
    getAuthorizer_authorizerId,
    authorizer_authType,
    authorizer_authorizerCredentials,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerUri,
    authorizer_id,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_type,

    -- ** GetAuthorizers
    getAuthorizers_limit,
    getAuthorizers_position,
    getAuthorizers_restApiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_position,
    getAuthorizersResponse_httpStatus,

    -- ** GetBasePathMapping
    getBasePathMapping_domainName,
    getBasePathMapping_basePath,
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,

    -- ** GetBasePathMappings
    getBasePathMappings_limit,
    getBasePathMappings_position,
    getBasePathMappings_domainName,
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,

    -- ** GetClientCertificate
    getClientCertificate_clientCertificateId,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,

    -- ** GetClientCertificates
    getClientCertificates_limit,
    getClientCertificates_position,
    getClientCertificatesResponse_items,
    getClientCertificatesResponse_position,
    getClientCertificatesResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_embed,
    getDeployment_restApiId,
    getDeployment_deploymentId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,

    -- ** GetDeployments
    getDeployments_limit,
    getDeployments_position,
    getDeployments_restApiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_position,
    getDeploymentsResponse_httpStatus,

    -- ** GetDocumentationPart
    getDocumentationPart_restApiId,
    getDocumentationPart_documentationPartId,
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,

    -- ** GetDocumentationParts
    getDocumentationParts_limit,
    getDocumentationParts_locationStatus,
    getDocumentationParts_nameQuery,
    getDocumentationParts_path,
    getDocumentationParts_position,
    getDocumentationParts_type,
    getDocumentationParts_restApiId,
    getDocumentationPartsResponse_items,
    getDocumentationPartsResponse_position,
    getDocumentationPartsResponse_httpStatus,

    -- ** GetDocumentationVersion
    getDocumentationVersion_restApiId,
    getDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,

    -- ** GetDocumentationVersions
    getDocumentationVersions_limit,
    getDocumentationVersions_position,
    getDocumentationVersions_restApiId,
    getDocumentationVersionsResponse_items,
    getDocumentationVersionsResponse_position,
    getDocumentationVersionsResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
    domainName_tags,

    -- ** GetDomainNames
    getDomainNames_limit,
    getDomainNames_position,
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,

    -- ** GetExport
    getExport_accepts,
    getExport_parameters,
    getExport_restApiId,
    getExport_stageName,
    getExport_exportType,
    getExportResponse_body,
    getExportResponse_contentDisposition,
    getExportResponse_contentType,
    getExportResponse_httpStatus,

    -- ** GetGatewayResponse
    getGatewayResponse_restApiId,
    getGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,

    -- ** GetGatewayResponses
    getGatewayResponses_limit,
    getGatewayResponses_position,
    getGatewayResponses_restApiId,
    getGatewayResponsesResponse_items,
    getGatewayResponsesResponse_position,
    getGatewayResponsesResponse_httpStatus,

    -- ** GetIntegration
    getIntegration_restApiId,
    getIntegration_resourceId,
    getIntegration_httpMethod,
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,

    -- ** GetIntegrationResponse
    getIntegrationResponse_restApiId,
    getIntegrationResponse_resourceId,
    getIntegrationResponse_httpMethod,
    getIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,

    -- ** GetMethod
    getMethod_restApiId,
    getMethod_resourceId,
    getMethod_httpMethod,
    method_apiKeyRequired,
    method_authorizationScopes,
    method_authorizationType,
    method_authorizerId,
    method_httpMethod,
    method_methodIntegration,
    method_methodResponses,
    method_operationName,
    method_requestModels,
    method_requestParameters,
    method_requestValidatorId,

    -- ** GetMethodResponse
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,

    -- ** GetModel
    getModel_flatten,
    getModel_restApiId,
    getModel_modelName,
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,

    -- ** GetModelTemplate
    getModelTemplate_restApiId,
    getModelTemplate_modelName,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- ** GetModels
    getModels_limit,
    getModels_position,
    getModels_restApiId,
    getModelsResponse_items,
    getModelsResponse_position,
    getModelsResponse_httpStatus,

    -- ** GetRequestValidator
    getRequestValidator_restApiId,
    getRequestValidator_requestValidatorId,
    requestValidator_id,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_validateRequestParameters,

    -- ** GetRequestValidators
    getRequestValidators_limit,
    getRequestValidators_position,
    getRequestValidators_restApiId,
    getRequestValidatorsResponse_items,
    getRequestValidatorsResponse_position,
    getRequestValidatorsResponse_httpStatus,

    -- ** GetResource
    getResource_embed,
    getResource_restApiId,
    getResource_resourceId,
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,

    -- ** GetResources
    getResources_embed,
    getResources_limit,
    getResources_position,
    getResources_restApiId,
    getResourcesResponse_items,
    getResourcesResponse_position,
    getResourcesResponse_httpStatus,

    -- ** GetRestApi
    getRestApi_restApiId,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** GetRestApis
    getRestApis_limit,
    getRestApis_position,
    getRestApisResponse_items,
    getRestApisResponse_position,
    getRestApisResponse_httpStatus,

    -- ** GetSdk
    getSdk_parameters,
    getSdk_restApiId,
    getSdk_stageName,
    getSdk_sdkType,
    getSdkResponse_body,
    getSdkResponse_contentDisposition,
    getSdkResponse_contentType,
    getSdkResponse_httpStatus,

    -- ** GetSdkType
    getSdkType_id,
    sdkType_configurationProperties,
    sdkType_description,
    sdkType_friendlyName,
    sdkType_id,

    -- ** GetSdkTypes
    getSdkTypes_limit,
    getSdkTypes_position,
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,

    -- ** GetStage
    getStage_restApiId,
    getStage_stageName,
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,

    -- ** GetStages
    getStages_deploymentId,
    getStages_restApiId,
    getStagesResponse_item,
    getStagesResponse_httpStatus,

    -- ** GetTags
    getTags_limit,
    getTags_position,
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** GetUsage
    getUsage_keyId,
    getUsage_limit,
    getUsage_position,
    getUsage_usagePlanId,
    getUsage_startDate,
    getUsage_endDate,
    usage_endDate,
    usage_items,
    usage_position,
    usage_startDate,
    usage_usagePlanId,

    -- ** GetUsagePlan
    getUsagePlan_usagePlanId,
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,

    -- ** GetUsagePlanKey
    getUsagePlanKey_usagePlanId,
    getUsagePlanKey_keyId,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_value,

    -- ** GetUsagePlanKeys
    getUsagePlanKeys_limit,
    getUsagePlanKeys_nameQuery,
    getUsagePlanKeys_position,
    getUsagePlanKeys_usagePlanId,
    getUsagePlanKeysResponse_items,
    getUsagePlanKeysResponse_position,
    getUsagePlanKeysResponse_httpStatus,

    -- ** GetUsagePlans
    getUsagePlans_keyId,
    getUsagePlans_limit,
    getUsagePlans_position,
    getUsagePlansResponse_items,
    getUsagePlansResponse_position,
    getUsagePlansResponse_httpStatus,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,

    -- ** GetVpcLinks
    getVpcLinks_limit,
    getVpcLinks_position,
    getVpcLinksResponse_items,
    getVpcLinksResponse_position,
    getVpcLinksResponse_httpStatus,

    -- ** ImportApiKeys
    importApiKeys_failOnWarnings,
    importApiKeys_body,
    importApiKeys_format,
    importApiKeysResponse_ids,
    importApiKeysResponse_warnings,
    importApiKeysResponse_httpStatus,

    -- ** ImportDocumentationParts
    importDocumentationParts_failOnWarnings,
    importDocumentationParts_mode,
    importDocumentationParts_restApiId,
    importDocumentationParts_body,
    importDocumentationPartsResponse_ids,
    importDocumentationPartsResponse_warnings,
    importDocumentationPartsResponse_httpStatus,

    -- ** ImportRestApi
    importRestApi_failOnWarnings,
    importRestApi_parameters,
    importRestApi_body,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** PutGatewayResponse
    putGatewayResponse_responseParameters,
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,

    -- ** PutIntegration
    putIntegration_cacheKeyParameters,
    putIntegration_cacheNamespace,
    putIntegration_connectionId,
    putIntegration_connectionType,
    putIntegration_contentHandling,
    putIntegration_credentials,
    putIntegration_integrationHttpMethod,
    putIntegration_passthroughBehavior,
    putIntegration_requestParameters,
    putIntegration_requestTemplates,
    putIntegration_timeoutInMillis,
    putIntegration_tlsConfig,
    putIntegration_uri,
    putIntegration_restApiId,
    putIntegration_resourceId,
    putIntegration_httpMethod,
    putIntegration_type,
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,

    -- ** PutIntegrationResponse
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_responseParameters,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_selectionPattern,
    putIntegrationResponse_restApiId,
    putIntegrationResponse_resourceId,
    putIntegrationResponse_httpMethod,
    putIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,

    -- ** PutMethod
    putMethod_apiKeyRequired,
    putMethod_authorizationScopes,
    putMethod_authorizerId,
    putMethod_operationName,
    putMethod_requestModels,
    putMethod_requestParameters,
    putMethod_requestValidatorId,
    putMethod_restApiId,
    putMethod_resourceId,
    putMethod_httpMethod,
    putMethod_authorizationType,
    method_apiKeyRequired,
    method_authorizationScopes,
    method_authorizationType,
    method_authorizerId,
    method_httpMethod,
    method_methodIntegration,
    method_methodResponses,
    method_operationName,
    method_requestModels,
    method_requestParameters,
    method_requestValidatorId,

    -- ** PutMethodResponse
    putMethodResponse_responseModels,
    putMethodResponse_responseParameters,
    putMethodResponse_restApiId,
    putMethodResponse_resourceId,
    putMethodResponse_httpMethod,
    putMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,

    -- ** PutRestApi
    putRestApi_failOnWarnings,
    putRestApi_mode,
    putRestApi_parameters,
    putRestApi_restApiId,
    putRestApi_body,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** TestInvokeMethod
    testInvokeMethod_body,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_headers,
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_stageVariables,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateAccount
    updateAccount_patchOperations,
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** UpdateApiKey
    updateApiKey_patchOperations,
    updateApiKey_apiKey,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
    apiKey_value,

    -- ** UpdateAuthorizer
    updateAuthorizer_patchOperations,
    updateAuthorizer_restApiId,
    updateAuthorizer_authorizerId,
    authorizer_authType,
    authorizer_authorizerCredentials,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerUri,
    authorizer_id,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_type,

    -- ** UpdateBasePathMapping
    updateBasePathMapping_patchOperations,
    updateBasePathMapping_domainName,
    updateBasePathMapping_basePath,
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,

    -- ** UpdateClientCertificate
    updateClientCertificate_patchOperations,
    updateClientCertificate_clientCertificateId,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,

    -- ** UpdateDeployment
    updateDeployment_patchOperations,
    updateDeployment_restApiId,
    updateDeployment_deploymentId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,

    -- ** UpdateDocumentationPart
    updateDocumentationPart_patchOperations,
    updateDocumentationPart_restApiId,
    updateDocumentationPart_documentationPartId,
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,

    -- ** UpdateDocumentationVersion
    updateDocumentationVersion_patchOperations,
    updateDocumentationVersion_restApiId,
    updateDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,

    -- ** UpdateDomainName
    updateDomainName_patchOperations,
    updateDomainName_domainName,
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
    domainName_tags,

    -- ** UpdateGatewayResponse
    updateGatewayResponse_patchOperations,
    updateGatewayResponse_restApiId,
    updateGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,

    -- ** UpdateIntegration
    updateIntegration_patchOperations,
    updateIntegration_restApiId,
    updateIntegration_resourceId,
    updateIntegration_httpMethod,
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_patchOperations,
    updateIntegrationResponse_restApiId,
    updateIntegrationResponse_resourceId,
    updateIntegrationResponse_httpMethod,
    updateIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,

    -- ** UpdateMethod
    updateMethod_patchOperations,
    updateMethod_restApiId,
    updateMethod_resourceId,
    updateMethod_httpMethod,
    method_apiKeyRequired,
    method_authorizationScopes,
    method_authorizationType,
    method_authorizerId,
    method_httpMethod,
    method_methodIntegration,
    method_methodResponses,
    method_operationName,
    method_requestModels,
    method_requestParameters,
    method_requestValidatorId,

    -- ** UpdateMethodResponse
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,

    -- ** UpdateModel
    updateModel_patchOperations,
    updateModel_restApiId,
    updateModel_modelName,
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,

    -- ** UpdateRequestValidator
    updateRequestValidator_patchOperations,
    updateRequestValidator_restApiId,
    updateRequestValidator_requestValidatorId,
    requestValidator_id,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_validateRequestParameters,

    -- ** UpdateResource
    updateResource_patchOperations,
    updateResource_restApiId,
    updateResource_resourceId,
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,

    -- ** UpdateRestApi
    updateRestApi_patchOperations,
    updateRestApi_restApiId,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** UpdateStage
    updateStage_patchOperations,
    updateStage_restApiId,
    updateStage_stageName,
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,

    -- ** UpdateUsage
    updateUsage_patchOperations,
    updateUsage_usagePlanId,
    updateUsage_keyId,
    usage_endDate,
    usage_items,
    usage_position,
    usage_startDate,
    usage_usagePlanId,

    -- ** UpdateUsagePlan
    updateUsagePlan_patchOperations,
    updateUsagePlan_usagePlanId,
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,

    -- ** UpdateVpcLink
    updateVpcLink_patchOperations,
    updateVpcLink_vpcLinkId,
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_destinationArn,
    accessLogSettings_format,

    -- ** Account
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** ApiKey
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
    apiKey_value,

    -- ** ApiStage
    apiStage_apiId,
    apiStage_stage,
    apiStage_throttle,

    -- ** Authorizer
    authorizer_authType,
    authorizer_authorizerCredentials,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerUri,
    authorizer_id,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_type,

    -- ** BasePathMapping
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,

    -- ** CanarySettings
    canarySettings_deploymentId,
    canarySettings_percentTraffic,
    canarySettings_stageVariableOverrides,
    canarySettings_useStageCache,

    -- ** ClientCertificate
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,

    -- ** Deployment
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,

    -- ** DeploymentCanarySettings
    deploymentCanarySettings_percentTraffic,
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_useStageCache,

    -- ** DocumentationPart
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,

    -- ** DocumentationPartLocation
    documentationPartLocation_method,
    documentationPartLocation_name,
    documentationPartLocation_path,
    documentationPartLocation_statusCode,
    documentationPartLocation_type,

    -- ** DocumentationVersion
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,

    -- ** DomainName
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
    domainName_tags,

    -- ** EndpointConfiguration
    endpointConfiguration_types,
    endpointConfiguration_vpcEndpointIds,

    -- ** GatewayResponse
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,

    -- ** Integration
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,

    -- ** IntegrationResponse
    integrationResponse_contentHandling,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,

    -- ** Method
    method_apiKeyRequired,
    method_authorizationScopes,
    method_authorizationType,
    method_authorizerId,
    method_httpMethod,
    method_methodIntegration,
    method_methodResponses,
    method_operationName,
    method_requestModels,
    method_requestParameters,
    method_requestValidatorId,

    -- ** MethodResponse
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,

    -- ** MethodSetting
    methodSetting_cacheDataEncrypted,
    methodSetting_cacheTtlInSeconds,
    methodSetting_cachingEnabled,
    methodSetting_dataTraceEnabled,
    methodSetting_loggingLevel,
    methodSetting_metricsEnabled,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_throttlingBurstLimit,
    methodSetting_throttlingRateLimit,
    methodSetting_unauthorizedCacheControlHeaderStrategy,

    -- ** MethodSnapshot
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- ** Model
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreWarnings,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

    -- ** PatchOperation
    patchOperation_from,
    patchOperation_op,
    patchOperation_path,
    patchOperation_value,

    -- ** QuotaSettings
    quotaSettings_limit,
    quotaSettings_offset,
    quotaSettings_period,

    -- ** RequestValidator
    requestValidator_id,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_validateRequestParameters,

    -- ** Resource
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,

    -- ** RestApi
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- ** SdkConfigurationProperty
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_required,

    -- ** SdkType
    sdkType_configurationProperties,
    sdkType_description,
    sdkType_friendlyName,
    sdkType_id,

    -- ** Stage
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,

    -- ** StageKey
    stageKey_restApiId,
    stageKey_stageName,

    -- ** ThrottleSettings
    throttleSettings_burstLimit,
    throttleSettings_rateLimit,

    -- ** TlsConfig
    tlsConfig_insecureSkipVerification,

    -- ** Usage
    usage_endDate,
    usage_items,
    usage_position,
    usage_startDate,
    usage_usagePlanId,

    -- ** UsagePlan
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,

    -- ** UsagePlanKey
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_value,

    -- ** VpcLink
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,
  )
where

import Amazonka.APIGateway.CreateApiKey
import Amazonka.APIGateway.CreateAuthorizer
import Amazonka.APIGateway.CreateBasePathMapping
import Amazonka.APIGateway.CreateDeployment
import Amazonka.APIGateway.CreateDocumentationPart
import Amazonka.APIGateway.CreateDocumentationVersion
import Amazonka.APIGateway.CreateDomainName
import Amazonka.APIGateway.CreateModel
import Amazonka.APIGateway.CreateRequestValidator
import Amazonka.APIGateway.CreateResource
import Amazonka.APIGateway.CreateRestApi
import Amazonka.APIGateway.CreateStage
import Amazonka.APIGateway.CreateUsagePlan
import Amazonka.APIGateway.CreateUsagePlanKey
import Amazonka.APIGateway.CreateVpcLink
import Amazonka.APIGateway.DeleteApiKey
import Amazonka.APIGateway.DeleteAuthorizer
import Amazonka.APIGateway.DeleteBasePathMapping
import Amazonka.APIGateway.DeleteClientCertificate
import Amazonka.APIGateway.DeleteDeployment
import Amazonka.APIGateway.DeleteDocumentationPart
import Amazonka.APIGateway.DeleteDocumentationVersion
import Amazonka.APIGateway.DeleteDomainName
import Amazonka.APIGateway.DeleteGatewayResponse
import Amazonka.APIGateway.DeleteIntegration
import Amazonka.APIGateway.DeleteIntegrationResponse
import Amazonka.APIGateway.DeleteMethod
import Amazonka.APIGateway.DeleteMethodResponse
import Amazonka.APIGateway.DeleteModel
import Amazonka.APIGateway.DeleteRequestValidator
import Amazonka.APIGateway.DeleteResource
import Amazonka.APIGateway.DeleteRestApi
import Amazonka.APIGateway.DeleteStage
import Amazonka.APIGateway.DeleteUsagePlan
import Amazonka.APIGateway.DeleteUsagePlanKey
import Amazonka.APIGateway.DeleteVpcLink
import Amazonka.APIGateway.FlushStageAuthorizersCache
import Amazonka.APIGateway.FlushStageCache
import Amazonka.APIGateway.GenerateClientCertificate
import Amazonka.APIGateway.GetAccount
import Amazonka.APIGateway.GetApiKey
import Amazonka.APIGateway.GetApiKeys
import Amazonka.APIGateway.GetAuthorizer
import Amazonka.APIGateway.GetAuthorizers
import Amazonka.APIGateway.GetBasePathMapping
import Amazonka.APIGateway.GetBasePathMappings
import Amazonka.APIGateway.GetClientCertificate
import Amazonka.APIGateway.GetClientCertificates
import Amazonka.APIGateway.GetDeployment
import Amazonka.APIGateway.GetDeployments
import Amazonka.APIGateway.GetDocumentationPart
import Amazonka.APIGateway.GetDocumentationParts
import Amazonka.APIGateway.GetDocumentationVersion
import Amazonka.APIGateway.GetDocumentationVersions
import Amazonka.APIGateway.GetDomainName
import Amazonka.APIGateway.GetDomainNames
import Amazonka.APIGateway.GetExport
import Amazonka.APIGateway.GetGatewayResponse
import Amazonka.APIGateway.GetGatewayResponses
import Amazonka.APIGateway.GetIntegration
import Amazonka.APIGateway.GetIntegrationResponse
import Amazonka.APIGateway.GetMethod
import Amazonka.APIGateway.GetMethodResponse
import Amazonka.APIGateway.GetModel
import Amazonka.APIGateway.GetModelTemplate
import Amazonka.APIGateway.GetModels
import Amazonka.APIGateway.GetRequestValidator
import Amazonka.APIGateway.GetRequestValidators
import Amazonka.APIGateway.GetResource
import Amazonka.APIGateway.GetResources
import Amazonka.APIGateway.GetRestApi
import Amazonka.APIGateway.GetRestApis
import Amazonka.APIGateway.GetSdk
import Amazonka.APIGateway.GetSdkType
import Amazonka.APIGateway.GetSdkTypes
import Amazonka.APIGateway.GetStage
import Amazonka.APIGateway.GetStages
import Amazonka.APIGateway.GetTags
import Amazonka.APIGateway.GetUsage
import Amazonka.APIGateway.GetUsagePlan
import Amazonka.APIGateway.GetUsagePlanKey
import Amazonka.APIGateway.GetUsagePlanKeys
import Amazonka.APIGateway.GetUsagePlans
import Amazonka.APIGateway.GetVpcLink
import Amazonka.APIGateway.GetVpcLinks
import Amazonka.APIGateway.ImportApiKeys
import Amazonka.APIGateway.ImportDocumentationParts
import Amazonka.APIGateway.ImportRestApi
import Amazonka.APIGateway.PutGatewayResponse
import Amazonka.APIGateway.PutIntegration
import Amazonka.APIGateway.PutIntegrationResponse
import Amazonka.APIGateway.PutMethod
import Amazonka.APIGateway.PutMethodResponse
import Amazonka.APIGateway.PutRestApi
import Amazonka.APIGateway.TagResource
import Amazonka.APIGateway.TestInvokeAuthorizer
import Amazonka.APIGateway.TestInvokeMethod
import Amazonka.APIGateway.Types.AccessLogSettings
import Amazonka.APIGateway.Types.Account
import Amazonka.APIGateway.Types.ApiKey
import Amazonka.APIGateway.Types.ApiStage
import Amazonka.APIGateway.Types.Authorizer
import Amazonka.APIGateway.Types.BasePathMapping
import Amazonka.APIGateway.Types.CanarySettings
import Amazonka.APIGateway.Types.ClientCertificate
import Amazonka.APIGateway.Types.Deployment
import Amazonka.APIGateway.Types.DeploymentCanarySettings
import Amazonka.APIGateway.Types.DocumentationPart
import Amazonka.APIGateway.Types.DocumentationPartLocation
import Amazonka.APIGateway.Types.DocumentationVersion
import Amazonka.APIGateway.Types.DomainName
import Amazonka.APIGateway.Types.EndpointConfiguration
import Amazonka.APIGateway.Types.GatewayResponse
import Amazonka.APIGateway.Types.Integration
import Amazonka.APIGateway.Types.IntegrationResponse
import Amazonka.APIGateway.Types.Method
import Amazonka.APIGateway.Types.MethodResponse
import Amazonka.APIGateway.Types.MethodSetting
import Amazonka.APIGateway.Types.MethodSnapshot
import Amazonka.APIGateway.Types.Model
import Amazonka.APIGateway.Types.MutualTlsAuthentication
import Amazonka.APIGateway.Types.MutualTlsAuthenticationInput
import Amazonka.APIGateway.Types.PatchOperation
import Amazonka.APIGateway.Types.QuotaSettings
import Amazonka.APIGateway.Types.RequestValidator
import Amazonka.APIGateway.Types.Resource
import Amazonka.APIGateway.Types.RestApi
import Amazonka.APIGateway.Types.SdkConfigurationProperty
import Amazonka.APIGateway.Types.SdkType
import Amazonka.APIGateway.Types.Stage
import Amazonka.APIGateway.Types.StageKey
import Amazonka.APIGateway.Types.ThrottleSettings
import Amazonka.APIGateway.Types.TlsConfig
import Amazonka.APIGateway.Types.Usage
import Amazonka.APIGateway.Types.UsagePlan
import Amazonka.APIGateway.Types.UsagePlanKey
import Amazonka.APIGateway.Types.VpcLink
import Amazonka.APIGateway.UntagResource
import Amazonka.APIGateway.UpdateAccount
import Amazonka.APIGateway.UpdateApiKey
import Amazonka.APIGateway.UpdateAuthorizer
import Amazonka.APIGateway.UpdateBasePathMapping
import Amazonka.APIGateway.UpdateClientCertificate
import Amazonka.APIGateway.UpdateDeployment
import Amazonka.APIGateway.UpdateDocumentationPart
import Amazonka.APIGateway.UpdateDocumentationVersion
import Amazonka.APIGateway.UpdateDomainName
import Amazonka.APIGateway.UpdateGatewayResponse
import Amazonka.APIGateway.UpdateIntegration
import Amazonka.APIGateway.UpdateIntegrationResponse
import Amazonka.APIGateway.UpdateMethod
import Amazonka.APIGateway.UpdateMethodResponse
import Amazonka.APIGateway.UpdateModel
import Amazonka.APIGateway.UpdateRequestValidator
import Amazonka.APIGateway.UpdateResource
import Amazonka.APIGateway.UpdateRestApi
import Amazonka.APIGateway.UpdateStage
import Amazonka.APIGateway.UpdateUsage
import Amazonka.APIGateway.UpdateUsagePlan
import Amazonka.APIGateway.UpdateVpcLink
