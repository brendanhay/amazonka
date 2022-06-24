{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Lens
  ( -- * Operations

    -- ** CreateApiKey
    createApiKey_tags,
    createApiKey_generateDistinctId,
    createApiKey_customerId,
    createApiKey_name,
    createApiKey_description,
    createApiKey_stageKeys,
    createApiKey_enabled,
    createApiKey_value,
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,

    -- ** CreateAuthorizer
    createAuthorizer_authorizerCredentials,
    createAuthorizer_identitySource,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_authorizerUri,
    createAuthorizer_providerARNs,
    createAuthorizer_authType,
    createAuthorizer_restApiId,
    createAuthorizer_name,
    createAuthorizer_type,
    authorizer_name,
    authorizer_type,
    authorizer_authorizerCredentials,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_id,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_providerARNs,
    authorizer_authType,

    -- ** CreateBasePathMapping
    createBasePathMapping_stage,
    createBasePathMapping_basePath,
    createBasePathMapping_domainName,
    createBasePathMapping_restApiId,
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,

    -- ** CreateDeployment
    createDeployment_stageName,
    createDeployment_cacheClusterEnabled,
    createDeployment_description,
    createDeployment_tracingEnabled,
    createDeployment_cacheClusterSize,
    createDeployment_canarySettings,
    createDeployment_variables,
    createDeployment_stageDescription,
    createDeployment_restApiId,
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,

    -- ** CreateDocumentationPart
    createDocumentationPart_restApiId,
    createDocumentationPart_location,
    createDocumentationPart_properties,
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,

    -- ** CreateDocumentationVersion
    createDocumentationVersion_stageName,
    createDocumentationVersion_description,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,
    documentationVersion_description,
    documentationVersion_createdDate,
    documentationVersion_version,

    -- ** CreateDomainName
    createDomainName_tags,
    createDomainName_mutualTlsAuthentication,
    createDomainName_regionalCertificateName,
    createDomainName_ownershipVerificationCertificateArn,
    createDomainName_regionalCertificateArn,
    createDomainName_certificateName,
    createDomainName_certificateBody,
    createDomainName_certificateArn,
    createDomainName_certificatePrivateKey,
    createDomainName_certificateChain,
    createDomainName_endpointConfiguration,
    createDomainName_securityPolicy,
    createDomainName_domainName,
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,

    -- ** CreateModel
    createModel_description,
    createModel_schema,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,

    -- ** CreateRequestValidator
    createRequestValidator_validateRequestBody,
    createRequestValidator_name,
    createRequestValidator_validateRequestParameters,
    createRequestValidator_restApiId,
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,

    -- ** CreateResource
    createResource_restApiId,
    createResource_parentId,
    createResource_pathPart,
    resource_pathPart,
    resource_path,
    resource_parentId,
    resource_id,
    resource_resourceMethods,

    -- ** CreateRestApi
    createRestApi_tags,
    createRestApi_policy,
    createRestApi_cloneFrom,
    createRestApi_description,
    createRestApi_binaryMediaTypes,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_endpointConfiguration,
    createRestApi_apiKeySource,
    createRestApi_minimumCompressionSize,
    createRestApi_version,
    createRestApi_name,
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- ** CreateStage
    createStage_tags,
    createStage_cacheClusterEnabled,
    createStage_description,
    createStage_tracingEnabled,
    createStage_cacheClusterSize,
    createStage_canarySettings,
    createStage_documentationVersion,
    createStage_variables,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,

    -- ** CreateUsagePlan
    createUsagePlan_tags,
    createUsagePlan_quota,
    createUsagePlan_description,
    createUsagePlan_throttle,
    createUsagePlan_apiStages,
    createUsagePlan_name,
    usagePlan_tags,
    usagePlan_name,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_apiStages,

    -- ** CreateUsagePlanKey
    createUsagePlanKey_usagePlanId,
    createUsagePlanKey_keyId,
    createUsagePlanKey_keyType,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_id,
    usagePlanKey_value,

    -- ** CreateVpcLink
    createVpcLink_tags,
    createVpcLink_description,
    createVpcLink_name,
    createVpcLink_targetArns,
    vpcLink_tags,
    vpcLink_name,
    vpcLink_status,
    vpcLink_description,
    vpcLink_id,
    vpcLink_targetArns,
    vpcLink_statusMessage,

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
    generateClientCertificate_tags,
    generateClientCertificate_description,
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,

    -- ** GetAccount
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,
    account_apiKeyVersion,

    -- ** GetApiKey
    getApiKey_includeValue,
    getApiKey_apiKey,
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,

    -- ** GetApiKeys
    getApiKeys_customerId,
    getApiKeys_limit,
    getApiKeys_nameQuery,
    getApiKeys_position,
    getApiKeys_includeValues,
    getApiKeysResponse_items,
    getApiKeysResponse_warnings,
    getApiKeysResponse_position,
    getApiKeysResponse_httpStatus,

    -- ** GetAuthorizer
    getAuthorizer_restApiId,
    getAuthorizer_authorizerId,
    authorizer_name,
    authorizer_type,
    authorizer_authorizerCredentials,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_id,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_providerARNs,
    authorizer_authType,

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
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,

    -- ** GetBasePathMappings
    getBasePathMappings_limit,
    getBasePathMappings_position,
    getBasePathMappings_domainName,
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,

    -- ** GetClientCertificate
    getClientCertificate_clientCertificateId,
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,

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
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,

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
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,

    -- ** GetDocumentationParts
    getDocumentationParts_type,
    getDocumentationParts_path,
    getDocumentationParts_limit,
    getDocumentationParts_nameQuery,
    getDocumentationParts_position,
    getDocumentationParts_locationStatus,
    getDocumentationParts_restApiId,
    getDocumentationPartsResponse_items,
    getDocumentationPartsResponse_position,
    getDocumentationPartsResponse_httpStatus,

    -- ** GetDocumentationVersion
    getDocumentationVersion_restApiId,
    getDocumentationVersion_documentationVersion,
    documentationVersion_description,
    documentationVersion_createdDate,
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
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,

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
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
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
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,

    -- ** GetIntegrationResponse
    getIntegrationResponse_restApiId,
    getIntegrationResponse_resourceId,
    getIntegrationResponse_httpMethod,
    getIntegrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_contentHandling,
    integrationResponse_statusCode,

    -- ** GetMethod
    getMethod_restApiId,
    getMethod_resourceId,
    getMethod_httpMethod,
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,

    -- ** GetMethodResponse
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,
    methodResponse_responseParameters,
    methodResponse_statusCode,
    methodResponse_responseModels,

    -- ** GetModel
    getModel_flatten,
    getModel_restApiId,
    getModel_modelName,
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,

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
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,

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
    resource_pathPart,
    resource_path,
    resource_parentId,
    resource_id,
    resource_resourceMethods,

    -- ** GetResources
    getResources_limit,
    getResources_position,
    getResources_embed,
    getResources_restApiId,
    getResourcesResponse_items,
    getResourcesResponse_position,
    getResourcesResponse_httpStatus,

    -- ** GetRestApi
    getRestApi_restApiId,
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

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
    sdkType_description,
    sdkType_id,
    sdkType_friendlyName,
    sdkType_configurationProperties,

    -- ** GetSdkTypes
    getSdkTypes_limit,
    getSdkTypes_position,
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,

    -- ** GetStage
    getStage_restApiId,
    getStage_stageName,
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,

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
    getUsage_limit,
    getUsage_keyId,
    getUsage_position,
    getUsage_usagePlanId,
    getUsage_startDate,
    getUsage_endDate,
    usage_items,
    usage_endDate,
    usage_startDate,
    usage_usagePlanId,
    usage_position,

    -- ** GetUsagePlan
    getUsagePlan_usagePlanId,
    usagePlan_tags,
    usagePlan_name,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_apiStages,

    -- ** GetUsagePlanKey
    getUsagePlanKey_usagePlanId,
    getUsagePlanKey_keyId,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_id,
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
    getUsagePlans_limit,
    getUsagePlans_keyId,
    getUsagePlans_position,
    getUsagePlansResponse_items,
    getUsagePlansResponse_position,
    getUsagePlansResponse_httpStatus,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    vpcLink_tags,
    vpcLink_name,
    vpcLink_status,
    vpcLink_description,
    vpcLink_id,
    vpcLink_targetArns,
    vpcLink_statusMessage,

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
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- ** PutGatewayResponse
    putGatewayResponse_responseParameters,
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,

    -- ** PutIntegration
    putIntegration_cacheKeyParameters,
    putIntegration_requestParameters,
    putIntegration_integrationHttpMethod,
    putIntegration_connectionType,
    putIntegration_tlsConfig,
    putIntegration_cacheNamespace,
    putIntegration_uri,
    putIntegration_connectionId,
    putIntegration_credentials,
    putIntegration_timeoutInMillis,
    putIntegration_contentHandling,
    putIntegration_requestTemplates,
    putIntegration_passthroughBehavior,
    putIntegration_restApiId,
    putIntegration_resourceId,
    putIntegration_httpMethod,
    putIntegration_type,
    integration_cacheKeyParameters,
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,

    -- ** PutIntegrationResponse
    putIntegrationResponse_responseParameters,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_selectionPattern,
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_restApiId,
    putIntegrationResponse_resourceId,
    putIntegrationResponse_httpMethod,
    putIntegrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_contentHandling,
    integrationResponse_statusCode,

    -- ** PutMethod
    putMethod_requestModels,
    putMethod_requestParameters,
    putMethod_apiKeyRequired,
    putMethod_requestValidatorId,
    putMethod_authorizationScopes,
    putMethod_operationName,
    putMethod_authorizerId,
    putMethod_restApiId,
    putMethod_resourceId,
    putMethod_httpMethod,
    putMethod_authorizationType,
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,

    -- ** PutMethodResponse
    putMethodResponse_responseParameters,
    putMethodResponse_responseModels,
    putMethodResponse_restApiId,
    putMethodResponse_resourceId,
    putMethodResponse_httpMethod,
    putMethodResponse_statusCode,
    methodResponse_responseParameters,
    methodResponse_statusCode,
    methodResponse_responseModels,

    -- ** PutRestApi
    putRestApi_failOnWarnings,
    putRestApi_mode,
    putRestApi_parameters,
    putRestApi_restApiId,
    putRestApi_body,
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** TestInvokeMethod
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_headers,
    testInvokeMethod_body,
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_stageVariables,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateAccount
    updateAccount_patchOperations,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,
    account_apiKeyVersion,

    -- ** UpdateApiKey
    updateApiKey_patchOperations,
    updateApiKey_apiKey,
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,

    -- ** UpdateAuthorizer
    updateAuthorizer_patchOperations,
    updateAuthorizer_restApiId,
    updateAuthorizer_authorizerId,
    authorizer_name,
    authorizer_type,
    authorizer_authorizerCredentials,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_id,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_providerARNs,
    authorizer_authType,

    -- ** UpdateBasePathMapping
    updateBasePathMapping_patchOperations,
    updateBasePathMapping_domainName,
    updateBasePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,

    -- ** UpdateClientCertificate
    updateClientCertificate_patchOperations,
    updateClientCertificate_clientCertificateId,
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,

    -- ** UpdateDeployment
    updateDeployment_patchOperations,
    updateDeployment_restApiId,
    updateDeployment_deploymentId,
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,

    -- ** UpdateDocumentationPart
    updateDocumentationPart_patchOperations,
    updateDocumentationPart_restApiId,
    updateDocumentationPart_documentationPartId,
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,

    -- ** UpdateDocumentationVersion
    updateDocumentationVersion_patchOperations,
    updateDocumentationVersion_restApiId,
    updateDocumentationVersion_documentationVersion,
    documentationVersion_description,
    documentationVersion_createdDate,
    documentationVersion_version,

    -- ** UpdateDomainName
    updateDomainName_patchOperations,
    updateDomainName_domainName,
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,

    -- ** UpdateGatewayResponse
    updateGatewayResponse_patchOperations,
    updateGatewayResponse_restApiId,
    updateGatewayResponse_responseType,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,

    -- ** UpdateIntegration
    updateIntegration_patchOperations,
    updateIntegration_restApiId,
    updateIntegration_resourceId,
    updateIntegration_httpMethod,
    integration_cacheKeyParameters,
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_patchOperations,
    updateIntegrationResponse_restApiId,
    updateIntegrationResponse_resourceId,
    updateIntegrationResponse_httpMethod,
    updateIntegrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_contentHandling,
    integrationResponse_statusCode,

    -- ** UpdateMethod
    updateMethod_patchOperations,
    updateMethod_restApiId,
    updateMethod_resourceId,
    updateMethod_httpMethod,
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,

    -- ** UpdateMethodResponse
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,
    methodResponse_responseParameters,
    methodResponse_statusCode,
    methodResponse_responseModels,

    -- ** UpdateModel
    updateModel_patchOperations,
    updateModel_restApiId,
    updateModel_modelName,
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,

    -- ** UpdateRequestValidator
    updateRequestValidator_patchOperations,
    updateRequestValidator_restApiId,
    updateRequestValidator_requestValidatorId,
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,

    -- ** UpdateResource
    updateResource_patchOperations,
    updateResource_restApiId,
    updateResource_resourceId,
    resource_pathPart,
    resource_path,
    resource_parentId,
    resource_id,
    resource_resourceMethods,

    -- ** UpdateRestApi
    updateRestApi_patchOperations,
    updateRestApi_restApiId,
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- ** UpdateStage
    updateStage_patchOperations,
    updateStage_restApiId,
    updateStage_stageName,
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,

    -- ** UpdateUsage
    updateUsage_patchOperations,
    updateUsage_usagePlanId,
    updateUsage_keyId,
    usage_items,
    usage_endDate,
    usage_startDate,
    usage_usagePlanId,
    usage_position,

    -- ** UpdateUsagePlan
    updateUsagePlan_patchOperations,
    updateUsagePlan_usagePlanId,
    usagePlan_tags,
    usagePlan_name,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_apiStages,

    -- ** UpdateVpcLink
    updateVpcLink_patchOperations,
    updateVpcLink_vpcLinkId,
    vpcLink_tags,
    vpcLink_name,
    vpcLink_status,
    vpcLink_description,
    vpcLink_id,
    vpcLink_targetArns,
    vpcLink_statusMessage,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- ** Account
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,
    account_apiKeyVersion,

    -- ** ApiKey
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,

    -- ** ApiStage
    apiStage_apiId,
    apiStage_throttle,
    apiStage_stage,

    -- ** Authorizer
    authorizer_name,
    authorizer_type,
    authorizer_authorizerCredentials,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_id,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_providerARNs,
    authorizer_authType,

    -- ** BasePathMapping
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,

    -- ** CanarySettings
    canarySettings_deploymentId,
    canarySettings_useStageCache,
    canarySettings_stageVariableOverrides,
    canarySettings_percentTraffic,

    -- ** ClientCertificate
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,

    -- ** Deployment
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,

    -- ** DeploymentCanarySettings
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_percentTraffic,

    -- ** DocumentationPart
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,

    -- ** DocumentationPartLocation
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_path,
    documentationPartLocation_statusCode,
    documentationPartLocation_type,

    -- ** DocumentationVersion
    documentationVersion_description,
    documentationVersion_createdDate,
    documentationVersion_version,

    -- ** DomainName
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,

    -- ** EndpointConfiguration
    endpointConfiguration_vpcEndpointIds,
    endpointConfiguration_types,

    -- ** GatewayResponse
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,

    -- ** Integration
    integration_cacheKeyParameters,
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,

    -- ** IntegrationResponse
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_contentHandling,
    integrationResponse_statusCode,

    -- ** Method
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,

    -- ** MethodResponse
    methodResponse_responseParameters,
    methodResponse_statusCode,
    methodResponse_responseModels,

    -- ** MethodSetting
    methodSetting_throttlingRateLimit,
    methodSetting_loggingLevel,
    methodSetting_throttlingBurstLimit,
    methodSetting_metricsEnabled,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_unauthorizedCacheControlHeaderStrategy,
    methodSetting_cacheTtlInSeconds,
    methodSetting_dataTraceEnabled,
    methodSetting_cachingEnabled,
    methodSetting_cacheDataEncrypted,

    -- ** MethodSnapshot
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- ** Model
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreUri,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreVersion,
    mutualTlsAuthenticationInput_truststoreUri,

    -- ** PatchOperation
    patchOperation_from,
    patchOperation_op,
    patchOperation_path,
    patchOperation_value,

    -- ** QuotaSettings
    quotaSettings_offset,
    quotaSettings_period,
    quotaSettings_limit,

    -- ** RequestValidator
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,

    -- ** Resource
    resource_pathPart,
    resource_path,
    resource_parentId,
    resource_id,
    resource_resourceMethods,

    -- ** RestApi
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- ** SdkConfigurationProperty
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_friendlyName,

    -- ** SdkType
    sdkType_description,
    sdkType_id,
    sdkType_friendlyName,
    sdkType_configurationProperties,

    -- ** Stage
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,

    -- ** StageKey
    stageKey_stageName,
    stageKey_restApiId,

    -- ** ThrottleSettings
    throttleSettings_rateLimit,
    throttleSettings_burstLimit,

    -- ** TlsConfig
    tlsConfig_insecureSkipVerification,

    -- ** Usage
    usage_items,
    usage_endDate,
    usage_startDate,
    usage_usagePlanId,
    usage_position,

    -- ** UsagePlan
    usagePlan_tags,
    usagePlan_name,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_apiStages,

    -- ** UsagePlanKey
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_id,
    usagePlanKey_value,

    -- ** VpcLink
    vpcLink_tags,
    vpcLink_name,
    vpcLink_status,
    vpcLink_description,
    vpcLink_id,
    vpcLink_targetArns,
    vpcLink_statusMessage,
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
