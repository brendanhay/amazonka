{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Lens
  ( -- * Operations

    -- ** GetResource
    getResource_embed,
    getResource_restApiId,
    getResource_resourceId,
    resource_pathPart,
    resource_path,
    resource_id,
    resource_resourceMethods,
    resource_parentId,

    -- ** GetDeployments
    getDeployments_limit,
    getDeployments_position,
    getDeployments_restApiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_position,
    getDeploymentsResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_embed,
    getDeployment_restApiId,
    getDeployment_deploymentId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_id,
    deployment_description,

    -- ** GetTags
    getTags_limit,
    getTags_position,
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** DeleteGatewayResponse
    deleteGatewayResponse_restApiId,
    deleteGatewayResponse_responseType,

    -- ** UpdateGatewayResponse
    updateGatewayResponse_patchOperations,
    updateGatewayResponse_restApiId,
    updateGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,

    -- ** CreateUsagePlan
    createUsagePlan_apiStages,
    createUsagePlan_throttle,
    createUsagePlan_quota,
    createUsagePlan_description,
    createUsagePlan_tags,
    createUsagePlan_name,
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,

    -- ** GetDomainNames
    getDomainNames_limit,
    getDomainNames_position,
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,

    -- ** GetClientCertificate
    getClientCertificate_clientCertificateId,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_description,
    clientCertificate_tags,

    -- ** PutGatewayResponse
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_responseParameters,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,

    -- ** GetSdkType
    getSdkType_id,
    sdkType_friendlyName,
    sdkType_configurationProperties,
    sdkType_id,
    sdkType_description,

    -- ** GetMethodResponse
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** GetModels
    getModels_limit,
    getModels_position,
    getModels_restApiId,
    getModelsResponse_items,
    getModelsResponse_position,
    getModelsResponse_httpStatus,

    -- ** GetBasePathMapping
    getBasePathMapping_domainName,
    getBasePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_basePath,
    basePathMapping_restApiId,

    -- ** GetRequestValidators
    getRequestValidators_limit,
    getRequestValidators_position,
    getRequestValidators_restApiId,
    getRequestValidatorsResponse_items,
    getRequestValidatorsResponse_position,
    getRequestValidatorsResponse_httpStatus,

    -- ** PutMethodResponse
    putMethodResponse_responseModels,
    putMethodResponse_responseParameters,
    putMethodResponse_restApiId,
    putMethodResponse_resourceId,
    putMethodResponse_httpMethod,
    putMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** ImportRestApi
    importRestApi_failOnWarnings,
    importRestApi_parameters,
    importRestApi_body,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** DeleteMethodResponse
    deleteMethodResponse_restApiId,
    deleteMethodResponse_resourceId,
    deleteMethodResponse_httpMethod,
    deleteMethodResponse_statusCode,

    -- ** UpdateMethodResponse
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** DeleteStage
    deleteStage_restApiId,
    deleteStage_stageName,

    -- ** UpdateStage
    updateStage_patchOperations,
    updateStage_restApiId,
    updateStage_stageName,
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,

    -- ** GetRestApis
    getRestApis_limit,
    getRestApis_position,
    getRestApisResponse_items,
    getRestApisResponse_position,
    getRestApisResponse_httpStatus,

    -- ** GetDocumentationVersions
    getDocumentationVersions_limit,
    getDocumentationVersions_position,
    getDocumentationVersions_restApiId,
    getDocumentationVersionsResponse_items,
    getDocumentationVersionsResponse_position,
    getDocumentationVersionsResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_stageDescription,
    createDeployment_variables,
    createDeployment_tracingEnabled,
    createDeployment_cacheClusterSize,
    createDeployment_canarySettings,
    createDeployment_cacheClusterEnabled,
    createDeployment_stageName,
    createDeployment_description,
    createDeployment_restApiId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_id,
    deployment_description,

    -- ** GetVpcLinks
    getVpcLinks_limit,
    getVpcLinks_position,
    getVpcLinksResponse_items,
    getVpcLinksResponse_position,
    getVpcLinksResponse_httpStatus,

    -- ** CreateBasePathMapping
    createBasePathMapping_stage,
    createBasePathMapping_basePath,
    createBasePathMapping_domainName,
    createBasePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,
    basePathMapping_restApiId,

    -- ** GetIntegration
    getIntegration_restApiId,
    getIntegration_resourceId,
    getIntegration_httpMethod,
    integration_httpMethod,
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,

    -- ** GetDocumentationParts
    getDocumentationParts_path,
    getDocumentationParts_locationStatus,
    getDocumentationParts_nameQuery,
    getDocumentationParts_limit,
    getDocumentationParts_type,
    getDocumentationParts_position,
    getDocumentationParts_restApiId,
    getDocumentationPartsResponse_items,
    getDocumentationPartsResponse_position,
    getDocumentationPartsResponse_httpStatus,

    -- ** UpdateAccount
    updateAccount_patchOperations,
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** GetUsagePlan
    getUsagePlan_usagePlanId,
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,

    -- ** DeleteDeployment
    deleteDeployment_restApiId,
    deleteDeployment_deploymentId,

    -- ** UpdateDeployment
    updateDeployment_patchOperations,
    updateDeployment_restApiId,
    updateDeployment_deploymentId,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_id,
    deployment_description,

    -- ** GetDocumentationPart
    getDocumentationPart_restApiId,
    getDocumentationPart_documentationPartId,
    documentationPart_location,
    documentationPart_id,
    documentationPart_properties,

    -- ** DeleteResource
    deleteResource_restApiId,
    deleteResource_resourceId,

    -- ** UpdateResource
    updateResource_patchOperations,
    updateResource_restApiId,
    updateResource_resourceId,
    resource_pathPart,
    resource_path,
    resource_id,
    resource_resourceMethods,
    resource_parentId,

    -- ** CreateRequestValidator
    createRequestValidator_validateRequestParameters,
    createRequestValidator_name,
    createRequestValidator_validateRequestBody,
    createRequestValidator_restApiId,
    requestValidator_validateRequestParameters,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_id,

    -- ** ImportDocumentationParts
    importDocumentationParts_mode,
    importDocumentationParts_failOnWarnings,
    importDocumentationParts_restApiId,
    importDocumentationParts_body,
    importDocumentationPartsResponse_ids,
    importDocumentationPartsResponse_warnings,
    importDocumentationPartsResponse_httpStatus,

    -- ** GetUsage
    getUsage_keyId,
    getUsage_limit,
    getUsage_position,
    getUsage_usagePlanId,
    getUsage_startDate,
    getUsage_endDate,
    usage_usagePlanId,
    usage_endDate,
    usage_items,
    usage_startDate,
    usage_position,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,

    -- ** CreateModel
    createModel_schema,
    createModel_description,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,
    model_schema,
    model_name,
    model_id,
    model_description,
    model_contentType,

    -- ** GetIntegrationResponse
    getIntegrationResponse_restApiId,
    getIntegrationResponse_resourceId,
    getIntegrationResponse_httpMethod,
    getIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,

    -- ** CreateDomainName
    createDomainName_certificateName,
    createDomainName_ownershipVerificationCertificateArn,
    createDomainName_regionalCertificateArn,
    createDomainName_certificateArn,
    createDomainName_securityPolicy,
    createDomainName_mutualTlsAuthentication,
    createDomainName_certificatePrivateKey,
    createDomainName_regionalCertificateName,
    createDomainName_certificateBody,
    createDomainName_certificateChain,
    createDomainName_endpointConfiguration,
    createDomainName_tags,
    createDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,

    -- ** FlushStageAuthorizersCache
    flushStageAuthorizersCache_restApiId,
    flushStageAuthorizersCache_stageName,

    -- ** GetGatewayResponses
    getGatewayResponses_limit,
    getGatewayResponses_position,
    getGatewayResponses_restApiId,
    getGatewayResponsesResponse_items,
    getGatewayResponsesResponse_position,
    getGatewayResponsesResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_restApiId,
    deleteModel_modelName,

    -- ** UpdateModel
    updateModel_patchOperations,
    updateModel_restApiId,
    updateModel_modelName,
    model_schema,
    model_name,
    model_id,
    model_description,
    model_contentType,

    -- ** GetDocumentationVersion
    getDocumentationVersion_restApiId,
    getDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** DeleteApiKey
    deleteApiKey_apiKey,

    -- ** UpdateApiKey
    updateApiKey_patchOperations,
    updateApiKey_apiKey,
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,

    -- ** GetRestApi
    getRestApi_restApiId,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** GetStages
    getStages_deploymentId,
    getStages_restApiId,
    getStagesResponse_item,
    getStagesResponse_httpStatus,

    -- ** PutRestApi
    putRestApi_mode,
    putRestApi_failOnWarnings,
    putRestApi_parameters,
    putRestApi_restApiId,
    putRestApi_body,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** GetMethod
    getMethod_restApiId,
    getMethod_resourceId,
    getMethod_httpMethod,
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,

    -- ** GetModel
    getModel_flatten,
    getModel_restApiId,
    getModel_modelName,
    model_schema,
    model_name,
    model_id,
    model_description,
    model_contentType,

    -- ** UpdateRestApi
    updateRestApi_patchOperations,
    updateRestApi_restApiId,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** DeleteRestApi
    deleteRestApi_restApiId,

    -- ** ImportApiKeys
    importApiKeys_failOnWarnings,
    importApiKeys_body,
    importApiKeys_format,
    importApiKeysResponse_ids,
    importApiKeysResponse_warnings,
    importApiKeysResponse_httpStatus,

    -- ** CreateDocumentationPart
    createDocumentationPart_restApiId,
    createDocumentationPart_location,
    createDocumentationPart_properties,
    documentationPart_location,
    documentationPart_id,
    documentationPart_properties,

    -- ** TestInvokeMethod
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_body,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_stageVariables,
    testInvokeMethod_headers,
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_httpStatus,

    -- ** GetRequestValidator
    getRequestValidator_restApiId,
    getRequestValidator_requestValidatorId,
    requestValidator_validateRequestParameters,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_id,

    -- ** GetDomainName
    getDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,

    -- ** CreateVpcLink
    createVpcLink_description,
    createVpcLink_tags,
    createVpcLink_name,
    createVpcLink_targetArns,
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,

    -- ** DeleteDocumentationPart
    deleteDocumentationPart_restApiId,
    deleteDocumentationPart_documentationPartId,

    -- ** UpdateDocumentationPart
    updateDocumentationPart_patchOperations,
    updateDocumentationPart_restApiId,
    updateDocumentationPart_documentationPartId,
    documentationPart_location,
    documentationPart_id,
    documentationPart_properties,

    -- ** GetAuthorizers
    getAuthorizers_limit,
    getAuthorizers_position,
    getAuthorizers_restApiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_position,
    getAuthorizersResponse_httpStatus,

    -- ** CreateDocumentationVersion
    createDocumentationVersion_stageName,
    createDocumentationVersion_description,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** PutIntegrationResponse
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_selectionPattern,
    putIntegrationResponse_responseParameters,
    putIntegrationResponse_restApiId,
    putIntegrationResponse_resourceId,
    putIntegrationResponse_httpMethod,
    putIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,

    -- ** GetUsagePlanKeys
    getUsagePlanKeys_nameQuery,
    getUsagePlanKeys_limit,
    getUsagePlanKeys_position,
    getUsagePlanKeys_usagePlanId,
    getUsagePlanKeysResponse_items,
    getUsagePlanKeysResponse_position,
    getUsagePlanKeysResponse_httpStatus,

    -- ** DeleteVpcLink
    deleteVpcLink_vpcLinkId,

    -- ** UpdateVpcLink
    updateVpcLink_patchOperations,
    updateVpcLink_vpcLinkId,
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,

    -- ** FlushStageCache
    flushStageCache_restApiId,
    flushStageCache_stageName,

    -- ** CreateRestApi
    createRestApi_minimumCompressionSize,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_binaryMediaTypes,
    createRestApi_version,
    createRestApi_apiKeySource,
    createRestApi_cloneFrom,
    createRestApi_policy,
    createRestApi_endpointConfiguration,
    createRestApi_description,
    createRestApi_tags,
    createRestApi_name,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** DeleteIntegrationResponse
    deleteIntegrationResponse_restApiId,
    deleteIntegrationResponse_resourceId,
    deleteIntegrationResponse_httpMethod,
    deleteIntegrationResponse_statusCode,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_patchOperations,
    updateIntegrationResponse_restApiId,
    updateIntegrationResponse_resourceId,
    updateIntegrationResponse_httpMethod,
    updateIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,

    -- ** UpdateUsage
    updateUsage_patchOperations,
    updateUsage_usagePlanId,
    updateUsage_keyId,
    usage_usagePlanId,
    usage_endDate,
    usage_items,
    usage_startDate,
    usage_position,

    -- ** DeleteIntegration
    deleteIntegration_restApiId,
    deleteIntegration_resourceId,
    deleteIntegration_httpMethod,

    -- ** UpdateIntegration
    updateIntegration_patchOperations,
    updateIntegration_restApiId,
    updateIntegration_resourceId,
    updateIntegration_httpMethod,
    integration_httpMethod,
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** GenerateClientCertificate
    generateClientCertificate_description,
    generateClientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_description,
    clientCertificate_tags,

    -- ** GetResources
    getResources_embed,
    getResources_limit,
    getResources_position,
    getResources_restApiId,
    getResourcesResponse_items,
    getResourcesResponse_position,
    getResourcesResponse_httpStatus,

    -- ** GetUsagePlanKey
    getUsagePlanKey_usagePlanId,
    getUsagePlanKey_keyId,
    usagePlanKey_value,
    usagePlanKey_name,
    usagePlanKey_id,
    usagePlanKey_type,

    -- ** GetAccount
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** PutIntegration
    putIntegration_requestTemplates,
    putIntegration_credentials,
    putIntegration_connectionId,
    putIntegration_requestParameters,
    putIntegration_contentHandling,
    putIntegration_passthroughBehavior,
    putIntegration_uri,
    putIntegration_tlsConfig,
    putIntegration_cacheNamespace,
    putIntegration_timeoutInMillis,
    putIntegration_connectionType,
    putIntegration_integrationHttpMethod,
    putIntegration_cacheKeyParameters,
    putIntegration_restApiId,
    putIntegration_resourceId,
    putIntegration_httpMethod,
    putIntegration_type,
    integration_httpMethod,
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,

    -- ** GetAuthorizer
    getAuthorizer_restApiId,
    getAuthorizer_authorizerId,
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,

    -- ** DeleteUsagePlan
    deleteUsagePlan_usagePlanId,

    -- ** UpdateUsagePlan
    updateUsagePlan_patchOperations,
    updateUsagePlan_usagePlanId,
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,

    -- ** GetStage
    getStage_restApiId,
    getStage_stageName,
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,

    -- ** GetExport
    getExport_parameters,
    getExport_accepts,
    getExport_restApiId,
    getExport_stageName,
    getExport_exportType,
    getExportResponse_body,
    getExportResponse_contentDisposition,
    getExportResponse_contentType,
    getExportResponse_httpStatus,

    -- ** GetSdk
    getSdk_parameters,
    getSdk_restApiId,
    getSdk_stageName,
    getSdk_sdkType,
    getSdkResponse_body,
    getSdkResponse_contentDisposition,
    getSdkResponse_contentType,
    getSdkResponse_httpStatus,

    -- ** GetApiKeys
    getApiKeys_includeValues,
    getApiKeys_customerId,
    getApiKeys_nameQuery,
    getApiKeys_limit,
    getApiKeys_position,
    getApiKeysResponse_items,
    getApiKeysResponse_warnings,
    getApiKeysResponse_position,
    getApiKeysResponse_httpStatus,

    -- ** DeleteBasePathMapping
    deleteBasePathMapping_domainName,
    deleteBasePathMapping_basePath,

    -- ** UpdateBasePathMapping
    updateBasePathMapping_patchOperations,
    updateBasePathMapping_domainName,
    updateBasePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_basePath,
    basePathMapping_restApiId,

    -- ** DeleteClientCertificate
    deleteClientCertificate_clientCertificateId,

    -- ** UpdateClientCertificate
    updateClientCertificate_patchOperations,
    updateClientCertificate_clientCertificateId,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_description,
    clientCertificate_tags,

    -- ** GetGatewayResponse
    getGatewayResponse_restApiId,
    getGatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,

    -- ** CreateUsagePlanKey
    createUsagePlanKey_usagePlanId,
    createUsagePlanKey_keyId,
    createUsagePlanKey_keyType,
    usagePlanKey_value,
    usagePlanKey_name,
    usagePlanKey_id,
    usagePlanKey_type,

    -- ** CreateAuthorizer
    createAuthorizer_authorizerUri,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_providerARNs,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_authType,
    createAuthorizer_identitySource,
    createAuthorizer_authorizerCredentials,
    createAuthorizer_restApiId,
    createAuthorizer_name,
    createAuthorizer_type,
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,

    -- ** UpdateAuthorizer
    updateAuthorizer_patchOperations,
    updateAuthorizer_restApiId,
    updateAuthorizer_authorizerId,
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,

    -- ** DeleteAuthorizer
    deleteAuthorizer_restApiId,
    deleteAuthorizer_authorizerId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** CreateStage
    createStage_variables,
    createStage_documentationVersion,
    createStage_tracingEnabled,
    createStage_cacheClusterSize,
    createStage_canarySettings,
    createStage_cacheClusterEnabled,
    createStage_description,
    createStage_tags,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,

    -- ** DeleteUsagePlanKey
    deleteUsagePlanKey_usagePlanId,
    deleteUsagePlanKey_keyId,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** CreateApiKey
    createApiKey_enabled,
    createApiKey_value,
    createApiKey_customerId,
    createApiKey_generateDistinctId,
    createApiKey_name,
    createApiKey_stageKeys,
    createApiKey_description,
    createApiKey_tags,
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,

    -- ** GetUsagePlans
    getUsagePlans_keyId,
    getUsagePlans_limit,
    getUsagePlans_position,
    getUsagePlansResponse_items,
    getUsagePlansResponse_position,
    getUsagePlansResponse_httpStatus,

    -- ** PutMethod
    putMethod_authorizationScopes,
    putMethod_requestValidatorId,
    putMethod_requestModels,
    putMethod_requestParameters,
    putMethod_authorizerId,
    putMethod_operationName,
    putMethod_apiKeyRequired,
    putMethod_restApiId,
    putMethod_resourceId,
    putMethod_httpMethod,
    putMethod_authorizationType,
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,

    -- ** UpdateDomainName
    updateDomainName_patchOperations,
    updateDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,

    -- ** DeleteDomainName
    deleteDomainName_domainName,

    -- ** CreateResource
    createResource_restApiId,
    createResource_parentId,
    createResource_pathPart,
    resource_pathPart,
    resource_path,
    resource_id,
    resource_resourceMethods,
    resource_parentId,

    -- ** DeleteMethod
    deleteMethod_restApiId,
    deleteMethod_resourceId,
    deleteMethod_httpMethod,

    -- ** UpdateMethod
    updateMethod_patchOperations,
    updateMethod_restApiId,
    updateMethod_resourceId,
    updateMethod_httpMethod,
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,

    -- ** UpdateRequestValidator
    updateRequestValidator_patchOperations,
    updateRequestValidator_restApiId,
    updateRequestValidator_requestValidatorId,
    requestValidator_validateRequestParameters,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_id,

    -- ** DeleteRequestValidator
    deleteRequestValidator_restApiId,
    deleteRequestValidator_requestValidatorId,

    -- ** GetSdkTypes
    getSdkTypes_limit,
    getSdkTypes_position,
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,

    -- ** GetClientCertificates
    getClientCertificates_limit,
    getClientCertificates_position,
    getClientCertificatesResponse_items,
    getClientCertificatesResponse_position,
    getClientCertificatesResponse_httpStatus,

    -- ** GetModelTemplate
    getModelTemplate_restApiId,
    getModelTemplate_modelName,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- ** UpdateDocumentationVersion
    updateDocumentationVersion_patchOperations,
    updateDocumentationVersion_restApiId,
    updateDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** DeleteDocumentationVersion
    deleteDocumentationVersion_restApiId,
    deleteDocumentationVersion_documentationVersion,

    -- ** GetBasePathMappings
    getBasePathMappings_limit,
    getBasePathMappings_position,
    getBasePathMappings_domainName,
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,

    -- ** GetApiKey
    getApiKey_includeValue,
    getApiKey_apiKey,
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- ** Account
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- ** ApiKey
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,

    -- ** ApiStage
    apiStage_stage,
    apiStage_apiId,
    apiStage_throttle,

    -- ** Authorizer
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,

    -- ** BasePathMapping
    basePathMapping_stage,
    basePathMapping_basePath,
    basePathMapping_restApiId,

    -- ** CanarySettings
    canarySettings_deploymentId,
    canarySettings_stageVariableOverrides,
    canarySettings_useStageCache,
    canarySettings_percentTraffic,

    -- ** ClientCertificate
    clientCertificate_pemEncodedCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_description,
    clientCertificate_tags,

    -- ** Deployment
    deployment_apiSummary,
    deployment_createdDate,
    deployment_id,
    deployment_description,

    -- ** DeploymentCanarySettings
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_percentTraffic,

    -- ** DocumentationPart
    documentationPart_location,
    documentationPart_id,
    documentationPart_properties,

    -- ** DocumentationPartLocation
    documentationPartLocation_path,
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_statusCode,
    documentationPartLocation_type,

    -- ** DocumentationVersion
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** DomainName
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,

    -- ** EndpointConfiguration
    endpointConfiguration_types,
    endpointConfiguration_vpcEndpointIds,

    -- ** GatewayResponse
    gatewayResponse_defaultResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,

    -- ** Integration
    integration_httpMethod,
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,

    -- ** IntegrationResponse
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,

    -- ** Method
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,

    -- ** MethodResponse
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** MethodSetting
    methodSetting_cacheTtlInSeconds,
    methodSetting_dataTraceEnabled,
    methodSetting_throttlingBurstLimit,
    methodSetting_cacheDataEncrypted,
    methodSetting_loggingLevel,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_cachingEnabled,
    methodSetting_metricsEnabled,
    methodSetting_throttlingRateLimit,
    methodSetting_unauthorizedCacheControlHeaderStrategy,

    -- ** MethodSnapshot
    methodSnapshot_authorizationType,
    methodSnapshot_apiKeyRequired,

    -- ** Model
    model_schema,
    model_name,
    model_id,
    model_description,
    model_contentType,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

    -- ** PatchOperation
    patchOperation_op,
    patchOperation_path,
    patchOperation_value,
    patchOperation_from,

    -- ** QuotaSettings
    quotaSettings_offset,
    quotaSettings_period,
    quotaSettings_limit,

    -- ** RequestValidator
    requestValidator_validateRequestParameters,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_id,

    -- ** Resource
    resource_pathPart,
    resource_path,
    resource_id,
    resource_resourceMethods,
    resource_parentId,

    -- ** RestApi
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- ** SdkConfigurationProperty
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,

    -- ** SdkType
    sdkType_friendlyName,
    sdkType_configurationProperties,
    sdkType_id,
    sdkType_description,

    -- ** Stage
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,

    -- ** StageKey
    stageKey_restApiId,
    stageKey_stageName,

    -- ** ThrottleSettings
    throttleSettings_burstLimit,
    throttleSettings_rateLimit,

    -- ** TlsConfig
    tlsConfig_insecureSkipVerification,

    -- ** Usage
    usage_usagePlanId,
    usage_endDate,
    usage_items,
    usage_startDate,
    usage_position,

    -- ** UsagePlan
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,

    -- ** UsagePlanKey
    usagePlanKey_value,
    usagePlanKey_name,
    usagePlanKey_id,
    usagePlanKey_type,

    -- ** VpcLink
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,
  )
where

import Network.AWS.APIGateway.CreateApiKey
import Network.AWS.APIGateway.CreateAuthorizer
import Network.AWS.APIGateway.CreateBasePathMapping
import Network.AWS.APIGateway.CreateDeployment
import Network.AWS.APIGateway.CreateDocumentationPart
import Network.AWS.APIGateway.CreateDocumentationVersion
import Network.AWS.APIGateway.CreateDomainName
import Network.AWS.APIGateway.CreateModel
import Network.AWS.APIGateway.CreateRequestValidator
import Network.AWS.APIGateway.CreateResource
import Network.AWS.APIGateway.CreateRestApi
import Network.AWS.APIGateway.CreateStage
import Network.AWS.APIGateway.CreateUsagePlan
import Network.AWS.APIGateway.CreateUsagePlanKey
import Network.AWS.APIGateway.CreateVpcLink
import Network.AWS.APIGateway.DeleteApiKey
import Network.AWS.APIGateway.DeleteAuthorizer
import Network.AWS.APIGateway.DeleteBasePathMapping
import Network.AWS.APIGateway.DeleteClientCertificate
import Network.AWS.APIGateway.DeleteDeployment
import Network.AWS.APIGateway.DeleteDocumentationPart
import Network.AWS.APIGateway.DeleteDocumentationVersion
import Network.AWS.APIGateway.DeleteDomainName
import Network.AWS.APIGateway.DeleteGatewayResponse
import Network.AWS.APIGateway.DeleteIntegration
import Network.AWS.APIGateway.DeleteIntegrationResponse
import Network.AWS.APIGateway.DeleteMethod
import Network.AWS.APIGateway.DeleteMethodResponse
import Network.AWS.APIGateway.DeleteModel
import Network.AWS.APIGateway.DeleteRequestValidator
import Network.AWS.APIGateway.DeleteResource
import Network.AWS.APIGateway.DeleteRestApi
import Network.AWS.APIGateway.DeleteStage
import Network.AWS.APIGateway.DeleteUsagePlan
import Network.AWS.APIGateway.DeleteUsagePlanKey
import Network.AWS.APIGateway.DeleteVpcLink
import Network.AWS.APIGateway.FlushStageAuthorizersCache
import Network.AWS.APIGateway.FlushStageCache
import Network.AWS.APIGateway.GenerateClientCertificate
import Network.AWS.APIGateway.GetAccount
import Network.AWS.APIGateway.GetApiKey
import Network.AWS.APIGateway.GetApiKeys
import Network.AWS.APIGateway.GetAuthorizer
import Network.AWS.APIGateway.GetAuthorizers
import Network.AWS.APIGateway.GetBasePathMapping
import Network.AWS.APIGateway.GetBasePathMappings
import Network.AWS.APIGateway.GetClientCertificate
import Network.AWS.APIGateway.GetClientCertificates
import Network.AWS.APIGateway.GetDeployment
import Network.AWS.APIGateway.GetDeployments
import Network.AWS.APIGateway.GetDocumentationPart
import Network.AWS.APIGateway.GetDocumentationParts
import Network.AWS.APIGateway.GetDocumentationVersion
import Network.AWS.APIGateway.GetDocumentationVersions
import Network.AWS.APIGateway.GetDomainName
import Network.AWS.APIGateway.GetDomainNames
import Network.AWS.APIGateway.GetExport
import Network.AWS.APIGateway.GetGatewayResponse
import Network.AWS.APIGateway.GetGatewayResponses
import Network.AWS.APIGateway.GetIntegration
import Network.AWS.APIGateway.GetIntegrationResponse
import Network.AWS.APIGateway.GetMethod
import Network.AWS.APIGateway.GetMethodResponse
import Network.AWS.APIGateway.GetModel
import Network.AWS.APIGateway.GetModelTemplate
import Network.AWS.APIGateway.GetModels
import Network.AWS.APIGateway.GetRequestValidator
import Network.AWS.APIGateway.GetRequestValidators
import Network.AWS.APIGateway.GetResource
import Network.AWS.APIGateway.GetResources
import Network.AWS.APIGateway.GetRestApi
import Network.AWS.APIGateway.GetRestApis
import Network.AWS.APIGateway.GetSdk
import Network.AWS.APIGateway.GetSdkType
import Network.AWS.APIGateway.GetSdkTypes
import Network.AWS.APIGateway.GetStage
import Network.AWS.APIGateway.GetStages
import Network.AWS.APIGateway.GetTags
import Network.AWS.APIGateway.GetUsage
import Network.AWS.APIGateway.GetUsagePlan
import Network.AWS.APIGateway.GetUsagePlanKey
import Network.AWS.APIGateway.GetUsagePlanKeys
import Network.AWS.APIGateway.GetUsagePlans
import Network.AWS.APIGateway.GetVpcLink
import Network.AWS.APIGateway.GetVpcLinks
import Network.AWS.APIGateway.ImportApiKeys
import Network.AWS.APIGateway.ImportDocumentationParts
import Network.AWS.APIGateway.ImportRestApi
import Network.AWS.APIGateway.PutGatewayResponse
import Network.AWS.APIGateway.PutIntegration
import Network.AWS.APIGateway.PutIntegrationResponse
import Network.AWS.APIGateway.PutMethod
import Network.AWS.APIGateway.PutMethodResponse
import Network.AWS.APIGateway.PutRestApi
import Network.AWS.APIGateway.TagResource
import Network.AWS.APIGateway.TestInvokeAuthorizer
import Network.AWS.APIGateway.TestInvokeMethod
import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.Account
import Network.AWS.APIGateway.Types.ApiKey
import Network.AWS.APIGateway.Types.ApiStage
import Network.AWS.APIGateway.Types.Authorizer
import Network.AWS.APIGateway.Types.BasePathMapping
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.ClientCertificate
import Network.AWS.APIGateway.Types.Deployment
import Network.AWS.APIGateway.Types.DeploymentCanarySettings
import Network.AWS.APIGateway.Types.DocumentationPart
import Network.AWS.APIGateway.Types.DocumentationPartLocation
import Network.AWS.APIGateway.Types.DocumentationVersion
import Network.AWS.APIGateway.Types.DomainName
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.APIGateway.Types.GatewayResponse
import Network.AWS.APIGateway.Types.Integration
import Network.AWS.APIGateway.Types.IntegrationResponse
import Network.AWS.APIGateway.Types.Method
import Network.AWS.APIGateway.Types.MethodResponse
import Network.AWS.APIGateway.Types.MethodSetting
import Network.AWS.APIGateway.Types.MethodSnapshot
import Network.AWS.APIGateway.Types.Model
import Network.AWS.APIGateway.Types.MutualTlsAuthentication
import Network.AWS.APIGateway.Types.MutualTlsAuthenticationInput
import Network.AWS.APIGateway.Types.PatchOperation
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.RequestValidator
import Network.AWS.APIGateway.Types.Resource
import Network.AWS.APIGateway.Types.RestApi
import Network.AWS.APIGateway.Types.SdkConfigurationProperty
import Network.AWS.APIGateway.Types.SdkType
import Network.AWS.APIGateway.Types.Stage
import Network.AWS.APIGateway.Types.StageKey
import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.APIGateway.Types.TlsConfig
import Network.AWS.APIGateway.Types.Usage
import Network.AWS.APIGateway.Types.UsagePlan
import Network.AWS.APIGateway.Types.UsagePlanKey
import Network.AWS.APIGateway.Types.VpcLink
import Network.AWS.APIGateway.UntagResource
import Network.AWS.APIGateway.UpdateAccount
import Network.AWS.APIGateway.UpdateApiKey
import Network.AWS.APIGateway.UpdateAuthorizer
import Network.AWS.APIGateway.UpdateBasePathMapping
import Network.AWS.APIGateway.UpdateClientCertificate
import Network.AWS.APIGateway.UpdateDeployment
import Network.AWS.APIGateway.UpdateDocumentationPart
import Network.AWS.APIGateway.UpdateDocumentationVersion
import Network.AWS.APIGateway.UpdateDomainName
import Network.AWS.APIGateway.UpdateGatewayResponse
import Network.AWS.APIGateway.UpdateIntegration
import Network.AWS.APIGateway.UpdateIntegrationResponse
import Network.AWS.APIGateway.UpdateMethod
import Network.AWS.APIGateway.UpdateMethodResponse
import Network.AWS.APIGateway.UpdateModel
import Network.AWS.APIGateway.UpdateRequestValidator
import Network.AWS.APIGateway.UpdateResource
import Network.AWS.APIGateway.UpdateRestApi
import Network.AWS.APIGateway.UpdateStage
import Network.AWS.APIGateway.UpdateUsage
import Network.AWS.APIGateway.UpdateUsagePlan
import Network.AWS.APIGateway.UpdateVpcLink
