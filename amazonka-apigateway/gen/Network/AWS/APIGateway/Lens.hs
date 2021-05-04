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

    -- ** GenerateClientCertificate
    generateClientCertificate_tags,
    generateClientCertificate_description,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,

    -- ** UpdateIntegration
    updateIntegration_patchOperations,
    updateIntegration_restApiId,
    updateIntegration_resourceId,
    updateIntegration_httpMethod,
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,

    -- ** DeleteUsagePlan
    deleteUsagePlan_usagePlanId,

    -- ** DeleteIntegration
    deleteIntegration_restApiId,
    deleteIntegration_resourceId,
    deleteIntegration_httpMethod,

    -- ** GetUsagePlanKey
    getUsagePlanKey_usagePlanId,
    getUsagePlanKey_keyId,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_value,
    usagePlanKey_type,

    -- ** GetAuthorizer
    getAuthorizer_restApiId,
    getAuthorizer_authorizerId,
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,

    -- ** UpdateUsagePlan
    updateUsagePlan_patchOperations,
    updateUsagePlan_usagePlanId,
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,

    -- ** GetDeployments
    getDeployments_position,
    getDeployments_limit,
    getDeployments_restApiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_position,
    getDeploymentsResponse_httpStatus,

    -- ** DeleteIntegrationResponse
    deleteIntegrationResponse_restApiId,
    deleteIntegrationResponse_resourceId,
    deleteIntegrationResponse_httpMethod,
    deleteIntegrationResponse_statusCode,

    -- ** FlushStageCache
    flushStageCache_restApiId,
    flushStageCache_stageName,

    -- ** DeleteVpcLink
    deleteVpcLink_vpcLinkId,

    -- ** DeleteDocumentationPart
    deleteDocumentationPart_restApiId,
    deleteDocumentationPart_documentationPartId,

    -- ** UpdateVpcLink
    updateVpcLink_patchOperations,
    updateVpcLink_vpcLinkId,
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,

    -- ** CreateRestApi
    createRestApi_endpointConfiguration,
    createRestApi_binaryMediaTypes,
    createRestApi_version,
    createRestApi_tags,
    createRestApi_description,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_policy,
    createRestApi_cloneFrom,
    createRestApi_minimumCompressionSize,
    createRestApi_apiKeySource,
    createRestApi_name,
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

    -- ** UpdateUsage
    updateUsage_patchOperations,
    updateUsage_usagePlanId,
    updateUsage_keyId,
    usage_startDate,
    usage_items,
    usage_position,
    usage_usagePlanId,
    usage_endDate,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_patchOperations,
    updateIntegrationResponse_restApiId,
    updateIntegrationResponse_resourceId,
    updateIntegrationResponse_httpMethod,
    updateIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,

    -- ** UpdateDocumentationPart
    updateDocumentationPart_patchOperations,
    updateDocumentationPart_restApiId,
    updateDocumentationPart_documentationPartId,
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,

    -- ** GetSdkTypes
    getSdkTypes_position,
    getSdkTypes_limit,
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,

    -- ** GetBasePathMappings
    getBasePathMappings_position,
    getBasePathMappings_limit,
    getBasePathMappings_domainName,
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,

    -- ** GetModel
    getModel_flatten,
    getModel_restApiId,
    getModel_modelName,
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,

    -- ** GetClientCertificates
    getClientCertificates_position,
    getClientCertificates_limit,
    getClientCertificatesResponse_items,
    getClientCertificatesResponse_position,
    getClientCertificatesResponse_httpStatus,

    -- ** TestInvokeMethod
    testInvokeMethod_headers,
    testInvokeMethod_stageVariables,
    testInvokeMethod_body,
    testInvokeMethod_clientCertificateId,
    testInvokeMethod_multiValueHeaders,
    testInvokeMethod_pathWithQueryString,
    testInvokeMethod_restApiId,
    testInvokeMethod_resourceId,
    testInvokeMethod_httpMethod,
    testInvokeMethodResponse_status,
    testInvokeMethodResponse_headers,
    testInvokeMethodResponse_body,
    testInvokeMethodResponse_log,
    testInvokeMethodResponse_multiValueHeaders,
    testInvokeMethodResponse_latency,
    testInvokeMethodResponse_httpStatus,

    -- ** PutRestApi
    putRestApi_mode,
    putRestApi_parameters,
    putRestApi_failOnWarnings,
    putRestApi_restApiId,
    putRestApi_body,
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

    -- ** GetApiKey
    getApiKey_includeValue,
    getApiKey_apiKey,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,

    -- ** GetGatewayResponses
    getGatewayResponses_position,
    getGatewayResponses_limit,
    getGatewayResponses_restApiId,
    getGatewayResponsesResponse_items,
    getGatewayResponsesResponse_position,
    getGatewayResponsesResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_restApiId,
    deleteModel_modelName,

    -- ** DeleteMethod
    deleteMethod_restApiId,
    deleteMethod_resourceId,
    deleteMethod_httpMethod,

    -- ** GetDocumentationVersion
    getDocumentationVersion_restApiId,
    getDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** UpdateMethod
    updateMethod_patchOperations,
    updateMethod_restApiId,
    updateMethod_resourceId,
    updateMethod_httpMethod,
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,

    -- ** UpdateModel
    updateModel_patchOperations,
    updateModel_restApiId,
    updateModel_modelName,
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,

    -- ** UpdateDomainName
    updateDomainName_patchOperations,
    updateDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,

    -- ** CreateResource
    createResource_restApiId,
    createResource_parentId,
    createResource_pathPart,
    resource_id,
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,

    -- ** DeleteDomainName
    deleteDomainName_domainName,

    -- ** UpdateRequestValidator
    updateRequestValidator_patchOperations,
    updateRequestValidator_restApiId,
    updateRequestValidator_requestValidatorId,
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,

    -- ** DeleteRequestValidator
    deleteRequestValidator_restApiId,
    deleteRequestValidator_requestValidatorId,

    -- ** GetUsagePlans
    getUsagePlans_position,
    getUsagePlans_limit,
    getUsagePlans_keyId,
    getUsagePlansResponse_items,
    getUsagePlansResponse_position,
    getUsagePlansResponse_httpStatus,

    -- ** CreateRequestValidator
    createRequestValidator_validateRequestBody,
    createRequestValidator_validateRequestParameters,
    createRequestValidator_name,
    createRequestValidator_restApiId,
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,

    -- ** CreateDomainName
    createDomainName_certificatePrivateKey,
    createDomainName_regionalCertificateName,
    createDomainName_mutualTlsAuthentication,
    createDomainName_endpointConfiguration,
    createDomainName_certificateArn,
    createDomainName_tags,
    createDomainName_securityPolicy,
    createDomainName_certificateChain,
    createDomainName_certificateBody,
    createDomainName_regionalCertificateArn,
    createDomainName_certificateName,
    createDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,

    -- ** UpdateResource
    updateResource_patchOperations,
    updateResource_restApiId,
    updateResource_resourceId,
    resource_id,
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,

    -- ** GetDocumentationPart
    getDocumentationPart_restApiId,
    getDocumentationPart_documentationPartId,
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,

    -- ** GetUsage
    getUsage_position,
    getUsage_limit,
    getUsage_keyId,
    getUsage_usagePlanId,
    getUsage_startDate,
    getUsage_endDate,
    usage_startDate,
    usage_items,
    usage_position,
    usage_usagePlanId,
    usage_endDate,

    -- ** ImportDocumentationParts
    importDocumentationParts_mode,
    importDocumentationParts_failOnWarnings,
    importDocumentationParts_restApiId,
    importDocumentationParts_body,
    importDocumentationPartsResponse_warnings,
    importDocumentationPartsResponse_ids,
    importDocumentationPartsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DeleteResource
    deleteResource_restApiId,
    deleteResource_resourceId,

    -- ** GetIntegrationResponse
    getIntegrationResponse_restApiId,
    getIntegrationResponse_resourceId,
    getIntegrationResponse_httpMethod,
    getIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,

    -- ** DeleteDeployment
    deleteDeployment_restApiId,
    deleteDeployment_deploymentId,

    -- ** CreateStage
    createStage_tracingEnabled,
    createStage_cacheClusterEnabled,
    createStage_documentationVersion,
    createStage_variables,
    createStage_tags,
    createStage_description,
    createStage_canarySettings,
    createStage_cacheClusterSize,
    createStage_restApiId,
    createStage_stageName,
    createStage_deploymentId,
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,

    -- ** GetIntegration
    getIntegration_restApiId,
    getIntegration_resourceId,
    getIntegration_httpMethod,
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UpdateDeployment
    updateDeployment_patchOperations,
    updateDeployment_restApiId,
    updateDeployment_deploymentId,
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,

    -- ** GetUsagePlan
    getUsagePlan_usagePlanId,
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,

    -- ** GetRestApis
    getRestApis_position,
    getRestApis_limit,
    getRestApisResponse_items,
    getRestApisResponse_position,
    getRestApisResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_identityValidationExpression,
    createAuthorizer_authorizerCredentials,
    createAuthorizer_providerARNs,
    createAuthorizer_authorizerUri,
    createAuthorizer_identitySource,
    createAuthorizer_authType,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_restApiId,
    createAuthorizer_name,
    createAuthorizer_type,
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,

    -- ** DeleteStage
    deleteStage_restApiId,
    deleteStage_stageName,

    -- ** UpdateStage
    updateStage_patchOperations,
    updateStage_restApiId,
    updateStage_stageName,
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,

    -- ** CreateUsagePlanKey
    createUsagePlanKey_usagePlanId,
    createUsagePlanKey_keyId,
    createUsagePlanKey_keyType,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_value,
    usagePlanKey_type,

    -- ** GetGatewayResponse
    getGatewayResponse_restApiId,
    getGatewayResponse_responseType,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,

    -- ** ImportRestApi
    importRestApi_parameters,
    importRestApi_failOnWarnings,
    importRestApi_body,
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

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

    -- ** GetBasePathMapping
    getBasePathMapping_domainName,
    getBasePathMapping_basePath,
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,

    -- ** GetRequestValidators
    getRequestValidators_position,
    getRequestValidators_limit,
    getRequestValidators_restApiId,
    getRequestValidatorsResponse_items,
    getRequestValidatorsResponse_position,
    getRequestValidatorsResponse_httpStatus,

    -- ** GetDomainNames
    getDomainNames_position,
    getDomainNames_limit,
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,

    -- ** GetSdkType
    getSdkType_id,
    sdkType_friendlyName,
    sdkType_id,
    sdkType_configurationProperties,
    sdkType_description,

    -- ** PutGatewayResponse
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_responseParameters,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,

    -- ** GetExport
    getExport_accepts,
    getExport_parameters,
    getExport_restApiId,
    getExport_stageName,
    getExport_exportType,
    getExportResponse_contentType,
    getExportResponse_contentDisposition,
    getExportResponse_body,
    getExportResponse_httpStatus,

    -- ** GetClientCertificate
    getClientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_headers,
    testInvokeAuthorizer_stageVariables,
    testInvokeAuthorizer_additionalContext,
    testInvokeAuthorizer_body,
    testInvokeAuthorizer_multiValueHeaders,
    testInvokeAuthorizer_pathWithQueryString,
    testInvokeAuthorizer_restApiId,
    testInvokeAuthorizer_authorizerId,
    testInvokeAuthorizerResponse_claims,
    testInvokeAuthorizerResponse_clientStatus,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_log,
    testInvokeAuthorizerResponse_authorization,
    testInvokeAuthorizerResponse_policy,
    testInvokeAuthorizerResponse_latency,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** GetTags
    getTags_position,
    getTags_limit,
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_embed,
    getDeployment_restApiId,
    getDeployment_deploymentId,
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,

    -- ** GetAccount
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,

    -- ** PutIntegration
    putIntegration_integrationHttpMethod,
    putIntegration_passthroughBehavior,
    putIntegration_contentHandling,
    putIntegration_uri,
    putIntegration_connectionType,
    putIntegration_connectionId,
    putIntegration_requestTemplates,
    putIntegration_timeoutInMillis,
    putIntegration_cacheNamespace,
    putIntegration_cacheKeyParameters,
    putIntegration_tlsConfig,
    putIntegration_requestParameters,
    putIntegration_credentials,
    putIntegration_restApiId,
    putIntegration_resourceId,
    putIntegration_httpMethod,
    putIntegration_type,
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,

    -- ** GetResources
    getResources_position,
    getResources_embed,
    getResources_limit,
    getResources_restApiId,
    getResourcesResponse_items,
    getResourcesResponse_position,
    getResourcesResponse_httpStatus,

    -- ** GetResource
    getResource_embed,
    getResource_restApiId,
    getResource_resourceId,
    resource_id,
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,

    -- ** CreateDocumentationVersion
    createDocumentationVersion_stageName,
    createDocumentationVersion_description,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** GetAuthorizers
    getAuthorizers_position,
    getAuthorizers_limit,
    getAuthorizers_restApiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_position,
    getAuthorizersResponse_httpStatus,

    -- ** PutIntegrationResponse
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_responseParameters,
    putIntegrationResponse_selectionPattern,
    putIntegrationResponse_restApiId,
    putIntegrationResponse_resourceId,
    putIntegrationResponse_httpMethod,
    putIntegrationResponse_statusCode,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,

    -- ** GetUsagePlanKeys
    getUsagePlanKeys_position,
    getUsagePlanKeys_limit,
    getUsagePlanKeys_nameQuery,
    getUsagePlanKeys_usagePlanId,
    getUsagePlanKeysResponse_items,
    getUsagePlanKeysResponse_position,
    getUsagePlanKeysResponse_httpStatus,

    -- ** CreateDocumentationPart
    createDocumentationPart_restApiId,
    createDocumentationPart_location,
    createDocumentationPart_properties,
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,

    -- ** GetStages
    getStages_deploymentId,
    getStages_restApiId,
    getStagesResponse_item,
    getStagesResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,

    -- ** GetModelTemplate
    getModelTemplate_restApiId,
    getModelTemplate_modelName,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- ** DeleteRestApi
    deleteRestApi_restApiId,

    -- ** GetMethod
    getMethod_restApiId,
    getMethod_resourceId,
    getMethod_httpMethod,
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,

    -- ** UpdateRestApi
    updateRestApi_patchOperations,
    updateRestApi_restApiId,
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

    -- ** CreateVpcLink
    createVpcLink_tags,
    createVpcLink_description,
    createVpcLink_name,
    createVpcLink_targetArns,
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,

    -- ** GetRequestValidator
    getRequestValidator_restApiId,
    getRequestValidator_requestValidatorId,
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,

    -- ** UpdateDocumentationVersion
    updateDocumentationVersion_patchOperations,
    updateDocumentationVersion_restApiId,
    updateDocumentationVersion_documentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** ImportApiKeys
    importApiKeys_failOnWarnings,
    importApiKeys_body,
    importApiKeys_format,
    importApiKeysResponse_warnings,
    importApiKeysResponse_ids,
    importApiKeysResponse_httpStatus,

    -- ** DeleteDocumentationVersion
    deleteDocumentationVersion_restApiId,
    deleteDocumentationVersion_documentationVersion,

    -- ** PutMethod
    putMethod_apiKeyRequired,
    putMethod_requestModels,
    putMethod_operationName,
    putMethod_requestValidatorId,
    putMethod_authorizerId,
    putMethod_requestParameters,
    putMethod_authorizationScopes,
    putMethod_restApiId,
    putMethod_resourceId,
    putMethod_httpMethod,
    putMethod_authorizationType,
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,

    -- ** DeleteApiKey
    deleteApiKey_apiKey,

    -- ** FlushStageAuthorizersCache
    flushStageAuthorizersCache_restApiId,
    flushStageAuthorizersCache_stageName,

    -- ** UpdateApiKey
    updateApiKey_patchOperations,
    updateApiKey_apiKey,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,

    -- ** GetRestApi
    getRestApi_restApiId,
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

    -- ** CreateModel
    createModel_schema,
    createModel_description,
    createModel_restApiId,
    createModel_name,
    createModel_contentType,
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,

    -- ** CreateApiKey
    createApiKey_customerId,
    createApiKey_stageKeys,
    createApiKey_enabled,
    createApiKey_name,
    createApiKey_generateDistinctId,
    createApiKey_tags,
    createApiKey_description,
    createApiKey_value,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,

    -- ** DeleteUsagePlanKey
    deleteUsagePlanKey_usagePlanId,
    deleteUsagePlanKey_keyId,

    -- ** UpdateAccount
    updateAccount_patchOperations,
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,

    -- ** GetVpcLinks
    getVpcLinks_position,
    getVpcLinks_limit,
    getVpcLinksResponse_items,
    getVpcLinksResponse_position,
    getVpcLinksResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_patchOperations,
    updateAuthorizer_restApiId,
    updateAuthorizer_authorizerId,
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,

    -- ** CreateBasePathMapping
    createBasePathMapping_basePath,
    createBasePathMapping_stage,
    createBasePathMapping_domainName,
    createBasePathMapping_restApiId,
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,

    -- ** GetDocumentationParts
    getDocumentationParts_locationStatus,
    getDocumentationParts_position,
    getDocumentationParts_type,
    getDocumentationParts_limit,
    getDocumentationParts_path,
    getDocumentationParts_nameQuery,
    getDocumentationParts_restApiId,
    getDocumentationPartsResponse_items,
    getDocumentationPartsResponse_position,
    getDocumentationPartsResponse_httpStatus,

    -- ** DeleteAuthorizer
    deleteAuthorizer_restApiId,
    deleteAuthorizer_authorizerId,

    -- ** DeleteClientCertificate
    deleteClientCertificate_clientCertificateId,

    -- ** DeleteMethodResponse
    deleteMethodResponse_restApiId,
    deleteMethodResponse_resourceId,
    deleteMethodResponse_httpMethod,
    deleteMethodResponse_statusCode,

    -- ** DeleteBasePathMapping
    deleteBasePathMapping_domainName,
    deleteBasePathMapping_basePath,

    -- ** GetDocumentationVersions
    getDocumentationVersions_position,
    getDocumentationVersions_limit,
    getDocumentationVersions_restApiId,
    getDocumentationVersionsResponse_items,
    getDocumentationVersionsResponse_position,
    getDocumentationVersionsResponse_httpStatus,

    -- ** UpdateBasePathMapping
    updateBasePathMapping_patchOperations,
    updateBasePathMapping_domainName,
    updateBasePathMapping_basePath,
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,

    -- ** UpdateClientCertificate
    updateClientCertificate_patchOperations,
    updateClientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,

    -- ** UpdateMethodResponse
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** CreateDeployment
    createDeployment_tracingEnabled,
    createDeployment_cacheClusterEnabled,
    createDeployment_stageName,
    createDeployment_variables,
    createDeployment_stageDescription,
    createDeployment_description,
    createDeployment_canarySettings,
    createDeployment_cacheClusterSize,
    createDeployment_restApiId,
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,

    -- ** GetApiKeys
    getApiKeys_customerId,
    getApiKeys_includeValues,
    getApiKeys_position,
    getApiKeys_limit,
    getApiKeys_nameQuery,
    getApiKeysResponse_warnings,
    getApiKeysResponse_items,
    getApiKeysResponse_position,
    getApiKeysResponse_httpStatus,

    -- ** CreateUsagePlan
    createUsagePlan_apiStages,
    createUsagePlan_tags,
    createUsagePlan_description,
    createUsagePlan_quota,
    createUsagePlan_throttle,
    createUsagePlan_name,
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,

    -- ** UpdateGatewayResponse
    updateGatewayResponse_patchOperations,
    updateGatewayResponse_restApiId,
    updateGatewayResponse_responseType,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,

    -- ** GetSdk
    getSdk_parameters,
    getSdk_restApiId,
    getSdk_stageName,
    getSdk_sdkType,
    getSdkResponse_contentType,
    getSdkResponse_contentDisposition,
    getSdkResponse_body,
    getSdkResponse_httpStatus,

    -- ** GetMethodResponse
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** GetModels
    getModels_position,
    getModels_limit,
    getModels_restApiId,
    getModelsResponse_items,
    getModelsResponse_position,
    getModelsResponse_httpStatus,

    -- ** GetStage
    getStage_restApiId,
    getStage_stageName,
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,

    -- ** DeleteGatewayResponse
    deleteGatewayResponse_restApiId,
    deleteGatewayResponse_responseType,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_destinationArn,
    accessLogSettings_format,

    -- ** Account
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,

    -- ** ApiKey
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,

    -- ** ApiStage
    apiStage_apiId,
    apiStage_stage,
    apiStage_throttle,

    -- ** Authorizer
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,

    -- ** BasePathMapping
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,

    -- ** CanarySettings
    canarySettings_deploymentId,
    canarySettings_percentTraffic,
    canarySettings_useStageCache,
    canarySettings_stageVariableOverrides,

    -- ** ClientCertificate
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,

    -- ** Deployment
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,

    -- ** DeploymentCanarySettings
    deploymentCanarySettings_percentTraffic,
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_stageVariableOverrides,

    -- ** DocumentationPart
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,

    -- ** DocumentationPartLocation
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_statusCode,
    documentationPartLocation_path,
    documentationPartLocation_type,

    -- ** DocumentationVersion
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- ** DomainName
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,

    -- ** EndpointConfiguration
    endpointConfiguration_types,
    endpointConfiguration_vpcEndpointIds,

    -- ** GatewayResponse
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,

    -- ** Integration
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,

    -- ** IntegrationResponse
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,

    -- ** Method
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,

    -- ** MethodResponse
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- ** MethodSetting
    methodSetting_dataTraceEnabled,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_cacheDataEncrypted,
    methodSetting_throttlingRateLimit,
    methodSetting_throttlingBurstLimit,
    methodSetting_cacheTtlInSeconds,
    methodSetting_cachingEnabled,
    methodSetting_unauthorizedCacheControlHeaderStrategy,
    methodSetting_loggingLevel,
    methodSetting_metricsEnabled,

    -- ** MethodSnapshot
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- ** Model
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreWarnings,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreVersion,
    mutualTlsAuthenticationInput_truststoreUri,

    -- ** PatchOperation
    patchOperation_op,
    patchOperation_from,
    patchOperation_value,
    patchOperation_path,

    -- ** QuotaSettings
    quotaSettings_period,
    quotaSettings_limit,
    quotaSettings_offset,

    -- ** RequestValidator
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,

    -- ** Resource
    resource_id,
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,

    -- ** RestApi
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,

    -- ** SdkConfigurationProperty
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_defaultValue,

    -- ** SdkType
    sdkType_friendlyName,
    sdkType_id,
    sdkType_configurationProperties,
    sdkType_description,

    -- ** Stage
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,

    -- ** StageKey
    stageKey_stageName,
    stageKey_restApiId,

    -- ** ThrottleSettings
    throttleSettings_burstLimit,
    throttleSettings_rateLimit,

    -- ** TlsConfig
    tlsConfig_insecureSkipVerification,

    -- ** Usage
    usage_startDate,
    usage_items,
    usage_position,
    usage_usagePlanId,
    usage_endDate,

    -- ** UsagePlan
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,

    -- ** UsagePlanKey
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_value,
    usagePlanKey_type,

    -- ** VpcLink
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,
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
