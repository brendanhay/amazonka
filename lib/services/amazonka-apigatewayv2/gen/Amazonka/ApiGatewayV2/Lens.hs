{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Lens
  ( -- * Operations

    -- ** CreateApi
    createApi_tags,
    createApi_credentialsArn,
    createApi_routeSelectionExpression,
    createApi_target,
    createApi_description,
    createApi_routeKey,
    createApi_apiKeySelectionExpression,
    createApi_disableExecuteApiEndpoint,
    createApi_disableSchemaValidation,
    createApi_corsConfiguration,
    createApi_version,
    createApi_protocolType,
    createApi_name,
    createApiResponse_tags,
    createApiResponse_name,
    createApiResponse_apiEndpoint,
    createApiResponse_apiId,
    createApiResponse_routeSelectionExpression,
    createApiResponse_description,
    createApiResponse_warnings,
    createApiResponse_apiKeySelectionExpression,
    createApiResponse_protocolType,
    createApiResponse_disableExecuteApiEndpoint,
    createApiResponse_createdDate,
    createApiResponse_disableSchemaValidation,
    createApiResponse_importInfo,
    createApiResponse_corsConfiguration,
    createApiResponse_apiGatewayManaged,
    createApiResponse_version,
    createApiResponse_httpStatus,

    -- ** CreateApiMapping
    createApiMapping_apiMappingKey,
    createApiMapping_domainName,
    createApiMapping_stage,
    createApiMapping_apiId,
    createApiMappingResponse_apiId,
    createApiMappingResponse_apiMappingKey,
    createApiMappingResponse_stage,
    createApiMappingResponse_apiMappingId,
    createApiMappingResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_authorizerCredentialsArn,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_jwtConfiguration,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_authorizerUri,
    createAuthorizer_enableSimpleResponses,
    createAuthorizer_authorizerPayloadFormatVersion,
    createAuthorizer_apiId,
    createAuthorizer_authorizerType,
    createAuthorizer_identitySource,
    createAuthorizer_name,
    createAuthorizerResponse_name,
    createAuthorizerResponse_authorizerCredentialsArn,
    createAuthorizerResponse_identitySource,
    createAuthorizerResponse_authorizerResultTtlInSeconds,
    createAuthorizerResponse_jwtConfiguration,
    createAuthorizerResponse_identityValidationExpression,
    createAuthorizerResponse_authorizerUri,
    createAuthorizerResponse_authorizerType,
    createAuthorizerResponse_enableSimpleResponses,
    createAuthorizerResponse_authorizerId,
    createAuthorizerResponse_authorizerPayloadFormatVersion,
    createAuthorizerResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_stageName,
    createDeployment_description,
    createDeployment_apiId,
    createDeploymentResponse_deploymentStatus,
    createDeploymentResponse_autoDeployed,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_description,
    createDeploymentResponse_deploymentStatusMessage,
    createDeploymentResponse_createdDate,
    createDeploymentResponse_httpStatus,

    -- ** CreateDomainName
    createDomainName_tags,
    createDomainName_mutualTlsAuthentication,
    createDomainName_domainNameConfigurations,
    createDomainName_domainName,
    createDomainNameResponse_tags,
    createDomainNameResponse_mutualTlsAuthentication,
    createDomainNameResponse_domainName,
    createDomainNameResponse_domainNameConfigurations,
    createDomainNameResponse_apiMappingSelectionExpression,
    createDomainNameResponse_httpStatus,

    -- ** CreateIntegration
    createIntegration_credentialsArn,
    createIntegration_requestParameters,
    createIntegration_connectionType,
    createIntegration_tlsConfig,
    createIntegration_templateSelectionExpression,
    createIntegration_connectionId,
    createIntegration_description,
    createIntegration_integrationMethod,
    createIntegration_integrationUri,
    createIntegration_responseParameters,
    createIntegration_payloadFormatVersion,
    createIntegration_timeoutInMillis,
    createIntegration_contentHandlingStrategy,
    createIntegration_requestTemplates,
    createIntegration_passthroughBehavior,
    createIntegration_integrationSubtype,
    createIntegration_apiId,
    createIntegration_integrationType,
    createIntegrationResponse'_credentialsArn,
    createIntegrationResponse'_requestParameters,
    createIntegrationResponse'_connectionType,
    createIntegrationResponse'_tlsConfig,
    createIntegrationResponse'_templateSelectionExpression,
    createIntegrationResponse'_integrationResponseSelectionExpression,
    createIntegrationResponse'_connectionId,
    createIntegrationResponse'_description,
    createIntegrationResponse'_integrationMethod,
    createIntegrationResponse'_integrationUri,
    createIntegrationResponse'_integrationType,
    createIntegrationResponse'_responseParameters,
    createIntegrationResponse'_payloadFormatVersion,
    createIntegrationResponse'_timeoutInMillis,
    createIntegrationResponse'_integrationId,
    createIntegrationResponse'_contentHandlingStrategy,
    createIntegrationResponse'_requestTemplates,
    createIntegrationResponse'_apiGatewayManaged,
    createIntegrationResponse'_passthroughBehavior,
    createIntegrationResponse'_integrationSubtype,
    createIntegrationResponse'_httpStatus,

    -- ** CreateIntegrationResponse
    createIntegrationResponse_templateSelectionExpression,
    createIntegrationResponse_responseParameters,
    createIntegrationResponse_responseTemplates,
    createIntegrationResponse_contentHandlingStrategy,
    createIntegrationResponse_apiId,
    createIntegrationResponse_integrationId,
    createIntegrationResponse_integrationResponseKey,
    createIntegrationResponseResponse_templateSelectionExpression,
    createIntegrationResponseResponse_responseParameters,
    createIntegrationResponseResponse_integrationResponseKey,
    createIntegrationResponseResponse_responseTemplates,
    createIntegrationResponseResponse_contentHandlingStrategy,
    createIntegrationResponseResponse_integrationResponseId,
    createIntegrationResponseResponse_httpStatus,

    -- ** CreateModel
    createModel_description,
    createModel_contentType,
    createModel_apiId,
    createModel_schema,
    createModel_name,
    createModelResponse_name,
    createModelResponse_description,
    createModelResponse_schema,
    createModelResponse_modelId,
    createModelResponse_contentType,
    createModelResponse_httpStatus,

    -- ** CreateRoute
    createRoute_requestModels,
    createRoute_requestParameters,
    createRoute_apiKeyRequired,
    createRoute_target,
    createRoute_modelSelectionExpression,
    createRoute_authorizationScopes,
    createRoute_authorizationType,
    createRoute_operationName,
    createRoute_routeResponseSelectionExpression,
    createRoute_authorizerId,
    createRoute_apiId,
    createRoute_routeKey,
    createRouteResponse'_requestModels,
    createRouteResponse'_requestParameters,
    createRouteResponse'_apiKeyRequired,
    createRouteResponse'_target,
    createRouteResponse'_modelSelectionExpression,
    createRouteResponse'_routeKey,
    createRouteResponse'_routeId,
    createRouteResponse'_authorizationScopes,
    createRouteResponse'_authorizationType,
    createRouteResponse'_operationName,
    createRouteResponse'_apiGatewayManaged,
    createRouteResponse'_routeResponseSelectionExpression,
    createRouteResponse'_authorizerId,
    createRouteResponse'_httpStatus,

    -- ** CreateRouteResponse
    createRouteResponse_modelSelectionExpression,
    createRouteResponse_responseParameters,
    createRouteResponse_responseModels,
    createRouteResponse_apiId,
    createRouteResponse_routeId,
    createRouteResponse_routeResponseKey,
    createRouteResponseResponse_routeResponseKey,
    createRouteResponseResponse_modelSelectionExpression,
    createRouteResponseResponse_responseParameters,
    createRouteResponseResponse_responseModels,
    createRouteResponseResponse_routeResponseId,
    createRouteResponseResponse_httpStatus,

    -- ** CreateStage
    createStage_tags,
    createStage_accessLogSettings,
    createStage_deploymentId,
    createStage_autoDeploy,
    createStage_stageVariables,
    createStage_description,
    createStage_clientCertificateId,
    createStage_defaultRouteSettings,
    createStage_routeSettings,
    createStage_apiId,
    createStage_stageName,
    createStageResponse_tags,
    createStageResponse_stageName,
    createStageResponse_accessLogSettings,
    createStageResponse_deploymentId,
    createStageResponse_autoDeploy,
    createStageResponse_lastUpdatedDate,
    createStageResponse_stageVariables,
    createStageResponse_description,
    createStageResponse_clientCertificateId,
    createStageResponse_lastDeploymentStatusMessage,
    createStageResponse_defaultRouteSettings,
    createStageResponse_createdDate,
    createStageResponse_apiGatewayManaged,
    createStageResponse_routeSettings,
    createStageResponse_httpStatus,

    -- ** CreateVpcLink
    createVpcLink_tags,
    createVpcLink_securityGroupIds,
    createVpcLink_subnetIds,
    createVpcLink_name,
    createVpcLinkResponse_tags,
    createVpcLinkResponse_vpcLinkStatusMessage,
    createVpcLinkResponse_name,
    createVpcLinkResponse_securityGroupIds,
    createVpcLinkResponse_vpcLinkVersion,
    createVpcLinkResponse_vpcLinkStatus,
    createVpcLinkResponse_vpcLinkId,
    createVpcLinkResponse_createdDate,
    createVpcLinkResponse_subnetIds,
    createVpcLinkResponse_httpStatus,

    -- ** DeleteAccessLogSettings
    deleteAccessLogSettings_stageName,
    deleteAccessLogSettings_apiId,

    -- ** DeleteApi
    deleteApi_apiId,

    -- ** DeleteApiMapping
    deleteApiMapping_apiMappingId,
    deleteApiMapping_domainName,

    -- ** DeleteAuthorizer
    deleteAuthorizer_authorizerId,
    deleteAuthorizer_apiId,

    -- ** DeleteCorsConfiguration
    deleteCorsConfiguration_apiId,

    -- ** DeleteDeployment
    deleteDeployment_apiId,
    deleteDeployment_deploymentId,

    -- ** DeleteDomainName
    deleteDomainName_domainName,

    -- ** DeleteIntegration
    deleteIntegration_apiId,
    deleteIntegration_integrationId,

    -- ** DeleteIntegrationResponse
    deleteIntegrationResponse_apiId,
    deleteIntegrationResponse_integrationResponseId,
    deleteIntegrationResponse_integrationId,

    -- ** DeleteModel
    deleteModel_modelId,
    deleteModel_apiId,

    -- ** DeleteRoute
    deleteRoute_apiId,
    deleteRoute_routeId,

    -- ** DeleteRouteRequestParameter
    deleteRouteRequestParameter_requestParameterKey,
    deleteRouteRequestParameter_apiId,
    deleteRouteRequestParameter_routeId,

    -- ** DeleteRouteResponse
    deleteRouteResponse_routeResponseId,
    deleteRouteResponse_apiId,
    deleteRouteResponse_routeId,

    -- ** DeleteRouteSettings
    deleteRouteSettings_stageName,
    deleteRouteSettings_routeKey,
    deleteRouteSettings_apiId,

    -- ** DeleteStage
    deleteStage_stageName,
    deleteStage_apiId,

    -- ** DeleteVpcLink
    deleteVpcLink_vpcLinkId,
    deleteVpcLinkResponse_httpStatus,

    -- ** ExportApi
    exportApi_stageName,
    exportApi_exportVersion,
    exportApi_includeExtensions,
    exportApi_specification,
    exportApi_outputType,
    exportApi_apiId,
    exportApiResponse_body,
    exportApiResponse_httpStatus,

    -- ** GetApi
    getApi_apiId,
    getApiResponse_tags,
    getApiResponse_name,
    getApiResponse_apiEndpoint,
    getApiResponse_apiId,
    getApiResponse_routeSelectionExpression,
    getApiResponse_description,
    getApiResponse_warnings,
    getApiResponse_apiKeySelectionExpression,
    getApiResponse_protocolType,
    getApiResponse_disableExecuteApiEndpoint,
    getApiResponse_createdDate,
    getApiResponse_disableSchemaValidation,
    getApiResponse_importInfo,
    getApiResponse_corsConfiguration,
    getApiResponse_apiGatewayManaged,
    getApiResponse_version,
    getApiResponse_httpStatus,

    -- ** GetApiMapping
    getApiMapping_apiMappingId,
    getApiMapping_domainName,
    getApiMappingResponse_apiId,
    getApiMappingResponse_apiMappingKey,
    getApiMappingResponse_stage,
    getApiMappingResponse_apiMappingId,
    getApiMappingResponse_httpStatus,

    -- ** GetApiMappings
    getApiMappings_nextToken,
    getApiMappings_maxResults,
    getApiMappings_domainName,
    getApiMappingsResponse_items,
    getApiMappingsResponse_nextToken,
    getApiMappingsResponse_httpStatus,

    -- ** GetApis
    getApis_nextToken,
    getApis_maxResults,
    getApisResponse_items,
    getApisResponse_nextToken,
    getApisResponse_httpStatus,

    -- ** GetAuthorizer
    getAuthorizer_authorizerId,
    getAuthorizer_apiId,
    getAuthorizerResponse_name,
    getAuthorizerResponse_authorizerCredentialsArn,
    getAuthorizerResponse_identitySource,
    getAuthorizerResponse_authorizerResultTtlInSeconds,
    getAuthorizerResponse_jwtConfiguration,
    getAuthorizerResponse_identityValidationExpression,
    getAuthorizerResponse_authorizerUri,
    getAuthorizerResponse_authorizerType,
    getAuthorizerResponse_enableSimpleResponses,
    getAuthorizerResponse_authorizerId,
    getAuthorizerResponse_authorizerPayloadFormatVersion,
    getAuthorizerResponse_httpStatus,

    -- ** GetAuthorizers
    getAuthorizers_nextToken,
    getAuthorizers_maxResults,
    getAuthorizers_apiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_nextToken,
    getAuthorizersResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_apiId,
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_autoDeployed,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_description,
    getDeploymentResponse_deploymentStatusMessage,
    getDeploymentResponse_createdDate,
    getDeploymentResponse_httpStatus,

    -- ** GetDeployments
    getDeployments_nextToken,
    getDeployments_maxResults,
    getDeployments_apiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_nextToken,
    getDeploymentsResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    getDomainNameResponse_tags,
    getDomainNameResponse_mutualTlsAuthentication,
    getDomainNameResponse_domainName,
    getDomainNameResponse_domainNameConfigurations,
    getDomainNameResponse_apiMappingSelectionExpression,
    getDomainNameResponse_httpStatus,

    -- ** GetDomainNames
    getDomainNames_nextToken,
    getDomainNames_maxResults,
    getDomainNamesResponse_items,
    getDomainNamesResponse_nextToken,
    getDomainNamesResponse_httpStatus,

    -- ** GetIntegration
    getIntegration_apiId,
    getIntegration_integrationId,
    getIntegrationResponse'_credentialsArn,
    getIntegrationResponse'_requestParameters,
    getIntegrationResponse'_connectionType,
    getIntegrationResponse'_tlsConfig,
    getIntegrationResponse'_templateSelectionExpression,
    getIntegrationResponse'_integrationResponseSelectionExpression,
    getIntegrationResponse'_connectionId,
    getIntegrationResponse'_description,
    getIntegrationResponse'_integrationMethod,
    getIntegrationResponse'_integrationUri,
    getIntegrationResponse'_integrationType,
    getIntegrationResponse'_responseParameters,
    getIntegrationResponse'_payloadFormatVersion,
    getIntegrationResponse'_timeoutInMillis,
    getIntegrationResponse'_integrationId,
    getIntegrationResponse'_contentHandlingStrategy,
    getIntegrationResponse'_requestTemplates,
    getIntegrationResponse'_apiGatewayManaged,
    getIntegrationResponse'_passthroughBehavior,
    getIntegrationResponse'_integrationSubtype,
    getIntegrationResponse'_httpStatus,

    -- ** GetIntegrationResponse
    getIntegrationResponse_apiId,
    getIntegrationResponse_integrationResponseId,
    getIntegrationResponse_integrationId,
    getIntegrationResponseResponse_templateSelectionExpression,
    getIntegrationResponseResponse_responseParameters,
    getIntegrationResponseResponse_integrationResponseKey,
    getIntegrationResponseResponse_responseTemplates,
    getIntegrationResponseResponse_contentHandlingStrategy,
    getIntegrationResponseResponse_integrationResponseId,
    getIntegrationResponseResponse_httpStatus,

    -- ** GetIntegrationResponses
    getIntegrationResponses_nextToken,
    getIntegrationResponses_maxResults,
    getIntegrationResponses_integrationId,
    getIntegrationResponses_apiId,
    getIntegrationResponsesResponse_items,
    getIntegrationResponsesResponse_nextToken,
    getIntegrationResponsesResponse_httpStatus,

    -- ** GetIntegrations
    getIntegrations_nextToken,
    getIntegrations_maxResults,
    getIntegrations_apiId,
    getIntegrationsResponse_items,
    getIntegrationsResponse_nextToken,
    getIntegrationsResponse_httpStatus,

    -- ** GetModel
    getModel_modelId,
    getModel_apiId,
    getModelResponse_name,
    getModelResponse_description,
    getModelResponse_schema,
    getModelResponse_modelId,
    getModelResponse_contentType,
    getModelResponse_httpStatus,

    -- ** GetModelTemplate
    getModelTemplate_modelId,
    getModelTemplate_apiId,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- ** GetModels
    getModels_nextToken,
    getModels_maxResults,
    getModels_apiId,
    getModelsResponse_items,
    getModelsResponse_nextToken,
    getModelsResponse_httpStatus,

    -- ** GetRoute
    getRoute_apiId,
    getRoute_routeId,
    getRouteResponse'_requestModels,
    getRouteResponse'_requestParameters,
    getRouteResponse'_apiKeyRequired,
    getRouteResponse'_target,
    getRouteResponse'_modelSelectionExpression,
    getRouteResponse'_routeKey,
    getRouteResponse'_routeId,
    getRouteResponse'_authorizationScopes,
    getRouteResponse'_authorizationType,
    getRouteResponse'_operationName,
    getRouteResponse'_apiGatewayManaged,
    getRouteResponse'_routeResponseSelectionExpression,
    getRouteResponse'_authorizerId,
    getRouteResponse'_httpStatus,

    -- ** GetRouteResponse
    getRouteResponse_routeResponseId,
    getRouteResponse_apiId,
    getRouteResponse_routeId,
    getRouteResponseResponse_routeResponseKey,
    getRouteResponseResponse_modelSelectionExpression,
    getRouteResponseResponse_responseParameters,
    getRouteResponseResponse_responseModels,
    getRouteResponseResponse_routeResponseId,
    getRouteResponseResponse_httpStatus,

    -- ** GetRouteResponses
    getRouteResponses_nextToken,
    getRouteResponses_maxResults,
    getRouteResponses_routeId,
    getRouteResponses_apiId,
    getRouteResponsesResponse_items,
    getRouteResponsesResponse_nextToken,
    getRouteResponsesResponse_httpStatus,

    -- ** GetRoutes
    getRoutes_nextToken,
    getRoutes_maxResults,
    getRoutes_apiId,
    getRoutesResponse_items,
    getRoutesResponse_nextToken,
    getRoutesResponse_httpStatus,

    -- ** GetStage
    getStage_stageName,
    getStage_apiId,
    getStageResponse_tags,
    getStageResponse_stageName,
    getStageResponse_accessLogSettings,
    getStageResponse_deploymentId,
    getStageResponse_autoDeploy,
    getStageResponse_lastUpdatedDate,
    getStageResponse_stageVariables,
    getStageResponse_description,
    getStageResponse_clientCertificateId,
    getStageResponse_lastDeploymentStatusMessage,
    getStageResponse_defaultRouteSettings,
    getStageResponse_createdDate,
    getStageResponse_apiGatewayManaged,
    getStageResponse_routeSettings,
    getStageResponse_httpStatus,

    -- ** GetStages
    getStages_nextToken,
    getStages_maxResults,
    getStages_apiId,
    getStagesResponse_items,
    getStagesResponse_nextToken,
    getStagesResponse_httpStatus,

    -- ** GetTags
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    getVpcLinkResponse_tags,
    getVpcLinkResponse_vpcLinkStatusMessage,
    getVpcLinkResponse_name,
    getVpcLinkResponse_securityGroupIds,
    getVpcLinkResponse_vpcLinkVersion,
    getVpcLinkResponse_vpcLinkStatus,
    getVpcLinkResponse_vpcLinkId,
    getVpcLinkResponse_createdDate,
    getVpcLinkResponse_subnetIds,
    getVpcLinkResponse_httpStatus,

    -- ** GetVpcLinks
    getVpcLinks_nextToken,
    getVpcLinks_maxResults,
    getVpcLinksResponse_items,
    getVpcLinksResponse_nextToken,
    getVpcLinksResponse_httpStatus,

    -- ** ImportApi
    importApi_failOnWarnings,
    importApi_basepath,
    importApi_body,
    importApiResponse_tags,
    importApiResponse_name,
    importApiResponse_apiEndpoint,
    importApiResponse_apiId,
    importApiResponse_routeSelectionExpression,
    importApiResponse_description,
    importApiResponse_warnings,
    importApiResponse_apiKeySelectionExpression,
    importApiResponse_protocolType,
    importApiResponse_disableExecuteApiEndpoint,
    importApiResponse_createdDate,
    importApiResponse_disableSchemaValidation,
    importApiResponse_importInfo,
    importApiResponse_corsConfiguration,
    importApiResponse_apiGatewayManaged,
    importApiResponse_version,
    importApiResponse_httpStatus,

    -- ** ReimportApi
    reimportApi_failOnWarnings,
    reimportApi_basepath,
    reimportApi_apiId,
    reimportApi_body,
    reimportApiResponse_tags,
    reimportApiResponse_name,
    reimportApiResponse_apiEndpoint,
    reimportApiResponse_apiId,
    reimportApiResponse_routeSelectionExpression,
    reimportApiResponse_description,
    reimportApiResponse_warnings,
    reimportApiResponse_apiKeySelectionExpression,
    reimportApiResponse_protocolType,
    reimportApiResponse_disableExecuteApiEndpoint,
    reimportApiResponse_createdDate,
    reimportApiResponse_disableSchemaValidation,
    reimportApiResponse_importInfo,
    reimportApiResponse_corsConfiguration,
    reimportApiResponse_apiGatewayManaged,
    reimportApiResponse_version,
    reimportApiResponse_httpStatus,

    -- ** ResetAuthorizersCache
    resetAuthorizersCache_stageName,
    resetAuthorizersCache_apiId,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApi
    updateApi_credentialsArn,
    updateApi_name,
    updateApi_routeSelectionExpression,
    updateApi_target,
    updateApi_description,
    updateApi_routeKey,
    updateApi_apiKeySelectionExpression,
    updateApi_disableExecuteApiEndpoint,
    updateApi_disableSchemaValidation,
    updateApi_corsConfiguration,
    updateApi_version,
    updateApi_apiId,
    updateApiResponse_tags,
    updateApiResponse_name,
    updateApiResponse_apiEndpoint,
    updateApiResponse_apiId,
    updateApiResponse_routeSelectionExpression,
    updateApiResponse_description,
    updateApiResponse_warnings,
    updateApiResponse_apiKeySelectionExpression,
    updateApiResponse_protocolType,
    updateApiResponse_disableExecuteApiEndpoint,
    updateApiResponse_createdDate,
    updateApiResponse_disableSchemaValidation,
    updateApiResponse_importInfo,
    updateApiResponse_corsConfiguration,
    updateApiResponse_apiGatewayManaged,
    updateApiResponse_version,
    updateApiResponse_httpStatus,

    -- ** UpdateApiMapping
    updateApiMapping_apiMappingKey,
    updateApiMapping_stage,
    updateApiMapping_apiMappingId,
    updateApiMapping_apiId,
    updateApiMapping_domainName,
    updateApiMappingResponse_apiId,
    updateApiMappingResponse_apiMappingKey,
    updateApiMappingResponse_stage,
    updateApiMappingResponse_apiMappingId,
    updateApiMappingResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_name,
    updateAuthorizer_authorizerCredentialsArn,
    updateAuthorizer_identitySource,
    updateAuthorizer_authorizerResultTtlInSeconds,
    updateAuthorizer_jwtConfiguration,
    updateAuthorizer_identityValidationExpression,
    updateAuthorizer_authorizerUri,
    updateAuthorizer_authorizerType,
    updateAuthorizer_enableSimpleResponses,
    updateAuthorizer_authorizerPayloadFormatVersion,
    updateAuthorizer_authorizerId,
    updateAuthorizer_apiId,
    updateAuthorizerResponse_name,
    updateAuthorizerResponse_authorizerCredentialsArn,
    updateAuthorizerResponse_identitySource,
    updateAuthorizerResponse_authorizerResultTtlInSeconds,
    updateAuthorizerResponse_jwtConfiguration,
    updateAuthorizerResponse_identityValidationExpression,
    updateAuthorizerResponse_authorizerUri,
    updateAuthorizerResponse_authorizerType,
    updateAuthorizerResponse_enableSimpleResponses,
    updateAuthorizerResponse_authorizerId,
    updateAuthorizerResponse_authorizerPayloadFormatVersion,
    updateAuthorizerResponse_httpStatus,

    -- ** UpdateDeployment
    updateDeployment_description,
    updateDeployment_apiId,
    updateDeployment_deploymentId,
    updateDeploymentResponse_deploymentStatus,
    updateDeploymentResponse_autoDeployed,
    updateDeploymentResponse_deploymentId,
    updateDeploymentResponse_description,
    updateDeploymentResponse_deploymentStatusMessage,
    updateDeploymentResponse_createdDate,
    updateDeploymentResponse_httpStatus,

    -- ** UpdateDomainName
    updateDomainName_mutualTlsAuthentication,
    updateDomainName_domainNameConfigurations,
    updateDomainName_domainName,
    updateDomainNameResponse_tags,
    updateDomainNameResponse_mutualTlsAuthentication,
    updateDomainNameResponse_domainName,
    updateDomainNameResponse_domainNameConfigurations,
    updateDomainNameResponse_apiMappingSelectionExpression,
    updateDomainNameResponse_httpStatus,

    -- ** UpdateIntegration
    updateIntegration_credentialsArn,
    updateIntegration_requestParameters,
    updateIntegration_connectionType,
    updateIntegration_tlsConfig,
    updateIntegration_templateSelectionExpression,
    updateIntegration_connectionId,
    updateIntegration_description,
    updateIntegration_integrationMethod,
    updateIntegration_integrationUri,
    updateIntegration_integrationType,
    updateIntegration_responseParameters,
    updateIntegration_payloadFormatVersion,
    updateIntegration_timeoutInMillis,
    updateIntegration_contentHandlingStrategy,
    updateIntegration_requestTemplates,
    updateIntegration_passthroughBehavior,
    updateIntegration_integrationSubtype,
    updateIntegration_apiId,
    updateIntegration_integrationId,
    updateIntegrationResponse'_credentialsArn,
    updateIntegrationResponse'_requestParameters,
    updateIntegrationResponse'_connectionType,
    updateIntegrationResponse'_tlsConfig,
    updateIntegrationResponse'_templateSelectionExpression,
    updateIntegrationResponse'_integrationResponseSelectionExpression,
    updateIntegrationResponse'_connectionId,
    updateIntegrationResponse'_description,
    updateIntegrationResponse'_integrationMethod,
    updateIntegrationResponse'_integrationUri,
    updateIntegrationResponse'_integrationType,
    updateIntegrationResponse'_responseParameters,
    updateIntegrationResponse'_payloadFormatVersion,
    updateIntegrationResponse'_timeoutInMillis,
    updateIntegrationResponse'_integrationId,
    updateIntegrationResponse'_contentHandlingStrategy,
    updateIntegrationResponse'_requestTemplates,
    updateIntegrationResponse'_apiGatewayManaged,
    updateIntegrationResponse'_passthroughBehavior,
    updateIntegrationResponse'_integrationSubtype,
    updateIntegrationResponse'_httpStatus,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_templateSelectionExpression,
    updateIntegrationResponse_responseParameters,
    updateIntegrationResponse_integrationResponseKey,
    updateIntegrationResponse_responseTemplates,
    updateIntegrationResponse_contentHandlingStrategy,
    updateIntegrationResponse_apiId,
    updateIntegrationResponse_integrationResponseId,
    updateIntegrationResponse_integrationId,
    updateIntegrationResponseResponse_templateSelectionExpression,
    updateIntegrationResponseResponse_responseParameters,
    updateIntegrationResponseResponse_integrationResponseKey,
    updateIntegrationResponseResponse_responseTemplates,
    updateIntegrationResponseResponse_contentHandlingStrategy,
    updateIntegrationResponseResponse_integrationResponseId,
    updateIntegrationResponseResponse_httpStatus,

    -- ** UpdateModel
    updateModel_name,
    updateModel_description,
    updateModel_schema,
    updateModel_contentType,
    updateModel_modelId,
    updateModel_apiId,
    updateModelResponse_name,
    updateModelResponse_description,
    updateModelResponse_schema,
    updateModelResponse_modelId,
    updateModelResponse_contentType,
    updateModelResponse_httpStatus,

    -- ** UpdateRoute
    updateRoute_requestModels,
    updateRoute_requestParameters,
    updateRoute_apiKeyRequired,
    updateRoute_target,
    updateRoute_modelSelectionExpression,
    updateRoute_routeKey,
    updateRoute_authorizationScopes,
    updateRoute_authorizationType,
    updateRoute_operationName,
    updateRoute_routeResponseSelectionExpression,
    updateRoute_authorizerId,
    updateRoute_apiId,
    updateRoute_routeId,
    updateRouteResponse'_requestModels,
    updateRouteResponse'_requestParameters,
    updateRouteResponse'_apiKeyRequired,
    updateRouteResponse'_target,
    updateRouteResponse'_modelSelectionExpression,
    updateRouteResponse'_routeKey,
    updateRouteResponse'_routeId,
    updateRouteResponse'_authorizationScopes,
    updateRouteResponse'_authorizationType,
    updateRouteResponse'_operationName,
    updateRouteResponse'_apiGatewayManaged,
    updateRouteResponse'_routeResponseSelectionExpression,
    updateRouteResponse'_authorizerId,
    updateRouteResponse'_httpStatus,

    -- ** UpdateRouteResponse
    updateRouteResponse_routeResponseKey,
    updateRouteResponse_modelSelectionExpression,
    updateRouteResponse_responseParameters,
    updateRouteResponse_responseModels,
    updateRouteResponse_routeResponseId,
    updateRouteResponse_apiId,
    updateRouteResponse_routeId,
    updateRouteResponseResponse_routeResponseKey,
    updateRouteResponseResponse_modelSelectionExpression,
    updateRouteResponseResponse_responseParameters,
    updateRouteResponseResponse_responseModels,
    updateRouteResponseResponse_routeResponseId,
    updateRouteResponseResponse_httpStatus,

    -- ** UpdateStage
    updateStage_accessLogSettings,
    updateStage_deploymentId,
    updateStage_autoDeploy,
    updateStage_stageVariables,
    updateStage_description,
    updateStage_clientCertificateId,
    updateStage_defaultRouteSettings,
    updateStage_routeSettings,
    updateStage_stageName,
    updateStage_apiId,
    updateStageResponse_tags,
    updateStageResponse_stageName,
    updateStageResponse_accessLogSettings,
    updateStageResponse_deploymentId,
    updateStageResponse_autoDeploy,
    updateStageResponse_lastUpdatedDate,
    updateStageResponse_stageVariables,
    updateStageResponse_description,
    updateStageResponse_clientCertificateId,
    updateStageResponse_lastDeploymentStatusMessage,
    updateStageResponse_defaultRouteSettings,
    updateStageResponse_createdDate,
    updateStageResponse_apiGatewayManaged,
    updateStageResponse_routeSettings,
    updateStageResponse_httpStatus,

    -- ** UpdateVpcLink
    updateVpcLink_name,
    updateVpcLink_vpcLinkId,
    updateVpcLinkResponse_tags,
    updateVpcLinkResponse_vpcLinkStatusMessage,
    updateVpcLinkResponse_name,
    updateVpcLinkResponse_securityGroupIds,
    updateVpcLinkResponse_vpcLinkVersion,
    updateVpcLinkResponse_vpcLinkStatus,
    updateVpcLinkResponse_vpcLinkId,
    updateVpcLinkResponse_createdDate,
    updateVpcLinkResponse_subnetIds,
    updateVpcLinkResponse_httpStatus,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- ** Api
    api_tags,
    api_apiEndpoint,
    api_apiId,
    api_description,
    api_warnings,
    api_apiKeySelectionExpression,
    api_disableExecuteApiEndpoint,
    api_createdDate,
    api_disableSchemaValidation,
    api_importInfo,
    api_corsConfiguration,
    api_apiGatewayManaged,
    api_version,
    api_routeSelectionExpression,
    api_name,
    api_protocolType,

    -- ** ApiMapping
    apiMapping_apiMappingKey,
    apiMapping_apiMappingId,
    apiMapping_stage,
    apiMapping_apiId,

    -- ** Authorizer
    authorizer_authorizerCredentialsArn,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_jwtConfiguration,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_authorizerType,
    authorizer_enableSimpleResponses,
    authorizer_authorizerId,
    authorizer_authorizerPayloadFormatVersion,
    authorizer_name,

    -- ** Cors
    cors_allowHeaders,
    cors_exposeHeaders,
    cors_allowCredentials,
    cors_allowMethods,
    cors_allowOrigins,
    cors_maxAge,

    -- ** Deployment
    deployment_deploymentStatus,
    deployment_autoDeployed,
    deployment_deploymentId,
    deployment_description,
    deployment_deploymentStatusMessage,
    deployment_createdDate,

    -- ** DomainName
    domainName_tags,
    domainName_mutualTlsAuthentication,
    domainName_domainNameConfigurations,
    domainName_apiMappingSelectionExpression,
    domainName_domainName,

    -- ** DomainNameConfiguration
    domainNameConfiguration_hostedZoneId,
    domainNameConfiguration_domainNameStatus,
    domainNameConfiguration_ownershipVerificationCertificateArn,
    domainNameConfiguration_certificateName,
    domainNameConfiguration_endpointType,
    domainNameConfiguration_domainNameStatusMessage,
    domainNameConfiguration_certificateArn,
    domainNameConfiguration_certificateUploadDate,
    domainNameConfiguration_apiGatewayDomainName,
    domainNameConfiguration_securityPolicy,

    -- ** Integration
    integration_credentialsArn,
    integration_requestParameters,
    integration_connectionType,
    integration_tlsConfig,
    integration_templateSelectionExpression,
    integration_integrationResponseSelectionExpression,
    integration_connectionId,
    integration_description,
    integration_integrationMethod,
    integration_integrationUri,
    integration_integrationType,
    integration_responseParameters,
    integration_payloadFormatVersion,
    integration_timeoutInMillis,
    integration_integrationId,
    integration_contentHandlingStrategy,
    integration_requestTemplates,
    integration_apiGatewayManaged,
    integration_passthroughBehavior,
    integration_integrationSubtype,

    -- ** IntegrationResponse
    integrationResponse_templateSelectionExpression,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_contentHandlingStrategy,
    integrationResponse_integrationResponseId,
    integrationResponse_integrationResponseKey,

    -- ** JWTConfiguration
    jWTConfiguration_issuer,
    jWTConfiguration_audience,

    -- ** Model
    model_description,
    model_schema,
    model_modelId,
    model_contentType,
    model_name,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreUri,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreVersion,
    mutualTlsAuthenticationInput_truststoreUri,

    -- ** ParameterConstraints
    parameterConstraints_required,

    -- ** Route
    route_requestModels,
    route_requestParameters,
    route_apiKeyRequired,
    route_target,
    route_modelSelectionExpression,
    route_routeId,
    route_authorizationScopes,
    route_authorizationType,
    route_operationName,
    route_apiGatewayManaged,
    route_routeResponseSelectionExpression,
    route_authorizerId,
    route_routeKey,

    -- ** RouteResponse
    routeResponse_modelSelectionExpression,
    routeResponse_responseParameters,
    routeResponse_responseModels,
    routeResponse_routeResponseId,
    routeResponse_routeResponseKey,

    -- ** RouteSettings
    routeSettings_throttlingRateLimit,
    routeSettings_loggingLevel,
    routeSettings_throttlingBurstLimit,
    routeSettings_detailedMetricsEnabled,
    routeSettings_dataTraceEnabled,

    -- ** Stage
    stage_tags,
    stage_accessLogSettings,
    stage_deploymentId,
    stage_autoDeploy,
    stage_lastUpdatedDate,
    stage_stageVariables,
    stage_description,
    stage_clientCertificateId,
    stage_lastDeploymentStatusMessage,
    stage_defaultRouteSettings,
    stage_createdDate,
    stage_apiGatewayManaged,
    stage_routeSettings,
    stage_stageName,

    -- ** TlsConfig
    tlsConfig_serverNameToVerify,

    -- ** TlsConfigInput
    tlsConfigInput_serverNameToVerify,

    -- ** VpcLink
    vpcLink_tags,
    vpcLink_vpcLinkStatusMessage,
    vpcLink_vpcLinkVersion,
    vpcLink_vpcLinkStatus,
    vpcLink_createdDate,
    vpcLink_vpcLinkId,
    vpcLink_securityGroupIds,
    vpcLink_subnetIds,
    vpcLink_name,
  )
where

import Amazonka.ApiGatewayV2.CreateApi
import Amazonka.ApiGatewayV2.CreateApiMapping
import Amazonka.ApiGatewayV2.CreateAuthorizer
import Amazonka.ApiGatewayV2.CreateDeployment
import Amazonka.ApiGatewayV2.CreateDomainName
import Amazonka.ApiGatewayV2.CreateIntegration
import Amazonka.ApiGatewayV2.CreateIntegrationResponse
import Amazonka.ApiGatewayV2.CreateModel
import Amazonka.ApiGatewayV2.CreateRoute
import Amazonka.ApiGatewayV2.CreateRouteResponse
import Amazonka.ApiGatewayV2.CreateStage
import Amazonka.ApiGatewayV2.CreateVpcLink
import Amazonka.ApiGatewayV2.DeleteAccessLogSettings
import Amazonka.ApiGatewayV2.DeleteApi
import Amazonka.ApiGatewayV2.DeleteApiMapping
import Amazonka.ApiGatewayV2.DeleteAuthorizer
import Amazonka.ApiGatewayV2.DeleteCorsConfiguration
import Amazonka.ApiGatewayV2.DeleteDeployment
import Amazonka.ApiGatewayV2.DeleteDomainName
import Amazonka.ApiGatewayV2.DeleteIntegration
import Amazonka.ApiGatewayV2.DeleteIntegrationResponse
import Amazonka.ApiGatewayV2.DeleteModel
import Amazonka.ApiGatewayV2.DeleteRoute
import Amazonka.ApiGatewayV2.DeleteRouteRequestParameter
import Amazonka.ApiGatewayV2.DeleteRouteResponse
import Amazonka.ApiGatewayV2.DeleteRouteSettings
import Amazonka.ApiGatewayV2.DeleteStage
import Amazonka.ApiGatewayV2.DeleteVpcLink
import Amazonka.ApiGatewayV2.ExportApi
import Amazonka.ApiGatewayV2.GetApi
import Amazonka.ApiGatewayV2.GetApiMapping
import Amazonka.ApiGatewayV2.GetApiMappings
import Amazonka.ApiGatewayV2.GetApis
import Amazonka.ApiGatewayV2.GetAuthorizer
import Amazonka.ApiGatewayV2.GetAuthorizers
import Amazonka.ApiGatewayV2.GetDeployment
import Amazonka.ApiGatewayV2.GetDeployments
import Amazonka.ApiGatewayV2.GetDomainName
import Amazonka.ApiGatewayV2.GetDomainNames
import Amazonka.ApiGatewayV2.GetIntegration
import Amazonka.ApiGatewayV2.GetIntegrationResponse
import Amazonka.ApiGatewayV2.GetIntegrationResponses
import Amazonka.ApiGatewayV2.GetIntegrations
import Amazonka.ApiGatewayV2.GetModel
import Amazonka.ApiGatewayV2.GetModelTemplate
import Amazonka.ApiGatewayV2.GetModels
import Amazonka.ApiGatewayV2.GetRoute
import Amazonka.ApiGatewayV2.GetRouteResponse
import Amazonka.ApiGatewayV2.GetRouteResponses
import Amazonka.ApiGatewayV2.GetRoutes
import Amazonka.ApiGatewayV2.GetStage
import Amazonka.ApiGatewayV2.GetStages
import Amazonka.ApiGatewayV2.GetTags
import Amazonka.ApiGatewayV2.GetVpcLink
import Amazonka.ApiGatewayV2.GetVpcLinks
import Amazonka.ApiGatewayV2.ImportApi
import Amazonka.ApiGatewayV2.ReimportApi
import Amazonka.ApiGatewayV2.ResetAuthorizersCache
import Amazonka.ApiGatewayV2.TagResource
import Amazonka.ApiGatewayV2.Types.AccessLogSettings
import Amazonka.ApiGatewayV2.Types.Api
import Amazonka.ApiGatewayV2.Types.ApiMapping
import Amazonka.ApiGatewayV2.Types.Authorizer
import Amazonka.ApiGatewayV2.Types.Cors
import Amazonka.ApiGatewayV2.Types.Deployment
import Amazonka.ApiGatewayV2.Types.DomainName
import Amazonka.ApiGatewayV2.Types.DomainNameConfiguration
import Amazonka.ApiGatewayV2.Types.Integration
import Amazonka.ApiGatewayV2.Types.IntegrationResponse
import Amazonka.ApiGatewayV2.Types.JWTConfiguration
import Amazonka.ApiGatewayV2.Types.Model
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthenticationInput
import Amazonka.ApiGatewayV2.Types.ParameterConstraints
import Amazonka.ApiGatewayV2.Types.Route
import Amazonka.ApiGatewayV2.Types.RouteResponse
import Amazonka.ApiGatewayV2.Types.RouteSettings
import Amazonka.ApiGatewayV2.Types.Stage
import Amazonka.ApiGatewayV2.Types.TlsConfig
import Amazonka.ApiGatewayV2.Types.TlsConfigInput
import Amazonka.ApiGatewayV2.Types.VpcLink
import Amazonka.ApiGatewayV2.UntagResource
import Amazonka.ApiGatewayV2.UpdateApi
import Amazonka.ApiGatewayV2.UpdateApiMapping
import Amazonka.ApiGatewayV2.UpdateAuthorizer
import Amazonka.ApiGatewayV2.UpdateDeployment
import Amazonka.ApiGatewayV2.UpdateDomainName
import Amazonka.ApiGatewayV2.UpdateIntegration
import Amazonka.ApiGatewayV2.UpdateIntegrationResponse
import Amazonka.ApiGatewayV2.UpdateModel
import Amazonka.ApiGatewayV2.UpdateRoute
import Amazonka.ApiGatewayV2.UpdateRouteResponse
import Amazonka.ApiGatewayV2.UpdateStage
import Amazonka.ApiGatewayV2.UpdateVpcLink
