{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Lens
  ( -- * Operations

    -- ** CreateApi
    createApi_apiKeySelectionExpression,
    createApi_corsConfiguration,
    createApi_credentialsArn,
    createApi_description,
    createApi_disableExecuteApiEndpoint,
    createApi_disableSchemaValidation,
    createApi_routeKey,
    createApi_routeSelectionExpression,
    createApi_tags,
    createApi_target,
    createApi_version,
    createApi_protocolType,
    createApi_name,
    createApiResponse_apiEndpoint,
    createApiResponse_apiGatewayManaged,
    createApiResponse_apiId,
    createApiResponse_apiKeySelectionExpression,
    createApiResponse_corsConfiguration,
    createApiResponse_createdDate,
    createApiResponse_description,
    createApiResponse_disableExecuteApiEndpoint,
    createApiResponse_disableSchemaValidation,
    createApiResponse_importInfo,
    createApiResponse_name,
    createApiResponse_protocolType,
    createApiResponse_routeSelectionExpression,
    createApiResponse_tags,
    createApiResponse_version,
    createApiResponse_warnings,
    createApiResponse_httpStatus,

    -- ** CreateApiMapping
    createApiMapping_apiMappingKey,
    createApiMapping_domainName,
    createApiMapping_stage,
    createApiMapping_apiId,
    createApiMappingResponse_apiId,
    createApiMappingResponse_apiMappingId,
    createApiMappingResponse_apiMappingKey,
    createApiMappingResponse_stage,
    createApiMappingResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_authorizerCredentialsArn,
    createAuthorizer_authorizerPayloadFormatVersion,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_authorizerUri,
    createAuthorizer_enableSimpleResponses,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_jwtConfiguration,
    createAuthorizer_apiId,
    createAuthorizer_authorizerType,
    createAuthorizer_identitySource,
    createAuthorizer_name,
    createAuthorizerResponse_authorizerCredentialsArn,
    createAuthorizerResponse_authorizerId,
    createAuthorizerResponse_authorizerPayloadFormatVersion,
    createAuthorizerResponse_authorizerResultTtlInSeconds,
    createAuthorizerResponse_authorizerType,
    createAuthorizerResponse_authorizerUri,
    createAuthorizerResponse_enableSimpleResponses,
    createAuthorizerResponse_identitySource,
    createAuthorizerResponse_identityValidationExpression,
    createAuthorizerResponse_jwtConfiguration,
    createAuthorizerResponse_name,
    createAuthorizerResponse_httpStatus,

    -- ** CreateDeployment
    createDeployment_description,
    createDeployment_stageName,
    createDeployment_apiId,
    createDeploymentResponse_autoDeployed,
    createDeploymentResponse_createdDate,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_deploymentStatus,
    createDeploymentResponse_deploymentStatusMessage,
    createDeploymentResponse_description,
    createDeploymentResponse_httpStatus,

    -- ** CreateDomainName
    createDomainName_domainNameConfigurations,
    createDomainName_mutualTlsAuthentication,
    createDomainName_tags,
    createDomainName_domainName,
    createDomainNameResponse_apiMappingSelectionExpression,
    createDomainNameResponse_domainName,
    createDomainNameResponse_domainNameConfigurations,
    createDomainNameResponse_mutualTlsAuthentication,
    createDomainNameResponse_tags,
    createDomainNameResponse_httpStatus,

    -- ** CreateIntegration
    createIntegration_connectionId,
    createIntegration_connectionType,
    createIntegration_contentHandlingStrategy,
    createIntegration_credentialsArn,
    createIntegration_description,
    createIntegration_integrationMethod,
    createIntegration_integrationSubtype,
    createIntegration_integrationUri,
    createIntegration_passthroughBehavior,
    createIntegration_payloadFormatVersion,
    createIntegration_requestParameters,
    createIntegration_requestTemplates,
    createIntegration_responseParameters,
    createIntegration_templateSelectionExpression,
    createIntegration_timeoutInMillis,
    createIntegration_tlsConfig,
    createIntegration_apiId,
    createIntegration_integrationType,
    createIntegrationResponse'_apiGatewayManaged,
    createIntegrationResponse'_connectionId,
    createIntegrationResponse'_connectionType,
    createIntegrationResponse'_contentHandlingStrategy,
    createIntegrationResponse'_credentialsArn,
    createIntegrationResponse'_description,
    createIntegrationResponse'_integrationId,
    createIntegrationResponse'_integrationMethod,
    createIntegrationResponse'_integrationResponseSelectionExpression,
    createIntegrationResponse'_integrationSubtype,
    createIntegrationResponse'_integrationType,
    createIntegrationResponse'_integrationUri,
    createIntegrationResponse'_passthroughBehavior,
    createIntegrationResponse'_payloadFormatVersion,
    createIntegrationResponse'_requestParameters,
    createIntegrationResponse'_requestTemplates,
    createIntegrationResponse'_responseParameters,
    createIntegrationResponse'_templateSelectionExpression,
    createIntegrationResponse'_timeoutInMillis,
    createIntegrationResponse'_tlsConfig,
    createIntegrationResponse'_httpStatus,

    -- ** CreateIntegrationResponse
    createIntegrationResponse_contentHandlingStrategy,
    createIntegrationResponse_responseParameters,
    createIntegrationResponse_responseTemplates,
    createIntegrationResponse_templateSelectionExpression,
    createIntegrationResponse_apiId,
    createIntegrationResponse_integrationId,
    createIntegrationResponse_integrationResponseKey,
    createIntegrationResponseResponse_contentHandlingStrategy,
    createIntegrationResponseResponse_integrationResponseId,
    createIntegrationResponseResponse_integrationResponseKey,
    createIntegrationResponseResponse_responseParameters,
    createIntegrationResponseResponse_responseTemplates,
    createIntegrationResponseResponse_templateSelectionExpression,
    createIntegrationResponseResponse_httpStatus,

    -- ** CreateModel
    createModel_contentType,
    createModel_description,
    createModel_apiId,
    createModel_schema,
    createModel_name,
    createModelResponse_contentType,
    createModelResponse_description,
    createModelResponse_modelId,
    createModelResponse_name,
    createModelResponse_schema,
    createModelResponse_httpStatus,

    -- ** CreateRoute
    createRoute_apiKeyRequired,
    createRoute_authorizationScopes,
    createRoute_authorizationType,
    createRoute_authorizerId,
    createRoute_modelSelectionExpression,
    createRoute_operationName,
    createRoute_requestModels,
    createRoute_requestParameters,
    createRoute_routeResponseSelectionExpression,
    createRoute_target,
    createRoute_apiId,
    createRoute_routeKey,
    createRouteResponse'_apiGatewayManaged,
    createRouteResponse'_apiKeyRequired,
    createRouteResponse'_authorizationScopes,
    createRouteResponse'_authorizationType,
    createRouteResponse'_authorizerId,
    createRouteResponse'_modelSelectionExpression,
    createRouteResponse'_operationName,
    createRouteResponse'_requestModels,
    createRouteResponse'_requestParameters,
    createRouteResponse'_routeId,
    createRouteResponse'_routeKey,
    createRouteResponse'_routeResponseSelectionExpression,
    createRouteResponse'_target,
    createRouteResponse'_httpStatus,

    -- ** CreateRouteResponse
    createRouteResponse_modelSelectionExpression,
    createRouteResponse_responseModels,
    createRouteResponse_responseParameters,
    createRouteResponse_apiId,
    createRouteResponse_routeId,
    createRouteResponse_routeResponseKey,
    createRouteResponseResponse_modelSelectionExpression,
    createRouteResponseResponse_responseModels,
    createRouteResponseResponse_responseParameters,
    createRouteResponseResponse_routeResponseId,
    createRouteResponseResponse_routeResponseKey,
    createRouteResponseResponse_httpStatus,

    -- ** CreateStage
    createStage_accessLogSettings,
    createStage_autoDeploy,
    createStage_clientCertificateId,
    createStage_defaultRouteSettings,
    createStage_deploymentId,
    createStage_description,
    createStage_routeSettings,
    createStage_stageVariables,
    createStage_tags,
    createStage_apiId,
    createStage_stageName,
    createStageResponse_accessLogSettings,
    createStageResponse_apiGatewayManaged,
    createStageResponse_autoDeploy,
    createStageResponse_clientCertificateId,
    createStageResponse_createdDate,
    createStageResponse_defaultRouteSettings,
    createStageResponse_deploymentId,
    createStageResponse_description,
    createStageResponse_lastDeploymentStatusMessage,
    createStageResponse_lastUpdatedDate,
    createStageResponse_routeSettings,
    createStageResponse_stageName,
    createStageResponse_stageVariables,
    createStageResponse_tags,
    createStageResponse_httpStatus,

    -- ** CreateVpcLink
    createVpcLink_securityGroupIds,
    createVpcLink_tags,
    createVpcLink_subnetIds,
    createVpcLink_name,
    createVpcLinkResponse_createdDate,
    createVpcLinkResponse_name,
    createVpcLinkResponse_securityGroupIds,
    createVpcLinkResponse_subnetIds,
    createVpcLinkResponse_tags,
    createVpcLinkResponse_vpcLinkId,
    createVpcLinkResponse_vpcLinkStatus,
    createVpcLinkResponse_vpcLinkStatusMessage,
    createVpcLinkResponse_vpcLinkVersion,
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
    exportApi_exportVersion,
    exportApi_includeExtensions,
    exportApi_stageName,
    exportApi_specification,
    exportApi_outputType,
    exportApi_apiId,
    exportApiResponse_body,
    exportApiResponse_httpStatus,

    -- ** GetApi
    getApi_apiId,
    getApiResponse_apiEndpoint,
    getApiResponse_apiGatewayManaged,
    getApiResponse_apiId,
    getApiResponse_apiKeySelectionExpression,
    getApiResponse_corsConfiguration,
    getApiResponse_createdDate,
    getApiResponse_description,
    getApiResponse_disableExecuteApiEndpoint,
    getApiResponse_disableSchemaValidation,
    getApiResponse_importInfo,
    getApiResponse_name,
    getApiResponse_protocolType,
    getApiResponse_routeSelectionExpression,
    getApiResponse_tags,
    getApiResponse_version,
    getApiResponse_warnings,
    getApiResponse_httpStatus,

    -- ** GetApiMapping
    getApiMapping_apiMappingId,
    getApiMapping_domainName,
    getApiMappingResponse_apiId,
    getApiMappingResponse_apiMappingId,
    getApiMappingResponse_apiMappingKey,
    getApiMappingResponse_stage,
    getApiMappingResponse_httpStatus,

    -- ** GetApiMappings
    getApiMappings_maxResults,
    getApiMappings_nextToken,
    getApiMappings_domainName,
    getApiMappingsResponse_items,
    getApiMappingsResponse_nextToken,
    getApiMappingsResponse_httpStatus,

    -- ** GetApis
    getApis_maxResults,
    getApis_nextToken,
    getApisResponse_items,
    getApisResponse_nextToken,
    getApisResponse_httpStatus,

    -- ** GetAuthorizer
    getAuthorizer_authorizerId,
    getAuthorizer_apiId,
    getAuthorizerResponse_authorizerCredentialsArn,
    getAuthorizerResponse_authorizerId,
    getAuthorizerResponse_authorizerPayloadFormatVersion,
    getAuthorizerResponse_authorizerResultTtlInSeconds,
    getAuthorizerResponse_authorizerType,
    getAuthorizerResponse_authorizerUri,
    getAuthorizerResponse_enableSimpleResponses,
    getAuthorizerResponse_identitySource,
    getAuthorizerResponse_identityValidationExpression,
    getAuthorizerResponse_jwtConfiguration,
    getAuthorizerResponse_name,
    getAuthorizerResponse_httpStatus,

    -- ** GetAuthorizers
    getAuthorizers_maxResults,
    getAuthorizers_nextToken,
    getAuthorizers_apiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_nextToken,
    getAuthorizersResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_apiId,
    getDeployment_deploymentId,
    getDeploymentResponse_autoDeployed,
    getDeploymentResponse_createdDate,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_deploymentStatusMessage,
    getDeploymentResponse_description,
    getDeploymentResponse_httpStatus,

    -- ** GetDeployments
    getDeployments_maxResults,
    getDeployments_nextToken,
    getDeployments_apiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_nextToken,
    getDeploymentsResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    getDomainNameResponse_apiMappingSelectionExpression,
    getDomainNameResponse_domainName,
    getDomainNameResponse_domainNameConfigurations,
    getDomainNameResponse_mutualTlsAuthentication,
    getDomainNameResponse_tags,
    getDomainNameResponse_httpStatus,

    -- ** GetDomainNames
    getDomainNames_maxResults,
    getDomainNames_nextToken,
    getDomainNamesResponse_items,
    getDomainNamesResponse_nextToken,
    getDomainNamesResponse_httpStatus,

    -- ** GetIntegration
    getIntegration_apiId,
    getIntegration_integrationId,
    getIntegrationResponse'_apiGatewayManaged,
    getIntegrationResponse'_connectionId,
    getIntegrationResponse'_connectionType,
    getIntegrationResponse'_contentHandlingStrategy,
    getIntegrationResponse'_credentialsArn,
    getIntegrationResponse'_description,
    getIntegrationResponse'_integrationId,
    getIntegrationResponse'_integrationMethod,
    getIntegrationResponse'_integrationResponseSelectionExpression,
    getIntegrationResponse'_integrationSubtype,
    getIntegrationResponse'_integrationType,
    getIntegrationResponse'_integrationUri,
    getIntegrationResponse'_passthroughBehavior,
    getIntegrationResponse'_payloadFormatVersion,
    getIntegrationResponse'_requestParameters,
    getIntegrationResponse'_requestTemplates,
    getIntegrationResponse'_responseParameters,
    getIntegrationResponse'_templateSelectionExpression,
    getIntegrationResponse'_timeoutInMillis,
    getIntegrationResponse'_tlsConfig,
    getIntegrationResponse'_httpStatus,

    -- ** GetIntegrationResponse
    getIntegrationResponse_apiId,
    getIntegrationResponse_integrationResponseId,
    getIntegrationResponse_integrationId,
    getIntegrationResponseResponse_contentHandlingStrategy,
    getIntegrationResponseResponse_integrationResponseId,
    getIntegrationResponseResponse_integrationResponseKey,
    getIntegrationResponseResponse_responseParameters,
    getIntegrationResponseResponse_responseTemplates,
    getIntegrationResponseResponse_templateSelectionExpression,
    getIntegrationResponseResponse_httpStatus,

    -- ** GetIntegrationResponses
    getIntegrationResponses_maxResults,
    getIntegrationResponses_nextToken,
    getIntegrationResponses_integrationId,
    getIntegrationResponses_apiId,
    getIntegrationResponsesResponse_items,
    getIntegrationResponsesResponse_nextToken,
    getIntegrationResponsesResponse_httpStatus,

    -- ** GetIntegrations
    getIntegrations_maxResults,
    getIntegrations_nextToken,
    getIntegrations_apiId,
    getIntegrationsResponse_items,
    getIntegrationsResponse_nextToken,
    getIntegrationsResponse_httpStatus,

    -- ** GetModel
    getModel_modelId,
    getModel_apiId,
    getModelResponse_contentType,
    getModelResponse_description,
    getModelResponse_modelId,
    getModelResponse_name,
    getModelResponse_schema,
    getModelResponse_httpStatus,

    -- ** GetModelTemplate
    getModelTemplate_modelId,
    getModelTemplate_apiId,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- ** GetModels
    getModels_maxResults,
    getModels_nextToken,
    getModels_apiId,
    getModelsResponse_items,
    getModelsResponse_nextToken,
    getModelsResponse_httpStatus,

    -- ** GetRoute
    getRoute_apiId,
    getRoute_routeId,
    getRouteResponse'_apiGatewayManaged,
    getRouteResponse'_apiKeyRequired,
    getRouteResponse'_authorizationScopes,
    getRouteResponse'_authorizationType,
    getRouteResponse'_authorizerId,
    getRouteResponse'_modelSelectionExpression,
    getRouteResponse'_operationName,
    getRouteResponse'_requestModels,
    getRouteResponse'_requestParameters,
    getRouteResponse'_routeId,
    getRouteResponse'_routeKey,
    getRouteResponse'_routeResponseSelectionExpression,
    getRouteResponse'_target,
    getRouteResponse'_httpStatus,

    -- ** GetRouteResponse
    getRouteResponse_routeResponseId,
    getRouteResponse_apiId,
    getRouteResponse_routeId,
    getRouteResponseResponse_modelSelectionExpression,
    getRouteResponseResponse_responseModels,
    getRouteResponseResponse_responseParameters,
    getRouteResponseResponse_routeResponseId,
    getRouteResponseResponse_routeResponseKey,
    getRouteResponseResponse_httpStatus,

    -- ** GetRouteResponses
    getRouteResponses_maxResults,
    getRouteResponses_nextToken,
    getRouteResponses_routeId,
    getRouteResponses_apiId,
    getRouteResponsesResponse_items,
    getRouteResponsesResponse_nextToken,
    getRouteResponsesResponse_httpStatus,

    -- ** GetRoutes
    getRoutes_maxResults,
    getRoutes_nextToken,
    getRoutes_apiId,
    getRoutesResponse_items,
    getRoutesResponse_nextToken,
    getRoutesResponse_httpStatus,

    -- ** GetStage
    getStage_stageName,
    getStage_apiId,
    getStageResponse_accessLogSettings,
    getStageResponse_apiGatewayManaged,
    getStageResponse_autoDeploy,
    getStageResponse_clientCertificateId,
    getStageResponse_createdDate,
    getStageResponse_defaultRouteSettings,
    getStageResponse_deploymentId,
    getStageResponse_description,
    getStageResponse_lastDeploymentStatusMessage,
    getStageResponse_lastUpdatedDate,
    getStageResponse_routeSettings,
    getStageResponse_stageName,
    getStageResponse_stageVariables,
    getStageResponse_tags,
    getStageResponse_httpStatus,

    -- ** GetStages
    getStages_maxResults,
    getStages_nextToken,
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
    getVpcLinkResponse_createdDate,
    getVpcLinkResponse_name,
    getVpcLinkResponse_securityGroupIds,
    getVpcLinkResponse_subnetIds,
    getVpcLinkResponse_tags,
    getVpcLinkResponse_vpcLinkId,
    getVpcLinkResponse_vpcLinkStatus,
    getVpcLinkResponse_vpcLinkStatusMessage,
    getVpcLinkResponse_vpcLinkVersion,
    getVpcLinkResponse_httpStatus,

    -- ** GetVpcLinks
    getVpcLinks_maxResults,
    getVpcLinks_nextToken,
    getVpcLinksResponse_items,
    getVpcLinksResponse_nextToken,
    getVpcLinksResponse_httpStatus,

    -- ** ImportApi
    importApi_basepath,
    importApi_failOnWarnings,
    importApi_body,
    importApiResponse_apiEndpoint,
    importApiResponse_apiGatewayManaged,
    importApiResponse_apiId,
    importApiResponse_apiKeySelectionExpression,
    importApiResponse_corsConfiguration,
    importApiResponse_createdDate,
    importApiResponse_description,
    importApiResponse_disableExecuteApiEndpoint,
    importApiResponse_disableSchemaValidation,
    importApiResponse_importInfo,
    importApiResponse_name,
    importApiResponse_protocolType,
    importApiResponse_routeSelectionExpression,
    importApiResponse_tags,
    importApiResponse_version,
    importApiResponse_warnings,
    importApiResponse_httpStatus,

    -- ** ReimportApi
    reimportApi_basepath,
    reimportApi_failOnWarnings,
    reimportApi_apiId,
    reimportApi_body,
    reimportApiResponse_apiEndpoint,
    reimportApiResponse_apiGatewayManaged,
    reimportApiResponse_apiId,
    reimportApiResponse_apiKeySelectionExpression,
    reimportApiResponse_corsConfiguration,
    reimportApiResponse_createdDate,
    reimportApiResponse_description,
    reimportApiResponse_disableExecuteApiEndpoint,
    reimportApiResponse_disableSchemaValidation,
    reimportApiResponse_importInfo,
    reimportApiResponse_name,
    reimportApiResponse_protocolType,
    reimportApiResponse_routeSelectionExpression,
    reimportApiResponse_tags,
    reimportApiResponse_version,
    reimportApiResponse_warnings,
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
    updateApi_apiKeySelectionExpression,
    updateApi_corsConfiguration,
    updateApi_credentialsArn,
    updateApi_description,
    updateApi_disableExecuteApiEndpoint,
    updateApi_disableSchemaValidation,
    updateApi_name,
    updateApi_routeKey,
    updateApi_routeSelectionExpression,
    updateApi_target,
    updateApi_version,
    updateApi_apiId,
    updateApiResponse_apiEndpoint,
    updateApiResponse_apiGatewayManaged,
    updateApiResponse_apiId,
    updateApiResponse_apiKeySelectionExpression,
    updateApiResponse_corsConfiguration,
    updateApiResponse_createdDate,
    updateApiResponse_description,
    updateApiResponse_disableExecuteApiEndpoint,
    updateApiResponse_disableSchemaValidation,
    updateApiResponse_importInfo,
    updateApiResponse_name,
    updateApiResponse_protocolType,
    updateApiResponse_routeSelectionExpression,
    updateApiResponse_tags,
    updateApiResponse_version,
    updateApiResponse_warnings,
    updateApiResponse_httpStatus,

    -- ** UpdateApiMapping
    updateApiMapping_apiMappingKey,
    updateApiMapping_stage,
    updateApiMapping_apiMappingId,
    updateApiMapping_apiId,
    updateApiMapping_domainName,
    updateApiMappingResponse_apiId,
    updateApiMappingResponse_apiMappingId,
    updateApiMappingResponse_apiMappingKey,
    updateApiMappingResponse_stage,
    updateApiMappingResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_authorizerCredentialsArn,
    updateAuthorizer_authorizerPayloadFormatVersion,
    updateAuthorizer_authorizerResultTtlInSeconds,
    updateAuthorizer_authorizerType,
    updateAuthorizer_authorizerUri,
    updateAuthorizer_enableSimpleResponses,
    updateAuthorizer_identitySource,
    updateAuthorizer_identityValidationExpression,
    updateAuthorizer_jwtConfiguration,
    updateAuthorizer_name,
    updateAuthorizer_authorizerId,
    updateAuthorizer_apiId,
    updateAuthorizerResponse_authorizerCredentialsArn,
    updateAuthorizerResponse_authorizerId,
    updateAuthorizerResponse_authorizerPayloadFormatVersion,
    updateAuthorizerResponse_authorizerResultTtlInSeconds,
    updateAuthorizerResponse_authorizerType,
    updateAuthorizerResponse_authorizerUri,
    updateAuthorizerResponse_enableSimpleResponses,
    updateAuthorizerResponse_identitySource,
    updateAuthorizerResponse_identityValidationExpression,
    updateAuthorizerResponse_jwtConfiguration,
    updateAuthorizerResponse_name,
    updateAuthorizerResponse_httpStatus,

    -- ** UpdateDeployment
    updateDeployment_description,
    updateDeployment_apiId,
    updateDeployment_deploymentId,
    updateDeploymentResponse_autoDeployed,
    updateDeploymentResponse_createdDate,
    updateDeploymentResponse_deploymentId,
    updateDeploymentResponse_deploymentStatus,
    updateDeploymentResponse_deploymentStatusMessage,
    updateDeploymentResponse_description,
    updateDeploymentResponse_httpStatus,

    -- ** UpdateDomainName
    updateDomainName_domainNameConfigurations,
    updateDomainName_mutualTlsAuthentication,
    updateDomainName_domainName,
    updateDomainNameResponse_apiMappingSelectionExpression,
    updateDomainNameResponse_domainName,
    updateDomainNameResponse_domainNameConfigurations,
    updateDomainNameResponse_mutualTlsAuthentication,
    updateDomainNameResponse_tags,
    updateDomainNameResponse_httpStatus,

    -- ** UpdateIntegration
    updateIntegration_connectionId,
    updateIntegration_connectionType,
    updateIntegration_contentHandlingStrategy,
    updateIntegration_credentialsArn,
    updateIntegration_description,
    updateIntegration_integrationMethod,
    updateIntegration_integrationSubtype,
    updateIntegration_integrationType,
    updateIntegration_integrationUri,
    updateIntegration_passthroughBehavior,
    updateIntegration_payloadFormatVersion,
    updateIntegration_requestParameters,
    updateIntegration_requestTemplates,
    updateIntegration_responseParameters,
    updateIntegration_templateSelectionExpression,
    updateIntegration_timeoutInMillis,
    updateIntegration_tlsConfig,
    updateIntegration_apiId,
    updateIntegration_integrationId,
    updateIntegrationResponse'_apiGatewayManaged,
    updateIntegrationResponse'_connectionId,
    updateIntegrationResponse'_connectionType,
    updateIntegrationResponse'_contentHandlingStrategy,
    updateIntegrationResponse'_credentialsArn,
    updateIntegrationResponse'_description,
    updateIntegrationResponse'_integrationId,
    updateIntegrationResponse'_integrationMethod,
    updateIntegrationResponse'_integrationResponseSelectionExpression,
    updateIntegrationResponse'_integrationSubtype,
    updateIntegrationResponse'_integrationType,
    updateIntegrationResponse'_integrationUri,
    updateIntegrationResponse'_passthroughBehavior,
    updateIntegrationResponse'_payloadFormatVersion,
    updateIntegrationResponse'_requestParameters,
    updateIntegrationResponse'_requestTemplates,
    updateIntegrationResponse'_responseParameters,
    updateIntegrationResponse'_templateSelectionExpression,
    updateIntegrationResponse'_timeoutInMillis,
    updateIntegrationResponse'_tlsConfig,
    updateIntegrationResponse'_httpStatus,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_contentHandlingStrategy,
    updateIntegrationResponse_integrationResponseKey,
    updateIntegrationResponse_responseParameters,
    updateIntegrationResponse_responseTemplates,
    updateIntegrationResponse_templateSelectionExpression,
    updateIntegrationResponse_apiId,
    updateIntegrationResponse_integrationResponseId,
    updateIntegrationResponse_integrationId,
    updateIntegrationResponseResponse_contentHandlingStrategy,
    updateIntegrationResponseResponse_integrationResponseId,
    updateIntegrationResponseResponse_integrationResponseKey,
    updateIntegrationResponseResponse_responseParameters,
    updateIntegrationResponseResponse_responseTemplates,
    updateIntegrationResponseResponse_templateSelectionExpression,
    updateIntegrationResponseResponse_httpStatus,

    -- ** UpdateModel
    updateModel_contentType,
    updateModel_description,
    updateModel_name,
    updateModel_schema,
    updateModel_modelId,
    updateModel_apiId,
    updateModelResponse_contentType,
    updateModelResponse_description,
    updateModelResponse_modelId,
    updateModelResponse_name,
    updateModelResponse_schema,
    updateModelResponse_httpStatus,

    -- ** UpdateRoute
    updateRoute_apiKeyRequired,
    updateRoute_authorizationScopes,
    updateRoute_authorizationType,
    updateRoute_authorizerId,
    updateRoute_modelSelectionExpression,
    updateRoute_operationName,
    updateRoute_requestModels,
    updateRoute_requestParameters,
    updateRoute_routeKey,
    updateRoute_routeResponseSelectionExpression,
    updateRoute_target,
    updateRoute_apiId,
    updateRoute_routeId,
    updateRouteResponse'_apiGatewayManaged,
    updateRouteResponse'_apiKeyRequired,
    updateRouteResponse'_authorizationScopes,
    updateRouteResponse'_authorizationType,
    updateRouteResponse'_authorizerId,
    updateRouteResponse'_modelSelectionExpression,
    updateRouteResponse'_operationName,
    updateRouteResponse'_requestModels,
    updateRouteResponse'_requestParameters,
    updateRouteResponse'_routeId,
    updateRouteResponse'_routeKey,
    updateRouteResponse'_routeResponseSelectionExpression,
    updateRouteResponse'_target,
    updateRouteResponse'_httpStatus,

    -- ** UpdateRouteResponse
    updateRouteResponse_modelSelectionExpression,
    updateRouteResponse_responseModels,
    updateRouteResponse_responseParameters,
    updateRouteResponse_routeResponseKey,
    updateRouteResponse_routeResponseId,
    updateRouteResponse_apiId,
    updateRouteResponse_routeId,
    updateRouteResponseResponse_modelSelectionExpression,
    updateRouteResponseResponse_responseModels,
    updateRouteResponseResponse_responseParameters,
    updateRouteResponseResponse_routeResponseId,
    updateRouteResponseResponse_routeResponseKey,
    updateRouteResponseResponse_httpStatus,

    -- ** UpdateStage
    updateStage_accessLogSettings,
    updateStage_autoDeploy,
    updateStage_clientCertificateId,
    updateStage_defaultRouteSettings,
    updateStage_deploymentId,
    updateStage_description,
    updateStage_routeSettings,
    updateStage_stageVariables,
    updateStage_stageName,
    updateStage_apiId,
    updateStageResponse_accessLogSettings,
    updateStageResponse_apiGatewayManaged,
    updateStageResponse_autoDeploy,
    updateStageResponse_clientCertificateId,
    updateStageResponse_createdDate,
    updateStageResponse_defaultRouteSettings,
    updateStageResponse_deploymentId,
    updateStageResponse_description,
    updateStageResponse_lastDeploymentStatusMessage,
    updateStageResponse_lastUpdatedDate,
    updateStageResponse_routeSettings,
    updateStageResponse_stageName,
    updateStageResponse_stageVariables,
    updateStageResponse_tags,
    updateStageResponse_httpStatus,

    -- ** UpdateVpcLink
    updateVpcLink_name,
    updateVpcLink_vpcLinkId,
    updateVpcLinkResponse_createdDate,
    updateVpcLinkResponse_name,
    updateVpcLinkResponse_securityGroupIds,
    updateVpcLinkResponse_subnetIds,
    updateVpcLinkResponse_tags,
    updateVpcLinkResponse_vpcLinkId,
    updateVpcLinkResponse_vpcLinkStatus,
    updateVpcLinkResponse_vpcLinkStatusMessage,
    updateVpcLinkResponse_vpcLinkVersion,
    updateVpcLinkResponse_httpStatus,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_destinationArn,
    accessLogSettings_format,

    -- ** Api
    api_apiEndpoint,
    api_apiGatewayManaged,
    api_apiId,
    api_apiKeySelectionExpression,
    api_corsConfiguration,
    api_createdDate,
    api_description,
    api_disableExecuteApiEndpoint,
    api_disableSchemaValidation,
    api_importInfo,
    api_tags,
    api_version,
    api_warnings,
    api_routeSelectionExpression,
    api_name,
    api_protocolType,

    -- ** ApiMapping
    apiMapping_apiMappingId,
    apiMapping_apiMappingKey,
    apiMapping_stage,
    apiMapping_apiId,

    -- ** Authorizer
    authorizer_authorizerCredentialsArn,
    authorizer_authorizerId,
    authorizer_authorizerPayloadFormatVersion,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerType,
    authorizer_authorizerUri,
    authorizer_enableSimpleResponses,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_jwtConfiguration,
    authorizer_name,

    -- ** Cors
    cors_allowCredentials,
    cors_allowHeaders,
    cors_allowMethods,
    cors_allowOrigins,
    cors_exposeHeaders,
    cors_maxAge,

    -- ** Deployment
    deployment_autoDeployed,
    deployment_createdDate,
    deployment_deploymentId,
    deployment_deploymentStatus,
    deployment_deploymentStatusMessage,
    deployment_description,

    -- ** DomainName
    domainName_apiMappingSelectionExpression,
    domainName_domainNameConfigurations,
    domainName_mutualTlsAuthentication,
    domainName_tags,
    domainName_domainName,

    -- ** DomainNameConfiguration
    domainNameConfiguration_apiGatewayDomainName,
    domainNameConfiguration_certificateArn,
    domainNameConfiguration_certificateName,
    domainNameConfiguration_certificateUploadDate,
    domainNameConfiguration_domainNameStatus,
    domainNameConfiguration_domainNameStatusMessage,
    domainNameConfiguration_endpointType,
    domainNameConfiguration_hostedZoneId,
    domainNameConfiguration_ownershipVerificationCertificateArn,
    domainNameConfiguration_securityPolicy,

    -- ** Integration
    integration_apiGatewayManaged,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandlingStrategy,
    integration_credentialsArn,
    integration_description,
    integration_integrationId,
    integration_integrationMethod,
    integration_integrationResponseSelectionExpression,
    integration_integrationSubtype,
    integration_integrationType,
    integration_integrationUri,
    integration_passthroughBehavior,
    integration_payloadFormatVersion,
    integration_requestParameters,
    integration_requestTemplates,
    integration_responseParameters,
    integration_templateSelectionExpression,
    integration_timeoutInMillis,
    integration_tlsConfig,

    -- ** IntegrationResponse
    integrationResponse_contentHandlingStrategy,
    integrationResponse_integrationResponseId,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_templateSelectionExpression,
    integrationResponse_integrationResponseKey,

    -- ** JWTConfiguration
    jWTConfiguration_audience,
    jWTConfiguration_issuer,

    -- ** Model
    model_contentType,
    model_description,
    model_modelId,
    model_schema,
    model_name,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreWarnings,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

    -- ** ParameterConstraints
    parameterConstraints_required,

    -- ** Route
    route_apiGatewayManaged,
    route_apiKeyRequired,
    route_authorizationScopes,
    route_authorizationType,
    route_authorizerId,
    route_modelSelectionExpression,
    route_operationName,
    route_requestModels,
    route_requestParameters,
    route_routeId,
    route_routeResponseSelectionExpression,
    route_target,
    route_routeKey,

    -- ** RouteResponse
    routeResponse_modelSelectionExpression,
    routeResponse_responseModels,
    routeResponse_responseParameters,
    routeResponse_routeResponseId,
    routeResponse_routeResponseKey,

    -- ** RouteSettings
    routeSettings_dataTraceEnabled,
    routeSettings_detailedMetricsEnabled,
    routeSettings_loggingLevel,
    routeSettings_throttlingBurstLimit,
    routeSettings_throttlingRateLimit,

    -- ** Stage
    stage_accessLogSettings,
    stage_apiGatewayManaged,
    stage_autoDeploy,
    stage_clientCertificateId,
    stage_createdDate,
    stage_defaultRouteSettings,
    stage_deploymentId,
    stage_description,
    stage_lastDeploymentStatusMessage,
    stage_lastUpdatedDate,
    stage_routeSettings,
    stage_stageVariables,
    stage_tags,
    stage_stageName,

    -- ** TlsConfig
    tlsConfig_serverNameToVerify,

    -- ** TlsConfigInput
    tlsConfigInput_serverNameToVerify,

    -- ** VpcLink
    vpcLink_createdDate,
    vpcLink_tags,
    vpcLink_vpcLinkStatus,
    vpcLink_vpcLinkStatusMessage,
    vpcLink_vpcLinkVersion,
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
