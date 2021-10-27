{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Lens
  ( -- * Operations

    -- ** CreateApi
    createApi_credentialsArn,
    createApi_disableExecuteApiEndpoint,
    createApi_version,
    createApi_apiKeySelectionExpression,
    createApi_corsConfiguration,
    createApi_routeSelectionExpression,
    createApi_disableSchemaValidation,
    createApi_description,
    createApi_routeKey,
    createApi_tags,
    createApi_target,
    createApi_protocolType,
    createApi_name,
    createApiResponse_apiId,
    createApiResponse_disableExecuteApiEndpoint,
    createApiResponse_apiEndpoint,
    createApiResponse_warnings,
    createApiResponse_createdDate,
    createApiResponse_name,
    createApiResponse_version,
    createApiResponse_apiGatewayManaged,
    createApiResponse_apiKeySelectionExpression,
    createApiResponse_corsConfiguration,
    createApiResponse_routeSelectionExpression,
    createApiResponse_importInfo,
    createApiResponse_disableSchemaValidation,
    createApiResponse_description,
    createApiResponse_protocolType,
    createApiResponse_tags,
    createApiResponse_httpStatus,

    -- ** GetDeployments
    getDeployments_nextToken,
    getDeployments_maxResults,
    getDeployments_apiId,
    getDeploymentsResponse_items,
    getDeploymentsResponse_nextToken,
    getDeploymentsResponse_httpStatus,

    -- ** DeleteAccessLogSettings
    deleteAccessLogSettings_stageName,
    deleteAccessLogSettings_apiId,

    -- ** GetRouteResponses
    getRouteResponses_nextToken,
    getRouteResponses_maxResults,
    getRouteResponses_routeId,
    getRouteResponses_apiId,
    getRouteResponsesResponse_items,
    getRouteResponsesResponse_nextToken,
    getRouteResponsesResponse_httpStatus,

    -- ** GetDeployment
    getDeployment_apiId,
    getDeployment_deploymentId,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_autoDeployed,
    getDeploymentResponse_deploymentStatusMessage,
    getDeploymentResponse_createdDate,
    getDeploymentResponse_deploymentStatus,
    getDeploymentResponse_description,
    getDeploymentResponse_httpStatus,

    -- ** GetTags
    getTags_resourceArn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** GetDomainNames
    getDomainNames_nextToken,
    getDomainNames_maxResults,
    getDomainNamesResponse_items,
    getDomainNamesResponse_nextToken,
    getDomainNamesResponse_httpStatus,

    -- ** ReimportApi
    reimportApi_basepath,
    reimportApi_failOnWarnings,
    reimportApi_apiId,
    reimportApi_body,
    reimportApiResponse_apiId,
    reimportApiResponse_disableExecuteApiEndpoint,
    reimportApiResponse_apiEndpoint,
    reimportApiResponse_warnings,
    reimportApiResponse_createdDate,
    reimportApiResponse_name,
    reimportApiResponse_version,
    reimportApiResponse_apiGatewayManaged,
    reimportApiResponse_apiKeySelectionExpression,
    reimportApiResponse_corsConfiguration,
    reimportApiResponse_routeSelectionExpression,
    reimportApiResponse_importInfo,
    reimportApiResponse_disableSchemaValidation,
    reimportApiResponse_description,
    reimportApiResponse_protocolType,
    reimportApiResponse_tags,
    reimportApiResponse_httpStatus,

    -- ** GetModels
    getModels_nextToken,
    getModels_maxResults,
    getModels_apiId,
    getModelsResponse_items,
    getModelsResponse_nextToken,
    getModelsResponse_httpStatus,

    -- ** CreateIntegration
    createIntegration_requestTemplates,
    createIntegration_integrationSubtype,
    createIntegration_credentialsArn,
    createIntegration_integrationUri,
    createIntegration_requestParameters,
    createIntegration_connectionId,
    createIntegration_passthroughBehavior,
    createIntegration_integrationMethod,
    createIntegration_tlsConfig,
    createIntegration_payloadFormatVersion,
    createIntegration_templateSelectionExpression,
    createIntegration_timeoutInMillis,
    createIntegration_contentHandlingStrategy,
    createIntegration_description,
    createIntegration_connectionType,
    createIntegration_responseParameters,
    createIntegration_apiId,
    createIntegration_integrationType,
    createIntegrationResponse'_integrationResponseSelectionExpression,
    createIntegrationResponse'_requestTemplates,
    createIntegrationResponse'_integrationSubtype,
    createIntegrationResponse'_credentialsArn,
    createIntegrationResponse'_integrationUri,
    createIntegrationResponse'_integrationId,
    createIntegrationResponse'_requestParameters,
    createIntegrationResponse'_connectionId,
    createIntegrationResponse'_passthroughBehavior,
    createIntegrationResponse'_integrationMethod,
    createIntegrationResponse'_tlsConfig,
    createIntegrationResponse'_payloadFormatVersion,
    createIntegrationResponse'_templateSelectionExpression,
    createIntegrationResponse'_timeoutInMillis,
    createIntegrationResponse'_apiGatewayManaged,
    createIntegrationResponse'_contentHandlingStrategy,
    createIntegrationResponse'_integrationType,
    createIntegrationResponse'_description,
    createIntegrationResponse'_connectionType,
    createIntegrationResponse'_responseParameters,
    createIntegrationResponse'_httpStatus,

    -- ** DeleteStage
    deleteStage_stageName,
    deleteStage_apiId,

    -- ** UpdateStage
    updateStage_deploymentId,
    updateStage_routeSettings,
    updateStage_accessLogSettings,
    updateStage_clientCertificateId,
    updateStage_stageVariables,
    updateStage_autoDeploy,
    updateStage_defaultRouteSettings,
    updateStage_description,
    updateStage_stageName,
    updateStage_apiId,
    updateStageResponse_lastDeploymentStatusMessage,
    updateStageResponse_deploymentId,
    updateStageResponse_routeSettings,
    updateStageResponse_accessLogSettings,
    updateStageResponse_clientCertificateId,
    updateStageResponse_stageVariables,
    updateStageResponse_autoDeploy,
    updateStageResponse_createdDate,
    updateStageResponse_defaultRouteSettings,
    updateStageResponse_apiGatewayManaged,
    updateStageResponse_stageName,
    updateStageResponse_lastUpdatedDate,
    updateStageResponse_description,
    updateStageResponse_tags,
    updateStageResponse_httpStatus,

    -- ** DeleteRouteSettings
    deleteRouteSettings_stageName,
    deleteRouteSettings_routeKey,
    deleteRouteSettings_apiId,

    -- ** CreateDeployment
    createDeployment_stageName,
    createDeployment_description,
    createDeployment_apiId,
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_autoDeployed,
    createDeploymentResponse_deploymentStatusMessage,
    createDeploymentResponse_createdDate,
    createDeploymentResponse_deploymentStatus,
    createDeploymentResponse_description,
    createDeploymentResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_apiId,
    deleteRoute_routeId,

    -- ** UpdateRoute
    updateRoute_authorizationScopes,
    updateRoute_modelSelectionExpression,
    updateRoute_requestModels,
    updateRoute_routeResponseSelectionExpression,
    updateRoute_requestParameters,
    updateRoute_authorizerId,
    updateRoute_operationName,
    updateRoute_authorizationType,
    updateRoute_apiKeyRequired,
    updateRoute_routeKey,
    updateRoute_target,
    updateRoute_apiId,
    updateRoute_routeId,
    updateRouteResponse'_authorizationScopes,
    updateRouteResponse'_modelSelectionExpression,
    updateRouteResponse'_requestModels,
    updateRouteResponse'_routeResponseSelectionExpression,
    updateRouteResponse'_requestParameters,
    updateRouteResponse'_routeId,
    updateRouteResponse'_authorizerId,
    updateRouteResponse'_operationName,
    updateRouteResponse'_apiGatewayManaged,
    updateRouteResponse'_authorizationType,
    updateRouteResponse'_apiKeyRequired,
    updateRouteResponse'_routeKey,
    updateRouteResponse'_target,
    updateRouteResponse'_httpStatus,

    -- ** GetVpcLinks
    getVpcLinks_nextToken,
    getVpcLinks_maxResults,
    getVpcLinksResponse_items,
    getVpcLinksResponse_nextToken,
    getVpcLinksResponse_httpStatus,

    -- ** GetIntegrationResponses
    getIntegrationResponses_nextToken,
    getIntegrationResponses_maxResults,
    getIntegrationResponses_integrationId,
    getIntegrationResponses_apiId,
    getIntegrationResponsesResponse_items,
    getIntegrationResponsesResponse_nextToken,
    getIntegrationResponsesResponse_httpStatus,

    -- ** GetIntegration
    getIntegration_apiId,
    getIntegration_integrationId,
    getIntegrationResponse'_integrationResponseSelectionExpression,
    getIntegrationResponse'_requestTemplates,
    getIntegrationResponse'_integrationSubtype,
    getIntegrationResponse'_credentialsArn,
    getIntegrationResponse'_integrationUri,
    getIntegrationResponse'_integrationId,
    getIntegrationResponse'_requestParameters,
    getIntegrationResponse'_connectionId,
    getIntegrationResponse'_passthroughBehavior,
    getIntegrationResponse'_integrationMethod,
    getIntegrationResponse'_tlsConfig,
    getIntegrationResponse'_payloadFormatVersion,
    getIntegrationResponse'_templateSelectionExpression,
    getIntegrationResponse'_timeoutInMillis,
    getIntegrationResponse'_apiGatewayManaged,
    getIntegrationResponse'_contentHandlingStrategy,
    getIntegrationResponse'_integrationType,
    getIntegrationResponse'_description,
    getIntegrationResponse'_connectionType,
    getIntegrationResponse'_responseParameters,
    getIntegrationResponse'_httpStatus,

    -- ** DeleteDeployment
    deleteDeployment_apiId,
    deleteDeployment_deploymentId,

    -- ** UpdateDeployment
    updateDeployment_description,
    updateDeployment_apiId,
    updateDeployment_deploymentId,
    updateDeploymentResponse_deploymentId,
    updateDeploymentResponse_autoDeployed,
    updateDeploymentResponse_deploymentStatusMessage,
    updateDeploymentResponse_createdDate,
    updateDeploymentResponse_deploymentStatus,
    updateDeploymentResponse_description,
    updateDeploymentResponse_httpStatus,

    -- ** DeleteRouteResponse
    deleteRouteResponse_routeResponseId,
    deleteRouteResponse_apiId,
    deleteRouteResponse_routeId,

    -- ** UpdateRouteResponse
    updateRouteResponse_modelSelectionExpression,
    updateRouteResponse_responseModels,
    updateRouteResponse_routeResponseKey,
    updateRouteResponse_responseParameters,
    updateRouteResponse_routeResponseId,
    updateRouteResponse_apiId,
    updateRouteResponse_routeId,
    updateRouteResponseResponse_modelSelectionExpression,
    updateRouteResponseResponse_responseModels,
    updateRouteResponseResponse_routeResponseId,
    updateRouteResponseResponse_routeResponseKey,
    updateRouteResponseResponse_responseParameters,
    updateRouteResponseResponse_httpStatus,

    -- ** GetVpcLink
    getVpcLink_vpcLinkId,
    getVpcLinkResponse_securityGroupIds,
    getVpcLinkResponse_subnetIds,
    getVpcLinkResponse_vpcLinkId,
    getVpcLinkResponse_createdDate,
    getVpcLinkResponse_vpcLinkVersion,
    getVpcLinkResponse_name,
    getVpcLinkResponse_vpcLinkStatusMessage,
    getVpcLinkResponse_tags,
    getVpcLinkResponse_vpcLinkStatus,
    getVpcLinkResponse_httpStatus,

    -- ** ResetAuthorizersCache
    resetAuthorizersCache_stageName,
    resetAuthorizersCache_apiId,

    -- ** CreateModel
    createModel_description,
    createModel_contentType,
    createModel_apiId,
    createModel_schema,
    createModel_name,
    createModelResponse_modelId,
    createModelResponse_schema,
    createModelResponse_name,
    createModelResponse_description,
    createModelResponse_contentType,
    createModelResponse_httpStatus,

    -- ** GetIntegrationResponse
    getIntegrationResponse_apiId,
    getIntegrationResponse_integrationResponseId,
    getIntegrationResponse_integrationId,
    getIntegrationResponseResponse_integrationResponseId,
    getIntegrationResponseResponse_integrationResponseKey,
    getIntegrationResponseResponse_templateSelectionExpression,
    getIntegrationResponseResponse_contentHandlingStrategy,
    getIntegrationResponseResponse_responseTemplates,
    getIntegrationResponseResponse_responseParameters,
    getIntegrationResponseResponse_httpStatus,

    -- ** CreateDomainName
    createDomainName_domainNameConfigurations,
    createDomainName_mutualTlsAuthentication,
    createDomainName_tags,
    createDomainName_domainName,
    createDomainNameResponse_domainNameConfigurations,
    createDomainNameResponse_domainName,
    createDomainNameResponse_mutualTlsAuthentication,
    createDomainNameResponse_apiMappingSelectionExpression,
    createDomainNameResponse_tags,
    createDomainNameResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelId,
    deleteModel_apiId,

    -- ** UpdateModel
    updateModel_schema,
    updateModel_name,
    updateModel_description,
    updateModel_contentType,
    updateModel_modelId,
    updateModel_apiId,
    updateModelResponse_modelId,
    updateModelResponse_schema,
    updateModelResponse_name,
    updateModelResponse_description,
    updateModelResponse_contentType,
    updateModelResponse_httpStatus,

    -- ** CreateRouteResponse
    createRouteResponse_modelSelectionExpression,
    createRouteResponse_responseModels,
    createRouteResponse_responseParameters,
    createRouteResponse_apiId,
    createRouteResponse_routeId,
    createRouteResponse_routeResponseKey,
    createRouteResponseResponse_modelSelectionExpression,
    createRouteResponseResponse_responseModels,
    createRouteResponseResponse_routeResponseId,
    createRouteResponseResponse_routeResponseKey,
    createRouteResponseResponse_responseParameters,
    createRouteResponseResponse_httpStatus,

    -- ** GetStages
    getStages_nextToken,
    getStages_maxResults,
    getStages_apiId,
    getStagesResponse_items,
    getStagesResponse_nextToken,
    getStagesResponse_httpStatus,

    -- ** GetModel
    getModel_modelId,
    getModel_apiId,
    getModelResponse_modelId,
    getModelResponse_schema,
    getModelResponse_name,
    getModelResponse_description,
    getModelResponse_contentType,
    getModelResponse_httpStatus,

    -- ** GetApiMappings
    getApiMappings_nextToken,
    getApiMappings_maxResults,
    getApiMappings_domainName,
    getApiMappingsResponse_items,
    getApiMappingsResponse_nextToken,
    getApiMappingsResponse_httpStatus,

    -- ** CreateIntegrationResponse
    createIntegrationResponse_templateSelectionExpression,
    createIntegrationResponse_contentHandlingStrategy,
    createIntegrationResponse_responseTemplates,
    createIntegrationResponse_responseParameters,
    createIntegrationResponse_apiId,
    createIntegrationResponse_integrationId,
    createIntegrationResponse_integrationResponseKey,
    createIntegrationResponseResponse_integrationResponseId,
    createIntegrationResponseResponse_integrationResponseKey,
    createIntegrationResponseResponse_templateSelectionExpression,
    createIntegrationResponseResponse_contentHandlingStrategy,
    createIntegrationResponseResponse_responseTemplates,
    createIntegrationResponseResponse_responseParameters,
    createIntegrationResponseResponse_httpStatus,

    -- ** GetDomainName
    getDomainName_domainName,
    getDomainNameResponse_domainNameConfigurations,
    getDomainNameResponse_domainName,
    getDomainNameResponse_mutualTlsAuthentication,
    getDomainNameResponse_apiMappingSelectionExpression,
    getDomainNameResponse_tags,
    getDomainNameResponse_httpStatus,

    -- ** CreateVpcLink
    createVpcLink_securityGroupIds,
    createVpcLink_tags,
    createVpcLink_subnetIds,
    createVpcLink_name,
    createVpcLinkResponse_securityGroupIds,
    createVpcLinkResponse_subnetIds,
    createVpcLinkResponse_vpcLinkId,
    createVpcLinkResponse_createdDate,
    createVpcLinkResponse_vpcLinkVersion,
    createVpcLinkResponse_name,
    createVpcLinkResponse_vpcLinkStatusMessage,
    createVpcLinkResponse_tags,
    createVpcLinkResponse_vpcLinkStatus,
    createVpcLinkResponse_httpStatus,

    -- ** GetAuthorizers
    getAuthorizers_nextToken,
    getAuthorizers_maxResults,
    getAuthorizers_apiId,
    getAuthorizersResponse_items,
    getAuthorizersResponse_nextToken,
    getAuthorizersResponse_httpStatus,

    -- ** GetRouteResponse
    getRouteResponse_routeResponseId,
    getRouteResponse_apiId,
    getRouteResponse_routeId,
    getRouteResponseResponse_modelSelectionExpression,
    getRouteResponseResponse_responseModels,
    getRouteResponseResponse_routeResponseId,
    getRouteResponseResponse_routeResponseKey,
    getRouteResponseResponse_responseParameters,
    getRouteResponseResponse_httpStatus,

    -- ** ExportApi
    exportApi_exportVersion,
    exportApi_stageName,
    exportApi_includeExtensions,
    exportApi_specification,
    exportApi_outputType,
    exportApi_apiId,
    exportApiResponse_body,
    exportApiResponse_httpStatus,

    -- ** GetRoutes
    getRoutes_nextToken,
    getRoutes_maxResults,
    getRoutes_apiId,
    getRoutesResponse_items,
    getRoutesResponse_nextToken,
    getRoutesResponse_httpStatus,

    -- ** DeleteCorsConfiguration
    deleteCorsConfiguration_apiId,

    -- ** DeleteVpcLink
    deleteVpcLink_vpcLinkId,
    deleteVpcLinkResponse_httpStatus,

    -- ** UpdateVpcLink
    updateVpcLink_name,
    updateVpcLink_vpcLinkId,
    updateVpcLinkResponse_securityGroupIds,
    updateVpcLinkResponse_subnetIds,
    updateVpcLinkResponse_vpcLinkId,
    updateVpcLinkResponse_createdDate,
    updateVpcLinkResponse_vpcLinkVersion,
    updateVpcLinkResponse_name,
    updateVpcLinkResponse_vpcLinkStatusMessage,
    updateVpcLinkResponse_tags,
    updateVpcLinkResponse_vpcLinkStatus,
    updateVpcLinkResponse_httpStatus,

    -- ** DeleteIntegrationResponse
    deleteIntegrationResponse_apiId,
    deleteIntegrationResponse_integrationResponseId,
    deleteIntegrationResponse_integrationId,

    -- ** UpdateIntegrationResponse
    updateIntegrationResponse_integrationResponseKey,
    updateIntegrationResponse_templateSelectionExpression,
    updateIntegrationResponse_contentHandlingStrategy,
    updateIntegrationResponse_responseTemplates,
    updateIntegrationResponse_responseParameters,
    updateIntegrationResponse_apiId,
    updateIntegrationResponse_integrationResponseId,
    updateIntegrationResponse_integrationId,
    updateIntegrationResponseResponse_integrationResponseId,
    updateIntegrationResponseResponse_integrationResponseKey,
    updateIntegrationResponseResponse_templateSelectionExpression,
    updateIntegrationResponseResponse_contentHandlingStrategy,
    updateIntegrationResponseResponse_responseTemplates,
    updateIntegrationResponseResponse_responseParameters,
    updateIntegrationResponseResponse_httpStatus,

    -- ** DeleteIntegration
    deleteIntegration_apiId,
    deleteIntegration_integrationId,

    -- ** UpdateIntegration
    updateIntegration_requestTemplates,
    updateIntegration_integrationSubtype,
    updateIntegration_credentialsArn,
    updateIntegration_integrationUri,
    updateIntegration_requestParameters,
    updateIntegration_connectionId,
    updateIntegration_passthroughBehavior,
    updateIntegration_integrationMethod,
    updateIntegration_tlsConfig,
    updateIntegration_payloadFormatVersion,
    updateIntegration_templateSelectionExpression,
    updateIntegration_timeoutInMillis,
    updateIntegration_contentHandlingStrategy,
    updateIntegration_integrationType,
    updateIntegration_description,
    updateIntegration_connectionType,
    updateIntegration_responseParameters,
    updateIntegration_apiId,
    updateIntegration_integrationId,
    updateIntegrationResponse'_integrationResponseSelectionExpression,
    updateIntegrationResponse'_requestTemplates,
    updateIntegrationResponse'_integrationSubtype,
    updateIntegrationResponse'_credentialsArn,
    updateIntegrationResponse'_integrationUri,
    updateIntegrationResponse'_integrationId,
    updateIntegrationResponse'_requestParameters,
    updateIntegrationResponse'_connectionId,
    updateIntegrationResponse'_passthroughBehavior,
    updateIntegrationResponse'_integrationMethod,
    updateIntegrationResponse'_tlsConfig,
    updateIntegrationResponse'_payloadFormatVersion,
    updateIntegrationResponse'_templateSelectionExpression,
    updateIntegrationResponse'_timeoutInMillis,
    updateIntegrationResponse'_apiGatewayManaged,
    updateIntegrationResponse'_contentHandlingStrategy,
    updateIntegrationResponse'_integrationType,
    updateIntegrationResponse'_description,
    updateIntegrationResponse'_connectionType,
    updateIntegrationResponse'_responseParameters,
    updateIntegrationResponse'_httpStatus,

    -- ** GetRoute
    getRoute_apiId,
    getRoute_routeId,
    getRouteResponse'_authorizationScopes,
    getRouteResponse'_modelSelectionExpression,
    getRouteResponse'_requestModels,
    getRouteResponse'_routeResponseSelectionExpression,
    getRouteResponse'_requestParameters,
    getRouteResponse'_routeId,
    getRouteResponse'_authorizerId,
    getRouteResponse'_operationName,
    getRouteResponse'_apiGatewayManaged,
    getRouteResponse'_authorizationType,
    getRouteResponse'_apiKeyRequired,
    getRouteResponse'_routeKey,
    getRouteResponse'_target,
    getRouteResponse'_httpStatus,

    -- ** GetAuthorizer
    getAuthorizer_authorizerId,
    getAuthorizer_apiId,
    getAuthorizerResponse_authorizerCredentialsArn,
    getAuthorizerResponse_identityValidationExpression,
    getAuthorizerResponse_enableSimpleResponses,
    getAuthorizerResponse_authorizerUri,
    getAuthorizerResponse_authorizerPayloadFormatVersion,
    getAuthorizerResponse_jwtConfiguration,
    getAuthorizerResponse_authorizerId,
    getAuthorizerResponse_name,
    getAuthorizerResponse_authorizerResultTtlInSeconds,
    getAuthorizerResponse_identitySource,
    getAuthorizerResponse_authorizerType,
    getAuthorizerResponse_httpStatus,

    -- ** GetStage
    getStage_stageName,
    getStage_apiId,
    getStageResponse_lastDeploymentStatusMessage,
    getStageResponse_deploymentId,
    getStageResponse_routeSettings,
    getStageResponse_accessLogSettings,
    getStageResponse_clientCertificateId,
    getStageResponse_stageVariables,
    getStageResponse_autoDeploy,
    getStageResponse_createdDate,
    getStageResponse_defaultRouteSettings,
    getStageResponse_apiGatewayManaged,
    getStageResponse_stageName,
    getStageResponse_lastUpdatedDate,
    getStageResponse_description,
    getStageResponse_tags,
    getStageResponse_httpStatus,

    -- ** GetApiMapping
    getApiMapping_apiMappingId,
    getApiMapping_domainName,
    getApiMappingResponse_stage,
    getApiMappingResponse_apiId,
    getApiMappingResponse_apiMappingKey,
    getApiMappingResponse_apiMappingId,
    getApiMappingResponse_httpStatus,

    -- ** ImportApi
    importApi_basepath,
    importApi_failOnWarnings,
    importApi_body,
    importApiResponse_apiId,
    importApiResponse_disableExecuteApiEndpoint,
    importApiResponse_apiEndpoint,
    importApiResponse_warnings,
    importApiResponse_createdDate,
    importApiResponse_name,
    importApiResponse_version,
    importApiResponse_apiGatewayManaged,
    importApiResponse_apiKeySelectionExpression,
    importApiResponse_corsConfiguration,
    importApiResponse_routeSelectionExpression,
    importApiResponse_importInfo,
    importApiResponse_disableSchemaValidation,
    importApiResponse_description,
    importApiResponse_protocolType,
    importApiResponse_tags,
    importApiResponse_httpStatus,

    -- ** GetApis
    getApis_nextToken,
    getApis_maxResults,
    getApisResponse_items,
    getApisResponse_nextToken,
    getApisResponse_httpStatus,

    -- ** UpdateApiMapping
    updateApiMapping_stage,
    updateApiMapping_apiMappingKey,
    updateApiMapping_apiMappingId,
    updateApiMapping_apiId,
    updateApiMapping_domainName,
    updateApiMappingResponse_stage,
    updateApiMappingResponse_apiId,
    updateApiMappingResponse_apiMappingKey,
    updateApiMappingResponse_apiMappingId,
    updateApiMappingResponse_httpStatus,

    -- ** DeleteApiMapping
    deleteApiMapping_apiMappingId,
    deleteApiMapping_domainName,

    -- ** CreateRoute
    createRoute_authorizationScopes,
    createRoute_modelSelectionExpression,
    createRoute_requestModels,
    createRoute_routeResponseSelectionExpression,
    createRoute_requestParameters,
    createRoute_authorizerId,
    createRoute_operationName,
    createRoute_authorizationType,
    createRoute_apiKeyRequired,
    createRoute_target,
    createRoute_apiId,
    createRoute_routeKey,
    createRouteResponse'_authorizationScopes,
    createRouteResponse'_modelSelectionExpression,
    createRouteResponse'_requestModels,
    createRouteResponse'_routeResponseSelectionExpression,
    createRouteResponse'_requestParameters,
    createRouteResponse'_routeId,
    createRouteResponse'_authorizerId,
    createRouteResponse'_operationName,
    createRouteResponse'_apiGatewayManaged,
    createRouteResponse'_authorizationType,
    createRouteResponse'_apiKeyRequired,
    createRouteResponse'_routeKey,
    createRouteResponse'_target,
    createRouteResponse'_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_authorizerCredentialsArn,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_enableSimpleResponses,
    createAuthorizer_authorizerUri,
    createAuthorizer_authorizerPayloadFormatVersion,
    createAuthorizer_jwtConfiguration,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_apiId,
    createAuthorizer_authorizerType,
    createAuthorizer_identitySource,
    createAuthorizer_name,
    createAuthorizerResponse_authorizerCredentialsArn,
    createAuthorizerResponse_identityValidationExpression,
    createAuthorizerResponse_enableSimpleResponses,
    createAuthorizerResponse_authorizerUri,
    createAuthorizerResponse_authorizerPayloadFormatVersion,
    createAuthorizerResponse_jwtConfiguration,
    createAuthorizerResponse_authorizerId,
    createAuthorizerResponse_name,
    createAuthorizerResponse_authorizerResultTtlInSeconds,
    createAuthorizerResponse_identitySource,
    createAuthorizerResponse_authorizerType,
    createAuthorizerResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_authorizerCredentialsArn,
    updateAuthorizer_identityValidationExpression,
    updateAuthorizer_enableSimpleResponses,
    updateAuthorizer_authorizerUri,
    updateAuthorizer_authorizerPayloadFormatVersion,
    updateAuthorizer_jwtConfiguration,
    updateAuthorizer_name,
    updateAuthorizer_authorizerResultTtlInSeconds,
    updateAuthorizer_identitySource,
    updateAuthorizer_authorizerType,
    updateAuthorizer_authorizerId,
    updateAuthorizer_apiId,
    updateAuthorizerResponse_authorizerCredentialsArn,
    updateAuthorizerResponse_identityValidationExpression,
    updateAuthorizerResponse_enableSimpleResponses,
    updateAuthorizerResponse_authorizerUri,
    updateAuthorizerResponse_authorizerPayloadFormatVersion,
    updateAuthorizerResponse_jwtConfiguration,
    updateAuthorizerResponse_authorizerId,
    updateAuthorizerResponse_name,
    updateAuthorizerResponse_authorizerResultTtlInSeconds,
    updateAuthorizerResponse_identitySource,
    updateAuthorizerResponse_authorizerType,
    updateAuthorizerResponse_httpStatus,

    -- ** DeleteAuthorizer
    deleteAuthorizer_authorizerId,
    deleteAuthorizer_apiId,

    -- ** CreateApiMapping
    createApiMapping_apiMappingKey,
    createApiMapping_domainName,
    createApiMapping_stage,
    createApiMapping_apiId,
    createApiMappingResponse_stage,
    createApiMappingResponse_apiId,
    createApiMappingResponse_apiMappingKey,
    createApiMappingResponse_apiMappingId,
    createApiMappingResponse_httpStatus,

    -- ** DeleteRouteRequestParameter
    deleteRouteRequestParameter_requestParameterKey,
    deleteRouteRequestParameter_apiId,
    deleteRouteRequestParameter_routeId,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,
    tagResourceResponse_httpStatus,

    -- ** CreateStage
    createStage_deploymentId,
    createStage_routeSettings,
    createStage_accessLogSettings,
    createStage_clientCertificateId,
    createStage_stageVariables,
    createStage_autoDeploy,
    createStage_defaultRouteSettings,
    createStage_description,
    createStage_tags,
    createStage_apiId,
    createStage_stageName,
    createStageResponse_lastDeploymentStatusMessage,
    createStageResponse_deploymentId,
    createStageResponse_routeSettings,
    createStageResponse_accessLogSettings,
    createStageResponse_clientCertificateId,
    createStageResponse_stageVariables,
    createStageResponse_autoDeploy,
    createStageResponse_createdDate,
    createStageResponse_defaultRouteSettings,
    createStageResponse_apiGatewayManaged,
    createStageResponse_stageName,
    createStageResponse_lastUpdatedDate,
    createStageResponse_description,
    createStageResponse_tags,
    createStageResponse_httpStatus,

    -- ** GetIntegrations
    getIntegrations_nextToken,
    getIntegrations_maxResults,
    getIntegrations_apiId,
    getIntegrationsResponse_items,
    getIntegrationsResponse_nextToken,
    getIntegrationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateDomainName
    updateDomainName_domainNameConfigurations,
    updateDomainName_mutualTlsAuthentication,
    updateDomainName_domainName,
    updateDomainNameResponse_domainNameConfigurations,
    updateDomainNameResponse_domainName,
    updateDomainNameResponse_mutualTlsAuthentication,
    updateDomainNameResponse_apiMappingSelectionExpression,
    updateDomainNameResponse_tags,
    updateDomainNameResponse_httpStatus,

    -- ** DeleteDomainName
    deleteDomainName_domainName,

    -- ** GetApi
    getApi_apiId,
    getApiResponse_apiId,
    getApiResponse_disableExecuteApiEndpoint,
    getApiResponse_apiEndpoint,
    getApiResponse_warnings,
    getApiResponse_createdDate,
    getApiResponse_name,
    getApiResponse_version,
    getApiResponse_apiGatewayManaged,
    getApiResponse_apiKeySelectionExpression,
    getApiResponse_corsConfiguration,
    getApiResponse_routeSelectionExpression,
    getApiResponse_importInfo,
    getApiResponse_disableSchemaValidation,
    getApiResponse_description,
    getApiResponse_protocolType,
    getApiResponse_tags,
    getApiResponse_httpStatus,

    -- ** DeleteApi
    deleteApi_apiId,

    -- ** UpdateApi
    updateApi_credentialsArn,
    updateApi_disableExecuteApiEndpoint,
    updateApi_name,
    updateApi_version,
    updateApi_apiKeySelectionExpression,
    updateApi_corsConfiguration,
    updateApi_routeSelectionExpression,
    updateApi_disableSchemaValidation,
    updateApi_description,
    updateApi_routeKey,
    updateApi_target,
    updateApi_apiId,
    updateApiResponse_apiId,
    updateApiResponse_disableExecuteApiEndpoint,
    updateApiResponse_apiEndpoint,
    updateApiResponse_warnings,
    updateApiResponse_createdDate,
    updateApiResponse_name,
    updateApiResponse_version,
    updateApiResponse_apiGatewayManaged,
    updateApiResponse_apiKeySelectionExpression,
    updateApiResponse_corsConfiguration,
    updateApiResponse_routeSelectionExpression,
    updateApiResponse_importInfo,
    updateApiResponse_disableSchemaValidation,
    updateApiResponse_description,
    updateApiResponse_protocolType,
    updateApiResponse_tags,
    updateApiResponse_httpStatus,

    -- ** GetModelTemplate
    getModelTemplate_modelId,
    getModelTemplate_apiId,
    getModelTemplateResponse_value,
    getModelTemplateResponse_httpStatus,

    -- * Types

    -- ** AccessLogSettings
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- ** Api
    api_apiId,
    api_disableExecuteApiEndpoint,
    api_apiEndpoint,
    api_warnings,
    api_createdDate,
    api_version,
    api_apiGatewayManaged,
    api_apiKeySelectionExpression,
    api_corsConfiguration,
    api_importInfo,
    api_disableSchemaValidation,
    api_description,
    api_tags,
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
    authorizer_identityValidationExpression,
    authorizer_enableSimpleResponses,
    authorizer_authorizerUri,
    authorizer_authorizerPayloadFormatVersion,
    authorizer_jwtConfiguration,
    authorizer_authorizerId,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_identitySource,
    authorizer_authorizerType,
    authorizer_name,

    -- ** Cors
    cors_maxAge,
    cors_allowMethods,
    cors_allowHeaders,
    cors_exposeHeaders,
    cors_allowOrigins,
    cors_allowCredentials,

    -- ** Deployment
    deployment_deploymentId,
    deployment_autoDeployed,
    deployment_deploymentStatusMessage,
    deployment_createdDate,
    deployment_deploymentStatus,
    deployment_description,

    -- ** DomainName
    domainName_domainNameConfigurations,
    domainName_mutualTlsAuthentication,
    domainName_apiMappingSelectionExpression,
    domainName_tags,
    domainName_domainName,

    -- ** DomainNameConfiguration
    domainNameConfiguration_apiGatewayDomainName,
    domainNameConfiguration_ownershipVerificationCertificateArn,
    domainNameConfiguration_certificateName,
    domainNameConfiguration_hostedZoneId,
    domainNameConfiguration_certificateArn,
    domainNameConfiguration_endpointType,
    domainNameConfiguration_securityPolicy,
    domainNameConfiguration_certificateUploadDate,
    domainNameConfiguration_domainNameStatusMessage,
    domainNameConfiguration_domainNameStatus,

    -- ** Integration
    integration_integrationResponseSelectionExpression,
    integration_requestTemplates,
    integration_integrationSubtype,
    integration_credentialsArn,
    integration_integrationUri,
    integration_integrationId,
    integration_requestParameters,
    integration_connectionId,
    integration_passthroughBehavior,
    integration_integrationMethod,
    integration_tlsConfig,
    integration_payloadFormatVersion,
    integration_templateSelectionExpression,
    integration_timeoutInMillis,
    integration_apiGatewayManaged,
    integration_contentHandlingStrategy,
    integration_integrationType,
    integration_description,
    integration_connectionType,
    integration_responseParameters,

    -- ** IntegrationResponse
    integrationResponse_integrationResponseId,
    integrationResponse_templateSelectionExpression,
    integrationResponse_contentHandlingStrategy,
    integrationResponse_responseTemplates,
    integrationResponse_responseParameters,
    integrationResponse_integrationResponseKey,

    -- ** JWTConfiguration
    jWTConfiguration_audience,
    jWTConfiguration_issuer,

    -- ** Model
    model_modelId,
    model_schema,
    model_description,
    model_contentType,
    model_name,

    -- ** MutualTlsAuthentication
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,

    -- ** MutualTlsAuthenticationInput
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

    -- ** ParameterConstraints
    parameterConstraints_required,

    -- ** Route
    route_authorizationScopes,
    route_modelSelectionExpression,
    route_requestModels,
    route_routeResponseSelectionExpression,
    route_requestParameters,
    route_routeId,
    route_authorizerId,
    route_operationName,
    route_apiGatewayManaged,
    route_authorizationType,
    route_apiKeyRequired,
    route_target,
    route_routeKey,

    -- ** RouteResponse
    routeResponse_modelSelectionExpression,
    routeResponse_responseModels,
    routeResponse_routeResponseId,
    routeResponse_responseParameters,
    routeResponse_routeResponseKey,

    -- ** RouteSettings
    routeSettings_dataTraceEnabled,
    routeSettings_throttlingBurstLimit,
    routeSettings_loggingLevel,
    routeSettings_throttlingRateLimit,
    routeSettings_detailedMetricsEnabled,

    -- ** Stage
    stage_lastDeploymentStatusMessage,
    stage_deploymentId,
    stage_routeSettings,
    stage_accessLogSettings,
    stage_clientCertificateId,
    stage_stageVariables,
    stage_autoDeploy,
    stage_createdDate,
    stage_defaultRouteSettings,
    stage_apiGatewayManaged,
    stage_lastUpdatedDate,
    stage_description,
    stage_tags,
    stage_stageName,

    -- ** TlsConfig
    tlsConfig_serverNameToVerify,

    -- ** TlsConfigInput
    tlsConfigInput_serverNameToVerify,

    -- ** VpcLink
    vpcLink_createdDate,
    vpcLink_vpcLinkVersion,
    vpcLink_vpcLinkStatusMessage,
    vpcLink_tags,
    vpcLink_vpcLinkStatus,
    vpcLink_vpcLinkId,
    vpcLink_securityGroupIds,
    vpcLink_subnetIds,
    vpcLink_name,
  )
where

import Network.AWS.ApiGatewayV2.CreateApi
import Network.AWS.ApiGatewayV2.CreateApiMapping
import Network.AWS.ApiGatewayV2.CreateAuthorizer
import Network.AWS.ApiGatewayV2.CreateDeployment
import Network.AWS.ApiGatewayV2.CreateDomainName
import Network.AWS.ApiGatewayV2.CreateIntegration
import Network.AWS.ApiGatewayV2.CreateIntegrationResponse
import Network.AWS.ApiGatewayV2.CreateModel
import Network.AWS.ApiGatewayV2.CreateRoute
import Network.AWS.ApiGatewayV2.CreateRouteResponse
import Network.AWS.ApiGatewayV2.CreateStage
import Network.AWS.ApiGatewayV2.CreateVpcLink
import Network.AWS.ApiGatewayV2.DeleteAccessLogSettings
import Network.AWS.ApiGatewayV2.DeleteApi
import Network.AWS.ApiGatewayV2.DeleteApiMapping
import Network.AWS.ApiGatewayV2.DeleteAuthorizer
import Network.AWS.ApiGatewayV2.DeleteCorsConfiguration
import Network.AWS.ApiGatewayV2.DeleteDeployment
import Network.AWS.ApiGatewayV2.DeleteDomainName
import Network.AWS.ApiGatewayV2.DeleteIntegration
import Network.AWS.ApiGatewayV2.DeleteIntegrationResponse
import Network.AWS.ApiGatewayV2.DeleteModel
import Network.AWS.ApiGatewayV2.DeleteRoute
import Network.AWS.ApiGatewayV2.DeleteRouteRequestParameter
import Network.AWS.ApiGatewayV2.DeleteRouteResponse
import Network.AWS.ApiGatewayV2.DeleteRouteSettings
import Network.AWS.ApiGatewayV2.DeleteStage
import Network.AWS.ApiGatewayV2.DeleteVpcLink
import Network.AWS.ApiGatewayV2.ExportApi
import Network.AWS.ApiGatewayV2.GetApi
import Network.AWS.ApiGatewayV2.GetApiMapping
import Network.AWS.ApiGatewayV2.GetApiMappings
import Network.AWS.ApiGatewayV2.GetApis
import Network.AWS.ApiGatewayV2.GetAuthorizer
import Network.AWS.ApiGatewayV2.GetAuthorizers
import Network.AWS.ApiGatewayV2.GetDeployment
import Network.AWS.ApiGatewayV2.GetDeployments
import Network.AWS.ApiGatewayV2.GetDomainName
import Network.AWS.ApiGatewayV2.GetDomainNames
import Network.AWS.ApiGatewayV2.GetIntegration
import Network.AWS.ApiGatewayV2.GetIntegrationResponse
import Network.AWS.ApiGatewayV2.GetIntegrationResponses
import Network.AWS.ApiGatewayV2.GetIntegrations
import Network.AWS.ApiGatewayV2.GetModel
import Network.AWS.ApiGatewayV2.GetModelTemplate
import Network.AWS.ApiGatewayV2.GetModels
import Network.AWS.ApiGatewayV2.GetRoute
import Network.AWS.ApiGatewayV2.GetRouteResponse
import Network.AWS.ApiGatewayV2.GetRouteResponses
import Network.AWS.ApiGatewayV2.GetRoutes
import Network.AWS.ApiGatewayV2.GetStage
import Network.AWS.ApiGatewayV2.GetStages
import Network.AWS.ApiGatewayV2.GetTags
import Network.AWS.ApiGatewayV2.GetVpcLink
import Network.AWS.ApiGatewayV2.GetVpcLinks
import Network.AWS.ApiGatewayV2.ImportApi
import Network.AWS.ApiGatewayV2.ReimportApi
import Network.AWS.ApiGatewayV2.ResetAuthorizersCache
import Network.AWS.ApiGatewayV2.TagResource
import Network.AWS.ApiGatewayV2.Types.AccessLogSettings
import Network.AWS.ApiGatewayV2.Types.Api
import Network.AWS.ApiGatewayV2.Types.ApiMapping
import Network.AWS.ApiGatewayV2.Types.Authorizer
import Network.AWS.ApiGatewayV2.Types.Cors
import Network.AWS.ApiGatewayV2.Types.Deployment
import Network.AWS.ApiGatewayV2.Types.DomainName
import Network.AWS.ApiGatewayV2.Types.DomainNameConfiguration
import Network.AWS.ApiGatewayV2.Types.Integration
import Network.AWS.ApiGatewayV2.Types.IntegrationResponse
import Network.AWS.ApiGatewayV2.Types.JWTConfiguration
import Network.AWS.ApiGatewayV2.Types.Model
import Network.AWS.ApiGatewayV2.Types.MutualTlsAuthentication
import Network.AWS.ApiGatewayV2.Types.MutualTlsAuthenticationInput
import Network.AWS.ApiGatewayV2.Types.ParameterConstraints
import Network.AWS.ApiGatewayV2.Types.Route
import Network.AWS.ApiGatewayV2.Types.RouteResponse
import Network.AWS.ApiGatewayV2.Types.RouteSettings
import Network.AWS.ApiGatewayV2.Types.Stage
import Network.AWS.ApiGatewayV2.Types.TlsConfig
import Network.AWS.ApiGatewayV2.Types.TlsConfigInput
import Network.AWS.ApiGatewayV2.Types.VpcLink
import Network.AWS.ApiGatewayV2.UntagResource
import Network.AWS.ApiGatewayV2.UpdateApi
import Network.AWS.ApiGatewayV2.UpdateApiMapping
import Network.AWS.ApiGatewayV2.UpdateAuthorizer
import Network.AWS.ApiGatewayV2.UpdateDeployment
import Network.AWS.ApiGatewayV2.UpdateDomainName
import Network.AWS.ApiGatewayV2.UpdateIntegration
import Network.AWS.ApiGatewayV2.UpdateIntegrationResponse
import Network.AWS.ApiGatewayV2.UpdateModel
import Network.AWS.ApiGatewayV2.UpdateRoute
import Network.AWS.ApiGatewayV2.UpdateRouteResponse
import Network.AWS.ApiGatewayV2.UpdateStage
import Network.AWS.ApiGatewayV2.UpdateVpcLink
