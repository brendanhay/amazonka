{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ApiGatewayV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon API Gateway V2
module Amazonka.ApiGatewayV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApi
    CreateApi (CreateApi'),
    newCreateApi,
    CreateApiResponse (CreateApiResponse'),
    newCreateApiResponse,

    -- ** CreateApiMapping
    CreateApiMapping (CreateApiMapping'),
    newCreateApiMapping,
    CreateApiMappingResponse (CreateApiMappingResponse'),
    newCreateApiMappingResponse,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    CreateAuthorizerResponse (CreateAuthorizerResponse'),
    newCreateAuthorizerResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** CreateDomainName
    CreateDomainName (CreateDomainName'),
    newCreateDomainName,
    CreateDomainNameResponse (CreateDomainNameResponse'),
    newCreateDomainNameResponse,

    -- ** CreateIntegration
    CreateIntegration (CreateIntegration'),
    newCreateIntegration,
    CreateIntegrationResponse' (CreateIntegrationResponse''),
    newCreateIntegrationResponse',

    -- ** CreateIntegrationResponse
    CreateIntegrationResponse (CreateIntegrationResponse'),
    newCreateIntegrationResponse,
    CreateIntegrationResponseResponse (CreateIntegrationResponseResponse'),
    newCreateIntegrationResponseResponse,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    CreateModelResponse (CreateModelResponse'),
    newCreateModelResponse,

    -- ** CreateRoute
    CreateRoute (CreateRoute'),
    newCreateRoute,
    CreateRouteResponse' (CreateRouteResponse''),
    newCreateRouteResponse',

    -- ** CreateRouteResponse
    CreateRouteResponse (CreateRouteResponse'),
    newCreateRouteResponse,
    CreateRouteResponseResponse (CreateRouteResponseResponse'),
    newCreateRouteResponseResponse,

    -- ** CreateStage
    CreateStage (CreateStage'),
    newCreateStage,
    CreateStageResponse (CreateStageResponse'),
    newCreateStageResponse,

    -- ** CreateVpcLink
    CreateVpcLink (CreateVpcLink'),
    newCreateVpcLink,
    CreateVpcLinkResponse (CreateVpcLinkResponse'),
    newCreateVpcLinkResponse,

    -- ** DeleteAccessLogSettings
    DeleteAccessLogSettings (DeleteAccessLogSettings'),
    newDeleteAccessLogSettings,
    DeleteAccessLogSettingsResponse (DeleteAccessLogSettingsResponse'),
    newDeleteAccessLogSettingsResponse,

    -- ** DeleteApi
    DeleteApi (DeleteApi'),
    newDeleteApi,
    DeleteApiResponse (DeleteApiResponse'),
    newDeleteApiResponse,

    -- ** DeleteApiMapping
    DeleteApiMapping (DeleteApiMapping'),
    newDeleteApiMapping,
    DeleteApiMappingResponse (DeleteApiMappingResponse'),
    newDeleteApiMappingResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** DeleteCorsConfiguration
    DeleteCorsConfiguration (DeleteCorsConfiguration'),
    newDeleteCorsConfiguration,
    DeleteCorsConfigurationResponse (DeleteCorsConfigurationResponse'),
    newDeleteCorsConfigurationResponse,

    -- ** DeleteDeployment
    DeleteDeployment (DeleteDeployment'),
    newDeleteDeployment,
    DeleteDeploymentResponse (DeleteDeploymentResponse'),
    newDeleteDeploymentResponse,

    -- ** DeleteDomainName
    DeleteDomainName (DeleteDomainName'),
    newDeleteDomainName,
    DeleteDomainNameResponse (DeleteDomainNameResponse'),
    newDeleteDomainNameResponse,

    -- ** DeleteIntegration
    DeleteIntegration (DeleteIntegration'),
    newDeleteIntegration,
    DeleteIntegrationResponse' (DeleteIntegrationResponse''),
    newDeleteIntegrationResponse',

    -- ** DeleteIntegrationResponse
    DeleteIntegrationResponse (DeleteIntegrationResponse'),
    newDeleteIntegrationResponse,
    DeleteIntegrationResponseResponse (DeleteIntegrationResponseResponse'),
    newDeleteIntegrationResponseResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteRoute
    DeleteRoute (DeleteRoute'),
    newDeleteRoute,
    DeleteRouteResponse' (DeleteRouteResponse''),
    newDeleteRouteResponse',

    -- ** DeleteRouteRequestParameter
    DeleteRouteRequestParameter (DeleteRouteRequestParameter'),
    newDeleteRouteRequestParameter,
    DeleteRouteRequestParameterResponse (DeleteRouteRequestParameterResponse'),
    newDeleteRouteRequestParameterResponse,

    -- ** DeleteRouteResponse
    DeleteRouteResponse (DeleteRouteResponse'),
    newDeleteRouteResponse,
    DeleteRouteResponseResponse (DeleteRouteResponseResponse'),
    newDeleteRouteResponseResponse,

    -- ** DeleteRouteSettings
    DeleteRouteSettings (DeleteRouteSettings'),
    newDeleteRouteSettings,
    DeleteRouteSettingsResponse (DeleteRouteSettingsResponse'),
    newDeleteRouteSettingsResponse,

    -- ** DeleteStage
    DeleteStage (DeleteStage'),
    newDeleteStage,
    DeleteStageResponse (DeleteStageResponse'),
    newDeleteStageResponse,

    -- ** DeleteVpcLink
    DeleteVpcLink (DeleteVpcLink'),
    newDeleteVpcLink,
    DeleteVpcLinkResponse (DeleteVpcLinkResponse'),
    newDeleteVpcLinkResponse,

    -- ** ExportApi
    ExportApi (ExportApi'),
    newExportApi,
    ExportApiResponse (ExportApiResponse'),
    newExportApiResponse,

    -- ** GetApi
    GetApi (GetApi'),
    newGetApi,
    GetApiResponse (GetApiResponse'),
    newGetApiResponse,

    -- ** GetApiMapping
    GetApiMapping (GetApiMapping'),
    newGetApiMapping,
    GetApiMappingResponse (GetApiMappingResponse'),
    newGetApiMappingResponse,

    -- ** GetApiMappings
    GetApiMappings (GetApiMappings'),
    newGetApiMappings,
    GetApiMappingsResponse (GetApiMappingsResponse'),
    newGetApiMappingsResponse,

    -- ** GetApis (Paginated)
    GetApis (GetApis'),
    newGetApis,
    GetApisResponse (GetApisResponse'),
    newGetApisResponse,

    -- ** GetAuthorizer
    GetAuthorizer (GetAuthorizer'),
    newGetAuthorizer,
    GetAuthorizerResponse (GetAuthorizerResponse'),
    newGetAuthorizerResponse,

    -- ** GetAuthorizers (Paginated)
    GetAuthorizers (GetAuthorizers'),
    newGetAuthorizers,
    GetAuthorizersResponse (GetAuthorizersResponse'),
    newGetAuthorizersResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

    -- ** GetDeployments (Paginated)
    GetDeployments (GetDeployments'),
    newGetDeployments,
    GetDeploymentsResponse (GetDeploymentsResponse'),
    newGetDeploymentsResponse,

    -- ** GetDomainName
    GetDomainName (GetDomainName'),
    newGetDomainName,
    GetDomainNameResponse (GetDomainNameResponse'),
    newGetDomainNameResponse,

    -- ** GetDomainNames (Paginated)
    GetDomainNames (GetDomainNames'),
    newGetDomainNames,
    GetDomainNamesResponse (GetDomainNamesResponse'),
    newGetDomainNamesResponse,

    -- ** GetIntegration
    GetIntegration (GetIntegration'),
    newGetIntegration,
    GetIntegrationResponse' (GetIntegrationResponse''),
    newGetIntegrationResponse',

    -- ** GetIntegrationResponse
    GetIntegrationResponse (GetIntegrationResponse'),
    newGetIntegrationResponse,
    GetIntegrationResponseResponse (GetIntegrationResponseResponse'),
    newGetIntegrationResponseResponse,

    -- ** GetIntegrationResponses (Paginated)
    GetIntegrationResponses (GetIntegrationResponses'),
    newGetIntegrationResponses,
    GetIntegrationResponsesResponse (GetIntegrationResponsesResponse'),
    newGetIntegrationResponsesResponse,

    -- ** GetIntegrations (Paginated)
    GetIntegrations (GetIntegrations'),
    newGetIntegrations,
    GetIntegrationsResponse (GetIntegrationsResponse'),
    newGetIntegrationsResponse,

    -- ** GetModel
    GetModel (GetModel'),
    newGetModel,
    GetModelResponse (GetModelResponse'),
    newGetModelResponse,

    -- ** GetModelTemplate
    GetModelTemplate (GetModelTemplate'),
    newGetModelTemplate,
    GetModelTemplateResponse (GetModelTemplateResponse'),
    newGetModelTemplateResponse,

    -- ** GetModels (Paginated)
    GetModels (GetModels'),
    newGetModels,
    GetModelsResponse (GetModelsResponse'),
    newGetModelsResponse,

    -- ** GetRoute
    GetRoute (GetRoute'),
    newGetRoute,
    GetRouteResponse' (GetRouteResponse''),
    newGetRouteResponse',

    -- ** GetRouteResponse
    GetRouteResponse (GetRouteResponse'),
    newGetRouteResponse,
    GetRouteResponseResponse (GetRouteResponseResponse'),
    newGetRouteResponseResponse,

    -- ** GetRouteResponses (Paginated)
    GetRouteResponses (GetRouteResponses'),
    newGetRouteResponses,
    GetRouteResponsesResponse (GetRouteResponsesResponse'),
    newGetRouteResponsesResponse,

    -- ** GetRoutes (Paginated)
    GetRoutes (GetRoutes'),
    newGetRoutes,
    GetRoutesResponse (GetRoutesResponse'),
    newGetRoutesResponse,

    -- ** GetStage
    GetStage (GetStage'),
    newGetStage,
    GetStageResponse (GetStageResponse'),
    newGetStageResponse,

    -- ** GetStages (Paginated)
    GetStages (GetStages'),
    newGetStages,
    GetStagesResponse (GetStagesResponse'),
    newGetStagesResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** GetVpcLink
    GetVpcLink (GetVpcLink'),
    newGetVpcLink,
    GetVpcLinkResponse (GetVpcLinkResponse'),
    newGetVpcLinkResponse,

    -- ** GetVpcLinks
    GetVpcLinks (GetVpcLinks'),
    newGetVpcLinks,
    GetVpcLinksResponse (GetVpcLinksResponse'),
    newGetVpcLinksResponse,

    -- ** ImportApi
    ImportApi (ImportApi'),
    newImportApi,
    ImportApiResponse (ImportApiResponse'),
    newImportApiResponse,

    -- ** ReimportApi
    ReimportApi (ReimportApi'),
    newReimportApi,
    ReimportApiResponse (ReimportApiResponse'),
    newReimportApiResponse,

    -- ** ResetAuthorizersCache
    ResetAuthorizersCache (ResetAuthorizersCache'),
    newResetAuthorizersCache,
    ResetAuthorizersCacheResponse (ResetAuthorizersCacheResponse'),
    newResetAuthorizersCacheResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApi
    UpdateApi (UpdateApi'),
    newUpdateApi,
    UpdateApiResponse (UpdateApiResponse'),
    newUpdateApiResponse,

    -- ** UpdateApiMapping
    UpdateApiMapping (UpdateApiMapping'),
    newUpdateApiMapping,
    UpdateApiMappingResponse (UpdateApiMappingResponse'),
    newUpdateApiMappingResponse,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    UpdateAuthorizerResponse (UpdateAuthorizerResponse'),
    newUpdateAuthorizerResponse,

    -- ** UpdateDeployment
    UpdateDeployment (UpdateDeployment'),
    newUpdateDeployment,
    UpdateDeploymentResponse (UpdateDeploymentResponse'),
    newUpdateDeploymentResponse,

    -- ** UpdateDomainName
    UpdateDomainName (UpdateDomainName'),
    newUpdateDomainName,
    UpdateDomainNameResponse (UpdateDomainNameResponse'),
    newUpdateDomainNameResponse,

    -- ** UpdateIntegration
    UpdateIntegration (UpdateIntegration'),
    newUpdateIntegration,
    UpdateIntegrationResponse' (UpdateIntegrationResponse''),
    newUpdateIntegrationResponse',

    -- ** UpdateIntegrationResponse
    UpdateIntegrationResponse (UpdateIntegrationResponse'),
    newUpdateIntegrationResponse,
    UpdateIntegrationResponseResponse (UpdateIntegrationResponseResponse'),
    newUpdateIntegrationResponseResponse,

    -- ** UpdateModel
    UpdateModel (UpdateModel'),
    newUpdateModel,
    UpdateModelResponse (UpdateModelResponse'),
    newUpdateModelResponse,

    -- ** UpdateRoute
    UpdateRoute (UpdateRoute'),
    newUpdateRoute,
    UpdateRouteResponse' (UpdateRouteResponse''),
    newUpdateRouteResponse',

    -- ** UpdateRouteResponse
    UpdateRouteResponse (UpdateRouteResponse'),
    newUpdateRouteResponse,
    UpdateRouteResponseResponse (UpdateRouteResponseResponse'),
    newUpdateRouteResponseResponse,

    -- ** UpdateStage
    UpdateStage (UpdateStage'),
    newUpdateStage,
    UpdateStageResponse (UpdateStageResponse'),
    newUpdateStageResponse,

    -- ** UpdateVpcLink
    UpdateVpcLink (UpdateVpcLink'),
    newUpdateVpcLink,
    UpdateVpcLinkResponse (UpdateVpcLinkResponse'),
    newUpdateVpcLinkResponse,

    -- * Types

    -- ** AuthorizationType
    AuthorizationType (..),

    -- ** AuthorizerType
    AuthorizerType (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** ContentHandlingStrategy
    ContentHandlingStrategy (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** DomainNameStatus
    DomainNameStatus (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** IntegrationType
    IntegrationType (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** PassthroughBehavior
    PassthroughBehavior (..),

    -- ** ProtocolType
    ProtocolType (..),

    -- ** SecurityPolicy
    SecurityPolicy (..),

    -- ** VpcLinkStatus
    VpcLinkStatus (..),

    -- ** VpcLinkVersion
    VpcLinkVersion (..),

    -- ** AccessLogSettings
    AccessLogSettings (AccessLogSettings'),
    newAccessLogSettings,

    -- ** Api
    Api (Api'),
    newApi,

    -- ** ApiMapping
    ApiMapping (ApiMapping'),
    newApiMapping,

    -- ** Authorizer
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** Cors
    Cors (Cors'),
    newCors,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DomainName
    DomainName (DomainName'),
    newDomainName,

    -- ** DomainNameConfiguration
    DomainNameConfiguration (DomainNameConfiguration'),
    newDomainNameConfiguration,

    -- ** Integration
    Integration (Integration'),
    newIntegration,

    -- ** IntegrationResponse
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** JWTConfiguration
    JWTConfiguration (JWTConfiguration'),
    newJWTConfiguration,

    -- ** Model
    Model (Model'),
    newModel,

    -- ** MutualTlsAuthentication
    MutualTlsAuthentication (MutualTlsAuthentication'),
    newMutualTlsAuthentication,

    -- ** MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (MutualTlsAuthenticationInput'),
    newMutualTlsAuthenticationInput,

    -- ** ParameterConstraints
    ParameterConstraints (ParameterConstraints'),
    newParameterConstraints,

    -- ** Route
    Route (Route'),
    newRoute,

    -- ** RouteResponse
    RouteResponse (RouteResponse'),
    newRouteResponse,

    -- ** RouteSettings
    RouteSettings (RouteSettings'),
    newRouteSettings,

    -- ** Stage
    Stage (Stage'),
    newStage,

    -- ** TlsConfig
    TlsConfig (TlsConfig'),
    newTlsConfig,

    -- ** TlsConfigInput
    TlsConfigInput (TlsConfigInput'),
    newTlsConfigInput,

    -- ** VpcLink
    VpcLink (VpcLink'),
    newVpcLink,
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
import Amazonka.ApiGatewayV2.Lens
import Amazonka.ApiGatewayV2.ReimportApi
import Amazonka.ApiGatewayV2.ResetAuthorizersCache
import Amazonka.ApiGatewayV2.TagResource
import Amazonka.ApiGatewayV2.Types
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
import Amazonka.ApiGatewayV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ApiGatewayV2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
