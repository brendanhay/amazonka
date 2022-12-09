{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.APIGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-07-09@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon API Gateway
--
-- Amazon API Gateway helps developers deliver robust, secure, and scalable
-- mobile and web application back ends. API Gateway allows developers to
-- securely connect mobile and web applications to APIs that run on AWS
-- Lambda, Amazon EC2, or other publicly addressable web services that are
-- hosted outside of AWS.
module Amazonka.APIGateway
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApiKey
    CreateApiKey (CreateApiKey'),
    newCreateApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** CreateBasePathMapping
    CreateBasePathMapping (CreateBasePathMapping'),
    newCreateBasePathMapping,
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** CreateDocumentationPart
    CreateDocumentationPart (CreateDocumentationPart'),
    newCreateDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** CreateDocumentationVersion
    CreateDocumentationVersion (CreateDocumentationVersion'),
    newCreateDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** CreateDomainName
    CreateDomainName (CreateDomainName'),
    newCreateDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    Model (Model'),
    newModel,

    -- ** CreateRequestValidator
    CreateRequestValidator (CreateRequestValidator'),
    newCreateRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    Resource (Resource'),
    newResource,

    -- ** CreateRestApi
    CreateRestApi (CreateRestApi'),
    newCreateRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** CreateStage
    CreateStage (CreateStage'),
    newCreateStage,
    Stage (Stage'),
    newStage,

    -- ** CreateUsagePlan
    CreateUsagePlan (CreateUsagePlan'),
    newCreateUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** CreateUsagePlanKey
    CreateUsagePlanKey (CreateUsagePlanKey'),
    newCreateUsagePlanKey,
    UsagePlanKey (UsagePlanKey'),
    newUsagePlanKey,

    -- ** CreateVpcLink
    CreateVpcLink (CreateVpcLink'),
    newCreateVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- ** DeleteApiKey
    DeleteApiKey (DeleteApiKey'),
    newDeleteApiKey,
    DeleteApiKeyResponse (DeleteApiKeyResponse'),
    newDeleteApiKeyResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** DeleteBasePathMapping
    DeleteBasePathMapping (DeleteBasePathMapping'),
    newDeleteBasePathMapping,
    DeleteBasePathMappingResponse (DeleteBasePathMappingResponse'),
    newDeleteBasePathMappingResponse,

    -- ** DeleteClientCertificate
    DeleteClientCertificate (DeleteClientCertificate'),
    newDeleteClientCertificate,
    DeleteClientCertificateResponse (DeleteClientCertificateResponse'),
    newDeleteClientCertificateResponse,

    -- ** DeleteDeployment
    DeleteDeployment (DeleteDeployment'),
    newDeleteDeployment,
    DeleteDeploymentResponse (DeleteDeploymentResponse'),
    newDeleteDeploymentResponse,

    -- ** DeleteDocumentationPart
    DeleteDocumentationPart (DeleteDocumentationPart'),
    newDeleteDocumentationPart,
    DeleteDocumentationPartResponse (DeleteDocumentationPartResponse'),
    newDeleteDocumentationPartResponse,

    -- ** DeleteDocumentationVersion
    DeleteDocumentationVersion (DeleteDocumentationVersion'),
    newDeleteDocumentationVersion,
    DeleteDocumentationVersionResponse (DeleteDocumentationVersionResponse'),
    newDeleteDocumentationVersionResponse,

    -- ** DeleteDomainName
    DeleteDomainName (DeleteDomainName'),
    newDeleteDomainName,
    DeleteDomainNameResponse (DeleteDomainNameResponse'),
    newDeleteDomainNameResponse,

    -- ** DeleteGatewayResponse
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,
    DeleteGatewayResponseResponse (DeleteGatewayResponseResponse'),
    newDeleteGatewayResponseResponse,

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

    -- ** DeleteMethod
    DeleteMethod (DeleteMethod'),
    newDeleteMethod,
    DeleteMethodResponse' (DeleteMethodResponse''),
    newDeleteMethodResponse',

    -- ** DeleteMethodResponse
    DeleteMethodResponse (DeleteMethodResponse'),
    newDeleteMethodResponse,
    DeleteMethodResponseResponse (DeleteMethodResponseResponse'),
    newDeleteMethodResponseResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteRequestValidator
    DeleteRequestValidator (DeleteRequestValidator'),
    newDeleteRequestValidator,
    DeleteRequestValidatorResponse (DeleteRequestValidatorResponse'),
    newDeleteRequestValidatorResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** DeleteRestApi
    DeleteRestApi (DeleteRestApi'),
    newDeleteRestApi,
    DeleteRestApiResponse (DeleteRestApiResponse'),
    newDeleteRestApiResponse,

    -- ** DeleteStage
    DeleteStage (DeleteStage'),
    newDeleteStage,
    DeleteStageResponse (DeleteStageResponse'),
    newDeleteStageResponse,

    -- ** DeleteUsagePlan
    DeleteUsagePlan (DeleteUsagePlan'),
    newDeleteUsagePlan,
    DeleteUsagePlanResponse (DeleteUsagePlanResponse'),
    newDeleteUsagePlanResponse,

    -- ** DeleteUsagePlanKey
    DeleteUsagePlanKey (DeleteUsagePlanKey'),
    newDeleteUsagePlanKey,
    DeleteUsagePlanKeyResponse (DeleteUsagePlanKeyResponse'),
    newDeleteUsagePlanKeyResponse,

    -- ** DeleteVpcLink
    DeleteVpcLink (DeleteVpcLink'),
    newDeleteVpcLink,
    DeleteVpcLinkResponse (DeleteVpcLinkResponse'),
    newDeleteVpcLinkResponse,

    -- ** FlushStageAuthorizersCache
    FlushStageAuthorizersCache (FlushStageAuthorizersCache'),
    newFlushStageAuthorizersCache,
    FlushStageAuthorizersCacheResponse (FlushStageAuthorizersCacheResponse'),
    newFlushStageAuthorizersCacheResponse,

    -- ** FlushStageCache
    FlushStageCache (FlushStageCache'),
    newFlushStageCache,
    FlushStageCacheResponse (FlushStageCacheResponse'),
    newFlushStageCacheResponse,

    -- ** GenerateClientCertificate
    GenerateClientCertificate (GenerateClientCertificate'),
    newGenerateClientCertificate,
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    Account (Account'),
    newAccount,

    -- ** GetApiKey
    GetApiKey (GetApiKey'),
    newGetApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** GetApiKeys (Paginated)
    GetApiKeys (GetApiKeys'),
    newGetApiKeys,
    GetApiKeysResponse (GetApiKeysResponse'),
    newGetApiKeysResponse,

    -- ** GetAuthorizer
    GetAuthorizer (GetAuthorizer'),
    newGetAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** GetAuthorizers (Paginated)
    GetAuthorizers (GetAuthorizers'),
    newGetAuthorizers,
    GetAuthorizersResponse (GetAuthorizersResponse'),
    newGetAuthorizersResponse,

    -- ** GetBasePathMapping
    GetBasePathMapping (GetBasePathMapping'),
    newGetBasePathMapping,
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** GetBasePathMappings (Paginated)
    GetBasePathMappings (GetBasePathMappings'),
    newGetBasePathMappings,
    GetBasePathMappingsResponse (GetBasePathMappingsResponse'),
    newGetBasePathMappingsResponse,

    -- ** GetClientCertificate
    GetClientCertificate (GetClientCertificate'),
    newGetClientCertificate,
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** GetClientCertificates (Paginated)
    GetClientCertificates (GetClientCertificates'),
    newGetClientCertificates,
    GetClientCertificatesResponse (GetClientCertificatesResponse'),
    newGetClientCertificatesResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetDeployments (Paginated)
    GetDeployments (GetDeployments'),
    newGetDeployments,
    GetDeploymentsResponse (GetDeploymentsResponse'),
    newGetDeploymentsResponse,

    -- ** GetDocumentationPart
    GetDocumentationPart (GetDocumentationPart'),
    newGetDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** GetDocumentationParts (Paginated)
    GetDocumentationParts (GetDocumentationParts'),
    newGetDocumentationParts,
    GetDocumentationPartsResponse (GetDocumentationPartsResponse'),
    newGetDocumentationPartsResponse,

    -- ** GetDocumentationVersion
    GetDocumentationVersion (GetDocumentationVersion'),
    newGetDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** GetDocumentationVersions (Paginated)
    GetDocumentationVersions (GetDocumentationVersions'),
    newGetDocumentationVersions,
    GetDocumentationVersionsResponse (GetDocumentationVersionsResponse'),
    newGetDocumentationVersionsResponse,

    -- ** GetDomainName
    GetDomainName (GetDomainName'),
    newGetDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** GetDomainNames (Paginated)
    GetDomainNames (GetDomainNames'),
    newGetDomainNames,
    GetDomainNamesResponse (GetDomainNamesResponse'),
    newGetDomainNamesResponse,

    -- ** GetExport
    GetExport (GetExport'),
    newGetExport,
    GetExportResponse (GetExportResponse'),
    newGetExportResponse,

    -- ** GetGatewayResponse
    GetGatewayResponse (GetGatewayResponse'),
    newGetGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** GetGatewayResponses (Paginated)
    GetGatewayResponses (GetGatewayResponses'),
    newGetGatewayResponses,
    GetGatewayResponsesResponse (GetGatewayResponsesResponse'),
    newGetGatewayResponsesResponse,

    -- ** GetIntegration
    GetIntegration (GetIntegration'),
    newGetIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** GetIntegrationResponse
    GetIntegrationResponse (GetIntegrationResponse'),
    newGetIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** GetMethod
    GetMethod (GetMethod'),
    newGetMethod,
    Method (Method'),
    newMethod,

    -- ** GetMethodResponse
    GetMethodResponse (GetMethodResponse'),
    newGetMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** GetModel
    GetModel (GetModel'),
    newGetModel,
    Model (Model'),
    newModel,

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

    -- ** GetRequestValidator
    GetRequestValidator (GetRequestValidator'),
    newGetRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** GetRequestValidators (Paginated)
    GetRequestValidators (GetRequestValidators'),
    newGetRequestValidators,
    GetRequestValidatorsResponse (GetRequestValidatorsResponse'),
    newGetRequestValidatorsResponse,

    -- ** GetResource
    GetResource (GetResource'),
    newGetResource,
    Resource (Resource'),
    newResource,

    -- ** GetResources (Paginated)
    GetResources (GetResources'),
    newGetResources,
    GetResourcesResponse (GetResourcesResponse'),
    newGetResourcesResponse,

    -- ** GetRestApi
    GetRestApi (GetRestApi'),
    newGetRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** GetRestApis (Paginated)
    GetRestApis (GetRestApis'),
    newGetRestApis,
    GetRestApisResponse (GetRestApisResponse'),
    newGetRestApisResponse,

    -- ** GetSdk
    GetSdk (GetSdk'),
    newGetSdk,
    GetSdkResponse (GetSdkResponse'),
    newGetSdkResponse,

    -- ** GetSdkType
    GetSdkType (GetSdkType'),
    newGetSdkType,
    SdkType (SdkType'),
    newSdkType,

    -- ** GetSdkTypes (Paginated)
    GetSdkTypes (GetSdkTypes'),
    newGetSdkTypes,
    GetSdkTypesResponse (GetSdkTypesResponse'),
    newGetSdkTypesResponse,

    -- ** GetStage
    GetStage (GetStage'),
    newGetStage,
    Stage (Stage'),
    newStage,

    -- ** GetStages
    GetStages (GetStages'),
    newGetStages,
    GetStagesResponse (GetStagesResponse'),
    newGetStagesResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** GetUsage (Paginated)
    GetUsage (GetUsage'),
    newGetUsage,
    Usage (Usage'),
    newUsage,

    -- ** GetUsagePlan
    GetUsagePlan (GetUsagePlan'),
    newGetUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** GetUsagePlanKey
    GetUsagePlanKey (GetUsagePlanKey'),
    newGetUsagePlanKey,
    UsagePlanKey (UsagePlanKey'),
    newUsagePlanKey,

    -- ** GetUsagePlanKeys (Paginated)
    GetUsagePlanKeys (GetUsagePlanKeys'),
    newGetUsagePlanKeys,
    GetUsagePlanKeysResponse (GetUsagePlanKeysResponse'),
    newGetUsagePlanKeysResponse,

    -- ** GetUsagePlans (Paginated)
    GetUsagePlans (GetUsagePlans'),
    newGetUsagePlans,
    GetUsagePlansResponse (GetUsagePlansResponse'),
    newGetUsagePlansResponse,

    -- ** GetVpcLink
    GetVpcLink (GetVpcLink'),
    newGetVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- ** GetVpcLinks (Paginated)
    GetVpcLinks (GetVpcLinks'),
    newGetVpcLinks,
    GetVpcLinksResponse (GetVpcLinksResponse'),
    newGetVpcLinksResponse,

    -- ** ImportApiKeys
    ImportApiKeys (ImportApiKeys'),
    newImportApiKeys,
    ImportApiKeysResponse (ImportApiKeysResponse'),
    newImportApiKeysResponse,

    -- ** ImportDocumentationParts
    ImportDocumentationParts (ImportDocumentationParts'),
    newImportDocumentationParts,
    ImportDocumentationPartsResponse (ImportDocumentationPartsResponse'),
    newImportDocumentationPartsResponse,

    -- ** ImportRestApi
    ImportRestApi (ImportRestApi'),
    newImportRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** PutGatewayResponse
    PutGatewayResponse (PutGatewayResponse'),
    newPutGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** PutIntegration
    PutIntegration (PutIntegration'),
    newPutIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** PutIntegrationResponse
    PutIntegrationResponse (PutIntegrationResponse'),
    newPutIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** PutMethod
    PutMethod (PutMethod'),
    newPutMethod,
    Method (Method'),
    newMethod,

    -- ** PutMethodResponse
    PutMethodResponse (PutMethodResponse'),
    newPutMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** PutRestApi
    PutRestApi (PutRestApi'),
    newPutRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestInvokeAuthorizer
    TestInvokeAuthorizer (TestInvokeAuthorizer'),
    newTestInvokeAuthorizer,
    TestInvokeAuthorizerResponse (TestInvokeAuthorizerResponse'),
    newTestInvokeAuthorizerResponse,

    -- ** TestInvokeMethod
    TestInvokeMethod (TestInvokeMethod'),
    newTestInvokeMethod,
    TestInvokeMethodResponse (TestInvokeMethodResponse'),
    newTestInvokeMethodResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccount
    UpdateAccount (UpdateAccount'),
    newUpdateAccount,
    Account (Account'),
    newAccount,

    -- ** UpdateApiKey
    UpdateApiKey (UpdateApiKey'),
    newUpdateApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** UpdateBasePathMapping
    UpdateBasePathMapping (UpdateBasePathMapping'),
    newUpdateBasePathMapping,
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** UpdateClientCertificate
    UpdateClientCertificate (UpdateClientCertificate'),
    newUpdateClientCertificate,
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** UpdateDeployment
    UpdateDeployment (UpdateDeployment'),
    newUpdateDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** UpdateDocumentationPart
    UpdateDocumentationPart (UpdateDocumentationPart'),
    newUpdateDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** UpdateDocumentationVersion
    UpdateDocumentationVersion (UpdateDocumentationVersion'),
    newUpdateDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** UpdateDomainName
    UpdateDomainName (UpdateDomainName'),
    newUpdateDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** UpdateGatewayResponse
    UpdateGatewayResponse (UpdateGatewayResponse'),
    newUpdateGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** UpdateIntegration
    UpdateIntegration (UpdateIntegration'),
    newUpdateIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** UpdateIntegrationResponse
    UpdateIntegrationResponse (UpdateIntegrationResponse'),
    newUpdateIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** UpdateMethod
    UpdateMethod (UpdateMethod'),
    newUpdateMethod,
    Method (Method'),
    newMethod,

    -- ** UpdateMethodResponse
    UpdateMethodResponse (UpdateMethodResponse'),
    newUpdateMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** UpdateModel
    UpdateModel (UpdateModel'),
    newUpdateModel,
    Model (Model'),
    newModel,

    -- ** UpdateRequestValidator
    UpdateRequestValidator (UpdateRequestValidator'),
    newUpdateRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    Resource (Resource'),
    newResource,

    -- ** UpdateRestApi
    UpdateRestApi (UpdateRestApi'),
    newUpdateRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** UpdateStage
    UpdateStage (UpdateStage'),
    newUpdateStage,
    Stage (Stage'),
    newStage,

    -- ** UpdateUsage
    UpdateUsage (UpdateUsage'),
    newUpdateUsage,
    Usage (Usage'),
    newUsage,

    -- ** UpdateUsagePlan
    UpdateUsagePlan (UpdateUsagePlan'),
    newUpdateUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** UpdateVpcLink
    UpdateVpcLink (UpdateVpcLink'),
    newUpdateVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- * Types

    -- ** ApiKeySourceType
    ApiKeySourceType (..),

    -- ** ApiKeysFormat
    ApiKeysFormat (..),

    -- ** AuthorizerType
    AuthorizerType (..),

    -- ** CacheClusterSize
    CacheClusterSize (..),

    -- ** CacheClusterStatus
    CacheClusterStatus (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** ContentHandlingStrategy
    ContentHandlingStrategy (..),

    -- ** DocumentationPartType
    DocumentationPartType (..),

    -- ** DomainNameStatus
    DomainNameStatus (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** GatewayResponseType
    GatewayResponseType (..),

    -- ** IntegrationType
    IntegrationType (..),

    -- ** LocationStatusType
    LocationStatusType (..),

    -- ** Op
    Op (..),

    -- ** PutMode
    PutMode (..),

    -- ** QuotaPeriodType
    QuotaPeriodType (..),

    -- ** SecurityPolicy
    SecurityPolicy (..),

    -- ** UnauthorizedCacheControlHeaderStrategy
    UnauthorizedCacheControlHeaderStrategy (..),

    -- ** VpcLinkStatus
    VpcLinkStatus (..),

    -- ** AccessLogSettings
    AccessLogSettings (AccessLogSettings'),
    newAccessLogSettings,

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** ApiKey
    ApiKey (ApiKey'),
    newApiKey,

    -- ** ApiStage
    ApiStage (ApiStage'),
    newApiStage,

    -- ** Authorizer
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** BasePathMapping
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** CanarySettings
    CanarySettings (CanarySettings'),
    newCanarySettings,

    -- ** ClientCertificate
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DeploymentCanarySettings
    DeploymentCanarySettings (DeploymentCanarySettings'),
    newDeploymentCanarySettings,

    -- ** DocumentationPart
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** DocumentationPartLocation
    DocumentationPartLocation (DocumentationPartLocation'),
    newDocumentationPartLocation,

    -- ** DocumentationVersion
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** DomainName
    DomainName (DomainName'),
    newDomainName,

    -- ** EndpointConfiguration
    EndpointConfiguration (EndpointConfiguration'),
    newEndpointConfiguration,

    -- ** GatewayResponse
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** Integration
    Integration (Integration'),
    newIntegration,

    -- ** IntegrationResponse
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** Method
    Method (Method'),
    newMethod,

    -- ** MethodResponse
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** MethodSetting
    MethodSetting (MethodSetting'),
    newMethodSetting,

    -- ** MethodSnapshot
    MethodSnapshot (MethodSnapshot'),
    newMethodSnapshot,

    -- ** Model
    Model (Model'),
    newModel,

    -- ** MutualTlsAuthentication
    MutualTlsAuthentication (MutualTlsAuthentication'),
    newMutualTlsAuthentication,

    -- ** MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (MutualTlsAuthenticationInput'),
    newMutualTlsAuthenticationInput,

    -- ** PatchOperation
    PatchOperation (PatchOperation'),
    newPatchOperation,

    -- ** QuotaSettings
    QuotaSettings (QuotaSettings'),
    newQuotaSettings,

    -- ** RequestValidator
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** RestApi
    RestApi (RestApi'),
    newRestApi,

    -- ** SdkConfigurationProperty
    SdkConfigurationProperty (SdkConfigurationProperty'),
    newSdkConfigurationProperty,

    -- ** SdkType
    SdkType (SdkType'),
    newSdkType,

    -- ** Stage
    Stage (Stage'),
    newStage,

    -- ** StageKey
    StageKey (StageKey'),
    newStageKey,

    -- ** ThrottleSettings
    ThrottleSettings (ThrottleSettings'),
    newThrottleSettings,

    -- ** TlsConfig
    TlsConfig (TlsConfig'),
    newTlsConfig,

    -- ** Usage
    Usage (Usage'),
    newUsage,

    -- ** UsagePlan
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** UsagePlanKey
    UsagePlanKey (UsagePlanKey'),
    newUsagePlanKey,

    -- ** VpcLink
    VpcLink (VpcLink'),
    newVpcLink,
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
import Amazonka.APIGateway.Lens
import Amazonka.APIGateway.PutGatewayResponse
import Amazonka.APIGateway.PutIntegration
import Amazonka.APIGateway.PutIntegrationResponse
import Amazonka.APIGateway.PutMethod
import Amazonka.APIGateway.PutMethodResponse
import Amazonka.APIGateway.PutRestApi
import Amazonka.APIGateway.TagResource
import Amazonka.APIGateway.TestInvokeAuthorizer
import Amazonka.APIGateway.TestInvokeMethod
import Amazonka.APIGateway.Types
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
import Amazonka.APIGateway.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'APIGateway'.

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
