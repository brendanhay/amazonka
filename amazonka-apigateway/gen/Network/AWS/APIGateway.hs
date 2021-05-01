{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon API Gateway
--
-- Amazon API Gateway helps developers deliver robust, secure, and scalable
-- mobile and web application back ends. API Gateway allows developers to
-- securely connect mobile and web applications to APIs that run on AWS
-- Lambda, Amazon EC2, or other publicly addressable web services that are
-- hosted outside of AWS.
module Network.AWS.APIGateway
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GenerateClientCertificate
    GenerateClientCertificate (GenerateClientCertificate'),
    newGenerateClientCertificate,
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** UpdateIntegration
    UpdateIntegration (UpdateIntegration'),
    newUpdateIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** DeleteUsagePlan
    DeleteUsagePlan (DeleteUsagePlan'),
    newDeleteUsagePlan,
    DeleteUsagePlanResponse (DeleteUsagePlanResponse'),
    newDeleteUsagePlanResponse,

    -- ** DeleteIntegration
    DeleteIntegration (DeleteIntegration'),
    newDeleteIntegration,
    DeleteIntegrationResponse' (DeleteIntegrationResponse''),
    newDeleteIntegrationResponse',

    -- ** GetUsagePlanKey
    GetUsagePlanKey (GetUsagePlanKey'),
    newGetUsagePlanKey,
    UsagePlanKey (UsagePlanKey'),
    newUsagePlanKey,

    -- ** GetAuthorizer
    GetAuthorizer (GetAuthorizer'),
    newGetAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** UpdateUsagePlan
    UpdateUsagePlan (UpdateUsagePlan'),
    newUpdateUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** GetDeployments (Paginated)
    GetDeployments (GetDeployments'),
    newGetDeployments,
    GetDeploymentsResponse (GetDeploymentsResponse'),
    newGetDeploymentsResponse,

    -- ** DeleteIntegrationResponse
    DeleteIntegrationResponse (DeleteIntegrationResponse'),
    newDeleteIntegrationResponse,
    DeleteIntegrationResponseResponse (DeleteIntegrationResponseResponse'),
    newDeleteIntegrationResponseResponse,

    -- ** FlushStageCache
    FlushStageCache (FlushStageCache'),
    newFlushStageCache,
    FlushStageCacheResponse (FlushStageCacheResponse'),
    newFlushStageCacheResponse,

    -- ** DeleteVpcLink
    DeleteVpcLink (DeleteVpcLink'),
    newDeleteVpcLink,
    DeleteVpcLinkResponse (DeleteVpcLinkResponse'),
    newDeleteVpcLinkResponse,

    -- ** DeleteDocumentationPart
    DeleteDocumentationPart (DeleteDocumentationPart'),
    newDeleteDocumentationPart,
    DeleteDocumentationPartResponse (DeleteDocumentationPartResponse'),
    newDeleteDocumentationPartResponse,

    -- ** UpdateVpcLink
    UpdateVpcLink (UpdateVpcLink'),
    newUpdateVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- ** CreateRestApi
    CreateRestApi (CreateRestApi'),
    newCreateRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** UpdateUsage
    UpdateUsage (UpdateUsage'),
    newUpdateUsage,
    Usage (Usage'),
    newUsage,

    -- ** UpdateIntegrationResponse
    UpdateIntegrationResponse (UpdateIntegrationResponse'),
    newUpdateIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** UpdateDocumentationPart
    UpdateDocumentationPart (UpdateDocumentationPart'),
    newUpdateDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** GetSdkTypes (Paginated)
    GetSdkTypes (GetSdkTypes'),
    newGetSdkTypes,
    GetSdkTypesResponse (GetSdkTypesResponse'),
    newGetSdkTypesResponse,

    -- ** GetBasePathMappings (Paginated)
    GetBasePathMappings (GetBasePathMappings'),
    newGetBasePathMappings,
    GetBasePathMappingsResponse (GetBasePathMappingsResponse'),
    newGetBasePathMappingsResponse,

    -- ** GetModel
    GetModel (GetModel'),
    newGetModel,
    Model (Model'),
    newModel,

    -- ** GetClientCertificates (Paginated)
    GetClientCertificates (GetClientCertificates'),
    newGetClientCertificates,
    GetClientCertificatesResponse (GetClientCertificatesResponse'),
    newGetClientCertificatesResponse,

    -- ** TestInvokeMethod
    TestInvokeMethod (TestInvokeMethod'),
    newTestInvokeMethod,
    TestInvokeMethodResponse (TestInvokeMethodResponse'),
    newTestInvokeMethodResponse,

    -- ** PutRestApi
    PutRestApi (PutRestApi'),
    newPutRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** GetApiKey
    GetApiKey (GetApiKey'),
    newGetApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** GetGatewayResponses (Paginated)
    GetGatewayResponses (GetGatewayResponses'),
    newGetGatewayResponses,
    GetGatewayResponsesResponse (GetGatewayResponsesResponse'),
    newGetGatewayResponsesResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteMethod
    DeleteMethod (DeleteMethod'),
    newDeleteMethod,
    DeleteMethodResponse' (DeleteMethodResponse''),
    newDeleteMethodResponse',

    -- ** GetDocumentationVersion
    GetDocumentationVersion (GetDocumentationVersion'),
    newGetDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** UpdateMethod
    UpdateMethod (UpdateMethod'),
    newUpdateMethod,
    Method (Method'),
    newMethod,

    -- ** UpdateModel
    UpdateModel (UpdateModel'),
    newUpdateModel,
    Model (Model'),
    newModel,

    -- ** UpdateDomainName
    UpdateDomainName (UpdateDomainName'),
    newUpdateDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    Resource (Resource'),
    newResource,

    -- ** DeleteDomainName
    DeleteDomainName (DeleteDomainName'),
    newDeleteDomainName,
    DeleteDomainNameResponse (DeleteDomainNameResponse'),
    newDeleteDomainNameResponse,

    -- ** UpdateRequestValidator
    UpdateRequestValidator (UpdateRequestValidator'),
    newUpdateRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** DeleteRequestValidator
    DeleteRequestValidator (DeleteRequestValidator'),
    newDeleteRequestValidator,
    DeleteRequestValidatorResponse (DeleteRequestValidatorResponse'),
    newDeleteRequestValidatorResponse,

    -- ** GetUsagePlans (Paginated)
    GetUsagePlans (GetUsagePlans'),
    newGetUsagePlans,
    GetUsagePlansResponse (GetUsagePlansResponse'),
    newGetUsagePlansResponse,

    -- ** CreateRequestValidator
    CreateRequestValidator (CreateRequestValidator'),
    newCreateRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** CreateDomainName
    CreateDomainName (CreateDomainName'),
    newCreateDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** GetVpcLink
    GetVpcLink (GetVpcLink'),
    newGetVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    Resource (Resource'),
    newResource,

    -- ** GetDocumentationPart
    GetDocumentationPart (GetDocumentationPart'),
    newGetDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** GetUsage (Paginated)
    GetUsage (GetUsage'),
    newGetUsage,
    Usage (Usage'),
    newUsage,

    -- ** ImportDocumentationParts
    ImportDocumentationParts (ImportDocumentationParts'),
    newImportDocumentationParts,
    ImportDocumentationPartsResponse (ImportDocumentationPartsResponse'),
    newImportDocumentationPartsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** GetIntegrationResponse
    GetIntegrationResponse (GetIntegrationResponse'),
    newGetIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** DeleteDeployment
    DeleteDeployment (DeleteDeployment'),
    newDeleteDeployment,
    DeleteDeploymentResponse (DeleteDeploymentResponse'),
    newDeleteDeploymentResponse,

    -- ** CreateStage
    CreateStage (CreateStage'),
    newCreateStage,
    Stage (Stage'),
    newStage,

    -- ** GetIntegration
    GetIntegration (GetIntegration'),
    newGetIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UpdateDeployment
    UpdateDeployment (UpdateDeployment'),
    newUpdateDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetUsagePlan
    GetUsagePlan (GetUsagePlan'),
    newGetUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** GetRestApis (Paginated)
    GetRestApis (GetRestApis'),
    newGetRestApis,
    GetRestApisResponse (GetRestApisResponse'),
    newGetRestApisResponse,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** DeleteStage
    DeleteStage (DeleteStage'),
    newDeleteStage,
    DeleteStageResponse (DeleteStageResponse'),
    newDeleteStageResponse,

    -- ** UpdateStage
    UpdateStage (UpdateStage'),
    newUpdateStage,
    Stage (Stage'),
    newStage,

    -- ** CreateUsagePlanKey
    CreateUsagePlanKey (CreateUsagePlanKey'),
    newCreateUsagePlanKey,
    UsagePlanKey (UsagePlanKey'),
    newUsagePlanKey,

    -- ** GetGatewayResponse
    GetGatewayResponse (GetGatewayResponse'),
    newGetGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** ImportRestApi
    ImportRestApi (ImportRestApi'),
    newImportRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** PutMethodResponse
    PutMethodResponse (PutMethodResponse'),
    newPutMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** GetBasePathMapping
    GetBasePathMapping (GetBasePathMapping'),
    newGetBasePathMapping,
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** GetRequestValidators (Paginated)
    GetRequestValidators (GetRequestValidators'),
    newGetRequestValidators,
    GetRequestValidatorsResponse (GetRequestValidatorsResponse'),
    newGetRequestValidatorsResponse,

    -- ** GetDomainNames (Paginated)
    GetDomainNames (GetDomainNames'),
    newGetDomainNames,
    GetDomainNamesResponse (GetDomainNamesResponse'),
    newGetDomainNamesResponse,

    -- ** GetSdkType
    GetSdkType (GetSdkType'),
    newGetSdkType,
    SdkType (SdkType'),
    newSdkType,

    -- ** PutGatewayResponse
    PutGatewayResponse (PutGatewayResponse'),
    newPutGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** GetExport
    GetExport (GetExport'),
    newGetExport,
    GetExportResponse (GetExportResponse'),
    newGetExportResponse,

    -- ** GetClientCertificate
    GetClientCertificate (GetClientCertificate'),
    newGetClientCertificate,
    ClientCertificate (ClientCertificate'),
    newClientCertificate,

    -- ** TestInvokeAuthorizer
    TestInvokeAuthorizer (TestInvokeAuthorizer'),
    newTestInvokeAuthorizer,
    TestInvokeAuthorizerResponse (TestInvokeAuthorizerResponse'),
    newTestInvokeAuthorizerResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    Account (Account'),
    newAccount,

    -- ** PutIntegration
    PutIntegration (PutIntegration'),
    newPutIntegration,
    Integration (Integration'),
    newIntegration,

    -- ** GetResources (Paginated)
    GetResources (GetResources'),
    newGetResources,
    GetResourcesResponse (GetResourcesResponse'),
    newGetResourcesResponse,

    -- ** GetResource
    GetResource (GetResource'),
    newGetResource,
    Resource (Resource'),
    newResource,

    -- ** CreateDocumentationVersion
    CreateDocumentationVersion (CreateDocumentationVersion'),
    newCreateDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** GetAuthorizers (Paginated)
    GetAuthorizers (GetAuthorizers'),
    newGetAuthorizers,
    GetAuthorizersResponse (GetAuthorizersResponse'),
    newGetAuthorizersResponse,

    -- ** PutIntegrationResponse
    PutIntegrationResponse (PutIntegrationResponse'),
    newPutIntegrationResponse,
    IntegrationResponse (IntegrationResponse'),
    newIntegrationResponse,

    -- ** GetUsagePlanKeys (Paginated)
    GetUsagePlanKeys (GetUsagePlanKeys'),
    newGetUsagePlanKeys,
    GetUsagePlanKeysResponse (GetUsagePlanKeysResponse'),
    newGetUsagePlanKeysResponse,

    -- ** CreateDocumentationPart
    CreateDocumentationPart (CreateDocumentationPart'),
    newCreateDocumentationPart,
    DocumentationPart (DocumentationPart'),
    newDocumentationPart,

    -- ** GetStages
    GetStages (GetStages'),
    newGetStages,
    GetStagesResponse (GetStagesResponse'),
    newGetStagesResponse,

    -- ** GetDomainName
    GetDomainName (GetDomainName'),
    newGetDomainName,
    DomainName (DomainName'),
    newDomainName,

    -- ** GetModelTemplate
    GetModelTemplate (GetModelTemplate'),
    newGetModelTemplate,
    GetModelTemplateResponse (GetModelTemplateResponse'),
    newGetModelTemplateResponse,

    -- ** DeleteRestApi
    DeleteRestApi (DeleteRestApi'),
    newDeleteRestApi,
    DeleteRestApiResponse (DeleteRestApiResponse'),
    newDeleteRestApiResponse,

    -- ** GetMethod
    GetMethod (GetMethod'),
    newGetMethod,
    Method (Method'),
    newMethod,

    -- ** UpdateRestApi
    UpdateRestApi (UpdateRestApi'),
    newUpdateRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** CreateVpcLink
    CreateVpcLink (CreateVpcLink'),
    newCreateVpcLink,
    VpcLink (VpcLink'),
    newVpcLink,

    -- ** GetRequestValidator
    GetRequestValidator (GetRequestValidator'),
    newGetRequestValidator,
    RequestValidator (RequestValidator'),
    newRequestValidator,

    -- ** UpdateDocumentationVersion
    UpdateDocumentationVersion (UpdateDocumentationVersion'),
    newUpdateDocumentationVersion,
    DocumentationVersion (DocumentationVersion'),
    newDocumentationVersion,

    -- ** ImportApiKeys
    ImportApiKeys (ImportApiKeys'),
    newImportApiKeys,
    ImportApiKeysResponse (ImportApiKeysResponse'),
    newImportApiKeysResponse,

    -- ** DeleteDocumentationVersion
    DeleteDocumentationVersion (DeleteDocumentationVersion'),
    newDeleteDocumentationVersion,
    DeleteDocumentationVersionResponse (DeleteDocumentationVersionResponse'),
    newDeleteDocumentationVersionResponse,

    -- ** PutMethod
    PutMethod (PutMethod'),
    newPutMethod,
    Method (Method'),
    newMethod,

    -- ** DeleteApiKey
    DeleteApiKey (DeleteApiKey'),
    newDeleteApiKey,
    DeleteApiKeyResponse (DeleteApiKeyResponse'),
    newDeleteApiKeyResponse,

    -- ** FlushStageAuthorizersCache
    FlushStageAuthorizersCache (FlushStageAuthorizersCache'),
    newFlushStageAuthorizersCache,
    FlushStageAuthorizersCacheResponse (FlushStageAuthorizersCacheResponse'),
    newFlushStageAuthorizersCacheResponse,

    -- ** UpdateApiKey
    UpdateApiKey (UpdateApiKey'),
    newUpdateApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** GetRestApi
    GetRestApi (GetRestApi'),
    newGetRestApi,
    RestApi (RestApi'),
    newRestApi,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    Model (Model'),
    newModel,

    -- ** CreateApiKey
    CreateApiKey (CreateApiKey'),
    newCreateApiKey,
    ApiKey (ApiKey'),
    newApiKey,

    -- ** DeleteUsagePlanKey
    DeleteUsagePlanKey (DeleteUsagePlanKey'),
    newDeleteUsagePlanKey,
    DeleteUsagePlanKeyResponse (DeleteUsagePlanKeyResponse'),
    newDeleteUsagePlanKeyResponse,

    -- ** UpdateAccount
    UpdateAccount (UpdateAccount'),
    newUpdateAccount,
    Account (Account'),
    newAccount,

    -- ** GetVpcLinks (Paginated)
    GetVpcLinks (GetVpcLinks'),
    newGetVpcLinks,
    GetVpcLinksResponse (GetVpcLinksResponse'),
    newGetVpcLinksResponse,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    Authorizer (Authorizer'),
    newAuthorizer,

    -- ** CreateBasePathMapping
    CreateBasePathMapping (CreateBasePathMapping'),
    newCreateBasePathMapping,
    BasePathMapping (BasePathMapping'),
    newBasePathMapping,

    -- ** GetDocumentationParts (Paginated)
    GetDocumentationParts (GetDocumentationParts'),
    newGetDocumentationParts,
    GetDocumentationPartsResponse (GetDocumentationPartsResponse'),
    newGetDocumentationPartsResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** DeleteClientCertificate
    DeleteClientCertificate (DeleteClientCertificate'),
    newDeleteClientCertificate,
    DeleteClientCertificateResponse (DeleteClientCertificateResponse'),
    newDeleteClientCertificateResponse,

    -- ** DeleteMethodResponse
    DeleteMethodResponse (DeleteMethodResponse'),
    newDeleteMethodResponse,
    DeleteMethodResponseResponse (DeleteMethodResponseResponse'),
    newDeleteMethodResponseResponse,

    -- ** DeleteBasePathMapping
    DeleteBasePathMapping (DeleteBasePathMapping'),
    newDeleteBasePathMapping,
    DeleteBasePathMappingResponse (DeleteBasePathMappingResponse'),
    newDeleteBasePathMappingResponse,

    -- ** GetDocumentationVersions (Paginated)
    GetDocumentationVersions (GetDocumentationVersions'),
    newGetDocumentationVersions,
    GetDocumentationVersionsResponse (GetDocumentationVersionsResponse'),
    newGetDocumentationVersionsResponse,

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

    -- ** UpdateMethodResponse
    UpdateMethodResponse (UpdateMethodResponse'),
    newUpdateMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    Deployment (Deployment'),
    newDeployment,

    -- ** GetApiKeys (Paginated)
    GetApiKeys (GetApiKeys'),
    newGetApiKeys,
    GetApiKeysResponse (GetApiKeysResponse'),
    newGetApiKeysResponse,

    -- ** CreateUsagePlan
    CreateUsagePlan (CreateUsagePlan'),
    newCreateUsagePlan,
    UsagePlan (UsagePlan'),
    newUsagePlan,

    -- ** UpdateGatewayResponse
    UpdateGatewayResponse (UpdateGatewayResponse'),
    newUpdateGatewayResponse,
    GatewayResponse (GatewayResponse'),
    newGatewayResponse,

    -- ** GetSdk
    GetSdk (GetSdk'),
    newGetSdk,
    GetSdkResponse (GetSdkResponse'),
    newGetSdkResponse,

    -- ** GetMethodResponse
    GetMethodResponse (GetMethodResponse'),
    newGetMethodResponse,
    MethodResponse (MethodResponse'),
    newMethodResponse,

    -- ** GetModels (Paginated)
    GetModels (GetModels'),
    newGetModels,
    GetModelsResponse (GetModelsResponse'),
    newGetModelsResponse,

    -- ** GetStage
    GetStage (GetStage'),
    newGetStage,
    Stage (Stage'),
    newStage,

    -- ** DeleteGatewayResponse
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,
    DeleteGatewayResponseResponse (DeleteGatewayResponseResponse'),
    newDeleteGatewayResponseResponse,

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
import Network.AWS.APIGateway.Lens
import Network.AWS.APIGateway.PutGatewayResponse
import Network.AWS.APIGateway.PutIntegration
import Network.AWS.APIGateway.PutIntegrationResponse
import Network.AWS.APIGateway.PutMethod
import Network.AWS.APIGateway.PutMethodResponse
import Network.AWS.APIGateway.PutRestApi
import Network.AWS.APIGateway.TagResource
import Network.AWS.APIGateway.TestInvokeAuthorizer
import Network.AWS.APIGateway.TestInvokeMethod
import Network.AWS.APIGateway.Types
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
import Network.AWS.APIGateway.Waiters

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
