{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon API Gateway__
--
-- Amazon API Gateway helps developers deliver robust, secure, and scalable mobile and web application back ends. API Gateway allows developers to securely connect mobile and web applications to APIs that run on AWS Lambda, Amazon EC2, or other publicly addressable web services that are hosted outside of AWS.
module Network.AWS.APIGateway
  ( -- * Service configuration
    apiGatewayService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetResource
    module Network.AWS.APIGateway.GetResource,

    -- ** GetDeployments (Paginated)
    module Network.AWS.APIGateway.GetDeployments,

    -- ** GetDeployment
    module Network.AWS.APIGateway.GetDeployment,

    -- ** GetTags
    module Network.AWS.APIGateway.GetTags,

    -- ** DeleteGatewayResponse
    module Network.AWS.APIGateway.DeleteGatewayResponse,

    -- ** UpdateGatewayResponse
    module Network.AWS.APIGateway.UpdateGatewayResponse,

    -- ** CreateUsagePlan
    module Network.AWS.APIGateway.CreateUsagePlan,

    -- ** GetDomainNames (Paginated)
    module Network.AWS.APIGateway.GetDomainNames,

    -- ** GetClientCertificate
    module Network.AWS.APIGateway.GetClientCertificate,

    -- ** PutGatewayResponse
    module Network.AWS.APIGateway.PutGatewayResponse,

    -- ** GetSDKType
    module Network.AWS.APIGateway.GetSDKType,

    -- ** GetMethodResponse
    module Network.AWS.APIGateway.GetMethodResponse,

    -- ** GetModels (Paginated)
    module Network.AWS.APIGateway.GetModels,

    -- ** GetBasePathMapping
    module Network.AWS.APIGateway.GetBasePathMapping,

    -- ** GetRequestValidators (Paginated)
    module Network.AWS.APIGateway.GetRequestValidators,

    -- ** PutMethodResponse
    module Network.AWS.APIGateway.PutMethodResponse,

    -- ** ImportRestAPI
    module Network.AWS.APIGateway.ImportRestAPI,

    -- ** DeleteMethodResponse
    module Network.AWS.APIGateway.DeleteMethodResponse,

    -- ** UpdateMethodResponse
    module Network.AWS.APIGateway.UpdateMethodResponse,

    -- ** DeleteStage
    module Network.AWS.APIGateway.DeleteStage,

    -- ** UpdateStage
    module Network.AWS.APIGateway.UpdateStage,

    -- ** GetRestAPIs (Paginated)
    module Network.AWS.APIGateway.GetRestAPIs,

    -- ** GetDocumentationVersions (Paginated)
    module Network.AWS.APIGateway.GetDocumentationVersions,

    -- ** CreateDeployment
    module Network.AWS.APIGateway.CreateDeployment,

    -- ** GetVPCLinks (Paginated)
    module Network.AWS.APIGateway.GetVPCLinks,

    -- ** CreateBasePathMapping
    module Network.AWS.APIGateway.CreateBasePathMapping,

    -- ** GetIntegration
    module Network.AWS.APIGateway.GetIntegration,

    -- ** GetDocumentationParts (Paginated)
    module Network.AWS.APIGateway.GetDocumentationParts,

    -- ** UpdateAccount
    module Network.AWS.APIGateway.UpdateAccount,

    -- ** GetUsagePlan
    module Network.AWS.APIGateway.GetUsagePlan,

    -- ** DeleteDeployment
    module Network.AWS.APIGateway.DeleteDeployment,

    -- ** UpdateDeployment
    module Network.AWS.APIGateway.UpdateDeployment,

    -- ** GetDocumentationPart
    module Network.AWS.APIGateway.GetDocumentationPart,

    -- ** DeleteResource
    module Network.AWS.APIGateway.DeleteResource,

    -- ** UpdateResource
    module Network.AWS.APIGateway.UpdateResource,

    -- ** CreateRequestValidator
    module Network.AWS.APIGateway.CreateRequestValidator,

    -- ** ImportDocumentationParts
    module Network.AWS.APIGateway.ImportDocumentationParts,

    -- ** GetUsage (Paginated)
    module Network.AWS.APIGateway.GetUsage,

    -- ** GetVPCLink
    module Network.AWS.APIGateway.GetVPCLink,

    -- ** CreateModel
    module Network.AWS.APIGateway.CreateModel,

    -- ** GetIntegrationResponse
    module Network.AWS.APIGateway.GetIntegrationResponse,

    -- ** CreateDomainName
    module Network.AWS.APIGateway.CreateDomainName,

    -- ** FlushStageAuthorizersCache
    module Network.AWS.APIGateway.FlushStageAuthorizersCache,

    -- ** GetGatewayResponses (Paginated)
    module Network.AWS.APIGateway.GetGatewayResponses,

    -- ** DeleteModel
    module Network.AWS.APIGateway.DeleteModel,

    -- ** UpdateModel
    module Network.AWS.APIGateway.UpdateModel,

    -- ** GetDocumentationVersion
    module Network.AWS.APIGateway.GetDocumentationVersion,

    -- ** DeleteAPIKey
    module Network.AWS.APIGateway.DeleteAPIKey,

    -- ** UpdateAPIKey
    module Network.AWS.APIGateway.UpdateAPIKey,

    -- ** GetRestAPI
    module Network.AWS.APIGateway.GetRestAPI,

    -- ** GetStages
    module Network.AWS.APIGateway.GetStages,

    -- ** PutRestAPI
    module Network.AWS.APIGateway.PutRestAPI,

    -- ** GetMethod
    module Network.AWS.APIGateway.GetMethod,

    -- ** GetModel
    module Network.AWS.APIGateway.GetModel,

    -- ** UpdateRestAPI
    module Network.AWS.APIGateway.UpdateRestAPI,

    -- ** DeleteRestAPI
    module Network.AWS.APIGateway.DeleteRestAPI,

    -- ** ImportAPIKeys
    module Network.AWS.APIGateway.ImportAPIKeys,

    -- ** CreateDocumentationPart
    module Network.AWS.APIGateway.CreateDocumentationPart,

    -- ** TestInvokeMethod
    module Network.AWS.APIGateway.TestInvokeMethod,

    -- ** GetRequestValidator
    module Network.AWS.APIGateway.GetRequestValidator,

    -- ** GetDomainName
    module Network.AWS.APIGateway.GetDomainName,

    -- ** CreateVPCLink
    module Network.AWS.APIGateway.CreateVPCLink,

    -- ** DeleteDocumentationPart
    module Network.AWS.APIGateway.DeleteDocumentationPart,

    -- ** UpdateDocumentationPart
    module Network.AWS.APIGateway.UpdateDocumentationPart,

    -- ** GetAuthorizers (Paginated)
    module Network.AWS.APIGateway.GetAuthorizers,

    -- ** CreateDocumentationVersion
    module Network.AWS.APIGateway.CreateDocumentationVersion,

    -- ** PutIntegrationResponse
    module Network.AWS.APIGateway.PutIntegrationResponse,

    -- ** GetUsagePlanKeys (Paginated)
    module Network.AWS.APIGateway.GetUsagePlanKeys,

    -- ** DeleteVPCLink
    module Network.AWS.APIGateway.DeleteVPCLink,

    -- ** UpdateVPCLink
    module Network.AWS.APIGateway.UpdateVPCLink,

    -- ** FlushStageCache
    module Network.AWS.APIGateway.FlushStageCache,

    -- ** CreateRestAPI
    module Network.AWS.APIGateway.CreateRestAPI,

    -- ** DeleteIntegrationResponse
    module Network.AWS.APIGateway.DeleteIntegrationResponse,

    -- ** UpdateIntegrationResponse
    module Network.AWS.APIGateway.UpdateIntegrationResponse,

    -- ** UpdateUsage
    module Network.AWS.APIGateway.UpdateUsage,

    -- ** DeleteIntegration
    module Network.AWS.APIGateway.DeleteIntegration,

    -- ** UpdateIntegration
    module Network.AWS.APIGateway.UpdateIntegration,

    -- ** TestInvokeAuthorizer
    module Network.AWS.APIGateway.TestInvokeAuthorizer,

    -- ** GenerateClientCertificate
    module Network.AWS.APIGateway.GenerateClientCertificate,

    -- ** GetResources (Paginated)
    module Network.AWS.APIGateway.GetResources,

    -- ** GetUsagePlanKey
    module Network.AWS.APIGateway.GetUsagePlanKey,

    -- ** GetAccount
    module Network.AWS.APIGateway.GetAccount,

    -- ** PutIntegration
    module Network.AWS.APIGateway.PutIntegration,

    -- ** GetAuthorizer
    module Network.AWS.APIGateway.GetAuthorizer,

    -- ** DeleteUsagePlan
    module Network.AWS.APIGateway.DeleteUsagePlan,

    -- ** UpdateUsagePlan
    module Network.AWS.APIGateway.UpdateUsagePlan,

    -- ** GetStage
    module Network.AWS.APIGateway.GetStage,

    -- ** GetExport
    module Network.AWS.APIGateway.GetExport,

    -- ** GetSDK
    module Network.AWS.APIGateway.GetSDK,

    -- ** GetAPIKeys (Paginated)
    module Network.AWS.APIGateway.GetAPIKeys,

    -- ** DeleteBasePathMapping
    module Network.AWS.APIGateway.DeleteBasePathMapping,

    -- ** UpdateBasePathMapping
    module Network.AWS.APIGateway.UpdateBasePathMapping,

    -- ** DeleteClientCertificate
    module Network.AWS.APIGateway.DeleteClientCertificate,

    -- ** UpdateClientCertificate
    module Network.AWS.APIGateway.UpdateClientCertificate,

    -- ** GetGatewayResponse
    module Network.AWS.APIGateway.GetGatewayResponse,

    -- ** CreateUsagePlanKey
    module Network.AWS.APIGateway.CreateUsagePlanKey,

    -- ** CreateAuthorizer
    module Network.AWS.APIGateway.CreateAuthorizer,

    -- ** UpdateAuthorizer
    module Network.AWS.APIGateway.UpdateAuthorizer,

    -- ** DeleteAuthorizer
    module Network.AWS.APIGateway.DeleteAuthorizer,

    -- ** TagResource
    module Network.AWS.APIGateway.TagResource,

    -- ** CreateStage
    module Network.AWS.APIGateway.CreateStage,

    -- ** DeleteUsagePlanKey
    module Network.AWS.APIGateway.DeleteUsagePlanKey,

    -- ** UntagResource
    module Network.AWS.APIGateway.UntagResource,

    -- ** CreateAPIKey
    module Network.AWS.APIGateway.CreateAPIKey,

    -- ** GetUsagePlans (Paginated)
    module Network.AWS.APIGateway.GetUsagePlans,

    -- ** PutMethod
    module Network.AWS.APIGateway.PutMethod,

    -- ** UpdateDomainName
    module Network.AWS.APIGateway.UpdateDomainName,

    -- ** DeleteDomainName
    module Network.AWS.APIGateway.DeleteDomainName,

    -- ** CreateResource
    module Network.AWS.APIGateway.CreateResource,

    -- ** DeleteMethod
    module Network.AWS.APIGateway.DeleteMethod,

    -- ** UpdateMethod
    module Network.AWS.APIGateway.UpdateMethod,

    -- ** UpdateRequestValidator
    module Network.AWS.APIGateway.UpdateRequestValidator,

    -- ** DeleteRequestValidator
    module Network.AWS.APIGateway.DeleteRequestValidator,

    -- ** GetSDKTypes (Paginated)
    module Network.AWS.APIGateway.GetSDKTypes,

    -- ** GetClientCertificates (Paginated)
    module Network.AWS.APIGateway.GetClientCertificates,

    -- ** GetModelTemplate
    module Network.AWS.APIGateway.GetModelTemplate,

    -- ** UpdateDocumentationVersion
    module Network.AWS.APIGateway.UpdateDocumentationVersion,

    -- ** DeleteDocumentationVersion
    module Network.AWS.APIGateway.DeleteDocumentationVersion,

    -- ** GetBasePathMappings (Paginated)
    module Network.AWS.APIGateway.GetBasePathMappings,

    -- ** GetAPIKey
    module Network.AWS.APIGateway.GetAPIKey,

    -- * Types

    -- ** APIKeySourceType
    APIKeySourceType (..),

    -- ** APIKeysFormat
    APIKeysFormat (..),

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

    -- ** VPCLinkStatus
    VPCLinkStatus (..),

    -- ** APIKey
    APIKey (..),
    mkAPIKey,
    akEnabled,
    akValue,
    akCustomerId,
    akCreatedDate,
    akName,
    akId,
    akStageKeys,
    akLastUpdatedDate,
    akDescription,
    akTags,

    -- ** APIStage
    APIStage (..),
    mkAPIStage,
    asStage,
    asApiId,
    asThrottle,

    -- ** AccessLogSettings
    AccessLogSettings (..),
    mkAccessLogSettings,
    alsFormat,
    alsDestinationARN,

    -- ** Account
    Account (..),
    mkAccount,
    aApiKeyVersion,
    aCloudwatchRoleARN,
    aFeatures,
    aThrottleSettings,

    -- ** Authorizer
    Authorizer (..),
    mkAuthorizer,
    aAuthorizerURI,
    aIdentityValidationExpression,
    aProviderARNs,
    aName,
    aId,
    aAuthorizerResultTtlInSeconds,
    aAuthType,
    aType,
    aIdentitySource,
    aAuthorizerCredentials,

    -- ** BasePathMapping
    BasePathMapping (..),
    mkBasePathMapping,
    bpmStage,
    bpmBasePath,
    bpmRestAPIId,

    -- ** CanarySettings
    CanarySettings (..),
    mkCanarySettings,
    csDeploymentId,
    csStageVariableOverrides,
    csUseStageCache,
    csPercentTraffic,

    -- ** ClientCertificate
    ClientCertificate (..),
    mkClientCertificate,
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,

    -- ** Deployment
    Deployment (..),
    mkDeployment,
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,

    -- ** DeploymentCanarySettings
    DeploymentCanarySettings (..),
    mkDeploymentCanarySettings,
    dcsStageVariableOverrides,
    dcsUseStageCache,
    dcsPercentTraffic,

    -- ** DocumentationPart
    DocumentationPart (..),
    mkDocumentationPart,
    dpLocation,
    dpId,
    dpProperties,

    -- ** DocumentationPartLocation
    DocumentationPartLocation (..),
    mkDocumentationPartLocation,
    dplPath,
    dplName,
    dplMethod,
    dplStatusCode,
    dplType,

    -- ** DocumentationVersion
    DocumentationVersion (..),
    mkDocumentationVersion,
    dvCreatedDate,
    dvVersion,
    dvDescription,

    -- ** DomainName
    DomainName (..),
    mkDomainName,
    dnRegionalHostedZoneId,
    dnCertificateName,
    dnRegionalCertificateARN,
    dnCertificateARN,
    dnDistributionHostedZoneId,
    dnSecurityPolicy,
    dnDomainName,
    dnMutualTLSAuthentication,
    dnRegionalCertificateName,
    dnRegionalDomainName,
    dnCertificateUploadDate,
    dnDistributionDomainName,
    dnDomainNameStatusMessage,
    dnEndpointConfiguration,
    dnDomainNameStatus,
    dnTags,

    -- ** EndpointConfiguration
    EndpointConfiguration (..),
    mkEndpointConfiguration,
    ecTypes,
    ecVpcEndpointIds,

    -- ** GatewayResponse
    GatewayResponse (..),
    mkGatewayResponse,
    gDefaultResponse,
    gResponseTemplates,
    gResponseType,
    gStatusCode,
    gResponseParameters,

    -- ** Integration
    Integration (..),
    mkIntegration,
    iHttpMethod,
    iRequestTemplates,
    iCredentials,
    iConnectionId,
    iRequestParameters,
    iContentHandling,
    iPassthroughBehavior,
    iUri,
    iIntegrationResponses,
    iTlsConfig,
    iCacheNamespace,
    iTimeoutInMillis,
    iType,
    iConnectionType,
    iCacheKeyParameters,

    -- ** IntegrationResponse
    IntegrationResponse (..),
    mkIntegrationResponse,
    intContentHandling,
    intResponseTemplates,
    intSelectionPattern,
    intStatusCode,
    intResponseParameters,

    -- ** Method
    Method (..),
    mkMethod,
    mMethodResponses,
    mHttpMethod,
    mAuthorizationScopes,
    mRequestValidatorId,
    mRequestModels,
    mRequestParameters,
    mAuthorizerId,
    mOperationName,
    mAuthorizationType,
    mApiKeyRequired,
    mMethodIntegration,

    -- ** MethodResponse
    MethodResponse (..),
    mkMethodResponse,
    mResponseModels,
    mStatusCode,
    mResponseParameters,

    -- ** MethodSetting
    MethodSetting (..),
    mkMethodSetting,
    msCacheTtlInSeconds,
    msDataTraceEnabled,
    msThrottlingBurstLimit,
    msCacheDataEncrypted,
    msLoggingLevel,
    msRequireAuthorizationForCacheControl,
    msCachingEnabled,
    msMetricsEnabled,
    msThrottlingRateLimit,
    msUnauthorizedCacheControlHeaderStrategy,

    -- ** MethodSnapshot
    MethodSnapshot (..),
    mkMethodSnapshot,
    msAuthorizationType,
    msApiKeyRequired,

    -- ** Model
    Model (..),
    mkModel,
    mSchema,
    mName,
    mId,
    mDescription,
    mContentType,

    -- ** MutualTLSAuthentication
    MutualTLSAuthentication (..),
    mkMutualTLSAuthentication,
    mtaTruststoreWarnings,
    mtaTruststoreURI,
    mtaTruststoreVersion,

    -- ** MutualTLSAuthenticationInput
    MutualTLSAuthenticationInput (..),
    mkMutualTLSAuthenticationInput,
    mtaiTruststoreURI,
    mtaiTruststoreVersion,

    -- ** PatchOperation
    PatchOperation (..),
    mkPatchOperation,
    poOp,
    poPath,
    poValue,
    poFrom,

    -- ** QuotaSettings
    QuotaSettings (..),
    mkQuotaSettings,
    qsOffset,
    qsPeriod,
    qsLimit,

    -- ** RequestValidator
    RequestValidator (..),
    mkRequestValidator,
    rvValidateRequestParameters,
    rvName,
    rvValidateRequestBody,
    rvId,

    -- ** Resource
    Resource (..),
    mkResource,
    rPathPart,
    rPath,
    rId,
    rResourceMethods,
    rParentId,

    -- ** RestAPI
    RestAPI (..),
    mkRestAPI,
    raMinimumCompressionSize,
    raDisableExecuteAPIEndpoint,
    raBinaryMediaTypes,
    raWarnings,
    raCreatedDate,
    raName,
    raVersion,
    raApiKeySource,
    raId,
    raPolicy,
    raEndpointConfiguration,
    raDescription,
    raTags,

    -- ** SDKConfigurationProperty
    SDKConfigurationProperty (..),
    mkSDKConfigurationProperty,
    scpFriendlyName,
    scpRequired,
    scpName,
    scpDefaultValue,
    scpDescription,

    -- ** SDKType
    SDKType (..),
    mkSDKType,
    stFriendlyName,
    stConfigurationProperties,
    stId,
    stDescription,

    -- ** Stage
    Stage (..),
    mkStage,
    sDeploymentId,
    sVariables,
    sAccessLogSettings,
    sDocumentationVersion,
    sClientCertificateId,
    sTracingEnabled,
    sCreatedDate,
    sCacheClusterStatus,
    sMethodSettings,
    sLastUpdatedDate,
    sCacheClusterSize,
    sWebACLARN,
    sCanarySettings,
    sCacheClusterEnabled,
    sStageName,
    sDescription,
    sTags,

    -- ** StageKey
    StageKey (..),
    mkStageKey,
    skRestAPIId,
    skStageName,

    -- ** TLSConfig
    TLSConfig (..),
    mkTLSConfig,
    tcInsecureSkipVerification,

    -- ** ThrottleSettings
    ThrottleSettings (..),
    mkThrottleSettings,
    tsBurstLimit,
    tsRateLimit,

    -- ** Usage
    Usage (..),
    mkUsage,
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,

    -- ** UsagePlan
    UsagePlan (..),
    mkUsagePlan,
    upApiStages,
    upName,
    upId,
    upThrottle,
    upQuota,
    upDescription,
    upProductCode,
    upTags,

    -- ** UsagePlanKey
    UsagePlanKey (..),
    mkUsagePlanKey,
    upkValue,
    upkName,
    upkId,
    upkType,

    -- ** VPCLink
    VPCLink (..),
    mkVPCLink,
    vlStatus,
    vlTargetARNs,
    vlName,
    vlStatusMessage,
    vlId,
    vlDescription,
    vlTags,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.APIGateway.CreateAPIKey
import Network.AWS.APIGateway.CreateAuthorizer
import Network.AWS.APIGateway.CreateBasePathMapping
import Network.AWS.APIGateway.CreateDeployment
import Network.AWS.APIGateway.CreateDocumentationPart
import Network.AWS.APIGateway.CreateDocumentationVersion
import Network.AWS.APIGateway.CreateDomainName
import Network.AWS.APIGateway.CreateModel
import Network.AWS.APIGateway.CreateRequestValidator
import Network.AWS.APIGateway.CreateResource
import Network.AWS.APIGateway.CreateRestAPI
import Network.AWS.APIGateway.CreateStage
import Network.AWS.APIGateway.CreateUsagePlan
import Network.AWS.APIGateway.CreateUsagePlanKey
import Network.AWS.APIGateway.CreateVPCLink
import Network.AWS.APIGateway.DeleteAPIKey
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
import Network.AWS.APIGateway.DeleteRestAPI
import Network.AWS.APIGateway.DeleteStage
import Network.AWS.APIGateway.DeleteUsagePlan
import Network.AWS.APIGateway.DeleteUsagePlanKey
import Network.AWS.APIGateway.DeleteVPCLink
import Network.AWS.APIGateway.FlushStageAuthorizersCache
import Network.AWS.APIGateway.FlushStageCache
import Network.AWS.APIGateway.GenerateClientCertificate
import Network.AWS.APIGateway.GetAPIKey
import Network.AWS.APIGateway.GetAPIKeys
import Network.AWS.APIGateway.GetAccount
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
import Network.AWS.APIGateway.GetRestAPI
import Network.AWS.APIGateway.GetRestAPIs
import Network.AWS.APIGateway.GetSDK
import Network.AWS.APIGateway.GetSDKType
import Network.AWS.APIGateway.GetSDKTypes
import Network.AWS.APIGateway.GetStage
import Network.AWS.APIGateway.GetStages
import Network.AWS.APIGateway.GetTags
import Network.AWS.APIGateway.GetUsage
import Network.AWS.APIGateway.GetUsagePlan
import Network.AWS.APIGateway.GetUsagePlanKey
import Network.AWS.APIGateway.GetUsagePlanKeys
import Network.AWS.APIGateway.GetUsagePlans
import Network.AWS.APIGateway.GetVPCLink
import Network.AWS.APIGateway.GetVPCLinks
import Network.AWS.APIGateway.ImportAPIKeys
import Network.AWS.APIGateway.ImportDocumentationParts
import Network.AWS.APIGateway.ImportRestAPI
import Network.AWS.APIGateway.PutGatewayResponse
import Network.AWS.APIGateway.PutIntegration
import Network.AWS.APIGateway.PutIntegrationResponse
import Network.AWS.APIGateway.PutMethod
import Network.AWS.APIGateway.PutMethodResponse
import Network.AWS.APIGateway.PutRestAPI
import Network.AWS.APIGateway.TagResource
import Network.AWS.APIGateway.TestInvokeAuthorizer
import Network.AWS.APIGateway.TestInvokeMethod
import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.UntagResource
import Network.AWS.APIGateway.UpdateAPIKey
import Network.AWS.APIGateway.UpdateAccount
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
import Network.AWS.APIGateway.UpdateRestAPI
import Network.AWS.APIGateway.UpdateStage
import Network.AWS.APIGateway.UpdateUsage
import Network.AWS.APIGateway.UpdateUsagePlan
import Network.AWS.APIGateway.UpdateVPCLink
import Network.AWS.APIGateway.Waiters
import qualified Network.AWS.Prelude as Lude

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
