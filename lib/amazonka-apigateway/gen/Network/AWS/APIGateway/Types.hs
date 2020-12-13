-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types
  ( -- * Service configuration
    apiGatewayService,

    -- * Errors

    -- * APIKeySourceType
    APIKeySourceType (..),

    -- * APIKeysFormat
    APIKeysFormat (..),

    -- * AuthorizerType
    AuthorizerType (..),

    -- * CacheClusterSize
    CacheClusterSize (..),

    -- * CacheClusterStatus
    CacheClusterStatus (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * ContentHandlingStrategy
    ContentHandlingStrategy (..),

    -- * DocumentationPartType
    DocumentationPartType (..),

    -- * DomainNameStatus
    DomainNameStatus (..),

    -- * EndpointType
    EndpointType (..),

    -- * GatewayResponseType
    GatewayResponseType (..),

    -- * IntegrationType
    IntegrationType (..),

    -- * LocationStatusType
    LocationStatusType (..),

    -- * Op
    Op (..),

    -- * PutMode
    PutMode (..),

    -- * QuotaPeriodType
    QuotaPeriodType (..),

    -- * SecurityPolicy
    SecurityPolicy (..),

    -- * UnauthorizedCacheControlHeaderStrategy
    UnauthorizedCacheControlHeaderStrategy (..),

    -- * VPCLinkStatus
    VPCLinkStatus (..),

    -- * APIKey
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

    -- * APIStage
    APIStage (..),
    mkAPIStage,
    asStage,
    asApiId,
    asThrottle,

    -- * AccessLogSettings
    AccessLogSettings (..),
    mkAccessLogSettings,
    alsFormat,
    alsDestinationARN,

    -- * Account
    Account (..),
    mkAccount,
    aApiKeyVersion,
    aCloudwatchRoleARN,
    aFeatures,
    aThrottleSettings,

    -- * Authorizer
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

    -- * BasePathMapping
    BasePathMapping (..),
    mkBasePathMapping,
    bpmStage,
    bpmBasePath,
    bpmRestAPIId,

    -- * CanarySettings
    CanarySettings (..),
    mkCanarySettings,
    csDeploymentId,
    csStageVariableOverrides,
    csUseStageCache,
    csPercentTraffic,

    -- * ClientCertificate
    ClientCertificate (..),
    mkClientCertificate,
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,

    -- * Deployment
    Deployment (..),
    mkDeployment,
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings (..),
    mkDeploymentCanarySettings,
    dcsStageVariableOverrides,
    dcsUseStageCache,
    dcsPercentTraffic,

    -- * DocumentationPart
    DocumentationPart (..),
    mkDocumentationPart,
    dpLocation,
    dpId,
    dpProperties,

    -- * DocumentationPartLocation
    DocumentationPartLocation (..),
    mkDocumentationPartLocation,
    dplPath,
    dplName,
    dplMethod,
    dplType,
    dplStatusCode,

    -- * DocumentationVersion
    DocumentationVersion (..),
    mkDocumentationVersion,
    dvCreatedDate,
    dvVersion,
    dvDescription,

    -- * DomainName
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

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    mkEndpointConfiguration,
    ecTypes,
    ecVpcEndpointIds,

    -- * GatewayResponse
    GatewayResponse (..),
    mkGatewayResponse,
    gDefaultResponse,
    gResponseTemplates,
    gResponseType,
    gStatusCode,
    gResponseParameters,

    -- * Integration
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

    -- * IntegrationResponse
    IntegrationResponse (..),
    mkIntegrationResponse,
    ifContentHandling,
    ifResponseTemplates,
    ifSelectionPattern,
    ifStatusCode,
    ifResponseParameters,

    -- * Method
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

    -- * MethodResponse
    MethodResponse (..),
    mkMethodResponse,
    mResponseModels,
    mStatusCode,
    mResponseParameters,

    -- * MethodSetting
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

    -- * MethodSnapshot
    MethodSnapshot (..),
    mkMethodSnapshot,
    msAuthorizationType,
    msApiKeyRequired,

    -- * Model
    Model (..),
    mkModel,
    mSchema,
    mName,
    mId,
    mDescription,
    mContentType,

    -- * MutualTLSAuthentication
    MutualTLSAuthentication (..),
    mkMutualTLSAuthentication,
    mtaTruststoreWarnings,
    mtaTruststoreURI,
    mtaTruststoreVersion,

    -- * MutualTLSAuthenticationInput
    MutualTLSAuthenticationInput (..),
    mkMutualTLSAuthenticationInput,
    mtaiTruststoreURI,
    mtaiTruststoreVersion,

    -- * PatchOperation
    PatchOperation (..),
    mkPatchOperation,
    poOp,
    poPath,
    poValue,
    poFrom,

    -- * QuotaSettings
    QuotaSettings (..),
    mkQuotaSettings,
    qsOffset,
    qsPeriod,
    qsLimit,

    -- * RequestValidator
    RequestValidator (..),
    mkRequestValidator,
    rvValidateRequestParameters,
    rvName,
    rvValidateRequestBody,
    rvId,

    -- * Resource
    Resource (..),
    mkResource,
    rPathPart,
    rPath,
    rId,
    rResourceMethods,
    rParentId,

    -- * RestAPI
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

    -- * SDKConfigurationProperty
    SDKConfigurationProperty (..),
    mkSDKConfigurationProperty,
    scpFriendlyName,
    scpRequired,
    scpName,
    scpDefaultValue,
    scpDescription,

    -- * SDKType
    SDKType (..),
    mkSDKType,
    stFriendlyName,
    stConfigurationProperties,
    stId,
    stDescription,

    -- * Stage
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

    -- * StageKey
    StageKey (..),
    mkStageKey,
    skRestAPIId,
    skStageName,

    -- * TLSConfig
    TLSConfig (..),
    mkTLSConfig,
    tcInsecureSkipVerification,

    -- * ThrottleSettings
    ThrottleSettings (..),
    mkThrottleSettings,
    tsBurstLimit,
    tsRateLimit,

    -- * Usage
    Usage (..),
    mkUsage,
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,

    -- * UsagePlan
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

    -- * UsagePlanKey
    UsagePlanKey (..),
    mkUsagePlanKey,
    upkValue,
    upkName,
    upkId,
    upkType,

    -- * VPCLink
    VPCLink (..),
    mkVPCLink,
    vlStatus,
    vlTargetARNs,
    vlName,
    vlStatusMessage,
    vlId,
    vlDescription,
    vlTags,
  )
where

import Network.AWS.APIGateway.Types.APIKey
import Network.AWS.APIGateway.Types.APIKeySourceType
import Network.AWS.APIGateway.Types.APIKeysFormat
import Network.AWS.APIGateway.Types.APIStage
import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.Account
import Network.AWS.APIGateway.Types.Authorizer
import Network.AWS.APIGateway.Types.AuthorizerType
import Network.AWS.APIGateway.Types.BasePathMapping
import Network.AWS.APIGateway.Types.CacheClusterSize
import Network.AWS.APIGateway.Types.CacheClusterStatus
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.ClientCertificate
import Network.AWS.APIGateway.Types.ConnectionType
import Network.AWS.APIGateway.Types.ContentHandlingStrategy
import Network.AWS.APIGateway.Types.Deployment
import Network.AWS.APIGateway.Types.DeploymentCanarySettings
import Network.AWS.APIGateway.Types.DocumentationPart
import Network.AWS.APIGateway.Types.DocumentationPartLocation
import Network.AWS.APIGateway.Types.DocumentationPartType
import Network.AWS.APIGateway.Types.DocumentationVersion
import Network.AWS.APIGateway.Types.DomainName
import Network.AWS.APIGateway.Types.DomainNameStatus
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.APIGateway.Types.EndpointType
import Network.AWS.APIGateway.Types.GatewayResponse
import Network.AWS.APIGateway.Types.GatewayResponseType
import Network.AWS.APIGateway.Types.Integration
import Network.AWS.APIGateway.Types.IntegrationResponse
import Network.AWS.APIGateway.Types.IntegrationType
import Network.AWS.APIGateway.Types.LocationStatusType
import Network.AWS.APIGateway.Types.Method
import Network.AWS.APIGateway.Types.MethodResponse
import Network.AWS.APIGateway.Types.MethodSetting
import Network.AWS.APIGateway.Types.MethodSnapshot
import Network.AWS.APIGateway.Types.Model
import Network.AWS.APIGateway.Types.MutualTLSAuthentication
import Network.AWS.APIGateway.Types.MutualTLSAuthenticationInput
import Network.AWS.APIGateway.Types.Op
import Network.AWS.APIGateway.Types.PatchOperation
import Network.AWS.APIGateway.Types.PutMode
import Network.AWS.APIGateway.Types.QuotaPeriodType
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.RequestValidator
import Network.AWS.APIGateway.Types.Resource
import Network.AWS.APIGateway.Types.RestAPI
import Network.AWS.APIGateway.Types.SDKConfigurationProperty
import Network.AWS.APIGateway.Types.SDKType
import Network.AWS.APIGateway.Types.SecurityPolicy
import Network.AWS.APIGateway.Types.Stage
import Network.AWS.APIGateway.Types.StageKey
import Network.AWS.APIGateway.Types.TLSConfig
import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import Network.AWS.APIGateway.Types.Usage
import Network.AWS.APIGateway.Types.UsagePlan
import Network.AWS.APIGateway.Types.UsagePlanKey
import Network.AWS.APIGateway.Types.VPCLink
import Network.AWS.APIGateway.Types.VPCLinkStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
apiGatewayService :: Lude.Service
apiGatewayService =
  Lude.Service
    { Lude._svcAbbrev = "APIGateway",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "apigateway",
      Lude._svcVersion = "2015-07-09",
      Lude._svcEndpoint = Lude.defaultEndpoint apiGatewayService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "APIGateway",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
