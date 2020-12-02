{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types
  ( -- * Service Configuration
    apiGateway,

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
    APIKey,
    apiKey,
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
    APIStage,
    apiStage,
    asStage,
    asApiId,
    asThrottle,

    -- * AccessLogSettings
    AccessLogSettings,
    accessLogSettings,
    alsFormat,
    alsDestinationARN,

    -- * Account
    Account,
    account,
    aApiKeyVersion,
    aCloudwatchRoleARN,
    aFeatures,
    aThrottleSettings,

    -- * Authorizer
    Authorizer,
    authorizer,
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
    BasePathMapping,
    basePathMapping,
    bpmStage,
    bpmBasePath,
    bpmRestAPIId,

    -- * CanarySettings
    CanarySettings,
    canarySettings,
    csDeploymentId,
    csStageVariableOverrides,
    csUseStageCache,
    csPercentTraffic,

    -- * ClientCertificate
    ClientCertificate,
    clientCertificate,
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,

    -- * Deployment
    Deployment,
    deployment,
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings,
    deploymentCanarySettings,
    dcsStageVariableOverrides,
    dcsUseStageCache,
    dcsPercentTraffic,

    -- * DocumentationPart
    DocumentationPart,
    documentationPart,
    dpLocation,
    dpId,
    dpProperties,

    -- * DocumentationPartLocation
    DocumentationPartLocation,
    documentationPartLocation,
    dplPath,
    dplName,
    dplMethod,
    dplStatusCode,
    dplType,

    -- * DocumentationVersion
    DocumentationVersion,
    documentationVersion,
    dvCreatedDate,
    dvVersion,
    dvDescription,

    -- * DomainName
    DomainName,
    domainName,
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
    EndpointConfiguration,
    endpointConfiguration,
    ecTypes,
    ecVpcEndpointIds,

    -- * GatewayResponse
    GatewayResponse,
    gatewayResponse,
    gDefaultResponse,
    gResponseTemplates,
    gResponseType,
    gStatusCode,
    gResponseParameters,

    -- * Integration
    Integration,
    integration,
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
    IntegrationResponse,
    integrationResponse,
    intContentHandling,
    intResponseTemplates,
    intSelectionPattern,
    intStatusCode,
    intResponseParameters,

    -- * Method
    Method,
    method,
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
    MethodResponse,
    methodResponse,
    mResponseModels,
    mStatusCode,
    mResponseParameters,

    -- * MethodSetting
    MethodSetting,
    methodSetting,
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
    MethodSnapshot,
    methodSnapshot,
    msAuthorizationType,
    msApiKeyRequired,

    -- * Model
    Model,
    model,
    mSchema,
    mName,
    mId,
    mDescription,
    mContentType,

    -- * MutualTLSAuthentication
    MutualTLSAuthentication,
    mutualTLSAuthentication,
    mtaTruststoreWarnings,
    mtaTruststoreURI,
    mtaTruststoreVersion,

    -- * MutualTLSAuthenticationInput
    MutualTLSAuthenticationInput,
    mutualTLSAuthenticationInput,
    mtaiTruststoreURI,
    mtaiTruststoreVersion,

    -- * PatchOperation
    PatchOperation,
    patchOperation,
    poOp,
    poPath,
    poValue,
    poFrom,

    -- * QuotaSettings
    QuotaSettings,
    quotaSettings,
    qsOffset,
    qsPeriod,
    qsLimit,

    -- * RequestValidator
    RequestValidator,
    requestValidator,
    rvValidateRequestParameters,
    rvName,
    rvValidateRequestBody,
    rvId,

    -- * Resource
    Resource,
    resource,
    rPathPart,
    rPath,
    rId,
    rResourceMethods,
    rParentId,

    -- * RestAPI
    RestAPI,
    restAPI,
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
    SDKConfigurationProperty,
    sdkConfigurationProperty,
    scpFriendlyName,
    scpRequired,
    scpName,
    scpDefaultValue,
    scpDescription,

    -- * SDKType
    SDKType,
    sdkType,
    stFriendlyName,
    stConfigurationProperties,
    stId,
    stDescription,

    -- * Stage
    Stage,
    stage,
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
    StageKey,
    stageKey,
    skRestAPIId,
    skStageName,

    -- * TLSConfig
    TLSConfig,
    tlsConfig,
    tcInsecureSkipVerification,

    -- * ThrottleSettings
    ThrottleSettings,
    throttleSettings,
    tsBurstLimit,
    tsRateLimit,

    -- * Usage
    Usage,
    usage,
    uUsagePlanId,
    uEndDate,
    uItems,
    uStartDate,
    uPosition,

    -- * UsagePlan
    UsagePlan,
    usagePlan,
    upApiStages,
    upName,
    upId,
    upThrottle,
    upQuota,
    upDescription,
    upProductCode,
    upTags,

    -- * UsagePlanKey
    UsagePlanKey,
    usagePlanKey,
    upkValue,
    upkName,
    upkId,
    upkType,

    -- * VPCLink
    VPCLink,
    vpcLink,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
apiGateway :: Service
apiGateway =
  Service
    { _svcAbbrev = "APIGateway",
      _svcSigner = v4,
      _svcPrefix = "apigateway",
      _svcVersion = "2015-07-09",
      _svcEndpoint = defaultEndpoint apiGateway,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "APIGateway",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
