{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _UnauthorizedException,
    _ServiceUnavailableException,
    _LimitExceededException,
    _ConflictException,
    _TooManyRequestsException,

    -- * ApiKeySourceType
    ApiKeySourceType (..),

    -- * ApiKeysFormat
    ApiKeysFormat (..),

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

    -- * VpcLinkStatus
    VpcLinkStatus (..),

    -- * AccessLogSettings
    AccessLogSettings (..),
    newAccessLogSettings,
    accessLogSettings_destinationArn,
    accessLogSettings_format,

    -- * Account
    Account (..),
    newAccount,
    account_throttleSettings,
    account_apiKeyVersion,
    account_features,
    account_cloudwatchRoleArn,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
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

    -- * ApiStage
    ApiStage (..),
    newApiStage,
    apiStage_apiId,
    apiStage_stage,
    apiStage_throttle,

    -- * Authorizer
    Authorizer (..),
    newAuthorizer,
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

    -- * BasePathMapping
    BasePathMapping (..),
    newBasePathMapping,
    basePathMapping_basePath,
    basePathMapping_stage,
    basePathMapping_restApiId,

    -- * CanarySettings
    CanarySettings (..),
    newCanarySettings,
    canarySettings_deploymentId,
    canarySettings_percentTraffic,
    canarySettings_useStageCache,
    canarySettings_stageVariableOverrides,

    -- * ClientCertificate
    ClientCertificate (..),
    newClientCertificate,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings (..),
    newDeploymentCanarySettings,
    deploymentCanarySettings_percentTraffic,
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_stageVariableOverrides,

    -- * DocumentationPart
    DocumentationPart (..),
    newDocumentationPart,
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,

    -- * DocumentationPartLocation
    DocumentationPartLocation (..),
    newDocumentationPartLocation,
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_statusCode,
    documentationPartLocation_path,
    documentationPartLocation_type,

    -- * DocumentationVersion
    DocumentationVersion (..),
    newDocumentationVersion,
    documentationVersion_createdDate,
    documentationVersion_version,
    documentationVersion_description,

    -- * DomainName
    DomainName (..),
    newDomainName,
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

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    newEndpointConfiguration,
    endpointConfiguration_types,
    endpointConfiguration_vpcEndpointIds,

    -- * GatewayResponse
    GatewayResponse (..),
    newGatewayResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,

    -- * Integration
    Integration (..),
    newIntegration,
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

    -- * IntegrationResponse
    IntegrationResponse (..),
    newIntegrationResponse,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,

    -- * Method
    Method (..),
    newMethod,
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

    -- * MethodResponse
    MethodResponse (..),
    newMethodResponse,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- * MethodSetting
    MethodSetting (..),
    newMethodSetting,
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

    -- * MethodSnapshot
    MethodSnapshot (..),
    newMethodSnapshot,
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- * Model
    Model (..),
    newModel,
    model_contentType,
    model_schema,
    model_id,
    model_name,
    model_description,

    -- * MutualTlsAuthentication
    MutualTlsAuthentication (..),
    newMutualTlsAuthentication,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreWarnings,

    -- * MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (..),
    newMutualTlsAuthenticationInput,
    mutualTlsAuthenticationInput_truststoreVersion,
    mutualTlsAuthenticationInput_truststoreUri,

    -- * PatchOperation
    PatchOperation (..),
    newPatchOperation,
    patchOperation_op,
    patchOperation_from,
    patchOperation_value,
    patchOperation_path,

    -- * QuotaSettings
    QuotaSettings (..),
    newQuotaSettings,
    quotaSettings_period,
    quotaSettings_limit,
    quotaSettings_offset,

    -- * RequestValidator
    RequestValidator (..),
    newRequestValidator,
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,

    -- * Resource
    Resource (..),
    newResource,
    resource_id,
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,

    -- * RestApi
    RestApi (..),
    newRestApi,
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

    -- * SdkConfigurationProperty
    SdkConfigurationProperty (..),
    newSdkConfigurationProperty,
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_defaultValue,

    -- * SdkType
    SdkType (..),
    newSdkType,
    sdkType_friendlyName,
    sdkType_id,
    sdkType_configurationProperties,
    sdkType_description,

    -- * Stage
    Stage (..),
    newStage,
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

    -- * StageKey
    StageKey (..),
    newStageKey,
    stageKey_stageName,
    stageKey_restApiId,

    -- * ThrottleSettings
    ThrottleSettings (..),
    newThrottleSettings,
    throttleSettings_burstLimit,
    throttleSettings_rateLimit,

    -- * TlsConfig
    TlsConfig (..),
    newTlsConfig,
    tlsConfig_insecureSkipVerification,

    -- * Usage
    Usage (..),
    newUsage,
    usage_startDate,
    usage_items,
    usage_position,
    usage_usagePlanId,
    usage_endDate,

    -- * UsagePlan
    UsagePlan (..),
    newUsagePlan,
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,

    -- * UsagePlanKey
    UsagePlanKey (..),
    newUsagePlanKey,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_value,
    usagePlanKey_type,

    -- * VpcLink
    VpcLink (..),
    newVpcLink,
    vpcLink_statusMessage,
    vpcLink_status,
    vpcLink_id,
    vpcLink_name,
    vpcLink_targetArns,
    vpcLink_tags,
    vpcLink_description,
  )
where

import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.Account
import Network.AWS.APIGateway.Types.ApiKey
import Network.AWS.APIGateway.Types.ApiKeySourceType
import Network.AWS.APIGateway.Types.ApiKeysFormat
import Network.AWS.APIGateway.Types.ApiStage
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
import Network.AWS.APIGateway.Types.MutualTlsAuthentication
import Network.AWS.APIGateway.Types.MutualTlsAuthenticationInput
import Network.AWS.APIGateway.Types.Op
import Network.AWS.APIGateway.Types.PatchOperation
import Network.AWS.APIGateway.Types.PutMode
import Network.AWS.APIGateway.Types.QuotaPeriodType
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.RequestValidator
import Network.AWS.APIGateway.Types.Resource
import Network.AWS.APIGateway.Types.RestApi
import Network.AWS.APIGateway.Types.SdkConfigurationProperty
import Network.AWS.APIGateway.Types.SdkType
import Network.AWS.APIGateway.Types.SecurityPolicy
import Network.AWS.APIGateway.Types.Stage
import Network.AWS.APIGateway.Types.StageKey
import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.APIGateway.Types.TlsConfig
import Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import Network.AWS.APIGateway.Types.Usage
import Network.AWS.APIGateway.Types.UsagePlan
import Network.AWS.APIGateway.Types.UsagePlanKey
import Network.AWS.APIGateway.Types.VpcLink
import Network.AWS.APIGateway.Types.VpcLinkStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "APIGateway",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "apigateway",
      Core._serviceSigningName = "apigateway",
      Core._serviceVersion = "2015-07-09",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "APIGateway",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The requested resource is not found. Make sure that the request URI is
-- correct.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | The submitted request is not valid, for example, the input is incomplete
-- or incorrect. See the accompanying error message for details.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | The request is denied because the caller has insufficient permissions.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Core.. Core.hasStatus 401

-- | The requested service is not available. For details see the accompanying
-- error message. Retry after the specified time period.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Core.. Core.hasStatus 503

-- | The request exceeded the rate limit. Retry after the specified time
-- period.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Core.. Core.hasStatus 429

-- | The request configuration has conflicts. For details, see the
-- accompanying error message.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Core.. Core.hasStatus 409

-- | The request has reached its throttling limit. Retry after the specified
-- time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429
