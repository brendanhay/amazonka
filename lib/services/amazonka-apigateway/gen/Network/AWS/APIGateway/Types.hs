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
    _ConflictException,
    _NotFoundException,
    _TooManyRequestsException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _BadRequestException,
    _LimitExceededException,

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
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- * Account
    Account (..),
    newAccount,
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,

    -- * ApiStage
    ApiStage (..),
    newApiStage,
    apiStage_stage,
    apiStage_apiId,
    apiStage_throttle,

    -- * Authorizer
    Authorizer (..),
    newAuthorizer,
    authorizer_authorizerUri,
    authorizer_identityValidationExpression,
    authorizer_providerARNs,
    authorizer_name,
    authorizer_id,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authType,
    authorizer_type,
    authorizer_identitySource,
    authorizer_authorizerCredentials,

    -- * BasePathMapping
    BasePathMapping (..),
    newBasePathMapping,
    basePathMapping_stage,
    basePathMapping_basePath,
    basePathMapping_restApiId,

    -- * CanarySettings
    CanarySettings (..),
    newCanarySettings,
    canarySettings_deploymentId,
    canarySettings_stageVariableOverrides,
    canarySettings_useStageCache,
    canarySettings_percentTraffic,

    -- * ClientCertificate
    ClientCertificate (..),
    newClientCertificate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_description,
    clientCertificate_tags,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_id,
    deployment_description,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings (..),
    newDeploymentCanarySettings,
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_percentTraffic,

    -- * DocumentationPart
    DocumentationPart (..),
    newDocumentationPart,
    documentationPart_location,
    documentationPart_id,
    documentationPart_properties,

    -- * DocumentationPartLocation
    DocumentationPartLocation (..),
    newDocumentationPartLocation,
    documentationPartLocation_path,
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_statusCode,
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
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    newEndpointConfiguration,
    endpointConfiguration_types,
    endpointConfiguration_vpcEndpointIds,

    -- * GatewayResponse
    GatewayResponse (..),
    newGatewayResponse,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,

    -- * Integration
    Integration (..),
    newIntegration,
    integration_httpMethod,
    integration_requestTemplates,
    integration_credentials,
    integration_connectionId,
    integration_requestParameters,
    integration_contentHandling,
    integration_passthroughBehavior,
    integration_uri,
    integration_integrationResponses,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_timeoutInMillis,
    integration_type,
    integration_connectionType,
    integration_cacheKeyParameters,

    -- * IntegrationResponse
    IntegrationResponse (..),
    newIntegrationResponse,
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,

    -- * Method
    Method (..),
    newMethod,
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,

    -- * MethodResponse
    MethodResponse (..),
    newMethodResponse,
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,

    -- * MethodSetting
    MethodSetting (..),
    newMethodSetting,
    methodSetting_cacheTtlInSeconds,
    methodSetting_dataTraceEnabled,
    methodSetting_throttlingBurstLimit,
    methodSetting_cacheDataEncrypted,
    methodSetting_loggingLevel,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_cachingEnabled,
    methodSetting_metricsEnabled,
    methodSetting_throttlingRateLimit,
    methodSetting_unauthorizedCacheControlHeaderStrategy,

    -- * MethodSnapshot
    MethodSnapshot (..),
    newMethodSnapshot,
    methodSnapshot_authorizationType,
    methodSnapshot_apiKeyRequired,

    -- * Model
    Model (..),
    newModel,
    model_schema,
    model_name,
    model_id,
    model_description,
    model_contentType,

    -- * MutualTlsAuthentication
    MutualTlsAuthentication (..),
    newMutualTlsAuthentication,
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,

    -- * MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (..),
    newMutualTlsAuthenticationInput,
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

    -- * PatchOperation
    PatchOperation (..),
    newPatchOperation,
    patchOperation_op,
    patchOperation_path,
    patchOperation_value,
    patchOperation_from,

    -- * QuotaSettings
    QuotaSettings (..),
    newQuotaSettings,
    quotaSettings_offset,
    quotaSettings_period,
    quotaSettings_limit,

    -- * RequestValidator
    RequestValidator (..),
    newRequestValidator,
    requestValidator_validateRequestParameters,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_id,

    -- * Resource
    Resource (..),
    newResource,
    resource_pathPart,
    resource_path,
    resource_id,
    resource_resourceMethods,
    resource_parentId,

    -- * RestApi
    RestApi (..),
    newRestApi,
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,

    -- * SdkConfigurationProperty
    SdkConfigurationProperty (..),
    newSdkConfigurationProperty,
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,

    -- * SdkType
    SdkType (..),
    newSdkType,
    sdkType_friendlyName,
    sdkType_configurationProperties,
    sdkType_id,
    sdkType_description,

    -- * Stage
    Stage (..),
    newStage,
    stage_deploymentId,
    stage_variables,
    stage_accessLogSettings,
    stage_documentationVersion,
    stage_clientCertificateId,
    stage_tracingEnabled,
    stage_createdDate,
    stage_cacheClusterStatus,
    stage_methodSettings,
    stage_lastUpdatedDate,
    stage_cacheClusterSize,
    stage_webAclArn,
    stage_canarySettings,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_description,
    stage_tags,

    -- * StageKey
    StageKey (..),
    newStageKey,
    stageKey_restApiId,
    stageKey_stageName,

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
    usage_usagePlanId,
    usage_endDate,
    usage_items,
    usage_startDate,
    usage_position,

    -- * UsagePlan
    UsagePlan (..),
    newUsagePlan,
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,

    -- * UsagePlanKey
    UsagePlanKey (..),
    newUsagePlanKey,
    usagePlanKey_value,
    usagePlanKey_name,
    usagePlanKey_id,
    usagePlanKey_type,

    -- * VpcLink
    VpcLink (..),
    newVpcLink,
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,
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
import qualified Network.AWS.Prelude as Prelude
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
      Core._serviceTimeout = Prelude.Just 70,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request configuration has conflicts. For details, see the
-- accompanying error message.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The requested resource is not found. Make sure that the request URI is
-- correct.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request has reached its throttling limit. Retry after the specified
-- time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The requested service is not available. For details see the accompanying
-- error message. Retry after the specified time period.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request is denied because the caller has insufficient permissions.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The submitted request is not valid, for example, the input is incomplete
-- or incorrect. See the accompanying error message for details.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request exceeded the rate limit. Retry after the specified time
-- period.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429
