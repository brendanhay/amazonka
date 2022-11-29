{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnauthorizedException,
    _NotFoundException,
    _ServiceUnavailableException,
    _LimitExceededException,
    _ConflictException,
    _BadRequestException,
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
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- * Account
    Account (..),
    newAccount,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,
    account_apiKeyVersion,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,

    -- * ApiStage
    ApiStage (..),
    newApiStage,
    apiStage_apiId,
    apiStage_throttle,
    apiStage_stage,

    -- * Authorizer
    Authorizer (..),
    newAuthorizer,
    authorizer_name,
    authorizer_type,
    authorizer_authorizerCredentials,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_id,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_providerARNs,
    authorizer_authType,

    -- * BasePathMapping
    BasePathMapping (..),
    newBasePathMapping,
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,

    -- * CanarySettings
    CanarySettings (..),
    newCanarySettings,
    canarySettings_deploymentId,
    canarySettings_useStageCache,
    canarySettings_stageVariableOverrides,
    canarySettings_percentTraffic,

    -- * ClientCertificate
    ClientCertificate (..),
    newClientCertificate,
    clientCertificate_tags,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_description,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_expirationDate,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings (..),
    newDeploymentCanarySettings,
    deploymentCanarySettings_useStageCache,
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_percentTraffic,

    -- * DocumentationPart
    DocumentationPart (..),
    newDocumentationPart,
    documentationPart_properties,
    documentationPart_id,
    documentationPart_location,

    -- * DocumentationPartLocation
    DocumentationPartLocation (..),
    newDocumentationPartLocation,
    documentationPartLocation_name,
    documentationPartLocation_method,
    documentationPartLocation_path,
    documentationPartLocation_statusCode,
    documentationPartLocation_type,

    -- * DocumentationVersion
    DocumentationVersion (..),
    newDocumentationVersion,
    documentationVersion_description,
    documentationVersion_createdDate,
    documentationVersion_version,

    -- * DomainName
    DomainName (..),
    newDomainName,
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    newEndpointConfiguration,
    endpointConfiguration_vpcEndpointIds,
    endpointConfiguration_types,

    -- * GatewayResponse
    GatewayResponse (..),
    newGatewayResponse,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,

    -- * Integration
    Integration (..),
    newIntegration,
    integration_cacheKeyParameters,
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,

    -- * IntegrationResponse
    IntegrationResponse (..),
    newIntegrationResponse,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_contentHandling,
    integrationResponse_statusCode,

    -- * Method
    Method (..),
    newMethod,
    method_requestModels,
    method_requestParameters,
    method_methodResponses,
    method_apiKeyRequired,
    method_requestValidatorId,
    method_httpMethod,
    method_methodIntegration,
    method_authorizationScopes,
    method_authorizationType,
    method_operationName,
    method_authorizerId,

    -- * MethodResponse
    MethodResponse (..),
    newMethodResponse,
    methodResponse_responseParameters,
    methodResponse_statusCode,
    methodResponse_responseModels,

    -- * MethodSetting
    MethodSetting (..),
    newMethodSetting,
    methodSetting_throttlingRateLimit,
    methodSetting_loggingLevel,
    methodSetting_throttlingBurstLimit,
    methodSetting_metricsEnabled,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_unauthorizedCacheControlHeaderStrategy,
    methodSetting_cacheTtlInSeconds,
    methodSetting_dataTraceEnabled,
    methodSetting_cachingEnabled,
    methodSetting_cacheDataEncrypted,

    -- * MethodSnapshot
    MethodSnapshot (..),
    newMethodSnapshot,
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- * Model
    Model (..),
    newModel,
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,

    -- * MutualTlsAuthentication
    MutualTlsAuthentication (..),
    newMutualTlsAuthentication,
    mutualTlsAuthentication_truststoreWarnings,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreUri,

    -- * MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (..),
    newMutualTlsAuthenticationInput,
    mutualTlsAuthenticationInput_truststoreVersion,
    mutualTlsAuthenticationInput_truststoreUri,

    -- * PatchOperation
    PatchOperation (..),
    newPatchOperation,
    patchOperation_from,
    patchOperation_op,
    patchOperation_path,
    patchOperation_value,

    -- * QuotaSettings
    QuotaSettings (..),
    newQuotaSettings,
    quotaSettings_offset,
    quotaSettings_period,
    quotaSettings_limit,

    -- * RequestValidator
    RequestValidator (..),
    newRequestValidator,
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,

    -- * Resource
    Resource (..),
    newResource,
    resource_pathPart,
    resource_path,
    resource_parentId,
    resource_id,
    resource_resourceMethods,

    -- * RestApi
    RestApi (..),
    newRestApi,
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,

    -- * SdkConfigurationProperty
    SdkConfigurationProperty (..),
    newSdkConfigurationProperty,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_required,
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_friendlyName,

    -- * SdkType
    SdkType (..),
    newSdkType,
    sdkType_description,
    sdkType_id,
    sdkType_friendlyName,
    sdkType_configurationProperties,

    -- * Stage
    Stage (..),
    newStage,
    stage_tags,
    stage_webAclArn,
    stage_stageName,
    stage_cacheClusterEnabled,
    stage_accessLogSettings,
    stage_cacheClusterStatus,
    stage_deploymentId,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_description,
    stage_tracingEnabled,
    stage_clientCertificateId,
    stage_cacheClusterSize,
    stage_canarySettings,
    stage_createdDate,
    stage_documentationVersion,
    stage_variables,

    -- * StageKey
    StageKey (..),
    newStageKey,
    stageKey_stageName,
    stageKey_restApiId,

    -- * ThrottleSettings
    ThrottleSettings (..),
    newThrottleSettings,
    throttleSettings_rateLimit,
    throttleSettings_burstLimit,

    -- * TlsConfig
    TlsConfig (..),
    newTlsConfig,
    tlsConfig_insecureSkipVerification,

    -- * Usage
    Usage (..),
    newUsage,
    usage_items,
    usage_endDate,
    usage_startDate,
    usage_usagePlanId,
    usage_position,

    -- * UsagePlan
    UsagePlan (..),
    newUsagePlan,
    usagePlan_tags,
    usagePlan_name,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_apiStages,

    -- * UsagePlanKey
    UsagePlanKey (..),
    newUsagePlanKey,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_id,
    usagePlanKey_value,

    -- * VpcLink
    VpcLink (..),
    newVpcLink,
    vpcLink_tags,
    vpcLink_name,
    vpcLink_status,
    vpcLink_description,
    vpcLink_id,
    vpcLink_targetArns,
    vpcLink_statusMessage,
  )
where

import Amazonka.APIGateway.Types.AccessLogSettings
import Amazonka.APIGateway.Types.Account
import Amazonka.APIGateway.Types.ApiKey
import Amazonka.APIGateway.Types.ApiKeySourceType
import Amazonka.APIGateway.Types.ApiKeysFormat
import Amazonka.APIGateway.Types.ApiStage
import Amazonka.APIGateway.Types.Authorizer
import Amazonka.APIGateway.Types.AuthorizerType
import Amazonka.APIGateway.Types.BasePathMapping
import Amazonka.APIGateway.Types.CacheClusterSize
import Amazonka.APIGateway.Types.CacheClusterStatus
import Amazonka.APIGateway.Types.CanarySettings
import Amazonka.APIGateway.Types.ClientCertificate
import Amazonka.APIGateway.Types.ConnectionType
import Amazonka.APIGateway.Types.ContentHandlingStrategy
import Amazonka.APIGateway.Types.Deployment
import Amazonka.APIGateway.Types.DeploymentCanarySettings
import Amazonka.APIGateway.Types.DocumentationPart
import Amazonka.APIGateway.Types.DocumentationPartLocation
import Amazonka.APIGateway.Types.DocumentationPartType
import Amazonka.APIGateway.Types.DocumentationVersion
import Amazonka.APIGateway.Types.DomainName
import Amazonka.APIGateway.Types.DomainNameStatus
import Amazonka.APIGateway.Types.EndpointConfiguration
import Amazonka.APIGateway.Types.EndpointType
import Amazonka.APIGateway.Types.GatewayResponse
import Amazonka.APIGateway.Types.GatewayResponseType
import Amazonka.APIGateway.Types.Integration
import Amazonka.APIGateway.Types.IntegrationResponse
import Amazonka.APIGateway.Types.IntegrationType
import Amazonka.APIGateway.Types.LocationStatusType
import Amazonka.APIGateway.Types.Method
import Amazonka.APIGateway.Types.MethodResponse
import Amazonka.APIGateway.Types.MethodSetting
import Amazonka.APIGateway.Types.MethodSnapshot
import Amazonka.APIGateway.Types.Model
import Amazonka.APIGateway.Types.MutualTlsAuthentication
import Amazonka.APIGateway.Types.MutualTlsAuthenticationInput
import Amazonka.APIGateway.Types.Op
import Amazonka.APIGateway.Types.PatchOperation
import Amazonka.APIGateway.Types.PutMode
import Amazonka.APIGateway.Types.QuotaPeriodType
import Amazonka.APIGateway.Types.QuotaSettings
import Amazonka.APIGateway.Types.RequestValidator
import Amazonka.APIGateway.Types.Resource
import Amazonka.APIGateway.Types.RestApi
import Amazonka.APIGateway.Types.SdkConfigurationProperty
import Amazonka.APIGateway.Types.SdkType
import Amazonka.APIGateway.Types.SecurityPolicy
import Amazonka.APIGateway.Types.Stage
import Amazonka.APIGateway.Types.StageKey
import Amazonka.APIGateway.Types.ThrottleSettings
import Amazonka.APIGateway.Types.TlsConfig
import Amazonka.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import Amazonka.APIGateway.Types.Usage
import Amazonka.APIGateway.Types.UsagePlan
import Amazonka.APIGateway.Types.UsagePlanKey
import Amazonka.APIGateway.Types.VpcLink
import Amazonka.APIGateway.Types.VpcLinkStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon API Gateway SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "APIGateway",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "apigateway",
      Core.signingName = "apigateway",
      Core.version = "2015-07-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "APIGateway",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request is denied because the caller has insufficient permissions.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The requested resource is not found. Make sure that the request URI is
-- correct.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The requested service is not available. For details see the accompanying
-- error message. Retry after the specified time period.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request exceeded the rate limit. Retry after the specified time
-- period.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The request configuration has conflicts. For details, see the
-- accompanying error message.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The submitted request is not valid, for example, the input is incomplete
-- or incorrect. See the accompanying error message for details.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request has reached its throttling limit. Retry after the specified
-- time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
