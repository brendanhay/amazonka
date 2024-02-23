{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _LimitExceededException,
    _NotFoundException,
    _ServiceUnavailableException,
    _TooManyRequestsException,
    _UnauthorizedException,

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
    account_apiKeyVersion,
    account_cloudwatchRoleArn,
    account_features,
    account_throttleSettings,

    -- * ApiKey
    ApiKey (..),
    newApiKey,
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_description,
    apiKey_enabled,
    apiKey_id,
    apiKey_lastUpdatedDate,
    apiKey_name,
    apiKey_stageKeys,
    apiKey_tags,
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
    authorizer_authType,
    authorizer_authorizerCredentials,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_authorizerUri,
    authorizer_id,
    authorizer_identitySource,
    authorizer_identityValidationExpression,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_type,

    -- * BasePathMapping
    BasePathMapping (..),
    newBasePathMapping,
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,

    -- * CanarySettings
    CanarySettings (..),
    newCanarySettings,
    canarySettings_deploymentId,
    canarySettings_percentTraffic,
    canarySettings_stageVariableOverrides,
    canarySettings_useStageCache,

    -- * ClientCertificate
    ClientCertificate (..),
    newClientCertificate,
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_apiSummary,
    deployment_createdDate,
    deployment_description,
    deployment_id,

    -- * DeploymentCanarySettings
    DeploymentCanarySettings (..),
    newDeploymentCanarySettings,
    deploymentCanarySettings_percentTraffic,
    deploymentCanarySettings_stageVariableOverrides,
    deploymentCanarySettings_useStageCache,

    -- * DocumentationPart
    DocumentationPart (..),
    newDocumentationPart,
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,

    -- * DocumentationPartLocation
    DocumentationPartLocation (..),
    newDocumentationPartLocation,
    documentationPartLocation_method,
    documentationPartLocation_name,
    documentationPartLocation_path,
    documentationPartLocation_statusCode,
    documentationPartLocation_type,

    -- * DocumentationVersion
    DocumentationVersion (..),
    newDocumentationVersion,
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,

    -- * DomainName
    DomainName (..),
    newDomainName,
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
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
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,

    -- * Integration
    Integration (..),
    newIntegration,
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,

    -- * IntegrationResponse
    IntegrationResponse (..),
    newIntegrationResponse,
    integrationResponse_contentHandling,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,

    -- * Method
    Method (..),
    newMethod,
    method_apiKeyRequired,
    method_authorizationScopes,
    method_authorizationType,
    method_authorizerId,
    method_httpMethod,
    method_methodIntegration,
    method_methodResponses,
    method_operationName,
    method_requestModels,
    method_requestParameters,
    method_requestValidatorId,

    -- * MethodResponse
    MethodResponse (..),
    newMethodResponse,
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,

    -- * MethodSetting
    MethodSetting (..),
    newMethodSetting,
    methodSetting_cacheDataEncrypted,
    methodSetting_cacheTtlInSeconds,
    methodSetting_cachingEnabled,
    methodSetting_dataTraceEnabled,
    methodSetting_loggingLevel,
    methodSetting_metricsEnabled,
    methodSetting_requireAuthorizationForCacheControl,
    methodSetting_throttlingBurstLimit,
    methodSetting_throttlingRateLimit,
    methodSetting_unauthorizedCacheControlHeaderStrategy,

    -- * MethodSnapshot
    MethodSnapshot (..),
    newMethodSnapshot,
    methodSnapshot_apiKeyRequired,
    methodSnapshot_authorizationType,

    -- * Model
    Model (..),
    newModel,
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,

    -- * MutualTlsAuthentication
    MutualTlsAuthentication (..),
    newMutualTlsAuthentication,
    mutualTlsAuthentication_truststoreUri,
    mutualTlsAuthentication_truststoreVersion,
    mutualTlsAuthentication_truststoreWarnings,

    -- * MutualTlsAuthenticationInput
    MutualTlsAuthenticationInput (..),
    newMutualTlsAuthenticationInput,
    mutualTlsAuthenticationInput_truststoreUri,
    mutualTlsAuthenticationInput_truststoreVersion,

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
    quotaSettings_limit,
    quotaSettings_offset,
    quotaSettings_period,

    -- * RequestValidator
    RequestValidator (..),
    newRequestValidator,
    requestValidator_id,
    requestValidator_name,
    requestValidator_validateRequestBody,
    requestValidator_validateRequestParameters,

    -- * Resource
    Resource (..),
    newResource,
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,

    -- * RestApi
    RestApi (..),
    newRestApi,
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,

    -- * SdkConfigurationProperty
    SdkConfigurationProperty (..),
    newSdkConfigurationProperty,
    sdkConfigurationProperty_defaultValue,
    sdkConfigurationProperty_description,
    sdkConfigurationProperty_friendlyName,
    sdkConfigurationProperty_name,
    sdkConfigurationProperty_required,

    -- * SdkType
    SdkType (..),
    newSdkType,
    sdkType_configurationProperties,
    sdkType_description,
    sdkType_friendlyName,
    sdkType_id,

    -- * Stage
    Stage (..),
    newStage,
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,

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
    usage_endDate,
    usage_items,
    usage_position,
    usage_startDate,
    usage_usagePlanId,

    -- * UsagePlan
    UsagePlan (..),
    newUsagePlan,
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,

    -- * UsagePlanKey
    UsagePlanKey (..),
    newUsagePlanKey,
    usagePlanKey_id,
    usagePlanKey_name,
    usagePlanKey_type,
    usagePlanKey_value,

    -- * VpcLink
    VpcLink (..),
    newVpcLink,
    vpcLink_description,
    vpcLink_id,
    vpcLink_name,
    vpcLink_status,
    vpcLink_statusMessage,
    vpcLink_tags,
    vpcLink_targetArns,
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The submitted request is not valid, for example, the input is incomplete
-- or incorrect. See the accompanying error message for details.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request configuration has conflicts. For details, see the
-- accompanying error message.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request exceeded the rate limit. Retry after the specified time
-- period.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The requested resource is not found. Make sure that the request URI is
-- correct.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The requested service is not available. For details see the accompanying
-- error message. Retry after the specified time period.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request has reached its throttling limit. Retry after the specified
-- time period.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The request is denied because the caller has insufficient permissions.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401
