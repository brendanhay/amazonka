{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _NotFoundException,
    _ConflictException,
    _BadRequestException,
    _TooManyRequestsException,

    -- * AuthorizationType
    AuthorizationType (..),

    -- * AuthorizerType
    AuthorizerType (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * ContentHandlingStrategy
    ContentHandlingStrategy (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DomainNameStatus
    DomainNameStatus (..),

    -- * EndpointType
    EndpointType (..),

    -- * IntegrationType
    IntegrationType (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * PassthroughBehavior
    PassthroughBehavior (..),

    -- * ProtocolType
    ProtocolType (..),

    -- * SecurityPolicy
    SecurityPolicy (..),

    -- * VpcLinkStatus
    VpcLinkStatus (..),

    -- * VpcLinkVersion
    VpcLinkVersion (..),

    -- * AccessLogSettings
    AccessLogSettings (..),
    newAccessLogSettings,
    accessLogSettings_format,
    accessLogSettings_destinationArn,

    -- * Api
    Api (..),
    newApi,
    api_tags,
    api_apiEndpoint,
    api_apiId,
    api_description,
    api_warnings,
    api_apiKeySelectionExpression,
    api_disableExecuteApiEndpoint,
    api_createdDate,
    api_disableSchemaValidation,
    api_importInfo,
    api_corsConfiguration,
    api_apiGatewayManaged,
    api_version,
    api_routeSelectionExpression,
    api_name,
    api_protocolType,

    -- * ApiMapping
    ApiMapping (..),
    newApiMapping,
    apiMapping_apiMappingKey,
    apiMapping_apiMappingId,
    apiMapping_stage,
    apiMapping_apiId,

    -- * Authorizer
    Authorizer (..),
    newAuthorizer,
    authorizer_authorizerCredentialsArn,
    authorizer_identitySource,
    authorizer_authorizerResultTtlInSeconds,
    authorizer_jwtConfiguration,
    authorizer_identityValidationExpression,
    authorizer_authorizerUri,
    authorizer_authorizerType,
    authorizer_enableSimpleResponses,
    authorizer_authorizerId,
    authorizer_authorizerPayloadFormatVersion,
    authorizer_name,

    -- * Cors
    Cors (..),
    newCors,
    cors_allowHeaders,
    cors_exposeHeaders,
    cors_allowCredentials,
    cors_allowMethods,
    cors_allowOrigins,
    cors_maxAge,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentStatus,
    deployment_autoDeployed,
    deployment_deploymentId,
    deployment_description,
    deployment_deploymentStatusMessage,
    deployment_createdDate,

    -- * DomainName
    DomainName (..),
    newDomainName,
    domainName_tags,
    domainName_mutualTlsAuthentication,
    domainName_domainNameConfigurations,
    domainName_apiMappingSelectionExpression,
    domainName_domainName,

    -- * DomainNameConfiguration
    DomainNameConfiguration (..),
    newDomainNameConfiguration,
    domainNameConfiguration_hostedZoneId,
    domainNameConfiguration_domainNameStatus,
    domainNameConfiguration_ownershipVerificationCertificateArn,
    domainNameConfiguration_certificateName,
    domainNameConfiguration_endpointType,
    domainNameConfiguration_domainNameStatusMessage,
    domainNameConfiguration_certificateArn,
    domainNameConfiguration_certificateUploadDate,
    domainNameConfiguration_apiGatewayDomainName,
    domainNameConfiguration_securityPolicy,

    -- * Integration
    Integration (..),
    newIntegration,
    integration_credentialsArn,
    integration_requestParameters,
    integration_connectionType,
    integration_tlsConfig,
    integration_templateSelectionExpression,
    integration_integrationResponseSelectionExpression,
    integration_connectionId,
    integration_description,
    integration_integrationMethod,
    integration_integrationUri,
    integration_integrationType,
    integration_responseParameters,
    integration_payloadFormatVersion,
    integration_timeoutInMillis,
    integration_integrationId,
    integration_contentHandlingStrategy,
    integration_requestTemplates,
    integration_apiGatewayManaged,
    integration_passthroughBehavior,
    integration_integrationSubtype,

    -- * IntegrationResponse
    IntegrationResponse (..),
    newIntegrationResponse,
    integrationResponse_templateSelectionExpression,
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_contentHandlingStrategy,
    integrationResponse_integrationResponseId,
    integrationResponse_integrationResponseKey,

    -- * JWTConfiguration
    JWTConfiguration (..),
    newJWTConfiguration,
    jWTConfiguration_issuer,
    jWTConfiguration_audience,

    -- * Model
    Model (..),
    newModel,
    model_description,
    model_schema,
    model_modelId,
    model_contentType,
    model_name,

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

    -- * ParameterConstraints
    ParameterConstraints (..),
    newParameterConstraints,
    parameterConstraints_required,

    -- * Route
    Route (..),
    newRoute,
    route_requestModels,
    route_requestParameters,
    route_apiKeyRequired,
    route_target,
    route_modelSelectionExpression,
    route_routeId,
    route_authorizationScopes,
    route_authorizationType,
    route_operationName,
    route_apiGatewayManaged,
    route_routeResponseSelectionExpression,
    route_authorizerId,
    route_routeKey,

    -- * RouteResponse
    RouteResponse (..),
    newRouteResponse,
    routeResponse_modelSelectionExpression,
    routeResponse_responseParameters,
    routeResponse_responseModels,
    routeResponse_routeResponseId,
    routeResponse_routeResponseKey,

    -- * RouteSettings
    RouteSettings (..),
    newRouteSettings,
    routeSettings_throttlingRateLimit,
    routeSettings_loggingLevel,
    routeSettings_throttlingBurstLimit,
    routeSettings_detailedMetricsEnabled,
    routeSettings_dataTraceEnabled,

    -- * Stage
    Stage (..),
    newStage,
    stage_tags,
    stage_accessLogSettings,
    stage_deploymentId,
    stage_autoDeploy,
    stage_lastUpdatedDate,
    stage_stageVariables,
    stage_description,
    stage_clientCertificateId,
    stage_lastDeploymentStatusMessage,
    stage_defaultRouteSettings,
    stage_createdDate,
    stage_apiGatewayManaged,
    stage_routeSettings,
    stage_stageName,

    -- * TlsConfig
    TlsConfig (..),
    newTlsConfig,
    tlsConfig_serverNameToVerify,

    -- * TlsConfigInput
    TlsConfigInput (..),
    newTlsConfigInput,
    tlsConfigInput_serverNameToVerify,

    -- * VpcLink
    VpcLink (..),
    newVpcLink,
    vpcLink_tags,
    vpcLink_vpcLinkStatusMessage,
    vpcLink_vpcLinkVersion,
    vpcLink_vpcLinkStatus,
    vpcLink_createdDate,
    vpcLink_vpcLinkId,
    vpcLink_securityGroupIds,
    vpcLink_subnetIds,
    vpcLink_name,
  )
where

import Amazonka.ApiGatewayV2.Types.AccessLogSettings
import Amazonka.ApiGatewayV2.Types.Api
import Amazonka.ApiGatewayV2.Types.ApiMapping
import Amazonka.ApiGatewayV2.Types.AuthorizationType
import Amazonka.ApiGatewayV2.Types.Authorizer
import Amazonka.ApiGatewayV2.Types.AuthorizerType
import Amazonka.ApiGatewayV2.Types.ConnectionType
import Amazonka.ApiGatewayV2.Types.ContentHandlingStrategy
import Amazonka.ApiGatewayV2.Types.Cors
import Amazonka.ApiGatewayV2.Types.Deployment
import Amazonka.ApiGatewayV2.Types.DeploymentStatus
import Amazonka.ApiGatewayV2.Types.DomainName
import Amazonka.ApiGatewayV2.Types.DomainNameConfiguration
import Amazonka.ApiGatewayV2.Types.DomainNameStatus
import Amazonka.ApiGatewayV2.Types.EndpointType
import Amazonka.ApiGatewayV2.Types.Integration
import Amazonka.ApiGatewayV2.Types.IntegrationResponse
import Amazonka.ApiGatewayV2.Types.IntegrationType
import Amazonka.ApiGatewayV2.Types.JWTConfiguration
import Amazonka.ApiGatewayV2.Types.LoggingLevel
import Amazonka.ApiGatewayV2.Types.Model
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthenticationInput
import Amazonka.ApiGatewayV2.Types.ParameterConstraints
import Amazonka.ApiGatewayV2.Types.PassthroughBehavior
import Amazonka.ApiGatewayV2.Types.ProtocolType
import Amazonka.ApiGatewayV2.Types.Route
import Amazonka.ApiGatewayV2.Types.RouteResponse
import Amazonka.ApiGatewayV2.Types.RouteSettings
import Amazonka.ApiGatewayV2.Types.SecurityPolicy
import Amazonka.ApiGatewayV2.Types.Stage
import Amazonka.ApiGatewayV2.Types.TlsConfig
import Amazonka.ApiGatewayV2.Types.TlsConfigInput
import Amazonka.ApiGatewayV2.Types.VpcLink
import Amazonka.ApiGatewayV2.Types.VpcLinkStatus
import Amazonka.ApiGatewayV2.Types.VpcLinkVersion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-29@ of the Amazon ApiGatewayV2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ApiGatewayV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "apigateway",
      Core.signingName = "apigateway",
      Core.version = "2018-11-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ApiGatewayV2",
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

-- | Prism for AccessDeniedException' errors.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The resource specified in the request was not found. See the message
-- field for more information.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request. See the accompanying error message for
-- details.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request is not valid, for example, the input is incomplete or
-- incorrect. See the accompanying error message for details.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | A limit has been exceeded. See the accompanying error message for
-- details.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
