{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _InvalidResourcePolicyException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * ApiGatewayEndpointType
    ApiGatewayEndpointType (..),

    -- * ApplicationState
    ApplicationState (..),

    -- * EnvironmentState
    EnvironmentState (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ErrorResourceType
    ErrorResourceType (..),

    -- * HttpMethod
    HttpMethod (..),

    -- * NetworkFabricType
    NetworkFabricType (..),

    -- * ProxyType
    ProxyType (..),

    -- * RouteActivationState
    RouteActivationState (..),

    -- * RouteState
    RouteState (..),

    -- * RouteType
    RouteType (..),

    -- * ServiceEndpointType
    ServiceEndpointType (..),

    -- * ServiceState
    ServiceState (..),

    -- * ApiGatewayProxyConfig
    ApiGatewayProxyConfig (..),
    newApiGatewayProxyConfig,
    apiGatewayProxyConfig_stageName,
    apiGatewayProxyConfig_proxyUrl,
    apiGatewayProxyConfig_endpointType,
    apiGatewayProxyConfig_apiGatewayId,
    apiGatewayProxyConfig_nlbName,
    apiGatewayProxyConfig_vpcLinkId,
    apiGatewayProxyConfig_nlbArn,

    -- * ApiGatewayProxyInput
    ApiGatewayProxyInput (..),
    newApiGatewayProxyInput,
    apiGatewayProxyInput_stageName,
    apiGatewayProxyInput_endpointType,

    -- * ApiGatewayProxySummary
    ApiGatewayProxySummary (..),
    newApiGatewayProxySummary,
    apiGatewayProxySummary_stageName,
    apiGatewayProxySummary_proxyUrl,
    apiGatewayProxySummary_endpointType,
    apiGatewayProxySummary_apiGatewayId,
    apiGatewayProxySummary_nlbName,
    apiGatewayProxySummary_vpcLinkId,
    apiGatewayProxySummary_nlbArn,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_tags,
    applicationSummary_name,
    applicationSummary_proxyType,
    applicationSummary_createdTime,
    applicationSummary_createdByAccountId,
    applicationSummary_arn,
    applicationSummary_state,
    applicationSummary_lastUpdatedTime,
    applicationSummary_ownerAccountId,
    applicationSummary_vpcId,
    applicationSummary_environmentId,
    applicationSummary_error,
    applicationSummary_applicationId,
    applicationSummary_apiGatewayProxy,

    -- * DefaultRouteInput
    DefaultRouteInput (..),
    newDefaultRouteInput,
    defaultRouteInput_activationState,

    -- * EnvironmentSummary
    EnvironmentSummary (..),
    newEnvironmentSummary,
    environmentSummary_tags,
    environmentSummary_name,
    environmentSummary_createdTime,
    environmentSummary_transitGatewayId,
    environmentSummary_arn,
    environmentSummary_state,
    environmentSummary_lastUpdatedTime,
    environmentSummary_description,
    environmentSummary_ownerAccountId,
    environmentSummary_environmentId,
    environmentSummary_error,
    environmentSummary_networkFabricType,

    -- * EnvironmentVpc
    EnvironmentVpc (..),
    newEnvironmentVpc,
    environmentVpc_createdTime,
    environmentVpc_vpcName,
    environmentVpc_lastUpdatedTime,
    environmentVpc_accountId,
    environmentVpc_vpcId,
    environmentVpc_environmentId,
    environmentVpc_cidrBlocks,

    -- * ErrorResponse
    ErrorResponse (..),
    newErrorResponse,
    errorResponse_resourceType,
    errorResponse_message,
    errorResponse_additionalDetails,
    errorResponse_code,
    errorResponse_accountId,
    errorResponse_resourceIdentifier,

    -- * LambdaEndpointConfig
    LambdaEndpointConfig (..),
    newLambdaEndpointConfig,
    lambdaEndpointConfig_arn,

    -- * LambdaEndpointInput
    LambdaEndpointInput (..),
    newLambdaEndpointInput,
    lambdaEndpointInput_arn,

    -- * LambdaEndpointSummary
    LambdaEndpointSummary (..),
    newLambdaEndpointSummary,
    lambdaEndpointSummary_arn,

    -- * RouteSummary
    RouteSummary (..),
    newRouteSummary,
    routeSummary_tags,
    routeSummary_routeType,
    routeSummary_createdTime,
    routeSummary_createdByAccountId,
    routeSummary_arn,
    routeSummary_state,
    routeSummary_pathResourceToId,
    routeSummary_lastUpdatedTime,
    routeSummary_ownerAccountId,
    routeSummary_routeId,
    routeSummary_methods,
    routeSummary_environmentId,
    routeSummary_error,
    routeSummary_applicationId,
    routeSummary_sourcePath,
    routeSummary_includeChildPaths,
    routeSummary_serviceId,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_tags,
    serviceSummary_name,
    serviceSummary_createdTime,
    serviceSummary_createdByAccountId,
    serviceSummary_arn,
    serviceSummary_state,
    serviceSummary_urlEndpoint,
    serviceSummary_lastUpdatedTime,
    serviceSummary_endpointType,
    serviceSummary_description,
    serviceSummary_ownerAccountId,
    serviceSummary_lambdaEndpoint,
    serviceSummary_vpcId,
    serviceSummary_environmentId,
    serviceSummary_error,
    serviceSummary_applicationId,
    serviceSummary_serviceId,

    -- * UriPathRouteInput
    UriPathRouteInput (..),
    newUriPathRouteInput,
    uriPathRouteInput_methods,
    uriPathRouteInput_includeChildPaths,
    uriPathRouteInput_activationState,
    uriPathRouteInput_sourcePath,

    -- * UrlEndpointConfig
    UrlEndpointConfig (..),
    newUrlEndpointConfig,
    urlEndpointConfig_url,
    urlEndpointConfig_healthUrl,

    -- * UrlEndpointInput
    UrlEndpointInput (..),
    newUrlEndpointInput,
    urlEndpointInput_healthUrl,
    urlEndpointInput_url,

    -- * UrlEndpointSummary
    UrlEndpointSummary (..),
    newUrlEndpointSummary,
    urlEndpointSummary_url,
    urlEndpointSummary_healthUrl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayEndpointType
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyConfig
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyInput
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxySummary
import Amazonka.MigrationHubReFactorSpaces.Types.ApplicationState
import Amazonka.MigrationHubReFactorSpaces.Types.ApplicationSummary
import Amazonka.MigrationHubReFactorSpaces.Types.DefaultRouteInput
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentState
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentSummary
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentVpc
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorCode
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResourceType
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.HttpMethod
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointConfig
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointInput
import Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointSummary
import Amazonka.MigrationHubReFactorSpaces.Types.NetworkFabricType
import Amazonka.MigrationHubReFactorSpaces.Types.ProxyType
import Amazonka.MigrationHubReFactorSpaces.Types.RouteActivationState
import Amazonka.MigrationHubReFactorSpaces.Types.RouteState
import Amazonka.MigrationHubReFactorSpaces.Types.RouteSummary
import Amazonka.MigrationHubReFactorSpaces.Types.RouteType
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceEndpointType
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceState
import Amazonka.MigrationHubReFactorSpaces.Types.ServiceSummary
import Amazonka.MigrationHubReFactorSpaces.Types.UriPathRouteInput
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointConfig
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointInput
import Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-10-26@ of the Amazon Migration Hub Refactor Spaces SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "MigrationHubReFactorSpaces",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "refactor-spaces",
      Core.signingName = "refactor-spaces",
      Core.version = "2021-10-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "MigrationHubReFactorSpaces",
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

-- | The user does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An unexpected error occurred while processing the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource policy is not valid.
_InvalidResourcePolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourcePolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidResourcePolicyException"
    Prelude.. Core.hasStatus 400

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Request was denied because the request was throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input does not satisfy the constraints specified by an Amazon Web
-- Service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
