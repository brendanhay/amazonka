{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _InvalidResourcePolicyException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
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
    apiGatewayProxyConfig_apiGatewayId,
    apiGatewayProxyConfig_endpointType,
    apiGatewayProxyConfig_nlbArn,
    apiGatewayProxyConfig_nlbName,
    apiGatewayProxyConfig_proxyUrl,
    apiGatewayProxyConfig_stageName,
    apiGatewayProxyConfig_vpcLinkId,

    -- * ApiGatewayProxyInput
    ApiGatewayProxyInput (..),
    newApiGatewayProxyInput,
    apiGatewayProxyInput_endpointType,
    apiGatewayProxyInput_stageName,

    -- * ApiGatewayProxySummary
    ApiGatewayProxySummary (..),
    newApiGatewayProxySummary,
    apiGatewayProxySummary_apiGatewayId,
    apiGatewayProxySummary_endpointType,
    apiGatewayProxySummary_nlbArn,
    apiGatewayProxySummary_nlbName,
    apiGatewayProxySummary_proxyUrl,
    apiGatewayProxySummary_stageName,
    apiGatewayProxySummary_vpcLinkId,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_apiGatewayProxy,
    applicationSummary_applicationId,
    applicationSummary_arn,
    applicationSummary_createdByAccountId,
    applicationSummary_createdTime,
    applicationSummary_environmentId,
    applicationSummary_error,
    applicationSummary_lastUpdatedTime,
    applicationSummary_name,
    applicationSummary_ownerAccountId,
    applicationSummary_proxyType,
    applicationSummary_state,
    applicationSummary_tags,
    applicationSummary_vpcId,

    -- * DefaultRouteInput
    DefaultRouteInput (..),
    newDefaultRouteInput,
    defaultRouteInput_activationState,

    -- * EnvironmentSummary
    EnvironmentSummary (..),
    newEnvironmentSummary,
    environmentSummary_arn,
    environmentSummary_createdTime,
    environmentSummary_description,
    environmentSummary_environmentId,
    environmentSummary_error,
    environmentSummary_lastUpdatedTime,
    environmentSummary_name,
    environmentSummary_networkFabricType,
    environmentSummary_ownerAccountId,
    environmentSummary_state,
    environmentSummary_tags,
    environmentSummary_transitGatewayId,

    -- * EnvironmentVpc
    EnvironmentVpc (..),
    newEnvironmentVpc,
    environmentVpc_accountId,
    environmentVpc_cidrBlocks,
    environmentVpc_createdTime,
    environmentVpc_environmentId,
    environmentVpc_lastUpdatedTime,
    environmentVpc_vpcId,
    environmentVpc_vpcName,

    -- * ErrorResponse
    ErrorResponse (..),
    newErrorResponse,
    errorResponse_accountId,
    errorResponse_additionalDetails,
    errorResponse_code,
    errorResponse_message,
    errorResponse_resourceIdentifier,
    errorResponse_resourceType,

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
    routeSummary_applicationId,
    routeSummary_arn,
    routeSummary_createdByAccountId,
    routeSummary_createdTime,
    routeSummary_environmentId,
    routeSummary_error,
    routeSummary_includeChildPaths,
    routeSummary_lastUpdatedTime,
    routeSummary_methods,
    routeSummary_ownerAccountId,
    routeSummary_pathResourceToId,
    routeSummary_routeId,
    routeSummary_routeType,
    routeSummary_serviceId,
    routeSummary_sourcePath,
    routeSummary_state,
    routeSummary_tags,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_applicationId,
    serviceSummary_arn,
    serviceSummary_createdByAccountId,
    serviceSummary_createdTime,
    serviceSummary_description,
    serviceSummary_endpointType,
    serviceSummary_environmentId,
    serviceSummary_error,
    serviceSummary_lambdaEndpoint,
    serviceSummary_lastUpdatedTime,
    serviceSummary_name,
    serviceSummary_ownerAccountId,
    serviceSummary_serviceId,
    serviceSummary_state,
    serviceSummary_tags,
    serviceSummary_urlEndpoint,
    serviceSummary_vpcId,

    -- * UriPathRouteInput
    UriPathRouteInput (..),
    newUriPathRouteInput,
    uriPathRouteInput_includeChildPaths,
    uriPathRouteInput_methods,
    uriPathRouteInput_activationState,
    uriPathRouteInput_sourcePath,

    -- * UrlEndpointConfig
    UrlEndpointConfig (..),
    newUrlEndpointConfig,
    urlEndpointConfig_healthUrl,
    urlEndpointConfig_url,

    -- * UrlEndpointInput
    UrlEndpointInput (..),
    newUrlEndpointInput,
    urlEndpointInput_healthUrl,
    urlEndpointInput_url,

    -- * UrlEndpointSummary
    UrlEndpointSummary (..),
    newUrlEndpointSummary,
    urlEndpointSummary_healthUrl,
    urlEndpointSummary_url,
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

-- | The user does not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred while processing the request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource policy is not valid.
_InvalidResourcePolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResourcePolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidResourcePolicyException"
    Prelude.. Core.hasStatus 400

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request was denied because the request was throttled.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input does not satisfy the constraints specified by an Amazon Web
-- Service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
