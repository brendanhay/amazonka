{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSOOIDC.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOOIDC.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InvalidRequestException,
    _InvalidScopeException,
    _UnsupportedGrantTypeException,
    _InvalidClientException,
    _InvalidClientMetadataException,
    _UnauthorizedClientException,
    _SlowDownException,
    _InternalServerException,
    _InvalidGrantException,
    _AuthorizationPendingException,
    _ExpiredTokenException,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-06-10@ of the Amazon SSO OIDC SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SSOOIDC",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "oidc",
      Core._serviceSigningName = "awsssooidc",
      Core._serviceVersion = "2019-06-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SSOOIDC",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter might be missing or out of range.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the scope provided in the request is invalid.
_InvalidScopeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidScopeException =
  Core._MatchServiceError
    defaultService
    "InvalidScopeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the grant type in the request is not supported by the
-- service.
_UnsupportedGrantTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedGrantTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedGrantTypeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the @clientId@ or @clientSecret@ in the request is
-- invalid. For example, this can occur when a client sends an incorrect
-- @clientId@ or an expired @clientSecret@.
_InvalidClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientException =
  Core._MatchServiceError
    defaultService
    "InvalidClientException"
    Prelude.. Core.hasStatus 401

-- | Indicates that the client information sent in the request during
-- registration is invalid.
_InvalidClientMetadataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientMetadataException =
  Core._MatchServiceError
    defaultService
    "InvalidClientMetadataException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the client is not currently authorized to make the
-- request. This can happen when a @clientId@ is not issued for a public
-- client.
_UnauthorizedClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the client is making the request too frequently and is
-- more than the service can handle.
_SlowDownException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SlowDownException =
  Core._MatchServiceError
    defaultService
    "SlowDownException"
    Prelude.. Core.hasStatus 400

-- | Indicates that an error from the service occurred while trying to
-- process a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Indicates that a request contains an invalid grant. This can occur if a
-- client makes a CreateToken request with an invalid grant type.
_InvalidGrantException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGrantException =
  Core._MatchServiceError
    defaultService
    "InvalidGrantException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a request to authorize a client with an access user
-- session token is pending.
_AuthorizationPendingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationPendingException =
  Core._MatchServiceError
    defaultService
    "AuthorizationPendingException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the token issued by the service is expired and is no
-- longer valid.
_ExpiredTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredTokenException"
    Prelude.. Core.hasStatus 400
