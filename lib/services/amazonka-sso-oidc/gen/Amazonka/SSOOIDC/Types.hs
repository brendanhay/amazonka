{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOOIDC.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOOIDC.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AuthorizationPendingException,
    _ExpiredTokenException,
    _InternalServerException,
    _InvalidClientException,
    _InvalidClientMetadataException,
    _InvalidGrantException,
    _InvalidRequestException,
    _InvalidScopeException,
    _SlowDownException,
    _UnauthorizedClientException,
    _UnsupportedGrantTypeException,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-06-10@ of the Amazon SSO OIDC SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SSOOIDC",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "oidc",
      Core.signingName = "awsssooidc",
      Core.version = "2019-06-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SSOOIDC",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a request to authorize a client with an access user
-- session token is pending.
_AuthorizationPendingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationPendingException =
  Core._MatchServiceError
    defaultService
    "AuthorizationPendingException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the token issued by the service is expired and is no
-- longer valid.
_ExpiredTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredTokenException"
    Prelude.. Core.hasStatus 400

-- | Indicates that an error from the service occurred while trying to
-- process a request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Indicates that the @clientId@ or @clientSecret@ in the request is
-- invalid. For example, this can occur when a client sends an incorrect
-- @clientId@ or an expired @clientSecret@.
_InvalidClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClientException =
  Core._MatchServiceError
    defaultService
    "InvalidClientException"
    Prelude.. Core.hasStatus 401

-- | Indicates that the client information sent in the request during
-- registration is invalid.
_InvalidClientMetadataException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClientMetadataException =
  Core._MatchServiceError
    defaultService
    "InvalidClientMetadataException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a request contains an invalid grant. This can occur if a
-- client makes a CreateToken request with an invalid grant type.
_InvalidGrantException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGrantException =
  Core._MatchServiceError
    defaultService
    "InvalidGrantException"
    Prelude.. Core.hasStatus 400

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter might be missing or out of range.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the scope provided in the request is invalid.
_InvalidScopeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidScopeException =
  Core._MatchServiceError
    defaultService
    "InvalidScopeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the client is making the request too frequently and is
-- more than the service can handle.
_SlowDownException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SlowDownException =
  Core._MatchServiceError
    defaultService
    "SlowDownException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the client is not currently authorized to make the
-- request. This can happen when a @clientId@ is not issued for a public
-- client.
_UnauthorizedClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the grant type in the request is not supported by the
-- service.
_UnsupportedGrantTypeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedGrantTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedGrantTypeException"
    Prelude.. Core.hasStatus 400
