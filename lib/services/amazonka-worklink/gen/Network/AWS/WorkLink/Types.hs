{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkLink.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _UnauthorizedException,
    _ResourceNotFoundException,

    -- * AuthorizationProviderType
    AuthorizationProviderType (..),

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * FleetStatus
    FleetStatus (..),

    -- * IdentityProviderType
    IdentityProviderType (..),

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_deviceStatus,
    deviceSummary_deviceId,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_displayName,
    domainSummary_domainName,
    domainSummary_createdTime,
    domainSummary_domainStatus,

    -- * FleetSummary
    FleetSummary (..),
    newFleetSummary,
    fleetSummary_lastUpdatedTime,
    fleetSummary_fleetStatus,
    fleetSummary_companyCode,
    fleetSummary_createdTime,
    fleetSummary_fleetArn,
    fleetSummary_displayName,
    fleetSummary_fleetName,
    fleetSummary_tags,

    -- * WebsiteAuthorizationProviderSummary
    WebsiteAuthorizationProviderSummary (..),
    newWebsiteAuthorizationProviderSummary,
    websiteAuthorizationProviderSummary_authorizationProviderId,
    websiteAuthorizationProviderSummary_createdTime,
    websiteAuthorizationProviderSummary_domainName,
    websiteAuthorizationProviderSummary_authorizationProviderType,

    -- * WebsiteCaSummary
    WebsiteCaSummary (..),
    newWebsiteCaSummary,
    websiteCaSummary_createdTime,
    websiteCaSummary_websiteCaId,
    websiteCaSummary_displayName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkLink.Types.AuthorizationProviderType
import Amazonka.WorkLink.Types.DeviceStatus
import Amazonka.WorkLink.Types.DeviceSummary
import Amazonka.WorkLink.Types.DomainStatus
import Amazonka.WorkLink.Types.DomainSummary
import Amazonka.WorkLink.Types.FleetStatus
import Amazonka.WorkLink.Types.FleetSummary
import Amazonka.WorkLink.Types.IdentityProviderType
import Amazonka.WorkLink.Types.WebsiteAuthorizationProviderSummary
import Amazonka.WorkLink.Types.WebsiteCaSummary

-- | API version @2018-09-25@ of the Amazon WorkLink SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "WorkLink",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "worklink",
      Core._serviceSigningName = "worklink",
      Core._serviceVersion = "2018-09-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "WorkLink",
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

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The number of requests exceeds the limit.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The service is temporarily unavailable.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | You are not authorized to perform this action.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 403

-- | The requested resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
