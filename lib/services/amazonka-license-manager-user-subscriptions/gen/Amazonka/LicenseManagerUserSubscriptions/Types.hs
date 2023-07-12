{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * ActiveDirectoryIdentityProvider
    ActiveDirectoryIdentityProvider (..),
    newActiveDirectoryIdentityProvider,
    activeDirectoryIdentityProvider_directoryId,

    -- * Filter
    Filter (..),
    newFilter,
    filter_attribute,
    filter_operation,
    filter_value,

    -- * IdentityProvider
    IdentityProvider (..),
    newIdentityProvider,
    identityProvider_activeDirectoryIdentityProvider,

    -- * IdentityProviderSummary
    IdentityProviderSummary (..),
    newIdentityProviderSummary,
    identityProviderSummary_failureMessage,
    identityProviderSummary_identityProvider,
    identityProviderSummary_product,
    identityProviderSummary_settings,
    identityProviderSummary_status,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_lastStatusCheckDate,
    instanceSummary_statusMessage,
    instanceSummary_instanceId,
    instanceSummary_products,
    instanceSummary_status,

    -- * InstanceUserSummary
    InstanceUserSummary (..),
    newInstanceUserSummary,
    instanceUserSummary_associationDate,
    instanceUserSummary_disassociationDate,
    instanceUserSummary_domain,
    instanceUserSummary_statusMessage,
    instanceUserSummary_identityProvider,
    instanceUserSummary_instanceId,
    instanceUserSummary_status,
    instanceUserSummary_username,

    -- * ProductUserSummary
    ProductUserSummary (..),
    newProductUserSummary,
    productUserSummary_domain,
    productUserSummary_statusMessage,
    productUserSummary_subscriptionEndDate,
    productUserSummary_subscriptionStartDate,
    productUserSummary_identityProvider,
    productUserSummary_product,
    productUserSummary_status,
    productUserSummary_username,

    -- * Settings
    Settings (..),
    newSettings,
    settings_securityGroupId,
    settings_subnets,

    -- * UpdateSettings
    UpdateSettings (..),
    newUpdateSettings,
    updateSettings_securityGroupId,
    updateSettings_addSubnets,
    updateSettings_removeSubnets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManagerUserSubscriptions.Types.ActiveDirectoryIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.Types.Filter
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProviderSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.InstanceSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.InstanceUserSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.ProductUserSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.Settings
import Amazonka.LicenseManagerUserSubscriptions.Types.UpdateSettings
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon License Manager User Subscriptions SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "LicenseManagerUserSubscriptions",
      Core.signer = Sign.v4,
      Core.endpointPrefix =
        "license-manager-user-subscriptions",
      Core.signingName =
        "license-manager-user-subscriptions",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError
          "LicenseManagerUserSubscriptions",
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

-- | You don\'t have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request couldn\'t be completed because it conflicted with the
-- current state of the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An exception occurred with the service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The resource couldn\'t be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request failed because a service quota is exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied because of request throttling. Retry the request.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | A parameter is not valid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
