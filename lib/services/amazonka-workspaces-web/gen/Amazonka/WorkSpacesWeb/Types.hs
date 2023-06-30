{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpacesWeb.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _TooManyTagsException,
    _ValidationException,

    -- * BrowserType
    BrowserType (..),

    -- * EnabledType
    EnabledType (..),

    -- * IdentityProviderType
    IdentityProviderType (..),

    -- * PortalStatus
    PortalStatus (..),

    -- * RendererType
    RendererType (..),

    -- * BrowserSettings
    BrowserSettings (..),
    newBrowserSettings,
    browserSettings_associatedPortalArns,
    browserSettings_browserPolicy,
    browserSettings_browserSettingsArn,

    -- * BrowserSettingsSummary
    BrowserSettingsSummary (..),
    newBrowserSettingsSummary,
    browserSettingsSummary_browserSettingsArn,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_body,
    certificate_issuer,
    certificate_notValidAfter,
    certificate_notValidBefore,
    certificate_subject,
    certificate_thumbprint,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_issuer,
    certificateSummary_notValidAfter,
    certificateSummary_notValidBefore,
    certificateSummary_subject,
    certificateSummary_thumbprint,

    -- * IdentityProvider
    IdentityProvider (..),
    newIdentityProvider,
    identityProvider_identityProviderDetails,
    identityProvider_identityProviderName,
    identityProvider_identityProviderType,
    identityProvider_identityProviderArn,

    -- * IdentityProviderSummary
    IdentityProviderSummary (..),
    newIdentityProviderSummary,
    identityProviderSummary_identityProviderArn,
    identityProviderSummary_identityProviderName,
    identityProviderSummary_identityProviderType,

    -- * NetworkSettings
    NetworkSettings (..),
    newNetworkSettings,
    networkSettings_associatedPortalArns,
    networkSettings_securityGroupIds,
    networkSettings_subnetIds,
    networkSettings_vpcId,
    networkSettings_networkSettingsArn,

    -- * NetworkSettingsSummary
    NetworkSettingsSummary (..),
    newNetworkSettingsSummary,
    networkSettingsSummary_networkSettingsArn,
    networkSettingsSummary_vpcId,

    -- * Portal
    Portal (..),
    newPortal,
    portal_browserSettingsArn,
    portal_browserType,
    portal_creationDate,
    portal_displayName,
    portal_networkSettingsArn,
    portal_portalArn,
    portal_portalEndpoint,
    portal_portalStatus,
    portal_rendererType,
    portal_statusReason,
    portal_trustStoreArn,
    portal_userAccessLoggingSettingsArn,
    portal_userSettingsArn,

    -- * PortalSummary
    PortalSummary (..),
    newPortalSummary,
    portalSummary_browserSettingsArn,
    portalSummary_browserType,
    portalSummary_creationDate,
    portalSummary_displayName,
    portalSummary_networkSettingsArn,
    portalSummary_portalArn,
    portalSummary_portalEndpoint,
    portalSummary_portalStatus,
    portalSummary_rendererType,
    portalSummary_trustStoreArn,
    portalSummary_userAccessLoggingSettingsArn,
    portalSummary_userSettingsArn,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrustStore
    TrustStore (..),
    newTrustStore,
    trustStore_associatedPortalArns,
    trustStore_trustStoreArn,

    -- * TrustStoreSummary
    TrustStoreSummary (..),
    newTrustStoreSummary,
    trustStoreSummary_trustStoreArn,

    -- * UserAccessLoggingSettings
    UserAccessLoggingSettings (..),
    newUserAccessLoggingSettings,
    userAccessLoggingSettings_associatedPortalArns,
    userAccessLoggingSettings_kinesisStreamArn,
    userAccessLoggingSettings_userAccessLoggingSettingsArn,

    -- * UserAccessLoggingSettingsSummary
    UserAccessLoggingSettingsSummary (..),
    newUserAccessLoggingSettingsSummary,
    userAccessLoggingSettingsSummary_kinesisStreamArn,
    userAccessLoggingSettingsSummary_userAccessLoggingSettingsArn,

    -- * UserSettings
    UserSettings (..),
    newUserSettings,
    userSettings_associatedPortalArns,
    userSettings_copyAllowed,
    userSettings_disconnectTimeoutInMinutes,
    userSettings_downloadAllowed,
    userSettings_idleDisconnectTimeoutInMinutes,
    userSettings_pasteAllowed,
    userSettings_printAllowed,
    userSettings_uploadAllowed,
    userSettings_userSettingsArn,

    -- * UserSettingsSummary
    UserSettingsSummary (..),
    newUserSettingsSummary,
    userSettingsSummary_copyAllowed,
    userSettingsSummary_disconnectTimeoutInMinutes,
    userSettingsSummary_downloadAllowed,
    userSettingsSummary_idleDisconnectTimeoutInMinutes,
    userSettingsSummary_pasteAllowed,
    userSettingsSummary_printAllowed,
    userSettingsSummary_uploadAllowed,
    userSettingsSummary_userSettingsArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkSpacesWeb.Types.BrowserSettings
import Amazonka.WorkSpacesWeb.Types.BrowserSettingsSummary
import Amazonka.WorkSpacesWeb.Types.BrowserType
import Amazonka.WorkSpacesWeb.Types.Certificate
import Amazonka.WorkSpacesWeb.Types.CertificateSummary
import Amazonka.WorkSpacesWeb.Types.EnabledType
import Amazonka.WorkSpacesWeb.Types.IdentityProvider
import Amazonka.WorkSpacesWeb.Types.IdentityProviderSummary
import Amazonka.WorkSpacesWeb.Types.IdentityProviderType
import Amazonka.WorkSpacesWeb.Types.NetworkSettings
import Amazonka.WorkSpacesWeb.Types.NetworkSettingsSummary
import Amazonka.WorkSpacesWeb.Types.Portal
import Amazonka.WorkSpacesWeb.Types.PortalStatus
import Amazonka.WorkSpacesWeb.Types.PortalSummary
import Amazonka.WorkSpacesWeb.Types.RendererType
import Amazonka.WorkSpacesWeb.Types.Tag
import Amazonka.WorkSpacesWeb.Types.TrustStore
import Amazonka.WorkSpacesWeb.Types.TrustStoreSummary
import Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettings
import Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettingsSummary
import Amazonka.WorkSpacesWeb.Types.UserSettings
import Amazonka.WorkSpacesWeb.Types.UserSettingsSummary

-- | API version @2020-07-08@ of the Amazon WorkSpaces Web SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WorkSpacesWeb",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "workspaces-web",
      Core.signingName = "workspaces-web",
      Core.version = "2020-07-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "WorkSpacesWeb",
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

-- | Access is denied.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There is a conflict.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There is an internal server error.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource cannot be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | There is a throttling error.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | There are too many tags.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | There is a validation error.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
