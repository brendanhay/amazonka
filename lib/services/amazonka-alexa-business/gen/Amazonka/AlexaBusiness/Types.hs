{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _ConcurrentModificationException,
    _DeviceNotRegisteredException,
    _InvalidCertificateAuthorityException,
    _InvalidDeviceException,
    _InvalidSecretsManagerResourceException,
    _InvalidServiceLinkedRoleStateException,
    _InvalidUserStatusException,
    _LimitExceededException,
    _NameInUseException,
    _NotFoundException,
    _ResourceAssociatedException,
    _ResourceInUseException,
    _SkillNotLinkedException,
    _UnauthorizedException,

    -- * Locale
    Locale (..),

    -- * Audio
    Audio (..),
    newAudio,
    audio_locale,
    audio_location,

    -- * Content
    Content (..),
    newContent,
    content_audioList,
    content_ssmlList,
    content_textList,

    -- * Filter
    Filter (..),
    newFilter,
    filter_key,
    filter_values,

    -- * Ssml
    Ssml (..),
    newSsml,
    ssml_locale,
    ssml_value,

    -- * TextMessage
    TextMessage (..),
    newTextMessage,
    textMessage_locale,
    textMessage_value,
  )
where

import Amazonka.AlexaBusiness.Types.Audio
import Amazonka.AlexaBusiness.Types.Content
import Amazonka.AlexaBusiness.Types.Filter
import Amazonka.AlexaBusiness.Types.Locale
import Amazonka.AlexaBusiness.Types.Ssml
import Amazonka.AlexaBusiness.Types.TextMessage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-09@ of the Amazon Alexa For Business SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AlexaBusiness",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "a4b",
      Core.signingName = "a4b",
      Core.version = "2017-11-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AlexaBusiness",
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

-- | The resource being created already exists.
_AlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | There is a concurrent modification of resources.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request failed because this device is no longer registered and
-- therefore no longer managed by this account.
_DeviceNotRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DeviceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "DeviceNotRegisteredException"

-- | The Certificate Authority can\'t issue or revoke a certificate.
_InvalidCertificateAuthorityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCertificateAuthorityException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateAuthorityException"

-- | The device is in an invalid state.
_InvalidDeviceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDeviceException =
  Core._MatchServiceError
    defaultService
    "InvalidDeviceException"

-- | A password in SecretsManager is in an invalid state.
_InvalidSecretsManagerResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSecretsManagerResourceException =
  Core._MatchServiceError
    defaultService
    "InvalidSecretsManagerResourceException"

-- | The service linked role is locked for deletion.
_InvalidServiceLinkedRoleStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidServiceLinkedRoleStateException =
  Core._MatchServiceError
    defaultService
    "InvalidServiceLinkedRoleStateException"

-- | The attempt to update a user is invalid due to the user\'s current
-- status.
_InvalidUserStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUserStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidUserStatusException"

-- | You are performing an action that would put you beyond your account\'s
-- limits.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The name sent in the request is already in use.
_NameInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NameInUseException =
  Core._MatchServiceError
    defaultService
    "NameInUseException"

-- | The resource is not found.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Another resource is associated with the resource in the request.
_ResourceAssociatedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | The resource in the request is already in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The skill must be linked to a third-party account.
_SkillNotLinkedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SkillNotLinkedException =
  Core._MatchServiceError
    defaultService
    "SkillNotLinkedException"

-- | The caller has no permissions to operate on the resource involved in the
-- API call.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
