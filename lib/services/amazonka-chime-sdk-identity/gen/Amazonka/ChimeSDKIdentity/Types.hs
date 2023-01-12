{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKIdentity.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _ResourceLimitExceededException,
    _ServiceFailureException,
    _ServiceUnavailableException,
    _ThrottledClientException,
    _UnauthorizedClientException,

    -- * AllowMessages
    AllowMessages (..),

    -- * AppInstanceUserEndpointType
    AppInstanceUserEndpointType (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * EndpointStatusReason
    EndpointStatusReason (..),

    -- * AppInstance
    AppInstance (..),
    newAppInstance,
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,
    appInstance_metadata,
    appInstance_name,

    -- * AppInstanceAdmin
    AppInstanceAdmin (..),
    newAppInstanceAdmin,
    appInstanceAdmin_admin,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_createdTimestamp,

    -- * AppInstanceAdminSummary
    AppInstanceAdminSummary (..),
    newAppInstanceAdminSummary,
    appInstanceAdminSummary_admin,

    -- * AppInstanceRetentionSettings
    AppInstanceRetentionSettings (..),
    newAppInstanceRetentionSettings,
    appInstanceRetentionSettings_channelRetentionSettings,

    -- * AppInstanceSummary
    AppInstanceSummary (..),
    newAppInstanceSummary,
    appInstanceSummary_appInstanceArn,
    appInstanceSummary_metadata,
    appInstanceSummary_name,

    -- * AppInstanceUser
    AppInstanceUser (..),
    newAppInstanceUser,
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_metadata,
    appInstanceUser_name,

    -- * AppInstanceUserEndpoint
    AppInstanceUserEndpoint (..),
    newAppInstanceUserEndpoint,
    appInstanceUserEndpoint_allowMessages,
    appInstanceUserEndpoint_appInstanceUserArn,
    appInstanceUserEndpoint_createdTimestamp,
    appInstanceUserEndpoint_endpointAttributes,
    appInstanceUserEndpoint_endpointId,
    appInstanceUserEndpoint_endpointState,
    appInstanceUserEndpoint_lastUpdatedTimestamp,
    appInstanceUserEndpoint_name,
    appInstanceUserEndpoint_resourceArn,
    appInstanceUserEndpoint_type,

    -- * AppInstanceUserEndpointSummary
    AppInstanceUserEndpointSummary (..),
    newAppInstanceUserEndpointSummary,
    appInstanceUserEndpointSummary_allowMessages,
    appInstanceUserEndpointSummary_appInstanceUserArn,
    appInstanceUserEndpointSummary_endpointId,
    appInstanceUserEndpointSummary_endpointState,
    appInstanceUserEndpointSummary_name,
    appInstanceUserEndpointSummary_type,

    -- * AppInstanceUserSummary
    AppInstanceUserSummary (..),
    newAppInstanceUserSummary,
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_name,

    -- * ChannelRetentionSettings
    ChannelRetentionSettings (..),
    newChannelRetentionSettings,
    channelRetentionSettings_retentionDays,

    -- * EndpointAttributes
    EndpointAttributes (..),
    newEndpointAttributes,
    endpointAttributes_voipDeviceToken,
    endpointAttributes_deviceToken,

    -- * EndpointState
    EndpointState (..),
    newEndpointState,
    endpointState_statusReason,
    endpointState_status,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_arn,
    identity_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Amazonka.ChimeSDKIdentity.Types.AllowMessages
import Amazonka.ChimeSDKIdentity.Types.AppInstance
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdmin
import Amazonka.ChimeSDKIdentity.Types.AppInstanceAdminSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.AppInstanceSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUser
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpoint
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointSummary
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointType
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserSummary
import Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
import Amazonka.ChimeSDKIdentity.Types.EndpointAttributes
import Amazonka.ChimeSDKIdentity.Types.EndpointState
import Amazonka.ChimeSDKIdentity.Types.EndpointStatus
import Amazonka.ChimeSDKIdentity.Types.EndpointStatusReason
import Amazonka.ChimeSDKIdentity.Types.Identity
import Amazonka.ChimeSDKIdentity.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-04-20@ of the Amazon Chime SDK Identity SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ChimeSDKIdentity",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "identity-chime",
      Core.signingName = "chime",
      Core.version = "2021-04-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ChimeSDKIdentity",
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

-- | The input parameters don\'t match the service\'s restrictions.
_BadRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The client is permanently forbidden from making the request.
_ForbiddenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The request exceeds the resource limit.
_ResourceLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The service encountered an unexpected error.
_ServiceFailureException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The client exceeded its request rate limit.
_ThrottledClientException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | The client is not currently authorized to make the request.
_UnauthorizedClientException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401
