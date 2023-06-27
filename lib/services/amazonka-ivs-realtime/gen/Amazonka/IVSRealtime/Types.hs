{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVSRealtime.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _PendingVerification,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * EventErrorCode
    EventErrorCode (..),

    -- * EventName
    EventName (..),

    -- * ParticipantState
    ParticipantState (..),

    -- * ParticipantTokenCapability
    ParticipantTokenCapability (..),

    -- * Event
    Event (..),
    newEvent,
    event_errorCode,
    event_eventTime,
    event_name,
    event_participantId,
    event_remoteParticipantId,

    -- * Participant
    Participant (..),
    newParticipant,
    participant_attributes,
    participant_firstJoinTime,
    participant_participantId,
    participant_published,
    participant_state,
    participant_userId,

    -- * ParticipantSummary
    ParticipantSummary (..),
    newParticipantSummary,
    participantSummary_firstJoinTime,
    participantSummary_participantId,
    participantSummary_published,
    participantSummary_state,
    participantSummary_userId,

    -- * ParticipantToken
    ParticipantToken (..),
    newParticipantToken,
    participantToken_attributes,
    participantToken_capabilities,
    participantToken_duration,
    participantToken_expirationTime,
    participantToken_participantId,
    participantToken_token,
    participantToken_userId,

    -- * ParticipantTokenConfiguration
    ParticipantTokenConfiguration (..),
    newParticipantTokenConfiguration,
    participantTokenConfiguration_attributes,
    participantTokenConfiguration_capabilities,
    participantTokenConfiguration_duration,
    participantTokenConfiguration_userId,

    -- * Stage
    Stage (..),
    newStage,
    stage_activeSessionId,
    stage_name,
    stage_tags,
    stage_arn,

    -- * StageSession
    StageSession (..),
    newStageSession,
    stageSession_endTime,
    stageSession_sessionId,
    stageSession_startTime,

    -- * StageSessionSummary
    StageSessionSummary (..),
    newStageSessionSummary,
    stageSessionSummary_endTime,
    stageSessionSummary_sessionId,
    stageSessionSummary_startTime,

    -- * StageSummary
    StageSummary (..),
    newStageSummary,
    stageSummary_activeSessionId,
    stageSummary_name,
    stageSummary_tags,
    stageSummary_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVSRealtime.Types.Event
import Amazonka.IVSRealtime.Types.EventErrorCode
import Amazonka.IVSRealtime.Types.EventName
import Amazonka.IVSRealtime.Types.Participant
import Amazonka.IVSRealtime.Types.ParticipantState
import Amazonka.IVSRealtime.Types.ParticipantSummary
import Amazonka.IVSRealtime.Types.ParticipantToken
import Amazonka.IVSRealtime.Types.ParticipantTokenCapability
import Amazonka.IVSRealtime.Types.ParticipantTokenConfiguration
import Amazonka.IVSRealtime.Types.Stage
import Amazonka.IVSRealtime.Types.StageSession
import Amazonka.IVSRealtime.Types.StageSessionSummary
import Amazonka.IVSRealtime.Types.StageSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-14@ of the Amazon Interactive Video Service RealTime SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IVSRealtime",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ivsrealtime",
      Core.signingName = "ivs",
      Core.version = "2020-07-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IVSRealtime",
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

_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

_PendingVerification :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PendingVerification =
  Core._MatchServiceError
    defaultService
    "PendingVerification"
    Prelude.. Core.hasStatus 403

_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
