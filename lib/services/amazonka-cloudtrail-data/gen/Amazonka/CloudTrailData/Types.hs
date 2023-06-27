{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrailData.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrailData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ChannelInsufficientPermission,
    _ChannelNotFound,
    _ChannelUnsupportedSchema,
    _DuplicatedAuditEventId,
    _InvalidChannelARN,
    _UnsupportedOperationException,

    -- * AuditEvent
    AuditEvent (..),
    newAuditEvent,
    auditEvent_eventDataChecksum,
    auditEvent_eventData,
    auditEvent_id,

    -- * AuditEventResultEntry
    AuditEventResultEntry (..),
    newAuditEventResultEntry,
    auditEventResultEntry_eventID,
    auditEventResultEntry_id,

    -- * ResultErrorEntry
    ResultErrorEntry (..),
    newResultErrorEntry,
    resultErrorEntry_errorCode,
    resultErrorEntry_errorMessage,
    resultErrorEntry_id,
  )
where

import Amazonka.CloudTrailData.Types.AuditEvent
import Amazonka.CloudTrailData.Types.AuditEventResultEntry
import Amazonka.CloudTrailData.Types.ResultErrorEntry
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-08-11@ of the Amazon CloudTrail Data Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudTrailData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudtrail-data",
      Core.signingName = "cloudtrail-data",
      Core.version = "2021-08-11",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudTrailData",
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

-- | The caller\'s account ID must be the same as the channel owner\'s
-- account ID.
_ChannelInsufficientPermission :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelInsufficientPermission =
  Core._MatchServiceError
    defaultService
    "ChannelInsufficientPermission"

-- | The channel could not be found.
_ChannelNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelNotFound =
  Core._MatchServiceError
    defaultService
    "ChannelNotFound"

-- | The schema type of the event is not supported.
_ChannelUnsupportedSchema :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelUnsupportedSchema =
  Core._MatchServiceError
    defaultService
    "ChannelUnsupportedSchema"

-- | Two or more entries in the request have the same event ID.
_DuplicatedAuditEventId :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicatedAuditEventId =
  Core._MatchServiceError
    defaultService
    "DuplicatedAuditEventId"

-- | The specified channel ARN is not a valid channel ARN.
_InvalidChannelARN :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidChannelARN =
  Core._MatchServiceError
    defaultService
    "InvalidChannelARN"

-- | The operation requested is not supported in this region or account.
_UnsupportedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
