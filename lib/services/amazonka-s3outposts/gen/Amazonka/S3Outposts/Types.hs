{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3Outposts.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _OutpostOfflineException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * EndpointAccessType
    EndpointAccessType (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_accessType,
    endpoint_cidrBlock,
    endpoint_creationTime,
    endpoint_customerOwnedIpv4Pool,
    endpoint_endpointArn,
    endpoint_failedReason,
    endpoint_networkInterfaces,
    endpoint_outpostsId,
    endpoint_securityGroupId,
    endpoint_status,
    endpoint_subnetId,
    endpoint_vpcId,

    -- * FailedReason
    FailedReason (..),
    newFailedReason,
    failedReason_errorCode,
    failedReason_message,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_networkInterfaceId,

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_capacityInBytes,
    outpost_outpostArn,
    outpost_outpostId,
    outpost_ownerId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3Outposts.Types.Endpoint
import Amazonka.S3Outposts.Types.EndpointAccessType
import Amazonka.S3Outposts.Types.EndpointStatus
import Amazonka.S3Outposts.Types.FailedReason
import Amazonka.S3Outposts.Types.NetworkInterface
import Amazonka.S3Outposts.Types.Outpost
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon S3 on Outposts SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "S3Outposts",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "s3-outposts",
      Core.signingName = "s3-outposts",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "S3Outposts",
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

-- | Access was denied for this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict with this action, and it could not be completed.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There was an exception with the internal server.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The service link connection to your Outposts home Region is down. Check
-- your connection and try again.
_OutpostOfflineException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OutpostOfflineException =
  Core._MatchServiceError
    defaultService
    "OutpostOfflineException"
    Prelude.. Core.hasStatus 400

-- | The requested resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | There was an exception validating this data.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
