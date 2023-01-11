{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ArcZonalShift.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * AppliedStatus
    AppliedStatus (..),

    -- * ZonalShiftStatus
    ZonalShiftStatus (..),

    -- * ManagedResourceSummary
    ManagedResourceSummary (..),
    newManagedResourceSummary,
    managedResourceSummary_arn,
    managedResourceSummary_name,
    managedResourceSummary_availabilityZones,

    -- * ZonalShift
    ZonalShift (..),
    newZonalShift,
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,

    -- * ZonalShiftInResource
    ZonalShiftInResource (..),
    newZonalShiftInResource,
    zonalShiftInResource_appliedStatus,
    zonalShiftInResource_awayFrom,
    zonalShiftInResource_comment,
    zonalShiftInResource_expiryTime,
    zonalShiftInResource_resourceIdentifier,
    zonalShiftInResource_startTime,
    zonalShiftInResource_zonalShiftId,

    -- * ZonalShiftSummary
    ZonalShiftSummary (..),
    newZonalShiftSummary,
    zonalShiftSummary_awayFrom,
    zonalShiftSummary_comment,
    zonalShiftSummary_expiryTime,
    zonalShiftSummary_resourceIdentifier,
    zonalShiftSummary_startTime,
    zonalShiftSummary_status,
    zonalShiftSummary_zonalShiftId,
  )
where

import Amazonka.ArcZonalShift.Types.AppliedStatus
import Amazonka.ArcZonalShift.Types.ManagedResourceSummary
import Amazonka.ArcZonalShift.Types.ZonalShift
import Amazonka.ArcZonalShift.Types.ZonalShiftInResource
import Amazonka.ArcZonalShift.Types.ZonalShiftStatus
import Amazonka.ArcZonalShift.Types.ZonalShiftSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-10-30@ of the Amazon ARC - Zonal Shift SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ArcZonalShift",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "arc-zonal-shift",
      Core.signingName = "arc-zonal-shift",
      Core.version = "2022-10-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ArcZonalShift",
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
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There was an internal server error.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The input requested a resource that was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
