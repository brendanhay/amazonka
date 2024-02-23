{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTRoboRunner.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types
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

    -- * DestinationState
    DestinationState (..),

    -- * CartesianCoordinates
    CartesianCoordinates (..),
    newCartesianCoordinates,
    cartesianCoordinates_z,
    cartesianCoordinates_x,
    cartesianCoordinates_y,

    -- * Destination
    Destination (..),
    newDestination,
    destination_additionalFixedProperties,
    destination_arn,
    destination_id,
    destination_name,
    destination_site,
    destination_createdAt,
    destination_updatedAt,
    destination_state,

    -- * Orientation
    Orientation (..),
    newOrientation,
    orientation_degrees,

    -- * PositionCoordinates
    PositionCoordinates (..),
    newPositionCoordinates,
    positionCoordinates_cartesianCoordinates,

    -- * Site
    Site (..),
    newSite,
    site_arn,
    site_name,
    site_countryCode,
    site_createdAt,

    -- * VendorProperties
    VendorProperties (..),
    newVendorProperties,
    vendorProperties_vendorAdditionalFixedProperties,
    vendorProperties_vendorAdditionalTransientProperties,
    vendorProperties_vendorWorkerIpAddress,
    vendorProperties_vendorWorkerId,

    -- * Worker
    Worker (..),
    newWorker,
    worker_additionalFixedProperties,
    worker_additionalTransientProperties,
    worker_orientation,
    worker_position,
    worker_vendorProperties,
    worker_arn,
    worker_id,
    worker_fleet,
    worker_createdAt,
    worker_updatedAt,
    worker_name,
    worker_site,

    -- * WorkerFleet
    WorkerFleet (..),
    newWorkerFleet,
    workerFleet_additionalFixedProperties,
    workerFleet_arn,
    workerFleet_id,
    workerFleet_name,
    workerFleet_site,
    workerFleet_createdAt,
    workerFleet_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types.CartesianCoordinates
import Amazonka.IoTRoboRunner.Types.Destination
import Amazonka.IoTRoboRunner.Types.DestinationState
import Amazonka.IoTRoboRunner.Types.Orientation
import Amazonka.IoTRoboRunner.Types.PositionCoordinates
import Amazonka.IoTRoboRunner.Types.Site
import Amazonka.IoTRoboRunner.Types.VendorProperties
import Amazonka.IoTRoboRunner.Types.Worker
import Amazonka.IoTRoboRunner.Types.WorkerFleet
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon IoT RoboRunner SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTRoboRunner",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "iotroborunner",
      Core.signingName = "iotroborunner",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTRoboRunner",
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

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Exception thrown if a resource in a create request already exists.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Exception thrown if something goes wrong within the service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Exception thrown if a resource referenced in the request doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Exception thrown if the user\'s AWS account has reached a service limit
-- and the operation cannot proceed.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Exception thrown if the api has been called too quickly be the client.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Exception thrown if an invalid parameter is provided to an API.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
