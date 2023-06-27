{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT1ClickDevices.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickDevices.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ForbiddenException,
    _InternalFailureException,
    _InvalidRequestException,
    _PreconditionFailedException,
    _RangeNotSatisfiableException,
    _ResourceConflictException,
    _ResourceNotFoundException,

    -- * Attributes
    Attributes (..),
    newAttributes,

    -- * Device
    Device (..),
    newDevice,
    device_attributes,
    device_deviceId,
    device_type,

    -- * DeviceDescription
    DeviceDescription (..),
    newDeviceDescription,
    deviceDescription_arn,
    deviceDescription_attributes,
    deviceDescription_deviceId,
    deviceDescription_enabled,
    deviceDescription_remainingLife,
    deviceDescription_tags,
    deviceDescription_type,

    -- * DeviceEvent
    DeviceEvent (..),
    newDeviceEvent,
    deviceEvent_device,
    deviceEvent_stdEvent,

    -- * DeviceMethod
    DeviceMethod (..),
    newDeviceMethod,
    deviceMethod_deviceType,
    deviceMethod_methodName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT1ClickDevices.Types.Attributes
import Amazonka.IoT1ClickDevices.Types.Device
import Amazonka.IoT1ClickDevices.Types.DeviceDescription
import Amazonka.IoT1ClickDevices.Types.DeviceEvent
import Amazonka.IoT1ClickDevices.Types.DeviceMethod
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-14@ of the Amazon IoT 1-Click Devices Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoT1ClickDevices",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "devices.iot1click",
      Core.signingName = "iot1click",
      Core.version = "2018-05-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoT1ClickDevices",
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

-- | Prism for ForbiddenException' errors.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Prism for InternalFailureException' errors.
_InternalFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | Prism for InvalidRequestException' errors.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | Prism for PreconditionFailedException' errors.
_PreconditionFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | Prism for RangeNotSatisfiableException' errors.
_RangeNotSatisfiableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RangeNotSatisfiableException =
  Core._MatchServiceError
    defaultService
    "RangeNotSatisfiableException"
    Prelude.. Core.hasStatus 416

-- | Prism for ResourceConflictException' errors.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | Prism for ResourceNotFoundException' errors.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
