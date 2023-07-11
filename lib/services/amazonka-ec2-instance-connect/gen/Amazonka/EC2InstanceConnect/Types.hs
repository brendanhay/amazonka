{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2InstanceConnect.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2InstanceConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AuthException,
    _EC2InstanceNotFoundException,
    _EC2InstanceStateInvalidException,
    _EC2InstanceTypeInvalidException,
    _EC2InstanceUnavailableException,
    _InvalidArgsException,
    _SerialConsoleAccessDisabledException,
    _SerialConsoleSessionLimitExceededException,
    _SerialConsoleSessionUnavailableException,
    _ServiceException,
    _ThrottlingException,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-04-02@ of the Amazon EC2 Instance Connect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "EC2InstanceConnect",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ec2-instance-connect",
      Core.signingName = "ec2-instance-connect",
      Core.version = "2018-04-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "EC2InstanceConnect",
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

-- | Either your AWS credentials are not valid or you do not have access to
-- the EC2 instance.
_AuthException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthException =
  Core._MatchServiceError
    defaultService
    "AuthException"

-- | The specified instance was not found.
_EC2InstanceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2InstanceNotFoundException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceNotFoundException"

-- | Unable to connect because the instance is not in a valid state.
-- Connecting to a stopped or terminated instance is not supported. If the
-- instance is stopped, start your instance, and try to connect again.
_EC2InstanceStateInvalidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2InstanceStateInvalidException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceStateInvalidException"

-- | The instance type is not supported for connecting via the serial
-- console. Only Nitro instance types are currently supported.
_EC2InstanceTypeInvalidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2InstanceTypeInvalidException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceTypeInvalidException"

-- | The instance is currently unavailable. Wait a few minutes and try again.
_EC2InstanceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EC2InstanceUnavailableException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceUnavailableException"

-- | One of the parameters is not valid.
_InvalidArgsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | Your account is not authorized to use the EC2 Serial Console. To
-- authorize your account, run the EnableSerialConsoleAccess API. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EnableSerialConsoleAccess.html EnableSerialConsoleAccess>
-- in the /Amazon EC2 API Reference/.
_SerialConsoleAccessDisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SerialConsoleAccessDisabledException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleAccessDisabledException"

-- | The instance currently has 1 active serial console session. Only 1
-- session is supported at a time.
_SerialConsoleSessionLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SerialConsoleSessionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleSessionLimitExceededException"

-- | Unable to start a serial console session. Please try again.
_SerialConsoleSessionUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SerialConsoleSessionUnavailableException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleSessionUnavailableException"

-- | The service encountered an error. Follow the instructions in the error
-- message and try again.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | The requests were made too frequently and have been throttled. Wait a
-- while and try again. To increase the limit on your request frequency,
-- contact AWS Support.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
