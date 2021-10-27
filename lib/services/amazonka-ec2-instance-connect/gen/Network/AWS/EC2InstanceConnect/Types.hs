{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2InstanceConnect.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2InstanceConnect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _SerialConsoleSessionUnavailableException,
    _AuthException,
    _SerialConsoleSessionLimitExceededException,
    _InvalidArgsException,
    _SerialConsoleAccessDisabledException,
    _ThrottlingException,
    _ServiceException,
    _EC2InstanceTypeInvalidException,
    _EC2InstanceNotFoundException,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-04-02@ of the Amazon EC2 Instance Connect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "EC2InstanceConnect",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ec2-instance-connect",
      Core._serviceSigningName = "ec2-instance-connect",
      Core._serviceVersion = "2018-04-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "EC2InstanceConnect",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Unable to start a serial console session. Please try again.
_SerialConsoleSessionUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SerialConsoleSessionUnavailableException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleSessionUnavailableException"

-- | Either your AWS credentials are not valid or you do not have access to
-- the EC2 instance.
_AuthException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthException =
  Core._MatchServiceError
    defaultService
    "AuthException"

-- | The instance currently has 1 active serial console session. Only 1
-- session is supported at a time.
_SerialConsoleSessionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SerialConsoleSessionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleSessionLimitExceededException"

-- | One of the parameters is not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | Your account is not authorized to use the EC2 Serial Console. To
-- authorize your account, run the EnableSerialConsoleAccess API. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EnableSerialConsoleAccess.html EnableSerialConsoleAccess>
-- in the /Amazon EC2 API Reference/.
_SerialConsoleAccessDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SerialConsoleAccessDisabledException =
  Core._MatchServiceError
    defaultService
    "SerialConsoleAccessDisabledException"

-- | The requests were made too frequently and have been throttled. Wait a
-- while and try again. To increase the limit on your request frequency,
-- contact AWS Support.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The service encountered an error. Follow the instructions in the error
-- message and try again.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | The instance type is not supported for connecting via the serial
-- console. Only Nitro instance types are currently supported.
_EC2InstanceTypeInvalidException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2InstanceTypeInvalidException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceTypeInvalidException"

-- | The specified instance was not found.
_EC2InstanceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EC2InstanceNotFoundException =
  Core._MatchServiceError
    defaultService
    "EC2InstanceNotFoundException"
