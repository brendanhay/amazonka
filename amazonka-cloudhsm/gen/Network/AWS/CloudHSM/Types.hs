{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CloudHsmInternalException,
    _InvalidRequestException,
    _CloudHsmServiceException,

    -- * ClientVersion
    ClientVersion (..),

    -- * CloudHsmObjectState
    CloudHsmObjectState (..),

    -- * HsmStatus
    HsmStatus (..),

    -- * SubscriptionType
    SubscriptionType (..),

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudHSM.Types.ClientVersion
import Network.AWS.CloudHSM.Types.CloudHsmObjectState
import Network.AWS.CloudHSM.Types.HsmStatus
import Network.AWS.CloudHSM.Types.SubscriptionType
import Network.AWS.CloudHSM.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-05-30@ of the Amazon CloudHSM SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudHSM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudhsm",
      Core._serviceSigningName = "cloudhsm",
      Core._serviceVersion = "2014-05-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "CloudHSM",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Indicates that an internal error occurred.
_CloudHsmInternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInternalException"

-- | Indicates that one or more of the request parameters are not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Indicates that an exception occurred in the AWS CloudHSM service.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    defaultService
    "CloudHsmServiceException"
