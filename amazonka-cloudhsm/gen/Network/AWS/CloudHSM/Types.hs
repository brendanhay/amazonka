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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-05-30@ of the Amazon CloudHSM SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CloudHSM",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cloudhsm",
      Prelude._svcVersion = "2014-05-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CloudHSM",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that an internal error occurred.
_CloudHsmInternalException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmInternalException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmInternalException"

-- | Indicates that one or more of the request parameters are not valid.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Indicates that an exception occurred in the AWS CloudHSM service.
_CloudHsmServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudHsmServiceException =
  Prelude._MatchServiceError
    defaultService
    "CloudHsmServiceException"
