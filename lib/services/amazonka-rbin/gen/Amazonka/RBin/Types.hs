{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RBin.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * LockState
    LockState (..),

    -- * ResourceType
    ResourceType (..),

    -- * RetentionPeriodUnit
    RetentionPeriodUnit (..),

    -- * RuleStatus
    RuleStatus (..),

    -- * UnlockDelayUnit
    UnlockDelayUnit (..),

    -- * LockConfiguration
    LockConfiguration (..),
    newLockConfiguration,
    lockConfiguration_unlockDelay,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_resourceTagValue,
    resourceTag_resourceTagKey,

    -- * RetentionPeriod
    RetentionPeriod (..),
    newRetentionPeriod,
    retentionPeriod_retentionPeriodValue,
    retentionPeriod_retentionPeriodUnit,

    -- * RuleSummary
    RuleSummary (..),
    newRuleSummary,
    ruleSummary_description,
    ruleSummary_identifier,
    ruleSummary_lockState,
    ruleSummary_retentionPeriod,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UnlockDelay
    UnlockDelay (..),
    newUnlockDelay,
    unlockDelay_unlockDelayValue,
    unlockDelay_unlockDelayUnit,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.LockConfiguration
import Amazonka.RBin.Types.LockState
import Amazonka.RBin.Types.ResourceTag
import Amazonka.RBin.Types.ResourceType
import Amazonka.RBin.Types.RetentionPeriod
import Amazonka.RBin.Types.RetentionPeriodUnit
import Amazonka.RBin.Types.RuleStatus
import Amazonka.RBin.Types.RuleSummary
import Amazonka.RBin.Types.Tag
import Amazonka.RBin.Types.UnlockDelay
import Amazonka.RBin.Types.UnlockDelayUnit
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-06-15@ of the Amazon Recycle Bin SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RBin",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rbin",
      Core.signingName = "rbin",
      Core.version = "2021-06-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RBin",
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

-- | The specified retention rule lock request can\'t be completed.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The service could not respond to the request due to an internal problem.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service quota for the number of tags per
-- resource to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | One or more of the parameters in the request is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
