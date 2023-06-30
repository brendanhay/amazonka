{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Macie.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalException,
    _InvalidInputException,
    _LimitExceededException,

    -- * S3ContinuousClassificationType
    S3ContinuousClassificationType (..),

    -- * S3OneTimeClassificationType
    S3OneTimeClassificationType (..),

    -- * ClassificationType
    ClassificationType (..),
    newClassificationType,
    classificationType_oneTime,
    classificationType_continuous,

    -- * ClassificationTypeUpdate
    ClassificationTypeUpdate (..),
    newClassificationTypeUpdate,
    classificationTypeUpdate_continuous,
    classificationTypeUpdate_oneTime,

    -- * FailedS3Resource
    FailedS3Resource (..),
    newFailedS3Resource,
    failedS3Resource_errorCode,
    failedS3Resource_errorMessage,
    failedS3Resource_failedItem,

    -- * MemberAccount
    MemberAccount (..),
    newMemberAccount,
    memberAccount_accountId,

    -- * S3Resource
    S3Resource (..),
    newS3Resource,
    s3Resource_prefix,
    s3Resource_bucketName,

    -- * S3ResourceClassification
    S3ResourceClassification (..),
    newS3ResourceClassification,
    s3ResourceClassification_prefix,
    s3ResourceClassification_bucketName,
    s3ResourceClassification_classificationType,

    -- * S3ResourceClassificationUpdate
    S3ResourceClassificationUpdate (..),
    newS3ResourceClassificationUpdate,
    s3ResourceClassificationUpdate_prefix,
    s3ResourceClassificationUpdate_bucketName,
    s3ResourceClassificationUpdate_classificationTypeUpdate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Macie.Types.ClassificationType
import Amazonka.Macie.Types.ClassificationTypeUpdate
import Amazonka.Macie.Types.FailedS3Resource
import Amazonka.Macie.Types.MemberAccount
import Amazonka.Macie.Types.S3ContinuousClassificationType
import Amazonka.Macie.Types.S3OneTimeClassificationType
import Amazonka.Macie.Types.S3Resource
import Amazonka.Macie.Types.S3ResourceClassification
import Amazonka.Macie.Types.S3ResourceClassificationUpdate
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-12-19@ of the Amazon Macie SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Macie",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "macie",
      Core.signingName = "macie",
      Core.version = "2017-12-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Macie",
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

-- | (Discontinued) You do not have required permissions to access the
-- requested resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | (Discontinued) Internal server error.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | (Discontinued) The request was rejected because an invalid or
-- out-of-range value was supplied for an input parameter.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | (Discontinued) The request was rejected because it attempted to create
-- resources beyond the current Amazon Web Services account quotas. The
-- error code describes the quota exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
