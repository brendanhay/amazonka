{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMailMessageFlow.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMailMessageFlow.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidContentLocation,
    _ResourceNotFoundException,
    _MessageFrozen,
    _MessageRejected,

    -- * RawMessageContent
    RawMessageContent (..),
    newRawMessageContent,
    rawMessageContent_s3Reference,

    -- * S3Reference
    S3Reference (..),
    newS3Reference,
    s3Reference_objectVersion,
    s3Reference_bucket,
    s3Reference_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkMailMessageFlow.Types.RawMessageContent
import Amazonka.WorkMailMessageFlow.Types.S3Reference

-- | API version @2019-05-01@ of the Amazon WorkMail Message Flow SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WorkMailMessageFlow",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "workmailmessageflow",
      Core.signingName = "workmailmessageflow",
      Core.version = "2019-05-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "WorkMailMessageFlow",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | WorkMail could not access the updated email content. Possible reasons:
--
-- -   You made the request in a region other than your S3 bucket region.
--
-- -   The
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-owner-condition.html S3 bucket owner>
--     is not the same as the calling AWS account.
--
-- -   You have an incomplete or missing S3 bucket policy. For more
--     information about policies, see
--     <https://docs.aws.amazon.com/workmail/latest/adminguide/update-with-lambda.html Updating message content with AWS Lambda>
--     in the /WorkMail Administrator Guide/.
_InvalidContentLocation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidContentLocation =
  Core._MatchServiceError
    defaultService
    "InvalidContentLocation"

-- | The requested email message is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The requested email is not eligible for update. This is usually the case
-- for a redirected email.
_MessageFrozen :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageFrozen =
  Core._MatchServiceError
    defaultService
    "MessageFrozen"

-- | The requested email could not be updated due to an error in the MIME
-- content. Check the error message for more information about what caused
-- the error.
_MessageRejected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageRejected =
  Core._MatchServiceError
    defaultService
    "MessageRejected"
