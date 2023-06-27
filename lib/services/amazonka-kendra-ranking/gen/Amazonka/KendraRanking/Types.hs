{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KendraRanking.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * RescoreExecutionPlanStatus
    RescoreExecutionPlanStatus (..),

    -- * CapacityUnitsConfiguration
    CapacityUnitsConfiguration (..),
    newCapacityUnitsConfiguration,
    capacityUnitsConfiguration_rescoreCapacityUnits,

    -- * Document
    Document (..),
    newDocument,
    document_body,
    document_groupId,
    document_title,
    document_tokenizedBody,
    document_tokenizedTitle,
    document_id,
    document_originalScore,

    -- * RescoreExecutionPlanSummary
    RescoreExecutionPlanSummary (..),
    newRescoreExecutionPlanSummary,
    rescoreExecutionPlanSummary_createdAt,
    rescoreExecutionPlanSummary_id,
    rescoreExecutionPlanSummary_name,
    rescoreExecutionPlanSummary_status,
    rescoreExecutionPlanSummary_updatedAt,

    -- * RescoreResultItem
    RescoreResultItem (..),
    newRescoreResultItem,
    rescoreResultItem_documentId,
    rescoreResultItem_score,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KendraRanking.Types.CapacityUnitsConfiguration
import Amazonka.KendraRanking.Types.Document
import Amazonka.KendraRanking.Types.RescoreExecutionPlanStatus
import Amazonka.KendraRanking.Types.RescoreExecutionPlanSummary
import Amazonka.KendraRanking.Types.RescoreResultItem
import Amazonka.KendraRanking.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-10-19@ of the Amazon Kendra Intelligent Ranking SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "KendraRanking",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kendra-ranking",
      Core.signingName = "kendra-ranking",
      Core.version = "2022-10-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "KendraRanking",
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

-- | You don’t have sufficient access to perform this action. Please ensure
-- you have the required permission policies and user accounts and try
-- again.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | A conflict occurred with the request. Please fix any inconsistencies
-- with your resources and try again.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An issue occurred with the internal server used for your Amazon Kendra
-- Intelligent Ranking service. Please wait a few minutes and try again, or
-- contact <http://aws.amazon.com/contact-us/ Support> for help.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The resource you want to use doesn\'t exist. Please check you have
-- provided the correct resource and try again.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The resource you want to use is unavailable. Please check you have
-- provided the correct resource information and try again.
_ResourceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | You have exceeded the set limits for your Amazon Kendra Intelligent
-- Ranking service. Please see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas> for
-- more information, or contact <http://aws.amazon.com/contact-us/ Support>
-- to inquire about an increase of limits.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling. Please reduce the
-- number of requests and try again.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The input fails to satisfy the constraints set by the Amazon Kendra
-- Intelligent Ranking service. Please provide the correct input and try
-- again.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
