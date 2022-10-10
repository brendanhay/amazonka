{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectContactLens.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _InternalServiceException,
    _InvalidRequestException,

    -- * SentimentValue
    SentimentValue (..),

    -- * Categories
    Categories (..),
    newCategories,
    categories_matchedCategories,
    categories_matchedDetails,

    -- * CategoryDetails
    CategoryDetails (..),
    newCategoryDetails,
    categoryDetails_pointsOfInterest,

    -- * CharacterOffsets
    CharacterOffsets (..),
    newCharacterOffsets,
    characterOffsets_beginOffsetChar,
    characterOffsets_endOffsetChar,

    -- * IssueDetected
    IssueDetected (..),
    newIssueDetected,
    issueDetected_characterOffsets,

    -- * PointOfInterest
    PointOfInterest (..),
    newPointOfInterest,
    pointOfInterest_beginOffsetMillis,
    pointOfInterest_endOffsetMillis,

    -- * RealtimeContactAnalysisSegment
    RealtimeContactAnalysisSegment (..),
    newRealtimeContactAnalysisSegment,
    realtimeContactAnalysisSegment_transcript,
    realtimeContactAnalysisSegment_categories,

    -- * Transcript
    Transcript (..),
    newTranscript,
    transcript_issuesDetected,
    transcript_id,
    transcript_participantId,
    transcript_participantRole,
    transcript_content,
    transcript_beginOffsetMillis,
    transcript_endOffsetMillis,
    transcript_sentiment,
  )
where

import Amazonka.ConnectContactLens.Types.Categories
import Amazonka.ConnectContactLens.Types.CategoryDetails
import Amazonka.ConnectContactLens.Types.CharacterOffsets
import Amazonka.ConnectContactLens.Types.IssueDetected
import Amazonka.ConnectContactLens.Types.PointOfInterest
import Amazonka.ConnectContactLens.Types.RealtimeContactAnalysisSegment
import Amazonka.ConnectContactLens.Types.SentimentValue
import Amazonka.ConnectContactLens.Types.Transcript
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-21@ of the Amazon Connect Contact Lens SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ConnectContactLens",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "contact-lens",
      Core._serviceSigningName = "connect",
      Core._serviceVersion = "2020-08-21",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ConnectContactLens",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Request processing failed due to an error or failure with the service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400
