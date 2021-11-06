{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DocumentServiceException,
    _SearchException,

    -- * ContentType
    ContentType (..),

    -- * QueryParser
    QueryParser (..),

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_value,
    bucket_count,

    -- * BucketInfo
    BucketInfo (..),
    newBucketInfo,
    bucketInfo_buckets,

    -- * DocumentServiceWarning
    DocumentServiceWarning (..),
    newDocumentServiceWarning,
    documentServiceWarning_message,

    -- * FieldStats
    FieldStats (..),
    newFieldStats,
    fieldStats_max,
    fieldStats_mean,
    fieldStats_count,
    fieldStats_missing,
    fieldStats_stddev,
    fieldStats_min,
    fieldStats_sumOfSquares,
    fieldStats_sum,

    -- * Hit
    Hit (..),
    newHit,
    hit_exprs,
    hit_id,
    hit_highlights,
    hit_fields,

    -- * Hits
    Hits (..),
    newHits,
    hits_cursor,
    hits_hit,
    hits_start,
    hits_found,

    -- * SearchStatus
    SearchStatus (..),
    newSearchStatus,
    searchStatus_rid,
    searchStatus_timems,

    -- * SuggestModel
    SuggestModel (..),
    newSuggestModel,
    suggestModel_found,
    suggestModel_suggestions,
    suggestModel_query,

    -- * SuggestStatus
    SuggestStatus (..),
    newSuggestStatus,
    suggestStatus_rid,
    suggestStatus_timems,

    -- * SuggestionMatch
    SuggestionMatch (..),
    newSuggestionMatch,
    suggestionMatch_suggestion,
    suggestionMatch_score,
    suggestionMatch_id,
  )
where

import Amazonka.CloudSearchDomains.Types.Bucket
import Amazonka.CloudSearchDomains.Types.BucketInfo
import Amazonka.CloudSearchDomains.Types.ContentType
import Amazonka.CloudSearchDomains.Types.DocumentServiceWarning
import Amazonka.CloudSearchDomains.Types.FieldStats
import Amazonka.CloudSearchDomains.Types.Hit
import Amazonka.CloudSearchDomains.Types.Hits
import Amazonka.CloudSearchDomains.Types.QueryParser
import Amazonka.CloudSearchDomains.Types.SearchStatus
import Amazonka.CloudSearchDomains.Types.SuggestModel
import Amazonka.CloudSearchDomains.Types.SuggestStatus
import Amazonka.CloudSearchDomains.Types.SuggestionMatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch Domain SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CloudSearchDomains",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudsearchdomain",
      Core._serviceSigningName = "cloudsearch",
      Core._serviceVersion = "2013-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudSearchDomains",
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

-- | Information about any problems encountered while processing an upload
-- request.
_DocumentServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentServiceException =
  Core._MatchServiceError
    defaultService
    "DocumentServiceException"

-- | Information about any problems encountered while processing a search
-- request.
_SearchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SearchException =
  Core._MatchServiceError
    defaultService
    "SearchException"
