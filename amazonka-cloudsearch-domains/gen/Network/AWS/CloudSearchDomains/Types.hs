{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _SearchException,
    _DocumentServiceException,

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
    fieldStats_mean,
    fieldStats_missing,
    fieldStats_sum,
    fieldStats_min,
    fieldStats_max,
    fieldStats_stddev,
    fieldStats_count,
    fieldStats_sumOfSquares,

    -- * Hit
    Hit (..),
    newHit,
    hit_id,
    hit_exprs,
    hit_fields,
    hit_highlights,

    -- * Hits
    Hits (..),
    newHits,
    hits_found,
    hits_hit,
    hits_cursor,
    hits_start,

    -- * SearchStatus
    SearchStatus (..),
    newSearchStatus,
    searchStatus_timems,
    searchStatus_rid,

    -- * SuggestModel
    SuggestModel (..),
    newSuggestModel,
    suggestModel_suggestions,
    suggestModel_found,
    suggestModel_query,

    -- * SuggestStatus
    SuggestStatus (..),
    newSuggestStatus,
    suggestStatus_timems,
    suggestStatus_rid,

    -- * SuggestionMatch
    SuggestionMatch (..),
    newSuggestionMatch,
    suggestionMatch_suggestion,
    suggestionMatch_id,
    suggestionMatch_score,
  )
where

import Network.AWS.CloudSearchDomains.Types.Bucket
import Network.AWS.CloudSearchDomains.Types.BucketInfo
import Network.AWS.CloudSearchDomains.Types.ContentType
import Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
import Network.AWS.CloudSearchDomains.Types.FieldStats
import Network.AWS.CloudSearchDomains.Types.Hit
import Network.AWS.CloudSearchDomains.Types.Hits
import Network.AWS.CloudSearchDomains.Types.QueryParser
import Network.AWS.CloudSearchDomains.Types.SearchStatus
import Network.AWS.CloudSearchDomains.Types.SuggestModel
import Network.AWS.CloudSearchDomains.Types.SuggestStatus
import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

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
      Core._serviceTimeout = Core.Just 70,
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

-- | Information about any problems encountered while processing a search
-- request.
_SearchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SearchException =
  Core._MatchServiceError
    defaultService
    "SearchException"

-- | Information about any problems encountered while processing an upload
-- request.
_DocumentServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentServiceException =
  Core._MatchServiceError
    defaultService
    "DocumentServiceException"
