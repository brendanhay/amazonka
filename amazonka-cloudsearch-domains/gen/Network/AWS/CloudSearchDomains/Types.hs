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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch Domain SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CloudSearchDomains",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "cloudsearchdomain",
      Prelude._svcSigningName = "cloudsearch",
      Prelude._svcVersion = "2013-01-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CloudSearchDomains",
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

-- | Information about any problems encountered while processing a search
-- request.
_SearchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SearchException =
  Prelude._MatchServiceError
    defaultService
    "SearchException"

-- | Information about any problems encountered while processing an upload
-- request.
_DocumentServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DocumentServiceException =
  Prelude._MatchServiceError
    defaultService
    "DocumentServiceException"
