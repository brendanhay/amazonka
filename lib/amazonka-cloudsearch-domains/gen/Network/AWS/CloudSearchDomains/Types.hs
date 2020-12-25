-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _DocumentServiceException,
    _SearchException,

    -- * Expr
    Expr (..),

    -- * FilterQuery
    FilterQuery (..),

    -- * SearchStatus
    SearchStatus (..),
    mkSearchStatus,
    sRid,
    sTimems,

    -- * Cursor
    Cursor (..),

    -- * QueryOptions
    QueryOptions (..),

    -- * Return
    Return (..),

    -- * QueryParser
    QueryParser (..),

    -- * String
    String (..),

    -- * Hit
    Hit (..),
    mkHit,
    hExprs,
    hFields,
    hHighlights,
    hId,

    -- * Highlight
    Highlight (..),

    -- * SuggestStatus
    SuggestStatus (..),
    mkSuggestStatus,
    ssRid,
    ssTimems,

    -- * FieldStats
    FieldStats (..),
    mkFieldStats,
    fsCount,
    fsMax,
    fsMean,
    fsMin,
    fsMissing,
    fsStddev,
    fsSum,
    fsSumOfSquares,

    -- * Bucket
    Bucket (..),
    mkBucket,
    bCount,
    bValue,

    -- * SuggestionMatch
    SuggestionMatch (..),
    mkSuggestionMatch,
    smId,
    smScore,
    smSuggestion,

    -- * Sort
    Sort (..),

    -- * Query
    Query (..),

    -- * Stat
    Stat (..),

    -- * Facet
    Facet (..),

    -- * BucketInfo
    BucketInfo (..),
    mkBucketInfo,
    biBuckets,

    -- * DocumentServiceWarning
    DocumentServiceWarning (..),
    mkDocumentServiceWarning,
    dswMessage,

    -- * Suggester
    Suggester (..),

    -- * SuggestModel
    SuggestModel (..),
    mkSuggestModel,
    smFound,
    smQuery,
    smSuggestions,

    -- * Hits
    Hits (..),
    mkHits,
    hCursor,
    hFound,
    hHit,
    hStart,

    -- * ContentType
    ContentType (..),

    -- * Rid
    Rid (..),
  )
where

import Network.AWS.CloudSearchDomains.Types.Bucket
import Network.AWS.CloudSearchDomains.Types.BucketInfo
import Network.AWS.CloudSearchDomains.Types.ContentType
import Network.AWS.CloudSearchDomains.Types.Cursor
import Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
import Network.AWS.CloudSearchDomains.Types.Expr
import Network.AWS.CloudSearchDomains.Types.Facet
import Network.AWS.CloudSearchDomains.Types.FieldStats
import Network.AWS.CloudSearchDomains.Types.FilterQuery
import Network.AWS.CloudSearchDomains.Types.Highlight
import Network.AWS.CloudSearchDomains.Types.Hit
import Network.AWS.CloudSearchDomains.Types.Hits
import Network.AWS.CloudSearchDomains.Types.Query
import Network.AWS.CloudSearchDomains.Types.QueryOptions
import Network.AWS.CloudSearchDomains.Types.QueryParser
import Network.AWS.CloudSearchDomains.Types.Return
import Network.AWS.CloudSearchDomains.Types.Rid
import Network.AWS.CloudSearchDomains.Types.SearchStatus
import Network.AWS.CloudSearchDomains.Types.Sort
import Network.AWS.CloudSearchDomains.Types.Stat
import Network.AWS.CloudSearchDomains.Types.String
import Network.AWS.CloudSearchDomains.Types.SuggestModel
import Network.AWS.CloudSearchDomains.Types.SuggestStatus
import Network.AWS.CloudSearchDomains.Types.Suggester
import Network.AWS.CloudSearchDomains.Types.SuggestionMatch
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch Domain SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudSearchDomains",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloudsearchdomain",
      Core._svcVersion = "2013-01-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudSearchDomains",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Information about any problems encountered while processing an upload request.
_DocumentServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DocumentServiceException =
  Core._MatchServiceError
    mkServiceConfig
    "DocumentServiceException"
{-# DEPRECATED _DocumentServiceException "Use generic-lens or generic-optics instead." #-}

-- | Information about any problems encountered while processing a search request.
_SearchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SearchException =
  Core._MatchServiceError mkServiceConfig "SearchException"
{-# DEPRECATED _SearchException "Use generic-lens or generic-optics instead." #-}
