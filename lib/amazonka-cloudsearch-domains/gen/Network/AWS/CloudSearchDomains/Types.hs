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
    cloudSearchDomainsService,

    -- * Errors

    -- * ContentType
    ContentType (..),

    -- * QueryParser
    QueryParser (..),

    -- * Bucket
    Bucket (..),
    mkBucket,
    bValue,
    bCount,

    -- * BucketInfo
    BucketInfo (..),
    mkBucketInfo,
    biBuckets,

    -- * DocumentServiceWarning
    DocumentServiceWarning (..),
    mkDocumentServiceWarning,
    dswMessage,

    -- * FieldStats
    FieldStats (..),
    mkFieldStats,
    fsMax,
    fsMean,
    fsCount,
    fsMissing,
    fsStddev,
    fsMin,
    fsSumOfSquares,
    fsSum,

    -- * Hit
    Hit (..),
    mkHit,
    hExprs,
    hId,
    hHighlights,
    hFields,

    -- * Hits
    Hits (..),
    mkHits,
    hCursor,
    hHit,
    hStart,
    hFound,

    -- * SearchStatus
    SearchStatus (..),
    mkSearchStatus,
    sRid,
    sTimems,

    -- * SuggestModel
    SuggestModel (..),
    mkSuggestModel,
    smFound,
    smSuggestions,
    smQuery,

    -- * SuggestStatus
    SuggestStatus (..),
    mkSuggestStatus,
    ssRid,
    ssTimems,

    -- * SuggestionMatch
    SuggestionMatch (..),
    mkSuggestionMatch,
    smSuggestion,
    smScore,
    smId,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-01-01@ of the Amazon CloudSearch Domain SDK configuration.
cloudSearchDomainsService :: Lude.Service
cloudSearchDomainsService =
  Lude.Service
    { Lude._svcAbbrev = "CloudSearchDomains",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudsearchdomain",
      Lude._svcVersion = "2013-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudSearchDomainsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudSearchDomains",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
