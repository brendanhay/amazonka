{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types
    (
    -- * Service Configuration
      cloudSearchDomains

    -- * Errors
    , _DocumentServiceException
    , _SearchException

    -- * ContentType
    , ContentType (..)

    -- * QueryParser
    , QueryParser (..)

    -- * Bucket
    , Bucket
    , bucket
    , bValue
    , bCount

    -- * BucketInfo
    , BucketInfo
    , bucketInfo
    , biBuckets

    -- * DocumentServiceWarning
    , DocumentServiceWarning
    , documentServiceWarning
    , dswMessage

    -- * FieldStats
    , FieldStats
    , fieldStats
    , fsMax
    , fsMean
    , fsCount
    , fsMissing
    , fsStddev
    , fsMin
    , fsSumOfSquares
    , fsSum

    -- * Hit
    , Hit
    , hit
    , hitExprs
    , hitId
    , hitHighlights
    , hitFields

    -- * Hits
    , Hits
    , hits
    , hCursor
    , hHit
    , hStart
    , hFound

    -- * SearchStatus
    , SearchStatus
    , searchStatus
    , sRid
    , sTimems

    -- * SuggestModel
    , SuggestModel
    , suggestModel
    , smFound
    , smSuggestions
    , smQuery

    -- * SuggestStatus
    , SuggestStatus
    , suggestStatus
    , ssRid
    , ssTimems

    -- * SuggestionMatch
    , SuggestionMatch
    , suggestionMatch
    , smSuggestion
    , smScore
    , smId
    ) where

import Network.AWS.CloudSearchDomains.Types.Product
import Network.AWS.CloudSearchDomains.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-01-01@ of the Amazon CloudSearch Domain SDK configuration.
cloudSearchDomains :: Service
cloudSearchDomains =
  Service
    { _svcAbbrev = "CloudSearchDomains"
    , _svcSigner = v4
    , _svcPrefix = "cloudsearchdomain"
    , _svcVersion = "2013-01-01"
    , _svcEndpoint = defaultEndpoint cloudSearchDomains
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CloudSearchDomains"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Information about any problems encountered while processing an upload request.
--
--
_DocumentServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentServiceException =
  _MatchServiceError cloudSearchDomains "DocumentServiceException"


-- | Information about any problems encountered while processing a search request.
--
--
_SearchException :: AsError a => Getting (First ServiceError) a ServiceError
_SearchException = _MatchServiceError cloudSearchDomains "SearchException"

