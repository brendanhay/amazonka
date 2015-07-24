{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types
    (
    -- * Service
      CloudSearchDomains

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
    , ssRid
    , ssTimems

    -- * SuggestModel
    , SuggestModel
    , suggestModel
    , smFound
    , smSuggestions
    , smQuery

    -- * SuggestStatus
    , SuggestStatus
    , suggestStatus
    , sRid
    , sTimems

    -- * SuggestionMatch
    , SuggestionMatch
    , suggestionMatch
    , smSuggestion
    , smScore
    , smId
    ) where

import           Network.AWS.CloudSearchDomains.Types.Product
import           Network.AWS.CloudSearchDomains.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-01-01@ of the Amazon CloudSearch Domain SDK.
data CloudSearchDomains

instance AWSService CloudSearchDomains where
    type Sg CloudSearchDomains = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudSearchDomains"
            , _svcPrefix = "cloudsearchdomain"
            , _svcVersion = "2013-01-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Information about any problems encountered while processing an upload
-- request.
_DocumentServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_DocumentServiceException = _ServiceError . hasCode "DocumentServiceException"

-- | Information about any problems encountered while processing a search
-- request.
_SearchException :: AWSError a => Getting (First ServiceError) a ServiceError
_SearchException = _ServiceError . hasCode "SearchException"
