{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types
    (
    -- * Service Configuration
      awsHealth

    -- * Errors
    , _InvalidPaginationToken
    , _UnsupportedLocale

    -- * EntityStatusCode
    , EntityStatusCode (..)

    -- * EventAggregateField
    , EventAggregateField (..)

    -- * EventStatusCode
    , EventStatusCode (..)

    -- * EventTypeCategory
    , EventTypeCategory (..)

    -- * AffectedEntity
    , AffectedEntity
    , affectedEntity
    , aeLastUpdatedTime
    , aeEntityValue
    , aeAwsAccountId
    , aeEventARN
    , aeEntityARN
    , aeTags
    , aeStatusCode

    -- * DateTimeRange
    , DateTimeRange
    , dateTimeRange
    , dtrTo
    , dtrFrom

    -- * EntityAggregate
    , EntityAggregate
    , entityAggregate
    , eCount
    , eEventARN

    -- * EntityFilter
    , EntityFilter
    , entityFilter
    , eStatusCodes
    , eEntityARNs
    , eEntityValues
    , eTags
    , eLastUpdatedTimes
    , eEventARNs

    -- * Event
    , Event
    , event
    , eLastUpdatedTime
    , eArn
    , eService
    , eStartTime
    , eEventTypeCode
    , eEventTypeCategory
    , eAvailabilityZone
    , eEndTime
    , eRegion
    , eStatusCode

    -- * EventAggregate
    , EventAggregate
    , eventAggregate
    , eaCount
    , eaAggregateValue

    -- * EventDescription
    , EventDescription
    , eventDescription
    , edLatestDescription

    -- * EventDetails
    , EventDetails
    , eventDetails
    , edEvent
    , edEventDescription
    , edEventMetadata

    -- * EventDetailsErrorItem
    , EventDetailsErrorItem
    , eventDetailsErrorItem
    , edeiEventARN
    , edeiErrorName
    , edeiErrorMessage

    -- * EventFilter
    , EventFilter
    , eventFilter
    , efEventARNs
    , efEventTypeCategories
    , efEventTypeCodes
    , efRegions
    , efEventStatusCodes
    , efEndTimes
    , efAvailabilityZones
    , efEntityARNs
    , efEntityValues
    , efStartTimes
    , efServices
    , efTags
    , efLastUpdatedTimes

    -- * EventType
    , EventType
    , eventType
    , etService
    , etCategory
    , etCode

    -- * EventTypeFilter
    , EventTypeFilter
    , eventTypeFilter
    , etfEventTypeCategories
    , etfEventTypeCodes
    , etfServices
    ) where

import Network.AWS.AWSHealth.Types.Product
import Network.AWS.AWSHealth.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
awsHealth :: Service
awsHealth =
  Service
    { _svcAbbrev = "AWSHealth"
    , _svcSigner = v4
    , _svcPrefix = "health"
    , _svcVersion = "2016-08-04"
    , _svcEndpoint = defaultEndpoint awsHealth
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AWSHealth"
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


-- | The specified pagination token (@nextToken@ ) is not valid.
--
--
_InvalidPaginationToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPaginationToken = _MatchServiceError awsHealth "InvalidPaginationToken"


-- | The specified locale is not supported.
--
--
_UnsupportedLocale :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedLocale = _MatchServiceError awsHealth "UnsupportedLocale"

