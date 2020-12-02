{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types
  ( -- * Service Configuration
    awsHealth,

    -- * Errors

    -- * EntityStatusCode
    EntityStatusCode (..),

    -- * EventAggregateField
    EventAggregateField (..),

    -- * EventScopeCode
    EventScopeCode (..),

    -- * EventStatusCode
    EventStatusCode (..),

    -- * EventTypeCategory
    EventTypeCategory (..),

    -- * AffectedEntity
    AffectedEntity,
    affectedEntity,
    aeLastUpdatedTime,
    aeEntityValue,
    aeEntityURL,
    aeAwsAccountId,
    aeEventARN,
    aeEntityARN,
    aeTags,
    aeStatusCode,

    -- * DateTimeRange
    DateTimeRange,
    dateTimeRange,
    dtrTo,
    dtrFrom,

    -- * EntityAggregate
    EntityAggregate,
    entityAggregate,
    eCount,
    eEventARN,

    -- * EntityFilter
    EntityFilter,
    entityFilter,
    eStatusCodes,
    eEntityARNs,
    eEntityValues,
    eTags,
    eLastUpdatedTimes,
    eEventARNs,

    -- * Event
    Event,
    event,
    eLastUpdatedTime,
    eArn,
    eService,
    eStartTime,
    eEventScopeCode,
    eEventTypeCode,
    eEventTypeCategory,
    eAvailabilityZone,
    eEndTime,
    eRegion,
    eStatusCode,

    -- * EventAccountFilter
    EventAccountFilter,
    eventAccountFilter,
    eafAwsAccountId,
    eafEventARN,

    -- * EventAggregate
    EventAggregate,
    eventAggregate,
    eaCount,
    eaAggregateValue,

    -- * EventDescription
    EventDescription,
    eventDescription,
    edLatestDescription,

    -- * EventDetails
    EventDetails,
    eventDetails,
    edEvent,
    edEventDescription,
    edEventMetadata,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem,
    eventDetailsErrorItem,
    edeiEventARN,
    edeiErrorName,
    edeiErrorMessage,

    -- * EventFilter
    EventFilter,
    eventFilter,
    efEventARNs,
    efEventTypeCategories,
    efEventTypeCodes,
    efRegions,
    efEventStatusCodes,
    efEndTimes,
    efAvailabilityZones,
    efEntityARNs,
    efEntityValues,
    efStartTimes,
    efServices,
    efTags,
    efLastUpdatedTimes,

    -- * EventType
    EventType,
    eventType,
    etService,
    etCategory,
    etCode,

    -- * EventTypeFilter
    EventTypeFilter,
    eventTypeFilter,
    etfEventTypeCategories,
    etfEventTypeCodes,
    etfServices,

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem,
    organizationAffectedEntitiesErrorItem,
    oaeeiAwsAccountId,
    oaeeiEventARN,
    oaeeiErrorName,
    oaeeiErrorMessage,

    -- * OrganizationEvent
    OrganizationEvent,
    organizationEvent,
    oeLastUpdatedTime,
    oeArn,
    oeService,
    oeStartTime,
    oeEventScopeCode,
    oeEventTypeCode,
    oeEventTypeCategory,
    oeEndTime,
    oeRegion,
    oeStatusCode,

    -- * OrganizationEventDetails
    OrganizationEventDetails,
    organizationEventDetails,
    oedEvent,
    oedEventDescription,
    oedAwsAccountId,
    oedEventMetadata,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem,
    organizationEventDetailsErrorItem,
    oedeiAwsAccountId,
    oedeiEventARN,
    oedeiErrorName,
    oedeiErrorMessage,

    -- * OrganizationEventFilter
    OrganizationEventFilter,
    organizationEventFilter,
    oefLastUpdatedTime,
    oefAwsAccountIds,
    oefEventTypeCategories,
    oefEventTypeCodes,
    oefStartTime,
    oefRegions,
    oefEventStatusCodes,
    oefEndTime,
    oefEntityARNs,
    oefEntityValues,
    oefServices,
  )
where

import Network.AWS.AWSHealth.Types.AffectedEntity
import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityAggregate
import Network.AWS.AWSHealth.Types.EntityFilter
import Network.AWS.AWSHealth.Types.EntityStatusCode
import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventAccountFilter
import Network.AWS.AWSHealth.Types.EventAggregate
import Network.AWS.AWSHealth.Types.EventAggregateField
import Network.AWS.AWSHealth.Types.EventDescription
import Network.AWS.AWSHealth.Types.EventDetails
import Network.AWS.AWSHealth.Types.EventDetailsErrorItem
import Network.AWS.AWSHealth.Types.EventFilter
import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventType
import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.AWSHealth.Types.EventTypeFilter
import Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEvent
import Network.AWS.AWSHealth.Types.OrganizationEventDetails
import Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEventFilter
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
awsHealth :: Service
awsHealth =
  Service
    { _svcAbbrev = "AWSHealth",
      _svcSigner = v4,
      _svcPrefix = "health",
      _svcVersion = "2016-08-04",
      _svcEndpoint = defaultEndpoint awsHealth,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "AWSHealth",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
