-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types
  ( -- * Service configuration
    awsHealthService,

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
    AffectedEntity (..),
    mkAffectedEntity,
    aeLastUpdatedTime,
    aeEntityValue,
    aeEntityURL,
    aeAwsAccountId,
    aeEventARN,
    aeEntityARN,
    aeTags,
    aeStatusCode,

    -- * DateTimeRange
    DateTimeRange (..),
    mkDateTimeRange,
    dtrTo,
    dtrFrom,

    -- * EntityAggregate
    EntityAggregate (..),
    mkEntityAggregate,
    eCount,
    eEventARN,

    -- * EntityFilter
    EntityFilter (..),
    mkEntityFilter,
    eStatusCodes,
    eEntityARNs,
    eEntityValues,
    eTags,
    eLastUpdatedTimes,
    eEventARNs,

    -- * Event
    Event (..),
    mkEvent,
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
    EventAccountFilter (..),
    mkEventAccountFilter,
    eafAwsAccountId,
    eafEventARN,

    -- * EventAggregate
    EventAggregate (..),
    mkEventAggregate,
    eaCount,
    eaAggregateValue,

    -- * EventDescription
    EventDescription (..),
    mkEventDescription,
    edLatestDescription,

    -- * EventDetails
    EventDetails (..),
    mkEventDetails,
    edEvent,
    edEventDescription,
    edEventMetadata,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem (..),
    mkEventDetailsErrorItem,
    edeiEventARN,
    edeiErrorName,
    edeiErrorMessage,

    -- * EventFilter
    EventFilter (..),
    mkEventFilter,
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
    EventType (..),
    mkEventType,
    etService,
    etCategory,
    etCode,

    -- * EventTypeFilter
    EventTypeFilter (..),
    mkEventTypeFilter,
    etfEventTypeCategories,
    etfEventTypeCodes,
    etfServices,

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (..),
    mkOrganizationAffectedEntitiesErrorItem,
    oaeeiAwsAccountId,
    oaeeiEventARN,
    oaeeiErrorName,
    oaeeiErrorMessage,

    -- * OrganizationEvent
    OrganizationEvent (..),
    mkOrganizationEvent,
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
    OrganizationEventDetails (..),
    mkOrganizationEventDetails,
    oedEvent,
    oedEventDescription,
    oedAwsAccountId,
    oedEventMetadata,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    mkOrganizationEventDetailsErrorItem,
    oedeiAwsAccountId,
    oedeiEventARN,
    oedeiErrorName,
    oedeiErrorMessage,

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    mkOrganizationEventFilter,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
awsHealthService :: Lude.Service
awsHealthService =
  Lude.Service
    { Lude._svcAbbrev = "AWSHealth",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "health",
      Lude._svcVersion = "2016-08-04",
      Lude._svcEndpoint = Lude.defaultEndpoint awsHealthService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "AWSHealth",
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
