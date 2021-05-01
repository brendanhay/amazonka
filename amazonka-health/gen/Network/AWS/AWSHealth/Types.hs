{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _InvalidPaginationToken,
    _UnsupportedLocale,

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
    newAffectedEntity,
    affectedEntity_eventArn,
    affectedEntity_awsAccountId,
    affectedEntity_statusCode,
    affectedEntity_tags,
    affectedEntity_entityArn,
    affectedEntity_entityValue,
    affectedEntity_entityUrl,
    affectedEntity_lastUpdatedTime,

    -- * DateTimeRange
    DateTimeRange (..),
    newDateTimeRange,
    dateTimeRange_to,
    dateTimeRange_from,

    -- * EntityAggregate
    EntityAggregate (..),
    newEntityAggregate,
    entityAggregate_eventArn,
    entityAggregate_count,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_entityArns,
    entityFilter_statusCodes,
    entityFilter_lastUpdatedTimes,
    entityFilter_tags,
    entityFilter_entityValues,
    entityFilter_eventArns,

    -- * Event
    Event (..),
    newEvent,
    event_eventTypeCategory,
    event_eventScopeCode,
    event_startTime,
    event_service,
    event_arn,
    event_endTime,
    event_availabilityZone,
    event_statusCode,
    event_eventTypeCode,
    event_region,
    event_lastUpdatedTime,

    -- * EventAccountFilter
    EventAccountFilter (..),
    newEventAccountFilter,
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- * EventAggregate
    EventAggregate (..),
    newEventAggregate,
    eventAggregate_count,
    eventAggregate_aggregateValue,

    -- * EventDescription
    EventDescription (..),
    newEventDescription,
    eventDescription_latestDescription,

    -- * EventDetails
    EventDetails (..),
    newEventDetails,
    eventDetails_eventMetadata,
    eventDetails_eventDescription,
    eventDetails_event,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem (..),
    newEventDetailsErrorItem,
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorMessage,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_availabilityZones,
    eventFilter_endTimes,
    eventFilter_startTimes,
    eventFilter_services,
    eventFilter_entityArns,
    eventFilter_eventTypeCodes,
    eventFilter_lastUpdatedTimes,
    eventFilter_tags,
    eventFilter_eventStatusCodes,
    eventFilter_entityValues,
    eventFilter_regions,
    eventFilter_eventArns,
    eventFilter_eventTypeCategories,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_category,
    eventType_code,
    eventType_service,

    -- * EventTypeFilter
    EventTypeFilter (..),
    newEventTypeFilter,
    eventTypeFilter_services,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_eventTypeCategories,

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (..),
    newOrganizationAffectedEntitiesErrorItem,
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_errorMessage,

    -- * OrganizationEvent
    OrganizationEvent (..),
    newOrganizationEvent,
    organizationEvent_eventTypeCategory,
    organizationEvent_eventScopeCode,
    organizationEvent_startTime,
    organizationEvent_service,
    organizationEvent_arn,
    organizationEvent_endTime,
    organizationEvent_statusCode,
    organizationEvent_eventTypeCode,
    organizationEvent_region,
    organizationEvent_lastUpdatedTime,

    -- * OrganizationEventDetails
    OrganizationEventDetails (..),
    newOrganizationEventDetails,
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,
    organizationEventDetails_eventDescription,
    organizationEventDetails_event,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    newOrganizationEventDetailsErrorItem,
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_errorMessage,

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    newOrganizationEventFilter,
    organizationEventFilter_services,
    organizationEventFilter_startTime,
    organizationEventFilter_entityArns,
    organizationEventFilter_eventTypeCodes,
    organizationEventFilter_endTime,
    organizationEventFilter_eventStatusCodes,
    organizationEventFilter_entityValues,
    organizationEventFilter_regions,
    organizationEventFilter_eventTypeCategories,
    organizationEventFilter_awsAccountIds,
    organizationEventFilter_lastUpdatedTime,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "AWSHealth",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "health",
      Prelude._svcSigningName = "health",
      Prelude._svcVersion = "2016-08-04",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "AWSHealth",
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

-- | <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- is already in progress. Wait for the action to complete before trying
-- again. To get the current status, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeHealthServiceStatusForOrganization.html DescribeHealthServiceStatusForOrganization>
-- operation.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified pagination token (@nextToken@) is not valid.
_InvalidPaginationToken :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPaginationToken =
  Prelude._MatchServiceError
    defaultService
    "InvalidPaginationToken"

-- | The specified locale is not supported.
_UnsupportedLocale :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedLocale =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedLocale"
