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
    _InvalidPaginationToken,
    _ConcurrentModificationException,
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
    affectedEntity_lastUpdatedTime,
    affectedEntity_entityValue,
    affectedEntity_entityUrl,
    affectedEntity_awsAccountId,
    affectedEntity_eventArn,
    affectedEntity_entityArn,
    affectedEntity_tags,
    affectedEntity_statusCode,

    -- * DateTimeRange
    DateTimeRange (..),
    newDateTimeRange,
    dateTimeRange_to,
    dateTimeRange_from,

    -- * EntityAggregate
    EntityAggregate (..),
    newEntityAggregate,
    entityAggregate_count,
    entityAggregate_eventArn,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_statusCodes,
    entityFilter_entityArns,
    entityFilter_entityValues,
    entityFilter_tags,
    entityFilter_lastUpdatedTimes,
    entityFilter_eventArns,

    -- * Event
    Event (..),
    newEvent,
    event_lastUpdatedTime,
    event_arn,
    event_service,
    event_startTime,
    event_eventScopeCode,
    event_eventTypeCode,
    event_eventTypeCategory,
    event_availabilityZone,
    event_endTime,
    event_region,
    event_statusCode,

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
    eventDetails_event,
    eventDetails_eventDescription,
    eventDetails_eventMetadata,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem (..),
    newEventDetailsErrorItem,
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_errorMessage,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_eventArns,
    eventFilter_eventTypeCategories,
    eventFilter_eventTypeCodes,
    eventFilter_regions,
    eventFilter_eventStatusCodes,
    eventFilter_endTimes,
    eventFilter_availabilityZones,
    eventFilter_entityArns,
    eventFilter_entityValues,
    eventFilter_startTimes,
    eventFilter_services,
    eventFilter_tags,
    eventFilter_lastUpdatedTimes,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_service,
    eventType_category,
    eventType_code,

    -- * EventTypeFilter
    EventTypeFilter (..),
    newEventTypeFilter,
    eventTypeFilter_eventTypeCategories,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_services,

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (..),
    newOrganizationAffectedEntitiesErrorItem,
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_errorMessage,

    -- * OrganizationEvent
    OrganizationEvent (..),
    newOrganizationEvent,
    organizationEvent_lastUpdatedTime,
    organizationEvent_arn,
    organizationEvent_service,
    organizationEvent_startTime,
    organizationEvent_eventScopeCode,
    organizationEvent_eventTypeCode,
    organizationEvent_eventTypeCategory,
    organizationEvent_endTime,
    organizationEvent_region,
    organizationEvent_statusCode,

    -- * OrganizationEventDetails
    OrganizationEventDetails (..),
    newOrganizationEventDetails,
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    newOrganizationEventDetailsErrorItem,
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_errorMessage,

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    newOrganizationEventFilter,
    organizationEventFilter_lastUpdatedTime,
    organizationEventFilter_awsAccountIds,
    organizationEventFilter_eventTypeCategories,
    organizationEventFilter_eventTypeCodes,
    organizationEventFilter_startTime,
    organizationEventFilter_regions,
    organizationEventFilter_eventStatusCodes,
    organizationEventFilter_endTime,
    organizationEventFilter_entityArns,
    organizationEventFilter_entityValues,
    organizationEventFilter_services,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AWSHealth",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "health",
      Core._serviceSigningName = "health",
      Core._serviceVersion = "2016-08-04",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AWSHealth",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified pagination token (@nextToken@) is not valid.
_InvalidPaginationToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationToken"

-- | <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- is already in progress. Wait for the action to complete before trying
-- again. To get the current status, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeHealthServiceStatusForOrganization.html DescribeHealthServiceStatusForOrganization>
-- operation.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified locale is not supported.
_UnsupportedLocale :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLocale =
  Core._MatchServiceError
    defaultService
    "UnsupportedLocale"
