{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AWSHealth.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types
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
    affectedEntity_awsAccountId,
    affectedEntity_entityArn,
    affectedEntity_entityUrl,
    affectedEntity_entityValue,
    affectedEntity_eventArn,
    affectedEntity_lastUpdatedTime,
    affectedEntity_statusCode,
    affectedEntity_tags,

    -- * DateTimeRange
    DateTimeRange (..),
    newDateTimeRange,
    dateTimeRange_from,
    dateTimeRange_to,

    -- * EntityAggregate
    EntityAggregate (..),
    newEntityAggregate,
    entityAggregate_count,
    entityAggregate_eventArn,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_entityArns,
    entityFilter_entityValues,
    entityFilter_lastUpdatedTimes,
    entityFilter_statusCodes,
    entityFilter_tags,
    entityFilter_eventArns,

    -- * Event
    Event (..),
    newEvent,
    event_arn,
    event_availabilityZone,
    event_endTime,
    event_eventScopeCode,
    event_eventTypeCategory,
    event_eventTypeCode,
    event_lastUpdatedTime,
    event_region,
    event_service,
    event_startTime,
    event_statusCode,

    -- * EventAccountFilter
    EventAccountFilter (..),
    newEventAccountFilter,
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- * EventAggregate
    EventAggregate (..),
    newEventAggregate,
    eventAggregate_aggregateValue,
    eventAggregate_count,

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
    eventDetailsErrorItem_errorMessage,
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_eventArn,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_availabilityZones,
    eventFilter_endTimes,
    eventFilter_entityArns,
    eventFilter_entityValues,
    eventFilter_eventArns,
    eventFilter_eventStatusCodes,
    eventFilter_eventTypeCategories,
    eventFilter_eventTypeCodes,
    eventFilter_lastUpdatedTimes,
    eventFilter_regions,
    eventFilter_services,
    eventFilter_startTimes,
    eventFilter_tags,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_category,
    eventType_code,
    eventType_service,

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
    organizationAffectedEntitiesErrorItem_errorMessage,
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_eventArn,

    -- * OrganizationEvent
    OrganizationEvent (..),
    newOrganizationEvent,
    organizationEvent_arn,
    organizationEvent_endTime,
    organizationEvent_eventScopeCode,
    organizationEvent_eventTypeCategory,
    organizationEvent_eventTypeCode,
    organizationEvent_lastUpdatedTime,
    organizationEvent_region,
    organizationEvent_service,
    organizationEvent_startTime,
    organizationEvent_statusCode,

    -- * OrganizationEventDetails
    OrganizationEventDetails (..),
    newOrganizationEventDetails,
    organizationEventDetails_awsAccountId,
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,
    organizationEventDetails_eventMetadata,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    newOrganizationEventDetailsErrorItem,
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_errorMessage,
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_eventArn,

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    newOrganizationEventFilter,
    organizationEventFilter_awsAccountIds,
    organizationEventFilter_endTime,
    organizationEventFilter_entityArns,
    organizationEventFilter_entityValues,
    organizationEventFilter_eventStatusCodes,
    organizationEventFilter_eventTypeCategories,
    organizationEventFilter_eventTypeCodes,
    organizationEventFilter_lastUpdatedTime,
    organizationEventFilter_regions,
    organizationEventFilter_services,
    organizationEventFilter_startTime,
  )
where

import Amazonka.AWSHealth.Types.AffectedEntity
import Amazonka.AWSHealth.Types.DateTimeRange
import Amazonka.AWSHealth.Types.EntityAggregate
import Amazonka.AWSHealth.Types.EntityFilter
import Amazonka.AWSHealth.Types.EntityStatusCode
import Amazonka.AWSHealth.Types.Event
import Amazonka.AWSHealth.Types.EventAccountFilter
import Amazonka.AWSHealth.Types.EventAggregate
import Amazonka.AWSHealth.Types.EventAggregateField
import Amazonka.AWSHealth.Types.EventDescription
import Amazonka.AWSHealth.Types.EventDetails
import Amazonka.AWSHealth.Types.EventDetailsErrorItem
import Amazonka.AWSHealth.Types.EventFilter
import Amazonka.AWSHealth.Types.EventScopeCode
import Amazonka.AWSHealth.Types.EventStatusCode
import Amazonka.AWSHealth.Types.EventType
import Amazonka.AWSHealth.Types.EventTypeCategory
import Amazonka.AWSHealth.Types.EventTypeFilter
import Amazonka.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
import Amazonka.AWSHealth.Types.OrganizationEvent
import Amazonka.AWSHealth.Types.OrganizationEventDetails
import Amazonka.AWSHealth.Types.OrganizationEventDetailsErrorItem
import Amazonka.AWSHealth.Types.OrganizationEventFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AWSHealth",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "health",
      Core.signingName = "health",
      Core.version = "2016-08-04",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AWSHealth",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- is already in progress. Wait for the action to complete before trying
-- again. To get the current status, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeHealthServiceStatusForOrganization.html DescribeHealthServiceStatusForOrganization>
-- operation.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified pagination token (@nextToken@) is not valid.
_InvalidPaginationToken :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationToken"

-- | The specified locale is not supported.
_UnsupportedLocale :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedLocale =
  Core._MatchServiceError
    defaultService
    "UnsupportedLocale"
