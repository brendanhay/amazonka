{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AWSHealth.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    affectedEntity_tags,
    affectedEntity_entityValue,
    affectedEntity_eventArn,
    affectedEntity_entityUrl,
    affectedEntity_lastUpdatedTime,
    affectedEntity_statusCode,
    affectedEntity_entityArn,

    -- * DateTimeRange
    DateTimeRange (..),
    newDateTimeRange,
    dateTimeRange_from,
    dateTimeRange_to,

    -- * EntityAggregate
    EntityAggregate (..),
    newEntityAggregate,
    entityAggregate_eventArn,
    entityAggregate_count,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_tags,
    entityFilter_entityArns,
    entityFilter_lastUpdatedTimes,
    entityFilter_entityValues,
    entityFilter_statusCodes,
    entityFilter_eventArns,

    -- * Event
    Event (..),
    newEvent,
    event_arn,
    event_lastUpdatedTime,
    event_endTime,
    event_availabilityZone,
    event_service,
    event_eventScopeCode,
    event_region,
    event_eventTypeCode,
    event_eventTypeCategory,
    event_statusCode,
    event_startTime,

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
    eventDetails_eventMetadata,
    eventDetails_event,
    eventDetails_eventDescription,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem (..),
    newEventDetailsErrorItem,
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorMessage,
    eventDetailsErrorItem_errorName,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_tags,
    eventFilter_startTimes,
    eventFilter_entityArns,
    eventFilter_eventArns,
    eventFilter_regions,
    eventFilter_availabilityZones,
    eventFilter_services,
    eventFilter_eventStatusCodes,
    eventFilter_eventTypeCodes,
    eventFilter_endTimes,
    eventFilter_lastUpdatedTimes,
    eventFilter_entityValues,
    eventFilter_eventTypeCategories,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_code,
    eventType_service,
    eventType_category,

    -- * EventTypeFilter
    EventTypeFilter (..),
    newEventTypeFilter,
    eventTypeFilter_services,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_eventTypeCategories,

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (..),
    newOrganizationAffectedEntitiesErrorItem,
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_errorMessage,
    organizationAffectedEntitiesErrorItem_errorName,

    -- * OrganizationEvent
    OrganizationEvent (..),
    newOrganizationEvent,
    organizationEvent_arn,
    organizationEvent_lastUpdatedTime,
    organizationEvent_endTime,
    organizationEvent_service,
    organizationEvent_eventScopeCode,
    organizationEvent_region,
    organizationEvent_eventTypeCode,
    organizationEvent_eventTypeCategory,
    organizationEvent_statusCode,
    organizationEvent_startTime,

    -- * OrganizationEventDetails
    OrganizationEventDetails (..),
    newOrganizationEventDetails,
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    newOrganizationEventDetailsErrorItem,
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_errorMessage,
    organizationEventDetailsErrorItem_errorName,

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    newOrganizationEventFilter,
    organizationEventFilter_awsAccountIds,
    organizationEventFilter_entityArns,
    organizationEventFilter_regions,
    organizationEventFilter_lastUpdatedTime,
    organizationEventFilter_endTime,
    organizationEventFilter_services,
    organizationEventFilter_eventStatusCodes,
    organizationEventFilter_eventTypeCodes,
    organizationEventFilter_entityValues,
    organizationEventFilter_eventTypeCategories,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | The specified pagination token (@nextToken@) is not valid.
_InvalidPaginationToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationToken"

-- | The specified locale is not supported.
_UnsupportedLocale :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLocale =
  Core._MatchServiceError
    defaultService
    "UnsupportedLocale"
