{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AWSHealth.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Lens
  ( -- * Operations

    -- ** DescribeAffectedAccountsForOrganization
    describeAffectedAccountsForOrganization_maxResults,
    describeAffectedAccountsForOrganization_nextToken,
    describeAffectedAccountsForOrganization_eventArn,
    describeAffectedAccountsForOrganizationResponse_affectedAccounts,
    describeAffectedAccountsForOrganizationResponse_eventScopeCode,
    describeAffectedAccountsForOrganizationResponse_nextToken,
    describeAffectedAccountsForOrganizationResponse_httpStatus,

    -- ** DescribeAffectedEntities
    describeAffectedEntities_locale,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_nextToken,
    describeAffectedEntities_filter,
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_httpStatus,

    -- ** DescribeAffectedEntitiesForOrganization
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,

    -- ** DescribeEntityAggregates
    describeEntityAggregates_eventArns,
    describeEntityAggregatesResponse_entityAggregates,
    describeEntityAggregatesResponse_httpStatus,

    -- ** DescribeEventAggregates
    describeEventAggregates_filter,
    describeEventAggregates_maxResults,
    describeEventAggregates_nextToken,
    describeEventAggregates_aggregateField,
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_httpStatus,

    -- ** DescribeEventDetails
    describeEventDetails_locale,
    describeEventDetails_eventArns,
    describeEventDetailsResponse_failedSet,
    describeEventDetailsResponse_successfulSet,
    describeEventDetailsResponse_httpStatus,

    -- ** DescribeEventDetailsForOrganization
    describeEventDetailsForOrganization_locale,
    describeEventDetailsForOrganization_organizationEventDetailFilters,
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_httpStatus,

    -- ** DescribeEventTypes
    describeEventTypes_filter,
    describeEventTypes_locale,
    describeEventTypes_maxResults,
    describeEventTypes_nextToken,
    describeEventTypesResponse_eventTypes,
    describeEventTypesResponse_nextToken,
    describeEventTypesResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_filter,
    describeEvents_locale,
    describeEvents_maxResults,
    describeEvents_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,

    -- ** DescribeEventsForOrganization
    describeEventsForOrganization_filter,
    describeEventsForOrganization_locale,
    describeEventsForOrganization_maxResults,
    describeEventsForOrganization_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_httpStatus,

    -- ** DescribeHealthServiceStatusForOrganization
    describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization,
    describeHealthServiceStatusForOrganizationResponse_httpStatus,

    -- ** DisableHealthServiceAccessForOrganization

    -- ** EnableHealthServiceAccessForOrganization

    -- * Types

    -- ** AffectedEntity
    affectedEntity_awsAccountId,
    affectedEntity_entityArn,
    affectedEntity_entityUrl,
    affectedEntity_entityValue,
    affectedEntity_eventArn,
    affectedEntity_lastUpdatedTime,
    affectedEntity_statusCode,
    affectedEntity_tags,

    -- ** DateTimeRange
    dateTimeRange_from,
    dateTimeRange_to,

    -- ** EntityAggregate
    entityAggregate_count,
    entityAggregate_eventArn,

    -- ** EntityFilter
    entityFilter_entityArns,
    entityFilter_entityValues,
    entityFilter_lastUpdatedTimes,
    entityFilter_statusCodes,
    entityFilter_tags,
    entityFilter_eventArns,

    -- ** Event
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

    -- ** EventAccountFilter
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- ** EventAggregate
    eventAggregate_aggregateValue,
    eventAggregate_count,

    -- ** EventDescription
    eventDescription_latestDescription,

    -- ** EventDetails
    eventDetails_event,
    eventDetails_eventDescription,
    eventDetails_eventMetadata,

    -- ** EventDetailsErrorItem
    eventDetailsErrorItem_errorMessage,
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_eventArn,

    -- ** EventFilter
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

    -- ** EventType
    eventType_category,
    eventType_code,
    eventType_service,

    -- ** EventTypeFilter
    eventTypeFilter_eventTypeCategories,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_services,

    -- ** OrganizationAffectedEntitiesErrorItem
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_errorMessage,
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_eventArn,

    -- ** OrganizationEvent
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

    -- ** OrganizationEventDetails
    organizationEventDetails_awsAccountId,
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,
    organizationEventDetails_eventMetadata,

    -- ** OrganizationEventDetailsErrorItem
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_errorMessage,
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_eventArn,

    -- ** OrganizationEventFilter
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

import Amazonka.AWSHealth.DescribeAffectedAccountsForOrganization
import Amazonka.AWSHealth.DescribeAffectedEntities
import Amazonka.AWSHealth.DescribeAffectedEntitiesForOrganization
import Amazonka.AWSHealth.DescribeEntityAggregates
import Amazonka.AWSHealth.DescribeEventAggregates
import Amazonka.AWSHealth.DescribeEventDetails
import Amazonka.AWSHealth.DescribeEventDetailsForOrganization
import Amazonka.AWSHealth.DescribeEventTypes
import Amazonka.AWSHealth.DescribeEvents
import Amazonka.AWSHealth.DescribeEventsForOrganization
import Amazonka.AWSHealth.DescribeHealthServiceStatusForOrganization
import Amazonka.AWSHealth.DisableHealthServiceAccessForOrganization
import Amazonka.AWSHealth.EnableHealthServiceAccessForOrganization
import Amazonka.AWSHealth.Types.AffectedEntity
import Amazonka.AWSHealth.Types.DateTimeRange
import Amazonka.AWSHealth.Types.EntityAggregate
import Amazonka.AWSHealth.Types.EntityFilter
import Amazonka.AWSHealth.Types.Event
import Amazonka.AWSHealth.Types.EventAccountFilter
import Amazonka.AWSHealth.Types.EventAggregate
import Amazonka.AWSHealth.Types.EventDescription
import Amazonka.AWSHealth.Types.EventDetails
import Amazonka.AWSHealth.Types.EventDetailsErrorItem
import Amazonka.AWSHealth.Types.EventFilter
import Amazonka.AWSHealth.Types.EventType
import Amazonka.AWSHealth.Types.EventTypeFilter
import Amazonka.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
import Amazonka.AWSHealth.Types.OrganizationEvent
import Amazonka.AWSHealth.Types.OrganizationEventDetails
import Amazonka.AWSHealth.Types.OrganizationEventDetailsErrorItem
import Amazonka.AWSHealth.Types.OrganizationEventFilter
