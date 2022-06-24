{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AWSHealth.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Lens
  ( -- * Operations

    -- ** DescribeAffectedAccountsForOrganization
    describeAffectedAccountsForOrganization_nextToken,
    describeAffectedAccountsForOrganization_maxResults,
    describeAffectedAccountsForOrganization_eventArn,
    describeAffectedAccountsForOrganizationResponse_nextToken,
    describeAffectedAccountsForOrganizationResponse_eventScopeCode,
    describeAffectedAccountsForOrganizationResponse_affectedAccounts,
    describeAffectedAccountsForOrganizationResponse_httpStatus,

    -- ** DescribeAffectedEntities
    describeAffectedEntities_nextToken,
    describeAffectedEntities_locale,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_filter,
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_httpStatus,

    -- ** DescribeAffectedEntitiesForOrganization
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,

    -- ** DescribeEntityAggregates
    describeEntityAggregates_eventArns,
    describeEntityAggregatesResponse_entityAggregates,
    describeEntityAggregatesResponse_httpStatus,

    -- ** DescribeEventAggregates
    describeEventAggregates_nextToken,
    describeEventAggregates_filter,
    describeEventAggregates_maxResults,
    describeEventAggregates_aggregateField,
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_httpStatus,

    -- ** DescribeEventDetails
    describeEventDetails_locale,
    describeEventDetails_eventArns,
    describeEventDetailsResponse_successfulSet,
    describeEventDetailsResponse_failedSet,
    describeEventDetailsResponse_httpStatus,

    -- ** DescribeEventDetailsForOrganization
    describeEventDetailsForOrganization_locale,
    describeEventDetailsForOrganization_organizationEventDetailFilters,
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_httpStatus,

    -- ** DescribeEventTypes
    describeEventTypes_nextToken,
    describeEventTypes_locale,
    describeEventTypes_filter,
    describeEventTypes_maxResults,
    describeEventTypesResponse_nextToken,
    describeEventTypesResponse_eventTypes,
    describeEventTypesResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_locale,
    describeEvents_filter,
    describeEvents_maxResults,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeEventsForOrganization
    describeEventsForOrganization_nextToken,
    describeEventsForOrganization_locale,
    describeEventsForOrganization_filter,
    describeEventsForOrganization_maxResults,
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_httpStatus,

    -- ** DescribeHealthServiceStatusForOrganization
    describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization,
    describeHealthServiceStatusForOrganizationResponse_httpStatus,

    -- ** DisableHealthServiceAccessForOrganization

    -- ** EnableHealthServiceAccessForOrganization

    -- * Types

    -- ** AffectedEntity
    affectedEntity_awsAccountId,
    affectedEntity_tags,
    affectedEntity_entityValue,
    affectedEntity_eventArn,
    affectedEntity_entityUrl,
    affectedEntity_lastUpdatedTime,
    affectedEntity_statusCode,
    affectedEntity_entityArn,

    -- ** DateTimeRange
    dateTimeRange_from,
    dateTimeRange_to,

    -- ** EntityAggregate
    entityAggregate_eventArn,
    entityAggregate_count,

    -- ** EntityFilter
    entityFilter_tags,
    entityFilter_entityArns,
    entityFilter_lastUpdatedTimes,
    entityFilter_entityValues,
    entityFilter_statusCodes,
    entityFilter_eventArns,

    -- ** Event
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

    -- ** EventAccountFilter
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- ** EventAggregate
    eventAggregate_aggregateValue,
    eventAggregate_count,

    -- ** EventDescription
    eventDescription_latestDescription,

    -- ** EventDetails
    eventDetails_eventMetadata,
    eventDetails_event,
    eventDetails_eventDescription,

    -- ** EventDetailsErrorItem
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorMessage,
    eventDetailsErrorItem_errorName,

    -- ** EventFilter
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

    -- ** EventType
    eventType_code,
    eventType_service,
    eventType_category,

    -- ** EventTypeFilter
    eventTypeFilter_services,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_eventTypeCategories,

    -- ** OrganizationAffectedEntitiesErrorItem
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_errorMessage,
    organizationAffectedEntitiesErrorItem_errorName,

    -- ** OrganizationEvent
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

    -- ** OrganizationEventDetails
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,

    -- ** OrganizationEventDetailsErrorItem
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_errorMessage,
    organizationEventDetailsErrorItem_errorName,

    -- ** OrganizationEventFilter
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
