{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Lens
  ( -- * Operations

    -- ** EnableHealthServiceAccessForOrganization

    -- ** DescribeEntityAggregates
    describeEntityAggregates_eventArns,
    describeEntityAggregatesResponse_entityAggregates,
    describeEntityAggregatesResponse_httpStatus,

    -- ** DisableHealthServiceAccessForOrganization

    -- ** DescribeEventDetailsForOrganization
    describeEventDetailsForOrganization_locale,
    describeEventDetailsForOrganization_organizationEventDetailFilters,
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_httpStatus,

    -- ** DescribeEventDetails
    describeEventDetails_locale,
    describeEventDetails_eventArns,
    describeEventDetailsResponse_successfulSet,
    describeEventDetailsResponse_failedSet,
    describeEventDetailsResponse_httpStatus,

    -- ** DescribeAffectedAccountsForOrganization
    describeAffectedAccountsForOrganization_nextToken,
    describeAffectedAccountsForOrganization_maxResults,
    describeAffectedAccountsForOrganization_eventArn,
    describeAffectedAccountsForOrganizationResponse_nextToken,
    describeAffectedAccountsForOrganizationResponse_eventScopeCode,
    describeAffectedAccountsForOrganizationResponse_affectedAccounts,
    describeAffectedAccountsForOrganizationResponse_httpStatus,

    -- ** DescribeAffectedEntitiesForOrganization
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_maxResults,
    describeEvents_locale,
    describeEvents_filter,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeEventAggregates
    describeEventAggregates_nextToken,
    describeEventAggregates_maxResults,
    describeEventAggregates_filter,
    describeEventAggregates_aggregateField,
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_httpStatus,

    -- ** DescribeHealthServiceStatusForOrganization
    describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization,
    describeHealthServiceStatusForOrganizationResponse_httpStatus,

    -- ** DescribeEventTypes
    describeEventTypes_nextToken,
    describeEventTypes_maxResults,
    describeEventTypes_locale,
    describeEventTypes_filter,
    describeEventTypesResponse_eventTypes,
    describeEventTypesResponse_nextToken,
    describeEventTypesResponse_httpStatus,

    -- ** DescribeAffectedEntities
    describeAffectedEntities_nextToken,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_locale,
    describeAffectedEntities_filter,
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_httpStatus,

    -- ** DescribeEventsForOrganization
    describeEventsForOrganization_nextToken,
    describeEventsForOrganization_maxResults,
    describeEventsForOrganization_locale,
    describeEventsForOrganization_filter,
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_httpStatus,

    -- * Types

    -- ** AffectedEntity
    affectedEntity_eventArn,
    affectedEntity_awsAccountId,
    affectedEntity_statusCode,
    affectedEntity_tags,
    affectedEntity_entityArn,
    affectedEntity_entityValue,
    affectedEntity_entityUrl,
    affectedEntity_lastUpdatedTime,

    -- ** DateTimeRange
    dateTimeRange_to,
    dateTimeRange_from,

    -- ** EntityAggregate
    entityAggregate_eventArn,
    entityAggregate_count,

    -- ** EntityFilter
    entityFilter_entityArns,
    entityFilter_statusCodes,
    entityFilter_lastUpdatedTimes,
    entityFilter_tags,
    entityFilter_entityValues,
    entityFilter_eventArns,

    -- ** Event
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

    -- ** EventAccountFilter
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- ** EventAggregate
    eventAggregate_count,
    eventAggregate_aggregateValue,

    -- ** EventDescription
    eventDescription_latestDescription,

    -- ** EventDetails
    eventDetails_eventMetadata,
    eventDetails_eventDescription,
    eventDetails_event,

    -- ** EventDetailsErrorItem
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorMessage,

    -- ** EventFilter
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

    -- ** EventType
    eventType_category,
    eventType_code,
    eventType_service,

    -- ** EventTypeFilter
    eventTypeFilter_services,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_eventTypeCategories,

    -- ** OrganizationAffectedEntitiesErrorItem
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_errorMessage,

    -- ** OrganizationEvent
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

    -- ** OrganizationEventDetails
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,
    organizationEventDetails_eventDescription,
    organizationEventDetails_event,

    -- ** OrganizationEventDetailsErrorItem
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_errorMessage,

    -- ** OrganizationEventFilter
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

import Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
import Network.AWS.AWSHealth.DescribeAffectedEntities
import Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
import Network.AWS.AWSHealth.DescribeEntityAggregates
import Network.AWS.AWSHealth.DescribeEventAggregates
import Network.AWS.AWSHealth.DescribeEventDetails
import Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
import Network.AWS.AWSHealth.DescribeEventTypes
import Network.AWS.AWSHealth.DescribeEvents
import Network.AWS.AWSHealth.DescribeEventsForOrganization
import Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
import Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
import Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
import Network.AWS.AWSHealth.Types.AffectedEntity
import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityAggregate
import Network.AWS.AWSHealth.Types.EntityFilter
import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventAccountFilter
import Network.AWS.AWSHealth.Types.EventAggregate
import Network.AWS.AWSHealth.Types.EventDescription
import Network.AWS.AWSHealth.Types.EventDetails
import Network.AWS.AWSHealth.Types.EventDetailsErrorItem
import Network.AWS.AWSHealth.Types.EventFilter
import Network.AWS.AWSHealth.Types.EventType
import Network.AWS.AWSHealth.Types.EventTypeFilter
import Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEvent
import Network.AWS.AWSHealth.Types.OrganizationEventDetails
import Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEventFilter
