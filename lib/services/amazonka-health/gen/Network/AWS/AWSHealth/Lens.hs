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

    -- ** DescribeEvents
    describeEvents_locale,
    describeEvents_nextToken,
    describeEvents_filter,
    describeEvents_maxResults,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeEventsForOrganization
    describeEventsForOrganization_locale,
    describeEventsForOrganization_nextToken,
    describeEventsForOrganization_filter,
    describeEventsForOrganization_maxResults,
    describeEventsForOrganizationResponse_nextToken,
    describeEventsForOrganizationResponse_events,
    describeEventsForOrganizationResponse_httpStatus,

    -- ** DescribeAffectedAccountsForOrganization
    describeAffectedAccountsForOrganization_nextToken,
    describeAffectedAccountsForOrganization_maxResults,
    describeAffectedAccountsForOrganization_eventArn,
    describeAffectedAccountsForOrganizationResponse_affectedAccounts,
    describeAffectedAccountsForOrganizationResponse_eventScopeCode,
    describeAffectedAccountsForOrganizationResponse_nextToken,
    describeAffectedAccountsForOrganizationResponse_httpStatus,

    -- ** DescribeEventDetails
    describeEventDetails_locale,
    describeEventDetails_eventArns,
    describeEventDetailsResponse_successfulSet,
    describeEventDetailsResponse_failedSet,
    describeEventDetailsResponse_httpStatus,

    -- ** DescribeEventAggregates
    describeEventAggregates_nextToken,
    describeEventAggregates_filter,
    describeEventAggregates_maxResults,
    describeEventAggregates_aggregateField,
    describeEventAggregatesResponse_nextToken,
    describeEventAggregatesResponse_eventAggregates,
    describeEventAggregatesResponse_httpStatus,

    -- ** DescribeAffectedEntities
    describeAffectedEntities_locale,
    describeAffectedEntities_nextToken,
    describeAffectedEntities_maxResults,
    describeAffectedEntities_filter,
    describeAffectedEntitiesResponse_entities,
    describeAffectedEntitiesResponse_nextToken,
    describeAffectedEntitiesResponse_httpStatus,

    -- ** DescribeEventTypes
    describeEventTypes_locale,
    describeEventTypes_nextToken,
    describeEventTypes_filter,
    describeEventTypes_maxResults,
    describeEventTypesResponse_eventTypes,
    describeEventTypesResponse_nextToken,
    describeEventTypesResponse_httpStatus,

    -- ** DescribeAffectedEntitiesForOrganization
    describeAffectedEntitiesForOrganization_locale,
    describeAffectedEntitiesForOrganization_nextToken,
    describeAffectedEntitiesForOrganization_maxResults,
    describeAffectedEntitiesForOrganization_organizationEntityFilters,
    describeAffectedEntitiesForOrganizationResponse_entities,
    describeAffectedEntitiesForOrganizationResponse_failedSet,
    describeAffectedEntitiesForOrganizationResponse_nextToken,
    describeAffectedEntitiesForOrganizationResponse_httpStatus,

    -- ** DescribeHealthServiceStatusForOrganization
    describeHealthServiceStatusForOrganizationResponse_healthServiceAccessStatusForOrganization,
    describeHealthServiceStatusForOrganizationResponse_httpStatus,

    -- ** DescribeEventDetailsForOrganization
    describeEventDetailsForOrganization_locale,
    describeEventDetailsForOrganization_organizationEventDetailFilters,
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_httpStatus,

    -- ** DisableHealthServiceAccessForOrganization

    -- * Types

    -- ** AffectedEntity
    affectedEntity_lastUpdatedTime,
    affectedEntity_entityValue,
    affectedEntity_entityUrl,
    affectedEntity_awsAccountId,
    affectedEntity_eventArn,
    affectedEntity_entityArn,
    affectedEntity_tags,
    affectedEntity_statusCode,

    -- ** DateTimeRange
    dateTimeRange_to,
    dateTimeRange_from,

    -- ** EntityAggregate
    entityAggregate_count,
    entityAggregate_eventArn,

    -- ** EntityFilter
    entityFilter_statusCodes,
    entityFilter_entityArns,
    entityFilter_entityValues,
    entityFilter_tags,
    entityFilter_lastUpdatedTimes,
    entityFilter_eventArns,

    -- ** Event
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

    -- ** EventAccountFilter
    eventAccountFilter_awsAccountId,
    eventAccountFilter_eventArn,

    -- ** EventAggregate
    eventAggregate_count,
    eventAggregate_aggregateValue,

    -- ** EventDescription
    eventDescription_latestDescription,

    -- ** EventDetails
    eventDetails_event,
    eventDetails_eventDescription,
    eventDetails_eventMetadata,

    -- ** EventDetailsErrorItem
    eventDetailsErrorItem_eventArn,
    eventDetailsErrorItem_errorName,
    eventDetailsErrorItem_errorMessage,

    -- ** EventFilter
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

    -- ** EventType
    eventType_service,
    eventType_category,
    eventType_code,

    -- ** EventTypeFilter
    eventTypeFilter_eventTypeCategories,
    eventTypeFilter_eventTypeCodes,
    eventTypeFilter_services,

    -- ** OrganizationAffectedEntitiesErrorItem
    organizationAffectedEntitiesErrorItem_awsAccountId,
    organizationAffectedEntitiesErrorItem_eventArn,
    organizationAffectedEntitiesErrorItem_errorName,
    organizationAffectedEntitiesErrorItem_errorMessage,

    -- ** OrganizationEvent
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

    -- ** OrganizationEventDetails
    organizationEventDetails_event,
    organizationEventDetails_eventDescription,
    organizationEventDetails_awsAccountId,
    organizationEventDetails_eventMetadata,

    -- ** OrganizationEventDetailsErrorItem
    organizationEventDetailsErrorItem_awsAccountId,
    organizationEventDetailsErrorItem_eventArn,
    organizationEventDetailsErrorItem_errorName,
    organizationEventDetailsErrorItem_errorMessage,

    -- ** OrganizationEventFilter
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
