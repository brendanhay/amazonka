{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Health
--
-- The AWS Health API provides programmatic access to the AWS Health
-- information that appears in the
-- <https://phd.aws.amazon.com/phd/home#/ AWS Personal Health Dashboard>.
-- You can use the API operations to get information about AWS Health
-- events that affect your AWS services and resources.
--
-- You must have a Business or Enterprise support plan from
-- <http://aws.amazon.com/premiumsupport/ AWS Support> to use the AWS
-- Health API. If you call the AWS Health API from an AWS account that
-- doesn\'t have a Business or Enterprise support plan, you receive a
-- @SubscriptionRequiredException@ error.
--
-- AWS Health has a single endpoint: health.us-east-1.amazonaws.com
-- (HTTPS). Use this endpoint to call the AWS Health API operations.
--
-- For authentication of requests, AWS Health uses the
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If your AWS account is part of AWS Organizations, you can use the AWS
-- Health organizational view feature. This feature provides a centralized
-- view of AWS Health events across all accounts in your organization. You
-- can aggregate AWS Health events in real time to identify accounts in
-- your organization that are affected by an operational event or get
-- notified of security vulnerabilities. Use the organizational view API
-- operations to enable this feature and return event information. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events>
-- in the /AWS Health User Guide/.
--
-- When you use the AWS Health API operations to return AWS Health events,
-- see the following recommendations:
--
-- -   Use the
--     <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html#AWSHealth-Type-Event-eventScopeCode eventScopeCode>
--     parameter to specify whether to return AWS Health events that are
--     public or account-specific.
--
-- -   Use pagination to view all events from the response. For example, if
--     you call the @DescribeEventsForOrganization@ operation to get all
--     events in your organization, you might receive several page results.
--     Specify the @nextToken@ in the next request to return more results.
module Network.AWS.AWSHealth
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidPaginationToken
    _InvalidPaginationToken,

    -- ** UnsupportedLocale
    _UnsupportedLocale,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** EnableHealthServiceAccessForOrganization
    EnableHealthServiceAccessForOrganization (EnableHealthServiceAccessForOrganization'),
    newEnableHealthServiceAccessForOrganization,
    EnableHealthServiceAccessForOrganizationResponse (EnableHealthServiceAccessForOrganizationResponse'),
    newEnableHealthServiceAccessForOrganizationResponse,

    -- ** DescribeEntityAggregates
    DescribeEntityAggregates (DescribeEntityAggregates'),
    newDescribeEntityAggregates,
    DescribeEntityAggregatesResponse (DescribeEntityAggregatesResponse'),
    newDescribeEntityAggregatesResponse,

    -- ** DisableHealthServiceAccessForOrganization
    DisableHealthServiceAccessForOrganization (DisableHealthServiceAccessForOrganization'),
    newDisableHealthServiceAccessForOrganization,
    DisableHealthServiceAccessForOrganizationResponse (DisableHealthServiceAccessForOrganizationResponse'),
    newDisableHealthServiceAccessForOrganizationResponse,

    -- ** DescribeEventDetailsForOrganization
    DescribeEventDetailsForOrganization (DescribeEventDetailsForOrganization'),
    newDescribeEventDetailsForOrganization,
    DescribeEventDetailsForOrganizationResponse (DescribeEventDetailsForOrganizationResponse'),
    newDescribeEventDetailsForOrganizationResponse,

    -- ** DescribeEventDetails
    DescribeEventDetails (DescribeEventDetails'),
    newDescribeEventDetails,
    DescribeEventDetailsResponse (DescribeEventDetailsResponse'),
    newDescribeEventDetailsResponse,

    -- ** DescribeAffectedAccountsForOrganization (Paginated)
    DescribeAffectedAccountsForOrganization (DescribeAffectedAccountsForOrganization'),
    newDescribeAffectedAccountsForOrganization,
    DescribeAffectedAccountsForOrganizationResponse (DescribeAffectedAccountsForOrganizationResponse'),
    newDescribeAffectedAccountsForOrganizationResponse,

    -- ** DescribeAffectedEntitiesForOrganization (Paginated)
    DescribeAffectedEntitiesForOrganization (DescribeAffectedEntitiesForOrganization'),
    newDescribeAffectedEntitiesForOrganization,
    DescribeAffectedEntitiesForOrganizationResponse (DescribeAffectedEntitiesForOrganizationResponse'),
    newDescribeAffectedEntitiesForOrganizationResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeEventAggregates (Paginated)
    DescribeEventAggregates (DescribeEventAggregates'),
    newDescribeEventAggregates,
    DescribeEventAggregatesResponse (DescribeEventAggregatesResponse'),
    newDescribeEventAggregatesResponse,

    -- ** DescribeHealthServiceStatusForOrganization
    DescribeHealthServiceStatusForOrganization (DescribeHealthServiceStatusForOrganization'),
    newDescribeHealthServiceStatusForOrganization,
    DescribeHealthServiceStatusForOrganizationResponse (DescribeHealthServiceStatusForOrganizationResponse'),
    newDescribeHealthServiceStatusForOrganizationResponse,

    -- ** DescribeEventTypes (Paginated)
    DescribeEventTypes (DescribeEventTypes'),
    newDescribeEventTypes,
    DescribeEventTypesResponse (DescribeEventTypesResponse'),
    newDescribeEventTypesResponse,

    -- ** DescribeAffectedEntities (Paginated)
    DescribeAffectedEntities (DescribeAffectedEntities'),
    newDescribeAffectedEntities,
    DescribeAffectedEntitiesResponse (DescribeAffectedEntitiesResponse'),
    newDescribeAffectedEntitiesResponse,

    -- ** DescribeEventsForOrganization (Paginated)
    DescribeEventsForOrganization (DescribeEventsForOrganization'),
    newDescribeEventsForOrganization,
    DescribeEventsForOrganizationResponse (DescribeEventsForOrganizationResponse'),
    newDescribeEventsForOrganizationResponse,

    -- * Types

    -- ** EntityStatusCode
    EntityStatusCode (..),

    -- ** EventAggregateField
    EventAggregateField (..),

    -- ** EventScopeCode
    EventScopeCode (..),

    -- ** EventStatusCode
    EventStatusCode (..),

    -- ** EventTypeCategory
    EventTypeCategory (..),

    -- ** AffectedEntity
    AffectedEntity (AffectedEntity'),
    newAffectedEntity,

    -- ** DateTimeRange
    DateTimeRange (DateTimeRange'),
    newDateTimeRange,

    -- ** EntityAggregate
    EntityAggregate (EntityAggregate'),
    newEntityAggregate,

    -- ** EntityFilter
    EntityFilter (EntityFilter'),
    newEntityFilter,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventAccountFilter
    EventAccountFilter (EventAccountFilter'),
    newEventAccountFilter,

    -- ** EventAggregate
    EventAggregate (EventAggregate'),
    newEventAggregate,

    -- ** EventDescription
    EventDescription (EventDescription'),
    newEventDescription,

    -- ** EventDetails
    EventDetails (EventDetails'),
    newEventDetails,

    -- ** EventDetailsErrorItem
    EventDetailsErrorItem (EventDetailsErrorItem'),
    newEventDetailsErrorItem,

    -- ** EventFilter
    EventFilter (EventFilter'),
    newEventFilter,

    -- ** EventType
    EventType (EventType'),
    newEventType,

    -- ** EventTypeFilter
    EventTypeFilter (EventTypeFilter'),
    newEventTypeFilter,

    -- ** OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (OrganizationAffectedEntitiesErrorItem'),
    newOrganizationAffectedEntitiesErrorItem,

    -- ** OrganizationEvent
    OrganizationEvent (OrganizationEvent'),
    newOrganizationEvent,

    -- ** OrganizationEventDetails
    OrganizationEventDetails (OrganizationEventDetails'),
    newOrganizationEventDetails,

    -- ** OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (OrganizationEventDetailsErrorItem'),
    newOrganizationEventDetailsErrorItem,

    -- ** OrganizationEventFilter
    OrganizationEventFilter (OrganizationEventFilter'),
    newOrganizationEventFilter,
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
import Network.AWS.AWSHealth.Lens
import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AWSHealth'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
