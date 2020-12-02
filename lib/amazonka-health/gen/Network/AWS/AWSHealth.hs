{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Health__
--
-- The AWS Health API provides programmatic access to the AWS Health information that is presented in the <https://phd.aws.amazon.com/phd/home#/ AWS Personal Health Dashboard> . You can get information about events that affect your AWS resources:
--
--     * 'DescribeEvents' : Summary information about events.
--
--     * 'DescribeEventDetails' : Detailed information about one or more events.
--
--     * 'DescribeAffectedEntities' : Information about AWS resources that are affected by one or more events.
--
--
--
-- In addition, these operations provide information about event types and summary counts of events or affected entities:
--
--     * 'DescribeEventTypes' : Information about the kinds of events that AWS Health tracks.
--
--     * 'DescribeEventAggregates' : A count of the number of events that meet specified criteria.
--
--     * 'DescribeEntityAggregates' : A count of the number of affected entities that meet specified criteria.
--
--
--
-- The Health API requires a Business or Enterprise support plan from <http://aws.amazon.com/premiumsupport/ AWS Support> . Calling the Health API from an account that does not have a Business or Enterprise support plan causes a @SubscriptionRequiredException@ .
--
-- For authentication of requests, AWS Health uses the <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- See the <http://docs.aws.amazon.com/health/latest/ug/what-is-aws-health.html AWS Health User Guide> for information about how to use the API.
--
-- __Service Endpoint__
--
-- The HTTP endpoint for the AWS Health API is:
--
--     * https://health.us-east-1.amazonaws.com
--
--
--
module Network.AWS.AWSHealth
    (
    -- * Service Configuration
      awsHealth

    -- * Errors
    -- $errors

    -- ** InvalidPaginationToken
    , _InvalidPaginationToken

    -- ** UnsupportedLocale
    , _UnsupportedLocale

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeEntityAggregates
    , module Network.AWS.AWSHealth.DescribeEntityAggregates

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.AWSHealth.DescribeEvents

    -- ** DescribeEventDetails
    , module Network.AWS.AWSHealth.DescribeEventDetails

    -- ** DescribeEventAggregates (Paginated)
    , module Network.AWS.AWSHealth.DescribeEventAggregates

    -- ** DescribeAffectedEntities (Paginated)
    , module Network.AWS.AWSHealth.DescribeAffectedEntities

    -- ** DescribeEventTypes (Paginated)
    , module Network.AWS.AWSHealth.DescribeEventTypes

    -- * Types

    -- ** EntityStatusCode
    , EntityStatusCode (..)

    -- ** EventAggregateField
    , EventAggregateField (..)

    -- ** EventStatusCode
    , EventStatusCode (..)

    -- ** EventTypeCategory
    , EventTypeCategory (..)

    -- ** AffectedEntity
    , AffectedEntity
    , affectedEntity
    , aeLastUpdatedTime
    , aeEntityValue
    , aeAwsAccountId
    , aeEventARN
    , aeEntityARN
    , aeTags
    , aeStatusCode

    -- ** DateTimeRange
    , DateTimeRange
    , dateTimeRange
    , dtrTo
    , dtrFrom

    -- ** EntityAggregate
    , EntityAggregate
    , entityAggregate
    , eCount
    , eEventARN

    -- ** EntityFilter
    , EntityFilter
    , entityFilter
    , eStatusCodes
    , eEntityARNs
    , eEntityValues
    , eTags
    , eLastUpdatedTimes
    , eEventARNs

    -- ** Event
    , Event
    , event
    , eLastUpdatedTime
    , eArn
    , eService
    , eStartTime
    , eEventTypeCode
    , eEventTypeCategory
    , eAvailabilityZone
    , eEndTime
    , eRegion
    , eStatusCode

    -- ** EventAggregate
    , EventAggregate
    , eventAggregate
    , eaCount
    , eaAggregateValue

    -- ** EventDescription
    , EventDescription
    , eventDescription
    , edLatestDescription

    -- ** EventDetails
    , EventDetails
    , eventDetails
    , edEvent
    , edEventDescription
    , edEventMetadata

    -- ** EventDetailsErrorItem
    , EventDetailsErrorItem
    , eventDetailsErrorItem
    , edeiEventARN
    , edeiErrorName
    , edeiErrorMessage

    -- ** EventFilter
    , EventFilter
    , eventFilter
    , efEventARNs
    , efEventTypeCategories
    , efEventTypeCodes
    , efRegions
    , efEventStatusCodes
    , efEndTimes
    , efAvailabilityZones
    , efEntityARNs
    , efEntityValues
    , efStartTimes
    , efServices
    , efTags
    , efLastUpdatedTimes

    -- ** EventType
    , EventType
    , eventType
    , etService
    , etCategory
    , etCode

    -- ** EventTypeFilter
    , EventTypeFilter
    , eventTypeFilter
    , etfEventTypeCategories
    , etfEventTypeCodes
    , etfServices
    ) where

import Network.AWS.AWSHealth.DescribeAffectedEntities
import Network.AWS.AWSHealth.DescribeEntityAggregates
import Network.AWS.AWSHealth.DescribeEventAggregates
import Network.AWS.AWSHealth.DescribeEventDetails
import Network.AWS.AWSHealth.DescribeEvents
import Network.AWS.AWSHealth.DescribeEventTypes
import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AWSHealth'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
