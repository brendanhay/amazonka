{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Health__ 
--
-- The AWS Health API provides programmatic access to the AWS Health information that appears in the <https://phd.aws.amazon.com/phd/home#/ AWS Personal Health Dashboard> . You can use the API operations to get information about AWS Health events that affect your AWS services and resources.
-- AWS Health has a single endpoint: health.us-east-1.amazonaws.com (HTTPS). Use this endpoint to call the AWS Health API operations.
-- For authentication of requests, AWS Health uses the <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
-- If your AWS account is part of AWS Organizations, you can use the AWS Health organizational view feature. This feature provides a centralized view of AWS Health events across all accounts in your organization. You can aggregate AWS Health events in real time to identify accounts in your organization that are affected by an operational event or get notified of security vulnerabilities. Use the organizational view API operations to enable this feature and return event information. For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
module Network.AWS.AWSHealth
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidPaginationToken
    , _InvalidPaginationToken

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** UnsupportedLocale
    , _UnsupportedLocale

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** EnableHealthServiceAccessForOrganization 
    , module Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization

    -- ** DescribeEntityAggregates 
    , module Network.AWS.AWSHealth.DescribeEntityAggregates

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.AWSHealth.DescribeEvents

    -- ** DescribeEventsForOrganization (Paginated)
    , module Network.AWS.AWSHealth.DescribeEventsForOrganization

    -- ** DescribeAffectedAccountsForOrganization (Paginated)
    , module Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization

    -- ** DescribeEventDetails 
    , module Network.AWS.AWSHealth.DescribeEventDetails

    -- ** DescribeEventAggregates (Paginated)
    , module Network.AWS.AWSHealth.DescribeEventAggregates

    -- ** DescribeAffectedEntities (Paginated)
    , module Network.AWS.AWSHealth.DescribeAffectedEntities

    -- ** DescribeEventTypes (Paginated)
    , module Network.AWS.AWSHealth.DescribeEventTypes

    -- ** DescribeAffectedEntitiesForOrganization (Paginated)
    , module Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization

    -- ** DescribeHealthServiceStatusForOrganization 
    , module Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization

    -- ** DescribeEventDetailsForOrganization 
    , module Network.AWS.AWSHealth.DescribeEventDetailsForOrganization

    -- ** DisableHealthServiceAccessForOrganization 
    , module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization

    -- * Types

    -- ** EventTypeFilter
    , EventTypeFilter (..)
    , mkEventTypeFilter
    , etfEventTypeCategories
    , etfEventTypeCodes
    , etfServices

    -- ** Event
    , Event (..)
    , mkEvent
    , eArn
    , eAvailabilityZone
    , eEndTime
    , eEventScopeCode
    , eEventTypeCategory
    , eEventTypeCode
    , eLastUpdatedTime
    , eRegion
    , eService
    , eStartTime
    , eStatusCode

    -- ** OrganizationEvent
    , OrganizationEvent (..)
    , mkOrganizationEvent
    , oeArn
    , oeEndTime
    , oeEventScopeCode
    , oeEventTypeCategory
    , oeEventTypeCode
    , oeLastUpdatedTime
    , oeRegion
    , oeService
    , oeStartTime
    , oeStatusCode

    -- ** EventDescription
    , EventDescription (..)

    -- ** EntityAggregate
    , EntityAggregate (..)
    , mkEntityAggregate
    , eCount
    , eEventArn

    -- ** EntityStatusCode
    , EntityStatusCode (..)

    -- ** EventDescription
    , EventDescription (..)
    , mkEventDescription
    , edLatestDescription

    -- ** Service
    , Service (..)

    -- ** OrganizationEventDetails
    , OrganizationEventDetails (..)
    , mkOrganizationEventDetails
    , oedAwsAccountId
    , oedEvent
    , oedEventDescription
    , oedEventMetadata

    -- ** EventAggregate
    , EventAggregate (..)
    , mkEventAggregate
    , eaAggregateValue
    , eaCount

    -- ** OrganizationEventDetailsErrorItem
    , OrganizationEventDetailsErrorItem (..)
    , mkOrganizationEventDetailsErrorItem
    , oedeiAwsAccountId
    , oedeiErrorMessage
    , oedeiErrorName
    , oedeiEventArn

    -- ** EntityValue
    , EntityValue (..)

    -- ** Locale
    , Locale (..)

    -- ** EntityUrl
    , EntityUrl (..)

    -- ** EntityFilter
    , EntityFilter (..)
    , mkEntityFilter
    , efEventArns
    , efEntityArns
    , efEntityValues
    , efLastUpdatedTimes
    , efStatusCodes
    , efTags

    -- ** EventDetailsErrorItem
    , EventDetailsErrorItem (..)
    , mkEventDetailsErrorItem
    , edeiErrorMessage
    , edeiErrorName
    , edeiEventArn

    -- ** EventScopeCode
    , EventScopeCode (..)

    -- ** EventDetails
    , EventDetails (..)
    , mkEventDetails
    , edEvent
    , edEventDescription
    , edEventMetadata

    -- ** DateTimeRange
    , DateTimeRange (..)
    , mkDateTimeRange
    , dtrFrom
    , dtrTo

    -- ** HealthServiceAccessStatusForOrganization
    , HealthServiceAccessStatusForOrganization (..)

    -- ** EventArn
    , EventArn (..)

    -- ** OrganizationAffectedEntitiesErrorItem
    , OrganizationAffectedEntitiesErrorItem (..)
    , mkOrganizationAffectedEntitiesErrorItem
    , oaeeiAwsAccountId
    , oaeeiErrorMessage
    , oaeeiErrorName
    , oaeeiEventArn

    -- ** TagValue
    , TagValue (..)

    -- ** EventTypeCode
    , EventTypeCode (..)

    -- ** EventTypeCategory
    , EventTypeCategory (..)

    -- ** EventFilter
    , EventFilter (..)
    , mkEventFilter
    , eAvailabilityZones
    , eEndTimes
    , eEntityArns
    , eEntityValues
    , eEventArns
    , eEventStatusCodes
    , eEventTypeCategories
    , eEventTypeCodes
    , eLastUpdatedTimes
    , eRegions
    , eServices
    , eStartTimes
    , eTags

    -- ** EventType
    , EventType (..)

    -- ** EventType
    , EventType (..)
    , mkEventType
    , etCategory
    , etCode
    , etService

    -- ** NextToken
    , NextToken (..)

    -- ** AccountId
    , AccountId (..)

    -- ** OrganizationEventFilter
    , OrganizationEventFilter (..)
    , mkOrganizationEventFilter
    , oefAwsAccountIds
    , oefEndTime
    , oefEntityArns
    , oefEntityValues
    , oefEventStatusCodes
    , oefEventTypeCategories
    , oefEventTypeCodes
    , oefLastUpdatedTime
    , oefRegions
    , oefServices
    , oefStartTime

    -- ** EventAggregateField
    , EventAggregateField (..)

    -- ** AvailabilityZone
    , AvailabilityZone (..)

    -- ** MetadataKey
    , MetadataKey (..)

    -- ** TagKey
    , TagKey (..)

    -- ** EventStatusCode
    , EventStatusCode (..)

    -- ** EventAccountFilter
    , EventAccountFilter (..)
    , mkEventAccountFilter
    , eafEventArn
    , eafAwsAccountId

    -- ** Region
    , Region (..)

    -- ** MetadataValue
    , MetadataValue (..)

    -- ** EntityArn
    , EntityArn (..)

    -- ** AffectedEntity
    , AffectedEntity (..)
    , mkAffectedEntity
    , aeAwsAccountId
    , aeEntityArn
    , aeEntityUrl
    , aeEntityValue
    , aeEventArn
    , aeLastUpdatedTime
    , aeStatusCode
    , aeTags

    -- ** EventArn
    , EventArn (..)

    -- ** NextToken
    , NextToken (..)

    -- ** Arn
    , Arn (..)

    -- ** AvailabilityZone
    , AvailabilityZone (..)

    -- ** EventTypeCode
    , EventTypeCode (..)

    -- ** Region
    , Region (..)

    -- ** Service
    , Service (..)

    -- ** Locale
    , Locale (..)

    -- ** AwsAccountId
    , AwsAccountId (..)

    -- ** AggregateValue
    , AggregateValue (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Waiters
import Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
import Network.AWS.AWSHealth.DescribeEntityAggregates
import Network.AWS.AWSHealth.DescribeEvents
import Network.AWS.AWSHealth.DescribeEventsForOrganization
import Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
import Network.AWS.AWSHealth.DescribeEventDetails
import Network.AWS.AWSHealth.DescribeEventAggregates
import Network.AWS.AWSHealth.DescribeAffectedEntities
import Network.AWS.AWSHealth.DescribeEventTypes
import Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
import Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
import Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
import Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
import qualified Network.AWS.Prelude as Lude

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
