-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidPaginationToken,
    _ConcurrentModificationException,
    _UnsupportedLocale,

    -- * EventTypeFilter
    EventTypeFilter (..),
    mkEventTypeFilter,
    etfEventTypeCategories,
    etfEventTypeCodes,
    etfServices,

    -- * Event
    Event (..),
    mkEvent,
    eArn,
    eAvailabilityZone,
    eEndTime,
    eEventScopeCode,
    eEventTypeCategory,
    eEventTypeCode,
    eLastUpdatedTime,
    eRegion,
    eService,
    eStartTime,
    eStatusCode,

    -- * OrganizationEvent
    OrganizationEvent (..),
    mkOrganizationEvent,
    oeArn,
    oeEndTime,
    oeEventScopeCode,
    oeEventTypeCategory,
    oeEventTypeCode,
    oeLastUpdatedTime,
    oeRegion,
    oeService,
    oeStartTime,
    oeStatusCode,

    -- * EventDescription
    EventDescription (..),

    -- * EntityAggregate
    EntityAggregate (..),
    mkEntityAggregate,
    eCount,
    eEventArn,

    -- * EntityStatusCode
    EntityStatusCode (..),

    -- * EventDescription
    EventDescription (..),
    mkEventDescription,
    edLatestDescription,

    -- * Service
    Service (..),

    -- * OrganizationEventDetails
    OrganizationEventDetails (..),
    mkOrganizationEventDetails,
    oedAwsAccountId,
    oedEvent,
    oedEventDescription,
    oedEventMetadata,

    -- * String
    String (..),

    -- * EventAggregate
    EventAggregate (..),
    mkEventAggregate,
    eaAggregateValue,
    eaCount,

    -- * OrganizationEventDetailsErrorItem
    OrganizationEventDetailsErrorItem (..),
    mkOrganizationEventDetailsErrorItem,
    oedeiAwsAccountId,
    oedeiErrorMessage,
    oedeiErrorName,
    oedeiEventArn,

    -- * EntityValue
    EntityValue (..),

    -- * Locale
    Locale (..),

    -- * EntityUrl
    EntityUrl (..),

    -- * EntityFilter
    EntityFilter (..),
    mkEntityFilter,
    efEventArns,
    efEntityArns,
    efEntityValues,
    efLastUpdatedTimes,
    efStatusCodes,
    efTags,

    -- * EventDetailsErrorItem
    EventDetailsErrorItem (..),
    mkEventDetailsErrorItem,
    edeiErrorMessage,
    edeiErrorName,
    edeiEventArn,

    -- * EventScopeCode
    EventScopeCode (..),

    -- * EventDetails
    EventDetails (..),
    mkEventDetails,
    edEvent,
    edEventDescription,
    edEventMetadata,

    -- * DateTimeRange
    DateTimeRange (..),
    mkDateTimeRange,
    dtrFrom,
    dtrTo,

    -- * HealthServiceAccessStatusForOrganization
    HealthServiceAccessStatusForOrganization (..),

    -- * EventArn
    EventArn (..),

    -- * OrganizationAffectedEntitiesErrorItem
    OrganizationAffectedEntitiesErrorItem (..),
    mkOrganizationAffectedEntitiesErrorItem,
    oaeeiAwsAccountId,
    oaeeiErrorMessage,
    oaeeiErrorName,
    oaeeiEventArn,

    -- * TagValue
    TagValue (..),

    -- * EventTypeCode
    EventTypeCode (..),

    -- * EventTypeCategory
    EventTypeCategory (..),

    -- * EventFilter
    EventFilter (..),
    mkEventFilter,
    eAvailabilityZones,
    eEndTimes,
    eEntityArns,
    eEntityValues,
    eEventArns,
    eEventStatusCodes,
    eEventTypeCategories,
    eEventTypeCodes,
    eLastUpdatedTimes,
    eRegions,
    eServices,
    eStartTimes,
    eTags,

    -- * EventType
    EventType (..),

    -- * EventType
    EventType (..),
    mkEventType,
    etCategory,
    etCode,
    etService,

    -- * NextToken
    NextToken (..),

    -- * AccountId
    AccountId (..),

    -- * OrganizationEventFilter
    OrganizationEventFilter (..),
    mkOrganizationEventFilter,
    oefAwsAccountIds,
    oefEndTime,
    oefEntityArns,
    oefEntityValues,
    oefEventStatusCodes,
    oefEventTypeCategories,
    oefEventTypeCodes,
    oefLastUpdatedTime,
    oefRegions,
    oefServices,
    oefStartTime,

    -- * EventAggregateField
    EventAggregateField (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * MetadataKey
    MetadataKey (..),

    -- * TagKey
    TagKey (..),

    -- * EventStatusCode
    EventStatusCode (..),

    -- * EventAccountFilter
    EventAccountFilter (..),
    mkEventAccountFilter,
    eafEventArn,
    eafAwsAccountId,

    -- * Region
    Region (..),

    -- * MetadataValue
    MetadataValue (..),

    -- * EntityArn
    EntityArn (..),

    -- * AffectedEntity
    AffectedEntity (..),
    mkAffectedEntity,
    aeAwsAccountId,
    aeEntityArn,
    aeEntityUrl,
    aeEntityValue,
    aeEventArn,
    aeLastUpdatedTime,
    aeStatusCode,
    aeTags,

    -- * EventArn
    EventArn (..),

    -- * NextToken
    NextToken (..),

    -- * Arn
    Arn (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * EventTypeCode
    EventTypeCode (..),

    -- * Region
    Region (..),

    -- * Service
    Service (..),

    -- * Locale
    Locale (..),

    -- * AwsAccountId
    AwsAccountId (..),

    -- * AggregateValue
    AggregateValue (..),
  )
where

import Network.AWS.AWSHealth.Types.AccountId
import Network.AWS.AWSHealth.Types.AffectedEntity
import Network.AWS.AWSHealth.Types.AggregateValue
import Network.AWS.AWSHealth.Types.Arn
import Network.AWS.AWSHealth.Types.AvailabilityZone
import Network.AWS.AWSHealth.Types.AwsAccountId
import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityAggregate
import Network.AWS.AWSHealth.Types.EntityArn
import Network.AWS.AWSHealth.Types.EntityFilter
import Network.AWS.AWSHealth.Types.EntityStatusCode
import Network.AWS.AWSHealth.Types.EntityUrl
import Network.AWS.AWSHealth.Types.EntityValue
import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventAccountFilter
import Network.AWS.AWSHealth.Types.EventAggregate
import Network.AWS.AWSHealth.Types.EventAggregateField
import Network.AWS.AWSHealth.Types.EventArn
import Network.AWS.AWSHealth.Types.EventDescription
import Network.AWS.AWSHealth.Types.EventDetails
import Network.AWS.AWSHealth.Types.EventDetailsErrorItem
import Network.AWS.AWSHealth.Types.EventFilter
import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventType
import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.AWSHealth.Types.EventTypeCode
import Network.AWS.AWSHealth.Types.EventTypeFilter
import Network.AWS.AWSHealth.Types.HealthServiceAccessStatusForOrganization
import Network.AWS.AWSHealth.Types.Locale
import Network.AWS.AWSHealth.Types.MetadataKey
import Network.AWS.AWSHealth.Types.MetadataValue
import Network.AWS.AWSHealth.Types.NextToken
import Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEvent
import Network.AWS.AWSHealth.Types.OrganizationEventDetails
import Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
import Network.AWS.AWSHealth.Types.OrganizationEventFilter
import Network.AWS.AWSHealth.Types.Region
import Network.AWS.AWSHealth.Types.Service
import Network.AWS.AWSHealth.Types.String
import Network.AWS.AWSHealth.Types.TagKey
import Network.AWS.AWSHealth.Types.TagValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-04@ of the Amazon Health APIs and Notifications SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "AWSHealth",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "health",
      Core._svcVersion = "2016-08-04",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "AWSHealth",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The specified pagination token (@nextToken@ ) is not valid.
_InvalidPaginationToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationToken =
  Core._MatchServiceError mkServiceConfig "InvalidPaginationToken"
{-# DEPRECATED _InvalidPaginationToken "Use generic-lens or generic-optics instead." #-}

-- | <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> is already in progress. Wait for the action to complete before trying again. To get the current status, use the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeHealthServiceStatusForOrganization.html DescribeHealthServiceStatusForOrganization> operation.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The specified locale is not supported.
_UnsupportedLocale :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedLocale =
  Core._MatchServiceError mkServiceConfig "UnsupportedLocale"
{-# DEPRECATED _UnsupportedLocale "Use generic-lens or generic-optics instead." #-}
