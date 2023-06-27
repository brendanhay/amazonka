{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccountHasOngoingImportException,
    _AccountNotFoundException,
    _AccountNotRegisteredException,
    _AccountRegisteredException,
    _CannotDelegateManagementAccountException,
    _ChannelARNInvalidException,
    _ChannelAlreadyExistsException,
    _ChannelExistsForEDSException,
    _ChannelMaxLimitExceededException,
    _ChannelNotFoundException,
    _CloudTrailARNInvalidException,
    _CloudTrailAccessNotEnabledException,
    _CloudTrailInvalidClientTokenIdException,
    _CloudWatchLogsDeliveryUnavailableException,
    _ConflictException,
    _DelegatedAdminAccountLimitExceededException,
    _EventDataStoreARNInvalidException,
    _EventDataStoreAlreadyExistsException,
    _EventDataStoreHasOngoingImportException,
    _EventDataStoreMaxLimitExceededException,
    _EventDataStoreNotFoundException,
    _EventDataStoreTerminationProtectedException,
    _ImportNotFoundException,
    _InactiveEventDataStoreException,
    _InactiveQueryException,
    _InsightNotEnabledException,
    _InsufficientDependencyServiceAccessPermissionException,
    _InsufficientEncryptionPolicyException,
    _InsufficientS3BucketPolicyException,
    _InsufficientSnsTopicPolicyException,
    _InvalidCloudWatchLogsLogGroupArnException,
    _InvalidCloudWatchLogsRoleArnException,
    _InvalidDateRangeException,
    _InvalidEventCategoryException,
    _InvalidEventDataStoreCategoryException,
    _InvalidEventDataStoreStatusException,
    _InvalidEventSelectorsException,
    _InvalidHomeRegionException,
    _InvalidImportSourceException,
    _InvalidInsightSelectorsException,
    _InvalidKmsKeyIdException,
    _InvalidLookupAttributesException,
    _InvalidMaxResultsException,
    _InvalidNextTokenException,
    _InvalidParameterCombinationException,
    _InvalidParameterException,
    _InvalidQueryStatementException,
    _InvalidQueryStatusException,
    _InvalidS3BucketNameException,
    _InvalidS3PrefixException,
    _InvalidSnsTopicNameException,
    _InvalidSourceException,
    _InvalidTagParameterException,
    _InvalidTimeRangeException,
    _InvalidTokenException,
    _InvalidTrailNameException,
    _KmsException,
    _KmsKeyDisabledException,
    _KmsKeyNotFoundException,
    _MaxConcurrentQueriesException,
    _MaximumNumberOfTrailsExceededException,
    _NoManagementAccountSLRExistsException,
    _NotOrganizationManagementAccountException,
    _NotOrganizationMasterAccountException,
    _OperationNotPermittedException,
    _OrganizationNotInAllFeaturesModeException,
    _OrganizationsNotInUseException,
    _QueryIdNotFoundException,
    _ResourceARNNotValidException,
    _ResourceNotFoundException,
    _ResourcePolicyNotFoundException,
    _ResourcePolicyNotValidException,
    _ResourceTypeNotSupportedException,
    _S3BucketDoesNotExistException,
    _TagsLimitExceededException,
    _TrailAlreadyExistsException,
    _TrailNotFoundException,
    _TrailNotProvidedException,
    _UnsupportedOperationException,

    -- * DeliveryStatus
    DeliveryStatus (..),

    -- * DestinationType
    DestinationType (..),

    -- * EventCategory
    EventCategory (..),

    -- * EventDataStoreStatus
    EventDataStoreStatus (..),

    -- * ImportFailureStatus
    ImportFailureStatus (..),

    -- * ImportStatus
    ImportStatus (..),

    -- * InsightType
    InsightType (..),

    -- * LookupAttributeKey
    LookupAttributeKey (..),

    -- * QueryStatus
    QueryStatus (..),

    -- * ReadWriteType
    ReadWriteType (..),

    -- * AdvancedEventSelector
    AdvancedEventSelector (..),
    newAdvancedEventSelector,
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- * AdvancedFieldSelector
    AdvancedFieldSelector (..),
    newAdvancedFieldSelector,
    advancedFieldSelector_endsWith,
    advancedFieldSelector_equals,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_notEquals,
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_field,

    -- * Channel
    Channel (..),
    newChannel,
    channel_channelArn,
    channel_name,

    -- * DataResource
    DataResource (..),
    newDataResource,
    dataResource_type,
    dataResource_values,

    -- * Destination
    Destination (..),
    newDestination,
    destination_type,
    destination_location,

    -- * Event
    Event (..),
    newEvent,
    event_accessKeyId,
    event_cloudTrailEvent,
    event_eventId,
    event_eventName,
    event_eventSource,
    event_eventTime,
    event_readOnly,
    event_resources,
    event_username,

    -- * EventDataStore
    EventDataStore (..),
    newEventDataStore,
    eventDataStore_advancedEventSelectors,
    eventDataStore_createdTimestamp,
    eventDataStore_eventDataStoreArn,
    eventDataStore_multiRegionEnabled,
    eventDataStore_name,
    eventDataStore_organizationEnabled,
    eventDataStore_retentionPeriod,
    eventDataStore_status,
    eventDataStore_terminationProtectionEnabled,
    eventDataStore_updatedTimestamp,

    -- * EventSelector
    EventSelector (..),
    newEventSelector,
    eventSelector_dataResources,
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_readWriteType,

    -- * ImportFailureListItem
    ImportFailureListItem (..),
    newImportFailureListItem,
    importFailureListItem_errorMessage,
    importFailureListItem_errorType,
    importFailureListItem_lastUpdatedTime,
    importFailureListItem_location,
    importFailureListItem_status,

    -- * ImportSource
    ImportSource (..),
    newImportSource,
    importSource_s3,

    -- * ImportStatistics
    ImportStatistics (..),
    newImportStatistics,
    importStatistics_eventsCompleted,
    importStatistics_failedEntries,
    importStatistics_filesCompleted,
    importStatistics_prefixesCompleted,
    importStatistics_prefixesFound,

    -- * ImportsListItem
    ImportsListItem (..),
    newImportsListItem,
    importsListItem_createdTimestamp,
    importsListItem_destinations,
    importsListItem_importId,
    importsListItem_importStatus,
    importsListItem_updatedTimestamp,

    -- * IngestionStatus
    IngestionStatus (..),
    newIngestionStatus,
    ingestionStatus_latestIngestionAttemptEventID,
    ingestionStatus_latestIngestionAttemptTime,
    ingestionStatus_latestIngestionErrorCode,
    ingestionStatus_latestIngestionSuccessEventID,
    ingestionStatus_latestIngestionSuccessTime,

    -- * InsightSelector
    InsightSelector (..),
    newInsightSelector,
    insightSelector_insightType,

    -- * LookupAttribute
    LookupAttribute (..),
    newLookupAttribute,
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- * PublicKey
    PublicKey (..),
    newPublicKey,
    publicKey_fingerprint,
    publicKey_validityEndTime,
    publicKey_validityStartTime,
    publicKey_value,

    -- * Query
    Query (..),
    newQuery,
    query_creationTime,
    query_queryId,
    query_queryStatus,

    -- * QueryStatistics
    QueryStatistics (..),
    newQueryStatistics,
    queryStatistics_bytesScanned,
    queryStatistics_resultsCount,
    queryStatistics_totalResultsCount,

    -- * QueryStatisticsForDescribeQuery
    QueryStatisticsForDescribeQuery (..),
    newQueryStatisticsForDescribeQuery,
    queryStatisticsForDescribeQuery_bytesScanned,
    queryStatisticsForDescribeQuery_creationTime,
    queryStatisticsForDescribeQuery_eventsMatched,
    queryStatisticsForDescribeQuery_eventsScanned,
    queryStatisticsForDescribeQuery_executionTimeInMillis,

    -- * Resource
    Resource (..),
    newResource,
    resource_resourceName,
    resource_resourceType,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_resourceId,
    resourceTag_tagsList,

    -- * S3ImportSource
    S3ImportSource (..),
    newS3ImportSource,
    s3ImportSource_s3LocationUri,
    s3ImportSource_s3BucketRegion,
    s3ImportSource_s3BucketAccessRoleArn,

    -- * SourceConfig
    SourceConfig (..),
    newSourceConfig,
    sourceConfig_advancedEventSelectors,
    sourceConfig_applyToAllRegions,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Trail
    Trail (..),
    newTrail,
    trail_cloudWatchLogsLogGroupArn,
    trail_cloudWatchLogsRoleArn,
    trail_hasCustomEventSelectors,
    trail_hasInsightSelectors,
    trail_homeRegion,
    trail_includeGlobalServiceEvents,
    trail_isMultiRegionTrail,
    trail_isOrganizationTrail,
    trail_kmsKeyId,
    trail_logFileValidationEnabled,
    trail_name,
    trail_s3BucketName,
    trail_s3KeyPrefix,
    trail_snsTopicARN,
    trail_snsTopicName,
    trail_trailARN,

    -- * TrailInfo
    TrailInfo (..),
    newTrailInfo,
    trailInfo_homeRegion,
    trailInfo_name,
    trailInfo_trailARN,
  )
where

import Amazonka.CloudTrail.Types.AdvancedEventSelector
import Amazonka.CloudTrail.Types.AdvancedFieldSelector
import Amazonka.CloudTrail.Types.Channel
import Amazonka.CloudTrail.Types.DataResource
import Amazonka.CloudTrail.Types.DeliveryStatus
import Amazonka.CloudTrail.Types.Destination
import Amazonka.CloudTrail.Types.DestinationType
import Amazonka.CloudTrail.Types.Event
import Amazonka.CloudTrail.Types.EventCategory
import Amazonka.CloudTrail.Types.EventDataStore
import Amazonka.CloudTrail.Types.EventDataStoreStatus
import Amazonka.CloudTrail.Types.EventSelector
import Amazonka.CloudTrail.Types.ImportFailureListItem
import Amazonka.CloudTrail.Types.ImportFailureStatus
import Amazonka.CloudTrail.Types.ImportSource
import Amazonka.CloudTrail.Types.ImportStatistics
import Amazonka.CloudTrail.Types.ImportStatus
import Amazonka.CloudTrail.Types.ImportsListItem
import Amazonka.CloudTrail.Types.IngestionStatus
import Amazonka.CloudTrail.Types.InsightSelector
import Amazonka.CloudTrail.Types.InsightType
import Amazonka.CloudTrail.Types.LookupAttribute
import Amazonka.CloudTrail.Types.LookupAttributeKey
import Amazonka.CloudTrail.Types.PublicKey
import Amazonka.CloudTrail.Types.Query
import Amazonka.CloudTrail.Types.QueryStatistics
import Amazonka.CloudTrail.Types.QueryStatisticsForDescribeQuery
import Amazonka.CloudTrail.Types.QueryStatus
import Amazonka.CloudTrail.Types.ReadWriteType
import Amazonka.CloudTrail.Types.Resource
import Amazonka.CloudTrail.Types.ResourceTag
import Amazonka.CloudTrail.Types.S3ImportSource
import Amazonka.CloudTrail.Types.SourceConfig
import Amazonka.CloudTrail.Types.Tag
import Amazonka.CloudTrail.Types.Trail
import Amazonka.CloudTrail.Types.TrailInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudTrail",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudtrail",
      Core.signingName = "cloudtrail",
      Core.version = "2013-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudTrail",
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

-- | This exception is thrown when you start a new import and a previous
-- import is still in progress.
_AccountHasOngoingImportException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountHasOngoingImportException =
  Core._MatchServiceError
    defaultService
    "AccountHasOngoingImportException"

-- | This exception is thrown when the specified account is not found or not
-- part of an organization.
_AccountNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountNotFoundException =
  Core._MatchServiceError
    defaultService
    "AccountNotFoundException"

-- | This exception is thrown when the specified account is not registered as
-- the CloudTrail delegated administrator.
_AccountNotRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "AccountNotRegisteredException"

-- | This exception is thrown when the account is already registered as the
-- CloudTrail delegated administrator.
_AccountRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountRegisteredException =
  Core._MatchServiceError
    defaultService
    "AccountRegisteredException"

-- | This exception is thrown when the management account of an organization
-- is registered as the CloudTrail delegated administrator.
_CannotDelegateManagementAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CannotDelegateManagementAccountException =
  Core._MatchServiceError
    defaultService
    "CannotDelegateManagementAccountException"

-- | This exception is thrown when the specified value of @ChannelARN@ is not
-- valid.
_ChannelARNInvalidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelARNInvalidException =
  Core._MatchServiceError
    defaultService
    "ChannelARNInvalidException"

-- | This exception is thrown when the provided channel already exists.
_ChannelAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ChannelAlreadyExistsException"

-- | This exception is thrown when the specified event data store cannot yet
-- be deleted because it is in use by a channel.
_ChannelExistsForEDSException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelExistsForEDSException =
  Core._MatchServiceError
    defaultService
    "ChannelExistsForEDSException"

-- | This exception is thrown when the maximum number of channels limit is
-- exceeded.
_ChannelMaxLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelMaxLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ChannelMaxLimitExceededException"

-- | This exception is thrown when CloudTrail cannot find the specified
-- channel.
_ChannelNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ChannelNotFoundException =
  Core._MatchServiceError
    defaultService
    "ChannelNotFoundException"

-- | This exception is thrown when an operation is called with a trail ARN
-- that is not valid. The following is the format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- This exception is also thrown when you call @AddTags@ or @RemoveTags@ on
-- a trail, event data store, or channel with a resource ARN that is not
-- valid.
--
-- The following is the format of an event data store ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:eventdatastore\/EXAMPLE-f852-4e8f-8bd1-bcf6cEXAMPLE@
--
-- The following is the format of a channel ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/01234567890@
_CloudTrailARNInvalidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudTrailARNInvalidException =
  Core._MatchServiceError
    defaultService
    "CloudTrailARNInvalidException"

-- | This exception is thrown when trusted access has not been enabled
-- between CloudTrail and Organizations. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Enabling Trusted Access with Other Amazon Web Services Services>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>.
_CloudTrailAccessNotEnabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudTrailAccessNotEnabledException =
  Core._MatchServiceError
    defaultService
    "CloudTrailAccessNotEnabledException"

-- | This exception is thrown when a call results in the
-- @InvalidClientTokenId@ error code. This can occur when you are creating
-- or updating a trail to send notifications to an Amazon SNS topic that is
-- in a suspended Amazon Web Services account.
_CloudTrailInvalidClientTokenIdException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudTrailInvalidClientTokenIdException =
  Core._MatchServiceError
    defaultService
    "CloudTrailInvalidClientTokenIdException"

-- | Cannot set a CloudWatch Logs delivery for this Region.
_CloudWatchLogsDeliveryUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudWatchLogsDeliveryUnavailableException =
  Core._MatchServiceError
    defaultService
    "CloudWatchLogsDeliveryUnavailableException"

-- | This exception is thrown when the specified resource is not ready for an
-- operation. This can occur when you try to run an operation on a resource
-- before CloudTrail has time to fully load the resource, or because
-- another operation is modifying the resource. If this exception occurs,
-- wait a few minutes, and then try the operation again.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | This exception is thrown when the maximum number of CloudTrail delegated
-- administrators is reached.
_DelegatedAdminAccountLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DelegatedAdminAccountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DelegatedAdminAccountLimitExceededException"

-- | The specified event data store ARN is not valid or does not map to an
-- event data store in your account.
_EventDataStoreARNInvalidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreARNInvalidException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreARNInvalidException"

-- | An event data store with that name already exists.
_EventDataStoreAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreAlreadyExistsException"

-- | This exception is thrown when you try to update or delete an event data
-- store that currently has an import in progress.
_EventDataStoreHasOngoingImportException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreHasOngoingImportException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreHasOngoingImportException"

-- | Your account has used the maximum number of event data stores.
_EventDataStoreMaxLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreMaxLimitExceededException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreMaxLimitExceededException"

-- | The specified event data store was not found.
_EventDataStoreNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreNotFoundException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreNotFoundException"

-- | The event data store cannot be deleted because termination protection is
-- enabled for it.
_EventDataStoreTerminationProtectedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventDataStoreTerminationProtectedException =
  Core._MatchServiceError
    defaultService
    "EventDataStoreTerminationProtectedException"

-- | The specified import was not found.
_ImportNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ImportNotFoundException =
  Core._MatchServiceError
    defaultService
    "ImportNotFoundException"

-- | The event data store is inactive.
_InactiveEventDataStoreException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InactiveEventDataStoreException =
  Core._MatchServiceError
    defaultService
    "InactiveEventDataStoreException"

-- | The specified query cannot be canceled because it is in the @FINISHED@,
-- @FAILED@, @TIMED_OUT@, or @CANCELLED@ state.
_InactiveQueryException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InactiveQueryException =
  Core._MatchServiceError
    defaultService
    "InactiveQueryException"

-- | If you run @GetInsightSelectors@ on a trail that does not have Insights
-- events enabled, the operation throws the exception
-- @InsightNotEnabledException@.
_InsightNotEnabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsightNotEnabledException =
  Core._MatchServiceError
    defaultService
    "InsightNotEnabledException"

-- | This exception is thrown when the IAM identity that is used to create
-- the organization resource lacks one or more required permissions for
-- creating an organization resource in a required service.
_InsufficientDependencyServiceAccessPermissionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDependencyServiceAccessPermissionException =
  Core._MatchServiceError
    defaultService
    "InsufficientDependencyServiceAccessPermissionException"

-- | This exception is thrown when the policy on the S3 bucket or KMS key
-- does not have sufficient permissions for the operation.
_InsufficientEncryptionPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientEncryptionPolicyException =
  Core._MatchServiceError
    defaultService
    "InsufficientEncryptionPolicyException"

-- | This exception is thrown when the policy on the S3 bucket is not
-- sufficient.
_InsufficientS3BucketPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientS3BucketPolicyException =
  Core._MatchServiceError
    defaultService
    "InsufficientS3BucketPolicyException"

-- | This exception is thrown when the policy on the Amazon SNS topic is not
-- sufficient.
_InsufficientSnsTopicPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientSnsTopicPolicyException =
  Core._MatchServiceError
    defaultService
    "InsufficientSnsTopicPolicyException"

-- | This exception is thrown when the provided CloudWatch Logs log group is
-- not valid.
_InvalidCloudWatchLogsLogGroupArnException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCloudWatchLogsLogGroupArnException =
  Core._MatchServiceError
    defaultService
    "InvalidCloudWatchLogsLogGroupArnException"

-- | This exception is thrown when the provided role is not valid.
_InvalidCloudWatchLogsRoleArnException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCloudWatchLogsRoleArnException =
  Core._MatchServiceError
    defaultService
    "InvalidCloudWatchLogsRoleArnException"

-- | A date range for the query was specified that is not valid. Be sure that
-- the start time is chronologically before the end time. For more
-- information about writing a query, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-create-edit-query.html Create or edit a query>
-- in the /CloudTrail User Guide/.
_InvalidDateRangeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDateRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidDateRangeException"

-- | Occurs if an event category that is not valid is specified as a value of
-- @EventCategory@.
_InvalidEventCategoryException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventCategoryException =
  Core._MatchServiceError
    defaultService
    "InvalidEventCategoryException"

-- | This exception is thrown when event categories of specified event data
-- stores are not valid.
_InvalidEventDataStoreCategoryException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventDataStoreCategoryException =
  Core._MatchServiceError
    defaultService
    "InvalidEventDataStoreCategoryException"

-- | The event data store is not in a status that supports the operation.
_InvalidEventDataStoreStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventDataStoreStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidEventDataStoreStatusException"

-- | This exception is thrown when the @PutEventSelectors@ operation is
-- called with a number of event selectors, advanced event selectors, or
-- data resources that is not valid. The combination of event selectors or
-- advanced event selectors and data resources is not valid. A trail can
-- have up to 5 event selectors. If a trail uses advanced event selectors,
-- a maximum of 500 total values for all conditions in all advanced event
-- selectors is allowed. A trail is limited to 250 data resources. These
-- data resources can be distributed across event selectors, but the
-- overall total cannot exceed 250.
--
-- You can:
--
-- -   Specify a valid number of event selectors (1 to 5) for a trail.
--
-- -   Specify a valid number of data resources (1 to 250) for an event
--     selector. The limit of number of resources on an individual event
--     selector is configurable up to 250. However, this upper limit is
--     allowed only if the total number of data resources does not exceed
--     250 across all event selectors for a trail.
--
-- -   Specify up to 500 values for all conditions in all advanced event
--     selectors for a trail.
--
-- -   Specify a valid value for a parameter. For example, specifying the
--     @ReadWriteType@ parameter with a value of @read-only@ is not valid.
_InvalidEventSelectorsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventSelectorsException =
  Core._MatchServiceError
    defaultService
    "InvalidEventSelectorsException"

-- | This exception is thrown when an operation is called on a trail from a
-- Region other than the Region in which the trail was created.
_InvalidHomeRegionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidHomeRegionException =
  Core._MatchServiceError
    defaultService
    "InvalidHomeRegionException"

-- | This exception is thrown when the provided source S3 bucket is not valid
-- for import.
_InvalidImportSourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidImportSourceException =
  Core._MatchServiceError
    defaultService
    "InvalidImportSourceException"

-- | The formatting or syntax of the @InsightSelectors@ JSON statement in
-- your @PutInsightSelectors@ or @GetInsightSelectors@ request is not
-- valid, or the specified insight type in the @InsightSelectors@ statement
-- is not a valid insight type.
_InvalidInsightSelectorsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInsightSelectorsException =
  Core._MatchServiceError
    defaultService
    "InvalidInsightSelectorsException"

-- | This exception is thrown when the KMS key ARN is not valid.
_InvalidKmsKeyIdException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKmsKeyIdException =
  Core._MatchServiceError
    defaultService
    "InvalidKmsKeyIdException"

-- | Occurs when a lookup attribute is specified that is not valid.
_InvalidLookupAttributesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLookupAttributesException =
  Core._MatchServiceError
    defaultService
    "InvalidLookupAttributesException"

-- | This exception is thrown if the limit specified is not valid.
_InvalidMaxResultsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxResultsException"

-- | A token that is not valid, or a token that was previously used in a
-- request with different parameters. This exception is thrown if the token
-- is not valid.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | This exception is thrown when the combination of parameters provided is
-- not valid.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The request includes a parameter that is not valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The query that was submitted has validation errors, or uses incorrect
-- syntax or unsupported keywords. For more information about writing a
-- query, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-create-edit-query.html Create or edit a query>
-- in the /CloudTrail User Guide/.
_InvalidQueryStatementException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidQueryStatementException =
  Core._MatchServiceError
    defaultService
    "InvalidQueryStatementException"

-- | The query status is not valid for the operation.
_InvalidQueryStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidQueryStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidQueryStatusException"

-- | This exception is thrown when the provided S3 bucket name is not valid.
_InvalidS3BucketNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidS3BucketNameException =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketNameException"

-- | This exception is thrown when the provided S3 prefix is not valid.
_InvalidS3PrefixException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidS3PrefixException =
  Core._MatchServiceError
    defaultService
    "InvalidS3PrefixException"

-- | This exception is thrown when the provided SNS topic name is not valid.
_InvalidSnsTopicNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSnsTopicNameException =
  Core._MatchServiceError
    defaultService
    "InvalidSnsTopicNameException"

-- | This exception is thrown when the specified value of @Source@ is not
-- valid.
_InvalidSourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSourceException =
  Core._MatchServiceError
    defaultService
    "InvalidSourceException"

-- | This exception is thrown when the specified tag key or values are not
-- valid. It can also occur if there are duplicate tags or too many tags on
-- the resource.
_InvalidTagParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | Occurs if the timestamp values are not valid. Either the start time
-- occurs after the end time, or the time range is outside the range of
-- possible values.
_InvalidTimeRangeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTimeRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | Reserved for future use.
_InvalidTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidTokenException"

-- | This exception is thrown when the provided trail name is not valid.
-- Trail names must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
_InvalidTrailNameException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTrailNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTrailNameException"

-- | This exception is thrown when there is an issue with the specified KMS
-- key and the trail or event data store can\'t be updated.
_KmsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KmsException =
  Core._MatchServiceError
    defaultService
    "KmsException"

-- | This exception is no longer in use.
_KmsKeyDisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KmsKeyDisabledException =
  Core._MatchServiceError
    defaultService
    "KmsKeyDisabledException"

-- | This exception is thrown when the KMS key does not exist, when the S3
-- bucket and the KMS key are not in the same Region, or when the KMS key
-- associated with the Amazon SNS topic either does not exist or is not in
-- the same Region.
_KmsKeyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KmsKeyNotFoundException =
  Core._MatchServiceError
    defaultService
    "KmsKeyNotFoundException"

-- | You are already running the maximum number of concurrent queries. Wait a
-- minute for some queries to finish, and then run the query again.
_MaxConcurrentQueriesException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MaxConcurrentQueriesException =
  Core._MatchServiceError
    defaultService
    "MaxConcurrentQueriesException"

-- | This exception is thrown when the maximum number of trails is reached.
_MaximumNumberOfTrailsExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MaximumNumberOfTrailsExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumNumberOfTrailsExceededException"

-- | This exception is thrown when the management account does not have a
-- service-linked role.
_NoManagementAccountSLRExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoManagementAccountSLRExistsException =
  Core._MatchServiceError
    defaultService
    "NoManagementAccountSLRExistsException"

-- | This exception is thrown when the account making the request is not the
-- organization\'s management account.
_NotOrganizationManagementAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotOrganizationManagementAccountException =
  Core._MatchServiceError
    defaultService
    "NotOrganizationManagementAccountException"

-- | This exception is thrown when the Amazon Web Services account making the
-- request to create or update an organization trail or event data store is
-- not the management account for an organization in Organizations. For
-- more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>
-- or
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-event-data-store.html Create an event data store>.
_NotOrganizationMasterAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotOrganizationMasterAccountException =
  Core._MatchServiceError
    defaultService
    "NotOrganizationMasterAccountException"

-- | This exception is thrown when the requested operation is not permitted.
_OperationNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | This exception is thrown when Organizations is not configured to support
-- all features. All features must be enabled in Organizations to support
-- creating an organization trail or event data store.
_OrganizationNotInAllFeaturesModeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationNotInAllFeaturesModeException =
  Core._MatchServiceError
    defaultService
    "OrganizationNotInAllFeaturesModeException"

-- | This exception is thrown when the request is made from an Amazon Web
-- Services account that is not a member of an organization. To make this
-- request, sign in using the credentials of an account that belongs to an
-- organization.
_OrganizationsNotInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OrganizationsNotInUseException =
  Core._MatchServiceError
    defaultService
    "OrganizationsNotInUseException"

-- | The query ID does not exist or does not map to a query.
_QueryIdNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_QueryIdNotFoundException =
  Core._MatchServiceError
    defaultService
    "QueryIdNotFoundException"

-- | This exception is thrown when the provided resource does not exist, or
-- the ARN format of the resource is not valid. The following is the valid
-- format for a resource ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
_ResourceARNNotValidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceARNNotValidException =
  Core._MatchServiceError
    defaultService
    "ResourceARNNotValidException"

-- | This exception is thrown when the specified resource is not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is thrown when the specified resource policy is not
-- found.
_ResourcePolicyNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourcePolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyNotFoundException"

-- | This exception is thrown when the resouce-based policy has syntax
-- errors, or contains a principal that is not valid.
--
-- The following are requirements for the resource policy:
--
-- -   Contains only one action: cloudtrail-data:PutAuditEvents
--
-- -   Contains at least one statement. The policy can have a maximum of 20
--     statements.
--
-- -   Each statement contains at least one principal. A statement can have
--     a maximum of 50 principals.
_ResourcePolicyNotValidException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourcePolicyNotValidException =
  Core._MatchServiceError
    defaultService
    "ResourcePolicyNotValidException"

-- | This exception is thrown when the specified resource type is not
-- supported by CloudTrail.
_ResourceTypeNotSupportedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ResourceTypeNotSupportedException"

-- | This exception is thrown when the specified S3 bucket does not exist.
_S3BucketDoesNotExistException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_S3BucketDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "S3BucketDoesNotExistException"

-- | The number of tags per trail, event data store, or channel has exceeded
-- the permitted amount. Currently, the limit is 50.
_TagsLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagsLimitExceededException"

-- | This exception is thrown when the specified trail already exists.
_TrailAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrailAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TrailAlreadyExistsException"

-- | This exception is thrown when the trail with the given name is not
-- found.
_TrailNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrailNotFoundException =
  Core._MatchServiceError
    defaultService
    "TrailNotFoundException"

-- | This exception is no longer in use.
_TrailNotProvidedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrailNotProvidedException =
  Core._MatchServiceError
    defaultService
    "TrailNotProvidedException"

-- | This exception is thrown when the requested operation is not supported.
_UnsupportedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
