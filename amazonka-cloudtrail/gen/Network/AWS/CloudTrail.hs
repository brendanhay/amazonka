{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CloudTrail
--
-- This is the CloudTrail API Reference. It provides descriptions of
-- actions, data types, common parameters, and common errors for
-- CloudTrail.
--
-- CloudTrail is a web service that records AWS API calls for your AWS
-- account and delivers log files to an Amazon S3 bucket. The recorded
-- information includes the identity of the user, the start time of the AWS
-- API call, the source IP address, the request parameters, and the
-- response elements returned by the service.
--
-- As an alternative to the API, you can use one of the AWS SDKs, which
-- consist of libraries and sample code for various programming languages
-- and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs provide a
-- convenient way to create programmatic access to AWSCloudTrail. For
-- example, the SDKs take care of cryptographically signing requests,
-- managing errors, and retrying requests automatically. For information
-- about the AWS SDKs, including how to download and install them, see the
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services page>.
--
-- See the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-user-guide.html AWS CloudTrail User Guide>
-- for information about the data that is included with each AWS API call
-- listed in the log files.
module Network.AWS.CloudTrail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InsufficientDependencyServiceAccessPermissionException
    _InsufficientDependencyServiceAccessPermissionException,

    -- ** InvalidCloudWatchLogsLogGroupArnException
    _InvalidCloudWatchLogsLogGroupArnException,

    -- ** TrailNotProvidedException
    _TrailNotProvidedException,

    -- ** MaximumNumberOfTrailsExceededException
    _MaximumNumberOfTrailsExceededException,

    -- ** NotOrganizationMasterAccountException
    _NotOrganizationMasterAccountException,

    -- ** InsightNotEnabledException
    _InsightNotEnabledException,

    -- ** KmsKeyNotFoundException
    _KmsKeyNotFoundException,

    -- ** ResourceTypeNotSupportedException
    _ResourceTypeNotSupportedException,

    -- ** InvalidS3PrefixException
    _InvalidS3PrefixException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidInsightSelectorsException
    _InvalidInsightSelectorsException,

    -- ** InvalidLookupAttributesException
    _InvalidLookupAttributesException,

    -- ** OrganizationNotInAllFeaturesModeException
    _OrganizationNotInAllFeaturesModeException,

    -- ** TrailAlreadyExistsException
    _TrailAlreadyExistsException,

    -- ** TagsLimitExceededException
    _TagsLimitExceededException,

    -- ** KmsKeyDisabledException
    _KmsKeyDisabledException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** InsufficientEncryptionPolicyException
    _InsufficientEncryptionPolicyException,

    -- ** InsufficientSnsTopicPolicyException
    _InsufficientSnsTopicPolicyException,

    -- ** S3BucketDoesNotExistException
    _S3BucketDoesNotExistException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** CloudTrailInvalidClientTokenIdException
    _CloudTrailInvalidClientTokenIdException,

    -- ** InvalidS3BucketNameException
    _InvalidS3BucketNameException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** InsufficientS3BucketPolicyException
    _InsufficientS3BucketPolicyException,

    -- ** InvalidEventSelectorsException
    _InvalidEventSelectorsException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidKmsKeyIdException
    _InvalidKmsKeyIdException,

    -- ** InvalidHomeRegionException
    _InvalidHomeRegionException,

    -- ** TrailNotFoundException
    _TrailNotFoundException,

    -- ** CloudWatchLogsDeliveryUnavailableException
    _CloudWatchLogsDeliveryUnavailableException,

    -- ** OrganizationsNotInUseException
    _OrganizationsNotInUseException,

    -- ** InvalidSnsTopicNameException
    _InvalidSnsTopicNameException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidTrailNameException
    _InvalidTrailNameException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** CloudTrailAccessNotEnabledException
    _CloudTrailAccessNotEnabledException,

    -- ** InvalidCloudWatchLogsRoleArnException
    _InvalidCloudWatchLogsRoleArnException,

    -- ** CloudTrailARNInvalidException
    _CloudTrailARNInvalidException,

    -- ** InvalidTokenException
    _InvalidTokenException,

    -- ** InvalidEventCategoryException
    _InvalidEventCategoryException,

    -- ** KmsException
    _KmsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** ListPublicKeys (Paginated)
    ListPublicKeys (ListPublicKeys'),
    newListPublicKeys,
    ListPublicKeysResponse (ListPublicKeysResponse'),
    newListPublicKeysResponse,

    -- ** GetEventSelectors
    GetEventSelectors (GetEventSelectors'),
    newGetEventSelectors,
    GetEventSelectorsResponse (GetEventSelectorsResponse'),
    newGetEventSelectorsResponse,

    -- ** DescribeTrails
    DescribeTrails (DescribeTrails'),
    newDescribeTrails,
    DescribeTrailsResponse (DescribeTrailsResponse'),
    newDescribeTrailsResponse,

    -- ** CreateTrail
    CreateTrail (CreateTrail'),
    newCreateTrail,
    CreateTrailResponse (CreateTrailResponse'),
    newCreateTrailResponse,

    -- ** PutEventSelectors
    PutEventSelectors (PutEventSelectors'),
    newPutEventSelectors,
    PutEventSelectorsResponse (PutEventSelectorsResponse'),
    newPutEventSelectorsResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** GetTrail
    GetTrail (GetTrail'),
    newGetTrail,
    GetTrailResponse (GetTrailResponse'),
    newGetTrailResponse,

    -- ** PutInsightSelectors
    PutInsightSelectors (PutInsightSelectors'),
    newPutInsightSelectors,
    PutInsightSelectorsResponse (PutInsightSelectorsResponse'),
    newPutInsightSelectorsResponse,

    -- ** GetInsightSelectors
    GetInsightSelectors (GetInsightSelectors'),
    newGetInsightSelectors,
    GetInsightSelectorsResponse (GetInsightSelectorsResponse'),
    newGetInsightSelectorsResponse,

    -- ** StopLogging
    StopLogging (StopLogging'),
    newStopLogging,
    StopLoggingResponse (StopLoggingResponse'),
    newStopLoggingResponse,

    -- ** DeleteTrail
    DeleteTrail (DeleteTrail'),
    newDeleteTrail,
    DeleteTrailResponse (DeleteTrailResponse'),
    newDeleteTrailResponse,

    -- ** StartLogging
    StartLogging (StartLogging'),
    newStartLogging,
    StartLoggingResponse (StartLoggingResponse'),
    newStartLoggingResponse,

    -- ** UpdateTrail
    UpdateTrail (UpdateTrail'),
    newUpdateTrail,
    UpdateTrailResponse (UpdateTrailResponse'),
    newUpdateTrailResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ListTrails (Paginated)
    ListTrails (ListTrails'),
    newListTrails,
    ListTrailsResponse (ListTrailsResponse'),
    newListTrailsResponse,

    -- ** GetTrailStatus
    GetTrailStatus (GetTrailStatus'),
    newGetTrailStatus,
    GetTrailStatusResponse (GetTrailStatusResponse'),
    newGetTrailStatusResponse,

    -- ** LookupEvents (Paginated)
    LookupEvents (LookupEvents'),
    newLookupEvents,
    LookupEventsResponse (LookupEventsResponse'),
    newLookupEventsResponse,

    -- * Types

    -- ** EventCategory
    EventCategory (..),

    -- ** InsightType
    InsightType (..),

    -- ** LookupAttributeKey
    LookupAttributeKey (..),

    -- ** ReadWriteType
    ReadWriteType (..),

    -- ** AdvancedEventSelector
    AdvancedEventSelector (AdvancedEventSelector'),
    newAdvancedEventSelector,

    -- ** AdvancedFieldSelector
    AdvancedFieldSelector (AdvancedFieldSelector'),
    newAdvancedFieldSelector,

    -- ** DataResource
    DataResource (DataResource'),
    newDataResource,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventSelector
    EventSelector (EventSelector'),
    newEventSelector,

    -- ** InsightSelector
    InsightSelector (InsightSelector'),
    newInsightSelector,

    -- ** LookupAttribute
    LookupAttribute (LookupAttribute'),
    newLookupAttribute,

    -- ** PublicKey
    PublicKey (PublicKey'),
    newPublicKey,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Trail
    Trail (Trail'),
    newTrail,

    -- ** TrailInfo
    TrailInfo (TrailInfo'),
    newTrailInfo,
  )
where

import Network.AWS.CloudTrail.AddTags
import Network.AWS.CloudTrail.CreateTrail
import Network.AWS.CloudTrail.DeleteTrail
import Network.AWS.CloudTrail.DescribeTrails
import Network.AWS.CloudTrail.GetEventSelectors
import Network.AWS.CloudTrail.GetInsightSelectors
import Network.AWS.CloudTrail.GetTrail
import Network.AWS.CloudTrail.GetTrailStatus
import Network.AWS.CloudTrail.Lens
import Network.AWS.CloudTrail.ListPublicKeys
import Network.AWS.CloudTrail.ListTags
import Network.AWS.CloudTrail.ListTrails
import Network.AWS.CloudTrail.LookupEvents
import Network.AWS.CloudTrail.PutEventSelectors
import Network.AWS.CloudTrail.PutInsightSelectors
import Network.AWS.CloudTrail.RemoveTags
import Network.AWS.CloudTrail.StartLogging
import Network.AWS.CloudTrail.StopLogging
import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.UpdateTrail
import Network.AWS.CloudTrail.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudTrail'.

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
