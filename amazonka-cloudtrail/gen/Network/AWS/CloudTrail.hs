{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- CloudTrail
--
-- This is the CloudTrail API Reference. It provides descriptions of
-- actions, data types, common parameters, and common errors for
-- CloudTrail.
--
-- CloudTrail is a web service that records Amazon Web Services API calls
-- for your Amazon Web Services account and delivers log files to an Amazon
-- S3 bucket. The recorded information includes the identity of the user,
-- the start time of the Amazon Web Services API call, the source IP
-- address, the request parameters, and the response elements returned by
-- the service.
--
-- As an alternative to the API, you can use one of the Amazon Web Services
-- SDKs, which consist of libraries and sample code for various programming
-- languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs
-- provide programmatic access to CloudTrail. For example, the SDKs handle
-- cryptographically signing requests, managing errors, and retrying
-- requests automatically. For more information about the Amazon Web
-- Services SDKs, including how to download and install them, see
-- <http://aws.amazon.com/tools/ Tools to Build on Amazon Web Services>.
--
-- See the
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-user-guide.html CloudTrail User Guide>
-- for information about the data that is included with each Amazon Web
-- Services API call listed in the log files.
module Network.AWS.CloudTrail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TrailNotProvidedException
    _TrailNotProvidedException,

    -- ** InvalidCloudWatchLogsLogGroupArnException
    _InvalidCloudWatchLogsLogGroupArnException,

    -- ** InsufficientDependencyServiceAccessPermissionException
    _InsufficientDependencyServiceAccessPermissionException,

    -- ** MaximumNumberOfTrailsExceededException
    _MaximumNumberOfTrailsExceededException,

    -- ** NotOrganizationMasterAccountException
    _NotOrganizationMasterAccountException,

    -- ** InsightNotEnabledException
    _InsightNotEnabledException,

    -- ** InvalidS3PrefixException
    _InvalidS3PrefixException,

    -- ** InvalidInsightSelectorsException
    _InvalidInsightSelectorsException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** KmsKeyNotFoundException
    _KmsKeyNotFoundException,

    -- ** ResourceTypeNotSupportedException
    _ResourceTypeNotSupportedException,

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

    -- ** InsufficientEncryptionPolicyException
    _InsufficientEncryptionPolicyException,

    -- ** InvalidTagParameterException
    _InvalidTagParameterException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** InsufficientSnsTopicPolicyException
    _InsufficientSnsTopicPolicyException,

    -- ** S3BucketDoesNotExistException
    _S3BucketDoesNotExistException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** CloudTrailInvalidClientTokenIdException
    _CloudTrailInvalidClientTokenIdException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidS3BucketNameException
    _InvalidS3BucketNameException,

    -- ** InsufficientS3BucketPolicyException
    _InsufficientS3BucketPolicyException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** InvalidEventSelectorsException
    _InvalidEventSelectorsException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidKmsKeyIdException
    _InvalidKmsKeyIdException,

    -- ** InvalidHomeRegionException
    _InvalidHomeRegionException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TrailNotFoundException
    _TrailNotFoundException,

    -- ** CloudWatchLogsDeliveryUnavailableException
    _CloudWatchLogsDeliveryUnavailableException,

    -- ** InvalidSnsTopicNameException
    _InvalidSnsTopicNameException,

    -- ** OrganizationsNotInUseException
    _OrganizationsNotInUseException,

    -- ** InvalidTrailNameException
    _InvalidTrailNameException,

    -- ** CloudTrailARNInvalidException
    _CloudTrailARNInvalidException,

    -- ** CloudTrailAccessNotEnabledException
    _CloudTrailAccessNotEnabledException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** InvalidCloudWatchLogsRoleArnException
    _InvalidCloudWatchLogsRoleArnException,

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

    -- ** ListPublicKeys (Paginated)
    ListPublicKeys (ListPublicKeys'),
    newListPublicKeys,
    ListPublicKeysResponse (ListPublicKeysResponse'),
    newListPublicKeysResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

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

    -- ** ListTrails (Paginated)
    ListTrails (ListTrails'),
    newListTrails,
    ListTrailsResponse (ListTrailsResponse'),
    newListTrailsResponse,

    -- ** DeleteTrail
    DeleteTrail (DeleteTrail'),
    newDeleteTrail,
    DeleteTrailResponse (DeleteTrailResponse'),
    newDeleteTrailResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** StopLogging
    StopLogging (StopLogging'),
    newStopLogging,
    StopLoggingResponse (StopLoggingResponse'),
    newStopLoggingResponse,

    -- ** UpdateTrail
    UpdateTrail (UpdateTrail'),
    newUpdateTrail,
    UpdateTrailResponse (UpdateTrailResponse'),
    newUpdateTrailResponse,

    -- ** StartLogging
    StartLogging (StartLogging'),
    newStartLogging,
    StartLoggingResponse (StartLoggingResponse'),
    newStartLoggingResponse,

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
