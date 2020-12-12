{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CloudTrail__
--
-- This is the CloudTrail API Reference. It provides descriptions of actions, data types, common parameters, and common errors for CloudTrail.
-- CloudTrail is a web service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. The recorded information includes the identity of the user, the start time of the AWS API call, the source IP address, the request parameters, and the response elements returned by the service.
-- See the <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-user-guide.html AWS CloudTrail User Guide> for information about the data that is included with each AWS API call listed in the log files.
module Network.AWS.CloudTrail
  ( -- * Service configuration
    cloudTrailService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTrails
    module Network.AWS.CloudTrail.DescribeTrails,

    -- ** ListPublicKeys (Paginated)
    module Network.AWS.CloudTrail.ListPublicKeys,

    -- ** RemoveTags
    module Network.AWS.CloudTrail.RemoveTags,

    -- ** LookupEvents (Paginated)
    module Network.AWS.CloudTrail.LookupEvents,

    -- ** StopLogging
    module Network.AWS.CloudTrail.StopLogging,

    -- ** DeleteTrail
    module Network.AWS.CloudTrail.DeleteTrail,

    -- ** UpdateTrail
    module Network.AWS.CloudTrail.UpdateTrail,

    -- ** CreateTrail
    module Network.AWS.CloudTrail.CreateTrail,

    -- ** PutInsightSelectors
    module Network.AWS.CloudTrail.PutInsightSelectors,

    -- ** GetEventSelectors
    module Network.AWS.CloudTrail.GetEventSelectors,

    -- ** GetTrail
    module Network.AWS.CloudTrail.GetTrail,

    -- ** GetTrailStatus
    module Network.AWS.CloudTrail.GetTrailStatus,

    -- ** AddTags
    module Network.AWS.CloudTrail.AddTags,

    -- ** ListTags (Paginated)
    module Network.AWS.CloudTrail.ListTags,

    -- ** PutEventSelectors
    module Network.AWS.CloudTrail.PutEventSelectors,

    -- ** StartLogging
    module Network.AWS.CloudTrail.StartLogging,

    -- ** ListTrails (Paginated)
    module Network.AWS.CloudTrail.ListTrails,

    -- ** GetInsightSelectors
    module Network.AWS.CloudTrail.GetInsightSelectors,

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
    AdvancedEventSelector (..),
    mkAdvancedEventSelector,
    aesName,
    aesFieldSelectors,

    -- ** AdvancedFieldSelector
    AdvancedFieldSelector (..),
    mkAdvancedFieldSelector,
    afsEndsWith,
    afsNotStartsWith,
    afsEquals,
    afsNotEquals,
    afsNotEndsWith,
    afsStartsWith,
    afsField,

    -- ** DataResource
    DataResource (..),
    mkDataResource,
    drValues,
    drType,

    -- ** Event
    Event (..),
    mkEvent,
    eUsername,
    eResources,
    eEventTime,
    eCloudTrailEvent,
    eEventName,
    eReadOnly,
    eAccessKeyId,
    eEventSource,
    eEventId,

    -- ** EventSelector
    EventSelector (..),
    mkEventSelector,
    esDataResources,
    esReadWriteType,
    esExcludeManagementEventSources,
    esIncludeManagementEvents,

    -- ** InsightSelector
    InsightSelector (..),
    mkInsightSelector,
    isInsightType,

    -- ** LookupAttribute
    LookupAttribute (..),
    mkLookupAttribute,
    laAttributeKey,
    laAttributeValue,

    -- ** PublicKey
    PublicKey (..),
    mkPublicKey,
    pkFingerprint,
    pkValidityEndTime,
    pkValue,
    pkValidityStartTime,

    -- ** Resource
    Resource (..),
    mkResource,
    rResourceType,
    rResourceName,

    -- ** ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rResourceId,
    rTagsList,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** Trail
    Trail (..),
    mkTrail,
    tLogFileValidationEnabled,
    tTrailARN,
    tS3KeyPrefix,
    tHasInsightSelectors,
    tSNSTopicARN,
    tSNSTopicName,
    tCloudWatchLogsLogGroupARN,
    tKMSKeyId,
    tHomeRegion,
    tName,
    tIncludeGlobalServiceEvents,
    tHasCustomEventSelectors,
    tIsOrganizationTrail,
    tCloudWatchLogsRoleARN,
    tS3BucketName,
    tIsMultiRegionTrail,

    -- ** TrailInfo
    TrailInfo (..),
    mkTrailInfo,
    tiTrailARN,
    tiHomeRegion,
    tiName,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import qualified Network.AWS.Prelude as Lude

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
