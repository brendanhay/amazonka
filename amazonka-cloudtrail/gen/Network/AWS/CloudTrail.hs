{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
-- As an alternative to using the API, you can use one of the AWS SDKs,
-- which consist of libraries and sample code for various programming
-- languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs
-- provide a convenient way to create programmatic access to AWSCloudTrail.
-- For example, the SDKs take care of cryptographically signing requests,
-- managing errors, and retrying requests automatically. For information
-- about the AWS SDKs, including how to download and install them, see the
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services page>.
--
-- See the CloudTrail User Guide for information about the data that is
-- included with each AWS API call listed in the log files.
module Network.AWS.CloudTrail
    (
    -- * Service Configuration
      cloudTrail

    -- * Errors
    -- $errors

    -- ** InvalidTimeRangeException
    , _InvalidTimeRangeException

    -- ** InsufficientS3BucketPolicyException
    , _InsufficientS3BucketPolicyException

    -- ** MaximumNumberOfTrailsExceededException
    , _MaximumNumberOfTrailsExceededException

    -- ** UnsupportedOperationException
    , _UnsupportedOperationException

    -- ** KMSKeyDisabledException
    , _KMSKeyDisabledException

    -- ** InsufficientEncryptionPolicyException
    , _InsufficientEncryptionPolicyException

    -- ** InsufficientSNSTopicPolicyException
    , _InsufficientSNSTopicPolicyException

    -- ** InvalidCloudWatchLogsRoleARNException
    , _InvalidCloudWatchLogsRoleARNException

    -- ** TagsLimitExceededException
    , _TagsLimitExceededException

    -- ** CloudTrailARNInvalidException
    , _CloudTrailARNInvalidException

    -- ** InvalidLookupAttributesException
    , _InvalidLookupAttributesException

    -- ** InvalidTrailNameException
    , _InvalidTrailNameException

    -- ** InvalidSNSTopicNameException
    , _InvalidSNSTopicNameException

    -- ** ResourceTypeNotSupportedException
    , _ResourceTypeNotSupportedException

    -- ** CloudWatchLogsDeliveryUnavailableException
    , _CloudWatchLogsDeliveryUnavailableException

    -- ** KMSKeyNotFoundException
    , _KMSKeyNotFoundException

    -- ** TrailNotFoundException
    , _TrailNotFoundException

    -- ** TrailNotProvidedException
    , _TrailNotProvidedException

    -- ** InvalidS3BucketNameException
    , _InvalidS3BucketNameException

    -- ** InvalidCloudWatchLogsLogGroupARNException
    , _InvalidCloudWatchLogsLogGroupARNException

    -- ** S3BucketDoesNotExistException
    , _S3BucketDoesNotExistException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidTagParameterException
    , _InvalidTagParameterException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** InvalidTokenException
    , _InvalidTokenException

    -- ** InvalidMaxResultsException
    , _InvalidMaxResultsException

    -- ** TrailAlreadyExistsException
    , _TrailAlreadyExistsException

    -- ** InvalidS3PrefixException
    , _InvalidS3PrefixException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- ** InvalidKMSKeyIdException
    , _InvalidKMSKeyIdException

    -- ** InvalidHomeRegionException
    , _InvalidHomeRegionException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTrails
    , module Network.AWS.CloudTrail.DescribeTrails

    -- ** ListPublicKeys
    , module Network.AWS.CloudTrail.ListPublicKeys

    -- ** RemoveTags
    , module Network.AWS.CloudTrail.RemoveTags

    -- ** LookupEvents
    , module Network.AWS.CloudTrail.LookupEvents

    -- ** StopLogging
    , module Network.AWS.CloudTrail.StopLogging

    -- ** DeleteTrail
    , module Network.AWS.CloudTrail.DeleteTrail

    -- ** UpdateTrail
    , module Network.AWS.CloudTrail.UpdateTrail

    -- ** CreateTrail
    , module Network.AWS.CloudTrail.CreateTrail

    -- ** GetTrailStatus
    , module Network.AWS.CloudTrail.GetTrailStatus

    -- ** AddTags
    , module Network.AWS.CloudTrail.AddTags

    -- ** ListTags
    , module Network.AWS.CloudTrail.ListTags

    -- ** StartLogging
    , module Network.AWS.CloudTrail.StartLogging

    -- * Types

    -- ** LookupAttributeKey
    , LookupAttributeKey (..)

    -- ** Event
    , Event
    , event
    , eUsername
    , eResources
    , eEventTime
    , eCloudTrailEvent
    , eEventName
    , eEventId

    -- ** LookupAttribute
    , LookupAttribute
    , lookupAttribute
    , laAttributeKey
    , laAttributeValue

    -- ** PublicKey
    , PublicKey
    , publicKey
    , pkFingerprint
    , pkValidityEndTime
    , pkValue
    , pkValidityStartTime

    -- ** Resource
    , Resource
    , resource
    , rResourceType
    , rResourceName

    -- ** ResourceTag
    , ResourceTag
    , resourceTag
    , rResourceId
    , rTagsList

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** Trail
    , Trail
    , trail
    , tLogFileValidationEnabled
    , tTrailARN
    , tS3KeyPrefix
    , tSNSTopicName
    , tCloudWatchLogsLogGroupARN
    , tKMSKeyId
    , tHomeRegion
    , tName
    , tIncludeGlobalServiceEvents
    , tCloudWatchLogsRoleARN
    , tS3BucketName
    , tIsMultiRegionTrail
    ) where

import           Network.AWS.CloudTrail.AddTags
import           Network.AWS.CloudTrail.CreateTrail
import           Network.AWS.CloudTrail.DeleteTrail
import           Network.AWS.CloudTrail.DescribeTrails
import           Network.AWS.CloudTrail.GetTrailStatus
import           Network.AWS.CloudTrail.ListPublicKeys
import           Network.AWS.CloudTrail.ListTags
import           Network.AWS.CloudTrail.LookupEvents
import           Network.AWS.CloudTrail.RemoveTags
import           Network.AWS.CloudTrail.StartLogging
import           Network.AWS.CloudTrail.StopLogging
import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.UpdateTrail
import           Network.AWS.CloudTrail.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudTrail'.
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
