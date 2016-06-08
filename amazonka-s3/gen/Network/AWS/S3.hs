{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a simple web services interface that you can use to store and retrieve any amount of data, at any time, from anywhere on the web. It gives any developer access to the same highly scalable, reliable, fast, inexpensive data storage infrastructure that Amazon uses to run its own global network of web sites. The service aims to maximize benefits of scale and to pass those benefits on to developers.
module Network.AWS.S3
    (
    -- * Service Configuration
      s3

    -- * Errors
    -- $errors

    -- ** BucketAlreadyOwnedByYou
    , _BucketAlreadyOwnedByYou

    -- ** ObjectAlreadyInActiveTierError
    , _ObjectAlreadyInActiveTierError

    -- ** BucketAlreadyExists
    , _BucketAlreadyExists

    -- ** ObjectNotInActiveTierError
    , _ObjectNotInActiveTierError

    -- ** NoSuchUpload
    , _NoSuchUpload

    -- ** NoSuchBucket
    , _NoSuchBucket

    -- ** NoSuchKey
    , _NoSuchKey

    -- * Waiters
    -- $waiters

    -- ** ObjectNotExists
    , objectNotExists

    -- ** BucketExists
    , bucketExists

    -- ** ObjectExists
    , objectExists

    -- ** BucketNotExists
    , bucketNotExists

    -- * Operations
    -- $operations

    -- ** PutBucketRequestPayment
    , module Network.AWS.S3.PutBucketRequestPayment

    -- ** PutObject
    , module Network.AWS.S3.PutObject

    -- ** DeleteObject
    , module Network.AWS.S3.DeleteObject

    -- ** PutBucketLogging
    , module Network.AWS.S3.PutBucketLogging

    -- ** ListBuckets
    , module Network.AWS.S3.ListBuckets

    -- ** DeleteBucket
    , module Network.AWS.S3.DeleteBucket

    -- ** CreateBucket
    , module Network.AWS.S3.CreateBucket

    -- ** DeleteBucketTagging
    , module Network.AWS.S3.DeleteBucketTagging

    -- ** PutObjectACL
    , module Network.AWS.S3.PutObjectACL

    -- ** PutBucketTagging
    , module Network.AWS.S3.PutBucketTagging

    -- ** GetBucketLocation
    , module Network.AWS.S3.GetBucketLocation

    -- ** GetBucketNotificationConfiguration
    , module Network.AWS.S3.GetBucketNotificationConfiguration

    -- ** PutBucketAccelerateConfiguration
    , module Network.AWS.S3.PutBucketAccelerateConfiguration

    -- ** GetObject
    , module Network.AWS.S3.GetObject

    -- ** PutBucketReplication
    , module Network.AWS.S3.PutBucketReplication

    -- ** GetBucketWebsite
    , module Network.AWS.S3.GetBucketWebsite

    -- ** GetBucketRequestPayment
    , module Network.AWS.S3.GetBucketRequestPayment

    -- ** DeleteBucketReplication
    , module Network.AWS.S3.DeleteBucketReplication

    -- ** ListObjectVersions (Paginated)
    , module Network.AWS.S3.ListObjectVersions

    -- ** HeadBucket
    , module Network.AWS.S3.HeadBucket

    -- ** DeleteBucketLifecycle
    , module Network.AWS.S3.DeleteBucketLifecycle

    -- ** PutBucketLifecycleConfiguration
    , module Network.AWS.S3.PutBucketLifecycleConfiguration

    -- ** CreateMultipartUpload
    , module Network.AWS.S3.CreateMultipartUpload

    -- ** UploadPart
    , module Network.AWS.S3.UploadPart

    -- ** GetBucketReplication
    , module Network.AWS.S3.GetBucketReplication

    -- ** PutBucketWebsite
    , module Network.AWS.S3.PutBucketWebsite

    -- ** DeleteBucketWebsite
    , module Network.AWS.S3.DeleteBucketWebsite

    -- ** CompleteMultipartUpload
    , module Network.AWS.S3.CompleteMultipartUpload

    -- ** ListMultipartUploads (Paginated)
    , module Network.AWS.S3.ListMultipartUploads

    -- ** ListObjects (Paginated)
    , module Network.AWS.S3.ListObjects

    -- ** DeleteBucketPolicy
    , module Network.AWS.S3.DeleteBucketPolicy

    -- ** AbortMultipartUpload
    , module Network.AWS.S3.AbortMultipartUpload

    -- ** PutBucketPolicy
    , module Network.AWS.S3.PutBucketPolicy

    -- ** GetBucketAccelerateConfiguration
    , module Network.AWS.S3.GetBucketAccelerateConfiguration

    -- ** GetObjectTorrent
    , module Network.AWS.S3.GetObjectTorrent

    -- ** DeleteObjects
    , module Network.AWS.S3.DeleteObjects

    -- ** PutBucketNotificationConfiguration
    , module Network.AWS.S3.PutBucketNotificationConfiguration

    -- ** GetBucketVersioning
    , module Network.AWS.S3.GetBucketVersioning

    -- ** DeleteBucketCORS
    , module Network.AWS.S3.DeleteBucketCORS

    -- ** PutBucketCORS
    , module Network.AWS.S3.PutBucketCORS

    -- ** GetBucketCORS
    , module Network.AWS.S3.GetBucketCORS

    -- ** GetObjectACL
    , module Network.AWS.S3.GetObjectACL

    -- ** RestoreObject
    , module Network.AWS.S3.RestoreObject

    -- ** HeadObject
    , module Network.AWS.S3.HeadObject

    -- ** PutBucketVersioning
    , module Network.AWS.S3.PutBucketVersioning

    -- ** GetBucketTagging
    , module Network.AWS.S3.GetBucketTagging

    -- ** CopyObject
    , module Network.AWS.S3.CopyObject

    -- ** GetBucketPolicy
    , module Network.AWS.S3.GetBucketPolicy

    -- ** GetBucketLogging
    , module Network.AWS.S3.GetBucketLogging

    -- ** GetBucketACL
    , module Network.AWS.S3.GetBucketACL

    -- ** GetBucketLifecycleConfiguration
    , module Network.AWS.S3.GetBucketLifecycleConfiguration

    -- ** ListParts (Paginated)
    , module Network.AWS.S3.ListParts

    -- ** UploadPartCopy
    , module Network.AWS.S3.UploadPartCopy

    -- ** PutBucketACL
    , module Network.AWS.S3.PutBucketACL

    -- * Types

    -- ** Re-exported Types
    , module Network.AWS.S3.Internal

    -- ** BucketAccelerateStatus
    , BucketAccelerateStatus (..)

    -- ** BucketCannedACL
    , BucketCannedACL (..)

    -- ** BucketLogsPermission
    , BucketLogsPermission (..)

    -- ** BucketVersioningStatus
    , BucketVersioningStatus (..)

    -- ** EncodingType
    , EncodingType (..)

    -- ** Event
    , Event (..)

    -- ** ExpirationStatus
    , ExpirationStatus (..)

    -- ** FilterRuleName
    , FilterRuleName (..)

    -- ** MFADelete
    , MFADelete (..)

    -- ** MFADeleteStatus
    , MFADeleteStatus (..)

    -- ** MetadataDirective
    , MetadataDirective (..)

    -- ** ObjectCannedACL
    , ObjectCannedACL (..)

    -- ** ObjectStorageClass
    , ObjectStorageClass (..)

    -- ** ObjectVersionStorageClass
    , ObjectVersionStorageClass (..)

    -- ** Payer
    , Payer (..)

    -- ** Permission
    , Permission (..)

    -- ** Protocol
    , Protocol (..)

    -- ** ReplicationRuleStatus
    , ReplicationRuleStatus (..)

    -- ** ReplicationStatus
    , ReplicationStatus (..)

    -- ** RequestCharged
    , RequestCharged (..)

    -- ** RequestPayer
    , RequestPayer (..)

    -- ** ServerSideEncryption
    , ServerSideEncryption (..)

    -- ** StorageClass
    , StorageClass (..)

    -- ** TransitionStorageClass
    , TransitionStorageClass (..)

    -- ** Type
    , Type (..)

    -- ** AbortIncompleteMultipartUpload
    , AbortIncompleteMultipartUpload
    , abortIncompleteMultipartUpload
    , aimuDaysAfterInitiation

    -- ** AccelerateConfiguration
    , AccelerateConfiguration
    , accelerateConfiguration
    , acStatus

    -- ** AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- ** Bucket
    , Bucket
    , bucket
    , bCreationDate
    , bName

    -- ** BucketLifecycleConfiguration
    , BucketLifecycleConfiguration
    , bucketLifecycleConfiguration
    , blcRules

    -- ** BucketLoggingStatus
    , BucketLoggingStatus
    , bucketLoggingStatus
    , blsLoggingEnabled

    -- ** CORSConfiguration
    , CORSConfiguration
    , corsConfiguration
    , ccCORSRules

    -- ** CORSRule
    , CORSRule
    , corsRule
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crExposeHeaders
    , crAllowedMethods
    , crAllowedOrigins

    -- ** CommonPrefix
    , CommonPrefix
    , commonPrefix
    , cpPrefix

    -- ** CompletedMultipartUpload
    , CompletedMultipartUpload
    , completedMultipartUpload
    , cmuParts

    -- ** CompletedPart
    , CompletedPart
    , completedPart
    , cpPartNumber
    , cpETag

    -- ** Condition
    , Condition
    , condition
    , cKeyPrefixEquals
    , cHTTPErrorCodeReturnedEquals

    -- ** CopyObjectResult
    , CopyObjectResult
    , copyObjectResult
    , corETag
    , corLastModified

    -- ** CopyPartResult
    , CopyPartResult
    , copyPartResult
    , cprETag
    , cprLastModified

    -- ** CreateBucketConfiguration
    , CreateBucketConfiguration
    , createBucketConfiguration
    , cbcLocationConstraint

    -- ** Delete
    , Delete
    , delete'
    , dQuiet
    , dObjects

    -- ** DeleteMarkerEntry
    , DeleteMarkerEntry
    , deleteMarkerEntry
    , dmeVersionId
    , dmeIsLatest
    , dmeOwner
    , dmeKey
    , dmeLastModified

    -- ** DeletedObject
    , DeletedObject
    , deletedObject
    , dVersionId
    , dDeleteMarker
    , dDeleteMarkerVersionId
    , dKey

    -- ** Destination
    , Destination
    , destination
    , dStorageClass
    , dBucket

    -- ** ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- ** FilterRule
    , FilterRule
    , filterRule
    , frValue
    , frName

    -- ** Grant
    , Grant
    , grant
    , gPermission
    , gGrantee

    -- ** Grantee
    , Grantee
    , grantee
    , gURI
    , gEmailAddress
    , gDisplayName
    , gId
    , gType

    -- ** IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- ** Initiator
    , Initiator
    , initiator
    , iDisplayName
    , iId

    -- ** LambdaFunctionConfiguration
    , LambdaFunctionConfiguration
    , lambdaFunctionConfiguration
    , lfcId
    , lfcFilter
    , lfcLambdaFunctionARN
    , lfcEvents

    -- ** LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDays
    , leDate
    , leExpiredObjectDeleteMarker

    -- ** LifecycleRule
    , LifecycleRule
    , lifecycleRule
    , lrTransitions
    , lrNoncurrentVersionExpiration
    , lrNoncurrentVersionTransitions
    , lrExpiration
    , lrId
    , lrAbortIncompleteMultipartUpload
    , lrPrefix
    , lrStatus

    -- ** LoggingEnabled
    , LoggingEnabled
    , loggingEnabled
    , leTargetBucket
    , leTargetGrants
    , leTargetPrefix

    -- ** MultipartUpload
    , MultipartUpload
    , multipartUpload
    , muInitiated
    , muInitiator
    , muOwner
    , muKey
    , muStorageClass
    , muUploadId

    -- ** NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , noncurrentVersionExpiration
    , nveNoncurrentDays

    -- ** NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , noncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- ** NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations

    -- ** NotificationConfigurationFilter
    , NotificationConfigurationFilter
    , notificationConfigurationFilter
    , ncfKey

    -- ** Object
    , Object
    , object'
    , oOwner
    , oETag
    , oSize
    , oKey
    , oStorageClass
    , oLastModified

    -- ** ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiVersionId
    , oiKey

    -- ** ObjectVersion
    , ObjectVersion
    , objectVersion
    , ovETag
    , ovVersionId
    , ovSize
    , ovIsLatest
    , ovOwner
    , ovKey
    , ovStorageClass
    , ovLastModified

    -- ** Owner
    , Owner
    , owner
    , oDisplayName
    , oId

    -- ** Part
    , Part
    , part
    , pETag
    , pSize
    , pPartNumber
    , pLastModified

    -- ** QueueConfiguration
    , QueueConfiguration
    , queueConfiguration
    , qcId
    , qcFilter
    , qcQueueARN
    , qcEvents

    -- ** Redirect
    , Redirect
    , redirect
    , rHostName
    , rProtocol
    , rHTTPRedirectCode
    , rReplaceKeyWith
    , rReplaceKeyPrefixWith

    -- ** RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , redirectAllRequestsTo
    , rartProtocol
    , rartHostName

    -- ** ReplicationConfiguration
    , ReplicationConfiguration
    , replicationConfiguration
    , rcRole
    , rcRules

    -- ** ReplicationRule
    , ReplicationRule
    , replicationRule
    , rrId
    , rrPrefix
    , rrStatus
    , rrDestination

    -- ** RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , requestPaymentConfiguration
    , rpcPayer

    -- ** RestoreRequest
    , RestoreRequest
    , restoreRequest
    , rrDays

    -- ** RoutingRule
    , RoutingRule
    , routingRule
    , rrCondition
    , rrRedirect

    -- ** S3KeyFilter
    , S3KeyFilter
    , s3KeyFilter
    , skfFilterRules

    -- ** S3ServiceError
    , S3ServiceError
    , s3ServiceError
    , sseVersionId
    , sseKey
    , sseCode
    , sseMessage

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** Tagging
    , Tagging
    , tagging
    , tTagSet

    -- ** TargetGrant
    , TargetGrant
    , targetGrant
    , tgPermission
    , tgGrantee

    -- ** TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcId
    , tcFilter
    , tcTopicARN
    , tcEvents

    -- ** Transition
    , Transition
    , transition
    , tDays
    , tDate
    , tStorageClass

    -- ** VersioningConfiguration
    , VersioningConfiguration
    , versioningConfiguration
    , vcStatus
    , vcMFADelete

    -- ** WebsiteConfiguration
    , WebsiteConfiguration
    , websiteConfiguration
    , wcRedirectAllRequestsTo
    , wcErrorDocument
    , wcIndexDocument
    , wcRoutingRules
    ) where

import           Network.AWS.S3.AbortMultipartUpload
import           Network.AWS.S3.CompleteMultipartUpload
import           Network.AWS.S3.CopyObject
import           Network.AWS.S3.CreateBucket
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.DeleteBucket
import           Network.AWS.S3.DeleteBucketCORS
import           Network.AWS.S3.DeleteBucketLifecycle
import           Network.AWS.S3.DeleteBucketPolicy
import           Network.AWS.S3.DeleteBucketReplication
import           Network.AWS.S3.DeleteBucketTagging
import           Network.AWS.S3.DeleteBucketWebsite
import           Network.AWS.S3.DeleteObject
import           Network.AWS.S3.DeleteObjects
import           Network.AWS.S3.GetBucketAccelerateConfiguration
import           Network.AWS.S3.GetBucketACL
import           Network.AWS.S3.GetBucketCORS
import           Network.AWS.S3.GetBucketLifecycleConfiguration
import           Network.AWS.S3.GetBucketLocation
import           Network.AWS.S3.GetBucketLogging
import           Network.AWS.S3.GetBucketNotificationConfiguration
import           Network.AWS.S3.GetBucketPolicy
import           Network.AWS.S3.GetBucketReplication
import           Network.AWS.S3.GetBucketRequestPayment
import           Network.AWS.S3.GetBucketTagging
import           Network.AWS.S3.GetBucketVersioning
import           Network.AWS.S3.GetBucketWebsite
import           Network.AWS.S3.GetObject
import           Network.AWS.S3.GetObjectACL
import           Network.AWS.S3.GetObjectTorrent
import           Network.AWS.S3.HeadBucket
import           Network.AWS.S3.HeadObject
import           Network.AWS.S3.Internal
import           Network.AWS.S3.ListBuckets
import           Network.AWS.S3.ListMultipartUploads
import           Network.AWS.S3.ListObjects
import           Network.AWS.S3.ListObjectVersions
import           Network.AWS.S3.ListParts
import           Network.AWS.S3.PutBucketAccelerateConfiguration
import           Network.AWS.S3.PutBucketACL
import           Network.AWS.S3.PutBucketCORS
import           Network.AWS.S3.PutBucketLifecycleConfiguration
import           Network.AWS.S3.PutBucketLogging
import           Network.AWS.S3.PutBucketNotificationConfiguration
import           Network.AWS.S3.PutBucketPolicy
import           Network.AWS.S3.PutBucketReplication
import           Network.AWS.S3.PutBucketRequestPayment
import           Network.AWS.S3.PutBucketTagging
import           Network.AWS.S3.PutBucketVersioning
import           Network.AWS.S3.PutBucketWebsite
import           Network.AWS.S3.PutObject
import           Network.AWS.S3.PutObjectACL
import           Network.AWS.S3.RestoreObject
import           Network.AWS.S3.Types
import           Network.AWS.S3.UploadPart
import           Network.AWS.S3.UploadPartCopy
import           Network.AWS.S3.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'S3'.
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
