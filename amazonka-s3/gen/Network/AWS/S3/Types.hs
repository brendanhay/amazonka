{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types
    (
    -- * Service Configuration
      s3

    -- * Errors
    , _BucketAlreadyOwnedByYou
    , _ObjectAlreadyInActiveTierError
    , _BucketAlreadyExists
    , _ObjectNotInActiveTierError
    , _NoSuchUpload
    , _NoSuchBucket
    , _NoSuchKey

    -- * Re-exported Types
    , module Network.AWS.S3.Internal

    -- * BucketAccelerateStatus
    , BucketAccelerateStatus (..)

    -- * BucketCannedACL
    , BucketCannedACL (..)

    -- * BucketLogsPermission
    , BucketLogsPermission (..)

    -- * BucketVersioningStatus
    , BucketVersioningStatus (..)

    -- * EncodingType
    , EncodingType (..)

    -- * Event
    , Event (..)

    -- * ExpirationStatus
    , ExpirationStatus (..)

    -- * FilterRuleName
    , FilterRuleName (..)

    -- * MFADelete
    , MFADelete (..)

    -- * MFADeleteStatus
    , MFADeleteStatus (..)

    -- * MetadataDirective
    , MetadataDirective (..)

    -- * ObjectCannedACL
    , ObjectCannedACL (..)

    -- * ObjectStorageClass
    , ObjectStorageClass (..)

    -- * ObjectVersionStorageClass
    , ObjectVersionStorageClass (..)

    -- * Payer
    , Payer (..)

    -- * Permission
    , Permission (..)

    -- * Protocol
    , Protocol (..)

    -- * ReplicationRuleStatus
    , ReplicationRuleStatus (..)

    -- * ReplicationStatus
    , ReplicationStatus (..)

    -- * RequestCharged
    , RequestCharged (..)

    -- * RequestPayer
    , RequestPayer (..)

    -- * ServerSideEncryption
    , ServerSideEncryption (..)

    -- * StorageClass
    , StorageClass (..)

    -- * TransitionStorageClass
    , TransitionStorageClass (..)

    -- * Type
    , Type (..)

    -- * AbortIncompleteMultipartUpload
    , AbortIncompleteMultipartUpload
    , abortIncompleteMultipartUpload
    , aimuDaysAfterInitiation

    -- * AccelerateConfiguration
    , AccelerateConfiguration
    , accelerateConfiguration
    , acStatus

    -- * AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- * Bucket
    , Bucket
    , bucket
    , bCreationDate
    , bName

    -- * BucketLifecycleConfiguration
    , BucketLifecycleConfiguration
    , bucketLifecycleConfiguration
    , blcRules

    -- * BucketLoggingStatus
    , BucketLoggingStatus
    , bucketLoggingStatus
    , blsLoggingEnabled

    -- * CORSConfiguration
    , CORSConfiguration
    , corsConfiguration
    , ccCORSRules

    -- * CORSRule
    , CORSRule
    , corsRule
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crExposeHeaders
    , crAllowedMethods
    , crAllowedOrigins

    -- * CommonPrefix
    , CommonPrefix
    , commonPrefix
    , cpPrefix

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload
    , completedMultipartUpload
    , cmuParts

    -- * CompletedPart
    , CompletedPart
    , completedPart
    , cpPartNumber
    , cpETag

    -- * Condition
    , Condition
    , condition
    , cKeyPrefixEquals
    , cHTTPErrorCodeReturnedEquals

    -- * CopyObjectResult
    , CopyObjectResult
    , copyObjectResult
    , corETag
    , corLastModified

    -- * CopyPartResult
    , CopyPartResult
    , copyPartResult
    , cprETag
    , cprLastModified

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration
    , createBucketConfiguration
    , cbcLocationConstraint

    -- * Delete
    , Delete
    , delete'
    , dQuiet
    , dObjects

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry
    , deleteMarkerEntry
    , dmeVersionId
    , dmeIsLatest
    , dmeOwner
    , dmeKey
    , dmeLastModified

    -- * DeletedObject
    , DeletedObject
    , deletedObject
    , dVersionId
    , dDeleteMarker
    , dDeleteMarkerVersionId
    , dKey

    -- * Destination
    , Destination
    , destination
    , dStorageClass
    , dBucket

    -- * ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- * FilterRule
    , FilterRule
    , filterRule
    , frValue
    , frName

    -- * Grant
    , Grant
    , grant
    , gPermission
    , gGrantee

    -- * Grantee
    , Grantee
    , grantee
    , gURI
    , gEmailAddress
    , gDisplayName
    , gId
    , gType

    -- * IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- * Initiator
    , Initiator
    , initiator
    , iDisplayName
    , iId

    -- * LambdaFunctionConfiguration
    , LambdaFunctionConfiguration
    , lambdaFunctionConfiguration
    , lfcId
    , lfcFilter
    , lfcLambdaFunctionARN
    , lfcEvents

    -- * LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDays
    , leDate
    , leExpiredObjectDeleteMarker

    -- * LifecycleRule
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

    -- * LoggingEnabled
    , LoggingEnabled
    , loggingEnabled
    , leTargetBucket
    , leTargetGrants
    , leTargetPrefix

    -- * MultipartUpload
    , MultipartUpload
    , multipartUpload
    , muInitiated
    , muInitiator
    , muOwner
    , muKey
    , muStorageClass
    , muUploadId

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , noncurrentVersionExpiration
    , nveNoncurrentDays

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , noncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations

    -- * NotificationConfigurationFilter
    , NotificationConfigurationFilter
    , notificationConfigurationFilter
    , ncfKey

    -- * Object
    , Object
    , object'
    , oOwner
    , oETag
    , oSize
    , oKey
    , oStorageClass
    , oLastModified

    -- * ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiVersionId
    , oiKey

    -- * ObjectVersion
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

    -- * Owner
    , Owner
    , owner
    , oDisplayName
    , oId

    -- * Part
    , Part
    , part
    , pETag
    , pSize
    , pPartNumber
    , pLastModified

    -- * QueueConfiguration
    , QueueConfiguration
    , queueConfiguration
    , qcId
    , qcFilter
    , qcQueueARN
    , qcEvents

    -- * Redirect
    , Redirect
    , redirect
    , rHostName
    , rProtocol
    , rHTTPRedirectCode
    , rReplaceKeyWith
    , rReplaceKeyPrefixWith

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , redirectAllRequestsTo
    , rartProtocol
    , rartHostName

    -- * ReplicationConfiguration
    , ReplicationConfiguration
    , replicationConfiguration
    , rcRole
    , rcRules

    -- * ReplicationRule
    , ReplicationRule
    , replicationRule
    , rrId
    , rrPrefix
    , rrStatus
    , rrDestination

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , requestPaymentConfiguration
    , rpcPayer

    -- * RestoreRequest
    , RestoreRequest
    , restoreRequest
    , rrDays

    -- * RoutingRule
    , RoutingRule
    , routingRule
    , rrCondition
    , rrRedirect

    -- * S3KeyFilter
    , S3KeyFilter
    , s3KeyFilter
    , skfFilterRules

    -- * S3ServiceError
    , S3ServiceError
    , s3ServiceError
    , sseVersionId
    , sseKey
    , sseCode
    , sseMessage

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Tagging
    , Tagging
    , tagging
    , tTagSet

    -- * TargetGrant
    , TargetGrant
    , targetGrant
    , tgPermission
    , tgGrantee

    -- * TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcId
    , tcFilter
    , tcTopicARN
    , tcEvents

    -- * Transition
    , Transition
    , transition
    , tDays
    , tDate
    , tStorageClass

    -- * VersioningConfiguration
    , VersioningConfiguration
    , versioningConfiguration
    , vcStatus
    , vcMFADelete

    -- * WebsiteConfiguration
    , WebsiteConfiguration
    , websiteConfiguration
    , wcRedirectAllRequestsTo
    , wcErrorDocument
    , wcIndexDocument
    , wcRoutingRules
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.S3.Internal
import           Network.AWS.S3.Types.Product
import           Network.AWS.S3.Types.Sum
import           Network.AWS.Sign.V4

-- | API version @2006-03-01@ of the Amazon Simple Storage Service SDK configuration.
s3 :: Service
s3 =
    Service
    { _svcAbbrev = "S3"
    , _svcSigner = v4
    , _svcPrefix = "s3"
    , _svcVersion = "2006-03-01"
    , _svcEndpoint = defaultEndpoint s3
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "S3"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "BadDigest" . hasStatus 400) e = Just "contentmd5"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasCode "RequestTimeout" . hasStatus 400) e = Just "timeouts"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for BucketAlreadyOwnedByYou' errors.
_BucketAlreadyOwnedByYou :: AsError a => Getting (First ServiceError) a ServiceError
_BucketAlreadyOwnedByYou = _ServiceError . hasCode "BucketAlreadyOwnedByYou"

-- | This operation is not allowed against this storage tier
_ObjectAlreadyInActiveTierError :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectAlreadyInActiveTierError =
    _ServiceError . hasCode "ObjectAlreadyInActiveTierError"

-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
_BucketAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_BucketAlreadyExists = _ServiceError . hasCode "BucketAlreadyExists"

-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
_ObjectNotInActiveTierError :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotInActiveTierError =
    _ServiceError . hasCode "ObjectNotInActiveTierError"

-- | The specified multipart upload does not exist.
_NoSuchUpload :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchUpload = _ServiceError . hasCode "NoSuchUpload"

-- | The specified bucket does not exist.
_NoSuchBucket :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucket = _ServiceError . hasCode "NoSuchBucket"

-- | The specified key does not exist.
_NoSuchKey :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchKey = _ServiceError . hasCode "NoSuchKey"
