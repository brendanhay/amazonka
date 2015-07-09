{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types
    (
    -- * Service
      S3

    -- * Errors
    , _ObjectAlreadyInActiveTierError
    , _BucketAlreadyExists
    , _ObjectNotInActiveTierError
    , _NoSuchUpload
    , _NoSuchBucket
    , _NoSuchKey

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

    -- * AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- * Bucket
    , Bucket
    , bucket
    , bucCreationDate
    , bucName

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
    , crAllowedMethods
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crAllowedOrigins
    , crExposeHeaders

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
    , conKeyPrefixEquals
    , conHTTPErrorCodeReturnedEquals

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
    , delQuiet
    , delObjects

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
    , delVersionId
    , delDeleteMarker
    , delDeleteMarkerVersionId
    , delKey

    -- * Destination
    , Destination
    , destination
    , desBucket

    -- * ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- * Grant
    , Grant
    , grant
    , graPermission
    , graGrantee

    -- * Grantee
    , Grantee
    , grantee
    , graURI
    , graEmailAddress
    , graId
    , graDisplayName
    , graType

    -- * IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- * Initiator
    , Initiator
    , initiator
    , iniId
    , iniDisplayName

    -- * LambdaFunctionConfiguration
    , LambdaFunctionConfiguration
    , lambdaFunctionConfiguration
    , lfcId
    , lfcLambdaFunctionARN
    , lfcEvents

    -- * LifecycleConfiguration
    , LifecycleConfiguration
    , lifecycleConfiguration
    , lcRules

    -- * LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDays
    , leDate

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

    -- * Object
    , Object
    , object'
    , objOwner
    , objETag
    , objSize
    , objKey
    , objStorageClass
    , objLastModified

    -- * ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiVersionId
    , oiKey

    -- * ObjectVersion
    , ObjectVersion
    , objectVersion
    , ovVersionId
    , ovETag
    , ovSize
    , ovIsLatest
    , ovOwner
    , ovKey
    , ovStorageClass
    , ovLastModified

    -- * Owner
    , Owner
    , owner
    , ownId
    , ownDisplayName

    -- * Part
    , Part
    , part
    , parETag
    , parSize
    , parPartNumber
    , parLastModified

    -- * QueueConfiguration
    , QueueConfiguration
    , queueConfiguration
    , qcId
    , qcQueueARN
    , qcEvents

    -- * Redirect
    , Redirect
    , redirect
    , redHostName
    , redProtocol
    , redHTTPRedirectCode
    , redReplaceKeyWith
    , redReplaceKeyPrefixWith

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

    -- * Rule
    , Rule
    , rule
    , rulNoncurrentVersionExpiration
    , rulTransition
    , rulExpiration
    , rulNoncurrentVersionTransition
    , rulId
    , rulPrefix
    , rulStatus

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
    , tagTagSet

    -- * TargetGrant
    , TargetGrant
    , targetGrant
    , tgPermission
    , tgGrantee

    -- * TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcId
    , tcTopicARN
    , tcEvents

    -- * Transition
    , Transition
    , transition
    , traDays
    , traDate
    , traStorageClass

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
    , wcRoutingRules
    , wcIndexDocument

    , module Network.AWS.S3.Internal
    ) where

import           Network.AWS.Prelude
import           Network.AWS.S3.Internal
import           Network.AWS.Sign.V4

-- | Version @2006-03-01@ of the Amazon Simple Storage Service SDK.
data S3

instance AWSService S3 where
    type Sg S3 = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "S3"
            , _svcPrefix = "s3"
            , _svcVersion = "2006-03-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasCode "BadDigest" . hasStatus 400) e = Just "contentmd5"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasCode "RequestTimeout" . hasStatus 400) e = Just "timeouts"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | This operation is not allowed against this storage tier
_ObjectAlreadyInActiveTierError :: AWSError a => Getting (First ServiceError) a ServiceError
_ObjectAlreadyInActiveTierError =
    _ServiceError . hasCode "ObjectAlreadyInActiveTierError"

-- | The requested bucket name is not available. The bucket namespace is
-- shared by all users of the system. Please select a different name and
-- try again.
_BucketAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_BucketAlreadyExists = _ServiceError . hasCode "BucketAlreadyExists"

-- | The source object of the COPY operation is not in the active tier and is
-- only stored in Amazon Glacier.
_ObjectNotInActiveTierError :: AWSError a => Getting (First ServiceError) a ServiceError
_ObjectNotInActiveTierError =
    _ServiceError . hasCode "ObjectNotInActiveTierError"

-- | The specified multipart upload does not exist.
_NoSuchUpload :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchUpload = _ServiceError . hasCode "NoSuchUpload"

-- | The specified bucket does not exist.
_NoSuchBucket :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchBucket = _ServiceError . hasCode "NoSuchBucket"

-- | The specified key does not exist.
_NoSuchKey :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchKey = _ServiceError . hasCode "NoSuchKey"

data BucketCannedACL
    = BAuthenticatedRead
    | BPublicRead
    | BPublicReadWrite
    | BPrivate
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketCannedACL where
    parser = takeLowerText >>= \case
        "authenticated-read" -> pure BAuthenticatedRead
        "private" -> pure BPrivate
        "public-read" -> pure BPublicRead
        "public-read-write" -> pure BPublicReadWrite
        e -> fromTextError $ "Failure parsing BucketCannedACL from value: '" <> e
           <> "'. Accepted values: authenticated-read, private, public-read, public-read-write"

instance ToText BucketCannedACL where
    toText = \case
        BAuthenticatedRead -> "authenticated-read"
        BPrivate -> "private"
        BPublicRead -> "public-read"
        BPublicReadWrite -> "public-read-write"

instance Hashable BucketCannedACL
instance ToQuery BucketCannedACL
instance ToHeader BucketCannedACL

instance ToXML BucketCannedACL where
    toXML = toXMLText

data BucketLogsPermission
    = FullControl
    | Read
    | Write
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketLogsPermission where
    parser = takeLowerText >>= \case
        "full_control" -> pure FullControl
        "read" -> pure Read
        "write" -> pure Write
        e -> fromTextError $ "Failure parsing BucketLogsPermission from value: '" <> e
           <> "'. Accepted values: full_control, read, write"

instance ToText BucketLogsPermission where
    toText = \case
        FullControl -> "full_control"
        Read -> "read"
        Write -> "write"

instance Hashable BucketLogsPermission
instance ToQuery BucketLogsPermission
instance ToHeader BucketLogsPermission

instance FromXML BucketLogsPermission where
    parseXML = parseXMLText "BucketLogsPermission"

instance ToXML BucketLogsPermission where
    toXML = toXMLText

data BucketVersioningStatus
    = BVSSuspended
    | BVSEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketVersioningStatus where
    parser = takeLowerText >>= \case
        "enabled" -> pure BVSEnabled
        "suspended" -> pure BVSSuspended
        e -> fromTextError $ "Failure parsing BucketVersioningStatus from value: '" <> e
           <> "'. Accepted values: enabled, suspended"

instance ToText BucketVersioningStatus where
    toText = \case
        BVSEnabled -> "enabled"
        BVSSuspended -> "suspended"

instance Hashable BucketVersioningStatus
instance ToQuery BucketVersioningStatus
instance ToHeader BucketVersioningStatus

instance FromXML BucketVersioningStatus where
    parseXML = parseXMLText "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
    toXML = toXMLText

-- | Requests Amazon S3 to encode the object keys in the response and
-- specifies the encoding method to use. An object key may contain any
-- Unicode character; however, XML 1.0 parser cannot parse some characters,
-- such as characters with an ASCII value from 0 to 10. For characters that
-- are not supported in XML 1.0, you can add this parameter to request that
-- Amazon S3 encode the keys in the response.
data EncodingType =
    URL
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: url"

instance ToText EncodingType where
    toText = \case
        URL -> "url"

instance Hashable EncodingType
instance ToQuery EncodingType
instance ToHeader EncodingType

instance FromXML EncodingType where
    parseXML = parseXMLText "EncodingType"

instance ToXML EncodingType where
    toXML = toXMLText

-- | Bucket event for which to send notifications.
data Event
    = S3ObjectCreatedPut
    | S3ReducedRedundancyLostObject
    | S3ObjectCreatedPost
    | S3ObjectCreatedCopy
    | S3ObjectCreatedCompleteMultipartUpload
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Event where
    parser = takeLowerText >>= \case
        "s3:objectcreated:completemultipartupload" -> pure S3ObjectCreatedCompleteMultipartUpload
        "s3:objectcreated:copy" -> pure S3ObjectCreatedCopy
        "s3:objectcreated:post" -> pure S3ObjectCreatedPost
        "s3:objectcreated:put" -> pure S3ObjectCreatedPut
        "s3:reducedredundancylostobject" -> pure S3ReducedRedundancyLostObject
        e -> fromTextError $ "Failure parsing Event from value: '" <> e
           <> "'. Accepted values: s3:objectcreated:completemultipartupload, s3:objectcreated:copy, s3:objectcreated:post, s3:objectcreated:put, s3:reducedredundancylostobject"

instance ToText Event where
    toText = \case
        S3ObjectCreatedCompleteMultipartUpload -> "s3:objectcreated:completemultipartupload"
        S3ObjectCreatedCopy -> "s3:objectcreated:copy"
        S3ObjectCreatedPost -> "s3:objectcreated:post"
        S3ObjectCreatedPut -> "s3:objectcreated:put"
        S3ReducedRedundancyLostObject -> "s3:reducedredundancylostobject"

instance Hashable Event
instance ToQuery Event
instance ToHeader Event

instance FromXML Event where
    parseXML = parseXMLText "Event"

instance ToXML Event where
    toXML = toXMLText

data ExpirationStatus
    = ESDisabled
    | ESEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExpirationStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESDisabled
        "enabled" -> pure ESEnabled
        e -> fromTextError $ "Failure parsing ExpirationStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ExpirationStatus where
    toText = \case
        ESDisabled -> "disabled"
        ESEnabled -> "enabled"

instance Hashable ExpirationStatus
instance ToQuery ExpirationStatus
instance ToHeader ExpirationStatus

instance FromXML ExpirationStatus where
    parseXML = parseXMLText "ExpirationStatus"

instance ToXML ExpirationStatus where
    toXML = toXMLText

data MFADelete
    = MDDisabled
    | MDEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MFADelete where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDDisabled
        "enabled" -> pure MDEnabled
        e -> fromTextError $ "Failure parsing MFADelete from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText MFADelete where
    toText = \case
        MDDisabled -> "disabled"
        MDEnabled -> "enabled"

instance Hashable MFADelete
instance ToQuery MFADelete
instance ToHeader MFADelete

instance ToXML MFADelete where
    toXML = toXMLText

data MFADeleteStatus
    = MDSEnabled
    | MDSDisabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MFADeleteStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDSDisabled
        "enabled" -> pure MDSEnabled
        e -> fromTextError $ "Failure parsing MFADeleteStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText MFADeleteStatus where
    toText = \case
        MDSDisabled -> "disabled"
        MDSEnabled -> "enabled"

instance Hashable MFADeleteStatus
instance ToQuery MFADeleteStatus
instance ToHeader MFADeleteStatus

instance FromXML MFADeleteStatus where
    parseXML = parseXMLText "MFADeleteStatus"

data MetadataDirective
    = Replace
    | Copy
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MetadataDirective where
    parser = takeLowerText >>= \case
        "copy" -> pure Copy
        "replace" -> pure Replace
        e -> fromTextError $ "Failure parsing MetadataDirective from value: '" <> e
           <> "'. Accepted values: copy, replace"

instance ToText MetadataDirective where
    toText = \case
        Copy -> "copy"
        Replace -> "replace"

instance Hashable MetadataDirective
instance ToQuery MetadataDirective
instance ToHeader MetadataDirective

instance ToXML MetadataDirective where
    toXML = toXMLText

data ObjectCannedACL
    = OPublicRead
    | OPrivate
    | OPublicReadWrite
    | OAuthenticatedRead
    | OBucketOwnerFullControl
    | OBucketOwnerRead
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectCannedACL where
    parser = takeLowerText >>= \case
        "authenticated-read" -> pure OAuthenticatedRead
        "bucket-owner-full-control" -> pure OBucketOwnerFullControl
        "bucket-owner-read" -> pure OBucketOwnerRead
        "private" -> pure OPrivate
        "public-read" -> pure OPublicRead
        "public-read-write" -> pure OPublicReadWrite
        e -> fromTextError $ "Failure parsing ObjectCannedACL from value: '" <> e
           <> "'. Accepted values: authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText ObjectCannedACL where
    toText = \case
        OAuthenticatedRead -> "authenticated-read"
        OBucketOwnerFullControl -> "bucket-owner-full-control"
        OBucketOwnerRead -> "bucket-owner-read"
        OPrivate -> "private"
        OPublicRead -> "public-read"
        OPublicReadWrite -> "public-read-write"

instance Hashable ObjectCannedACL
instance ToQuery ObjectCannedACL
instance ToHeader ObjectCannedACL

instance ToXML ObjectCannedACL where
    toXML = toXMLText

data ObjectStorageClass
    = OSCStandard
    | OSCReducedRedundancy
    | OSCGlacier
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure OSCGlacier
        "reduced_redundancy" -> pure OSCReducedRedundancy
        "standard" -> pure OSCStandard
        e -> fromTextError $ "Failure parsing ObjectStorageClass from value: '" <> e
           <> "'. Accepted values: glacier, reduced_redundancy, standard"

instance ToText ObjectStorageClass where
    toText = \case
        OSCGlacier -> "glacier"
        OSCReducedRedundancy -> "reduced_redundancy"
        OSCStandard -> "standard"

instance Hashable ObjectStorageClass
instance ToQuery ObjectStorageClass
instance ToHeader ObjectStorageClass

instance FromXML ObjectStorageClass where
    parseXML = parseXMLText "ObjectStorageClass"

data ObjectVersionStorageClass =
    OVSCStandard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectVersionStorageClass where
    parser = takeLowerText >>= \case
        "standard" -> pure OVSCStandard
        e -> fromTextError $ "Failure parsing ObjectVersionStorageClass from value: '" <> e
           <> "'. Accepted values: standard"

instance ToText ObjectVersionStorageClass where
    toText = \case
        OVSCStandard -> "standard"

instance Hashable ObjectVersionStorageClass
instance ToQuery ObjectVersionStorageClass
instance ToHeader ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
    parseXML = parseXMLText "ObjectVersionStorageClass"

data Payer
    = BucketOwner
    | Requester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Payer where
    parser = takeLowerText >>= \case
        "bucketowner" -> pure BucketOwner
        "requester" -> pure Requester
        e -> fromTextError $ "Failure parsing Payer from value: '" <> e
           <> "'. Accepted values: bucketowner, requester"

instance ToText Payer where
    toText = \case
        BucketOwner -> "bucketowner"
        Requester -> "requester"

instance Hashable Payer
instance ToQuery Payer
instance ToHeader Payer

instance FromXML Payer where
    parseXML = parseXMLText "Payer"

instance ToXML Payer where
    toXML = toXMLText

data Permission
    = PerReadAcp
    | PerWrite
    | PerWriteAcp
    | PerFullControl
    | PerRead
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Permission where
    parser = takeLowerText >>= \case
        "full_control" -> pure PerFullControl
        "read" -> pure PerRead
        "read_acp" -> pure PerReadAcp
        "write" -> pure PerWrite
        "write_acp" -> pure PerWriteAcp
        e -> fromTextError $ "Failure parsing Permission from value: '" <> e
           <> "'. Accepted values: full_control, read, read_acp, write, write_acp"

instance ToText Permission where
    toText = \case
        PerFullControl -> "full_control"
        PerRead -> "read"
        PerReadAcp -> "read_acp"
        PerWrite -> "write"
        PerWriteAcp -> "write_acp"

instance Hashable Permission
instance ToQuery Permission
instance ToHeader Permission

instance FromXML Permission where
    parseXML = parseXMLText "Permission"

instance ToXML Permission where
    toXML = toXMLText

data Protocol
    = HTTPS
    | HTTP
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Protocol where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "https" -> pure HTTPS
        e -> fromTextError $ "Failure parsing Protocol from value: '" <> e
           <> "'. Accepted values: http, https"

instance ToText Protocol where
    toText = \case
        HTTP -> "http"
        HTTPS -> "https"

instance Hashable Protocol
instance ToQuery Protocol
instance ToHeader Protocol

instance FromXML Protocol where
    parseXML = parseXMLText "Protocol"

instance ToXML Protocol where
    toXML = toXMLText

data ReplicationRuleStatus
    = Enabled
    | Disabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReplicationRuleStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing ReplicationRuleStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ReplicationRuleStatus where
    toText = \case
        Disabled -> "disabled"
        Enabled -> "enabled"

instance Hashable ReplicationRuleStatus
instance ToQuery ReplicationRuleStatus
instance ToHeader ReplicationRuleStatus

instance FromXML ReplicationRuleStatus where
    parseXML = parseXMLText "ReplicationRuleStatus"

instance ToXML ReplicationRuleStatus where
    toXML = toXMLText

data ReplicationStatus
    = Pending
    | Replica
    | Failed
    | Complete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReplicationStatus where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "failed" -> pure Failed
        "pending" -> pure Pending
        "replica" -> pure Replica
        e -> fromTextError $ "Failure parsing ReplicationStatus from value: '" <> e
           <> "'. Accepted values: complete, failed, pending, replica"

instance ToText ReplicationStatus where
    toText = \case
        Complete -> "complete"
        Failed -> "failed"
        Pending -> "pending"
        Replica -> "replica"

instance Hashable ReplicationStatus
instance ToQuery ReplicationStatus
instance ToHeader ReplicationStatus

instance FromXML ReplicationStatus where
    parseXML = parseXMLText "ReplicationStatus"

-- | If present, indicates that the requester was successfully charged for
-- the request.
data RequestCharged =
    RCRequester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RequestCharged where
    parser = takeLowerText >>= \case
        "requester" -> pure RCRequester
        e -> fromTextError $ "Failure parsing RequestCharged from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestCharged where
    toText = \case
        RCRequester -> "requester"

instance Hashable RequestCharged
instance ToQuery RequestCharged
instance ToHeader RequestCharged

instance FromXML RequestCharged where
    parseXML = parseXMLText "RequestCharged"

-- | Confirms that the requester knows that she or he will be charged for the
-- request. Bucket owners need not specify this parameter in their
-- requests. Documentation on downloading objects from requester pays
-- buckets can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/ObjectsinRequesterPaysBuckets.html
data RequestPayer =
    RPRequester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RequestPayer where
    parser = takeLowerText >>= \case
        "requester" -> pure RPRequester
        e -> fromTextError $ "Failure parsing RequestPayer from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestPayer where
    toText = \case
        RPRequester -> "requester"

instance Hashable RequestPayer
instance ToQuery RequestPayer
instance ToHeader RequestPayer

instance ToXML RequestPayer where
    toXML = toXMLText

data ServerSideEncryption =
    AES256
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ServerSideEncryption where
    parser = takeLowerText >>= \case
        "aes256" -> pure AES256
        e -> fromTextError $ "Failure parsing ServerSideEncryption from value: '" <> e
           <> "'. Accepted values: aes256"

instance ToText ServerSideEncryption where
    toText = \case
        AES256 -> "aes256"

instance Hashable ServerSideEncryption
instance ToQuery ServerSideEncryption
instance ToHeader ServerSideEncryption

instance FromXML ServerSideEncryption where
    parseXML = parseXMLText "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXML = toXMLText

data StorageClass
    = Standard
    | ReducedRedundancy
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "reduced_redundancy" -> pure ReducedRedundancy
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: reduced_redundancy, standard"

instance ToText StorageClass where
    toText = \case
        ReducedRedundancy -> "reduced_redundancy"
        Standard -> "standard"

instance Hashable StorageClass
instance ToQuery StorageClass
instance ToHeader StorageClass

instance FromXML StorageClass where
    parseXML = parseXMLText "StorageClass"

instance ToXML StorageClass where
    toXML = toXMLText

data TransitionStorageClass =
    Glacier
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TransitionStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure Glacier
        e -> fromTextError $ "Failure parsing TransitionStorageClass from value: '" <> e
           <> "'. Accepted values: glacier"

instance ToText TransitionStorageClass where
    toText = \case
        Glacier -> "glacier"

instance Hashable TransitionStorageClass
instance ToQuery TransitionStorageClass
instance ToHeader TransitionStorageClass

instance FromXML TransitionStorageClass where
    parseXML = parseXMLText "TransitionStorageClass"

instance ToXML TransitionStorageClass where
    toXML = toXMLText

data Type
    = Group
    | CanonicalUser
    | AmazonCustomerByEmail
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Type where
    parser = takeLowerText >>= \case
        "amazoncustomerbyemail" -> pure AmazonCustomerByEmail
        "canonicaluser" -> pure CanonicalUser
        "group" -> pure Group
        e -> fromTextError $ "Failure parsing Type from value: '" <> e
           <> "'. Accepted values: amazoncustomerbyemail, canonicaluser, group"

instance ToText Type where
    toText = \case
        AmazonCustomerByEmail -> "amazoncustomerbyemail"
        CanonicalUser -> "canonicaluser"
        Group -> "group"

instance Hashable Type
instance ToQuery Type
instance ToHeader Type

instance FromXML Type where
    parseXML = parseXMLText "Type"

instance ToXML Type where
    toXML = toXMLText

-- | /See:/ 'accessControlPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acpGrants'
--
-- * 'acpOwner'
data AccessControlPolicy = AccessControlPolicy'
    { _acpGrants :: !(Maybe [Grant])
    , _acpOwner  :: !(Maybe Owner)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessControlPolicy' smart constructor.
accessControlPolicy :: AccessControlPolicy
accessControlPolicy =
    AccessControlPolicy'
    { _acpGrants = Nothing
    , _acpOwner = Nothing
    }

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\ s a -> s{_acpGrants = a}) . _Default;

-- | FIXME: Undocumented member.
acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\ s a -> s{_acpOwner = a});

instance ToXML AccessControlPolicy where
        toXML AccessControlPolicy'{..}
          = mconcat
              ["AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _acpGrants),
               "Owner" @= _acpOwner]

-- | /See:/ 'bucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bucCreationDate'
--
-- * 'bucName'
data Bucket = Bucket'
    { _bucCreationDate :: !RFC822
    , _bucName         :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Bucket' smart constructor.
bucket :: UTCTime -> BucketName -> Bucket
bucket pCreationDate pName =
    Bucket'
    { _bucCreationDate = _Time # pCreationDate
    , _bucName = pName
    }

-- | Date the bucket was created.
bucCreationDate :: Lens' Bucket UTCTime
bucCreationDate = lens _bucCreationDate (\ s a -> s{_bucCreationDate = a}) . _Time;

-- | The name of the bucket.
bucName :: Lens' Bucket BucketName
bucName = lens _bucName (\ s a -> s{_bucName = a});

instance FromXML Bucket where
        parseXML x
          = Bucket' <$> (x .@ "CreationDate") <*> (x .@ "Name")

-- | /See:/ 'bucketLoggingStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'blsLoggingEnabled'
newtype BucketLoggingStatus = BucketLoggingStatus'
    { _blsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BucketLoggingStatus' smart constructor.
bucketLoggingStatus :: BucketLoggingStatus
bucketLoggingStatus =
    BucketLoggingStatus'
    { _blsLoggingEnabled = Nothing
    }

-- | FIXME: Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\ s a -> s{_blsLoggingEnabled = a});

instance ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus'{..}
          = mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

-- | /See:/ 'corsConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccCORSRules'
newtype CORSConfiguration = CORSConfiguration'
    { _ccCORSRules :: Maybe [CORSRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CORSConfiguration' smart constructor.
corsConfiguration :: CORSConfiguration
corsConfiguration =
    CORSConfiguration'
    { _ccCORSRules = Nothing
    }

-- | FIXME: Undocumented member.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\ s a -> s{_ccCORSRules = a}) . _Default;

instance ToXML CORSConfiguration where
        toXML CORSConfiguration'{..}
          = mconcat
              [toXML (toXMLList "CORSRule" <$> _ccCORSRules)]

-- | /See:/ 'corsRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crAllowedMethods'
--
-- * 'crMaxAgeSeconds'
--
-- * 'crAllowedHeaders'
--
-- * 'crAllowedOrigins'
--
-- * 'crExposeHeaders'
data CORSRule = CORSRule'
    { _crAllowedMethods :: !(Maybe [Text])
    , _crMaxAgeSeconds  :: !(Maybe Int)
    , _crAllowedHeaders :: !(Maybe [Text])
    , _crAllowedOrigins :: !(Maybe [Text])
    , _crExposeHeaders  :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CORSRule' smart constructor.
corsRule :: CORSRule
corsRule =
    CORSRule'
    { _crAllowedMethods = Nothing
    , _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crAllowedOrigins = Nothing
    , _crExposeHeaders = Nothing
    }

-- | Identifies HTTP methods that the domain\/origin specified in the rule is
-- allowed to execute.
crAllowedMethods :: Lens' CORSRule [Text]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Default;

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a});

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default;

-- | One or more origins you want customers to be able to access the bucket
-- from.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Default;

-- | One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default;

instance FromXML CORSRule where
        parseXML x
          = CORSRule' <$>
              (may (parseXMLList "AllowedMethod") x) <*>
                (x .@? "MaxAgeSeconds")
                <*> (may (parseXMLList "AllowedHeader") x)
                <*> (may (parseXMLList "AllowedOrigin") x)
                <*> (may (parseXMLList "ExposeHeader") x)

instance ToXML CORSRule where
        toXML CORSRule'{..}
          = mconcat
              [toXML
                 (toXMLList "AllowedMethod" <$> _crAllowedMethods),
               "MaxAgeSeconds" @= _crMaxAgeSeconds,
               toXML
                 (toXMLList "AllowedHeader" <$> _crAllowedHeaders),
               toXML
                 (toXMLList "AllowedOrigin" <$> _crAllowedOrigins),
               toXML
                 (toXMLList "ExposeHeader" <$> _crExposeHeaders)]

-- | /See:/ 'commonPrefix' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPrefix'
newtype CommonPrefix = CommonPrefix'
    { _cpPrefix :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CommonPrefix' smart constructor.
commonPrefix :: CommonPrefix
commonPrefix =
    CommonPrefix'
    { _cpPrefix = Nothing
    }

-- | FIXME: Undocumented member.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\ s a -> s{_cpPrefix = a});

instance FromXML CommonPrefix where
        parseXML x = CommonPrefix' <$> (x .@? "Prefix")

-- | /See:/ 'completedMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuParts'
newtype CompletedMultipartUpload = CompletedMultipartUpload'
    { _cmuParts :: Maybe (List1 CompletedPart)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompletedMultipartUpload' smart constructor.
completedMultipartUpload :: CompletedMultipartUpload
completedMultipartUpload =
    CompletedMultipartUpload'
    { _cmuParts = Nothing
    }

-- | FIXME: Undocumented member.
cmuParts :: Lens' CompletedMultipartUpload (Maybe (NonEmpty CompletedPart))
cmuParts = lens _cmuParts (\ s a -> s{_cmuParts = a}) . mapping _List1;

instance ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload'{..}
          = mconcat [toXML (toXMLList "Part" <$> _cmuParts)]

-- | /See:/ 'completedPart' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPartNumber'
--
-- * 'cpETag'
data CompletedPart = CompletedPart'
    { _cpPartNumber :: !Int
    , _cpETag       :: !ETag
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CompletedPart' smart constructor.
completedPart :: Int -> ETag -> CompletedPart
completedPart pPartNumber pETag =
    CompletedPart'
    { _cpPartNumber = pPartNumber
    , _cpETag = pETag
    }

-- | Part number that identifies the part.
cpPartNumber :: Lens' CompletedPart Int
cpPartNumber = lens _cpPartNumber (\ s a -> s{_cpPartNumber = a});

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart ETag
cpETag = lens _cpETag (\ s a -> s{_cpETag = a});

instance ToXML CompletedPart where
        toXML CompletedPart'{..}
          = mconcat
              ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]

-- | /See:/ 'condition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'conKeyPrefixEquals'
--
-- * 'conHTTPErrorCodeReturnedEquals'
data Condition = Condition'
    { _conKeyPrefixEquals             :: !(Maybe Text)
    , _conHTTPErrorCodeReturnedEquals :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Condition' smart constructor.
condition :: Condition
condition =
    Condition'
    { _conKeyPrefixEquals = Nothing
    , _conHTTPErrorCodeReturnedEquals = Nothing
    }

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix
-- docs\/, the key prefix will be \/docs, which identifies all objects in
-- the docs\/ folder. Required when the parent element Condition is
-- specified and sibling HttpErrorCodeReturnedEquals is not specified. If
-- both conditions are specified, both must be true for the redirect to be
-- applied.
conKeyPrefixEquals :: Lens' Condition (Maybe Text)
conKeyPrefixEquals = lens _conKeyPrefixEquals (\ s a -> s{_conKeyPrefixEquals = a});

-- | The HTTP error code when the redirect is applied. In the event of an
-- error, if the error code equals this value, then the specified redirect
-- is applied. Required when parent element Condition is specified and
-- sibling KeyPrefixEquals is not specified. If both are specified, then
-- both must be true for the redirect to be applied.
conHTTPErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
conHTTPErrorCodeReturnedEquals = lens _conHTTPErrorCodeReturnedEquals (\ s a -> s{_conHTTPErrorCodeReturnedEquals = a});

instance FromXML Condition where
        parseXML x
          = Condition' <$>
              (x .@? "KeyPrefixEquals") <*>
                (x .@? "HttpErrorCodeReturnedEquals")

instance ToXML Condition where
        toXML Condition'{..}
          = mconcat
              ["KeyPrefixEquals" @= _conKeyPrefixEquals,
               "HttpErrorCodeReturnedEquals" @=
                 _conHTTPErrorCodeReturnedEquals]

-- | /See:/ 'copyObjectResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corETag'
--
-- * 'corLastModified'
data CopyObjectResult = CopyObjectResult'
    { _corETag         :: !(Maybe ETag)
    , _corLastModified :: !(Maybe RFC822)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CopyObjectResult' smart constructor.
copyObjectResult :: CopyObjectResult
copyObjectResult =
    CopyObjectResult'
    { _corETag = Nothing
    , _corLastModified = Nothing
    }

-- | FIXME: Undocumented member.
corETag :: Lens' CopyObjectResult (Maybe ETag)
corETag = lens _corETag (\ s a -> s{_corETag = a});

-- | FIXME: Undocumented member.
corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\ s a -> s{_corLastModified = a}) . mapping _Time;

instance FromXML CopyObjectResult where
        parseXML x
          = CopyObjectResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

-- | /See:/ 'copyPartResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprETag'
--
-- * 'cprLastModified'
data CopyPartResult = CopyPartResult'
    { _cprETag         :: !(Maybe ETag)
    , _cprLastModified :: !(Maybe RFC822)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CopyPartResult' smart constructor.
copyPartResult :: CopyPartResult
copyPartResult =
    CopyPartResult'
    { _cprETag = Nothing
    , _cprLastModified = Nothing
    }

-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\ s a -> s{_cprETag = a});

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\ s a -> s{_cprLastModified = a}) . mapping _Time;

instance FromXML CopyPartResult where
        parseXML x
          = CopyPartResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

-- | /See:/ 'createBucketConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbcLocationConstraint'
newtype CreateBucketConfiguration = CreateBucketConfiguration'
    { _cbcLocationConstraint :: Maybe Region
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CreateBucketConfiguration' smart constructor.
createBucketConfiguration :: CreateBucketConfiguration
createBucketConfiguration =
    CreateBucketConfiguration'
    { _cbcLocationConstraint = Nothing
    }

-- | Specifies the region where the bucket will be created. If you don\'t
-- specify a region, the bucket will be created in US Standard.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe Region)
cbcLocationConstraint = lens _cbcLocationConstraint (\ s a -> s{_cbcLocationConstraint = a});

instance ToXML CreateBucketConfiguration where
        toXML CreateBucketConfiguration'{..}
          = mconcat
              ["LocationConstraint" @= _cbcLocationConstraint]

-- | /See:/ 'delete'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delQuiet'
--
-- * 'delObjects'
data Delete = Delete'
    { _delQuiet   :: !(Maybe Bool)
    , _delObjects :: ![ObjectIdentifier]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Delete' smart constructor.
delete' :: Delete
delete' =
    Delete'
    { _delQuiet = Nothing
    , _delObjects = mempty
    }

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
delQuiet :: Lens' Delete (Maybe Bool)
delQuiet = lens _delQuiet (\ s a -> s{_delQuiet = a});

-- | FIXME: Undocumented member.
delObjects :: Lens' Delete [ObjectIdentifier]
delObjects = lens _delObjects (\ s a -> s{_delObjects = a});

instance ToXML Delete where
        toXML Delete'{..}
          = mconcat
              ["Quiet" @= _delQuiet,
               toXMLList "Object" _delObjects]

-- | /See:/ 'deleteMarkerEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmeVersionId'
--
-- * 'dmeIsLatest'
--
-- * 'dmeOwner'
--
-- * 'dmeKey'
--
-- * 'dmeLastModified'
data DeleteMarkerEntry = DeleteMarkerEntry'
    { _dmeVersionId    :: !(Maybe ObjectVersionId)
    , _dmeIsLatest     :: !(Maybe Bool)
    , _dmeOwner        :: !(Maybe Owner)
    , _dmeKey          :: !(Maybe ObjectKey)
    , _dmeLastModified :: !(Maybe RFC822)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteMarkerEntry' smart constructor.
deleteMarkerEntry :: DeleteMarkerEntry
deleteMarkerEntry =
    DeleteMarkerEntry'
    { _dmeVersionId = Nothing
    , _dmeIsLatest = Nothing
    , _dmeOwner = Nothing
    , _dmeKey = Nothing
    , _dmeLastModified = Nothing
    }

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\ s a -> s{_dmeVersionId = a});

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\ s a -> s{_dmeIsLatest = a});

-- | FIXME: Undocumented member.
dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\ s a -> s{_dmeOwner = a});

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\ s a -> s{_dmeKey = a});

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\ s a -> s{_dmeLastModified = a}) . mapping _Time;

instance FromXML DeleteMarkerEntry where
        parseXML x
          = DeleteMarkerEntry' <$>
              (x .@? "VersionId") <*> (x .@? "IsLatest") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "LastModified")

-- | /See:/ 'deletedObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delVersionId'
--
-- * 'delDeleteMarker'
--
-- * 'delDeleteMarkerVersionId'
--
-- * 'delKey'
data DeletedObject = DeletedObject'
    { _delVersionId             :: !(Maybe ObjectVersionId)
    , _delDeleteMarker          :: !(Maybe Bool)
    , _delDeleteMarkerVersionId :: !(Maybe Text)
    , _delKey                   :: !(Maybe ObjectKey)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeletedObject' smart constructor.
deletedObject :: DeletedObject
deletedObject =
    DeletedObject'
    { _delVersionId = Nothing
    , _delDeleteMarker = Nothing
    , _delDeleteMarkerVersionId = Nothing
    , _delKey = Nothing
    }

-- | FIXME: Undocumented member.
delVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
delVersionId = lens _delVersionId (\ s a -> s{_delVersionId = a});

-- | FIXME: Undocumented member.
delDeleteMarker :: Lens' DeletedObject (Maybe Bool)
delDeleteMarker = lens _delDeleteMarker (\ s a -> s{_delDeleteMarker = a});

-- | FIXME: Undocumented member.
delDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
delDeleteMarkerVersionId = lens _delDeleteMarkerVersionId (\ s a -> s{_delDeleteMarkerVersionId = a});

-- | FIXME: Undocumented member.
delKey :: Lens' DeletedObject (Maybe ObjectKey)
delKey = lens _delKey (\ s a -> s{_delKey = a});

instance FromXML DeletedObject where
        parseXML x
          = DeletedObject' <$>
              (x .@? "VersionId") <*> (x .@? "DeleteMarker") <*>
                (x .@? "DeleteMarkerVersionId")
                <*> (x .@? "Key")

-- | /See:/ 'destination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desBucket'
newtype Destination = Destination'
    { _desBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Destination' smart constructor.
destination :: BucketName -> Destination
destination pBucket =
    Destination'
    { _desBucket = pBucket
    }

-- | Amazon resource name (ARN) of the bucket where you want Amazon S3 to
-- store replicas of the object identified by the rule.
desBucket :: Lens' Destination BucketName
desBucket = lens _desBucket (\ s a -> s{_desBucket = a});

instance FromXML Destination where
        parseXML x = Destination' <$> (x .@ "Bucket")

instance ToXML Destination where
        toXML Destination'{..}
          = mconcat ["Bucket" @= _desBucket]

-- | /See:/ 'errorDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edKey'
newtype ErrorDocument = ErrorDocument'
    { _edKey :: ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ErrorDocument' smart constructor.
errorDocument :: ObjectKey -> ErrorDocument
errorDocument pKey =
    ErrorDocument'
    { _edKey = pKey
    }

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\ s a -> s{_edKey = a});

instance FromXML ErrorDocument where
        parseXML x = ErrorDocument' <$> (x .@ "Key")

instance ToXML ErrorDocument where
        toXML ErrorDocument'{..} = mconcat ["Key" @= _edKey]

-- | /See:/ 'grant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'graPermission'
--
-- * 'graGrantee'
data Grant = Grant'
    { _graPermission :: !(Maybe Permission)
    , _graGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Grant' smart constructor.
grant :: Grant
grant =
    Grant'
    { _graPermission = Nothing
    , _graGrantee = Nothing
    }

-- | Specifies the permission given to the grantee.
graPermission :: Lens' Grant (Maybe Permission)
graPermission = lens _graPermission (\ s a -> s{_graPermission = a});

-- | FIXME: Undocumented member.
graGrantee :: Lens' Grant (Maybe Grantee)
graGrantee = lens _graGrantee (\ s a -> s{_graGrantee = a});

instance FromXML Grant where
        parseXML x
          = Grant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance ToXML Grant where
        toXML Grant'{..}
          = mconcat
              ["Permission" @= _graPermission,
               "Grantee" @= _graGrantee]

-- | /See:/ 'grantee' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'graURI'
--
-- * 'graEmailAddress'
--
-- * 'graId'
--
-- * 'graDisplayName'
--
-- * 'graType'
data Grantee = Grantee'
    { _graURI          :: !(Maybe Text)
    , _graEmailAddress :: !(Maybe Text)
    , _graId           :: !(Maybe Text)
    , _graDisplayName  :: !(Maybe Text)
    , _graType         :: !Type
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Grantee' smart constructor.
grantee :: Type -> Grantee
grantee pType =
    Grantee'
    { _graURI = Nothing
    , _graEmailAddress = Nothing
    , _graId = Nothing
    , _graDisplayName = Nothing
    , _graType = pType
    }

-- | URI of the grantee group.
graURI :: Lens' Grantee (Maybe Text)
graURI = lens _graURI (\ s a -> s{_graURI = a});

-- | Email address of the grantee.
graEmailAddress :: Lens' Grantee (Maybe Text)
graEmailAddress = lens _graEmailAddress (\ s a -> s{_graEmailAddress = a});

-- | The canonical user ID of the grantee.
graId :: Lens' Grantee (Maybe Text)
graId = lens _graId (\ s a -> s{_graId = a});

-- | Screen name of the grantee.
graDisplayName :: Lens' Grantee (Maybe Text)
graDisplayName = lens _graDisplayName (\ s a -> s{_graDisplayName = a});

-- | Type of grantee
graType :: Lens' Grantee Type
graType = lens _graType (\ s a -> s{_graType = a});

instance FromXML Grantee where
        parseXML x
          = Grantee' <$>
              (x .@? "URI") <*> (x .@? "EmailAddress") <*>
                (x .@? "ID")
                <*> (x .@? "DisplayName")
                <*> (x .@ "xsi:type")

instance ToXML Grantee where
        toXML Grantee'{..}
          = mconcat
              ["URI" @= _graURI,
               "EmailAddress" @= _graEmailAddress, "ID" @= _graId,
               "DisplayName" @= _graDisplayName,
               "xsi:type" @= _graType]

-- | /See:/ 'indexDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idSuffix'
newtype IndexDocument = IndexDocument'
    { _idSuffix :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IndexDocument' smart constructor.
indexDocument :: Text -> IndexDocument
indexDocument pSuffix =
    IndexDocument'
    { _idSuffix = pSuffix
    }

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (e.g. if the suffix is index.html and you make a
-- request to samplebucket\/images\/ the data that is returned will be for
-- the object with the key name images\/index.html) The suffix must not be
-- empty and must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\ s a -> s{_idSuffix = a});

instance FromXML IndexDocument where
        parseXML x = IndexDocument' <$> (x .@ "Suffix")

instance ToXML IndexDocument where
        toXML IndexDocument'{..}
          = mconcat ["Suffix" @= _idSuffix]

-- | /See:/ 'initiator' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniId'
--
-- * 'iniDisplayName'
data Initiator = Initiator'
    { _iniId          :: !(Maybe Text)
    , _iniDisplayName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Initiator' smart constructor.
initiator :: Initiator
initiator =
    Initiator'
    { _iniId = Nothing
    , _iniDisplayName = Nothing
    }

-- | If the principal is an AWS account, it provides the Canonical User ID.
-- If the principal is an IAM User, it provides a user ARN value.
iniId :: Lens' Initiator (Maybe Text)
iniId = lens _iniId (\ s a -> s{_iniId = a});

-- | Name of the Principal.
iniDisplayName :: Lens' Initiator (Maybe Text)
iniDisplayName = lens _iniDisplayName (\ s a -> s{_iniDisplayName = a});

instance FromXML Initiator where
        parseXML x
          = Initiator' <$>
              (x .@? "ID") <*> (x .@? "DisplayName")

-- | Container for specifying the AWS Lambda notification configuration.
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfcId'
--
-- * 'lfcLambdaFunctionARN'
--
-- * 'lfcEvents'
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
    { _lfcId                :: !(Maybe Text)
    , _lfcLambdaFunctionARN :: !Text
    , _lfcEvents            :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LambdaFunctionConfiguration' smart constructor.
lambdaFunctionConfiguration :: Text -> LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN =
    LambdaFunctionConfiguration'
    { _lfcId = Nothing
    , _lfcLambdaFunctionARN = pLambdaFunctionARN
    , _lfcEvents = mempty
    }

-- | FIXME: Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\ s a -> s{_lfcId = a});

-- | Lambda cloud function ARN that Amazon S3 can invoke when it detects
-- events of the specified type.
lfcLambdaFunctionARN :: Lens' LambdaFunctionConfiguration Text
lfcLambdaFunctionARN = lens _lfcLambdaFunctionARN (\ s a -> s{_lfcLambdaFunctionARN = a});

-- | FIXME: Undocumented member.
lfcEvents :: Lens' LambdaFunctionConfiguration [Event]
lfcEvents = lens _lfcEvents (\ s a -> s{_lfcEvents = a});

instance FromXML LambdaFunctionConfiguration where
        parseXML x
          = LambdaFunctionConfiguration' <$>
              (x .@? "Id") <*> (x .@ "CloudFunction") <*>
                (parseXMLList "Event" x)

instance ToXML LambdaFunctionConfiguration where
        toXML LambdaFunctionConfiguration'{..}
          = mconcat
              ["Id" @= _lfcId,
               "CloudFunction" @= _lfcLambdaFunctionARN,
               toXMLList "Event" _lfcEvents]

-- | /See:/ 'lifecycleConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcRules'
newtype LifecycleConfiguration = LifecycleConfiguration'
    { _lcRules :: [Rule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LifecycleConfiguration' smart constructor.
lifecycleConfiguration :: LifecycleConfiguration
lifecycleConfiguration =
    LifecycleConfiguration'
    { _lcRules = mempty
    }

-- | FIXME: Undocumented member.
lcRules :: Lens' LifecycleConfiguration [Rule]
lcRules = lens _lcRules (\ s a -> s{_lcRules = a});

instance ToXML LifecycleConfiguration where
        toXML LifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _lcRules]

-- | /See:/ 'lifecycleExpiration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leDays'
--
-- * 'leDate'
data LifecycleExpiration = LifecycleExpiration'
    { _leDays :: !(Maybe Int)
    , _leDate :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LifecycleExpiration' smart constructor.
lifecycleExpiration :: LifecycleExpiration
lifecycleExpiration =
    LifecycleExpiration'
    { _leDays = Nothing
    , _leDate = Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\ s a -> s{_leDays = a});

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\ s a -> s{_leDate = a}) . mapping _Time;

instance FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' <$>
              (x .@? "Days") <*> (x .@? "Date")

instance ToXML LifecycleExpiration where
        toXML LifecycleExpiration'{..}
          = mconcat ["Days" @= _leDays, "Date" @= _leDate]

-- | /See:/ 'loggingEnabled' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leTargetBucket'
--
-- * 'leTargetGrants'
--
-- * 'leTargetPrefix'
data LoggingEnabled = LoggingEnabled'
    { _leTargetBucket :: !(Maybe Text)
    , _leTargetGrants :: !(Maybe [TargetGrant])
    , _leTargetPrefix :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoggingEnabled' smart constructor.
loggingEnabled :: LoggingEnabled
loggingEnabled =
    LoggingEnabled'
    { _leTargetBucket = Nothing
    , _leTargetGrants = Nothing
    , _leTargetPrefix = Nothing
    }

-- | Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In
-- this case you should choose a different TargetPrefix for each source
-- bucket so that the delivered log files can be distinguished by key.
leTargetBucket :: Lens' LoggingEnabled (Maybe Text)
leTargetBucket = lens _leTargetBucket (\ s a -> s{_leTargetBucket = a});

-- | FIXME: Undocumented member.
leTargetGrants :: Lens' LoggingEnabled [TargetGrant]
leTargetGrants = lens _leTargetGrants (\ s a -> s{_leTargetGrants = a}) . _Default;

-- | This element lets you specify a prefix for the keys that the log files
-- will be stored under.
leTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
leTargetPrefix = lens _leTargetPrefix (\ s a -> s{_leTargetPrefix = a});

instance FromXML LoggingEnabled where
        parseXML x
          = LoggingEnabled' <$>
              (x .@? "TargetBucket") <*>
                (x .@? "TargetGrants" .!@ mempty >>=
                   may (parseXMLList "Grant"))
                <*> (x .@? "TargetPrefix")

instance ToXML LoggingEnabled where
        toXML LoggingEnabled'{..}
          = mconcat
              ["TargetBucket" @= _leTargetBucket,
               "TargetGrants" @=
                 toXML (toXMLList "Grant" <$> _leTargetGrants),
               "TargetPrefix" @= _leTargetPrefix]

-- | /See:/ 'multipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'muInitiated'
--
-- * 'muInitiator'
--
-- * 'muOwner'
--
-- * 'muKey'
--
-- * 'muStorageClass'
--
-- * 'muUploadId'
data MultipartUpload = MultipartUpload'
    { _muInitiated    :: !(Maybe RFC822)
    , _muInitiator    :: !(Maybe Initiator)
    , _muOwner        :: !(Maybe Owner)
    , _muKey          :: !(Maybe ObjectKey)
    , _muStorageClass :: !(Maybe StorageClass)
    , _muUploadId     :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'MultipartUpload' smart constructor.
multipartUpload :: MultipartUpload
multipartUpload =
    MultipartUpload'
    { _muInitiated = Nothing
    , _muInitiator = Nothing
    , _muOwner = Nothing
    , _muKey = Nothing
    , _muStorageClass = Nothing
    , _muUploadId = Nothing
    }

-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\ s a -> s{_muInitiated = a}) . mapping _Time;

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\ s a -> s{_muInitiator = a});

-- | FIXME: Undocumented member.
muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\ s a -> s{_muOwner = a});

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\ s a -> s{_muKey = a});

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\ s a -> s{_muStorageClass = a});

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\ s a -> s{_muUploadId = a});

instance FromXML MultipartUpload where
        parseXML x
          = MultipartUpload' <$>
              (x .@? "Initiated") <*> (x .@? "Initiator") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "UploadId")

-- | Specifies when noncurrent object versions expire. Upon expiration,
-- Amazon S3 permanently deletes the noncurrent object versions. You set
-- this lifecycle configuration action on a bucket that has versioning
-- enabled (or suspended) to request that Amazon S3 delete noncurrent
-- object versions at a specific period in the object\'s lifetime.
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nveNoncurrentDays'
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
    { _nveNoncurrentDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NoncurrentVersionExpiration' smart constructor.
noncurrentVersionExpiration :: Int -> NoncurrentVersionExpiration
noncurrentVersionExpiration pNoncurrentDays =
    NoncurrentVersionExpiration'
    { _nveNoncurrentDays = pNoncurrentDays
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- </AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the Amazon Simple Storage Service Developer Guide.
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays = lens _nveNoncurrentDays (\ s a -> s{_nveNoncurrentDays = a});

instance FromXML NoncurrentVersionExpiration where
        parseXML x
          = NoncurrentVersionExpiration' <$>
              (x .@ "NoncurrentDays")

instance ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration'{..}
          = mconcat ["NoncurrentDays" @= _nveNoncurrentDays]

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action
-- to request that Amazon S3 transition noncurrent object versions to the
-- GLACIER storage class at a specific period in the object\'s lifetime.
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nvtNoncurrentDays'
--
-- * 'nvtStorageClass'
data NoncurrentVersionTransition = NoncurrentVersionTransition'
    { _nvtNoncurrentDays :: !Int
    , _nvtStorageClass   :: !TransitionStorageClass
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NoncurrentVersionTransition' smart constructor.
noncurrentVersionTransition :: Int -> TransitionStorageClass -> NoncurrentVersionTransition
noncurrentVersionTransition pNoncurrentDays pStorageClass =
    NoncurrentVersionTransition'
    { _nvtNoncurrentDays = pNoncurrentDays
    , _nvtStorageClass = pStorageClass
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- </AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the Amazon Simple Storage Service Developer Guide.
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays = lens _nvtNoncurrentDays (\ s a -> s{_nvtNoncurrentDays = a});

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = lens _nvtStorageClass (\ s a -> s{_nvtStorageClass = a});

instance FromXML NoncurrentVersionTransition where
        parseXML x
          = NoncurrentVersionTransition' <$>
              (x .@ "NoncurrentDays") <*> (x .@ "StorageClass")

instance ToXML NoncurrentVersionTransition where
        toXML NoncurrentVersionTransition'{..}
          = mconcat
              ["NoncurrentDays" @= _nvtNoncurrentDays,
               "StorageClass" @= _nvtStorageClass]

-- | Container for specifying the notification configuration of the bucket.
-- If this element is empty, notifications are turned off on the bucket.
--
-- /See:/ 'notificationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncQueueConfigurations'
--
-- * 'ncTopicConfigurations'
--
-- * 'ncLambdaFunctionConfigurations'
data NotificationConfiguration = NotificationConfiguration'
    { _ncQueueConfigurations          :: !(Maybe [QueueConfiguration])
    , _ncTopicConfigurations          :: !(Maybe [TopicConfiguration])
    , _ncLambdaFunctionConfigurations :: !(Maybe [LambdaFunctionConfiguration])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NotificationConfiguration' smart constructor.
notificationConfiguration :: NotificationConfiguration
notificationConfiguration =
    NotificationConfiguration'
    { _ncQueueConfigurations = Nothing
    , _ncTopicConfigurations = Nothing
    , _ncLambdaFunctionConfigurations = Nothing
    }

-- | FIXME: Undocumented member.
ncQueueConfigurations :: Lens' NotificationConfiguration [QueueConfiguration]
ncQueueConfigurations = lens _ncQueueConfigurations (\ s a -> s{_ncQueueConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
ncTopicConfigurations :: Lens' NotificationConfiguration [TopicConfiguration]
ncTopicConfigurations = lens _ncTopicConfigurations (\ s a -> s{_ncTopicConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
ncLambdaFunctionConfigurations :: Lens' NotificationConfiguration [LambdaFunctionConfiguration]
ncLambdaFunctionConfigurations = lens _ncLambdaFunctionConfigurations (\ s a -> s{_ncLambdaFunctionConfigurations = a}) . _Default;

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (may (parseXMLList "QueueConfiguration") x) <*>
                (may (parseXMLList "TopicConfiguration") x)
                <*>
                (may (parseXMLList "CloudFunctionConfiguration") x)

instance ToXML NotificationConfiguration where
        toXML NotificationConfiguration'{..}
          = mconcat
              [toXML
                 (toXMLList "QueueConfiguration" <$>
                    _ncQueueConfigurations),
               toXML
                 (toXMLList "TopicConfiguration" <$>
                    _ncTopicConfigurations),
               toXML
                 (toXMLList "CloudFunctionConfiguration" <$>
                    _ncLambdaFunctionConfigurations)]

-- | /See:/ 'object'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'objOwner'
--
-- * 'objETag'
--
-- * 'objSize'
--
-- * 'objKey'
--
-- * 'objStorageClass'
--
-- * 'objLastModified'
data Object = Object'
    { _objOwner        :: !(Maybe Owner)
    , _objETag         :: !ETag
    , _objSize         :: !Int
    , _objKey          :: !ObjectKey
    , _objStorageClass :: !ObjectStorageClass
    , _objLastModified :: !RFC822
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Object' smart constructor.
object' :: ETag -> Int -> ObjectKey -> ObjectStorageClass -> UTCTime -> Object
object' pETag pSize pKey pStorageClass pLastModified =
    Object'
    { _objOwner = Nothing
    , _objETag = pETag
    , _objSize = pSize
    , _objKey = pKey
    , _objStorageClass = pStorageClass
    , _objLastModified = _Time # pLastModified
    }

-- | FIXME: Undocumented member.
objOwner :: Lens' Object (Maybe Owner)
objOwner = lens _objOwner (\ s a -> s{_objOwner = a});

-- | FIXME: Undocumented member.
objETag :: Lens' Object ETag
objETag = lens _objETag (\ s a -> s{_objETag = a});

-- | FIXME: Undocumented member.
objSize :: Lens' Object Int
objSize = lens _objSize (\ s a -> s{_objSize = a});

-- | FIXME: Undocumented member.
objKey :: Lens' Object ObjectKey
objKey = lens _objKey (\ s a -> s{_objKey = a});

-- | The class of storage used to store the object.
objStorageClass :: Lens' Object ObjectStorageClass
objStorageClass = lens _objStorageClass (\ s a -> s{_objStorageClass = a});

-- | FIXME: Undocumented member.
objLastModified :: Lens' Object UTCTime
objLastModified = lens _objLastModified (\ s a -> s{_objLastModified = a}) . _Time;

instance FromXML Object where
        parseXML x
          = Object' <$>
              (x .@? "Owner") <*> (x .@ "ETag") <*> (x .@ "Size")
                <*> (x .@ "Key")
                <*> (x .@ "StorageClass")
                <*> (x .@ "LastModified")

-- | /See:/ 'objectIdentifier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oiVersionId'
--
-- * 'oiKey'
data ObjectIdentifier = ObjectIdentifier'
    { _oiVersionId :: !(Maybe ObjectVersionId)
    , _oiKey       :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ObjectIdentifier' smart constructor.
objectIdentifier :: ObjectKey -> ObjectIdentifier
objectIdentifier pKey =
    ObjectIdentifier'
    { _oiVersionId = Nothing
    , _oiKey = pKey
    }

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\ s a -> s{_oiVersionId = a});

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\ s a -> s{_oiKey = a});

instance ToXML ObjectIdentifier where
        toXML ObjectIdentifier'{..}
          = mconcat
              ["VersionId" @= _oiVersionId, "Key" @= _oiKey]

-- | /See:/ 'objectVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ovVersionId'
--
-- * 'ovETag'
--
-- * 'ovSize'
--
-- * 'ovIsLatest'
--
-- * 'ovOwner'
--
-- * 'ovKey'
--
-- * 'ovStorageClass'
--
-- * 'ovLastModified'
data ObjectVersion = ObjectVersion'
    { _ovVersionId    :: !(Maybe ObjectVersionId)
    , _ovETag         :: !(Maybe ETag)
    , _ovSize         :: !(Maybe Int)
    , _ovIsLatest     :: !(Maybe Bool)
    , _ovOwner        :: !(Maybe Owner)
    , _ovKey          :: !(Maybe ObjectKey)
    , _ovStorageClass :: !(Maybe ObjectVersionStorageClass)
    , _ovLastModified :: !(Maybe RFC822)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ObjectVersion' smart constructor.
objectVersion :: ObjectVersion
objectVersion =
    ObjectVersion'
    { _ovVersionId = Nothing
    , _ovETag = Nothing
    , _ovSize = Nothing
    , _ovIsLatest = Nothing
    , _ovOwner = Nothing
    , _ovKey = Nothing
    , _ovStorageClass = Nothing
    , _ovLastModified = Nothing
    }

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\ s a -> s{_ovVersionId = a});

-- | FIXME: Undocumented member.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\ s a -> s{_ovETag = a});

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\ s a -> s{_ovSize = a});

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\ s a -> s{_ovIsLatest = a});

-- | FIXME: Undocumented member.
ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\ s a -> s{_ovOwner = a});

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\ s a -> s{_ovKey = a});

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\ s a -> s{_ovStorageClass = a});

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\ s a -> s{_ovLastModified = a}) . mapping _Time;

instance FromXML ObjectVersion where
        parseXML x
          = ObjectVersion' <$>
              (x .@? "VersionId") <*> (x .@? "ETag") <*>
                (x .@? "Size")
                <*> (x .@? "IsLatest")
                <*> (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "LastModified")

-- | /See:/ 'owner' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ownId'
--
-- * 'ownDisplayName'
data Owner = Owner'
    { _ownId          :: !(Maybe Text)
    , _ownDisplayName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Owner' smart constructor.
owner :: Owner
owner =
    Owner'
    { _ownId = Nothing
    , _ownDisplayName = Nothing
    }

-- | FIXME: Undocumented member.
ownId :: Lens' Owner (Maybe Text)
ownId = lens _ownId (\ s a -> s{_ownId = a});

-- | FIXME: Undocumented member.
ownDisplayName :: Lens' Owner (Maybe Text)
ownDisplayName = lens _ownDisplayName (\ s a -> s{_ownDisplayName = a});

instance FromXML Owner where
        parseXML x
          = Owner' <$> (x .@? "ID") <*> (x .@? "DisplayName")

instance ToXML Owner where
        toXML Owner'{..}
          = mconcat
              ["ID" @= _ownId, "DisplayName" @= _ownDisplayName]

-- | /See:/ 'part' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parETag'
--
-- * 'parSize'
--
-- * 'parPartNumber'
--
-- * 'parLastModified'
data Part = Part'
    { _parETag         :: !(Maybe ETag)
    , _parSize         :: !(Maybe Int)
    , _parPartNumber   :: !(Maybe Int)
    , _parLastModified :: !(Maybe RFC822)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Part' smart constructor.
part :: Part
part =
    Part'
    { _parETag = Nothing
    , _parSize = Nothing
    , _parPartNumber = Nothing
    , _parLastModified = Nothing
    }

-- | Entity tag returned when the part was uploaded.
parETag :: Lens' Part (Maybe ETag)
parETag = lens _parETag (\ s a -> s{_parETag = a});

-- | Size of the uploaded part data.
parSize :: Lens' Part (Maybe Int)
parSize = lens _parSize (\ s a -> s{_parSize = a});

-- | Part number identifying the part.
parPartNumber :: Lens' Part (Maybe Int)
parPartNumber = lens _parPartNumber (\ s a -> s{_parPartNumber = a});

-- | Date and time at which the part was uploaded.
parLastModified :: Lens' Part (Maybe UTCTime)
parLastModified = lens _parLastModified (\ s a -> s{_parLastModified = a}) . mapping _Time;

instance FromXML Part where
        parseXML x
          = Part' <$>
              (x .@? "ETag") <*> (x .@? "Size") <*>
                (x .@? "PartNumber")
                <*> (x .@? "LastModified")

-- | Container for specifying an configuration when you want Amazon S3 to
-- publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
--
-- /See:/ 'queueConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qcId'
--
-- * 'qcQueueARN'
--
-- * 'qcEvents'
data QueueConfiguration = QueueConfiguration'
    { _qcId       :: !(Maybe Text)
    , _qcQueueARN :: !Text
    , _qcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'QueueConfiguration' smart constructor.
queueConfiguration :: Text -> QueueConfiguration
queueConfiguration pQueueARN =
    QueueConfiguration'
    { _qcId = Nothing
    , _qcQueueARN = pQueueARN
    , _qcEvents = mempty
    }

-- | FIXME: Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\ s a -> s{_qcId = a});

-- | Amazon SQS queue ARN to which Amazon S3 will publish a message when it
-- detects events of specified type.
qcQueueARN :: Lens' QueueConfiguration Text
qcQueueARN = lens _qcQueueARN (\ s a -> s{_qcQueueARN = a});

-- | FIXME: Undocumented member.
qcEvents :: Lens' QueueConfiguration [Event]
qcEvents = lens _qcEvents (\ s a -> s{_qcEvents = a});

instance FromXML QueueConfiguration where
        parseXML x
          = QueueConfiguration' <$>
              (x .@? "Id") <*> (x .@ "Queue") <*>
                (parseXMLList "Event" x)

instance ToXML QueueConfiguration where
        toXML QueueConfiguration'{..}
          = mconcat
              ["Id" @= _qcId, "Queue" @= _qcQueueARN,
               toXMLList "Event" _qcEvents]

-- | /See:/ 'redirect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'redHostName'
--
-- * 'redProtocol'
--
-- * 'redHTTPRedirectCode'
--
-- * 'redReplaceKeyWith'
--
-- * 'redReplaceKeyPrefixWith'
data Redirect = Redirect'
    { _redHostName             :: !(Maybe Text)
    , _redProtocol             :: !(Maybe Protocol)
    , _redHTTPRedirectCode     :: !(Maybe Text)
    , _redReplaceKeyWith       :: !(Maybe Text)
    , _redReplaceKeyPrefixWith :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Redirect' smart constructor.
redirect :: Redirect
redirect =
    Redirect'
    { _redHostName = Nothing
    , _redProtocol = Nothing
    , _redHTTPRedirectCode = Nothing
    , _redReplaceKeyWith = Nothing
    , _redReplaceKeyPrefixWith = Nothing
    }

-- | The host name to use in the redirect request.
redHostName :: Lens' Redirect (Maybe Text)
redHostName = lens _redHostName (\ s a -> s{_redHostName = a});

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
redProtocol :: Lens' Redirect (Maybe Protocol)
redProtocol = lens _redProtocol (\ s a -> s{_redProtocol = a});

-- | The HTTP redirect code to use on the response. Not required if one of
-- the siblings is present.
redHTTPRedirectCode :: Lens' Redirect (Maybe Text)
redHTTPRedirectCode = lens _redHTTPRedirectCode (\ s a -> s{_redHTTPRedirectCode = a});

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
redReplaceKeyWith :: Lens' Redirect (Maybe Text)
redReplaceKeyWith = lens _redReplaceKeyWith (\ s a -> s{_redReplaceKeyWith = a});

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs\/ (objects in the
-- docs\/ folder) to documents\/, you can set a condition block with
-- KeyPrefixEquals set to docs\/ and in the Redirect set
-- ReplaceKeyPrefixWith to \/documents. Not required if one of the siblings
-- is present. Can be present only if ReplaceKeyWith is not provided.
redReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
redReplaceKeyPrefixWith = lens _redReplaceKeyPrefixWith (\ s a -> s{_redReplaceKeyPrefixWith = a});

instance FromXML Redirect where
        parseXML x
          = Redirect' <$>
              (x .@? "HostName") <*> (x .@? "Protocol") <*>
                (x .@? "HttpRedirectCode")
                <*> (x .@? "ReplaceKeyWith")
                <*> (x .@? "ReplaceKeyPrefixWith")

instance ToXML Redirect where
        toXML Redirect'{..}
          = mconcat
              ["HostName" @= _redHostName,
               "Protocol" @= _redProtocol,
               "HttpRedirectCode" @= _redHTTPRedirectCode,
               "ReplaceKeyWith" @= _redReplaceKeyWith,
               "ReplaceKeyPrefixWith" @= _redReplaceKeyPrefixWith]

-- | /See:/ 'redirectAllRequestsTo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rartProtocol'
--
-- * 'rartHostName'
data RedirectAllRequestsTo = RedirectAllRequestsTo'
    { _rartProtocol :: !(Maybe Protocol)
    , _rartHostName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RedirectAllRequestsTo' smart constructor.
redirectAllRequestsTo :: Text -> RedirectAllRequestsTo
redirectAllRequestsTo pHostName =
    RedirectAllRequestsTo'
    { _rartProtocol = Nothing
    , _rartHostName = pHostName
    }

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\ s a -> s{_rartProtocol = a});

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\ s a -> s{_rartHostName = a});

instance FromXML RedirectAllRequestsTo where
        parseXML x
          = RedirectAllRequestsTo' <$>
              (x .@? "Protocol") <*> (x .@ "HostName")

instance ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo'{..}
          = mconcat
              ["Protocol" @= _rartProtocol,
               "HostName" @= _rartHostName]

-- | Container for replication rules. You can add as many as 1,000 rules.
-- Total replication configuration size can be up to 2 MB.
--
-- /See:/ 'replicationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRole'
--
-- * 'rcRules'
data ReplicationConfiguration = ReplicationConfiguration'
    { _rcRole  :: !Text
    , _rcRules :: ![ReplicationRule]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ReplicationConfiguration' smart constructor.
replicationConfiguration :: Text -> ReplicationConfiguration
replicationConfiguration pRole =
    ReplicationConfiguration'
    { _rcRole = pRole
    , _rcRules = mempty
    }

-- | Amazon Resource Name (ARN) of an IAM role for Amazon S3 to assume when
-- replicating the objects.
rcRole :: Lens' ReplicationConfiguration Text
rcRole = lens _rcRole (\ s a -> s{_rcRole = a});

-- | Container for information about a particular replication rule.
-- Replication configuration must have at least one rule and can contain up
-- to 1,000 rules.
rcRules :: Lens' ReplicationConfiguration [ReplicationRule]
rcRules = lens _rcRules (\ s a -> s{_rcRules = a});

instance FromXML ReplicationConfiguration where
        parseXML x
          = ReplicationConfiguration' <$>
              (x .@ "Role") <*> (parseXMLList "Rule" x)

instance ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration'{..}
          = mconcat
              ["Role" @= _rcRole, toXMLList "Rule" _rcRules]

-- | /See:/ 'replicationRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrId'
--
-- * 'rrPrefix'
--
-- * 'rrStatus'
--
-- * 'rrDestination'
data ReplicationRule = ReplicationRule'
    { _rrId          :: !(Maybe Text)
    , _rrPrefix      :: !Text
    , _rrStatus      :: !ReplicationRuleStatus
    , _rrDestination :: !Destination
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ReplicationRule' smart constructor.
replicationRule :: Text -> ReplicationRuleStatus -> Destination -> ReplicationRule
replicationRule pPrefix pStatus pDestination =
    ReplicationRule'
    { _rrId = Nothing
    , _rrPrefix = pPrefix
    , _rrStatus = pStatus
    , _rrDestination = pDestination
    }

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rrId :: Lens' ReplicationRule (Maybe Text)
rrId = lens _rrId (\ s a -> s{_rrId = a});

-- | Object keyname prefix identifying one or more objects to which the rule
-- applies. Maximum prefix length can be up to 1,024 characters.
-- Overlapping prefixes are not supported.
rrPrefix :: Lens' ReplicationRule Text
rrPrefix = lens _rrPrefix (\ s a -> s{_rrPrefix = a});

-- | The rule is ignored if status is not Enabled.
rrStatus :: Lens' ReplicationRule ReplicationRuleStatus
rrStatus = lens _rrStatus (\ s a -> s{_rrStatus = a});

-- | FIXME: Undocumented member.
rrDestination :: Lens' ReplicationRule Destination
rrDestination = lens _rrDestination (\ s a -> s{_rrDestination = a});

instance FromXML ReplicationRule where
        parseXML x
          = ReplicationRule' <$>
              (x .@? "ID") <*> (x .@ "Prefix") <*> (x .@ "Status")
                <*> (x .@ "Destination")

instance ToXML ReplicationRule where
        toXML ReplicationRule'{..}
          = mconcat
              ["ID" @= _rrId, "Prefix" @= _rrPrefix,
               "Status" @= _rrStatus,
               "Destination" @= _rrDestination]

-- | /See:/ 'requestPaymentConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpcPayer'
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
    { _rpcPayer :: Payer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestPaymentConfiguration' smart constructor.
requestPaymentConfiguration :: Payer -> RequestPaymentConfiguration
requestPaymentConfiguration pPayer =
    RequestPaymentConfiguration'
    { _rpcPayer = pPayer
    }

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\ s a -> s{_rpcPayer = a});

instance ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration'{..}
          = mconcat ["Payer" @= _rpcPayer]

-- | /See:/ 'restoreRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrDays'
newtype RestoreRequest = RestoreRequest'
    { _rrDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreRequest' smart constructor.
restoreRequest :: Int -> RestoreRequest
restoreRequest pDays =
    RestoreRequest'
    { _rrDays = pDays
    }

-- | Lifetime of the active copy in days
rrDays :: Lens' RestoreRequest Int
rrDays = lens _rrDays (\ s a -> s{_rrDays = a});

instance ToXML RestoreRequest where
        toXML RestoreRequest'{..}
          = mconcat ["Days" @= _rrDays]

-- | /See:/ 'routingRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrCondition'
--
-- * 'rrRedirect'
data RoutingRule = RoutingRule'
    { _rrCondition :: !(Maybe Condition)
    , _rrRedirect  :: !Redirect
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RoutingRule' smart constructor.
routingRule :: Redirect -> RoutingRule
routingRule pRedirect =
    RoutingRule'
    { _rrCondition = Nothing
    , _rrRedirect = pRedirect
    }

-- | A container for describing a condition that must be met for the
-- specified redirect to apply. For example, 1. If request is for pages in
-- the \/docs folder, redirect to the \/documents folder. 2. If request
-- results in HTTP error 4xx, redirect request to another host where you
-- might process the error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\ s a -> s{_rrCondition = a});

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an
-- error, you can can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\ s a -> s{_rrRedirect = a});

instance FromXML RoutingRule where
        parseXML x
          = RoutingRule' <$>
              (x .@? "Condition") <*> (x .@ "Redirect")

instance ToXML RoutingRule where
        toXML RoutingRule'{..}
          = mconcat
              ["Condition" @= _rrCondition,
               "Redirect" @= _rrRedirect]

-- | /See:/ 'rule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rulNoncurrentVersionExpiration'
--
-- * 'rulTransition'
--
-- * 'rulExpiration'
--
-- * 'rulNoncurrentVersionTransition'
--
-- * 'rulId'
--
-- * 'rulPrefix'
--
-- * 'rulStatus'
data Rule = Rule'
    { _rulNoncurrentVersionExpiration :: !(Maybe NoncurrentVersionExpiration)
    , _rulTransition                  :: !(Maybe Transition)
    , _rulExpiration                  :: !(Maybe LifecycleExpiration)
    , _rulNoncurrentVersionTransition :: !(Maybe NoncurrentVersionTransition)
    , _rulId                          :: !(Maybe Text)
    , _rulPrefix                      :: !Text
    , _rulStatus                      :: !ExpirationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Rule' smart constructor.
rule :: Text -> ExpirationStatus -> Rule
rule pPrefix pStatus =
    Rule'
    { _rulNoncurrentVersionExpiration = Nothing
    , _rulTransition = Nothing
    , _rulExpiration = Nothing
    , _rulNoncurrentVersionTransition = Nothing
    , _rulId = Nothing
    , _rulPrefix = pPrefix
    , _rulStatus = pStatus
    }

-- | FIXME: Undocumented member.
rulNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
rulNoncurrentVersionExpiration = lens _rulNoncurrentVersionExpiration (\ s a -> s{_rulNoncurrentVersionExpiration = a});

-- | FIXME: Undocumented member.
rulTransition :: Lens' Rule (Maybe Transition)
rulTransition = lens _rulTransition (\ s a -> s{_rulTransition = a});

-- | FIXME: Undocumented member.
rulExpiration :: Lens' Rule (Maybe LifecycleExpiration)
rulExpiration = lens _rulExpiration (\ s a -> s{_rulExpiration = a});

-- | FIXME: Undocumented member.
rulNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
rulNoncurrentVersionTransition = lens _rulNoncurrentVersionTransition (\ s a -> s{_rulNoncurrentVersionTransition = a});

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rulId :: Lens' Rule (Maybe Text)
rulId = lens _rulId (\ s a -> s{_rulId = a});

-- | Prefix identifying one or more objects to which the rule applies.
rulPrefix :: Lens' Rule Text
rulPrefix = lens _rulPrefix (\ s a -> s{_rulPrefix = a});

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
rulStatus :: Lens' Rule ExpirationStatus
rulStatus = lens _rulStatus (\ s a -> s{_rulStatus = a});

instance FromXML Rule where
        parseXML x
          = Rule' <$>
              (x .@? "NoncurrentVersionExpiration") <*>
                (x .@? "Transition")
                <*> (x .@? "Expiration")
                <*> (x .@? "NoncurrentVersionTransition")
                <*> (x .@? "ID")
                <*> (x .@ "Prefix")
                <*> (x .@ "Status")

instance ToXML Rule where
        toXML Rule'{..}
          = mconcat
              ["NoncurrentVersionExpiration" @=
                 _rulNoncurrentVersionExpiration,
               "Transition" @= _rulTransition,
               "Expiration" @= _rulExpiration,
               "NoncurrentVersionTransition" @=
                 _rulNoncurrentVersionTransition,
               "ID" @= _rulId, "Prefix" @= _rulPrefix,
               "Status" @= _rulStatus]

-- | /See:/ 's3ServiceError' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sseVersionId'
--
-- * 'sseKey'
--
-- * 'sseCode'
--
-- * 'sseMessage'
data S3ServiceError = S3ServiceError'
    { _sseVersionId :: !(Maybe ObjectVersionId)
    , _sseKey       :: !(Maybe ObjectKey)
    , _sseCode      :: !(Maybe Text)
    , _sseMessage   :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'S3ServiceError' smart constructor.
s3ServiceError :: S3ServiceError
s3ServiceError =
    S3ServiceError'
    { _sseVersionId = Nothing
    , _sseKey = Nothing
    , _sseCode = Nothing
    , _sseMessage = Nothing
    }

-- | FIXME: Undocumented member.
sseVersionId :: Lens' S3ServiceError (Maybe ObjectVersionId)
sseVersionId = lens _sseVersionId (\ s a -> s{_sseVersionId = a});

-- | FIXME: Undocumented member.
sseKey :: Lens' S3ServiceError (Maybe ObjectKey)
sseKey = lens _sseKey (\ s a -> s{_sseKey = a});

-- | FIXME: Undocumented member.
sseCode :: Lens' S3ServiceError (Maybe Text)
sseCode = lens _sseCode (\ s a -> s{_sseCode = a});

-- | FIXME: Undocumented member.
sseMessage :: Lens' S3ServiceError (Maybe Text)
sseMessage = lens _sseMessage (\ s a -> s{_sseMessage = a});

instance FromXML S3ServiceError where
        parseXML x
          = S3ServiceError' <$>
              (x .@? "VersionId") <*> (x .@? "Key") <*>
                (x .@? "Code")
                <*> (x .@? "Message")

-- | /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey'
--
-- * 'tagValue'
data Tag = Tag'
    { _tagKey   :: !ObjectKey
    , _tagValue :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: ObjectKey -> Text -> Tag
tag pKey pValue =
    Tag'
    { _tagKey = pKey
    , _tagValue = pValue
    }

-- | Name of the tag.
tagKey :: Lens' Tag ObjectKey
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | Value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Key" @= _tagKey, "Value" @= _tagValue]

-- | /See:/ 'tagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagTagSet'
newtype Tagging = Tagging'
    { _tagTagSet :: [Tag]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Tagging' smart constructor.
tagging :: Tagging
tagging =
    Tagging'
    { _tagTagSet = mempty
    }

-- | FIXME: Undocumented member.
tagTagSet :: Lens' Tagging [Tag]
tagTagSet = lens _tagTagSet (\ s a -> s{_tagTagSet = a});

instance ToXML Tagging where
        toXML Tagging'{..}
          = mconcat ["TagSet" @= toXMLList "Tag" _tagTagSet]

-- | /See:/ 'targetGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tgPermission'
--
-- * 'tgGrantee'
data TargetGrant = TargetGrant'
    { _tgPermission :: !(Maybe BucketLogsPermission)
    , _tgGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TargetGrant' smart constructor.
targetGrant :: TargetGrant
targetGrant =
    TargetGrant'
    { _tgPermission = Nothing
    , _tgGrantee = Nothing
    }

-- | Logging permissions assigned to the Grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\ s a -> s{_tgPermission = a});

-- | FIXME: Undocumented member.
tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\ s a -> s{_tgGrantee = a});

instance FromXML TargetGrant where
        parseXML x
          = TargetGrant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance ToXML TargetGrant where
        toXML TargetGrant'{..}
          = mconcat
              ["Permission" @= _tgPermission,
               "Grantee" @= _tgGrantee]

-- | Container for specifying the configuration when you want Amazon S3 to
-- publish events to an Amazon Simple Notification Service (Amazon SNS)
-- topic.
--
-- /See:/ 'topicConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tcId'
--
-- * 'tcTopicARN'
--
-- * 'tcEvents'
data TopicConfiguration = TopicConfiguration'
    { _tcId       :: !(Maybe Text)
    , _tcTopicARN :: !Text
    , _tcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TopicConfiguration' smart constructor.
topicConfiguration :: Text -> TopicConfiguration
topicConfiguration pTopicARN =
    TopicConfiguration'
    { _tcId = Nothing
    , _tcTopicARN = pTopicARN
    , _tcEvents = mempty
    }

-- | FIXME: Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\ s a -> s{_tcId = a});

-- | Amazon SNS topic ARN to which Amazon S3 will publish a message when it
-- detects events of specified type.
tcTopicARN :: Lens' TopicConfiguration Text
tcTopicARN = lens _tcTopicARN (\ s a -> s{_tcTopicARN = a});

-- | FIXME: Undocumented member.
tcEvents :: Lens' TopicConfiguration [Event]
tcEvents = lens _tcEvents (\ s a -> s{_tcEvents = a});

instance FromXML TopicConfiguration where
        parseXML x
          = TopicConfiguration' <$>
              (x .@? "Id") <*> (x .@ "Topic") <*>
                (parseXMLList "Event" x)

instance ToXML TopicConfiguration where
        toXML TopicConfiguration'{..}
          = mconcat
              ["Id" @= _tcId, "Topic" @= _tcTopicARN,
               toXMLList "Event" _tcEvents]

-- | /See:/ 'transition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'traDays'
--
-- * 'traDate'
--
-- * 'traStorageClass'
data Transition = Transition'
    { _traDays         :: !(Maybe Int)
    , _traDate         :: !(Maybe RFC822)
    , _traStorageClass :: !(Maybe TransitionStorageClass)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Transition' smart constructor.
transition :: Transition
transition =
    Transition'
    { _traDays = Nothing
    , _traDate = Nothing
    , _traStorageClass = Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
traDays :: Lens' Transition (Maybe Int)
traDays = lens _traDays (\ s a -> s{_traDays = a});

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
traDate :: Lens' Transition (Maybe UTCTime)
traDate = lens _traDate (\ s a -> s{_traDate = a}) . mapping _Time;

-- | The class of storage used to store the object.
traStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
traStorageClass = lens _traStorageClass (\ s a -> s{_traStorageClass = a});

instance FromXML Transition where
        parseXML x
          = Transition' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "StorageClass")

instance ToXML Transition where
        toXML Transition'{..}
          = mconcat
              ["Days" @= _traDays, "Date" @= _traDate,
               "StorageClass" @= _traStorageClass]

-- | /See:/ 'versioningConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcStatus'
--
-- * 'vcMFADelete'
data VersioningConfiguration = VersioningConfiguration'
    { _vcStatus    :: !(Maybe BucketVersioningStatus)
    , _vcMFADelete :: !(Maybe MFADelete)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VersioningConfiguration' smart constructor.
versioningConfiguration :: VersioningConfiguration
versioningConfiguration =
    VersioningConfiguration'
    { _vcStatus = Nothing
    , _vcMFADelete = Nothing
    }

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe BucketVersioningStatus)
vcStatus = lens _vcStatus (\ s a -> s{_vcStatus = a});

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe MFADelete)
vcMFADelete = lens _vcMFADelete (\ s a -> s{_vcMFADelete = a});

instance ToXML VersioningConfiguration where
        toXML VersioningConfiguration'{..}
          = mconcat
              ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]

-- | /See:/ 'websiteConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wcRedirectAllRequestsTo'
--
-- * 'wcErrorDocument'
--
-- * 'wcRoutingRules'
--
-- * 'wcIndexDocument'
data WebsiteConfiguration = WebsiteConfiguration'
    { _wcRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
    , _wcErrorDocument         :: !(Maybe ErrorDocument)
    , _wcRoutingRules          :: !(Maybe [RoutingRule])
    , _wcIndexDocument         :: !(Maybe IndexDocument)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'WebsiteConfiguration' smart constructor.
websiteConfiguration :: WebsiteConfiguration
websiteConfiguration =
    WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing
    , _wcErrorDocument = Nothing
    , _wcRoutingRules = Nothing
    , _wcIndexDocument = Nothing
    }

-- | FIXME: Undocumented member.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\ s a -> s{_wcRedirectAllRequestsTo = a});

-- | FIXME: Undocumented member.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\ s a -> s{_wcErrorDocument = a});

-- | FIXME: Undocumented member.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\ s a -> s{_wcRoutingRules = a}) . _Default;

-- | FIXME: Undocumented member.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\ s a -> s{_wcIndexDocument = a});

instance ToXML WebsiteConfiguration where
        toXML WebsiteConfiguration'{..}
          = mconcat
              ["RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
               "ErrorDocument" @= _wcErrorDocument,
               "RoutingRules" @=
                 toXML (toXMLList "RoutingRule" <$> _wcRoutingRules),
               "IndexDocument" @= _wcIndexDocument]
