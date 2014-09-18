{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a
-- simple web services interface that you can use to store and retrieve any
-- amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast, inexpensive
-- data storage infrastructure that Amazon uses to run its own global network
-- of web sites. The service aims to maximize benefits of scale and to pass
-- those benefits on to developers.
module Network.AWS.S3.Types
    (
    -- * Service
      S3
    -- ** Errors
    , S3Error (..)
    , _BucketAlreadyExists
    , _NoSuchBucket
    , _NoSuchKey
    , _NoSuchUpload
    , _ObjectAlreadyInActiveTierError
    , _ObjectNotInActiveTierError
    , _S3Client
    , _S3Serializer
    , _S3Service
    -- ** XML
    , xmlOptions

    -- * BucketVersioningStatus
    , BucketVersioningStatus

    -- * ExpirationStatus
    , ExpirationStatus

    -- * MFADelete
    , MFADelete

    -- * MFADeleteStatus
    , MFADeleteStatus

    -- * BucketCannedACL
    , BucketCannedACL (..)

    -- * BucketLogsPermission
    , BucketLogsPermission (..)

    -- * EncodingType
    , EncodingType (..)

    -- * Event
    , Event (..)

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

    -- * ServerSideEncryption
    , ServerSideEncryption (..)

    -- * StorageClass
    , StorageClass (..)

    -- * TransitionStorageClass
    , TransitionStorageClass (..)

    -- * Type
    , Type (..)

    -- * BucketLoggingStatus
    , BucketLoggingStatus
    , bucketLoggingStatus
    , blsLoggingEnabled

    -- * CORSConfiguration
    , CORSConfiguration
    , cORSConfiguration
    , corscCORSRules

    -- * CommonPrefix
    , CommonPrefix
    , commonPrefix
    , cprPrefix

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload
    , completedMultipartUpload
    , cmu1Parts

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration
    , createBucketConfiguration
    , cbcLocationConstraint

    -- * ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- * IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- * LifecycleConfiguration
    , LifecycleConfiguration
    , lifecycleConfiguration
    , lcRules

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , noncurrentVersionExpiration
    , nveNoncurrentDays

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicConfiguration

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , requestPaymentConfiguration
    , rpcPayer

    -- * RestoreRequest
    , RestoreRequest
    , restoreRequest
    , rr1Days

    -- * Tagging
    , Tagging
    , tagging
    , t1TagSet

    -- * AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- * Bucket
    , Bucket
    , bucket
    , bName
    , bCreationDate

    -- * CORSRule
    , CORSRule
    , cORSRule
    , corsrAllowedHeaders
    , corsrAllowedMethods
    , corsrAllowedOrigins
    , corsrExposeHeaders
    , corsrMaxAgeSeconds

    -- * CompletedPart
    , CompletedPart
    , completedPart
    , cpETag
    , cpPartNumber

    -- * Condition
    , Condition
    , condition
    , cHttpErrorCodeReturnedEquals
    , cKeyPrefixEquals

    -- * CopyObjectResult
    , CopyObjectResult
    , copyObjectResult
    , corrETag
    , corrLastModified

    -- * CopyPartResult
    , CopyPartResult
    , copyPartResult
    , cprrETag
    , cprrLastModified

    -- * Delete
    , Delete
    , delete
    , dObjects
    , dQuiet

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry
    , deleteMarkerEntry
    , dmeOwner
    , dmeKey
    , dmeVersionId
    , dmeIsLatest
    , dmeLastModified

    -- * DeletedObject
    , DeletedObject
    , deletedObject
    , do1rKey
    , do1rVersionId
    , do1rDeleteMarker
    , do1rDeleteMarkerVersionId

    -- * Error
    , Error
    , error'
    , eKey
    , eVersionId
    , eCode
    , eMessage

    -- * Grant
    , Grant
    , grant
    , gGrantee
    , gPermission

    -- * Grantee
    , Grantee
    , grantee
    , g1DisplayName
    , g1EmailAddress
    , g1ID
    , g1Type
    , g1URI

    -- * Initiator
    , Initiator
    , initiator
    , iID
    , iDisplayName

    -- * LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDate
    , leDays

    -- * LoggingEnabled
    , LoggingEnabled
    , loggingEnabled
    , lerTargetBucket
    , lerTargetGrants
    , lerTargetPrefix

    -- * MultipartUpload
    , MultipartUpload
    , multipartUpload
    , muUploadId
    , muKey
    , muInitiated
    , muStorageClass
    , muOwner
    , muInitiator

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , noncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * Object
    , Object
    , object
    , orKey
    , orLastModified
    , orETag
    , orSize
    , orStorageClass
    , orOwner

    -- * ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiKey
    , oiVersionId

    -- * ObjectVersion
    , ObjectVersion
    , objectVersion
    , ovETag
    , ovSize
    , ovStorageClass
    , ovKey
    , ovVersionId
    , ovIsLatest
    , ovLastModified
    , ovOwner

    -- * Owner
    , Owner
    , owner
    , oDisplayName
    , oID

    -- * Part
    , Part
    , part
    , pPartNumber
    , pLastModified
    , pETag
    , pSize

    -- * Redirect
    , Redirect
    , redirect
    , r1HostName
    , r1HttpRedirectCode
    , r1Protocol
    , r1ReplaceKeyPrefixWith
    , r1ReplaceKeyWith

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , redirectAllRequestsTo
    , rartHostName
    , rartProtocol

    -- * RoutingRule
    , RoutingRule
    , routingRule
    , rrCondition
    , rrRedirect

    -- * Rule
    , Rule
    , rule
    , rExpiration
    , rID
    , rPrefix
    , rStatus
    , rTransition
    , rNoncurrentVersionTransition
    , rNoncurrentVersionExpiration

    -- * Tag
    , Tag
    , tag
    , trKey
    , trValue

    -- * TargetGrant
    , TargetGrant
    , targetGrant
    , tgGrantee
    , tgPermission

    -- * TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcEvent
    , tcTopic

    -- * Transition
    , Transition
    , transition
    , tDate
    , tDays
    , tStorageClass

    -- * VersioningConfiguration
    , VersioningConfiguration
    , versioningConfiguration
    , vcStatus
    , vcMfaDelete

    -- * WebsiteConfiguration
    , WebsiteConfiguration
    , websiteConfiguration
    , wcErrorDocument
    , wcIndexDocument
    , wcRedirectAllRequestsTo
    , wcRoutingRules

    -- * Common
    , module Network.AWS.S3.Internal.Types
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4
import Network.AWS.Types (Region)
import Network.AWS.S3.Internal.Types

-- | Supported version (@2006-03-01@) of the
-- @Amazon Simple Storage Service@ service.
data S3 deriving (Typeable)

instance AWSService S3 where
    type Sg S3 = V4
    type Er S3 = S3Error

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "s3"
        , _svcVersion  = "2006-03-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'S3' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data S3Error
      -- | The requested bucket name is not available. The bucket namespace
      -- is shared by all users of the system. Please select a different
      -- name and try again.
    = BucketAlreadyExists
      -- | The specified bucket does not exist.
    | NoSuchBucket
      -- | The specified key does not exist.
    | NoSuchKey
      -- | The specified multipart upload does not exist.
    | NoSuchUpload
      -- | This operation is not allowed against this storage tier.
    | ObjectAlreadyInActiveTierError
      -- | The source object of the COPY operation is not in the active tier
      -- and is only stored in Amazon Glacier.
    | ObjectNotInActiveTierError
    | S3Client HttpException
    | S3Serializer String
    | S3Service String
      deriving (Show, Typeable, Generic)

instance AWSError S3Error where
    awsError = const "S3Error"

instance AWSServiceError S3Error where
    serviceError    = S3Service
    clientError     = S3Client
    serializerError = S3Serializer

instance Exception S3Error

-- | The requested bucket name is not available. The bucket namespace is shared
-- by all users of the system. Please select a different name and try again.
--
-- See: 'BucketAlreadyExists'
_BucketAlreadyExists :: Prism' S3Error ()
_BucketAlreadyExists = prism
    (const BucketAlreadyExists)
    (\case
        BucketAlreadyExists -> Right ()
        x -> Left x)

-- | The specified bucket does not exist.
--
-- See: 'NoSuchBucket'
_NoSuchBucket :: Prism' S3Error ()
_NoSuchBucket = prism
    (const NoSuchBucket)
    (\case
        NoSuchBucket -> Right ()
        x -> Left x)

-- | The specified key does not exist.
--
-- See: 'NoSuchKey'
_NoSuchKey :: Prism' S3Error ()
_NoSuchKey = prism
    (const NoSuchKey)
    (\case
        NoSuchKey -> Right ()
        x -> Left x)

-- | The specified multipart upload does not exist.
--
-- See: 'NoSuchUpload'
_NoSuchUpload :: Prism' S3Error ()
_NoSuchUpload = prism
    (const NoSuchUpload)
    (\case
        NoSuchUpload -> Right ()
        x -> Left x)

-- | This operation is not allowed against this storage tier.
--
-- See: 'ObjectAlreadyInActiveTierError'
_ObjectAlreadyInActiveTierError :: Prism' S3Error ()
_ObjectAlreadyInActiveTierError = prism
    (const ObjectAlreadyInActiveTierError)
    (\case
        ObjectAlreadyInActiveTierError -> Right ()
        x -> Left x)

-- | The source object of the COPY operation is not in the active tier and is
-- only stored in Amazon Glacier.
--
-- See: 'ObjectNotInActiveTierError'
_ObjectNotInActiveTierError :: Prism' S3Error ()
_ObjectNotInActiveTierError = prism
    (const ObjectNotInActiveTierError)
    (\case
        ObjectNotInActiveTierError -> Right ()
        x -> Left x)

-- | See: 'S3Client'
_S3Client :: Prism' S3Error HttpException
_S3Client = prism
    S3Client
    (\case
        S3Client p1 -> Right p1
        x -> Left x)

-- | See: 'S3Serializer'
_S3Serializer :: Prism' S3Error String
_S3Serializer = prism
    S3Serializer
    (\case
        S3Serializer p1 -> Right p1
        x -> Left x)

-- | See: 'S3Service'
_S3Service :: Prism' S3Error String
_S3Service = prism
    S3Service
    (\case
        S3Service p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data BucketVersioningStatus

instance FromText (Switch BucketVersioningStatus) where
    parser = match "Suspended" Disabled
         <|> match "Enabled" Enabled

instance ToText (Switch BucketVersioningStatus) where
    toText Disabled = "Suspended"
    toText Enabled = "Enabled"

instance ToByteString (Switch BucketVersioningStatus)

instance FromXML (Switch BucketVersioningStatus) where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketVersioningStatus"

instance ToXML (Switch BucketVersioningStatus) where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketVersioningStatus"

data ExpirationStatus

instance FromText (Switch ExpirationStatus) where
    parser = match "Disabled" Disabled
         <|> match "Enabled" Enabled

instance ToText (Switch ExpirationStatus) where
    toText Disabled = "Disabled"
    toText Enabled = "Enabled"

instance ToByteString (Switch ExpirationStatus)

instance FromXML (Switch ExpirationStatus) where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpirationStatus"

instance ToXML (Switch ExpirationStatus) where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ExpirationStatus"

data MFADelete

instance FromText (Switch MFADelete) where
    parser = match "Disabled" Disabled
         <|> match "Enabled" Enabled

instance ToText (Switch MFADelete) where
    toText Disabled = "Disabled"
    toText Enabled = "Enabled"

instance ToByteString (Switch MFADelete)

instance ToXML (Switch MFADelete) where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MfaDelete"

data MFADeleteStatus

instance FromText (Switch MFADeleteStatus) where
    parser = match "Disabled" Disabled
         <|> match "Enabled" Enabled

instance ToText (Switch MFADeleteStatus) where
    toText Disabled = "Disabled"
    toText Enabled = "Enabled"

instance ToByteString (Switch MFADeleteStatus)

instance FromXML (Switch MFADeleteStatus) where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MfaDelete"

data BucketCannedACL
    = BucketCannedACLAuthenticatedRead -- ^ authenticated-read
    | BucketCannedACLPrivate -- ^ private
    | BucketCannedACLPublicRead -- ^ public-read
    | BucketCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance Hashable BucketCannedACL

instance FromText BucketCannedACL where
    parser = match "authenticated-read" BucketCannedACLAuthenticatedRead
         <|> match "private" BucketCannedACLPrivate
         <|> match "public-read" BucketCannedACLPublicRead
         <|> match "public-read-write" BucketCannedACLPublicReadWrite

instance ToText BucketCannedACL where
    toText BucketCannedACLAuthenticatedRead = "authenticated-read"
    toText BucketCannedACLPrivate = "private"
    toText BucketCannedACLPublicRead = "public-read"
    toText BucketCannedACLPublicReadWrite = "public-read-write"

instance ToByteString BucketCannedACL

instance ToXML BucketCannedACL where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketCannedACL"

instance ToQuery BucketCannedACL where
      toQuery = toQuery . toBS

data BucketLogsPermission
    = BucketLogsPermissionFullControl -- ^ FULL_CONTROL
    | BucketLogsPermissionRead -- ^ READ
    | BucketLogsPermissionWrite -- ^ WRITE
      deriving (Eq, Show, Generic)

instance Hashable BucketLogsPermission

instance FromText BucketLogsPermission where
    parser = match "FULL_CONTROL" BucketLogsPermissionFullControl
         <|> match "READ" BucketLogsPermissionRead
         <|> match "WRITE" BucketLogsPermissionWrite

instance ToText BucketLogsPermission where
    toText BucketLogsPermissionFullControl = "FULL_CONTROL"
    toText BucketLogsPermissionRead = "READ"
    toText BucketLogsPermissionWrite = "WRITE"

instance ToByteString BucketLogsPermission

instance FromXML BucketLogsPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketLogsPermission"

instance ToXML BucketLogsPermission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLogsPermission"

instance ToQuery BucketLogsPermission where
      toQuery = toQuery . toBS

data EncodingType
    = EncodingTypeUrl -- ^ url
      deriving (Eq, Show, Generic)

instance Hashable EncodingType

instance FromText EncodingType where
    parser = match "url" EncodingTypeUrl

instance ToText EncodingType where
    toText EncodingTypeUrl = "url"

instance ToByteString EncodingType

instance FromXML EncodingType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EncodingType"

instance ToXML EncodingType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "EncodingType"

instance ToQuery EncodingType where
      toQuery = toQuery . toBS

data Event
    = EventS3ReducedRedundancyLostObject -- ^ s3:ReducedRedundancyLostObject
      deriving (Eq, Show, Generic)

instance Hashable Event

instance FromText Event where
    parser = match "s3:ReducedRedundancyLostObject" EventS3ReducedRedundancyLostObject

instance ToText Event where
    toText EventS3ReducedRedundancyLostObject = "s3:ReducedRedundancyLostObject"

instance ToByteString Event

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

instance ToXML Event where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Event"

instance ToQuery Event where
      toQuery = toQuery . toBS

data MetadataDirective
    = MetadataDirectiveCopy -- ^ COPY
    | MetadataDirectiveReplace -- ^ REPLACE
      deriving (Eq, Show, Generic)

instance Hashable MetadataDirective

instance FromText MetadataDirective where
    parser = match "COPY" MetadataDirectiveCopy
         <|> match "REPLACE" MetadataDirectiveReplace

instance ToText MetadataDirective where
    toText MetadataDirectiveCopy = "COPY"
    toText MetadataDirectiveReplace = "REPLACE"

instance ToByteString MetadataDirective

instance ToXML MetadataDirective where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MetadataDirective"

instance ToQuery MetadataDirective where
      toQuery = toQuery . toBS

data ObjectCannedACL
    = ObjectCannedACLAuthenticatedRead -- ^ authenticated-read
    | ObjectCannedACLBucketOwnerFullControl -- ^ bucket-owner-full-control
    | ObjectCannedACLBucketOwnerRead -- ^ bucket-owner-read
    | ObjectCannedACLPrivate -- ^ private
    | ObjectCannedACLPublicRead -- ^ public-read
    | ObjectCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance Hashable ObjectCannedACL

instance FromText ObjectCannedACL where
    parser = match "authenticated-read" ObjectCannedACLAuthenticatedRead
         <|> match "bucket-owner-full-control" ObjectCannedACLBucketOwnerFullControl
         <|> match "bucket-owner-read" ObjectCannedACLBucketOwnerRead
         <|> match "private" ObjectCannedACLPrivate
         <|> match "public-read" ObjectCannedACLPublicRead
         <|> match "public-read-write" ObjectCannedACLPublicReadWrite

instance ToText ObjectCannedACL where
    toText ObjectCannedACLAuthenticatedRead = "authenticated-read"
    toText ObjectCannedACLBucketOwnerFullControl = "bucket-owner-full-control"
    toText ObjectCannedACLBucketOwnerRead = "bucket-owner-read"
    toText ObjectCannedACLPrivate = "private"
    toText ObjectCannedACLPublicRead = "public-read"
    toText ObjectCannedACLPublicReadWrite = "public-read-write"

instance ToByteString ObjectCannedACL

instance ToXML ObjectCannedACL where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectCannedACL"

instance ToQuery ObjectCannedACL where
      toQuery = toQuery . toBS

data ObjectStorageClass
    = ObjectStorageClassGlacier -- ^ GLACIER
    | ObjectStorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | ObjectStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance Hashable ObjectStorageClass

instance FromText ObjectStorageClass where
    parser = match "GLACIER" ObjectStorageClassGlacier
         <|> match "REDUCED_REDUNDANCY" ObjectStorageClassReducedRedundancy
         <|> match "STANDARD" ObjectStorageClassStandard

instance ToText ObjectStorageClass where
    toText ObjectStorageClassGlacier = "GLACIER"
    toText ObjectStorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toText ObjectStorageClassStandard = "STANDARD"

instance ToByteString ObjectStorageClass

instance FromXML ObjectStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectStorageClass"

instance ToXML ObjectStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectStorageClass"

instance ToQuery ObjectStorageClass where
      toQuery = toQuery . toBS

data ObjectVersionStorageClass
    = ObjectVersionStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance Hashable ObjectVersionStorageClass

instance FromText ObjectVersionStorageClass where
    parser = match "STANDARD" ObjectVersionStorageClassStandard

instance ToText ObjectVersionStorageClass where
    toText ObjectVersionStorageClassStandard = "STANDARD"

instance ToByteString ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersionStorageClass"

instance ToXML ObjectVersionStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectVersionStorageClass"

instance ToQuery ObjectVersionStorageClass where
      toQuery = toQuery . toBS

data Payer
    = PayerBucketOwner -- ^ BucketOwner
    | PayerRequester -- ^ Requester
      deriving (Eq, Show, Generic)

instance Hashable Payer

instance FromText Payer where
    parser = match "BucketOwner" PayerBucketOwner
         <|> match "Requester" PayerRequester

instance ToText Payer where
    toText PayerBucketOwner = "BucketOwner"
    toText PayerRequester = "Requester"

instance ToByteString Payer

instance FromXML Payer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Payer"

instance ToXML Payer where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Payer"

instance ToQuery Payer where
      toQuery = toQuery . toBS

data Permission
    = PermissionFullControl -- ^ FULL_CONTROL
    | PermissionRead -- ^ READ
    | PermissionReadAcp -- ^ READ_ACP
    | PermissionWrite -- ^ WRITE
    | PermissionWriteAcp -- ^ WRITE_ACP
      deriving (Eq, Show, Generic)

instance Hashable Permission

instance FromText Permission where
    parser = match "FULL_CONTROL" PermissionFullControl
         <|> match "READ" PermissionRead
         <|> match "READ_ACP" PermissionReadAcp
         <|> match "WRITE" PermissionWrite
         <|> match "WRITE_ACP" PermissionWriteAcp

instance ToText Permission where
    toText PermissionFullControl = "FULL_CONTROL"
    toText PermissionRead = "READ"
    toText PermissionReadAcp = "READ_ACP"
    toText PermissionWrite = "WRITE"
    toText PermissionWriteAcp = "WRITE_ACP"

instance ToByteString Permission

instance FromXML Permission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Permission"

instance ToXML Permission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Permission"

instance ToQuery Permission where
      toQuery = toQuery . toBS

data Protocol
    = ProtocolHttp -- ^ http
    | ProtocolHttps -- ^ https
      deriving (Eq, Show, Generic)

instance Hashable Protocol

instance FromText Protocol where
    parser = match "http" ProtocolHttp
         <|> match "https" ProtocolHttps

instance ToText Protocol where
    toText ProtocolHttp = "http"
    toText ProtocolHttps = "https"

instance ToByteString Protocol

instance FromXML Protocol where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Protocol"

instance ToXML Protocol where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Protocol"

instance ToQuery Protocol where
      toQuery = toQuery . toBS

data ServerSideEncryption
    = ServerSideEncryptionAES256 -- ^ AES256
      deriving (Eq, Show, Generic)

instance Hashable ServerSideEncryption

instance FromText ServerSideEncryption where
    parser = match "AES256" ServerSideEncryptionAES256

instance ToText ServerSideEncryption where
    toText ServerSideEncryptionAES256 = "AES256"

instance ToByteString ServerSideEncryption

instance FromXML ServerSideEncryption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ServerSideEncryption"

instance ToQuery ServerSideEncryption where
      toQuery = toQuery . toBS

data StorageClass
    = StorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | StorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance Hashable StorageClass

instance FromText StorageClass where
    parser = match "REDUCED_REDUNDANCY" StorageClassReducedRedundancy
         <|> match "STANDARD" StorageClassStandard

instance ToText StorageClass where
    toText StorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toText StorageClassStandard = "STANDARD"

instance ToByteString StorageClass

instance FromXML StorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StorageClass"

instance ToXML StorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StorageClass"

instance ToQuery StorageClass where
      toQuery = toQuery . toBS

data TransitionStorageClass
    = TransitionStorageClassGlacier -- ^ GLACIER
      deriving (Eq, Show, Generic)

instance Hashable TransitionStorageClass

instance FromText TransitionStorageClass where
    parser = match "GLACIER" TransitionStorageClassGlacier

instance ToText TransitionStorageClass where
    toText TransitionStorageClassGlacier = "GLACIER"

instance ToByteString TransitionStorageClass

instance FromXML TransitionStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TransitionStorageClass"

instance ToXML TransitionStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TransitionStorageClass"

instance ToQuery TransitionStorageClass where
      toQuery = toQuery . toBS

data Type
    = TypeAmazonCustomerByEmail -- ^ AmazonCustomerByEmail
    | TypeCanonicalUser -- ^ CanonicalUser
    | TypeGroup -- ^ Group
      deriving (Eq, Show, Generic)

instance Hashable Type

instance FromText Type where
    parser = match "AmazonCustomerByEmail" TypeAmazonCustomerByEmail
         <|> match "CanonicalUser" TypeCanonicalUser
         <|> match "Group" TypeGroup

instance ToText Type where
    toText TypeAmazonCustomerByEmail = "AmazonCustomerByEmail"
    toText TypeCanonicalUser = "CanonicalUser"
    toText TypeGroup = "Group"

instance ToByteString Type

instance FromXML Type where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "xsi:type"

instance ToXML Type where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "xsi:type"

instance ToQuery Type where
      toQuery = toQuery . toBS

newtype BucketLoggingStatus = BucketLoggingStatus
    { _blsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BucketLoggingStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoggingEnabled ::@ @Maybe LoggingEnabled@
--
bucketLoggingStatus :: BucketLoggingStatus
bucketLoggingStatus = BucketLoggingStatus
    { _blsLoggingEnabled = Nothing
    }

blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled =
    lens _blsLoggingEnabled (\s a -> s { _blsLoggingEnabled = a })

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLoggingStatus"

newtype CORSConfiguration = CORSConfiguration
    { _corscCORSRules :: [CORSRule]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CORSConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CORSRules ::@ @[CORSRule]@
--
cORSConfiguration :: CORSConfiguration
cORSConfiguration = CORSConfiguration
    { _corscCORSRules = mempty
    }

corscCORSRules :: Lens' CORSConfiguration [CORSRule]
corscCORSRules = lens _corscCORSRules (\s a -> s { _corscCORSRules = a })

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSConfiguration"

newtype CommonPrefix = CommonPrefix
    { _cprPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CommonPrefix' data type.
--
-- 'CommonPrefix' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Prefix ::@ @Maybe Text@
--
commonPrefix :: CommonPrefix
commonPrefix = CommonPrefix
    { _cprPrefix = Nothing
    }

cprPrefix :: Lens' CommonPrefix (Maybe Text)
cprPrefix = lens _cprPrefix (\s a -> s { _cprPrefix = a })

instance FromXML CommonPrefix where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CommonPrefix"

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { _cmu1Parts :: [CompletedPart]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompletedMultipartUpload' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Parts ::@ @[CompletedPart]@
--
completedMultipartUpload :: CompletedMultipartUpload
completedMultipartUpload = CompletedMultipartUpload
    { _cmu1Parts = mempty
    }

cmu1Parts :: Lens' CompletedMultipartUpload [CompletedPart]
cmu1Parts = lens _cmu1Parts (\s a -> s { _cmu1Parts = a })

instance ToXML CompletedMultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompleteMultipartUpload"

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint :: Maybe Region
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateBucketConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LocationConstraint ::@ @Maybe Region@
--
createBucketConfiguration :: CreateBucketConfiguration
createBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint = Nothing
    }

-- | Specifies the region where the bucket will be created.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe Region)
cbcLocationConstraint =
    lens _cbcLocationConstraint (\s a -> s { _cbcLocationConstraint = a })

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateBucketConfiguration"

newtype ErrorDocument = ErrorDocument
    { _edKey :: ObjectKey
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ErrorDocument' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @ObjectKey@
--
errorDocument :: ObjectKey -- ^ 'edKey'
                -> ErrorDocument
errorDocument p1 = ErrorDocument
    { _edKey = p1
    }

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\s a -> s { _edKey = a })

instance FromXML ErrorDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ErrorDocument"

instance ToXML ErrorDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ErrorDocument"

newtype IndexDocument = IndexDocument
    { _idSuffix :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IndexDocument' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Suffix ::@ @Text@
--
indexDocument :: Text -- ^ 'idSuffix'
                -> IndexDocument
indexDocument p1 = IndexDocument
    { _idSuffix = p1
    }

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (e.g. if the suffix is index.html and you make a request
-- to samplebucket/images/ the data that is returned will be for the object
-- with the key name images/index.html) The suffix must not be empty and must
-- not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\s a -> s { _idSuffix = a })

instance FromXML IndexDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexDocument"

instance ToXML IndexDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "IndexDocument"

newtype LifecycleConfiguration = LifecycleConfiguration
    { _lcRules :: [Rule]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LifecycleConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Rules ::@ @[Rule]@
--
lifecycleConfiguration :: [Rule] -- ^ 'lcRules'
                         -> LifecycleConfiguration
lifecycleConfiguration p1 = LifecycleConfiguration
    { _lcRules = p1
    }

lcRules :: Lens' LifecycleConfiguration [Rule]
lcRules = lens _lcRules (\s a -> s { _lcRules = a })

instance ToXML LifecycleConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleConfiguration"

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon
-- S3 permanently deletes the noncurrent object versions. You set this
-- lifecycle configuration action on a bucket that has versioning enabled (or
-- suspended) to request that Amazon S3 delete noncurrent object versions at a
-- specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration
    { _nveNoncurrentDays :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NoncurrentVersionExpiration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NoncurrentDays ::@ @Maybe Integer@
--
noncurrentVersionExpiration :: NoncurrentVersionExpiration
noncurrentVersionExpiration = NoncurrentVersionExpiration
    { _nveNoncurrentDays = Nothing
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became Noncurrent
-- in the Amazon Simple Storage Service Developer Guide.
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration (Maybe Integer)
nveNoncurrentDays =
    lens _nveNoncurrentDays (\s a -> s { _nveNoncurrentDays = a })

instance FromXML NoncurrentVersionExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionExpiration"

instance ToXML NoncurrentVersionExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionExpiration"

newtype NotificationConfiguration = NotificationConfiguration
    { _ncTopicConfiguration :: TopicConfiguration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NotificationConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicConfiguration ::@ @TopicConfiguration@
--
notificationConfiguration :: TopicConfiguration -- ^ 'ncTopicConfiguration'
                            -> NotificationConfiguration
notificationConfiguration p1 = NotificationConfiguration
    { _ncTopicConfiguration = p1
    }

ncTopicConfiguration :: Lens' NotificationConfiguration TopicConfiguration
ncTopicConfiguration =
    lens _ncTopicConfiguration (\s a -> s { _ncTopicConfiguration = a })

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NotificationConfiguration"

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { _rpcPayer :: Payer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestPaymentConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Payer ::@ @Payer@
--
requestPaymentConfiguration :: Payer -- ^ 'rpcPayer'
                              -> RequestPaymentConfiguration
requestPaymentConfiguration p1 = RequestPaymentConfiguration
    { _rpcPayer = p1
    }

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\s a -> s { _rpcPayer = a })

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RequestPaymentConfiguration"

newtype RestoreRequest = RestoreRequest
    { _rr1Days :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RestoreRequest' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Days ::@ @Integer@
--
restoreRequest :: Integer -- ^ 'rr1Days'
                 -> RestoreRequest
restoreRequest p1 = RestoreRequest
    { _rr1Days = p1
    }

-- | Lifetime of the active copy in days.
rr1Days :: Lens' RestoreRequest Integer
rr1Days = lens _rr1Days (\s a -> s { _rr1Days = a })

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RestoreRequest"

newtype Tagging = Tagging
    { _t1TagSet :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tagging' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TagSet ::@ @[Tag]@
--
tagging :: [Tag] -- ^ 't1TagSet'
          -> Tagging
tagging p1 = Tagging
    { _t1TagSet = p1
    }

t1TagSet :: Lens' Tagging [Tag]
t1TagSet = lens _t1TagSet (\s a -> s { _t1TagSet = a })

instance ToXML Tagging where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tagging"

data AccessControlPolicy = AccessControlPolicy
    { _acpGrants :: [Grant]
    , _acpOwner :: Maybe Owner
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessControlPolicy' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Grants ::@ @[Grant]@
--
-- * @Owner ::@ @Maybe Owner@
--
accessControlPolicy :: AccessControlPolicy
accessControlPolicy = AccessControlPolicy
    { _acpGrants = mempty
    , _acpOwner = Nothing
    }

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\s a -> s { _acpGrants = a })

acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\s a -> s { _acpOwner = a })

instance ToXML AccessControlPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AccessControlPolicy"

data Bucket = Bucket
    { _bName :: Maybe BucketName
    , _bCreationDate :: Maybe RFC822
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Bucket' data type.
--
-- 'Bucket' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe BucketName@
--
-- * @CreationDate ::@ @Maybe RFC822@
--
bucket :: Bucket
bucket = Bucket
    { _bName = Nothing
    , _bCreationDate = Nothing
    }

-- | The name of the bucket.
bName :: Lens' Bucket (Maybe BucketName)
bName = lens _bName (\s a -> s { _bName = a })

-- | Date the bucket was created.
bCreationDate :: Lens' Bucket (Maybe RFC822)
bCreationDate = lens _bCreationDate (\s a -> s { _bCreationDate = a })

instance FromXML Bucket where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Bucket"

data CORSRule = CORSRule
    { _corsrAllowedHeaders :: [Text]
    , _corsrAllowedMethods :: [Text]
    , _corsrAllowedOrigins :: [Text]
    , _corsrExposeHeaders :: [Text]
    , _corsrMaxAgeSeconds :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CORSRule' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AllowedHeaders ::@ @[Text]@
--
-- * @AllowedMethods ::@ @[Text]@
--
-- * @AllowedOrigins ::@ @[Text]@
--
-- * @ExposeHeaders ::@ @[Text]@
--
-- * @MaxAgeSeconds ::@ @Maybe Integer@
--
cORSRule :: CORSRule
cORSRule = CORSRule
    { _corsrAllowedHeaders = mempty
    , _corsrAllowedMethods = mempty
    , _corsrAllowedOrigins = mempty
    , _corsrExposeHeaders = mempty
    , _corsrMaxAgeSeconds = Nothing
    }

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
corsrAllowedHeaders :: Lens' CORSRule [Text]
corsrAllowedHeaders =
    lens _corsrAllowedHeaders (\s a -> s { _corsrAllowedHeaders = a })

-- | Identifies HTTP methods that the domain/origin specified in the rule is
-- allowed to execute.
corsrAllowedMethods :: Lens' CORSRule [Text]
corsrAllowedMethods =
    lens _corsrAllowedMethods (\s a -> s { _corsrAllowedMethods = a })

-- | One or more origins you want customers to be able to access the bucket
-- from.
corsrAllowedOrigins :: Lens' CORSRule [Text]
corsrAllowedOrigins =
    lens _corsrAllowedOrigins (\s a -> s { _corsrAllowedOrigins = a })

-- | One or more headers in the response that you want customers to be able to
-- access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
corsrExposeHeaders :: Lens' CORSRule [Text]
corsrExposeHeaders =
    lens _corsrExposeHeaders (\s a -> s { _corsrExposeHeaders = a })

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
corsrMaxAgeSeconds :: Lens' CORSRule (Maybe Integer)
corsrMaxAgeSeconds =
    lens _corsrMaxAgeSeconds (\s a -> s { _corsrMaxAgeSeconds = a })

instance FromXML CORSRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CORSRule"

instance ToXML CORSRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSRule"

data CompletedPart = CompletedPart
    { _cpETag :: Maybe ETag
    , _cpPartNumber :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompletedPart' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @PartNumber ::@ @Maybe Integer@
--
completedPart :: CompletedPart
completedPart = CompletedPart
    { _cpETag = Nothing
    , _cpPartNumber = Nothing
    }

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart (Maybe ETag)
cpETag = lens _cpETag (\s a -> s { _cpETag = a })

-- | Part number that identifies the part.
cpPartNumber :: Lens' CompletedPart (Maybe Integer)
cpPartNumber = lens _cpPartNumber (\s a -> s { _cpPartNumber = a })

instance ToXML CompletedPart where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompletedPart"

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
data Condition = Condition
    { _cHttpErrorCodeReturnedEquals :: Maybe Text
    , _cKeyPrefixEquals :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Condition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HttpErrorCodeReturnedEquals ::@ @Maybe Text@
--
-- * @KeyPrefixEquals ::@ @Maybe Text@
--
condition :: Condition
condition = Condition
    { _cHttpErrorCodeReturnedEquals = Nothing
    , _cKeyPrefixEquals = Nothing
    }

-- | The HTTP error code when the redirect is applied. In the event of an error,
-- if the error code equals this value, then the specified redirect is
-- applied. Required when parent element Condition is specified and sibling
-- KeyPrefixEquals is not specified. If both are specified, then both must be
-- true for the redirect to be applied.
cHttpErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHttpErrorCodeReturnedEquals =
    lens _cHttpErrorCodeReturnedEquals
         (\s a -> s { _cHttpErrorCodeReturnedEquals = a })

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix docs/,
-- the key prefix will be /docs, which identifies all objects in the docs/
-- folder. Required when the parent element Condition is specified and sibling
-- HttpErrorCodeReturnedEquals is not specified. If both conditions are
-- specified, both must be true for the redirect to be applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals =
    lens _cKeyPrefixEquals (\s a -> s { _cKeyPrefixEquals = a })

instance FromXML Condition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Condition"

instance ToXML Condition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Condition"

data CopyObjectResult = CopyObjectResult
    { _corrETag :: Maybe ETag
    , _corrLastModified :: Maybe RFC822
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CopyObjectResult' data type.
--
-- 'CopyObjectResult' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @LastModified ::@ @Maybe RFC822@
--
copyObjectResult :: CopyObjectResult
copyObjectResult = CopyObjectResult
    { _corrETag = Nothing
    , _corrLastModified = Nothing
    }

corrETag :: Lens' CopyObjectResult (Maybe ETag)
corrETag = lens _corrETag (\s a -> s { _corrETag = a })

corrLastModified :: Lens' CopyObjectResult (Maybe RFC822)
corrLastModified =
    lens _corrLastModified (\s a -> s { _corrLastModified = a })

instance FromXML CopyObjectResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyObjectResult"

data CopyPartResult = CopyPartResult
    { _cprrETag :: Maybe ETag
    , _cprrLastModified :: Maybe RFC822
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CopyPartResult' data type.
--
-- 'CopyPartResult' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @LastModified ::@ @Maybe RFC822@
--
copyPartResult :: CopyPartResult
copyPartResult = CopyPartResult
    { _cprrETag = Nothing
    , _cprrLastModified = Nothing
    }

-- | Entity tag of the object.
cprrETag :: Lens' CopyPartResult (Maybe ETag)
cprrETag = lens _cprrETag (\s a -> s { _cprrETag = a })

-- | Date and time at which the object was uploaded.
cprrLastModified :: Lens' CopyPartResult (Maybe RFC822)
cprrLastModified =
    lens _cprrLastModified (\s a -> s { _cprrLastModified = a })

instance FromXML CopyPartResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyPartResult"

data Delete = Delete
    { _dObjects :: [ObjectIdentifier]
    , _dQuiet :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Delete' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Objects ::@ @[ObjectIdentifier]@
--
-- * @Quiet ::@ @Maybe Bool@
--
delete :: [ObjectIdentifier] -- ^ 'dObjects'
         -> Delete
delete p1 = Delete
    { _dObjects = p1
    , _dQuiet = Nothing
    }

dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\s a -> s { _dObjects = a })

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\s a -> s { _dQuiet = a })

instance ToXML Delete where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Delete"

data DeleteMarkerEntry = DeleteMarkerEntry
    { _dmeOwner :: Maybe Owner
    , _dmeKey :: Maybe ObjectKey
    , _dmeVersionId :: Maybe ObjectVersionId
    , _dmeIsLatest :: Maybe Bool
    , _dmeLastModified :: Maybe RFC822
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeleteMarkerEntry' data type.
--
-- 'DeleteMarkerEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Owner ::@ @Maybe Owner@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @IsLatest ::@ @Maybe Bool@
--
-- * @LastModified ::@ @Maybe RFC822@
--
deleteMarkerEntry :: DeleteMarkerEntry
deleteMarkerEntry = DeleteMarkerEntry
    { _dmeOwner = Nothing
    , _dmeKey = Nothing
    , _dmeVersionId = Nothing
    , _dmeIsLatest = Nothing
    , _dmeLastModified = Nothing
    }

dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\s a -> s { _dmeOwner = a })

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\s a -> s { _dmeKey = a })

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\s a -> s { _dmeVersionId = a })

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\s a -> s { _dmeIsLatest = a })

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe RFC822)
dmeLastModified = lens _dmeLastModified (\s a -> s { _dmeLastModified = a })

instance FromXML DeleteMarkerEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMarkerEntry"

data DeletedObject = DeletedObject
    { _do1rKey :: Maybe ObjectKey
    , _do1rVersionId :: Maybe ObjectVersionId
    , _do1rDeleteMarker :: Maybe Bool
    , _do1rDeleteMarkerVersionId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeletedObject' data type.
--
-- 'DeletedObject' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @DeleteMarker ::@ @Maybe Bool@
--
-- * @DeleteMarkerVersionId ::@ @Maybe Text@
--
deletedObject :: DeletedObject
deletedObject = DeletedObject
    { _do1rKey = Nothing
    , _do1rVersionId = Nothing
    , _do1rDeleteMarker = Nothing
    , _do1rDeleteMarkerVersionId = Nothing
    }

do1rKey :: Lens' DeletedObject (Maybe ObjectKey)
do1rKey = lens _do1rKey (\s a -> s { _do1rKey = a })

do1rVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
do1rVersionId = lens _do1rVersionId (\s a -> s { _do1rVersionId = a })

do1rDeleteMarker :: Lens' DeletedObject (Maybe Bool)
do1rDeleteMarker =
    lens _do1rDeleteMarker (\s a -> s { _do1rDeleteMarker = a })

do1rDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
do1rDeleteMarkerVersionId =
    lens _do1rDeleteMarkerVersionId
         (\s a -> s { _do1rDeleteMarkerVersionId = a })

instance FromXML DeletedObject where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletedObject"

data Error = Error
    { _eKey :: Maybe ObjectKey
    , _eVersionId :: Maybe ObjectVersionId
    , _eCode :: Maybe Text
    , _eMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Error' data type.
--
-- 'Error' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @Code ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
error' :: Error
error' = Error
    { _eKey = Nothing
    , _eVersionId = Nothing
    , _eCode = Nothing
    , _eMessage = Nothing
    }

eKey :: Lens' Error (Maybe ObjectKey)
eKey = lens _eKey (\s a -> s { _eKey = a })

eVersionId :: Lens' Error (Maybe ObjectVersionId)
eVersionId = lens _eVersionId (\s a -> s { _eVersionId = a })

eCode :: Lens' Error (Maybe Text)
eCode = lens _eCode (\s a -> s { _eCode = a })

eMessage :: Lens' Error (Maybe Text)
eMessage = lens _eMessage (\s a -> s { _eMessage = a })

instance FromXML Error where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Error"

data Grant = Grant
    { _gGrantee :: Maybe Grantee
    , _gPermission :: Maybe Permission
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Grant' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Grantee ::@ @Maybe Grantee@
--
-- * @Permission ::@ @Maybe Permission@
--
grant :: Grant
grant = Grant
    { _gGrantee = Nothing
    , _gPermission = Nothing
    }

gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\s a -> s { _gGrantee = a })

-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\s a -> s { _gPermission = a })

instance FromXML Grant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML Grant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data Grantee = Grantee
    { _g1DisplayName :: Maybe Text
    , _g1EmailAddress :: Maybe Text
    , _g1ID :: Maybe Text
    , _g1Type :: Type
    , _g1URI :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Grantee' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DisplayName ::@ @Maybe Text@
--
-- * @EmailAddress ::@ @Maybe Text@
--
-- * @ID ::@ @Maybe Text@
--
-- * @Type ::@ @Type@
--
-- * @URI ::@ @Maybe Text@
--
grantee :: Type -- ^ 'g1Type'
          -> Grantee
grantee p4 = Grantee
    { _g1DisplayName = Nothing
    , _g1EmailAddress = Nothing
    , _g1ID = Nothing
    , _g1Type = p4
    , _g1URI = Nothing
    }

-- | Screen name of the grantee.
g1DisplayName :: Lens' Grantee (Maybe Text)
g1DisplayName = lens _g1DisplayName (\s a -> s { _g1DisplayName = a })

-- | Email address of the grantee.
g1EmailAddress :: Lens' Grantee (Maybe Text)
g1EmailAddress = lens _g1EmailAddress (\s a -> s { _g1EmailAddress = a })

-- | The canonical user ID of the grantee.
g1ID :: Lens' Grantee (Maybe Text)
g1ID = lens _g1ID (\s a -> s { _g1ID = a })

-- | Type of grantee.
g1Type :: Lens' Grantee Type
g1Type = lens _g1Type (\s a -> s { _g1Type = a })

-- | URI of the grantee group.
g1URI :: Lens' Grantee (Maybe Text)
g1URI = lens _g1URI (\s a -> s { _g1URI = a })

instance FromXML Grantee where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grantee"

instance ToXML Grantee where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grantee"

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { _iID :: Maybe Text
    , _iDisplayName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Initiator' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ID ::@ @Maybe Text@
--
-- * @DisplayName ::@ @Maybe Text@
--
initiator :: Initiator
initiator = Initiator
    { _iID = Nothing
    , _iDisplayName = Nothing
    }

-- | If the principal is an AWS account, it provides the Canonical User ID. If
-- the principal is an IAM User, it provides a user ARN value.
iID :: Lens' Initiator (Maybe Text)
iID = lens _iID (\s a -> s { _iID = a })

-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\s a -> s { _iDisplayName = a })

instance FromXML Initiator where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Initiator"

instance ToXML Initiator where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Initiator"

data LifecycleExpiration = LifecycleExpiration
    { _leDate :: Maybe RFC822
    , _leDays :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LifecycleExpiration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Date ::@ @Maybe RFC822@
--
-- * @Days ::@ @Maybe Integer@
--
lifecycleExpiration :: LifecycleExpiration
lifecycleExpiration = LifecycleExpiration
    { _leDate = Nothing
    , _leDays = Nothing
    }

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe RFC822)
leDate = lens _leDate (\s a -> s { _leDate = a })

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Integer)
leDays = lens _leDays (\s a -> s { _leDays = a })

instance FromXML LifecycleExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleExpiration"

instance ToXML LifecycleExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleExpiration"

data LoggingEnabled = LoggingEnabled
    { _lerTargetBucket :: Maybe Text
    , _lerTargetGrants :: [TargetGrant]
    , _lerTargetPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoggingEnabled' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetBucket ::@ @Maybe Text@
--
-- * @TargetGrants ::@ @[TargetGrant]@
--
-- * @TargetPrefix ::@ @Maybe Text@
--
loggingEnabled :: LoggingEnabled
loggingEnabled = LoggingEnabled
    { _lerTargetBucket = Nothing
    , _lerTargetGrants = mempty
    , _lerTargetPrefix = Nothing
    }

-- | Specifies the bucket where you want Amazon S3 to store server access logs.
-- You can have your logs delivered to any bucket that you own, including the
-- same bucket that is being logged. You can also configure multiple buckets
-- to deliver their logs to the same target bucket. In this case you should
-- choose a different TargetPrefix for each source bucket so that the
-- delivered log files can be distinguished by key.
lerTargetBucket :: Lens' LoggingEnabled (Maybe Text)
lerTargetBucket = lens _lerTargetBucket (\s a -> s { _lerTargetBucket = a })

lerTargetGrants :: Lens' LoggingEnabled [TargetGrant]
lerTargetGrants = lens _lerTargetGrants (\s a -> s { _lerTargetGrants = a })

-- | This element lets you specify a prefix for the keys that the log files will
-- be stored under.
lerTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
lerTargetPrefix = lens _lerTargetPrefix (\s a -> s { _lerTargetPrefix = a })

instance FromXML LoggingEnabled where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoggingEnabled"

instance ToXML LoggingEnabled where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LoggingEnabled"

data MultipartUpload = MultipartUpload
    { _muUploadId :: Maybe Text
    , _muKey :: Maybe ObjectKey
    , _muInitiated :: Maybe RFC822
    , _muStorageClass :: Maybe StorageClass
    , _muOwner :: Maybe Owner
    , _muInitiator :: Maybe Initiator
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MultipartUpload' data type.
--
-- 'MultipartUpload' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UploadId ::@ @Maybe Text@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @Initiated ::@ @Maybe RFC822@
--
-- * @StorageClass ::@ @Maybe StorageClass@
--
-- * @Owner ::@ @Maybe Owner@
--
-- * @Initiator ::@ @Maybe Initiator@
--
multipartUpload :: MultipartUpload
multipartUpload = MultipartUpload
    { _muUploadId = Nothing
    , _muKey = Nothing
    , _muInitiated = Nothing
    , _muStorageClass = Nothing
    , _muOwner = Nothing
    , _muInitiator = Nothing
    }

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\s a -> s { _muUploadId = a })

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\s a -> s { _muKey = a })

-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe RFC822)
muInitiated = lens _muInitiated (\s a -> s { _muInitiated = a })

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\s a -> s { _muStorageClass = a })

muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\s a -> s { _muOwner = a })

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\s a -> s { _muInitiator = a })

instance FromXML MultipartUpload where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MultipartUpload"

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
data NoncurrentVersionTransition = NoncurrentVersionTransition
    { _nvtNoncurrentDays :: Maybe Integer
    , _nvtStorageClass :: Maybe TransitionStorageClass
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NoncurrentVersionTransition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NoncurrentDays ::@ @Maybe Integer@
--
-- * @StorageClass ::@ @Maybe TransitionStorageClass@
--
noncurrentVersionTransition :: NoncurrentVersionTransition
noncurrentVersionTransition = NoncurrentVersionTransition
    { _nvtNoncurrentDays = Nothing
    , _nvtStorageClass = Nothing
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became Noncurrent
-- in the Amazon Simple Storage Service Developer Guide.
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition (Maybe Integer)
nvtNoncurrentDays =
    lens _nvtNoncurrentDays (\s a -> s { _nvtNoncurrentDays = a })

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition (Maybe TransitionStorageClass)
nvtStorageClass = lens _nvtStorageClass (\s a -> s { _nvtStorageClass = a })

instance FromXML NoncurrentVersionTransition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionTransition"

instance ToXML NoncurrentVersionTransition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionTransition"

data Object = Object
    { _orKey :: ObjectKey
    , _orLastModified :: RFC822
    , _orETag :: ETag
    , _orSize :: Integer
    , _orStorageClass :: ObjectStorageClass
    , _orOwner :: Owner
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Object' data type.
--
-- 'Object' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @ObjectKey@
--
-- * @LastModified ::@ @RFC822@
--
-- * @ETag ::@ @ETag@
--
-- * @Size ::@ @Integer@
--
-- * @StorageClass ::@ @ObjectStorageClass@
--
-- * @Owner ::@ @Owner@
--
object :: ObjectKey -- ^ 'orKey'
         -> RFC822 -- ^ 'orLastModified'
         -> ETag -- ^ 'orETag'
         -> Integer -- ^ 'orSize'
         -> ObjectStorageClass -- ^ 'orStorageClass'
         -> Owner -- ^ 'orOwner'
         -> Object
object p1 p2 p3 p4 p5 p6 = Object
    { _orKey = p1
    , _orLastModified = p2
    , _orETag = p3
    , _orSize = p4
    , _orStorageClass = p5
    , _orOwner = p6
    }

orKey :: Lens' Object ObjectKey
orKey = lens _orKey (\s a -> s { _orKey = a })

orLastModified :: Lens' Object RFC822
orLastModified = lens _orLastModified (\s a -> s { _orLastModified = a })

orETag :: Lens' Object ETag
orETag = lens _orETag (\s a -> s { _orETag = a })

orSize :: Lens' Object Integer
orSize = lens _orSize (\s a -> s { _orSize = a })

-- | The class of storage used to store the object.
orStorageClass :: Lens' Object ObjectStorageClass
orStorageClass = lens _orStorageClass (\s a -> s { _orStorageClass = a })

orOwner :: Lens' Object Owner
orOwner = lens _orOwner (\s a -> s { _orOwner = a })

instance FromXML Object where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Object"

data ObjectIdentifier = ObjectIdentifier
    { _oiKey :: ObjectKey
    , _oiVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ObjectIdentifier' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
objectIdentifier :: ObjectKey -- ^ 'oiKey'
                   -> ObjectIdentifier
objectIdentifier p1 = ObjectIdentifier
    { _oiKey = p1
    , _oiVersionId = Nothing
    }

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\s a -> s { _oiKey = a })

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\s a -> s { _oiVersionId = a })

instance ToXML ObjectIdentifier where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectIdentifier"

data ObjectVersion = ObjectVersion
    { _ovETag :: Maybe ETag
    , _ovSize :: Maybe Integer
    , _ovStorageClass :: Maybe ObjectVersionStorageClass
    , _ovKey :: Maybe ObjectKey
    , _ovVersionId :: Maybe ObjectVersionId
    , _ovIsLatest :: Maybe Bool
    , _ovLastModified :: Maybe RFC822
    , _ovOwner :: Maybe Owner
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ObjectVersion' data type.
--
-- 'ObjectVersion' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @Size ::@ @Maybe Integer@
--
-- * @StorageClass ::@ @Maybe ObjectVersionStorageClass@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
-- * @IsLatest ::@ @Maybe Bool@
--
-- * @LastModified ::@ @Maybe RFC822@
--
-- * @Owner ::@ @Maybe Owner@
--
objectVersion :: ObjectVersion
objectVersion = ObjectVersion
    { _ovETag = Nothing
    , _ovSize = Nothing
    , _ovStorageClass = Nothing
    , _ovKey = Nothing
    , _ovVersionId = Nothing
    , _ovIsLatest = Nothing
    , _ovLastModified = Nothing
    , _ovOwner = Nothing
    }

ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\s a -> s { _ovETag = a })

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Integer)
ovSize = lens _ovSize (\s a -> s { _ovSize = a })

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\s a -> s { _ovStorageClass = a })

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\s a -> s { _ovKey = a })

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\s a -> s { _ovVersionId = a })

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\s a -> s { _ovIsLatest = a })

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe RFC822)
ovLastModified = lens _ovLastModified (\s a -> s { _ovLastModified = a })

ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\s a -> s { _ovOwner = a })

instance FromXML ObjectVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersion"

data Owner = Owner
    { _oDisplayName :: Maybe Text
    , _oID :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Owner' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DisplayName ::@ @Maybe Text@
--
-- * @ID ::@ @Maybe Text@
--
owner :: Owner
owner = Owner
    { _oDisplayName = Nothing
    , _oID = Nothing
    }

oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\s a -> s { _oDisplayName = a })

oID :: Lens' Owner (Maybe Text)
oID = lens _oID (\s a -> s { _oID = a })

instance FromXML Owner where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Owner"

instance ToXML Owner where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Owner"

data Part = Part
    { _pPartNumber :: Maybe Integer
    , _pLastModified :: Maybe RFC822
    , _pETag :: Maybe ETag
    , _pSize :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Part' data type.
--
-- 'Part' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PartNumber ::@ @Maybe Integer@
--
-- * @LastModified ::@ @Maybe RFC822@
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @Size ::@ @Maybe Integer@
--
part :: Part
part = Part
    { _pPartNumber = Nothing
    , _pLastModified = Nothing
    , _pETag = Nothing
    , _pSize = Nothing
    }

-- | Part number identifying the part.
pPartNumber :: Lens' Part (Maybe Integer)
pPartNumber = lens _pPartNumber (\s a -> s { _pPartNumber = a })

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe RFC822)
pLastModified = lens _pLastModified (\s a -> s { _pLastModified = a })

-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe ETag)
pETag = lens _pETag (\s a -> s { _pETag = a })

-- | Size of the uploaded part data.
pSize :: Lens' Part (Maybe Integer)
pSize = lens _pSize (\s a -> s { _pSize = a })

instance FromXML Part where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Part"

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
data Redirect = Redirect
    { _r1HostName :: Maybe Text
    , _r1HttpRedirectCode :: Maybe Text
    , _r1Protocol :: Maybe Protocol
    , _r1ReplaceKeyPrefixWith :: Maybe Text
    , _r1ReplaceKeyWith :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Redirect' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HostName ::@ @Maybe Text@
--
-- * @HttpRedirectCode ::@ @Maybe Text@
--
-- * @Protocol ::@ @Maybe Protocol@
--
-- * @ReplaceKeyPrefixWith ::@ @Maybe Text@
--
-- * @ReplaceKeyWith ::@ @Maybe Text@
--
redirect :: Redirect
redirect = Redirect
    { _r1HostName = Nothing
    , _r1HttpRedirectCode = Nothing
    , _r1Protocol = Nothing
    , _r1ReplaceKeyPrefixWith = Nothing
    , _r1ReplaceKeyWith = Nothing
    }

-- | The host name to use in the redirect request.
r1HostName :: Lens' Redirect (Maybe Text)
r1HostName = lens _r1HostName (\s a -> s { _r1HostName = a })

-- | The HTTP redirect code to use on the response. Not required if one of the
-- siblings is present.
r1HttpRedirectCode :: Lens' Redirect (Maybe Text)
r1HttpRedirectCode =
    lens _r1HttpRedirectCode (\s a -> s { _r1HttpRedirectCode = a })

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
r1Protocol :: Lens' Redirect (Maybe Protocol)
r1Protocol = lens _r1Protocol (\s a -> s { _r1Protocol = a })

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs/ (objects in the docs/
-- folder) to documents/, you can set a condition block with KeyPrefixEquals
-- set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents.
-- Not required if one of the siblings is present. Can be present only if
-- ReplaceKeyWith is not provided.
r1ReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
r1ReplaceKeyPrefixWith =
    lens _r1ReplaceKeyPrefixWith (\s a -> s { _r1ReplaceKeyPrefixWith = a })

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
r1ReplaceKeyWith :: Lens' Redirect (Maybe Text)
r1ReplaceKeyWith =
    lens _r1ReplaceKeyWith (\s a -> s { _r1ReplaceKeyWith = a })

instance FromXML Redirect where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Redirect"

instance ToXML Redirect where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Redirect"

data RedirectAllRequestsTo = RedirectAllRequestsTo
    { _rartHostName :: Text
    , _rartProtocol :: Maybe Protocol
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RedirectAllRequestsTo' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HostName ::@ @Text@
--
-- * @Protocol ::@ @Maybe Protocol@
--
redirectAllRequestsTo :: Text -- ^ 'rartHostName'
                        -> RedirectAllRequestsTo
redirectAllRequestsTo p1 = RedirectAllRequestsTo
    { _rartHostName = p1
    , _rartProtocol = Nothing
    }

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\s a -> s { _rartHostName = a })

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\s a -> s { _rartProtocol = a })

instance FromXML RedirectAllRequestsTo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RedirectAllRequestsTo"

instance ToXML RedirectAllRequestsTo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RedirectAllRequestsTo"

data RoutingRule = RoutingRule
    { _rrCondition :: Maybe Condition
    , _rrRedirect :: Redirect
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RoutingRule' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Condition ::@ @Maybe Condition@
--
-- * @Redirect ::@ @Redirect@
--
routingRule :: Redirect -- ^ 'rrRedirect'
              -> RoutingRule
routingRule p2 = RoutingRule
    { _rrCondition = Nothing
    , _rrRedirect = p2
    }

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\s a -> s { _rrCondition = a })

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\s a -> s { _rrRedirect = a })

instance FromXML RoutingRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RoutingRule"

instance ToXML RoutingRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RoutingRule"

data Rule = Rule
    { _rExpiration :: Maybe LifecycleExpiration
    , _rID :: Maybe Text
    , _rPrefix :: Text
    , _rStatus :: Switch ExpirationStatus
    , _rTransition :: Maybe Transition
    , _rNoncurrentVersionTransition :: Maybe NoncurrentVersionTransition
    , _rNoncurrentVersionExpiration :: Maybe NoncurrentVersionExpiration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Rule' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Expiration ::@ @Maybe LifecycleExpiration@
--
-- * @ID ::@ @Maybe Text@
--
-- * @Prefix ::@ @Text@
--
-- * @Status ::@ @Switch ExpirationStatus@
--
-- * @Transition ::@ @Maybe Transition@
--
-- * @NoncurrentVersionTransition ::@ @Maybe NoncurrentVersionTransition@
--
-- * @NoncurrentVersionExpiration ::@ @Maybe NoncurrentVersionExpiration@
--
rule :: Text -- ^ 'rPrefix'
       -> Switch ExpirationStatus -- ^ 'rStatus'
       -> Rule
rule p3 p4 = Rule
    { _rExpiration = Nothing
    , _rID = Nothing
    , _rPrefix = p3
    , _rStatus = p4
    , _rTransition = Nothing
    , _rNoncurrentVersionTransition = Nothing
    , _rNoncurrentVersionExpiration = Nothing
    }

rExpiration :: Lens' Rule (Maybe LifecycleExpiration)
rExpiration = lens _rExpiration (\s a -> s { _rExpiration = a })

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rID :: Lens' Rule (Maybe Text)
rID = lens _rID (\s a -> s { _rID = a })

-- | Prefix identifying one or more objects to which the rule applies.
rPrefix :: Lens' Rule Text
rPrefix = lens _rPrefix (\s a -> s { _rPrefix = a })

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
rStatus :: Lens' Rule (Switch ExpirationStatus)
rStatus = lens _rStatus (\s a -> s { _rStatus = a })

rTransition :: Lens' Rule (Maybe Transition)
rTransition = lens _rTransition (\s a -> s { _rTransition = a })

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
rNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
rNoncurrentVersionTransition =
    lens _rNoncurrentVersionTransition
         (\s a -> s { _rNoncurrentVersionTransition = a })

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon
-- S3 permanently deletes the noncurrent object versions. You set this
-- lifecycle configuration action on a bucket that has versioning enabled (or
-- suspended) to request that Amazon S3 delete noncurrent object versions at a
-- specific period in the object's lifetime.
rNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
rNoncurrentVersionExpiration =
    lens _rNoncurrentVersionExpiration
         (\s a -> s { _rNoncurrentVersionExpiration = a })

instance FromXML Rule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Rule"

instance ToXML Rule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Rule"

data Tag = Tag
    { _trKey :: ObjectKey
    , _trValue :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @ObjectKey@
--
-- * @Value ::@ @Text@
--
tag :: ObjectKey -- ^ 'trKey'
      -> Text -- ^ 'trValue'
      -> Tag
tag p1 p2 = Tag
    { _trKey = p1
    , _trValue = p2
    }

-- | Name of the tag.
trKey :: Lens' Tag ObjectKey
trKey = lens _trKey (\s a -> s { _trKey = a })

-- | Value of the tag.
trValue :: Lens' Tag Text
trValue = lens _trValue (\s a -> s { _trValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"

data TargetGrant = TargetGrant
    { _tgGrantee :: Maybe Grantee
    , _tgPermission :: Maybe BucketLogsPermission
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TargetGrant' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Grantee ::@ @Maybe Grantee@
--
-- * @Permission ::@ @Maybe BucketLogsPermission@
--
targetGrant :: TargetGrant
targetGrant = TargetGrant
    { _tgGrantee = Nothing
    , _tgPermission = Nothing
    }

tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\s a -> s { _tgGrantee = a })

-- | Logging permissions assigned to the Grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\s a -> s { _tgPermission = a })

instance FromXML TargetGrant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML TargetGrant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data TopicConfiguration = TopicConfiguration
    { _tcEvent :: Maybe Event
    , _tcTopic :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TopicConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Event ::@ @Maybe Event@
--
-- * @Topic ::@ @Maybe Text@
--
topicConfiguration :: TopicConfiguration
topicConfiguration = TopicConfiguration
    { _tcEvent = Nothing
    , _tcTopic = Nothing
    }

-- | Bucket event for which to send notifications.
tcEvent :: Lens' TopicConfiguration (Maybe Event)
tcEvent = lens _tcEvent (\s a -> s { _tcEvent = a })

-- | Amazon SNS topic to which Amazon S3 will publish a message to report the
-- specified events for the bucket.
tcTopic :: Lens' TopicConfiguration (Maybe Text)
tcTopic = lens _tcTopic (\s a -> s { _tcTopic = a })

instance FromXML TopicConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TopicConfiguration"

instance ToXML TopicConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TopicConfiguration"

data Transition = Transition
    { _tDate :: Maybe RFC822
    , _tDays :: Maybe Integer
    , _tStorageClass :: Maybe TransitionStorageClass
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Transition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Date ::@ @Maybe RFC822@
--
-- * @Days ::@ @Maybe Integer@
--
-- * @StorageClass ::@ @Maybe TransitionStorageClass@
--
transition :: Transition
transition = Transition
    { _tDate = Nothing
    , _tDays = Nothing
    , _tStorageClass = Nothing
    }

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
tDate :: Lens' Transition (Maybe RFC822)
tDate = lens _tDate (\s a -> s { _tDate = a })

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
tDays :: Lens' Transition (Maybe Integer)
tDays = lens _tDays (\s a -> s { _tDays = a })

-- | The class of storage used to store the object.
tStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
tStorageClass = lens _tStorageClass (\s a -> s { _tStorageClass = a })

instance FromXML Transition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Transition"

instance ToXML Transition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Transition"

data VersioningConfiguration = VersioningConfiguration
    { _vcStatus :: Maybe (Switch BucketVersioningStatus)
    , _vcMfaDelete :: Maybe (Switch MFADelete)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VersioningConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe (Switch BucketVersioningStatus)@
--
-- * @MfaDelete ::@ @Maybe (Switch MFADelete)@
--
versioningConfiguration :: VersioningConfiguration
versioningConfiguration = VersioningConfiguration
    { _vcStatus = Nothing
    , _vcMfaDelete = Nothing
    }

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe (Switch BucketVersioningStatus))
vcStatus = lens _vcStatus (\s a -> s { _vcStatus = a })

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMfaDelete :: Lens' VersioningConfiguration (Maybe (Switch MFADelete))
vcMfaDelete = lens _vcMfaDelete (\s a -> s { _vcMfaDelete = a })

instance ToXML VersioningConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "VersioningConfiguration"

data WebsiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument :: Maybe ErrorDocument
    , _wcIndexDocument :: Maybe IndexDocument
    , _wcRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _wcRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WebsiteConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ErrorDocument ::@ @Maybe ErrorDocument@
--
-- * @IndexDocument ::@ @Maybe IndexDocument@
--
-- * @RedirectAllRequestsTo ::@ @Maybe RedirectAllRequestsTo@
--
-- * @RoutingRules ::@ @[RoutingRule]@
--
websiteConfiguration :: WebsiteConfiguration
websiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument = Nothing
    , _wcIndexDocument = Nothing
    , _wcRedirectAllRequestsTo = Nothing
    , _wcRoutingRules = mempty
    }

wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\s a -> s { _wcErrorDocument = a })

wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\s a -> s { _wcIndexDocument = a })

wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo =
    lens _wcRedirectAllRequestsTo
         (\s a -> s { _wcRedirectAllRequestsTo = a })

wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\s a -> s { _wcRoutingRules = a })

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "WebsiteConfiguration"
