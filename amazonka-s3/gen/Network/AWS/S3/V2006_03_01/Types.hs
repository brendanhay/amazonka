{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.Types
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
module Network.AWS.S3.V2006_03_01.Types
    (
    -- * Service
      S3
    -- ** Errors
    , Er (..)
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

    -- * BucketLocationConstraint
    , BucketLocationConstraint (..)

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
    , mkBucketLoggingStatus
    , blsLoggingEnabled

    -- * CORSConfiguration
    , CORSConfiguration
    , mkCORSConfiguration
    , corscCORSRules

    -- * CommonPrefix
    , CommonPrefix
    , ccxPrefix

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload
    , mkCompletedMultipartUpload
    , cmuParts

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration
    , mkCreateBucketConfiguration
    , cbcLocationConstraint

    -- * ErrorDocument
    , ErrorDocument
    , mkErrorDocument
    , edKey

    -- * IndexDocument
    , IndexDocument
    , mkIndexDocument
    , ihSuffix

    -- * LifecycleConfiguration
    , LifecycleConfiguration
    , mkLifecycleConfiguration
    , lcRules

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , mkNoncurrentVersionExpiration
    , nveNoncurrentDays

    -- * NotificationConfiguration
    , NotificationConfiguration
    , mkNotificationConfiguration
    , ncTopicConfiguration

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , mkRequestPaymentConfiguration
    , rpcPayer

    -- * RestoreRequest
    , RestoreRequest
    , mkRestoreRequest
    , rzDays

    -- * Tagging
    , Tagging
    , mkTagging
    , twTagSet

    -- * AccessControlPolicy
    , AccessControlPolicy
    , mkAccessControlPolicy
    , acpGrants
    , acpOwner

    -- * Bucket
    , Bucket
    , bbxName
    , bbxCreationDate

    -- * CORSRule
    , CORSRule
    , mkCORSRule
    , corssAllowedHeaders
    , corssAllowedMethods
    , corssAllowedOrigins
    , corssExposeHeaders
    , corssMaxAgeSeconds

    -- * CompletedPart
    , CompletedPart
    , mkCompletedPart
    , cpETag
    , cpPartNumber

    -- * Condition
    , Condition
    , mkCondition
    , cnHttpErrorCodeReturnedEquals
    , cnKeyPrefixEquals

    -- * CopyObjectResult
    , CopyObjectResult
    , cosETag
    , cosLastModified

    -- * CopyPartResult
    , CopyPartResult
    , cprETag
    , cprLastModified

    -- * Delete
    , Delete
    , mkDelete
    , kObjects
    , kQuiet

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry
    , dmeOwner
    , dmeKey
    , dmeVersionId
    , dmeIsLatest
    , dmeLastModified

    -- * DeletedObject
    , DeletedObject
    , dpKey
    , dpVersionId
    , dpDeleteMarker
    , dpDeleteMarkerVersionId

    -- * Error
    , Error
    , oKey
    , oVersionId
    , oCode
    , oMessage

    -- * Grant
    , Grant
    , mkGrant
    , guGrantee
    , guPermission

    -- * Grantee
    , Grantee
    , mkGrantee
    , geDisplayName
    , geEmailAddress
    , geID
    , geType
    , geURI

    -- * Initiator
    , Initiator
    , mkInitiator
    , irID
    , irDisplayName

    -- * LifecycleExpiration
    , LifecycleExpiration
    , mkLifecycleExpiration
    , leDate
    , leDays

    -- * LoggingEnabled
    , LoggingEnabled
    , mkLoggingEnabled
    , lfTargetBucket
    , lfTargetGrants
    , lfTargetPrefix

    -- * MultipartUpload
    , MultipartUpload
    , mwUploadId
    , mwKey
    , mwInitiated
    , mwStorageClass
    , mwOwner
    , mwInitiator

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , mkNoncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * Object
    , Object
    , oowKey
    , oowLastModified
    , oowETag
    , oowSize
    , oowStorageClass
    , oowOwner

    -- * ObjectIdentifier
    , ObjectIdentifier
    , mkObjectIdentifier
    , oiKey
    , oiVersionId

    -- * ObjectVersion
    , ObjectVersion
    , oonETag
    , oonSize
    , oonStorageClass
    , oonKey
    , oonVersionId
    , oonIsLatest
    , oonLastModified
    , oonOwner

    -- * Owner
    , Owner
    , mkOwner
    , sDisplayName
    , sID

    -- * Part
    , Part
    , ptPartNumber
    , ptLastModified
    , ptETag
    , ptSize

    -- * Redirect
    , Redirect
    , mkRedirect
    , ruHostName
    , ruHttpRedirectCode
    , ruProtocol
    , ruReplaceKeyPrefixWith
    , ruReplaceKeyWith

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , mkRedirectAllRequestsTo
    , rartHostName
    , rartProtocol

    -- * RoutingRule
    , RoutingRule
    , mkRoutingRule
    , rtCondition
    , rtRedirect

    -- * Rule
    , Rule
    , mkRule
    , reExpiration
    , reID
    , rePrefix
    , reStatus
    , reTransition
    , reNoncurrentVersionTransition
    , reNoncurrentVersionExpiration

    -- * Tag
    , Tag
    , mkTag
    , tiKey
    , tiValue

    -- * TargetGrant
    , TargetGrant
    , mkTargetGrant
    , thGrantee
    , thPermission

    -- * TopicConfiguration
    , TopicConfiguration
    , mkTopicConfiguration
    , tcEvent
    , tcTopic

    -- * Transition
    , Transition
    , mkTransition
    , tnDate
    , tnDays
    , tnStorageClass

    -- * VersioningConfiguration
    , VersioningConfiguration
    , mkVersioningConfiguration
    , vcStatus
    , vcMfaDelete

    -- * WebsiteConfiguration
    , WebsiteConfiguration
    , mkWebsiteConfiguration
    , wcErrorDocument
    , wcIndexDocument
    , wcRedirectAllRequestsTo
    , wcRoutingRules

    -- * Common
    , module Network.AWS.S3.Internal.Types
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Signing.V4
import           Network.AWS.Types     (Region)
import qualified Network.AWS.Types.Map as Map
import           Network.AWS.S3.Internal.Types

-- | Supported version (@2006-03-01@) of the
-- @Amazon Simple Storage Service@ service.
data S3 deriving (Typeable)

instance AWSService S3 where
    type Sg S3 = V4
    data Er S3
        = BucketAlreadyExists
        | NoSuchBucket
        | NoSuchKey
        | NoSuchUpload
        | ObjectAlreadyInActiveTierError
        | ObjectNotInActiveTierError
        | S3Client HttpException
        | S3Serializer String
        | S3Service String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "s3"
        , _svcVersion  = "2006-03-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er S3)
deriving instance Generic (Er S3)

instance AWSError (Er S3) where
    awsError = const "S3Error"

instance AWSServiceError (Er S3) where
    serviceError    = S3Service
    clientError     = S3Client
    serializerError = S3Serializer

instance Exception (Er S3)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://s3.amazonaws.com/doc/2006-03-01/"
    }

-- | The versioning state of the bucket.
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

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
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

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
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

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
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

-- | The canned ACL to apply to the bucket.
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

-- | Specifies the region where the bucket will be created.
newtype BucketLocationConstraint = BucketLocationConstraint Region
    deriving (Eq, Show, Generic)

instance FromText BucketLocationConstraint where
    parser = BucketLocationConstraint <$> parser

instance ToText BucketLocationConstraint where
    toText (BucketLocationConstraint r) = toText r

instance ToByteString BucketLocationConstraint

instance FromXML BucketLocationConstraint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketLocationConstraint"
    fromXML        = const fromNodeContent

instance ToXML BucketLocationConstraint where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLocationConstraint"
    toXML o      = toXML (retag o) . toText

-- | Logging permissions assigned to the Grantee for the bucket.
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

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
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

-- | Bucket event for which to send notifications.
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

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
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

-- | The canned ACL to apply to the object.
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

-- | The class of storage used to store the object.
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

-- | The class of storage used to store the object.
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

-- | Specifies who pays for the download and request fees.
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

-- | Specifies the permission given to the grantee.
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

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
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

-- | The Server-side encryption algorithm used when storing this object in S3.
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

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
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

-- | The class of storage used to store the object.
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

-- | Type of grantee.
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

blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\s a -> s { _blsLoggingEnabled = a })
{-# INLINE blsLoggingEnabled #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BucketLoggingStatus' data type to populate a request.
mkBucketLoggingStatus :: BucketLoggingStatus
mkBucketLoggingStatus = BucketLoggingStatus
    { _blsLoggingEnabled = Nothing
    }
{-# INLINE mkBucketLoggingStatus #-}

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLoggingStatus"

newtype CORSConfiguration = CORSConfiguration
    { _corscCORSRules :: [CORSRule]
    } deriving (Show, Generic)

corscCORSRules :: Lens' CORSConfiguration ([CORSRule])
corscCORSRules = lens _corscCORSRules (\s a -> s { _corscCORSRules = a })
{-# INLINE corscCORSRules #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CORSConfiguration' data type to populate a request.
mkCORSConfiguration :: CORSConfiguration
mkCORSConfiguration = CORSConfiguration
    { _corscCORSRules = mempty
    }
{-# INLINE mkCORSConfiguration #-}

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSConfiguration"

newtype CommonPrefix = CommonPrefix
    { _ccxPrefix :: Maybe Text
    } deriving (Show, Generic)

ccxPrefix :: Lens' CommonPrefix (Maybe Text)
ccxPrefix = lens _ccxPrefix (\s a -> s { _ccxPrefix = a })
{-# INLINE ccxPrefix #-}

instance FromXML CommonPrefix where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CommonPrefix"

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { _cmuParts :: [CompletedPart]
    } deriving (Show, Generic)

cmuParts :: Lens' CompletedMultipartUpload ([CompletedPart])
cmuParts = lens _cmuParts (\s a -> s { _cmuParts = a })
{-# INLINE cmuParts #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompletedMultipartUpload' data type to populate a request.
mkCompletedMultipartUpload :: CompletedMultipartUpload
mkCompletedMultipartUpload = CompletedMultipartUpload
    { _cmuParts = mempty
    }
{-# INLINE mkCompletedMultipartUpload #-}

instance ToXML CompletedMultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompleteMultipartUpload"

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint :: Maybe BucketLocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Show, Generic)

-- | Specifies the region where the bucket will be created.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe BucketLocationConstraint)
cbcLocationConstraint = lens _cbcLocationConstraint (\s a -> s { _cbcLocationConstraint = a })
{-# INLINE cbcLocationConstraint #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CreateBucketConfiguration' data type to populate a request.
mkCreateBucketConfiguration :: CreateBucketConfiguration
mkCreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint = Nothing
    }
{-# INLINE mkCreateBucketConfiguration #-}

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateBucketConfiguration"

newtype ErrorDocument = ErrorDocument
    { _edKey :: ObjectKey
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Show, Generic)

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument (ObjectKey)
edKey = lens _edKey (\s a -> s { _edKey = a })
{-# INLINE edKey #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ErrorDocument' data type to populate a request.
mkErrorDocument :: ObjectKey -- ^ 'edKey'
                -> ErrorDocument
mkErrorDocument p1 = ErrorDocument
    { _edKey = p1
    }
{-# INLINE mkErrorDocument #-}

instance FromXML ErrorDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ErrorDocument"

instance ToXML ErrorDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ErrorDocument"

newtype IndexDocument = IndexDocument
    { _ihSuffix :: Text
      -- ^ A suffix that is appended to a request that is for a directory on
      -- the website endpoint (e.g. if the suffix is index.html and you
      -- make a request to samplebucket/images/ the data that is returned
      -- will be for the object with the key name images/index.html) The
      -- suffix must not be empty and must not include a slash character.
    } deriving (Show, Generic)

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (e.g. if the suffix is index.html and you make a request
-- to samplebucket/images/ the data that is returned will be for the object
-- with the key name images/index.html) The suffix must not be empty and must
-- not include a slash character.
ihSuffix :: Lens' IndexDocument (Text)
ihSuffix = lens _ihSuffix (\s a -> s { _ihSuffix = a })
{-# INLINE ihSuffix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IndexDocument' data type to populate a request.
mkIndexDocument :: Text -- ^ 'ihSuffix'
                -> IndexDocument
mkIndexDocument p1 = IndexDocument
    { _ihSuffix = p1
    }
{-# INLINE mkIndexDocument #-}

instance FromXML IndexDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexDocument"

instance ToXML IndexDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "IndexDocument"

newtype LifecycleConfiguration = LifecycleConfiguration
    { _lcRules :: [Rule]
    } deriving (Show, Generic)

lcRules :: Lens' LifecycleConfiguration ([Rule])
lcRules = lens _lcRules (\s a -> s { _lcRules = a })
{-# INLINE lcRules #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LifecycleConfiguration' data type to populate a request.
mkLifecycleConfiguration :: [Rule] -- ^ 'lcRules'
                         -> LifecycleConfiguration
mkLifecycleConfiguration p1 = LifecycleConfiguration
    { _lcRules = p1
    }
{-# INLINE mkLifecycleConfiguration #-}

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
      -- ^ Specifies the number of days an object is noncurrent before
      -- Amazon S3 can perform the associated action. For information
      -- about the noncurrent days calculations, see How Amazon S3
      -- Calculates When an Object Became Noncurrent in the Amazon Simple
      -- Storage Service Developer Guide.
    } deriving (Show, Generic)

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became Noncurrent
-- in the Amazon Simple Storage Service Developer Guide.
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration (Maybe Integer)
nveNoncurrentDays = lens _nveNoncurrentDays (\s a -> s { _nveNoncurrentDays = a })
{-# INLINE nveNoncurrentDays #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NoncurrentVersionExpiration' data type to populate a request.
mkNoncurrentVersionExpiration :: NoncurrentVersionExpiration
mkNoncurrentVersionExpiration = NoncurrentVersionExpiration
    { _nveNoncurrentDays = Nothing
    }
{-# INLINE mkNoncurrentVersionExpiration #-}

instance FromXML NoncurrentVersionExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionExpiration"

instance ToXML NoncurrentVersionExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionExpiration"

newtype NotificationConfiguration = NotificationConfiguration
    { _ncTopicConfiguration :: TopicConfiguration
    } deriving (Show, Generic)

ncTopicConfiguration :: Lens' NotificationConfiguration (TopicConfiguration)
ncTopicConfiguration = lens _ncTopicConfiguration (\s a -> s { _ncTopicConfiguration = a })
{-# INLINE ncTopicConfiguration #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NotificationConfiguration' data type to populate a request.
mkNotificationConfiguration :: TopicConfiguration -- ^ 'ncTopicConfiguration'
                            -> NotificationConfiguration
mkNotificationConfiguration p1 = NotificationConfiguration
    { _ncTopicConfiguration = p1
    }
{-# INLINE mkNotificationConfiguration #-}

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NotificationConfiguration"

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { _rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Show, Generic)

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration (Payer)
rpcPayer = lens _rpcPayer (\s a -> s { _rpcPayer = a })
{-# INLINE rpcPayer #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestPaymentConfiguration' data type to populate a request.
mkRequestPaymentConfiguration :: Payer -- ^ 'rpcPayer'
                              -> RequestPaymentConfiguration
mkRequestPaymentConfiguration p1 = RequestPaymentConfiguration
    { _rpcPayer = p1
    }
{-# INLINE mkRequestPaymentConfiguration #-}

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RequestPaymentConfiguration"

newtype RestoreRequest = RestoreRequest
    { _rzDays :: Integer
      -- ^ Lifetime of the active copy in days.
    } deriving (Show, Generic)

-- | Lifetime of the active copy in days.
rzDays :: Lens' RestoreRequest (Integer)
rzDays = lens _rzDays (\s a -> s { _rzDays = a })
{-# INLINE rzDays #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RestoreRequest' data type to populate a request.
mkRestoreRequest :: Integer -- ^ 'rzDays'
                 -> RestoreRequest
mkRestoreRequest p1 = RestoreRequest
    { _rzDays = p1
    }
{-# INLINE mkRestoreRequest #-}

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RestoreRequest"

newtype Tagging = Tagging
    { _twTagSet :: [Tag]
    } deriving (Show, Generic)

twTagSet :: Lens' Tagging ([Tag])
twTagSet = lens _twTagSet (\s a -> s { _twTagSet = a })
{-# INLINE twTagSet #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tagging' data type to populate a request.
mkTagging :: [Tag] -- ^ 'twTagSet'
          -> Tagging
mkTagging p1 = Tagging
    { _twTagSet = p1
    }
{-# INLINE mkTagging #-}

instance ToXML Tagging where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tagging"

data AccessControlPolicy = AccessControlPolicy
    { _acpGrants :: [Grant]
      -- ^ A list of grants.
    , _acpOwner :: Maybe Owner
    } deriving (Show, Generic)

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy ([Grant])
acpGrants = lens _acpGrants (\s a -> s { _acpGrants = a })
{-# INLINE acpGrants #-}

acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\s a -> s { _acpOwner = a })
{-# INLINE acpOwner #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessControlPolicy' data type to populate a request.
mkAccessControlPolicy :: AccessControlPolicy
mkAccessControlPolicy = AccessControlPolicy
    { _acpGrants = mempty
    , _acpOwner = Nothing
    }
{-# INLINE mkAccessControlPolicy #-}

instance ToXML AccessControlPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AccessControlPolicy"

data Bucket = Bucket
    { _bbxName :: Maybe BucketName
      -- ^ The name of the bucket.
    , _bbxCreationDate :: Maybe RFC822
      -- ^ Date the bucket was created.
    } deriving (Show, Generic)

-- | The name of the bucket.
bbxName :: Lens' Bucket (Maybe BucketName)
bbxName = lens _bbxName (\s a -> s { _bbxName = a })
{-# INLINE bbxName #-}

-- | Date the bucket was created.
bbxCreationDate :: Lens' Bucket (Maybe RFC822)
bbxCreationDate = lens _bbxCreationDate (\s a -> s { _bbxCreationDate = a })
{-# INLINE bbxCreationDate #-}

instance FromXML Bucket where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Bucket"

data CORSRule = CORSRule
    { _corssAllowedHeaders :: [Text]
      -- ^ Specifies which headers are allowed in a pre-flight OPTIONS
      -- request.
    , _corssAllowedMethods :: [Text]
      -- ^ Identifies HTTP methods that the domain/origin specified in the
      -- rule is allowed to execute.
    , _corssAllowedOrigins :: [Text]
      -- ^ One or more origins you want customers to be able to access the
      -- bucket from.
    , _corssExposeHeaders :: [Text]
      -- ^ One or more headers in the response that you want customers to be
      -- able to access from their applications (for example, from a
      -- JavaScript XMLHttpRequest object).
    , _corssMaxAgeSeconds :: Maybe Integer
      -- ^ The time in seconds that your browser is to cache the preflight
      -- response for the specified resource.
    } deriving (Show, Generic)

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
corssAllowedHeaders :: Lens' CORSRule ([Text])
corssAllowedHeaders = lens _corssAllowedHeaders (\s a -> s { _corssAllowedHeaders = a })
{-# INLINE corssAllowedHeaders #-}

-- | Identifies HTTP methods that the domain/origin specified in the rule is
-- allowed to execute.
corssAllowedMethods :: Lens' CORSRule ([Text])
corssAllowedMethods = lens _corssAllowedMethods (\s a -> s { _corssAllowedMethods = a })
{-# INLINE corssAllowedMethods #-}

-- | One or more origins you want customers to be able to access the bucket
-- from.
corssAllowedOrigins :: Lens' CORSRule ([Text])
corssAllowedOrigins = lens _corssAllowedOrigins (\s a -> s { _corssAllowedOrigins = a })
{-# INLINE corssAllowedOrigins #-}

-- | One or more headers in the response that you want customers to be able to
-- access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
corssExposeHeaders :: Lens' CORSRule ([Text])
corssExposeHeaders = lens _corssExposeHeaders (\s a -> s { _corssExposeHeaders = a })
{-# INLINE corssExposeHeaders #-}

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
corssMaxAgeSeconds :: Lens' CORSRule (Maybe Integer)
corssMaxAgeSeconds = lens _corssMaxAgeSeconds (\s a -> s { _corssMaxAgeSeconds = a })
{-# INLINE corssMaxAgeSeconds #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CORSRule' data type to populate a request.
mkCORSRule :: CORSRule
mkCORSRule = CORSRule
    { _corssAllowedHeaders = mempty
    , _corssAllowedMethods = mempty
    , _corssAllowedOrigins = mempty
    , _corssExposeHeaders = mempty
    , _corssMaxAgeSeconds = Nothing
    }
{-# INLINE mkCORSRule #-}

instance FromXML CORSRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CORSRule"

instance ToXML CORSRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSRule"

data CompletedPart = CompletedPart
    { _cpETag :: Maybe ETag
      -- ^ Entity tag returned when the part was uploaded.
    , _cpPartNumber :: Maybe Integer
      -- ^ Part number that identifies the part.
    } deriving (Show, Generic)

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart (Maybe ETag)
cpETag = lens _cpETag (\s a -> s { _cpETag = a })
{-# INLINE cpETag #-}

-- | Part number that identifies the part.
cpPartNumber :: Lens' CompletedPart (Maybe Integer)
cpPartNumber = lens _cpPartNumber (\s a -> s { _cpPartNumber = a })
{-# INLINE cpPartNumber #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompletedPart' data type to populate a request.
mkCompletedPart :: CompletedPart
mkCompletedPart = CompletedPart
    { _cpETag = Nothing
    , _cpPartNumber = Nothing
    }
{-# INLINE mkCompletedPart #-}

instance ToXML CompletedPart where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompletedPart"

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
data Condition = Condition
    { _cnHttpErrorCodeReturnedEquals :: Maybe Text
      -- ^ The HTTP error code when the redirect is applied. In the event of
      -- an error, if the error code equals this value, then the specified
      -- redirect is applied. Required when parent element Condition is
      -- specified and sibling KeyPrefixEquals is not specified. If both
      -- are specified, then both must be true for the redirect to be
      -- applied.
    , _cnKeyPrefixEquals :: Maybe Text
      -- ^ The object key name prefix when the redirect is applied. For
      -- example, to redirect requests for ExamplePage.html, the key
      -- prefix will be ExamplePage.html. To redirect request for all
      -- pages with the prefix docs/, the key prefix will be /docs, which
      -- identifies all objects in the docs/ folder. Required when the
      -- parent element Condition is specified and sibling
      -- HttpErrorCodeReturnedEquals is not specified. If both conditions
      -- are specified, both must be true for the redirect to be applied.
    } deriving (Show, Generic)

-- | The HTTP error code when the redirect is applied. In the event of an error,
-- if the error code equals this value, then the specified redirect is
-- applied. Required when parent element Condition is specified and sibling
-- KeyPrefixEquals is not specified. If both are specified, then both must be
-- true for the redirect to be applied.
cnHttpErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cnHttpErrorCodeReturnedEquals = lens _cnHttpErrorCodeReturnedEquals (\s a -> s { _cnHttpErrorCodeReturnedEquals = a })
{-# INLINE cnHttpErrorCodeReturnedEquals #-}

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix docs/,
-- the key prefix will be /docs, which identifies all objects in the docs/
-- folder. Required when the parent element Condition is specified and sibling
-- HttpErrorCodeReturnedEquals is not specified. If both conditions are
-- specified, both must be true for the redirect to be applied.
cnKeyPrefixEquals :: Lens' Condition (Maybe Text)
cnKeyPrefixEquals = lens _cnKeyPrefixEquals (\s a -> s { _cnKeyPrefixEquals = a })
{-# INLINE cnKeyPrefixEquals #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Condition' data type to populate a request.
mkCondition :: Condition
mkCondition = Condition
    { _cnHttpErrorCodeReturnedEquals = Nothing
    , _cnKeyPrefixEquals = Nothing
    }
{-# INLINE mkCondition #-}

instance FromXML Condition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Condition"

instance ToXML Condition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Condition"

data CopyObjectResult = CopyObjectResult
    { _cosETag :: Maybe ETag
    , _cosLastModified :: Maybe RFC822
    } deriving (Show, Generic)

cosETag :: Lens' CopyObjectResult (Maybe ETag)
cosETag = lens _cosETag (\s a -> s { _cosETag = a })
{-# INLINE cosETag #-}

cosLastModified :: Lens' CopyObjectResult (Maybe RFC822)
cosLastModified = lens _cosLastModified (\s a -> s { _cosLastModified = a })
{-# INLINE cosLastModified #-}

instance FromXML CopyObjectResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyObjectResult"

data CopyPartResult = CopyPartResult
    { _cprETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cprLastModified :: Maybe RFC822
      -- ^ Date and time at which the object was uploaded.
    } deriving (Show, Generic)

-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\s a -> s { _cprETag = a })
{-# INLINE cprETag #-}

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe RFC822)
cprLastModified = lens _cprLastModified (\s a -> s { _cprLastModified = a })
{-# INLINE cprLastModified #-}

instance FromXML CopyPartResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyPartResult"

data Delete = Delete
    { _kObjects :: [ObjectIdentifier]
    , _kQuiet :: Maybe Bool
      -- ^ Element to enable quiet mode for the request. When you add this
      -- element, you must set its value to true.
    } deriving (Show, Generic)

kObjects :: Lens' Delete ([ObjectIdentifier])
kObjects = lens _kObjects (\s a -> s { _kObjects = a })
{-# INLINE kObjects #-}

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
kQuiet :: Lens' Delete (Maybe Bool)
kQuiet = lens _kQuiet (\s a -> s { _kQuiet = a })
{-# INLINE kQuiet #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Delete' data type to populate a request.
mkDelete :: [ObjectIdentifier] -- ^ 'kObjects'
         -> Delete
mkDelete p1 = Delete
    { _kObjects = p1
    , _kQuiet = Nothing
    }
{-# INLINE mkDelete #-}

instance ToXML Delete where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Delete"

data DeleteMarkerEntry = DeleteMarkerEntry
    { _dmeOwner :: Maybe Owner
    , _dmeKey :: Maybe ObjectKey
      -- ^ The object key.
    , _dmeVersionId :: Maybe ObjectVersionId
      -- ^ Version ID of an object.
    , _dmeIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , _dmeLastModified :: Maybe RFC822
      -- ^ Date and time the object was last modified.
    } deriving (Show, Generic)

dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\s a -> s { _dmeOwner = a })
{-# INLINE dmeOwner #-}

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\s a -> s { _dmeKey = a })
{-# INLINE dmeKey #-}

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\s a -> s { _dmeVersionId = a })
{-# INLINE dmeVersionId #-}

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\s a -> s { _dmeIsLatest = a })
{-# INLINE dmeIsLatest #-}

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe RFC822)
dmeLastModified = lens _dmeLastModified (\s a -> s { _dmeLastModified = a })
{-# INLINE dmeLastModified #-}

instance FromXML DeleteMarkerEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMarkerEntry"

data DeletedObject = DeletedObject
    { _dpKey :: Maybe ObjectKey
    , _dpVersionId :: Maybe ObjectVersionId
    , _dpDeleteMarker :: Maybe Bool
    , _dpDeleteMarkerVersionId :: Maybe Text
    } deriving (Show, Generic)

dpKey :: Lens' DeletedObject (Maybe ObjectKey)
dpKey = lens _dpKey (\s a -> s { _dpKey = a })
{-# INLINE dpKey #-}

dpVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dpVersionId = lens _dpVersionId (\s a -> s { _dpVersionId = a })
{-# INLINE dpVersionId #-}

dpDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dpDeleteMarker = lens _dpDeleteMarker (\s a -> s { _dpDeleteMarker = a })
{-# INLINE dpDeleteMarker #-}

dpDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dpDeleteMarkerVersionId = lens _dpDeleteMarkerVersionId (\s a -> s { _dpDeleteMarkerVersionId = a })
{-# INLINE dpDeleteMarkerVersionId #-}

instance FromXML DeletedObject where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletedObject"

data Error = Error
    { _oKey :: Maybe ObjectKey
    , _oVersionId :: Maybe ObjectVersionId
    , _oCode :: Maybe Text
    , _oMessage :: Maybe Text
    } deriving (Show, Generic)

oKey :: Lens' Error (Maybe ObjectKey)
oKey = lens _oKey (\s a -> s { _oKey = a })
{-# INLINE oKey #-}

oVersionId :: Lens' Error (Maybe ObjectVersionId)
oVersionId = lens _oVersionId (\s a -> s { _oVersionId = a })
{-# INLINE oVersionId #-}

oCode :: Lens' Error (Maybe Text)
oCode = lens _oCode (\s a -> s { _oCode = a })
{-# INLINE oCode #-}

oMessage :: Lens' Error (Maybe Text)
oMessage = lens _oMessage (\s a -> s { _oMessage = a })
{-# INLINE oMessage #-}

instance FromXML Error where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Error"

data Grant = Grant
    { _guGrantee :: Maybe Grantee
    , _guPermission :: Maybe Permission
      -- ^ Specifies the permission given to the grantee.
    } deriving (Show, Generic)

guGrantee :: Lens' Grant (Maybe Grantee)
guGrantee = lens _guGrantee (\s a -> s { _guGrantee = a })
{-# INLINE guGrantee #-}

-- | Specifies the permission given to the grantee.
guPermission :: Lens' Grant (Maybe Permission)
guPermission = lens _guPermission (\s a -> s { _guPermission = a })
{-# INLINE guPermission #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Grant' data type to populate a request.
mkGrant :: Grant
mkGrant = Grant
    { _guGrantee = Nothing
    , _guPermission = Nothing
    }
{-# INLINE mkGrant #-}

instance FromXML Grant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML Grant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data Grantee = Grantee
    { _geDisplayName :: Maybe Text
      -- ^ Screen name of the grantee.
    , _geEmailAddress :: Maybe Text
      -- ^ Email address of the grantee.
    , _geID :: Maybe Text
      -- ^ The canonical user ID of the grantee.
    , _geType :: Type
      -- ^ Type of grantee.
    , _geURI :: Maybe Text
      -- ^ URI of the grantee group.
    } deriving (Show, Generic)

-- | Screen name of the grantee.
geDisplayName :: Lens' Grantee (Maybe Text)
geDisplayName = lens _geDisplayName (\s a -> s { _geDisplayName = a })
{-# INLINE geDisplayName #-}

-- | Email address of the grantee.
geEmailAddress :: Lens' Grantee (Maybe Text)
geEmailAddress = lens _geEmailAddress (\s a -> s { _geEmailAddress = a })
{-# INLINE geEmailAddress #-}

-- | The canonical user ID of the grantee.
geID :: Lens' Grantee (Maybe Text)
geID = lens _geID (\s a -> s { _geID = a })
{-# INLINE geID #-}

-- | Type of grantee.
geType :: Lens' Grantee (Type)
geType = lens _geType (\s a -> s { _geType = a })
{-# INLINE geType #-}

-- | URI of the grantee group.
geURI :: Lens' Grantee (Maybe Text)
geURI = lens _geURI (\s a -> s { _geURI = a })
{-# INLINE geURI #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Grantee' data type to populate a request.
mkGrantee :: Type -- ^ 'geType'
          -> Grantee
mkGrantee p1 = Grantee
    { _geDisplayName = Nothing
    , _geEmailAddress = Nothing
    , _geID = Nothing
    , _geType = p4
    , _geURI = Nothing
    }
{-# INLINE mkGrantee #-}

instance FromXML Grantee where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grantee"

instance ToXML Grantee where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grantee"

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { _irID :: Maybe Text
      -- ^ If the principal is an AWS account, it provides the Canonical
      -- User ID. If the principal is an IAM User, it provides a user ARN
      -- value.
    , _irDisplayName :: Maybe Text
      -- ^ Name of the Principal.
    } deriving (Show, Generic)

-- | If the principal is an AWS account, it provides the Canonical User ID. If
-- the principal is an IAM User, it provides a user ARN value.
irID :: Lens' Initiator (Maybe Text)
irID = lens _irID (\s a -> s { _irID = a })
{-# INLINE irID #-}

-- | Name of the Principal.
irDisplayName :: Lens' Initiator (Maybe Text)
irDisplayName = lens _irDisplayName (\s a -> s { _irDisplayName = a })
{-# INLINE irDisplayName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Initiator' data type to populate a request.
mkInitiator :: Initiator
mkInitiator = Initiator
    { _irID = Nothing
    , _irDisplayName = Nothing
    }
{-# INLINE mkInitiator #-}

instance FromXML Initiator where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Initiator"

instance ToXML Initiator where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Initiator"

data LifecycleExpiration = LifecycleExpiration
    { _leDate :: Maybe RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    , _leDays :: Maybe Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    } deriving (Show, Generic)

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe RFC822)
leDate = lens _leDate (\s a -> s { _leDate = a })
{-# INLINE leDate #-}

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Integer)
leDays = lens _leDays (\s a -> s { _leDays = a })
{-# INLINE leDays #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LifecycleExpiration' data type to populate a request.
mkLifecycleExpiration :: LifecycleExpiration
mkLifecycleExpiration = LifecycleExpiration
    { _leDate = Nothing
    , _leDays = Nothing
    }
{-# INLINE mkLifecycleExpiration #-}

instance FromXML LifecycleExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleExpiration"

instance ToXML LifecycleExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleExpiration"

data LoggingEnabled = LoggingEnabled
    { _lfTargetBucket :: Maybe Text
      -- ^ Specifies the bucket where you want Amazon S3 to store server
      -- access logs. You can have your logs delivered to any bucket that
      -- you own, including the same bucket that is being logged. You can
      -- also configure multiple buckets to deliver their logs to the same
      -- target bucket. In this case you should choose a different
      -- TargetPrefix for each source bucket so that the delivered log
      -- files can be distinguished by key.
    , _lfTargetGrants :: [TargetGrant]
    , _lfTargetPrefix :: Maybe Text
      -- ^ This element lets you specify a prefix for the keys that the log
      -- files will be stored under.
    } deriving (Show, Generic)

-- | Specifies the bucket where you want Amazon S3 to store server access logs.
-- You can have your logs delivered to any bucket that you own, including the
-- same bucket that is being logged. You can also configure multiple buckets
-- to deliver their logs to the same target bucket. In this case you should
-- choose a different TargetPrefix for each source bucket so that the
-- delivered log files can be distinguished by key.
lfTargetBucket :: Lens' LoggingEnabled (Maybe Text)
lfTargetBucket = lens _lfTargetBucket (\s a -> s { _lfTargetBucket = a })
{-# INLINE lfTargetBucket #-}

lfTargetGrants :: Lens' LoggingEnabled ([TargetGrant])
lfTargetGrants = lens _lfTargetGrants (\s a -> s { _lfTargetGrants = a })
{-# INLINE lfTargetGrants #-}

-- | This element lets you specify a prefix for the keys that the log files will
-- be stored under.
lfTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
lfTargetPrefix = lens _lfTargetPrefix (\s a -> s { _lfTargetPrefix = a })
{-# INLINE lfTargetPrefix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoggingEnabled' data type to populate a request.
mkLoggingEnabled :: LoggingEnabled
mkLoggingEnabled = LoggingEnabled
    { _lfTargetBucket = Nothing
    , _lfTargetGrants = mempty
    , _lfTargetPrefix = Nothing
    }
{-# INLINE mkLoggingEnabled #-}

instance FromXML LoggingEnabled where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoggingEnabled"

instance ToXML LoggingEnabled where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LoggingEnabled"

data MultipartUpload = MultipartUpload
    { _mwUploadId :: Maybe Text
      -- ^ Upload ID that identifies the multipart upload.
    , _mwKey :: Maybe ObjectKey
      -- ^ Key of the object for which the multipart upload was initiated.
    , _mwInitiated :: Maybe RFC822
      -- ^ Date and time at which the multipart upload was initiated.
    , _mwStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , _mwOwner :: Maybe Owner
    , _mwInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    } deriving (Show, Generic)

-- | Upload ID that identifies the multipart upload.
mwUploadId :: Lens' MultipartUpload (Maybe Text)
mwUploadId = lens _mwUploadId (\s a -> s { _mwUploadId = a })
{-# INLINE mwUploadId #-}

-- | Key of the object for which the multipart upload was initiated.
mwKey :: Lens' MultipartUpload (Maybe ObjectKey)
mwKey = lens _mwKey (\s a -> s { _mwKey = a })
{-# INLINE mwKey #-}

-- | Date and time at which the multipart upload was initiated.
mwInitiated :: Lens' MultipartUpload (Maybe RFC822)
mwInitiated = lens _mwInitiated (\s a -> s { _mwInitiated = a })
{-# INLINE mwInitiated #-}

-- | The class of storage used to store the object.
mwStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
mwStorageClass = lens _mwStorageClass (\s a -> s { _mwStorageClass = a })
{-# INLINE mwStorageClass #-}

mwOwner :: Lens' MultipartUpload (Maybe Owner)
mwOwner = lens _mwOwner (\s a -> s { _mwOwner = a })
{-# INLINE mwOwner #-}

-- | Identifies who initiated the multipart upload.
mwInitiator :: Lens' MultipartUpload (Maybe Initiator)
mwInitiator = lens _mwInitiator (\s a -> s { _mwInitiator = a })
{-# INLINE mwInitiator #-}

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
      -- ^ Specifies the number of days an object is noncurrent before
      -- Amazon S3 can perform the associated action. For information
      -- about the noncurrent days calculations, see How Amazon S3
      -- Calculates When an Object Became Noncurrent in the Amazon Simple
      -- Storage Service Developer Guide.
    , _nvtStorageClass :: Maybe TransitionStorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became Noncurrent
-- in the Amazon Simple Storage Service Developer Guide.
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition (Maybe Integer)
nvtNoncurrentDays = lens _nvtNoncurrentDays (\s a -> s { _nvtNoncurrentDays = a })
{-# INLINE nvtNoncurrentDays #-}

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition (Maybe TransitionStorageClass)
nvtStorageClass = lens _nvtStorageClass (\s a -> s { _nvtStorageClass = a })
{-# INLINE nvtStorageClass #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NoncurrentVersionTransition' data type to populate a request.
mkNoncurrentVersionTransition :: NoncurrentVersionTransition
mkNoncurrentVersionTransition = NoncurrentVersionTransition
    { _nvtNoncurrentDays = Nothing
    , _nvtStorageClass = Nothing
    }
{-# INLINE mkNoncurrentVersionTransition #-}

instance FromXML NoncurrentVersionTransition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionTransition"

instance ToXML NoncurrentVersionTransition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionTransition"

data Object = Object
    { _oowKey :: ObjectKey
    , _oowLastModified :: RFC822
    , _oowETag :: ETag
    , _oowSize :: Integer
    , _oowStorageClass :: ObjectStorageClass
      -- ^ The class of storage used to store the object.
    , _oowOwner :: Owner
    } deriving (Show, Generic)

oowKey :: Lens' Object (ObjectKey)
oowKey = lens _oowKey (\s a -> s { _oowKey = a })
{-# INLINE oowKey #-}

oowLastModified :: Lens' Object (RFC822)
oowLastModified = lens _oowLastModified (\s a -> s { _oowLastModified = a })
{-# INLINE oowLastModified #-}

oowETag :: Lens' Object (ETag)
oowETag = lens _oowETag (\s a -> s { _oowETag = a })
{-# INLINE oowETag #-}

oowSize :: Lens' Object (Integer)
oowSize = lens _oowSize (\s a -> s { _oowSize = a })
{-# INLINE oowSize #-}

-- | The class of storage used to store the object.
oowStorageClass :: Lens' Object (ObjectStorageClass)
oowStorageClass = lens _oowStorageClass (\s a -> s { _oowStorageClass = a })
{-# INLINE oowStorageClass #-}

oowOwner :: Lens' Object (Owner)
oowOwner = lens _oowOwner (\s a -> s { _oowOwner = a })
{-# INLINE oowOwner #-}

instance FromXML Object where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Object"

data ObjectIdentifier = ObjectIdentifier
    { _oiKey :: ObjectKey
      -- ^ Key name of the object to delete.
    , _oiVersionId :: Maybe ObjectVersionId
      -- ^ VersionId for the specific version of the object to delete.
    } deriving (Show, Generic)

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier (ObjectKey)
oiKey = lens _oiKey (\s a -> s { _oiKey = a })
{-# INLINE oiKey #-}

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\s a -> s { _oiVersionId = a })
{-# INLINE oiVersionId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ObjectIdentifier' data type to populate a request.
mkObjectIdentifier :: ObjectKey -- ^ 'oiKey'
                   -> ObjectIdentifier
mkObjectIdentifier p1 = ObjectIdentifier
    { _oiKey = p1
    , _oiVersionId = Nothing
    }
{-# INLINE mkObjectIdentifier #-}

instance ToXML ObjectIdentifier where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectIdentifier"

data ObjectVersion = ObjectVersion
    { _oonETag :: Maybe ETag
    , _oonSize :: Maybe Integer
      -- ^ Size in bytes of the object.
    , _oonStorageClass :: Maybe ObjectVersionStorageClass
      -- ^ The class of storage used to store the object.
    , _oonKey :: Maybe ObjectKey
      -- ^ The object key.
    , _oonVersionId :: Maybe ObjectVersionId
      -- ^ Version ID of an object.
    , _oonIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , _oonLastModified :: Maybe RFC822
      -- ^ Date and time the object was last modified.
    , _oonOwner :: Maybe Owner
    } deriving (Show, Generic)

oonETag :: Lens' ObjectVersion (Maybe ETag)
oonETag = lens _oonETag (\s a -> s { _oonETag = a })
{-# INLINE oonETag #-}

-- | Size in bytes of the object.
oonSize :: Lens' ObjectVersion (Maybe Integer)
oonSize = lens _oonSize (\s a -> s { _oonSize = a })
{-# INLINE oonSize #-}

-- | The class of storage used to store the object.
oonStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
oonStorageClass = lens _oonStorageClass (\s a -> s { _oonStorageClass = a })
{-# INLINE oonStorageClass #-}

-- | The object key.
oonKey :: Lens' ObjectVersion (Maybe ObjectKey)
oonKey = lens _oonKey (\s a -> s { _oonKey = a })
{-# INLINE oonKey #-}

-- | Version ID of an object.
oonVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
oonVersionId = lens _oonVersionId (\s a -> s { _oonVersionId = a })
{-# INLINE oonVersionId #-}

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
oonIsLatest :: Lens' ObjectVersion (Maybe Bool)
oonIsLatest = lens _oonIsLatest (\s a -> s { _oonIsLatest = a })
{-# INLINE oonIsLatest #-}

-- | Date and time the object was last modified.
oonLastModified :: Lens' ObjectVersion (Maybe RFC822)
oonLastModified = lens _oonLastModified (\s a -> s { _oonLastModified = a })
{-# INLINE oonLastModified #-}

oonOwner :: Lens' ObjectVersion (Maybe Owner)
oonOwner = lens _oonOwner (\s a -> s { _oonOwner = a })
{-# INLINE oonOwner #-}

instance FromXML ObjectVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersion"

data Owner = Owner
    { _sDisplayName :: Maybe Text
    , _sID :: Maybe Text
    } deriving (Show, Generic)

sDisplayName :: Lens' Owner (Maybe Text)
sDisplayName = lens _sDisplayName (\s a -> s { _sDisplayName = a })
{-# INLINE sDisplayName #-}

sID :: Lens' Owner (Maybe Text)
sID = lens _sID (\s a -> s { _sID = a })
{-# INLINE sID #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Owner' data type to populate a request.
mkOwner :: Owner
mkOwner = Owner
    { _sDisplayName = Nothing
    , _sID = Nothing
    }
{-# INLINE mkOwner #-}

instance FromXML Owner where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Owner"

instance ToXML Owner where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Owner"

data Part = Part
    { _ptPartNumber :: Maybe Integer
      -- ^ Part number identifying the part.
    , _ptLastModified :: Maybe RFC822
      -- ^ Date and time at which the part was uploaded.
    , _ptETag :: Maybe ETag
      -- ^ Entity tag returned when the part was uploaded.
    , _ptSize :: Maybe Integer
      -- ^ Size of the uploaded part data.
    } deriving (Show, Generic)

-- | Part number identifying the part.
ptPartNumber :: Lens' Part (Maybe Integer)
ptPartNumber = lens _ptPartNumber (\s a -> s { _ptPartNumber = a })
{-# INLINE ptPartNumber #-}

-- | Date and time at which the part was uploaded.
ptLastModified :: Lens' Part (Maybe RFC822)
ptLastModified = lens _ptLastModified (\s a -> s { _ptLastModified = a })
{-# INLINE ptLastModified #-}

-- | Entity tag returned when the part was uploaded.
ptETag :: Lens' Part (Maybe ETag)
ptETag = lens _ptETag (\s a -> s { _ptETag = a })
{-# INLINE ptETag #-}

-- | Size of the uploaded part data.
ptSize :: Lens' Part (Maybe Integer)
ptSize = lens _ptSize (\s a -> s { _ptSize = a })
{-# INLINE ptSize #-}

instance FromXML Part where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Part"

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
data Redirect = Redirect
    { _ruHostName :: Maybe Text
      -- ^ The host name to use in the redirect request.
    , _ruHttpRedirectCode :: Maybe Text
      -- ^ The HTTP redirect code to use on the response. Not required if
      -- one of the siblings is present.
    , _ruProtocol :: Maybe Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    , _ruReplaceKeyPrefixWith :: Maybe Text
      -- ^ The object key prefix to use in the redirect request. For
      -- example, to redirect requests for all pages with prefix docs/
      -- (objects in the docs/ folder) to documents/, you can set a
      -- condition block with KeyPrefixEquals set to docs/ and in the
      -- Redirect set ReplaceKeyPrefixWith to /documents. Not required if
      -- one of the siblings is present. Can be present only if
      -- ReplaceKeyWith is not provided.
    , _ruReplaceKeyWith :: Maybe Text
      -- ^ The specific object key to use in the redirect request. For
      -- example, redirect request to error.html. Not required if one of
      -- the sibling is present. Can be present only if
      -- ReplaceKeyPrefixWith is not provided.
    } deriving (Show, Generic)

-- | The host name to use in the redirect request.
ruHostName :: Lens' Redirect (Maybe Text)
ruHostName = lens _ruHostName (\s a -> s { _ruHostName = a })
{-# INLINE ruHostName #-}

-- | The HTTP redirect code to use on the response. Not required if one of the
-- siblings is present.
ruHttpRedirectCode :: Lens' Redirect (Maybe Text)
ruHttpRedirectCode = lens _ruHttpRedirectCode (\s a -> s { _ruHttpRedirectCode = a })
{-# INLINE ruHttpRedirectCode #-}

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
ruProtocol :: Lens' Redirect (Maybe Protocol)
ruProtocol = lens _ruProtocol (\s a -> s { _ruProtocol = a })
{-# INLINE ruProtocol #-}

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs/ (objects in the docs/
-- folder) to documents/, you can set a condition block with KeyPrefixEquals
-- set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents.
-- Not required if one of the siblings is present. Can be present only if
-- ReplaceKeyWith is not provided.
ruReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
ruReplaceKeyPrefixWith = lens _ruReplaceKeyPrefixWith (\s a -> s { _ruReplaceKeyPrefixWith = a })
{-# INLINE ruReplaceKeyPrefixWith #-}

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
ruReplaceKeyWith :: Lens' Redirect (Maybe Text)
ruReplaceKeyWith = lens _ruReplaceKeyWith (\s a -> s { _ruReplaceKeyWith = a })
{-# INLINE ruReplaceKeyWith #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Redirect' data type to populate a request.
mkRedirect :: Redirect
mkRedirect = Redirect
    { _ruHostName = Nothing
    , _ruHttpRedirectCode = Nothing
    , _ruProtocol = Nothing
    , _ruReplaceKeyPrefixWith = Nothing
    , _ruReplaceKeyWith = Nothing
    }
{-# INLINE mkRedirect #-}

instance FromXML Redirect where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Redirect"

instance ToXML Redirect where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Redirect"

data RedirectAllRequestsTo = RedirectAllRequestsTo
    { _rartHostName :: Text
      -- ^ Name of the host where requests will be redirected.
    , _rartProtocol :: Maybe Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    } deriving (Show, Generic)

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo (Text)
rartHostName = lens _rartHostName (\s a -> s { _rartHostName = a })
{-# INLINE rartHostName #-}

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\s a -> s { _rartProtocol = a })
{-# INLINE rartProtocol #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RedirectAllRequestsTo' data type to populate a request.
mkRedirectAllRequestsTo :: Text -- ^ 'rartHostName'
                        -> RedirectAllRequestsTo
mkRedirectAllRequestsTo p1 = RedirectAllRequestsTo
    { _rartHostName = p1
    , _rartProtocol = Nothing
    }
{-# INLINE mkRedirectAllRequestsTo #-}

instance FromXML RedirectAllRequestsTo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RedirectAllRequestsTo"

instance ToXML RedirectAllRequestsTo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RedirectAllRequestsTo"

data RoutingRule = RoutingRule
    { _rtCondition :: Maybe Condition
      -- ^ A container for describing a condition that must be met for the
      -- specified redirect to apply. For example, 1. If request is for
      -- pages in the /docs folder, redirect to the /documents folder. 2.
      -- If request results in HTTP error 4xx, redirect request to another
      -- host where you might process the error.
    , _rtRedirect :: Redirect
      -- ^ Container for redirect information. You can redirect requests to
      -- another host, to another page, or with another protocol. In the
      -- event of an error, you can can specify a different error code to
      -- return.
    } deriving (Show, Generic)

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
rtCondition :: Lens' RoutingRule (Maybe Condition)
rtCondition = lens _rtCondition (\s a -> s { _rtCondition = a })
{-# INLINE rtCondition #-}

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
rtRedirect :: Lens' RoutingRule (Redirect)
rtRedirect = lens _rtRedirect (\s a -> s { _rtRedirect = a })
{-# INLINE rtRedirect #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RoutingRule' data type to populate a request.
mkRoutingRule :: Redirect -- ^ 'rtRedirect'
              -> RoutingRule
mkRoutingRule p1 = RoutingRule
    { _rtCondition = Nothing
    , _rtRedirect = p2
    }
{-# INLINE mkRoutingRule #-}

instance FromXML RoutingRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RoutingRule"

instance ToXML RoutingRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RoutingRule"

data Rule = Rule
    { _reExpiration :: Maybe LifecycleExpiration
    , _reID :: Maybe Text
      -- ^ Unique identifier for the rule. The value cannot be longer than
      -- 255 characters.
    , _rePrefix :: Text
      -- ^ Prefix identifying one or more objects to which the rule applies.
    , _reStatus :: Switch ExpirationStatus
      -- ^ If 'Enabled', the rule is currently being applied. If 'Disabled',
      -- the rule is not currently being applied.
    , _reTransition :: Maybe Transition
    , _reNoncurrentVersionTransition :: Maybe NoncurrentVersionTransition
      -- ^ Container for the transition rule that describes when noncurrent
      -- objects transition to the GLACIER storage class. If your bucket
      -- is versioning-enabled (or versioning is suspended), you can set
      -- this action to request that Amazon S3 transition noncurrent
      -- object versions to the GLACIER storage class at a specific period
      -- in the object's lifetime.
    , _reNoncurrentVersionExpiration :: Maybe NoncurrentVersionExpiration
      -- ^ Specifies when noncurrent object versions expire. Upon
      -- expiration, Amazon S3 permanently deletes the noncurrent object
      -- versions. You set this lifecycle configuration action on a bucket
      -- that has versioning enabled (or suspended) to request that Amazon
      -- S3 delete noncurrent object versions at a specific period in the
      -- object's lifetime.
    } deriving (Show, Generic)

reExpiration :: Lens' Rule (Maybe LifecycleExpiration)
reExpiration = lens _reExpiration (\s a -> s { _reExpiration = a })
{-# INLINE reExpiration #-}

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
reID :: Lens' Rule (Maybe Text)
reID = lens _reID (\s a -> s { _reID = a })
{-# INLINE reID #-}

-- | Prefix identifying one or more objects to which the rule applies.
rePrefix :: Lens' Rule (Text)
rePrefix = lens _rePrefix (\s a -> s { _rePrefix = a })
{-# INLINE rePrefix #-}

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
reStatus :: Lens' Rule (Switch ExpirationStatus)
reStatus = lens _reStatus (\s a -> s { _reStatus = a })
{-# INLINE reStatus #-}

reTransition :: Lens' Rule (Maybe Transition)
reTransition = lens _reTransition (\s a -> s { _reTransition = a })
{-# INLINE reTransition #-}

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
reNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
reNoncurrentVersionTransition = lens _reNoncurrentVersionTransition (\s a -> s { _reNoncurrentVersionTransition = a })
{-# INLINE reNoncurrentVersionTransition #-}

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon
-- S3 permanently deletes the noncurrent object versions. You set this
-- lifecycle configuration action on a bucket that has versioning enabled (or
-- suspended) to request that Amazon S3 delete noncurrent object versions at a
-- specific period in the object's lifetime.
reNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
reNoncurrentVersionExpiration = lens _reNoncurrentVersionExpiration (\s a -> s { _reNoncurrentVersionExpiration = a })
{-# INLINE reNoncurrentVersionExpiration #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Rule' data type to populate a request.
mkRule :: Text -- ^ 'rePrefix'
       -> Switch ExpirationStatus -- ^ 'reStatus'
       -> Rule
mkRule p1 p2 = Rule
    { _reExpiration = Nothing
    , _reID = Nothing
    , _rePrefix = p3
    , _reStatus = p4
    , _reTransition = Nothing
    , _reNoncurrentVersionTransition = Nothing
    , _reNoncurrentVersionExpiration = Nothing
    }
{-# INLINE mkRule #-}

instance FromXML Rule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Rule"

instance ToXML Rule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Rule"

data Tag = Tag
    { _tiKey :: ObjectKey
      -- ^ Name of the tag.
    , _tiValue :: Text
      -- ^ Value of the tag.
    } deriving (Show, Generic)

-- | Name of the tag.
tiKey :: Lens' Tag (ObjectKey)
tiKey = lens _tiKey (\s a -> s { _tiKey = a })
{-# INLINE tiKey #-}

-- | Value of the tag.
tiValue :: Lens' Tag (Text)
tiValue = lens _tiValue (\s a -> s { _tiValue = a })
{-# INLINE tiValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: ObjectKey -- ^ 'tiKey'
      -> Text -- ^ 'tiValue'
      -> Tag
mkTag p1 p2 = Tag
    { _tiKey = p1
    , _tiValue = p2
    }
{-# INLINE mkTag #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"

data TargetGrant = TargetGrant
    { _thGrantee :: Maybe Grantee
    , _thPermission :: Maybe BucketLogsPermission
      -- ^ Logging permissions assigned to the Grantee for the bucket.
    } deriving (Show, Generic)

thGrantee :: Lens' TargetGrant (Maybe Grantee)
thGrantee = lens _thGrantee (\s a -> s { _thGrantee = a })
{-# INLINE thGrantee #-}

-- | Logging permissions assigned to the Grantee for the bucket.
thPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
thPermission = lens _thPermission (\s a -> s { _thPermission = a })
{-# INLINE thPermission #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TargetGrant' data type to populate a request.
mkTargetGrant :: TargetGrant
mkTargetGrant = TargetGrant
    { _thGrantee = Nothing
    , _thPermission = Nothing
    }
{-# INLINE mkTargetGrant #-}

instance FromXML TargetGrant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML TargetGrant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data TopicConfiguration = TopicConfiguration
    { _tcEvent :: Maybe Event
      -- ^ Bucket event for which to send notifications.
    , _tcTopic :: Maybe Text
      -- ^ Amazon SNS topic to which Amazon S3 will publish a message to
      -- report the specified events for the bucket.
    } deriving (Show, Generic)

-- | Bucket event for which to send notifications.
tcEvent :: Lens' TopicConfiguration (Maybe Event)
tcEvent = lens _tcEvent (\s a -> s { _tcEvent = a })
{-# INLINE tcEvent #-}

-- | Amazon SNS topic to which Amazon S3 will publish a message to report the
-- specified events for the bucket.
tcTopic :: Lens' TopicConfiguration (Maybe Text)
tcTopic = lens _tcTopic (\s a -> s { _tcTopic = a })
{-# INLINE tcTopic #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TopicConfiguration' data type to populate a request.
mkTopicConfiguration :: TopicConfiguration
mkTopicConfiguration = TopicConfiguration
    { _tcEvent = Nothing
    , _tcTopic = Nothing
    }
{-# INLINE mkTopicConfiguration #-}

instance FromXML TopicConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TopicConfiguration"

instance ToXML TopicConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TopicConfiguration"

data Transition = Transition
    { _tnDate :: Maybe RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    , _tnDays :: Maybe Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , _tnStorageClass :: Maybe TransitionStorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
tnDate :: Lens' Transition (Maybe RFC822)
tnDate = lens _tnDate (\s a -> s { _tnDate = a })
{-# INLINE tnDate #-}

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
tnDays :: Lens' Transition (Maybe Integer)
tnDays = lens _tnDays (\s a -> s { _tnDays = a })
{-# INLINE tnDays #-}

-- | The class of storage used to store the object.
tnStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
tnStorageClass = lens _tnStorageClass (\s a -> s { _tnStorageClass = a })
{-# INLINE tnStorageClass #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Transition' data type to populate a request.
mkTransition :: Transition
mkTransition = Transition
    { _tnDate = Nothing
    , _tnDays = Nothing
    , _tnStorageClass = Nothing
    }
{-# INLINE mkTransition #-}

instance FromXML Transition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Transition"

instance ToXML Transition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Transition"

data VersioningConfiguration = VersioningConfiguration
    { _vcStatus :: Maybe (Switch BucketVersioningStatus)
      -- ^ The versioning state of the bucket.
    , _vcMfaDelete :: Maybe (Switch MFADelete)
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has
      -- been configured with MFA delete. If the bucket has never been so
      -- configured, this element is not returned.
    } deriving (Show, Generic)

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe (Switch BucketVersioningStatus))
vcStatus = lens _vcStatus (\s a -> s { _vcStatus = a })
{-# INLINE vcStatus #-}

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMfaDelete :: Lens' VersioningConfiguration (Maybe (Switch MFADelete))
vcMfaDelete = lens _vcMfaDelete (\s a -> s { _vcMfaDelete = a })
{-# INLINE vcMfaDelete #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VersioningConfiguration' data type to populate a request.
mkVersioningConfiguration :: VersioningConfiguration
mkVersioningConfiguration = VersioningConfiguration
    { _vcStatus = Nothing
    , _vcMfaDelete = Nothing
    }
{-# INLINE mkVersioningConfiguration #-}

instance ToXML VersioningConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "VersioningConfiguration"

data WebsiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument :: Maybe ErrorDocument
    , _wcIndexDocument :: Maybe IndexDocument
    , _wcRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _wcRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\s a -> s { _wcErrorDocument = a })
{-# INLINE wcErrorDocument #-}

wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\s a -> s { _wcIndexDocument = a })
{-# INLINE wcIndexDocument #-}

wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\s a -> s { _wcRedirectAllRequestsTo = a })
{-# INLINE wcRedirectAllRequestsTo #-}

wcRoutingRules :: Lens' WebsiteConfiguration ([RoutingRule])
wcRoutingRules = lens _wcRoutingRules (\s a -> s { _wcRoutingRules = a })
{-# INLINE wcRoutingRules #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WebsiteConfiguration' data type to populate a request.
mkWebsiteConfiguration :: WebsiteConfiguration
mkWebsiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument = Nothing
    , _wcIndexDocument = Nothing
    , _wcRedirectAllRequestsTo = Nothing
    , _wcRoutingRules = mempty
    }
{-# INLINE mkWebsiteConfiguration #-}

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "WebsiteConfiguration"
