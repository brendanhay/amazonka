{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , BucketLoggingStatus (..)
    , blsLoggingEnabled

    -- * CORSConfiguration
    , CORSConfiguration (..)
    , corscCORSRules

    -- * CommonPrefix
    , CommonPrefix (..)
    , ccxPrefix

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload (..)
    , cmuParts

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration (..)
    , cbcLocationConstraint

    -- * ErrorDocument
    , ErrorDocument (..)
    , edKey

    -- * IndexDocument
    , IndexDocument (..)
    , ihSuffix

    -- * LifecycleConfiguration
    , LifecycleConfiguration (..)
    , lcRules

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration (..)
    , nveNoncurrentDays

    -- * NotificationConfiguration
    , NotificationConfiguration (..)
    , ncTopicConfiguration

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration (..)
    , rpcPayer

    -- * RestoreRequest
    , RestoreRequest (..)
    , rzDays

    -- * Tagging
    , Tagging (..)
    , twTagSet

    -- * AccessControlPolicy
    , AccessControlPolicy (..)
    , acpGrants
    , acpOwner

    -- * Bucket
    , Bucket (..)
    , bbxName
    , bbxCreationDate

    -- * CORSRule
    , CORSRule (..)
    , corssAllowedHeaders
    , corssAllowedMethods
    , corssAllowedOrigins
    , corssExposeHeaders
    , corssMaxAgeSeconds

    -- * CompletedPart
    , CompletedPart (..)
    , cpETag
    , cpPartNumber

    -- * Condition
    , Condition (..)
    , cnHttpErrorCodeReturnedEquals
    , cnKeyPrefixEquals

    -- * CopyObjectResult
    , CopyObjectResult (..)
    , cosETag
    , cosLastModified

    -- * CopyPartResult
    , CopyPartResult (..)
    , cprETag
    , cprLastModified

    -- * Delete
    , Delete (..)
    , kObjects
    , kQuiet

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry (..)
    , dmeOwner
    , dmeKey
    , dmeVersionId
    , dmeIsLatest
    , dmeLastModified

    -- * DeletedObject
    , DeletedObject (..)
    , dpKey
    , dpVersionId
    , dpDeleteMarker
    , dpDeleteMarkerVersionId

    -- * Error
    , Error (..)
    , oKey
    , oVersionId
    , oCode
    , oMessage

    -- * Grant
    , Grant (..)
    , guGrantee
    , guPermission

    -- * Grantee
    , Grantee (..)
    , geDisplayName
    , geEmailAddress
    , geID
    , geType
    , geURI

    -- * Initiator
    , Initiator (..)
    , irID
    , irDisplayName

    -- * LifecycleExpiration
    , LifecycleExpiration (..)
    , leDate
    , leDays

    -- * LoggingEnabled
    , LoggingEnabled (..)
    , lfTargetBucket
    , lfTargetGrants
    , lfTargetPrefix

    -- * MultipartUpload
    , MultipartUpload (..)
    , mwUploadId
    , mwKey
    , mwInitiated
    , mwStorageClass
    , mwOwner
    , mwInitiator

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition (..)
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * Object
    , Object (..)
    , oowKey
    , oowLastModified
    , oowETag
    , oowSize
    , oowStorageClass
    , oowOwner

    -- * ObjectIdentifier
    , ObjectIdentifier (..)
    , oiKey
    , oiVersionId

    -- * ObjectVersion
    , ObjectVersion (..)
    , oonETag
    , oonSize
    , oonStorageClass
    , oonKey
    , oonVersionId
    , oonIsLatest
    , oonLastModified
    , oonOwner

    -- * Owner
    , Owner (..)
    , sDisplayName
    , sID

    -- * Part
    , Part (..)
    , ptPartNumber
    , ptLastModified
    , ptETag
    , ptSize

    -- * Redirect
    , Redirect (..)
    , ruHostName
    , ruHttpRedirectCode
    , ruProtocol
    , ruReplaceKeyPrefixWith
    , ruReplaceKeyWith

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo (..)
    , rartHostName
    , rartProtocol

    -- * RoutingRule
    , RoutingRule (..)
    , rtCondition
    , rtRedirect

    -- * Rule
    , Rule (..)
    , reExpiration
    , reID
    , rePrefix
    , reStatus
    , reTransition
    , reNoncurrentVersionTransition
    , reNoncurrentVersionExpiration

    -- * Tag
    , Tag (..)
    , tiKey
    , tiValue

    -- * TargetGrant
    , TargetGrant (..)
    , thGrantee
    , thPermission

    -- * TopicConfiguration
    , TopicConfiguration (..)
    , tcEvent
    , tcTopic

    -- * Transition
    , Transition (..)
    , tnDate
    , tnDays
    , tnStorageClass

    -- * VersioningConfiguration
    , VersioningConfiguration (..)
    , vcStatus
    , vcMfaDelete

    -- * WebsiteConfiguration
    , WebsiteConfiguration (..)
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
blsLoggingEnabled f x =
    f (_blsLoggingEnabled x)
        <&> \y -> x { _blsLoggingEnabled = y }
{-# INLINE blsLoggingEnabled #-}

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLoggingStatus"

newtype CORSConfiguration = CORSConfiguration
    { _corscCORSRules :: [CORSRule]
    } deriving (Show, Generic)

corscCORSRules :: Lens' CORSConfiguration ([CORSRule])
corscCORSRules f x =
    f (_corscCORSRules x)
        <&> \y -> x { _corscCORSRules = y }
{-# INLINE corscCORSRules #-}

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSConfiguration"

newtype CommonPrefix = CommonPrefix
    { _ccxPrefix :: Maybe Text
    } deriving (Show, Generic)

ccxPrefix :: Lens' CommonPrefix (Maybe Text)
ccxPrefix f x =
    f (_ccxPrefix x)
        <&> \y -> x { _ccxPrefix = y }
{-# INLINE ccxPrefix #-}

instance FromXML CommonPrefix where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CommonPrefix"

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { _cmuParts :: [CompletedPart]
    } deriving (Show, Generic)

cmuParts :: Lens' CompletedMultipartUpload ([CompletedPart])
cmuParts f x =
    f (_cmuParts x)
        <&> \y -> x { _cmuParts = y }
{-# INLINE cmuParts #-}

instance ToXML CompletedMultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompleteMultipartUpload"

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint :: Maybe BucketLocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Show, Generic)

-- | Specifies the region where the bucket will be created.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe BucketLocationConstraint)
cbcLocationConstraint f x =
    f (_cbcLocationConstraint x)
        <&> \y -> x { _cbcLocationConstraint = y }
{-# INLINE cbcLocationConstraint #-}

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateBucketConfiguration"

newtype ErrorDocument = ErrorDocument
    { _edKey :: ObjectKey
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Show, Generic)

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument (ObjectKey)
edKey f x =
    f (_edKey x)
        <&> \y -> x { _edKey = y }
{-# INLINE edKey #-}

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
ihSuffix f x =
    f (_ihSuffix x)
        <&> \y -> x { _ihSuffix = y }
{-# INLINE ihSuffix #-}

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
lcRules f x =
    f (_lcRules x)
        <&> \y -> x { _lcRules = y }
{-# INLINE lcRules #-}

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
nveNoncurrentDays f x =
    f (_nveNoncurrentDays x)
        <&> \y -> x { _nveNoncurrentDays = y }
{-# INLINE nveNoncurrentDays #-}

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
ncTopicConfiguration f x =
    f (_ncTopicConfiguration x)
        <&> \y -> x { _ncTopicConfiguration = y }
{-# INLINE ncTopicConfiguration #-}

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NotificationConfiguration"

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { _rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Show, Generic)

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration (Payer)
rpcPayer f x =
    f (_rpcPayer x)
        <&> \y -> x { _rpcPayer = y }
{-# INLINE rpcPayer #-}

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RequestPaymentConfiguration"

newtype RestoreRequest = RestoreRequest
    { _rzDays :: Integer
      -- ^ Lifetime of the active copy in days.
    } deriving (Show, Generic)

-- | Lifetime of the active copy in days.
rzDays :: Lens' RestoreRequest (Integer)
rzDays f x =
    f (_rzDays x)
        <&> \y -> x { _rzDays = y }
{-# INLINE rzDays #-}

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RestoreRequest"

newtype Tagging = Tagging
    { _twTagSet :: [Tag]
    } deriving (Show, Generic)

twTagSet :: Lens' Tagging ([Tag])
twTagSet f x =
    f (_twTagSet x)
        <&> \y -> x { _twTagSet = y }
{-# INLINE twTagSet #-}

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
acpGrants f x =
    f (_acpGrants x)
        <&> \y -> x { _acpGrants = y }
{-# INLINE acpGrants #-}

acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner f x =
    f (_acpOwner x)
        <&> \y -> x { _acpOwner = y }
{-# INLINE acpOwner #-}

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
bbxName f x =
    f (_bbxName x)
        <&> \y -> x { _bbxName = y }
{-# INLINE bbxName #-}

-- | Date the bucket was created.
bbxCreationDate :: Lens' Bucket (Maybe RFC822)
bbxCreationDate f x =
    f (_bbxCreationDate x)
        <&> \y -> x { _bbxCreationDate = y }
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
corssAllowedHeaders f x =
    f (_corssAllowedHeaders x)
        <&> \y -> x { _corssAllowedHeaders = y }
{-# INLINE corssAllowedHeaders #-}

-- | Identifies HTTP methods that the domain/origin specified in the rule is
-- allowed to execute.
corssAllowedMethods :: Lens' CORSRule ([Text])
corssAllowedMethods f x =
    f (_corssAllowedMethods x)
        <&> \y -> x { _corssAllowedMethods = y }
{-# INLINE corssAllowedMethods #-}

-- | One or more origins you want customers to be able to access the bucket
-- from.
corssAllowedOrigins :: Lens' CORSRule ([Text])
corssAllowedOrigins f x =
    f (_corssAllowedOrigins x)
        <&> \y -> x { _corssAllowedOrigins = y }
{-# INLINE corssAllowedOrigins #-}

-- | One or more headers in the response that you want customers to be able to
-- access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
corssExposeHeaders :: Lens' CORSRule ([Text])
corssExposeHeaders f x =
    f (_corssExposeHeaders x)
        <&> \y -> x { _corssExposeHeaders = y }
{-# INLINE corssExposeHeaders #-}

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
corssMaxAgeSeconds :: Lens' CORSRule (Maybe Integer)
corssMaxAgeSeconds f x =
    f (_corssMaxAgeSeconds x)
        <&> \y -> x { _corssMaxAgeSeconds = y }
{-# INLINE corssMaxAgeSeconds #-}

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
cpETag f x =
    f (_cpETag x)
        <&> \y -> x { _cpETag = y }
{-# INLINE cpETag #-}

-- | Part number that identifies the part.
cpPartNumber :: Lens' CompletedPart (Maybe Integer)
cpPartNumber f x =
    f (_cpPartNumber x)
        <&> \y -> x { _cpPartNumber = y }
{-# INLINE cpPartNumber #-}

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
cnHttpErrorCodeReturnedEquals f x =
    f (_cnHttpErrorCodeReturnedEquals x)
        <&> \y -> x { _cnHttpErrorCodeReturnedEquals = y }
{-# INLINE cnHttpErrorCodeReturnedEquals #-}

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix docs/,
-- the key prefix will be /docs, which identifies all objects in the docs/
-- folder. Required when the parent element Condition is specified and sibling
-- HttpErrorCodeReturnedEquals is not specified. If both conditions are
-- specified, both must be true for the redirect to be applied.
cnKeyPrefixEquals :: Lens' Condition (Maybe Text)
cnKeyPrefixEquals f x =
    f (_cnKeyPrefixEquals x)
        <&> \y -> x { _cnKeyPrefixEquals = y }
{-# INLINE cnKeyPrefixEquals #-}

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
cosETag f x =
    f (_cosETag x)
        <&> \y -> x { _cosETag = y }
{-# INLINE cosETag #-}

cosLastModified :: Lens' CopyObjectResult (Maybe RFC822)
cosLastModified f x =
    f (_cosLastModified x)
        <&> \y -> x { _cosLastModified = y }
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
cprETag f x =
    f (_cprETag x)
        <&> \y -> x { _cprETag = y }
{-# INLINE cprETag #-}

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe RFC822)
cprLastModified f x =
    f (_cprLastModified x)
        <&> \y -> x { _cprLastModified = y }
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
kObjects f x =
    f (_kObjects x)
        <&> \y -> x { _kObjects = y }
{-# INLINE kObjects #-}

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
kQuiet :: Lens' Delete (Maybe Bool)
kQuiet f x =
    f (_kQuiet x)
        <&> \y -> x { _kQuiet = y }
{-# INLINE kQuiet #-}

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
dmeOwner f x =
    f (_dmeOwner x)
        <&> \y -> x { _dmeOwner = y }
{-# INLINE dmeOwner #-}

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey f x =
    f (_dmeKey x)
        <&> \y -> x { _dmeKey = y }
{-# INLINE dmeKey #-}

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId f x =
    f (_dmeVersionId x)
        <&> \y -> x { _dmeVersionId = y }
{-# INLINE dmeVersionId #-}

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest f x =
    f (_dmeIsLatest x)
        <&> \y -> x { _dmeIsLatest = y }
{-# INLINE dmeIsLatest #-}

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe RFC822)
dmeLastModified f x =
    f (_dmeLastModified x)
        <&> \y -> x { _dmeLastModified = y }
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
dpKey f x =
    f (_dpKey x)
        <&> \y -> x { _dpKey = y }
{-# INLINE dpKey #-}

dpVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dpVersionId f x =
    f (_dpVersionId x)
        <&> \y -> x { _dpVersionId = y }
{-# INLINE dpVersionId #-}

dpDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dpDeleteMarker f x =
    f (_dpDeleteMarker x)
        <&> \y -> x { _dpDeleteMarker = y }
{-# INLINE dpDeleteMarker #-}

dpDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dpDeleteMarkerVersionId f x =
    f (_dpDeleteMarkerVersionId x)
        <&> \y -> x { _dpDeleteMarkerVersionId = y }
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
oKey f x =
    f (_oKey x)
        <&> \y -> x { _oKey = y }
{-# INLINE oKey #-}

oVersionId :: Lens' Error (Maybe ObjectVersionId)
oVersionId f x =
    f (_oVersionId x)
        <&> \y -> x { _oVersionId = y }
{-# INLINE oVersionId #-}

oCode :: Lens' Error (Maybe Text)
oCode f x =
    f (_oCode x)
        <&> \y -> x { _oCode = y }
{-# INLINE oCode #-}

oMessage :: Lens' Error (Maybe Text)
oMessage f x =
    f (_oMessage x)
        <&> \y -> x { _oMessage = y }
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
guGrantee f x =
    f (_guGrantee x)
        <&> \y -> x { _guGrantee = y }
{-# INLINE guGrantee #-}

-- | Specifies the permission given to the grantee.
guPermission :: Lens' Grant (Maybe Permission)
guPermission f x =
    f (_guPermission x)
        <&> \y -> x { _guPermission = y }
{-# INLINE guPermission #-}

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
geDisplayName f x =
    f (_geDisplayName x)
        <&> \y -> x { _geDisplayName = y }
{-# INLINE geDisplayName #-}

-- | Email address of the grantee.
geEmailAddress :: Lens' Grantee (Maybe Text)
geEmailAddress f x =
    f (_geEmailAddress x)
        <&> \y -> x { _geEmailAddress = y }
{-# INLINE geEmailAddress #-}

-- | The canonical user ID of the grantee.
geID :: Lens' Grantee (Maybe Text)
geID f x =
    f (_geID x)
        <&> \y -> x { _geID = y }
{-# INLINE geID #-}

-- | Type of grantee.
geType :: Lens' Grantee (Type)
geType f x =
    f (_geType x)
        <&> \y -> x { _geType = y }
{-# INLINE geType #-}

-- | URI of the grantee group.
geURI :: Lens' Grantee (Maybe Text)
geURI f x =
    f (_geURI x)
        <&> \y -> x { _geURI = y }
{-# INLINE geURI #-}

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
irID f x =
    f (_irID x)
        <&> \y -> x { _irID = y }
{-# INLINE irID #-}

-- | Name of the Principal.
irDisplayName :: Lens' Initiator (Maybe Text)
irDisplayName f x =
    f (_irDisplayName x)
        <&> \y -> x { _irDisplayName = y }
{-# INLINE irDisplayName #-}

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
leDate f x =
    f (_leDate x)
        <&> \y -> x { _leDate = y }
{-# INLINE leDate #-}

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Integer)
leDays f x =
    f (_leDays x)
        <&> \y -> x { _leDays = y }
{-# INLINE leDays #-}

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
lfTargetBucket f x =
    f (_lfTargetBucket x)
        <&> \y -> x { _lfTargetBucket = y }
{-# INLINE lfTargetBucket #-}

lfTargetGrants :: Lens' LoggingEnabled ([TargetGrant])
lfTargetGrants f x =
    f (_lfTargetGrants x)
        <&> \y -> x { _lfTargetGrants = y }
{-# INLINE lfTargetGrants #-}

-- | This element lets you specify a prefix for the keys that the log files will
-- be stored under.
lfTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
lfTargetPrefix f x =
    f (_lfTargetPrefix x)
        <&> \y -> x { _lfTargetPrefix = y }
{-# INLINE lfTargetPrefix #-}

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
mwUploadId f x =
    f (_mwUploadId x)
        <&> \y -> x { _mwUploadId = y }
{-# INLINE mwUploadId #-}

-- | Key of the object for which the multipart upload was initiated.
mwKey :: Lens' MultipartUpload (Maybe ObjectKey)
mwKey f x =
    f (_mwKey x)
        <&> \y -> x { _mwKey = y }
{-# INLINE mwKey #-}

-- | Date and time at which the multipart upload was initiated.
mwInitiated :: Lens' MultipartUpload (Maybe RFC822)
mwInitiated f x =
    f (_mwInitiated x)
        <&> \y -> x { _mwInitiated = y }
{-# INLINE mwInitiated #-}

-- | The class of storage used to store the object.
mwStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
mwStorageClass f x =
    f (_mwStorageClass x)
        <&> \y -> x { _mwStorageClass = y }
{-# INLINE mwStorageClass #-}

mwOwner :: Lens' MultipartUpload (Maybe Owner)
mwOwner f x =
    f (_mwOwner x)
        <&> \y -> x { _mwOwner = y }
{-# INLINE mwOwner #-}

-- | Identifies who initiated the multipart upload.
mwInitiator :: Lens' MultipartUpload (Maybe Initiator)
mwInitiator f x =
    f (_mwInitiator x)
        <&> \y -> x { _mwInitiator = y }
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
nvtNoncurrentDays f x =
    f (_nvtNoncurrentDays x)
        <&> \y -> x { _nvtNoncurrentDays = y }
{-# INLINE nvtNoncurrentDays #-}

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition (Maybe TransitionStorageClass)
nvtStorageClass f x =
    f (_nvtStorageClass x)
        <&> \y -> x { _nvtStorageClass = y }
{-# INLINE nvtStorageClass #-}

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
oowKey f x =
    f (_oowKey x)
        <&> \y -> x { _oowKey = y }
{-# INLINE oowKey #-}

oowLastModified :: Lens' Object (RFC822)
oowLastModified f x =
    f (_oowLastModified x)
        <&> \y -> x { _oowLastModified = y }
{-# INLINE oowLastModified #-}

oowETag :: Lens' Object (ETag)
oowETag f x =
    f (_oowETag x)
        <&> \y -> x { _oowETag = y }
{-# INLINE oowETag #-}

oowSize :: Lens' Object (Integer)
oowSize f x =
    f (_oowSize x)
        <&> \y -> x { _oowSize = y }
{-# INLINE oowSize #-}

-- | The class of storage used to store the object.
oowStorageClass :: Lens' Object (ObjectStorageClass)
oowStorageClass f x =
    f (_oowStorageClass x)
        <&> \y -> x { _oowStorageClass = y }
{-# INLINE oowStorageClass #-}

oowOwner :: Lens' Object (Owner)
oowOwner f x =
    f (_oowOwner x)
        <&> \y -> x { _oowOwner = y }
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
oiKey f x =
    f (_oiKey x)
        <&> \y -> x { _oiKey = y }
{-# INLINE oiKey #-}

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId f x =
    f (_oiVersionId x)
        <&> \y -> x { _oiVersionId = y }
{-# INLINE oiVersionId #-}

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
oonETag f x =
    f (_oonETag x)
        <&> \y -> x { _oonETag = y }
{-# INLINE oonETag #-}

-- | Size in bytes of the object.
oonSize :: Lens' ObjectVersion (Maybe Integer)
oonSize f x =
    f (_oonSize x)
        <&> \y -> x { _oonSize = y }
{-# INLINE oonSize #-}

-- | The class of storage used to store the object.
oonStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
oonStorageClass f x =
    f (_oonStorageClass x)
        <&> \y -> x { _oonStorageClass = y }
{-# INLINE oonStorageClass #-}

-- | The object key.
oonKey :: Lens' ObjectVersion (Maybe ObjectKey)
oonKey f x =
    f (_oonKey x)
        <&> \y -> x { _oonKey = y }
{-# INLINE oonKey #-}

-- | Version ID of an object.
oonVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
oonVersionId f x =
    f (_oonVersionId x)
        <&> \y -> x { _oonVersionId = y }
{-# INLINE oonVersionId #-}

-- | Specifies whether the object is (true) or is not (false) the latest version
-- of an object.
oonIsLatest :: Lens' ObjectVersion (Maybe Bool)
oonIsLatest f x =
    f (_oonIsLatest x)
        <&> \y -> x { _oonIsLatest = y }
{-# INLINE oonIsLatest #-}

-- | Date and time the object was last modified.
oonLastModified :: Lens' ObjectVersion (Maybe RFC822)
oonLastModified f x =
    f (_oonLastModified x)
        <&> \y -> x { _oonLastModified = y }
{-# INLINE oonLastModified #-}

oonOwner :: Lens' ObjectVersion (Maybe Owner)
oonOwner f x =
    f (_oonOwner x)
        <&> \y -> x { _oonOwner = y }
{-# INLINE oonOwner #-}

instance FromXML ObjectVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersion"

data Owner = Owner
    { _sDisplayName :: Maybe Text
    , _sID :: Maybe Text
    } deriving (Show, Generic)

sDisplayName :: Lens' Owner (Maybe Text)
sDisplayName f x =
    f (_sDisplayName x)
        <&> \y -> x { _sDisplayName = y }
{-# INLINE sDisplayName #-}

sID :: Lens' Owner (Maybe Text)
sID f x =
    f (_sID x)
        <&> \y -> x { _sID = y }
{-# INLINE sID #-}

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
ptPartNumber f x =
    f (_ptPartNumber x)
        <&> \y -> x { _ptPartNumber = y }
{-# INLINE ptPartNumber #-}

-- | Date and time at which the part was uploaded.
ptLastModified :: Lens' Part (Maybe RFC822)
ptLastModified f x =
    f (_ptLastModified x)
        <&> \y -> x { _ptLastModified = y }
{-# INLINE ptLastModified #-}

-- | Entity tag returned when the part was uploaded.
ptETag :: Lens' Part (Maybe ETag)
ptETag f x =
    f (_ptETag x)
        <&> \y -> x { _ptETag = y }
{-# INLINE ptETag #-}

-- | Size of the uploaded part data.
ptSize :: Lens' Part (Maybe Integer)
ptSize f x =
    f (_ptSize x)
        <&> \y -> x { _ptSize = y }
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
ruHostName f x =
    f (_ruHostName x)
        <&> \y -> x { _ruHostName = y }
{-# INLINE ruHostName #-}

-- | The HTTP redirect code to use on the response. Not required if one of the
-- siblings is present.
ruHttpRedirectCode :: Lens' Redirect (Maybe Text)
ruHttpRedirectCode f x =
    f (_ruHttpRedirectCode x)
        <&> \y -> x { _ruHttpRedirectCode = y }
{-# INLINE ruHttpRedirectCode #-}

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
ruProtocol :: Lens' Redirect (Maybe Protocol)
ruProtocol f x =
    f (_ruProtocol x)
        <&> \y -> x { _ruProtocol = y }
{-# INLINE ruProtocol #-}

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs/ (objects in the docs/
-- folder) to documents/, you can set a condition block with KeyPrefixEquals
-- set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents.
-- Not required if one of the siblings is present. Can be present only if
-- ReplaceKeyWith is not provided.
ruReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
ruReplaceKeyPrefixWith f x =
    f (_ruReplaceKeyPrefixWith x)
        <&> \y -> x { _ruReplaceKeyPrefixWith = y }
{-# INLINE ruReplaceKeyPrefixWith #-}

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
ruReplaceKeyWith :: Lens' Redirect (Maybe Text)
ruReplaceKeyWith f x =
    f (_ruReplaceKeyWith x)
        <&> \y -> x { _ruReplaceKeyWith = y }
{-# INLINE ruReplaceKeyWith #-}

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
rartHostName f x =
    f (_rartHostName x)
        <&> \y -> x { _rartHostName = y }
{-# INLINE rartHostName #-}

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol f x =
    f (_rartProtocol x)
        <&> \y -> x { _rartProtocol = y }
{-# INLINE rartProtocol #-}

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
rtCondition f x =
    f (_rtCondition x)
        <&> \y -> x { _rtCondition = y }
{-# INLINE rtCondition #-}

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
rtRedirect :: Lens' RoutingRule (Redirect)
rtRedirect f x =
    f (_rtRedirect x)
        <&> \y -> x { _rtRedirect = y }
{-# INLINE rtRedirect #-}

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
reExpiration f x =
    f (_reExpiration x)
        <&> \y -> x { _reExpiration = y }
{-# INLINE reExpiration #-}

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
reID :: Lens' Rule (Maybe Text)
reID f x =
    f (_reID x)
        <&> \y -> x { _reID = y }
{-# INLINE reID #-}

-- | Prefix identifying one or more objects to which the rule applies.
rePrefix :: Lens' Rule (Text)
rePrefix f x =
    f (_rePrefix x)
        <&> \y -> x { _rePrefix = y }
{-# INLINE rePrefix #-}

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
reStatus :: Lens' Rule (Switch ExpirationStatus)
reStatus f x =
    f (_reStatus x)
        <&> \y -> x { _reStatus = y }
{-# INLINE reStatus #-}

reTransition :: Lens' Rule (Maybe Transition)
reTransition f x =
    f (_reTransition x)
        <&> \y -> x { _reTransition = y }
{-# INLINE reTransition #-}

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
reNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
reNoncurrentVersionTransition f x =
    f (_reNoncurrentVersionTransition x)
        <&> \y -> x { _reNoncurrentVersionTransition = y }
{-# INLINE reNoncurrentVersionTransition #-}

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon
-- S3 permanently deletes the noncurrent object versions. You set this
-- lifecycle configuration action on a bucket that has versioning enabled (or
-- suspended) to request that Amazon S3 delete noncurrent object versions at a
-- specific period in the object's lifetime.
reNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
reNoncurrentVersionExpiration f x =
    f (_reNoncurrentVersionExpiration x)
        <&> \y -> x { _reNoncurrentVersionExpiration = y }
{-# INLINE reNoncurrentVersionExpiration #-}

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
tiKey f x =
    f (_tiKey x)
        <&> \y -> x { _tiKey = y }
{-# INLINE tiKey #-}

-- | Value of the tag.
tiValue :: Lens' Tag (Text)
tiValue f x =
    f (_tiValue x)
        <&> \y -> x { _tiValue = y }
{-# INLINE tiValue #-}

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
thGrantee f x =
    f (_thGrantee x)
        <&> \y -> x { _thGrantee = y }
{-# INLINE thGrantee #-}

-- | Logging permissions assigned to the Grantee for the bucket.
thPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
thPermission f x =
    f (_thPermission x)
        <&> \y -> x { _thPermission = y }
{-# INLINE thPermission #-}

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
tcEvent f x =
    f (_tcEvent x)
        <&> \y -> x { _tcEvent = y }
{-# INLINE tcEvent #-}

-- | Amazon SNS topic to which Amazon S3 will publish a message to report the
-- specified events for the bucket.
tcTopic :: Lens' TopicConfiguration (Maybe Text)
tcTopic f x =
    f (_tcTopic x)
        <&> \y -> x { _tcTopic = y }
{-# INLINE tcTopic #-}

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
tnDate f x =
    f (_tnDate x)
        <&> \y -> x { _tnDate = y }
{-# INLINE tnDate #-}

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
tnDays :: Lens' Transition (Maybe Integer)
tnDays f x =
    f (_tnDays x)
        <&> \y -> x { _tnDays = y }
{-# INLINE tnDays #-}

-- | The class of storage used to store the object.
tnStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
tnStorageClass f x =
    f (_tnStorageClass x)
        <&> \y -> x { _tnStorageClass = y }
{-# INLINE tnStorageClass #-}

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
vcStatus f x =
    f (_vcStatus x)
        <&> \y -> x { _vcStatus = y }
{-# INLINE vcStatus #-}

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMfaDelete :: Lens' VersioningConfiguration (Maybe (Switch MFADelete))
vcMfaDelete f x =
    f (_vcMfaDelete x)
        <&> \y -> x { _vcMfaDelete = y }
{-# INLINE vcMfaDelete #-}

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
wcErrorDocument f x =
    f (_wcErrorDocument x)
        <&> \y -> x { _wcErrorDocument = y }
{-# INLINE wcErrorDocument #-}

wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument f x =
    f (_wcIndexDocument x)
        <&> \y -> x { _wcIndexDocument = y }
{-# INLINE wcIndexDocument #-}

wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo f x =
    f (_wcRedirectAllRequestsTo x)
        <&> \y -> x { _wcRedirectAllRequestsTo = y }
{-# INLINE wcRedirectAllRequestsTo #-}

wcRoutingRules :: Lens' WebsiteConfiguration ([RoutingRule])
wcRoutingRules f x =
    f (_wcRoutingRules x)
        <&> \y -> x { _wcRoutingRules = y }
{-# INLINE wcRoutingRules #-}

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "WebsiteConfiguration"
