{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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


module Network.AWS.S3.V2006_03_01.Types where

import Control.Applicative
import Data.Text (Text)
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Signing.V4
import Network.AWS.Types hiding (Error)

-- | Supported version (@2006-03-01@) of the
-- @Amazon Simple Storage Service@ service.
data S3

instance AWSService S3 where
    type Sg S3 = V4

    service = Service
        { _svcEndpoint = Global
        , _svcPrefix   = "s3"
        , _svcVersion  = "2006-03-01"
        , _svcTarget   = Nothing
        }

data instance Er S3
    = BucketAlreadyExists
    | NoSuchBucket
    | NoSuchKey
    | NoSuchUpload
    | ObjectAlreadyInActiveTierError
    | ObjectNotInActiveTierError
    | S3Error String
    | S3Protocol ClientException
      deriving (Show, Generic)

instance AWSError (Er S3) where
    awsError = const "S3Error"

instance ServiceError (Er S3) where
    serviceError = S3Error
    clientError  = S3Protocol

-- | The versioning state of the bucket.
data BucketVersioningStatus

instance FromXML BucketVersioningStatus

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
data ExpirationStatus

instance FromXML ExpirationStatus

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
data MFADelete

instance FromXML MFADelete

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
data MFADeleteStatus

instance FromXML MFADeleteStatus

-- | The canned ACL to apply to the bucket.
data BucketCannedACL
    = BucketCannedACLAuthenticatedRead -- ^ authenticated-read
    | BucketCannedACLPrivate -- ^ private
    | BucketCannedACLPublicRead -- ^ public-read
    | BucketCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance FromText BucketCannedACL where
    parser = matchText "authenticated-read" BucketCannedACLAuthenticatedRead
         <|> matchText "private" BucketCannedACLPrivate
         <|> matchText "public-read" BucketCannedACLPublicRead
         <|> matchText "public-read-write" BucketCannedACLPublicReadWrite

instance ToText BucketCannedACL where
    toText BucketCannedACLAuthenticatedRead = "authenticated-read"
    toText BucketCannedACLPrivate = "private"
    toText BucketCannedACLPublicRead = "public-read"
    toText BucketCannedACLPublicReadWrite = "public-read-write"

instance ToByteString BucketCannedACL where
    toBS BucketCannedACLAuthenticatedRead = "authenticated-read"
    toBS BucketCannedACLPrivate = "private"
    toBS BucketCannedACLPublicRead = "public-read"
    toBS BucketCannedACLPublicReadWrite = "public-read-write"

instance FromHeader BucketCannedACL

instance FromXML BucketCannedACL

-- | Specifies the region where the bucket will be created.
newtype BucketLocationConstraint = BucketLocationConstraint Region
    deriving (Eq, Show, Generic)

instance FromXML BucketLocationConstraint

-- | Logging permissions assigned to the Grantee for the bucket.
data BucketLogsPermission
    = BucketLogsPermissionFullControl -- ^ FULL_CONTROL
    | BucketLogsPermissionRead -- ^ READ
    | BucketLogsPermissionWrite -- ^ WRITE
      deriving (Eq, Show, Generic)

instance FromText BucketLogsPermission where
    parser = matchText "FULL_CONTROL" BucketLogsPermissionFullControl
         <|> matchText "READ" BucketLogsPermissionRead
         <|> matchText "WRITE" BucketLogsPermissionWrite

instance ToText BucketLogsPermission where
    toText BucketLogsPermissionFullControl = "FULL_CONTROL"
    toText BucketLogsPermissionRead = "READ"
    toText BucketLogsPermissionWrite = "WRITE"

instance ToByteString BucketLogsPermission where
    toBS BucketLogsPermissionFullControl = "FULL_CONTROL"
    toBS BucketLogsPermissionRead = "READ"
    toBS BucketLogsPermissionWrite = "WRITE"

instance FromHeader BucketLogsPermission

instance FromXML BucketLogsPermission

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
data EncodingType
    = EncodingTypeUrl -- ^ url
      deriving (Eq, Show, Generic)

instance FromText EncodingType where
    parser = matchText "url" EncodingTypeUrl

instance ToText EncodingType where
    toText EncodingTypeUrl = "url"

instance ToByteString EncodingType where
    toBS EncodingTypeUrl = "url"

instance FromHeader EncodingType

instance FromXML EncodingType

-- | Bucket event for which to send notifications.
data Event
    = EventS3ReducedRedundancyLostObject -- ^ s3:ReducedRedundancyLostObject
      deriving (Eq, Show, Generic)

instance FromText Event where
    parser = matchText "s3:ReducedRedundancyLostObject" EventS3ReducedRedundancyLostObject

instance ToText Event where
    toText EventS3ReducedRedundancyLostObject = "s3:ReducedRedundancyLostObject"

instance ToByteString Event where
    toBS EventS3ReducedRedundancyLostObject = "s3:ReducedRedundancyLostObject"

instance FromHeader Event

instance FromXML Event

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
data MetadataDirective
    = MetadataDirectiveCopy -- ^ COPY
    | MetadataDirectiveReplace -- ^ REPLACE
      deriving (Eq, Show, Generic)

instance FromText MetadataDirective where
    parser = matchText "COPY" MetadataDirectiveCopy
         <|> matchText "REPLACE" MetadataDirectiveReplace

instance ToText MetadataDirective where
    toText MetadataDirectiveCopy = "COPY"
    toText MetadataDirectiveReplace = "REPLACE"

instance ToByteString MetadataDirective where
    toBS MetadataDirectiveCopy = "COPY"
    toBS MetadataDirectiveReplace = "REPLACE"

instance FromHeader MetadataDirective

instance FromXML MetadataDirective

-- | The canned ACL to apply to the object.
data ObjectCannedACL
    = ObjectCannedACLAuthenticatedRead -- ^ authenticated-read
    | ObjectCannedACLBucketOwnerFullControl -- ^ bucket-owner-full-control
    | ObjectCannedACLBucketOwnerRead -- ^ bucket-owner-read
    | ObjectCannedACLPrivate -- ^ private
    | ObjectCannedACLPublicRead -- ^ public-read
    | ObjectCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance FromText ObjectCannedACL where
    parser = matchText "authenticated-read" ObjectCannedACLAuthenticatedRead
         <|> matchText "bucket-owner-full-control" ObjectCannedACLBucketOwnerFullControl
         <|> matchText "bucket-owner-read" ObjectCannedACLBucketOwnerRead
         <|> matchText "private" ObjectCannedACLPrivate
         <|> matchText "public-read" ObjectCannedACLPublicRead
         <|> matchText "public-read-write" ObjectCannedACLPublicReadWrite

instance ToText ObjectCannedACL where
    toText ObjectCannedACLAuthenticatedRead = "authenticated-read"
    toText ObjectCannedACLBucketOwnerFullControl = "bucket-owner-full-control"
    toText ObjectCannedACLBucketOwnerRead = "bucket-owner-read"
    toText ObjectCannedACLPrivate = "private"
    toText ObjectCannedACLPublicRead = "public-read"
    toText ObjectCannedACLPublicReadWrite = "public-read-write"

instance ToByteString ObjectCannedACL where
    toBS ObjectCannedACLAuthenticatedRead = "authenticated-read"
    toBS ObjectCannedACLBucketOwnerFullControl = "bucket-owner-full-control"
    toBS ObjectCannedACLBucketOwnerRead = "bucket-owner-read"
    toBS ObjectCannedACLPrivate = "private"
    toBS ObjectCannedACLPublicRead = "public-read"
    toBS ObjectCannedACLPublicReadWrite = "public-read-write"

instance FromHeader ObjectCannedACL

instance FromXML ObjectCannedACL

-- | The class of storage used to store the object.
data ObjectStorageClass
    = ObjectStorageClassGlacier -- ^ GLACIER
    | ObjectStorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | ObjectStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance FromText ObjectStorageClass where
    parser = matchText "GLACIER" ObjectStorageClassGlacier
         <|> matchText "REDUCED_REDUNDANCY" ObjectStorageClassReducedRedundancy
         <|> matchText "STANDARD" ObjectStorageClassStandard

instance ToText ObjectStorageClass where
    toText ObjectStorageClassGlacier = "GLACIER"
    toText ObjectStorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toText ObjectStorageClassStandard = "STANDARD"

instance ToByteString ObjectStorageClass where
    toBS ObjectStorageClassGlacier = "GLACIER"
    toBS ObjectStorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toBS ObjectStorageClassStandard = "STANDARD"

instance FromHeader ObjectStorageClass

instance FromXML ObjectStorageClass

-- | The class of storage used to store the object.
data ObjectVersionStorageClass
    = ObjectVersionStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance FromText ObjectVersionStorageClass where
    parser = matchText "STANDARD" ObjectVersionStorageClassStandard

instance ToText ObjectVersionStorageClass where
    toText ObjectVersionStorageClassStandard = "STANDARD"

instance ToByteString ObjectVersionStorageClass where
    toBS ObjectVersionStorageClassStandard = "STANDARD"

instance FromHeader ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass

-- | Specifies who pays for the download and request fees.
data Payer
    = PayerBucketOwner -- ^ BucketOwner
    | PayerRequester -- ^ Requester
      deriving (Eq, Show, Generic)

instance FromText Payer where
    parser = matchText "BucketOwner" PayerBucketOwner
         <|> matchText "Requester" PayerRequester

instance ToText Payer where
    toText PayerBucketOwner = "BucketOwner"
    toText PayerRequester = "Requester"

instance ToByteString Payer where
    toBS PayerBucketOwner = "BucketOwner"
    toBS PayerRequester = "Requester"

instance FromHeader Payer

instance FromXML Payer

-- | Specifies the permission given to the grantee.
data Permission
    = PermissionFullControl -- ^ FULL_CONTROL
    | PermissionRead -- ^ READ
    | PermissionReadAcp -- ^ READ_ACP
    | PermissionWrite -- ^ WRITE
    | PermissionWriteAcp -- ^ WRITE_ACP
      deriving (Eq, Show, Generic)

instance FromText Permission where
    parser = matchText "FULL_CONTROL" PermissionFullControl
         <|> matchText "READ" PermissionRead
         <|> matchText "READ_ACP" PermissionReadAcp
         <|> matchText "WRITE" PermissionWrite
         <|> matchText "WRITE_ACP" PermissionWriteAcp

instance ToText Permission where
    toText PermissionFullControl = "FULL_CONTROL"
    toText PermissionRead = "READ"
    toText PermissionReadAcp = "READ_ACP"
    toText PermissionWrite = "WRITE"
    toText PermissionWriteAcp = "WRITE_ACP"

instance ToByteString Permission where
    toBS PermissionFullControl = "FULL_CONTROL"
    toBS PermissionRead = "READ"
    toBS PermissionReadAcp = "READ_ACP"
    toBS PermissionWrite = "WRITE"
    toBS PermissionWriteAcp = "WRITE_ACP"

instance FromHeader Permission

instance FromXML Permission

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
data Protocol
    = ProtocolHttp -- ^ http
    | ProtocolHttps -- ^ https
      deriving (Eq, Show, Generic)

instance FromText Protocol where
    parser = matchText "http" ProtocolHttp
         <|> matchText "https" ProtocolHttps

instance ToText Protocol where
    toText ProtocolHttp = "http"
    toText ProtocolHttps = "https"

instance ToByteString Protocol where
    toBS ProtocolHttp = "http"
    toBS ProtocolHttps = "https"

instance FromHeader Protocol

instance FromXML Protocol

-- | The Server-side encryption algorithm used when storing this object in S3.
data ServerSideEncryption
    = ServerSideEncryptionAES256 -- ^ AES256
      deriving (Eq, Show, Generic)

instance FromText ServerSideEncryption where
    parser = matchText "AES256" ServerSideEncryptionAES256

instance ToText ServerSideEncryption where
    toText ServerSideEncryptionAES256 = "AES256"

instance ToByteString ServerSideEncryption where
    toBS ServerSideEncryptionAES256 = "AES256"

instance FromHeader ServerSideEncryption

instance FromXML ServerSideEncryption

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
data StorageClass
    = StorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | StorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance FromText StorageClass where
    parser = matchText "REDUCED_REDUNDANCY" StorageClassReducedRedundancy
         <|> matchText "STANDARD" StorageClassStandard

instance ToText StorageClass where
    toText StorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toText StorageClassStandard = "STANDARD"

instance ToByteString StorageClass where
    toBS StorageClassReducedRedundancy = "REDUCED_REDUNDANCY"
    toBS StorageClassStandard = "STANDARD"

instance FromHeader StorageClass

instance FromXML StorageClass

-- | The class of storage used to store the object.
data TransitionStorageClass
    = TransitionStorageClassGlacier -- ^ GLACIER
      deriving (Eq, Show, Generic)

instance FromText TransitionStorageClass where
    parser = matchText "GLACIER" TransitionStorageClassGlacier

instance ToText TransitionStorageClass where
    toText TransitionStorageClassGlacier = "GLACIER"

instance ToByteString TransitionStorageClass where
    toBS TransitionStorageClassGlacier = "GLACIER"

instance FromHeader TransitionStorageClass

instance FromXML TransitionStorageClass

-- | Type of grantee.
data Type
    = TypeAmazonCustomerByEmail -- ^ AmazonCustomerByEmail
    | TypeCanonicalUser -- ^ CanonicalUser
    | TypeGroup -- ^ Group
      deriving (Eq, Show, Generic)

instance FromText Type where
    parser = matchText "AmazonCustomerByEmail" TypeAmazonCustomerByEmail
         <|> matchText "CanonicalUser" TypeCanonicalUser
         <|> matchText "Group" TypeGroup

instance ToText Type where
    toText TypeAmazonCustomerByEmail = "AmazonCustomerByEmail"
    toText TypeCanonicalUser = "CanonicalUser"
    toText TypeGroup = "Group"

instance ToByteString Type where
    toBS TypeAmazonCustomerByEmail = "AmazonCustomerByEmail"
    toBS TypeCanonicalUser = "CanonicalUser"
    toBS TypeGroup = "Group"

instance FromHeader Type

instance FromXML Type

newtype BucketLoggingStatus = BucketLoggingStatus
    { blsLoggingEnabled :: LoggingEnabled
    } deriving (Eq, Show, Generic)

instance FromXML BucketLoggingStatus

newtype CORSConfiguration = CORSConfiguration
    { corscCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic)

instance FromXML CORSConfiguration

newtype CommonPrefix = CommonPrefix
    { cpPrefix :: Text
    } deriving (Eq, Show, Generic)

instance FromXML CommonPrefix

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { cmuParts :: [CompletedPart]
    } deriving (Eq, Show, Generic)

instance FromXML CompletedMultipartUpload

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { cbcLocationConstraint :: BucketLocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Eq, Show, Generic)

instance FromXML CreateBucketConfiguration

newtype ErrorDocument = ErrorDocument
    { edKey :: ObjectKey
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Eq, Show, Generic)

instance FromXML ErrorDocument

newtype IndexDocument = IndexDocument
    { idSuffix :: Text
      -- ^ A suffix that is appended to a request that is for a directory on
      -- the website endpoint (e.g. if the suffix is index.html and you
      -- make a request to samplebucket/images/ the data that is returned
      -- will be for the object with the key name images/index.html) The
      -- suffix must not be empty and must not include a slash character.
    } deriving (Eq, Show, Generic)

instance FromXML IndexDocument

newtype LifecycleConfiguration = LifecycleConfiguration
    { lcRules :: [Rule]
    } deriving (Eq, Show, Generic)

instance FromXML LifecycleConfiguration

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon
-- S3 permanently deletes the noncurrent object versions. You set this
-- lifecycle configuration action on a bucket that has versioning enabled (or
-- suspended) to request that Amazon S3 delete noncurrent object versions at a
-- specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration
    { nveNoncurrentDays :: Integer
      -- ^ Specifies the number of days an object is noncurrent before
      -- Amazon S3 can perform the associated action. For information
      -- about the noncurrent days calculations, see How Amazon S3
      -- Calculates When an Object Became Noncurrent in the Amazon Simple
      -- Storage Service Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromXML NoncurrentVersionExpiration

newtype NotificationConfiguration = NotificationConfiguration
    { ncTopicConfiguration :: TopicConfiguration
    } deriving (Eq, Show, Generic)

instance FromXML NotificationConfiguration

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Eq, Show, Generic)

instance FromXML RequestPaymentConfiguration

newtype RestoreRequest = RestoreRequest
    { rrDays :: Integer
      -- ^ Lifetime of the active copy in days.
    } deriving (Eq, Show, Generic)

instance FromXML RestoreRequest

newtype Tagging = Tagging
    { tTagSet :: [Tag]
    } deriving (Eq, Show, Generic)

instance FromXML Tagging

data AccessControlPolicy = AccessControlPolicy
    { acpGrants :: [Grant]
      -- ^ A list of grants.
    , acpOwner :: Owner
    } deriving (Eq, Show, Generic)

instance FromXML AccessControlPolicy

data Bucket = Bucket
    { bName :: BucketName
      -- ^ The name of the bucket.
    , bCreationDate :: RFC822
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

instance FromXML Bucket

data CORSRule = CORSRule
    { corsrAllowedMethods :: [Text]
      -- ^ Identifies HTTP methods that the domain/origin specified in the
      -- rule is allowed to execute.
    , corsrMaxAgeSeconds :: Integer
      -- ^ The time in seconds that your browser is to cache the preflight
      -- response for the specified resource.
    , corsrAllowedHeaders :: [Text]
      -- ^ Specifies which headers are allowed in a pre-flight OPTIONS
      -- request.
    , corsrAllowedOrigins :: [Text]
      -- ^ One or more origins you want customers to be able to access the
      -- bucket from.
    , corsrExposeHeaders :: [Text]
      -- ^ One or more headers in the response that you want customers to be
      -- able to access from their applications (for example, from a
      -- JavaScript XMLHttpRequest object).
    } deriving (Eq, Show, Generic)

instance FromXML CORSRule

data CompletedPart = CompletedPart
    { cpETag :: ETag
      -- ^ Entity tag returned when the part was uploaded.
    , cpPartNumber :: Integer
      -- ^ Part number that identifies the part.
    } deriving (Eq, Show, Generic)

instance FromXML CompletedPart

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
data Condition = Condition
    { cKeyPrefixEquals :: Text
      -- ^ The object key name prefix when the redirect is applied. For
      -- example, to redirect requests for ExamplePage.html, the key
      -- prefix will be ExamplePage.html. To redirect request for all
      -- pages with the prefix docs/, the key prefix will be /docs, which
      -- identifies all objects in the docs/ folder. Required when the
      -- parent element Condition is specified and sibling
      -- HttpErrorCodeReturnedEquals is not specified. If both conditions
      -- are specified, both must be true for the redirect to be applied.
    , cHttpErrorCodeReturnedEquals :: Text
      -- ^ The HTTP error code when the redirect is applied. In the event of
      -- an error, if the error code equals this value, then the specified
      -- redirect is applied. Required when parent element Condition is
      -- specified and sibling KeyPrefixEquals is not specified. If both
      -- are specified, then both must be true for the redirect to be
      -- applied.
    } deriving (Eq, Show, Generic)

instance FromXML Condition

data CopyObjectResult = CopyObjectResult
    { corETag :: ETag
    , corLastModified :: RFC822
    } deriving (Eq, Show, Generic)

instance FromXML CopyObjectResult

data CopyPartResult = CopyPartResult
    { cprETag :: ETag
      -- ^ Entity tag of the object.
    , cprLastModified :: RFC822
      -- ^ Date and time at which the object was uploaded.
    } deriving (Eq, Show, Generic)

instance FromXML CopyPartResult

data Delete = Delete
    { dQuiet :: Bool
      -- ^ Element to enable quiet mode for the request. When you add this
      -- element, you must set its value to true.
    , dObjects :: [ObjectIdentifier]
    } deriving (Eq, Show, Generic)

instance FromXML Delete

data DeleteMarkerEntry = DeleteMarkerEntry
    { dmeVersionId :: ObjectVersionId
      -- ^ Version ID of an object.
    , dmeIsLatest :: Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , dmeOwner :: Owner
    , dmeKey :: ObjectKey
      -- ^ The object key.
    , dmeLastModified :: RFC822
      -- ^ Date and time the object was last modified.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteMarkerEntry

data DeletedObject = DeletedObject
    { doVersionId :: ObjectVersionId
    , doDeleteMarker :: Bool
    , doDeleteMarkerVersionId :: Text
    , doKey :: ObjectKey
    } deriving (Eq, Show, Generic)

instance FromXML DeletedObject

data Error = Error
    { eVersionId :: ObjectVersionId
    , eKey :: ObjectKey
    , eCode :: Text
    , eMessage :: Text
    } deriving (Eq, Show, Generic)

instance FromXML Error

data Grant = Grant
    { gPermission :: Permission
      -- ^ Specifies the permission given to the grantee.
    , gGrantee :: Grantee
    } deriving (Eq, Show, Generic)

instance FromXML Grant

data Grantee = Grantee
    { gURI :: Text
      -- ^ URI of the grantee group.
    , gEmailAddress :: Text
      -- ^ Email address of the grantee.
    , gDisplayName :: Text
      -- ^ Screen name of the grantee.
    , gID :: Text
      -- ^ The canonical user ID of the grantee.
    , gType :: Type
      -- ^ Type of grantee.
    } deriving (Eq, Show, Generic)

instance FromXML Grantee

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { iDisplayName :: Text
      -- ^ Name of the Principal.
    , iID :: Text
      -- ^ If the principal is an AWS account, it provides the Canonical
      -- User ID. If the principal is an IAM User, it provides a user ARN
      -- value.
    } deriving (Eq, Show, Generic)

instance FromXML Initiator

data LifecycleExpiration = LifecycleExpiration
    { leDays :: Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , leDate :: RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    } deriving (Eq, Show, Generic)

instance FromXML LifecycleExpiration

data LoggingEnabled = LoggingEnabled
    { leTargetBucket :: Text
      -- ^ Specifies the bucket where you want Amazon S3 to store server
      -- access logs. You can have your logs delivered to any bucket that
      -- you own, including the same bucket that is being logged. You can
      -- also configure multiple buckets to deliver their logs to the same
      -- target bucket. In this case you should choose a different
      -- TargetPrefix for each source bucket so that the delivered log
      -- files can be distinguished by key.
    , leTargetGrants :: [TargetGrant]
    , leTargetPrefix :: Text
      -- ^ This element lets you specify a prefix for the keys that the log
      -- files will be stored under.
    } deriving (Eq, Show, Generic)

instance FromXML LoggingEnabled

data MultipartUpload = MultipartUpload
    { muInitiated :: RFC822
      -- ^ Date and time at which the multipart upload was initiated.
    , muInitiator :: Initiator
      -- ^ Identifies who initiated the multipart upload.
    , muOwner :: Owner
    , muKey :: ObjectKey
      -- ^ Key of the object for which the multipart upload was initiated.
    , muStorageClass :: StorageClass
      -- ^ The class of storage used to store the object.
    , muUploadId :: Text
      -- ^ Upload ID that identifies the multipart upload.
    } deriving (Eq, Show, Generic)

instance FromXML MultipartUpload

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
data NoncurrentVersionTransition = NoncurrentVersionTransition
    { nvtStorageClass :: TransitionStorageClass
      -- ^ The class of storage used to store the object.
    , nvtNoncurrentDays :: Integer
      -- ^ Specifies the number of days an object is noncurrent before
      -- Amazon S3 can perform the associated action. For information
      -- about the noncurrent days calculations, see How Amazon S3
      -- Calculates When an Object Became Noncurrent in the Amazon Simple
      -- Storage Service Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromXML NoncurrentVersionTransition

data Object = Object
    { oETag :: ETag
    , oSize :: Integer
    , oOwner :: Owner
    , oKey :: ObjectKey
    , oStorageClass :: ObjectStorageClass
      -- ^ The class of storage used to store the object.
    , oLastModified :: RFC822
    } deriving (Eq, Show, Generic)

instance FromXML Object

data ObjectIdentifier = ObjectIdentifier
    { oiVersionId :: ObjectVersionId
      -- ^ VersionId for the specific version of the object to delete.
    , oiKey :: ObjectKey
      -- ^ Key name of the object to delete.
    } deriving (Eq, Show, Generic)

instance FromXML ObjectIdentifier

data ObjectVersion = ObjectVersion
    { ovETag :: ETag
    , ovVersionId :: ObjectVersionId
      -- ^ Version ID of an object.
    , ovSize :: Integer
      -- ^ Size in bytes of the object.
    , ovIsLatest :: Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , ovOwner :: Owner
    , ovKey :: ObjectKey
      -- ^ The object key.
    , ovStorageClass :: ObjectVersionStorageClass
      -- ^ The class of storage used to store the object.
    , ovLastModified :: RFC822
      -- ^ Date and time the object was last modified.
    } deriving (Eq, Show, Generic)

instance FromXML ObjectVersion

data Owner = Owner
    { oDisplayName :: Text
    , oID :: Text
    } deriving (Eq, Show, Generic)

instance FromXML Owner

data Part = Part
    { pETag :: ETag
      -- ^ Entity tag returned when the part was uploaded.
    , pSize :: Integer
      -- ^ Size of the uploaded part data.
    , pPartNumber :: Integer
      -- ^ Part number identifying the part.
    , pLastModified :: RFC822
      -- ^ Date and time at which the part was uploaded.
    } deriving (Eq, Show, Generic)

instance FromXML Part

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
data Redirect = Redirect
    { rHostName :: Text
      -- ^ The host name to use in the redirect request.
    , rProtocol :: Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    , rHttpRedirectCode :: Text
      -- ^ The HTTP redirect code to use on the response. Not required if
      -- one of the siblings is present.
    , rReplaceKeyWith :: Text
      -- ^ The specific object key to use in the redirect request. For
      -- example, redirect request to error.html. Not required if one of
      -- the sibling is present. Can be present only if
      -- ReplaceKeyPrefixWith is not provided.
    , rReplaceKeyPrefixWith :: Text
      -- ^ The object key prefix to use in the redirect request. For
      -- example, to redirect requests for all pages with prefix docs/
      -- (objects in the docs/ folder) to documents/, you can set a
      -- condition block with KeyPrefixEquals set to docs/ and in the
      -- Redirect set ReplaceKeyPrefixWith to /documents. Not required if
      -- one of the siblings is present. Can be present only if
      -- ReplaceKeyWith is not provided.
    } deriving (Eq, Show, Generic)

instance FromXML Redirect

data RedirectAllRequestsTo = RedirectAllRequestsTo
    { rartHostName :: Text
      -- ^ Name of the host where requests will be redirected.
    , rartProtocol :: Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    } deriving (Eq, Show, Generic)

instance FromXML RedirectAllRequestsTo

data RoutingRule = RoutingRule
    { rrRedirect :: Redirect
      -- ^ Container for redirect information. You can redirect requests to
      -- another host, to another page, or with another protocol. In the
      -- event of an error, you can can specify a different error code to
      -- return.
    , rrCondition :: Condition
      -- ^ A container for describing a condition that must be met for the
      -- specified redirect to apply. For example, 1. If request is for
      -- pages in the /docs folder, redirect to the /documents folder. 2.
      -- If request results in HTTP error 4xx, redirect request to another
      -- host where you might process the error.
    } deriving (Eq, Show, Generic)

instance FromXML RoutingRule

data Rule = Rule
    { rStatus :: Switch ExpirationStatus
      -- ^ If 'Enabled', the rule is currently being applied. If 'Disabled',
      -- the rule is not currently being applied.
    , rNoncurrentVersionExpiration :: NoncurrentVersionExpiration
      -- ^ Specifies when noncurrent object versions expire. Upon
      -- expiration, Amazon S3 permanently deletes the noncurrent object
      -- versions. You set this lifecycle configuration action on a bucket
      -- that has versioning enabled (or suspended) to request that Amazon
      -- S3 delete noncurrent object versions at a specific period in the
      -- object's lifetime.
    , rTransition :: Transition
    , rPrefix :: Text
      -- ^ Prefix identifying one or more objects to which the rule applies.
    , rExpiration :: LifecycleExpiration
    , rNoncurrentVersionTransition :: NoncurrentVersionTransition
      -- ^ Container for the transition rule that describes when noncurrent
      -- objects transition to the GLACIER storage class. If your bucket
      -- is versioning-enabled (or versioning is suspended), you can set
      -- this action to request that Amazon S3 transition noncurrent
      -- object versions to the GLACIER storage class at a specific period
      -- in the object's lifetime.
    , rID :: Text
      -- ^ Unique identifier for the rule. The value cannot be longer than
      -- 255 characters.
    } deriving (Eq, Show, Generic)

instance FromXML Rule

data Tag = Tag
    { tValue :: Text
      -- ^ Value of the tag.
    , tKey :: ObjectKey
      -- ^ Name of the tag.
    } deriving (Eq, Show, Generic)

instance FromXML Tag

data TargetGrant = TargetGrant
    { tgPermission :: BucketLogsPermission
      -- ^ Logging permissions assigned to the Grantee for the bucket.
    , tgGrantee :: Grantee
    } deriving (Eq, Show, Generic)

instance FromXML TargetGrant

data TopicConfiguration = TopicConfiguration
    { tcEvent :: Event
      -- ^ Bucket event for which to send notifications.
    , tcTopic :: Text
      -- ^ Amazon SNS topic to which Amazon S3 will publish a message to
      -- report the specified events for the bucket.
    } deriving (Eq, Show, Generic)

instance FromXML TopicConfiguration

data Transition = Transition
    { tDays :: Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , tDate :: RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    , tStorageClass :: TransitionStorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Eq, Show, Generic)

instance FromXML Transition

data VersioningConfiguration = VersioningConfiguration
    { vcStatus :: Switch BucketVersioningStatus
      -- ^ The versioning state of the bucket.
    , vcMfaDelete :: Switch MFADelete
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has
      -- been configured with MFA delete. If the bucket has never been so
      -- configured, this element is not returned.
    } deriving (Eq, Show, Generic)

instance FromXML VersioningConfiguration

data WebsiteConfiguration = WebsiteConfiguration
    { wcRedirectAllRequestsTo :: RedirectAllRequestsTo
    , wcErrorDocument :: ErrorDocument
    , wcIndexDocument :: IndexDocument
    , wcRoutingRules :: [RoutingRule]
    } deriving (Eq, Show, Generic)

instance FromXML WebsiteConfiguration
