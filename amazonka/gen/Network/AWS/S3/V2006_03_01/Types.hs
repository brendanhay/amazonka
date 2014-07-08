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
    | S3Protocol ClientException
      deriving (Show, Generic)

instance AWSError (Er S3) where
    awsError = const "S3Error"

instance ClientError (Er S3) where
    clientError = S3Protocol

-- | The versioning state of the bucket.
data BucketVersioningStatus

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule
-- is not currently being applied.
data ExpirationStatus

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
data MFADelete

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
data MFADeleteStatus

-- | The canned ACL to apply to the bucket.
data BucketCannedACL
    = BucketCannedACLAuthenticatedRead -- ^ authenticated-read
    | BucketCannedACLPrivate -- ^ private
    | BucketCannedACLPublicRead -- ^ public-read
    | BucketCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance ToByteString BucketCannedACL where
    toBS x = case x of
        BucketCannedACLAuthenticatedRead -> "authenticated-read"
        BucketCannedACLPrivate -> "private"
        BucketCannedACLPublicRead -> "public-read"
        BucketCannedACLPublicReadWrite -> "public-read-write"

-- | Specifies the region where the bucket will be created.
newtype BucketLocationConstraint = BucketLocationConstraint Region
    deriving (Eq, Show, Generic)

-- | Logging permissions assigned to the Grantee for the bucket.
data BucketLogsPermission
    = BucketLogsPermissionFullControl -- ^ FULL_CONTROL
    | BucketLogsPermissionRead -- ^ READ
    | BucketLogsPermissionWrite -- ^ WRITE
      deriving (Eq, Show, Generic)

instance ToByteString BucketLogsPermission where
    toBS x = case x of
        BucketLogsPermissionFullControl -> "FULL_CONTROL"
        BucketLogsPermissionRead -> "READ"
        BucketLogsPermissionWrite -> "WRITE"

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
data EncodingType
    = EncodingTypeUrl -- ^ url
      deriving (Eq, Show, Generic)

instance ToByteString EncodingType where
    toBS x = case x of
        EncodingTypeUrl -> "url"

-- | Bucket event for which to send notifications.
data Event
    = EventS3ReducedRedundancyLostObject -- ^ s3:ReducedRedundancyLostObject
      deriving (Eq, Show, Generic)

instance ToByteString Event where
    toBS x = case x of
        EventS3ReducedRedundancyLostObject -> "s3:ReducedRedundancyLostObject"

-- | Specifies whether the metadata is copied from the source object or replaced
-- with metadata provided in the request.
data MetadataDirective
    = MetadataDirectiveCopy -- ^ COPY
    | MetadataDirectiveReplace -- ^ REPLACE
      deriving (Eq, Show, Generic)

instance ToByteString MetadataDirective where
    toBS x = case x of
        MetadataDirectiveCopy -> "COPY"
        MetadataDirectiveReplace -> "REPLACE"

-- | The canned ACL to apply to the object.
data ObjectCannedACL
    = ObjectCannedACLAuthenticatedRead -- ^ authenticated-read
    | ObjectCannedACLBucketOwnerFullControl -- ^ bucket-owner-full-control
    | ObjectCannedACLBucketOwnerRead -- ^ bucket-owner-read
    | ObjectCannedACLPrivate -- ^ private
    | ObjectCannedACLPublicRead -- ^ public-read
    | ObjectCannedACLPublicReadWrite -- ^ public-read-write
      deriving (Eq, Show, Generic)

instance ToByteString ObjectCannedACL where
    toBS x = case x of
        ObjectCannedACLAuthenticatedRead -> "authenticated-read"
        ObjectCannedACLBucketOwnerFullControl -> "bucket-owner-full-control"
        ObjectCannedACLBucketOwnerRead -> "bucket-owner-read"
        ObjectCannedACLPrivate -> "private"
        ObjectCannedACLPublicRead -> "public-read"
        ObjectCannedACLPublicReadWrite -> "public-read-write"

-- | The class of storage used to store the object.
data ObjectStorageClass
    = ObjectStorageClassGlacier -- ^ GLACIER
    | ObjectStorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | ObjectStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance ToByteString ObjectStorageClass where
    toBS x = case x of
        ObjectStorageClassGlacier -> "GLACIER"
        ObjectStorageClassReducedRedundancy -> "REDUCED_REDUNDANCY"
        ObjectStorageClassStandard -> "STANDARD"

-- | The class of storage used to store the object.
data ObjectVersionStorageClass
    = ObjectVersionStorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance ToByteString ObjectVersionStorageClass where
    toBS x = case x of
        ObjectVersionStorageClassStandard -> "STANDARD"

-- | Specifies who pays for the download and request fees.
data Payer
    = PayerBucketOwner -- ^ BucketOwner
    | PayerRequester -- ^ Requester
      deriving (Eq, Show, Generic)

instance ToByteString Payer where
    toBS x = case x of
        PayerBucketOwner -> "BucketOwner"
        PayerRequester -> "Requester"

-- | Specifies the permission given to the grantee.
data Permission
    = PermissionFullControl -- ^ FULL_CONTROL
    | PermissionRead -- ^ READ
    | PermissionReadAcp -- ^ READ_ACP
    | PermissionWrite -- ^ WRITE
    | PermissionWriteAcp -- ^ WRITE_ACP
      deriving (Eq, Show, Generic)

instance ToByteString Permission where
    toBS x = case x of
        PermissionFullControl -> "FULL_CONTROL"
        PermissionRead -> "READ"
        PermissionReadAcp -> "READ_ACP"
        PermissionWrite -> "WRITE"
        PermissionWriteAcp -> "WRITE_ACP"

-- | Protocol to use (http, https) when redirecting requests. The default is the
-- protocol that is used in the original request.
data Protocol
    = ProtocolHttp -- ^ http
    | ProtocolHttps -- ^ https
      deriving (Eq, Show, Generic)

instance ToByteString Protocol where
    toBS x = case x of
        ProtocolHttp -> "http"
        ProtocolHttps -> "https"

-- | The Server-side encryption algorithm used when storing this object in S3.
data ServerSideEncryption
    = ServerSideEncryptionAES256 -- ^ AES256
      deriving (Eq, Show, Generic)

instance ToByteString ServerSideEncryption where
    toBS x = case x of
        ServerSideEncryptionAES256 -> "AES256"

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
data StorageClass
    = StorageClassReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | StorageClassStandard -- ^ STANDARD
      deriving (Eq, Show, Generic)

instance ToByteString StorageClass where
    toBS x = case x of
        StorageClassReducedRedundancy -> "REDUCED_REDUNDANCY"
        StorageClassStandard -> "STANDARD"

-- | The class of storage used to store the object.
data TransitionStorageClass
    = TransitionStorageClassGlacier -- ^ GLACIER
      deriving (Eq, Show, Generic)

instance ToByteString TransitionStorageClass where
    toBS x = case x of
        TransitionStorageClassGlacier -> "GLACIER"

-- | Type of grantee.
data Type
    = TypeAmazonCustomerByEmail -- ^ AmazonCustomerByEmail
    | TypeCanonicalUser -- ^ CanonicalUser
    | TypeGroup -- ^ Group
      deriving (Eq, Show, Generic)

instance ToByteString Type where
    toBS x = case x of
        TypeAmazonCustomerByEmail -> "AmazonCustomerByEmail"
        TypeCanonicalUser -> "CanonicalUser"
        TypeGroup -> "Group"

newtype BucketLoggingStatus = BucketLoggingStatus
    { blsLoggingEnabled :: LoggingEnabled
    } deriving (Eq, Show, Generic)

newtype CORSConfiguration = CORSConfiguration
    { corscCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic)

newtype CommonPrefix = CommonPrefix
    { cpPrefix :: Text
    } deriving (Eq, Show, Generic)

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { cmuParts :: [CompletedPart]
    } deriving (Eq, Show, Generic)

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { cbcLocationConstraint :: BucketLocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Eq, Show, Generic)

newtype ErrorDocument = ErrorDocument
    { edKey :: ObjectKey
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Eq, Show, Generic)

newtype IndexDocument = IndexDocument
    { idSuffix :: Text
      -- ^ A suffix that is appended to a request that is for a directory on
      -- the website endpoint (e.g. if the suffix is index.html and you
      -- make a request to samplebucket/images/ the data that is returned
      -- will be for the object with the key name images/index.html) The
      -- suffix must not be empty and must not include a slash character.
    } deriving (Eq, Show, Generic)

newtype LifecycleConfiguration = LifecycleConfiguration
    { lcRules :: [Rule]
    } deriving (Eq, Show, Generic)

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

newtype NotificationConfiguration = NotificationConfiguration
    { ncTopicConfiguration :: TopicConfiguration
    } deriving (Eq, Show, Generic)

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Eq, Show, Generic)

newtype RestoreRequest = RestoreRequest
    { rrDays :: Integer
      -- ^ Lifetime of the active copy in days.
    } deriving (Eq, Show, Generic)

newtype Tagging = Tagging
    { tTagSet :: [Tag]
    } deriving (Eq, Show, Generic)

data AccessControlPolicy = AccessControlPolicy
    { acpGrants :: [Grant]
      -- ^ A list of grants.
    , acpOwner :: Owner
    } deriving (Eq, Show, Generic)

data Bucket = Bucket
    { bName :: BucketName
      -- ^ The name of the bucket.
    , bCreationDate :: RFC822
      -- ^ Date the bucket was created.
    } deriving (Eq, Show, Generic)

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

data CompletedPart = CompletedPart
    { cpETag :: ETag
      -- ^ Entity tag returned when the part was uploaded.
    , cpPartNumber :: Integer
      -- ^ Part number that identifies the part.
    } deriving (Eq, Show, Generic)

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

data CopyObjectResult = CopyObjectResult
    { corETag :: ETag
    , corLastModified :: RFC822
    } deriving (Eq, Show, Generic)

data CopyPartResult = CopyPartResult
    { cprETag :: ETag
      -- ^ Entity tag of the object.
    , cprLastModified :: RFC822
      -- ^ Date and time at which the object was uploaded.
    } deriving (Eq, Show, Generic)

data Delete = Delete
    { dQuiet :: Bool
      -- ^ Element to enable quiet mode for the request. When you add this
      -- element, you must set its value to true.
    , dObjects :: [ObjectIdentifier]
    } deriving (Eq, Show, Generic)

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

data DeletedObject = DeletedObject
    { doVersionId :: ObjectVersionId
    , doDeleteMarker :: Bool
    , doDeleteMarkerVersionId :: Text
    , doKey :: ObjectKey
    } deriving (Eq, Show, Generic)

data Error = Error
    { eVersionId :: ObjectVersionId
    , eKey :: ObjectKey
    , eCode :: Text
    , eMessage :: Text
    } deriving (Eq, Show, Generic)

data Grant = Grant
    { gPermission :: Permission
      -- ^ Specifies the permission given to the grantee.
    , gGrantee :: Grantee
    } deriving (Eq, Show, Generic)

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

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { iDisplayName :: Text
      -- ^ Name of the Principal.
    , iID :: Text
      -- ^ If the principal is an AWS account, it provides the Canonical
      -- User ID. If the principal is an IAM User, it provides a user ARN
      -- value.
    } deriving (Eq, Show, Generic)

data LifecycleExpiration = LifecycleExpiration
    { leDays :: Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , leDate :: RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    } deriving (Eq, Show, Generic)

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

data Object = Object
    { oETag :: ETag
    , oSize :: Integer
    , oOwner :: Owner
    , oKey :: ObjectKey
    , oStorageClass :: ObjectStorageClass
      -- ^ The class of storage used to store the object.
    , oLastModified :: RFC822
    } deriving (Eq, Show, Generic)

data ObjectIdentifier = ObjectIdentifier
    { oiVersionId :: ObjectVersionId
      -- ^ VersionId for the specific version of the object to delete.
    , oiKey :: ObjectKey
      -- ^ Key name of the object to delete.
    } deriving (Eq, Show, Generic)

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

data Owner = Owner
    { oDisplayName :: Text
    , oID :: Text
    } deriving (Eq, Show, Generic)

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

data RedirectAllRequestsTo = RedirectAllRequestsTo
    { rartHostName :: Text
      -- ^ Name of the host where requests will be redirected.
    , rartProtocol :: Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    } deriving (Eq, Show, Generic)

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

data Tag = Tag
    { tValue :: Text
      -- ^ Value of the tag.
    , tKey :: ObjectKey
      -- ^ Name of the tag.
    } deriving (Eq, Show, Generic)

data TargetGrant = TargetGrant
    { tgPermission :: BucketLogsPermission
      -- ^ Logging permissions assigned to the Grantee for the bucket.
    , tgGrantee :: Grantee
    } deriving (Eq, Show, Generic)

data TopicConfiguration = TopicConfiguration
    { tcEvent :: Event
      -- ^ Bucket event for which to send notifications.
    , tcTopic :: Text
      -- ^ Amazon SNS topic to which Amazon S3 will publish a message to
      -- report the specified events for the bucket.
    } deriving (Eq, Show, Generic)

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

data VersioningConfiguration = VersioningConfiguration
    { vcStatus :: Switch BucketVersioningStatus
      -- ^ The versioning state of the bucket.
    , vcMfaDelete :: Switch MFADelete
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has
      -- been configured with MFA delete. If the bucket has never been so
      -- configured, this element is not returned.
    } deriving (Eq, Show, Generic)

data WebsiteConfiguration = WebsiteConfiguration
    { wcRedirectAllRequestsTo :: RedirectAllRequestsTo
    , wcErrorDocument :: ErrorDocument
    , wcIndexDocument :: IndexDocument
    , wcRoutingRules :: [RoutingRule]
    } deriving (Eq, Show, Generic)
