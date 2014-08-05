{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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
module Network.AWS.S3.V2006_03_01.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Types (Region)
import Network.AWS.Signing.V4

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

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLoggingStatus"

newtype CORSConfiguration = CORSConfiguration
    { _corscCORSRules :: [CORSRule]
    } deriving (Show, Generic)

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSConfiguration"

newtype CommonPrefix = CommonPrefix
    { _cqPrefix :: Maybe Text
    } deriving (Show, Generic)

instance FromXML CommonPrefix where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CommonPrefix"

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { _cmuParts :: [CompletedPart]
    } deriving (Show, Generic)

instance ToXML CompletedMultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompleteMultipartUpload"

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint :: Maybe BucketLocationConstraint
      -- ^ Specifies the region where the bucket will be created.
    } deriving (Show, Generic)

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateBucketConfiguration"

newtype ErrorDocument = ErrorDocument
    { _edKey :: ObjectKey
      -- ^ The object key name to use when a 4XX class error occurs.
    } deriving (Show, Generic)

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

instance FromXML IndexDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexDocument"

instance ToXML IndexDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "IndexDocument"

newtype LifecycleConfiguration = LifecycleConfiguration
    { _lcRules :: [Rule]
    } deriving (Show, Generic)

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

instance FromXML NoncurrentVersionExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionExpiration"

instance ToXML NoncurrentVersionExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionExpiration"

newtype NotificationConfiguration = NotificationConfiguration
    { _ncTopicConfiguration :: TopicConfiguration
    } deriving (Show, Generic)

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NotificationConfiguration"

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { _rpcPayer :: Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Show, Generic)

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RequestPaymentConfiguration"

newtype RestoreRequest = RestoreRequest
    { _rzDays :: Integer
      -- ^ Lifetime of the active copy in days.
    } deriving (Show, Generic)

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RestoreRequest"

newtype Tagging = Tagging
    { _tiTagSet :: [Tag]
    } deriving (Show, Generic)

instance ToXML Tagging where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tagging"

data AccessControlPolicy = AccessControlPolicy
    { _acpGrants :: [Grant]
      -- ^ A list of grants.
    , _acpOwner :: Maybe Owner
    } deriving (Show, Generic)

instance ToXML AccessControlPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AccessControlPolicy"

data Bucket = Bucket
    { _hName :: Maybe BucketName
      -- ^ The name of the bucket.
    , _hCreationDate :: Maybe RFC822
      -- ^ Date the bucket was created.
    } deriving (Show, Generic)

instance FromXML Bucket where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Bucket"

data CORSRule = CORSRule
    { _corssAllowedMethods :: [Text]
      -- ^ Identifies HTTP methods that the domain/origin specified in the
      -- rule is allowed to execute.
    , _corssMaxAgeSeconds :: Maybe Integer
      -- ^ The time in seconds that your browser is to cache the preflight
      -- response for the specified resource.
    , _corssAllowedHeaders :: [Text]
      -- ^ Specifies which headers are allowed in a pre-flight OPTIONS
      -- request.
    , _corssAllowedOrigins :: [Text]
      -- ^ One or more origins you want customers to be able to access the
      -- bucket from.
    , _corssExposeHeaders :: [Text]
      -- ^ One or more headers in the response that you want customers to be
      -- able to access from their applications (for example, from a
      -- JavaScript XMLHttpRequest object).
    } deriving (Show, Generic)

instance FromXML CORSRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CORSRule"

instance ToXML CORSRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSRule"

data CompletedPart = CompletedPart
    { _cyETag :: Maybe ETag
      -- ^ Entity tag returned when the part was uploaded.
    , _cyPartNumber :: Maybe Integer
      -- ^ Part number that identifies the part.
    } deriving (Show, Generic)

instance ToXML CompletedPart where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompletedPart"

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
data Condition = Condition
    { _cpKeyPrefixEquals :: Maybe Text
      -- ^ The object key name prefix when the redirect is applied. For
      -- example, to redirect requests for ExamplePage.html, the key
      -- prefix will be ExamplePage.html. To redirect request for all
      -- pages with the prefix docs/, the key prefix will be /docs, which
      -- identifies all objects in the docs/ folder. Required when the
      -- parent element Condition is specified and sibling
      -- HttpErrorCodeReturnedEquals is not specified. If both conditions
      -- are specified, both must be true for the redirect to be applied.
    , _cpHttpErrorCodeReturnedEquals :: Maybe Text
      -- ^ The HTTP error code when the redirect is applied. In the event of
      -- an error, if the error code equals this value, then the specified
      -- redirect is applied. Required when parent element Condition is
      -- specified and sibling KeyPrefixEquals is not specified. If both
      -- are specified, then both must be true for the redirect to be
      -- applied.
    } deriving (Show, Generic)

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

instance FromXML CopyObjectResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyObjectResult"

data CopyPartResult = CopyPartResult
    { _cprETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cprLastModified :: Maybe RFC822
      -- ^ Date and time at which the object was uploaded.
    } deriving (Show, Generic)

instance FromXML CopyPartResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyPartResult"

data Delete = Delete
    { _diQuiet :: Maybe Bool
      -- ^ Element to enable quiet mode for the request. When you add this
      -- element, you must set its value to true.
    , _diObjects :: [ObjectIdentifier]
    } deriving (Show, Generic)

instance ToXML Delete where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Delete"

data DeleteMarkerEntry = DeleteMarkerEntry
    { _dmeVersionId :: Maybe ObjectVersionId
      -- ^ Version ID of an object.
    , _dmeIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , _dmeOwner :: Maybe Owner
    , _dmeKey :: Maybe ObjectKey
      -- ^ The object key.
    , _dmeLastModified :: Maybe RFC822
      -- ^ Date and time the object was last modified.
    } deriving (Show, Generic)

instance FromXML DeleteMarkerEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMarkerEntry"

data DeletedObject = DeletedObject
    { _ddyVersionId :: Maybe ObjectVersionId
    , _ddyDeleteMarker :: Maybe Bool
    , _ddyDeleteMarkerVersionId :: Maybe Text
    , _ddyKey :: Maybe ObjectKey
    } deriving (Show, Generic)

instance FromXML DeletedObject where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletedObject"

data Error = Error
    { _erVersionId :: Maybe ObjectVersionId
    , _erKey :: Maybe ObjectKey
    , _erCode :: Maybe Text
    , _erMessage :: Maybe Text
    } deriving (Show, Generic)

instance FromXML Error where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Error"

data Grant = Grant
    { _mPermission :: Maybe Permission
      -- ^ Specifies the permission given to the grantee.
    , _mGrantee :: Maybe Grantee
    } deriving (Show, Generic)

instance FromXML Grant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML Grant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data Grantee = Grantee
    { _gURI :: Maybe Text
      -- ^ URI of the grantee group.
    , _gEmailAddress :: Maybe Text
      -- ^ Email address of the grantee.
    , _gDisplayName :: Maybe Text
      -- ^ Screen name of the grantee.
    , _gID :: Maybe Text
      -- ^ The canonical user ID of the grantee.
    , _gType :: Type
      -- ^ Type of grantee.
    } deriving (Show, Generic)

instance FromXML Grantee where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grantee"

instance ToXML Grantee where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grantee"

-- | Identifies who initiated the multipart upload.
data Initiator = Initiator
    { _irDisplayName :: Maybe Text
      -- ^ Name of the Principal.
    , _irID :: Maybe Text
      -- ^ If the principal is an AWS account, it provides the Canonical
      -- User ID. If the principal is an IAM User, it provides a user ARN
      -- value.
    } deriving (Show, Generic)

instance FromXML Initiator where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Initiator"

instance ToXML Initiator where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Initiator"

data LifecycleExpiration = LifecycleExpiration
    { _lfDays :: Maybe Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , _lfDate :: Maybe RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    } deriving (Show, Generic)

instance FromXML LifecycleExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleExpiration"

instance ToXML LifecycleExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleExpiration"

data LoggingEnabled = LoggingEnabled
    { _leTargetBucket :: Maybe Text
      -- ^ Specifies the bucket where you want Amazon S3 to store server
      -- access logs. You can have your logs delivered to any bucket that
      -- you own, including the same bucket that is being logged. You can
      -- also configure multiple buckets to deliver their logs to the same
      -- target bucket. In this case you should choose a different
      -- TargetPrefix for each source bucket so that the delivered log
      -- files can be distinguished by key.
    , _leTargetGrants :: [TargetGrant]
    , _leTargetPrefix :: Maybe Text
      -- ^ This element lets you specify a prefix for the keys that the log
      -- files will be stored under.
    } deriving (Show, Generic)

instance FromXML LoggingEnabled where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoggingEnabled"

instance ToXML LoggingEnabled where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LoggingEnabled"

data MultipartUpload = MultipartUpload
    { _mwInitiated :: Maybe RFC822
      -- ^ Date and time at which the multipart upload was initiated.
    , _mwInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , _mwOwner :: Maybe Owner
    , _mwKey :: Maybe ObjectKey
      -- ^ Key of the object for which the multipart upload was initiated.
    , _mwStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , _mwUploadId :: Maybe Text
      -- ^ Upload ID that identifies the multipart upload.
    } deriving (Show, Generic)

instance FromXML MultipartUpload where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MultipartUpload"

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action to
-- request that Amazon S3 transition noncurrent object versions to the GLACIER
-- storage class at a specific period in the object's lifetime.
data NoncurrentVersionTransition = NoncurrentVersionTransition
    { _nvtStorageClass :: Maybe TransitionStorageClass
      -- ^ The class of storage used to store the object.
    , _nvtNoncurrentDays :: Maybe Integer
      -- ^ Specifies the number of days an object is noncurrent before
      -- Amazon S3 can perform the associated action. For information
      -- about the noncurrent days calculations, see How Amazon S3
      -- Calculates When an Object Became Noncurrent in the Amazon Simple
      -- Storage Service Developer Guide.
    } deriving (Show, Generic)

instance FromXML NoncurrentVersionTransition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionTransition"

instance ToXML NoncurrentVersionTransition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionTransition"

data Object = Object
    { _oouETag :: ETag
    , _oouSize :: Integer
    , _oouOwner :: Owner
    , _oouKey :: ObjectKey
    , _oouStorageClass :: ObjectStorageClass
      -- ^ The class of storage used to store the object.
    , _oouLastModified :: RFC822
    } deriving (Show, Generic)

instance FromXML Object where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Object"

data ObjectIdentifier = ObjectIdentifier
    { _oiVersionId :: Maybe ObjectVersionId
      -- ^ VersionId for the specific version of the object to delete.
    , _oiKey :: ObjectKey
      -- ^ Key name of the object to delete.
    } deriving (Show, Generic)

instance ToXML ObjectIdentifier where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectIdentifier"

data ObjectVersion = ObjectVersion
    { _ovETag :: Maybe ETag
    , _ovVersionId :: Maybe ObjectVersionId
      -- ^ Version ID of an object.
    , _ovSize :: Maybe Integer
      -- ^ Size in bytes of the object.
    , _ovIsLatest :: Maybe Bool
      -- ^ Specifies whether the object is (true) or is not (false) the
      -- latest version of an object.
    , _ovOwner :: Maybe Owner
    , _ovKey :: Maybe ObjectKey
      -- ^ The object key.
    , _ovStorageClass :: Maybe ObjectVersionStorageClass
      -- ^ The class of storage used to store the object.
    , _ovLastModified :: Maybe RFC822
      -- ^ Date and time the object was last modified.
    } deriving (Show, Generic)

instance FromXML ObjectVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersion"

data Owner = Owner
    { _oDisplayName :: Maybe Text
    , _oID :: Maybe Text
    } deriving (Show, Generic)

instance FromXML Owner where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Owner"

instance ToXML Owner where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Owner"

data Part = Part
    { _pvETag :: Maybe ETag
      -- ^ Entity tag returned when the part was uploaded.
    , _pvSize :: Maybe Integer
      -- ^ Size of the uploaded part data.
    , _pvPartNumber :: Maybe Integer
      -- ^ Part number identifying the part.
    , _pvLastModified :: Maybe RFC822
      -- ^ Date and time at which the part was uploaded.
    } deriving (Show, Generic)

instance FromXML Part where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Part"

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an error,
-- you can can specify a different error code to return.
data Redirect = Redirect
    { _rtHostName :: Maybe Text
      -- ^ The host name to use in the redirect request.
    , _rtProtocol :: Maybe Protocol
      -- ^ Protocol to use (http, https) when redirecting requests. The
      -- default is the protocol that is used in the original request.
    , _rtHttpRedirectCode :: Maybe Text
      -- ^ The HTTP redirect code to use on the response. Not required if
      -- one of the siblings is present.
    , _rtReplaceKeyWith :: Maybe Text
      -- ^ The specific object key to use in the redirect request. For
      -- example, redirect request to error.html. Not required if one of
      -- the sibling is present. Can be present only if
      -- ReplaceKeyPrefixWith is not provided.
    , _rtReplaceKeyPrefixWith :: Maybe Text
      -- ^ The object key prefix to use in the redirect request. For
      -- example, to redirect requests for all pages with prefix docs/
      -- (objects in the docs/ folder) to documents/, you can set a
      -- condition block with KeyPrefixEquals set to docs/ and in the
      -- Redirect set ReplaceKeyPrefixWith to /documents. Not required if
      -- one of the siblings is present. Can be present only if
      -- ReplaceKeyWith is not provided.
    } deriving (Show, Generic)

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

instance FromXML RedirectAllRequestsTo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RedirectAllRequestsTo"

instance ToXML RedirectAllRequestsTo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RedirectAllRequestsTo"

data RoutingRule = RoutingRule
    { _rsRedirect :: Redirect
      -- ^ Container for redirect information. You can redirect requests to
      -- another host, to another page, or with another protocol. In the
      -- event of an error, you can can specify a different error code to
      -- return.
    , _rsCondition :: Maybe Condition
      -- ^ A container for describing a condition that must be met for the
      -- specified redirect to apply. For example, 1. If request is for
      -- pages in the /docs folder, redirect to the /documents folder. 2.
      -- If request results in HTTP error 4xx, redirect request to another
      -- host where you might process the error.
    } deriving (Show, Generic)

instance FromXML RoutingRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RoutingRule"

instance ToXML RoutingRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RoutingRule"

data Rule = Rule
    { _rhStatus :: Switch ExpirationStatus
      -- ^ If 'Enabled', the rule is currently being applied. If 'Disabled',
      -- the rule is not currently being applied.
    , _rhNoncurrentVersionExpiration :: Maybe NoncurrentVersionExpiration
      -- ^ Specifies when noncurrent object versions expire. Upon
      -- expiration, Amazon S3 permanently deletes the noncurrent object
      -- versions. You set this lifecycle configuration action on a bucket
      -- that has versioning enabled (or suspended) to request that Amazon
      -- S3 delete noncurrent object versions at a specific period in the
      -- object's lifetime.
    , _rhTransition :: Maybe Transition
    , _rhPrefix :: Text
      -- ^ Prefix identifying one or more objects to which the rule applies.
    , _rhExpiration :: Maybe LifecycleExpiration
    , _rhNoncurrentVersionTransition :: Maybe NoncurrentVersionTransition
      -- ^ Container for the transition rule that describes when noncurrent
      -- objects transition to the GLACIER storage class. If your bucket
      -- is versioning-enabled (or versioning is suspended), you can set
      -- this action to request that Amazon S3 transition noncurrent
      -- object versions to the GLACIER storage class at a specific period
      -- in the object's lifetime.
    , _rhID :: Maybe Text
      -- ^ Unique identifier for the rule. The value cannot be longer than
      -- 255 characters.
    } deriving (Show, Generic)

instance FromXML Rule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Rule"

instance ToXML Rule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Rule"

data Tag = Tag
    { _tjValue :: Text
      -- ^ Value of the tag.
    , _tjKey :: ObjectKey
      -- ^ Name of the tag.
    } deriving (Show, Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"

data TargetGrant = TargetGrant
    { _thPermission :: Maybe BucketLogsPermission
      -- ^ Logging permissions assigned to the Grantee for the bucket.
    , _thGrantee :: Maybe Grantee
    } deriving (Show, Generic)

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

instance FromXML TopicConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TopicConfiguration"

instance ToXML TopicConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TopicConfiguration"

data Transition = Transition
    { _tnDays :: Maybe Integer
      -- ^ Indicates the lifetime, in days, of the objects that are subject
      -- to the rule. The value must be a non-zero positive integer.
    , _tnDate :: Maybe RFC822
      -- ^ Indicates at what date the object is to be moved or deleted.
      -- Should be in GMT ISO 8601 Format.
    , _tnStorageClass :: Maybe TransitionStorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

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

instance ToXML VersioningConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "VersioningConfiguration"

data WebsiteConfiguration = WebsiteConfiguration
    { _wcRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _wcErrorDocument :: Maybe ErrorDocument
    , _wcIndexDocument :: Maybe IndexDocument
    , _wcRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "WebsiteConfiguration"

-- Newtypes
makeIso ''BucketLoggingStatus
makeIso ''CORSConfiguration
makeIso ''CommonPrefix
makeIso ''CompletedMultipartUpload
makeIso ''CreateBucketConfiguration
makeIso ''ErrorDocument
makeIso ''IndexDocument
makeIso ''LifecycleConfiguration
makeIso ''NoncurrentVersionExpiration
makeIso ''NotificationConfiguration
makeIso ''RequestPaymentConfiguration
makeIso ''RestoreRequest
makeIso ''Tagging

-- Products
makeLenses ''AccessControlPolicy
makeLenses ''Bucket
makeLenses ''CORSRule
makeLenses ''CompletedPart
makeLenses ''Condition
makeLenses ''CopyObjectResult
makeLenses ''CopyPartResult
makeLenses ''Delete
makeLenses ''DeleteMarkerEntry
makeLenses ''DeletedObject
makeLenses ''Error
makeLenses ''Grant
makeLenses ''Grantee
makeLenses ''Initiator
makeLenses ''LifecycleExpiration
makeLenses ''LoggingEnabled
makeLenses ''MultipartUpload
makeLenses ''NoncurrentVersionTransition
makeLenses ''Object
makeLenses ''ObjectIdentifier
makeLenses ''ObjectVersion
makeLenses ''Owner
makeLenses ''Part
makeLenses ''Redirect
makeLenses ''RedirectAllRequestsTo
makeLenses ''RoutingRule
makeLenses ''Rule
makeLenses ''Tag
makeLenses ''TargetGrant
makeLenses ''TopicConfiguration
makeLenses ''Transition
makeLenses ''VersioningConfiguration
makeLenses ''WebsiteConfiguration
