{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Sum where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data AnalyticsS3ExportFileFormat =
  CSV
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AnalyticsS3ExportFileFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        e -> fromTextError $ "Failure parsing AnalyticsS3ExportFileFormat from value: '" <> e
           <> "'. Accepted values: csv"

instance ToText AnalyticsS3ExportFileFormat where
    toText = \case
        CSV -> "CSV"

instance Hashable     AnalyticsS3ExportFileFormat
instance NFData       AnalyticsS3ExportFileFormat
instance ToByteString AnalyticsS3ExportFileFormat
instance ToQuery      AnalyticsS3ExportFileFormat
instance ToHeader     AnalyticsS3ExportFileFormat

instance FromXML AnalyticsS3ExportFileFormat where
    parseXML = parseXMLText "AnalyticsS3ExportFileFormat"

instance ToXML AnalyticsS3ExportFileFormat where
    toXML = toXMLText

data BucketAccelerateStatus
  = BASEnabled
  | BASSuspended
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BucketAccelerateStatus where
    parser = takeLowerText >>= \case
        "enabled" -> pure BASEnabled
        "suspended" -> pure BASSuspended
        e -> fromTextError $ "Failure parsing BucketAccelerateStatus from value: '" <> e
           <> "'. Accepted values: enabled, suspended"

instance ToText BucketAccelerateStatus where
    toText = \case
        BASEnabled -> "Enabled"
        BASSuspended -> "Suspended"

instance Hashable     BucketAccelerateStatus
instance NFData       BucketAccelerateStatus
instance ToByteString BucketAccelerateStatus
instance ToQuery      BucketAccelerateStatus
instance ToHeader     BucketAccelerateStatus

instance FromXML BucketAccelerateStatus where
    parseXML = parseXMLText "BucketAccelerateStatus"

instance ToXML BucketAccelerateStatus where
    toXML = toXMLText

data BucketCannedACL
  = BAuthenticatedRead
  | BPrivate
  | BPublicRead
  | BPublicReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

instance Hashable     BucketCannedACL
instance NFData       BucketCannedACL
instance ToByteString BucketCannedACL
instance ToQuery      BucketCannedACL
instance ToHeader     BucketCannedACL

instance ToXML BucketCannedACL where
    toXML = toXMLText

data BucketLogsPermission
  = FullControl
  | Read
  | Write
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BucketLogsPermission where
    parser = takeLowerText >>= \case
        "full_control" -> pure FullControl
        "read" -> pure Read
        "write" -> pure Write
        e -> fromTextError $ "Failure parsing BucketLogsPermission from value: '" <> e
           <> "'. Accepted values: full_control, read, write"

instance ToText BucketLogsPermission where
    toText = \case
        FullControl -> "FULL_CONTROL"
        Read -> "READ"
        Write -> "WRITE"

instance Hashable     BucketLogsPermission
instance NFData       BucketLogsPermission
instance ToByteString BucketLogsPermission
instance ToQuery      BucketLogsPermission
instance ToHeader     BucketLogsPermission

instance FromXML BucketLogsPermission where
    parseXML = parseXMLText "BucketLogsPermission"

instance ToXML BucketLogsPermission where
    toXML = toXMLText

data BucketVersioningStatus
  = BVSEnabled
  | BVSSuspended
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BucketVersioningStatus where
    parser = takeLowerText >>= \case
        "enabled" -> pure BVSEnabled
        "suspended" -> pure BVSSuspended
        e -> fromTextError $ "Failure parsing BucketVersioningStatus from value: '" <> e
           <> "'. Accepted values: enabled, suspended"

instance ToText BucketVersioningStatus where
    toText = \case
        BVSEnabled -> "Enabled"
        BVSSuspended -> "Suspended"

instance Hashable     BucketVersioningStatus
instance NFData       BucketVersioningStatus
instance ToByteString BucketVersioningStatus
instance ToQuery      BucketVersioningStatus
instance ToHeader     BucketVersioningStatus

instance FromXML BucketVersioningStatus where
    parseXML = parseXMLText "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
    toXML = toXMLText

data CompressionType
  = CTGzip
  | CTNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionType where
    parser = takeLowerText >>= \case
        "gzip" -> pure CTGzip
        "none" -> pure CTNone
        e -> fromTextError $ "Failure parsing CompressionType from value: '" <> e
           <> "'. Accepted values: gzip, none"

instance ToText CompressionType where
    toText = \case
        CTGzip -> "GZIP"
        CTNone -> "NONE"

instance Hashable     CompressionType
instance NFData       CompressionType
instance ToByteString CompressionType
instance ToQuery      CompressionType
instance ToHeader     CompressionType

instance ToXML CompressionType where
    toXML = toXMLText

-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
data EncodingType =
  URL
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: url"

instance ToText EncodingType where
    toText = \case
        URL -> "url"

instance Hashable     EncodingType
instance NFData       EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

instance FromXML EncodingType where
    parseXML = parseXMLText "EncodingType"

instance ToXML EncodingType where
    toXML = toXMLText

-- | Bucket event for which to send notifications.
data Event
  = S3ObjectCreated
  | S3ObjectCreatedCompleteMultipartUpload
  | S3ObjectCreatedCopy
  | S3ObjectCreatedPost
  | S3ObjectCreatedPut
  | S3ObjectRemoved
  | S3ObjectRemovedDelete
  | S3ObjectRemovedDeleteMarkerCreated
  | S3ReducedRedundancyLostObject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Event where
    parser = takeLowerText >>= \case
        "s3:objectcreated:*" -> pure S3ObjectCreated
        "s3:objectcreated:completemultipartupload" -> pure S3ObjectCreatedCompleteMultipartUpload
        "s3:objectcreated:copy" -> pure S3ObjectCreatedCopy
        "s3:objectcreated:post" -> pure S3ObjectCreatedPost
        "s3:objectcreated:put" -> pure S3ObjectCreatedPut
        "s3:objectremoved:*" -> pure S3ObjectRemoved
        "s3:objectremoved:delete" -> pure S3ObjectRemovedDelete
        "s3:objectremoved:deletemarkercreated" -> pure S3ObjectRemovedDeleteMarkerCreated
        "s3:reducedredundancylostobject" -> pure S3ReducedRedundancyLostObject
        e -> fromTextError $ "Failure parsing Event from value: '" <> e
           <> "'. Accepted values: s3:objectcreated:*, s3:objectcreated:completemultipartupload, s3:objectcreated:copy, s3:objectcreated:post, s3:objectcreated:put, s3:objectremoved:*, s3:objectremoved:delete, s3:objectremoved:deletemarkercreated, s3:reducedredundancylostobject"

instance ToText Event where
    toText = \case
        S3ObjectCreated -> "s3:ObjectCreated:*"
        S3ObjectCreatedCompleteMultipartUpload -> "s3:ObjectCreated:CompleteMultipartUpload"
        S3ObjectCreatedCopy -> "s3:ObjectCreated:Copy"
        S3ObjectCreatedPost -> "s3:ObjectCreated:Post"
        S3ObjectCreatedPut -> "s3:ObjectCreated:Put"
        S3ObjectRemoved -> "s3:ObjectRemoved:*"
        S3ObjectRemovedDelete -> "s3:ObjectRemoved:Delete"
        S3ObjectRemovedDeleteMarkerCreated -> "s3:ObjectRemoved:DeleteMarkerCreated"
        S3ReducedRedundancyLostObject -> "s3:ReducedRedundancyLostObject"

instance Hashable     Event
instance NFData       Event
instance ToByteString Event
instance ToQuery      Event
instance ToHeader     Event

instance FromXML Event where
    parseXML = parseXMLText "Event"

instance ToXML Event where
    toXML = toXMLText

data ExpirationStatus
  = ESDisabled
  | ESEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExpirationStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESDisabled
        "enabled" -> pure ESEnabled
        e -> fromTextError $ "Failure parsing ExpirationStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ExpirationStatus where
    toText = \case
        ESDisabled -> "Disabled"
        ESEnabled -> "Enabled"

instance Hashable     ExpirationStatus
instance NFData       ExpirationStatus
instance ToByteString ExpirationStatus
instance ToQuery      ExpirationStatus
instance ToHeader     ExpirationStatus

instance FromXML ExpirationStatus where
    parseXML = parseXMLText "ExpirationStatus"

instance ToXML ExpirationStatus where
    toXML = toXMLText

data ExpressionType =
  Sql
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExpressionType where
    parser = takeLowerText >>= \case
        "sql" -> pure Sql
        e -> fromTextError $ "Failure parsing ExpressionType from value: '" <> e
           <> "'. Accepted values: sql"

instance ToText ExpressionType where
    toText = \case
        Sql -> "SQL"

instance Hashable     ExpressionType
instance NFData       ExpressionType
instance ToByteString ExpressionType
instance ToQuery      ExpressionType
instance ToHeader     ExpressionType

instance ToXML ExpressionType where
    toXML = toXMLText

data FileHeaderInfo
  = Ignore
  | None
  | Use
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FileHeaderInfo where
    parser = takeLowerText >>= \case
        "ignore" -> pure Ignore
        "none" -> pure None
        "use" -> pure Use
        e -> fromTextError $ "Failure parsing FileHeaderInfo from value: '" <> e
           <> "'. Accepted values: ignore, none, use"

instance ToText FileHeaderInfo where
    toText = \case
        Ignore -> "IGNORE"
        None -> "NONE"
        Use -> "USE"

instance Hashable     FileHeaderInfo
instance NFData       FileHeaderInfo
instance ToByteString FileHeaderInfo
instance ToQuery      FileHeaderInfo
instance ToHeader     FileHeaderInfo

instance ToXML FileHeaderInfo where
    toXML = toXMLText

data FilterRuleName
  = Prefix
  | Suffix
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FilterRuleName where
    parser = takeLowerText >>= \case
        "prefix" -> pure Prefix
        "suffix" -> pure Suffix
        e -> fromTextError $ "Failure parsing FilterRuleName from value: '" <> e
           <> "'. Accepted values: prefix, suffix"

instance ToText FilterRuleName where
    toText = \case
        Prefix -> "prefix"
        Suffix -> "suffix"

instance Hashable     FilterRuleName
instance NFData       FilterRuleName
instance ToByteString FilterRuleName
instance ToQuery      FilterRuleName
instance ToHeader     FilterRuleName

instance FromXML FilterRuleName where
    parseXML = parseXMLText "FilterRuleName"

instance ToXML FilterRuleName where
    toXML = toXMLText

data InventoryFormat
  = IFCSV
  | IFOrc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventoryFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure IFCSV
        "orc" -> pure IFOrc
        e -> fromTextError $ "Failure parsing InventoryFormat from value: '" <> e
           <> "'. Accepted values: csv, orc"

instance ToText InventoryFormat where
    toText = \case
        IFCSV -> "CSV"
        IFOrc -> "ORC"

instance Hashable     InventoryFormat
instance NFData       InventoryFormat
instance ToByteString InventoryFormat
instance ToQuery      InventoryFormat
instance ToHeader     InventoryFormat

instance FromXML InventoryFormat where
    parseXML = parseXMLText "InventoryFormat"

instance ToXML InventoryFormat where
    toXML = toXMLText

data InventoryFrequency
  = Daily
  | Weekly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventoryFrequency where
    parser = takeLowerText >>= \case
        "daily" -> pure Daily
        "weekly" -> pure Weekly
        e -> fromTextError $ "Failure parsing InventoryFrequency from value: '" <> e
           <> "'. Accepted values: daily, weekly"

instance ToText InventoryFrequency where
    toText = \case
        Daily -> "Daily"
        Weekly -> "Weekly"

instance Hashable     InventoryFrequency
instance NFData       InventoryFrequency
instance ToByteString InventoryFrequency
instance ToQuery      InventoryFrequency
instance ToHeader     InventoryFrequency

instance FromXML InventoryFrequency where
    parseXML = parseXMLText "InventoryFrequency"

instance ToXML InventoryFrequency where
    toXML = toXMLText

data InventoryIncludedObjectVersions
  = All
  | Current
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventoryIncludedObjectVersions where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "current" -> pure Current
        e -> fromTextError $ "Failure parsing InventoryIncludedObjectVersions from value: '" <> e
           <> "'. Accepted values: all, current"

instance ToText InventoryIncludedObjectVersions where
    toText = \case
        All -> "All"
        Current -> "Current"

instance Hashable     InventoryIncludedObjectVersions
instance NFData       InventoryIncludedObjectVersions
instance ToByteString InventoryIncludedObjectVersions
instance ToQuery      InventoryIncludedObjectVersions
instance ToHeader     InventoryIncludedObjectVersions

instance FromXML InventoryIncludedObjectVersions where
    parseXML = parseXMLText "InventoryIncludedObjectVersions"

instance ToXML InventoryIncludedObjectVersions where
    toXML = toXMLText

data InventoryOptionalField
  = FieldETag
  | FieldEncryptionStatus
  | FieldIsMultipartUploaded
  | FieldLastModifiedDate
  | FieldReplicationStatus
  | FieldSize
  | FieldStorageClass
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventoryOptionalField where
    parser = takeLowerText >>= \case
        "etag" -> pure FieldETag
        "encryptionstatus" -> pure FieldEncryptionStatus
        "ismultipartuploaded" -> pure FieldIsMultipartUploaded
        "lastmodifieddate" -> pure FieldLastModifiedDate
        "replicationstatus" -> pure FieldReplicationStatus
        "size" -> pure FieldSize
        "storageclass" -> pure FieldStorageClass
        e -> fromTextError $ "Failure parsing InventoryOptionalField from value: '" <> e
           <> "'. Accepted values: etag, encryptionstatus, ismultipartuploaded, lastmodifieddate, replicationstatus, size, storageclass"

instance ToText InventoryOptionalField where
    toText = \case
        FieldETag -> "ETag"
        FieldEncryptionStatus -> "EncryptionStatus"
        FieldIsMultipartUploaded -> "IsMultipartUploaded"
        FieldLastModifiedDate -> "LastModifiedDate"
        FieldReplicationStatus -> "ReplicationStatus"
        FieldSize -> "Size"
        FieldStorageClass -> "StorageClass"

instance Hashable     InventoryOptionalField
instance NFData       InventoryOptionalField
instance ToByteString InventoryOptionalField
instance ToQuery      InventoryOptionalField
instance ToHeader     InventoryOptionalField

instance FromXML InventoryOptionalField where
    parseXML = parseXMLText "InventoryOptionalField"

instance ToXML InventoryOptionalField where
    toXML = toXMLText

data JSONType
  = Document
  | Lines
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JSONType where
    parser = takeLowerText >>= \case
        "document" -> pure Document
        "lines" -> pure Lines
        e -> fromTextError $ "Failure parsing JSONType from value: '" <> e
           <> "'. Accepted values: document, lines"

instance ToText JSONType where
    toText = \case
        Document -> "DOCUMENT"
        Lines -> "LINES"

instance Hashable     JSONType
instance NFData       JSONType
instance ToByteString JSONType
instance ToQuery      JSONType
instance ToHeader     JSONType

instance ToXML JSONType where
    toXML = toXMLText

data MFADelete
  = MDDisabled
  | MDEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MFADelete where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDDisabled
        "enabled" -> pure MDEnabled
        e -> fromTextError $ "Failure parsing MFADelete from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText MFADelete where
    toText = \case
        MDDisabled -> "Disabled"
        MDEnabled -> "Enabled"

instance Hashable     MFADelete
instance NFData       MFADelete
instance ToByteString MFADelete
instance ToQuery      MFADelete
instance ToHeader     MFADelete

instance ToXML MFADelete where
    toXML = toXMLText

data MFADeleteStatus
  = MDSDisabled
  | MDSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MFADeleteStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDSDisabled
        "enabled" -> pure MDSEnabled
        e -> fromTextError $ "Failure parsing MFADeleteStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText MFADeleteStatus where
    toText = \case
        MDSDisabled -> "Disabled"
        MDSEnabled -> "Enabled"

instance Hashable     MFADeleteStatus
instance NFData       MFADeleteStatus
instance ToByteString MFADeleteStatus
instance ToQuery      MFADeleteStatus
instance ToHeader     MFADeleteStatus

instance FromXML MFADeleteStatus where
    parseXML = parseXMLText "MFADeleteStatus"

data MetadataDirective
  = MDCopy
  | MDReplace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetadataDirective where
    parser = takeLowerText >>= \case
        "copy" -> pure MDCopy
        "replace" -> pure MDReplace
        e -> fromTextError $ "Failure parsing MetadataDirective from value: '" <> e
           <> "'. Accepted values: copy, replace"

instance ToText MetadataDirective where
    toText = \case
        MDCopy -> "COPY"
        MDReplace -> "REPLACE"

instance Hashable     MetadataDirective
instance NFData       MetadataDirective
instance ToByteString MetadataDirective
instance ToQuery      MetadataDirective
instance ToHeader     MetadataDirective

instance ToXML MetadataDirective where
    toXML = toXMLText

data ObjectCannedACL
  = OAWSExecRead
  | OAuthenticatedRead
  | OBucketOwnerFullControl
  | OBucketOwnerRead
  | OPrivate
  | OPublicRead
  | OPublicReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectCannedACL where
    parser = takeLowerText >>= \case
        "aws-exec-read" -> pure OAWSExecRead
        "authenticated-read" -> pure OAuthenticatedRead
        "bucket-owner-full-control" -> pure OBucketOwnerFullControl
        "bucket-owner-read" -> pure OBucketOwnerRead
        "private" -> pure OPrivate
        "public-read" -> pure OPublicRead
        "public-read-write" -> pure OPublicReadWrite
        e -> fromTextError $ "Failure parsing ObjectCannedACL from value: '" <> e
           <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText ObjectCannedACL where
    toText = \case
        OAWSExecRead -> "aws-exec-read"
        OAuthenticatedRead -> "authenticated-read"
        OBucketOwnerFullControl -> "bucket-owner-full-control"
        OBucketOwnerRead -> "bucket-owner-read"
        OPrivate -> "private"
        OPublicRead -> "public-read"
        OPublicReadWrite -> "public-read-write"

instance Hashable     ObjectCannedACL
instance NFData       ObjectCannedACL
instance ToByteString ObjectCannedACL
instance ToQuery      ObjectCannedACL
instance ToHeader     ObjectCannedACL

instance ToXML ObjectCannedACL where
    toXML = toXMLText

data ObjectStorageClass
  = OSCGlacier
  | OSCReducedRedundancy
  | OSCStandard
  | OSCStandardIA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure OSCGlacier
        "reduced_redundancy" -> pure OSCReducedRedundancy
        "standard" -> pure OSCStandard
        "standard_ia" -> pure OSCStandardIA
        e -> fromTextError $ "Failure parsing ObjectStorageClass from value: '" <> e
           <> "'. Accepted values: glacier, reduced_redundancy, standard, standard_ia"

instance ToText ObjectStorageClass where
    toText = \case
        OSCGlacier -> "GLACIER"
        OSCReducedRedundancy -> "REDUCED_REDUNDANCY"
        OSCStandard -> "STANDARD"
        OSCStandardIA -> "STANDARD_IA"

instance Hashable     ObjectStorageClass
instance NFData       ObjectStorageClass
instance ToByteString ObjectStorageClass
instance ToQuery      ObjectStorageClass
instance ToHeader     ObjectStorageClass

instance FromXML ObjectStorageClass where
    parseXML = parseXMLText "ObjectStorageClass"

data ObjectVersionStorageClass =
  OVSCStandard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectVersionStorageClass where
    parser = takeLowerText >>= \case
        "standard" -> pure OVSCStandard
        e -> fromTextError $ "Failure parsing ObjectVersionStorageClass from value: '" <> e
           <> "'. Accepted values: standard"

instance ToText ObjectVersionStorageClass where
    toText = \case
        OVSCStandard -> "STANDARD"

instance Hashable     ObjectVersionStorageClass
instance NFData       ObjectVersionStorageClass
instance ToByteString ObjectVersionStorageClass
instance ToQuery      ObjectVersionStorageClass
instance ToHeader     ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
    parseXML = parseXMLText "ObjectVersionStorageClass"

data OwnerOverride =
  Destination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OwnerOverride where
    parser = takeLowerText >>= \case
        "destination" -> pure Destination
        e -> fromTextError $ "Failure parsing OwnerOverride from value: '" <> e
           <> "'. Accepted values: destination"

instance ToText OwnerOverride where
    toText = \case
        Destination -> "Destination"

instance Hashable     OwnerOverride
instance NFData       OwnerOverride
instance ToByteString OwnerOverride
instance ToQuery      OwnerOverride
instance ToHeader     OwnerOverride

instance FromXML OwnerOverride where
    parseXML = parseXMLText "OwnerOverride"

instance ToXML OwnerOverride where
    toXML = toXMLText

data Payer
  = BucketOwner
  | Requester
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Payer where
    parser = takeLowerText >>= \case
        "bucketowner" -> pure BucketOwner
        "requester" -> pure Requester
        e -> fromTextError $ "Failure parsing Payer from value: '" <> e
           <> "'. Accepted values: bucketowner, requester"

instance ToText Payer where
    toText = \case
        BucketOwner -> "BucketOwner"
        Requester -> "Requester"

instance Hashable     Payer
instance NFData       Payer
instance ToByteString Payer
instance ToQuery      Payer
instance ToHeader     Payer

instance FromXML Payer where
    parseXML = parseXMLText "Payer"

instance ToXML Payer where
    toXML = toXMLText

data Permission
  = PFullControl
  | PRead
  | PReadAcp
  | PWrite
  | PWriteAcp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Permission where
    parser = takeLowerText >>= \case
        "full_control" -> pure PFullControl
        "read" -> pure PRead
        "read_acp" -> pure PReadAcp
        "write" -> pure PWrite
        "write_acp" -> pure PWriteAcp
        e -> fromTextError $ "Failure parsing Permission from value: '" <> e
           <> "'. Accepted values: full_control, read, read_acp, write, write_acp"

instance ToText Permission where
    toText = \case
        PFullControl -> "FULL_CONTROL"
        PRead -> "READ"
        PReadAcp -> "READ_ACP"
        PWrite -> "WRITE"
        PWriteAcp -> "WRITE_ACP"

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance FromXML Permission where
    parseXML = parseXMLText "Permission"

instance ToXML Permission where
    toXML = toXMLText

data Protocol
  = HTTP
  | HTTPS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

instance Hashable     Protocol
instance NFData       Protocol
instance ToByteString Protocol
instance ToQuery      Protocol
instance ToHeader     Protocol

instance FromXML Protocol where
    parseXML = parseXMLText "Protocol"

instance ToXML Protocol where
    toXML = toXMLText

data QuoteFields
  = ASNeeded
  | Always
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QuoteFields where
    parser = takeLowerText >>= \case
        "asneeded" -> pure ASNeeded
        "always" -> pure Always
        e -> fromTextError $ "Failure parsing QuoteFields from value: '" <> e
           <> "'. Accepted values: asneeded, always"

instance ToText QuoteFields where
    toText = \case
        ASNeeded -> "ASNEEDED"
        Always -> "ALWAYS"

instance Hashable     QuoteFields
instance NFData       QuoteFields
instance ToByteString QuoteFields
instance ToQuery      QuoteFields
instance ToHeader     QuoteFields

instance ToXML QuoteFields where
    toXML = toXMLText

data ReplicationRuleStatus
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationRuleStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing ReplicationRuleStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ReplicationRuleStatus where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     ReplicationRuleStatus
instance NFData       ReplicationRuleStatus
instance ToByteString ReplicationRuleStatus
instance ToQuery      ReplicationRuleStatus
instance ToHeader     ReplicationRuleStatus

instance FromXML ReplicationRuleStatus where
    parseXML = parseXMLText "ReplicationRuleStatus"

instance ToXML ReplicationRuleStatus where
    toXML = toXMLText

data ReplicationStatus
  = Completed
  | Failed
  | Pending
  | Replica
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "pending" -> pure Pending
        "replica" -> pure Replica
        e -> fromTextError $ "Failure parsing ReplicationStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, pending, replica"

instance ToText ReplicationStatus where
    toText = \case
        Completed -> "COMPLETED"
        Failed -> "FAILED"
        Pending -> "PENDING"
        Replica -> "REPLICA"

instance Hashable     ReplicationStatus
instance NFData       ReplicationStatus
instance ToByteString ReplicationStatus
instance ToQuery      ReplicationStatus
instance ToHeader     ReplicationStatus

instance FromXML ReplicationStatus where
    parseXML = parseXMLText "ReplicationStatus"

-- | If present, indicates that the requester was successfully charged for the request.
data RequestCharged =
  RCRequester
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequestCharged where
    parser = takeLowerText >>= \case
        "requester" -> pure RCRequester
        e -> fromTextError $ "Failure parsing RequestCharged from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestCharged where
    toText = \case
        RCRequester -> "requester"

instance Hashable     RequestCharged
instance NFData       RequestCharged
instance ToByteString RequestCharged
instance ToQuery      RequestCharged
instance ToHeader     RequestCharged

instance FromXML RequestCharged where
    parseXML = parseXMLText "RequestCharged"

-- | Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html
data RequestPayer =
  RPRequester
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequestPayer where
    parser = takeLowerText >>= \case
        "requester" -> pure RPRequester
        e -> fromTextError $ "Failure parsing RequestPayer from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestPayer where
    toText = \case
        RPRequester -> "requester"

instance Hashable     RequestPayer
instance NFData       RequestPayer
instance ToByteString RequestPayer
instance ToQuery      RequestPayer
instance ToHeader     RequestPayer

instance ToXML RequestPayer where
    toXML = toXMLText

data RestoreRequestType =
  Select
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RestoreRequestType where
    parser = takeLowerText >>= \case
        "select" -> pure Select
        e -> fromTextError $ "Failure parsing RestoreRequestType from value: '" <> e
           <> "'. Accepted values: select"

instance ToText RestoreRequestType where
    toText = \case
        Select -> "SELECT"

instance Hashable     RestoreRequestType
instance NFData       RestoreRequestType
instance ToByteString RestoreRequestType
instance ToQuery      RestoreRequestType
instance ToHeader     RestoreRequestType

instance ToXML RestoreRequestType where
    toXML = toXMLText

data ServerSideEncryption
  = AES256
  | AWSKMS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerSideEncryption where
    parser = takeLowerText >>= \case
        "aes256" -> pure AES256
        "aws:kms" -> pure AWSKMS
        e -> fromTextError $ "Failure parsing ServerSideEncryption from value: '" <> e
           <> "'. Accepted values: aes256, aws:kms"

instance ToText ServerSideEncryption where
    toText = \case
        AES256 -> "AES256"
        AWSKMS -> "aws:kms"

instance Hashable     ServerSideEncryption
instance NFData       ServerSideEncryption
instance ToByteString ServerSideEncryption
instance ToQuery      ServerSideEncryption
instance ToHeader     ServerSideEncryption

instance FromXML ServerSideEncryption where
    parseXML = parseXMLText "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXML = toXMLText

data SseKMSEncryptedObjectsStatus
  = SKEOSDisabled
  | SKEOSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SseKMSEncryptedObjectsStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure SKEOSDisabled
        "enabled" -> pure SKEOSEnabled
        e -> fromTextError $ "Failure parsing SseKMSEncryptedObjectsStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText SseKMSEncryptedObjectsStatus where
    toText = \case
        SKEOSDisabled -> "Disabled"
        SKEOSEnabled -> "Enabled"

instance Hashable     SseKMSEncryptedObjectsStatus
instance NFData       SseKMSEncryptedObjectsStatus
instance ToByteString SseKMSEncryptedObjectsStatus
instance ToQuery      SseKMSEncryptedObjectsStatus
instance ToHeader     SseKMSEncryptedObjectsStatus

instance FromXML SseKMSEncryptedObjectsStatus where
    parseXML = parseXMLText "SseKMSEncryptedObjectsStatus"

instance ToXML SseKMSEncryptedObjectsStatus where
    toXML = toXMLText

data StorageClass
  = OnezoneIA
  | ReducedRedundancy
  | Standard
  | StandardIA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "onezone_ia" -> pure OnezoneIA
        "reduced_redundancy" -> pure ReducedRedundancy
        "standard" -> pure Standard
        "standard_ia" -> pure StandardIA
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: onezone_ia, reduced_redundancy, standard, standard_ia"

instance ToText StorageClass where
    toText = \case
        OnezoneIA -> "ONEZONE_IA"
        ReducedRedundancy -> "REDUCED_REDUNDANCY"
        Standard -> "STANDARD"
        StandardIA -> "STANDARD_IA"

instance Hashable     StorageClass
instance NFData       StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance FromXML StorageClass where
    parseXML = parseXMLText "StorageClass"

instance ToXML StorageClass where
    toXML = toXMLText

data StorageClassAnalysisSchemaVersion =
  V1
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageClassAnalysisSchemaVersion where
    parser = takeLowerText >>= \case
        "v_1" -> pure V1
        e -> fromTextError $ "Failure parsing StorageClassAnalysisSchemaVersion from value: '" <> e
           <> "'. Accepted values: v_1"

instance ToText StorageClassAnalysisSchemaVersion where
    toText = \case
        V1 -> "V_1"

instance Hashable     StorageClassAnalysisSchemaVersion
instance NFData       StorageClassAnalysisSchemaVersion
instance ToByteString StorageClassAnalysisSchemaVersion
instance ToQuery      StorageClassAnalysisSchemaVersion
instance ToHeader     StorageClassAnalysisSchemaVersion

instance FromXML StorageClassAnalysisSchemaVersion where
    parseXML = parseXMLText "StorageClassAnalysisSchemaVersion"

instance ToXML StorageClassAnalysisSchemaVersion where
    toXML = toXMLText

data TaggingDirective
  = Copy
  | Replace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaggingDirective where
    parser = takeLowerText >>= \case
        "copy" -> pure Copy
        "replace" -> pure Replace
        e -> fromTextError $ "Failure parsing TaggingDirective from value: '" <> e
           <> "'. Accepted values: copy, replace"

instance ToText TaggingDirective where
    toText = \case
        Copy -> "COPY"
        Replace -> "REPLACE"

instance Hashable     TaggingDirective
instance NFData       TaggingDirective
instance ToByteString TaggingDirective
instance ToQuery      TaggingDirective
instance ToHeader     TaggingDirective

instance ToXML TaggingDirective where
    toXML = toXMLText

data Tier
  = TBulk
  | TExpedited
  | TStandard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Tier where
    parser = takeLowerText >>= \case
        "bulk" -> pure TBulk
        "expedited" -> pure TExpedited
        "standard" -> pure TStandard
        e -> fromTextError $ "Failure parsing Tier from value: '" <> e
           <> "'. Accepted values: bulk, expedited, standard"

instance ToText Tier where
    toText = \case
        TBulk -> "Bulk"
        TExpedited -> "Expedited"
        TStandard -> "Standard"

instance Hashable     Tier
instance NFData       Tier
instance ToByteString Tier
instance ToQuery      Tier
instance ToHeader     Tier

instance ToXML Tier where
    toXML = toXMLText

data TransitionStorageClass
  = TSCGlacier
  | TSCOnezoneIA
  | TSCStandardIA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransitionStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure TSCGlacier
        "onezone_ia" -> pure TSCOnezoneIA
        "standard_ia" -> pure TSCStandardIA
        e -> fromTextError $ "Failure parsing TransitionStorageClass from value: '" <> e
           <> "'. Accepted values: glacier, onezone_ia, standard_ia"

instance ToText TransitionStorageClass where
    toText = \case
        TSCGlacier -> "GLACIER"
        TSCOnezoneIA -> "ONEZONE_IA"
        TSCStandardIA -> "STANDARD_IA"

instance Hashable     TransitionStorageClass
instance NFData       TransitionStorageClass
instance ToByteString TransitionStorageClass
instance ToQuery      TransitionStorageClass
instance ToHeader     TransitionStorageClass

instance FromXML TransitionStorageClass where
    parseXML = parseXMLText "TransitionStorageClass"

instance ToXML TransitionStorageClass where
    toXML = toXMLText

data Type
  = AmazonCustomerByEmail
  | CanonicalUser
  | Group
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Type where
    parser = takeLowerText >>= \case
        "amazoncustomerbyemail" -> pure AmazonCustomerByEmail
        "canonicaluser" -> pure CanonicalUser
        "group" -> pure Group
        e -> fromTextError $ "Failure parsing Type from value: '" <> e
           <> "'. Accepted values: amazoncustomerbyemail, canonicaluser, group"

instance ToText Type where
    toText = \case
        AmazonCustomerByEmail -> "AmazonCustomerByEmail"
        CanonicalUser -> "CanonicalUser"
        Group -> "Group"

instance Hashable     Type
instance NFData       Type
instance ToByteString Type
instance ToQuery      Type
instance ToHeader     Type

instance FromXML Type where
    parseXML = parseXMLText "Type"

instance ToXML Type where
    toXML = toXMLText
