{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Sum where

import Network.AWS.Prelude

data ActionCode
  = ArchiveRetrieval
  | InventoryRetrieval
  | Select
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionCode where
    parser = takeLowerText >>= \case
        "archiveretrieval" -> pure ArchiveRetrieval
        "inventoryretrieval" -> pure InventoryRetrieval
        "select" -> pure Select
        e -> fromTextError $ "Failure parsing ActionCode from value: '" <> e
           <> "'. Accepted values: archiveretrieval, inventoryretrieval, select"

instance ToText ActionCode where
    toText = \case
        ArchiveRetrieval -> "ArchiveRetrieval"
        InventoryRetrieval -> "InventoryRetrieval"
        Select -> "Select"

instance Hashable     ActionCode
instance NFData       ActionCode
instance ToByteString ActionCode
instance ToQuery      ActionCode
instance ToHeader     ActionCode

instance FromJSON ActionCode where
    parseJSON = parseJSONText "ActionCode"

data CannedACL
  = AWSExecRead
  | AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
  | Private
  | PublicRead
  | PublicReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CannedACL where
    parser = takeLowerText >>= \case
        "aws-exec-read" -> pure AWSExecRead
        "authenticated-read" -> pure AuthenticatedRead
        "bucket-owner-full-control" -> pure BucketOwnerFullControl
        "bucket-owner-read" -> pure BucketOwnerRead
        "private" -> pure Private
        "public-read" -> pure PublicRead
        "public-read-write" -> pure PublicReadWrite
        e -> fromTextError $ "Failure parsing CannedACL from value: '" <> e
           <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText CannedACL where
    toText = \case
        AWSExecRead -> "aws-exec-read"
        AuthenticatedRead -> "authenticated-read"
        BucketOwnerFullControl -> "bucket-owner-full-control"
        BucketOwnerRead -> "bucket-owner-read"
        Private -> "private"
        PublicRead -> "public-read"
        PublicReadWrite -> "public-read-write"

instance Hashable     CannedACL
instance NFData       CannedACL
instance ToByteString CannedACL
instance ToQuery      CannedACL
instance ToHeader     CannedACL

instance ToJSON CannedACL where
    toJSON = toJSONText

instance FromJSON CannedACL where
    parseJSON = parseJSONText "CannedACL"

data EncryptionType
  = AES256
  | AWSKMS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionType where
    parser = takeLowerText >>= \case
        "aes256" -> pure AES256
        "aws:kms" -> pure AWSKMS
        e -> fromTextError $ "Failure parsing EncryptionType from value: '" <> e
           <> "'. Accepted values: aes256, aws:kms"

instance ToText EncryptionType where
    toText = \case
        AES256 -> "AES256"
        AWSKMS -> "aws:kms"

instance Hashable     EncryptionType
instance NFData       EncryptionType
instance ToByteString EncryptionType
instance ToQuery      EncryptionType
instance ToHeader     EncryptionType

instance ToJSON EncryptionType where
    toJSON = toJSONText

instance FromJSON EncryptionType where
    parseJSON = parseJSONText "EncryptionType"

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

instance ToJSON ExpressionType where
    toJSON = toJSONText

instance FromJSON ExpressionType where
    parseJSON = parseJSONText "ExpressionType"

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

instance ToJSON FileHeaderInfo where
    toJSON = toJSONText

instance FromJSON FileHeaderInfo where
    parseJSON = parseJSONText "FileHeaderInfo"

data Permission
  = FullControl
  | Read
  | ReadAcp
  | Write
  | WriteAcp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Permission where
    parser = takeLowerText >>= \case
        "full_control" -> pure FullControl
        "read" -> pure Read
        "read_acp" -> pure ReadAcp
        "write" -> pure Write
        "write_acp" -> pure WriteAcp
        e -> fromTextError $ "Failure parsing Permission from value: '" <> e
           <> "'. Accepted values: full_control, read, read_acp, write, write_acp"

instance ToText Permission where
    toText = \case
        FullControl -> "FULL_CONTROL"
        Read -> "READ"
        ReadAcp -> "READ_ACP"
        Write -> "WRITE"
        WriteAcp -> "WRITE_ACP"

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance ToJSON Permission where
    toJSON = toJSONText

instance FromJSON Permission where
    parseJSON = parseJSONText "Permission"

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

instance ToJSON QuoteFields where
    toJSON = toJSONText

instance FromJSON QuoteFields where
    parseJSON = parseJSONText "QuoteFields"

data StatusCode
  = Failed
  | InProgress
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusCode where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing StatusCode from value: '" <> e
           <> "'. Accepted values: failed, inprogress, succeeded"

instance ToText StatusCode where
    toText = \case
        Failed -> "Failed"
        InProgress -> "InProgress"
        Succeeded -> "Succeeded"

instance Hashable     StatusCode
instance NFData       StatusCode
instance ToByteString StatusCode
instance ToQuery      StatusCode
instance ToHeader     StatusCode

instance FromJSON StatusCode where
    parseJSON = parseJSONText "StatusCode"

data StorageClass
  = ReducedRedundancy
  | Standard
  | StandardIA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "reduced_redundancy" -> pure ReducedRedundancy
        "standard" -> pure Standard
        "standard_ia" -> pure StandardIA
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: reduced_redundancy, standard, standard_ia"

instance ToText StorageClass where
    toText = \case
        ReducedRedundancy -> "REDUCED_REDUNDANCY"
        Standard -> "STANDARD"
        StandardIA -> "STANDARD_IA"

instance Hashable     StorageClass
instance NFData       StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance ToJSON StorageClass where
    toJSON = toJSONText

instance FromJSON StorageClass where
    parseJSON = parseJSONText "StorageClass"

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

instance ToJSON Type where
    toJSON = toJSONText

instance FromJSON Type where
    parseJSON = parseJSONText "Type"
