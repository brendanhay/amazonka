{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.QLDB.Types.Sum where

import Network.AWS.Prelude

data ExportStatus
  = Cancelled
  | Completed
  | InProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "completed" -> pure Completed
        "in_progress" -> pure InProgress
        e -> fromTextError $ "Failure parsing ExportStatus from value: '" <> e
           <> "'. Accepted values: cancelled, completed, in_progress"

instance ToText ExportStatus where
    toText = \case
        Cancelled -> "CANCELLED"
        Completed -> "COMPLETED"
        InProgress -> "IN_PROGRESS"

instance Hashable     ExportStatus
instance NFData       ExportStatus
instance ToByteString ExportStatus
instance ToQuery      ExportStatus
instance ToHeader     ExportStatus

instance FromJSON ExportStatus where
    parseJSON = parseJSONText "ExportStatus"

data LedgerState
  = Active
  | Creating
  | Deleted
  | Deleting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LedgerState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing LedgerState from value: '" <> e
           <> "'. Accepted values: active, creating, deleted, deleting"

instance ToText LedgerState where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleted -> "DELETED"
        Deleting -> "DELETING"

instance Hashable     LedgerState
instance NFData       LedgerState
instance ToByteString LedgerState
instance ToQuery      LedgerState
instance ToHeader     LedgerState

instance FromJSON LedgerState where
    parseJSON = parseJSONText "LedgerState"

data PermissionsMode =
  AllowAll
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PermissionsMode where
    parser = takeLowerText >>= \case
        "allow_all" -> pure AllowAll
        e -> fromTextError $ "Failure parsing PermissionsMode from value: '" <> e
           <> "'. Accepted values: allow_all"

instance ToText PermissionsMode where
    toText = \case
        AllowAll -> "ALLOW_ALL"

instance Hashable     PermissionsMode
instance NFData       PermissionsMode
instance ToByteString PermissionsMode
instance ToQuery      PermissionsMode
instance ToHeader     PermissionsMode

instance ToJSON PermissionsMode where
    toJSON = toJSONText

data S3ObjectEncryptionType
  = NoEncryption
  | SseKMS
  | SseS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3ObjectEncryptionType where
    parser = takeLowerText >>= \case
        "no_encryption" -> pure NoEncryption
        "sse_kms" -> pure SseKMS
        "sse_s3" -> pure SseS3
        e -> fromTextError $ "Failure parsing S3ObjectEncryptionType from value: '" <> e
           <> "'. Accepted values: no_encryption, sse_kms, sse_s3"

instance ToText S3ObjectEncryptionType where
    toText = \case
        NoEncryption -> "NO_ENCRYPTION"
        SseKMS -> "SSE_KMS"
        SseS3 -> "SSE_S3"

instance Hashable     S3ObjectEncryptionType
instance NFData       S3ObjectEncryptionType
instance ToByteString S3ObjectEncryptionType
instance ToQuery      S3ObjectEncryptionType
instance ToHeader     S3ObjectEncryptionType

instance ToJSON S3ObjectEncryptionType where
    toJSON = toJSONText

instance FromJSON S3ObjectEncryptionType where
    parseJSON = parseJSONText "S3ObjectEncryptionType"
