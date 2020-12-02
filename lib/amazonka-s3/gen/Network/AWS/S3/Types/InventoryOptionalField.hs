{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryOptionalField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryOptionalField where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryOptionalField
  = FieldETag
  | FieldEncryptionStatus
  | FieldIntelligentTieringAccessTier
  | FieldIsMultipartUploaded
  | FieldLastModifiedDate
  | FieldObjectLockLegalHoldStatus
  | FieldObjectLockMode
  | FieldObjectLockRetainUntilDate
  | FieldReplicationStatus
  | FieldSize
  | FieldStorageClass
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText InventoryOptionalField where
  parser =
    takeLowerText >>= \case
      "etag" -> pure FieldETag
      "encryptionstatus" -> pure FieldEncryptionStatus
      "intelligenttieringaccesstier" -> pure FieldIntelligentTieringAccessTier
      "ismultipartuploaded" -> pure FieldIsMultipartUploaded
      "lastmodifieddate" -> pure FieldLastModifiedDate
      "objectlocklegalholdstatus" -> pure FieldObjectLockLegalHoldStatus
      "objectlockmode" -> pure FieldObjectLockMode
      "objectlockretainuntildate" -> pure FieldObjectLockRetainUntilDate
      "replicationstatus" -> pure FieldReplicationStatus
      "size" -> pure FieldSize
      "storageclass" -> pure FieldStorageClass
      e ->
        fromTextError $
          "Failure parsing InventoryOptionalField from value: '" <> e
            <> "'. Accepted values: etag, encryptionstatus, intelligenttieringaccesstier, ismultipartuploaded, lastmodifieddate, objectlocklegalholdstatus, objectlockmode, objectlockretainuntildate, replicationstatus, size, storageclass"

instance ToText InventoryOptionalField where
  toText = \case
    FieldETag -> "ETag"
    FieldEncryptionStatus -> "EncryptionStatus"
    FieldIntelligentTieringAccessTier -> "IntelligentTieringAccessTier"
    FieldIsMultipartUploaded -> "IsMultipartUploaded"
    FieldLastModifiedDate -> "LastModifiedDate"
    FieldObjectLockLegalHoldStatus -> "ObjectLockLegalHoldStatus"
    FieldObjectLockMode -> "ObjectLockMode"
    FieldObjectLockRetainUntilDate -> "ObjectLockRetainUntilDate"
    FieldReplicationStatus -> "ReplicationStatus"
    FieldSize -> "Size"
    FieldStorageClass -> "StorageClass"

instance Hashable InventoryOptionalField

instance NFData InventoryOptionalField

instance ToByteString InventoryOptionalField

instance ToQuery InventoryOptionalField

instance ToHeader InventoryOptionalField

instance FromXML InventoryOptionalField where
  parseXML = parseXMLText "InventoryOptionalField"

instance ToXML InventoryOptionalField where
  toXML = toXMLText
