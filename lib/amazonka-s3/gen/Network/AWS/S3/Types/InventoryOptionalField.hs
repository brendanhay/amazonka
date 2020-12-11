-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryOptionalField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryOptionalField
  ( InventoryOptionalField
      ( InventoryOptionalField',
        FieldETag,
        FieldEncryptionStatus,
        FieldIntelligentTieringAccessTier,
        FieldIsMultipartUploaded,
        FieldLastModifiedDate,
        FieldObjectLockLegalHoldStatus,
        FieldObjectLockMode,
        FieldObjectLockRetainUntilDate,
        FieldReplicationStatus,
        FieldSize,
        FieldStorageClass
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype InventoryOptionalField = InventoryOptionalField' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern FieldETag :: InventoryOptionalField
pattern FieldETag = InventoryOptionalField' "ETag"

pattern FieldEncryptionStatus :: InventoryOptionalField
pattern FieldEncryptionStatus = InventoryOptionalField' "EncryptionStatus"

pattern FieldIntelligentTieringAccessTier :: InventoryOptionalField
pattern FieldIntelligentTieringAccessTier = InventoryOptionalField' "IntelligentTieringAccessTier"

pattern FieldIsMultipartUploaded :: InventoryOptionalField
pattern FieldIsMultipartUploaded = InventoryOptionalField' "IsMultipartUploaded"

pattern FieldLastModifiedDate :: InventoryOptionalField
pattern FieldLastModifiedDate = InventoryOptionalField' "LastModifiedDate"

pattern FieldObjectLockLegalHoldStatus :: InventoryOptionalField
pattern FieldObjectLockLegalHoldStatus = InventoryOptionalField' "ObjectLockLegalHoldStatus"

pattern FieldObjectLockMode :: InventoryOptionalField
pattern FieldObjectLockMode = InventoryOptionalField' "ObjectLockMode"

pattern FieldObjectLockRetainUntilDate :: InventoryOptionalField
pattern FieldObjectLockRetainUntilDate = InventoryOptionalField' "ObjectLockRetainUntilDate"

pattern FieldReplicationStatus :: InventoryOptionalField
pattern FieldReplicationStatus = InventoryOptionalField' "ReplicationStatus"

pattern FieldSize :: InventoryOptionalField
pattern FieldSize = InventoryOptionalField' "Size"

pattern FieldStorageClass :: InventoryOptionalField
pattern FieldStorageClass = InventoryOptionalField' "StorageClass"

{-# COMPLETE
  FieldETag,
  FieldEncryptionStatus,
  FieldIntelligentTieringAccessTier,
  FieldIsMultipartUploaded,
  FieldLastModifiedDate,
  FieldObjectLockLegalHoldStatus,
  FieldObjectLockMode,
  FieldObjectLockRetainUntilDate,
  FieldReplicationStatus,
  FieldSize,
  FieldStorageClass,
  InventoryOptionalField'
  #-}
