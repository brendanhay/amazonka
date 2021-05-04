{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryOptionalField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryOptionalField
  ( InventoryOptionalField
      ( ..,
        InventoryOptionalField_ETag,
        InventoryOptionalField_EncryptionStatus,
        InventoryOptionalField_IntelligentTieringAccessTier,
        InventoryOptionalField_IsMultipartUploaded,
        InventoryOptionalField_LastModifiedDate,
        InventoryOptionalField_ObjectLockLegalHoldStatus,
        InventoryOptionalField_ObjectLockMode,
        InventoryOptionalField_ObjectLockRetainUntilDate,
        InventoryOptionalField_ReplicationStatus,
        InventoryOptionalField_Size,
        InventoryOptionalField_StorageClass
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype InventoryOptionalField = InventoryOptionalField'
  { fromInventoryOptionalField ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern InventoryOptionalField_ETag :: InventoryOptionalField
pattern InventoryOptionalField_ETag = InventoryOptionalField' "ETag"

pattern InventoryOptionalField_EncryptionStatus :: InventoryOptionalField
pattern InventoryOptionalField_EncryptionStatus = InventoryOptionalField' "EncryptionStatus"

pattern InventoryOptionalField_IntelligentTieringAccessTier :: InventoryOptionalField
pattern InventoryOptionalField_IntelligentTieringAccessTier = InventoryOptionalField' "IntelligentTieringAccessTier"

pattern InventoryOptionalField_IsMultipartUploaded :: InventoryOptionalField
pattern InventoryOptionalField_IsMultipartUploaded = InventoryOptionalField' "IsMultipartUploaded"

pattern InventoryOptionalField_LastModifiedDate :: InventoryOptionalField
pattern InventoryOptionalField_LastModifiedDate = InventoryOptionalField' "LastModifiedDate"

pattern InventoryOptionalField_ObjectLockLegalHoldStatus :: InventoryOptionalField
pattern InventoryOptionalField_ObjectLockLegalHoldStatus = InventoryOptionalField' "ObjectLockLegalHoldStatus"

pattern InventoryOptionalField_ObjectLockMode :: InventoryOptionalField
pattern InventoryOptionalField_ObjectLockMode = InventoryOptionalField' "ObjectLockMode"

pattern InventoryOptionalField_ObjectLockRetainUntilDate :: InventoryOptionalField
pattern InventoryOptionalField_ObjectLockRetainUntilDate = InventoryOptionalField' "ObjectLockRetainUntilDate"

pattern InventoryOptionalField_ReplicationStatus :: InventoryOptionalField
pattern InventoryOptionalField_ReplicationStatus = InventoryOptionalField' "ReplicationStatus"

pattern InventoryOptionalField_Size :: InventoryOptionalField
pattern InventoryOptionalField_Size = InventoryOptionalField' "Size"

pattern InventoryOptionalField_StorageClass :: InventoryOptionalField
pattern InventoryOptionalField_StorageClass = InventoryOptionalField' "StorageClass"

{-# COMPLETE
  InventoryOptionalField_ETag,
  InventoryOptionalField_EncryptionStatus,
  InventoryOptionalField_IntelligentTieringAccessTier,
  InventoryOptionalField_IsMultipartUploaded,
  InventoryOptionalField_LastModifiedDate,
  InventoryOptionalField_ObjectLockLegalHoldStatus,
  InventoryOptionalField_ObjectLockMode,
  InventoryOptionalField_ObjectLockRetainUntilDate,
  InventoryOptionalField_ReplicationStatus,
  InventoryOptionalField_Size,
  InventoryOptionalField_StorageClass,
  InventoryOptionalField'
  #-}
