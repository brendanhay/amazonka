{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.Types.InventoryOptionalField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InventoryOptionalField
  ( InventoryOptionalField
      ( ..,
        InventoryOptionalField_BucketKeyStatus,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype InventoryOptionalField = InventoryOptionalField'
  { fromInventoryOptionalField ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern InventoryOptionalField_BucketKeyStatus :: InventoryOptionalField
pattern InventoryOptionalField_BucketKeyStatus = InventoryOptionalField' "BucketKeyStatus"

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
  InventoryOptionalField_BucketKeyStatus,
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
