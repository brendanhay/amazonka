{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BackfillErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillErrorCode
  ( BackfillErrorCode
      ( BackfillErrorCode',
        EncryptedPartitionError,
        InternalError,
        InvalidPartitionTypeDataError,
        MissingPartitionValueError,
        UnsupportedPartitionCharacterError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BackfillErrorCode = BackfillErrorCode' Lude.Text
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

pattern EncryptedPartitionError :: BackfillErrorCode
pattern EncryptedPartitionError = BackfillErrorCode' "ENCRYPTED_PARTITION_ERROR"

pattern InternalError :: BackfillErrorCode
pattern InternalError = BackfillErrorCode' "INTERNAL_ERROR"

pattern InvalidPartitionTypeDataError :: BackfillErrorCode
pattern InvalidPartitionTypeDataError = BackfillErrorCode' "INVALID_PARTITION_TYPE_DATA_ERROR"

pattern MissingPartitionValueError :: BackfillErrorCode
pattern MissingPartitionValueError = BackfillErrorCode' "MISSING_PARTITION_VALUE_ERROR"

pattern UnsupportedPartitionCharacterError :: BackfillErrorCode
pattern UnsupportedPartitionCharacterError = BackfillErrorCode' "UNSUPPORTED_PARTITION_CHARACTER_ERROR"

{-# COMPLETE
  EncryptedPartitionError,
  InternalError,
  InvalidPartitionTypeDataError,
  MissingPartitionValueError,
  UnsupportedPartitionCharacterError,
  BackfillErrorCode'
  #-}
