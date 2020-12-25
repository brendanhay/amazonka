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
        BackfillErrorCodeEncryptedPartitionError,
        BackfillErrorCodeInternalError,
        BackfillErrorCodeInvalidPartitionTypeDataError,
        BackfillErrorCodeMissingPartitionValueError,
        BackfillErrorCodeUnsupportedPartitionCharacterError,
        fromBackfillErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BackfillErrorCode = BackfillErrorCode'
  { fromBackfillErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BackfillErrorCodeEncryptedPartitionError :: BackfillErrorCode
pattern BackfillErrorCodeEncryptedPartitionError = BackfillErrorCode' "ENCRYPTED_PARTITION_ERROR"

pattern BackfillErrorCodeInternalError :: BackfillErrorCode
pattern BackfillErrorCodeInternalError = BackfillErrorCode' "INTERNAL_ERROR"

pattern BackfillErrorCodeInvalidPartitionTypeDataError :: BackfillErrorCode
pattern BackfillErrorCodeInvalidPartitionTypeDataError = BackfillErrorCode' "INVALID_PARTITION_TYPE_DATA_ERROR"

pattern BackfillErrorCodeMissingPartitionValueError :: BackfillErrorCode
pattern BackfillErrorCodeMissingPartitionValueError = BackfillErrorCode' "MISSING_PARTITION_VALUE_ERROR"

pattern BackfillErrorCodeUnsupportedPartitionCharacterError :: BackfillErrorCode
pattern BackfillErrorCodeUnsupportedPartitionCharacterError = BackfillErrorCode' "UNSUPPORTED_PARTITION_CHARACTER_ERROR"

{-# COMPLETE
  BackfillErrorCodeEncryptedPartitionError,
  BackfillErrorCodeInternalError,
  BackfillErrorCodeInvalidPartitionTypeDataError,
  BackfillErrorCodeMissingPartitionValueError,
  BackfillErrorCodeUnsupportedPartitionCharacterError,
  BackfillErrorCode'
  #-}
