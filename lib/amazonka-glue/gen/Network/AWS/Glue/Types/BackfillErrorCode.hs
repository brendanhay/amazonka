{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BackfillErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillErrorCode where

import Network.AWS.Prelude

data BackfillErrorCode
  = EncryptedPartitionError
  | InternalError
  | InvalidPartitionTypeDataError
  | MissingPartitionValueError
  | UnsupportedPartitionCharacterError
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

instance FromText BackfillErrorCode where
  parser =
    takeLowerText >>= \case
      "encrypted_partition_error" -> pure EncryptedPartitionError
      "internal_error" -> pure InternalError
      "invalid_partition_type_data_error" -> pure InvalidPartitionTypeDataError
      "missing_partition_value_error" -> pure MissingPartitionValueError
      "unsupported_partition_character_error" -> pure UnsupportedPartitionCharacterError
      e ->
        fromTextError $
          "Failure parsing BackfillErrorCode from value: '" <> e
            <> "'. Accepted values: encrypted_partition_error, internal_error, invalid_partition_type_data_error, missing_partition_value_error, unsupported_partition_character_error"

instance ToText BackfillErrorCode where
  toText = \case
    EncryptedPartitionError -> "ENCRYPTED_PARTITION_ERROR"
    InternalError -> "INTERNAL_ERROR"
    InvalidPartitionTypeDataError -> "INVALID_PARTITION_TYPE_DATA_ERROR"
    MissingPartitionValueError -> "MISSING_PARTITION_VALUE_ERROR"
    UnsupportedPartitionCharacterError -> "UNSUPPORTED_PARTITION_CHARACTER_ERROR"

instance Hashable BackfillErrorCode

instance NFData BackfillErrorCode

instance ToByteString BackfillErrorCode

instance ToQuery BackfillErrorCode

instance ToHeader BackfillErrorCode

instance FromJSON BackfillErrorCode where
  parseJSON = parseJSONText "BackfillErrorCode"
