{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EncryptionModeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncryptionModeValue where

import Network.AWS.Prelude

data EncryptionModeValue
  = SseKMS
  | SseS3
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

instance FromText EncryptionModeValue where
  parser =
    takeLowerText >>= \case
      "sse-kms" -> pure SseKMS
      "sse-s3" -> pure SseS3
      e ->
        fromTextError $
          "Failure parsing EncryptionModeValue from value: '" <> e
            <> "'. Accepted values: sse-kms, sse-s3"

instance ToText EncryptionModeValue where
  toText = \case
    SseKMS -> "sse-kms"
    SseS3 -> "sse-s3"

instance Hashable EncryptionModeValue

instance NFData EncryptionModeValue

instance ToByteString EncryptionModeValue

instance ToQuery EncryptionModeValue

instance ToHeader EncryptionModeValue

instance ToJSON EncryptionModeValue where
  toJSON = toJSONText

instance FromJSON EncryptionModeValue where
  parseJSON = parseJSONText "EncryptionModeValue"
