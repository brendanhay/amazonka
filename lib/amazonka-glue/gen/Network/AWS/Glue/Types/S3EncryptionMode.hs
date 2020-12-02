{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3EncryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3EncryptionMode where

import Network.AWS.Prelude

data S3EncryptionMode
  = Disabled
  | SseKMS
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

instance FromText S3EncryptionMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "sse-kms" -> pure SseKMS
      "sse-s3" -> pure SseS3
      e ->
        fromTextError $
          "Failure parsing S3EncryptionMode from value: '" <> e
            <> "'. Accepted values: disabled, sse-kms, sse-s3"

instance ToText S3EncryptionMode where
  toText = \case
    Disabled -> "DISABLED"
    SseKMS -> "SSE-KMS"
    SseS3 -> "SSE-S3"

instance Hashable S3EncryptionMode

instance NFData S3EncryptionMode

instance ToByteString S3EncryptionMode

instance ToQuery S3EncryptionMode

instance ToHeader S3EncryptionMode

instance ToJSON S3EncryptionMode where
  toJSON = toJSONText

instance FromJSON S3EncryptionMode where
  parseJSON = parseJSONText "S3EncryptionMode"
