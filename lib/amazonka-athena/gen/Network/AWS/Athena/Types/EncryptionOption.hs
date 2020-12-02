{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionOption where

import Network.AWS.Prelude

data EncryptionOption
  = CseKMS
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

instance FromText EncryptionOption where
  parser =
    takeLowerText >>= \case
      "cse_kms" -> pure CseKMS
      "sse_kms" -> pure SseKMS
      "sse_s3" -> pure SseS3
      e ->
        fromTextError $
          "Failure parsing EncryptionOption from value: '" <> e
            <> "'. Accepted values: cse_kms, sse_kms, sse_s3"

instance ToText EncryptionOption where
  toText = \case
    CseKMS -> "CSE_KMS"
    SseKMS -> "SSE_KMS"
    SseS3 -> "SSE_S3"

instance Hashable EncryptionOption

instance NFData EncryptionOption

instance ToByteString EncryptionOption

instance ToQuery EncryptionOption

instance ToHeader EncryptionOption

instance ToJSON EncryptionOption where
  toJSON = toJSONText

instance FromJSON EncryptionOption where
  parseJSON = parseJSONText "EncryptionOption"
