{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.S3SseAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.S3SseAlgorithm where

import Network.AWS.Prelude

data S3SseAlgorithm
  = SSAAES256
  | SSAKMS
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

instance FromText S3SseAlgorithm where
  parser =
    takeLowerText >>= \case
      "aes256" -> pure SSAAES256
      "kms" -> pure SSAKMS
      e ->
        fromTextError $
          "Failure parsing S3SseAlgorithm from value: '" <> e
            <> "'. Accepted values: aes256, kms"

instance ToText S3SseAlgorithm where
  toText = \case
    SSAAES256 -> "AES256"
    SSAKMS -> "KMS"

instance Hashable S3SseAlgorithm

instance NFData S3SseAlgorithm

instance ToByteString S3SseAlgorithm

instance ToQuery S3SseAlgorithm

instance ToHeader S3SseAlgorithm

instance ToJSON S3SseAlgorithm where
  toJSON = toJSONText

instance FromJSON S3SseAlgorithm where
  parseJSON = parseJSONText "S3SseAlgorithm"
