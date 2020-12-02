{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEType where

import Network.AWS.Prelude

data SSEType
  = AES256
  | KMS
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

instance FromText SSEType where
  parser =
    takeLowerText >>= \case
      "aes256" -> pure AES256
      "kms" -> pure KMS
      e ->
        fromTextError $
          "Failure parsing SSEType from value: '" <> e
            <> "'. Accepted values: aes256, kms"

instance ToText SSEType where
  toText = \case
    AES256 -> "AES256"
    KMS -> "KMS"

instance Hashable SSEType

instance NFData SSEType

instance ToByteString SSEType

instance ToQuery SSEType

instance ToHeader SSEType

instance ToJSON SSEType where
  toJSON = toJSONText

instance FromJSON SSEType where
  parseJSON = parseJSONText "SSEType"
