{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoKeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamoKeyType where

import Network.AWS.Prelude

data DynamoKeyType
  = DKTNumber
  | DKTString
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

instance FromText DynamoKeyType where
  parser =
    takeLowerText >>= \case
      "number" -> pure DKTNumber
      "string" -> pure DKTString
      e ->
        fromTextError $
          "Failure parsing DynamoKeyType from value: '" <> e
            <> "'. Accepted values: number, string"

instance ToText DynamoKeyType where
  toText = \case
    DKTNumber -> "NUMBER"
    DKTString -> "STRING"

instance Hashable DynamoKeyType

instance NFData DynamoKeyType

instance ToByteString DynamoKeyType

instance ToQuery DynamoKeyType

instance ToHeader DynamoKeyType

instance ToJSON DynamoKeyType where
  toJSON = toJSONText

instance FromJSON DynamoKeyType where
  parseJSON = parseJSONText "DynamoKeyType"
