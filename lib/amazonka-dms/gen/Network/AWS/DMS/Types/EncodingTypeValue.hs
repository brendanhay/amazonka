{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EncodingTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncodingTypeValue where

import Network.AWS.Prelude

data EncodingTypeValue
  = Plain
  | PlainDictionary
  | RleDictionary
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

instance FromText EncodingTypeValue where
  parser =
    takeLowerText >>= \case
      "plain" -> pure Plain
      "plain-dictionary" -> pure PlainDictionary
      "rle-dictionary" -> pure RleDictionary
      e ->
        fromTextError $
          "Failure parsing EncodingTypeValue from value: '" <> e
            <> "'. Accepted values: plain, plain-dictionary, rle-dictionary"

instance ToText EncodingTypeValue where
  toText = \case
    Plain -> "plain"
    PlainDictionary -> "plain-dictionary"
    RleDictionary -> "rle-dictionary"

instance Hashable EncodingTypeValue

instance NFData EncodingTypeValue

instance ToByteString EncodingTypeValue

instance ToQuery EncodingTypeValue

instance ToHeader EncodingTypeValue

instance ToJSON EncodingTypeValue where
  toJSON = toJSONText

instance FromJSON EncodingTypeValue where
  parseJSON = parseJSONText "EncodingTypeValue"
