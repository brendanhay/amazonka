{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.EncodingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.EncodingType where

import Network.AWS.Prelude

data EncodingType
  = Binary
  | JSON
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

instance FromText EncodingType where
  parser =
    takeLowerText >>= \case
      "binary" -> pure Binary
      "json" -> pure JSON
      e ->
        fromTextError $
          "Failure parsing EncodingType from value: '" <> e
            <> "'. Accepted values: binary, json"

instance ToText EncodingType where
  toText = \case
    Binary -> "binary"
    JSON -> "json"

instance Hashable EncodingType

instance NFData EncodingType

instance ToByteString EncodingType

instance ToQuery EncodingType

instance ToHeader EncodingType

instance ToJSON EncodingType where
  toJSON = toJSONText

instance FromJSON EncodingType where
  parseJSON = parseJSONText "EncodingType"
