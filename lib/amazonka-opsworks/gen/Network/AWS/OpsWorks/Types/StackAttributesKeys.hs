{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackAttributesKeys where

import Network.AWS.Prelude

data StackAttributesKeys = Color
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

instance FromText StackAttributesKeys where
  parser =
    takeLowerText >>= \case
      "color" -> pure Color
      e ->
        fromTextError $
          "Failure parsing StackAttributesKeys from value: '" <> e
            <> "'. Accepted values: color"

instance ToText StackAttributesKeys where
  toText = \case
    Color -> "Color"

instance Hashable StackAttributesKeys

instance NFData StackAttributesKeys

instance ToByteString StackAttributesKeys

instance ToQuery StackAttributesKeys

instance ToHeader StackAttributesKeys

instance ToJSON StackAttributesKeys where
  toJSON = toJSONText

instance FromJSON StackAttributesKeys where
  parseJSON = parseJSONText "StackAttributesKeys"
