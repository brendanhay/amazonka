{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryAttributeDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryAttributeDataType where

import Network.AWS.Prelude

data InventoryAttributeDataType
  = IADTNumber
  | IADTString
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

instance FromText InventoryAttributeDataType where
  parser =
    takeLowerText >>= \case
      "number" -> pure IADTNumber
      "string" -> pure IADTString
      e ->
        fromTextError $
          "Failure parsing InventoryAttributeDataType from value: '" <> e
            <> "'. Accepted values: number, string"

instance ToText InventoryAttributeDataType where
  toText = \case
    IADTNumber -> "number"
    IADTString -> "string"

instance Hashable InventoryAttributeDataType

instance NFData InventoryAttributeDataType

instance ToByteString InventoryAttributeDataType

instance ToQuery InventoryAttributeDataType

instance ToHeader InventoryAttributeDataType

instance FromJSON InventoryAttributeDataType where
  parseJSON = parseJSONText "InventoryAttributeDataType"
