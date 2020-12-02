{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventorySchemaDeleteOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventorySchemaDeleteOption where

import Network.AWS.Prelude

data InventorySchemaDeleteOption
  = DeleteSchema
  | DisableSchema
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

instance FromText InventorySchemaDeleteOption where
  parser =
    takeLowerText >>= \case
      "deleteschema" -> pure DeleteSchema
      "disableschema" -> pure DisableSchema
      e ->
        fromTextError $
          "Failure parsing InventorySchemaDeleteOption from value: '" <> e
            <> "'. Accepted values: deleteschema, disableschema"

instance ToText InventorySchemaDeleteOption where
  toText = \case
    DeleteSchema -> "DeleteSchema"
    DisableSchema -> "DisableSchema"

instance Hashable InventorySchemaDeleteOption

instance NFData InventorySchemaDeleteOption

instance ToByteString InventorySchemaDeleteOption

instance ToQuery InventorySchemaDeleteOption

instance ToHeader InventorySchemaDeleteOption

instance ToJSON InventorySchemaDeleteOption where
  toJSON = toJSONText
