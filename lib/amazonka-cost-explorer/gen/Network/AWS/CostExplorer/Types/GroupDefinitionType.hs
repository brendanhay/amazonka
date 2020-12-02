{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinitionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.GroupDefinitionType where

import Network.AWS.Prelude

data GroupDefinitionType
  = CostCategory
  | Dimension
  | Tag
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

instance FromText GroupDefinitionType where
  parser =
    takeLowerText >>= \case
      "cost_category" -> pure CostCategory
      "dimension" -> pure Dimension
      "tag" -> pure Tag
      e ->
        fromTextError $
          "Failure parsing GroupDefinitionType from value: '" <> e
            <> "'. Accepted values: cost_category, dimension, tag"

instance ToText GroupDefinitionType where
  toText = \case
    CostCategory -> "COST_CATEGORY"
    Dimension -> "DIMENSION"
    Tag -> "TAG"

instance Hashable GroupDefinitionType

instance NFData GroupDefinitionType

instance ToByteString GroupDefinitionType

instance ToQuery GroupDefinitionType

instance ToHeader GroupDefinitionType

instance ToJSON GroupDefinitionType where
  toJSON = toJSONText

instance FromJSON GroupDefinitionType where
  parseJSON = parseJSONText "GroupDefinitionType"
