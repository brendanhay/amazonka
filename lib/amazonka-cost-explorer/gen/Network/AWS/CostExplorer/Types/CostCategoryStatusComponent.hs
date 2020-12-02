{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryStatusComponent where

import Network.AWS.Prelude

data CostCategoryStatusComponent = CostExplorer
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

instance FromText CostCategoryStatusComponent where
  parser =
    takeLowerText >>= \case
      "cost_explorer" -> pure CostExplorer
      e ->
        fromTextError $
          "Failure parsing CostCategoryStatusComponent from value: '" <> e
            <> "'. Accepted values: cost_explorer"

instance ToText CostCategoryStatusComponent where
  toText = \case
    CostExplorer -> "COST_EXPLORER"

instance Hashable CostCategoryStatusComponent

instance NFData CostCategoryStatusComponent

instance ToByteString CostCategoryStatusComponent

instance ToQuery CostCategoryStatusComponent

instance ToHeader CostCategoryStatusComponent

instance FromJSON CostCategoryStatusComponent where
  parseJSON = parseJSONText "CostCategoryStatusComponent"
