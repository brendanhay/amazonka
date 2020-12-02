{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryRuleVersion where

import Network.AWS.Prelude

-- | The rule schema version in this particular Cost Category.
data CostCategoryRuleVersion = CostCategoryExpression_V1
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

instance FromText CostCategoryRuleVersion where
  parser =
    takeLowerText >>= \case
      "costcategoryexpression.v1" -> pure CostCategoryExpression_V1
      e ->
        fromTextError $
          "Failure parsing CostCategoryRuleVersion from value: '" <> e
            <> "'. Accepted values: costcategoryexpression.v1"

instance ToText CostCategoryRuleVersion where
  toText = \case
    CostCategoryExpression_V1 -> "CostCategoryExpression.v1"

instance Hashable CostCategoryRuleVersion

instance NFData CostCategoryRuleVersion

instance ToByteString CostCategoryRuleVersion

instance ToQuery CostCategoryRuleVersion

instance ToHeader CostCategoryRuleVersion

instance ToJSON CostCategoryRuleVersion where
  toJSON = toJSONText

instance FromJSON CostCategoryRuleVersion where
  parseJSON = parseJSONText "CostCategoryRuleVersion"
