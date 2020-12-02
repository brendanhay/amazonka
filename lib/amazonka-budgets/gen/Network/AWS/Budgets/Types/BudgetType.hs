{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetType where

import Network.AWS.Prelude

-- | The type of a budget. It must be one of the following types:
--
--
-- @COST@ , @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , or @SAVINGS_PLANS_COVERAGE@ .
data BudgetType
  = Cost
  | RiCoverage
  | RiUtilization
  | SavingsPlansCoverage
  | SavingsPlansUtilization
  | Usage
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

instance FromText BudgetType where
  parser =
    takeLowerText >>= \case
      "cost" -> pure Cost
      "ri_coverage" -> pure RiCoverage
      "ri_utilization" -> pure RiUtilization
      "savings_plans_coverage" -> pure SavingsPlansCoverage
      "savings_plans_utilization" -> pure SavingsPlansUtilization
      "usage" -> pure Usage
      e ->
        fromTextError $
          "Failure parsing BudgetType from value: '" <> e
            <> "'. Accepted values: cost, ri_coverage, ri_utilization, savings_plans_coverage, savings_plans_utilization, usage"

instance ToText BudgetType where
  toText = \case
    Cost -> "COST"
    RiCoverage -> "RI_COVERAGE"
    RiUtilization -> "RI_UTILIZATION"
    SavingsPlansCoverage -> "SAVINGS_PLANS_COVERAGE"
    SavingsPlansUtilization -> "SAVINGS_PLANS_UTILIZATION"
    Usage -> "USAGE"

instance Hashable BudgetType

instance NFData BudgetType

instance ToByteString BudgetType

instance ToQuery BudgetType

instance ToHeader BudgetType

instance ToJSON BudgetType where
  toJSON = toJSONText

instance FromJSON BudgetType where
  parseJSON = parseJSONText "BudgetType"
