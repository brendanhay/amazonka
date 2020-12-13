{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetType
  ( BudgetType
      ( BudgetType',
        Usage,
        Cost,
        RiUtilization,
        RiCoverage,
        SavingsPlansUtilization,
        SavingsPlansCoverage
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of a budget. It must be one of the following types:
--
-- @COST@ , @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , or @SAVINGS_PLANS_COVERAGE@ .
newtype BudgetType = BudgetType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Usage :: BudgetType
pattern Usage = BudgetType' "USAGE"

pattern Cost :: BudgetType
pattern Cost = BudgetType' "COST"

pattern RiUtilization :: BudgetType
pattern RiUtilization = BudgetType' "RI_UTILIZATION"

pattern RiCoverage :: BudgetType
pattern RiCoverage = BudgetType' "RI_COVERAGE"

pattern SavingsPlansUtilization :: BudgetType
pattern SavingsPlansUtilization = BudgetType' "SAVINGS_PLANS_UTILIZATION"

pattern SavingsPlansCoverage :: BudgetType
pattern SavingsPlansCoverage = BudgetType' "SAVINGS_PLANS_COVERAGE"

{-# COMPLETE
  Usage,
  Cost,
  RiUtilization,
  RiCoverage,
  SavingsPlansUtilization,
  SavingsPlansCoverage,
  BudgetType'
  #-}
