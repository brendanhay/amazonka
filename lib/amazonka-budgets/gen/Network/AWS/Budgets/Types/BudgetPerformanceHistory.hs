{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetPerformanceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.BudgetPerformanceHistory
  ( BudgetPerformanceHistory (..)
  -- * Smart constructor
  , mkBudgetPerformanceHistory
  -- * Lenses
  , bphBudgetName
  , bphBudgetType
  , bphBudgetedAndActualAmountsList
  , bphCostFilters
  , bphCostTypes
  , bphTimeUnit
  ) where

import qualified Network.AWS.Budgets.Types.BudgetName as Types
import qualified Network.AWS.Budgets.Types.BudgetType as Types
import qualified Network.AWS.Budgets.Types.BudgetedAndActualAmounts as Types
import qualified Network.AWS.Budgets.Types.CostTypes as Types
import qualified Network.AWS.Budgets.Types.GenericString as Types
import qualified Network.AWS.Budgets.Types.TimeUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A history of the state of a budget at the end of the budget's specified time period.
--
-- /See:/ 'mkBudgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { budgetName :: Core.Maybe Types.BudgetName
  , budgetType :: Core.Maybe Types.BudgetType
  , budgetedAndActualAmountsList :: Core.Maybe [Types.BudgetedAndActualAmounts]
    -- ^ A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
  , costFilters :: Core.Maybe (Core.HashMap Types.GenericString [Types.GenericString])
    -- ^ The history of the cost filters for a budget during the specified time period.
  , costTypes :: Core.Maybe Types.CostTypes
    -- ^ The history of the cost types for a budget during the specified time period.
  , timeUnit :: Core.Maybe Types.TimeUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BudgetPerformanceHistory' value with any optional fields omitted.
mkBudgetPerformanceHistory
    :: BudgetPerformanceHistory
mkBudgetPerformanceHistory
  = BudgetPerformanceHistory'{budgetName = Core.Nothing,
                              budgetType = Core.Nothing,
                              budgetedAndActualAmountsList = Core.Nothing,
                              costFilters = Core.Nothing, costTypes = Core.Nothing,
                              timeUnit = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetName :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe Types.BudgetName)
bphBudgetName = Lens.field @"budgetName"
{-# INLINEABLE bphBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetType :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe Types.BudgetType)
bphBudgetType = Lens.field @"budgetType"
{-# INLINEABLE bphBudgetType #-}
{-# DEPRECATED budgetType "Use generic-lens or generic-optics with 'budgetType' instead"  #-}

-- | A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
--
-- /Note:/ Consider using 'budgetedAndActualAmountsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetedAndActualAmountsList :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe [Types.BudgetedAndActualAmounts])
bphBudgetedAndActualAmountsList = Lens.field @"budgetedAndActualAmountsList"
{-# INLINEABLE bphBudgetedAndActualAmountsList #-}
{-# DEPRECATED budgetedAndActualAmountsList "Use generic-lens or generic-optics with 'budgetedAndActualAmountsList' instead"  #-}

-- | The history of the cost filters for a budget during the specified time period.
--
-- /Note:/ Consider using 'costFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphCostFilters :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe (Core.HashMap Types.GenericString [Types.GenericString]))
bphCostFilters = Lens.field @"costFilters"
{-# INLINEABLE bphCostFilters #-}
{-# DEPRECATED costFilters "Use generic-lens or generic-optics with 'costFilters' instead"  #-}

-- | The history of the cost types for a budget during the specified time period.
--
-- /Note:/ Consider using 'costTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphCostTypes :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe Types.CostTypes)
bphCostTypes = Lens.field @"costTypes"
{-# INLINEABLE bphCostTypes #-}
{-# DEPRECATED costTypes "Use generic-lens or generic-optics with 'costTypes' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphTimeUnit :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe Types.TimeUnit)
bphTimeUnit = Lens.field @"timeUnit"
{-# INLINEABLE bphTimeUnit #-}
{-# DEPRECATED timeUnit "Use generic-lens or generic-optics with 'timeUnit' instead"  #-}

instance Core.FromJSON BudgetPerformanceHistory where
        parseJSON
          = Core.withObject "BudgetPerformanceHistory" Core.$
              \ x ->
                BudgetPerformanceHistory' Core.<$>
                  (x Core..:? "BudgetName") Core.<*> x Core..:? "BudgetType" Core.<*>
                    x Core..:? "BudgetedAndActualAmountsList"
                    Core.<*> x Core..:? "CostFilters"
                    Core.<*> x Core..:? "CostTypes"
                    Core.<*> x Core..:? "TimeUnit"
