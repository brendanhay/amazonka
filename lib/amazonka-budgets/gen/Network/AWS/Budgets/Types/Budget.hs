{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Budget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Budget
  ( Budget (..),

    -- * Smart constructor
    mkBudget,

    -- * Lenses
    bBudgetName,
    bTimeUnit,
    bBudgetType,
    bBudgetLimit,
    bCalculatedSpend,
    bCostFilters,
    bCostTypes,
    bLastUpdatedTime,
    bPlannedBudgetLimits,
    bTimePeriod,
  )
where

import qualified Network.AWS.Budgets.Types.BudgetName as Types
import qualified Network.AWS.Budgets.Types.BudgetType as Types
import qualified Network.AWS.Budgets.Types.CalculatedSpend as Types
import qualified Network.AWS.Budgets.Types.CostTypes as Types
import qualified Network.AWS.Budgets.Types.GenericString as Types
import qualified Network.AWS.Budgets.Types.Spend as Types
import qualified Network.AWS.Budgets.Types.TimePeriod as Types
import qualified Network.AWS.Budgets.Types.TimeUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of the @CreateBudget@ operation. The content consists of the detailed metadata and data file information, and the current status of the @budget@ object.
--
-- This is the ARN pattern for a budget:
-- @arn:aws:budgets::AccountId:budget/budgetName@
--
-- /See:/ 'mkBudget' smart constructor.
data Budget = Budget'
  { -- | The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
    budgetName :: Types.BudgetName,
    -- | The length of time until a budget resets the actual and forecasted spend.
    timeUnit :: Types.TimeUnit,
    -- | Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
    budgetType :: Types.BudgetType,
    -- | The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget.
    --
    -- @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
    budgetLimit :: Core.Maybe Types.Spend,
    -- | The actual and forecasted cost or usage that the budget tracks.
    calculatedSpend :: Core.Maybe Types.CalculatedSpend,
    -- | The cost filters, such as service or tag, that are applied to a budget.
    --
    -- AWS Budgets supports the following services as a filter for RI budgets:
    --
    --     * Amazon Elastic Compute Cloud - Compute
    --
    --
    --     * Amazon Redshift
    --
    --
    --     * Amazon Relational Database Service
    --
    --
    --     * Amazon ElastiCache
    --
    --
    --     * Amazon Elasticsearch Service
    costFilters :: Core.Maybe (Core.HashMap Types.GenericString [Types.GenericString]),
    -- | The types of costs that are included in this @COST@ budget.
    --
    -- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
    costTypes :: Core.Maybe Types.CostTypes,
    -- | The last time that you updated this budget.
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime,
    -- | A map containing multiple @BudgetLimit@ , including current or future limits.
    --
    -- @PlannedBudgetLimits@ is available for cost or usage budget and supports monthly and quarterly @TimeUnit@ .
    -- For monthly budgets, provide 12 months of @PlannedBudgetLimits@ values. This must start from the current month and include the next 11 months. The @key@ is the start of the month, @UTC@ in epoch seconds.
    -- For quarterly budgets, provide 4 quarters of @PlannedBudgetLimits@ value entries in standard calendar quarter increments. This must start from the current quarter and include the next 3 quarters. The @key@ is the start of the quarter, @UTC@ in epoch seconds.
    -- If the planned budget expires before 12 months for monthly or 4 quarters for quarterly, provide the @PlannedBudgetLimits@ values only for the remaining periods.
    -- If the budget begins at a date in the future, provide @PlannedBudgetLimits@ values from the start date of the budget.
    -- After all of the @BudgetLimit@ values in @PlannedBudgetLimits@ are used, the budget continues to use the last limit as the @BudgetLimit@ . At that point, the planned budget provides the same experience as a fixed budget.
    -- @DescribeBudget@ and @DescribeBudgets@ response along with @PlannedBudgetLimits@ will also contain @BudgetLimit@ representing the current month or quarter limit present in @PlannedBudgetLimits@ . This only applies to budgets created with @PlannedBudgetLimits@ . Budgets created without @PlannedBudgetLimits@ will only contain @BudgetLimit@ , and no @PlannedBudgetLimits@ .
    plannedBudgetLimits :: Core.Maybe (Core.HashMap Types.GenericString Types.Spend),
    -- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .
    --
    -- If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
    -- You can change either date with the @UpdateBudget@ operation.
    -- After the end date, AWS deletes the budget and all associated notifications and subscribers.
    timePeriod :: Core.Maybe Types.TimePeriod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Budget' value with any optional fields omitted.
mkBudget ::
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'timeUnit'
  Types.TimeUnit ->
  -- | 'budgetType'
  Types.BudgetType ->
  Budget
mkBudget budgetName timeUnit budgetType =
  Budget'
    { budgetName,
      timeUnit,
      budgetType,
      budgetLimit = Core.Nothing,
      calculatedSpend = Core.Nothing,
      costFilters = Core.Nothing,
      costTypes = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      plannedBudgetLimits = Core.Nothing,
      timePeriod = Core.Nothing
    }

-- | The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetName :: Lens.Lens' Budget Types.BudgetName
bBudgetName = Lens.field @"budgetName"
{-# DEPRECATED bBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The length of time until a budget resets the actual and forecasted spend.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimeUnit :: Lens.Lens' Budget Types.TimeUnit
bTimeUnit = Lens.field @"timeUnit"
{-# DEPRECATED bTimeUnit "Use generic-lens or generic-optics with 'timeUnit' instead." #-}

-- | Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
--
-- /Note:/ Consider using 'budgetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetType :: Lens.Lens' Budget Types.BudgetType
bBudgetType = Lens.field @"budgetType"
{-# DEPRECATED bBudgetType "Use generic-lens or generic-optics with 'budgetType' instead." #-}

-- | The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget.
--
-- @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
--
-- /Note:/ Consider using 'budgetLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetLimit :: Lens.Lens' Budget (Core.Maybe Types.Spend)
bBudgetLimit = Lens.field @"budgetLimit"
{-# DEPRECATED bBudgetLimit "Use generic-lens or generic-optics with 'budgetLimit' instead." #-}

-- | The actual and forecasted cost or usage that the budget tracks.
--
-- /Note:/ Consider using 'calculatedSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCalculatedSpend :: Lens.Lens' Budget (Core.Maybe Types.CalculatedSpend)
bCalculatedSpend = Lens.field @"calculatedSpend"
{-# DEPRECATED bCalculatedSpend "Use generic-lens or generic-optics with 'calculatedSpend' instead." #-}

-- | The cost filters, such as service or tag, that are applied to a budget.
--
-- AWS Budgets supports the following services as a filter for RI budgets:
--
--     * Amazon Elastic Compute Cloud - Compute
--
--
--     * Amazon Redshift
--
--
--     * Amazon Relational Database Service
--
--
--     * Amazon ElastiCache
--
--
--     * Amazon Elasticsearch Service
--
--
--
-- /Note:/ Consider using 'costFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCostFilters :: Lens.Lens' Budget (Core.Maybe (Core.HashMap Types.GenericString [Types.GenericString]))
bCostFilters = Lens.field @"costFilters"
{-# DEPRECATED bCostFilters "Use generic-lens or generic-optics with 'costFilters' instead." #-}

-- | The types of costs that are included in this @COST@ budget.
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
-- /Note:/ Consider using 'costTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCostTypes :: Lens.Lens' Budget (Core.Maybe Types.CostTypes)
bCostTypes = Lens.field @"costTypes"
{-# DEPRECATED bCostTypes "Use generic-lens or generic-optics with 'costTypes' instead." #-}

-- | The last time that you updated this budget.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLastUpdatedTime :: Lens.Lens' Budget (Core.Maybe Core.NominalDiffTime)
bLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED bLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | A map containing multiple @BudgetLimit@ , including current or future limits.
--
-- @PlannedBudgetLimits@ is available for cost or usage budget and supports monthly and quarterly @TimeUnit@ .
-- For monthly budgets, provide 12 months of @PlannedBudgetLimits@ values. This must start from the current month and include the next 11 months. The @key@ is the start of the month, @UTC@ in epoch seconds.
-- For quarterly budgets, provide 4 quarters of @PlannedBudgetLimits@ value entries in standard calendar quarter increments. This must start from the current quarter and include the next 3 quarters. The @key@ is the start of the quarter, @UTC@ in epoch seconds.
-- If the planned budget expires before 12 months for monthly or 4 quarters for quarterly, provide the @PlannedBudgetLimits@ values only for the remaining periods.
-- If the budget begins at a date in the future, provide @PlannedBudgetLimits@ values from the start date of the budget.
-- After all of the @BudgetLimit@ values in @PlannedBudgetLimits@ are used, the budget continues to use the last limit as the @BudgetLimit@ . At that point, the planned budget provides the same experience as a fixed budget.
-- @DescribeBudget@ and @DescribeBudgets@ response along with @PlannedBudgetLimits@ will also contain @BudgetLimit@ representing the current month or quarter limit present in @PlannedBudgetLimits@ . This only applies to budgets created with @PlannedBudgetLimits@ . Budgets created without @PlannedBudgetLimits@ will only contain @BudgetLimit@ , and no @PlannedBudgetLimits@ .
--
-- /Note:/ Consider using 'plannedBudgetLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPlannedBudgetLimits :: Lens.Lens' Budget (Core.Maybe (Core.HashMap Types.GenericString Types.Spend))
bPlannedBudgetLimits = Lens.field @"plannedBudgetLimits"
{-# DEPRECATED bPlannedBudgetLimits "Use generic-lens or generic-optics with 'plannedBudgetLimits' instead." #-}

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .
--
-- If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
-- You can change either date with the @UpdateBudget@ operation.
-- After the end date, AWS deletes the budget and all associated notifications and subscribers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimePeriod :: Lens.Lens' Budget (Core.Maybe Types.TimePeriod)
bTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED bTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Core.FromJSON Budget where
  toJSON Budget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("TimeUnit" Core..= timeUnit),
            Core.Just ("BudgetType" Core..= budgetType),
            ("BudgetLimit" Core..=) Core.<$> budgetLimit,
            ("CalculatedSpend" Core..=) Core.<$> calculatedSpend,
            ("CostFilters" Core..=) Core.<$> costFilters,
            ("CostTypes" Core..=) Core.<$> costTypes,
            ("LastUpdatedTime" Core..=) Core.<$> lastUpdatedTime,
            ("PlannedBudgetLimits" Core..=) Core.<$> plannedBudgetLimits,
            ("TimePeriod" Core..=) Core.<$> timePeriod
          ]
      )

instance Core.FromJSON Budget where
  parseJSON =
    Core.withObject "Budget" Core.$
      \x ->
        Budget'
          Core.<$> (x Core..: "BudgetName")
          Core.<*> (x Core..: "TimeUnit")
          Core.<*> (x Core..: "BudgetType")
          Core.<*> (x Core..:? "BudgetLimit")
          Core.<*> (x Core..:? "CalculatedSpend")
          Core.<*> (x Core..:? "CostFilters")
          Core.<*> (x Core..:? "CostTypes")
          Core.<*> (x Core..:? "LastUpdatedTime")
          Core.<*> (x Core..:? "PlannedBudgetLimits")
          Core.<*> (x Core..:? "TimePeriod")
