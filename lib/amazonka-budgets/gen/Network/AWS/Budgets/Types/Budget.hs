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
    bCalculatedSpend,
    bPlannedBudgetLimits,
    bLastUpdatedTime,
    bBudgetLimit,
    bTimePeriod,
    bCostTypes,
    bCostFilters,
    bBudgetName,
    bTimeUnit,
    bBudgetType,
  )
where

import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.CalculatedSpend
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.TimePeriod
import Network.AWS.Budgets.Types.TimeUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of the @CreateBudget@ operation. The content consists of the detailed metadata and data file information, and the current status of the @budget@ object.
--
-- This is the ARN pattern for a budget:
-- @arn:aws:budgets::AccountId:budget/budgetName@
--
-- /See:/ 'mkBudget' smart constructor.
data Budget = Budget'
  { calculatedSpend ::
      Lude.Maybe CalculatedSpend,
    plannedBudgetLimits :: Lude.Maybe (Lude.HashMap Lude.Text (Spend)),
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    budgetLimit :: Lude.Maybe Spend,
    timePeriod :: Lude.Maybe TimePeriod,
    costTypes :: Lude.Maybe CostTypes,
    costFilters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    budgetName :: Lude.Text,
    timeUnit :: TimeUnit,
    budgetType :: BudgetType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Budget' with the minimum fields required to make a request.
--
-- * 'budgetLimit' - The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget.
--
-- @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
-- * 'budgetName' - The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
-- * 'budgetType' - Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
-- * 'calculatedSpend' - The actual and forecasted cost or usage that the budget tracks.
-- * 'costFilters' - The cost filters, such as service or tag, that are applied to a budget.
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
-- * 'costTypes' - The types of costs that are included in this @COST@ budget.
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
-- * 'lastUpdatedTime' - The last time that you updated this budget.
-- * 'plannedBudgetLimits' - A map containing multiple @BudgetLimit@ , including current or future limits.
--
-- @PlannedBudgetLimits@ is available for cost or usage budget and supports monthly and quarterly @TimeUnit@ .
-- For monthly budgets, provide 12 months of @PlannedBudgetLimits@ values. This must start from the current month and include the next 11 months. The @key@ is the start of the month, @UTC@ in epoch seconds.
-- For quarterly budgets, provide 4 quarters of @PlannedBudgetLimits@ value entries in standard calendar quarter increments. This must start from the current quarter and include the next 3 quarters. The @key@ is the start of the quarter, @UTC@ in epoch seconds.
-- If the planned budget expires before 12 months for monthly or 4 quarters for quarterly, provide the @PlannedBudgetLimits@ values only for the remaining periods.
-- If the budget begins at a date in the future, provide @PlannedBudgetLimits@ values from the start date of the budget.
-- After all of the @BudgetLimit@ values in @PlannedBudgetLimits@ are used, the budget continues to use the last limit as the @BudgetLimit@ . At that point, the planned budget provides the same experience as a fixed budget.
-- @DescribeBudget@ and @DescribeBudgets@ response along with @PlannedBudgetLimits@ will also contain @BudgetLimit@ representing the current month or quarter limit present in @PlannedBudgetLimits@ . This only applies to budgets created with @PlannedBudgetLimits@ . Budgets created without @PlannedBudgetLimits@ will only contain @BudgetLimit@ , and no @PlannedBudgetLimits@ .
-- * 'timePeriod' - The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .
--
-- If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
-- You can change either date with the @UpdateBudget@ operation.
-- After the end date, AWS deletes the budget and all associated notifications and subscribers.
-- * 'timeUnit' - The length of time until a budget resets the actual and forecasted spend.
mkBudget ::
  -- | 'budgetName'
  Lude.Text ->
  -- | 'timeUnit'
  TimeUnit ->
  -- | 'budgetType'
  BudgetType ->
  Budget
mkBudget pBudgetName_ pTimeUnit_ pBudgetType_ =
  Budget'
    { calculatedSpend = Lude.Nothing,
      plannedBudgetLimits = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      budgetLimit = Lude.Nothing,
      timePeriod = Lude.Nothing,
      costTypes = Lude.Nothing,
      costFilters = Lude.Nothing,
      budgetName = pBudgetName_,
      timeUnit = pTimeUnit_,
      budgetType = pBudgetType_
    }

-- | The actual and forecasted cost or usage that the budget tracks.
--
-- /Note:/ Consider using 'calculatedSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCalculatedSpend :: Lens.Lens' Budget (Lude.Maybe CalculatedSpend)
bCalculatedSpend = Lens.lens (calculatedSpend :: Budget -> Lude.Maybe CalculatedSpend) (\s a -> s {calculatedSpend = a} :: Budget)
{-# DEPRECATED bCalculatedSpend "Use generic-lens or generic-optics with 'calculatedSpend' instead." #-}

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
bPlannedBudgetLimits :: Lens.Lens' Budget (Lude.Maybe (Lude.HashMap Lude.Text (Spend)))
bPlannedBudgetLimits = Lens.lens (plannedBudgetLimits :: Budget -> Lude.Maybe (Lude.HashMap Lude.Text (Spend))) (\s a -> s {plannedBudgetLimits = a} :: Budget)
{-# DEPRECATED bPlannedBudgetLimits "Use generic-lens or generic-optics with 'plannedBudgetLimits' instead." #-}

-- | The last time that you updated this budget.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLastUpdatedTime :: Lens.Lens' Budget (Lude.Maybe Lude.Timestamp)
bLastUpdatedTime = Lens.lens (lastUpdatedTime :: Budget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: Budget)
{-# DEPRECATED bLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget.
--
-- @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
--
-- /Note:/ Consider using 'budgetLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetLimit :: Lens.Lens' Budget (Lude.Maybe Spend)
bBudgetLimit = Lens.lens (budgetLimit :: Budget -> Lude.Maybe Spend) (\s a -> s {budgetLimit = a} :: Budget)
{-# DEPRECATED bBudgetLimit "Use generic-lens or generic-optics with 'budgetLimit' instead." #-}

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .
--
-- If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.
-- You can change either date with the @UpdateBudget@ operation.
-- After the end date, AWS deletes the budget and all associated notifications and subscribers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimePeriod :: Lens.Lens' Budget (Lude.Maybe TimePeriod)
bTimePeriod = Lens.lens (timePeriod :: Budget -> Lude.Maybe TimePeriod) (\s a -> s {timePeriod = a} :: Budget)
{-# DEPRECATED bTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The types of costs that are included in this @COST@ budget.
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
-- /Note:/ Consider using 'costTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCostTypes :: Lens.Lens' Budget (Lude.Maybe CostTypes)
bCostTypes = Lens.lens (costTypes :: Budget -> Lude.Maybe CostTypes) (\s a -> s {costTypes = a} :: Budget)
{-# DEPRECATED bCostTypes "Use generic-lens or generic-optics with 'costTypes' instead." #-}

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
bCostFilters :: Lens.Lens' Budget (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
bCostFilters = Lens.lens (costFilters :: Budget -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {costFilters = a} :: Budget)
{-# DEPRECATED bCostFilters "Use generic-lens or generic-optics with 'costFilters' instead." #-}

-- | The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetName :: Lens.Lens' Budget Lude.Text
bBudgetName = Lens.lens (budgetName :: Budget -> Lude.Text) (\s a -> s {budgetName = a} :: Budget)
{-# DEPRECATED bBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The length of time until a budget resets the actual and forecasted spend.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimeUnit :: Lens.Lens' Budget TimeUnit
bTimeUnit = Lens.lens (timeUnit :: Budget -> TimeUnit) (\s a -> s {timeUnit = a} :: Budget)
{-# DEPRECATED bTimeUnit "Use generic-lens or generic-optics with 'timeUnit' instead." #-}

-- | Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
--
-- /Note:/ Consider using 'budgetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBudgetType :: Lens.Lens' Budget BudgetType
bBudgetType = Lens.lens (budgetType :: Budget -> BudgetType) (\s a -> s {budgetType = a} :: Budget)
{-# DEPRECATED bBudgetType "Use generic-lens or generic-optics with 'budgetType' instead." #-}

instance Lude.FromJSON Budget where
  parseJSON =
    Lude.withObject
      "Budget"
      ( \x ->
          Budget'
            Lude.<$> (x Lude..:? "CalculatedSpend")
            Lude.<*> (x Lude..:? "PlannedBudgetLimits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "BudgetLimit")
            Lude.<*> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "CostTypes")
            Lude.<*> (x Lude..:? "CostFilters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "BudgetName")
            Lude.<*> (x Lude..: "TimeUnit")
            Lude.<*> (x Lude..: "BudgetType")
      )

instance Lude.ToJSON Budget where
  toJSON Budget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CalculatedSpend" Lude..=) Lude.<$> calculatedSpend,
            ("PlannedBudgetLimits" Lude..=) Lude.<$> plannedBudgetLimits,
            ("LastUpdatedTime" Lude..=) Lude.<$> lastUpdatedTime,
            ("BudgetLimit" Lude..=) Lude.<$> budgetLimit,
            ("TimePeriod" Lude..=) Lude.<$> timePeriod,
            ("CostTypes" Lude..=) Lude.<$> costTypes,
            ("CostFilters" Lude..=) Lude.<$> costFilters,
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("TimeUnit" Lude..= timeUnit),
            Lude.Just ("BudgetType" Lude..= budgetType)
          ]
      )
