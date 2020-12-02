{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Budget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Budget where

import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.CalculatedSpend
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.TimePeriod
import Network.AWS.Budgets.Types.TimeUnit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of the @CreateBudget@ operation. The content consists of the detailed metadata and data file information, and the current status of the @budget@ object.
--
--
-- This is the ARN pattern for a budget:
--
-- @arn:aws:budgets::AccountId:budget/budgetName@
--
--
-- /See:/ 'budget' smart constructor.
data Budget = Budget'
  { _bCalculatedSpend ::
      !(Maybe CalculatedSpend),
    _bPlannedBudgetLimits :: !(Maybe (Map Text (Spend))),
    _bLastUpdatedTime :: !(Maybe POSIX),
    _bBudgetLimit :: !(Maybe Spend),
    _bTimePeriod :: !(Maybe TimePeriod),
    _bCostTypes :: !(Maybe CostTypes),
    _bCostFilters :: !(Maybe (Map Text ([Text]))),
    _bBudgetName :: !Text,
    _bTimeUnit :: !TimeUnit,
    _bBudgetType :: !BudgetType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Budget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCalculatedSpend' - The actual and forecasted cost or usage that the budget tracks.
--
-- * 'bPlannedBudgetLimits' - A map containing multiple @BudgetLimit@ , including current or future limits. @PlannedBudgetLimits@ is available for cost or usage budget and supports monthly and quarterly @TimeUnit@ .  For monthly budgets, provide 12 months of @PlannedBudgetLimits@ values. This must start from the current month and include the next 11 months. The @key@ is the start of the month, @UTC@ in epoch seconds.  For quarterly budgets, provide 4 quarters of @PlannedBudgetLimits@ value entries in standard calendar quarter increments. This must start from the current quarter and include the next 3 quarters. The @key@ is the start of the quarter, @UTC@ in epoch seconds.  If the planned budget expires before 12 months for monthly or 4 quarters for quarterly, provide the @PlannedBudgetLimits@ values only for the remaining periods. If the budget begins at a date in the future, provide @PlannedBudgetLimits@ values from the start date of the budget.  After all of the @BudgetLimit@ values in @PlannedBudgetLimits@ are used, the budget continues to use the last limit as the @BudgetLimit@ . At that point, the planned budget provides the same experience as a fixed budget.  @DescribeBudget@ and @DescribeBudgets@ response along with @PlannedBudgetLimits@ will also contain @BudgetLimit@ representing the current month or quarter limit present in @PlannedBudgetLimits@ . This only applies to budgets created with @PlannedBudgetLimits@ . Budgets created without @PlannedBudgetLimits@ will only contain @BudgetLimit@ , and no @PlannedBudgetLimits@ .
--
-- * 'bLastUpdatedTime' - The last time that you updated this budget.
--
-- * 'bBudgetLimit' - The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget. @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
--
-- * 'bTimePeriod' - The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .  If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.  You can change either date with the @UpdateBudget@ operation. After the end date, AWS deletes the budget and all associated notifications and subscribers.
--
-- * 'bCostTypes' - The types of costs that are included in this @COST@ budget. @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
-- * 'bCostFilters' - The cost filters, such as service or tag, that are applied to a budget. AWS Budgets supports the following services as a filter for RI budgets:     * Amazon Elastic Compute Cloud - Compute     * Amazon Redshift     * Amazon Relational Database Service     * Amazon ElastiCache     * Amazon Elasticsearch Service
--
-- * 'bBudgetName' - The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
--
-- * 'bTimeUnit' - The length of time until a budget resets the actual and forecasted spend.
--
-- * 'bBudgetType' - Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
budget ::
  -- | 'bBudgetName'
  Text ->
  -- | 'bTimeUnit'
  TimeUnit ->
  -- | 'bBudgetType'
  BudgetType ->
  Budget
budget pBudgetName_ pTimeUnit_ pBudgetType_ =
  Budget'
    { _bCalculatedSpend = Nothing,
      _bPlannedBudgetLimits = Nothing,
      _bLastUpdatedTime = Nothing,
      _bBudgetLimit = Nothing,
      _bTimePeriod = Nothing,
      _bCostTypes = Nothing,
      _bCostFilters = Nothing,
      _bBudgetName = pBudgetName_,
      _bTimeUnit = pTimeUnit_,
      _bBudgetType = pBudgetType_
    }

-- | The actual and forecasted cost or usage that the budget tracks.
bCalculatedSpend :: Lens' Budget (Maybe CalculatedSpend)
bCalculatedSpend = lens _bCalculatedSpend (\s a -> s {_bCalculatedSpend = a})

-- | A map containing multiple @BudgetLimit@ , including current or future limits. @PlannedBudgetLimits@ is available for cost or usage budget and supports monthly and quarterly @TimeUnit@ .  For monthly budgets, provide 12 months of @PlannedBudgetLimits@ values. This must start from the current month and include the next 11 months. The @key@ is the start of the month, @UTC@ in epoch seconds.  For quarterly budgets, provide 4 quarters of @PlannedBudgetLimits@ value entries in standard calendar quarter increments. This must start from the current quarter and include the next 3 quarters. The @key@ is the start of the quarter, @UTC@ in epoch seconds.  If the planned budget expires before 12 months for monthly or 4 quarters for quarterly, provide the @PlannedBudgetLimits@ values only for the remaining periods. If the budget begins at a date in the future, provide @PlannedBudgetLimits@ values from the start date of the budget.  After all of the @BudgetLimit@ values in @PlannedBudgetLimits@ are used, the budget continues to use the last limit as the @BudgetLimit@ . At that point, the planned budget provides the same experience as a fixed budget.  @DescribeBudget@ and @DescribeBudgets@ response along with @PlannedBudgetLimits@ will also contain @BudgetLimit@ representing the current month or quarter limit present in @PlannedBudgetLimits@ . This only applies to budgets created with @PlannedBudgetLimits@ . Budgets created without @PlannedBudgetLimits@ will only contain @BudgetLimit@ , and no @PlannedBudgetLimits@ .
bPlannedBudgetLimits :: Lens' Budget (HashMap Text (Spend))
bPlannedBudgetLimits = lens _bPlannedBudgetLimits (\s a -> s {_bPlannedBudgetLimits = a}) . _Default . _Map

-- | The last time that you updated this budget.
bLastUpdatedTime :: Lens' Budget (Maybe UTCTime)
bLastUpdatedTime = lens _bLastUpdatedTime (\s a -> s {_bLastUpdatedTime = a}) . mapping _Time

-- | The total amount of cost, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage that you want to track with your budget. @BudgetLimit@ is required for cost or usage budgets, but optional for RI or Savings Plans utilization or coverage budgets. RI and Savings Plans utilization or coverage budgets default to @100@ , which is the only valid value for RI or Savings Plans utilization or coverage budgets. You can't use @BudgetLimit@ with @PlannedBudgetLimits@ for @CreateBudget@ and @UpdateBudget@ actions.
bBudgetLimit :: Lens' Budget (Maybe Spend)
bBudgetLimit = lens _bBudgetLimit (\s a -> s {_bBudgetLimit = a})

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .  If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.  You can change either date with the @UpdateBudget@ operation. After the end date, AWS deletes the budget and all associated notifications and subscribers.
bTimePeriod :: Lens' Budget (Maybe TimePeriod)
bTimePeriod = lens _bTimePeriod (\s a -> s {_bTimePeriod = a})

-- | The types of costs that are included in this @COST@ budget. @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
bCostTypes :: Lens' Budget (Maybe CostTypes)
bCostTypes = lens _bCostTypes (\s a -> s {_bCostTypes = a})

-- | The cost filters, such as service or tag, that are applied to a budget. AWS Budgets supports the following services as a filter for RI budgets:     * Amazon Elastic Compute Cloud - Compute     * Amazon Redshift     * Amazon Relational Database Service     * Amazon ElastiCache     * Amazon Elasticsearch Service
bCostFilters :: Lens' Budget (HashMap Text ([Text]))
bCostFilters = lens _bCostFilters (\s a -> s {_bCostFilters = a}) . _Default . _Map

-- | The name of a budget. The name must be unique within an account. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
bBudgetName :: Lens' Budget Text
bBudgetName = lens _bBudgetName (\s a -> s {_bBudgetName = a})

-- | The length of time until a budget resets the actual and forecasted spend.
bTimeUnit :: Lens' Budget TimeUnit
bTimeUnit = lens _bTimeUnit (\s a -> s {_bTimeUnit = a})

-- | Whether this budget tracks costs, usage, RI utilization, RI coverage, Savings Plans utilization, or Savings Plans coverage.
bBudgetType :: Lens' Budget BudgetType
bBudgetType = lens _bBudgetType (\s a -> s {_bBudgetType = a})

instance FromJSON Budget where
  parseJSON =
    withObject
      "Budget"
      ( \x ->
          Budget'
            <$> (x .:? "CalculatedSpend")
            <*> (x .:? "PlannedBudgetLimits" .!= mempty)
            <*> (x .:? "LastUpdatedTime")
            <*> (x .:? "BudgetLimit")
            <*> (x .:? "TimePeriod")
            <*> (x .:? "CostTypes")
            <*> (x .:? "CostFilters" .!= mempty)
            <*> (x .: "BudgetName")
            <*> (x .: "TimeUnit")
            <*> (x .: "BudgetType")
      )

instance Hashable Budget

instance NFData Budget

instance ToJSON Budget where
  toJSON Budget' {..} =
    object
      ( catMaybes
          [ ("CalculatedSpend" .=) <$> _bCalculatedSpend,
            ("PlannedBudgetLimits" .=) <$> _bPlannedBudgetLimits,
            ("LastUpdatedTime" .=) <$> _bLastUpdatedTime,
            ("BudgetLimit" .=) <$> _bBudgetLimit,
            ("TimePeriod" .=) <$> _bTimePeriod,
            ("CostTypes" .=) <$> _bCostTypes,
            ("CostFilters" .=) <$> _bCostFilters,
            Just ("BudgetName" .= _bBudgetName),
            Just ("TimeUnit" .= _bTimeUnit),
            Just ("BudgetType" .= _bBudgetType)
          ]
      )
