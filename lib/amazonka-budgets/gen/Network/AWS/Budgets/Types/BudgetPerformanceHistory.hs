{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetPerformanceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetPerformanceHistory where

import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.BudgetedAndActualAmounts
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.TimeUnit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A history of the state of a budget at the end of the budget's specified time period.
--
--
--
-- /See:/ 'budgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { _bphBudgetedAndActualAmountsList ::
      !(Maybe [BudgetedAndActualAmounts]),
    _bphTimeUnit :: !(Maybe TimeUnit),
    _bphBudgetName :: !(Maybe Text),
    _bphBudgetType :: !(Maybe BudgetType),
    _bphCostTypes :: !(Maybe CostTypes),
    _bphCostFilters ::
      !(Maybe (Map Text ([Text])))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BudgetPerformanceHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bphBudgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
--
-- * 'bphTimeUnit' - Undocumented member.
--
-- * 'bphBudgetName' - Undocumented member.
--
-- * 'bphBudgetType' - Undocumented member.
--
-- * 'bphCostTypes' - The history of the cost types for a budget during the specified time period.
--
-- * 'bphCostFilters' - The history of the cost filters for a budget during the specified time period.
budgetPerformanceHistory ::
  BudgetPerformanceHistory
budgetPerformanceHistory =
  BudgetPerformanceHistory'
    { _bphBudgetedAndActualAmountsList =
        Nothing,
      _bphTimeUnit = Nothing,
      _bphBudgetName = Nothing,
      _bphBudgetType = Nothing,
      _bphCostTypes = Nothing,
      _bphCostFilters = Nothing
    }

-- | A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
bphBudgetedAndActualAmountsList :: Lens' BudgetPerformanceHistory [BudgetedAndActualAmounts]
bphBudgetedAndActualAmountsList = lens _bphBudgetedAndActualAmountsList (\s a -> s {_bphBudgetedAndActualAmountsList = a}) . _Default . _Coerce

-- | Undocumented member.
bphTimeUnit :: Lens' BudgetPerformanceHistory (Maybe TimeUnit)
bphTimeUnit = lens _bphTimeUnit (\s a -> s {_bphTimeUnit = a})

-- | Undocumented member.
bphBudgetName :: Lens' BudgetPerformanceHistory (Maybe Text)
bphBudgetName = lens _bphBudgetName (\s a -> s {_bphBudgetName = a})

-- | Undocumented member.
bphBudgetType :: Lens' BudgetPerformanceHistory (Maybe BudgetType)
bphBudgetType = lens _bphBudgetType (\s a -> s {_bphBudgetType = a})

-- | The history of the cost types for a budget during the specified time period.
bphCostTypes :: Lens' BudgetPerformanceHistory (Maybe CostTypes)
bphCostTypes = lens _bphCostTypes (\s a -> s {_bphCostTypes = a})

-- | The history of the cost filters for a budget during the specified time period.
bphCostFilters :: Lens' BudgetPerformanceHistory (HashMap Text ([Text]))
bphCostFilters = lens _bphCostFilters (\s a -> s {_bphCostFilters = a}) . _Default . _Map

instance FromJSON BudgetPerformanceHistory where
  parseJSON =
    withObject
      "BudgetPerformanceHistory"
      ( \x ->
          BudgetPerformanceHistory'
            <$> (x .:? "BudgetedAndActualAmountsList" .!= mempty)
            <*> (x .:? "TimeUnit")
            <*> (x .:? "BudgetName")
            <*> (x .:? "BudgetType")
            <*> (x .:? "CostTypes")
            <*> (x .:? "CostFilters" .!= mempty)
      )

instance Hashable BudgetPerformanceHistory

instance NFData BudgetPerformanceHistory
