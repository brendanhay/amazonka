{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetedAndActualAmounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetedAndActualAmounts where

import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.TimePeriod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of cost or usage that you created the budget for, compared to your actual costs or usage.
--
--
--
-- /See:/ 'budgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { _baaaTimePeriod ::
      !(Maybe TimePeriod),
    _baaaActualAmount :: !(Maybe Spend),
    _baaaBudgetedAmount :: !(Maybe Spend)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BudgetedAndActualAmounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baaaTimePeriod' - The time period covered by this budget comparison.
--
-- * 'baaaActualAmount' - Your actual costs or usage for a budget period.
--
-- * 'baaaBudgetedAmount' - The amount of cost or usage that you created the budget for.
budgetedAndActualAmounts ::
  BudgetedAndActualAmounts
budgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { _baaaTimePeriod = Nothing,
      _baaaActualAmount = Nothing,
      _baaaBudgetedAmount = Nothing
    }

-- | The time period covered by this budget comparison.
baaaTimePeriod :: Lens' BudgetedAndActualAmounts (Maybe TimePeriod)
baaaTimePeriod = lens _baaaTimePeriod (\s a -> s {_baaaTimePeriod = a})

-- | Your actual costs or usage for a budget period.
baaaActualAmount :: Lens' BudgetedAndActualAmounts (Maybe Spend)
baaaActualAmount = lens _baaaActualAmount (\s a -> s {_baaaActualAmount = a})

-- | The amount of cost or usage that you created the budget for.
baaaBudgetedAmount :: Lens' BudgetedAndActualAmounts (Maybe Spend)
baaaBudgetedAmount = lens _baaaBudgetedAmount (\s a -> s {_baaaBudgetedAmount = a})

instance FromJSON BudgetedAndActualAmounts where
  parseJSON =
    withObject
      "BudgetedAndActualAmounts"
      ( \x ->
          BudgetedAndActualAmounts'
            <$> (x .:? "TimePeriod")
            <*> (x .:? "ActualAmount")
            <*> (x .:? "BudgetedAmount")
      )

instance Hashable BudgetedAndActualAmounts

instance NFData BudgetedAndActualAmounts
