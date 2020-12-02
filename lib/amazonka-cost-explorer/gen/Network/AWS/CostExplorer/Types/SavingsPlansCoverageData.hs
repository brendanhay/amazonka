{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverageData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specific coverage percentage, On-Demand costs, and spend covered by Savings Plans, and total Savings Plans costs for an account.
--
--
--
-- /See:/ 'savingsPlansCoverageData' smart constructor.
data SavingsPlansCoverageData = SavingsPlansCoverageData'
  { _spcdOnDemandCost ::
      !(Maybe Text),
    _spcdSpendCoveredBySavingsPlans ::
      !(Maybe Text),
    _spcdCoveragePercentage :: !(Maybe Text),
    _spcdTotalCost :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansCoverageData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spcdOnDemandCost' - The cost of your AWS usage at the public On-Demand rate.
--
-- * 'spcdSpendCoveredBySavingsPlans' - The amount of your AWS usage that is covered by a Savings Plans.
--
-- * 'spcdCoveragePercentage' - The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
--
-- * 'spcdTotalCost' - The total cost of your AWS usage, regardless of your purchase option.
savingsPlansCoverageData ::
  SavingsPlansCoverageData
savingsPlansCoverageData =
  SavingsPlansCoverageData'
    { _spcdOnDemandCost = Nothing,
      _spcdSpendCoveredBySavingsPlans = Nothing,
      _spcdCoveragePercentage = Nothing,
      _spcdTotalCost = Nothing
    }

-- | The cost of your AWS usage at the public On-Demand rate.
spcdOnDemandCost :: Lens' SavingsPlansCoverageData (Maybe Text)
spcdOnDemandCost = lens _spcdOnDemandCost (\s a -> s {_spcdOnDemandCost = a})

-- | The amount of your AWS usage that is covered by a Savings Plans.
spcdSpendCoveredBySavingsPlans :: Lens' SavingsPlansCoverageData (Maybe Text)
spcdSpendCoveredBySavingsPlans = lens _spcdSpendCoveredBySavingsPlans (\s a -> s {_spcdSpendCoveredBySavingsPlans = a})

-- | The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
spcdCoveragePercentage :: Lens' SavingsPlansCoverageData (Maybe Text)
spcdCoveragePercentage = lens _spcdCoveragePercentage (\s a -> s {_spcdCoveragePercentage = a})

-- | The total cost of your AWS usage, regardless of your purchase option.
spcdTotalCost :: Lens' SavingsPlansCoverageData (Maybe Text)
spcdTotalCost = lens _spcdTotalCost (\s a -> s {_spcdTotalCost = a})

instance FromJSON SavingsPlansCoverageData where
  parseJSON =
    withObject
      "SavingsPlansCoverageData"
      ( \x ->
          SavingsPlansCoverageData'
            <$> (x .:? "OnDemandCost")
            <*> (x .:? "SpendCoveredBySavingsPlans")
            <*> (x .:? "CoveragePercentage")
            <*> (x .:? "TotalCost")
      )

instance Hashable SavingsPlansCoverageData

instance NFData SavingsPlansCoverageData
