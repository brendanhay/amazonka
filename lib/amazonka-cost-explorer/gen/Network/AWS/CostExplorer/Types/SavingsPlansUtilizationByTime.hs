{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of Savings Plans utilization, in hours.
--
--
--
-- /See:/ 'savingsPlansUtilizationByTime' smart constructor.
data SavingsPlansUtilizationByTime = SavingsPlansUtilizationByTime'
  { _spubtAmortizedCommitment ::
      !( Maybe
           SavingsPlansAmortizedCommitment
       ),
    _spubtSavings ::
      !(Maybe SavingsPlansSavings),
    _spubtTimePeriod ::
      !DateInterval,
    _spubtUtilization ::
      !SavingsPlansUtilization
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansUtilizationByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spubtAmortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
--
-- * 'spubtSavings' - The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- * 'spubtTimePeriod' - Undocumented member.
--
-- * 'spubtUtilization' - A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
savingsPlansUtilizationByTime ::
  -- | 'spubtTimePeriod'
  DateInterval ->
  -- | 'spubtUtilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationByTime
savingsPlansUtilizationByTime pTimePeriod_ pUtilization_ =
  SavingsPlansUtilizationByTime'
    { _spubtAmortizedCommitment =
        Nothing,
      _spubtSavings = Nothing,
      _spubtTimePeriod = pTimePeriod_,
      _spubtUtilization = pUtilization_
    }

-- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
spubtAmortizedCommitment :: Lens' SavingsPlansUtilizationByTime (Maybe SavingsPlansAmortizedCommitment)
spubtAmortizedCommitment = lens _spubtAmortizedCommitment (\s a -> s {_spubtAmortizedCommitment = a})

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
spubtSavings :: Lens' SavingsPlansUtilizationByTime (Maybe SavingsPlansSavings)
spubtSavings = lens _spubtSavings (\s a -> s {_spubtSavings = a})

-- | Undocumented member.
spubtTimePeriod :: Lens' SavingsPlansUtilizationByTime DateInterval
spubtTimePeriod = lens _spubtTimePeriod (\s a -> s {_spubtTimePeriod = a})

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
spubtUtilization :: Lens' SavingsPlansUtilizationByTime SavingsPlansUtilization
spubtUtilization = lens _spubtUtilization (\s a -> s {_spubtUtilization = a})

instance FromJSON SavingsPlansUtilizationByTime where
  parseJSON =
    withObject
      "SavingsPlansUtilizationByTime"
      ( \x ->
          SavingsPlansUtilizationByTime'
            <$> (x .:? "AmortizedCommitment")
            <*> (x .:? "Savings")
            <*> (x .: "TimePeriod")
            <*> (x .: "Utilization")
      )

instance Hashable SavingsPlansUtilizationByTime

instance NFData SavingsPlansUtilizationByTime
