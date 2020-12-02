{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail where

import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A single daily or monthly Savings Plans utilization rate, and details for your account. A management account in an organization have access to member accounts. You can use @GetDimensionValues@ to determine the possible dimension values.
--
--
--
-- /See:/ 'savingsPlansUtilizationDetail' smart constructor.
data SavingsPlansUtilizationDetail = SavingsPlansUtilizationDetail'
  { _spudAmortizedCommitment ::
      !( Maybe
           SavingsPlansAmortizedCommitment
       ),
    _spudSavings ::
      !(Maybe SavingsPlansSavings),
    _spudAttributes ::
      !(Maybe (Map Text (Text))),
    _spudUtilization ::
      !( Maybe
           SavingsPlansUtilization
       ),
    _spudSavingsPlanARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansUtilizationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spudAmortizedCommitment' - The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
--
-- * 'spudSavings' - The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- * 'spudAttributes' - The attribute that applies to a specific @Dimension@ .
--
-- * 'spudUtilization' - A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- * 'spudSavingsPlanARN' - The unique Amazon Resource Name (ARN) for a particular Savings Plan.
savingsPlansUtilizationDetail ::
  SavingsPlansUtilizationDetail
savingsPlansUtilizationDetail =
  SavingsPlansUtilizationDetail'
    { _spudAmortizedCommitment =
        Nothing,
      _spudSavings = Nothing,
      _spudAttributes = Nothing,
      _spudUtilization = Nothing,
      _spudSavingsPlanARN = Nothing
    }

-- | The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
spudAmortizedCommitment :: Lens' SavingsPlansUtilizationDetail (Maybe SavingsPlansAmortizedCommitment)
spudAmortizedCommitment = lens _spudAmortizedCommitment (\s a -> s {_spudAmortizedCommitment = a})

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
spudSavings :: Lens' SavingsPlansUtilizationDetail (Maybe SavingsPlansSavings)
spudSavings = lens _spudSavings (\s a -> s {_spudSavings = a})

-- | The attribute that applies to a specific @Dimension@ .
spudAttributes :: Lens' SavingsPlansUtilizationDetail (HashMap Text (Text))
spudAttributes = lens _spudAttributes (\s a -> s {_spudAttributes = a}) . _Default . _Map

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
spudUtilization :: Lens' SavingsPlansUtilizationDetail (Maybe SavingsPlansUtilization)
spudUtilization = lens _spudUtilization (\s a -> s {_spudUtilization = a})

-- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
spudSavingsPlanARN :: Lens' SavingsPlansUtilizationDetail (Maybe Text)
spudSavingsPlanARN = lens _spudSavingsPlanARN (\s a -> s {_spudSavingsPlanARN = a})

instance FromJSON SavingsPlansUtilizationDetail where
  parseJSON =
    withObject
      "SavingsPlansUtilizationDetail"
      ( \x ->
          SavingsPlansUtilizationDetail'
            <$> (x .:? "AmortizedCommitment")
            <*> (x .:? "Savings")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "Utilization")
            <*> (x .:? "SavingsPlanArn")
      )

instance Hashable SavingsPlansUtilizationDetail

instance NFData SavingsPlansUtilizationDetail
