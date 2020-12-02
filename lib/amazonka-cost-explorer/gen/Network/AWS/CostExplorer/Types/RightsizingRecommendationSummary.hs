{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary of rightsizing recommendations
--
--
--
-- /See:/ 'rightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { _rrsSavingsPercentage ::
      !(Maybe Text),
    _rrsSavingsCurrencyCode ::
      !(Maybe Text),
    _rrsTotalRecommendationCount ::
      !(Maybe Text),
    _rrsEstimatedTotalMonthlySavingsAmount ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RightsizingRecommendationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsSavingsPercentage' - Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
--
-- * 'rrsSavingsCurrencyCode' - The currency code that AWS used to calculate the savings.
--
-- * 'rrsTotalRecommendationCount' - Total number of instance recommendations.
--
-- * 'rrsEstimatedTotalMonthlySavingsAmount' - Estimated total savings resulting from modifications, on a monthly basis.
rightsizingRecommendationSummary ::
  RightsizingRecommendationSummary
rightsizingRecommendationSummary =
  RightsizingRecommendationSummary'
    { _rrsSavingsPercentage =
        Nothing,
      _rrsSavingsCurrencyCode = Nothing,
      _rrsTotalRecommendationCount = Nothing,
      _rrsEstimatedTotalMonthlySavingsAmount = Nothing
    }

-- | Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
rrsSavingsPercentage :: Lens' RightsizingRecommendationSummary (Maybe Text)
rrsSavingsPercentage = lens _rrsSavingsPercentage (\s a -> s {_rrsSavingsPercentage = a})

-- | The currency code that AWS used to calculate the savings.
rrsSavingsCurrencyCode :: Lens' RightsizingRecommendationSummary (Maybe Text)
rrsSavingsCurrencyCode = lens _rrsSavingsCurrencyCode (\s a -> s {_rrsSavingsCurrencyCode = a})

-- | Total number of instance recommendations.
rrsTotalRecommendationCount :: Lens' RightsizingRecommendationSummary (Maybe Text)
rrsTotalRecommendationCount = lens _rrsTotalRecommendationCount (\s a -> s {_rrsTotalRecommendationCount = a})

-- | Estimated total savings resulting from modifications, on a monthly basis.
rrsEstimatedTotalMonthlySavingsAmount :: Lens' RightsizingRecommendationSummary (Maybe Text)
rrsEstimatedTotalMonthlySavingsAmount = lens _rrsEstimatedTotalMonthlySavingsAmount (\s a -> s {_rrsEstimatedTotalMonthlySavingsAmount = a})

instance FromJSON RightsizingRecommendationSummary where
  parseJSON =
    withObject
      "RightsizingRecommendationSummary"
      ( \x ->
          RightsizingRecommendationSummary'
            <$> (x .:? "SavingsPercentage")
            <*> (x .:? "SavingsCurrencyCode")
            <*> (x .:? "TotalRecommendationCount")
            <*> (x .:? "EstimatedTotalMonthlySavingsAmount")
      )

instance Hashable RightsizingRecommendationSummary

instance NFData RightsizingRecommendationSummary
