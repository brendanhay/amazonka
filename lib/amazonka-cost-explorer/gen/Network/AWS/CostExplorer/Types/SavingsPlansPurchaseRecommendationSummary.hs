{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
--
--
-- /See:/ 'savingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { _spprsCurrencyCode ::
      !( Maybe
           Text
       ),
    _spprsDailyCommitmentToPurchase ::
      !( Maybe
           Text
       ),
    _spprsEstimatedTotalCost ::
      !( Maybe
           Text
       ),
    _spprsEstimatedROI ::
      !( Maybe
           Text
       ),
    _spprsEstimatedSavingsAmount ::
      !( Maybe
           Text
       ),
    _spprsEstimatedMonthlySavingsAmount ::
      !( Maybe
           Text
       ),
    _spprsEstimatedOnDemandCostWithCurrentCommitment ::
      !( Maybe
           Text
       ),
    _spprsEstimatedSavingsPercentage ::
      !( Maybe
           Text
       ),
    _spprsTotalRecommendationCount ::
      !( Maybe
           Text
       ),
    _spprsCurrentOnDemandSpend ::
      !( Maybe
           Text
       ),
    _spprsHourlyCommitmentToPurchase ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'SavingsPlansPurchaseRecommendationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spprsCurrencyCode' - The currency code AWS used to generate the recommendations and present potential savings.
--
-- * 'spprsDailyCommitmentToPurchase' - The recommended Savings Plans cost on a daily (24 hourly) basis.
--
-- * 'spprsEstimatedTotalCost' - The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
--
-- * 'spprsEstimatedROI' - The estimated return on investment based on the recommended Savings Plans and estimated savings.
--
-- * 'spprsEstimatedSavingsAmount' - The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
--
-- * 'spprsEstimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings Plans purchase.
--
-- * 'spprsEstimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
--
-- * 'spprsEstimatedSavingsPercentage' - The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
--
-- * 'spprsTotalRecommendationCount' - The aggregate number of Savings Plans recommendations that exist for your account.
--
-- * 'spprsCurrentOnDemandSpend' - The current total on demand spend of the applicable usage types over the lookback period.
--
-- * 'spprsHourlyCommitmentToPurchase' - The recommended hourly commitment based on the recommendation parameters.
savingsPlansPurchaseRecommendationSummary ::
  SavingsPlansPurchaseRecommendationSummary
savingsPlansPurchaseRecommendationSummary =
  SavingsPlansPurchaseRecommendationSummary'
    { _spprsCurrencyCode =
        Nothing,
      _spprsDailyCommitmentToPurchase = Nothing,
      _spprsEstimatedTotalCost = Nothing,
      _spprsEstimatedROI = Nothing,
      _spprsEstimatedSavingsAmount = Nothing,
      _spprsEstimatedMonthlySavingsAmount = Nothing,
      _spprsEstimatedOnDemandCostWithCurrentCommitment =
        Nothing,
      _spprsEstimatedSavingsPercentage = Nothing,
      _spprsTotalRecommendationCount = Nothing,
      _spprsCurrentOnDemandSpend = Nothing,
      _spprsHourlyCommitmentToPurchase = Nothing
    }

-- | The currency code AWS used to generate the recommendations and present potential savings.
spprsCurrencyCode :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsCurrencyCode = lens _spprsCurrencyCode (\s a -> s {_spprsCurrencyCode = a})

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
spprsDailyCommitmentToPurchase :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsDailyCommitmentToPurchase = lens _spprsDailyCommitmentToPurchase (\s a -> s {_spprsDailyCommitmentToPurchase = a})

-- | The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
spprsEstimatedTotalCost :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedTotalCost = lens _spprsEstimatedTotalCost (\s a -> s {_spprsEstimatedTotalCost = a})

-- | The estimated return on investment based on the recommended Savings Plans and estimated savings.
spprsEstimatedROI :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedROI = lens _spprsEstimatedROI (\s a -> s {_spprsEstimatedROI = a})

-- | The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
spprsEstimatedSavingsAmount :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedSavingsAmount = lens _spprsEstimatedSavingsAmount (\s a -> s {_spprsEstimatedSavingsAmount = a})

-- | The estimated monthly savings amount, based on the recommended Savings Plans purchase.
spprsEstimatedMonthlySavingsAmount :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedMonthlySavingsAmount = lens _spprsEstimatedMonthlySavingsAmount (\s a -> s {_spprsEstimatedMonthlySavingsAmount = a})

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
spprsEstimatedOnDemandCostWithCurrentCommitment :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedOnDemandCostWithCurrentCommitment = lens _spprsEstimatedOnDemandCostWithCurrentCommitment (\s a -> s {_spprsEstimatedOnDemandCostWithCurrentCommitment = a})

-- | The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
spprsEstimatedSavingsPercentage :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsEstimatedSavingsPercentage = lens _spprsEstimatedSavingsPercentage (\s a -> s {_spprsEstimatedSavingsPercentage = a})

-- | The aggregate number of Savings Plans recommendations that exist for your account.
spprsTotalRecommendationCount :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsTotalRecommendationCount = lens _spprsTotalRecommendationCount (\s a -> s {_spprsTotalRecommendationCount = a})

-- | The current total on demand spend of the applicable usage types over the lookback period.
spprsCurrentOnDemandSpend :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsCurrentOnDemandSpend = lens _spprsCurrentOnDemandSpend (\s a -> s {_spprsCurrentOnDemandSpend = a})

-- | The recommended hourly commitment based on the recommendation parameters.
spprsHourlyCommitmentToPurchase :: Lens' SavingsPlansPurchaseRecommendationSummary (Maybe Text)
spprsHourlyCommitmentToPurchase = lens _spprsHourlyCommitmentToPurchase (\s a -> s {_spprsHourlyCommitmentToPurchase = a})

instance FromJSON SavingsPlansPurchaseRecommendationSummary where
  parseJSON =
    withObject
      "SavingsPlansPurchaseRecommendationSummary"
      ( \x ->
          SavingsPlansPurchaseRecommendationSummary'
            <$> (x .:? "CurrencyCode")
            <*> (x .:? "DailyCommitmentToPurchase")
            <*> (x .:? "EstimatedTotalCost")
            <*> (x .:? "EstimatedROI")
            <*> (x .:? "EstimatedSavingsAmount")
            <*> (x .:? "EstimatedMonthlySavingsAmount")
            <*> (x .:? "EstimatedOnDemandCostWithCurrentCommitment")
            <*> (x .:? "EstimatedSavingsPercentage")
            <*> (x .:? "TotalRecommendationCount")
            <*> (x .:? "CurrentOnDemandSpend")
            <*> (x .:? "HourlyCommitmentToPurchase")
      )

instance Hashable SavingsPlansPurchaseRecommendationSummary

instance NFData SavingsPlansPurchaseRecommendationSummary
