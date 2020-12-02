{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail where

import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for your recommended Savings Plans.
--
--
--
-- /See:/ 'savingsPlansPurchaseRecommendationDetail' smart constructor.
data SavingsPlansPurchaseRecommendationDetail = SavingsPlansPurchaseRecommendationDetail'
  { _spprdCurrencyCode ::
      !( Maybe
           Text
       ),
    _spprdCurrentAverageHourlyOnDemandSpend ::
      !( Maybe
           Text
       ),
    _spprdSavingsPlansDetails ::
      !( Maybe
           SavingsPlansDetails
       ),
    _spprdCurrentMinimumHourlyOnDemandSpend ::
      !( Maybe
           Text
       ),
    _spprdEstimatedROI ::
      !( Maybe
           Text
       ),
    _spprdCurrentMaximumHourlyOnDemandSpend ::
      !( Maybe
           Text
       ),
    _spprdEstimatedSavingsAmount ::
      !( Maybe
           Text
       ),
    _spprdAccountId ::
      !( Maybe
           Text
       ),
    _spprdEstimatedMonthlySavingsAmount ::
      !( Maybe
           Text
       ),
    _spprdEstimatedOnDemandCost ::
      !( Maybe
           Text
       ),
    _spprdEstimatedOnDemandCostWithCurrentCommitment ::
      !( Maybe
           Text
       ),
    _spprdUpfrontCost ::
      !( Maybe
           Text
       ),
    _spprdEstimatedSPCost ::
      !( Maybe
           Text
       ),
    _spprdEstimatedSavingsPercentage ::
      !( Maybe
           Text
       ),
    _spprdEstimatedAverageUtilization ::
      !( Maybe
           Text
       ),
    _spprdHourlyCommitmentToPurchase ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SavingsPlansPurchaseRecommendationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spprdCurrencyCode' - The currency code AWS used to generate the recommendations and present potential savings.
--
-- * 'spprdCurrentAverageHourlyOnDemandSpend' - The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- * 'spprdSavingsPlansDetails' - Details for your recommended Savings Plans.
--
-- * 'spprdCurrentMinimumHourlyOnDemandSpend' - The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- * 'spprdEstimatedROI' - The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
--
-- * 'spprdCurrentMaximumHourlyOnDemandSpend' - The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- * 'spprdEstimatedSavingsAmount' - The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
--
-- * 'spprdAccountId' - The @AccountID@ the recommendation is generated for.
--
-- * 'spprdEstimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings Plans.
--
-- * 'spprdEstimatedOnDemandCost' - The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
--
-- * 'spprdEstimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
--
-- * 'spprdUpfrontCost' - The upfront cost of the recommended Savings Plans, based on the selected payment option.
--
-- * 'spprdEstimatedSPCost' - The cost of the recommended Savings Plans over the length of the lookback period.
--
-- * 'spprdEstimatedSavingsPercentage' - The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
--
-- * 'spprdEstimatedAverageUtilization' - The estimated utilization of the recommended Savings Plans.
--
-- * 'spprdHourlyCommitmentToPurchase' - The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
savingsPlansPurchaseRecommendationDetail ::
  SavingsPlansPurchaseRecommendationDetail
savingsPlansPurchaseRecommendationDetail =
  SavingsPlansPurchaseRecommendationDetail'
    { _spprdCurrencyCode =
        Nothing,
      _spprdCurrentAverageHourlyOnDemandSpend = Nothing,
      _spprdSavingsPlansDetails = Nothing,
      _spprdCurrentMinimumHourlyOnDemandSpend = Nothing,
      _spprdEstimatedROI = Nothing,
      _spprdCurrentMaximumHourlyOnDemandSpend = Nothing,
      _spprdEstimatedSavingsAmount = Nothing,
      _spprdAccountId = Nothing,
      _spprdEstimatedMonthlySavingsAmount = Nothing,
      _spprdEstimatedOnDemandCost = Nothing,
      _spprdEstimatedOnDemandCostWithCurrentCommitment =
        Nothing,
      _spprdUpfrontCost = Nothing,
      _spprdEstimatedSPCost = Nothing,
      _spprdEstimatedSavingsPercentage = Nothing,
      _spprdEstimatedAverageUtilization = Nothing,
      _spprdHourlyCommitmentToPurchase = Nothing
    }

-- | The currency code AWS used to generate the recommendations and present potential savings.
spprdCurrencyCode :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdCurrencyCode = lens _spprdCurrencyCode (\s a -> s {_spprdCurrencyCode = a})

-- | The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
spprdCurrentAverageHourlyOnDemandSpend :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdCurrentAverageHourlyOnDemandSpend = lens _spprdCurrentAverageHourlyOnDemandSpend (\s a -> s {_spprdCurrentAverageHourlyOnDemandSpend = a})

-- | Details for your recommended Savings Plans.
spprdSavingsPlansDetails :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe SavingsPlansDetails)
spprdSavingsPlansDetails = lens _spprdSavingsPlansDetails (\s a -> s {_spprdSavingsPlansDetails = a})

-- | The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
spprdCurrentMinimumHourlyOnDemandSpend :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdCurrentMinimumHourlyOnDemandSpend = lens _spprdCurrentMinimumHourlyOnDemandSpend (\s a -> s {_spprdCurrentMinimumHourlyOnDemandSpend = a})

-- | The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
spprdEstimatedROI :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedROI = lens _spprdEstimatedROI (\s a -> s {_spprdEstimatedROI = a})

-- | The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
spprdCurrentMaximumHourlyOnDemandSpend :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdCurrentMaximumHourlyOnDemandSpend = lens _spprdCurrentMaximumHourlyOnDemandSpend (\s a -> s {_spprdCurrentMaximumHourlyOnDemandSpend = a})

-- | The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
spprdEstimatedSavingsAmount :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedSavingsAmount = lens _spprdEstimatedSavingsAmount (\s a -> s {_spprdEstimatedSavingsAmount = a})

-- | The @AccountID@ the recommendation is generated for.
spprdAccountId :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdAccountId = lens _spprdAccountId (\s a -> s {_spprdAccountId = a})

-- | The estimated monthly savings amount, based on the recommended Savings Plans.
spprdEstimatedMonthlySavingsAmount :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedMonthlySavingsAmount = lens _spprdEstimatedMonthlySavingsAmount (\s a -> s {_spprdEstimatedMonthlySavingsAmount = a})

-- | The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
spprdEstimatedOnDemandCost :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedOnDemandCost = lens _spprdEstimatedOnDemandCost (\s a -> s {_spprdEstimatedOnDemandCost = a})

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
spprdEstimatedOnDemandCostWithCurrentCommitment :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedOnDemandCostWithCurrentCommitment = lens _spprdEstimatedOnDemandCostWithCurrentCommitment (\s a -> s {_spprdEstimatedOnDemandCostWithCurrentCommitment = a})

-- | The upfront cost of the recommended Savings Plans, based on the selected payment option.
spprdUpfrontCost :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdUpfrontCost = lens _spprdUpfrontCost (\s a -> s {_spprdUpfrontCost = a})

-- | The cost of the recommended Savings Plans over the length of the lookback period.
spprdEstimatedSPCost :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedSPCost = lens _spprdEstimatedSPCost (\s a -> s {_spprdEstimatedSPCost = a})

-- | The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
spprdEstimatedSavingsPercentage :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedSavingsPercentage = lens _spprdEstimatedSavingsPercentage (\s a -> s {_spprdEstimatedSavingsPercentage = a})

-- | The estimated utilization of the recommended Savings Plans.
spprdEstimatedAverageUtilization :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdEstimatedAverageUtilization = lens _spprdEstimatedAverageUtilization (\s a -> s {_spprdEstimatedAverageUtilization = a})

-- | The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
spprdHourlyCommitmentToPurchase :: Lens' SavingsPlansPurchaseRecommendationDetail (Maybe Text)
spprdHourlyCommitmentToPurchase = lens _spprdHourlyCommitmentToPurchase (\s a -> s {_spprdHourlyCommitmentToPurchase = a})

instance FromJSON SavingsPlansPurchaseRecommendationDetail where
  parseJSON =
    withObject
      "SavingsPlansPurchaseRecommendationDetail"
      ( \x ->
          SavingsPlansPurchaseRecommendationDetail'
            <$> (x .:? "CurrencyCode")
            <*> (x .:? "CurrentAverageHourlyOnDemandSpend")
            <*> (x .:? "SavingsPlansDetails")
            <*> (x .:? "CurrentMinimumHourlyOnDemandSpend")
            <*> (x .:? "EstimatedROI")
            <*> (x .:? "CurrentMaximumHourlyOnDemandSpend")
            <*> (x .:? "EstimatedSavingsAmount")
            <*> (x .:? "AccountId")
            <*> (x .:? "EstimatedMonthlySavingsAmount")
            <*> (x .:? "EstimatedOnDemandCost")
            <*> (x .:? "EstimatedOnDemandCostWithCurrentCommitment")
            <*> (x .:? "UpfrontCost")
            <*> (x .:? "EstimatedSPCost")
            <*> (x .:? "EstimatedSavingsPercentage")
            <*> (x .:? "EstimatedAverageUtilization")
            <*> (x .:? "HourlyCommitmentToPurchase")
      )

instance Hashable SavingsPlansPurchaseRecommendationDetail

instance NFData SavingsPlansPurchaseRecommendationDetail
