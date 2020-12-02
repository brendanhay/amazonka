{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail where

import Network.AWS.CostExplorer.Types.InstanceDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about your recommended reservation purchase.
--
--
--
-- /See:/ 'reservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { _rprdMaximumNormalizedUnitsUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdRecurringStandardMonthlyCost ::
      !( Maybe
           Text
       ),
    _rprdAverageNormalizedUnitsUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdCurrencyCode ::
      !( Maybe
           Text
       ),
    _rprdEstimatedMonthlySavingsPercentage ::
      !( Maybe
           Text
       ),
    _rprdRecommendedNormalizedUnitsToPurchase ::
      !( Maybe
           Text
       ),
    _rprdAverageUtilization ::
      !( Maybe
           Text
       ),
    _rprdAccountId ::
      !( Maybe
           Text
       ),
    _rprdEstimatedMonthlySavingsAmount ::
      !( Maybe
           Text
       ),
    _rprdUpfrontCost ::
      !( Maybe
           Text
       ),
    _rprdMinimumNormalizedUnitsUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdEstimatedMonthlyOnDemandCost ::
      !( Maybe
           Text
       ),
    _rprdRecommendedNumberOfInstancesToPurchase ::
      !( Maybe
           Text
       ),
    _rprdMaximumNumberOfInstancesUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdEstimatedReservationCostForLookbackPeriod ::
      !( Maybe
           Text
       ),
    _rprdInstanceDetails ::
      !( Maybe
           InstanceDetails
       ),
    _rprdAverageNumberOfInstancesUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdMinimumNumberOfInstancesUsedPerHour ::
      !( Maybe
           Text
       ),
    _rprdEstimatedBreakEvenInMonths ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationPurchaseRecommendationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprdMaximumNormalizedUnitsUsedPerHour' - The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdRecurringStandardMonthlyCost' - How much purchasing this instance costs you on a monthly basis.
--
-- * 'rprdAverageNormalizedUnitsUsedPerHour' - The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdCurrencyCode' - The currency code that AWS used to calculate the costs for this instance.
--
-- * 'rprdEstimatedMonthlySavingsPercentage' - How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
--
-- * 'rprdRecommendedNormalizedUnitsToPurchase' - The number of normalized units that AWS recommends that you purchase.
--
-- * 'rprdAverageUtilization' - The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdAccountId' - The account that this RI recommendation is for.
--
-- * 'rprdEstimatedMonthlySavingsAmount' - How much AWS estimates that this specific recommendation could save you in a month.
--
-- * 'rprdUpfrontCost' - How much purchasing this instance costs you upfront.
--
-- * 'rprdMinimumNormalizedUnitsUsedPerHour' - The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedMonthlyOnDemandCost' - How much AWS estimates that you spend on On-Demand Instances in a month.
--
-- * 'rprdRecommendedNumberOfInstancesToPurchase' - The number of instances that AWS recommends that you purchase.
--
-- * 'rprdMaximumNumberOfInstancesUsedPerHour' - The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedReservationCostForLookbackPeriod' - How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
--
-- * 'rprdInstanceDetails' - Details about the instances that AWS recommends that you purchase.
--
-- * 'rprdAverageNumberOfInstancesUsedPerHour' - The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdMinimumNumberOfInstancesUsedPerHour' - The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedBreakEvenInMonths' - How long AWS estimates that it takes for this instance to start saving you money, in months.
reservationPurchaseRecommendationDetail ::
  ReservationPurchaseRecommendationDetail
reservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { _rprdMaximumNormalizedUnitsUsedPerHour =
        Nothing,
      _rprdRecurringStandardMonthlyCost = Nothing,
      _rprdAverageNormalizedUnitsUsedPerHour = Nothing,
      _rprdCurrencyCode = Nothing,
      _rprdEstimatedMonthlySavingsPercentage = Nothing,
      _rprdRecommendedNormalizedUnitsToPurchase = Nothing,
      _rprdAverageUtilization = Nothing,
      _rprdAccountId = Nothing,
      _rprdEstimatedMonthlySavingsAmount = Nothing,
      _rprdUpfrontCost = Nothing,
      _rprdMinimumNormalizedUnitsUsedPerHour = Nothing,
      _rprdEstimatedMonthlyOnDemandCost = Nothing,
      _rprdRecommendedNumberOfInstancesToPurchase = Nothing,
      _rprdMaximumNumberOfInstancesUsedPerHour = Nothing,
      _rprdEstimatedReservationCostForLookbackPeriod =
        Nothing,
      _rprdInstanceDetails = Nothing,
      _rprdAverageNumberOfInstancesUsedPerHour = Nothing,
      _rprdMinimumNumberOfInstancesUsedPerHour = Nothing,
      _rprdEstimatedBreakEvenInMonths = Nothing
    }

-- | The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMaximumNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMaximumNormalizedUnitsUsedPerHour = lens _rprdMaximumNormalizedUnitsUsedPerHour (\s a -> s {_rprdMaximumNormalizedUnitsUsedPerHour = a})

-- | How much purchasing this instance costs you on a monthly basis.
rprdRecurringStandardMonthlyCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecurringStandardMonthlyCost = lens _rprdRecurringStandardMonthlyCost (\s a -> s {_rprdRecurringStandardMonthlyCost = a})

-- | The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdAverageNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageNormalizedUnitsUsedPerHour = lens _rprdAverageNormalizedUnitsUsedPerHour (\s a -> s {_rprdAverageNormalizedUnitsUsedPerHour = a})

-- | The currency code that AWS used to calculate the costs for this instance.
rprdCurrencyCode :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdCurrencyCode = lens _rprdCurrencyCode (\s a -> s {_rprdCurrencyCode = a})

-- | How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
rprdEstimatedMonthlySavingsPercentage :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlySavingsPercentage = lens _rprdEstimatedMonthlySavingsPercentage (\s a -> s {_rprdEstimatedMonthlySavingsPercentage = a})

-- | The number of normalized units that AWS recommends that you purchase.
rprdRecommendedNormalizedUnitsToPurchase :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecommendedNormalizedUnitsToPurchase = lens _rprdRecommendedNormalizedUnitsToPurchase (\s a -> s {_rprdRecommendedNormalizedUnitsToPurchase = a})

-- | The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
rprdAverageUtilization :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageUtilization = lens _rprdAverageUtilization (\s a -> s {_rprdAverageUtilization = a})

-- | The account that this RI recommendation is for.
rprdAccountId :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAccountId = lens _rprdAccountId (\s a -> s {_rprdAccountId = a})

-- | How much AWS estimates that this specific recommendation could save you in a month.
rprdEstimatedMonthlySavingsAmount :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlySavingsAmount = lens _rprdEstimatedMonthlySavingsAmount (\s a -> s {_rprdEstimatedMonthlySavingsAmount = a})

-- | How much purchasing this instance costs you upfront.
rprdUpfrontCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdUpfrontCost = lens _rprdUpfrontCost (\s a -> s {_rprdUpfrontCost = a})

-- | The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMinimumNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMinimumNormalizedUnitsUsedPerHour = lens _rprdMinimumNormalizedUnitsUsedPerHour (\s a -> s {_rprdMinimumNormalizedUnitsUsedPerHour = a})

-- | How much AWS estimates that you spend on On-Demand Instances in a month.
rprdEstimatedMonthlyOnDemandCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlyOnDemandCost = lens _rprdEstimatedMonthlyOnDemandCost (\s a -> s {_rprdEstimatedMonthlyOnDemandCost = a})

-- | The number of instances that AWS recommends that you purchase.
rprdRecommendedNumberOfInstancesToPurchase :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecommendedNumberOfInstancesToPurchase = lens _rprdRecommendedNumberOfInstancesToPurchase (\s a -> s {_rprdRecommendedNumberOfInstancesToPurchase = a})

-- | The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMaximumNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMaximumNumberOfInstancesUsedPerHour = lens _rprdMaximumNumberOfInstancesUsedPerHour (\s a -> s {_rprdMaximumNumberOfInstancesUsedPerHour = a})

-- | How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
rprdEstimatedReservationCostForLookbackPeriod :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedReservationCostForLookbackPeriod = lens _rprdEstimatedReservationCostForLookbackPeriod (\s a -> s {_rprdEstimatedReservationCostForLookbackPeriod = a})

-- | Details about the instances that AWS recommends that you purchase.
rprdInstanceDetails :: Lens' ReservationPurchaseRecommendationDetail (Maybe InstanceDetails)
rprdInstanceDetails = lens _rprdInstanceDetails (\s a -> s {_rprdInstanceDetails = a})

-- | The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdAverageNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageNumberOfInstancesUsedPerHour = lens _rprdAverageNumberOfInstancesUsedPerHour (\s a -> s {_rprdAverageNumberOfInstancesUsedPerHour = a})

-- | The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMinimumNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMinimumNumberOfInstancesUsedPerHour = lens _rprdMinimumNumberOfInstancesUsedPerHour (\s a -> s {_rprdMinimumNumberOfInstancesUsedPerHour = a})

-- | How long AWS estimates that it takes for this instance to start saving you money, in months.
rprdEstimatedBreakEvenInMonths :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedBreakEvenInMonths = lens _rprdEstimatedBreakEvenInMonths (\s a -> s {_rprdEstimatedBreakEvenInMonths = a})

instance FromJSON ReservationPurchaseRecommendationDetail where
  parseJSON =
    withObject
      "ReservationPurchaseRecommendationDetail"
      ( \x ->
          ReservationPurchaseRecommendationDetail'
            <$> (x .:? "MaximumNormalizedUnitsUsedPerHour")
            <*> (x .:? "RecurringStandardMonthlyCost")
            <*> (x .:? "AverageNormalizedUnitsUsedPerHour")
            <*> (x .:? "CurrencyCode")
            <*> (x .:? "EstimatedMonthlySavingsPercentage")
            <*> (x .:? "RecommendedNormalizedUnitsToPurchase")
            <*> (x .:? "AverageUtilization")
            <*> (x .:? "AccountId")
            <*> (x .:? "EstimatedMonthlySavingsAmount")
            <*> (x .:? "UpfrontCost")
            <*> (x .:? "MinimumNormalizedUnitsUsedPerHour")
            <*> (x .:? "EstimatedMonthlyOnDemandCost")
            <*> (x .:? "RecommendedNumberOfInstancesToPurchase")
            <*> (x .:? "MaximumNumberOfInstancesUsedPerHour")
            <*> (x .:? "EstimatedReservationCostForLookbackPeriod")
            <*> (x .:? "InstanceDetails")
            <*> (x .:? "AverageNumberOfInstancesUsedPerHour")
            <*> (x .:? "MinimumNumberOfInstancesUsedPerHour")
            <*> (x .:? "EstimatedBreakEvenInMonths")
      )

instance Hashable ReservationPurchaseRecommendationDetail

instance NFData ReservationPurchaseRecommendationDetail
