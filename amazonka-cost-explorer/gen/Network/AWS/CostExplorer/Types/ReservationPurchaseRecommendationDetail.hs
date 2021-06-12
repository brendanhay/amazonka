{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.InstanceDetails
import qualified Network.AWS.Lens as Lens

-- | Details about your recommended reservation purchase.
--
-- /See:/ 'newReservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { -- | How much purchasing this instance costs you upfront.
    upfrontCost :: Core.Maybe Core.Text,
    -- | The account that this RI recommendation is for.
    accountId :: Core.Maybe Core.Text,
    -- | How much AWS estimates that this specific recommendation could save you
    -- in a month.
    estimatedMonthlySavingsAmount :: Core.Maybe Core.Text,
    -- | How much purchasing this instance costs you on a monthly basis.
    recurringStandardMonthlyCost :: Core.Maybe Core.Text,
    -- | The number of normalized units that AWS recommends that you purchase.
    recommendedNormalizedUnitsToPurchase :: Core.Maybe Core.Text,
    -- | The average utilization of your instances. AWS uses this to calculate
    -- your recommended reservation purchases.
    averageUtilization :: Core.Maybe Core.Text,
    -- | The average number of instances that you used in an hour during the
    -- historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    averageNumberOfInstancesUsedPerHour :: Core.Maybe Core.Text,
    -- | How much AWS estimates that you would have spent for all usage during
    -- the specified historical period if you had a reservation.
    estimatedReservationCostForLookbackPeriod :: Core.Maybe Core.Text,
    -- | Details about the instances that AWS recommends that you purchase.
    instanceDetails :: Core.Maybe InstanceDetails,
    -- | The maximum number of instances that you used in an hour during the
    -- historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    maximumNumberOfInstancesUsedPerHour :: Core.Maybe Core.Text,
    -- | The number of instances that AWS recommends that you purchase.
    recommendedNumberOfInstancesToPurchase :: Core.Maybe Core.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Core.Maybe Core.Text,
    -- | The minimum number of normalized units that you used in an hour during
    -- the historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    minimumNormalizedUnitsUsedPerHour :: Core.Maybe Core.Text,
    -- | The average number of normalized units that you used in an hour during
    -- the historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    averageNormalizedUnitsUsedPerHour :: Core.Maybe Core.Text,
    -- | The maximum number of normalized units that you used in an hour during
    -- the historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    maximumNormalizedUnitsUsedPerHour :: Core.Maybe Core.Text,
    -- | How long AWS estimates that it takes for this instance to start saving
    -- you money, in months.
    estimatedBreakEvenInMonths :: Core.Maybe Core.Text,
    -- | The minimum number of instances that you used in an hour during the
    -- historical period. AWS uses this to calculate your recommended
    -- reservation purchases.
    minimumNumberOfInstancesUsedPerHour :: Core.Maybe Core.Text,
    -- | How much AWS estimates that this specific recommendation could save you
    -- in a month, as a percentage of your overall costs.
    estimatedMonthlySavingsPercentage :: Core.Maybe Core.Text,
    -- | How much AWS estimates that you spend on On-Demand Instances in a month.
    estimatedMonthlyOnDemandCost :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upfrontCost', 'reservationPurchaseRecommendationDetail_upfrontCost' - How much purchasing this instance costs you upfront.
--
-- 'accountId', 'reservationPurchaseRecommendationDetail_accountId' - The account that this RI recommendation is for.
--
-- 'estimatedMonthlySavingsAmount', 'reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount' - How much AWS estimates that this specific recommendation could save you
-- in a month.
--
-- 'recurringStandardMonthlyCost', 'reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost' - How much purchasing this instance costs you on a monthly basis.
--
-- 'recommendedNormalizedUnitsToPurchase', 'reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase' - The number of normalized units that AWS recommends that you purchase.
--
-- 'averageUtilization', 'reservationPurchaseRecommendationDetail_averageUtilization' - The average utilization of your instances. AWS uses this to calculate
-- your recommended reservation purchases.
--
-- 'averageNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour' - The average number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'estimatedReservationCostForLookbackPeriod', 'reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod' - How much AWS estimates that you would have spent for all usage during
-- the specified historical period if you had a reservation.
--
-- 'instanceDetails', 'reservationPurchaseRecommendationDetail_instanceDetails' - Details about the instances that AWS recommends that you purchase.
--
-- 'maximumNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour' - The maximum number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'recommendedNumberOfInstancesToPurchase', 'reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase' - The number of instances that AWS recommends that you purchase.
--
-- 'currencyCode', 'reservationPurchaseRecommendationDetail_currencyCode' - The currency code that AWS used to calculate the costs for this
-- instance.
--
-- 'minimumNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour' - The minimum number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'averageNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour' - The average number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'maximumNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour' - The maximum number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'estimatedBreakEvenInMonths', 'reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths' - How long AWS estimates that it takes for this instance to start saving
-- you money, in months.
--
-- 'minimumNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour' - The minimum number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
--
-- 'estimatedMonthlySavingsPercentage', 'reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage' - How much AWS estimates that this specific recommendation could save you
-- in a month, as a percentage of your overall costs.
--
-- 'estimatedMonthlyOnDemandCost', 'reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost' - How much AWS estimates that you spend on On-Demand Instances in a month.
newReservationPurchaseRecommendationDetail ::
  ReservationPurchaseRecommendationDetail
newReservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { upfrontCost =
        Core.Nothing,
      accountId = Core.Nothing,
      estimatedMonthlySavingsAmount =
        Core.Nothing,
      recurringStandardMonthlyCost =
        Core.Nothing,
      recommendedNormalizedUnitsToPurchase =
        Core.Nothing,
      averageUtilization = Core.Nothing,
      averageNumberOfInstancesUsedPerHour =
        Core.Nothing,
      estimatedReservationCostForLookbackPeriod =
        Core.Nothing,
      instanceDetails = Core.Nothing,
      maximumNumberOfInstancesUsedPerHour =
        Core.Nothing,
      recommendedNumberOfInstancesToPurchase =
        Core.Nothing,
      currencyCode = Core.Nothing,
      minimumNormalizedUnitsUsedPerHour =
        Core.Nothing,
      averageNormalizedUnitsUsedPerHour =
        Core.Nothing,
      maximumNormalizedUnitsUsedPerHour =
        Core.Nothing,
      estimatedBreakEvenInMonths =
        Core.Nothing,
      minimumNumberOfInstancesUsedPerHour =
        Core.Nothing,
      estimatedMonthlySavingsPercentage =
        Core.Nothing,
      estimatedMonthlyOnDemandCost =
        Core.Nothing
    }

-- | How much purchasing this instance costs you upfront.
reservationPurchaseRecommendationDetail_upfrontCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_upfrontCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {upfrontCost} -> upfrontCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {upfrontCost = a} :: ReservationPurchaseRecommendationDetail)

-- | The account that this RI recommendation is for.
reservationPurchaseRecommendationDetail_accountId :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_accountId = Lens.lens (\ReservationPurchaseRecommendationDetail' {accountId} -> accountId) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {accountId = a} :: ReservationPurchaseRecommendationDetail)

-- | How much AWS estimates that this specific recommendation could save you
-- in a month.
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationDetail)

-- | How much purchasing this instance costs you on a monthly basis.
reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {recurringStandardMonthlyCost} -> recurringStandardMonthlyCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recurringStandardMonthlyCost = a} :: ReservationPurchaseRecommendationDetail)

-- | The number of normalized units that AWS recommends that you purchase.
reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase = Lens.lens (\ReservationPurchaseRecommendationDetail' {recommendedNormalizedUnitsToPurchase} -> recommendedNormalizedUnitsToPurchase) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recommendedNormalizedUnitsToPurchase = a} :: ReservationPurchaseRecommendationDetail)

-- | The average utilization of your instances. AWS uses this to calculate
-- your recommended reservation purchases.
reservationPurchaseRecommendationDetail_averageUtilization :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_averageUtilization = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageUtilization} -> averageUtilization) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageUtilization = a} :: ReservationPurchaseRecommendationDetail)

-- | The average number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageNumberOfInstancesUsedPerHour} -> averageNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much AWS estimates that you would have spent for all usage during
-- the specified historical period if you had a reservation.
reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedReservationCostForLookbackPeriod} -> estimatedReservationCostForLookbackPeriod) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedReservationCostForLookbackPeriod = a} :: ReservationPurchaseRecommendationDetail)

-- | Details about the instances that AWS recommends that you purchase.
reservationPurchaseRecommendationDetail_instanceDetails :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe InstanceDetails)
reservationPurchaseRecommendationDetail_instanceDetails = Lens.lens (\ReservationPurchaseRecommendationDetail' {instanceDetails} -> instanceDetails) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {instanceDetails = a} :: ReservationPurchaseRecommendationDetail)

-- | The maximum number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {maximumNumberOfInstancesUsedPerHour} -> maximumNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {maximumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | The number of instances that AWS recommends that you purchase.
reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase = Lens.lens (\ReservationPurchaseRecommendationDetail' {recommendedNumberOfInstancesToPurchase} -> recommendedNumberOfInstancesToPurchase) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recommendedNumberOfInstancesToPurchase = a} :: ReservationPurchaseRecommendationDetail)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
reservationPurchaseRecommendationDetail_currencyCode :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_currencyCode = Lens.lens (\ReservationPurchaseRecommendationDetail' {currencyCode} -> currencyCode) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {currencyCode = a} :: ReservationPurchaseRecommendationDetail)

-- | The minimum number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {minimumNormalizedUnitsUsedPerHour} -> minimumNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {minimumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | The average number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageNormalizedUnitsUsedPerHour} -> averageNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | The maximum number of normalized units that you used in an hour during
-- the historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {maximumNormalizedUnitsUsedPerHour} -> maximumNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {maximumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How long AWS estimates that it takes for this instance to start saving
-- you money, in months.
reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedBreakEvenInMonths} -> estimatedBreakEvenInMonths) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedBreakEvenInMonths = a} :: ReservationPurchaseRecommendationDetail)

-- | The minimum number of instances that you used in an hour during the
-- historical period. AWS uses this to calculate your recommended
-- reservation purchases.
reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {minimumNumberOfInstancesUsedPerHour} -> minimumNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {minimumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much AWS estimates that this specific recommendation could save you
-- in a month, as a percentage of your overall costs.
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlySavingsPercentage} -> estimatedMonthlySavingsPercentage) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationDetail)

-- | How much AWS estimates that you spend on On-Demand Instances in a month.
reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Core.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlyOnDemandCost} -> estimatedMonthlyOnDemandCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlyOnDemandCost = a} :: ReservationPurchaseRecommendationDetail)

instance
  Core.FromJSON
    ReservationPurchaseRecommendationDetail
  where
  parseJSON =
    Core.withObject
      "ReservationPurchaseRecommendationDetail"
      ( \x ->
          ReservationPurchaseRecommendationDetail'
            Core.<$> (x Core..:? "UpfrontCost")
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "EstimatedMonthlySavingsAmount")
            Core.<*> (x Core..:? "RecurringStandardMonthlyCost")
            Core.<*> (x Core..:? "RecommendedNormalizedUnitsToPurchase")
            Core.<*> (x Core..:? "AverageUtilization")
            Core.<*> (x Core..:? "AverageNumberOfInstancesUsedPerHour")
            Core.<*> ( x
                         Core..:? "EstimatedReservationCostForLookbackPeriod"
                     )
            Core.<*> (x Core..:? "InstanceDetails")
            Core.<*> (x Core..:? "MaximumNumberOfInstancesUsedPerHour")
            Core.<*> (x Core..:? "RecommendedNumberOfInstancesToPurchase")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> (x Core..:? "MinimumNormalizedUnitsUsedPerHour")
            Core.<*> (x Core..:? "AverageNormalizedUnitsUsedPerHour")
            Core.<*> (x Core..:? "MaximumNormalizedUnitsUsedPerHour")
            Core.<*> (x Core..:? "EstimatedBreakEvenInMonths")
            Core.<*> (x Core..:? "MinimumNumberOfInstancesUsedPerHour")
            Core.<*> (x Core..:? "EstimatedMonthlySavingsPercentage")
            Core.<*> (x Core..:? "EstimatedMonthlyOnDemandCost")
      )

instance
  Core.Hashable
    ReservationPurchaseRecommendationDetail

instance
  Core.NFData
    ReservationPurchaseRecommendationDetail
