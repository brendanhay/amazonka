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
-- Module      : Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.InstanceDetails
import qualified Amazonka.Prelude as Prelude

-- | Details about your recommended reservation purchase.
--
-- /See:/ 'newReservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { -- | The number of instances that Amazon Web Services recommends that you
    -- purchase.
    recommendedNumberOfInstancesToPurchase :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances that you used in an hour during the
    -- historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    maximumNumberOfInstancesUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | The average number of instances that you used in an hour during the
    -- historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    averageNumberOfInstancesUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | How much Amazon Web Services estimates that this specific recommendation
    -- might save you in a month.
    estimatedMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The minimum number of instances that you used in an hour during the
    -- historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    minimumNumberOfInstancesUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | How much purchasing this instance costs you on a monthly basis.
    recurringStandardMonthlyCost :: Prelude.Maybe Prelude.Text,
    -- | Details about the instances that Amazon Web Services recommends that you
    -- purchase.
    instanceDetails :: Prelude.Maybe InstanceDetails,
    -- | How much purchasing this instance costs you upfront.
    upfrontCost :: Prelude.Maybe Prelude.Text,
    -- | How much Amazon Web Services estimates that you spend on On-Demand
    -- Instances in a month.
    estimatedMonthlyOnDemandCost :: Prelude.Maybe Prelude.Text,
    -- | The average utilization of your instances. Amazon Web Services uses this
    -- to calculate your recommended reservation purchases.
    averageUtilization :: Prelude.Maybe Prelude.Text,
    -- | How long Amazon Web Services estimates that it takes for this instance
    -- to start saving you money, in months.
    estimatedBreakEvenInMonths :: Prelude.Maybe Prelude.Text,
    -- | The currency code that Amazon Web Services used to calculate the costs
    -- for this instance.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The account that this Reserved Instance (RI) recommendation is for.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of normalized units that you used in an hour during
    -- the historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    maximumNormalizedUnitsUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | How much Amazon Web Services estimates that you might spend for all
    -- usage during the specified historical period if you had a reservation.
    estimatedReservationCostForLookbackPeriod :: Prelude.Maybe Prelude.Text,
    -- | The average number of normalized units that you used in an hour during
    -- the historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    averageNormalizedUnitsUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | The number of normalized units that Amazon Web Services recommends that
    -- you purchase.
    recommendedNormalizedUnitsToPurchase :: Prelude.Maybe Prelude.Text,
    -- | The minimum number of normalized units that you used in an hour during
    -- the historical period. Amazon Web Services uses this to calculate your
    -- recommended reservation purchases.
    minimumNormalizedUnitsUsedPerHour :: Prelude.Maybe Prelude.Text,
    -- | How much Amazon Web Services estimates that this specific recommendation
    -- might save you in a month, as a percentage of your overall costs.
    estimatedMonthlySavingsPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedNumberOfInstancesToPurchase', 'reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase' - The number of instances that Amazon Web Services recommends that you
-- purchase.
--
-- 'maximumNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour' - The maximum number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'averageNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour' - The average number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'estimatedMonthlySavingsAmount', 'reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount' - How much Amazon Web Services estimates that this specific recommendation
-- might save you in a month.
--
-- 'minimumNumberOfInstancesUsedPerHour', 'reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour' - The minimum number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'recurringStandardMonthlyCost', 'reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost' - How much purchasing this instance costs you on a monthly basis.
--
-- 'instanceDetails', 'reservationPurchaseRecommendationDetail_instanceDetails' - Details about the instances that Amazon Web Services recommends that you
-- purchase.
--
-- 'upfrontCost', 'reservationPurchaseRecommendationDetail_upfrontCost' - How much purchasing this instance costs you upfront.
--
-- 'estimatedMonthlyOnDemandCost', 'reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost' - How much Amazon Web Services estimates that you spend on On-Demand
-- Instances in a month.
--
-- 'averageUtilization', 'reservationPurchaseRecommendationDetail_averageUtilization' - The average utilization of your instances. Amazon Web Services uses this
-- to calculate your recommended reservation purchases.
--
-- 'estimatedBreakEvenInMonths', 'reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths' - How long Amazon Web Services estimates that it takes for this instance
-- to start saving you money, in months.
--
-- 'currencyCode', 'reservationPurchaseRecommendationDetail_currencyCode' - The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
--
-- 'accountId', 'reservationPurchaseRecommendationDetail_accountId' - The account that this Reserved Instance (RI) recommendation is for.
--
-- 'maximumNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour' - The maximum number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'estimatedReservationCostForLookbackPeriod', 'reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod' - How much Amazon Web Services estimates that you might spend for all
-- usage during the specified historical period if you had a reservation.
--
-- 'averageNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour' - The average number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'recommendedNormalizedUnitsToPurchase', 'reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase' - The number of normalized units that Amazon Web Services recommends that
-- you purchase.
--
-- 'minimumNormalizedUnitsUsedPerHour', 'reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour' - The minimum number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
--
-- 'estimatedMonthlySavingsPercentage', 'reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage' - How much Amazon Web Services estimates that this specific recommendation
-- might save you in a month, as a percentage of your overall costs.
newReservationPurchaseRecommendationDetail ::
  ReservationPurchaseRecommendationDetail
newReservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { recommendedNumberOfInstancesToPurchase =
        Prelude.Nothing,
      maximumNumberOfInstancesUsedPerHour =
        Prelude.Nothing,
      averageNumberOfInstancesUsedPerHour =
        Prelude.Nothing,
      estimatedMonthlySavingsAmount =
        Prelude.Nothing,
      minimumNumberOfInstancesUsedPerHour =
        Prelude.Nothing,
      recurringStandardMonthlyCost =
        Prelude.Nothing,
      instanceDetails = Prelude.Nothing,
      upfrontCost = Prelude.Nothing,
      estimatedMonthlyOnDemandCost =
        Prelude.Nothing,
      averageUtilization =
        Prelude.Nothing,
      estimatedBreakEvenInMonths =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      accountId = Prelude.Nothing,
      maximumNormalizedUnitsUsedPerHour =
        Prelude.Nothing,
      estimatedReservationCostForLookbackPeriod =
        Prelude.Nothing,
      averageNormalizedUnitsUsedPerHour =
        Prelude.Nothing,
      recommendedNormalizedUnitsToPurchase =
        Prelude.Nothing,
      minimumNormalizedUnitsUsedPerHour =
        Prelude.Nothing,
      estimatedMonthlySavingsPercentage =
        Prelude.Nothing
    }

-- | The number of instances that Amazon Web Services recommends that you
-- purchase.
reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase = Lens.lens (\ReservationPurchaseRecommendationDetail' {recommendedNumberOfInstancesToPurchase} -> recommendedNumberOfInstancesToPurchase) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recommendedNumberOfInstancesToPurchase = a} :: ReservationPurchaseRecommendationDetail)

-- | The maximum number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {maximumNumberOfInstancesUsedPerHour} -> maximumNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {maximumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | The average number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageNumberOfInstancesUsedPerHour} -> averageNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much Amazon Web Services estimates that this specific recommendation
-- might save you in a month.
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationDetail)

-- | The minimum number of instances that you used in an hour during the
-- historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {minimumNumberOfInstancesUsedPerHour} -> minimumNumberOfInstancesUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {minimumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much purchasing this instance costs you on a monthly basis.
reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {recurringStandardMonthlyCost} -> recurringStandardMonthlyCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recurringStandardMonthlyCost = a} :: ReservationPurchaseRecommendationDetail)

-- | Details about the instances that Amazon Web Services recommends that you
-- purchase.
reservationPurchaseRecommendationDetail_instanceDetails :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe InstanceDetails)
reservationPurchaseRecommendationDetail_instanceDetails = Lens.lens (\ReservationPurchaseRecommendationDetail' {instanceDetails} -> instanceDetails) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {instanceDetails = a} :: ReservationPurchaseRecommendationDetail)

-- | How much purchasing this instance costs you upfront.
reservationPurchaseRecommendationDetail_upfrontCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_upfrontCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {upfrontCost} -> upfrontCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {upfrontCost = a} :: ReservationPurchaseRecommendationDetail)

-- | How much Amazon Web Services estimates that you spend on On-Demand
-- Instances in a month.
reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlyOnDemandCost} -> estimatedMonthlyOnDemandCost) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlyOnDemandCost = a} :: ReservationPurchaseRecommendationDetail)

-- | The average utilization of your instances. Amazon Web Services uses this
-- to calculate your recommended reservation purchases.
reservationPurchaseRecommendationDetail_averageUtilization :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_averageUtilization = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageUtilization} -> averageUtilization) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageUtilization = a} :: ReservationPurchaseRecommendationDetail)

-- | How long Amazon Web Services estimates that it takes for this instance
-- to start saving you money, in months.
reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedBreakEvenInMonths} -> estimatedBreakEvenInMonths) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedBreakEvenInMonths = a} :: ReservationPurchaseRecommendationDetail)

-- | The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
reservationPurchaseRecommendationDetail_currencyCode :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_currencyCode = Lens.lens (\ReservationPurchaseRecommendationDetail' {currencyCode} -> currencyCode) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {currencyCode = a} :: ReservationPurchaseRecommendationDetail)

-- | The account that this Reserved Instance (RI) recommendation is for.
reservationPurchaseRecommendationDetail_accountId :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_accountId = Lens.lens (\ReservationPurchaseRecommendationDetail' {accountId} -> accountId) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {accountId = a} :: ReservationPurchaseRecommendationDetail)

-- | The maximum number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {maximumNormalizedUnitsUsedPerHour} -> maximumNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {maximumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much Amazon Web Services estimates that you might spend for all
-- usage during the specified historical period if you had a reservation.
reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedReservationCostForLookbackPeriod} -> estimatedReservationCostForLookbackPeriod) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedReservationCostForLookbackPeriod = a} :: ReservationPurchaseRecommendationDetail)

-- | The average number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {averageNormalizedUnitsUsedPerHour} -> averageNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {averageNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | The number of normalized units that Amazon Web Services recommends that
-- you purchase.
reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase = Lens.lens (\ReservationPurchaseRecommendationDetail' {recommendedNormalizedUnitsToPurchase} -> recommendedNormalizedUnitsToPurchase) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {recommendedNormalizedUnitsToPurchase = a} :: ReservationPurchaseRecommendationDetail)

-- | The minimum number of normalized units that you used in an hour during
-- the historical period. Amazon Web Services uses this to calculate your
-- recommended reservation purchases.
reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour = Lens.lens (\ReservationPurchaseRecommendationDetail' {minimumNormalizedUnitsUsedPerHour} -> minimumNormalizedUnitsUsedPerHour) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {minimumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)

-- | How much Amazon Web Services estimates that this specific recommendation
-- might save you in a month, as a percentage of your overall costs.
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationDetail (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage = Lens.lens (\ReservationPurchaseRecommendationDetail' {estimatedMonthlySavingsPercentage} -> estimatedMonthlySavingsPercentage) (\s@ReservationPurchaseRecommendationDetail' {} a -> s {estimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationDetail)

instance
  Core.FromJSON
    ReservationPurchaseRecommendationDetail
  where
  parseJSON =
    Core.withObject
      "ReservationPurchaseRecommendationDetail"
      ( \x ->
          ReservationPurchaseRecommendationDetail'
            Prelude.<$> (x Core..:? "RecommendedNumberOfInstancesToPurchase")
            Prelude.<*> (x Core..:? "MaximumNumberOfInstancesUsedPerHour")
            Prelude.<*> (x Core..:? "AverageNumberOfInstancesUsedPerHour")
            Prelude.<*> (x Core..:? "EstimatedMonthlySavingsAmount")
            Prelude.<*> (x Core..:? "MinimumNumberOfInstancesUsedPerHour")
            Prelude.<*> (x Core..:? "RecurringStandardMonthlyCost")
            Prelude.<*> (x Core..:? "InstanceDetails")
            Prelude.<*> (x Core..:? "UpfrontCost")
            Prelude.<*> (x Core..:? "EstimatedMonthlyOnDemandCost")
            Prelude.<*> (x Core..:? "AverageUtilization")
            Prelude.<*> (x Core..:? "EstimatedBreakEvenInMonths")
            Prelude.<*> (x Core..:? "CurrencyCode")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "MaximumNormalizedUnitsUsedPerHour")
            Prelude.<*> ( x
                            Core..:? "EstimatedReservationCostForLookbackPeriod"
                        )
            Prelude.<*> (x Core..:? "AverageNormalizedUnitsUsedPerHour")
            Prelude.<*> (x Core..:? "RecommendedNormalizedUnitsToPurchase")
            Prelude.<*> (x Core..:? "MinimumNormalizedUnitsUsedPerHour")
            Prelude.<*> (x Core..:? "EstimatedMonthlySavingsPercentage")
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendationDetail
  where
  hashWithSalt
    _salt
    ReservationPurchaseRecommendationDetail' {..} =
      _salt
        `Prelude.hashWithSalt` recommendedNumberOfInstancesToPurchase
        `Prelude.hashWithSalt` maximumNumberOfInstancesUsedPerHour
        `Prelude.hashWithSalt` averageNumberOfInstancesUsedPerHour
        `Prelude.hashWithSalt` estimatedMonthlySavingsAmount
        `Prelude.hashWithSalt` minimumNumberOfInstancesUsedPerHour
        `Prelude.hashWithSalt` recurringStandardMonthlyCost
        `Prelude.hashWithSalt` instanceDetails
        `Prelude.hashWithSalt` upfrontCost
        `Prelude.hashWithSalt` estimatedMonthlyOnDemandCost
        `Prelude.hashWithSalt` averageUtilization
        `Prelude.hashWithSalt` estimatedBreakEvenInMonths
        `Prelude.hashWithSalt` currencyCode
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` maximumNormalizedUnitsUsedPerHour
        `Prelude.hashWithSalt` estimatedReservationCostForLookbackPeriod
        `Prelude.hashWithSalt` averageNormalizedUnitsUsedPerHour
        `Prelude.hashWithSalt` recommendedNormalizedUnitsToPurchase
        `Prelude.hashWithSalt` minimumNormalizedUnitsUsedPerHour
        `Prelude.hashWithSalt` estimatedMonthlySavingsPercentage

instance
  Prelude.NFData
    ReservationPurchaseRecommendationDetail
  where
  rnf ReservationPurchaseRecommendationDetail' {..} =
    Prelude.rnf recommendedNumberOfInstancesToPurchase
      `Prelude.seq` Prelude.rnf maximumNumberOfInstancesUsedPerHour
      `Prelude.seq` Prelude.rnf averageNumberOfInstancesUsedPerHour
      `Prelude.seq` Prelude.rnf estimatedMonthlySavingsAmount
      `Prelude.seq` Prelude.rnf minimumNumberOfInstancesUsedPerHour
      `Prelude.seq` Prelude.rnf recurringStandardMonthlyCost
      `Prelude.seq` Prelude.rnf instanceDetails
      `Prelude.seq` Prelude.rnf upfrontCost
      `Prelude.seq` Prelude.rnf estimatedMonthlyOnDemandCost
      `Prelude.seq` Prelude.rnf averageUtilization
      `Prelude.seq` Prelude.rnf estimatedBreakEvenInMonths
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf
        maximumNormalizedUnitsUsedPerHour
      `Prelude.seq` Prelude.rnf
        estimatedReservationCostForLookbackPeriod
      `Prelude.seq` Prelude.rnf
        averageNormalizedUnitsUsedPerHour
      `Prelude.seq` Prelude.rnf
        recommendedNormalizedUnitsToPurchase
      `Prelude.seq` Prelude.rnf
        minimumNormalizedUnitsUsedPerHour
      `Prelude.seq` Prelude.rnf
        estimatedMonthlySavingsPercentage
