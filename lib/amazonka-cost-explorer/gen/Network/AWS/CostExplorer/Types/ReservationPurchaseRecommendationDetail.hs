-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
  ( ReservationPurchaseRecommendationDetail (..),

    -- * Smart constructor
    mkReservationPurchaseRecommendationDetail,

    -- * Lenses
    rprdMaximumNormalizedUnitsUsedPerHour,
    rprdRecurringStandardMonthlyCost,
    rprdAverageNormalizedUnitsUsedPerHour,
    rprdCurrencyCode,
    rprdEstimatedMonthlySavingsPercentage,
    rprdRecommendedNormalizedUnitsToPurchase,
    rprdAverageUtilization,
    rprdAccountId,
    rprdEstimatedMonthlySavingsAmount,
    rprdUpfrontCost,
    rprdMinimumNormalizedUnitsUsedPerHour,
    rprdEstimatedMonthlyOnDemandCost,
    rprdRecommendedNumberOfInstancesToPurchase,
    rprdMaximumNumberOfInstancesUsedPerHour,
    rprdEstimatedReservationCostForLookbackPeriod,
    rprdInstanceDetails,
    rprdAverageNumberOfInstancesUsedPerHour,
    rprdMinimumNumberOfInstancesUsedPerHour,
    rprdEstimatedBreakEvenInMonths,
  )
where

import Network.AWS.CostExplorer.Types.InstanceDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about your recommended reservation purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { maximumNormalizedUnitsUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    recurringStandardMonthlyCost ::
      Lude.Maybe
        Lude.Text,
    averageNormalizedUnitsUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    currencyCode ::
      Lude.Maybe
        Lude.Text,
    estimatedMonthlySavingsPercentage ::
      Lude.Maybe
        Lude.Text,
    recommendedNormalizedUnitsToPurchase ::
      Lude.Maybe
        Lude.Text,
    averageUtilization ::
      Lude.Maybe
        Lude.Text,
    accountId ::
      Lude.Maybe
        Lude.Text,
    estimatedMonthlySavingsAmount ::
      Lude.Maybe
        Lude.Text,
    upfrontCost ::
      Lude.Maybe
        Lude.Text,
    minimumNormalizedUnitsUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    estimatedMonthlyOnDemandCost ::
      Lude.Maybe
        Lude.Text,
    recommendedNumberOfInstancesToPurchase ::
      Lude.Maybe
        Lude.Text,
    maximumNumberOfInstancesUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    estimatedReservationCostForLookbackPeriod ::
      Lude.Maybe
        Lude.Text,
    instanceDetails ::
      Lude.Maybe
        InstanceDetails,
    averageNumberOfInstancesUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    minimumNumberOfInstancesUsedPerHour ::
      Lude.Maybe
        Lude.Text,
    estimatedBreakEvenInMonths ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPurchaseRecommendationDetail' with the minimum fields required to make a request.
--
-- * 'accountId' - The account that this RI recommendation is for.
-- * 'averageNormalizedUnitsUsedPerHour' - The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'averageNumberOfInstancesUsedPerHour' - The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'averageUtilization' - The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
-- * 'currencyCode' - The currency code that AWS used to calculate the costs for this instance.
-- * 'estimatedBreakEvenInMonths' - How long AWS estimates that it takes for this instance to start saving you money, in months.
-- * 'estimatedMonthlyOnDemandCost' - How much AWS estimates that you spend on On-Demand Instances in a month.
-- * 'estimatedMonthlySavingsAmount' - How much AWS estimates that this specific recommendation could save you in a month.
-- * 'estimatedMonthlySavingsPercentage' - How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
-- * 'estimatedReservationCostForLookbackPeriod' - How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
-- * 'instanceDetails' - Details about the instances that AWS recommends that you purchase.
-- * 'maximumNormalizedUnitsUsedPerHour' - The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'maximumNumberOfInstancesUsedPerHour' - The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'minimumNormalizedUnitsUsedPerHour' - The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'minimumNumberOfInstancesUsedPerHour' - The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
-- * 'recommendedNormalizedUnitsToPurchase' - The number of normalized units that AWS recommends that you purchase.
-- * 'recommendedNumberOfInstancesToPurchase' - The number of instances that AWS recommends that you purchase.
-- * 'recurringStandardMonthlyCost' - How much purchasing this instance costs you on a monthly basis.
-- * 'upfrontCost' - How much purchasing this instance costs you upfront.
mkReservationPurchaseRecommendationDetail ::
  ReservationPurchaseRecommendationDetail
mkReservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { maximumNormalizedUnitsUsedPerHour =
        Lude.Nothing,
      recurringStandardMonthlyCost = Lude.Nothing,
      averageNormalizedUnitsUsedPerHour = Lude.Nothing,
      currencyCode = Lude.Nothing,
      estimatedMonthlySavingsPercentage = Lude.Nothing,
      recommendedNormalizedUnitsToPurchase = Lude.Nothing,
      averageUtilization = Lude.Nothing,
      accountId = Lude.Nothing,
      estimatedMonthlySavingsAmount = Lude.Nothing,
      upfrontCost = Lude.Nothing,
      minimumNormalizedUnitsUsedPerHour = Lude.Nothing,
      estimatedMonthlyOnDemandCost = Lude.Nothing,
      recommendedNumberOfInstancesToPurchase = Lude.Nothing,
      maximumNumberOfInstancesUsedPerHour = Lude.Nothing,
      estimatedReservationCostForLookbackPeriod =
        Lude.Nothing,
      instanceDetails = Lude.Nothing,
      averageNumberOfInstancesUsedPerHour = Lude.Nothing,
      minimumNumberOfInstancesUsedPerHour = Lude.Nothing,
      estimatedBreakEvenInMonths = Lude.Nothing
    }

-- | The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'maximumNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMaximumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdMaximumNormalizedUnitsUsedPerHour = Lens.lens (maximumNormalizedUnitsUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {maximumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdMaximumNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'maximumNormalizedUnitsUsedPerHour' instead." #-}

-- | How much purchasing this instance costs you on a monthly basis.
--
-- /Note:/ Consider using 'recurringStandardMonthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecurringStandardMonthlyCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdRecurringStandardMonthlyCost = Lens.lens (recurringStandardMonthlyCost :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {recurringStandardMonthlyCost = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdRecurringStandardMonthlyCost "Use generic-lens or generic-optics with 'recurringStandardMonthlyCost' instead." #-}

-- | The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdAverageNormalizedUnitsUsedPerHour = Lens.lens (averageNormalizedUnitsUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {averageNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdAverageNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'averageNormalizedUnitsUsedPerHour' instead." #-}

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdCurrencyCode :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdCurrencyCode = Lens.lens (currencyCode :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdEstimatedMonthlySavingsPercentage = Lens.lens (estimatedMonthlySavingsPercentage :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdEstimatedMonthlySavingsPercentage "Use generic-lens or generic-optics with 'estimatedMonthlySavingsPercentage' instead." #-}

-- | The number of normalized units that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'recommendedNormalizedUnitsToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecommendedNormalizedUnitsToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdRecommendedNormalizedUnitsToPurchase = Lens.lens (recommendedNormalizedUnitsToPurchase :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {recommendedNormalizedUnitsToPurchase = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdRecommendedNormalizedUnitsToPurchase "Use generic-lens or generic-optics with 'recommendedNormalizedUnitsToPurchase' instead." #-}

-- | The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageUtilization :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdAverageUtilization = Lens.lens (averageUtilization :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {averageUtilization = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdAverageUtilization "Use generic-lens or generic-optics with 'averageUtilization' instead." #-}

-- | The account that this RI recommendation is for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAccountId :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdAccountId = Lens.lens (accountId :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | How much AWS estimates that this specific recommendation could save you in a month.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdEstimatedMonthlySavingsAmount = Lens.lens (estimatedMonthlySavingsAmount :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead." #-}

-- | How much purchasing this instance costs you upfront.
--
-- /Note:/ Consider using 'upfrontCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdUpfrontCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdUpfrontCost = Lens.lens (upfrontCost :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {upfrontCost = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdUpfrontCost "Use generic-lens or generic-optics with 'upfrontCost' instead." #-}

-- | The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'minimumNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMinimumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdMinimumNormalizedUnitsUsedPerHour = Lens.lens (minimumNormalizedUnitsUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {minimumNormalizedUnitsUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdMinimumNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'minimumNormalizedUnitsUsedPerHour' instead." #-}

-- | How much AWS estimates that you spend on On-Demand Instances in a month.
--
-- /Note:/ Consider using 'estimatedMonthlyOnDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlyOnDemandCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdEstimatedMonthlyOnDemandCost = Lens.lens (estimatedMonthlyOnDemandCost :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlyOnDemandCost = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdEstimatedMonthlyOnDemandCost "Use generic-lens or generic-optics with 'estimatedMonthlyOnDemandCost' instead." #-}

-- | The number of instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'recommendedNumberOfInstancesToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecommendedNumberOfInstancesToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdRecommendedNumberOfInstancesToPurchase = Lens.lens (recommendedNumberOfInstancesToPurchase :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {recommendedNumberOfInstancesToPurchase = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdRecommendedNumberOfInstancesToPurchase "Use generic-lens or generic-optics with 'recommendedNumberOfInstancesToPurchase' instead." #-}

-- | The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'maximumNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMaximumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdMaximumNumberOfInstancesUsedPerHour = Lens.lens (maximumNumberOfInstancesUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {maximumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdMaximumNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'maximumNumberOfInstancesUsedPerHour' instead." #-}

-- | How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
--
-- /Note:/ Consider using 'estimatedReservationCostForLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedReservationCostForLookbackPeriod :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdEstimatedReservationCostForLookbackPeriod = Lens.lens (estimatedReservationCostForLookbackPeriod :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedReservationCostForLookbackPeriod = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdEstimatedReservationCostForLookbackPeriod "Use generic-lens or generic-optics with 'estimatedReservationCostForLookbackPeriod' instead." #-}

-- | Details about the instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'instanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdInstanceDetails :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe InstanceDetails)
rprdInstanceDetails = Lens.lens (instanceDetails :: ReservationPurchaseRecommendationDetail -> Lude.Maybe InstanceDetails) (\s a -> s {instanceDetails = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdInstanceDetails "Use generic-lens or generic-optics with 'instanceDetails' instead." #-}

-- | The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdAverageNumberOfInstancesUsedPerHour = Lens.lens (averageNumberOfInstancesUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {averageNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdAverageNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'averageNumberOfInstancesUsedPerHour' instead." #-}

-- | The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'minimumNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMinimumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdMinimumNumberOfInstancesUsedPerHour = Lens.lens (minimumNumberOfInstancesUsedPerHour :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {minimumNumberOfInstancesUsedPerHour = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdMinimumNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'minimumNumberOfInstancesUsedPerHour' instead." #-}

-- | How long AWS estimates that it takes for this instance to start saving you money, in months.
--
-- /Note:/ Consider using 'estimatedBreakEvenInMonths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedBreakEvenInMonths :: Lens.Lens' ReservationPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
rprdEstimatedBreakEvenInMonths = Lens.lens (estimatedBreakEvenInMonths :: ReservationPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedBreakEvenInMonths = a} :: ReservationPurchaseRecommendationDetail)
{-# DEPRECATED rprdEstimatedBreakEvenInMonths "Use generic-lens or generic-optics with 'estimatedBreakEvenInMonths' instead." #-}

instance Lude.FromJSON ReservationPurchaseRecommendationDetail where
  parseJSON =
    Lude.withObject
      "ReservationPurchaseRecommendationDetail"
      ( \x ->
          ReservationPurchaseRecommendationDetail'
            Lude.<$> (x Lude..:? "MaximumNormalizedUnitsUsedPerHour")
            Lude.<*> (x Lude..:? "RecurringStandardMonthlyCost")
            Lude.<*> (x Lude..:? "AverageNormalizedUnitsUsedPerHour")
            Lude.<*> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavingsPercentage")
            Lude.<*> (x Lude..:? "RecommendedNormalizedUnitsToPurchase")
            Lude.<*> (x Lude..:? "AverageUtilization")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavingsAmount")
            Lude.<*> (x Lude..:? "UpfrontCost")
            Lude.<*> (x Lude..:? "MinimumNormalizedUnitsUsedPerHour")
            Lude.<*> (x Lude..:? "EstimatedMonthlyOnDemandCost")
            Lude.<*> (x Lude..:? "RecommendedNumberOfInstancesToPurchase")
            Lude.<*> (x Lude..:? "MaximumNumberOfInstancesUsedPerHour")
            Lude.<*> (x Lude..:? "EstimatedReservationCostForLookbackPeriod")
            Lude.<*> (x Lude..:? "InstanceDetails")
            Lude.<*> (x Lude..:? "AverageNumberOfInstancesUsedPerHour")
            Lude.<*> (x Lude..:? "MinimumNumberOfInstancesUsedPerHour")
            Lude.<*> (x Lude..:? "EstimatedBreakEvenInMonths")
      )
