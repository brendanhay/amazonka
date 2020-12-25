{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rprdAccountId,
    rprdAverageNormalizedUnitsUsedPerHour,
    rprdAverageNumberOfInstancesUsedPerHour,
    rprdAverageUtilization,
    rprdCurrencyCode,
    rprdEstimatedBreakEvenInMonths,
    rprdEstimatedMonthlyOnDemandCost,
    rprdEstimatedMonthlySavingsAmount,
    rprdEstimatedMonthlySavingsPercentage,
    rprdEstimatedReservationCostForLookbackPeriod,
    rprdInstanceDetails,
    rprdMaximumNormalizedUnitsUsedPerHour,
    rprdMaximumNumberOfInstancesUsedPerHour,
    rprdMinimumNormalizedUnitsUsedPerHour,
    rprdMinimumNumberOfInstancesUsedPerHour,
    rprdRecommendedNormalizedUnitsToPurchase,
    rprdRecommendedNumberOfInstancesToPurchase,
    rprdRecurringStandardMonthlyCost,
    rprdUpfrontCost,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.CostExplorer.Types.InstanceDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about your recommended reservation purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { -- | The account that this RI recommendation is for.
    accountId :: Core.Maybe Types.GenericString,
    -- | The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    averageNormalizedUnitsUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    averageNumberOfInstancesUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
    averageUtilization :: Core.Maybe Types.GenericString,
    -- | The currency code that AWS used to calculate the costs for this instance.
    currencyCode :: Core.Maybe Types.GenericString,
    -- | How long AWS estimates that it takes for this instance to start saving you money, in months.
    estimatedBreakEvenInMonths :: Core.Maybe Types.GenericString,
    -- | How much AWS estimates that you spend on On-Demand Instances in a month.
    estimatedMonthlyOnDemandCost :: Core.Maybe Types.GenericString,
    -- | How much AWS estimates that this specific recommendation could save you in a month.
    estimatedMonthlySavingsAmount :: Core.Maybe Types.GenericString,
    -- | How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
    estimatedMonthlySavingsPercentage :: Core.Maybe Types.GenericString,
    -- | How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
    estimatedReservationCostForLookbackPeriod :: Core.Maybe Types.GenericString,
    -- | Details about the instances that AWS recommends that you purchase.
    instanceDetails :: Core.Maybe Types.InstanceDetails,
    -- | The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    maximumNormalizedUnitsUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    maximumNumberOfInstancesUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    minimumNormalizedUnitsUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
    minimumNumberOfInstancesUsedPerHour :: Core.Maybe Types.GenericString,
    -- | The number of normalized units that AWS recommends that you purchase.
    recommendedNormalizedUnitsToPurchase :: Core.Maybe Types.GenericString,
    -- | The number of instances that AWS recommends that you purchase.
    recommendedNumberOfInstancesToPurchase :: Core.Maybe Types.GenericString,
    -- | How much purchasing this instance costs you on a monthly basis.
    recurringStandardMonthlyCost :: Core.Maybe Types.GenericString,
    -- | How much purchasing this instance costs you upfront.
    upfrontCost :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationPurchaseRecommendationDetail' value with any optional fields omitted.
mkReservationPurchaseRecommendationDetail ::
  ReservationPurchaseRecommendationDetail
mkReservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { accountId =
        Core.Nothing,
      averageNormalizedUnitsUsedPerHour = Core.Nothing,
      averageNumberOfInstancesUsedPerHour = Core.Nothing,
      averageUtilization = Core.Nothing,
      currencyCode = Core.Nothing,
      estimatedBreakEvenInMonths = Core.Nothing,
      estimatedMonthlyOnDemandCost = Core.Nothing,
      estimatedMonthlySavingsAmount = Core.Nothing,
      estimatedMonthlySavingsPercentage = Core.Nothing,
      estimatedReservationCostForLookbackPeriod =
        Core.Nothing,
      instanceDetails = Core.Nothing,
      maximumNormalizedUnitsUsedPerHour = Core.Nothing,
      maximumNumberOfInstancesUsedPerHour = Core.Nothing,
      minimumNormalizedUnitsUsedPerHour = Core.Nothing,
      minimumNumberOfInstancesUsedPerHour = Core.Nothing,
      recommendedNormalizedUnitsToPurchase = Core.Nothing,
      recommendedNumberOfInstancesToPurchase = Core.Nothing,
      recurringStandardMonthlyCost = Core.Nothing,
      upfrontCost = Core.Nothing
    }

-- | The account that this RI recommendation is for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAccountId :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdAccountId = Lens.field @"accountId"
{-# DEPRECATED rprdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdAverageNormalizedUnitsUsedPerHour = Lens.field @"averageNormalizedUnitsUsedPerHour"
{-# DEPRECATED rprdAverageNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'averageNormalizedUnitsUsedPerHour' instead." #-}

-- | The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdAverageNumberOfInstancesUsedPerHour = Lens.field @"averageNumberOfInstancesUsedPerHour"
{-# DEPRECATED rprdAverageNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'averageNumberOfInstancesUsedPerHour' instead." #-}

-- | The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'averageUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdAverageUtilization :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdAverageUtilization = Lens.field @"averageUtilization"
{-# DEPRECATED rprdAverageUtilization "Use generic-lens or generic-optics with 'averageUtilization' instead." #-}

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdCurrencyCode :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rprdCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | How long AWS estimates that it takes for this instance to start saving you money, in months.
--
-- /Note:/ Consider using 'estimatedBreakEvenInMonths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedBreakEvenInMonths :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdEstimatedBreakEvenInMonths = Lens.field @"estimatedBreakEvenInMonths"
{-# DEPRECATED rprdEstimatedBreakEvenInMonths "Use generic-lens or generic-optics with 'estimatedBreakEvenInMonths' instead." #-}

-- | How much AWS estimates that you spend on On-Demand Instances in a month.
--
-- /Note:/ Consider using 'estimatedMonthlyOnDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlyOnDemandCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdEstimatedMonthlyOnDemandCost = Lens.field @"estimatedMonthlyOnDemandCost"
{-# DEPRECATED rprdEstimatedMonthlyOnDemandCost "Use generic-lens or generic-optics with 'estimatedMonthlyOnDemandCost' instead." #-}

-- | How much AWS estimates that this specific recommendation could save you in a month.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdEstimatedMonthlySavingsAmount = Lens.field @"estimatedMonthlySavingsAmount"
{-# DEPRECATED rprdEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead." #-}

-- | How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdEstimatedMonthlySavingsPercentage = Lens.field @"estimatedMonthlySavingsPercentage"
{-# DEPRECATED rprdEstimatedMonthlySavingsPercentage "Use generic-lens or generic-optics with 'estimatedMonthlySavingsPercentage' instead." #-}

-- | How much AWS estimates that you would have spent for all usage during the specified historical period if you had a reservation.
--
-- /Note:/ Consider using 'estimatedReservationCostForLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdEstimatedReservationCostForLookbackPeriod :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdEstimatedReservationCostForLookbackPeriod = Lens.field @"estimatedReservationCostForLookbackPeriod"
{-# DEPRECATED rprdEstimatedReservationCostForLookbackPeriod "Use generic-lens or generic-optics with 'estimatedReservationCostForLookbackPeriod' instead." #-}

-- | Details about the instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'instanceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdInstanceDetails :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.InstanceDetails)
rprdInstanceDetails = Lens.field @"instanceDetails"
{-# DEPRECATED rprdInstanceDetails "Use generic-lens or generic-optics with 'instanceDetails' instead." #-}

-- | The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'maximumNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMaximumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdMaximumNormalizedUnitsUsedPerHour = Lens.field @"maximumNormalizedUnitsUsedPerHour"
{-# DEPRECATED rprdMaximumNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'maximumNormalizedUnitsUsedPerHour' instead." #-}

-- | The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'maximumNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMaximumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdMaximumNumberOfInstancesUsedPerHour = Lens.field @"maximumNumberOfInstancesUsedPerHour"
{-# DEPRECATED rprdMaximumNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'maximumNumberOfInstancesUsedPerHour' instead." #-}

-- | The minimum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'minimumNormalizedUnitsUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMinimumNormalizedUnitsUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdMinimumNormalizedUnitsUsedPerHour = Lens.field @"minimumNormalizedUnitsUsedPerHour"
{-# DEPRECATED rprdMinimumNormalizedUnitsUsedPerHour "Use generic-lens or generic-optics with 'minimumNormalizedUnitsUsedPerHour' instead." #-}

-- | The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- /Note:/ Consider using 'minimumNumberOfInstancesUsedPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdMinimumNumberOfInstancesUsedPerHour :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdMinimumNumberOfInstancesUsedPerHour = Lens.field @"minimumNumberOfInstancesUsedPerHour"
{-# DEPRECATED rprdMinimumNumberOfInstancesUsedPerHour "Use generic-lens or generic-optics with 'minimumNumberOfInstancesUsedPerHour' instead." #-}

-- | The number of normalized units that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'recommendedNormalizedUnitsToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecommendedNormalizedUnitsToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdRecommendedNormalizedUnitsToPurchase = Lens.field @"recommendedNormalizedUnitsToPurchase"
{-# DEPRECATED rprdRecommendedNormalizedUnitsToPurchase "Use generic-lens or generic-optics with 'recommendedNormalizedUnitsToPurchase' instead." #-}

-- | The number of instances that AWS recommends that you purchase.
--
-- /Note:/ Consider using 'recommendedNumberOfInstancesToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecommendedNumberOfInstancesToPurchase :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdRecommendedNumberOfInstancesToPurchase = Lens.field @"recommendedNumberOfInstancesToPurchase"
{-# DEPRECATED rprdRecommendedNumberOfInstancesToPurchase "Use generic-lens or generic-optics with 'recommendedNumberOfInstancesToPurchase' instead." #-}

-- | How much purchasing this instance costs you on a monthly basis.
--
-- /Note:/ Consider using 'recurringStandardMonthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdRecurringStandardMonthlyCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdRecurringStandardMonthlyCost = Lens.field @"recurringStandardMonthlyCost"
{-# DEPRECATED rprdRecurringStandardMonthlyCost "Use generic-lens or generic-optics with 'recurringStandardMonthlyCost' instead." #-}

-- | How much purchasing this instance costs you upfront.
--
-- /Note:/ Consider using 'upfrontCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprdUpfrontCost :: Lens.Lens' ReservationPurchaseRecommendationDetail (Core.Maybe Types.GenericString)
rprdUpfrontCost = Lens.field @"upfrontCost"
{-# DEPRECATED rprdUpfrontCost "Use generic-lens or generic-optics with 'upfrontCost' instead." #-}

instance Core.FromJSON ReservationPurchaseRecommendationDetail where
  parseJSON =
    Core.withObject "ReservationPurchaseRecommendationDetail" Core.$
      \x ->
        ReservationPurchaseRecommendationDetail'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "AverageNormalizedUnitsUsedPerHour")
          Core.<*> (x Core..:? "AverageNumberOfInstancesUsedPerHour")
          Core.<*> (x Core..:? "AverageUtilization")
          Core.<*> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "EstimatedBreakEvenInMonths")
          Core.<*> (x Core..:? "EstimatedMonthlyOnDemandCost")
          Core.<*> (x Core..:? "EstimatedMonthlySavingsAmount")
          Core.<*> (x Core..:? "EstimatedMonthlySavingsPercentage")
          Core.<*> (x Core..:? "EstimatedReservationCostForLookbackPeriod")
          Core.<*> (x Core..:? "InstanceDetails")
          Core.<*> (x Core..:? "MaximumNormalizedUnitsUsedPerHour")
          Core.<*> (x Core..:? "MaximumNumberOfInstancesUsedPerHour")
          Core.<*> (x Core..:? "MinimumNormalizedUnitsUsedPerHour")
          Core.<*> (x Core..:? "MinimumNumberOfInstancesUsedPerHour")
          Core.<*> (x Core..:? "RecommendedNormalizedUnitsToPurchase")
          Core.<*> (x Core..:? "RecommendedNumberOfInstancesToPurchase")
          Core.<*> (x Core..:? "RecurringStandardMonthlyCost")
          Core.<*> (x Core..:? "UpfrontCost")
