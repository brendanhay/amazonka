{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CurrentInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CurrentInstance
  ( CurrentInstance (..),

    -- * Smart constructor
    mkCurrentInstance,

    -- * Lenses
    ciCurrencyCode,
    ciInstanceName,
    ciMonthlyCost,
    ciOnDemandHoursInLookbackPeriod,
    ciReservationCoveredHoursInLookbackPeriod,
    ciResourceDetails,
    ciResourceId,
    ciResourceUtilization,
    ciSavingsPlansCoveredHoursInLookbackPeriod,
    ciTags,
    ciTotalRunningHoursInLookbackPeriod,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.CostExplorer.Types.ResourceDetails as Types
import qualified Network.AWS.CostExplorer.Types.ResourceUtilization as Types
import qualified Network.AWS.CostExplorer.Types.TagValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Context about the current instance.
--
-- /See:/ 'mkCurrentInstance' smart constructor.
data CurrentInstance = CurrentInstance'
  { -- | The currency code that AWS used to calculate the costs for this instance.
    currencyCode :: Core.Maybe Types.GenericString,
    -- | The name you've given an instance. This field will show as blank if you haven't given the instance a name.
    instanceName :: Core.Maybe Types.GenericString,
    -- | Current On-Demand cost of operating this instance on a monthly basis.
    monthlyCost :: Core.Maybe Types.GenericString,
    -- | Number of hours during the lookback period billed at On-Demand rates.
    onDemandHoursInLookbackPeriod :: Core.Maybe Types.GenericString,
    -- | Number of hours during the lookback period covered by reservations.
    reservationCoveredHoursInLookbackPeriod :: Core.Maybe Types.GenericString,
    -- | Details about the resource and utilization.
    resourceDetails :: Core.Maybe Types.ResourceDetails,
    -- | Resource ID of the current instance.
    resourceId :: Core.Maybe Types.GenericString,
    -- | Utilization information of the current instance during the lookback period.
    resourceUtilization :: Core.Maybe Types.ResourceUtilization,
    -- | Number of hours during the lookback period covered by Savings Plans.
    savingsPlansCoveredHoursInLookbackPeriod :: Core.Maybe Types.GenericString,
    -- | Cost allocation resource tags applied to the instance.
    tags :: Core.Maybe [Types.TagValues],
    -- | The total number of hours the instance ran during the lookback period.
    totalRunningHoursInLookbackPeriod :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CurrentInstance' value with any optional fields omitted.
mkCurrentInstance ::
  CurrentInstance
mkCurrentInstance =
  CurrentInstance'
    { currencyCode = Core.Nothing,
      instanceName = Core.Nothing,
      monthlyCost = Core.Nothing,
      onDemandHoursInLookbackPeriod = Core.Nothing,
      reservationCoveredHoursInLookbackPeriod = Core.Nothing,
      resourceDetails = Core.Nothing,
      resourceId = Core.Nothing,
      resourceUtilization = Core.Nothing,
      savingsPlansCoveredHoursInLookbackPeriod = Core.Nothing,
      tags = Core.Nothing,
      totalRunningHoursInLookbackPeriod = Core.Nothing
    }

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCurrencyCode :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED ciCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The name you've given an instance. This field will show as blank if you haven't given the instance a name.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceName :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciInstanceName = Lens.field @"instanceName"
{-# DEPRECATED ciInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | Current On-Demand cost of operating this instance on a monthly basis.
--
-- /Note:/ Consider using 'monthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMonthlyCost :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciMonthlyCost = Lens.field @"monthlyCost"
{-# DEPRECATED ciMonthlyCost "Use generic-lens or generic-optics with 'monthlyCost' instead." #-}

-- | Number of hours during the lookback period billed at On-Demand rates.
--
-- /Note:/ Consider using 'onDemandHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOnDemandHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciOnDemandHoursInLookbackPeriod = Lens.field @"onDemandHoursInLookbackPeriod"
{-# DEPRECATED ciOnDemandHoursInLookbackPeriod "Use generic-lens or generic-optics with 'onDemandHoursInLookbackPeriod' instead." #-}

-- | Number of hours during the lookback period covered by reservations.
--
-- /Note:/ Consider using 'reservationCoveredHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciReservationCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciReservationCoveredHoursInLookbackPeriod = Lens.field @"reservationCoveredHoursInLookbackPeriod"
{-# DEPRECATED ciReservationCoveredHoursInLookbackPeriod "Use generic-lens or generic-optics with 'reservationCoveredHoursInLookbackPeriod' instead." #-}

-- | Details about the resource and utilization.
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceDetails :: Lens.Lens' CurrentInstance (Core.Maybe Types.ResourceDetails)
ciResourceDetails = Lens.field @"resourceDetails"
{-# DEPRECATED ciResourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead." #-}

-- | Resource ID of the current instance.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceId :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciResourceId = Lens.field @"resourceId"
{-# DEPRECATED ciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Utilization information of the current instance during the lookback period.
--
-- /Note:/ Consider using 'resourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceUtilization :: Lens.Lens' CurrentInstance (Core.Maybe Types.ResourceUtilization)
ciResourceUtilization = Lens.field @"resourceUtilization"
{-# DEPRECATED ciResourceUtilization "Use generic-lens or generic-optics with 'resourceUtilization' instead." #-}

-- | Number of hours during the lookback period covered by Savings Plans.
--
-- /Note:/ Consider using 'savingsPlansCoveredHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSavingsPlansCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciSavingsPlansCoveredHoursInLookbackPeriod = Lens.field @"savingsPlansCoveredHoursInLookbackPeriod"
{-# DEPRECATED ciSavingsPlansCoveredHoursInLookbackPeriod "Use generic-lens or generic-optics with 'savingsPlansCoveredHoursInLookbackPeriod' instead." #-}

-- | Cost allocation resource tags applied to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CurrentInstance (Core.Maybe [Types.TagValues])
ciTags = Lens.field @"tags"
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The total number of hours the instance ran during the lookback period.
--
-- /Note:/ Consider using 'totalRunningHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTotalRunningHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Types.GenericString)
ciTotalRunningHoursInLookbackPeriod = Lens.field @"totalRunningHoursInLookbackPeriod"
{-# DEPRECATED ciTotalRunningHoursInLookbackPeriod "Use generic-lens or generic-optics with 'totalRunningHoursInLookbackPeriod' instead." #-}

instance Core.FromJSON CurrentInstance where
  parseJSON =
    Core.withObject "CurrentInstance" Core.$
      \x ->
        CurrentInstance'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "InstanceName")
          Core.<*> (x Core..:? "MonthlyCost")
          Core.<*> (x Core..:? "OnDemandHoursInLookbackPeriod")
          Core.<*> (x Core..:? "ReservationCoveredHoursInLookbackPeriod")
          Core.<*> (x Core..:? "ResourceDetails")
          Core.<*> (x Core..:? "ResourceId")
          Core.<*> (x Core..:? "ResourceUtilization")
          Core.<*> (x Core..:? "SavingsPlansCoveredHoursInLookbackPeriod")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TotalRunningHoursInLookbackPeriod")
