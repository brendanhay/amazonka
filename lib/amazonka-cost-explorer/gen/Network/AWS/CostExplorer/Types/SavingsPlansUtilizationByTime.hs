{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
  ( SavingsPlansUtilizationByTime (..),

    -- * Smart constructor
    mkSavingsPlansUtilizationByTime,

    -- * Lenses
    spubtTimePeriod,
    spubtUtilization,
    spubtAmortizedCommitment,
    spubtSavings,
  )
where

import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansSavings as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of Savings Plans utilization, in hours.
--
-- /See:/ 'mkSavingsPlansUtilizationByTime' smart constructor.
data SavingsPlansUtilizationByTime = SavingsPlansUtilizationByTime'
  { timePeriod :: Types.DateInterval,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
    utilization :: Types.SavingsPlansUtilization,
    -- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Core.Maybe Types.SavingsPlansAmortizedCommitment,
    -- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
    savings :: Core.Maybe Types.SavingsPlansSavings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansUtilizationByTime' value with any optional fields omitted.
mkSavingsPlansUtilizationByTime ::
  -- | 'timePeriod'
  Types.DateInterval ->
  -- | 'utilization'
  Types.SavingsPlansUtilization ->
  SavingsPlansUtilizationByTime
mkSavingsPlansUtilizationByTime timePeriod utilization =
  SavingsPlansUtilizationByTime'
    { timePeriod,
      utilization,
      amortizedCommitment = Core.Nothing,
      savings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtTimePeriod :: Lens.Lens' SavingsPlansUtilizationByTime Types.DateInterval
spubtTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED spubtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtUtilization :: Lens.Lens' SavingsPlansUtilizationByTime Types.SavingsPlansUtilization
spubtUtilization = Lens.field @"utilization"
{-# DEPRECATED spubtUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

-- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationByTime (Core.Maybe Types.SavingsPlansAmortizedCommitment)
spubtAmortizedCommitment = Lens.field @"amortizedCommitment"
{-# DEPRECATED spubtAmortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead." #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtSavings :: Lens.Lens' SavingsPlansUtilizationByTime (Core.Maybe Types.SavingsPlansSavings)
spubtSavings = Lens.field @"savings"
{-# DEPRECATED spubtSavings "Use generic-lens or generic-optics with 'savings' instead." #-}

instance Core.FromJSON SavingsPlansUtilizationByTime where
  parseJSON =
    Core.withObject "SavingsPlansUtilizationByTime" Core.$
      \x ->
        SavingsPlansUtilizationByTime'
          Core.<$> (x Core..: "TimePeriod")
          Core.<*> (x Core..: "Utilization")
          Core.<*> (x Core..:? "AmortizedCommitment")
          Core.<*> (x Core..:? "Savings")
