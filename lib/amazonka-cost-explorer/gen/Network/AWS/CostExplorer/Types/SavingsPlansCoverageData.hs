{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
  ( SavingsPlansCoverageData (..),

    -- * Smart constructor
    mkSavingsPlansCoverageData,

    -- * Lenses
    spcdCoveragePercentage,
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdTotalCost,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specific coverage percentage, On-Demand costs, and spend covered by Savings Plans, and total Savings Plans costs for an account.
--
-- /See:/ 'mkSavingsPlansCoverageData' smart constructor.
data SavingsPlansCoverageData = SavingsPlansCoverageData'
  { -- | The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
    coveragePercentage :: Core.Maybe Types.GenericString,
    -- | The cost of your AWS usage at the public On-Demand rate.
    onDemandCost :: Core.Maybe Types.GenericString,
    -- | The amount of your AWS usage that is covered by a Savings Plans.
    spendCoveredBySavingsPlans :: Core.Maybe Types.GenericString,
    -- | The total cost of your AWS usage, regardless of your purchase option.
    totalCost :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansCoverageData' value with any optional fields omitted.
mkSavingsPlansCoverageData ::
  SavingsPlansCoverageData
mkSavingsPlansCoverageData =
  SavingsPlansCoverageData'
    { coveragePercentage = Core.Nothing,
      onDemandCost = Core.Nothing,
      spendCoveredBySavingsPlans = Core.Nothing,
      totalCost = Core.Nothing
    }

-- | The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
--
-- /Note:/ Consider using 'coveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdCoveragePercentage :: Lens.Lens' SavingsPlansCoverageData (Core.Maybe Types.GenericString)
spcdCoveragePercentage = Lens.field @"coveragePercentage"
{-# DEPRECATED spcdCoveragePercentage "Use generic-lens or generic-optics with 'coveragePercentage' instead." #-}

-- | The cost of your AWS usage at the public On-Demand rate.
--
-- /Note:/ Consider using 'onDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdOnDemandCost :: Lens.Lens' SavingsPlansCoverageData (Core.Maybe Types.GenericString)
spcdOnDemandCost = Lens.field @"onDemandCost"
{-# DEPRECATED spcdOnDemandCost "Use generic-lens or generic-optics with 'onDemandCost' instead." #-}

-- | The amount of your AWS usage that is covered by a Savings Plans.
--
-- /Note:/ Consider using 'spendCoveredBySavingsPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdSpendCoveredBySavingsPlans :: Lens.Lens' SavingsPlansCoverageData (Core.Maybe Types.GenericString)
spcdSpendCoveredBySavingsPlans = Lens.field @"spendCoveredBySavingsPlans"
{-# DEPRECATED spcdSpendCoveredBySavingsPlans "Use generic-lens or generic-optics with 'spendCoveredBySavingsPlans' instead." #-}

-- | The total cost of your AWS usage, regardless of your purchase option.
--
-- /Note:/ Consider using 'totalCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdTotalCost :: Lens.Lens' SavingsPlansCoverageData (Core.Maybe Types.GenericString)
spcdTotalCost = Lens.field @"totalCost"
{-# DEPRECATED spcdTotalCost "Use generic-lens or generic-optics with 'totalCost' instead." #-}

instance Core.FromJSON SavingsPlansCoverageData where
  parseJSON =
    Core.withObject "SavingsPlansCoverageData" Core.$
      \x ->
        SavingsPlansCoverageData'
          Core.<$> (x Core..:? "CoveragePercentage")
          Core.<*> (x Core..:? "OnDemandCost")
          Core.<*> (x Core..:? "SpendCoveredBySavingsPlans")
          Core.<*> (x Core..:? "TotalCost")
