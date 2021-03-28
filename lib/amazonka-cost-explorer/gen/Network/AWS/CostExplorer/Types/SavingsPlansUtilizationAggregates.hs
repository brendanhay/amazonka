{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
  ( SavingsPlansUtilizationAggregates (..)
  -- * Smart constructor
  , mkSavingsPlansUtilizationAggregates
  -- * Lenses
  , spuaUtilization
  , spuaAmortizedCommitment
  , spuaSavings
  ) where

import qualified Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansSavings as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The aggregated utilization metrics for your Savings Plans usage.
--
-- /See:/ 'mkSavingsPlansUtilizationAggregates' smart constructor.
data SavingsPlansUtilizationAggregates = SavingsPlansUtilizationAggregates'
  { utilization :: Types.SavingsPlansUtilization
    -- ^ A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
  , amortizedCommitment :: Core.Maybe Types.SavingsPlansAmortizedCommitment
    -- ^ The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
  , savings :: Core.Maybe Types.SavingsPlansSavings
    -- ^ The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans, as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansUtilizationAggregates' value with any optional fields omitted.
mkSavingsPlansUtilizationAggregates
    :: Types.SavingsPlansUtilization -- ^ 'utilization'
    -> SavingsPlansUtilizationAggregates
mkSavingsPlansUtilizationAggregates utilization
  = SavingsPlansUtilizationAggregates'{utilization,
                                       amortizedCommitment = Core.Nothing, savings = Core.Nothing}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaUtilization :: Lens.Lens' SavingsPlansUtilizationAggregates Types.SavingsPlansUtilization
spuaUtilization = Lens.field @"utilization"
{-# INLINEABLE spuaUtilization #-}
{-# DEPRECATED utilization "Use generic-lens or generic-optics with 'utilization' instead"  #-}

-- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationAggregates (Core.Maybe Types.SavingsPlansAmortizedCommitment)
spuaAmortizedCommitment = Lens.field @"amortizedCommitment"
{-# INLINEABLE spuaAmortizedCommitment #-}
{-# DEPRECATED amortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead"  #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans, as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaSavings :: Lens.Lens' SavingsPlansUtilizationAggregates (Core.Maybe Types.SavingsPlansSavings)
spuaSavings = Lens.field @"savings"
{-# INLINEABLE spuaSavings #-}
{-# DEPRECATED savings "Use generic-lens or generic-optics with 'savings' instead"  #-}

instance Core.FromJSON SavingsPlansUtilizationAggregates where
        parseJSON
          = Core.withObject "SavingsPlansUtilizationAggregates" Core.$
              \ x ->
                SavingsPlansUtilizationAggregates' Core.<$>
                  (x Core..: "Utilization") Core.<*> x Core..:? "AmortizedCommitment"
                    Core.<*> x Core..:? "Savings"
