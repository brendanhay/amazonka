{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansUtilization
  ( SavingsPlansUtilization (..)
  -- * Smart constructor
  , mkSavingsPlansUtilization
  -- * Lenses
  , spuTotalCommitment
  , spuUnusedCommitment
  , spuUsedCommitment
  , spuUtilizationPercentage
  ) where

import qualified Network.AWS.CostExplorer.Types.TotalCommitment as Types
import qualified Network.AWS.CostExplorer.Types.UnusedCommitment as Types
import qualified Network.AWS.CostExplorer.Types.UsedCommitment as Types
import qualified Network.AWS.CostExplorer.Types.UtilizationPercentage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The measurement of how well you are using your existing Savings Plans.
--
-- /See:/ 'mkSavingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { totalCommitment :: Core.Maybe Types.TotalCommitment
    -- ^ The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
  , unusedCommitment :: Core.Maybe Types.UnusedCommitment
    -- ^ The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
  , usedCommitment :: Core.Maybe Types.UsedCommitment
    -- ^ The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
  , utilizationPercentage :: Core.Maybe Types.UtilizationPercentage
    -- ^ The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansUtilization' value with any optional fields omitted.
mkSavingsPlansUtilization
    :: SavingsPlansUtilization
mkSavingsPlansUtilization
  = SavingsPlansUtilization'{totalCommitment = Core.Nothing,
                             unusedCommitment = Core.Nothing, usedCommitment = Core.Nothing,
                             utilizationPercentage = Core.Nothing}

-- | The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
--
-- /Note:/ Consider using 'totalCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuTotalCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Types.TotalCommitment)
spuTotalCommitment = Lens.field @"totalCommitment"
{-# INLINEABLE spuTotalCommitment #-}
{-# DEPRECATED totalCommitment "Use generic-lens or generic-optics with 'totalCommitment' instead"  #-}

-- | The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
--
-- /Note:/ Consider using 'unusedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUnusedCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Types.UnusedCommitment)
spuUnusedCommitment = Lens.field @"unusedCommitment"
{-# INLINEABLE spuUnusedCommitment #-}
{-# DEPRECATED unusedCommitment "Use generic-lens or generic-optics with 'unusedCommitment' instead"  #-}

-- | The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
--
-- /Note:/ Consider using 'usedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUsedCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Types.UsedCommitment)
spuUsedCommitment = Lens.field @"usedCommitment"
{-# INLINEABLE spuUsedCommitment #-}
{-# DEPRECATED usedCommitment "Use generic-lens or generic-optics with 'usedCommitment' instead"  #-}

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
--
-- /Note:/ Consider using 'utilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUtilizationPercentage :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Types.UtilizationPercentage)
spuUtilizationPercentage = Lens.field @"utilizationPercentage"
{-# INLINEABLE spuUtilizationPercentage #-}
{-# DEPRECATED utilizationPercentage "Use generic-lens or generic-optics with 'utilizationPercentage' instead"  #-}

instance Core.FromJSON SavingsPlansUtilization where
        parseJSON
          = Core.withObject "SavingsPlansUtilization" Core.$
              \ x ->
                SavingsPlansUtilization' Core.<$>
                  (x Core..:? "TotalCommitment") Core.<*>
                    x Core..:? "UnusedCommitment"
                    Core.<*> x Core..:? "UsedCommitment"
                    Core.<*> x Core..:? "UtilizationPercentage"
