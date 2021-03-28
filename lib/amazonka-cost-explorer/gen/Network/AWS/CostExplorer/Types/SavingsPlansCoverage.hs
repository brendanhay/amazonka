{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansCoverage
  ( SavingsPlansCoverage (..)
  -- * Smart constructor
  , mkSavingsPlansCoverage
  -- * Lenses
  , spcAttributes
  , spcCoverage
  , spcTimePeriod
  ) where

import qualified Network.AWS.CostExplorer.Types.AttributeType as Types
import qualified Network.AWS.CostExplorer.Types.AttributeValue as Types
import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansCoverageData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of Savings Plans eligible usage that is covered by Savings Plans. All calculations consider the On-Demand equivalent of your Savings Plans usage.
--
-- /See:/ 'mkSavingsPlansCoverage' smart constructor.
data SavingsPlansCoverage = SavingsPlansCoverage'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue)
    -- ^ The attribute that applies to a specific @Dimension@ .
  , coverage :: Core.Maybe Types.SavingsPlansCoverageData
    -- ^ The amount of Savings Plans eligible usage that the Savings Plans covered.
  , timePeriod :: Core.Maybe Types.DateInterval
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansCoverage' value with any optional fields omitted.
mkSavingsPlansCoverage
    :: SavingsPlansCoverage
mkSavingsPlansCoverage
  = SavingsPlansCoverage'{attributes = Core.Nothing,
                          coverage = Core.Nothing, timePeriod = Core.Nothing}

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcAttributes :: Lens.Lens' SavingsPlansCoverage (Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue))
spcAttributes = Lens.field @"attributes"
{-# INLINEABLE spcAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The amount of Savings Plans eligible usage that the Savings Plans covered.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcCoverage :: Lens.Lens' SavingsPlansCoverage (Core.Maybe Types.SavingsPlansCoverageData)
spcCoverage = Lens.field @"coverage"
{-# INLINEABLE spcCoverage #-}
{-# DEPRECATED coverage "Use generic-lens or generic-optics with 'coverage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcTimePeriod :: Lens.Lens' SavingsPlansCoverage (Core.Maybe Types.DateInterval)
spcTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE spcTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

instance Core.FromJSON SavingsPlansCoverage where
        parseJSON
          = Core.withObject "SavingsPlansCoverage" Core.$
              \ x ->
                SavingsPlansCoverage' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Coverage" Core.<*>
                    x Core..:? "TimePeriod"
