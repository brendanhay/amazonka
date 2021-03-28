{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyDateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.AnomalyDateInterval
  ( AnomalyDateInterval (..)
  -- * Smart constructor
  , mkAnomalyDateInterval
  -- * Lenses
  , adiStartDate
  , adiEndDate
  ) where

import qualified Network.AWS.CostExplorer.Types.EndDate as Types
import qualified Network.AWS.CostExplorer.Types.StartDate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The time period for an anomaly. 
--
-- /See:/ 'mkAnomalyDateInterval' smart constructor.
data AnomalyDateInterval = AnomalyDateInterval'
  { startDate :: Types.StartDate
    -- ^ The first date an anomaly was observed. 
  , endDate :: Core.Maybe Types.EndDate
    -- ^ The last date an anomaly was observed. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnomalyDateInterval' value with any optional fields omitted.
mkAnomalyDateInterval
    :: Types.StartDate -- ^ 'startDate'
    -> AnomalyDateInterval
mkAnomalyDateInterval startDate
  = AnomalyDateInterval'{startDate, endDate = Core.Nothing}

-- | The first date an anomaly was observed. 
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adiStartDate :: Lens.Lens' AnomalyDateInterval Types.StartDate
adiStartDate = Lens.field @"startDate"
{-# INLINEABLE adiStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | The last date an anomaly was observed. 
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adiEndDate :: Lens.Lens' AnomalyDateInterval (Core.Maybe Types.EndDate)
adiEndDate = Lens.field @"endDate"
{-# INLINEABLE adiEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

instance Core.FromJSON AnomalyDateInterval where
        toJSON AnomalyDateInterval{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartDate" Core..= startDate),
                  ("EndDate" Core..=) Core.<$> endDate])
