{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.DateInterval
  ( DateInterval (..)
  -- * Smart constructor
  , mkDateInterval
  -- * Lenses
  , diStart
  , diEnd
  ) where

import qualified Network.AWS.CostExplorer.Types.End as Types
import qualified Network.AWS.CostExplorer.Types.Start as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The time period that you want the usage and costs for. 
--
-- /See:/ 'mkDateInterval' smart constructor.
data DateInterval = DateInterval'
  { start :: Types.Start
    -- ^ The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
  , end :: Types.End
    -- ^ The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DateInterval' value with any optional fields omitted.
mkDateInterval
    :: Types.Start -- ^ 'start'
    -> Types.End -- ^ 'end'
    -> DateInterval
mkDateInterval start end = DateInterval'{start, end}

-- | The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStart :: Lens.Lens' DateInterval Types.Start
diStart = Lens.field @"start"
{-# INLINEABLE diStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEnd :: Lens.Lens' DateInterval Types.End
diEnd = Lens.field @"end"
{-# INLINEABLE diEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

instance Core.FromJSON DateInterval where
        toJSON DateInterval{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Start" Core..= start), Core.Just ("End" Core..= end)])

instance Core.FromJSON DateInterval where
        parseJSON
          = Core.withObject "DateInterval" Core.$
              \ x ->
                DateInterval' Core.<$> (x Core..: "Start") Core.<*> x Core..: "End"
