{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SimpleCondition
  ( SimpleCondition (..)
  -- * Smart constructor
  , mkSimpleCondition
  -- * Lenses
  , scEventCondition
  , scSegmentCondition
  , scSegmentDimensions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventCondition as Types
import qualified Network.AWS.Pinpoint.Types.SegmentCondition as Types
import qualified Network.AWS.Pinpoint.Types.SegmentDimensions as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a condition to evaluate for an activity in a journey.
--
-- /See:/ 'mkSimpleCondition' smart constructor.
data SimpleCondition = SimpleCondition'
  { eventCondition :: Core.Maybe Types.EventCondition
    -- ^ The dimension settings for the event that's associated with the activity.
  , segmentCondition :: Core.Maybe Types.SegmentCondition
    -- ^ The segment that's associated with the activity.
  , segmentDimensions :: Core.Maybe Types.SegmentDimensions
    -- ^ The dimension settings for the segment that's associated with the activity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimpleCondition' value with any optional fields omitted.
mkSimpleCondition
    :: SimpleCondition
mkSimpleCondition
  = SimpleCondition'{eventCondition = Core.Nothing,
                     segmentCondition = Core.Nothing, segmentDimensions = Core.Nothing}

-- | The dimension settings for the event that's associated with the activity.
--
-- /Note:/ Consider using 'eventCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEventCondition :: Lens.Lens' SimpleCondition (Core.Maybe Types.EventCondition)
scEventCondition = Lens.field @"eventCondition"
{-# INLINEABLE scEventCondition #-}
{-# DEPRECATED eventCondition "Use generic-lens or generic-optics with 'eventCondition' instead"  #-}

-- | The segment that's associated with the activity.
--
-- /Note:/ Consider using 'segmentCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentCondition :: Lens.Lens' SimpleCondition (Core.Maybe Types.SegmentCondition)
scSegmentCondition = Lens.field @"segmentCondition"
{-# INLINEABLE scSegmentCondition #-}
{-# DEPRECATED segmentCondition "Use generic-lens or generic-optics with 'segmentCondition' instead"  #-}

-- | The dimension settings for the segment that's associated with the activity.
--
-- /Note:/ Consider using 'segmentDimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentDimensions :: Lens.Lens' SimpleCondition (Core.Maybe Types.SegmentDimensions)
scSegmentDimensions = Lens.field @"segmentDimensions"
{-# INLINEABLE scSegmentDimensions #-}
{-# DEPRECATED segmentDimensions "Use generic-lens or generic-optics with 'segmentDimensions' instead"  #-}

instance Core.FromJSON SimpleCondition where
        toJSON SimpleCondition{..}
          = Core.object
              (Core.catMaybes
                 [("EventCondition" Core..=) Core.<$> eventCondition,
                  ("SegmentCondition" Core..=) Core.<$> segmentCondition,
                  ("segmentDimensions" Core..=) Core.<$> segmentDimensions])

instance Core.FromJSON SimpleCondition where
        parseJSON
          = Core.withObject "SimpleCondition" Core.$
              \ x ->
                SimpleCondition' Core.<$>
                  (x Core..:? "EventCondition") Core.<*>
                    x Core..:? "SegmentCondition"
                    Core.<*> x Core..:? "segmentDimensions"
