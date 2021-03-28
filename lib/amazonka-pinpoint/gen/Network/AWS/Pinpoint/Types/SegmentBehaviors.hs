{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentBehaviors
  ( SegmentBehaviors (..)
  -- * Smart constructor
  , mkSegmentBehaviors
  -- * Lenses
  , sbRecency
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.RecencyDimension as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies dimension settings for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
-- /See:/ 'mkSegmentBehaviors' smart constructor.
newtype SegmentBehaviors = SegmentBehaviors'
  { recency :: Core.Maybe Types.RecencyDimension
    -- ^ The dimension settings that are based on how recently an endpoint was active.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentBehaviors' value with any optional fields omitted.
mkSegmentBehaviors
    :: SegmentBehaviors
mkSegmentBehaviors = SegmentBehaviors'{recency = Core.Nothing}

-- | The dimension settings that are based on how recently an endpoint was active.
--
-- /Note:/ Consider using 'recency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbRecency :: Lens.Lens' SegmentBehaviors (Core.Maybe Types.RecencyDimension)
sbRecency = Lens.field @"recency"
{-# INLINEABLE sbRecency #-}
{-# DEPRECATED recency "Use generic-lens or generic-optics with 'recency' instead"  #-}

instance Core.FromJSON SegmentBehaviors where
        toJSON SegmentBehaviors{..}
          = Core.object
              (Core.catMaybes [("Recency" Core..=) Core.<$> recency])

instance Core.FromJSON SegmentBehaviors where
        parseJSON
          = Core.withObject "SegmentBehaviors" Core.$
              \ x -> SegmentBehaviors' Core.<$> (x Core..:? "Recency")
