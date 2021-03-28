{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecencyDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RecencyDimension
  ( RecencyDimension (..)
  -- * Smart constructor
  , mkRecencyDimension
  -- * Lenses
  , rdDuration
  , rdRecencyType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Duration as Types
import qualified Network.AWS.Pinpoint.Types.RecencyType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies criteria for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
-- /See:/ 'mkRecencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
  { duration :: Types.Duration
    -- ^ The duration to use when determining whether an endpoint is active or inactive.
  , recencyType :: Types.RecencyType
    -- ^ The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecencyDimension' value with any optional fields omitted.
mkRecencyDimension
    :: Types.Duration -- ^ 'duration'
    -> Types.RecencyType -- ^ 'recencyType'
    -> RecencyDimension
mkRecencyDimension duration recencyType
  = RecencyDimension'{duration, recencyType}

-- | The duration to use when determining whether an endpoint is active or inactive.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDuration :: Lens.Lens' RecencyDimension Types.Duration
rdDuration = Lens.field @"duration"
{-# INLINEABLE rdDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
--
-- /Note:/ Consider using 'recencyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecencyType :: Lens.Lens' RecencyDimension Types.RecencyType
rdRecencyType = Lens.field @"recencyType"
{-# INLINEABLE rdRecencyType #-}
{-# DEPRECATED recencyType "Use generic-lens or generic-optics with 'recencyType' instead"  #-}

instance Core.FromJSON RecencyDimension where
        toJSON RecencyDimension{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Duration" Core..= duration),
                  Core.Just ("RecencyType" Core..= recencyType)])

instance Core.FromJSON RecencyDimension where
        parseJSON
          = Core.withObject "RecencyDimension" Core.$
              \ x ->
                RecencyDimension' Core.<$>
                  (x Core..: "Duration") Core.<*> x Core..: "RecencyType"
