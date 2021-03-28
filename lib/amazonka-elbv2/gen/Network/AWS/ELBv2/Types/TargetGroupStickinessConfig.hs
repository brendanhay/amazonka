{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
  ( TargetGroupStickinessConfig (..)
  -- * Smart constructor
  , mkTargetGroupStickinessConfig
  -- * Lenses
  , tgscDurationSeconds
  , tgscEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the target group stickiness for a rule.
--
-- /See:/ 'mkTargetGroupStickinessConfig' smart constructor.
data TargetGroupStickinessConfig = TargetGroupStickinessConfig'
  { durationSeconds :: Core.Maybe Core.Int
    -- ^ The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether target group stickiness is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupStickinessConfig' value with any optional fields omitted.
mkTargetGroupStickinessConfig
    :: TargetGroupStickinessConfig
mkTargetGroupStickinessConfig
  = TargetGroupStickinessConfig'{durationSeconds = Core.Nothing,
                                 enabled = Core.Nothing}

-- | The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgscDurationSeconds :: Lens.Lens' TargetGroupStickinessConfig (Core.Maybe Core.Int)
tgscDurationSeconds = Lens.field @"durationSeconds"
{-# INLINEABLE tgscDurationSeconds #-}
{-# DEPRECATED durationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead"  #-}

-- | Indicates whether target group stickiness is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgscEnabled :: Lens.Lens' TargetGroupStickinessConfig (Core.Maybe Core.Bool)
tgscEnabled = Lens.field @"enabled"
{-# INLINEABLE tgscEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery TargetGroupStickinessConfig where
        toQuery TargetGroupStickinessConfig{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DurationSeconds")
              durationSeconds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled

instance Core.FromXML TargetGroupStickinessConfig where
        parseXML x
          = TargetGroupStickinessConfig' Core.<$>
              (x Core..@? "DurationSeconds") Core.<*> x Core..@? "Enabled"
