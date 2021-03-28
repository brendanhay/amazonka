{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
  ( MultiplexStatmuxVideoSettings (..)
  -- * Smart constructor
  , mkMultiplexStatmuxVideoSettings
  -- * Lenses
  , msvsMaximumBitrate
  , msvsMinimumBitrate
  , msvsPriority
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Statmux rate control settings
--
-- /See:/ 'mkMultiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'
  { maximumBitrate :: Core.Maybe Core.Natural
    -- ^ Maximum statmux bitrate.
  , minimumBitrate :: Core.Maybe Core.Natural
    -- ^ Minimum statmux bitrate.
  , priority :: Core.Maybe Core.Int
    -- ^ The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexStatmuxVideoSettings' value with any optional fields omitted.
mkMultiplexStatmuxVideoSettings
    :: MultiplexStatmuxVideoSettings
mkMultiplexStatmuxVideoSettings
  = MultiplexStatmuxVideoSettings'{maximumBitrate = Core.Nothing,
                                   minimumBitrate = Core.Nothing, priority = Core.Nothing}

-- | Maximum statmux bitrate.
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsMaximumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Natural)
msvsMaximumBitrate = Lens.field @"maximumBitrate"
{-# INLINEABLE msvsMaximumBitrate #-}
{-# DEPRECATED maximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead"  #-}

-- | Minimum statmux bitrate.
--
-- /Note:/ Consider using 'minimumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsMinimumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Natural)
msvsMinimumBitrate = Lens.field @"minimumBitrate"
{-# INLINEABLE msvsMinimumBitrate #-}
{-# DEPRECATED minimumBitrate "Use generic-lens or generic-optics with 'minimumBitrate' instead"  #-}

-- | The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsPriority :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Int)
msvsPriority = Lens.field @"priority"
{-# INLINEABLE msvsPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

instance Core.FromJSON MultiplexStatmuxVideoSettings where
        toJSON MultiplexStatmuxVideoSettings{..}
          = Core.object
              (Core.catMaybes
                 [("maximumBitrate" Core..=) Core.<$> maximumBitrate,
                  ("minimumBitrate" Core..=) Core.<$> minimumBitrate,
                  ("priority" Core..=) Core.<$> priority])

instance Core.FromJSON MultiplexStatmuxVideoSettings where
        parseJSON
          = Core.withObject "MultiplexStatmuxVideoSettings" Core.$
              \ x ->
                MultiplexStatmuxVideoSettings' Core.<$>
                  (x Core..:? "maximumBitrate") Core.<*> x Core..:? "minimumBitrate"
                    Core.<*> x Core..:? "priority"
