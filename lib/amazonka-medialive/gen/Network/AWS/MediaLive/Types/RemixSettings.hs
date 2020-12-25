{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RemixSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RemixSettings
  ( RemixSettings (..),

    -- * Smart constructor
    mkRemixSettings,

    -- * Lenses
    rsChannelMappings,
    rsChannelsIn,
    rsChannelsOut,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioChannelMapping as Types
import qualified Network.AWS.Prelude as Core

-- | Remix Settings
--
-- /See:/ 'mkRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Mapping of input channels to output channels, with appropriate gain adjustments.
    channelMappings :: [Types.AudioChannelMapping],
    -- | Number of input channels to be used.
    channelsIn :: Core.Maybe Core.Natural,
    -- | Number of output channels to be produced.
    --
    -- Valid values: 1, 2, 4, 6, 8
    channelsOut :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemixSettings' value with any optional fields omitted.
mkRemixSettings ::
  RemixSettings
mkRemixSettings =
  RemixSettings'
    { channelMappings = Core.mempty,
      channelsIn = Core.Nothing,
      channelsOut = Core.Nothing
    }

-- | Mapping of input channels to output channels, with appropriate gain adjustments.
--
-- /Note:/ Consider using 'channelMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelMappings :: Lens.Lens' RemixSettings [Types.AudioChannelMapping]
rsChannelMappings = Lens.field @"channelMappings"
{-# DEPRECATED rsChannelMappings "Use generic-lens or generic-optics with 'channelMappings' instead." #-}

-- | Number of input channels to be used.
--
-- /Note:/ Consider using 'channelsIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsIn :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
rsChannelsIn = Lens.field @"channelsIn"
{-# DEPRECATED rsChannelsIn "Use generic-lens or generic-optics with 'channelsIn' instead." #-}

-- | Number of output channels to be produced.
--
-- Valid values: 1, 2, 4, 6, 8
--
-- /Note:/ Consider using 'channelsOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsOut :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
rsChannelsOut = Lens.field @"channelsOut"
{-# DEPRECATED rsChannelsOut "Use generic-lens or generic-optics with 'channelsOut' instead." #-}

instance Core.FromJSON RemixSettings where
  toJSON RemixSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelMappings" Core..= channelMappings),
            ("channelsIn" Core..=) Core.<$> channelsIn,
            ("channelsOut" Core..=) Core.<$> channelsOut
          ]
      )

instance Core.FromJSON RemixSettings where
  parseJSON =
    Core.withObject "RemixSettings" Core.$
      \x ->
        RemixSettings'
          Core.<$> (x Core..:? "channelMappings" Core..!= Core.mempty)
          Core.<*> (x Core..:? "channelsIn")
          Core.<*> (x Core..:? "channelsOut")
