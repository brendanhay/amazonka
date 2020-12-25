{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RemixSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RemixSettings
  ( RemixSettings (..),

    -- * Smart constructor
    mkRemixSettings,

    -- * Lenses
    rsChannelMapping,
    rsChannelsIn,
    rsChannelsOut,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.ChannelMapping as Types
import qualified Network.AWS.Prelude as Core

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each audio channel in each output of your job. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
--
-- /See:/ 'mkRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
    channelMapping :: Core.Maybe Types.ChannelMapping,
    -- | Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
    channelsIn :: Core.Maybe Core.Natural,
    -- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
    channelsOut :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemixSettings' value with any optional fields omitted.
mkRemixSettings ::
  RemixSettings
mkRemixSettings =
  RemixSettings'
    { channelMapping = Core.Nothing,
      channelsIn = Core.Nothing,
      channelsOut = Core.Nothing
    }

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /Note:/ Consider using 'channelMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelMapping :: Lens.Lens' RemixSettings (Core.Maybe Types.ChannelMapping)
rsChannelMapping = Lens.field @"channelMapping"
{-# DEPRECATED rsChannelMapping "Use generic-lens or generic-optics with 'channelMapping' instead." #-}

-- | Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
--
-- /Note:/ Consider using 'channelsIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsIn :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
rsChannelsIn = Lens.field @"channelsIn"
{-# DEPRECATED rsChannelsIn "Use generic-lens or generic-optics with 'channelsIn' instead." #-}

-- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
--
-- /Note:/ Consider using 'channelsOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsOut :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
rsChannelsOut = Lens.field @"channelsOut"
{-# DEPRECATED rsChannelsOut "Use generic-lens or generic-optics with 'channelsOut' instead." #-}

instance Core.FromJSON RemixSettings where
  toJSON RemixSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("channelMapping" Core..=) Core.<$> channelMapping,
            ("channelsIn" Core..=) Core.<$> channelsIn,
            ("channelsOut" Core..=) Core.<$> channelsOut
          ]
      )

instance Core.FromJSON RemixSettings where
  parseJSON =
    Core.withObject "RemixSettings" Core.$
      \x ->
        RemixSettings'
          Core.<$> (x Core..:? "channelMapping")
          Core.<*> (x Core..:? "channelsIn")
          Core.<*> (x Core..:? "channelsOut")
