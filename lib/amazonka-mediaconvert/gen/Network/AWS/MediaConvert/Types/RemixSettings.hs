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
import Network.AWS.MediaConvert.Types.ChannelMapping
import qualified Network.AWS.Prelude as Lude

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each audio channel in each output of your job. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
--
-- /See:/ 'mkRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { channelMapping ::
      Lude.Maybe ChannelMapping,
    channelsIn :: Lude.Maybe Lude.Natural,
    channelsOut :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- * 'channelMapping' - Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
-- * 'channelsIn' - Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
-- * 'channelsOut' - Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
mkRemixSettings ::
  RemixSettings
mkRemixSettings =
  RemixSettings'
    { channelMapping = Lude.Nothing,
      channelsIn = Lude.Nothing,
      channelsOut = Lude.Nothing
    }

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /Note:/ Consider using 'channelMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelMapping :: Lens.Lens' RemixSettings (Lude.Maybe ChannelMapping)
rsChannelMapping = Lens.lens (channelMapping :: RemixSettings -> Lude.Maybe ChannelMapping) (\s a -> s {channelMapping = a} :: RemixSettings)
{-# DEPRECATED rsChannelMapping "Use generic-lens or generic-optics with 'channelMapping' instead." #-}

-- | Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
--
-- /Note:/ Consider using 'channelsIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsIn :: Lens.Lens' RemixSettings (Lude.Maybe Lude.Natural)
rsChannelsIn = Lens.lens (channelsIn :: RemixSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channelsIn = a} :: RemixSettings)
{-# DEPRECATED rsChannelsIn "Use generic-lens or generic-optics with 'channelsIn' instead." #-}

-- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
--
-- /Note:/ Consider using 'channelsOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsOut :: Lens.Lens' RemixSettings (Lude.Maybe Lude.Natural)
rsChannelsOut = Lens.lens (channelsOut :: RemixSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channelsOut = a} :: RemixSettings)
{-# DEPRECATED rsChannelsOut "Use generic-lens or generic-optics with 'channelsOut' instead." #-}

instance Lude.FromJSON RemixSettings where
  parseJSON =
    Lude.withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            Lude.<$> (x Lude..:? "channelMapping")
            Lude.<*> (x Lude..:? "channelsIn")
            Lude.<*> (x Lude..:? "channelsOut")
      )

instance Lude.ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channelMapping" Lude..=) Lude.<$> channelMapping,
            ("channelsIn" Lude..=) Lude.<$> channelsIn,
            ("channelsOut" Lude..=) Lude.<$> channelsOut
          ]
      )
