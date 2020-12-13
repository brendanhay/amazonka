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
import Network.AWS.MediaLive.Types.AudioChannelMapping
import qualified Network.AWS.Prelude as Lude

-- | Remix Settings
--
-- /See:/ 'mkRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Mapping of input channels to output channels, with appropriate gain adjustments.
    channelMappings :: [AudioChannelMapping],
    -- | Number of input channels to be used.
    channelsIn :: Lude.Maybe Lude.Natural,
    -- | Number of output channels to be produced.
    --
    -- Valid values: 1, 2, 4, 6, 8
    channelsOut :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- * 'channelMappings' - Mapping of input channels to output channels, with appropriate gain adjustments.
-- * 'channelsIn' - Number of input channels to be used.
-- * 'channelsOut' - Number of output channels to be produced.
--
-- Valid values: 1, 2, 4, 6, 8
mkRemixSettings ::
  RemixSettings
mkRemixSettings =
  RemixSettings'
    { channelMappings = Lude.mempty,
      channelsIn = Lude.Nothing,
      channelsOut = Lude.Nothing
    }

-- | Mapping of input channels to output channels, with appropriate gain adjustments.
--
-- /Note:/ Consider using 'channelMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelMappings :: Lens.Lens' RemixSettings [AudioChannelMapping]
rsChannelMappings = Lens.lens (channelMappings :: RemixSettings -> [AudioChannelMapping]) (\s a -> s {channelMappings = a} :: RemixSettings)
{-# DEPRECATED rsChannelMappings "Use generic-lens or generic-optics with 'channelMappings' instead." #-}

-- | Number of input channels to be used.
--
-- /Note:/ Consider using 'channelsIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsChannelsIn :: Lens.Lens' RemixSettings (Lude.Maybe Lude.Natural)
rsChannelsIn = Lens.lens (channelsIn :: RemixSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channelsIn = a} :: RemixSettings)
{-# DEPRECATED rsChannelsIn "Use generic-lens or generic-optics with 'channelsIn' instead." #-}

-- | Number of output channels to be produced.
--
-- Valid values: 1, 2, 4, 6, 8
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
            Lude.<$> (x Lude..:? "channelMappings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "channelsIn")
            Lude.<*> (x Lude..:? "channelsOut")
      )

instance Lude.ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("channelMappings" Lude..= channelMappings),
            ("channelsIn" Lude..=) Lude.<$> channelsIn,
            ("channelsOut" Lude..=) Lude.<$> channelsOut
          ]
      )
