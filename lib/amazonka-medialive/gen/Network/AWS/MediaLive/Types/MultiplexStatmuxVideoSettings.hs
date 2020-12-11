-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
  ( MultiplexStatmuxVideoSettings (..),

    -- * Smart constructor
    mkMultiplexStatmuxVideoSettings,

    -- * Lenses
    msvsPriority,
    msvsMinimumBitrate,
    msvsMaximumBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statmux rate control settings
--
-- /See:/ 'mkMultiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'
  { priority ::
      Lude.Maybe Lude.Int,
    minimumBitrate ::
      Lude.Maybe Lude.Natural,
    maximumBitrate ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexStatmuxVideoSettings' with the minimum fields required to make a request.
--
-- * 'maximumBitrate' - Maximum statmux bitrate.
-- * 'minimumBitrate' - Minimum statmux bitrate.
-- * 'priority' - The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
mkMultiplexStatmuxVideoSettings ::
  MultiplexStatmuxVideoSettings
mkMultiplexStatmuxVideoSettings =
  MultiplexStatmuxVideoSettings'
    { priority = Lude.Nothing,
      minimumBitrate = Lude.Nothing,
      maximumBitrate = Lude.Nothing
    }

-- | The purpose of the priority is to use a combination of the\nmultiplex rate control algorithm and the QVBR capability of the\nencoder to prioritize the video quality of some channels in a\nmultiplex over others.  Channels that have a higher priority will\nget higher video quality at the expense of the video quality of\nother channels in the multiplex with lower priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsPriority :: Lens.Lens' MultiplexStatmuxVideoSettings (Lude.Maybe Lude.Int)
msvsPriority = Lens.lens (priority :: MultiplexStatmuxVideoSettings -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: MultiplexStatmuxVideoSettings)
{-# DEPRECATED msvsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Minimum statmux bitrate.
--
-- /Note:/ Consider using 'minimumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsMinimumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Lude.Maybe Lude.Natural)
msvsMinimumBitrate = Lens.lens (minimumBitrate :: MultiplexStatmuxVideoSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minimumBitrate = a} :: MultiplexStatmuxVideoSettings)
{-# DEPRECATED msvsMinimumBitrate "Use generic-lens or generic-optics with 'minimumBitrate' instead." #-}

-- | Maximum statmux bitrate.
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msvsMaximumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Lude.Maybe Lude.Natural)
msvsMaximumBitrate = Lens.lens (maximumBitrate :: MultiplexStatmuxVideoSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maximumBitrate = a} :: MultiplexStatmuxVideoSettings)
{-# DEPRECATED msvsMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

instance Lude.FromJSON MultiplexStatmuxVideoSettings where
  parseJSON =
    Lude.withObject
      "MultiplexStatmuxVideoSettings"
      ( \x ->
          MultiplexStatmuxVideoSettings'
            Lude.<$> (x Lude..:? "priority")
            Lude.<*> (x Lude..:? "minimumBitrate")
            Lude.<*> (x Lude..:? "maximumBitrate")
      )

instance Lude.ToJSON MultiplexStatmuxVideoSettings where
  toJSON MultiplexStatmuxVideoSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("priority" Lude..=) Lude.<$> priority,
            ("minimumBitrate" Lude..=) Lude.<$> minimumBitrate,
            ("maximumBitrate" Lude..=) Lude.<$> maximumBitrate
          ]
      )
