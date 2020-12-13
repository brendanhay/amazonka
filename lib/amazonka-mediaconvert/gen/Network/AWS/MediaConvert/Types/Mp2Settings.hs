{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp2Settings
  ( Mp2Settings (..),

    -- * Smart constructor
    mkMp2Settings,

    -- * Lenses
    mssChannels,
    mssSampleRate,
    mssBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /See:/ 'mkMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
    channels :: Lude.Maybe Lude.Natural,
    -- | Sample rate in hz.
    sampleRate :: Lude.Maybe Lude.Natural,
    -- | Specify the average bitrate in bits per second.
    bitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- * 'channels' - Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
-- * 'sampleRate' - Sample rate in hz.
-- * 'bitrate' - Specify the average bitrate in bits per second.
mkMp2Settings ::
  Mp2Settings
mkMp2Settings =
  Mp2Settings'
    { channels = Lude.Nothing,
      sampleRate = Lude.Nothing,
      bitrate = Lude.Nothing
    }

-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssChannels :: Lens.Lens' Mp2Settings (Lude.Maybe Lude.Natural)
mssChannels = Lens.lens (channels :: Mp2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: Mp2Settings)
{-# DEPRECATED mssChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssSampleRate :: Lens.Lens' Mp2Settings (Lude.Maybe Lude.Natural)
mssSampleRate = Lens.lens (sampleRate :: Mp2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: Mp2Settings)
{-# DEPRECATED mssSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Specify the average bitrate in bits per second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssBitrate :: Lens.Lens' Mp2Settings (Lude.Maybe Lude.Natural)
mssBitrate = Lens.lens (bitrate :: Mp2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Mp2Settings)
{-# DEPRECATED mssBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

instance Lude.FromJSON Mp2Settings where
  parseJSON =
    Lude.withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            Lude.<$> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "bitrate")
      )

instance Lude.ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channels" Lude..=) Lude.<$> channels,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("bitrate" Lude..=) Lude.<$> bitrate
          ]
      )
