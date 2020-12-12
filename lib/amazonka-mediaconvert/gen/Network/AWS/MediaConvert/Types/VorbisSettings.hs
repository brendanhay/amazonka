{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VorbisSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VorbisSettings
  ( VorbisSettings (..),

    -- * Smart constructor
    mkVorbisSettings,

    -- * Lenses
    vsChannels,
    vsSampleRate,
    vsVbrQuality,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- /See:/ 'mkVorbisSettings' smart constructor.
data VorbisSettings = VorbisSettings'
  { channels ::
      Lude.Maybe Lude.Natural,
    sampleRate :: Lude.Maybe Lude.Natural,
    vbrQuality :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VorbisSettings' with the minimum fields required to make a request.
--
-- * 'channels' - Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
-- * 'sampleRate' - Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
-- * 'vbrQuality' - Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
mkVorbisSettings ::
  VorbisSettings
mkVorbisSettings =
  VorbisSettings'
    { channels = Lude.Nothing,
      sampleRate = Lude.Nothing,
      vbrQuality = Lude.Nothing
    }

-- | Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsChannels :: Lens.Lens' VorbisSettings (Lude.Maybe Lude.Natural)
vsChannels = Lens.lens (channels :: VorbisSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: VorbisSettings)
{-# DEPRECATED vsChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSampleRate :: Lens.Lens' VorbisSettings (Lude.Maybe Lude.Natural)
vsSampleRate = Lens.lens (sampleRate :: VorbisSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: VorbisSettings)
{-# DEPRECATED vsSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVbrQuality :: Lens.Lens' VorbisSettings (Lude.Maybe Lude.Int)
vsVbrQuality = Lens.lens (vbrQuality :: VorbisSettings -> Lude.Maybe Lude.Int) (\s a -> s {vbrQuality = a} :: VorbisSettings)
{-# DEPRECATED vsVbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

instance Lude.FromJSON VorbisSettings where
  parseJSON =
    Lude.withObject
      "VorbisSettings"
      ( \x ->
          VorbisSettings'
            Lude.<$> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "vbrQuality")
      )

instance Lude.ToJSON VorbisSettings where
  toJSON VorbisSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channels" Lude..=) Lude.<$> channels,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("vbrQuality" Lude..=) Lude.<$> vbrQuality
          ]
      )
