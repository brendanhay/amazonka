{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OpusSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OpusSettings
  ( OpusSettings (..),

    -- * Smart constructor
    mkOpusSettings,

    -- * Lenses
    osChannels,
    osSampleRate,
    osBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- /See:/ 'mkOpusSettings' smart constructor.
data OpusSettings = OpusSettings'
  { -- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
    channels :: Lude.Maybe Lude.Natural,
    -- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
    sampleRate :: Lude.Maybe Lude.Natural,
    -- | Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
    bitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpusSettings' with the minimum fields required to make a request.
--
-- * 'channels' - Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
-- * 'sampleRate' - Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
-- * 'bitrate' - Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
mkOpusSettings ::
  OpusSettings
mkOpusSettings =
  OpusSettings'
    { channels = Lude.Nothing,
      sampleRate = Lude.Nothing,
      bitrate = Lude.Nothing
    }

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osChannels :: Lens.Lens' OpusSettings (Lude.Maybe Lude.Natural)
osChannels = Lens.lens (channels :: OpusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: OpusSettings)
{-# DEPRECATED osChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSampleRate :: Lens.Lens' OpusSettings (Lude.Maybe Lude.Natural)
osSampleRate = Lens.lens (sampleRate :: OpusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: OpusSettings)
{-# DEPRECATED osSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osBitrate :: Lens.Lens' OpusSettings (Lude.Maybe Lude.Natural)
osBitrate = Lens.lens (bitrate :: OpusSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: OpusSettings)
{-# DEPRECATED osBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

instance Lude.FromJSON OpusSettings where
  parseJSON =
    Lude.withObject
      "OpusSettings"
      ( \x ->
          OpusSettings'
            Lude.<$> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "bitrate")
      )

instance Lude.ToJSON OpusSettings where
  toJSON OpusSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channels" Lude..=) Lude.<$> channels,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("bitrate" Lude..=) Lude.<$> bitrate
          ]
      )
