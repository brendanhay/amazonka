-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3Settings
  ( Mp3Settings (..),

    -- * Smart constructor
    mkMp3Settings,

    -- * Lenses
    mp3Channels,
    mp3RateControlMode,
    mp3SampleRate,
    mp3Bitrate,
    mp3VbrQuality,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Mp3RateControlMode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- /See:/ 'mkMp3Settings' smart constructor.
data Mp3Settings = Mp3Settings'
  { channels ::
      Lude.Maybe Lude.Natural,
    rateControlMode :: Lude.Maybe Mp3RateControlMode,
    sampleRate :: Lude.Maybe Lude.Natural,
    bitrate :: Lude.Maybe Lude.Natural,
    vbrQuality :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mp3Settings' with the minimum fields required to make a request.
--
-- * 'bitrate' - Specify the average bitrate in bits per second.
-- * 'channels' - Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
-- * 'rateControlMode' - Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
-- * 'sampleRate' - Sample rate in hz.
-- * 'vbrQuality' - Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
mkMp3Settings ::
  Mp3Settings
mkMp3Settings =
  Mp3Settings'
    { channels = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      bitrate = Lude.Nothing,
      vbrQuality = Lude.Nothing
    }

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mp3Channels :: Lens.Lens' Mp3Settings (Lude.Maybe Lude.Natural)
mp3Channels = Lens.lens (channels :: Mp3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: Mp3Settings)
{-# DEPRECATED mp3Channels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mp3RateControlMode :: Lens.Lens' Mp3Settings (Lude.Maybe Mp3RateControlMode)
mp3RateControlMode = Lens.lens (rateControlMode :: Mp3Settings -> Lude.Maybe Mp3RateControlMode) (\s a -> s {rateControlMode = a} :: Mp3Settings)
{-# DEPRECATED mp3RateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mp3SampleRate :: Lens.Lens' Mp3Settings (Lude.Maybe Lude.Natural)
mp3SampleRate = Lens.lens (sampleRate :: Mp3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: Mp3Settings)
{-# DEPRECATED mp3SampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Specify the average bitrate in bits per second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mp3Bitrate :: Lens.Lens' Mp3Settings (Lude.Maybe Lude.Natural)
mp3Bitrate = Lens.lens (bitrate :: Mp3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Mp3Settings)
{-# DEPRECATED mp3Bitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mp3VbrQuality :: Lens.Lens' Mp3Settings (Lude.Maybe Lude.Natural)
mp3VbrQuality = Lens.lens (vbrQuality :: Mp3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {vbrQuality = a} :: Mp3Settings)
{-# DEPRECATED mp3VbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

instance Lude.FromJSON Mp3Settings where
  parseJSON =
    Lude.withObject
      "Mp3Settings"
      ( \x ->
          Mp3Settings'
            Lude.<$> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "vbrQuality")
      )

instance Lude.ToJSON Mp3Settings where
  toJSON Mp3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("channels" Lude..=) Lude.<$> channels,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("vbrQuality" Lude..=) Lude.<$> vbrQuality
          ]
      )
