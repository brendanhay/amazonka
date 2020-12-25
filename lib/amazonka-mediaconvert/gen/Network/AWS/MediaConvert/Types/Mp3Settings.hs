{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mBitrate,
    mChannels,
    mRateControlMode,
    mSampleRate,
    mVbrQuality,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Mp3RateControlMode as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- /See:/ 'mkMp3Settings' smart constructor.
data Mp3Settings = Mp3Settings'
  { -- | Specify the average bitrate in bits per second.
    bitrate :: Core.Maybe Core.Natural,
    -- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
    channels :: Core.Maybe Core.Natural,
    -- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
    rateControlMode :: Core.Maybe Types.Mp3RateControlMode,
    -- | Sample rate in hz.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
    vbrQuality :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mp3Settings' value with any optional fields omitted.
mkMp3Settings ::
  Mp3Settings
mkMp3Settings =
  Mp3Settings'
    { bitrate = Core.Nothing,
      channels = Core.Nothing,
      rateControlMode = Core.Nothing,
      sampleRate = Core.Nothing,
      vbrQuality = Core.Nothing
    }

-- | Specify the average bitrate in bits per second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBitrate :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mBitrate = Lens.field @"bitrate"
{-# DEPRECATED mBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mChannels :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mChannels = Lens.field @"channels"
{-# DEPRECATED mChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRateControlMode :: Lens.Lens' Mp3Settings (Core.Maybe Types.Mp3RateControlMode)
mRateControlMode = Lens.field @"rateControlMode"
{-# DEPRECATED mRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSampleRate :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED mSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mVbrQuality :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mVbrQuality = Lens.field @"vbrQuality"
{-# DEPRECATED mVbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

instance Core.FromJSON Mp3Settings where
  toJSON Mp3Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("channels" Core..=) Core.<$> channels,
            ("rateControlMode" Core..=) Core.<$> rateControlMode,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("vbrQuality" Core..=) Core.<$> vbrQuality
          ]
      )

instance Core.FromJSON Mp3Settings where
  parseJSON =
    Core.withObject "Mp3Settings" Core.$
      \x ->
        Mp3Settings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "channels")
          Core.<*> (x Core..:? "rateControlMode")
          Core.<*> (x Core..:? "sampleRate")
          Core.<*> (x Core..:? "vbrQuality")
