{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.WavSettings
  ( WavSettings (..)
  -- * Smart constructor
  , mkWavSettings
  -- * Lenses
  , wsBitDepth
  , wsCodingMode
  , wsSampleRate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.WavCodingMode as Types
import qualified Network.AWS.Prelude as Core

-- | Wav Settings
--
-- /See:/ 'mkWavSettings' smart constructor.
data WavSettings = WavSettings'
  { bitDepth :: Core.Maybe Core.Double
    -- ^ Bits per sample.
  , codingMode :: Core.Maybe Types.WavCodingMode
    -- ^ The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
  , sampleRate :: Core.Maybe Core.Double
    -- ^ Sample rate in Hz.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WavSettings' value with any optional fields omitted.
mkWavSettings
    :: WavSettings
mkWavSettings
  = WavSettings'{bitDepth = Core.Nothing, codingMode = Core.Nothing,
                 sampleRate = Core.Nothing}

-- | Bits per sample.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsBitDepth :: Lens.Lens' WavSettings (Core.Maybe Core.Double)
wsBitDepth = Lens.field @"bitDepth"
{-# INLINEABLE wsBitDepth #-}
{-# DEPRECATED bitDepth "Use generic-lens or generic-optics with 'bitDepth' instead"  #-}

-- | The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsCodingMode :: Lens.Lens' WavSettings (Core.Maybe Types.WavCodingMode)
wsCodingMode = Lens.field @"codingMode"
{-# INLINEABLE wsCodingMode #-}
{-# DEPRECATED codingMode "Use generic-lens or generic-optics with 'codingMode' instead"  #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsSampleRate :: Lens.Lens' WavSettings (Core.Maybe Core.Double)
wsSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE wsSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

instance Core.FromJSON WavSettings where
        toJSON WavSettings{..}
          = Core.object
              (Core.catMaybes
                 [("bitDepth" Core..=) Core.<$> bitDepth,
                  ("codingMode" Core..=) Core.<$> codingMode,
                  ("sampleRate" Core..=) Core.<$> sampleRate])

instance Core.FromJSON WavSettings where
        parseJSON
          = Core.withObject "WavSettings" Core.$
              \ x ->
                WavSettings' Core.<$>
                  (x Core..:? "bitDepth") Core.<*> x Core..:? "codingMode" Core.<*>
                    x Core..:? "sampleRate"
