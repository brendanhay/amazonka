{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AiffSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AiffSettings
  ( AiffSettings (..)
  -- * Smart constructor
  , mkAiffSettings
  -- * Lenses
  , assBitDepth
  , assChannels
  , assSampleRate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /See:/ 'mkAiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { bitDepth :: Core.Maybe Core.Natural
    -- ^ Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
  , channels :: Core.Maybe Core.Natural
    -- ^ Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
  , sampleRate :: Core.Maybe Core.Natural
    -- ^ Sample rate in hz.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AiffSettings' value with any optional fields omitted.
mkAiffSettings
    :: AiffSettings
mkAiffSettings
  = AiffSettings'{bitDepth = Core.Nothing, channels = Core.Nothing,
                  sampleRate = Core.Nothing}

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assBitDepth :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
assBitDepth = Lens.field @"bitDepth"
{-# INLINEABLE assBitDepth #-}
{-# DEPRECATED bitDepth "Use generic-lens or generic-optics with 'bitDepth' instead"  #-}

-- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assChannels :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
assChannels = Lens.field @"channels"
{-# INLINEABLE assChannels #-}
{-# DEPRECATED channels "Use generic-lens or generic-optics with 'channels' instead"  #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSampleRate :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
assSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE assSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

instance Core.FromJSON AiffSettings where
        toJSON AiffSettings{..}
          = Core.object
              (Core.catMaybes
                 [("bitDepth" Core..=) Core.<$> bitDepth,
                  ("channels" Core..=) Core.<$> channels,
                  ("sampleRate" Core..=) Core.<$> sampleRate])

instance Core.FromJSON AiffSettings where
        parseJSON
          = Core.withObject "AiffSettings" Core.$
              \ x ->
                AiffSettings' Core.<$>
                  (x Core..:? "bitDepth") Core.<*> x Core..:? "channels" Core.<*>
                    x Core..:? "sampleRate"
