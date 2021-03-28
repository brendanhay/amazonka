{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Ac3Settings
  ( Ac3Settings (..)
  -- * Smart constructor
  , mkAc3Settings
  -- * Lenses
  , aBitrate
  , aBitstreamMode
  , aCodingMode
  , aDialnorm
  , aDynamicRangeCompressionProfile
  , aLfeFilter
  , aMetadataControl
  , aSampleRate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Ac3BitstreamMode as Types
import qualified Network.AWS.MediaConvert.Types.Ac3CodingMode as Types
import qualified Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile as Types
import qualified Network.AWS.MediaConvert.Types.Ac3LfeFilter as Types
import qualified Network.AWS.MediaConvert.Types.Ac3MetadataControl as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /See:/ 'mkAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { bitrate :: Core.Maybe Core.Natural
    -- ^ Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
  , bitstreamMode :: Core.Maybe Types.Ac3BitstreamMode
    -- ^ Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
  , codingMode :: Core.Maybe Types.Ac3CodingMode
    -- ^ Dolby Digital coding mode. Determines number of channels.
  , dialnorm :: Core.Maybe Core.Natural
    -- ^ Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
  , dynamicRangeCompressionProfile :: Core.Maybe Types.Ac3DynamicRangeCompressionProfile
    -- ^ If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
  , lfeFilter :: Core.Maybe Types.Ac3LfeFilter
    -- ^ Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
  , metadataControl :: Core.Maybe Types.Ac3MetadataControl
    -- ^ When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
  , sampleRate :: Core.Maybe Core.Natural
    -- ^ This value is always 48000. It represents the sample rate in Hz.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ac3Settings' value with any optional fields omitted.
mkAc3Settings
    :: Ac3Settings
mkAc3Settings
  = Ac3Settings'{bitrate = Core.Nothing,
                 bitstreamMode = Core.Nothing, codingMode = Core.Nothing,
                 dialnorm = Core.Nothing,
                 dynamicRangeCompressionProfile = Core.Nothing,
                 lfeFilter = Core.Nothing, metadataControl = Core.Nothing,
                 sampleRate = Core.Nothing}

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitrate :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
aBitrate = Lens.field @"bitrate"
{-# INLINEABLE aBitrate #-}
{-# DEPRECATED bitrate "Use generic-lens or generic-optics with 'bitrate' instead"  #-}

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitstreamMode :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3BitstreamMode)
aBitstreamMode = Lens.field @"bitstreamMode"
{-# INLINEABLE aBitstreamMode #-}
{-# DEPRECATED bitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead"  #-}

-- | Dolby Digital coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCodingMode :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3CodingMode)
aCodingMode = Lens.field @"codingMode"
{-# INLINEABLE aCodingMode #-}
{-# DEPRECATED codingMode "Use generic-lens or generic-optics with 'codingMode' instead"  #-}

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDialnorm :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
aDialnorm = Lens.field @"dialnorm"
{-# INLINEABLE aDialnorm #-}
{-# DEPRECATED dialnorm "Use generic-lens or generic-optics with 'dialnorm' instead"  #-}

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
--
-- /Note:/ Consider using 'dynamicRangeCompressionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3DynamicRangeCompressionProfile)
aDynamicRangeCompressionProfile = Lens.field @"dynamicRangeCompressionProfile"
{-# INLINEABLE aDynamicRangeCompressionProfile #-}
{-# DEPRECATED dynamicRangeCompressionProfile "Use generic-lens or generic-optics with 'dynamicRangeCompressionProfile' instead"  #-}

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLfeFilter :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3LfeFilter)
aLfeFilter = Lens.field @"lfeFilter"
{-# INLINEABLE aLfeFilter #-}
{-# DEPRECATED lfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead"  #-}

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetadataControl :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3MetadataControl)
aMetadataControl = Lens.field @"metadataControl"
{-# INLINEABLE aMetadataControl #-}
{-# DEPRECATED metadataControl "Use generic-lens or generic-optics with 'metadataControl' instead"  #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSampleRate :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
aSampleRate = Lens.field @"sampleRate"
{-# INLINEABLE aSampleRate #-}
{-# DEPRECATED sampleRate "Use generic-lens or generic-optics with 'sampleRate' instead"  #-}

instance Core.FromJSON Ac3Settings where
        toJSON Ac3Settings{..}
          = Core.object
              (Core.catMaybes
                 [("bitrate" Core..=) Core.<$> bitrate,
                  ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
                  ("codingMode" Core..=) Core.<$> codingMode,
                  ("dialnorm" Core..=) Core.<$> dialnorm,
                  ("dynamicRangeCompressionProfile" Core..=) Core.<$>
                    dynamicRangeCompressionProfile,
                  ("lfeFilter" Core..=) Core.<$> lfeFilter,
                  ("metadataControl" Core..=) Core.<$> metadataControl,
                  ("sampleRate" Core..=) Core.<$> sampleRate])

instance Core.FromJSON Ac3Settings where
        parseJSON
          = Core.withObject "Ac3Settings" Core.$
              \ x ->
                Ac3Settings' Core.<$>
                  (x Core..:? "bitrate") Core.<*> x Core..:? "bitstreamMode" Core.<*>
                    x Core..:? "codingMode"
                    Core.<*> x Core..:? "dialnorm"
                    Core.<*> x Core..:? "dynamicRangeCompressionProfile"
                    Core.<*> x Core..:? "lfeFilter"
                    Core.<*> x Core..:? "metadataControl"
                    Core.<*> x Core..:? "sampleRate"
