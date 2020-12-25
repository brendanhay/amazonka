{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3Settings
  ( Ac3Settings (..),

    -- * Smart constructor
    mkAc3Settings,

    -- * Lenses
    aBitrate,
    aBitstreamMode,
    aCodingMode,
    aDialnorm,
    aDrcProfile,
    aLfeFilter,
    aMetadataControl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Ac3BitstreamMode as Types
import qualified Network.AWS.MediaLive.Types.Ac3CodingMode as Types
import qualified Network.AWS.MediaLive.Types.Ac3DrcProfile as Types
import qualified Network.AWS.MediaLive.Types.Ac3LfeFilter as Types
import qualified Network.AWS.MediaLive.Types.Ac3MetadataControl as Types
import qualified Network.AWS.Prelude as Core

-- | Ac3 Settings
--
-- /See:/ 'mkAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
    bitrate :: Core.Maybe Core.Double,
    -- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
    bitstreamMode :: Core.Maybe Types.Ac3BitstreamMode,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Core.Maybe Types.Ac3CodingMode,
    -- | Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
    dialnorm :: Core.Maybe Core.Natural,
    -- | If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
    drcProfile :: Core.Maybe Types.Ac3DrcProfile,
    -- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
    lfeFilter :: Core.Maybe Types.Ac3LfeFilter,
    -- | When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Core.Maybe Types.Ac3MetadataControl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ac3Settings' value with any optional fields omitted.
mkAc3Settings ::
  Ac3Settings
mkAc3Settings =
  Ac3Settings'
    { bitrate = Core.Nothing,
      bitstreamMode = Core.Nothing,
      codingMode = Core.Nothing,
      dialnorm = Core.Nothing,
      drcProfile = Core.Nothing,
      lfeFilter = Core.Nothing,
      metadataControl = Core.Nothing
    }

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitrate :: Lens.Lens' Ac3Settings (Core.Maybe Core.Double)
aBitrate = Lens.field @"bitrate"
{-# DEPRECATED aBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitstreamMode :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3BitstreamMode)
aBitstreamMode = Lens.field @"bitstreamMode"
{-# DEPRECATED aBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | Dolby Digital coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCodingMode :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3CodingMode)
aCodingMode = Lens.field @"codingMode"
{-# DEPRECATED aCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDialnorm :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
aDialnorm = Lens.field @"dialnorm"
{-# DEPRECATED aDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

-- | If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
--
-- /Note:/ Consider using 'drcProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDrcProfile :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3DrcProfile)
aDrcProfile = Lens.field @"drcProfile"
{-# DEPRECATED aDrcProfile "Use generic-lens or generic-optics with 'drcProfile' instead." #-}

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLfeFilter :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3LfeFilter)
aLfeFilter = Lens.field @"lfeFilter"
{-# DEPRECATED aLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetadataControl :: Lens.Lens' Ac3Settings (Core.Maybe Types.Ac3MetadataControl)
aMetadataControl = Lens.field @"metadataControl"
{-# DEPRECATED aMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

instance Core.FromJSON Ac3Settings where
  toJSON Ac3Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("dialnorm" Core..=) Core.<$> dialnorm,
            ("drcProfile" Core..=) Core.<$> drcProfile,
            ("lfeFilter" Core..=) Core.<$> lfeFilter,
            ("metadataControl" Core..=) Core.<$> metadataControl
          ]
      )

instance Core.FromJSON Ac3Settings where
  parseJSON =
    Core.withObject "Ac3Settings" Core.$
      \x ->
        Ac3Settings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "bitstreamMode")
          Core.<*> (x Core..:? "codingMode")
          Core.<*> (x Core..:? "dialnorm")
          Core.<*> (x Core..:? "drcProfile")
          Core.<*> (x Core..:? "lfeFilter")
          Core.<*> (x Core..:? "metadataControl")
