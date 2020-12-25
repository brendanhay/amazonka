{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3Settings
  ( Eac3Settings (..),

    -- * Smart constructor
    mkEac3Settings,

    -- * Lenses
    esAttenuationControl,
    esBitrate,
    esBitstreamMode,
    esCodingMode,
    esDcFilter,
    esDialnorm,
    esDrcLine,
    esDrcRf,
    esLfeControl,
    esLfeFilter,
    esLoRoCenterMixLevel,
    esLoRoSurroundMixLevel,
    esLtRtCenterMixLevel,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esPassthroughControl,
    esPhaseControl,
    esStereoDownmix,
    esSurroundExMode,
    esSurroundMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Eac3AttenuationControl as Types
import qualified Network.AWS.MediaLive.Types.Eac3BitstreamMode as Types
import qualified Network.AWS.MediaLive.Types.Eac3CodingMode as Types
import qualified Network.AWS.MediaLive.Types.Eac3DcFilter as Types
import qualified Network.AWS.MediaLive.Types.Eac3DrcLine as Types
import qualified Network.AWS.MediaLive.Types.Eac3DrcRf as Types
import qualified Network.AWS.MediaLive.Types.Eac3LfeControl as Types
import qualified Network.AWS.MediaLive.Types.Eac3LfeFilter as Types
import qualified Network.AWS.MediaLive.Types.Eac3MetadataControl as Types
import qualified Network.AWS.MediaLive.Types.Eac3PassthroughControl as Types
import qualified Network.AWS.MediaLive.Types.Eac3PhaseControl as Types
import qualified Network.AWS.MediaLive.Types.Eac3StereoDownmix as Types
import qualified Network.AWS.MediaLive.Types.Eac3SurroundExMode as Types
import qualified Network.AWS.MediaLive.Types.Eac3SurroundMode as Types
import qualified Network.AWS.Prelude as Core

-- | Eac3 Settings
--
-- /See:/ 'mkEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
    attenuationControl :: Core.Maybe Types.Eac3AttenuationControl,
    -- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
    bitrate :: Core.Maybe Core.Double,
    -- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
    bitstreamMode :: Core.Maybe Types.Eac3BitstreamMode,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Core.Maybe Types.Eac3CodingMode,
    -- | When set to enabled, activates a DC highpass filter for all input channels.
    dcFilter :: Core.Maybe Types.Eac3DcFilter,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
    dialnorm :: Core.Maybe Core.Natural,
    -- | Sets the Dolby dynamic range compression profile.
    drcLine :: Core.Maybe Types.Eac3DrcLine,
    -- | Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
    drcRf :: Core.Maybe Types.Eac3DrcRf,
    -- | When encoding 3/2 audio, setting to lfe enables the LFE channel
    lfeControl :: Core.Maybe Types.Eac3LfeControl,
    -- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
    lfeFilter :: Core.Maybe Types.Eac3LfeFilter,
    -- | Left only/Right only center mix level. Only used for 3/2 coding mode.
    loRoCenterMixLevel :: Core.Maybe Core.Double,
    -- | Left only/Right only surround mix level. Only used for 3/2 coding mode.
    loRoSurroundMixLevel :: Core.Maybe Core.Double,
    -- | Left total/Right total center mix level. Only used for 3/2 coding mode.
    ltRtCenterMixLevel :: Core.Maybe Core.Double,
    -- | Left total/Right total surround mix level. Only used for 3/2 coding mode.
    ltRtSurroundMixLevel :: Core.Maybe Core.Double,
    -- | When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Core.Maybe Types.Eac3MetadataControl,
    -- | When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
    passthroughControl :: Core.Maybe Types.Eac3PassthroughControl,
    -- | When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
    phaseControl :: Core.Maybe Types.Eac3PhaseControl,
    -- | Stereo downmix preference. Only used for 3/2 coding mode.
    stereoDownmix :: Core.Maybe Types.Eac3StereoDownmix,
    -- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Core.Maybe Types.Eac3SurroundExMode,
    -- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
    surroundMode :: Core.Maybe Types.Eac3SurroundMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Eac3Settings' value with any optional fields omitted.
mkEac3Settings ::
  Eac3Settings
mkEac3Settings =
  Eac3Settings'
    { attenuationControl = Core.Nothing,
      bitrate = Core.Nothing,
      bitstreamMode = Core.Nothing,
      codingMode = Core.Nothing,
      dcFilter = Core.Nothing,
      dialnorm = Core.Nothing,
      drcLine = Core.Nothing,
      drcRf = Core.Nothing,
      lfeControl = Core.Nothing,
      lfeFilter = Core.Nothing,
      loRoCenterMixLevel = Core.Nothing,
      loRoSurroundMixLevel = Core.Nothing,
      ltRtCenterMixLevel = Core.Nothing,
      ltRtSurroundMixLevel = Core.Nothing,
      metadataControl = Core.Nothing,
      passthroughControl = Core.Nothing,
      phaseControl = Core.Nothing,
      stereoDownmix = Core.Nothing,
      surroundExMode = Core.Nothing,
      surroundMode = Core.Nothing
    }

-- | When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'attenuationControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAttenuationControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3AttenuationControl)
esAttenuationControl = Lens.field @"attenuationControl"
{-# DEPRECATED esAttenuationControl "Use generic-lens or generic-optics with 'attenuationControl' instead." #-}

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitrate :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esBitrate = Lens.field @"bitrate"
{-# DEPRECATED esBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitstreamMode :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3BitstreamMode)
esBitstreamMode = Lens.field @"bitstreamMode"
{-# DEPRECATED esBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | Dolby Digital Plus coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCodingMode :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3CodingMode)
esCodingMode = Lens.field @"codingMode"
{-# DEPRECATED esCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | When set to enabled, activates a DC highpass filter for all input channels.
--
-- /Note:/ Consider using 'dcFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDcFilter :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3DcFilter)
esDcFilter = Lens.field @"dcFilter"
{-# DEPRECATED esDcFilter "Use generic-lens or generic-optics with 'dcFilter' instead." #-}

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDialnorm :: Lens.Lens' Eac3Settings (Core.Maybe Core.Natural)
esDialnorm = Lens.field @"dialnorm"
{-# DEPRECATED esDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

-- | Sets the Dolby dynamic range compression profile.
--
-- /Note:/ Consider using 'drcLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDrcLine :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3DrcLine)
esDrcLine = Lens.field @"drcLine"
{-# DEPRECATED esDrcLine "Use generic-lens or generic-optics with 'drcLine' instead." #-}

-- | Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
--
-- /Note:/ Consider using 'drcRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDrcRf :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3DrcRf)
esDrcRf = Lens.field @"drcRf"
{-# DEPRECATED esDrcRf "Use generic-lens or generic-optics with 'drcRf' instead." #-}

-- | When encoding 3/2 audio, setting to lfe enables the LFE channel
--
-- /Note:/ Consider using 'lfeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3LfeControl)
esLfeControl = Lens.field @"lfeControl"
{-# DEPRECATED esLfeControl "Use generic-lens or generic-optics with 'lfeControl' instead." #-}

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeFilter :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3LfeFilter)
esLfeFilter = Lens.field @"lfeFilter"
{-# DEPRECATED esLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | Left only/Right only center mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLoRoCenterMixLevel = Lens.field @"loRoCenterMixLevel"
{-# DEPRECATED esLoRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead." #-}

-- | Left only/Right only surround mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLoRoSurroundMixLevel = Lens.field @"loRoSurroundMixLevel"
{-# DEPRECATED esLoRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead." #-}

-- | Left total/Right total center mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLtRtCenterMixLevel = Lens.field @"ltRtCenterMixLevel"
{-# DEPRECATED esLtRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead." #-}

-- | Left total/Right total surround mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLtRtSurroundMixLevel = Lens.field @"ltRtSurroundMixLevel"
{-# DEPRECATED esLtRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead." #-}

-- | When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esMetadataControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3MetadataControl)
esMetadataControl = Lens.field @"metadataControl"
{-# DEPRECATED esMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- /Note:/ Consider using 'passthroughControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassthroughControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3PassthroughControl)
esPassthroughControl = Lens.field @"passthroughControl"
{-# DEPRECATED esPassthroughControl "Use generic-lens or generic-optics with 'passthroughControl' instead." #-}

-- | When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'phaseControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPhaseControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3PhaseControl)
esPhaseControl = Lens.field @"phaseControl"
{-# DEPRECATED esPhaseControl "Use generic-lens or generic-optics with 'phaseControl' instead." #-}

-- | Stereo downmix preference. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'stereoDownmix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStereoDownmix :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3StereoDownmix)
esStereoDownmix = Lens.field @"stereoDownmix"
{-# DEPRECATED esStereoDownmix "Use generic-lens or generic-optics with 'stereoDownmix' instead." #-}

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
--
-- /Note:/ Consider using 'surroundExMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundExMode :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3SurroundExMode)
esSurroundExMode = Lens.field @"surroundExMode"
{-# DEPRECATED esSurroundExMode "Use generic-lens or generic-optics with 'surroundExMode' instead." #-}

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
--
-- /Note:/ Consider using 'surroundMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundMode :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3SurroundMode)
esSurroundMode = Lens.field @"surroundMode"
{-# DEPRECATED esSurroundMode "Use generic-lens or generic-optics with 'surroundMode' instead." #-}

instance Core.FromJSON Eac3Settings where
  toJSON Eac3Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("attenuationControl" Core..=) Core.<$> attenuationControl,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("dcFilter" Core..=) Core.<$> dcFilter,
            ("dialnorm" Core..=) Core.<$> dialnorm,
            ("drcLine" Core..=) Core.<$> drcLine,
            ("drcRf" Core..=) Core.<$> drcRf,
            ("lfeControl" Core..=) Core.<$> lfeControl,
            ("lfeFilter" Core..=) Core.<$> lfeFilter,
            ("loRoCenterMixLevel" Core..=) Core.<$> loRoCenterMixLevel,
            ("loRoSurroundMixLevel" Core..=) Core.<$> loRoSurroundMixLevel,
            ("ltRtCenterMixLevel" Core..=) Core.<$> ltRtCenterMixLevel,
            ("ltRtSurroundMixLevel" Core..=) Core.<$> ltRtSurroundMixLevel,
            ("metadataControl" Core..=) Core.<$> metadataControl,
            ("passthroughControl" Core..=) Core.<$> passthroughControl,
            ("phaseControl" Core..=) Core.<$> phaseControl,
            ("stereoDownmix" Core..=) Core.<$> stereoDownmix,
            ("surroundExMode" Core..=) Core.<$> surroundExMode,
            ("surroundMode" Core..=) Core.<$> surroundMode
          ]
      )

instance Core.FromJSON Eac3Settings where
  parseJSON =
    Core.withObject "Eac3Settings" Core.$
      \x ->
        Eac3Settings'
          Core.<$> (x Core..:? "attenuationControl")
          Core.<*> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "bitstreamMode")
          Core.<*> (x Core..:? "codingMode")
          Core.<*> (x Core..:? "dcFilter")
          Core.<*> (x Core..:? "dialnorm")
          Core.<*> (x Core..:? "drcLine")
          Core.<*> (x Core..:? "drcRf")
          Core.<*> (x Core..:? "lfeControl")
          Core.<*> (x Core..:? "lfeFilter")
          Core.<*> (x Core..:? "loRoCenterMixLevel")
          Core.<*> (x Core..:? "loRoSurroundMixLevel")
          Core.<*> (x Core..:? "ltRtCenterMixLevel")
          Core.<*> (x Core..:? "ltRtSurroundMixLevel")
          Core.<*> (x Core..:? "metadataControl")
          Core.<*> (x Core..:? "passthroughControl")
          Core.<*> (x Core..:? "phaseControl")
          Core.<*> (x Core..:? "stereoDownmix")
          Core.<*> (x Core..:? "surroundExMode")
          Core.<*> (x Core..:? "surroundMode")
