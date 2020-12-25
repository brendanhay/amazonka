{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3Settings
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
    esDynamicRangeCompressionLine,
    esDynamicRangeCompressionRf,
    esLfeControl,
    esLfeFilter,
    esLoRoCenterMixLevel,
    esLoRoSurroundMixLevel,
    esLtRtCenterMixLevel,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esPassthroughControl,
    esPhaseControl,
    esSampleRate,
    esStereoDownmix,
    esSurroundExMode,
    esSurroundMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Eac3AttenuationControl as Types
import qualified Network.AWS.MediaConvert.Types.Eac3BitstreamMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3CodingMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3DcFilter as Types
import qualified Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine as Types
import qualified Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf as Types
import qualified Network.AWS.MediaConvert.Types.Eac3LfeControl as Types
import qualified Network.AWS.MediaConvert.Types.Eac3LfeFilter as Types
import qualified Network.AWS.MediaConvert.Types.Eac3MetadataControl as Types
import qualified Network.AWS.MediaConvert.Types.Eac3PassthroughControl as Types
import qualified Network.AWS.MediaConvert.Types.Eac3PhaseControl as Types
import qualified Network.AWS.MediaConvert.Types.Eac3StereoDownmix as Types
import qualified Network.AWS.MediaConvert.Types.Eac3SurroundExMode as Types
import qualified Network.AWS.MediaConvert.Types.Eac3SurroundMode as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /See:/ 'mkEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
    attenuationControl :: Core.Maybe Types.Eac3AttenuationControl,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
    bitrate :: Core.Maybe Core.Natural,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
    bitstreamMode :: Core.Maybe Types.Eac3BitstreamMode,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Core.Maybe Types.Eac3CodingMode,
    -- | Activates a DC highpass filter for all input channels.
    dcFilter :: Core.Maybe Types.Eac3DcFilter,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
    dialnorm :: Core.Maybe Core.Natural,
    -- | Specify the absolute peak level for a signal with dynamic range compression.
    dynamicRangeCompressionLine :: Core.Maybe Types.Eac3DynamicRangeCompressionLine,
    -- | Specify how the service limits the audio dynamic range when compressing the audio.
    dynamicRangeCompressionRf :: Core.Maybe Types.Eac3DynamicRangeCompressionRf,
    -- | When encoding 3/2 audio, controls whether the LFE channel is enabled
    lfeControl :: Core.Maybe Types.Eac3LfeControl,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
    lfeFilter :: Core.Maybe Types.Eac3LfeFilter,
    -- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
    loRoCenterMixLevel :: Core.Maybe Core.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
    loRoSurroundMixLevel :: Core.Maybe Core.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
    ltRtCenterMixLevel :: Core.Maybe Core.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
    ltRtSurroundMixLevel :: Core.Maybe Core.Double,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Core.Maybe Types.Eac3MetadataControl,
    -- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
    passthroughControl :: Core.Maybe Types.Eac3PassthroughControl,
    -- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
    phaseControl :: Core.Maybe Types.Eac3PhaseControl,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
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
      dynamicRangeCompressionLine = Core.Nothing,
      dynamicRangeCompressionRf = Core.Nothing,
      lfeControl = Core.Nothing,
      lfeFilter = Core.Nothing,
      loRoCenterMixLevel = Core.Nothing,
      loRoSurroundMixLevel = Core.Nothing,
      ltRtCenterMixLevel = Core.Nothing,
      ltRtSurroundMixLevel = Core.Nothing,
      metadataControl = Core.Nothing,
      passthroughControl = Core.Nothing,
      phaseControl = Core.Nothing,
      sampleRate = Core.Nothing,
      stereoDownmix = Core.Nothing,
      surroundExMode = Core.Nothing,
      surroundMode = Core.Nothing
    }

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'attenuationControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAttenuationControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3AttenuationControl)
esAttenuationControl = Lens.field @"attenuationControl"
{-# DEPRECATED esAttenuationControl "Use generic-lens or generic-optics with 'attenuationControl' instead." #-}

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitrate :: Lens.Lens' Eac3Settings (Core.Maybe Core.Natural)
esBitrate = Lens.field @"bitrate"
{-# DEPRECATED esBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
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

-- | Activates a DC highpass filter for all input channels.
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

-- | Specify the absolute peak level for a signal with dynamic range compression.
--
-- /Note:/ Consider using 'dynamicRangeCompressionLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDynamicRangeCompressionLine :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3DynamicRangeCompressionLine)
esDynamicRangeCompressionLine = Lens.field @"dynamicRangeCompressionLine"
{-# DEPRECATED esDynamicRangeCompressionLine "Use generic-lens or generic-optics with 'dynamicRangeCompressionLine' instead." #-}

-- | Specify how the service limits the audio dynamic range when compressing the audio.
--
-- /Note:/ Consider using 'dynamicRangeCompressionRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDynamicRangeCompressionRf :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3DynamicRangeCompressionRf)
esDynamicRangeCompressionRf = Lens.field @"dynamicRangeCompressionRf"
{-# DEPRECATED esDynamicRangeCompressionRf "Use generic-lens or generic-optics with 'dynamicRangeCompressionRf' instead." #-}

-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
--
-- /Note:/ Consider using 'lfeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3LfeControl)
esLfeControl = Lens.field @"lfeControl"
{-# DEPRECATED esLfeControl "Use generic-lens or generic-optics with 'lfeControl' instead." #-}

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeFilter :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3LfeFilter)
esLfeFilter = Lens.field @"lfeFilter"
{-# DEPRECATED esLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLoRoCenterMixLevel = Lens.field @"loRoCenterMixLevel"
{-# DEPRECATED esLoRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLoRoSurroundMixLevel = Lens.field @"loRoSurroundMixLevel"
{-# DEPRECATED esLoRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtCenterMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLtRtCenterMixLevel = Lens.field @"ltRtCenterMixLevel"
{-# DEPRECATED esLtRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Core.Maybe Core.Double)
esLtRtSurroundMixLevel = Lens.field @"ltRtSurroundMixLevel"
{-# DEPRECATED esLtRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead." #-}

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esMetadataControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3MetadataControl)
esMetadataControl = Lens.field @"metadataControl"
{-# DEPRECATED esMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- /Note:/ Consider using 'passthroughControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassthroughControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3PassthroughControl)
esPassthroughControl = Lens.field @"passthroughControl"
{-# DEPRECATED esPassthroughControl "Use generic-lens or generic-optics with 'passthroughControl' instead." #-}

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'phaseControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPhaseControl :: Lens.Lens' Eac3Settings (Core.Maybe Types.Eac3PhaseControl)
esPhaseControl = Lens.field @"phaseControl"
{-# DEPRECATED esPhaseControl "Use generic-lens or generic-optics with 'phaseControl' instead." #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSampleRate :: Lens.Lens' Eac3Settings (Core.Maybe Core.Natural)
esSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED esSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
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
            ("dynamicRangeCompressionLine" Core..=)
              Core.<$> dynamicRangeCompressionLine,
            ("dynamicRangeCompressionRf" Core..=)
              Core.<$> dynamicRangeCompressionRf,
            ("lfeControl" Core..=) Core.<$> lfeControl,
            ("lfeFilter" Core..=) Core.<$> lfeFilter,
            ("loRoCenterMixLevel" Core..=) Core.<$> loRoCenterMixLevel,
            ("loRoSurroundMixLevel" Core..=) Core.<$> loRoSurroundMixLevel,
            ("ltRtCenterMixLevel" Core..=) Core.<$> ltRtCenterMixLevel,
            ("ltRtSurroundMixLevel" Core..=) Core.<$> ltRtSurroundMixLevel,
            ("metadataControl" Core..=) Core.<$> metadataControl,
            ("passthroughControl" Core..=) Core.<$> passthroughControl,
            ("phaseControl" Core..=) Core.<$> phaseControl,
            ("sampleRate" Core..=) Core.<$> sampleRate,
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
          Core.<*> (x Core..:? "dynamicRangeCompressionLine")
          Core.<*> (x Core..:? "dynamicRangeCompressionRf")
          Core.<*> (x Core..:? "lfeControl")
          Core.<*> (x Core..:? "lfeFilter")
          Core.<*> (x Core..:? "loRoCenterMixLevel")
          Core.<*> (x Core..:? "loRoSurroundMixLevel")
          Core.<*> (x Core..:? "ltRtCenterMixLevel")
          Core.<*> (x Core..:? "ltRtSurroundMixLevel")
          Core.<*> (x Core..:? "metadataControl")
          Core.<*> (x Core..:? "passthroughControl")
          Core.<*> (x Core..:? "phaseControl")
          Core.<*> (x Core..:? "sampleRate")
          Core.<*> (x Core..:? "stereoDownmix")
          Core.<*> (x Core..:? "surroundExMode")
          Core.<*> (x Core..:? "surroundMode")
