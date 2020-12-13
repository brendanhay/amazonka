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
    esStereoDownmix,
    esLoRoCenterMixLevel,
    esLtRtCenterMixLevel,
    esLfeFilter,
    esDynamicRangeCompressionLine,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esLoRoSurroundMixLevel,
    esSurroundMode,
    esAttenuationControl,
    esPassthroughControl,
    esBitstreamMode,
    esLfeControl,
    esDynamicRangeCompressionRf,
    esCodingMode,
    esSampleRate,
    esDcFilter,
    esBitrate,
    esPhaseControl,
    esSurroundExMode,
    esDialnorm,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Eac3AttenuationControl
import Network.AWS.MediaConvert.Types.Eac3BitstreamMode
import Network.AWS.MediaConvert.Types.Eac3CodingMode
import Network.AWS.MediaConvert.Types.Eac3DcFilter
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3LfeControl
import Network.AWS.MediaConvert.Types.Eac3LfeFilter
import Network.AWS.MediaConvert.Types.Eac3MetadataControl
import Network.AWS.MediaConvert.Types.Eac3PassthroughControl
import Network.AWS.MediaConvert.Types.Eac3PhaseControl
import Network.AWS.MediaConvert.Types.Eac3StereoDownmix
import Network.AWS.MediaConvert.Types.Eac3SurroundExMode
import Network.AWS.MediaConvert.Types.Eac3SurroundMode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /See:/ 'mkEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
    stereoDownmix :: Lude.Maybe Eac3StereoDownmix,
    -- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
    loRoCenterMixLevel :: Lude.Maybe Lude.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
    ltRtCenterMixLevel :: Lude.Maybe Lude.Double,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
    lfeFilter :: Lude.Maybe Eac3LfeFilter,
    -- | Specify the absolute peak level for a signal with dynamic range compression.
    dynamicRangeCompressionLine :: Lude.Maybe Eac3DynamicRangeCompressionLine,
    -- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
    ltRtSurroundMixLevel :: Lude.Maybe Lude.Double,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Lude.Maybe Eac3MetadataControl,
    -- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
    loRoSurroundMixLevel :: Lude.Maybe Lude.Double,
    -- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
    surroundMode :: Lude.Maybe Eac3SurroundMode,
    -- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
    attenuationControl :: Lude.Maybe Eac3AttenuationControl,
    -- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
    passthroughControl :: Lude.Maybe Eac3PassthroughControl,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
    bitstreamMode :: Lude.Maybe Eac3BitstreamMode,
    -- | When encoding 3/2 audio, controls whether the LFE channel is enabled
    lfeControl :: Lude.Maybe Eac3LfeControl,
    -- | Specify how the service limits the audio dynamic range when compressing the audio.
    dynamicRangeCompressionRf :: Lude.Maybe Eac3DynamicRangeCompressionRf,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Lude.Maybe Eac3CodingMode,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Lude.Maybe Lude.Natural,
    -- | Activates a DC highpass filter for all input channels.
    dcFilter :: Lude.Maybe Eac3DcFilter,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
    phaseControl :: Lude.Maybe Eac3PhaseControl,
    -- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Lude.Maybe Eac3SurroundExMode,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
    dialnorm :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Eac3Settings' with the minimum fields required to make a request.
--
-- * 'stereoDownmix' - Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
-- * 'loRoCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
-- * 'ltRtCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
-- * 'lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
-- * 'dynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range compression.
-- * 'ltRtSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
-- * 'metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
-- * 'loRoSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
-- * 'surroundMode' - When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
-- * 'attenuationControl' - If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
-- * 'passthroughControl' - When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
-- * 'bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
-- * 'lfeControl' - When encoding 3/2 audio, controls whether the LFE channel is enabled
-- * 'dynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing the audio.
-- * 'codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
-- * 'sampleRate' - This value is always 48000. It represents the sample rate in Hz.
-- * 'dcFilter' - Activates a DC highpass filter for all input channels.
-- * 'bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
-- * 'phaseControl' - Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
-- * 'surroundExMode' - When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
-- * 'dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
mkEac3Settings ::
  Eac3Settings
mkEac3Settings =
  Eac3Settings'
    { stereoDownmix = Lude.Nothing,
      loRoCenterMixLevel = Lude.Nothing,
      ltRtCenterMixLevel = Lude.Nothing,
      lfeFilter = Lude.Nothing,
      dynamicRangeCompressionLine = Lude.Nothing,
      ltRtSurroundMixLevel = Lude.Nothing,
      metadataControl = Lude.Nothing,
      loRoSurroundMixLevel = Lude.Nothing,
      surroundMode = Lude.Nothing,
      attenuationControl = Lude.Nothing,
      passthroughControl = Lude.Nothing,
      bitstreamMode = Lude.Nothing,
      lfeControl = Lude.Nothing,
      dynamicRangeCompressionRf = Lude.Nothing,
      codingMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      dcFilter = Lude.Nothing,
      bitrate = Lude.Nothing,
      phaseControl = Lude.Nothing,
      surroundExMode = Lude.Nothing,
      dialnorm = Lude.Nothing
    }

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
--
-- /Note:/ Consider using 'stereoDownmix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStereoDownmix :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3StereoDownmix)
esStereoDownmix = Lens.lens (stereoDownmix :: Eac3Settings -> Lude.Maybe Eac3StereoDownmix) (\s a -> s {stereoDownmix = a} :: Eac3Settings)
{-# DEPRECATED esStereoDownmix "Use generic-lens or generic-optics with 'stereoDownmix' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoCenterMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLoRoCenterMixLevel = Lens.lens (loRoCenterMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {loRoCenterMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLoRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtCenterMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLtRtCenterMixLevel = Lens.lens (ltRtCenterMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtCenterMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLtRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead." #-}

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeFilter :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3LfeFilter)
esLfeFilter = Lens.lens (lfeFilter :: Eac3Settings -> Lude.Maybe Eac3LfeFilter) (\s a -> s {lfeFilter = a} :: Eac3Settings)
{-# DEPRECATED esLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | Specify the absolute peak level for a signal with dynamic range compression.
--
-- /Note:/ Consider using 'dynamicRangeCompressionLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDynamicRangeCompressionLine :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DynamicRangeCompressionLine)
esDynamicRangeCompressionLine = Lens.lens (dynamicRangeCompressionLine :: Eac3Settings -> Lude.Maybe Eac3DynamicRangeCompressionLine) (\s a -> s {dynamicRangeCompressionLine = a} :: Eac3Settings)
{-# DEPRECATED esDynamicRangeCompressionLine "Use generic-lens or generic-optics with 'dynamicRangeCompressionLine' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLtRtSurroundMixLevel = Lens.lens (ltRtSurroundMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtSurroundMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLtRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead." #-}

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esMetadataControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3MetadataControl)
esMetadataControl = Lens.lens (metadataControl :: Eac3Settings -> Lude.Maybe Eac3MetadataControl) (\s a -> s {metadataControl = a} :: Eac3Settings)
{-# DEPRECATED esMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLoRoSurroundMixLevel = Lens.lens (loRoSurroundMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {loRoSurroundMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLoRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead." #-}

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
--
-- /Note:/ Consider using 'surroundMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3SurroundMode)
esSurroundMode = Lens.lens (surroundMode :: Eac3Settings -> Lude.Maybe Eac3SurroundMode) (\s a -> s {surroundMode = a} :: Eac3Settings)
{-# DEPRECATED esSurroundMode "Use generic-lens or generic-optics with 'surroundMode' instead." #-}

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'attenuationControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAttenuationControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3AttenuationControl)
esAttenuationControl = Lens.lens (attenuationControl :: Eac3Settings -> Lude.Maybe Eac3AttenuationControl) (\s a -> s {attenuationControl = a} :: Eac3Settings)
{-# DEPRECATED esAttenuationControl "Use generic-lens or generic-optics with 'attenuationControl' instead." #-}

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- /Note:/ Consider using 'passthroughControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassthroughControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3PassthroughControl)
esPassthroughControl = Lens.lens (passthroughControl :: Eac3Settings -> Lude.Maybe Eac3PassthroughControl) (\s a -> s {passthroughControl = a} :: Eac3Settings)
{-# DEPRECATED esPassthroughControl "Use generic-lens or generic-optics with 'passthroughControl' instead." #-}

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitstreamMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3BitstreamMode)
esBitstreamMode = Lens.lens (bitstreamMode :: Eac3Settings -> Lude.Maybe Eac3BitstreamMode) (\s a -> s {bitstreamMode = a} :: Eac3Settings)
{-# DEPRECATED esBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
--
-- /Note:/ Consider using 'lfeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3LfeControl)
esLfeControl = Lens.lens (lfeControl :: Eac3Settings -> Lude.Maybe Eac3LfeControl) (\s a -> s {lfeControl = a} :: Eac3Settings)
{-# DEPRECATED esLfeControl "Use generic-lens or generic-optics with 'lfeControl' instead." #-}

-- | Specify how the service limits the audio dynamic range when compressing the audio.
--
-- /Note:/ Consider using 'dynamicRangeCompressionRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDynamicRangeCompressionRf :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DynamicRangeCompressionRf)
esDynamicRangeCompressionRf = Lens.lens (dynamicRangeCompressionRf :: Eac3Settings -> Lude.Maybe Eac3DynamicRangeCompressionRf) (\s a -> s {dynamicRangeCompressionRf = a} :: Eac3Settings)
{-# DEPRECATED esDynamicRangeCompressionRf "Use generic-lens or generic-optics with 'dynamicRangeCompressionRf' instead." #-}

-- | Dolby Digital Plus coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCodingMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3CodingMode)
esCodingMode = Lens.lens (codingMode :: Eac3Settings -> Lude.Maybe Eac3CodingMode) (\s a -> s {codingMode = a} :: Eac3Settings)
{-# DEPRECATED esCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSampleRate :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Natural)
esSampleRate = Lens.lens (sampleRate :: Eac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: Eac3Settings)
{-# DEPRECATED esSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Activates a DC highpass filter for all input channels.
--
-- /Note:/ Consider using 'dcFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDcFilter :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DcFilter)
esDcFilter = Lens.lens (dcFilter :: Eac3Settings -> Lude.Maybe Eac3DcFilter) (\s a -> s {dcFilter = a} :: Eac3Settings)
{-# DEPRECATED esDcFilter "Use generic-lens or generic-optics with 'dcFilter' instead." #-}

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitrate :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Natural)
esBitrate = Lens.lens (bitrate :: Eac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Eac3Settings)
{-# DEPRECATED esBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'phaseControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPhaseControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3PhaseControl)
esPhaseControl = Lens.lens (phaseControl :: Eac3Settings -> Lude.Maybe Eac3PhaseControl) (\s a -> s {phaseControl = a} :: Eac3Settings)
{-# DEPRECATED esPhaseControl "Use generic-lens or generic-optics with 'phaseControl' instead." #-}

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
--
-- /Note:/ Consider using 'surroundExMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundExMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3SurroundExMode)
esSurroundExMode = Lens.lens (surroundExMode :: Eac3Settings -> Lude.Maybe Eac3SurroundExMode) (\s a -> s {surroundExMode = a} :: Eac3Settings)
{-# DEPRECATED esSurroundExMode "Use generic-lens or generic-optics with 'surroundExMode' instead." #-}

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDialnorm :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Natural)
esDialnorm = Lens.lens (dialnorm :: Eac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {dialnorm = a} :: Eac3Settings)
{-# DEPRECATED esDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

instance Lude.FromJSON Eac3Settings where
  parseJSON =
    Lude.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Lude.<$> (x Lude..:? "stereoDownmix")
            Lude.<*> (x Lude..:? "loRoCenterMixLevel")
            Lude.<*> (x Lude..:? "ltRtCenterMixLevel")
            Lude.<*> (x Lude..:? "lfeFilter")
            Lude.<*> (x Lude..:? "dynamicRangeCompressionLine")
            Lude.<*> (x Lude..:? "ltRtSurroundMixLevel")
            Lude.<*> (x Lude..:? "metadataControl")
            Lude.<*> (x Lude..:? "loRoSurroundMixLevel")
            Lude.<*> (x Lude..:? "surroundMode")
            Lude.<*> (x Lude..:? "attenuationControl")
            Lude.<*> (x Lude..:? "passthroughControl")
            Lude.<*> (x Lude..:? "bitstreamMode")
            Lude.<*> (x Lude..:? "lfeControl")
            Lude.<*> (x Lude..:? "dynamicRangeCompressionRf")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "dcFilter")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "phaseControl")
            Lude.<*> (x Lude..:? "surroundExMode")
            Lude.<*> (x Lude..:? "dialnorm")
      )

instance Lude.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stereoDownmix" Lude..=) Lude.<$> stereoDownmix,
            ("loRoCenterMixLevel" Lude..=) Lude.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Lude..=) Lude.<$> ltRtCenterMixLevel,
            ("lfeFilter" Lude..=) Lude.<$> lfeFilter,
            ("dynamicRangeCompressionLine" Lude..=)
              Lude.<$> dynamicRangeCompressionLine,
            ("ltRtSurroundMixLevel" Lude..=) Lude.<$> ltRtSurroundMixLevel,
            ("metadataControl" Lude..=) Lude.<$> metadataControl,
            ("loRoSurroundMixLevel" Lude..=) Lude.<$> loRoSurroundMixLevel,
            ("surroundMode" Lude..=) Lude.<$> surroundMode,
            ("attenuationControl" Lude..=) Lude.<$> attenuationControl,
            ("passthroughControl" Lude..=) Lude.<$> passthroughControl,
            ("bitstreamMode" Lude..=) Lude.<$> bitstreamMode,
            ("lfeControl" Lude..=) Lude.<$> lfeControl,
            ("dynamicRangeCompressionRf" Lude..=)
              Lude.<$> dynamicRangeCompressionRf,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("dcFilter" Lude..=) Lude.<$> dcFilter,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("phaseControl" Lude..=) Lude.<$> phaseControl,
            ("surroundExMode" Lude..=) Lude.<$> surroundExMode,
            ("dialnorm" Lude..=) Lude.<$> dialnorm
          ]
      )
