{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSettings
  ( Eac3AtmosSettings (..),

    -- * Smart constructor
    mkEac3AtmosSettings,

    -- * Lenses
    easStereoDownmix,
    easLoRoCenterMixLevel,
    easLtRtCenterMixLevel,
    easDynamicRangeCompressionLine,
    easLtRtSurroundMixLevel,
    easLoRoSurroundMixLevel,
    easBitstreamMode,
    easDynamicRangeCompressionRf,
    easCodingMode,
    easSampleRate,
    easSpeechThreshold,
    easBitrate,
    easDialogueIntelligence,
    easMeteringMode,
    easSurroundExMode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
import Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
import Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
import Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
import Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- /See:/ 'mkEac3AtmosSettings' smart constructor.
data Eac3AtmosSettings = Eac3AtmosSettings'
  { -- | Choose how the service does stereo downmixing.
    stereoDownmix :: Lude.Maybe Eac3AtmosStereoDownmix,
    -- | Specify a value for the following Dolby Atmos setting: Left only/Right only center mix
    --
    -- (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this
    -- value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix).
    -- Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
    loRoCenterMixLevel :: Lude.Maybe Lude.Double,
    -- | Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
    ltRtCenterMixLevel :: Lude.Maybe Lude.Double,
    -- | Specify the absolute peak level for a signal with dynamic range compression.
    dynamicRangeCompressionLine :: Lude.Maybe Eac3AtmosDynamicRangeCompressionLine,
    -- | Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
    ltRtSurroundMixLevel :: Lude.Maybe Lude.Double,
    -- | Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
    loRoSurroundMixLevel :: Lude.Maybe Lude.Double,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
    bitstreamMode :: Lude.Maybe Eac3AtmosBitstreamMode,
    -- | Specify how the service limits the audio dynamic range when compressing the audio.
    dynamicRangeCompressionRf :: Lude.Maybe Eac3AtmosDynamicRangeCompressionRf,
    -- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
    codingMode :: Lude.Maybe Eac3AtmosCodingMode,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Lude.Maybe Lude.Natural,
    -- | Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
    speechThreshold :: Lude.Maybe Lude.Natural,
    -- | Specify the average bitrate in bits per second.
    --
    -- Valid values: 384k, 448k, 640k, 768k
    bitrate :: Lude.Maybe Lude.Natural,
    -- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
    dialogueIntelligence :: Lude.Maybe Eac3AtmosDialogueIntelligence,
    -- | Choose how the service meters the loudness of your audio.
    meteringMode :: Lude.Maybe Eac3AtmosMeteringMode,
    -- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
    surroundExMode :: Lude.Maybe Eac3AtmosSurroundExMode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Eac3AtmosSettings' with the minimum fields required to make a request.
--
-- * 'stereoDownmix' - Choose how the service does stereo downmixing.
-- * 'loRoCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left only/Right only center mix
--
-- (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this
-- value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix).
-- Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
-- * 'ltRtCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
-- * 'dynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range compression.
-- * 'ltRtSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
-- * 'loRoSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
-- * 'bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
-- * 'dynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing the audio.
-- * 'codingMode' - The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
-- * 'sampleRate' - This value is always 48000. It represents the sample rate in Hz.
-- * 'speechThreshold' - Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
-- * 'bitrate' - Specify the average bitrate in bits per second.
--
-- Valid values: 384k, 448k, 640k, 768k
-- * 'dialogueIntelligence' - Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
-- * 'meteringMode' - Choose how the service meters the loudness of your audio.
-- * 'surroundExMode' - Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
mkEac3AtmosSettings ::
  Eac3AtmosSettings
mkEac3AtmosSettings =
  Eac3AtmosSettings'
    { stereoDownmix = Lude.Nothing,
      loRoCenterMixLevel = Lude.Nothing,
      ltRtCenterMixLevel = Lude.Nothing,
      dynamicRangeCompressionLine = Lude.Nothing,
      ltRtSurroundMixLevel = Lude.Nothing,
      loRoSurroundMixLevel = Lude.Nothing,
      bitstreamMode = Lude.Nothing,
      dynamicRangeCompressionRf = Lude.Nothing,
      codingMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      speechThreshold = Lude.Nothing,
      bitrate = Lude.Nothing,
      dialogueIntelligence = Lude.Nothing,
      meteringMode = Lude.Nothing,
      surroundExMode = Lude.Nothing
    }

-- | Choose how the service does stereo downmixing.
--
-- /Note:/ Consider using 'stereoDownmix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easStereoDownmix :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosStereoDownmix)
easStereoDownmix = Lens.lens (stereoDownmix :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosStereoDownmix) (\s a -> s {stereoDownmix = a} :: Eac3AtmosSettings)
{-# DEPRECATED easStereoDownmix "Use generic-lens or generic-optics with 'stereoDownmix' instead." #-}

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only center mix
--
-- (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this
-- value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix).
-- Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLoRoCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Double)
easLoRoCenterMixLevel = Lens.lens (loRoCenterMixLevel :: Eac3AtmosSettings -> Lude.Maybe Lude.Double) (\s a -> s {loRoCenterMixLevel = a} :: Eac3AtmosSettings)
{-# DEPRECATED easLoRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead." #-}

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLtRtCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Double)
easLtRtCenterMixLevel = Lens.lens (ltRtCenterMixLevel :: Eac3AtmosSettings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtCenterMixLevel = a} :: Eac3AtmosSettings)
{-# DEPRECATED easLtRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead." #-}

-- | Specify the absolute peak level for a signal with dynamic range compression.
--
-- /Note:/ Consider using 'dynamicRangeCompressionLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDynamicRangeCompressionLine :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosDynamicRangeCompressionLine)
easDynamicRangeCompressionLine = Lens.lens (dynamicRangeCompressionLine :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosDynamicRangeCompressionLine) (\s a -> s {dynamicRangeCompressionLine = a} :: Eac3AtmosSettings)
{-# DEPRECATED easDynamicRangeCompressionLine "Use generic-lens or generic-optics with 'dynamicRangeCompressionLine' instead." #-}

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLtRtSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Double)
easLtRtSurroundMixLevel = Lens.lens (ltRtSurroundMixLevel :: Eac3AtmosSettings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtSurroundMixLevel = a} :: Eac3AtmosSettings)
{-# DEPRECATED easLtRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead." #-}

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easLoRoSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Double)
easLoRoSurroundMixLevel = Lens.lens (loRoSurroundMixLevel :: Eac3AtmosSettings -> Lude.Maybe Lude.Double) (\s a -> s {loRoSurroundMixLevel = a} :: Eac3AtmosSettings)
{-# DEPRECATED easLoRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead." #-}

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easBitstreamMode :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosBitstreamMode)
easBitstreamMode = Lens.lens (bitstreamMode :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosBitstreamMode) (\s a -> s {bitstreamMode = a} :: Eac3AtmosSettings)
{-# DEPRECATED easBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | Specify how the service limits the audio dynamic range when compressing the audio.
--
-- /Note:/ Consider using 'dynamicRangeCompressionRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDynamicRangeCompressionRf :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosDynamicRangeCompressionRf)
easDynamicRangeCompressionRf = Lens.lens (dynamicRangeCompressionRf :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosDynamicRangeCompressionRf) (\s a -> s {dynamicRangeCompressionRf = a} :: Eac3AtmosSettings)
{-# DEPRECATED easDynamicRangeCompressionRf "Use generic-lens or generic-optics with 'dynamicRangeCompressionRf' instead." #-}

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easCodingMode :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosCodingMode)
easCodingMode = Lens.lens (codingMode :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosCodingMode) (\s a -> s {codingMode = a} :: Eac3AtmosSettings)
{-# DEPRECATED easCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSampleRate :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Natural)
easSampleRate = Lens.lens (sampleRate :: Eac3AtmosSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: Eac3AtmosSettings)
{-# DEPRECATED easSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
--
-- /Note:/ Consider using 'speechThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSpeechThreshold :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Natural)
easSpeechThreshold = Lens.lens (speechThreshold :: Eac3AtmosSettings -> Lude.Maybe Lude.Natural) (\s a -> s {speechThreshold = a} :: Eac3AtmosSettings)
{-# DEPRECATED easSpeechThreshold "Use generic-lens or generic-optics with 'speechThreshold' instead." #-}

-- | Specify the average bitrate in bits per second.
--
-- Valid values: 384k, 448k, 640k, 768k
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easBitrate :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Lude.Natural)
easBitrate = Lens.lens (bitrate :: Eac3AtmosSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Eac3AtmosSettings)
{-# DEPRECATED easBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
--
-- /Note:/ Consider using 'dialogueIntelligence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easDialogueIntelligence :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosDialogueIntelligence)
easDialogueIntelligence = Lens.lens (dialogueIntelligence :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosDialogueIntelligence) (\s a -> s {dialogueIntelligence = a} :: Eac3AtmosSettings)
{-# DEPRECATED easDialogueIntelligence "Use generic-lens or generic-optics with 'dialogueIntelligence' instead." #-}

-- | Choose how the service meters the loudness of your audio.
--
-- /Note:/ Consider using 'meteringMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easMeteringMode :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosMeteringMode)
easMeteringMode = Lens.lens (meteringMode :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosMeteringMode) (\s a -> s {meteringMode = a} :: Eac3AtmosSettings)
{-# DEPRECATED easMeteringMode "Use generic-lens or generic-optics with 'meteringMode' instead." #-}

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
--
-- /Note:/ Consider using 'surroundExMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easSurroundExMode :: Lens.Lens' Eac3AtmosSettings (Lude.Maybe Eac3AtmosSurroundExMode)
easSurroundExMode = Lens.lens (surroundExMode :: Eac3AtmosSettings -> Lude.Maybe Eac3AtmosSurroundExMode) (\s a -> s {surroundExMode = a} :: Eac3AtmosSettings)
{-# DEPRECATED easSurroundExMode "Use generic-lens or generic-optics with 'surroundExMode' instead." #-}

instance Lude.FromJSON Eac3AtmosSettings where
  parseJSON =
    Lude.withObject
      "Eac3AtmosSettings"
      ( \x ->
          Eac3AtmosSettings'
            Lude.<$> (x Lude..:? "stereoDownmix")
            Lude.<*> (x Lude..:? "loRoCenterMixLevel")
            Lude.<*> (x Lude..:? "ltRtCenterMixLevel")
            Lude.<*> (x Lude..:? "dynamicRangeCompressionLine")
            Lude.<*> (x Lude..:? "ltRtSurroundMixLevel")
            Lude.<*> (x Lude..:? "loRoSurroundMixLevel")
            Lude.<*> (x Lude..:? "bitstreamMode")
            Lude.<*> (x Lude..:? "dynamicRangeCompressionRf")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "speechThreshold")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "dialogueIntelligence")
            Lude.<*> (x Lude..:? "meteringMode")
            Lude.<*> (x Lude..:? "surroundExMode")
      )

instance Lude.ToJSON Eac3AtmosSettings where
  toJSON Eac3AtmosSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stereoDownmix" Lude..=) Lude.<$> stereoDownmix,
            ("loRoCenterMixLevel" Lude..=) Lude.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Lude..=) Lude.<$> ltRtCenterMixLevel,
            ("dynamicRangeCompressionLine" Lude..=)
              Lude.<$> dynamicRangeCompressionLine,
            ("ltRtSurroundMixLevel" Lude..=) Lude.<$> ltRtSurroundMixLevel,
            ("loRoSurroundMixLevel" Lude..=) Lude.<$> loRoSurroundMixLevel,
            ("bitstreamMode" Lude..=) Lude.<$> bitstreamMode,
            ("dynamicRangeCompressionRf" Lude..=)
              Lude.<$> dynamicRangeCompressionRf,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("speechThreshold" Lude..=) Lude.<$> speechThreshold,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("dialogueIntelligence" Lude..=) Lude.<$> dialogueIntelligence,
            ("meteringMode" Lude..=) Lude.<$> meteringMode,
            ("surroundExMode" Lude..=) Lude.<$> surroundExMode
          ]
      )
