{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
import Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
import Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
import Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
import Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
--
-- /See:/ 'newEac3AtmosSettings' smart constructor.
data Eac3AtmosSettings = Eac3AtmosSettings'
  { -- | Specify a value for the following Dolby Atmos setting: Left only\/Right
    -- only center mix (Lo\/Ro center). MediaConvert uses this value for
    -- downmixing. How the service uses this value depends on the value that
    -- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
    -- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
    loRoCenterMixLevel :: Core.Maybe Core.Double,
    -- | Specify a value for the following Dolby Atmos setting: Left total\/Right
    -- total center mix (Lt\/Rt center). MediaConvert uses this value for
    -- downmixing. How the service uses this value depends on the value that
    -- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
    -- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
    ltRtCenterMixLevel :: Core.Maybe Core.Double,
    -- | Specify the percentage of audio content that must be speech before the
    -- encoder uses the measured speech loudness as the overall program
    -- loudness.
    speechThreshold :: Core.Maybe Core.Natural,
    -- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6
    -- (CODING_MODE_9_1_6).
    codingMode :: Core.Maybe Eac3AtmosCodingMode,
    -- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue
    -- analysis.
    dialogueIntelligence :: Core.Maybe Eac3AtmosDialogueIntelligence,
    -- | Specify a value for the following Dolby Atmos setting: Left only\/Right
    -- only (Lo\/Ro surround). MediaConvert uses this value for downmixing. How
    -- the service uses this value depends on the value that you choose for
    -- Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5,
    -- -6.0, and -60. The value -60 mutes the channel.
    loRoSurroundMixLevel :: Core.Maybe Core.Double,
    -- | Specify a value for the following Dolby Atmos setting: Left total\/Right
    -- total surround mix (Lt\/Rt surround). MediaConvert uses this value for
    -- downmixing. How the service uses this value depends on the value that
    -- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
    -- -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
    ltRtSurroundMixLevel :: Core.Maybe Core.Double,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Choose how the service does stereo downmixing.
    stereoDownmix :: Core.Maybe Eac3AtmosStereoDownmix,
    -- | Choose how the service meters the loudness of your audio.
    meteringMode :: Core.Maybe Eac3AtmosMeteringMode,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
    -- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Core.Maybe Eac3AtmosBitstreamMode,
    -- | Specify whether your input audio has an additional center rear surround
    -- channel matrix encoded into your left and right surround channels.
    surroundExMode :: Core.Maybe Eac3AtmosSurroundExMode,
    -- | Specify how the service limits the audio dynamic range when compressing
    -- the audio.
    dynamicRangeCompressionRf :: Core.Maybe Eac3AtmosDynamicRangeCompressionRf,
    -- | Specify the average bitrate in bits per second. Valid values: 384k,
    -- 448k, 640k, 768k
    bitrate :: Core.Maybe Core.Natural,
    -- | Specify the absolute peak level for a signal with dynamic range
    -- compression.
    dynamicRangeCompressionLine :: Core.Maybe Eac3AtmosDynamicRangeCompressionLine
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Eac3AtmosSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRoCenterMixLevel', 'eac3AtmosSettings_loRoCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left only\/Right
-- only center mix (Lo\/Ro center). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- 'ltRtCenterMixLevel', 'eac3AtmosSettings_ltRtCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left total\/Right
-- total center mix (Lt\/Rt center). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- 'speechThreshold', 'eac3AtmosSettings_speechThreshold' - Specify the percentage of audio content that must be speech before the
-- encoder uses the measured speech loudness as the overall program
-- loudness.
--
-- 'codingMode', 'eac3AtmosSettings_codingMode' - The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6
-- (CODING_MODE_9_1_6).
--
-- 'dialogueIntelligence', 'eac3AtmosSettings_dialogueIntelligence' - Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue
-- analysis.
--
-- 'loRoSurroundMixLevel', 'eac3AtmosSettings_loRoSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left only\/Right
-- only (Lo\/Ro surround). MediaConvert uses this value for downmixing. How
-- the service uses this value depends on the value that you choose for
-- Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5,
-- -6.0, and -60. The value -60 mutes the channel.
--
-- 'ltRtSurroundMixLevel', 'eac3AtmosSettings_ltRtSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left total\/Right
-- total surround mix (Lt\/Rt surround). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- 'sampleRate', 'eac3AtmosSettings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'stereoDownmix', 'eac3AtmosSettings_stereoDownmix' - Choose how the service does stereo downmixing.
--
-- 'meteringMode', 'eac3AtmosSettings_meteringMode' - Choose how the service meters the loudness of your audio.
--
-- 'bitstreamMode', 'eac3AtmosSettings_bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'surroundExMode', 'eac3AtmosSettings_surroundExMode' - Specify whether your input audio has an additional center rear surround
-- channel matrix encoded into your left and right surround channels.
--
-- 'dynamicRangeCompressionRf', 'eac3AtmosSettings_dynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing
-- the audio.
--
-- 'bitrate', 'eac3AtmosSettings_bitrate' - Specify the average bitrate in bits per second. Valid values: 384k,
-- 448k, 640k, 768k
--
-- 'dynamicRangeCompressionLine', 'eac3AtmosSettings_dynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range
-- compression.
newEac3AtmosSettings ::
  Eac3AtmosSettings
newEac3AtmosSettings =
  Eac3AtmosSettings'
    { loRoCenterMixLevel =
        Core.Nothing,
      ltRtCenterMixLevel = Core.Nothing,
      speechThreshold = Core.Nothing,
      codingMode = Core.Nothing,
      dialogueIntelligence = Core.Nothing,
      loRoSurroundMixLevel = Core.Nothing,
      ltRtSurroundMixLevel = Core.Nothing,
      sampleRate = Core.Nothing,
      stereoDownmix = Core.Nothing,
      meteringMode = Core.Nothing,
      bitstreamMode = Core.Nothing,
      surroundExMode = Core.Nothing,
      dynamicRangeCompressionRf = Core.Nothing,
      bitrate = Core.Nothing,
      dynamicRangeCompressionLine = Core.Nothing
    }

-- | Specify a value for the following Dolby Atmos setting: Left only\/Right
-- only center mix (Lo\/Ro center). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
eac3AtmosSettings_loRoCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
eac3AtmosSettings_loRoCenterMixLevel = Lens.lens (\Eac3AtmosSettings' {loRoCenterMixLevel} -> loRoCenterMixLevel) (\s@Eac3AtmosSettings' {} a -> s {loRoCenterMixLevel = a} :: Eac3AtmosSettings)

-- | Specify a value for the following Dolby Atmos setting: Left total\/Right
-- total center mix (Lt\/Rt center). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
eac3AtmosSettings_ltRtCenterMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
eac3AtmosSettings_ltRtCenterMixLevel = Lens.lens (\Eac3AtmosSettings' {ltRtCenterMixLevel} -> ltRtCenterMixLevel) (\s@Eac3AtmosSettings' {} a -> s {ltRtCenterMixLevel = a} :: Eac3AtmosSettings)

-- | Specify the percentage of audio content that must be speech before the
-- encoder uses the measured speech loudness as the overall program
-- loudness.
eac3AtmosSettings_speechThreshold :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
eac3AtmosSettings_speechThreshold = Lens.lens (\Eac3AtmosSettings' {speechThreshold} -> speechThreshold) (\s@Eac3AtmosSettings' {} a -> s {speechThreshold = a} :: Eac3AtmosSettings)

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6
-- (CODING_MODE_9_1_6).
eac3AtmosSettings_codingMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosCodingMode)
eac3AtmosSettings_codingMode = Lens.lens (\Eac3AtmosSettings' {codingMode} -> codingMode) (\s@Eac3AtmosSettings' {} a -> s {codingMode = a} :: Eac3AtmosSettings)

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue
-- analysis.
eac3AtmosSettings_dialogueIntelligence :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosDialogueIntelligence)
eac3AtmosSettings_dialogueIntelligence = Lens.lens (\Eac3AtmosSettings' {dialogueIntelligence} -> dialogueIntelligence) (\s@Eac3AtmosSettings' {} a -> s {dialogueIntelligence = a} :: Eac3AtmosSettings)

-- | Specify a value for the following Dolby Atmos setting: Left only\/Right
-- only (Lo\/Ro surround). MediaConvert uses this value for downmixing. How
-- the service uses this value depends on the value that you choose for
-- Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5,
-- -6.0, and -60. The value -60 mutes the channel.
eac3AtmosSettings_loRoSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
eac3AtmosSettings_loRoSurroundMixLevel = Lens.lens (\Eac3AtmosSettings' {loRoSurroundMixLevel} -> loRoSurroundMixLevel) (\s@Eac3AtmosSettings' {} a -> s {loRoSurroundMixLevel = a} :: Eac3AtmosSettings)

-- | Specify a value for the following Dolby Atmos setting: Left total\/Right
-- total surround mix (Lt\/Rt surround). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values:
-- -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
eac3AtmosSettings_ltRtSurroundMixLevel :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Double)
eac3AtmosSettings_ltRtSurroundMixLevel = Lens.lens (\Eac3AtmosSettings' {ltRtSurroundMixLevel} -> ltRtSurroundMixLevel) (\s@Eac3AtmosSettings' {} a -> s {ltRtSurroundMixLevel = a} :: Eac3AtmosSettings)

-- | This value is always 48000. It represents the sample rate in Hz.
eac3AtmosSettings_sampleRate :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
eac3AtmosSettings_sampleRate = Lens.lens (\Eac3AtmosSettings' {sampleRate} -> sampleRate) (\s@Eac3AtmosSettings' {} a -> s {sampleRate = a} :: Eac3AtmosSettings)

-- | Choose how the service does stereo downmixing.
eac3AtmosSettings_stereoDownmix :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosStereoDownmix)
eac3AtmosSettings_stereoDownmix = Lens.lens (\Eac3AtmosSettings' {stereoDownmix} -> stereoDownmix) (\s@Eac3AtmosSettings' {} a -> s {stereoDownmix = a} :: Eac3AtmosSettings)

-- | Choose how the service meters the loudness of your audio.
eac3AtmosSettings_meteringMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosMeteringMode)
eac3AtmosSettings_meteringMode = Lens.lens (\Eac3AtmosSettings' {meteringMode} -> meteringMode) (\s@Eac3AtmosSettings' {} a -> s {meteringMode = a} :: Eac3AtmosSettings)

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
eac3AtmosSettings_bitstreamMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosBitstreamMode)
eac3AtmosSettings_bitstreamMode = Lens.lens (\Eac3AtmosSettings' {bitstreamMode} -> bitstreamMode) (\s@Eac3AtmosSettings' {} a -> s {bitstreamMode = a} :: Eac3AtmosSettings)

-- | Specify whether your input audio has an additional center rear surround
-- channel matrix encoded into your left and right surround channels.
eac3AtmosSettings_surroundExMode :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosSurroundExMode)
eac3AtmosSettings_surroundExMode = Lens.lens (\Eac3AtmosSettings' {surroundExMode} -> surroundExMode) (\s@Eac3AtmosSettings' {} a -> s {surroundExMode = a} :: Eac3AtmosSettings)

-- | Specify how the service limits the audio dynamic range when compressing
-- the audio.
eac3AtmosSettings_dynamicRangeCompressionRf :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosDynamicRangeCompressionRf)
eac3AtmosSettings_dynamicRangeCompressionRf = Lens.lens (\Eac3AtmosSettings' {dynamicRangeCompressionRf} -> dynamicRangeCompressionRf) (\s@Eac3AtmosSettings' {} a -> s {dynamicRangeCompressionRf = a} :: Eac3AtmosSettings)

-- | Specify the average bitrate in bits per second. Valid values: 384k,
-- 448k, 640k, 768k
eac3AtmosSettings_bitrate :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Core.Natural)
eac3AtmosSettings_bitrate = Lens.lens (\Eac3AtmosSettings' {bitrate} -> bitrate) (\s@Eac3AtmosSettings' {} a -> s {bitrate = a} :: Eac3AtmosSettings)

-- | Specify the absolute peak level for a signal with dynamic range
-- compression.
eac3AtmosSettings_dynamicRangeCompressionLine :: Lens.Lens' Eac3AtmosSettings (Core.Maybe Eac3AtmosDynamicRangeCompressionLine)
eac3AtmosSettings_dynamicRangeCompressionLine = Lens.lens (\Eac3AtmosSettings' {dynamicRangeCompressionLine} -> dynamicRangeCompressionLine) (\s@Eac3AtmosSettings' {} a -> s {dynamicRangeCompressionLine = a} :: Eac3AtmosSettings)

instance Core.FromJSON Eac3AtmosSettings where
  parseJSON =
    Core.withObject
      "Eac3AtmosSettings"
      ( \x ->
          Eac3AtmosSettings'
            Core.<$> (x Core..:? "loRoCenterMixLevel")
            Core.<*> (x Core..:? "ltRtCenterMixLevel")
            Core.<*> (x Core..:? "speechThreshold")
            Core.<*> (x Core..:? "codingMode")
            Core.<*> (x Core..:? "dialogueIntelligence")
            Core.<*> (x Core..:? "loRoSurroundMixLevel")
            Core.<*> (x Core..:? "ltRtSurroundMixLevel")
            Core.<*> (x Core..:? "sampleRate")
            Core.<*> (x Core..:? "stereoDownmix")
            Core.<*> (x Core..:? "meteringMode")
            Core.<*> (x Core..:? "bitstreamMode")
            Core.<*> (x Core..:? "surroundExMode")
            Core.<*> (x Core..:? "dynamicRangeCompressionRf")
            Core.<*> (x Core..:? "bitrate")
            Core.<*> (x Core..:? "dynamicRangeCompressionLine")
      )

instance Core.Hashable Eac3AtmosSettings

instance Core.NFData Eac3AtmosSettings

instance Core.ToJSON Eac3AtmosSettings where
  toJSON Eac3AtmosSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("loRoCenterMixLevel" Core..=)
              Core.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Core..=)
              Core.<$> ltRtCenterMixLevel,
            ("speechThreshold" Core..=) Core.<$> speechThreshold,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("dialogueIntelligence" Core..=)
              Core.<$> dialogueIntelligence,
            ("loRoSurroundMixLevel" Core..=)
              Core.<$> loRoSurroundMixLevel,
            ("ltRtSurroundMixLevel" Core..=)
              Core.<$> ltRtSurroundMixLevel,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("stereoDownmix" Core..=) Core.<$> stereoDownmix,
            ("meteringMode" Core..=) Core.<$> meteringMode,
            ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
            ("surroundExMode" Core..=) Core.<$> surroundExMode,
            ("dynamicRangeCompressionRf" Core..=)
              Core.<$> dynamicRangeCompressionRf,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("dynamicRangeCompressionLine" Core..=)
              Core.<$> dynamicRangeCompressionLine
          ]
      )
