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
-- Module      : Network.AWS.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
import Network.AWS.MediaConvert.Types.Ac3CodingMode
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
import Network.AWS.MediaConvert.Types.Ac3MetadataControl

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- /See:/ 'newAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital, dialnorm will be passed through.
    dialnorm :: Core.Maybe Core.Natural,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Core.Maybe Ac3CodingMode,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Core.Maybe Ac3LfeFilter,
    -- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
    -- output bitstream as defined in the Dolby Digital specification.
    dynamicRangeCompressionProfile :: Core.Maybe Ac3DynamicRangeCompressionProfile,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
    -- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Core.Maybe Ac3BitstreamMode,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on
    -- the coding mode.
    bitrate :: Core.Maybe Core.Natural,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Core.Maybe Ac3MetadataControl
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Ac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialnorm', 'ac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
--
-- 'codingMode', 'ac3Settings_codingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- 'lfeFilter', 'ac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
--
-- 'dynamicRangeCompressionProfile', 'ac3Settings_dynamicRangeCompressionProfile' - If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
--
-- 'sampleRate', 'ac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'bitstreamMode', 'ac3Settings_bitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'bitrate', 'ac3Settings_bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
--
-- 'metadataControl', 'ac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
newAc3Settings ::
  Ac3Settings
newAc3Settings =
  Ac3Settings'
    { dialnorm = Core.Nothing,
      codingMode = Core.Nothing,
      lfeFilter = Core.Nothing,
      dynamicRangeCompressionProfile = Core.Nothing,
      sampleRate = Core.Nothing,
      bitstreamMode = Core.Nothing,
      bitrate = Core.Nothing,
      metadataControl = Core.Nothing
    }

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
ac3Settings_dialnorm :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
ac3Settings_dialnorm = Lens.lens (\Ac3Settings' {dialnorm} -> dialnorm) (\s@Ac3Settings' {} a -> s {dialnorm = a} :: Ac3Settings)

-- | Dolby Digital coding mode. Determines number of channels.
ac3Settings_codingMode :: Lens.Lens' Ac3Settings (Core.Maybe Ac3CodingMode)
ac3Settings_codingMode = Lens.lens (\Ac3Settings' {codingMode} -> codingMode) (\s@Ac3Settings' {} a -> s {codingMode = a} :: Ac3Settings)

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
ac3Settings_lfeFilter :: Lens.Lens' Ac3Settings (Core.Maybe Ac3LfeFilter)
ac3Settings_lfeFilter = Lens.lens (\Ac3Settings' {lfeFilter} -> lfeFilter) (\s@Ac3Settings' {} a -> s {lfeFilter = a} :: Ac3Settings)

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
ac3Settings_dynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Core.Maybe Ac3DynamicRangeCompressionProfile)
ac3Settings_dynamicRangeCompressionProfile = Lens.lens (\Ac3Settings' {dynamicRangeCompressionProfile} -> dynamicRangeCompressionProfile) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionProfile = a} :: Ac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
ac3Settings_sampleRate :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
ac3Settings_sampleRate = Lens.lens (\Ac3Settings' {sampleRate} -> sampleRate) (\s@Ac3Settings' {} a -> s {sampleRate = a} :: Ac3Settings)

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
ac3Settings_bitstreamMode :: Lens.Lens' Ac3Settings (Core.Maybe Ac3BitstreamMode)
ac3Settings_bitstreamMode = Lens.lens (\Ac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Ac3Settings' {} a -> s {bitstreamMode = a} :: Ac3Settings)

-- | Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
ac3Settings_bitrate :: Lens.Lens' Ac3Settings (Core.Maybe Core.Natural)
ac3Settings_bitrate = Lens.lens (\Ac3Settings' {bitrate} -> bitrate) (\s@Ac3Settings' {} a -> s {bitrate = a} :: Ac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
ac3Settings_metadataControl :: Lens.Lens' Ac3Settings (Core.Maybe Ac3MetadataControl)
ac3Settings_metadataControl = Lens.lens (\Ac3Settings' {metadataControl} -> metadataControl) (\s@Ac3Settings' {} a -> s {metadataControl = a} :: Ac3Settings)

instance Core.FromJSON Ac3Settings where
  parseJSON =
    Core.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Core.<$> (x Core..:? "dialnorm")
            Core.<*> (x Core..:? "codingMode")
            Core.<*> (x Core..:? "lfeFilter")
            Core.<*> (x Core..:? "dynamicRangeCompressionProfile")
            Core.<*> (x Core..:? "sampleRate")
            Core.<*> (x Core..:? "bitstreamMode")
            Core.<*> (x Core..:? "bitrate")
            Core.<*> (x Core..:? "metadataControl")
      )

instance Core.Hashable Ac3Settings

instance Core.NFData Ac3Settings

instance Core.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("dialnorm" Core..=) Core.<$> dialnorm,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("lfeFilter" Core..=) Core.<$> lfeFilter,
            ("dynamicRangeCompressionProfile" Core..=)
              Core.<$> dynamicRangeCompressionProfile,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("bitstreamMode" Core..=) Core.<$> bitstreamMode,
            ("bitrate" Core..=) Core.<$> bitrate,
            ("metadataControl" Core..=)
              Core.<$> metadataControl
          ]
      )
