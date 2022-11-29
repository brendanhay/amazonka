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
-- Module      : Amazonka.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Ac3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.Ac3BitstreamMode
import Amazonka.MediaConvert.Types.Ac3CodingMode
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Ac3LfeFilter
import Amazonka.MediaConvert.Types.Ac3MetadataControl
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- /See:/ 'newAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
    -- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Ac3BitstreamMode,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the RF operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionRf :: Prelude.Maybe Ac3DynamicRangeCompressionRf,
    -- | Specify the average bitrate in bits per second. The bitrate that you
    -- specify must be a multiple of 8000 within the allowed minimum and
    -- maximum values. Leave blank to use the default bitrate for the coding
    -- mode you select according ETSI TS 102 366. Valid bitrates for coding
    -- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
    -- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
    -- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
    -- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
    -- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | When you want to add Dolby dynamic range compression (DRC) signaling to
    -- your output stream, we recommend that you use the mode-specific settings
    -- instead of Dynamic range compression profile
    -- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
    -- range compression profile, line mode (dynamicRangeCompressionLine) and
    -- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
    -- Note that when you specify values for all three settings, MediaConvert
    -- ignores the value of this setting in favor of the mode-specific
    -- settings. If you do use this setting instead of the mode-specific
    -- settings, choose None (NONE) to leave out DRC signaling. Keep the
    -- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
    -- film standard profile for all operating modes.
    dynamicRangeCompressionProfile :: Prelude.Maybe Ac3DynamicRangeCompressionProfile,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the line operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionLine :: Prelude.Maybe Ac3DynamicRangeCompressionLine,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Ac3CodingMode,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Prelude.Maybe Ac3MetadataControl,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Ac3LfeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitstreamMode', 'ac3Settings_bitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'dynamicRangeCompressionRf', 'ac3Settings_dynamicRangeCompressionRf' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'bitrate', 'ac3Settings_bitrate' - Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
-- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
-- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
-- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
-- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
--
-- 'sampleRate', 'ac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'dynamicRangeCompressionProfile', 'ac3Settings_dynamicRangeCompressionProfile' - When you want to add Dolby dynamic range compression (DRC) signaling to
-- your output stream, we recommend that you use the mode-specific settings
-- instead of Dynamic range compression profile
-- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
-- range compression profile, line mode (dynamicRangeCompressionLine) and
-- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
-- Note that when you specify values for all three settings, MediaConvert
-- ignores the value of this setting in favor of the mode-specific
-- settings. If you do use this setting instead of the mode-specific
-- settings, choose None (NONE) to leave out DRC signaling. Keep the
-- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
-- film standard profile for all operating modes.
--
-- 'dialnorm', 'ac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
--
-- 'dynamicRangeCompressionLine', 'ac3Settings_dynamicRangeCompressionLine' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'codingMode', 'ac3Settings_codingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- 'metadataControl', 'ac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
--
-- 'lfeFilter', 'ac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
newAc3Settings ::
  Ac3Settings
newAc3Settings =
  Ac3Settings'
    { bitstreamMode = Prelude.Nothing,
      dynamicRangeCompressionRf = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      dynamicRangeCompressionProfile = Prelude.Nothing,
      dialnorm = Prelude.Nothing,
      dynamicRangeCompressionLine = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      metadataControl = Prelude.Nothing,
      lfeFilter = Prelude.Nothing
    }

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
ac3Settings_bitstreamMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3BitstreamMode)
ac3Settings_bitstreamMode = Lens.lens (\Ac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Ac3Settings' {} a -> s {bitstreamMode = a} :: Ac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
ac3Settings_dynamicRangeCompressionRf :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionRf)
ac3Settings_dynamicRangeCompressionRf = Lens.lens (\Ac3Settings' {dynamicRangeCompressionRf} -> dynamicRangeCompressionRf) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionRf = a} :: Ac3Settings)

-- | Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
-- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
-- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
-- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
-- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
ac3Settings_bitrate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_bitrate = Lens.lens (\Ac3Settings' {bitrate} -> bitrate) (\s@Ac3Settings' {} a -> s {bitrate = a} :: Ac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
ac3Settings_sampleRate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_sampleRate = Lens.lens (\Ac3Settings' {sampleRate} -> sampleRate) (\s@Ac3Settings' {} a -> s {sampleRate = a} :: Ac3Settings)

-- | When you want to add Dolby dynamic range compression (DRC) signaling to
-- your output stream, we recommend that you use the mode-specific settings
-- instead of Dynamic range compression profile
-- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
-- range compression profile, line mode (dynamicRangeCompressionLine) and
-- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
-- Note that when you specify values for all three settings, MediaConvert
-- ignores the value of this setting in favor of the mode-specific
-- settings. If you do use this setting instead of the mode-specific
-- settings, choose None (NONE) to leave out DRC signaling. Keep the
-- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
-- film standard profile for all operating modes.
ac3Settings_dynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionProfile)
ac3Settings_dynamicRangeCompressionProfile = Lens.lens (\Ac3Settings' {dynamicRangeCompressionProfile} -> dynamicRangeCompressionProfile) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionProfile = a} :: Ac3Settings)

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
ac3Settings_dialnorm :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_dialnorm = Lens.lens (\Ac3Settings' {dialnorm} -> dialnorm) (\s@Ac3Settings' {} a -> s {dialnorm = a} :: Ac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
ac3Settings_dynamicRangeCompressionLine :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionLine)
ac3Settings_dynamicRangeCompressionLine = Lens.lens (\Ac3Settings' {dynamicRangeCompressionLine} -> dynamicRangeCompressionLine) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionLine = a} :: Ac3Settings)

-- | Dolby Digital coding mode. Determines number of channels.
ac3Settings_codingMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3CodingMode)
ac3Settings_codingMode = Lens.lens (\Ac3Settings' {codingMode} -> codingMode) (\s@Ac3Settings' {} a -> s {codingMode = a} :: Ac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
ac3Settings_metadataControl :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3MetadataControl)
ac3Settings_metadataControl = Lens.lens (\Ac3Settings' {metadataControl} -> metadataControl) (\s@Ac3Settings' {} a -> s {metadataControl = a} :: Ac3Settings)

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
ac3Settings_lfeFilter :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3LfeFilter)
ac3Settings_lfeFilter = Lens.lens (\Ac3Settings' {lfeFilter} -> lfeFilter) (\s@Ac3Settings' {} a -> s {lfeFilter = a} :: Ac3Settings)

instance Core.FromJSON Ac3Settings where
  parseJSON =
    Core.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Prelude.<$> (x Core..:? "bitstreamMode")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionRf")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionProfile")
            Prelude.<*> (x Core..:? "dialnorm")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionLine")
            Prelude.<*> (x Core..:? "codingMode")
            Prelude.<*> (x Core..:? "metadataControl")
            Prelude.<*> (x Core..:? "lfeFilter")
      )

instance Prelude.Hashable Ac3Settings where
  hashWithSalt _salt Ac3Settings' {..} =
    _salt `Prelude.hashWithSalt` bitstreamMode
      `Prelude.hashWithSalt` dynamicRangeCompressionRf
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` dynamicRangeCompressionProfile
      `Prelude.hashWithSalt` dialnorm
      `Prelude.hashWithSalt` dynamicRangeCompressionLine
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` metadataControl
      `Prelude.hashWithSalt` lfeFilter

instance Prelude.NFData Ac3Settings where
  rnf Ac3Settings' {..} =
    Prelude.rnf bitstreamMode
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionRf
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionProfile
      `Prelude.seq` Prelude.rnf dialnorm
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionLine
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf metadataControl
      `Prelude.seq` Prelude.rnf lfeFilter

instance Core.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bitstreamMode" Core..=) Prelude.<$> bitstreamMode,
            ("dynamicRangeCompressionRf" Core..=)
              Prelude.<$> dynamicRangeCompressionRf,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("dynamicRangeCompressionProfile" Core..=)
              Prelude.<$> dynamicRangeCompressionProfile,
            ("dialnorm" Core..=) Prelude.<$> dialnorm,
            ("dynamicRangeCompressionLine" Core..=)
              Prelude.<$> dynamicRangeCompressionLine,
            ("codingMode" Core..=) Prelude.<$> codingMode,
            ("metadataControl" Core..=)
              Prelude.<$> metadataControl,
            ("lfeFilter" Core..=) Prelude.<$> lfeFilter
          ]
      )
